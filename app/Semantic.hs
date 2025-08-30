module Semantic
(
    resolve
) where

import Parse
import TypeCheck

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (isJust)

type SemanticMonad m = ReaderT LocalVars (State SemanticState) m
type IdentMap = M.Map String (String, Bool)

data SemanticState = SemanticState {
  blockVars :: IdentMap
, labels :: [(String, String)]
, nameCount  :: Int
, err :: Maybe String
, switchLabels :: [Maybe Integer]
}

data LocalVars = LocalVars {
    identifierMap :: IdentMap,
    breakLabel :: Maybe String,
    continueLabel :: Maybe String,
    switchLabel :: Maybe String,
    localSwitch :: [Maybe Integer]
}

localVars :: LocalVars
localVars = LocalVars {
      identifierMap = M.empty
    , breakLabel = Nothing
    , continueLabel = Nothing
    , switchLabel = Nothing
    , localSwitch = []
}

initState :: SemanticState
initState = SemanticState {
      blockVars = M.empty
    , labels = []
    , switchLabels = []
    , nameCount = 0
    , err = Nothing}

resolve :: Program -> Either String Program
resolve prog = do
    let result = runState (runReaderT (resolveProg prog) localVars) initState
    case err (snd result) of
        Just e -> Left e
        Nothing -> resolveGoto result

resolveGoto :: (Program, SemanticState) -> Either String Program
resolveGoto (prog, s) = do
    let (prog', s') = runState (runReaderT (gotoProg prog) localVars) s
    case err s' of
        Just e -> Left e
        Nothing -> resolveType prog'

gotoProg :: Program -> SemanticMonad Program
gotoProg (Program f) = Program <$> mapM gotoFunc f

gotoFunc :: Function -> SemanticMonad Function
gotoFunc (Function name params (Just (Block items))) = Function name params . Just . Block <$> mapM gotoItem items
gotoFunc nothingFun = return nothingFun

gotoItem :: BlockItem -> SemanticMonad BlockItem
gotoItem (S stmt) = S <$> gotoStmt stmt
gotoItem (D decl) = return $ D decl

gotoStmt :: Statement -> SemanticMonad Statement
gotoStmt (Labelled name stmt) = Labelled name <$> gotoStmt stmt
gotoStmt (Compound (Block items)) = Compound . Block <$> mapM gotoItem items
gotoStmt (Goto name) = do
    m <- gets labels
    case lookup name m of
        Just x -> return $ Goto x
        Nothing -> writeError "Goto missing label!"
gotoStmt (If e1 s1 s2) = do
    s1' <- gotoStmt s1
    s2' <- resolveOpt s2 gotoStmt
    return $ If e1 s1' s2'
gotoStmt (Switch e s n c) = do
    s' <- gotoStmt s
    return $ Switch e s' n c
gotoStmt (Case e s) = Case e <$> gotoStmt s
gotoStmt (Default s) = Default <$> gotoStmt s
gotoStmt (DoWhile s e n) = do
    s' <- gotoStmt s
    return $ DoWhile s' e n
gotoStmt (While e s n) = do
    s' <- gotoStmt s
    return $ While e s' n
gotoStmt (For i e1 e2 s n) = do
    s' <- gotoStmt s
    return $ For i e1 e2 s' n
gotoStmt s = return s

resolveProg :: Program -> SemanticMonad Program
resolveProg (Program f) = Program <$> mapM resolveFunc f

resolveFunc :: Function -> SemanticMonad Function
resolveFunc (Function name params blk) = do
    gm <- gets blockVars
    lm <- asks identifierMap
    case M.lookup name gm of 
        Just x -> if snd x then return () else writeError "Duplicate declaration"
        Nothing -> case M.lookup name lm of
                    Just _ -> return ()
                    Nothing -> return ()
    modify $ \x -> x { blockVars = M.insert name (name, True) gm }

    s <- gets (remap . blockVars)
    (params', blk') <- local s $ do
        modify $ \x -> x { blockVars = M.empty}
        mapM_ (resolveDecl . VarDecl . \x -> (x, Nothing)) params
        params' <- gets (map fst . M.elems . blockVars)
        blk' <- case blk of 
                Just (Block bs) -> Just . Block <$> mapM resolveItem bs
                Nothing -> return Nothing
        oldVars <- asks identifierMap
        modify $ \x -> x { blockVars = oldVars }
        return (params', blk')
    return $ Function name params' blk'

descend :: (a -> SemanticMonad a) -> a -> SemanticMonad a
descend f arg = do
    s <- gets (remap . blockVars)
    local s $ do
        modify $ \x -> x { blockVars = M.empty}
        r <- f arg
        oldVars <- asks identifierMap
        modify $ \x -> x { blockVars = oldVars }
        return r

resolveBlock :: [BlockItem] -> SemanticMonad Block
resolveBlock items = Block <$> descend (mapM resolveItem) items

remap :: IdentMap -> LocalVars -> LocalVars
remap s r = r { identifierMap = M.union s (identifierMap r) }

resolveItem :: BlockItem -> SemanticMonad BlockItem
resolveItem (S stmt) = S <$> resolveStmt stmt
resolveItem (D decl) = D <$> resolveDecl decl

resolveOpt :: Monad f => Maybe a -> (a -> f a) -> f (Maybe a)
resolveOpt (Just e) f = Just <$> f e
resolveOpt Nothing _ = return Nothing

resolveForInit :: ForInit -> SemanticMonad ForInit
resolveForInit (InitExpr e) = InitExpr <$> resolveOpt e resolveExpr
resolveForInit (InitDecl d) = InitDecl <$> resolveDecl d

resolveFor :: Statement -> SemanticMonad Statement
resolveFor (For initial cond post body name) = do
    i <- resolveForInit initial
    c <- resolveOpt cond resolveExpr
    p <- resolveOpt post resolveExpr
    b <- resolveStmt body
    return $ For i c p b name
resolveFor _ = error "Shouldn't happen"

newLoopLabel :: String -> LocalVars -> LocalVars
newLoopLabel new l = l { breakLabel = Just new, continueLabel = Just new }

newSwitchLabel :: String -> [Maybe Integer] -> LocalVars -> LocalVars
newSwitchLabel new lbls l = l { breakLabel = Just new, switchLabel = Just new, localSwitch = lbls}

evalConstant :: Expr -> Integer
evalConstant (Int i) = i
evalConstant _ = error "Cannot parse complicated expressions yet"

resolveStmt :: Statement -> SemanticMonad Statement
resolveStmt (Return e) = Return <$> resolveExpr e
resolveStmt (Expression e) = Expression <$> resolveExpr e
resolveStmt (If e1 e2 e3) = do
    r1 <- resolveExpr e1
    r2 <- resolveStmt e2
    r3 <- resolveOpt e3 resolveStmt
    return $ If r1 r2 r3
resolveStmt (While e s _) = do
    label <- uniqueLabel
    local (newLoopLabel label) $ do
        e1 <- resolveExpr e
        s1 <- resolveStmt s
        return $ While e1 s1 label
resolveStmt (DoWhile s e _) = do
    label <- uniqueLabel
    local (newLoopLabel label) $ do
        e1 <- resolveExpr e
        s1 <- resolveStmt s
        return $ DoWhile s1 e1 label
resolveStmt (Case e s) = do
    l <- asks switchLabel
    e1 <- resolveExpr e
    s1 <- resolveStmt s
    case l of
        Just l' -> do
            let n = evalConstant e1
            lbls <- gets switchLabels
            if Just n `elem` lbls then writeError "Duplicate case!"
            else do
                modify $ \x -> x { switchLabels = Just n : switchLabels x}
                return $ Labelled (l' ++ "." ++ show n) s1
        Nothing   -> writeError "Not in switch!"
resolveStmt (Default s) = do
    l <- asks switchLabel
    s1 <- resolveStmt s
    case l of
        Just l' -> do
            lbls <- gets switchLabels
            if Nothing `elem` lbls then writeError "Duplicate default!"
            else do
                modify $ \x -> x { switchLabels = Nothing : switchLabels x}
                return $ Labelled (l' ++ ".default") s1
        Nothing   -> writeError "Not in switch!"
resolveStmt (For i c p b _) = do
    label <- uniqueLabel
    local (newLoopLabel label) $ descend resolveFor (For i c p b label)
resolveStmt (Switch e s _ _) = do
    label <- uniqueLabel
    lbls <- gets switchLabels
    local (newSwitchLabel label lbls) $ do
        modify $ \x -> x { switchLabels = []}
        e1 <- resolveExpr e
        s1 <- resolveStmt s
        slbl <- gets switchLabels
        oldLabels <- asks localSwitch
        modify $ \x -> x { switchLabels = oldLabels }
        return $ Switch e1 s1 label slbl
resolveStmt (Break _) = do
    l <- asks breakLabel
    case l of
        Just name -> return $ Break name
        Nothing   -> writeError "No label!"
resolveStmt (Continue _) = do
    l <- asks continueLabel
    case l of
        Just name -> return $ Continue name
        Nothing   -> writeError "No label!"
resolveStmt (Goto label) = return $ Goto label
resolveStmt (Labelled name stmt) = do
    s1 <- resolveStmt stmt
    m <- gets labels
    unique <- uniqueLabel
    if isJust (lookup name m) then writeError "Duplicate label declaration!"
    else modify $ \x -> x {labels = (name, unique) : labels x}
    return $ Labelled unique s1
resolveStmt (Compound (Block items)) = Compound <$> resolveBlock items
resolveStmt op = return op

writeError :: String -> SemanticMonad a
writeError s = do
    modify (\x -> x { err = Just s}) *> error s

resolveDecl :: Declaration -> SemanticMonad Declaration
resolveDecl (VarDecl (name, i)) = do
    m <- gets blockVars
    unique <- uniqueVar
    if isJust (M.lookup name m) then writeError "Duplicate variable declaration!"
    else
        modify $ \x -> x {blockVars = M.insert name (unique, False) $ blockVars x}
    case i of
        Just x -> do
            e <- resolveExpr x
            return $ VarDecl (unique, Just e)
        Nothing -> return $ VarDecl (unique, i)
resolveDecl (FuncDecl fun@(Function _ _ body)) = do
    if isJust body 
        then writeError "Function defined not at global scope!"
        else FuncDecl <$> resolveFunc fun

resolveExpr :: Expr -> SemanticMonad Expr
resolveExpr (Assignment (Var s) r) = do
    Assignment <$> resolveExpr (Var s) <*> resolveExpr r
resolveExpr (Assignment _ _) = writeError "Invalid lvalue!"
resolveExpr (CompoundAssignment op (Var s) r) = do
    CompoundAssignment op <$> resolveExpr (Var s) <*> resolveExpr r
resolveExpr (CompoundAssignment {}) = writeError "Invalid lvalue!"
resolveExpr (Var v) = do
    gm <- gets blockVars
    lm <- asks identifierMap
    case M.lookup v gm of
        Just x -> return $ Var (fst x)
        Nothing -> case M.lookup v lm of
                    Just x -> return $ Var (fst x)
                    Nothing -> writeError $ "Undeclared variable: " ++ show v
resolveExpr (Unary PreDec (Var s)) = Unary PreDec <$> resolveExpr (Var s)
resolveExpr (Unary PreInc (Var s)) = Unary PreInc <$> resolveExpr (Var s)
resolveExpr (Unary PreDec _) = writeError "Invalid lvalue!"
resolveExpr (Unary PreInc _) = writeError "Invalid lvalue!"
resolveExpr (Unary PostDec (Var s)) = Unary PostDec <$> resolveExpr (Var s)
resolveExpr (Unary PostInc (Var s)) = Unary PostInc <$> resolveExpr (Var s)
resolveExpr (Unary PostDec _) = writeError "Invalid lvalue!"
resolveExpr (Unary PostInc _) = writeError "Invalid lvalue!"
resolveExpr (Unary op e) = Unary op <$> resolveExpr e
resolveExpr (Binary op e1 e2) = Binary op <$> resolveExpr e1 <*> resolveExpr e2
resolveExpr (Int i) = return (Int i)
resolveExpr (Conditional e1 e2 e3) = Conditional <$> resolveExpr e1 <*> resolveExpr e2 <*> resolveExpr e3
resolveExpr (FunctionCall name args) = do
    gm <- gets blockVars
    lm <- asks identifierMap
    case M.lookup name gm of
        Just x -> FunctionCall (fst x) <$> mapM resolveExpr args
        Nothing -> case M.lookup name lm of
                    Just x -> FunctionCall (fst x) <$> mapM resolveExpr args
                    Nothing -> writeError $ "Undeclared function: " ++ show name

uniqueName :: String -> SemanticMonad String
uniqueName s = do
    ct <- gets (show . nameCount)
    modify $ \x -> x {nameCount = 1 + nameCount x}
    return $ s ++ ct

uniqueVar :: SemanticMonad String
uniqueVar = uniqueName "var."

uniqueLabel :: SemanticMonad String
uniqueLabel = uniqueName "lbl_"