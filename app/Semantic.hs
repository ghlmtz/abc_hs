module Semantic
(
    resolve
) where

import Parse
import qualified TypeCheck as TC

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust, isNothing)

type SemanticMonad m = ReaderT LocalVars (State SemanticState) m
type IdentMap = M.Map String (String, Bool)

data SemanticState = SemanticState {
  blockVars :: IdentMap
, labels :: [(String, String)]
, nameCount  :: Int
, err :: Maybe String
, switchLabels :: [Maybe StaticInit]
}

data LocalVars = LocalVars {
    identifierMap :: IdentMap,
    breakLabel :: Maybe String,
    continueLabel :: Maybe String,
    switchLabel :: Maybe String,
    localSwitch :: [Maybe StaticInit],
    blockScope :: Bool
}

localVars :: LocalVars
localVars = LocalVars {
      identifierMap = M.empty
    , breakLabel = Nothing
    , continueLabel = Nothing
    , switchLabel = Nothing
    , localSwitch = []
    , blockScope = False
}

initState :: SemanticState
initState = SemanticState {
      blockVars = M.empty
    , labels = []
    , switchLabels = []
    , nameCount = 0
    , err = Nothing}

resolve :: Program -> Either String (TC.Program, M.Map String (PType, TC.IdentAttr))
resolve prog = do
    let result = runState (runReaderT (resolveProg prog) localVars) initState
    case err (snd result) of
        Just e -> Left e
        Nothing -> TC.resolveType $ fst result

gotoFunc :: Declaration -> SemanticMonad Declaration
gotoFunc (FuncDecl name params s t (Just (Block items))) = FuncDecl name params s t . Just . Block <$> mapM gotoItem items
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
    s2' <- traverse gotoStmt s2
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
resolveProg (Program f) = Program <$> mapM resolveDecl f

resolveFunc :: Declaration -> SemanticMonad Declaration
resolveFunc (FuncDecl name params st t blk) = do
    gm <- gets blockVars
    level <- asks blockScope
    when (level && st == Just Static) $ writeError "Cannot have static function at block level"
    modify $ \x -> x { blockVars = M.insert name (name, True) gm }

    s <- gets (remap . blockVars)
    (params', blk') <- local s $ do
        modify $ \x -> x { blockVars = M.empty}
        mapM_ (resolveDecl . (\x -> VarDecl x Nothing TInt Nothing)) params
        vars <- gets blockVars
        let params' = map (fst . fromJust . flip M.lookup vars) params
        blk' <- case blk of
                Just (Block bs) -> Just . Block <$> mapM resolveItem bs
                Nothing -> return Nothing
        oldVars <- asks identifierMap
        modify $ \x -> x { blockVars = oldVars }
        return (params', blk')
    g <- gotoFunc $ FuncDecl name params' st t blk'
    modify $ \x -> x { labels = [] }
    return g
resolveFunc _ = error "Hmm"

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
remap s r = r { identifierMap = M.union s (identifierMap r), blockScope = True }

resolveItem :: BlockItem -> SemanticMonad BlockItem
resolveItem (S stmt) = S <$> resolveStmt stmt
resolveItem (D decl) = D <$> resolveDecl decl

resolveForInit :: ForInit -> SemanticMonad ForInit
resolveForInit (InitExpr e) = InitExpr <$> traverse resolveExpr e
resolveForInit (InitDecl d) = do
    d' <- resolveDecl d
    case d' of
        FuncDecl {} -> writeError "Cannot declare function in for init"
        VarDecl _ (Just _) _ _ -> writeError "Cannot have storage ident in for init"
        _ -> return $ InitDecl d'

resolveFor :: Statement -> SemanticMonad Statement
resolveFor (For initial cond post body name) = do
    i <- resolveForInit initial
    c <- traverse resolveExpr cond
    p <- traverse resolveExpr post
    b <- resolveStmt body
    return $ For i c p b name
resolveFor _ = error "Shouldn't happen"

newLoopLabel :: String -> LocalVars -> LocalVars
newLoopLabel new l = l { breakLabel = Just new, continueLabel = Just new }

newSwitchLabel :: String -> [Maybe StaticInit] -> LocalVars -> LocalVars
newSwitchLabel new lbls l = l { breakLabel = Just new, switchLabel = Just new, localSwitch = lbls}

evalConstant :: Expr -> Maybe StaticInit
evalConstant (Constant (ConstInt i)) = Just (IntInit (fromIntegral i))
evalConstant (Constant (ConstLong i)) = Just (LongInit (fromIntegral i))
evalConstant _ = Nothing

resolveStmt :: Statement -> SemanticMonad Statement
resolveStmt (Return e) = Return <$> resolveExpr e
resolveStmt (Expression e) = Expression <$> resolveExpr e
resolveStmt (If e1 e2 e3) = do
    r1 <- resolveExpr e1
    r2 <- resolveStmt e2
    r3 <- traverse resolveStmt e3
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
    n <- resolveExpr e
    s1 <- resolveStmt s
    let n' = evalConstant n
    when (isNothing n') $ writeError "Non-constant case expression"
    case l of
        Just l' -> do
            lbls <- gets switchLabels
            when (n' `elem` lbls) $ writeError "Duplicate case!"
            modify $ \x -> x { switchLabels = n' : lbls}
            return $ Labelled (l' ++ "." ++ show (fromJust n')) s1
        Nothing   -> writeError "Not in switch!"
resolveStmt (Default s) = do
    l <- asks switchLabel
    s1 <- resolveStmt s
    case l of
        Just l' -> do
            lbls <- gets switchLabels
            when (Nothing `elem` lbls) $ writeError "Duplicate default!"
            modify $ \x -> x { switchLabels = Nothing : lbls}
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
    maybe (writeError "No label!") (return . Break) l
resolveStmt (Continue _) = do
    l <- asks continueLabel
    maybe (writeError "No label!") (return . Continue) l
resolveStmt (Goto label) = return $ Goto label
resolveStmt (Labelled name stmt) = do
    s1 <- resolveStmt stmt
    m <- gets labels
    unique <- uniqueLabel
    when (isJust (lookup name m)) $ writeError "Duplicate label declaration!"
    modify $ \x -> x {labels = (name, unique) : labels x}
    return $ Labelled unique s1
resolveStmt (Compound (Block items)) = Compound <$> resolveBlock items
resolveStmt op = return op

writeError :: String -> SemanticMonad a
writeError s = modify (\x -> x { err = Just s }) *> error s

resolveDecl :: Declaration -> SemanticMonad Declaration
resolveDecl (VarDecl name s t initial) = do
    m <- gets blockVars
    inBlock <- asks blockScope
    uniq <- if not inBlock
        then do
            modify $ \x -> x {blockVars = M.insert name (name, True) m}
            return name
        else case M.lookup name m of
                Just (v, b) -> if not (b && s == Just Extern)
                               then writeError "Duplicate variable declaration!"
                               else modify id >> return v
                Nothing -> do
                    unique <- if s == Just Extern then return name else uniqueVar
                    modify $ \x -> x {blockVars = M.insert name (unique, s == Just Extern) m}
                    return unique
    case initial of
        Just x  -> VarDecl uniq s t . Just <$> resolveExpr x
        Nothing -> return $ VarDecl uniq s t Nothing
resolveDecl fun = resolveFunc fun

resolveExpr :: Expr -> SemanticMonad Expr
resolveExpr (Assignment (Var s) r) = do
    Assignment <$> resolveExpr (Var s) <*> resolveExpr r
resolveExpr (Assignment _ _) = writeError "Invalid lvalue!"
resolveExpr (CompoundAssignment op (Var s) r) = do
    CompoundAssignment op <$> resolveExpr (Var s) <*> resolveExpr r
resolveExpr (CompoundAssignment {}) = writeError "Invalid lvalue!"
resolveExpr (Var v) = do
    gm <- gets (M.lookup v . blockVars)
    lm <- asks (M.lookup v . identifierMap)
    case gm of
        Just x -> return $ Var (fst x)
        Nothing -> case lm of
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
resolveExpr (Constant i) = return (Constant i)
resolveExpr (Conditional e1 e2 e3) = Conditional <$> resolveExpr e1 <*> resolveExpr e2 <*> resolveExpr e3
resolveExpr (FunctionCall name args) = do
    gm <- gets (M.lookup name . blockVars)
    lm <- asks (M.lookup name . identifierMap)
    case gm of
        Just x -> FunctionCall (fst x) <$> mapM resolveExpr args
        Nothing -> case lm of
                    Just x -> FunctionCall (fst x) <$> mapM resolveExpr args
                    Nothing -> writeError $ "Undeclared function: " ++ show name
resolveExpr (Cast t e) = Cast t <$> resolveExpr e

uniqueName :: String -> SemanticMonad String
uniqueName s = do
    ct <- gets nameCount
    modify $ \x -> x {nameCount = 1 + ct}
    return $ s ++ show ct

uniqueVar :: SemanticMonad String
uniqueVar = uniqueName "var."

uniqueLabel :: SemanticMonad String
uniqueLabel = uniqueName "lbl."