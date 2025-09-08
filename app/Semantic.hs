module Semantic
(
  resolve
, Expr(..)
, Declaration(..)
, Statement(..)
, ForInit(..)
, Block(..)
, BlockItem(..)
, SProgram(..)
) where

import qualified Parse as P
import Parse (UnaryOp(..))

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)

data Expr = Int Integer
           | Unary P.UnaryOp Expr
           | Binary P.BinaryOp Expr Expr
           | Var String
           | Assignment Expr Expr
           | CompoundAssignment P.BinaryOp Expr Expr
           | Conditional Expr Expr Expr
           | FunctionCall String [Expr]
    deriving (Show)
data Declaration = FuncDecl String [String] (Maybe P.Storage) (Maybe Block) 
                 | VarDecl String (Maybe P.Storage) (Maybe Expr) 
    deriving (Show)
newtype Block = Block [BlockItem]
    deriving (Show)
data ForInit = InitDecl Declaration | InitExpr (Maybe Expr)
    deriving (Show)
data Statement = Return Expr
               | Expression Expr
               | Goto String
               | Compound Block
               | If Expr Statement (Maybe Statement)
               | Switch Expr Statement String [Maybe Integer]
               | Labelled String Statement
               | Case Expr Statement
               | Default Statement
               | Break String
               | Continue String
               | While Expr Statement String
               | DoWhile Statement Expr String
               | For ForInit (Maybe Expr) (Maybe Expr) Statement String
               | Null
    deriving (Show)
data BlockItem = S Statement | D Declaration
    deriving (Show)
newtype SProgram = SProgram [Declaration]
    deriving (Show)

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
    localSwitch :: [Maybe Integer],
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

resolve :: P.Program -> Either String SProgram
resolve prog = do
    let result = runState (runReaderT (resolveProg prog) localVars) initState
    case err (snd result) of
        Just e -> Left e
        Nothing -> Right (fst result)

gotoFunc :: Declaration -> SemanticMonad Declaration
gotoFunc (FuncDecl name params s (Just (Block items))) = FuncDecl name params s . Just . Block <$> mapM gotoItem items
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

resolveProg :: P.Program -> SemanticMonad SProgram
resolveProg (P.Program f) = SProgram <$> mapM resolveDecl f

resolveFunc :: P.Declaration -> SemanticMonad Declaration
resolveFunc (P.FuncDecl name params t blk) = do
    gm <- gets blockVars
    lm <- asks identifierMap
    level <- asks blockScope
    when (level && t == Just P.Static) $ writeError "Cannot have static function at block level"
    case M.lookup name gm of
        Just x -> if snd x then return () else writeError "Duplicate declaration"
        Nothing -> case M.lookup name lm of
                    Just _ -> return ()
                    Nothing -> return ()
    modify $ \x -> x { blockVars = M.insert name (name, True) gm }

    s <- gets (remap . blockVars)
    (params', blk') <- local s $ do
        modify $ \x -> x { blockVars = M.empty}
        mapM_ (resolveDecl . (\x -> P.VarDecl x Nothing Nothing)) params
        vars <- gets blockVars
        let params' = map (fst . fromJust . flip M.lookup vars) params
        blk' <- case blk of
                Just (P.Block bs) -> Just . Block <$> mapM resolveItem bs
                Nothing -> return Nothing
        oldVars <- asks identifierMap
        modify $ \x -> x { blockVars = oldVars }
        return (params', blk')
    g <- gotoFunc $ FuncDecl name params' t blk'
    modify $ \x -> x { labels = [] }
    return g
resolveFunc _ = error "Hmm"

descend :: (a -> SemanticMonad b) -> a -> SemanticMonad b
descend f arg = do
    s <- gets (remap . blockVars)
    local s $ do
        modify $ \x -> x { blockVars = M.empty}
        r <- f arg
        oldVars <- asks identifierMap
        modify $ \x -> x { blockVars = oldVars }
        return r

resolveBlock :: [P.BlockItem] -> SemanticMonad Block
resolveBlock items = Block <$> descend (mapM resolveItem) items

remap :: IdentMap -> LocalVars -> LocalVars
remap s r = r { identifierMap = M.union s (identifierMap r), blockScope = True }

resolveItem :: P.BlockItem -> SemanticMonad BlockItem
resolveItem (P.S stmt) = S <$> resolveStmt stmt
resolveItem (P.D decl) = D <$> resolveDecl decl

resolveForInit :: P.ForInit -> SemanticMonad ForInit
resolveForInit (P.InitExpr e) = InitExpr <$> traverse resolveExpr e
resolveForInit (P.InitDecl d) = do
    d' <- resolveDecl d
    case d' of
        FuncDecl {} -> writeError "Cannot declare function in for init"
        VarDecl _ (Just _) _ -> writeError "Cannot have storage ident in for init"
        _ -> return $ InitDecl d'

resolveFor :: P.Statement -> SemanticMonad Statement
resolveFor (P.For initial cond post body name) = do
    i <- resolveForInit initial
    c <- traverse resolveExpr cond 
    p <- traverse resolveExpr post 
    b <- resolveStmt body
    return $ For i c p b name
resolveFor _ = error "Shouldn't happen"

newLoopLabel :: String -> LocalVars -> LocalVars
newLoopLabel new l = l { breakLabel = Just new, continueLabel = Just new }

newSwitchLabel :: String -> [Maybe Integer] -> LocalVars -> LocalVars
newSwitchLabel new lbls l = l { breakLabel = Just new, switchLabel = Just new, localSwitch = lbls}

evalConstant :: Expr -> SemanticMonad Integer
evalConstant (Int i) = return i
evalConstant _ = writeError "Cannot parse complicated expressions yet"

resolveStmt :: P.Statement -> SemanticMonad Statement
resolveStmt (P.Return e) = Return <$> resolveExpr e
resolveStmt (P.Expression e) = Expression <$> resolveExpr e
resolveStmt (P.If e1 e2 e3) = do
    r1 <- resolveExpr e1
    r2 <- resolveStmt e2
    r3 <- traverse resolveStmt e3
    return $ If r1 r2 r3
resolveStmt (P.While e s _) = do
    label <- uniqueLabel
    local (newLoopLabel label) $ do
        e1 <- resolveExpr e
        s1 <- resolveStmt s
        return $ While e1 s1 label
resolveStmt (P.DoWhile s e _) = do
    label <- uniqueLabel
    local (newLoopLabel label) $ do
        e1 <- resolveExpr e
        s1 <- resolveStmt s
        return $ DoWhile s1 e1 label
resolveStmt (P.Case e s) = do
    l <- asks switchLabel
    e1 <- resolveExpr e
    s1 <- resolveStmt s
    case l of
        Just l' -> do
            n <- evalConstant e1
            lbls <- gets switchLabels
            if Just n `elem` lbls then writeError "Duplicate case!"
            else do
                modify $ \x -> x { switchLabels = Just n : switchLabels x}
                return $ Labelled (l' ++ "." ++ show n) s1
        Nothing   -> writeError "Not in switch!"
resolveStmt (P.Default s) = do
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
resolveStmt (P.For i c p b _) = do
    label <- uniqueLabel
    local (newLoopLabel label) $ descend resolveFor (P.For i c p b label)
resolveStmt (P.Switch e s _ _) = do
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
resolveStmt (P.Break _) = do
    l <- asks breakLabel
    case l of
        Just name -> return $ Break name
        Nothing   -> writeError "No label!"
resolveStmt (P.Continue _) = do
    l <- asks continueLabel
    case l of
        Just name -> return $ Continue name
        Nothing   -> writeError "No label!"
resolveStmt (P.Goto label) = return $ Goto label
resolveStmt (P.Labelled name stmt) = do
    s1 <- resolveStmt stmt
    m <- gets labels
    unique <- uniqueLabel
    if isJust (lookup name m) then writeError "Duplicate label declaration!"
    else modify $ \x -> x {labels = (name, unique) : labels x}
    return $ Labelled unique s1
resolveStmt (P.Compound (P.Block items)) = Compound <$> resolveBlock items
resolveStmt P.Null = return Null

writeError :: String -> SemanticMonad a
writeError s = do
    modify (\x -> x { err = Just s}) *> error s

resolveDecl :: P.Declaration -> SemanticMonad Declaration
resolveDecl (P.VarDecl name s initial) = do
    m <- gets blockVars
    inBlock <- asks blockScope
    uniq <- if not inBlock
        then do
            modify $ \x -> x {blockVars = M.insert name (name, True) $ blockVars x}
            return name
        else do
            let scopeVar = M.lookup name m
            case scopeVar of
                Just (v, b) -> if not (b && s == Just P.Extern)
                               then writeError "Duplicate variable declaration!"
                               else modify id >> return v
                Nothing -> do
                    unique <- if s == Just P.Extern then return name else uniqueVar
                    modify $ \x -> x {blockVars = M.insert name (unique, s == Just P.Extern) $ blockVars x}
                    return unique
    case initial of
        Just x  -> VarDecl uniq s . Just <$> resolveExpr x
        Nothing -> return $ VarDecl uniq s Nothing
resolveDecl fun = resolveFunc fun

resolveExpr :: P.Expr -> SemanticMonad Expr
resolveExpr (P.Assignment (P.Var s) r) = do
    Assignment <$> resolveExpr (P.Var s) <*> resolveExpr r
resolveExpr (P.Assignment _ _) = writeError "Invalid lvalue!"
resolveExpr (P.CompoundAssignment op (P.Var s) r) = do
    CompoundAssignment op <$> resolveExpr (P.Var s) <*> resolveExpr r
resolveExpr (P.CompoundAssignment {}) = writeError "Invalid lvalue!"
resolveExpr (P.Var v) = do
    gm <- gets blockVars
    lm <- asks identifierMap
    case M.lookup v gm of
        Just x -> return $ Var (fst x)
        Nothing -> case M.lookup v lm of
                    Just x -> return $ Var (fst x)
                    Nothing -> writeError $ "Undeclared variable: " ++ show v
resolveExpr (P.Unary PreDec (P.Var s)) = Unary PreDec <$> resolveExpr (P.Var s)
resolveExpr (P.Unary PreInc (P.Var s)) = Unary PreInc <$> resolveExpr (P.Var s)
resolveExpr (P.Unary PreDec _) = writeError "Invalid lvalue!"
resolveExpr (P.Unary PreInc _) = writeError "Invalid lvalue!"
resolveExpr (P.Unary PostDec (P.Var s)) = Unary PostDec <$> resolveExpr (P.Var s)
resolveExpr (P.Unary PostInc (P.Var s)) = Unary PostInc <$> resolveExpr (P.Var s)
resolveExpr (P.Unary PostDec _) = writeError "Invalid lvalue!"
resolveExpr (P.Unary PostInc _) = writeError "Invalid lvalue!"
resolveExpr (P.Unary op e) = Unary op <$> resolveExpr e
resolveExpr (P.Binary op e1 e2) = Binary op <$> resolveExpr e1 <*> resolveExpr e2
resolveExpr (P.Int i) = return (Int i)
resolveExpr (P.Conditional e1 e2 e3) = Conditional <$> resolveExpr e1 <*> resolveExpr e2 <*> resolveExpr e3
resolveExpr (P.FunctionCall name args) = do
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