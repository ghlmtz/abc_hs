module TypeCheck
(
  resolveType
, IdentAttr(..)
, InitValue(..)
, Program
) where

import qualified Parse as P
import Parse(PType(..), Const(..), UnaryOp(..), BinaryOp(..), Storage(..))

import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)

data IdentAttr = FunAttr { fDef :: Bool, fGlobal :: Bool }
    | StaticAttr InitValue Bool | LocalAttr
    deriving (Eq, Show)
data InitValue = Tentative | Initial Integer | NoInitializer
    deriving (Eq, Show)
data TypeState = TypeState {
  symbols :: M.Map String (PType, IdentAttr)
, err :: Maybe String
, blockScope :: Bool
, funType :: Maybe PType
}
data TypedExpr = TypedExpr Expr PType
    deriving (Show)
data Expr = Constant Const
          | Unary UnaryOp TypedExpr
          | Binary BinaryOp TypedExpr TypedExpr
          | Var String
          | Cast PType TypedExpr
          | Assignment TypedExpr TypedExpr
          | CompoundAssignment BinaryOp TypedExpr TypedExpr
          | Conditional TypedExpr TypedExpr TypedExpr
          | FunctionCall String [Expr]
    deriving (Show)
data Declaration = FuncDecl { fName :: String
                            , fArgNames :: [String]
                            , fStorage :: Maybe Storage
                            , fType :: PType
                            , fBlock :: Maybe Block }
                 | VarDecl { vName :: String
                           , vStorage :: Maybe Storage
                           , vType :: PType
                           , vInit :: Maybe Expr }
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
newtype Program = Program [Declaration]
    deriving (Show)

type TypeMonad m = State TypeState m

writeError :: String -> TypeMonad a
writeError s = modify (\x -> x { err = Just s}) *> error s

resolveType :: P.Program -> Either String (Program, M.Map String (PType, IdentAttr))
resolveType prog = do
    let result = runState (typeProg prog) (TypeState M.empty Nothing False Nothing)
    case err (snd result) of
        Just e -> Left e
        Nothing -> Right (fst result, symbols (snd result))

typeProg :: P.Program -> TypeMonad Program
typeProg (P.Program f) = Program <$> mapM typeDecl f

evalConstant :: P.Expr -> Maybe Integer
evalConstant (P.Constant (ConstInt i)) = Just i
evalConstant (P.Constant (ConstLong i)) = Just i
evalConstant _ = Nothing

typeDecl :: P.Declaration -> TypeMonad Declaration
typeDecl (P.FuncDecl name params s t@(TFun pTypes ret) (Just (P.Block items))) = do
    scope <- gets blockScope
    when scope $ writeError "Nested function definition"
    syms <- gets symbols
    case M.lookup name syms of
        Just (TFun args oldRet, FunAttr False _) -> do
            when (oldRet /= ret) $ writeError "Function return type mismatch"
            when (length args /= length pTypes) $ writeError "Incompatible function declaration"
            when (args /= pTypes) $ writeError "Function argument type mismatch"
        Just (_, FunAttr True _) -> writeError "Function is defined more than once"
        Just _ -> writeError "Incompatible function declaration"
        Nothing -> return ()
    let syms' = foldl (\m k -> M.insert (snd k) (fst k, LocalAttr) m) syms (zip pTypes params)
    g <- funcGlobal name s
    modify $ \x -> x { symbols = M.insert name (t, FunAttr True g) syms' }
    modify $ \x -> x { blockScope = True, funType = Just ret }
    items' <- mapM typeItem items
    modify $ \x -> x { blockScope = False, funType = Nothing }
    return . FuncDecl name params s t . Just . Block $ items'
typeDecl (P.FuncDecl name params s t@(TFun pTypes ret) Nothing) = do
    syms <- gets symbols
    def <- case M.lookup name syms of
        Just (TFun a oldRet, FunAttr def _) -> do
          when (oldRet /= ret) $ writeError "Function return type mismatch"
          when (length a /= length pTypes) $ writeError "Incompatible function declaration"
          when (a /= pTypes) $ writeError "Function argument type mismatch"
          return def

        Just _ -> writeError "Incompatible function declaration"
        Nothing -> return False
    g <- funcGlobal name s
    let syms' = foldl (\m k -> M.insert (snd k) (fst k, LocalAttr) m) syms (zip pTypes params)
    modify $ \x -> x { symbols = M.insert name (t, FunAttr def g) syms' }
    return $ FuncDecl name params s t Nothing
typeDecl (P.FuncDecl {}) = writeError "Var in fun decl"
typeDecl var@(P.VarDecl {}) = do
    scope <- gets blockScope
    if scope then blockVar var else fileVar var

funcGlobal :: String -> Maybe Storage -> TypeMonad Bool
funcGlobal name s = do
    syms <- gets symbols
    let g = s /= Just Static
    case M.lookup name syms of
        Just (_, at) -> if s == Just Static && attrGlobal at
            then writeError "Static function declaration follows non-static"
            else return (attrGlobal at)
        Nothing -> return g

fileVar :: P.Declaration -> TypeMonad Declaration
fileVar (P.VarDecl name s typ initial) = do
    let blankVal = if s == Just Extern then NoInitializer else Tentative
        value = maybe (Just blankVal) (fmap Initial . evalConstant) initial
    when (isNothing value) $ writeError "Cannot assign non-constant expr to file var"
    syms <- gets symbols
    newG <- getGlobal name s
    newV <- case M.lookup name syms of
              Just (oldType, at) -> do
                when (oldType /= typ) $ writeError "Type mismatch"
                case getConstant at of
                    Just v -> case value of
                        Just (Initial _) -> writeError "Conflicting file scope variable definitions"
                        _ -> return v
                    Nothing -> if not (isInitial (fromJust value)) && isTentative at
                        then return Tentative
                        else return $ fromJust value
              Nothing -> return $ fromJust value
    modify $ \x -> x { symbols = M.insert name (typ, StaticAttr newV newG) syms}
    VarDecl name s typ <$> ugly initial
fileVar _ = error "Unreachable"

isInitial :: InitValue -> Bool
isInitial (Initial _) = True
isInitial _ = False

isTentative :: IdentAttr -> Bool
isTentative (StaticAttr Tentative _) = True
isTentative _ = False

getConstant :: IdentAttr -> Maybe InitValue
getConstant (StaticAttr v@(Initial _) _) = Just v
getConstant _ = Nothing

attrGlobal :: IdentAttr -> Bool
attrGlobal (StaticAttr _ a) = a
attrGlobal (FunAttr _ a) = a
attrGlobal LocalAttr = error "This shouldn't happen"

getGlobal :: String -> Maybe Storage -> TypeMonad Bool
getGlobal name s = do
    let g = s /= Just Static
    syms <- gets symbols
    case M.lookup name syms of
        Just (t, at) -> do
            when (t /= TInt && t /= TLong) $ writeError "Function redeclared as variable"
            if s == Just Extern
                then return (attrGlobal at)
                else if attrGlobal at /= g
                    then writeError "Conflicting variable linkage"
                    else return g
        Nothing -> return g

blockVar :: P.Declaration -> TypeMonad Declaration
blockVar (P.VarDecl _ (Just Extern) _ (Just _)) = writeError "Cannot initialize extern var decl"
blockVar (P.VarDecl name (Just Extern) typ Nothing) = do
    syms <- gets symbols
    case M.lookup name syms of
        Just (TFun {}, _) -> writeError "Function redeclared as variable"
        Just (oldType, _) -> do
            when (oldType /= typ) $ writeError "Type mismatch"
            return $ VarDecl name (Just Extern) typ Nothing
        _ -> do modify $ \x -> x { symbols = M.insert name (typ, StaticAttr NoInitializer True) syms}
                return $ VarDecl name (Just Extern) typ Nothing
blockVar (P.VarDecl name (Just Static) t initial) = do
    let value = maybe (Just 0) evalConstant initial
    when (isNothing value) $ writeError "Cannot assign non-constant expr to static var"
    modify $ \x -> x { symbols = M.insert name (t, StaticAttr (Initial (fromIntegral (fromJust value))) True) $ symbols x}
    VarDecl name (Just Static) t <$> ugly initial
blockVar (P.VarDecl name s t initial) = do
    modify $ \x -> x {symbols = M.insert name (t, LocalAttr) $ symbols x}
    case initial of
        Just x -> do
            e <- typeExpr x
            return $ VarDecl name s t (Just (fst e))
        Nothing -> return $ VarDecl name s t Nothing
blockVar _ = error "Unreachable"

typeItem :: P.BlockItem -> TypeMonad BlockItem
typeItem (P.S stmt) = S <$> typeStmt stmt
typeItem (P.D decl) = D <$> typeDecl decl

typeStmt :: P.Statement -> TypeMonad Statement
typeStmt (P.Labelled name stmt) = Labelled name <$> typeStmt stmt
typeStmt (P.Compound (P.Block items)) = Compound . Block <$> mapM typeItem items
typeStmt (P.If e1 s1 s2) = do
    e1' <- typeExpr e1
    s1' <- typeStmt s1
    s2' <- traverse typeStmt s2
    return $ If (fst e1') s1' s2'
typeStmt (P.Switch e s n c) = do
    e' <- typeExpr e
    s' <- typeStmt s
    return $ Switch (fst e') s' n c
typeStmt (P.Case e s) = Case . fst <$> typeExpr e <*> typeStmt s
typeStmt (P.Default s) = Default <$> typeStmt s
typeStmt (P.DoWhile s e n) = do
    s' <- typeStmt s
    e' <- typeExpr e
    return $ DoWhile s' (fst e') n
typeStmt (P.While e s n) = do
    s' <- typeStmt s
    e' <- typeExpr e
    return $ While (fst e') s' n
typeStmt (P.For (P.InitDecl v@(P.VarDecl {})) e1 e2 s n) = do
    v' <- blockVar v
    s' <- typeStmt s
    e1' <- ugly e1
    e2' <- ugly e2
    return $ For (InitDecl v') e1' e2' s' n
typeStmt (P.For (P.InitExpr e) e1 e2 s n) = do
    s' <- typeStmt s
    e' <- ugly e
    e1' <- ugly e1
    e2' <- ugly e2
    return $ For (InitExpr e') e1' e2' s' n
typeStmt (P.For {}) = writeError "Function declaration in for statement"
typeStmt (P.Return e) = Return . fst <$> typeExpr e
typeStmt (P.Expression e) = Expression . fst <$> typeExpr e
typeStmt P.Null = return Null
typeStmt (P.Goto s) = return $ Goto s
typeStmt (P.Continue s) = return $ Continue s
typeStmt (P.Break s) = return $ Break s

ugly :: Maybe P.Expr -> TypeMonad (Maybe Expr)
ugly Nothing = return Nothing
ugly (Just e) = fmap (Just . fst) (typeExpr e)

typeExpr :: P.Expr -> TypeMonad (Expr, PType)
typeExpr (P.FunctionCall name args) = do
    t <- gets (M.lookup name . symbols)
    args' <- case t of
            Just (TInt, _) -> writeError "Variable used as function name"
            Just (TFun r _, _) -> if length r /= length args then writeError "Wrong number of args"
                else mapM typeExpr args
            _ -> writeError "This shouldn't happen"
    return (FunctionCall name (map fst args'), TInt)
typeExpr (P.Var v) = do
    t <- gets (M.lookup v . symbols)
    case t of
        Just (TInt, _) -> return (Var v, TInt)
        Just (TLong, _) -> return (Var v, TLong)
        Nothing -> writeError $ "This probably shouldn't happen" ++ show v
        _ -> writeError "Function name used as variable"
typeExpr (P.CompoundAssignment op e r) = do
    left <- typeExpr e
    right <- typeExpr r
    return (CompoundAssignment op (uncurry TypedExpr left) (uncurry TypedExpr right), TLong)
typeExpr (P.Assignment e r) = do
    left <- typeExpr e
    right <- typeExpr r
    return (Assignment (uncurry TypedExpr left) (uncurry TypedExpr right), TLong)
typeExpr (P.Unary op e) = do
    e' <- typeExpr e
    return (Unary op (uncurry TypedExpr e'), TLong)
typeExpr (P.Binary op e1 e2) = do
    left <- typeExpr e1
    right <- typeExpr e2
    return (Binary op (uncurry TypedExpr left) (uncurry TypedExpr right), TLong)
typeExpr (P.Conditional e1 e2 e3) = do
    t1 <- typeExpr e1
    t2 <- typeExpr e2
    t3 <- typeExpr e3
    return (Conditional (uncurry TypedExpr t1) (uncurry TypedExpr t2) (uncurry TypedExpr t3), TLong)
typeExpr (P.Constant e@(ConstInt _)) = return  (Constant e, TInt)
typeExpr (P.Constant e@(ConstLong _)) = return (Constant e, TLong)
typeExpr (P.Cast ty e) = do
    t1 <- typeExpr e
    return (Cast ty (uncurry TypedExpr t1), ty)
