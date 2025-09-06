module TypeCheck
(
  resolveType
, IdentAttr(..)
, InitValue(..)
) where

import Parse

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
--data TypedExpr = TypedExpr PType Expr

type TypeMonad m = State TypeState m

writeError :: String -> TypeMonad a
writeError s = modify (\x -> x { err = Just s}) *> error s

resolveType :: Program -> Either String (Program, M.Map String (PType, IdentAttr))
resolveType prog = do
    let result = runState (typeProg prog) (TypeState M.empty Nothing False Nothing)
    case err (snd result) of
        Just e -> Left e
        Nothing -> Right (fst result, symbols (snd result))

typeProg :: Program -> TypeMonad Program
typeProg (Program f) = Program <$> mapM typeDecl f

evalConstant :: Expr -> Maybe Integer
evalConstant (Constant (ConstInt i)) = Just i
evalConstant (Constant (ConstLong i)) = Just i
evalConstant _ = Nothing

typeDecl :: Declaration -> TypeMonad Declaration
typeDecl (FuncDecl name params s t@(TFun _ ret) (Just (Block items))) = do
    scope <- gets blockScope
    when scope $ writeError "Nested function definition"
    syms <- gets symbols
    case M.lookup name syms of
        Just (TFun args _, FunAttr False _) -> 
            when (length args /= length params) $ writeError "Incompatible function declaration"
        Just (_, FunAttr True _) -> writeError "Function is defined more than once"
        Just _ -> writeError "Incompatible function declaration"
        Nothing -> return ()
    let syms' = foldl(\m k -> M.insert k (TInt, LocalAttr) m) syms params
    g <- funcGlobal name s
    modify $ \x -> x { symbols = M.insert name (t, FunAttr True g) syms' }
    modify $ \x -> x { blockScope = True, funType = Just ret }
    items' <- mapM typeItem items
    modify $ \x -> x { blockScope = False, funType = Nothing }
    return . FuncDecl name params s t . Just . Block $ items'
typeDecl nothing@(FuncDecl name params s (TFun args t) Nothing) = do
    syms <- gets symbols
    def <- case M.lookup name syms of
        Just (TFun a _, FunAttr def _) -> 
          if length a /= length params then writeError "Incompatible function declaration"
          else return def
            
        Just _ -> writeError "Incompatible function declaration"
        Nothing -> return False
    g <- funcGlobal name s
    let syms' = foldl(\m k -> M.insert k (TInt, LocalAttr) m) syms params
    modify $ \x -> x { symbols = M.insert name (TFun args t, FunAttr def g) syms' }
    return nothing
typeDecl (FuncDecl {}) = writeError "Var in fun decl"
typeDecl var@(VarDecl {}) = do
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

fileVar :: Declaration -> TypeMonad Declaration
fileVar var@(VarDecl name s _ initial) = do
    let blankVal = if s == Just Extern then NoInitializer else Tentative
        value = maybe (Just blankVal) (fmap Initial . evalConstant) initial
    when (isNothing value) $ writeError "Cannot assign non-constant expr to file var"
    syms <- gets symbols
    newG <- getGlobal name s
    newV <- case M.lookup name syms of
              Just (_, at) -> do
                case getConstant at of
                    Just v -> case value of 
                        Just (Initial _) -> writeError "Conflicting file scope variable definitions"
                        _ -> return v
                    Nothing -> if not (isInitial (fromJust value)) && isTentative at
                        then return Tentative
                        else return $ fromJust value
              Nothing -> return $ fromJust value
    modify $ \x -> x { symbols = M.insert name (TInt, StaticAttr newV newG) syms}
    return var
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
            when (t /= TInt) $ writeError "Function redeclared as variable"
            if s == Just Extern
                then return (attrGlobal at)
                else if attrGlobal at /= g
                    then writeError "Conflicting variable linkage"
                    else return g
        Nothing -> return g

blockVar :: Declaration -> TypeMonad Declaration
blockVar (VarDecl _ (Just Extern) _ (Just _)) = writeError "Cannot initialize extern var decl"
blockVar var@(VarDecl name (Just Extern) t Nothing) = do
    syms <- gets symbols
    case M.lookup name syms of
        Just (TFun {}, _) -> writeError "Function redeclared as variable"
        Just _ -> return var
        _ -> do modify $ \x -> x { symbols = M.insert name (t, StaticAttr NoInitializer True) syms}
                return var
blockVar var@(VarDecl name (Just Static) t initial) = do
    let value = maybe (Just 0) evalConstant initial
    when (isNothing value) $ writeError "Cannot assign non-constant expr to static var"
    modify $ \x -> x { symbols = M.insert name (t, StaticAttr (Initial (fromIntegral (fromJust value))) True) $ symbols x}
    return var
blockVar (VarDecl name s t initial) = do
    modify $ \x -> x {symbols = M.insert name (t, LocalAttr) $ symbols x}
    case initial of
        Just x -> do
            e <- typeExpr x
            return $ VarDecl name s t (Just (fst e))
        Nothing -> return $ VarDecl name s t Nothing
blockVar _ = error "Unreachable"

typeItem :: BlockItem -> TypeMonad BlockItem
typeItem (S stmt) = S <$> typeStmt stmt
typeItem (D decl) = D <$> typeDecl decl

typeStmt :: Statement -> TypeMonad Statement
typeStmt (Labelled name stmt) = Labelled name <$> typeStmt stmt
typeStmt (Compound (Block items)) = Compound . Block <$> mapM typeItem items
typeStmt (If e1 s1 s2) = do
    e1' <- typeExpr e1
    s1' <- typeStmt s1
    s2' <- traverse typeStmt s2
    return $ If (fst e1') s1' s2'
typeStmt (Switch e s n c) = do
    e' <- typeExpr e
    s' <- typeStmt s
    return $ Switch (fst e') s' n c
typeStmt (Case e s) = Case . fst <$> typeExpr e <*> typeStmt s
typeStmt (Default s) = Default <$> typeStmt s
typeStmt (DoWhile s e n) = do
    s' <- typeStmt s
    e' <- typeExpr e
    return $ DoWhile s' (fst e') n
typeStmt (While e s n) = do
    s' <- typeStmt s
    e' <- typeExpr e
    return $ While (fst e') s' n
typeStmt (For (InitDecl v@(VarDecl {})) e1 e2 s n) = do
    v' <- blockVar v
    s' <- typeStmt s
    return $ For (InitDecl v') e1 e2 s' n
typeStmt (For i e1 e2 s n) = do
    s' <- typeStmt s
    return $ For i e1 e2 s' n
typeStmt (Return e) = Return . fst <$> typeExpr e
typeStmt (Expression e) = Expression . fst <$> typeExpr e
typeStmt s = return s

typeExpr :: Expr -> TypeMonad (Expr, PType)
typeExpr (FunctionCall name args) = do
    t <- gets (M.lookup name . symbols)
    args' <- case t of 
            Just (TInt, _) -> writeError "Variable used as function name"
            Just (TFun r _, _) -> if length r /= length args then writeError "Wrong number of args"
                else mapM typeExpr args
            _ -> writeError "This shouldn't happen"
    return (FunctionCall name (map fst args'), TInt)
typeExpr (Var v) = do
    t <- gets (M.lookup v . symbols)
    case t of
        Just (TInt, _) -> return (Var v, TInt)
        Just (TLong, _) -> return (Var v, TLong)
        Nothing -> writeError $ "This probably shouldn't happen" ++ show v
        _ -> writeError "Function name used as variable"
typeExpr (CompoundAssignment op e r) = do
    left <- typeExpr e
    right <- typeExpr r
    return (CompoundAssignment op (fst left) (fst right), TLong)
typeExpr (Assignment e r) = do
    left <- typeExpr e
    right <- typeExpr r
    return (Assignment (fst left) (fst right), TLong)
typeExpr (Unary op e) = do
    e' <- typeExpr e
    return (Unary op (fst e'), TLong) 
typeExpr (Binary op e1 e2) = do
    left <- typeExpr e1
    right <- typeExpr e2
    return (Binary op (fst left) (fst right), TLong)
typeExpr (Conditional e1 e2 e3) = do
    t1 <- typeExpr e1 
    t2 <- typeExpr e2 
    t3 <- typeExpr e3
    return (Conditional (fst t1) (fst t2) (fst t3), TLong)
typeExpr e = return (e, TLong)
