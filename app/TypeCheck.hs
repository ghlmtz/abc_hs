module TypeCheck
(
  resolveType
, Type(..)
, IdentAttr(..)
, InitValue(..)
) where

import Parse

import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)

data Type = TInt | TFunc Int Bool
    deriving (Eq, Show)
data IdentAttr = FunAttr Bool Bool | StaticAttr InitValue Bool | LocalAttr
    deriving (Eq, Show)
data InitValue = Tentative | Initial Integer | NoInitializer
    deriving (Eq, Show)
data TypeState = TypeState {
  symbols :: M.Map String (Type, IdentAttr)
, err :: Maybe String
, blockScope :: Bool
}

type TypeMonad m = State TypeState m

writeError :: String -> TypeMonad a
writeError s = modify (\x -> x { err = Just s}) *> error s

resolveType :: Program -> Either String (Program, M.Map String (Type, IdentAttr))
resolveType prog = do
    let result = runState (typeProg prog) (TypeState M.empty Nothing False)
    case err (snd result) of
        Just e -> Left e
        Nothing -> Right (fst result, symbols (snd result))

typeProg :: Program -> TypeMonad Program
typeProg (Program f) = Program <$> mapM typeDecl f

evalConstant :: Expr -> Maybe Integer
evalConstant (Int i) = Just i
evalConstant _ = Nothing

typeDecl :: Declaration -> TypeMonad Declaration
typeDecl (FuncDecl name params s (Just (Block items))) = do
    scope <- gets blockScope
    when scope $ writeError "Nested function definition"
    syms <- gets symbols
    case M.lookup name syms of

        Just (TFunc args False, _) -> 
            when (args /= length params) $ writeError "Incompatible function declaration"
        Just (TFunc _ True, _) -> writeError "Function is defined more than once"
        Just _ -> writeError "Incompatible function declaration"
        Nothing -> return ()
    let syms' = foldl(\m k -> M.insert k (TInt, LocalAttr) m) syms params
    g <- funcGlobal name s
    modify $ \x -> x { symbols = M.insert name (TFunc (length params) True, FunAttr True g) syms' }
    modify $ \x -> x { blockScope = True }
    items' <- mapM typeItem items
    modify $ \x -> x { blockScope = False }
    return . FuncDecl name params s . Just . Block $ items'
typeDecl nothing@(FuncDecl name params s Nothing) = do
    syms <- gets symbols
    def <- case M.lookup name syms of
        Just (TFunc args def, _) -> 
          if args /= length params then writeError "Incompatible function declaration"
          else return def
            
        Just _ -> writeError "Incompatible function declaration"
        Nothing -> return False
    g <- funcGlobal name s
    let syms' = foldl(\m k -> M.insert k (TInt, LocalAttr) m) syms params
    modify $ \x -> x { symbols = M.insert name (TFunc (length params) def, FunAttr def g) syms' }
    return nothing
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
fileVar var@(VarDecl name s initial) = do
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
blockVar (VarDecl _ (Just Extern) (Just _)) = writeError "Cannot initialize extern var decl"
blockVar var@(VarDecl name (Just Extern) Nothing) = do
    syms <- gets symbols
    case M.lookup name syms of
        Just (TFunc _ _, _) -> writeError "Function redeclared as variable"
        Just _ -> return var
        _ -> do modify $ \x -> x { symbols = M.insert name (TInt, StaticAttr NoInitializer True) syms}
                return var
blockVar var@(VarDecl name (Just Static) initial) = do
    let value = maybe (Just 0) evalConstant initial
    when (isNothing value) $ writeError "Cannot assign non-constant expr to static var"
    modify $ \x -> x { symbols = M.insert name (TInt, StaticAttr (Initial (fromIntegral (fromJust value))) True) $ symbols x}
    return var
blockVar (VarDecl name s initial) = do
    modify $ \x -> x {symbols = M.insert name (TInt, LocalAttr) $ symbols x}
    case initial of
        Just x -> do
            e <- typeExpr x
            return $ VarDecl name s (Just e)
        Nothing -> return $ VarDecl name s Nothing
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
    s2' <- resolveOpt s2 typeStmt
    return $ If e1' s1' s2'
typeStmt (Switch e s n c) = do
    e' <- typeExpr e
    s' <- typeStmt s
    return $ Switch e' s' n c
typeStmt (Case e s) = Case <$> typeExpr e <*> typeStmt s
typeStmt (Default s) = Default <$> typeStmt s
typeStmt (DoWhile s e n) = do
    s' <- typeStmt s
    e' <- typeExpr e
    return $ DoWhile s' e' n
typeStmt (While e s n) = do
    s' <- typeStmt s
    e' <- typeExpr e
    return $ While e' s' n
typeStmt (For i e1 e2 s n) = do
    s' <- typeStmt s
    return $ For i e1 e2 s' n
typeStmt (Return e) = Return <$> typeExpr e
typeStmt (Expression e) = Expression <$> typeExpr e
typeStmt s = return s

typeExpr :: Expr -> TypeMonad Expr
typeExpr (FunctionCall name args) = do
    t <- gets (M.lookup name . symbols)
    args' <- case t of 
            Just (TInt, _) -> writeError "Variable used as function name"
            Just (TFunc r _, _) -> if r /= length args then writeError "Wrong number of args"
                else mapM typeExpr args
            _ -> writeError "This shouldn't happen"
    return $ FunctionCall name args'
typeExpr (Var v) = do
    t <- gets (M.lookup v. symbols)
    case t of
        Just (TInt, _) -> return $ Var v
        Nothing -> return $ Var v
        _ -> writeError "Function name used as variable"
typeExpr (CompoundAssignment op e r) = do
    CompoundAssignment op <$> typeExpr e <*> typeExpr r
typeExpr (Assignment e r) = do
    Assignment <$> typeExpr e <*> typeExpr r
typeExpr (Unary op e) = Unary op <$> typeExpr e
typeExpr (Binary op e1 e2) = Binary op <$> typeExpr e1 <*> typeExpr e2
typeExpr (Conditional e1 e2 e3) = Conditional <$> typeExpr e1 <*> typeExpr e2 <*> typeExpr e3
typeExpr e = return e

resolveOpt :: Monad f => Maybe a -> (a -> f a) -> f (Maybe a)
resolveOpt (Just e) f = Just <$> f e
resolveOpt Nothing _ = return Nothing