module TypeCheck
(
  resolveType
) where

import Parse

import Control.Monad.State
import qualified Data.Map as M

data Type = TInt | TFunc Int Bool

data TypeState = TypeState {
  symbols :: M.Map String Type
, err :: Maybe String
}

type TypeMonad m = State TypeState m

writeError :: String -> TypeMonad a
writeError s = do
    modify (\x -> x { err = Just s}) *> error s

resolveType :: Program -> Either String Program
resolveType prog = do
    let result = runState (typeProg prog) (TypeState M.empty Nothing)
    case err (snd result) of
        Just e -> Left e
        Nothing -> Right (fst result)

typeProg :: Program -> TypeMonad Program
typeProg (Program f) = Program <$> mapM typeFunc f

typeFunc :: Function -> TypeMonad Function
typeFunc (Function name params (Just (Block items))) = do
    syms <- gets symbols
    case M.lookup name syms of
        Just (TFunc args False) -> 
            when (args /= length params) $ writeError "Incompatible function declaration"
        Just (TFunc _ True) -> writeError "Function is defined more than once"
        Just _ -> writeError "Incompatible function declaration"
        Nothing -> return ()
    let syms' = foldl(\m k -> M.insert k TInt m) syms params
    modify $ \x -> x { symbols = M.insert name (TFunc (length params) True) syms' }
    Function name params . Just . Block <$> mapM typeItem items
typeFunc nothing@(Function name params Nothing) = do
    syms <- gets symbols
    def <- case M.lookup name syms of
        Just (TFunc args def) -> 
          if args /= length params then writeError "Incompatible function declaration"
          else return def
            
        Just _ -> writeError "Incompatible function declaration"
        Nothing -> return False
    let syms' = foldl(\m k -> M.insert k TInt m) syms params
    modify $ \x -> x { symbols = M.insert name (TFunc (length params) def) syms' }
    return nothing

typeDecl :: Declaration -> TypeMonad Declaration
typeDecl (VarDecl (name, i)) = do
    modify $ \x -> x {symbols = M.insert name TInt $ symbols x}
    case i of
        Just x -> do
            e <- typeExpr x
            return $ VarDecl (name, Just e)
        Nothing -> return $ VarDecl (name, i)
typeDecl (FuncDecl fun) = FuncDecl <$> typeFunc fun

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
            Just TInt -> writeError "Variable used as function name"
            Just (TFunc r _) -> if r /= length args then writeError "Wrong number of args"
                else mapM typeExpr args
            _ -> writeError "This shouldn't happen"
    return $ FunctionCall name args'
typeExpr (Var v) = do
    t <- gets (M.lookup v. symbols)
    case t of
        Just TInt -> return $ Var v
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