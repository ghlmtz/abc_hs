module Semantic
(
    resolve
) where

import Parse

import Control.Monad.State
import Data.Maybe (isJust)

data SemanticState = SemanticState {
  variableMap :: [(String, String)]
, nameCount  :: Int
, err :: Maybe String
}

resolve :: Program -> Either String Program
resolve prog = do
    let (prog', s) = runState (resolveProg prog) SemanticState {variableMap = [], nameCount = 0, err = Nothing}
    case err s of
        Just e -> Left e
        Nothing -> Right prog'

resolveProg :: Program -> State SemanticState Program
resolveProg (Program f) = Program <$> resolveFunc f

resolveFunc :: Function -> State SemanticState Function
resolveFunc (Function name items) = Function name <$> mapM resolveItem items

resolveItem :: BlockItem -> State SemanticState BlockItem
resolveItem (S stmt) = S <$> resolveStmt stmt
resolveItem (D decl) = D <$> resolveDecl decl

resolveStmt :: Statement -> State SemanticState Statement
resolveStmt (Return e) = Return <$> resolveExpr e
resolveStmt (Expression e) = Expression <$> resolveExpr e
resolveStmt Null = return Null

writeError :: String -> State SemanticState a
writeError s = do
    modify (\x -> x { err = Just s}) *> error s

resolveDecl :: Declaration -> State SemanticState Declaration
resolveDecl (Declaration name i) = do
    m <- gets variableMap
    unique <- uniqueName
    if isJust (lookup name m) then writeError "Duplicate variable declaration!"
    else
        modify $ \x -> x {variableMap = (name, unique) : variableMap x}
    case i of
        Just x -> do
            e <- resolveExpr x
            return $ Declaration unique (Just e)
        Nothing -> return $ Declaration unique i

resolveExpr :: Expr -> State SemanticState Expr
resolveExpr (Assignment (Var s) r) = do
    Assignment <$> resolveExpr (Var s) <*> resolveExpr r
resolveExpr (Assignment _ _) = writeError "Invalid lvalue!"
resolveExpr (Var v) = do
    m <- gets variableMap
    case lookup v m of
        Just x -> return $ Var x
        Nothing -> writeError "Undeclared variable!"
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

uniqueName :: State SemanticState String
uniqueName = do
    ct <- gets (show . nameCount)
    modify $ \x -> x {nameCount = 1 + nameCount x}
    return $ "var." ++ ct