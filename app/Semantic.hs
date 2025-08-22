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
}

resolve :: Program -> Either String Program
resolve prog = Right $ evalState (resolveProg prog) SemanticState {variableMap = [], nameCount = 0}

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

resolveDecl :: Declaration -> State SemanticState Declaration
resolveDecl (Declaration name i) = do
    m <- gets variableMap
    unique <- uniqueName
    if isJust (lookup name m) then error "Duplicate variable declaration!"
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
resolveExpr (Assignment _ _) = error "Invalid lvalue!"
resolveExpr (Var v) = do
    m <- gets variableMap
    return $ case lookup v m of
        Just x -> Var x
        Nothing -> error "Undeclared variable!"
resolveExpr (Unary op e) = Unary op <$> resolveExpr e
resolveExpr (Binary op e1 e2) = Binary op <$> resolveExpr e1 <*> resolveExpr e2
resolveExpr (Int i) = return (Int i)

uniqueName :: State SemanticState String
uniqueName = do
    ct <- gets (show . nameCount)
    modify $ \x -> x {nameCount = 1 + nameCount x}
    return $ "var." ++ ct