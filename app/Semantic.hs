module Semantic
(
    resolve
) where

import Parse

import Control.Monad.State
import Data.Maybe (isJust)

data SemanticState = SemanticState {
  variableMap :: [(String, String)]
, labels :: [(String, String)]
, nameCount  :: Int
, err :: Maybe String
}

initState :: SemanticState
initState = SemanticState {
      variableMap = []
    , labels = []
    , nameCount = 0
    , err = Nothing}

resolve :: Program -> Either String Program
resolve prog = do
    let result = runState (resolveProg prog) initState 
    case err (snd result) of
        Just e -> Left e
        Nothing -> resolveGoto result

resolveGoto :: (Program, SemanticState) -> Either String Program
resolveGoto (prog, s) = do
    let (prog', s') = runState (gotoProg prog) s 
    case err s' of
        Just e -> Left e
        Nothing -> Right prog'

gotoProg :: Program -> State SemanticState Program
gotoProg (Program f) = Program <$> gotoFunc f

gotoFunc :: Function -> State SemanticState Function
gotoFunc (Function name items) = Function name <$> mapM gotoItem items

gotoItem :: BlockItem -> State SemanticState BlockItem
gotoItem (S stmt) = S <$> gotoStmt stmt
gotoItem (D decl) = return $ D decl

gotoStmt :: Statement -> State SemanticState Statement
gotoStmt (Labelled name stmt) = Labelled name <$> gotoStmt stmt
gotoStmt (Goto name) = do
    m <- gets labels
    case lookup name m of
        Just x -> return $ Goto x
        Nothing -> writeError "Goto missing label!"
gotoStmt (If e1 s1 s2) = do
    s1' <- gotoStmt s1
    s2' <- case s2 of
            Just e -> Just <$> gotoStmt e
            Nothing -> return Nothing
    return $ If e1 s1' s2'
gotoStmt s = return s

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
resolveStmt (If e1 e2 e3) = do
    r1 <- resolveExpr e1
    r2 <- resolveStmt e2
    r3 <- case e3 of
            Just e -> Just <$> resolveStmt e
            Nothing -> return Nothing
    return $ If r1 r2 r3
resolveStmt (Goto label) = return $ Goto label
resolveStmt (Labelled name stmt) = do
    m <- gets labels
    s1 <- resolveStmt stmt
    unique <- uniqueLabel
    if isJust (lookup name m) then writeError "Duplicate label declaration!"
    else
        modify $ \x -> x {labels = (name, unique) : labels x}
    return $ Labelled unique s1
resolveStmt Null = return Null

writeError :: String -> State SemanticState a
writeError s = do
    modify (\x -> x { err = Just s}) *> error s

resolveDecl :: Declaration -> State SemanticState Declaration
resolveDecl (Declaration name i) = do
    m <- gets variableMap
    unique <- uniqueVar
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
resolveExpr (CompoundAssignment op (Var s) r) = do
    CompoundAssignment op <$> resolveExpr (Var s) <*> resolveExpr r
resolveExpr (CompoundAssignment {}) = writeError "Invalid lvalue!"
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
resolveExpr (Conditional e1 e2 e3) = Conditional <$> resolveExpr e1 <*> resolveExpr e2 <*> resolveExpr e3

uniqueName :: String -> State SemanticState String
uniqueName s = do
    ct <- gets (show . nameCount)
    modify $ \x -> x {nameCount = 1 + nameCount x}
    return $ s ++ ct

uniqueVar :: State SemanticState String
uniqueVar = uniqueName "var."

uniqueLabel :: State SemanticState String
uniqueLabel = uniqueName "lbl_"