module Tacky
(
  tack
, Program(..)
, FuncDef(..)
, Instruction(..)
, UnaryOp(..)
, Value(..)
) where

import qualified Parse as P

import Control.Monad.State
import Data.Maybe (catMaybes)

type Counter = State (Int, Int)

newtype Program = Program [FuncDef]
    deriving (Show)
data FuncDef = FuncDef String [String] [Instruction]
    deriving (Show)
data Instruction = Return Value
                 | Unary UnaryOp Value Value
                 | Binary P.BinaryOp Value Value Value
                 | Copy Value Value
                 | Jump String
                 | JZero Value String
                 | JNZero Value String
                 | Label String
                 | FunctionCall String [Value] Value
    deriving (Show)
data Value = Constant Integer | Var String
    deriving (Show)
-- data BinaryOp = Add | Subtract | Multiply | Divide | Remainder
--               | LeftShift | RightShift | And | Or | Xor
--               | LogAnd | LogOr | Equal | NotEqual | LessThan | LessEqual
--               | GreaterThan | GreaterEqual
--     deriving (Show)
data UnaryOp = Complement | Negate | Not
    deriving (Show)

tack :: P.Program -> Either String Program
tack prog = Right $ evalState (scan prog) (0, 0)

scan :: P.Program -> Counter Program
scan (P.Program f) = Program <$> mapM funcDef f

funcDef :: P.Function -> Counter FuncDef
funcDef (P.Function name params (Just (P.Block items))) =
    FuncDef name params . concat <$> mapM blockItem (items ++ [P.S (P.Return (P.Int 0))])
funcDef (P.Function name params Nothing) = return $ FuncDef name params []

blockItem :: P.BlockItem -> Counter [Instruction]
blockItem (P.S s) = statement s
blockItem (P.D (P.VarDecl (name, Just v))) = do
    foo <- expr $ P.Assignment (P.Var name) v
    return $ snd foo
blockItem (P.D (P.VarDecl (_, Nothing))) = return []
blockItem (P.D (P.FuncDecl _)) = return []

statement :: P.Statement -> Counter [Instruction]
statement (P.Return e) = do
    (dst, is) <- expr e
    return $ is ++ [Return dst]
statement (P.Expression e) = snd <$> expr e
statement (P.Goto lbl) = return [Jump lbl]
statement (P.Labelled lbl stmt) = (:) (Label lbl) <$> statement stmt
statement (P.If e1 e2 e3) = do
    (cond, is) <- expr e1
    ifBlock <- statement e2
    end <- tmpLabel "end"
    is' <- case e3 of
            Just e -> do
                elseLbl <- tmpLabel "else"
                elseBlock <- statement e
                return $ [JZero cond elseLbl] ++ ifBlock
                    ++ [Jump end, Label elseLbl] ++ elseBlock
            Nothing -> return $ JZero cond end : ifBlock
    return $ is ++ is' ++ [Label end]
statement (P.Compound (P.Block items)) = concat <$> mapM blockItem items
statement (P.Break name) = return [Jump ("break_" ++ name)]
statement (P.Continue name) = return [Jump ("continue_" ++ name)]
statement (P.DoWhile s e name) = do
    start <- tmpLabel "start"
    (cond, is) <- expr e
    body <- statement s
    return $ [Label start] ++ body ++ [Label ("continue_" ++ name)]
        ++ is ++ [JNZero cond start, Label ("break_" ++ name)]
statement (P.While e s name) = do
    let contLbl = "continue_" ++ name
    let brkLbl = "break_" ++ name
    (cond, is) <- expr e
    body <- statement s
    return $ [Label contLbl] ++ is ++ [JZero cond brkLbl] ++ body
        ++ [Jump contLbl, Label brkLbl]
statement (P.For i c p b name) = do
    let contLbl = "continue_" ++ name
    let brkLbl = "break_" ++ name
    start <- tmpLabel "start"
    initial <- initFor i
    cond <- case c of
        Just e -> do
            (v, is) <- expr e
            return $ is ++ [JZero v brkLbl]
        Nothing -> return []
    post <- case p of
        Just e -> snd <$> expr e
        Nothing -> return []
    body <- statement b
    return $ initial ++ [Label start] ++ cond ++ body ++ [Label contLbl]
        ++ post ++ [Jump start, Label brkLbl]
statement (P.Switch e s name cases) = switchStmt e s name cases
statement P.Null = return []
statement (P.Case _ _) = error "Should not occur"
statement (P.Default _) = error "Should not occur"

switchStmt :: P.Expr -> P.Statement -> [Char] -> [Maybe Integer] -> Counter [Instruction]
switchStmt e s name cases = do
    let brkLbl = "break_" ++ name
    (cond, is) <- expr e
    body <- statement s
    casesIs <- mapM (makeCase cond name) (catMaybes cases)
    let pre = if Nothing `notElem` cases
        then [Jump brkLbl]
        else [Jump (name ++ ".default")]
    return $ is ++ concat casesIs ++ pre ++ body ++ [Label brkLbl]

makeCase :: Value -> [Char] -> Integer -> Counter [Instruction]
makeCase cond name n = do
    end <- tmpLabel "end"
    dst <- Var <$> tmpVar
    let is = JZero dst end : [Jump lblName]
    return $ [Binary P.Equal cond (Constant n) dst] ++ is ++ [Label end]
  where lblName = name ++ "." ++ show n

initFor :: P.ForInit -> Counter [Instruction]
initFor (P.InitExpr (Just e)) = snd <$> expr e
initFor (P.InitExpr Nothing) = return []
initFor (P.InitDecl d) = blockItem (P.D d)

operand :: P.UnaryOp -> UnaryOp
operand P.Complement = Complement
operand P.Negate = Negate
operand P.Not = Not
operand _ = error "Invalid unary operand!"

incDec :: P.BinaryOp -> P.Expr -> Bool -> Counter (Value, [Instruction])
incDec op e post = do
    src <- expr e
    dst <- Var <$> tmpVar
    if post then do
        ret <- Var <$> tmpVar
        return (ret, snd src ++
            [ Copy (fst src) ret
            , Binary op (fst src) (Constant 1) dst
            , Copy dst (fst src)])
    else return (fst src,
        [ Binary op (fst src) (Constant 1) dst
        , Copy dst (fst src)])

expr :: P.Expr -> Counter (Value, [Instruction])
expr (P.Int c) = return (Constant c, [])
expr (P.Unary P.PreInc e) = incDec P.Add e False
expr (P.Unary P.PostInc e) = incDec P.Add e True
expr (P.Unary P.PreDec e) = incDec P.Subtract e False
expr (P.Unary P.PostDec e) = incDec P.Subtract e True
expr (P.Unary op e) = do
    src <- expr e
    dst <- Var <$> tmpVar
    return (dst, snd src ++ [Unary (operand op) (fst src) dst])
expr (P.Binary P.LogAnd e1 e2) = do
    s1 <- expr e1
    s2 <- expr e2
    false <- tmpLabel "false"
    end <- tmpLabel "end"
    dst <- Var <$> tmpVar
    return (dst, snd s1 ++
        [JZero (fst s1) false] ++ snd s2 ++
        [ JZero (fst s2) false
        , Copy (Constant 1) dst
        , Jump end
        , Label false
        , Copy (Constant 0) dst
        , Label end])
expr (P.Binary P.LogOr e1 e2) = do
    s1 <- expr e1
    s2 <- expr e2
    true <- tmpLabel "true"
    end <- tmpLabel "end"
    dst <- Var <$> tmpVar
    return (dst, snd s1 ++
        [JNZero (fst s1) true] ++ snd s2 ++
        [ JNZero (fst s2) true
        , Copy (Constant 0) dst
        , Jump end
        , Label true
        , Copy (Constant 1) dst
        , Label end])
expr (P.Binary op e1 e2) = do
    s1 <- expr e1
    s2 <- expr e2
    dst <- Var <$> tmpVar
    return (dst, snd s1 ++ snd s2 ++ [Binary op (fst s1) (fst s2) dst])
expr (P.Var v) = return (Var v, [])
expr (P.Assignment (P.Var v) right) = do
    rs <- expr right
    return (Var v, snd rs ++ [Copy (fst rs) (Var v)])
expr (P.CompoundAssignment op (P.Var v) right) = do
    rs <- expr $ P.Binary op (P.Var v) right
    return (Var v, snd rs ++ [Copy (fst rs) (Var v)])
expr (P.Conditional eCond eIf eElse) = do
    (cond, is) <- expr eCond
    e2Label <- tmpLabel "e2_"
    end <- tmpLabel "end"
    ret <- Var <$> tmpVar
    ifIs <- expr eIf
    elseIs <- expr eElse

    return (ret, is ++ [JZero cond e2Label] ++ snd ifIs
        ++ [Copy (fst ifIs) ret, Jump end, Label e2Label]
        ++ snd elseIs ++ [Copy (fst elseIs) ret, Label end])
expr (P.FunctionCall name args) = do
    es <- mapM expr args
    dst <- Var <$> tmpVar
    return (dst, concatMap snd es ++ [FunctionCall name (map fst es) dst])

expr _ = error "Invalid expression!"

tmpVar :: Counter String
tmpVar = do
    idx <- gets (show . fst)
    modify $ \(x,y) -> (x+1, y)
    return $ "tmp." ++ idx

tmpLabel :: String -> Counter String
tmpLabel name = do
    idx <- gets (show . snd)
    modify $ \(x,y) -> (x, y+1)
    return $ "label_" ++ name ++ idx