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

type Counter = State (Int, Int)

newtype Program = Program FuncDef
    deriving (Show)
data FuncDef = FuncDef String [Instruction]
    deriving (Show)
data Instruction = Return Value
                 | Unary UnaryOp Value Value
                 | Binary P.BinaryOp Value Value Value
                 | Copy Value Value
                 | Jump String
                 | JZero Value String
                 | JNZero Value String
                 | Label String
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
scan (P.Program f) = Program <$> funcDef f

funcDef :: P.Function -> Counter FuncDef
funcDef (P.Function name (P.Block items)) =
    FuncDef name . concat <$> mapM blockItem (items ++ [P.S (P.Return (P.Int 0))])

blockItem :: P.BlockItem -> Counter [Instruction]
blockItem (P.S s) = statement s
blockItem (P.D (P.Declaration name (Just v))) = do
    foo <- expr $ P.Assignment (P.Var name) v
    return $ snd foo
blockItem (P.D (P.Declaration _ Nothing)) = return []

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
statement P.Null = return []

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