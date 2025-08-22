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
funcDef (P.Function name items) = 
    FuncDef name . (:) (Return (Constant 0)) . concat <$> mapM blockItem items

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
statement P.Null = return []
statement (P.Expression e) = snd <$> expr e

operand :: P.UnaryOp -> UnaryOp
operand P.Complement = Complement
operand P.Negate = Negate
operand P.Not = Not

expr :: P.Expr -> Counter (Value, [Instruction])
expr (P.Int c) = return (Constant c, [])
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