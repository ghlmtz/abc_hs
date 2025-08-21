module Tacky
(
  tack
, Program(..)
, FuncDef(..)
, Instruction(..)
, UnaryOp(..)
, BinaryOp(..)
, Value(..)
) where

import qualified Parse as P

import Control.Monad.State

type Counter = State Int

newtype Program = Program FuncDef
    deriving (Show)
data FuncDef = FuncDef String [Instruction]
    deriving (Show)
data Instruction = Return Value 
                 | Unary UnaryOp Value Value 
                 | Binary BinaryOp Value Value Value
    deriving (Show)
data Value = Constant Integer | Var String
    deriving (Show)
data BinaryOp = Add | Subtract | Multiply | Divide | Remainder
              | LeftShift | RightShift | And | Or | Xor
    deriving (Show)
data UnaryOp = Complement | Negate
    deriving (Show)

tack :: P.Program -> Either String Program
tack prog = Right $ evalState (scan prog) 0

scan :: P.Program -> Counter Program
scan (P.Program f) = Program <$> funcDef f

funcDef :: P.Function -> Counter FuncDef
funcDef (P.Function name stmt) = FuncDef name <$> statement stmt

statement :: P.Statement -> Counter [Instruction]
statement (P.Return e) = do
    (dst, is) <- expr e
    return $ is ++ [Return dst]

operand :: P.UnaryOp -> UnaryOp
operand P.Complement = Complement
operand P.Negate = Negate

expr :: P.Expr -> Counter (Value, [Instruction])
expr (P.Int c) = return (Constant c, [])
expr (P.Unary op e) = do
    src <- expr e
    d <- tmpVar
    let dst = Var d
    return (dst, snd src ++ [Unary (operand op) (fst src) dst])
expr (P.Binary op e1 e2) = do
    s1 <- expr e1
    s2 <- expr e2
    d <- tmpVar
    let dst = Var d
    return (dst, snd s1 ++ snd s2 ++ [Binary (binOp op) (fst s1) (fst s2) dst])

binOp :: P.BinaryOp -> BinaryOp
binOp P.Add = Add
binOp P.Subtract = Subtract
binOp P.Multiply = Multiply
binOp P.Divide = Divide
binOp P.Remainder = Remainder
binOp P.Xor = Xor
binOp P.Or = Or
binOp P.And = And
binOp P.LeftShift = LeftShift
binOp P.RightShift = RightShift

tmpVar :: Counter String
tmpVar = do
    s <- get
    modify (+1)
    return $ "tmp." ++ show s