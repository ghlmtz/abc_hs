module Tacky
(
    tack
) where

import Types

import Control.Monad.State

type Counter = State Int

tack :: NProgram -> Either String TProgram
tack prog = Right $ evalState (scan prog) 0

scan :: NProgram -> Counter TProgram
scan (NFunction f) = TProgram <$> funcDef f

funcDef :: NFunction -> Counter TFuncDef
funcDef (Function name stmt) = TFuncDef name <$> statement stmt

statement :: NStatement -> Counter [TInstruction]
statement (NReturn e) = do
    (dst, is) <- expr e
    return $ is ++ [TReturn dst]

operand :: UnaryOp -> TOperand
operand Complement = TComplement
operand Negate = TNegate

expr :: NExpr -> Counter (TValue, [TInstruction])
expr (NInt c) = return (TConstant c, [])
expr (NUnary op e) = do
    src <- expr e
    d <- tmpVar
    let dst = TVar d
    return (dst, snd src ++ [TUnary (operand op) (fst src) dst])
expr (NBinary op e1 e2) = do
    s1 <- expr e1
    s2 <- expr e2
    d <- tmpVar
    let dst = TVar d
    return (dst, snd s1 ++ snd s2 ++ [TBinary (binOp op) (fst s1) (fst s2) dst])

binOp :: BinaryOp -> TBinOperand
binOp Add = TAdd
binOp Subtract = TSubtract
binOp Multiply = TMultiply
binOp Divide = TDivide
binOp Remainder = TRemainder

tmpVar :: Counter String
tmpVar = do
    s <- get
    modify (+1)
    return $ "tmp." ++ show s