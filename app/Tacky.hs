module Tacky
(
    tack
) where

import Types

import Control.Monad.State

tack :: NProgram -> Either String TProgram
tack prog = Right $ evalState (scan prog) 0

scan :: NProgram -> State Int TProgram
scan (NFunction f) = TProgram <$> funcDef f

funcDef :: NFunction -> State Int TFuncDef
funcDef (Function name stmt) = TFuncDef name <$> statement stmt

statement :: NStatement -> State Int [TInstruction]
statement (NReturn e) = do
    (dst, is) <- expr e
    return $ is ++ [TReturn dst]

operand :: UnaryOp -> TOperand
operand Complement = TComplement
operand Negate = TNegate

expr :: NExpr -> State Int (TValue, [TInstruction])
expr (NInt c) = return (TConstant c, [])
expr (NUnary op e) = do
    src <- expr e
    d <- tmpVar
    let dst = TVar d
    return (dst, snd src ++ [TUnary (operand op) (fst src) dst])

tmpVar :: State Int String
tmpVar = do
    s <- get
    modify (+1)
    return $ "tmp." ++ show s