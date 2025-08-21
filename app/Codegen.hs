module Codegen
(
    translateParse, genCode
) where

import Types

import Control.Monad.State
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

data Program = Program FuncDef
data FuncDef = FuncDef String [Instruction]
data Instruction = Mov Operand Operand
                 | Ret
                 | AllocateStack Integer
                 | Unary UnaryOperand Operand
data UnaryOperand = Neg | Not
data Operand = Imm Integer
             | Reg Register
             | Stack Integer
data Register = AX | R10

instance Show Operand where show = showOperand
showOperand :: Operand -> String
showOperand (Imm i) = "$" ++ show i
showOperand (Reg AX) = "%eax"
showOperand (Reg R10) = "%r10d"
showOperand (Stack n) = show n ++ "(%rbp)"

instance Show Instruction where show = showInstruction

showInstruction :: Instruction -> [Char]
showInstruction (Mov o1 o2) = "\tmovl\t" ++ show o1 ++ ", " ++ show o2
showInstruction Ret = "\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret"
showInstruction (AllocateStack n) = "\tsubq\t$" ++ show n ++ ", %rsp"
showInstruction (Unary un op) = show un ++ "\t" ++ show op

instance Show FuncDef where show = showFuncDef

showFuncDef :: FuncDef -> [Char]
showFuncDef (FuncDef s is) = "\t.globl " ++ s ++ "\n" ++ s ++ ":\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp\n" ++ concatMap (flip (++) "\n" . show) is

instance Show UnaryOperand where show = showUnaryOp
showUnaryOp :: UnaryOperand -> [Char]
showUnaryOp Neg = "\tnegl"
showUnaryOp Not = "\tnotl"

instance Show Program where show = showProgram

showProgram :: Program -> [Char]
showProgram (Program f) = show f ++ "\n.section .note.GNU-stack,\"\",@progbits\n"

translateParse :: TProgram -> String
translateParse = show . genCode

genCode :: TProgram -> MayError Program
genCode prog = pure $ evalState (pass1 prog) []

pass1 :: TProgram -> State [String] Program
pass1 (TProgram f) = Program <$> funcDef f

funcDef :: TFuncDef -> State [String] FuncDef
funcDef (TFuncDef name stmt) = do
    foo <- mapM statement stmt
    s <- get
    return $ FuncDef name (AllocateStack (negate $ (+4) $ convertIdx $ length s) : concat foo)

statement :: TInstruction -> State [String] [Instruction]
statement (TReturn e) = do
    e' <- expr e
    return [Mov e' (Reg AX), Ret]
statement (TUnary op src dst) = do
    src' <- expr src
    dst' <- expr dst
    if isStack src' && isStack dst'
        then return [Mov src' (Reg R10), Mov (Reg R10) dst', Unary (operand op) dst']
        else return [Mov src' dst', Unary (operand op) dst']

isStack :: Operand -> Bool
isStack (Stack _) = True
isStack _ = False

operand :: TOperand -> UnaryOperand
operand TComplement = Not
operand TNegate = Neg

expr :: TValue -> State [String] Operand
expr (TConstant i) = return $ Imm i
expr (TVar v) = do
    s <- get
    if v `elem` s then return $ Stack $ convertIdx $ fromMaybe (-1) $ elemIndex v s
    else put (s ++ [v]) >> return (Stack $ convertIdx $ length s)

convertIdx :: Int -> Integer
convertIdx = fromIntegral . (*) (-4) . (+1)