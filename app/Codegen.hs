module Codegen
(
    translateParse
) where

import Types

data Program = Program FuncDef
data FuncDef = FuncDef String [Instruction]
data Instruction = Mov Operand Operand | Ret
data Operand = Imm Integer | Register

instance Show Operand where show = showOperand
showOperand :: Operand -> String
showOperand (Imm i) = "$" ++ show i
showOperand Register = "%eax"

instance Show Instruction where show = showInstruction

showInstruction :: Instruction -> [Char]
showInstruction (Mov o1 o2) = "\tmovl\t" ++ show o1 ++ ", " ++ show o2
showInstruction Ret = "\tret"

instance Show FuncDef where show = showFuncDef

showFuncDef :: FuncDef -> [Char]
showFuncDef (FuncDef s is) = "\t.globl " ++ s ++ "\n" ++ s ++ ":\n" ++ concatMap (flip (++) "\n" . show) is

instance Show Program where show = showProgram

showProgram :: Program -> [Char]
showProgram (Program f) = show f ++ "\n.section .note.GNU-stack,\"\",@progbits\n"

translateParse :: NProgram -> String
translateParse = show . scan 

scan :: NProgram -> Program
scan (NFunction f) = Program $ funcDef f

funcDef :: NFunction -> FuncDef
funcDef (Function name stmt) = FuncDef name (statement stmt)

statement :: NStatement -> [Instruction]
statement (NReturn e) = [Mov (expr e) Register, Ret]

expr :: NExpr -> Operand
expr (NInt i) = Imm i

