module Emit
  ( showProgram,
  )
where

import Codegen
import qualified Semantic as S
import TypeCheck (getStatic)

showType :: AsmType -> String
showType Longword = "l"
showType Quadword = "q"

showCode :: CondCode -> String
showCode E = "e"
showCode NE = "ne"
showCode L = "l"
showCode LE = "le"
showCode G = "g"
showCode GE = "ge"
showCode A = "a"
showCode AE = "ae"
showCode B = "b"
showCode BE = "be"

showOperand :: Operand -> String
showOperand (Imm i) = "$" ++ show i
showOperand (Reg AX) = "%eax"
showOperand (Reg DX) = "%edx"
showOperand (Reg R8) = "%r8d"
showOperand (Reg R9) = "%r9d"
showOperand (Reg R10) = "%r10d"
showOperand (Reg R11) = "%r11d"
showOperand (Reg SP) = "%esp"
showOperand (Stack n) = show n ++ "(%rbp)"
showOperand (Reg CX) = "%ecx"
showOperand (Reg DI) = "%edi"
showOperand (Reg SI) = "%esi"
showOperand (Reg1 CX) = "%cl"
showOperand (Reg1 _) = error "Not implemented!"
showOperand (Reg8 AX) = "%rax"
showOperand (Reg8 DX) = "%rdx"
showOperand (Reg8 CX) = "%rcx"
showOperand (Reg8 DI) = "%rdi"
showOperand (Reg8 SI) = "%rsi"
showOperand (Reg8 R8) = "%r8"
showOperand (Reg8 R9) = "%r9"
showOperand (Reg8 R10) = "%r10"
showOperand (Reg8 R11) = "%r11"
showOperand (Reg8 SP) = "%rsp"
showOperand (Data name) = name ++ "(%rip)"

showOperandType :: AsmType -> Operand -> String
showOperandType Longword op@(Reg _) = showOperand op
showOperandType Quadword (Reg x) = showOperand (Reg8 x)
showOperandType _ op = showOperand op

showInstruction :: Instruction -> [Char]
showInstruction (Mov ty o1 o2) = "\tmov" ++ showType ty ++ "\t" ++ showOperandType ty o1 ++ ", " ++ showOperandType ty o2
showInstruction (MovB o1 o2) = "\tmovb\t" ++ showOperand o1 ++ ", " ++ showOperand o2
showInstruction (Movsx o1 o2) = "\tmovslq\t" ++ showOperandType Longword o1 ++ ", " ++ showOperandType Quadword o2
showInstruction Ret = "\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret"
showInstruction (Unary un ty op) = showUnaryOp un ++ showType ty ++ "\t" ++ showOperandType ty op
showInstruction (Binary bi ty op1 op2) = showBinaryOp bi ++ showType ty ++ "\t" ++ showOperandType ty op1 ++ ", " ++ showOperandType ty op2
showInstruction (IDiv ty op) = "\tidiv" ++ showType ty ++ "\t" ++ showOperandType ty op
showInstruction (Div ty op) = "\tdiv" ++ showType ty ++ "\t" ++ showOperandType ty op
showInstruction (Cdq Longword) = "\tcdq"
showInstruction (Cdq Quadword) = "\tcqo"
showInstruction (Label lbl) = ".L" ++ lbl ++ ":"
showInstruction (Jmp lbl) = "\tjmp\t.L" ++ lbl
showInstruction (Cmp ty op1 op2) = "\tcmp" ++ showType ty ++ "\t" ++ showOperandType ty op1 ++ ", " ++ showOperandType ty op2
showInstruction (JmpCC code lbl) = "\tj" ++ showCode code ++ "\t.L" ++ lbl
showInstruction (SetCC code op) = "\tset" ++ showCode code ++ "\t" ++ showOperand op
showInstruction (Call name) = "\tcall\t" ++ name
showInstruction (Push op) = "\tpushq\t" ++ showOperand op

showGlobal :: [Char] -> Bool -> [Char]
showGlobal s True = "\t.globl " ++ s ++ "\n"
showGlobal _ False = ""

foo :: S.StaticInit -> Integer
foo = getStatic

showTopLevel :: TopLevel -> [Char]
showTopLevel (FuncDef s global is) =
  showGlobal s global ++ "\t.text\n" ++ s ++ ":\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp\n" ++ concatMap (flip (++) "\n" . showInstruction) is
showTopLevel (StaticVar s global align initial) =
  let dat = if initial == S.IntInit 0 || initial == S.LongInit 0 || initial == S.UIntInit 0 || initial == S.ULongInit 0 then "\t.bss\n" else "\t.data\n"
      i = case foo initial of
        0 -> ":\n\t.zero " ++ show align
        x -> if align == 4 then ":\n\t.long " ++ show x else ":\n\t.quad " ++ show x
   in showGlobal s global ++ dat ++ "\t.align " ++ show align ++ "\n" ++ s ++ i ++ "\n"

showUnaryOp :: UnaryOp -> [Char]
showUnaryOp Neg = "\tneg"
showUnaryOp Not = "\tnot"

showBinaryOp :: BinaryOp -> [Char]
showBinaryOp Add = "\tadd"
showBinaryOp Sub = "\tsub"
showBinaryOp Mult = "\timul"
showBinaryOp And = "\tand"
showBinaryOp Or = "\tor"
showBinaryOp Xor = "\txor"
showBinaryOp LeftShift = "\tsal"
showBinaryOp RightShift = "\tsar"
showBinaryOp RightLShift = "\tshr"

type MayError = Either String

showProgram :: Program -> MayError [Char]
showProgram (Program f) = pure $ concatMap showTopLevel f ++ "\n.section .note.GNU-stack,\"\",@progbits\n"
