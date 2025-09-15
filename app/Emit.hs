module Emit
  ( showProgram,
  )
where

import Codegen
import qualified Parse as P

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

showInstruction :: Instruction -> [Char]
showInstruction (Mov ty o1 o2) = "\tmov" ++ showType ty ++ "\t" ++ showOperand o1 ++ ", " ++ showOperand o2
showInstruction (MovB o1 o2) = "\tmovb\t" ++ showOperand o1 ++ ", " ++ showOperand o2
showInstruction (Movsx o1 o2) = "\tmovslq\t" ++ showOperand o1 ++ ", " ++ showOperand o2
showInstruction Ret = "\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret"
showInstruction (Unary un ty op) = showUnaryOp un ++ showType ty ++ "\t" ++ showOperand op
showInstruction (Binary bi ty op1 op2) = showBinaryOp bi ++ showType ty ++ "\t" ++ showOperand op1 ++ ", " ++ showOperand op2
showInstruction (IDiv ty op) = "\tidiv" ++ showType ty ++ "\t" ++ showOperand op
showInstruction (Cdq Longword) = "\tcdq"
showInstruction (Cdq Quadword) = "\tcqo"
showInstruction (Label lbl) = ".L" ++ lbl ++ ":"
showInstruction (Jmp lbl) = "\tjmp\t.L" ++ lbl
showInstruction (Cmp ty op1 op2) = "\tcmp" ++ showType ty ++ "\t" ++ showOperand op1 ++ ", " ++ showOperand op2
showInstruction (JmpCC code lbl) = "\tj" ++ showCode code ++ "\t.L" ++ lbl
showInstruction (SetCC code op) = "\tset" ++ showCode code ++ "\t" ++ showOperand op
showInstruction (Call name) = "\tcall\t" ++ name
showInstruction (Push op) = "\tpushq\t" ++ showOperand op

showGlobal :: [Char] -> Bool -> [Char]
showGlobal s True = "\t.globl " ++ s ++ "\n"
showGlobal _ False = ""

showTopLevel :: TopLevel -> [Char]
showTopLevel (FuncDef s global is) =
  showGlobal s global ++ "\t.text\n" ++ s ++ ":\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp\n" ++ concatMap (flip (++) "\n" . showInstruction) is
showTopLevel (StaticVar s global align initial) =
  let dat = if initial == P.IntInit 0 || initial == P.LongInit 0 then "\t.bss\n" else "\t.data\n"
      i = case initial of
        (P.IntInit 0) -> ":\n\t.zero 4"
        (P.LongInit 0) -> ":\n\t.zero 8"
        (P.IntInit x) -> ":\n\t.long " ++ show x
        (P.LongInit x) -> ":\n\t.quad " ++ show x
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

type MayError = Either String

showProgram :: Program -> MayError [Char]
showProgram (Program f) = pure $ concatMap showTopLevel f ++ "\n.section .note.GNU-stack,\"\",@progbits\n"
