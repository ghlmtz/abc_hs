module Codegen
(
    genCode
) where

import qualified Tacky as T
import qualified Parse as P

import Control.Monad.State
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

newtype Program = Program FuncDef
data FuncDef = FuncDef String [Instruction]
data Instruction = Mov Operand Operand
                 | MovB Operand Operand
                 | Ret
                 | AllocateStack Integer
                 | Unary UnaryOp Operand
                 | Binary BinaryOp Operand Operand
                 | IDiv Operand
                 | Cdq
                 | Cmp Operand Operand
                 | Jmp String
                 | JmpCC CondCode String
                 | SetCC CondCode Operand
                 | Label String
data UnaryOp = Neg | Not
data BinaryOp = Add | Sub | Mult | And | Or | Xor | LeftShift | RightShift
data Operand = Imm Integer
             | Reg Register
             | Stack Integer
data CondCode = E | NE | G | GE | L | LE
data Register = AX | CX | DX | R10 | R11

type VarList = State [String]
type MayError = Either String

instance Show CondCode where show = showCode
showCode :: CondCode -> String
showCode E = "e"
showCode NE = "ne"
showCode L = "l"
showCode LE = "le"
showCode G = "g"
showCode GE = "ge"

instance Show Operand where show = showOperand
showOperand :: Operand -> String
showOperand (Imm i) = "$" ++ show i
showOperand (Reg AX) = "%eax"
showOperand (Reg DX) = "%edx"
showOperand (Reg R10) = "%r10d"
showOperand (Reg R11) = "%r11d"
showOperand (Stack n) = show n ++ "(%rbp)"
showOperand (Reg CX) = "%cl"

instance Show Instruction where show = showInstruction

showInstruction :: Instruction -> [Char]
showInstruction (Mov o1 o2) = "\tmovl\t" ++ show o1 ++ ", " ++ show o2
showInstruction (MovB o1 o2) = "\tmovb\t" ++ show o1 ++ ", " ++ show o2
showInstruction Ret = "\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret"
showInstruction (AllocateStack n) = "\tsubq\t$" ++ show n ++ ", %rsp"
showInstruction (Unary un op) = show un ++ "\t" ++ show op
showInstruction (Binary bi op1 op2) = show bi ++ "\t" ++ show op1 ++ ", " ++ show op2
showInstruction (IDiv op) = "\tidivl\t" ++ show op
showInstruction Cdq = "\tcdq"
showInstruction (Label lbl) = ".L" ++ lbl ++ ":"
showInstruction (Jmp lbl) = "\tjmp\t.L" ++ lbl
showInstruction (Cmp op1 op2) = "\tcmpl\t" ++ show op1 ++ ", " ++ show op2
showInstruction (JmpCC code lbl) = "\tj" ++ show code ++ "\t.L" ++ lbl
showInstruction (SetCC code op) = "\tset" ++ show code ++ "\t" ++ show op

instance Show FuncDef where show = showFuncDef

showFuncDef :: FuncDef -> [Char]
showFuncDef (FuncDef s is) = "\t.globl " ++ s ++ "\n" ++ s ++ ":\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp\n" ++ concatMap (flip (++) "\n" . show) is

instance Show UnaryOp where show = showUnaryOp
showUnaryOp :: UnaryOp -> [Char]
showUnaryOp Neg = "\tnegl"
showUnaryOp Not = "\tnotl"

instance Show BinaryOp where show = showBinaryOp
showBinaryOp :: BinaryOp -> [Char]
showBinaryOp Add = "\taddl"
showBinaryOp Sub = "\tsubl"
showBinaryOp Mult = "\timul"
showBinaryOp And = "\tand"
showBinaryOp Or = "\tor"
showBinaryOp Xor = "\txor"
showBinaryOp LeftShift = "\tsal"
showBinaryOp RightShift = "\tsar"

instance Show Program where show = showProgram

showProgram :: Program -> [Char]
showProgram (Program f) = show f ++ "\n.section .note.GNU-stack,\"\",@progbits\n"

genCode :: T.Program -> MayError Program
genCode prog = pure $ evalState (pass1 prog) []

pass1 :: T.Program -> VarList Program
pass1 (T.Program f) = Program <$> funcDef f

funcDef :: T.FuncDef -> VarList FuncDef
funcDef (T.FuncDef name stmt) = do
    mapStmt <- mapM statement stmt
    len <- gets length
    return $ FuncDef name (AllocateStack (negate $ (+4) $ convertIdx len) : concat mapStmt)

fixCmp :: Operand -> Operand -> [Instruction]
fixCmp v1 v2 = do
    let v1' = if isStack v1 then Reg R10 else v1
        v2' = if isConstant v2 then Reg R11 else v2
        v1_int = ([Mov v1 (Reg R10) | isStack v1])
        v2_int = ([Mov v2 (Reg R11) | isConstant v2])
    v1_int ++ v2_int ++ [Cmp v1' v2']

statement :: T.Instruction -> VarList [Instruction]
statement (T.Return e) = do
    e' <- expr e
    return [Mov e' (Reg AX), Ret]
statement (T.Unary T.Not src dst) = do
    src' <- expr src
    dst' <- expr dst
    return $ fixCmp (Imm 0) src' ++ [Mov (Imm 0) dst', SetCC E dst']
statement (T.Unary op src dst) = do
    src' <- expr src
    dst' <- expr dst
    return $ fixMov src' dst' ++ [Unary (operand op) dst']
statement (T.Binary op s1 s2 dst) =
    binaryOp op <$> expr s1 <*> expr s2 <*> expr dst
statement (T.Jump target) = return [Jmp target]
statement (T.JZero cond target) = do
    cond' <- expr cond
    return $ fixCmp (Imm 0) cond' ++ [JmpCC E target]
statement (T.JNZero cond target) = do
    cond' <- expr cond
    return $ fixCmp (Imm 0) cond' ++ [JmpCC NE target]
statement (T.Label lbl) = return [Label lbl]
statement (T.Copy src dst) = do
    src' <- expr src
    dst' <- expr dst
    return [Mov src' dst']

binaryOp :: P.BinaryOp -> Operand -> Operand -> Operand -> [Instruction]
binaryOp op
    | op == P.Divide || op == P.Remainder = divide (if op == P.Divide then AX else DX)
    | op == P.LeftShift || op == P.RightShift = shift op
    | op == P.And || op == P.Or || op == P.Xor || op == P.Multiply = foo op
    | op == P.Equal || op == P.NotEqual || op == P.LessEqual || op == P.GreaterEqual
      || op == P.LessThan || op == P.GreaterThan = relational (condCode op)
    | otherwise = genericBinary op

condCode :: P.BinaryOp -> CondCode
condCode P.Equal = E
condCode P.NotEqual = NE
condCode P.LessThan = L
condCode P.GreaterEqual = GE
condCode P.LessEqual = LE
condCode P.GreaterThan = G
condCode _ = error "Invalid binary operand"

relational :: CondCode -> Operand -> Operand -> Operand -> [Instruction]
relational op s1 s2 dst = fixCmp s2 s1 ++ [Mov (Imm 0) dst, SetCC op dst]

fixMov :: Operand -> Operand -> [Instruction]
fixMov src dst = if isStack src && isStack dst
                 then [Mov src (Reg R10), Mov (Reg R10) dst]
                 else [Mov src dst]

genericBinary :: P.BinaryOp -> Operand -> Operand -> Operand -> [Instruction]
genericBinary op s1 s2 dst = do
    let cmd = if isStack s2 && isStack dst
              then [Mov s2 (Reg R10), Binary (binOp op) (Reg R10) dst]
              else [Binary (binOp op) s2 dst]
    fixMov s1 dst ++ cmd

divide :: Register -> Operand -> Operand -> Operand -> [Instruction]
divide rx s1 s2 dst = do
    let d = case s2 of
          Imm _ -> [Mov s2 (Reg R10), IDiv (Reg R10)]
          _     -> [IDiv s2]
    [Mov s1 (Reg AX), Cdq] ++ d ++ [Mov (Reg rx) dst]

foo :: P.BinaryOp -> Operand -> Operand -> Operand -> [Instruction]
foo op s1 s2 dst = do
    let cmd = if isStack dst
              then [Mov dst (Reg R11), Binary (binOp op) s2 (Reg R11), Mov (Reg R11) dst]
              else [Binary (binOp op) s2 dst]
    fixMov s1 dst ++ cmd

shift :: P.BinaryOp -> Operand -> Operand -> Operand -> [Instruction]
shift op s1 s2 dst = do
    let s2_int = ([MovB s2 (Reg CX) | isStack s2])
        s2'' = if isStack s2 then Reg CX else s2

    fixMov s1 dst ++ s2_int ++ 
        if isStack dst
        then [Mov dst (Reg R11), Binary (binOp op) s2'' (Reg R11), Mov (Reg R11) dst]
        else [Binary (binOp op) s2'' dst]

isStack :: Operand -> Bool
isStack (Stack _) = True
isStack _ = False

isConstant :: Operand -> Bool
isConstant (Imm _) = True
isConstant _ = False

operand :: T.UnaryOp -> UnaryOp
operand T.Complement = Not
operand T.Negate = Neg
operand _ = error "Bad unary operand"

binOp :: P.BinaryOp -> BinaryOp
binOp P.Add = Add
binOp P.Subtract = Sub
binOp P.Multiply = Mult
binOp P.And = And
binOp P.Or = Or
binOp P.Xor = Xor
binOp P.LeftShift = LeftShift
binOp P.RightShift = RightShift
binOp _ = error "Bad binary operand"

expr :: T.Value -> VarList Operand
expr (T.Constant i) = return $ Imm i
expr (T.Var v) = do
    s <- get
    if v `elem` s then return $ Stack $ convertIdx $ fromMaybe (-1) $ elemIndex v s
    else put (s ++ [v]) >> return (Stack $ convertIdx $ length s)

convertIdx :: Int -> Integer
convertIdx = fromIntegral . (*) (-4) . (+1)