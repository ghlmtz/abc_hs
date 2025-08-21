module Codegen
(
    translateParse, genCode
) where

import qualified Tacky as T

import Control.Monad.State
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

data Program = Program FuncDef
data FuncDef = FuncDef String [Instruction]
data Instruction = Mov Operand Operand
                 | Ret
                 | AllocateStack Integer
                 | Unary UnaryOp Operand
                 | Binary BinaryOp Operand Operand
                 | IDiv Operand
                 | Cdq
data UnaryOp = Neg | Not
data BinaryOp = Add | Sub | Mult
data Operand = Imm Integer
             | Reg Register
             | Stack Integer
data Register = AX | DX | R10 | R11

type VarList = State [String]
type MayError = Either String 

instance Show Operand where show = showOperand
showOperand :: Operand -> String
showOperand (Imm i) = "$" ++ show i
showOperand (Reg AX) = "%eax"
showOperand (Reg DX) = "%edx"
showOperand (Reg R10) = "%r10d"
showOperand (Reg R11) = "%r11d"
showOperand (Stack n) = show n ++ "(%rbp)"

instance Show Instruction where show = showInstruction

showInstruction :: Instruction -> [Char]
showInstruction (Mov o1 o2) = "\tmovl\t" ++ show o1 ++ ", " ++ show o2
showInstruction Ret = "\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret"
showInstruction (AllocateStack n) = "\tsubq\t$" ++ show n ++ ", %rsp"
showInstruction (Unary un op) = show un ++ "\t" ++ show op
showInstruction (Binary bi op1 op2) = show bi ++ "\t" ++ show op1 ++ ", " ++ show op2
showInstruction (IDiv op) = "\tidivl\t" ++ show op
showInstruction Cdq = "\tcdq"

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

instance Show Program where show = showProgram

showProgram :: Program -> [Char]
showProgram (Program f) = show f ++ "\n.section .note.GNU-stack,\"\",@progbits\n"

translateParse :: T.Program -> String
translateParse = show . genCode

genCode :: T.Program -> MayError Program
genCode prog = pure $ evalState (pass1 prog) []

pass1 :: T.Program -> VarList Program
pass1 (T.Program f) = Program <$> funcDef f

funcDef :: T.FuncDef -> VarList FuncDef
funcDef (T.FuncDef name stmt) = do
    foo <- mapM statement stmt
    s <- get
    return $ FuncDef name (AllocateStack (negate $ (+4) $ convertIdx $ length s) : concat foo)

statement :: T.Instruction -> VarList [Instruction]
statement (T.Return e) = do
    e' <- expr e
    return [Mov e' (Reg AX), Ret]
statement (T.Unary op src dst) = do
    src' <- expr src
    dst' <- expr dst
    if isStack src' && isStack dst'
        then return [Mov src' (Reg R10), Mov (Reg R10) dst', Unary (operand op) dst']
        else return [Mov src' dst', Unary (operand op) dst']
statement (T.Binary T.Divide s1 s2 dst) = do
    s1' <- expr s1
    s2' <- expr s2
    dst' <- expr dst
    let d = case s2' of
          Imm _ -> [Mov s2' (Reg R10), IDiv (Reg R10)]
          _     -> [IDiv s2']
    return $ [Mov s1' (Reg AX), Cdq] ++ d ++ [Mov (Reg AX) dst']
statement (T.Binary T.Remainder s1 s2 dst) = do
    s1' <- expr s1
    s2' <- expr s2
    dst' <- expr dst
    let d = case s2' of
          Imm _ -> [Mov s2' (Reg R10), IDiv (Reg R10)]
          _     -> [IDiv s2']
    return $ [Mov s1' (Reg AX), Cdq] ++ d ++ [Mov (Reg DX) dst']
statement (T.Binary T.Multiply s1 s2 dst) = do
    s1' <- expr s1
    s2' <- expr s2
    dst' <- expr dst
    let mov = if isStack s1' && isStack dst'
              then [Mov s1' (Reg R10), Mov (Reg R10) dst']
              else [Mov s1' dst']
    if isStack dst'
        then return $ mov ++ [Mov dst' (Reg R11), Binary Mult s2' (Reg R11), Mov (Reg R11) dst']
        else return $ mov ++ [Binary Mult s2' dst']
statement (T.Binary op s1 s2 dst) = do
    s1' <- expr s1
    s2' <- expr s2
    dst' <- expr dst
    let cmd = if isStack s2' && isStack dst'
              then [Mov s2' (Reg R10), Binary (binOp op) (Reg R10) dst']
              else [Binary (binOp op) s2' dst']
        mov = if isStack s1' && isStack dst'
              then [Mov s1' (Reg R10), Mov (Reg R10) dst']
              else [Mov s1' dst']
    return $ mov ++ cmd

isStack :: Operand -> Bool
isStack (Stack _) = True
isStack _ = False

operand :: T.UnaryOp -> UnaryOp
operand T.Complement = Not
operand T.Negate = Neg

binOp :: T.BinaryOp -> BinaryOp
binOp T.Add = Add
binOp T.Subtract = Sub
binOp _ = error "Bad binary operand"

expr :: T.Value -> VarList Operand
expr (T.Constant i) = return $ Imm i
expr (T.Var v) = do
    s <- get
    if v `elem` s then return $ Stack $ convertIdx $ fromMaybe (-1) $ elemIndex v s
    else put (s ++ [v]) >> return (Stack $ convertIdx $ length s)

convertIdx :: Int -> Integer
convertIdx = fromIntegral . (*) (-4) . (+1)