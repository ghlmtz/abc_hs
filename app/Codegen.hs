module Codegen
(
    genCode
) where

import qualified Tacky as T
import qualified Parse as P

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import TypeCheck
import qualified Data.Map as M
import Control.Monad.RWS

newtype Program = Program [TopLevel]
data TopLevel = FuncDef String Bool [Instruction]
              | StaticVar String Bool Integer
data Instruction = Mov Operand Operand
                 | MovB Operand Operand
                 | Ret
                 | AllocateStack Integer
                 | DeallocStack Integer
                 | Push Operand
                 | Call String
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
             | Reg1 Register
             | Reg8 Register
             | Stack Integer
             | Data String
data CondCode = E | NE | G | GE | L | LE
data Register = AX | CX | DX | DI | SI | R8 | R9 | R10 | R11

data CodeState = CodeState {
  varList :: [String]
}
data CodeReader = CodeReader {
  symbols :: M.Map String (Type, IdentAttr)
}
type CodeMonad = RWS CodeReader [Instruction] CodeState
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
showOperand (Reg R8) = "%r8d"
showOperand (Reg R9) = "%r9d"
showOperand (Reg R10) = "%r10d"
showOperand (Reg R11) = "%r11d"
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
showOperand (Data name) = name ++ "(%rip)"
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
showInstruction (Call name) = "\tcall\t" ++ name
showInstruction (DeallocStack n) = "\taddq\t$" ++ show n ++ ", %rsp"
showInstruction (Push op) = "\tpushq\t" ++ show op

instance Show TopLevel where show = showTopLevel

showGlobal :: [Char] -> Bool -> [Char]
showGlobal s True = "\t.globl " ++ s ++ "\n"
showGlobal _ False = ""

showTopLevel :: TopLevel -> [Char]
showTopLevel (FuncDef s global is) =
    showGlobal s global ++ "\t.text\n" ++ s ++ ":\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp\n" ++ concatMap (flip (++) "\n" . show) is
showTopLevel (StaticVar s global initial) =
    let dat = if initial == 0 then "\t.bss\n" else "\t.data\n"
        i = if initial == 0 then ":\n\t.zero 4" else ":\n\t.long " ++ show initial in
    showGlobal s global ++ dat ++ "\t.align 4\n" ++ s ++ i ++ "\n"
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
showProgram (Program f) = concatMap show f ++ "\n.section .note.GNU-stack,\"\",@progbits\n"

genCode :: (T.TackyProg, M.Map String (Type, IdentAttr)) -> MayError Program
genCode (prog, syms) = pure $ fst $ evalRWS (pass1 prog) (CodeReader syms) (CodeState [])

pass1 :: T.TackyProg -> CodeMonad Program
pass1 (T.TackyProg f) = Program <$> mapM topLevel f

topLevel :: T.TopLevel -> CodeMonad TopLevel
topLevel (T.FuncDef name global params stmt) = do
    (_, parsedStmts) <- listen $ do
        zipWithM_ parseParams [0..] params
        mapM_ statement stmt
    len <- gets (length . varList)
    let s = negate $ (+4) $ convertIdx len
        s' = ((s - s `mod` 16) ` div` 16) * 16 + 16
    return $ FuncDef name global (AllocateStack s' : parsedStmts)
topLevel (T.StaticVar name global initial) = return $ StaticVar name global initial

regs :: [Operand]
regs = [Reg DI, Reg SI, Reg DX, Reg CX, Reg R8, Reg R9]

parseParams :: Int -> String -> CodeMonad ()
parseParams n x = do
    e <- expr (T.Var x)
    if n < 6
        then tell [Mov (regs !! n) e]
        else fixMov (Stack ((fromIntegral n - 6) * 8 + 16)) e

fixCmp :: Operand -> Operand -> CodeMonad ()
fixCmp v1 v2 = do
    let v1' = if isStack v1 then Reg R10 else v1
        v2' = if isConstant v2 then Reg R11 else v2
    tell $ [Mov v1 (Reg R10) | isStack v1] ++ [Mov v2 (Reg R11) | isConstant v2] ++ [Cmp v1' v2']

statement :: T.Instruction -> CodeMonad ()
statement (T.Return e) = do
    e' <- expr e
    tell [Mov e' (Reg AX), Ret]
statement (T.Unary T.Not src dst) = do
    src' <- expr src
    dst' <- expr dst
    fixCmp (Imm 0) src'
    tell [Mov (Imm 0) dst', SetCC E dst']
statement (T.Unary op src dst) = do
    src' <- expr src
    dst' <- expr dst
    fixMov src' dst'
    tell [Unary (operand op) dst']
statement (T.Binary op s1 s2 dst) = do
    s1' <- expr s1
    s2' <- expr s2
    dst' <- expr dst
    binaryOp op s1' s2' dst'
statement (T.Jump target) = tell [Jmp target]
statement (T.JZero cond target) = do
    fixCmp (Imm 0) =<< expr cond
    tell [JmpCC E target]
statement (T.JNZero cond target) = do
    fixCmp (Imm 0) =<< expr cond
    tell [JmpCC NE target]
statement (T.Label lbl) = tell [Label lbl]
statement (T.Copy src dst) = do
    src' <- expr src
    dst' <- expr dst
    fixMov src' dst'
statement (T.FunctionCall name args dst) = funCall name args dst

funCall :: String -> [T.Value] -> T.Value -> CodeMonad ()
funCall name args dst = do
    let regArgs = take 6 args
        stackArgs = reverse $ drop 6 args
        padding = if odd (length stackArgs) then 8 else 0
        start = [AllocateStack padding | padding > 0]
        remove = padding + 8 * fromIntegral (length stackArgs)
        dealloc = [DeallocStack remove | remove > 0]
    eDst <- expr dst
    tell start
    mapM_ resolveReg $ zip regArgs regs
    mapM_ resolveStack stackArgs
    tell $ [Call name] ++ dealloc ++ [Mov (Reg AX) eDst]

resolveReg :: (T.Value, Operand) -> CodeMonad ()
resolveReg (v,r) = do
    e <- expr v
    tell [Mov e r]

resolveStack :: T.Value -> CodeMonad ()
resolveStack v = do
    e <- expr v
    tell $ if isStack e
        then [Mov e (Reg AX), Push (Reg8 AX)]
        else [Push e]

binaryOp :: P.BinaryOp -> Operand -> Operand -> Operand -> CodeMonad ()
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

relational :: CondCode -> Operand -> Operand -> Operand -> CodeMonad ()
relational op s1 s2 dst = fixCmp s2 s1 >> tell [Mov (Imm 0) dst, SetCC op dst]

fixMov :: Operand -> Operand -> CodeMonad ()
fixMov src dst = tell $ if isStack src && isStack dst
                        then [Mov src (Reg R10), Mov (Reg R10) dst]
                        else [Mov src dst]

genericBinary :: P.BinaryOp -> Operand -> Operand -> Operand -> CodeMonad ()
genericBinary op s1 s2 dst = do
    fixMov s1 dst
    tell $ if isStack s2 && isStack dst
              then [Mov s2 (Reg R10), Binary (binOp op) (Reg R10) dst]
              else [Binary (binOp op) s2 dst]

divide :: Register -> Operand -> Operand -> Operand -> CodeMonad ()
divide rx s1 s2 dst = do
    tell [Mov s1 (Reg AX), Cdq]
    tell $ case s2 of
          Imm _ -> [Mov s2 (Reg R10), IDiv (Reg R10)]
          _     -> [IDiv s2]
    tell [Mov (Reg rx) dst]

foo :: P.BinaryOp -> Operand -> Operand -> Operand -> CodeMonad ()
foo op s1 s2 dst = do
    fixMov s1 dst
    tell $ if isStack dst
              then [Mov dst (Reg R11), Binary (binOp op) s2 (Reg R11), Mov (Reg R11) dst]
              else [Binary (binOp op) s2 dst]

shift :: P.BinaryOp -> Operand -> Operand -> Operand -> CodeMonad ()
shift op s1 s2 dst = do
    let s2_int = ([MovB s2 (Reg1 CX) | isStack s2])
        s2'' = if isStack s2 then Reg1 CX else s2
    fixMov s1 dst
    tell s2_int
    tell $ if isStack dst
        then [Mov dst (Reg R11), Binary (binOp op) s2'' (Reg R11), Mov (Reg R11) dst]
        else [Binary (binOp op) s2'' dst]

isStack :: Operand -> Bool
isStack (Stack _) = True
isStack (Data _) = True
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

expr :: T.Value -> CodeMonad Operand
expr (T.Constant i) = return $ Imm i
expr (T.Var v) = do
    s <- gets varList
    syms <- asks symbols
    case M.lookup v syms of
        Just (_, StaticAttr {}) -> return (Data v)
        _ -> if v `elem` s then return $ Stack $ convertIdx $ fromMaybe (-1) $ elemIndex v s
                else do
                    modify $ \x -> x {varList = s ++ [v]}
                    return (Stack $ convertIdx $ length s)

convertIdx :: Int -> Integer
convertIdx = fromIntegral . (*) (-4) . (+1)