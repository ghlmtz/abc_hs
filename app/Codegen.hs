module Codegen
(
    genCode
) where

import qualified Tacky as T
import qualified Parse as P

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Control.Monad.RWS
import TypeCheck (IdentAttr(..))

newtype Program = Program [TopLevel]
data AsmType = Longword | Quadword
    deriving (Eq)
data TopLevel = FuncDef String Bool [Instruction]
              | StaticVar String Bool Int P.StaticInit
data Instruction = Mov AsmType Operand Operand
                 | MovB Operand Operand
                 | Movsx Operand Operand
                 | Ret
                 | Push Operand
                 | Call String
                 | Unary UnaryOp AsmType Operand
                 | Binary BinaryOp AsmType Operand Operand
                 | IDiv AsmType Operand
                 | Cdq AsmType
                 | Cmp AsmType Operand Operand
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
data Register = AX | CX | DX | DI | SI | R8 | R9 | R10 | R11 | SP

data CodeState = CodeState {
  varList :: [String]
}
data CodeReader = CodeReader {
  symbols :: M.Map String (P.Type, IdentAttr)
}
type CodeMonad = RWS CodeReader [Instruction] CodeState
type MayError = Either String

instance Show AsmType where show = showType
showType :: AsmType -> String
showType Longword = "l"
showType Quadword = "q"

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
instance Show Instruction where show = showInstruction

showInstruction :: Instruction -> [Char]
showInstruction (Mov ty o1 o2) = "\tmov" ++ show ty ++ "\t" ++ show o1 ++ ", " ++ show o2
showInstruction (MovB o1 o2) = "\tmovb\t" ++ show o1 ++ ", " ++ show o2
showInstruction (Movsx o1 o2) = "\tmovslq\t" ++ show o1 ++ ", " ++ show o2
showInstruction Ret = "\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret"
showInstruction (Unary un ty op) = show un ++ show ty ++ "\t" ++ show op
showInstruction (Binary bi ty op1 op2) = show bi ++ show ty ++ "\t" ++ show op1 ++ ", " ++ show op2
showInstruction (IDiv ty op) = "\tidiv" ++ show ty ++ "\t" ++ show op
showInstruction (Cdq Longword) = "\tcdq"
showInstruction (Cdq Quadword) = "\tcqo"
showInstruction (Label lbl) = ".L" ++ lbl ++ ":"
showInstruction (Jmp lbl) = "\tjmp\t.L" ++ lbl
showInstruction (Cmp ty op1 op2) = "\tcmp" ++ show ty ++ "\t" ++ show op1 ++ ", " ++ show op2
showInstruction (JmpCC code lbl) = "\tj" ++ show code ++ "\t.L" ++ lbl
showInstruction (SetCC code op) = "\tset" ++ show code ++ "\t" ++ show op
showInstruction (Call name) = "\tcall\t" ++ name
showInstruction (Push op) = "\tpushq\t" ++ show op

instance Show TopLevel where show = showTopLevel

showGlobal :: [Char] -> Bool -> [Char]
showGlobal s True = "\t.globl " ++ s ++ "\n"
showGlobal _ False = ""

showTopLevel :: TopLevel -> [Char]
showTopLevel (FuncDef s global is) =
    showGlobal s global ++ "\t.text\n" ++ s ++ ":\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp\n" ++ concatMap (flip (++) "\n" . show) is
showTopLevel (StaticVar s global align initial) =
    let dat = if initial == P.IntInit 0 || initial == P.LongInit 0 then "\t.bss\n" else "\t.data\n"
        i = case initial of
                (P.IntInit 0) -> ":\n\t.zero 4" 
                (P.LongInit 0) -> ":\n\t.zero 8" 
                (P.IntInit x) -> ":\n\t.long " ++ show x
                (P.LongInit x) -> ":\n\t.quad " ++ show x in
    showGlobal s global ++ dat ++ "\t.align " ++ show align ++ "\n" ++ s ++ i ++ "\n"
instance Show UnaryOp where show = showUnaryOp
showUnaryOp :: UnaryOp -> [Char]
showUnaryOp Neg = "\tneg"
showUnaryOp Not = "\tnot"

instance Show BinaryOp where show = showBinaryOp
showBinaryOp :: BinaryOp -> [Char]
showBinaryOp Add = "\tadd"
showBinaryOp Sub = "\tsub"
showBinaryOp Mult = "\timul"
showBinaryOp And = "\tand"
showBinaryOp Or = "\tor"
showBinaryOp Xor = "\txor"
showBinaryOp LeftShift = "\tsal"
showBinaryOp RightShift = "\tsar"

instance Show Program where show = showProgram

showProgram :: Program -> [Char]
showProgram (Program f) = concatMap show f ++ "\n.section .note.GNU-stack,\"\",@progbits\n"

genCode :: (T.TackyProg, M.Map String (P.Type, IdentAttr)) -> MayError Program
genCode (prog, syms) = pure $ fst $ evalRWS (pass1 prog) (CodeReader syms) (CodeState [])

pass1 :: T.TackyProg -> CodeMonad Program
pass1 (T.TackyProg f) = Program <$> mapM topLevel f

allocateStack :: Integer -> Instruction
allocateStack s = Binary Sub Quadword (Imm s) (Reg8 SP)
deallocateStack :: Integer -> Instruction
deallocateStack s = Binary Add Quadword (Imm s) (Reg8 SP)

topLevel :: T.TopLevel -> CodeMonad TopLevel
topLevel (T.FuncDef name global params stmt) = do
    (_, parsedStmts) <- listen $ do
        zipWithM_ parseParams [0..] params
        mapM_ statement stmt
    len <- gets (length . varList)
    let s = negate $ (+4) $ convertIdx len
        s' = ((s - s `mod` 16) ` div` 16) * 16 + 16
    return $ FuncDef name global (allocateStack s' : parsedStmts)
topLevel (T.StaticVar name global P.TInt initial) = return $ StaticVar name global 4 initial
topLevel (T.StaticVar name global P.TLong initial) = return $ StaticVar name global 8 initial
topLevel _ = error "Bad"

regs :: [Operand]
regs = [Reg DI, Reg SI, Reg DX, Reg CX, Reg R8, Reg R9]

parseParams :: Int -> String -> CodeMonad ()
parseParams n x = do
    (t, e) <- expr (T.Var x)
    if n < 6
        then tell [Mov t (regs !! n) e]
        else fixMov t (Stack ((fromIntegral n - 6) * 8 + 16)) e

fixCmp :: AsmType -> Operand -> Operand -> CodeMonad ()
fixCmp t v1 v2 = do
    let v1' = if isStack v1 then Reg R10 else v1
        v2' = if isConstant v2 then Reg R11 else v2
    tell $ [Mov t v1 (Reg R10) | isStack v1] ++ [Mov t v2 (Reg R11) | isConstant v2] ++ [Cmp t v1' v2']

statement :: T.Instruction -> CodeMonad ()
statement (T.Return e) = do
    (t, e') <- expr e
    tell [Mov t e' (Reg AX), Ret]
statement (T.Unary P.Not src dst) = do
    (st, src') <- expr src
    (dt, dst') <- expr dst
    fixCmp st (Imm 0) src'
    tell [Mov dt (Imm 0) dst', SetCC E dst']
statement (T.Unary op src dst) = do
    (st, src') <- expr src
    (_, dst') <- expr dst
    fixMov st src' dst'
    tell [Unary (operand op) st dst']
statement (T.Binary op s1 s2 dst) = do
    (st, s1') <- expr s1
    (_, s2') <- expr s2
    (dt, dst') <- expr dst
    binaryOp op st s1' s2' dt dst'
statement (T.Jump target) = tell [Jmp target]
statement (T.JZero cond target) = do
    (t, e') <- expr cond
    fixCmp t (Imm 0) e'
    tell [JmpCC E target]
statement (T.JNZero cond target) = do
    (t, e') <- expr cond
    fixCmp t (Imm 0) e'
    tell [JmpCC E target]
    tell [JmpCC NE target]
statement (T.Label lbl) = tell [Label lbl]
statement (T.Copy src dst) = do
    (st, src') <- expr src
    (_, dst') <- expr dst
    fixMov st src' dst'
statement (T.FunctionCall name args dst) = funCall name args dst
statement (T.SignExtend src dst) = do
    src' <- expr src
    dst' <- expr dst
    tell [Movsx (snd src') (snd dst')]
statement (T.Truncate src dst) = do
    src' <- expr src
    dst' <- expr dst
    tell [Mov Longword (snd src') (snd dst')]

funCall :: String -> [T.Value] -> T.Value -> CodeMonad ()
funCall name args dst = do
    let regArgs = take 6 args
        stackArgs = reverse $ drop 6 args
        padding = if odd (length stackArgs) then 8 else 0
        start = [allocateStack padding | padding > 0]
        remove = padding + 8 * fromIntegral (length stackArgs)
        dealloc = [deallocateStack remove | remove > 0]
    (t, eDst) <- expr dst
    tell start
    mapM_ resolveReg $ zip regArgs regs
    mapM_ resolveStack stackArgs
    tell $ [Call name] ++ dealloc ++ [Mov t (Reg AX) eDst]

resolveReg :: (T.Value, Operand) -> CodeMonad ()
resolveReg (v,r) = do
    (t, e) <- expr v
    tell [Mov t e r]

resolveStack :: T.Value -> CodeMonad ()
resolveStack v = do
    (t, e) <- expr v
    tell $ if isStack e || t == Quadword
        then [Mov Longword e (Reg AX), Push (Reg8 AX)]
        else [Push e]

binaryOp :: P.BinaryOp -> AsmType -> Operand -> Operand -> AsmType -> Operand -> CodeMonad ()
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

relational :: CondCode -> AsmType -> Operand -> Operand -> AsmType -> Operand -> CodeMonad ()
relational op st s1 s2 dt dst = fixCmp st s2 s1 >> tell [Mov dt (Imm 0) dst, SetCC op dst]

fixMov :: AsmType -> Operand -> Operand -> CodeMonad ()
fixMov t src dst = tell $ if isStack src && isStack dst
                        then [Mov t src (Reg R10), Mov t (Reg R10) dst]
                        else [Mov t src dst]

genericBinary :: P.BinaryOp -> AsmType -> Operand -> Operand -> AsmType -> Operand -> CodeMonad ()
genericBinary op st s1 s2 _ dst = do
    fixMov st s1 dst
    tell $ if isStack s2 && isStack dst
              then [Mov st s2 (Reg R10), Binary (binOp op) st (Reg R10) dst]
              else [Binary (binOp op) st s2 dst]

divide :: Register -> AsmType -> Operand -> Operand -> AsmType -> Operand -> CodeMonad ()
divide rx st s1 s2 _ dst = do
    tell [Mov st s1 (Reg AX), Cdq st]
    tell $ case s2 of
          Imm _ -> [Mov st s2 (Reg R10), IDiv st (Reg R10)]
          _     -> [IDiv st s2]
    tell [Mov st (Reg rx) dst]

foo :: P.BinaryOp -> AsmType -> Operand -> Operand -> AsmType -> Operand  -> CodeMonad ()
foo op st s1 s2 _ dst = do
    fixMov st s1 dst
    tell $ if isStack dst
              then [Mov st dst (Reg R11), Binary (binOp op) st s2 (Reg R11), Mov st (Reg R11) dst]
              else [Binary (binOp op) st s2 dst]

shift :: P.BinaryOp -> AsmType -> Operand -> Operand -> AsmType -> Operand  -> CodeMonad ()
shift op st s1 s2 _ dst = do
    let s2_int = ([MovB s2 (Reg1 CX) | isStack s2])
        s2'' = if isStack s2 then Reg1 CX else s2
    fixMov st s1 dst
    tell s2_int
    tell $ if isStack dst
        then [Mov st dst (Reg R11), Binary (binOp op) st s2'' (Reg R11), Mov st (Reg R11) dst]
        else [Binary (binOp op) st s2'' dst]

isStack :: Operand -> Bool
isStack (Stack _) = True
isStack (Data _) = True
isStack _ = False

isConstant :: Operand -> Bool
isConstant (Imm _) = True
isConstant _ = False

operand :: P.UnaryOp -> UnaryOp
operand P.Complement = Not
operand P.Negate = Neg
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

asmType :: P.Type -> AsmType
asmType P.TInt = Longword
asmType P.TLong = Quadword
asmType _ = error "bad"

expr :: T.Value -> CodeMonad (AsmType, Operand)
expr (T.Constant (P.ConstInt i)) = return (Longword, Imm i)
expr (T.Constant (P.ConstLong i)) = return (Quadword, Imm i)
expr (T.Var v) = do
    s <- gets varList
    syms <- asks symbols
    case M.lookup v syms of
        Just (t, StaticAttr {}) -> return (asmType t, Data v)
        Just (t, _) -> if v `elem` s 
            then let f = Stack $ convertIdx $ fromMaybe (-1) $ elemIndex v s
                    in return (asmType t, f)
                else do
                    modify $ \x -> x {varList = s ++ [v]}
                    return (asmType t, Stack $ convertIdx $ length s)
        Nothing -> error "Hmm"

convertIdx :: Int -> Integer
convertIdx = fromIntegral . (*) (-4) . (+1)