module Codegen
  ( genCode,
    Program (..),
    TopLevel (..),
    AsmType (..),
    Instruction (..),
    CondCode (..),
    Operand (..),
    UnaryOp (..),
    BinaryOp (..),
    Register (..),
  )
where

import Control.Monad.RWS
import qualified Data.Map as M
import qualified Parse as P
import qualified Tacky as T
import TypeCheck (IdentAttr (..), signed)
import qualified TypeCheck as TC

newtype Program = Program [TopLevel]
  deriving (Show)

data AsmType = Longword | Quadword
  deriving (Eq, Show)

data TopLevel
  = FuncDef String Bool [Instruction]
  | StaticVar String Bool Int P.StaticInit
  deriving (Show)

data Instruction
  = Mov AsmType Operand Operand
  | MovB Operand Operand
  | Movsx Operand Operand
  | Ret
  | Push Operand
  | Call String
  | Unary UnaryOp AsmType Operand
  | Binary BinaryOp AsmType Operand Operand
  | IDiv AsmType Operand
  | Div AsmType Operand
  | Cdq AsmType
  | Cmp AsmType Operand Operand
  | Jmp String
  | JmpCC CondCode String
  | SetCC CondCode Operand
  | Label String
  deriving (Show)

data UnaryOp = Neg | Not
  deriving (Show)

data BinaryOp = Add | Sub | Mult | And | Or | Xor | LeftShift | RightShift | RightLShift
  deriving (Show)

data Operand
  = Imm Integer
  | Reg Register
  | Reg1 Register
  | Reg8 Register
  | Stack Integer
  | Data String
  deriving (Show)

data CondCode = E | NE | G | GE | L | LE | A | AE | B | BE
  deriving (Show)

data Register = AX | CX | DX | DI | SI | R8 | R9 | R10 | R11 | SP
  deriving (Show)

newtype CodeState = CodeState
  { varList :: M.Map String Integer
  }

newtype CodeReader = CodeReader
  { symbols :: M.Map String (P.Type, IdentAttr)
  }

type CodeMonad = RWS CodeReader [Instruction] CodeState

type MayError = Either String

genCode :: (T.TackyProg, M.Map String (P.Type, IdentAttr)) -> MayError Program
genCode (prog, syms) = pure $ fst $ evalRWS (pass1 prog) (CodeReader syms) (CodeState M.empty)

pass1 :: T.TackyProg -> CodeMonad Program
pass1 (T.TackyProg f) = Program <$> mapM topLevel f

allocateStack :: Integer -> Instruction
allocateStack s = Binary Sub Quadword (Imm s) (Reg8 SP)

deallocateStack :: Integer -> Instruction
deallocateStack s = Binary Add Quadword (Imm s) (Reg8 SP)

topLevel :: T.TopLevel -> CodeMonad TopLevel
topLevel (T.FuncDef name global params stmt) = do
  (_, parsedStmts) <- listen $ do
    zipWithM_ parseParams [0 ..] params
    mapM_ statement stmt
  len <- minOfVars
  let s = negate len
      s' = ((s - s `mod` 16) `div` 16) * 16 + 16
  modify $ \x -> x {varList = M.empty}
  return $ FuncDef name global (allocateStack s' : parsedStmts)
topLevel (T.StaticVar name global P.TInt initial) = return $ StaticVar name global 4 initial
topLevel (T.StaticVar name global P.TLong initial) = return $ StaticVar name global 8 initial
topLevel (T.StaticVar name global P.TUInt initial) = return $ StaticVar name global 4 initial
topLevel (T.StaticVar name global P.TULong initial) = return $ StaticVar name global 8 initial

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
  let v1' = if isStack v1 || checkLargeImm2 v1 then Reg R10 else v1
      v2' = if isConstant v2 then Reg R11 else v2
  tell $ [Mov t v1 (Reg R10) | isStack v1 || checkLargeImm2 v1] ++ [Mov t v2 (Reg R11) | isConstant v2] ++ [Cmp t v1' v2']

statement :: T.Instruction -> CodeMonad ()
statement (T.Return e) = do
  (t, e') <- expr e
  tell [Mov t e' (Reg AX), Ret]
statement (T.Unary T.Not src dst) = do
  (st, src') <- expr src
  (dt, dst') <- expr dst
  fixCmp st (Imm 0) src'
  tell [Mov dt (Imm 0) dst', SetCC E dst']
statement (T.Unary T.Complement src dst) = do
  (st, src') <- expr src
  (_, dst') <- expr dst
  fixMov st src' dst'
  tell [Unary Not st dst']
statement (T.Unary T.Negate src dst) = do
  (st, src') <- expr src
  (_, dst') <- expr dst
  fixMov st src' dst'
  tell [Unary Neg st dst']
statement (T.Binary op s1 s2 dst) = do
  (st, s1') <- expr s1
  (_, s2') <- expr s2
  ct <- cType s1
  (dt, dst') <- expr dst
  binaryOp op ct st s1' s2' dt dst'
statement (T.Jump target) = tell [Jmp target]
statement (T.JZero cond target) = do
  (t, e') <- expr cond
  fixCmp t (Imm 0) e'
  tell [JmpCC E target]
statement (T.JNZero cond target) = do
  (t, e') <- expr cond
  fixCmp t (Imm 0) e'
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
  fixMovsx (snd src') (snd dst')
statement (T.Truncate src dst) = do
  (_, src') <- expr src
  (_, dst') <- expr dst
  fixMov Longword src' dst'
statement (T.ZeroExtend src dst) = do
  (_, src') <- expr src
  (_, dst') <- expr dst
  tell $ case dst' of
    (Reg _) -> [Mov Longword src' dst']
    _ -> [Mov Longword src' (Reg R11), Mov Quadword (Reg R11) dst']

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
resolveReg (v, r) = do
  (t, e) <- expr v
  tell [Mov t e r]

fixPush :: Operand -> [Instruction]
fixPush e =
  if checkLargeImm2 e
    then [Mov Quadword e (Reg R10), Push (Reg8 R10)]
    else [Push e]

resolveStack :: T.Value -> CodeMonad ()
resolveStack v = do
  (t, e) <- expr v
  tell $
    if not (isStack e) || t == Quadword
      then fixPush e
      else [Mov Longword e (Reg AX), Push (Reg8 AX)]

binaryOp :: TC.BinaryOp -> P.VarType -> AsmType -> Operand -> Operand -> AsmType -> Operand -> CodeMonad ()
binaryOp op ty
  | op == TC.Divide || op == TC.Remainder = divide (if op == TC.Divide then AX else DX) (signed ty)
  | op == TC.LeftShift || op == TC.RightShift || op == TC.RightLShift = shift op
  | op == TC.And || op == TC.Or || op == TC.Xor = foo op
  | op == TC.Multiply = mult op
  | op == TC.Equal
      || op == TC.NotEqual
      || op == TC.LessEqual
      || op == TC.GreaterEqual
      || op == TC.LessThan
      || op == TC.GreaterThan =
      relational (condCode op (signed ty))
  | otherwise = genericBinary op

condCode :: TC.BinaryOp -> Bool -> CondCode
condCode TC.Equal _ = E
condCode TC.NotEqual _ = NE
condCode TC.LessThan True = L
condCode TC.GreaterEqual True = GE
condCode TC.LessEqual True = LE
condCode TC.GreaterThan True = G
condCode TC.LessThan False = B
condCode TC.GreaterEqual False = AE
condCode TC.LessEqual False = BE
condCode TC.GreaterThan False = A
condCode _ _ = error "Invalid binary operand"

relational :: CondCode -> AsmType -> Operand -> Operand -> AsmType -> Operand -> CodeMonad ()
relational op st s1 s2 dt dst = fixCmp st s2 s1 >> tell [Mov dt (Imm 0) dst, SetCC op dst]

checkLargeImm :: Operand -> Operand -> Bool
checkLargeImm (Imm x) dst = x > 2147483647 && isStack dst
checkLargeImm _ _ = False

checkLargeImm2 :: Operand -> Bool
checkLargeImm2 (Imm x) = x > 2147483647
checkLargeImm2 _ = False

fixMov :: AsmType -> Operand -> Operand -> CodeMonad ()
fixMov t src dst =
  tell $
    if (isStack src && isStack dst) || (t == Quadword && checkLargeImm src dst)
      then [Mov t src (Reg R10), Mov t (Reg R10) dst]
      else [Mov t src dst]

fixMovsx :: Operand -> Operand -> CodeMonad ()
fixMovsx src dst = do
  let src' = if isConstant src then Reg R10 else src
      dst' = if isStack dst then Reg R11 else dst
  when (isConstant src) $ tell [Mov Longword src (Reg R10)]
  tell [Movsx src' dst']
  when (isStack dst) $ tell [Mov Quadword (Reg R11) dst]

genericBinary :: TC.BinaryOp -> AsmType -> Operand -> Operand -> AsmType -> Operand -> CodeMonad ()
genericBinary op st s1 s2 _ dst = do
  fixMov st s1 dst
  tell $
    if isStack s2 && isStack dst || checkLargeImm2 s2
      then [Mov st s2 (Reg R10), Binary (binOp op) st (Reg R10) dst]
      else [Binary (binOp op) st s2 dst]

divide :: Register -> Bool -> AsmType -> Operand -> Operand -> AsmType -> Operand -> CodeMonad ()
divide rx True st s1 s2 _ dst = do
  tell [Mov st s1 (Reg AX), Cdq st]
  tell $ case s2 of
    Imm _ -> [Mov st s2 (Reg R10), IDiv st (Reg R10)]
    _ -> [IDiv st s2]
  tell [Mov st (Reg rx) dst]
divide rx False st s1 s2 _ dst = do
  tell [Mov st s1 (Reg AX), Mov st (Imm 0) (Reg DX)]
  tell $ case s2 of
    Imm _ -> [Mov st s2 (Reg R10), Div st (Reg R10)]
    _ -> [Div st s2]
  tell [Mov st (Reg rx) dst]

mult :: TC.BinaryOp -> AsmType -> Operand -> Operand -> AsmType -> Operand -> CodeMonad ()
mult op st s1 s2 _ dst = do
  fixMov st s1 dst
  let s2' = if checkLargeImm2 s2 then Reg R10 else s2
  when (checkLargeImm2 s2) $ tell [Mov Quadword s2 (Reg R10)]
  tell $
    if isStack dst
      then [Mov st dst (Reg R11), Binary (binOp op) st s2' (Reg R11), Mov st (Reg R11) dst]
      else [Binary (binOp op) st s2' dst]

foo :: TC.BinaryOp -> AsmType -> Operand -> Operand -> AsmType -> Operand -> CodeMonad ()
foo op st s1 s2 _ dst = do
  fixMov st s1 dst
  let s2' = if checkLargeImm2 s2 then Reg R10 else s2
  when (checkLargeImm2 s2) $ tell [Mov Quadword s2 (Reg R10)]
  tell $
    if isStack dst
      then [Mov st dst (Reg R11), Binary (binOp op) st s2' (Reg R11), Mov st (Reg R11) dst]
      else [Binary (binOp op) st s2' dst]

shift :: TC.BinaryOp -> AsmType -> Operand -> Operand -> AsmType -> Operand -> CodeMonad ()
shift op st s1 s2 _ dst = do
  let s2_int = ([MovB s2 (Reg1 CX) | isStack s2])
      s2'' = if isStack s2 then Reg1 CX else s2
  fixMov st s1 dst
  tell s2_int
  tell $
    if isStack dst
      then [Mov st dst (Reg R11), Binary (binOp op) st s2'' (Reg R11), Mov st (Reg R11) dst]
      else [Binary (binOp op) st s2'' dst]

isStack :: Operand -> Bool
isStack (Stack _) = True
isStack (Data _) = True
isStack _ = False

isConstant :: Operand -> Bool
isConstant (Imm _) = True
isConstant _ = False

binOp :: TC.BinaryOp -> BinaryOp
binOp TC.Add = Add
binOp TC.Subtract = Sub
binOp TC.Multiply = Mult
binOp TC.And = And
binOp TC.Or = Or
binOp TC.Xor = Xor
binOp TC.LeftShift = LeftShift
binOp TC.RightShift = RightShift
binOp TC.RightLShift = RightLShift
binOp _ = error "Bad binary operand"

asmType :: P.VarType -> AsmType
asmType P.TInt = Longword
asmType P.TLong = Quadword
asmType P.TUInt = Longword
asmType P.TULong = Quadword

cType :: T.Value -> CodeMonad P.VarType
cType (T.Constant (P.ConstInt _)) = return P.TInt
cType (T.Constant (P.ConstLong _)) = return P.TLong
cType (T.Constant (P.ConstUInt _)) = return P.TUInt
cType (T.Constant (P.ConstULong _)) = return P.TULong
cType (T.Var v) = do
  syms <- asks symbols
  case M.lookup v syms of
    Just (P.TVar t, _) -> return t
    _ -> error "Hmm"

expr :: T.Value -> CodeMonad (AsmType, Operand)
expr (T.Constant (P.ConstInt i)) = return (Longword, Imm i)
expr (T.Constant (P.ConstLong i)) = return (Quadword, Imm i)
expr (T.Constant (P.ConstUInt i)) = return (Longword, Imm i)
expr (T.Constant (P.ConstULong i)) = return (Quadword, Imm i)
expr (T.Var v) = do
  s <- gets varList
  syms <- asks symbols
  case M.lookup v syms of
    Just (P.TVar t, StaticAttr {}) -> return (asmType t, Data v)
    Just (P.TVar t, _) ->
      case M.lookup v s of
        Just x -> return (asmType t, Stack x)
        Nothing -> do
          minVal <- minOfVars
          let newVal =
                if t == P.TLong || t == P.TULong
                  then
                    if minVal `mod` 8 /= 0
                      then minVal - 12
                      else minVal - 8
                  else minVal - 4
          modify $ \x -> x {varList = M.insert v newVal (varList x)}
          return (asmType t, Stack newVal)
    _ -> error "Hmm"

minOfVars :: CodeMonad Integer
minOfVars = M.foldr min 0 <$> gets varList
