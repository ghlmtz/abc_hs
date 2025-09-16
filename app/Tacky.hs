module Tacky
  ( tack,
    TackyProg (..),
    TopLevel (..),
    Instruction (..),
    Value (..),
  )
where

import Control.Monad.RWS.Strict
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust)
import Parse (Const (..), StaticInit (..), Storage (..), Type (..), UnaryOp (..), VarType (..))
import qualified Parse as P
import TypeCheck (BinaryOp (..), IdentAttr (..))
import qualified TypeCheck as T

data TackyState = TackyState
  { varCt :: Int,
    labelCt :: Int,
    symbols :: SymbolMap
  }

type SymbolMap = M.Map String (Type, IdentAttr)

type TackyMonad = RWS SymbolMap [Instruction] TackyState

newtype TackyProg = TackyProg [TopLevel]
  deriving (Show)

data TopLevel
  = FuncDef String Bool [String] [Instruction]
  | StaticVar
      { vName :: String,
        vGlobal :: Bool,
        vType :: VarType,
        vInit :: StaticInit
      }
  deriving (Show)

data Instruction
  = Return Value
  | Unary UnaryOp Value Value
  | Binary BinaryOp Value Value Value
  | SignExtend Value Value
  | Truncate Value Value
  | Copy Value Value
  | Jump String
  | JZero Value String
  | JNZero Value String
  | Label String
  | FunctionCall String [Value] Value
  | ZeroExtend Value Value
  deriving (Show)

data Value = Constant P.Const | Var String
  deriving (Show)

tack :: (T.TypeProg, SymbolMap) -> Either String (TackyProg, SymbolMap)
tack (prog, syms) = Right (bar syms plotz)
  where
    plotz = runRWS (scan prog) M.empty (TackyState 0 0 syms)

bar :: SymbolMap -> (TackyProg, TackyState, [Instruction]) -> (TackyProg, SymbolMap)
bar syms (p, st, _) = (combo (TackyProg (convertSyms (M.assocs syms))) p, symbols st)

combo :: TackyProg -> TackyProg -> TackyProg
combo (TackyProg a) (TackyProg b) = TackyProg (b ++ a)

convertSyms :: [(String, (P.Type, IdentAttr))] -> [TopLevel]
convertSyms ((name, (TVar ty, StaticAttr (T.Initial i) global)) : syms) = StaticVar name global ty i : convertSyms syms
convertSyms ((name, (TVar ty, StaticAttr T.Tentative global)) : syms) = do
  let f = case ty of
        TInt -> StaticVar name global TInt (IntInit 0)
        TLong -> StaticVar name global TLong (LongInit 0)
        TUInt -> StaticVar name global TUInt (UIntInit 0)
        TULong -> StaticVar name global TULong (ULongInit 0)
  f : convertSyms syms
convertSyms (_ : syms) = convertSyms syms
convertSyms [] = []

scan :: T.TypeProg -> TackyMonad TackyProg
scan (T.TypeProg f) = TackyProg <$> mapM funcDef (filter fil f)
  where
    fil (T.FuncDecl _ _ _ _ x) = isJust x
    fil _ = False

funcDef :: T.Declaration -> TackyMonad TopLevel
funcDef (T.FuncDecl name params _ _ block) = do
  g <- attrGlobal name
  FuncDef name g params <$> case block of
    Just (T.Block items) ->
      snd <$> listen (mapM blockItem (items ++ [T.S (T.Return (T.Constant (P.ConstInt 0)))]))
    Nothing -> return []
funcDef _ = error "Shouldn't happen"

attrGlobal :: String -> TackyMonad Bool
attrGlobal name = do
  val <- gets (M.lookup name . symbols)
  case val of
    Just (_, StaticAttr _ a) -> return a
    Just (_, FunAttr _ a) -> return a
    _ -> return False

wrap :: T.Expr -> T.TypedExpr
wrap e = T.TypedExpr e TInt

blockItem :: T.BlockItem -> TackyMonad ()
blockItem (T.S s) = statement s
blockItem (T.D (T.VarDecl _ (Just Static) _ _)) = return ()
blockItem (T.D (T.VarDecl name _ t (Just v))) =
  void $ expr $ T.TypedExpr (T.Assignment (T.TypedExpr (T.Var name) t) (T.TypedExpr v t)) t
blockItem _ = return ()

statement :: T.Statement -> TackyMonad ()
statement (T.Return e) = tell . return . Return =<< expr (wrap e)
statement (T.Expression e) = void (expr (wrap e))
statement (T.Goto lbl) = tell [Jump lbl]
statement (T.Labelled lbl stmt) = tell [Label lbl] >> statement stmt
statement (T.If e1 e2 e3) = do
  cond <- expr (wrap e1)
  end <- tmpLabel "end"
  case e3 of
    Just e -> do
      elseLbl <- tmpLabel "else"
      tell [JZero cond elseLbl]
      statement e2
      tell [Jump end, Label elseLbl]
      statement e
    Nothing -> tell [JZero cond end] >> statement e2
  tell [Label end]
statement (T.Compound (T.Block items)) = mapM_ blockItem items
statement (T.Break name) = tell [Jump ("break_" ++ name)]
statement (T.Continue name) = tell [Jump ("continue_" ++ name)]
statement (T.Switch e s name cases) = switchStmt e s name cases
statement (T.DoWhile s e name) = do
  start <- tmpLabel "start"
  tell [Label start]
  statement s
  tell [Label ("continue_" ++ name)]
  cond <- expr (wrap e)
  tell [JNZero cond start, Label ("break_" ++ name)]
statement (T.While e s name) = do
  let contLbl = "continue_" ++ name
  let brkLbl = "break_" ++ name
  tell [Label contLbl]
  cond <- expr (wrap e)
  tell [JZero cond brkLbl]
  statement s
  tell [Jump contLbl, Label brkLbl]
statement (T.For i c p b name) = do
  let contLbl = "continue_" ++ name
  let brkLbl = "break_" ++ name
  start <- tmpLabel "start"
  initFor i
  tell [Label start]
  case c of
    Just e -> do
      v <- expr (wrap e)
      tell [JZero v brkLbl]
    Nothing -> return ()
  statement b
  tell [Label contLbl]
  maybe (return ()) (void . expr . wrap) p
  tell [Jump start, Label brkLbl]
statement _ = return ()

getType :: T.TypedExpr -> VarType
getType (T.TypedExpr _ t) = t

constType :: T.TypedExpr -> Integer -> Const
constType (T.TypedExpr _ TInt) = ConstInt
constType (T.TypedExpr _ TLong) = ConstLong
constType (T.TypedExpr _ TUInt) = ConstUInt
constType (T.TypedExpr _ TULong) = ConstULong

expr :: T.TypedExpr -> TackyMonad Value
expr (T.TypedExpr (T.Assignment (T.TypedExpr (T.Var v) _) right) _) = do
  rs <- expr right
  tell [Copy rs (Var v)]
  return (Var v)
expr (T.TypedExpr (T.Unary P.PreInc e) _) = incDec Add e False
expr (T.TypedExpr (T.Unary P.PostInc e) _) = incDec Add e True
expr (T.TypedExpr (T.Unary P.PreDec e) _) = incDec Subtract e False
expr (T.TypedExpr (T.Unary P.PostDec e) _) = incDec Subtract e True
expr (T.TypedExpr (T.Unary op e) t) = do
  dst <- tackyVar t
  src <- expr e
  tell [Unary op src dst]
  return dst
expr (T.TypedExpr (T.Binary LogAnd e1 e2) t) = do
  false <- tmpLabel "false"
  end <- tmpLabel "end"
  dst <- tackyVar t
  s1 <- expr e1
  tell [JZero s1 false]
  s2 <- expr e2
  tell
    [ JZero s2 false,
      Copy (Constant . ConstInt $ 1) dst,
      Jump end,
      Label false,
      Copy (Constant . ConstInt $ 0) dst,
      Label end
    ]
  return dst
expr (T.TypedExpr (T.Binary LogOr e1 e2) _) = do
  true <- tmpLabel "true"
  end <- tmpLabel "end"
  dst <- tackyVar (getType e1)
  s1 <- expr e1
  tell [JNZero s1 true]
  s2 <- expr e2
  tell
    [ JNZero s2 true,
      Copy (Constant . ConstInt $ 0) dst,
      Jump end,
      Label true,
      Copy (Constant . ConstInt $ 1) dst,
      Label end
    ]
  return dst
expr (T.TypedExpr (T.Binary op e1 e2) t) = do
  dst <- tackyVar t
  s1 <- expr e1
  s2 <- expr e2
  tell [Binary op s1 s2 dst]
  return dst
expr (T.TypedExpr (T.Constant c) _) = return (Constant c)
expr (T.TypedExpr (T.FunctionCall name args) t) = do
  dst <- tackyVar t
  es <- mapM expr args
  tell [FunctionCall name es dst]
  return dst
expr (T.TypedExpr (T.Var v) _) = return $ Var v
expr (T.TypedExpr (T.Conditional eCond eIf eElse) t) = do
  e2Label <- tmpLabel "e2_"
  end <- tmpLabel "end"
  ret <- tackyVar t
  cond <- expr eCond
  tell [JZero cond e2Label]
  ifIs <- expr eIf
  tell [Copy ifIs ret, Jump end, Label e2Label]
  elseIs <- expr eElse
  tell [Copy elseIs ret, Label end]
  return ret
expr (T.TypedExpr (T.Cast t1 e) _) = do
  ret <- expr e
  let t2 = getType e
  if t1 == t2
    then return ret
    else do
      dst <- tackyVar t1
      casting t1 t2 ret dst
      return dst
expr t = error $ "Invalid expression! " ++ show t

casting :: VarType -> VarType -> Value -> Value -> TackyMonad ()
casting t1 t2 ret dst
  | T.size t1 == T.size t2 = tell [Copy ret dst]
  | T.size t1 < T.size t2 = tell [Truncate ret dst]
  | T.signed t2 = tell [SignExtend ret dst]
  | otherwise = tell [ZeroExtend ret dst]

switchStmt :: T.Expr -> T.Statement -> [Char] -> [Maybe StaticInit] -> TackyMonad ()
switchStmt e s name cases = do
  cond <- expr (wrap e)
  mapM_ (makeCase cond name) (catMaybes cases)
  tell
    [ Jump
        ( if Nothing `notElem` cases
            then "break_" ++ name
            else name ++ ".default"
        )
    ]
  statement s
  tell [Label ("break_" ++ name)]

makeCase :: Value -> [Char] -> StaticInit -> TackyMonad ()
makeCase cond name (IntInit n) = do
  end <- tmpLabel "end"
  dst <- tackyVar TInt
  let lblName = if n >= 0 then name ++ "." ++ show n else name ++ ".m" ++ show (-n)
  tell
    [ Binary Equal cond (Constant (ConstInt (fromIntegral n))) dst,
      JZero dst end,
      Jump lblName,
      Label end
    ]
makeCase cond name (LongInit n) = do
  end <- tmpLabel "end"
  dst <- tackyVar TLong
  let lblName = if n >= 0 then name ++ "." ++ show n else name ++ ".m" ++ show (-n)
  tell
    [ Binary Equal cond (Constant (ConstLong (fromIntegral n))) dst,
      JZero dst end,
      Jump lblName,
      Label end
    ]
makeCase cond name (UIntInit n) = do
  end <- tmpLabel "end"
  dst <- tackyVar TUInt
  let lblName = if n >= 0 then name ++ "." ++ show n else name ++ ".m" ++ show (-n)
  tell
    [ Binary Equal cond (Constant (ConstUInt (fromIntegral n))) dst,
      JZero dst end,
      Jump lblName,
      Label end
    ]
makeCase cond name (ULongInit n) = do
  end <- tmpLabel "end"
  dst <- tackyVar TULong
  let lblName = if n >= 0 then name ++ "." ++ show n else name ++ ".m" ++ show (-n)
  tell
    [ Binary Equal cond (Constant (ConstULong (fromIntegral n))) dst,
      JZero dst end,
      Jump lblName,
      Label end
    ]

initFor :: T.ForInit -> TackyMonad ()
initFor (T.InitExpr (Just e)) = void (expr (wrap e))
initFor (T.InitExpr Nothing) = return ()
initFor (T.InitDecl d) = blockItem (T.D d)

incDec :: BinaryOp -> T.TypedExpr -> Bool -> TackyMonad Value
incDec op e post = do
  dst <- tackyVar (getType e)
  src <- expr e
  let middle = [Binary op src (Constant . constType e $ 1) dst, Copy dst src]
  if post
    then do
      ret <- tackyVar (getType e)
      tell $ Copy src ret : middle
      return ret
    else do
      tell middle
      return src

tackyVar :: VarType -> TackyMonad Value
tackyVar ty = do
  name <- tmpVar
  modify $ \x -> x {symbols = M.insert name (TVar ty, LocalAttr) (symbols x)}
  return $ Var name

tmpVar :: TackyMonad String
tmpVar = do
  idx <- gets (show . varCt)
  modify $ \x -> x {varCt = 1 + varCt x}
  return $ "tmp." ++ idx

tmpLabel :: String -> TackyMonad String
tmpLabel name = do
  idx <- gets (show . labelCt)
  modify $ \x -> x {labelCt = 1 + labelCt x}
  return $ "label_" ++ name ++ idx