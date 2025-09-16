module TypeCheck
  ( resolveType,
    IdentAttr (..),
    InitValue (..),
    Expr (..),
    TypedExpr (..),
    Declaration (..),
    Statement (..),
    ForInit (..),
    Block (..),
    BlockItem (..),
    TypeProg (..),
    BinaryOp (..),
    size,
    signed,
    getStatic,
  )
where

import Control.Monad.RWS.Strict
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)
import Data.Void (Void)
import Parse (Const (..), StaticInit (..), Storage (..), Type (..), UnaryOp (..), VarType (..))
import qualified Parse as P
import qualified Semantic as S

data IdentAttr
  = FunAttr {fDef :: Bool, fGlobal :: Bool}
  | StaticAttr InitValue Bool
  | LocalAttr
  deriving (Eq, Show)

data InitValue = Tentative | Initial StaticInit | NoInitializer
  deriving (Eq, Show)

data TypeState = TypeState
  { symbols :: M.Map String (Type, IdentAttr),
    err :: Maybe String,
    blockScope :: Bool,
    funType :: Maybe VarType
  }

newtype TypeReader = TypeReader {switchType :: Maybe VarType}

data TypedExpr = TypedExpr Expr VarType
  deriving (Show)

data BinaryOp
  = Add
  | Subtract
  | Multiply
  | Divide
  | Remainder
  | And
  | Or
  | Xor
  | LeftShift
  | RightShift
  | RightLShift
  | LogAnd
  | LogOr
  | Equal
  | NotEqual
  | LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  deriving (Show, Eq)

data Expr
  = Constant Const
  | Unary UnaryOp TypedExpr
  | Binary BinaryOp TypedExpr TypedExpr
  | Var String
  | Cast VarType TypedExpr
  | Assignment TypedExpr TypedExpr
  | Conditional TypedExpr TypedExpr TypedExpr
  | FunctionCall String [TypedExpr]
  deriving (Show)

data Declaration
  = FuncDecl
      { fName :: String,
        fArgNames :: [String],
        fStorage :: Maybe Storage,
        fType :: Type,
        fBlock :: Maybe Block
      }
  | VarDecl
      { vName :: String,
        vStorage :: Maybe Storage,
        vType :: P.VarType,
        vInit :: Maybe Expr
      }
  deriving (Show)

newtype Block = Block [BlockItem]
  deriving (Show)

data ForInit = InitDecl Declaration | InitExpr (Maybe Expr)
  deriving (Show)

data Statement
  = Return Expr
  | Expression Expr
  | Goto String
  | Compound Block
  | If Expr Statement (Maybe Statement)
  | Switch Expr Statement String [Maybe StaticInit]
  | Labelled String Statement
  | Break String
  | Continue String
  | While Expr Statement String
  | DoWhile Statement Expr String
  | For ForInit (Maybe Expr) (Maybe Expr) Statement String
  | Null
  deriving (Show)

data BlockItem = S Statement | D Declaration
  deriving (Show)

newtype TypeProg = TypeProg [Declaration]
  deriving (Show)

type TypeMonad = RWS TypeReader [Void] TypeState

getStatic :: (Integral b) => StaticInit -> b
getStatic (IntInit x) = fromIntegral x
getStatic (LongInit x) = fromIntegral x
getStatic (UIntInit x) = fromIntegral x
getStatic (ULongInit x) = fromIntegral x

writeError :: String -> TypeMonad a
writeError s = modify (\x -> x {err = Just s}) *> error s

resolveType :: S.SProgram -> Either String (TypeProg, M.Map String (Type, IdentAttr))
resolveType prog = do
  let (prog', state', _) = runRWS (typeProg prog) (TypeReader Nothing) (TypeState M.empty Nothing False Nothing)
  case err state' of
    Just e -> Left e
    Nothing -> Right (prog', symbols state')

typeProg :: S.SProgram -> TypeMonad TypeProg
typeProg (S.SProgram f) = TypeProg <$> mapM typeDecl f

evalConstant :: S.Expr -> Maybe StaticInit
evalConstant (S.Constant (ConstInt i)) = Just (IntInit (fromIntegral i))
evalConstant (S.Constant (ConstLong i)) = Just (LongInit (fromIntegral i))
evalConstant (S.Constant (ConstUInt i)) = Just (UIntInit (fromIntegral i))
evalConstant (S.Constant (ConstULong i)) = Just (ULongInit (fromIntegral i))
evalConstant _ = Nothing

convertTo :: TypedExpr -> VarType -> TypedExpr
convertTo ex@(TypedExpr _ oldType) newType =
  if oldType == newType
    then ex
    else TypedExpr (Cast newType ex) newType

size :: VarType -> Int
size TInt = 4
size TUInt = 4
size _ = 8

signed :: VarType -> Bool
signed TInt = True
signed TLong = True
signed _ = False

commonType :: VarType -> VarType -> VarType
commonType a b
  | a == b = a
  | size a == size b = if signed a then b else a
  | size a > size b = a
  | otherwise = b

typeDecl :: S.Declaration -> TypeMonad Declaration
typeDecl (S.FuncDecl name params s t@(P.TFun pTypes ret) (Just (S.Block items))) = do
  scope <- gets blockScope
  when scope $ writeError "Nested function definition"
  syms <- gets symbols
  def <- case M.lookup name syms of
    Just (P.TFun args oldRet, FunAttr False _) -> do
      when (oldRet /= ret) $ writeError "Function return type mismatch"
      when (length args /= length pTypes) $ writeError "Incompatible function declaration"
      when (args /= pTypes) $ writeError "Function argument type mismatch"
      return True
    Just (_, FunAttr True _) -> writeError "Function is defined more than once"
    Just _ -> writeError "Incompatible function declaration"
    Nothing -> return True
  let syms' = foldl (\m k -> M.insert (snd k) (fst k, LocalAttr) m) syms (zip (map TVar pTypes) params)
  g <- funcGlobal name s
  modify $ \x -> x {symbols = M.insert name (t, FunAttr def g) syms'}
  modify $ \x -> x {blockScope = True, funType = Just ret}
  items' <- mapM typeItem items
  modify $ \x -> x {blockScope = False, funType = Nothing}
  return . FuncDecl name params s t . Just . Block $ items'
typeDecl (S.FuncDecl name params s t@(TFun pTypes ret) Nothing) = do
  syms <- gets symbols
  def <- case M.lookup name syms of
    Just (TFun a oldRet, FunAttr def _) -> do
      when (oldRet /= ret) $ writeError "Function return type mismatch"
      when (length a /= length pTypes) $ writeError "Incompatible function declaration"
      when (a /= pTypes) $ writeError "Function argument type mismatch"
      return def
    Just _ -> writeError "Incompatible function declaration"
    Nothing -> return False
  g <- funcGlobal name s
  let syms' = foldl (\m k -> M.insert (snd k) (fst k, LocalAttr) m) syms (zip (map TVar pTypes) params)
  modify $ \x -> x {symbols = M.insert name (t, FunAttr def g) syms'}
  return $ FuncDecl name params s t Nothing
typeDecl (S.FuncDecl {}) = writeError "Var in fun decl"
typeDecl (S.VarDecl a b c d) = do
  scope <- gets blockScope
  if scope then blockVar a b c d else fileVar a b c d

funcGlobal :: String -> Maybe Storage -> TypeMonad Bool
funcGlobal name s = do
  syms <- gets symbols
  let g = s /= Just Static
  case M.lookup name syms of
    Just (_, at) ->
      if s == Just Static && attrGlobal at
        then writeError "Static function declaration follows non-static"
        else return (attrGlobal at)
    Nothing -> return g

fileVar :: String -> Maybe Storage -> VarType -> Maybe S.Expr -> TypeMonad Declaration
fileVar name s typ initial = do
  let blankVal = if s == Just Extern then NoInitializer else Tentative
      value = maybe (Just blankVal) (fmap Initial . evalConstant) initial
  when (isNothing value) $ writeError "Cannot assign non-constant expr to file var"
  syms <- gets symbols
  newG <- getGlobal name s
  newV <- case M.lookup name syms of
    Just (oldType, at) -> do
      when (oldType /= TVar typ) $ writeError "Type mismatch"
      case getConstant at of
        Just v -> case value of
          Just (Initial _) -> writeError "Conflicting file scope variable definitions"
          _ -> return v
        Nothing ->
          if not (isInitial (fromJust value)) && isTentative at
            then return Tentative
            else return $ fromJust value
    Nothing -> return $ fromJust value
  modify $ \x -> x {symbols = M.insert name (TVar typ, StaticAttr newV newG) syms}
  VarDecl name s typ <$> ugly initial

isInitial :: InitValue -> Bool
isInitial (Initial _) = True
isInitial _ = False

isTentative :: IdentAttr -> Bool
isTentative (StaticAttr Tentative _) = True
isTentative _ = False

getConstant :: IdentAttr -> Maybe InitValue
getConstant (StaticAttr v@(Initial _) _) = Just v
getConstant _ = Nothing

attrGlobal :: IdentAttr -> Bool
attrGlobal (StaticAttr _ a) = a
attrGlobal (FunAttr _ a) = a
attrGlobal LocalAttr = error "This shouldn't happen"

getGlobal :: String -> Maybe Storage -> TypeMonad Bool
getGlobal name s = do
  let g = s /= Just Static
  syms <- gets symbols
  case M.lookup name syms of
    Just (TVar _, at) -> do
      if s == Just Extern
        then return (attrGlobal at)
        else
          if attrGlobal at /= g
            then writeError "Conflicting variable linkage"
            else return g
    Just (TFun {}, _) -> writeError "Function redeclared as variable"
    _ -> return g

convert :: VarType -> StaticInit -> StaticInit
convert TLong = LongInit . getStatic
convert TInt = IntInit . getStatic
convert TUInt = UIntInit . getStatic
convert TULong = ULongInit . getStatic

blockVar :: String -> Maybe Storage -> VarType -> Maybe S.Expr -> TypeMonad Declaration
blockVar _ (Just Extern) _ (Just _) = writeError "Cannot initialize extern var decl"
blockVar name (Just Extern) typ Nothing = do
  syms <- gets symbols
  case M.lookup name syms of
    Just (TFun {}, _) -> writeError "Function redeclared as variable"
    Just (TVar oldType, _) -> do
      when (oldType /= typ) $ writeError "Type mismatch"
      return $ VarDecl name (Just Extern) typ Nothing
    _ -> do
      modify $ \x -> x {symbols = M.insert name (TVar typ, StaticAttr NoInitializer True) syms}
      return $ VarDecl name (Just Extern) typ Nothing
blockVar name (Just Static) t initial = do
  let value = maybe (Just (IntInit 0)) evalConstant initial
  when (isNothing value) $ writeError "Cannot assign non-constant expr to static var"
  let newV = convert t (fromJust value)
  modify $ \x -> x {symbols = M.insert name (TVar t, StaticAttr (Initial newV) True) $ symbols x}
  return $ VarDecl name (Just Static) t Nothing
blockVar name s t initial = do
  modify $ \x -> x {symbols = M.insert name (TVar t, LocalAttr) $ symbols x}
  case initial of
    Just x -> do
      e <- typeExpr x
      let ex =
            if snd e /= t
              then Cast t (uncurry TypedExpr e)
              else fst e
      return $ VarDecl name s t (Just ex)
    Nothing -> return $ VarDecl name s t Nothing

typeItem :: S.BlockItem -> TypeMonad BlockItem
typeItem (S.S stmt) = S <$> typeStmt stmt
typeItem (S.D decl) = D <$> typeDecl decl

typeStmt :: S.Statement -> TypeMonad Statement
typeStmt (S.Labelled name stmt) = Labelled name <$> typeStmt stmt
typeStmt (S.Compound (S.Block items)) = Compound . Block <$> mapM typeItem items
typeStmt (S.If e1 s1 s2) = do
  e1' <- typeExpr e1
  s1' <- typeStmt s1
  s2' <- traverse typeStmt s2
  return $ If (fst e1') s1' s2'
typeStmt (S.Switch e s n cs) = do
  e' <- typeExpr e
  s' <- local (\l -> l {switchType = Just (snd e')}) $ typeStmt s
  let newV = map (fmap (convert (snd e'))) cs
  when (nub newV /= newV) $ writeError "Duplicate case statement"
  return $ Switch (fst e') s' n newV
typeStmt (S.Case lbl e s) = do
  ty <- asks (fromJust . switchType)
  let label = show $ convert ty e
      label' = if head label == '-' then "m" ++ tail label else label
  Labelled (lbl ++ "." ++ label') <$> typeStmt s
typeStmt (S.DoWhile s e n) = do
  s' <- typeStmt s
  e' <- typeExpr e
  return $ DoWhile s' (fst e') n
typeStmt (S.While e s n) = do
  s' <- typeStmt s
  e' <- typeExpr e
  return $ While (fst e') s' n
typeStmt (S.For (S.InitDecl (S.VarDecl a b c d)) e1 e2 s n) = do
  v' <- blockVar a b c d
  s' <- typeStmt s
  e1' <- ugly e1
  e2' <- ugly e2
  return $ For (InitDecl v') e1' e2' s' n
typeStmt (S.For (S.InitExpr e) e1 e2 s n) = do
  s' <- typeStmt s
  e' <- ugly e
  e1' <- ugly e1
  e2' <- ugly e2
  return $ For (InitExpr e') e1' e2' s' n
typeStmt (S.For {}) = writeError "Function declaration in for statement"
typeStmt (S.Return e) = do
  ft <- gets (fromJust . funType)
  e' <- typeExpr e
  return $ Return (Cast ft (uncurry TypedExpr e'))
typeStmt (S.Expression e) = Expression . fst <$> typeExpr e
typeStmt S.Null = return Null
typeStmt (S.Goto s) = return $ Goto s
typeStmt (S.Continue s) = return $ Continue s
typeStmt (S.Break s) = return $ Break s

ugly :: Maybe S.Expr -> TypeMonad (Maybe Expr)
ugly Nothing = return Nothing
ugly (Just e) = fmap (Just . fst) (typeExpr e)

binOp :: P.BinaryOp -> BinaryOp
binOp P.Add = Add
binOp P.Subtract = Subtract
binOp P.Multiply = Multiply
binOp P.And = And
binOp P.Or = Or
binOp P.Xor = Xor
binOp P.LeftShift = LeftShift
binOp P.RightShift = RightShift
binOp P.LogOr = LogOr
binOp P.LogAnd = LogAnd
binOp P.Divide = Divide
binOp P.Remainder = Remainder
binOp P.Equal = Equal
binOp P.NotEqual = NotEqual
binOp P.LessEqual = LessEqual
binOp P.LessThan = LessThan
binOp P.GreaterEqual = GreaterEqual
binOp P.GreaterThan = GreaterThan

typeExpr :: S.Expr -> TypeMonad (Expr, VarType)
typeExpr (S.FunctionCall name args) = do
  t <- gets (M.lookup name . symbols)
  case fromJust t of
    (TVar _, _) -> writeError "Variable used as function name"
    (TFun pTypes ret, _) -> do
      when (length pTypes /= length args) $ writeError "Wrong number of args"
      args' <- mapM typeExpr args
      let convArgs = zipWith convertTo (map (uncurry TypedExpr) args') pTypes
      return (FunctionCall name convArgs, ret)
typeExpr (S.Var v) = do
  t <- gets (M.lookup v . symbols)
  case t of
    Just (TVar x, _) -> return (Var v, x)
    _ -> writeError "Function name used as variable"
typeExpr (S.CompoundAssignment op e r) = do
  left <- typeExpr e
  right <- typeExpr (S.Binary op e r)
  let convR = convertTo (uncurry TypedExpr right) (snd left)
  return (Assignment (uncurry TypedExpr left) convR, snd left)
typeExpr (S.Assignment e r) = do
  left <- typeExpr e
  right <- typeExpr r
  let convR = convertTo (uncurry TypedExpr right) (snd left)
  return (Assignment (uncurry TypedExpr left) convR, snd left)
typeExpr (S.Unary op e) = do
  e' <- typeExpr e
  let ex = Unary op (uncurry TypedExpr e')
      retType = case op of
        Not -> TInt
        _ -> snd e'
  return (ex, retType)
typeExpr (S.Binary op e1 e2) = do
  left <- typeExpr e1
  right <- typeExpr e2
  let change RightShift ty = if signed ty then RightShift else RightLShift
      change o _ = o
  return $
    if op == P.LogAnd || op == P.LogOr
      then (Binary (binOp op) (uncurry TypedExpr left) (uncurry TypedExpr right), TInt)
      else
        if op == P.LeftShift || op == P.RightShift
          then (Binary (change (binOp op) (snd left)) (uncurry TypedExpr left) (convertTo (uncurry TypedExpr right) (snd left)), snd left)
          else do
            let common = commonType (snd left) (snd right)
                convL = convertTo (uncurry TypedExpr left) common
                convR = convertTo (uncurry TypedExpr right) common
            ( Binary (binOp op) convL convR,
              if op `elem` [P.Add, P.Subtract, P.Multiply, P.Divide, P.Remainder, P.And, P.Or, P.Xor]
                then common
                else TInt
              )
typeExpr (S.Conditional cond e1 e2) = do
  cond' <- typeExpr cond
  left <- typeExpr e1
  right <- typeExpr e2
  let common = commonType (snd left) (snd right)
      convL = convertTo (uncurry TypedExpr left) common
      convR = convertTo (uncurry TypedExpr right) common
  return (Conditional (uncurry TypedExpr cond') convL convR, common)
typeExpr (S.Constant e@(ConstInt _)) = return (Constant e, TInt)
typeExpr (S.Constant e@(ConstLong _)) = return (Constant e, TLong)
typeExpr (S.Constant e@(ConstUInt _)) = return (Constant e, TUInt)
typeExpr (S.Constant e@(ConstULong _)) = return (Constant e, TULong)
typeExpr (S.Cast ty e) = do
  t1 <- typeExpr e
  return (Cast ty (uncurry TypedExpr t1), ty)
