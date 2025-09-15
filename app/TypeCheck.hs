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
  )
where

import Control.Monad.State
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)
import Parse (BinaryOp (..), Const (..), StaticInit (..), Storage (..), Type (..), UnaryOp (..))
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
    funType :: Maybe Type
  }

data TypedExpr = TypedExpr Expr Type
  deriving (Show)

data Expr
  = Constant Const
  | Unary UnaryOp TypedExpr
  | Binary BinaryOp TypedExpr TypedExpr
  | Var String
  | Cast Type TypedExpr
  | Assignment TypedExpr TypedExpr
  | CompoundAssignment BinaryOp TypedExpr TypedExpr
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
        vType :: Type,
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
  | Case Expr Statement
  | Default Statement
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

type TypeMonad m = State TypeState m

writeError :: String -> TypeMonad a
writeError s = modify (\x -> x {err = Just s}) *> error s

resolveType :: S.SProgram -> Either String (TypeProg, M.Map String (Type, IdentAttr))
resolveType prog = do
  let result = runState (typeProg prog) (TypeState M.empty Nothing False Nothing)
  case err (snd result) of
    Just e -> Left e
    Nothing -> Right (fst result, symbols (snd result))

typeProg :: S.SProgram -> TypeMonad TypeProg
typeProg (S.SProgram f) = TypeProg <$> mapM typeDecl f

evalConstant :: S.Expr -> Maybe StaticInit
evalConstant (S.Constant (ConstInt i)) = Just (IntInit (fromIntegral i))
evalConstant (S.Constant (ConstLong i)) = Just (LongInit (fromIntegral i))
evalConstant _ = Nothing

convertTo :: TypedExpr -> Type -> TypedExpr
convertTo ex@(TypedExpr _ oldType) newType =
  if oldType == newType
    then ex
    else TypedExpr (Cast newType ex) newType

commonType :: Type -> Type -> Type
commonType a b
  | a == b = a
  | otherwise = TLong

typeDecl :: S.Declaration -> TypeMonad Declaration
typeDecl (S.FuncDecl name params s t@(TFun pTypes ret) (Just (S.Block items))) = do
  scope <- gets blockScope
  when scope $ writeError "Nested function definition"
  syms <- gets symbols
  def <- case M.lookup name syms of
    Just (TFun args oldRet, FunAttr False _) -> do
      when (oldRet /= ret) $ writeError "Function return type mismatch"
      when (length args /= length pTypes) $ writeError "Incompatible function declaration"
      when (args /= pTypes) $ writeError "Function argument type mismatch"
      return True
    Just (_, FunAttr True _) -> writeError "Function is defined more than once"
    Just _ -> writeError "Incompatible function declaration"
    Nothing -> return True
  let syms' = foldl (\m k -> M.insert (snd k) (fst k, LocalAttr) m) syms (zip pTypes params)
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
  let syms' = foldl (\m k -> M.insert (snd k) (fst k, LocalAttr) m) syms (zip pTypes params)
  modify $ \x -> x {symbols = M.insert name (t, FunAttr def g) syms'}
  return $ FuncDecl name params s t Nothing
typeDecl (S.FuncDecl {}) = writeError "Var in fun decl"
typeDecl var@(S.VarDecl {}) = do
  scope <- gets blockScope
  if scope then blockVar var else fileVar var

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

fileVar :: S.Declaration -> TypeMonad Declaration
fileVar (S.VarDecl name s typ initial) = do
  let blankVal = if s == Just Extern then NoInitializer else Tentative
      value = maybe (Just blankVal) (fmap Initial . evalConstant) initial
  when (isNothing value) $ writeError "Cannot assign non-constant expr to file var"
  syms <- gets symbols
  newG <- getGlobal name s
  newV <- case M.lookup name syms of
    Just (oldType, at) -> do
      when (oldType /= typ) $ writeError "Type mismatch"
      case getConstant at of
        Just v -> case value of
          Just (Initial _) -> writeError "Conflicting file scope variable definitions"
          _ -> return v
        Nothing ->
          if not (isInitial (fromJust value)) && isTentative at
            then return Tentative
            else return $ fromJust value
    Nothing -> return $ fromJust value
  modify $ \x -> x {symbols = M.insert name (typ, StaticAttr newV newG) syms}
  VarDecl name s typ <$> ugly initial
fileVar _ = error "Unreachable"

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
    Just (t, at) -> do
      when (t /= TInt && t /= TLong) $ writeError "Function redeclared as variable"
      if s == Just Extern
        then return (attrGlobal at)
        else
          if attrGlobal at /= g
            then writeError "Conflicting variable linkage"
            else return g
    Nothing -> return g

blockVar :: S.Declaration -> TypeMonad Declaration
blockVar (S.VarDecl _ (Just Extern) _ (Just _)) = writeError "Cannot initialize extern var decl"
blockVar (S.VarDecl name (Just Extern) typ Nothing) = do
  syms <- gets symbols
  case M.lookup name syms of
    Just (TFun {}, _) -> writeError "Function redeclared as variable"
    Just (oldType, _) -> do
      when (oldType /= typ) $ writeError "Type mismatch"
      return $ VarDecl name (Just Extern) typ Nothing
    _ -> do
      modify $ \x -> x {symbols = M.insert name (typ, StaticAttr NoInitializer True) syms}
      return $ VarDecl name (Just Extern) typ Nothing
blockVar (S.VarDecl name (Just Static) t initial) = do
  let value = maybe (Just (IntInit 0)) evalConstant initial
  when (isNothing value) $ writeError "Cannot assign non-constant expr to static var"
  newV <- case fromJust value of
    IntInit x -> return $ if t == TLong then LongInit (fromIntegral x) else IntInit x
    LongInit x -> return $ if t == TLong then LongInit x else IntInit (fromIntegral x)
  modify $ \x -> x {symbols = M.insert name (t, StaticAttr (Initial newV) True) $ symbols x}
  return $ VarDecl name (Just Static) t Nothing
blockVar (S.VarDecl name s t initial) = do
  modify $ \x -> x {symbols = M.insert name (t, LocalAttr) $ symbols x}
  case initial of
    Just x -> do
      e <- typeExpr x
      return $ VarDecl name s t (Just (fst e))
    Nothing -> return $ VarDecl name s t Nothing
blockVar _ = error "Unreachable"

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
  s' <- typeStmt s
  let frotz t c = case c of
        IntInit x -> if t == TLong then LongInit (fromIntegral x) else IntInit x
        LongInit x -> if t == TLong then LongInit x else IntInit (fromIntegral x)
  let newV = map (fmap (frotz (snd e'))) cs
  when (nub newV /= newV) $ writeError "Duplicate case statement"
  return $ Switch (fst e') s' n newV
typeStmt (S.Case e s) = Case . fst <$> typeExpr e <*> typeStmt s
typeStmt (S.Default s) = Default <$> typeStmt s
typeStmt (S.DoWhile s e n) = do
  s' <- typeStmt s
  e' <- typeExpr e
  return $ DoWhile s' (fst e') n
typeStmt (S.While e s n) = do
  s' <- typeStmt s
  e' <- typeExpr e
  return $ While (fst e') s' n
typeStmt (S.For (S.InitDecl v@(S.VarDecl {})) e1 e2 s n) = do
  v' <- blockVar v
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

typeExpr :: S.Expr -> TypeMonad (Expr, Type)
typeExpr (S.FunctionCall name args) = do
  t <- gets (M.lookup name . symbols)
  case t of
    Just (TInt, _) -> writeError "Variable used as function name"
    Just (TFun pTypes ret, _) -> do
      when (length pTypes /= length args) $ writeError "Wrong number of args"
      args' <- mapM typeExpr args
      let convArgs = zipWith convertTo (map (uncurry TypedExpr) args') pTypes
      return (FunctionCall name convArgs, ret)
    _ -> writeError "This shouldn't happen"
typeExpr (S.Var v) = do
  t <- gets (M.lookup v . symbols)
  case t of
    Just (TInt, _) -> return (Var v, TInt)
    Just (TLong, _) -> return (Var v, TLong)
    Nothing -> writeError $ "This probably shouldn't happen" ++ show v
    _ -> writeError "Function name used as variable"
typeExpr (S.CompoundAssignment op e r) = do
  left <- typeExpr e
  right <- typeExpr r
  let convR = convertTo (uncurry TypedExpr right) (snd left)
  return (CompoundAssignment op (uncurry TypedExpr left) convR, snd left)
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
  return $
    if op == And || op == Or
      then (Binary op (uncurry TypedExpr left) (uncurry TypedExpr right), TInt)
      else
        if op == LeftShift || op == RightShift
          then (Binary op (uncurry TypedExpr left) (convertTo (uncurry TypedExpr right) (snd left)), snd left)
          else do
            let common = commonType (snd left) (snd right)
                convL = convertTo (uncurry TypedExpr left) common
                convR = convertTo (uncurry TypedExpr right) common
            ( Binary op convL convR,
              if op `elem` [Add, Subtract, Multiply, Divide, Remainder]
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
typeExpr (S.Cast ty e) = do
  t1 <- typeExpr e
  return (Cast ty (uncurry TypedExpr t1), ty)
