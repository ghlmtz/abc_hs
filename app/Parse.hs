{-# LANGUAGE InstanceSigs #-}

module Parse
  ( parser,
    UnaryOp (..),
    BinaryOp (..),
    Expr (..),
    Declaration (..),
    Statement (..),
    ForInit (..),
    Block (..),
    BlockItem (..),
    Program (..),
    Storage (..),
    Type (..),
    Const (..),
    VarType (..),
  )
where

import Control.Monad.Combinators.Expr
import Data.List (partition)
import Data.Maybe (isJust)
import Data.Void (Void)
import Lex (CToken)
import qualified Lex as L
import Text.Megaparsec

type TokenParser = Parsec Void [CToken]

type MayError = Either String

data UnaryOp = Complement | Negate | Not | PreInc | PostInc | PreDec | PostDec
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
  | LogAnd
  | LogOr
  | Equal
  | NotEqual
  | LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  deriving (Show, Eq)

data Storage = Static | Extern
  deriving (Show, Eq)

data Const = ConstInt Integer | ConstLong Integer | ConstUInt Integer | ConstULong Integer | ConstDouble Double
  deriving (Show, Eq)

data Expr
  = Constant Const
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr
  | Var String
  | Cast VarType Expr
  | Assignment Expr Expr
  | CompoundAssignment BinaryOp Expr Expr
  | Conditional Expr Expr Expr
  | FunctionCall String [Expr]
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
        vType :: VarType,
        vInit :: Maybe Expr
      }
  deriving (Show)

data VarType
  = TInt
  | TLong
  | TUInt
  | TULong
  | TDouble
  deriving (Show, Eq)

data Type
  = TVar VarType
  | TFun
      { fArgTypes :: [VarType],
        fRet :: VarType
      }
  deriving (Show, Eq)

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
  | Switch Expr Statement
  | Labelled String Statement
  | Case Expr Statement
  | Default Statement
  | Break
  | Continue
  | While Expr Statement
  | DoWhile Statement Expr
  | For ForInit (Maybe Expr) (Maybe Expr) Statement String
  | Null
  deriving (Show)

data BlockItem = S Statement | D Declaration
  deriving (Show)

newtype Program = Program [Declaration]
  deriving (Show)

data Sign = Signed | Unsigned

parser :: [CToken] -> MayError Program
parser = either (Left . show) Right . parse (program <* eof) "abc_parse"

isToken :: (MonadParsec e s m) => Token s -> m (Token s)
isToken t = satisfy (== t)

getIdent :: TokenParser String
getIdent = getStr <$> satisfy isIdent
  where
    getStr (L.Identifier x) = x
    getStr _ = error "Impossible"
    isIdent (L.Identifier _) = True
    isIdent _ = False

program :: TokenParser Program
program = Program <$> many declaration

typeCalc :: Bool -> Maybe Sign -> VarType
typeCalc long sign =
  let t = if long then TLong else TInt
   in case sign of
        Just Unsigned -> if t == TLong then TULong else TUInt
        _ -> t

function :: TokenParser Declaration
function = do
  specs <- types
  name <- getIdent
  ps <- isToken L.LeftParen *> params <* isToken L.RightParen
  body <- (Just <$> block) <|> (isToken L.Semicolon >> return Nothing)
  let ts = map fst ps
      names = map snd ps
  return $ FuncDecl name names (snd specs) (TFun ts (fst specs)) body

params :: TokenParser [(VarType, String)]
params =
  []
    <$ isToken L.Void
      <|> sepBy1 param (isToken L.Comma)

checkDouble :: [CToken] -> TokenParser VarType
checkDouble l
  | length l == 1 && head l == L.Double = return TDouble
  | L.Double `elem` l = error "Double with other type"
  | otherwise = do
      let specs = map typeSpecifier l
      let (nt, long, sign) = foldl foldSpec (False, False, Nothing) specs
      if not (nt || long || isJust sign)
        then error "No type"
        else return $ typeCalc long sign

param :: TokenParser (VarType, String)
param = do
  ts <- types
  i <- getIdent
  if isJust (snd ts)
    then error "Storage specifier in function param"
    else return (fst ts, i)

blockItem :: TokenParser BlockItem
blockItem = D <$> declaration <|> S <$> statement

block :: TokenParser Block
block = Block <$> (isToken L.LeftBrace *> many blockItem <* isToken L.RightBrace)

declaration :: TokenParser Declaration
declaration = try variable <|> function

variable :: TokenParser Declaration
variable = do
  specs <- types
  name <- getIdent
  assign <- optional (isToken L.Equal *> expr)
  _ <- isToken L.Semicolon
  return $ VarDecl name (snd specs) (fst specs) assign

foldSpec :: (Bool, Bool, Maybe Sign) -> (Bool, Bool, Maybe Sign) -> (Bool, Bool, Maybe Sign)
foldSpec (False, b, c) (True, _, _) = (True, b, c)
foldSpec (a, False, c) (_, True, _) = (a, True, c)
foldSpec (a, b, Nothing) (_, _, Just x) = (a, b, Just x)
foldSpec _ _ = error "Invalid specifier"

typeSpecifier :: CToken -> (Bool, Bool, Maybe Sign)
typeSpecifier L.Int = (True, False, Nothing)
typeSpecifier L.Long = (False, True, Nothing)
typeSpecifier L.Signed = (False, False, Just Signed)
typeSpecifier L.Unsigned = (False, False, Just Unsigned)
typeSpecifier _ = error "Invalid type"

checkStorage :: (Monad m) => [CToken] -> m (Maybe Storage)
checkStorage l
  | length l == 1 && head l == L.Static = return $ Just Static
  | length l == 1 && head l == L.Extern = return $ Just Extern
  | null l = return Nothing
  | otherwise = error "Storage specifier with other storage specifier"

types :: TokenParser (VarType, Maybe Storage)
types = do
  specs <- some specifier
  let typeSpecifiers t = t `elem` [L.Int, L.Long, L.Signed, L.Unsigned, L.Double]
      foo = partition typeSpecifiers specs
  bar <- checkDouble (fst foo)
  baz <- checkStorage (snd foo)
  return (bar, baz)

specifier :: TokenParser CToken
specifier = isToken L.Int <|> isToken L.Long <|> isToken L.Signed <|> isToken L.Unsigned <|> isToken L.Double <|> isToken L.Static <|> isToken L.Extern

statement :: TokenParser Statement
statement =
  try labelStmt
    <|> Compound
    <$> block
      <|> Return
    <$> (isToken L.Return *> expr <* isToken L.Semicolon)
      <|> ifStmt
      <|> Goto
    <$> (isToken L.Goto *> getIdent <* isToken L.Semicolon)
      <|> Expression
    <$> expr
    <* isToken L.Semicolon
      <|> Null
    <$ isToken L.Semicolon
      <|> breakStmt
      <|> continueStmt
      <|> whileStmt
      <|> doWhileStmt
      <|> forStmt
      <|> switchStmt
      <|> caseStmt
      <|> defaultStmt
  where
    labelStmt = Labelled <$> getIdent <* isToken L.Colon <*> statement

switchStmt :: TokenParser Statement
switchStmt = do
  e <- isToken L.Switch *> isToken L.LeftParen *> expr <* isToken L.RightParen
  Switch e <$> statement

caseStmt :: TokenParser Statement
caseStmt = Case <$> (isToken L.Case *> ternary <* isToken L.Colon) <*> statement

defaultStmt :: TokenParser Statement
defaultStmt = Default <$> (isToken L.Default *> isToken L.Colon *> statement)

breakStmt :: TokenParser Statement
breakStmt = Break <$ isToken L.Break <* isToken L.Semicolon

continueStmt :: TokenParser Statement
continueStmt = Continue <$ isToken L.Continue <* isToken L.Semicolon

ifStmt :: TokenParser Statement
ifStmt = do
  e <- isToken L.If *> isToken L.LeftParen *> expr <* isToken L.RightParen
  s1 <- statement
  s2 <- optional (isToken L.Else *> statement)
  return $ If e s1 s2

whileStmt :: TokenParser Statement
whileStmt = do
  e <- isToken L.While *> isToken L.LeftParen *> expr <* isToken L.RightParen
  While e <$> statement

doWhileStmt :: TokenParser Statement
doWhileStmt = do
  s <- isToken L.Do *> statement
  e <- isToken L.While *> isToken L.LeftParen *> expr <* isToken L.RightParen <* isToken L.Semicolon
  return $ DoWhile s e

forInit :: TokenParser ForInit
forInit =
  InitDecl
    <$> variable
      <|> InitExpr
    <$> optional expr
    <* isToken L.Semicolon

forStmt :: TokenParser Statement
forStmt = do
  i <- isToken L.For *> isToken L.LeftParen *> forInit
  e1 <- optional expr <* isToken L.Semicolon
  e2 <- optional expr <* isToken L.RightParen
  s <- statement
  return $ For i e1 e2 s ""

expr :: TokenParser Expr
expr = makeExprParser ternary assignment

ternary :: TokenParser Expr
ternary = do
  e1 <- makeExprParser term precedence
  trinary e1 <|> return e1
  where
    trinary e1 = do
      e2 <- isToken L.Question *> expr <* isToken L.Colon
      Conditional e1 e2 <$> ternary

term :: TokenParser Expr
term = do
  t <- term'
  maybe t (foldr Unary t) <$> optional (many unaryPostfix)

term' :: TokenParser Expr
term' =
  constant
    <|> lConstant
    <|> uConstant
    <|> ulConstant
    <|> doubleConstant
    <|> var
    <|> unary
    <|> try cast
    <|> between (isToken L.LeftParen) (isToken L.RightParen) expr

cast :: TokenParser Expr
cast = do
  t <- between (isToken L.LeftParen) (isToken L.RightParen) types
  if isJust (snd t)
    then error "Storage class specifier in cast"
    else Cast (fst t) <$> term

var :: TokenParser Expr
var = do
  ident <- getIdent
  call <- optional (isToken L.LeftParen *> sepBy expr (isToken L.Comma) <* isToken L.RightParen)
  return $ maybe (Var ident) (FunctionCall ident) call

constant :: TokenParser Expr
constant = do
  let isConstant (L.Constant _) = True
      isConstant _ = False
      getConstant (L.Constant x) = x
      getConstant _ = 0
  c <- satisfy isConstant
  return $
    if getConstant c + 1 > (^) (2 :: Integer) (31 :: Integer)
      then Constant (ConstLong (getConstant c))
      else Constant (ConstInt (getConstant c))

doubleConstant :: TokenParser Expr
doubleConstant = do
  let isConstant (L.Float _) = True
      isConstant _ = False
      getConstant (L.Float x) = x
      getConstant _ = 0
  Constant . ConstDouble . getConstant <$> satisfy isConstant

lConstant :: TokenParser Expr
lConstant = do
  let isConstant (L.LConstant _) = True
      isConstant _ = False
      getConstant (L.LConstant x) = x
      getConstant _ = 0
  Constant . ConstLong . getConstant <$> satisfy isConstant

uConstant :: TokenParser Expr
uConstant = do
  let isConstant (L.UConstant _) = True
      isConstant _ = False
      getConstant (L.UConstant x) = x
      getConstant _ = 0
  c <- satisfy isConstant
  return $
    if getConstant c + 1 > (^) (2 :: Integer) (32 :: Integer)
      then Constant (ConstULong (getConstant c))
      else Constant (ConstUInt (getConstant c))

ulConstant :: TokenParser Expr
ulConstant = do
  let isConstant (L.ULConstant _) = True
      isConstant _ = False
      getConstant (L.ULConstant x) = x
      getConstant _ = 0
  Constant . ConstULong . getConstant <$> satisfy isConstant

unaryPostfix :: TokenParser UnaryOp
unaryPostfix =
  PostInc
    <$ isToken L.PlusPlus
      <|> PostDec
    <$ isToken L.MinusMinus

unary :: TokenParser Expr
unary = do
  tok <-
    PreInc
      <$ isToken L.PlusPlus
        <|> PreDec
      <$ isToken L.MinusMinus
        <|> Negate
      <$ isToken L.Minus
        <|> Complement
      <$ isToken L.Tilde
        <|> Not
      <$ isToken L.Bang
  Unary tok <$> term

precedence :: [[Operator TokenParser Expr]]
precedence =
  [ [ binary L.Star (Binary Multiply),
      binary L.Slash (Binary Divide),
      binary L.Percent (Binary Remainder)
    ],
    [ binary L.Plus (Binary Add),
      binary L.Minus (Binary Subtract)
    ],
    [ binary L.LeftShift (Binary LeftShift),
      binary L.RightShift (Binary RightShift)
    ],
    [ binary L.LessThan (Binary LessThan),
      binary L.GreaterThan (Binary GreaterThan),
      binary L.LessEqual (Binary LessEqual),
      binary L.GreaterEqual (Binary GreaterEqual)
    ],
    [ binary L.EqualEqual (Binary Equal),
      binary L.BangEqual (Binary NotEqual)
    ],
    [binary L.And (Binary And)],
    [binary L.Caret (Binary Xor)],
    [binary L.Pipe (Binary Or)],
    [binary L.AndAnd (Binary LogAnd)],
    [binary L.PipePipe (Binary LogOr)]
  ]

assignment :: [[Operator TokenParser Expr]]
assignment =
  [ [ binaryR L.Equal Assignment,
      binaryR L.PlusEqual (CompoundAssignment Add),
      binaryR L.MinusEqual (CompoundAssignment Subtract),
      binaryR L.StarEqual (CompoundAssignment Multiply),
      binaryR L.SlashEqual (CompoundAssignment Divide),
      binaryR L.PercentEqual (CompoundAssignment Remainder),
      binaryR L.AndEqual (CompoundAssignment And),
      binaryR L.OrEqual (CompoundAssignment Or),
      binaryR L.XorEqual (CompoundAssignment Xor),
      binaryR L.LeftShiftEqual (CompoundAssignment LeftShift),
      binaryR L.RightShiftEqual (CompoundAssignment RightShift)
    ]
  ]

binary :: (MonadParsec e s m) => Token s -> (a -> a -> a) -> Operator m a
binary name f = InfixL (f <$ isToken name)

binaryR :: (MonadParsec e s m) => Token s -> (a -> a -> a) -> Operator m a
binaryR name f = InfixR (f <$ isToken name)