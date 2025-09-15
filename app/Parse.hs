{-# LANGUAGE InstanceSigs #-}
module Parse
(
  parser
, UnaryOp(..)
, BinaryOp(..)
, Expr(..)
, Declaration(..)
, Statement(..)
, ForInit(..)
, Block(..)
, BlockItem(..)
, Program(..)
, Storage(..)
, Type(..)
, Const(..)
, StaticInit(..)
) where

import Lex (CToken)
import qualified Lex as L

import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Data.Int

type TokenParser = Parsec Void [CToken]
type MayError = Either String

data StaticInit = IntInit Int32 | LongInit Int64
    deriving (Eq)
instance Show StaticInit where show :: StaticInit -> String
                               show = showStatic
showStatic :: StaticInit -> String
showStatic (IntInit x) = show x
showStatic (LongInit x) = show x

data UnaryOp = Complement | Negate | Not | PreInc | PostInc | PreDec | PostDec
    deriving (Show)
data BinaryOp = Add | Subtract | Multiply | Divide | Remainder
              | And | Or | Xor | LeftShift | RightShift
              | LogAnd | LogOr | Equal | NotEqual | LessThan | LessEqual
              | GreaterThan | GreaterEqual
    deriving (Show, Eq)
data Storage = Static | Extern
    deriving (Show, Eq)
data Const = ConstInt Integer | ConstLong Integer
    deriving (Show, Eq)
data Expr = Constant Const
          | Unary UnaryOp Expr
          | Binary BinaryOp Expr Expr
          | Var String
          | Cast Type Expr
          | Assignment Expr Expr
          | CompoundAssignment BinaryOp Expr Expr
          | Conditional Expr Expr Expr
          | FunctionCall String [Expr]
    deriving (Show)
data Declaration = FuncDecl { fName :: String
                            , fArgNames :: [String]
                            , fStorage :: Maybe Storage
                            , fType :: Type
                            , fBlock :: Maybe Block }
                 | VarDecl { vName :: String
                           , vStorage :: Maybe Storage
                           , vType :: Type
                           , vInit :: Maybe Expr }
    deriving (Show)
data Type = TInt | TLong 
    | TFun { fArgTypes :: [Type]
           , fRet :: Type }
    deriving (Show, Eq)
newtype Block = Block [BlockItem]
    deriving (Show)
data ForInit = InitDecl Declaration | InitExpr (Maybe Expr)
    deriving (Show)
data Statement = Return Expr
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
newtype Program = Program [Declaration]
    deriving (Show)

parser :: [CToken] -> MayError Program
parser = either (Left . show) Right . parse (program <* eof) "abc_parse"

isToken :: MonadParsec e s m => Token s -> m (Token s)
isToken t = satisfy (== t)

getIdent :: TokenParser String
getIdent = getStr <$> satisfy isIdent
    where getStr (L.Identifier x) = x
          getStr _ = error "Impossible"
          isIdent (L.Identifier _) = True
          isIdent _ = False

program :: TokenParser Program
program = Program <$> many declaration

function :: TokenParser Declaration
function = do
    specs <- some specifier
    name <- getIdent
    ps <- isToken L.LeftParen *> params <* isToken L.RightParen
    body <- (Just <$> block) <|> (isToken L.Semicolon >> return Nothing)
    let (_, long, storage) = foldl foldSpec (False, False, Nothing) specs
        types = map fst ps
        names = map snd ps
        t = if long then TLong else TInt
    return $ FuncDecl name names storage (TFun types t) body

params :: TokenParser [(Type, String)]
params = [] <$ isToken L.Void
     <|> sepBy1 param (isToken L.Comma)

param :: TokenParser (Type, String)
param = do
    t <- some (isToken L.Int <|> isToken L.Long)
    i <- getIdent
    return (checkType t, i)

checkType :: [CToken] -> Type
checkType [L.Int] = TInt
checkType [L.Long] = TLong
checkType [L.Int, L.Long] = TLong
checkType [L.Long, L.Int] = TLong
checkType _ = error "Invalid type specifier"

blockItem :: TokenParser BlockItem
blockItem = D <$> declaration <|> S <$> statement

block :: TokenParser Block
block = Block <$> (isToken L.LeftBrace *> many blockItem <* isToken L.RightBrace)

declaration :: TokenParser Declaration
declaration = try variable <|> function

variable :: TokenParser Declaration
variable = do
    specs <- some specifier
    name <- getIdent
    assign <- optional (isToken L.Equal *> expr)
    _ <- isToken L.Semicolon
    let (nt, long, storage) = foldl foldSpec (False, False, Nothing) specs
        t = if long then TLong else TInt
    if not (nt || long) then error "No type" else
        return $ VarDecl name storage t assign

foldSpec :: (Bool, Bool, Maybe Storage) ->  (Bool, Bool, Maybe Storage) -> (Bool, Bool, Maybe Storage)
foldSpec (False, b, c) (True, _, _) = (True, b, c)
foldSpec (a, False, c) (_, True, _) = (a, True, c)
foldSpec (a, b, Nothing) (_, _, Just x) = (a, b, Just x)
foldSpec _ _ = error "Invalid specifier"

specifier :: TokenParser (Bool, Bool, Maybe Storage)
specifier =
        (True, False, Nothing) <$ isToken L.Int
    <|> (False, True, Nothing) <$ isToken L.Long
    <|> (False, False, Just Static) <$ isToken L.Static
    <|> (False, False, Just Extern) <$ isToken L.Extern

statement :: TokenParser Statement
statement = try labelStmt
        <|> Compound <$> block
        <|> Return <$> (isToken L.Return *> expr <* isToken L.Semicolon)
        <|> ifStmt <|> goto
        <|> Expression <$> expr <* isToken L.Semicolon
        <|> Null <$ isToken L.Semicolon
        <|> breakStmt <|> continueStmt
        <|> whileStmt <|> doWhileStmt <|> forStmt
        <|> switchStmt <|> caseStmt <|> defaultStmt
    where
          labelStmt = do
            s <- getIdent <* isToken L.Colon
            Labelled s <$> statement
          goto = do
            s <- isToken L.Goto *> getIdent <* isToken L.Semicolon
            return $ Goto s

switchStmt :: TokenParser Statement
switchStmt = do
    e <- isToken L.Switch *> isToken L.LeftParen *> expr <* isToken L.RightParen
    s <- statement
    return $ Switch e s "" []

caseStmt :: TokenParser Statement
caseStmt = do
    e <- isToken L.Case *> ternary <* isToken L.Colon
    Case e <$> statement

defaultStmt :: TokenParser Statement
defaultStmt = do
    e <- isToken L.Default *> isToken L.Colon *> statement
    return $ Default e

breakStmt :: TokenParser Statement
breakStmt = do
    _ <- isToken L.Break <* isToken L.Semicolon
    return $ Break ""

continueStmt :: TokenParser Statement
continueStmt = do
    _ <- isToken L.Continue <* isToken L.Semicolon
    return $ Continue ""

ifStmt :: TokenParser Statement
ifStmt = do
    e <- isToken L.If *> isToken L.LeftParen *> expr <* isToken L.RightParen
    s1 <- statement
    s2 <- optional (isToken L.Else *> statement)
    return $ If e s1 s2

whileStmt :: TokenParser Statement
whileStmt = do
    e <- isToken L.While *> isToken L.LeftParen *> expr <* isToken L.RightParen
    s <- statement
    return $ While e s ""

doWhileStmt :: TokenParser Statement
doWhileStmt = do
    s <- isToken L.Do *> statement
    e <- isToken L.While *> isToken L.LeftParen *> expr <* isToken L.RightParen <* isToken L.Semicolon
    return $ DoWhile s e ""

forInit :: TokenParser ForInit
forInit = InitDecl <$> variable
      <|> InitExpr <$> optional expr <* isToken L.Semicolon

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
term' = constant
    <|> lConstant
    <|> var
    <|> unary
    <|> try cast
    <|> between (isToken L.LeftParen) (isToken L.RightParen) expr

cast :: TokenParser Expr
cast = do
    t <- between (isToken L.LeftParen) (isToken L.RightParen) $ some (isToken L.Int <|> isToken L.Long)
    Cast (checkType t) <$> term

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
    return $ if getConstant c + 1 > (^) (2 :: Integer) (31 :: Integer)
        then Constant (ConstLong (getConstant c))
        else Constant (ConstInt (getConstant c))

lConstant :: TokenParser Expr
lConstant = do
    let isConstant (L.LConstant _) = True
        isConstant _ = False
        getConstant (L.LConstant x) = x
        getConstant _ = 0
    Constant . ConstLong . getConstant <$> satisfy isConstant

unaryPostfix :: TokenParser UnaryOp
unaryPostfix = PostInc <$ isToken L.PlusPlus
           <|> PostDec <$ isToken L.MinusMinus

unary :: TokenParser Expr
unary = do
    tok <- PreInc <$ isToken L.PlusPlus
       <|> PreDec <$ isToken L.MinusMinus
       <|> Negate <$ isToken L.Minus
       <|> Complement <$ isToken L.Tilde
       <|> Not <$ isToken L.Bang
    Unary tok <$> term

precedence :: [[Operator TokenParser Expr]]
precedence = [ [ binary  L.Star         (Binary Multiply)
               , binary  L.Slash        (Binary Divide)
               , binary  L.Percent      (Binary Remainder)]
             , [ binary  L.Plus         (Binary Add)
               , binary  L.Minus        (Binary Subtract)]
             , [ binary  L.LeftShift    (Binary LeftShift)
               , binary  L.RightShift   (Binary RightShift)]
             , [ binary  L.LessThan     (Binary LessThan)
               , binary  L.GreaterThan  (Binary GreaterThan)
               , binary  L.LessEqual    (Binary LessEqual)
               , binary  L.GreaterEqual (Binary GreaterEqual)]
             , [ binary  L.EqualEqual   (Binary Equal)
               , binary  L.BangEqual    (Binary NotEqual)]
             , [ binary  L.And          (Binary And)]
             , [ binary  L.Caret        (Binary Xor)]
             , [ binary  L.Pipe         (Binary Or)]
             , [ binary  L.AndAnd       (Binary LogAnd)]
             , [ binary  L.PipePipe     (Binary LogOr)]]

assignment :: [[Operator TokenParser Expr]]
assignment = [ [ binaryR L.Equal        Assignment
               , binaryR L.PlusEqual    (CompoundAssignment Add)
               , binaryR L.MinusEqual   (CompoundAssignment Subtract)
               , binaryR L.StarEqual    (CompoundAssignment Multiply)
               , binaryR L.SlashEqual   (CompoundAssignment Divide)
               , binaryR L.PercentEqual (CompoundAssignment Remainder)
               , binaryR L.AndEqual     (CompoundAssignment And)
               , binaryR L.OrEqual      (CompoundAssignment Or)
               , binaryR L.XorEqual     (CompoundAssignment Xor)
               , binaryR L.LeftShiftEqual (CompoundAssignment LeftShift)
               , binaryR L.RightShiftEqual (CompoundAssignment RightShift)]]

binary :: MonadParsec e s m => Token s -> (a -> a -> a) -> Operator m a
binary name f = InfixL (f <$ isToken name)
binaryR :: MonadParsec e s m => Token s -> (a -> a -> a) -> Operator m a
binaryR name f = InfixR (f <$ isToken name)