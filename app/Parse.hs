module Parse
(
  parser
, UnaryOp(..)
, BinaryOp(..)
, Expr(..)
, Declaration(..)
, Statement(..)
, Block(..)
, BlockItem(..)
, Function(..)
, Program(..)
) where

import Lex (CToken)
import qualified Lex as L

import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Data.Void (Void)

type TokenParser = Parsec Void [CToken]
type MayError = Either String 

data UnaryOp = Complement | Negate | Not | PreInc | PostInc | PreDec | PostDec
    deriving (Show)
data BinaryOp = Add | Subtract | Multiply | Divide | Remainder
              | And | Or | Xor | LeftShift | RightShift
              | LogAnd | LogOr | Equal | NotEqual | LessThan | LessEqual
              | GreaterThan | GreaterEqual
    deriving (Show, Eq)
data Expr = Int Integer 
           | Unary UnaryOp Expr
           | Binary BinaryOp Expr Expr
           | Var String
           | Assignment Expr Expr
           | CompoundAssignment BinaryOp Expr Expr
           | Conditional Expr Expr Expr
    deriving (Show)
data Declaration = Declaration String (Maybe Expr)
    deriving (Show)
newtype Block = Block [BlockItem]
    deriving (Show)
data Statement = Return Expr
               | Expression Expr
               | Goto String
               | Compound Block
               | If Expr Statement (Maybe Statement)
               | Labelled String Statement
               | Null
    deriving (Show)
data BlockItem = S Statement | D Declaration
    deriving (Show)
data Function = Function String Block
    deriving (Show)
newtype Program = Program Function
    deriving (Show)

parser :: [CToken] -> MayError Program
parser toks = case parse (program <* eof) "abc_parse" toks of
        Left e -> Left (show e)
        Right e -> Right e

isToken :: MonadParsec e s m => Token s -> m (Token s)
isToken t = satisfy (== t)

getIdent :: CToken -> String
getIdent (L.Identifier x) = x
getIdent _ = ""

isIdent :: CToken -> Bool
isIdent (L.Identifier _) = True
isIdent _ = False

program :: TokenParser Program
program = Program <$> function

function :: TokenParser Function
function = do
    name <- isToken L.Int *> satisfy isIdent
    body <- isToken L.LeftParen *> isToken L.Void *> isToken L.RightParen *> block
    return $ Function (getIdent name) body

blockItem :: TokenParser BlockItem
blockItem = D <$> declaration <|> S <$> statement

block :: TokenParser Block
block = do
    items <- isToken L.LeftBrace *> many blockItem <* isToken L.RightBrace
    return $ Block items

declaration :: TokenParser Declaration
declaration = do
    name <- isToken L.Int *> satisfy isIdent
    assign <- optional (isToken L.Equal *> expr)
    _ <- isToken L.Semicolon
    return $ Declaration (getIdent name) assign

statement :: TokenParser Statement
statement =  try labelStmt <|> Compound <$> block <|> ret <|> ifStmt <|> goto <|> Expression <$> expr <* isToken L.Semicolon <|> semicolon
    where semicolon = do
            _ <- isToken L.Semicolon
            return Null
          ret = Return <$> (isToken L.Return *> expr <* isToken L.Semicolon)
          labelStmt = do
            s <- satisfy isIdent <* isToken L.Colon
            Labelled (getIdent s) <$> statement
          goto = do
            s <- isToken L.Goto *> satisfy isIdent <* isToken L.Semicolon
            return $ Goto (getIdent s)

ifStmt :: TokenParser Statement
ifStmt = do
    e <- isToken L.If *> isToken L.LeftParen *> expr <* isToken L.RightParen
    s1 <- statement
    s2 <- optional (isToken L.Else *> statement)
    return $ If e s1 s2

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
    un <- optional unaryPostfix
    case un of
        Just post -> return $ Unary post t
        Nothing -> return t

term' :: TokenParser Expr
term' = constant
   <|> var
   <|> unary
   <|> between (isToken L.LeftParen) (isToken L.RightParen) expr

var :: TokenParser Expr
var = Var . getIdent <$> satisfy isIdent

constant :: TokenParser Expr
constant = do
    let isConstant (L.Constant _) = True
        isConstant _ = False
        getConstant (L.Constant x) = x
        getConstant _ = 0
    Int . getConstant <$> satisfy isConstant

unaryPostfix :: TokenParser UnaryOp
unaryPostfix = do
    tok <- isToken L.PlusPlus <|> isToken L.MinusMinus
    return $ case tok of
                  L.PlusPlus -> PostInc
                  L.MinusMinus -> PostDec
                  _ -> error "Invalid token"

unary :: TokenParser Expr
unary = do
    tok <- isToken L.PlusPlus <|> isToken L.MinusMinus <|> isToken L.Minus <|> isToken L.Tilde <|> isToken L.Bang
    let start = case tok of
                    L.PlusPlus -> PreInc
                    L.MinusMinus -> PreDec
                    L.Minus -> Negate
                    L.Bang  -> Not
                    _     -> Complement
    Unary start <$> term

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