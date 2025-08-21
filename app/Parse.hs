module Parse
(
  parser
, UnaryOp(..)
, BinaryOp(..)
, Expr(..)
, Statement(..)
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

data UnaryOp = Complement | Negate 
    deriving (Show)
data BinaryOp = Add | Subtract | Multiply | Divide | Remainder
              | And | Or | Xor | LeftShift | RightShift
    deriving (Show)
data Expr = Int Integer 
           | Unary UnaryOp Expr
           | Binary BinaryOp Expr Expr
    deriving (Show)
newtype Statement = Return Expr
    deriving (Show)
data Function = Function String Statement
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
    body <- isToken L.LeftParen *> isToken L.Void *> isToken L.RightParen *> isToken L.LeftBrace *> statement <* isToken L.RightBrace
    return $ Function (getIdent name) body

statement :: TokenParser Statement
statement = Return <$> (isToken L.Return *> expr <* isToken L.Semicolon)

term :: TokenParser Expr
term = constant 
   <|> unary
   <|> between (isToken L.LeftParen) (isToken L.RightParen) expr

constant :: TokenParser Expr
constant = do
    let isConstant (L.Constant _) = True
        isConstant _ = False
        getConstant (L.Constant x) = x
        getConstant _ = 0
    Int . getConstant <$> satisfy isConstant

expr :: TokenParser Expr
expr = makeExprParser term precedence

unary :: TokenParser Expr
unary = do
    tok <- isToken L.Minus <|> isToken L.Tilde
    let start = case tok of 
                    L.Minus -> Negate
                    _     -> Complement
    Unary start <$> term

precedence :: [[Operator TokenParser Expr]]
precedence = [ [ binary  L.Star       (Binary Multiply)
               , binary  L.Slash      (Binary Divide)
               , binary  L.Percent    (Binary Remainder)]
             , [ binary  L.Plus       (Binary Add)
               , binary  L.Minus      (Binary Subtract)]
             , [ binary  L.LeftShift  (Binary LeftShift)
               , binary  L.RightShift (Binary RightShift)]
             , [ binary  L.And        (Binary And)]
             , [ binary  L.Caret      (Binary Xor)]
             , [ binary  L.Pipe       (Binary Or)]]

binary :: MonadParsec e s m => Token s -> (a -> a -> a) -> Operator m a
binary name f = InfixL (f <$ isToken name)
