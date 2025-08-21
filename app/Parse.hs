module Parse
(
    parser
) where

import Types

import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Data.Void (Void)

type TokenParser = Parsec Void [CToken]

parser :: [CToken] -> MayError NProgram
parser toks = case parse (program <* eof) "abc_parse" toks of
        Left e -> Left (show e)
        Right e -> Right e

isToken :: MonadParsec e s m => Token s -> m (Token s)
isToken t = satisfy (== t)

getIdent :: CToken -> String
getIdent (Identifier x) = x
getIdent _ = ""

isIdent :: CToken -> Bool
isIdent (Identifier _) = True
isIdent _ = False

program :: TokenParser NProgram
program = NFunction <$> function

function :: TokenParser NFunction
function = do
    name <- isToken Int *> satisfy isIdent
    body <- isToken LeftParen *> isToken Void *> isToken RightParen *> isToken LeftBrace *> statement <* isToken RightBrace
    return $ Function (getIdent name) body

statement :: TokenParser NStatement
statement = NReturn <$> (isToken Return *> expr <* isToken Semicolon)

term :: TokenParser NExpr
term = constant 
   <|> unary
   <|> between (isToken LeftParen) (isToken RightParen) expr

constant :: TokenParser NExpr
constant = do
    let isConstant (Constant _) = True
        isConstant _ = False
        getConstant (Constant x) = x
        getConstant _ = 0
    NInt . getConstant <$> satisfy isConstant

expr :: TokenParser NExpr
expr = makeExprParser term precedence

unary :: TokenParser NExpr
unary = do
    tok <- isToken Minus <|> isToken Tilde
    let start = case tok of 
                    Minus -> Negate
                    _     -> Complement
    NUnary start <$> term

precedence :: [[Operator TokenParser NExpr]]
precedence = [ [ binary  Star    (NBinary Multiply)
               , binary  Slash   (NBinary Divide)
               , binary  Percent (NBinary Remainder)]
             , [ binary  Plus    (NBinary Add)
               , binary  Minus   (NBinary Subtract)]]

binary :: MonadParsec e s m => Token s -> (a -> a -> a) -> Operator m a
binary name f = InfixL (f <$ isToken name)
