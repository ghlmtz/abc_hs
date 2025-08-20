module Parse
(
    parser
) where

import Types
import Codegen (translateParse)

import Text.Megaparsec
import Data.Void (Void)
import System.Exit (exitFailure)

type TokenParser = Parsec Void [CToken]

parser :: [CToken] -> ProgType -> String -> IO ()
parser toks t file = case parse (program <* eof) "abc_parse" toks of
        Left e -> putStr (show e) >> exitFailure
        Right e -> if t == Parse then print e
           else do
            let arg = translateParse e
            if t == Normal then writeAsmFile arg file
            else print arg

writeAsmFile :: String -> String -> IO ()
writeAsmFile out = flip writeFile out . flip (++) "s" . init

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

expr :: TokenParser NExpr
expr = constant <|> unary <|> between (isToken LeftParen) (isToken RightParen) expr

constant :: TokenParser NExpr
constant = do
    let isConstant (Constant _) = True
        isConstant _ = False
        getConstant (Constant x) = x
        getConstant _ = 0
    NInt . getConstant <$> satisfy isConstant

unary :: TokenParser NExpr
unary = do
    tok <- isToken Minus <|> isToken Tilde
    let start = case tok of 
                    Minus -> Negate
                    _     -> Complement
    NUnary start <$> expr