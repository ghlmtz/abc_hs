module Main where
import System.Exit
import System.Environment
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

import Codegen
import Types

type Parser = Parsec Void String
type TokenParser = Parsec Void [CToken]

data ProgType = Normal | Lex | Parse | Codegen
    deriving (Eq)

main :: IO ()
main = do
    args <- getArgs
    if null args
        then print "Going to need an argument there, bud" >> exitFailure
    else
        if null (tail args) then lexer (head args) Normal
        else
            lexer (head args) $ case head (tail args) of
                "-l" -> Lex
                "-p" -> Parse
                "-c" -> Codegen
                _    -> error "Bad option!"

lexer :: String -> ProgType -> IO ()
lexer file t = do
    input <- readFile file
    case parse (skipSpace *> manyTill (parseFile <* optional skipSpace) eof) "abc_lexer" input of
        Left e -> putStr (errorBundlePretty e) >> exitFailure
        Right e -> if t == Lex then print e else parser e t file

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
expr = do
    let isConstant (Constant _) = True
        isConstant _ = False
        getConstant (Constant x) = x
        getConstant _ = 0
    NInt . getConstant <$> satisfy isConstant

getIdent :: CToken -> String
getIdent (Identifier x) = x
getIdent _ = ""

isIdent :: CToken -> Bool
isIdent (Identifier _) = True
isIdent _ = False

skipSpace :: Parser ()
skipSpace = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockCommentNested "/*" "*/")

parseFile :: Parser CToken
parseFile = do
        parseIdent
    <|> parseConstant
    <|> parseSymbols

parseIdent :: Parser CToken
parseIdent = do
    first <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_')
    let s = first : rest
    return $ case lookup s reserved of
        Just x -> x
        Nothing -> Identifier s

parseConstant :: Parser CToken
parseConstant = Constant . read <$> some digitChar <* notFollowedBy (letterChar <|> char '_')

parseSymbols :: Parser CToken
parseSymbols = choice $ map (try . uncurry singleChar) singleChars

singleChars :: [] (Char, CToken)
singleChars = [
    ('{', LeftBrace),
    ('}', RightBrace),
    ('(', LeftParen),
    (')', RightParen),
    (';', Semicolon)]

singleChar :: Char -> CToken -> Parser CToken
singleChar c t = char c >> pure t

reserved :: [] (String, CToken)
reserved = [
    ("int", Int),
    ("return", Return),
    ("void", Void)]

multiChar :: String -> CToken -> Parser CToken
multiChar c t = string c >> pure t