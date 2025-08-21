module Lex
(
  lexer
) where

import Types (CToken(..), MayError)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)

type Parser = Parsec Void String

lexer :: String -> MayError [CToken]
lexer input = do
    case parse (skipSpace *> manyTill (parseFile <* optional skipSpace) eof) "abc_lexer" input of
        Left e -> Left $ errorBundlePretty e
        Right e -> Right e

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
parseSymbols = choice $ map (try . uncurry multiChar) multiChars
                     ++ map (try . uncurry singleChar) singleChars

singleChars :: [] (Char, CToken)
singleChars = [
    ('{', LeftBrace),
    ('}', RightBrace),
    ('(', LeftParen),
    (')', RightParen),
    (';', Semicolon),
    ('-', Minus),
    ('~', Tilde)]

singleChar :: Char -> CToken -> Parser CToken
singleChar c t = char c >> pure t

reserved :: [] (String, CToken)
reserved = [
    ("int", Int),
    ("return", Return),
    ("void", Void)]

multiChars :: [] ([Char], CToken)
multiChars = [
    ("--", Decrement)]

multiChar :: String -> CToken -> Parser CToken
multiChar c t = string c >> return t