module Lex
  ( lexer,
    CToken (..),
  )
where

import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

type MayError = Either String

data CToken
  = Identifier String
  | Constant Integer
  | LConstant Integer
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Semicolon
  | Int
  | Return
  | Void
  | Tilde
  | Minus
  | Plus
  | Star
  | Slash
  | Percent
  | And
  | Pipe
  | Caret
  | LeftShift
  | RightShift
  | AndAnd
  | PipePipe
  | EqualEqual
  | BangEqual
  | Equal
  | Bang
  | LessThan
  | GreaterThan
  | LessEqual
  | GreaterEqual
  | PlusPlus
  | MinusMinus
  | PlusEqual
  | MinusEqual
  | StarEqual
  | SlashEqual
  | PercentEqual
  | AndEqual
  | OrEqual
  | XorEqual
  | LeftShiftEqual
  | RightShiftEqual
  | If
  | Else
  | Question
  | Colon
  | Goto
  | Break
  | Continue
  | Do
  | For
  | While
  | Case
  | Default
  | Switch
  | Comma
  | Static
  | Extern
  | Long
  deriving (Show, Eq, Ord)

lexer :: String -> MayError [CToken]
lexer = either (Left . errorBundlePretty) Right . parse pattern "abc_lexer"
  where
    pattern = skipSpace *> manyTill (parseFile <* optional skipSpace) eof

skipSpace :: Parser ()
skipSpace =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

parseFile :: Parser CToken
parseFile =
  parseIdent
    <|> parseConstant
    <|> parseSymbols

parseIdent :: Parser CToken
parseIdent = do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')
  let s = first : rest
  return $ fromMaybe (Identifier s) $ lookup s reserved

parseConstant :: Parser CToken
parseConstant = do
  digits <- some digitChar
  long <- optional (char 'l' <|> char 'L') <* notFollowedBy (letterChar <|> char '_')
  return $ case long of
    Nothing -> Constant (read digits)
    _ -> LConstant (read digits)

parseSymbols :: Parser CToken
parseSymbols =
  choice $
    map (try . uncurry multiChar) multiChars
      ++ map (try . uncurry singleChar) singleChars

singleChars :: [] (Char, CToken)
singleChars =
  [ ('{', LeftBrace),
    ('}', RightBrace),
    ('(', LeftParen),
    (')', RightParen),
    (';', Semicolon),
    ('-', Minus),
    ('~', Tilde),
    ('+', Plus),
    ('*', Star),
    ('/', Slash),
    ('%', Percent),
    ('&', And),
    ('|', Pipe),
    ('^', Caret),
    ('!', Bang),
    ('<', LessThan),
    ('>', GreaterThan),
    ('=', Equal),
    ('?', Question),
    (':', Colon),
    (',', Comma)
  ]

singleChar :: Char -> CToken -> Parser CToken
singleChar c t = t <$ char c

reserved :: [] (String, CToken)
reserved =
  [ ("break", Break),
    ("case", Case),
    ("continue", Continue),
    ("default", Default),
    ("do", Do),
    ("else", Else),
    ("extern", Extern),
    ("for", For),
    ("goto", Goto),
    ("if", If),
    ("int", Int),
    ("long", Long),
    ("return", Return),
    ("static", Static),
    ("switch", Switch),
    ("void", Void),
    ("while", While)
  ]

multiChars :: [] ([Char], CToken)
multiChars =
  [ ("<<=", LeftShiftEqual),
    (">>=", RightShiftEqual),
    ("--", MinusMinus),
    ("<<", LeftShift),
    (">>", RightShift),
    ("<=", LessEqual),
    (">=", GreaterEqual),
    ("==", EqualEqual),
    ("!=", BangEqual),
    ("&&", AndAnd),
    ("||", PipePipe),
    ("++", PlusPlus),
    ("+=", PlusEqual),
    ("-=", MinusEqual),
    ("*=", StarEqual),
    ("/=", SlashEqual),
    ("%=", PercentEqual),
    ("&=", AndEqual),
    ("|=", OrEqual),
    ("^=", XorEqual)
  ]

multiChar :: String -> CToken -> Parser CToken
multiChar c t = t <$ string c