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
  | UConstant Integer
  | ULConstant Integer
  | Float Double
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
  | Signed
  | Unsigned
  | Double
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
    <|> try parseFloat
    <|> parseConstant
    <|> parseSymbols

parseIdent :: Parser CToken
parseIdent = do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')
  let s = first : rest
  return $ fromMaybe (Identifier s) $ lookup s reserved

parseFloat :: Parser CToken
parseFloat = do
  Float <$> (try parseScientific <|> parseFractional) <* notFollowedBy (letterChar <|> char '_' <|> char '.')

parseScientific :: Parser Double
parseScientific = do
  signif <- try parseFloat2 <|> some digitChar <* optional (char '.')
  e <- (char 'e' <|> char 'E') *> optional (string "+" <|> string "-")
  expNum <- some digitChar
  return $ read $ signif ++ maybe "e" ("e" ++) e ++ expNum

parseFractional :: Parser Double
parseFractional = read <$> (try parseFloat2 <|> parseFloat3)

parseFloat2 :: Parser String
parseFloat2 = do
  i <- optional (some digitChar) <* char '.'
  j <- some digitChar
  return $ fromMaybe "0" i ++ "." ++ j

parseFloat3 :: Parser String
parseFloat3 = do
  i <- some digitChar <* char '.'
  return $ i ++ ".0"

parseConstant :: Parser CToken
parseConstant = do
  digits <- some digitChar
  ul <-
    optional
      ( string "ul"
          <|> string "UL"
          <|> string "uL"
          <|> string "Ul"
          <|> string "lu"
          <|> string "LU"
          <|> string "Lu"
          <|> string "lU"
          <|> string "l"
          <|> string "L"
          <|> string "u"
          <|> string "U"
      )
      <* notFollowedBy (letterChar <|> char '_')
  return $ case ul of
    Nothing -> Constant (read digits)
    Just "l" -> LConstant (read digits)
    Just "L" -> LConstant (read digits)
    Just "u" -> UConstant (read digits)
    Just "U" -> UConstant (read digits)
    _ -> ULConstant (read digits)

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
    ("double", Double),
    ("else", Else),
    ("extern", Extern),
    ("for", For),
    ("goto", Goto),
    ("if", If),
    ("int", Int),
    ("long", Long),
    ("return", Return),
    ("signed", Signed),
    ("static", Static),
    ("switch", Switch),
    ("unsigned", Unsigned),
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