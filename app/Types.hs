module Types
(
  UnaryOp(..)
, NExpr(..)
, NStatement(..)
, NFunction(..)
, NProgram(..)
, ProgType(..)
, CToken(..)
) where

data ProgType = Normal | Lex | Parse | Codegen
    deriving (Eq)

data CToken = Identifier String
           | Constant Integer
           | LeftParen
           | RightParen
           | LeftBrace
           | RightBrace
           | Semicolon
           | Minus
           | Tilde
           | Decrement
           | Int
           | Return
           | Void
    deriving (Show, Eq, Ord)

data UnaryOp = Complement | Negate 
    deriving (Show)
data NExpr = NInt Integer | NUnary UnaryOp NExpr
    deriving (Show)
newtype NStatement = NReturn NExpr
    deriving (Show)
data NFunction = Function String NStatement
    deriving (Show)
newtype NProgram = NFunction NFunction
    deriving (Show)

