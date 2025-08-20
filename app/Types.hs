module Types
(
  CToken(..)
, NExpr(..)
, NStatement(..)
, NFunction(..)
, NProgram(..)
) where

data CToken = Identifier String
           | Constant Integer
           | LeftParen
           | RightParen
           | LeftBrace
           | RightBrace
           | Semicolon
           | Int
           | Return
           | Void
    deriving (Show, Eq, Ord)

newtype NExpr = NInt Integer
    deriving (Show)
newtype NStatement = NReturn NExpr
    deriving (Show)
data NFunction = Function String NStatement
    deriving (Show)
newtype NProgram = NFunction NFunction
    deriving (Show)