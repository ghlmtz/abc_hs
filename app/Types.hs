module Types
(
  UnaryOp(..)
, NExpr(..)
, NStatement(..)
, NFunction(..)
, NProgram(..)
, CToken(..)
, TProgram(..)
, TFuncDef(..)
, TInstruction(..)
, TOperand(..)
, TValue(..)
, MayError
) where

type MayError = Either String 

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
           | Plus
           | Star
           | Slash
           | Percent
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

data TProgram = TProgram TFuncDef
    deriving (Show)
data TFuncDef = TFuncDef String [TInstruction]
    deriving (Show)
data TInstruction = TReturn TValue | TUnary TOperand TValue TValue 
    deriving (Show)
data TValue = TConstant Integer | TVar String
    deriving (Show)
data TOperand = TComplement | TNegate
    deriving (Show)