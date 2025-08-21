module Types
(
  UnaryOp(..)
, BinaryOp (..)
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
, TBinOperand(..)
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
data BinaryOp = Add | Subtract | Multiply | Divide | Remainder
    deriving (Show)
data NExpr = NInt Integer 
           | NUnary UnaryOp NExpr
           | NBinary BinaryOp NExpr NExpr
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
data TInstruction = TReturn TValue 
                  | TUnary TOperand TValue TValue 
                  | TBinary TBinOperand TValue TValue TValue
    deriving (Show)
data TValue = TConstant Integer | TVar String
    deriving (Show)
data TBinOperand = TAdd | TSubtract | TMultiply | TDivide | TRemainder
    deriving (Show)
data TOperand = TComplement | TNegate
    deriving (Show)