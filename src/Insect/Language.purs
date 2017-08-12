-- | This module defines the AST for Insect.
module Insect.Language
  ( MathFunction
  , EvalError(..)
  , Identifier
  , BinOp(..)
  , Func(..)
  , Expression(..)
  , Command(..)
  , Statement(..)
  ) where

import Prelude hiding (Unit)

import Data.Decimal (Decimal)
import Data.Either (Either)
import Data.Generic (class Generic, gShow)
import Data.List (List)
import Data.NonEmpty (NonEmpty)
import Data.Units (DerivedUnit)

import Quantities (Quantity, ConversionError)

-- | Type synonym for identifiers (variable names).
type Identifier = String

-- | Binary operators.
data BinOp
 = Add
 | Sub
 | Mul
 | Div
 | Pow
 | Mod
 | ConvertTo

derive instance eqBinOp ∷ Eq BinOp
derive instance genericBinOp ∷ Generic BinOp
instance showBinOp ∷ Show BinOp where show = gShow

-- | Types of errors that may appear during evaluation.
data EvalError
  = QConversionError ConversionError
  | WrongArityError Identifier Int Int
  | LookupError String
  | NumericalError
  | RedefinedConstantError Identifier

-- | Mathematical functions on physical quantities.
type MathFunction = NonEmpty List Quantity → Either EvalError Quantity

-- | A mathematical function.
data Func = Func Identifier MathFunction

instance eqFunc ∷ Eq Func where
  eq (Func id1 _) (Func id2 _) = eq id1 id2

instance showFunc ∷ Show Func where
  show (Func name _) = "(Func " <> show name <> " <FUNC>)"

-- | A mathematical expression.
data Expression
 = Scalar Decimal
 | Unit DerivedUnit
 | Variable Identifier
 | Factorial Expression
 | Negate Expression
 | Apply Func (NonEmpty List Expression)
 | BinOp BinOp Expression Expression

derive instance eqExpression ∷ Eq Expression
instance showExpression ∷ Show Expression where
  show (Scalar n)          = "(Scalar " <> show n <> ")"
  show (Unit u)            = "(Unit " <> show u <> ")"
  show (Variable n)        = "(Variable " <> show n <> ")"
  show (Factorial x)       = "(Factorial " <> show x <> ")"
  show (Negate x)          = "(Negate " <> show x <> ")"
  show (Apply fn x)        = "(Apply " <> show fn <> " " <> show x <> ")"
  show (BinOp op x y)      = "(BinOp " <> show op <> " " <> show x <> " " <> show y <> ")"

-- | A command in Insect.
data Command
 = Help
 | Reset
 | List
 | Clear
 | Quit

derive instance eqCommand ∷ Eq Command
derive instance genericCommand ∷ Generic Command
instance showCommand ∷ Show Command where show = gShow

-- | A statement in Insect.
data Statement
 = Expression Expression
 | VariableAssignment Identifier Expression
 | FunctionAssignment Identifier (NonEmpty List Identifier) Expression
 | Command Command

derive instance eqStatement ∷ Eq Statement
instance showStatement ∷ Show Statement where
  show (Expression e)              = "(Expression " <> show e <> ")"
  show (VariableAssignment i e)    = "(VariableAssignment " <> show i <> " " <> show e <> ")"
  show (FunctionAssignment f xs e) = "(FunctionAssignment " <> show f <> " " <> show xs <> " " <> show e <> ")"
  show (Command c)                 = "(Command " <> show c <> ")"
