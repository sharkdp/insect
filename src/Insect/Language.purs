-- | This module defines the AST for Insect.
module Insect.Language
  ( EvalError(..)
  , Identifier
  , BinOp(..)
  , Expression(..)
  , Command(..)
  , Statement(..)
  ) where

import Prelude hiding (Unit)

import Data.Decimal (Decimal)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.NonEmpty (NonEmpty)
import Data.Units (DerivedUnit)

import Quantities (ConversionError)

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
derive instance genericBinOp ∷ Generic BinOp _
instance showBinOp ∷ Show BinOp where show = genericShow

-- | Types of errors that may appear during evaluation.
data EvalError
  = QConversionError ConversionError
  | WrongArityError Identifier Int Int
  | LookupError String
  | NumericalError
  | RedefinedConstantError Identifier
  | InvalidIdentifier String

-- | A mathematical expression.
data Expression
 = Scalar Decimal
 | Unit DerivedUnit
 | Variable Identifier
 | Factorial Expression
 | Negate Expression
 | Apply Identifier (NonEmpty List Expression)
 | BinOp BinOp Expression Expression

derive instance eqExpression ∷ Eq Expression
instance showExpression ∷ Show Expression where
  show (Scalar n)          = "(Scalar " <> show n <> ")"
  show (Unit u)            = "(Unit " <> show u <> ")"
  show (Variable n)        = "(Variable " <> show n <> ")"
  show (Factorial x)       = "(Factorial " <> show x <> ")"
  show (Negate x)          = "(Negate " <> show x <> ")"
  show (Apply fn xs)       = "(Apply " <> show fn <> " " <> show xs <> ")"
  show (BinOp op x y)      = "(BinOp " <> show op <> " " <> show x <> " " <> show y <> ")"

-- | A command in Insect.
data Command
 = Help
 | Reset
 | List
 | Clear
 | Quit

derive instance eqCommand ∷ Eq Command
derive instance genericCommand ∷ Generic Command _
instance showCommand ∷ Show Command where show = genericShow

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
