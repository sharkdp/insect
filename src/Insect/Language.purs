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
import Data.Show.Generic (genericShow)
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

derive instance Eq BinOp
derive instance Generic BinOp _
instance Show BinOp where show = genericShow

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

derive instance Eq Expression
instance Show Expression where
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
 | Copy
 | Quit

derive instance Eq Command
derive instance Generic Command _
instance Show Command where show = genericShow

-- | A statement in Insect.
data Statement
 = Expression Expression
 | VariableAssignment Identifier Expression
 | FunctionAssignment Identifier (NonEmpty List Identifier) Expression
 | PrettyPrintFunction Identifier
 | Command Command

derive instance Eq Statement
instance Show Statement where
  show (Expression e)              = "(Expression " <> show e <> ")"
  show (VariableAssignment i e)    = "(VariableAssignment " <> show i <> " " <> show e <> ")"
  show (FunctionAssignment f xs e) = "(FunctionAssignment " <> show f <> " " <> show xs <> " " <> show e <> ")"
  show (PrettyPrintFunction f)     = "(PrettyPrintFunction " <> show f <> ")"
  show (Command c)                 = "(Command " <> show c <> ")"
