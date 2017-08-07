-- | This module defines the AST for Insect.
module Insect.Language
  ( Identifier
  , Func(..)
  , BinOp(..)
  , Expression(..)
  , Command(..)
  , Statement(..)
  ) where

import Prelude hiding (Unit)

import Data.Decimal (Decimal)
import Data.Generic (class Generic, gShow)
import Data.Units (DerivedUnit)

-- | Type synonym for identifiers (variable names).
type Identifier = String

-- | All available mathematical functions.
data Func
  = Acos
  | Acosh
  | Asin
  | Asinh
  | Atan
  | Atanh
  | Ceil
  | Cos
  | Cosh
  | Exp
  | Floor
  | FromCelsius
  | FromFahrenheit
  | Gamma
  | Ln
  | Log10
  | Round
  | Sin
  | Sinh
  | Sqrt
  | Tan
  | Tanh
  | ToCelsius
  | ToFahrenheit

derive instance eqFunc ∷ Eq Func
derive instance genericFunc ∷ Generic Func
instance showFunc ∷ Show Func where show = gShow

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

-- | A mathematical expression.
data Expression
 = Scalar Decimal
 | Unit DerivedUnit
 | Variable Identifier
 | Factorial Expression
 | Negate Expression
 | Apply Func Expression
 | BinOp BinOp Expression Expression

derive instance eqExpression ∷ Eq Expression
instance showExpression ∷ Show Expression where
  show (Scalar n)     = "(Scalar " <> show n <> ")"
  show (Unit u)       = "(Unit " <> show u <> ")"
  show (Variable n)   = "(Variable " <> show n <> ")"
  show (Factorial x)  = "(Factorial " <> show x <> ")"
  show (Negate x)     = "(Negate " <> show x <> ")"
  show (Apply fn x)   = "(Apply " <> show fn <> " " <> show x <> ")"
  show (BinOp op x y) = "(BinOp " <> show op <> " " <> show x <> " " <> show y <> ")"

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
 | Assignment Identifier Expression
 | Command Command

derive instance eqStatement ∷ Eq Statement
instance showStatement ∷ Show Statement where
  show (Expression e)   = "(Expression " <> show e <> ")"
  show (Assignment i e) = "(Assignment " <> show i <> " " <> show e <> ")"
  show (Command c)      = "(Command " <> show c <> ")"
