-- | This module defines the AST for the Insect language.
module Insect.Language
  ( Identifier
  , Exponent
  , Func(..)
  , BinOp(..)
  , Expression(..)
  , Command(..)
  , Statement(..)
  ) where

import Prelude hiding (Unit)

import Data.Units (DerivedUnit)
import Data.Generic (class Generic, gShow)

-- | Type synonym for identifiers in the Insect language.
type Identifier = String

-- | Type synonym for exponents in units.
type Exponent = Number

-- | Available mathematical functions.
data Func
  = Acos
  | Asin
  | Atan
  | Cos
  | Sin
  | Tan
  | Exp
  | Log
  | Sqrt

derive instance eqFunc ∷ Eq Func
derive instance genericFunc ∷ Generic Func
instance showFunc ∷ Show Func where show = gShow

-- | A binary operation.
data BinOp
 = Add
 | Sub
 | Mul
 | Div
 | Pow
 | ConvertTo

derive instance eqBinOp ∷ Eq BinOp
derive instance genericBinOp ∷ Generic BinOp
instance showBinOp ∷ Show BinOp where show = gShow

-- | A mathematical expression.
data Expression
 = Scalar Number
 | Unit DerivedUnit
 | Variable Identifier
 | Negate Expression
 | Apply Func Expression
 | BinOp BinOp Expression Expression

derive instance eqExpression ∷ Eq Expression
instance showExpression ∷ Show Expression where
  show (Scalar n)     = "(Scalar " <> show n <> ")"
  show (Unit u)       = "(Unit " <> show u <> ")"
  show (Variable n)   = "(Variable " <> show n <> ")"
  show (Negate x)     = "(Negate " <> show x <> ")"
  show (Apply fn x)   = "(Apply " <> show fn <> " " <> show x <> ")"
  show (BinOp op x y) = "(BinOp " <> show op <> " " <> show x <> " " <> show y <> ")"

-- | A command in the Insect language.
data Command
 = Help
 | Reset
 | List
 | Clear

derive instance eqCommand ∷ Eq Command
derive instance genericCommand ∷ Generic Command
instance showCommand ∷ Show Command where show = gShow

-- | A statement in the Insect language.
data Statement
 = Expression Expression
 | Assignment Identifier Expression
 | Command Command

derive instance eqStatement ∷ Eq Statement
instance showStatement ∷ Show Statement where
  show (Expression e) = "(Expression " <> show e <> ")"
  show (Assignment i e) = "(Assignment " <> show i <> " " <> show e <> ")"
  show (Command c) = "(Command " <> show c <> ")"
