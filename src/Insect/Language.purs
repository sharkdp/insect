-- | This module defines the AST for the Insect language.
module Insect.Language
  ( Identifier
  , Exponent
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

-- | A binary operation.
data BinOp
 = Add
 | Sub
 | Mul
 | Div
 | Pow

derive instance eqBinOp ∷ Eq BinOp
derive instance genericBinOp ∷ Generic BinOp
instance showBinOp ∷ Show BinOp where show = gShow

-- | A mathematical expression.
data Expression
 = Q Number DerivedUnit
 | Negate Expression
 | BinOp BinOp Expression Expression
 | Variable Identifier

derive instance eqExpression ∷ Eq Expression
instance showExpression ∷ Show Expression where
  show (Q n u) = "(Q " <> show n <> " " <> show u <> ")"
  show (Negate x) = "(Negate " <> show x <> ")"
  show (BinOp op x y) = "(BinOp " <> show op <> " " <> show x <> " " <> show y <> ")"
  show (Variable n) = "(Variable " <> show n <> ")"

-- | A command in the Insect language.
data Command
 = Help
 | Reset
 | Clear

derive instance eqCommand ∷ Eq Command
derive instance genericCommand ∷ Generic Command
instance showCommand ∷ Show Command where show = gShow

-- | A statement in the Insect language.
data Statement
 = Expression Expression
 | Conversion Expression DerivedUnit
 | Assignment Identifier Expression
 | Command Command

derive instance eqStatement ∷ Eq Statement
instance showStatement ∷ Show Statement where
  show (Expression e) = "(Expression " <> show e <> ")"
  show (Conversion e u) = "(Conversion " <> show e <> " " <> show u <> ")"
  show (Assignment i e) = "(Assignment " <> show i <> " " <> show e <> ")"
  show (Command c) = "(Command " <> show c <> ")"
