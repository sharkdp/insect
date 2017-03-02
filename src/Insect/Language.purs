module Insect.Language where

import Prelude hiding (Unit)

import Data.Units (DerivedUnit)
import Data.Generic (class Generic, gShow)

type Identifier = String

type Exponent = Number

data BinOp
 = Add
 | Sub
 | Mul
 | Div
 | Pow

derive instance eqBinOp :: Eq BinOp
derive instance genericBinOp :: Generic BinOp
instance showBinOp :: Show BinOp where show = gShow

data Expression
 = Q Number DerivedUnit
 | BinOp BinOp Expression Expression

derive instance eqExpression :: Eq Expression
instance showExpression :: Show Expression where
  show (Q n u) = "(Q " <> show n <> " " <> show u <> ")"
  show (BinOp op x y) = "(BinOp " <> show op <> " " <> show x <> " " <> show y <> ")"

data Command
 = Help
 | Reset
 | Clear

derive instance eqCommand :: Eq Command
derive instance genericCommand :: Generic Command
instance showCommand :: Show Command where show = gShow

data Statement
 = Expression Expression
 | Conversion Expression DerivedUnit
 | Assignment Identifier Expression
 | Command Command

derive instance eqStatement :: Eq Statement
instance showStatement :: Show Statement where
  show (Expression e) = "(Expression " <> show e <> ")"
  show (Conversion e u) = "(Conversion " <> show e <> " " <> show u <> ")"
  show (Assignment i e) = "(Assignment " <> show i <> " " <> show e <> ")"
  show (Command c) = "(Command " <> show c <> ")"
