-- | This module defines the interpreter for the Insect language.
module Insect.Interpreter
  ( MessageType(..)
  , Message(..)
  , startEnv
  , runInsect
  ) where

import Prelude hiding (degree)

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.StrMap (StrMap, lookup, fromFoldable)
import Data.Tuple (Tuple(..))
import Data.Bifunctor (lmap)

import Quantities (Quantity, DerivedUnit, UnificationError, asValueIn, pow,
                   qNegate, qAdd, qDivide, qMultiply, qSubtract, quantity,
                   unity, convert, errorMessage, prettyPrint, scalar, pi, e,
                   speedOfLight, gravitationalConstant, planckConstant, hbar)

import Insect.Language (BinOp(..), Expression(..), Command(..), Statement(..))

-- | The types of errors that may appear during evaluation
data EvalError
  = UnificationError UnificationError
  | LookupError String

type Expect = Either EvalError

data MessageType = Value | Info | Error

data Message = Message MessageType String

type Environment = StrMap Quantity

-- A few helper functions
qSubtract' ∷ Quantity → Quantity → Expect Quantity
qSubtract' q1 q2 = lmap UnificationError (qSubtract q1 q2)

qAdd' ∷ Quantity → Quantity → Expect Quantity
qAdd' q1 q2 = lmap UnificationError (qAdd q1 q2)

asScalar ∷ Quantity → Expect Number
asScalar q = lmap UnificationError (q `asValueIn` unity)

convert' ∷ DerivedUnit → Quantity → Expect Quantity
convert' d q = lmap UnificationError (convert d q)

-- | Evaluate a mathematical expression involving physical quantities.
eval ∷ Environment → Expression → Expect Quantity
eval env (Q n u)         = pure $ quantity n u
eval env (Negate x)      = qNegate <$> eval env x
eval env (BinOp Sub x y) = join $ qSubtract' <$> eval env x <*> eval env y
eval env (BinOp Add x y) = join $ qAdd'      <$> eval env x <*> eval env y
eval env (BinOp Mul x y) =        qMultiply  <$> eval env x <*> eval env y
eval env (BinOp Div x y) =        qDivide    <$> eval env x <*> eval env y
eval env (BinOp Pow x y) = do
  base ← eval env x
  exp ← eval env y
  expNumber ← asScalar exp
  pure $ base `pow` expNumber
eval env (Variable name) =
  case lookup name env of
    Just q → pure q
    Nothing → Left (LookupError name)

evalErrorMessage ∷ EvalError → String
evalErrorMessage (UnificationError ue) = errorMessage ue
evalErrorMessage (LookupError name) = "Unknown variable '" <> name <> "'"

message ∷ Expect Quantity → Message
message (Left e) = Message Error (evalErrorMessage e)
message (Right q) = Message Value (prettyPrint q)

startEnv ∷ Environment
startEnv = fromFoldable
  [ Tuple "e"    e
  , Tuple "pi"   pi
  , Tuple "c"    speedOfLight
  , Tuple "h"    planckConstant
  , Tuple "hbar" hbar
  ]

-- | Run a single statement of an Insect program.
runInsect ∷ Environment → Statement → Message
runInsect env (Expression e) = message (eval env e)
runInsect env (Conversion e u) = message (eval env e >>= convert' u)
runInsect _   (Assignment _ _) = Message Error "???"
runInsect _   (Command Help) = Message Info $ intercalate "\n"
  [ "INSECT is a command-line scientific calculator."
  , ""
  , "It understands simple calculations like 1920 / 16 · 9"
  , "as well as mathematical expressions involving physical"
  , "quantities."
  , ""
  , "You can start by trying one of these examples:"
  , ""
  , "> 1920/16·9"
  , "> 2min + 30s"
  , "> 60mph -> m/s"
  , "> 6Mbps*1.5h -> Gb"
  ]
runInsect _ (Command _) = Message Error "???"
