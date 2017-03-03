-- | This module defines the interpreter for the Insect language.
module Insect.Interpreter
  ( runInsect
  , MessageType(..)
  , Message(..)
  ) where

import Prelude hiding (degree)

import Data.Either (Either(..))
import Data.Foldable (intercalate)

import Quantities (Quantity, UnificationError, asValueIn, pow, qAdd, qDivide,
                   qMultiply, qSubtract, quantity, unity, convert, errorMessage,
                   prettyPrint)

import Insect.Language (BinOp(..), Expression(..), Command(..), Statement(..))

type Expect = Either UnificationError

data MessageType = Value | Info | Error

data Message = Message MessageType String

-- | Evaluate a mathematical expression involving physical quantities.
eval ∷ Expression → Expect Quantity
eval (Q n u)         = pure $ quantity n u
eval (BinOp Add x y) = join $ qAdd      <$> eval x <*> eval y
eval (BinOp Sub x y) = join $ qSubtract <$> eval x <*> eval y
eval (BinOp Mul x y) =        qMultiply <$> eval x <*> eval y
eval (BinOp Div x y) =        qDivide   <$> eval x <*> eval y
eval (BinOp Pow x y) = do
  base ← eval x
  exp ← eval y
  expNumber ← exp `asValueIn` unity
  pure $ base `pow` expNumber

message ∷ Expect Quantity → Message
message (Left e) = Message Error (errorMessage e)
message (Right q) = Message Value (prettyPrint q)

-- | Run a single statement of an Insect program.
runInsect ∷ Statement → Message
runInsect (Expression e) = message (eval e)
runInsect (Conversion e u) = message (eval e >>= convert u)
runInsect (Assignment _ _) = Message Error "???"
runInsect (Command Help) = Message Info $ intercalate "\n"
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
runInsect (Command _) = Message Error "???"
