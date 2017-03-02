module Insect.Interpreter where

import Prelude hiding (degree)

import Data.Either (Either)

import Quantities (Quantity, UnificationError, asValueIn, pow, qAdd, qDivide,
                   qMultiply, qSubtract, quantity, unity, convert)

import Insect.Language (BinOp(..), Expression(..), Statement(..))


type Expect = Either UnificationError

type Result = Expect Quantity

eval ∷ Expression → Result
eval (Q n u)         = pure $ quantity n u
eval (BinOp Add x y) = join $ qAdd      <$> eval x <*> eval y
eval (BinOp Sub x y) = join $ qSubtract <$> eval x <*> eval y
eval (BinOp Mul x y) =        qMultiply <$> eval x <*> eval y
eval (BinOp Div x y) =        qDivide   <$> eval x <*> eval y
eval (BinOp Pow x y) = do
  base <- eval x
  exp <- eval y
  expNumber <- exp `asValueIn` unity
  pure $ base `pow` expNumber

runInsect ∷ Statement → Result
runInsect (Expression e) = eval e
runInsect (Conversion e u) = eval e >>= convert u
runInsect (Assignment _ _) = pure $ quantity 42.0 unity
runInsect (Command _) = pure $ quantity 42.0 unity
