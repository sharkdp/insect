-- | This module defines the interpreter for the Insect language.
module Insect.Interpreter
  ( MessageType(..)
  , Message(..)
  , runInsect
  ) where

import Prelude hiding (degree)

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.StrMap (lookup, insert, foldMap)
import Data.Bifunctor (lmap)

import Quantities (Quantity, UnificationError, asValueIn, pow, scalar,
                   qNegate, qAdd, qDivide, qMultiply, qSubtract, quantity,
                   unity, convert, errorMessage, prettyPrint, fullSimplify,
                   derivedUnit)

import Insect.Language (BinOp(..), Expression(..), Command(..), Statement(..))
import Insect.Environment (Environment, initialEnvironment)

-- | The types of errors that may appear during evaluation
data EvalError
  = UnificationError UnificationError
  | LookupError String

type Expect = Either EvalError

data MessageType = Value | Info | Error

data Message = Message MessageType String

-- A few helper functions
qSubtract' ∷ Quantity → Quantity → Expect Quantity
qSubtract' q1 q2 = lmap UnificationError (qSubtract q1 q2)

qAdd' ∷ Quantity → Quantity → Expect Quantity
qAdd' q1 q2 = lmap UnificationError (qAdd q1 q2)

asScalar ∷ Quantity → Expect Number
asScalar q = lmap UnificationError (q `asValueIn` unity)

convert' ∷ Quantity → Quantity → Expect Quantity
convert' target source = lmap UnificationError (convert targetUnit source)
  where targetUnit = derivedUnit target

-- | Evaluate a mathematical expression involving physical quantities.
eval ∷ Environment → Expression → Expect Quantity
eval env (Scalar n)      = pure $ scalar n
eval env (Unit u)        = pure $ quantity 1.0 u
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
eval env (BinOp ConvertTo s t) = do
  source ← eval env s
  target ← eval env t
  res ← convert' target source
  pure res
eval env (Variable name) =
  case lookup name env of
    Just q → pure q
    Nothing → Left (LookupError name)

evalErrorMessage ∷ EvalError → String
evalErrorMessage (UnificationError ue) = errorMessage ue
evalErrorMessage (LookupError name) = "Unknown variable '" <> name <> "'"

message ∷ Environment → Expect Quantity → { msg ∷ Message, newEnv ∷ Environment }
message env (Left e) =
  { msg: Message Error (evalErrorMessage e)
  , newEnv: env
  }
message env (Right q) =
  { msg: Message Value (prettyPrint q)
  , newEnv: insert "ans" q env
  }

-- | Run a single statement of an Insect program.
runInsect ∷ Environment → Statement → { msg ∷ Message, newEnv ∷ Environment }
runInsect env (Expression e) = message env (fullSimplify <$> eval env e)
runInsect env (Assignment n v) =
  case eval env v of
    Left evalErr → message env (Left evalErr)
    Right value → message (insert n value env) (Right (fullSimplify value))
runInsect env (Command Help) = { msg: Message Info (intercalate "\n"
  [ "insect is a command-line scientific calculator."
  , ""
  , "It evaluates simple calculations like 1920/16·9 as"
  , "well as expressions involving physical quantities."
  , ""
  , "You can start by trying one of these examples:"
  , ""
  , "  > 1920/16*9"
  , ""
  , "  > 2min + 30s"
  , ""
  , "  > 60mph -> m/s"
  , ""
  , "  > 6Mbps*1.5h -> Gb"
  ]), newEnv : env }
runInsect env (Command List) =
  { msg: Message Info list
  , newEnv: env }
  where
    list = foldMap toLine env
    toLine k v = k <> " = " <> prettyPrint v <> "\n"
runInsect env (Command Reset) =
  { msg: Message Info "Environment has been reset"
  , newEnv: initialEnvironment }
  where
    list = foldMap toLine env
    toLine k v = k <> " = " <> prettyPrint v <> "\n"
runInsect env (Command _) = { msg: Message Error "???", newEnv: env }
