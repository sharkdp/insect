-- | This module defines the interpreter for Insect.
module Insect.Interpreter
  ( MessageType(..)
  , Message(..)
  , runInsect
  ) where

import Prelude hiding (degree)

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.StrMap (lookup, insert, foldMap)

import Quantities (Quantity, UnificationError, pow, scalar, qNegate, qAdd,
                   qDivide, qMultiply, qSubtract, quantity, toScalar', sqrt,
                   convertTo, errorMessage, prettyPrint, fullSimplify,
                   derivedUnit, acos, asin, atan, sin, cos, tan, exp, ln,
                   sinh, cosh, tanh, asinh, acosh, atanh, ceil, floor, log10,
                   round)

import Insect.Language (Func(..), BinOp(..), Expression(..), Command(..), Statement(..))
import Insect.Environment (Environment, initialEnvironment)

-- | Types of errors that may appear during evaluation.
data EvalError
  = UnificationError UnificationError
  | LookupError String

-- | A type synonym for error handling. A value of type `Expect Number` is
-- | expected to be a number but might also result in an evaluation error.
type Expect = Either EvalError

-- | Output types for highlighting.
data MessageType = Value | ValueSet | Info | Error | Other

-- | The output type of the interpreter.
data Message = Message MessageType String

-- | Apply a mathematical function to a physical quantity.
applyFunction ∷ Func → Quantity → Expect Quantity
applyFunction fn q = lmap UnificationError $ (run fn) q
  where
    run Acos  = acos
    run Acosh = acosh
    run Asin  = asin
    run Asinh = asinh
    run Atan  = atan
    run Atanh = atanh
    run Ceil  = ceil
    run Cos   = cos
    run Cosh  = cosh
    run Exp   = exp
    run Floor = floor
    run Ln    = ln
    run Log10 = log10
    run Round = round
    run Sin   = sin
    run Sinh  = sinh
    run Sqrt  = sqrt >>> pure
    run Tan   = tan
    run Tanh  = tanh

-- | Evaluate a mathematical expression involving physical quantities.
eval ∷ Environment → Expression → Expect Quantity
eval env (Scalar n)      = pure $ scalar n
eval env (Unit u)        = pure $ quantity 1.0 u
eval env (Variable name) =
  case lookup name env of
    Just q → pure q
    Nothing → Left (LookupError name)
eval env (Negate x)      = qNegate <$> eval env x
eval env (Apply fn x)    = eval env x >>= applyFunction fn
eval env (BinOp op x y)  = do
  x' <- eval env x
  y' <- eval env y
  (run op) x' y'
  where
    run :: BinOp -> Quantity -> Quantity -> Expect Quantity
    run Sub       a b = qSubtract' a b
    run Add       a b = qAdd' a b
    run Mul       a b = pure (qMultiply a b)
    run Div       a b = pure (qDivide a b)
    run Pow       a b = pow a <$> toScalar'' b
    run ConvertTo a b = convertTo' a b

    wrap ∷ ∀ a. Either UnificationError a → Either EvalError a
    wrap = lmap UnificationError

    qSubtract' q1 q2 = wrap (qSubtract q1 q2)
    qAdd' q1 q2 = wrap (qAdd q1 q2)
    toScalar'' q = wrap (toScalar' q)
    convertTo' source target = wrap (convertTo source (derivedUnit target))

-- | Get the error message for an evaluation error.
evalErrorMessage ∷ EvalError → String
evalErrorMessage (UnificationError ue) = errorMessage ue
evalErrorMessage (LookupError name) = "Unknown variable '" <> name <> "'"

-- | Interpreter return type
type Response = { msg ∷ Message, newEnv ∷ Environment }

-- | Helper to construct an interpreter response
message ∷ MessageType → Environment → Expect Quantity → Response
message _ env (Left e) =
  { msg: Message Error (evalErrorMessage e)
  , newEnv: env
  }
message mt env (Right q) =
  { msg: Message mt (prettyPrint q)
  , newEnv: insert "ans" q env
  }

-- | Run a single statement of an Insect program.
runInsect ∷ Environment → Statement → Response
runInsect env (Expression e) = message Value env (fullSimplify <$> eval env e)
runInsect env (Assignment n v) =
  case eval env v of
    Left evalErr → message Error env (Left evalErr)
    Right value → message ValueSet (insert n value env) (Right (fullSimplify value))
runInsect env (Command Help) = { msg: Message Other (intercalate "\n"
  [ ""
  , "*insect* evaluates mathematical expressions that can"
  , "involve physical quantities. You can start by trying"
  , "one of these examples:"
  , ""
  , "  > `1920/16*9`               > `sin(30deg)`"
  , ""
  , "  > `2min + 30s`              > `6Mbit/s * 1.5h -> Gb`"
  , ""
  , "  > `list`                    > `r = 80cm`"
  , "  > `40000km/c -> ms`         > `pi*r^2 -> m^2`"
  , ""
  , "More information: https://github.com/sharkdp/insect"
  ]), newEnv : env }
runInsect env (Command List) =
  { msg: Message Other list
  , newEnv: env }
  where
    list = "List of variables:\n" <> foldMap toLine env
    toLine k v = "\n  * " <> k <> " = `" <> prettyPrint v <> "`"
runInsect env (Command Reset) =
  { msg: Message Info "Environment has been reset"
  , newEnv: initialEnvironment }
runInsect env (Command _) = { msg: Message Error "???", newEnv: env }
