-- | This module defines the interpreter for Insect.
module Insect.Interpreter
  ( MessageType(..)
  , Message(..)
  , runInsect
  ) where

import Prelude hiding (degree)

import Data.Array ((:), concat)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (lookup, insert, foldMap)
import Data.Tuple (Tuple(..), fst)

import Quantities (Quantity, UnificationError(..), pow, scalar', qNegate, qAdd,
                   qDivide, qMultiply, qSubtract, quantity, toScalar', sqrt,
                   convertTo, prettyPrint', fullSimplify, derivedUnit, acos,
                   asin, atan, sin, cos, tan, exp, ln, sinh, cosh, tanh, asinh,
                   acosh, atanh, ceil, floor, log10, round, isFinite,
                   toStandardUnit, unity, toString)

import Insect.Language (Func(..), BinOp(..), Expression(..), Command(..),
                        Statement(..))
import Insect.Environment (Environment, initialEnvironment)
import Insect.Format (Markup)
import Insect.Format as F

-- | Types of errors that may appear during evaluation.
data EvalError
  = QUnificationError UnificationError
  | LookupError String
  | NumericalError

-- | A type synonym for error handling. A value of type `Expect Number` is
-- | expected to be a number but might also result in an evaluation error.
type Expect = Either EvalError

-- | Different kinds of messages that will be returned by the interpreter.
data MessageType = Value | ValueSet | Info | Error

-- | The output type of the interpreter.
data Message = Message MessageType Markup | MQuit | MClear

-- | Check if the numerical value of a quantity is finite, throw a
-- | `NumericalError` otherwise.
checkFinite ∷ Quantity → Expect Quantity
checkFinite q | isFinite q = pure q
              | otherwise  = Left NumericalError

-- | Apply a mathematical function to a physical quantity.
applyFunction ∷ Func → Quantity → Expect Quantity
applyFunction fn q = lmap QUnificationError $ (run fn) q
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
eval env (Scalar n)      = pure $ scalar' n
eval env (Unit u)        = pure $ quantity 1.0 u
eval env (Variable name) =
  case lookup name env of
    Just q → pure q
    Nothing → Left (LookupError name)
eval env (Negate x)      = qNegate <$> eval env x
eval env (Apply fn x)    = eval env x >>= applyFunction fn >>= checkFinite
eval env (BinOp op x y)  = do
  x' <- eval env x
  y' <- eval env y
  (run op) x' y' >>= checkFinite
  where
    run :: BinOp -> Quantity -> Quantity -> Expect Quantity
    run Sub       a b = qSubtract' a b
    run Add       a b = qAdd' a b
    run Mul       a b = pure (qMultiply a b)
    run Div       a b = pure (qDivide a b)
    run Pow       a b = pow a <$> toScalar'' b
    run ConvertTo a b = convertTo' a b

    wrap ∷ ∀ a. Either UnificationError a → Either EvalError a
    wrap = lmap QUnificationError

    qSubtract' q1 q2 = wrap (qSubtract q1 q2)
    qAdd' q1 q2 = wrap (qAdd q1 q2)
    toScalar'' q = wrap (toScalar' q)
    convertTo' source target = wrap (convertTo source (derivedUnit target))

-- | Render the error message for a unification error.
unificationErrorMessage ∷ UnificationError → Markup
unificationErrorMessage (UnificationError u1 u2) =
  if u1 == unity
    then scalarErr u2
    else
      if u2 == unity
        then scalarErr u1
        else
            [ F.error "Unification error:", F.nl, F.nl
            , F.text "  Cannot unify unit ", F.unit (toString u1)
            ] <> baseRep u1 <>
            [ F.nl
            , F.text "          with unit ", F.unit (toString u2)
            ] <> baseRep u2
  where
    scalarErr u =
      [ F.error "Unification error:", F.nl, F.nl
      , F.text "  Cannot convert quantity of unit "
      , F.unit (toString u)
      , F.text " to a ", F.unit "scalar"
      ]
    baseRep u =
      let u' = fst (toStandardUnit u)
      in
        if u' == unity
          then []
          else [F.text " (base units: ", F.unit (toString u'), F.text ")"]

-- | Get the error message for an evaluation error.
evalErrorMessage ∷ EvalError → Markup
evalErrorMessage (QUnificationError ue) =
  unificationErrorMessage ue
evalErrorMessage (LookupError name) =
  [ F.error "Unknown identifier: "
  , F.ident name]
evalErrorMessage NumericalError =
  [ F.error "Numerical error: "
  , F.text "division by zero or out-of-bounds error" ]

-- | Format a physical quantity.
prettyPrint ∷ Quantity → Markup
prettyPrint q =
  case prettyPrint' q of
    Tuple v u → [ F.val v, F.unit u ]

-- | Interpreter return type.
type Response = { msg ∷ Message, newEnv ∷ Environment }

-- | Helper to construct an interpreter response
message ∷ MessageType → Environment → Expect Quantity → Response
message _ env (Left e) =
  { msg: Message Error (evalErrorMessage e)
  , newEnv: env
  }
message mt env (Right q) =
  { msg: Message mt (F.whitespace "  " : prettyPrint q)
  , newEnv: insert "ans" q env
  }

-- | Run a single statement of an Insect program.
runInsect ∷ Environment → Statement → Response
runInsect env (Expression e) = message Value env (fullSimplify <$> eval env e)
runInsect env (Assignment n v) =
  case eval env v of
    Left evalErr → message Error env (Left evalErr)
    Right value → message ValueSet (insert n value env) (Right (fullSimplify value))
runInsect env (Command Help) = { msg: Message Info
  [ F.emph "insect", F.text " evaluates mathematical expressions that can", F.nl
  , F.text "involve physical quantities. You can start by trying", F.nl
  , F.text "one of these examples:", F.nl
  , F.text "", F.nl
  , F.emph "  > ", F.val "1920/16*9", F.text "             "
  , F.emph "  > ", F.val "sin(30", F.unit "deg", F.val ")", F.nl
  , F.text "", F.nl
  , F.emph "  > ", F.val "2", F.unit "min", F.val " + 30", F.unit "s", F.text "            "
  , F.emph "  > ", F.val "6", F.unit "Mbit/s", F.val " * 1.5", F.unit "h", F.val " -> ", F.unit "GB", F.val "", F.nl
  , F.text "", F.nl
  , F.emph "  > ", F.emph "list", F.text "                  "
  , F.emph "  > ", F.val "r = 80", F.unit "cm", F.nl
  , F.emph "  > ", F.val "40000", F.unit "km/c", F.val " -> ", F.unit "ms", F.text "       "
  , F.emph "  > ", F.val "pi*r^2 -> ", F.unit "m^2", F.nl
  , F.text "", F.nl
  , F.text "More information: https://github.com/sharkdp/insect"
  ], newEnv : env }
runInsect env (Command List) =
  { msg: Message Info list
  , newEnv: env }
  where
    list = [ F.text "List of variables:", F.nl ] <> foldMap toLine env
    toLine k v = concat [ [ F.nl, F.text "  "
                          , F.ident k
                          , F.text " = "
                          ],
                          prettyPrint v
                        ]
runInsect env (Command Reset) =
  { msg: Message Info [F.text "Environment has been reset."]
  , newEnv: initialEnvironment }
runInsect env (Command Quit) = { msg: MQuit, newEnv: initialEnvironment }
runInsect env (Command Clear) = { msg: MClear, newEnv: env }
