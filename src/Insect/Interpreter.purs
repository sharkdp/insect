-- | This module defines the interpreter for Insect.
module Insect.Interpreter
  ( MessageType(..)
  , Message(..)
  , runInsect
  ) where

import Prelude hiding (degree)

import Data.Array (fromFoldable, singleton)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldMap, intercalate, foldl)
import Data.Int (round, toNumber)
import Data.List (List(..), sortBy, filter, groupBy, (..), (:))
import Data.List.NonEmpty (NonEmptyList(..), head, length, zip)
import Data.Map (lookup, insert, delete, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.String (toLower)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Insect.Environment (Environment, StorageType(..), StoredValue(..), FunctionDescription(..), StoredFunction(..), initialEnvironment, MathFunction)
import Insect.Format (FormattedString, Markup)
import Insect.Format as F
import Insect.Language (BinOp(..), Expression(..), Command(..), Identifier, Statement(..), EvalError(..))
import Insect.PrettyPrint (pretty, prettyQuantity)
import Quantities (Quantity, ConversionError(..))
import Quantities as Q
import Control.Apply (lift2)

-- | A type synonym for error handling. A value of type `Expect Number` is
-- | expected to be a number but might also result in an evaluation error.
type Expect = Either EvalError

-- | Different kinds of messages that will be returned by the interpreter.
data MessageType = Value | ValueSet | Info | Error

-- | The output type of the interpreter.
data Message = Message MessageType Markup | MQuit | MCopy | MClear

-- | Check if the numerical value of a quantity is finite, throw a
-- | `NumericalError` otherwise.
checkFinite ∷ Quantity → Expect Quantity
checkFinite q | Q.isFinite q = pure q
              | otherwise    = Left NumericalError

-- | Evaluate a summation or product expression like
-- | `sum(expr, var, low, high)`.
evalSpecial ∷ String
            → Environment
            → Expression
            → Expression
            → Expression
            → Expression
            → Expect Quantity
evalSpecial func env expr (Variable varname) lowExpr highExpr = do
  -- Evaluate the expressions `low` and `high`
  lowQuantity ← eval env lowExpr
  highQuantity ← eval env highExpr

  -- Try to cast to numbers (and round to integers)
  low ← lmap QConversionError (round <$> Q.toScalar lowQuantity)
  high ← lmap QConversionError (round <$> Q.toScalar highQuantity)

  let
    iteration n =
      eval (env { values = insert varname
                                  (StoredValue UserDefined (Q.scalar (toNumber n)))
                                  env.values
                }) expr

    qs = if low > high
           then Nil
           else map iteration (low .. high)

  if func == "sum"
    then
      foldl qAdd (Right (Q.scalar 0.0)) qs
    else -- product
      foldl (lift2 Q.qMultiply) (Right (Q.scalar 1.0)) qs

  where
    qAdd q1' q2' = do
      q1 ← q1'
      q2 ← q2'
      lmap QConversionError (Q.qAdd q1 q2)
evalSpecial func _ _ _ _ _ = Left (InvalidIdentifier func)

-- | Evaluate Either.. mathematical expression involving physical quantities.
eval ∷ Environment → Expression → Expect Quantity
eval _ (Scalar n)             = pure $ Q.scalar' n
eval _ (Unit u)               = pure $ Q.quantity 1.0 u
eval env (Variable name)        = case lookup name env.values of
                                    Just (StoredValue _ q) → pure q
                                    Nothing → Left (LookupError name)
eval env (Factorial x)          = eval env x >>= Q.factorial >>> lmap QConversionError
eval env (Negate x)             = Q.qNegate <$> eval env x
eval env (Apply name xs)        =
  if name == "sum" || name == "product"
    then
      case xs of
        expr :| var : low : high : Nil →
          evalSpecial name env expr var low high
        _ →
          Left (WrongArityError name 4 (length (NonEmptyList xs)))
    else
      case lookup name env.functions of
        Just (StoredFunction _ fn _) →
          traverse (eval env) xs >>= fn >>= checkFinite
        Nothing → Left (LookupError name)
eval env (BinOp op x y)         = do
  x' <- eval env x
  y' <- eval env y
  run op x' y' >>= checkFinite
  where
    run ∷ BinOp → Quantity → Quantity → Expect Quantity
    run Sub       a b = qSubtract a b
    run Add       a b = qAdd a b
    run Mul       a b = pure (Q.qMultiply a b)
    run Div       a b = pure (Q.qDivide a b)
    run Pow       a b = Q.pow a <$> toScalar b
    run Mod       a b = modulo a b
    run ConvertTo a b = convertTo a b

    wrap ∷ ∀ a. Either ConversionError a → Either EvalError a
    wrap = lmap QConversionError

    qSubtract q1 q2 = wrap (Q.qSubtract q1 q2)
    qAdd q1 q2 = wrap (Q.qAdd q1 q2)
    toScalar q = wrap (Q.toScalar' q)
    modulo q1 q2 = wrap (Q.modulo q1 q2)
    convertTo source target = wrap (Q.convertTo source (Q.derivedUnit target))

-- | Like `eval`, but calls `fullSimplify` on the result if the user did not
-- | ask for an explicit conversion.
evalAndSimplify ∷ Environment → Expression → Expect Quantity
evalAndSimplify env e@(BinOp ConvertTo _ _) = eval env e
evalAndSimplify env e = Q.fullSimplify <$> eval env e

-- | Render the error message for a conversion error.
conversionErrorMessage ∷ ConversionError → Markup
conversionErrorMessage (ConversionError u1 u2) =
  if u1 == Q.unity
    then
      [ F.error "  Conversion error:", F.nl, F.nl
      , F.text "    Cannot convert a ", F.unit "scalar"
      , F.text " to a quantity of unit ", F.unit (Q.toString u2)
      ]
    else
      if u2 == Q.unity
        then
          [ F.error "  Conversion error:", F.nl, F.nl
          , F.text "    Cannot convert quantity of unit "
          , F.unit (Q.toString u1)
          , F.text " to a ", F.unit "scalar"
          ]
        else
            [ F.error "  Conversion error:", F.nl, F.nl
            , F.text "    Cannot convert unit ", F.unit (Q.toString u1)
            ] <> baseRep u1 <>
            [ F.nl
            , F.text "                to unit ", F.unit (Q.toString u2)
            ] <> baseRep u2
  where
    baseRep u =
      if fst (Q.toStandardUnit u) == Q.unity
        then []
        else
          if Q.toString u == F.format F.fmtPlain usStrs
            then []
            else br
      where
        br = [ F.text " (base units: " ] <> usStrs <> [ F.text ")" ]
        us = Q.baseRepresentation u
        us' = sortBy (comparing Q.toString) us
        usStrs = intercalate [ F.text "·" ] $
                   map (singleton <<< F.unit <<< Q.toString) us'


-- | Get the error message for an evaluation error.
evalErrorMessage ∷ EvalError → Markup
evalErrorMessage (QConversionError ue) =
  conversionErrorMessage ue
evalErrorMessage (WrongArityError fn expected given) =
  [ F.optional (F.text "  ")
  , F.error "Wrong number of arguments:", F.nl, F.nl
  , F.text "    The function '", F.function fn, F.text "'"
  , F.text " takes ", F.val (show expected)
  , F.text (if expected == 1 then " argument" else " arguments")
  , F.text " (got ", F.val (show given), F.text ")"
  ]
evalErrorMessage (LookupError name) =
  [ F.optional (F.text "  ")
  , F.error "Unknown identifier: "
  , F.ident name]
evalErrorMessage NumericalError =
  [ F.optional (F.text "  ")
  , F.error "Numerical error: "
  , F.text "division by zero or out-of-bounds error" ]
evalErrorMessage (RedefinedConstantError name) =
  [ F.optional (F.text "  ")
  , F.error "Assignment error: ", F.text "'", F.emph name
  , F.text "' cannot be redefined." ]
evalErrorMessage (InvalidIdentifier func) =
  [ F.optional (F.text "  ")
  , F.error "Invalid identifier: ", F.text "second argument of '"
  , F.function func, F.text "' must be a variable name."
  ]

-- | Interpreter return type.
type Response = { msg ∷ Message, newEnv ∷ Environment }

-- | Show pretty-printed input and the error message.
errorWithInput ∷ Markup → Expression → Environment → EvalError → Response
errorWithInput prefix expr env err =
  { msg: Message Error $ (F.optional <$> ([ F.text "  " ] <> prefix <> pretty expr ))
                         <> [ F.optional F.nl, F.optional F.nl ]
                         <> evalErrorMessage err
  , newEnv: env
  }

-- | Check whether a given identifier is the name of a constant value/function.
isConstant ∷ Environment → Identifier → Boolean
isConstant env name = isConstantValue || isConstantFunction
  where
    isConstantValue =
      case lookup name env.values of
        Just (StoredValue Constant _)       → true
        Just (StoredValue HiddenConstant _) → true
        _ → false
    isConstantFunction =
      case lookup name env.functions of
        Just (StoredFunction Constant _ _)       → true
        Just (StoredFunction HiddenConstant _ _) → true
        _ → false

-- | Format a function definition
prettyPrintFunction ∷ Identifier → NonEmpty List Identifier → Array FormattedString
prettyPrintFunction name argNames =
  [ F.function name, F.text "(" ] <> fArgs <> [ F.text ") = " ]
  where
    fArgs = intercalate [ F.text ", " ] ((\a → [ F.ident a ]) <$> argNames)

-- | Run a single statement of an Insect program.
runInsect ∷ Environment → Statement → Response
runInsect env (Expression e) =
  case evalAndSimplify env e of
    Left evalErr → errorWithInput [] e env evalErr
    Right value →
      { msg: Message Value $    (F.optional <$> ([ F.text "  " ] <> pretty e))
                             <> (F.optional <$> [ F.nl, F.nl , F.text "   = " ])
                             <> prettyQuantity value
      , newEnv:
          let storedValue = StoredValue UserDefined value
          in env { values = insert "ans" storedValue (insert "_" storedValue env.values) }
      }

runInsect env (VariableAssignment name val) =
  case evalAndSimplify env val of
    Left evalErr → errorWithInput [ F.ident name, F.text " = " ] val env evalErr
    Right value →
      if isConstant env name
        then
          errorWithInput [ F.ident name, F.text " = " ] val env (RedefinedConstantError name)
        else
          { msg: Message ValueSet $
                      (F.optional <$> [ F.text "  ", F.ident name, F.text " = " ])
                   <> prettyQuantity value
          , newEnv: env { values = insert name (StoredValue UserDefined value) env.values
                        , functions = delete name env.functions }
          }

runInsect env (FunctionAssignment name argNames expr) =
  if isConstant env name
    then
      errorWithInput (prettyPrintFunction name argNames) expr env (RedefinedConstantError name)
    else
      { msg: Message ValueSet $ (F.optional <$> ([ F.text "  " ] <> prettyPrintFunction name argNames)) <> pretty expr
      , newEnv: env { functions = insert name (StoredFunction UserDefined userFunc (UserFunction argNames expr)) env.functions
                    , values = delete name env.values
                    }
      }
  where
    argNames' = NonEmptyList argNames
    numExpected = length argNames'

    userFunc ∷ MathFunction
    userFunc argValues =
      if numGiven == numExpected
        then evalAndSimplify functionEnv expr
        else Left (WrongArityError name numExpected numGiven)
      where
        argValues' = NonEmptyList argValues
        numGiven = length argValues'
        args = zip argNames' argValues'

        insertArg map (Tuple argName val) = insert argName (StoredValue UserDefined val) map
        functionEnv = env { values = foldl insertArg env.values args
                          , functions = delete name env.functions
                          }

runInsect env (PrettyPrintFunction name) =
  { msg: message,
    newEnv: env
  }
  where
    message =
      case lookup name env.functions of
        Just (StoredFunction _ _ (BuiltinFunction args)) →
          Message Info [ F.optional (F.text "  "),
                         F.ident name,
                         F.text "(",
                         F.text argText,
                         F.text ") = builtin function" ]
          where
            argText = case args of
                        Just 1 → "x"
                        Just 2 → "x, y"
                        Just _ → "x, y, …"
                        Nothing → "x1, x2, …"
        Just (StoredFunction _ _ (UserFunction args expr)) →
          Message Info $ (F.optional <$> ([ F.text "  " ] <> prettyPrintFunction name args)) <> pretty expr
        Nothing → Message Error [ F.text "Unknown function" ]

runInsect env (Command Help) = { msg: Message Info
  [ F.emph "insect", F.text " evaluates mathematical expressions that can", F.nl
  , F.text "involve physical quantities. You can start by trying", F.nl
  , F.text "one of these examples:", F.nl
  , F.text "", F.nl
  , F.emph "  > ", F.val "1920", F.text " / ", F.val "16", F.text " * ", F.val "9", F.text "         "
  , F.emph "  > ", F.function "sin", F.text "(", F.val "30", F.text " ", F.unit "deg", F.text ")", F.nl
  , F.text "", F.nl
  , F.emph "  > ", F.val "2", F.text " ", F.unit "min", F.text " + ", F.val "30", F.text " ", F.unit "s", F.text "          "
  , F.emph "  > ", F.val "6", F.text " ", F.unit "Mbit/s", F.text " * ", F.val "1.5", F.text " ", F.unit "h", F.text " -> ", F.unit "GB", F.nl
  , F.text "", F.nl
  , F.emph "  > ", F.text "list", F.text "                  "
  , F.emph "  > ", F.ident "r", F.text " = ", F.val "80", F.text " ", F.unit "cm", F.nl
  , F.emph "  > ", F.val "40000", F.text " ", F.unit "km", F.text " / ", F.ident "c", F.text " -> ", F.unit "ms", F.text "    "
  , F.emph "  > ", F.ident "pi", F.text " * ", F.ident "r", F.text "^", F.val "2", F.text " -> ", F.unit "m", F.text "^", F.val "2", F.nl
  , F.text "", F.nl
  , F.text "Full documentation: https://github.com/sharkdp/insect"
  ], newEnv: env }

runInsect env (Command List) =
  { msg: Message Info list, newEnv: env }
  where
    storedValue (StoredValue _ value) = value
    storageType (StoredValue t _) = t
    visibleValues = filter (\e → storageType (snd e) /= HiddenConstant) (toUnfoldable env.values)
    envTuples = sortBy (comparing (_.number <<< Q.prettyPrint' <<< storedValue <<< snd)) visibleValues
    envGrouped = groupBy (\x y → storedValue (snd x) == storedValue (snd y)) envTuples
    envSorted = sortBy (comparing (toLower <<< fst <<< head)) envGrouped
    list = [ F.text "List of variables:", F.nl ] <> foldMap toLine envSorted
    toLine kvPairs =
         [ F.nl, F.text "  " ]
      <> identifiers
      <> [ F.text " = " ]
      <> prettyQuantity val
        where
          identifiers = fromFoldable $ intercalate [ F.text " = " ] $
                          (singleton <<< F.ident <<< fst) <$> kvPairs
          val = storedValue (snd (head kvPairs))

runInsect _ (Command Reset) =
  { msg: Message Info [F.text "Environment has been reset."]
  , newEnv: initialEnvironment }

runInsect _ (Command Quit) = { msg: MQuit, newEnv: initialEnvironment }

runInsect env (Command Copy) = { msg: MCopy, newEnv: env }

runInsect env (Command Clear) = { msg: MClear, newEnv: env }
