module Insect.Environment
  ( StorageType(..)
  , StoredValue(..)
  , MathFunction
  , FunctionDescription(..)
  , StoredFunction(..)
  , Environment
  , initialEnvironment
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..), length)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))

import Quantities (Quantity, ConversionError)
import Quantities as Q

import Insect.Language (EvalError(..), Identifier, Expression)
import Insect.Functions as F

-- | Values can be stored as constants, as constants that are not
-- | displayed when calling `list`, and as user-defined quantities.
data StorageType = Constant | HiddenConstant | UserDefined

derive instance Eq StorageType

-- | A quantity with a given `StorageType`.
data StoredValue = StoredValue StorageType Quantity

-- | Mathematical functions on physical quantities.
type MathFunction = NonEmpty List Quantity → Either EvalError Quantity

-- | Meta information for a function, mostly for pretty printing
data FunctionDescription = BuiltinFunction (Maybe Int) | UserFunction (NonEmpty List Identifier) Expression

-- | A mathematical function with a given `StorageType`.
data StoredFunction = StoredFunction StorageType MathFunction FunctionDescription

-- | The environment consists of identifiers that are mapped to specific
-- | quantities.
type Environment =
  { values    ∷ Map String StoredValue
  , functions ∷ Map String StoredFunction
  }

-- | The initial environment contains a few useful mathematical and physical
-- | constants.
initialEnvironment ∷ Environment
initialEnvironment =
  { values:
      fromFoldable
        [ constVal "alpha"              Q.α
        , constVal "avogadroConstant"   Q.avogadroConstant
        , constVal "bohrMagneton"       Q.µB
        , constVal "boltzmannConstant"  Q.kB
        , constVal "c"                  Q.speedOfLight
        , constVal "e"                  Q.e
        , constVal "electricConstant"   Q.ε0
        , constVal "eps0"               Q.ε0
        , constVal "ε0"                 Q.ε0
        , constVal "elementaryCharge"   Q.electronCharge
        , constVal "electronCharge"     Q.electronCharge
        , constVal "electronMass"       Q.electronMass
        , constVal "G"                  Q.gravitationalConstant
        , constVal "g0"                 Q.g0
        , constVal "goldenRatio"        Q.phi
        , constVal "φ"                  Q.phi
        , constVal "gravity"            Q.g0
        , constVal "h_bar"              Q.ℏ
        , constVal "ℏ"                  Q.ℏ
        , constVal "k_B"                Q.kB
        , constVal "magneticConstant"   Q.µ0
        , constVal "mu0"                Q.µ0
        , constVal "µ0"                 Q.µ0
        , constVal "muB"                Q.µB
        , constVal "µ_B"                Q.µB
        , constVal "N_A"                Q.avogadroConstant
        , constVal "pi"                 Q.pi
        , constVal "π"                  Q.pi
        , constVal "planckConstant"     Q.planckConstant
        , constVal "protonMass"         Q.protonMass
        , constVal "speedOfLight"       Q.speedOfLight
        , constVal "R"                  Q.idealGasConstant

        -- Hidden constants
        , hiddenVal "hundred"     (Q.scalar 1.0e2)
        , hiddenVal "thousand"    (Q.scalar 1.0e3)
        , hiddenVal "million"     (Q.scalar 1.0e6)
        , hiddenVal "billion"     (Q.scalar 1.0e9)
        , hiddenVal "trillion"    (Q.scalar 1.0e12)
        , hiddenVal "quadrillion" (Q.scalar 1.0e15)
        , hiddenVal "quintillion" (Q.scalar 1.0e18)

        , hiddenVal "googol"      (Q.scalar 1.0e100)

        , hiddenVal "tau"         Q.tau
        , hiddenVal "τ"           Q.tau
        ],
    functions:
      fromFoldable
        [ constFunc  "abs" (Q.abs >>> pure)
        , constFunc  "acos" Q.acos
        , constFunc  "acosh" Q.acosh
        , constFunc  "acot" Q.acot
        , constFunc  "arccotangent" Q.acot
        , constFunc  "acoth" Q.acoth
        , constFunc  "archypcotangent" Q.acoth
        , constFunc  "acsc" Q.acsc
        , constFunc  "arccosecant" Q.acsc
        , constFunc  "acsch" Q.acsch
        , constFunc  "archypcosecant" Q.acsch
        , constFunc  "arcsecant" Q.asec
        , constFunc  "asech" Q.asech
        , constFunc  "archypsecant" Q.asech
        , constFunc  "asin" Q.asin
        , constFunc  "asinh" Q.asinh
        , constFunc  "atan" Q.atan
        , constFunc2 "atan2" Q.atan2
        , constFunc  "atanh" Q.atanh
        , constFunc  "ceil" Q.ceil
        , constFunc  "cos" Q.cos
        , constFunc  "cosh" Q.cosh
        , constFunc  "cot" Q.cot
        , constFunc  "cotangent" Q.cot
        , constFunc  "coth" Q.coth
        , constFunc  "hypcotangent" Q.coth
        , constFunc  "csc" Q.csc
        , constFunc  "cosecant" Q.csc
        , constFunc  "csch" Q.csch
        , constFunc  "hypcosecant" Q.csch
        , constFunc  "exp" Q.exp
        , constFunc  "floor" Q.floor
        , constFunc  "fromCelsius" F.fromCelsius
        , constFunc  "fromFahrenheit" F.fromFahrenheit
        , constFunc  "gamma" Q.gamma
        , constFunc  "ln" Q.ln
        , constFunc  "log" Q.ln
        , constFunc  "log10" Q.log10
        , constFuncN "minimum" (lmap QConversionError <<< Q.min <<< NonEmptyList)
        , constFuncN "maximum" (lmap QConversionError <<< Q.max <<< NonEmptyList)
        , constFuncN "mean" (lmap QConversionError <<< Q.mean <<< NonEmptyList)
        , constFunc  "round" Q.round
        , constFunc  "secant" Q.sec
        , constFunc  "sech" Q.sech
        , constFunc  "hypsecant" Q.sech
        , constFunc  "sin" Q.sin
        , constFunc  "sinh" Q.sinh
        , constFunc  "sqrt" (Q.sqrt >>> pure)
        , constFunc  "tan" Q.tan
        , constFunc  "tanh" Q.tanh
        , constFunc  "toCelsius" F.toCelsius
        , constFunc  "toFahrenheit" F.toFahrenheit
        ]
  }
  where
    constVal identifier value = Tuple identifier (StoredValue Constant value)
    hiddenVal identifier value = Tuple identifier (StoredValue HiddenConstant value)
    constFunc  identifier func = Tuple identifier (StoredFunction Constant (wrapSimple identifier func) (BuiltinFunction (Just 1)))
    constFunc2 identifier func = Tuple identifier (StoredFunction Constant (wrapSimple2 identifier func) (BuiltinFunction (Just 2)))
    constFuncN identifier func = Tuple identifier (StoredFunction Constant func (BuiltinFunction Nothing))

    wrapSimple ∷ Identifier → (Quantity → Either ConversionError Quantity) → MathFunction
    wrapSimple name func qs =
      case qs of
        x :| Nil → lmap QConversionError $ func x
        _        → Left $ WrongArityError name 1 (length (NonEmptyList qs))

    wrapSimple2 ∷ Identifier → (Quantity → Quantity → Either ConversionError Quantity) → MathFunction
    wrapSimple2 name func qs =
      case qs of
        x1 :| x2 : Nil → lmap QConversionError $ func x1 x2
        _              → Left $ WrongArityError name 2 (length (NonEmptyList qs))
