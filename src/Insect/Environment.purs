module Insect.Environment
  ( StorageType(..)
  , StoredValue(..)
  , EvalFunctionError(..)
  , MathFunction
  , StoredFunction(..)
  , Environment
  , initialEnvironment
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList(..), head, length)
import Data.NonEmpty (NonEmpty)
import Data.StrMap (StrMap, fromFoldable)
import Data.Tuple (Tuple(..))

import Quantities (Quantity, ConversionError)
import Quantities as Q

import Insect.Functions (fromCelsius, fromFahrenheit, toCelsius, toFahrenheit)

-- | Values can be stored as constants, as constants that are not
-- | displayed when calling `list`, and as user-defined quantities.
data StorageType = Constant | HiddenConstant | UserDefined

derive instance eqStorageType ∷ Eq StorageType

-- | A quantity with a given `StorageType`.
data StoredValue = StoredValue StorageType Quantity

-- | Errors that may appear when applying a function to a list of
-- | arguments.
data EvalFunctionError
  = EFWrongArity Int Int
  | EFConversionError ConversionError

-- | Mathematical functions on physical quantities.
type MathFunction = NonEmpty List Quantity → Either EvalFunctionError Quantity

-- | A mathematical function with a given `StorageType`.
data StoredFunction = StoredFunction StorageType MathFunction

-- | The environment consists of identifiers that are mapped to specific
-- | quantities.
type Environment =
  { values    ∷ StrMap StoredValue
  , functions ∷ StrMap StoredFunction
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
        , constVal "electronCharge"     Q.electronCharge
        , constVal "electronMass"       Q.electronMass
        , constVal "G"                  Q.gravitationalConstant
        , constVal "g0"                 Q.g0
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
        [ constFunc "acos" Q.acos
        , constFunc "acosh" Q.acosh
        , constFunc "acos" Q.acos
        , constFunc "acosh" Q.acosh
        , constFunc "asin" Q.asin
        , constFunc "asinh" Q.asinh
        , constFunc "atan" Q.atan
        , constFunc "atanh" Q.atanh
        , constFunc "ceil" Q.ceil
        , constFunc "cos" Q.cos
        , constFunc "cosh" Q.cosh
        , constFunc "exp" Q.exp
        , constFunc "floor" Q.floor
        , constFunc "fromCelsius" fromCelsius
        , constFunc "fromFahrenheit" fromFahrenheit
        , constFunc "gamma" Q.gamma
        , constFunc "ln" Q.ln
        , constFunc "log" Q.ln
        , constFunc "log10" Q.log10
        , constFunc "round" Q.round
        , constFunc "sin" Q.sin
        , constFunc "sinh" Q.sinh
        , constFunc "sqrt" (Q.sqrt >>> pure)
        , constFunc "tan" Q.tan
        , constFunc "tanh" Q.tanh
        , constFunc "toCelsius" toCelsius
        , constFunc "toFahrenheit" toFahrenheit
        ]
  }
  where
    constVal identifier value = Tuple identifier (StoredValue Constant value)
    hiddenVal identifier value = Tuple identifier (StoredValue HiddenConstant value)
    constFunc identifier func = Tuple identifier (StoredFunction Constant (wrapSimple func))

    wrapSimple ∷ (Quantity → Either ConversionError Quantity) → MathFunction
    wrapSimple func qs =
      if numArgs == 1
        then lmap EFConversionError $ func (head args)
        else Left $ EFWrongArity 1 numArgs
      where
        args = NonEmptyList qs
        numArgs = length args
