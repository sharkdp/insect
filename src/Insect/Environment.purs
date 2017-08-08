module Insect.Environment
  ( StorageType(..)
  , StoredValue(..)
  , Environment
  , initialEnvironment
  ) where

import Prelude
import Data.StrMap (StrMap, fromFoldable)
import Data.Tuple (Tuple(..))
import Quantities (Quantity)
import Quantities as Q

-- | Values can be stored as constants, as constants that are not
-- | displayed when calling `list`, and as user-defined quantities.
data StorageType = Constant | HiddenConstant | UserDefined

derive instance eqStorageType ∷ Eq StorageType

-- | A stored value is a quantity with a given `StorageType`.
data StoredValue = StoredValue StorageType Quantity

-- | The environment consists of identifiers that are mapped to specific
-- | quantities.
type Environment =
  { values ∷ StrMap StoredValue
  }

-- | The initial environment contains a few useful mathematical and physical
-- | constants.
initialEnvironment ∷ Environment
initialEnvironment =
  { values:
      fromFoldable
        [ newConst "alpha"              Q.α
        , newConst "avogadroConstant"   Q.avogadroConstant
        , newConst "bohrMagneton"       Q.µB
        , newConst "boltzmannConstant"  Q.kB
        , newConst "c"                  Q.speedOfLight
        , newConst "e"                  Q.e
        , newConst "electricConstant"   Q.ε0
        , newConst "eps0"               Q.ε0
        , newConst "ε0"                 Q.ε0
        , newConst "electronCharge"     Q.electronCharge
        , newConst "electronMass"       Q.electronMass
        , newConst "G"                  Q.gravitationalConstant
        , newConst "g0"                 Q.g0
        , newConst "gravity"            Q.g0
        , newConst "h_bar"              Q.ℏ
        , newConst "ℏ"                  Q.ℏ
        , newConst "k_B"                Q.kB
        , newConst "magneticConstant"   Q.µ0
        , newConst "mu0"                Q.µ0
        , newConst "µ0"                 Q.µ0
        , newConst "muB"                Q.µB
        , newConst "µ_B"                Q.µB
        , newConst "N_A"                Q.avogadroConstant
        , newConst "pi"                 Q.pi
        , newConst "π"                  Q.pi
        , newConst "planckConstant"     Q.planckConstant
        , newConst "protonMass"         Q.protonMass
        , newConst "speedOfLight"       Q.speedOfLight

        -- Hidden constants
        , hiddenConst "hundred"     (Q.scalar 1.0e2)
        , hiddenConst "thousand"    (Q.scalar 1.0e3)
        , hiddenConst "million"     (Q.scalar 1.0e6)
        , hiddenConst "billion"     (Q.scalar 1.0e9)
        , hiddenConst "trillion"    (Q.scalar 1.0e12)
        , hiddenConst "quadrillion" (Q.scalar 1.0e15)
        , hiddenConst "quintillion" (Q.scalar 1.0e18)

        , hiddenConst "googol"      (Q.scalar 1.0e100)

        , hiddenConst "tau"         Q.tau
        , hiddenConst "τ"           Q.tau
        ]
  }
  where
    newConst identifier value = Tuple identifier (StoredValue Constant value)
    hiddenConst identifier value = Tuple identifier (StoredValue HiddenConstant value)
