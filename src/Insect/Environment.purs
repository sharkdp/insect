module Insect.Environment
  ( Environment
  , initialEnvironment
  ) where

import Data.StrMap (StrMap, fromFoldable)
import Data.Tuple (Tuple(..))

import Quantities (Quantity, e, pi, speedOfLight, gravitationalConstant,
                   planckConstant, ℏ, electronMass, electronCharge, µ0, ε0, µB,
                   α, protonMass, avogadroConstant, kB, g0)

-- | The environment consists of identifiers that are mapped to specific
-- | quantities.
type Environment = StrMap Quantity

-- | The initial environment contains a few useful mathematical and physical
-- | constants.
initialEnvironment ∷ Environment
initialEnvironment = fromFoldable
  [ Tuple "e"                  e
  , Tuple "pi"                 pi
  , Tuple "π"                  pi
  , Tuple "G"                  gravitationalConstant
  , Tuple "electronMass"       electronMass
  , Tuple "electronCharge"     electronCharge
  , Tuple "elementaryCharge"   electronCharge
  , Tuple "c"                  speedOfLight
  , Tuple "speedOfLight"       speedOfLight
  , Tuple "planckConstant"     planckConstant
  , Tuple "hbar"               ℏ
  , Tuple "ℏ"                  ℏ
  , Tuple "µ0"                 µ0
  , Tuple "mu0"                µ0
  , Tuple "magneticConstant"   µ0
  , Tuple "ε0"                 ε0
  , Tuple "eps0"               ε0
  , Tuple "electricConstant"   ε0
  , Tuple "bohrMagneton"       µB
  , Tuple "µ_B"                µB
  , Tuple "muB"                µB
  , Tuple "alpha"              α
  , Tuple "protonMass"         protonMass
  , Tuple "avogadroConstant"   avogadroConstant
  , Tuple "N_A"                avogadroConstant
  , Tuple "k_B"                kB
  , Tuple "boltzmannConstant"  kB
  , Tuple "g0"                 g0
  , Tuple "gravity"            g0
  ]
