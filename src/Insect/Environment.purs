module Insect.Environment
  ( Environment
  , initialEnvironment
  ) where

import Data.StrMap (StrMap, fromFoldable)
import Data.Tuple (Tuple(..))

import Quantities (Quantity, e, pi, tau, speedOfLight, gravitationalConstant,
                   planckConstant, ℏ, electronMass, electronCharge, µ0, ε0, µB,
                   α, protonMass, avogadroConstant, kB, g0)

-- | The environment consists of identifiers that are mapped to specific
-- | quantities.
type Environment = StrMap Quantity

-- | The initial environment contains a few useful mathematical and physical
-- | constants.
initialEnvironment ∷ Environment
initialEnvironment = fromFoldable
  [ Tuple "alpha"              α
  , Tuple "avogadroConstant"   avogadroConstant
  , Tuple "bohrMagneton"       µB
  , Tuple "boltzmannConstant"  kB
  , Tuple "c"                  speedOfLight
  , Tuple "e"                  e
  , Tuple "electricConstant"   ε0
  , Tuple "eps0"               ε0
  , Tuple "ε0"                 ε0
  , Tuple "electronCharge"     electronCharge
  , Tuple "electronMass"       electronMass
  , Tuple "G"                  gravitationalConstant
  , Tuple "g0"                 g0
  , Tuple "gravity"            g0
  , Tuple "h_bar"              ℏ
  , Tuple "ℏ"                  ℏ
  , Tuple "k_B"                kB
  , Tuple "magneticConstant"   µ0
  , Tuple "mu0"                µ0
  , Tuple "µ0"                 µ0
  , Tuple "muB"                µB
  , Tuple "µ_B"                µB
  , Tuple "N_A"                avogadroConstant
  , Tuple "pi"                 pi
  , Tuple "π"                  pi
  , Tuple "tau"                tau
  , Tuple "τ"                  tau
  , Tuple "planckConstant"     planckConstant
  , Tuple "protonMass"         protonMass
  , Tuple "speedOfLight"       speedOfLight
  ]
