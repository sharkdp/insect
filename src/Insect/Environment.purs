module Insect.Environment
  ( Environment
  , initialEnvironment
  ) where

import Data.StrMap (StrMap, fromFoldable)
import Data.Tuple (Tuple(..))

import Quantities (Quantity, e, pi, speedOfLight, planckConstant, hbar)

-- | The environment consists of identifiers that are mapped to specific
-- | quantities.
type Environment = StrMap Quantity

-- | The initial environment contains a few useful mathematical and physical
-- | constants.
initialEnvironment ∷ Environment
initialEnvironment = fromFoldable
  [ Tuple "e"    e
  , Tuple "pi"   pi
  , Tuple "π"    pi
  , Tuple "c"    speedOfLight
  , Tuple "h"    planckConstant
  , Tuple "hbar" hbar
  , Tuple "ℏ"    hbar
  ]
