module Insect.Functions
  ( fromCelsius
  , toCelsius
  , fromFahrenheit
  , toFahrenheit
  ) where

import Prelude

import Data.Either (Either)

import Quantities (Quantity, ConversionError, (.*))
import Quantities as Q

-- | Numerical offset when converting between degree Celsius and Kelvin
offsetCelsius ∷ Number
offsetCelsius = 273.15

-- | Numerical offset when converting between degree Fahrenheit and Kelvin
offsetFahrenheit ∷ Number
offsetFahrenheit = 459.67

-- | Numerical multiplier to convert between degree Fahrenheit and Kelvin
multiplierFahrenheit ∷ Number
multiplierFahrenheit = 5.0 / 9.0

-- | Interpret a scalar value as a value in degree Celsius (°C) and convert it
-- | to Kelvin (K).
-- | Type signature in physical units: scalar => kelvin
fromCelsius ∷ Quantity → Either ConversionError Quantity
fromCelsius tempCelsius' = do
  tempCelsius ← Q.toScalar tempCelsius'
  pure $ (tempCelsius + offsetCelsius) .* Q.kelvin

-- | Convert a quantity in Kelvin (K) to a scalar that can be interpreted as
-- | the value in degree Celsius (°C).
-- | Type signature in physical units: kelvin => scalar
toCelsius ∷ Quantity → Either ConversionError Quantity
toCelsius tempKelvin' = do
  tempKelvin ← tempKelvin' `Q.asValueIn` Q.kelvin
  pure $ Q.scalar (tempKelvin - offsetCelsius)

-- | Interpret a scalar value as a value in degree Fahrenheit (°F) and convert it
-- | to Kelvin (K).
-- | Type signature in physical units: scalar => kelvin
fromFahrenheit ∷ Quantity → Either ConversionError Quantity
fromFahrenheit tempFahrenheit' = do
  tempFahrenheit ← Q.toScalar tempFahrenheit'
  pure $ ((tempFahrenheit + offsetFahrenheit) * multiplierFahrenheit) .* Q.kelvin

-- | Convert a quantity in Kelvin (K) to a scalar that can be interpreted as
-- | the value in degree Celsius (°C).
-- | Type signature in physical units: kelvin => scalar
toFahrenheit ∷ Quantity → Either ConversionError Quantity
toFahrenheit tempKelvin' = do
  tempKelvin ← tempKelvin' `Q.asValueIn` Q.kelvin
  pure $ Q.scalar (tempKelvin / multiplierFahrenheit - offsetFahrenheit)
