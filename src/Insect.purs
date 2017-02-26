module Insect (
  repl
  ) where

import Prelude hiding (degree)

import Control.Alt ((<|>))
import Control.Apply (lift2)

import Data.Quantity
import Data.Quantity.Math

import Data.Units
import Data.Units.SI
import Data.Units.SI.Derived
import Data.Units.SI.Accepted
import Data.Units.Imperial
import Data.Units.Time
import Data.Units.Bit

import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Maybe
import Data.String

import Global (readFloat, isFinite)

import Partial.Unsafe

import Text.Parsing.StringParser
import Text.Parsing.StringParser.Combinators
import Text.Parsing.StringParser.Expr
import Text.Parsing.StringParser.String

pNumber :: Parser Number
pNumber = do
  intPart <- signAndDigits

  mFracPart <- optionMaybe (append <$> string "." <*> digits)
  let fracPart = fromMaybe "" mFracPart

  mExpPart <- optionMaybe (append <$> string "e" <*> signAndDigits)
  let expPart = fromMaybe "" mExpPart

  let num = readFloat (intPart <> fracPart <> expPart)

  if isFinite num
    then pure num
    else fail "Could not parse Number"

  where
    digits :: Parser String
    digits = do
      ds <- many1 anyDigit
      pure $ fromCharArray (fromFoldable ds)

    signAndDigits :: Parser String
    signAndDigits = do
      sign <- singleton <$> option '+' (oneOf ['+', '-'])
      intPart <- digits
      pure $ sign <> intPart

pSIPrefix :: Parser (DerivedUnit → DerivedUnit)
pSIPrefix =
      (string "a" *> pure atto)
  <|> (string "f" *> pure femto)
  <|> (string "p" *> pure pico)
  <|> (string "n" *> pure nano)
  <|> (string "µ" *> pure micro)
  <|> (string "m" *> pure milli)
  <|> (string "c" *> pure centi)
  <|> (string "d" *> pure deci)
  <|> (string "h" *> pure hecto)
  <|> (string "k" *> pure kilo)
  <|> (string "M" *> pure mega)
  <|> (string "G" *> pure giga)
  <|> (string "T" *> pure tera)
  <|> (string "P" *> pure peta)
  <|> (string "E" *> pure exa)
  <|> pure id

pNormalUnit :: Parser DerivedUnit
pNormalUnit =
      (string "radians" *> pure radian)
  <|> (string "radian"  *> pure radian)
  <|> (string "rad"     *> pure radian)
  <|> (string "degrees" *> pure degree)
  <|> (string "degree"  *> pure degree)
  <|> (string "deg"     *> pure degree)
  <|> (string "hertz"   *> pure hertz)
  <|> (string "Hz"      *> pure hertz)
  <|> (string "N"       *> pure newton)
  <|> (string "J"       *> pure joule)
  <|> (string "W"       *> pure watt)
  <|> (string "bytes"   *> pure byte)
  <|> (string "byte"    *> pure byte)
  <|> (string "bits"    *> pure bit)
  <|> (string "bit"     *> pure bit)
  <|> (string "bps"     *> pure (bit ./ second))
  <|> (string "b"       *> pure byte)
  <|> (string "sec"     *> pure second)
  <|> (string "min"     *> pure minute)
  <|> (string "hours"   *> pure hour)
  <|> (string "hour"    *> pure hour)
  <|> (string "h"       *> pure hour)
  <|> (string "days"    *> pure day)
  <|> (string "day"     *> pure day)
  <|> (string "d"       *> pure day)
  <|> (string "weeks"   *> pure week)
  <|> (string "week"    *> pure week)
  <|> (string "m/h"     *> pure (meter ./ hour)) -- TODO: handle this differently
  <|> (string "m/s"     *> pure (meter ./ second)) -- TODO: handle this differently
  <|> (string "g"       *> pure gram)
  <|> (string "m"       *> pure meter)
  <|> (string "s"       *> pure second)
  <?> "Expected unit"

pImperialUnit :: Parser DerivedUnit
pImperialUnit =
      (string "miles"  *> pure mile)
  <|> (string "mile"   *> pure mile)
  -- <|> (string "mi"     *> pure mile)    TODO: this is incompatible with 'min'
  <|> (string "mph"    *> pure (mile ./ hour))
  <|> (string "inches" *> pure inch)
  <|> (string "inch"   *> pure inch)
  <|> (string "in"     *> pure inch)
  <|> (string "yards"  *> pure yard)
  <|> (string "yard"   *> pure yard)
  <|> (string "yd"     *> pure yard)
  <|> (string "feet"   *> pure foot)
  <|> (string "foot"   *> pure foot)
  <|> (string "ft"     *> pure foot)
  <|> (string "ounces" *> pure ounce)
  <|> (string "ounce"  *> pure ounce)
  <|> (string "oz"     *> pure ounce)
  <|> (string "pound"  *> pure pound)
  <|> (string "lb"     *> pure pound)

pUnitWithSIPrefix :: Parser DerivedUnit
pUnitWithSIPrefix = do
  p <- pSIPrefix
  u <- pNormalUnit
  pure $ p u

pUnit :: Parser DerivedUnit
pUnit = try pUnitWithSIPrefix
    <|> try pImperialUnit
    <|> try pNormalUnit
    <|> pure unity

pQuantity :: Parser Quantity
pQuantity = quantity <$> pNumber <*> pUnit


qS ea eb = do
  a <- ea
  b <- eb
  a ⊖ b

qA ea eb = do
  a <- ea
  b <- eb
  a ⊕ b


pExpr :: Parser (Either UnificationError Quantity)
pExpr = buildExprParser [ [ Infix (string "/" $> lift2 (⊘)) AssocRight ]
                        , [ Infix (string "*" $> lift2 (⊗)) AssocRight ]
                        , [ Infix (string "-" $> qS) AssocRight ]
                        , [ Infix (string "+" $> qA) AssocRight ]
                        ] (pure <$> pQuantity)

pInput :: Parser (Either UnificationError Quantity)
pInput = try conversion
  <|> (pExpr <* eof)
  where
    conversion = do
      expr <- pExpr
      string " to "
      target <- pUnit
      eof
      pure (expr >>= (flip convertTo) target)

repl :: String -> { out :: String, divClass :: String }
repl inp =
  case runParser pInput inp of
    Left parseErr -> { out: show parseErr, divClass: "error" }
    Right res ->
      case res of
        Left unificationError -> { out: errorMessage unificationError, divClass: "error" }
        Right val -> { out: prettyPrint val, divClass: "value" }
