module Insect (
  repl
  ) where

import Prelude hiding (degree)

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Lazy (fix)

import Data.Quantity
import Data.Quantity.Math

import Data.Units
import Data.Units.SI
import Data.Units.SI.Derived
import Data.Units.SI.Accepted
import Data.Units.Imperial
import Data.Units.Time
import Data.Units.Bit

import Data.Either (Either(..), either)
import Data.Int (toNumber)

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.Token hiding (token)

type Expect = Either UnificationError
type P a = Parser String a

languageDef :: LanguageDef
languageDef = LanguageDef
  { commentStart: ""
  , commentEnd: ""
  , commentLine: "#"
  , nestedComments: false
  , identStart: letter
  , identLetter: alphaNum <|> char '_'
  , opStart: oneOf ['+', '-', '*', '/', '^', '=']
  , opLetter: oneOf ['>']
  , reservedNames: []
  , reservedOpNames: ["->", "+", "-", "*", "/", "^", "="]
  , caseSensitive: true
}

token :: TokenParser
token = makeTokenParser languageDef

-- TODO: this does not parse huge decimals correctly, as they are converted
-- to Int first
pNumber :: P Number
pNumber = either toNumber id <$> token.naturalOrFloat

parens :: ∀ a. P a -> P a
parens = token.parens

reservedOp :: String -> P Unit
reservedOp = token.reservedOp

reserved :: String -> P Unit
reserved = token.reserved

white :: P Unit
white = token.whiteSpace

{--
pNumber :: P Number
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
    digits :: P String
    digits = do
      ds <- some $ oneOf ['0', '1', '2']
      pure $ fromCharArray (fromFoldable ds)

    signAndDigits :: P String
    signAndDigits = do
      sign <- singleton <$> option '+' (oneOf ['+', '-'])
      intPart <- digits
      pure $ sign <> intPart
--}

pSIPrefix :: P (DerivedUnit → DerivedUnit)
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

pNormalUnit :: P DerivedUnit
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
  <|> (string "kmh"     *> pure (kilo meter ./ hour))
  <|> (string "m/h"     *> pure (meter ./ hour)) -- TODO: handle this differently
  <|> (string "m/s"     *> pure (meter ./ second)) -- TODO: handle this differently
  <|> (string "g"       *> pure gram)
  <|> (string "m"       *> pure meter)
  <|> (string "s"       *> pure second)
  <?> "Expected unit"

pImperialUnit :: P DerivedUnit
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

pUnitWithSIPrefix :: P DerivedUnit
pUnitWithSIPrefix = do
  p <- pSIPrefix
  u <- pNormalUnit
  pure $ p u

pUnit :: P DerivedUnit
pUnit = try pUnitWithSIPrefix
    <|> try pImperialUnit
    <|> try pNormalUnit
    <|> pure unity

pQuantity :: P Quantity
pQuantity = quantity <$> pNumber <*> pUnit


qS ea eb = do
  a <- ea
  b <- eb
  a ⊖ b

qA ea eb = do
  a <- ea
  b <- eb
  a ⊕ b

qPow eBase eExp = do
  base <- eBase
  exp <- eExp
  expNumber <- exp `asValueIn` unity
  pure $ base `pow` expNumber

pTerm :: P (Expect Quantity) -> P (Expect Quantity)
pTerm p = parens p <|> (Right <$> pQuantity)

pExpr :: P (Expect Quantity)
pExpr = fix \p ->
  buildExprParser [ [ Infix (reservedOp "/" $> lift2 (⊘)) AssocRight ]
                  , [ Infix (reservedOp "*" $> lift2 (⊗)) AssocRight ]
                  , [ Infix (reservedOp "-" $> qS) AssocRight ]
                  , [ Infix (reservedOp "+" $> qA) AssocRight ]
                  ] (pTerm p)

pConversion :: P (Expect Quantity)
pConversion = do
  expr <- pExpr
  white
  reservedOp "->"
  target <- pUnit
  eof
  pure (expr >>= (flip convertTo) target)


pInput :: P (Expect Quantity)
pInput = try pConversion <|> (pExpr <* eof)

repl :: String -> { out :: String, divClass :: String }
repl inp =
  case runParser inp pInput of
    Left pErr ->
      let pos = parseErrorPosition pErr
      in case pos of
           (Position rec) ->
             { out: "Parse error: " <> parseErrorMessage pErr <>
                    " at position " <> show rec.column
             , divClass: "error" }
    Right res ->
      case res of
        Left unificationError -> { out: errorMessage unificationError, divClass: "error" }
        Right val -> { out: prettyPrint val, divClass: "value" }
