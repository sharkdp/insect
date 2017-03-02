module Insect.Parser where

import Prelude hiding (degree)

import Control.Alt ((<|>))
import Control.Lazy (fix)

import Quantities (DerivedUnit, Unit, atto, bind, bit, byte, centi, day, deci,
    degree, exa, femto, foot, giga, gram, hecto, hertz, hour, id, inch, joule,
    kilo, mega, meter, micro, mile, milli, minute, nano, newton, ounce, peta,
    pico, pound, pure, radian, second, tera, unity, watt, week, yard, (./))

import Data.Either (Either)
import Data.Array (some, fromFoldable)
import Data.Maybe (fromMaybe)
import Data.String (fromCharArray, singleton)
import Global (readFloat, isFinite)

import Text.Parsing.Parser (Parser, ParseError, runParser, fail)
import Text.Parsing.Parser.Combinators (option, optionMaybe, try, (<?>))
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.String (string, char, eof, oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), LanguageDef, TokenParser,
    alphaNum, letter, makeTokenParser)

import Insect.Language (BinOp(..), Expression(..), Statement(..))

-- Our main parser type with `String` as input
type P a = Parser String a

insectLanguage :: LanguageDef
insectLanguage = LanguageDef
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
token = makeTokenParser insectLanguage

-- TODO: this does not parse huge decimals correctly, as they are converted
-- to Int first
{-- number :: P Number --}
{-- number = whiteSpace *> (either toNumber id <$> token.naturalOrFloat) --}

parens :: ∀ a. P a -> P a
parens = token.parens

reservedOp :: String -> P Unit
reservedOp = token.reservedOp

reserved :: String -> P Unit
reserved = token.reserved

whiteSpace :: P Unit
whiteSpace = token.whiteSpace

number :: P Number
number = do
  intPart <- signAndDigits

  mFracPart <- optionMaybe (append <$> string "." <*> digits)
  let fracPart = fromMaybe "" mFracPart

  mExpPart <- optionMaybe (append <$> string "e" <*> signAndDigits)
  let expPart = fromMaybe "" mExpPart

  whiteSpace

  let floatStr = intPart <> fracPart <> expPart
  let num = readFloat floatStr

  if isFinite num
    then pure num
    else fail $ "readFloat failed for input '" <> floatStr <> "'"

  where
    digits :: P String
    digits = do
      ds <- some $ oneOf ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] <?> "a digit"
      pure $ fromCharArray (fromFoldable ds)

    signAndDigits :: P String
    signAndDigits = do
      sign <- option '+' (oneOf ['+', '-'])
      intPart <- digits
      pure $ singleton sign <> intPart

siPrefix :: P (DerivedUnit → DerivedUnit)
siPrefix =
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

normalUnit :: P DerivedUnit
normalUnit =
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
  <|> (string "minutes" *> pure minute)
  <|> (string "minute"  *> pure minute)
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
  <|> (string "grams"   *> pure gram)
  <|> (string "gram"    *> pure gram)
  <|> (string "g"       *> pure gram)
  <|> (string "meters"  *> pure meter)
  <|> (string "meter"   *> pure meter)
  <|> (string "m"       *> pure meter)
  <|> (string "s"       *> pure second)
  <?> "unit"

imperialUnit :: P DerivedUnit
imperialUnit =
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

unitWithSIPrefix :: P DerivedUnit
unitWithSIPrefix = do
  p <- siPrefix
  u <- normalUnit
  pure $ p u

derivedUnit :: P DerivedUnit
derivedUnit =
      try unitWithSIPrefix
  <|> imperialUnit
  <|> normalUnit

quantity :: P Expression
quantity = whiteSpace *> (Q <$> number <*> (derivedUnit <|> pure unity)) <* whiteSpace

term :: P Expression -> P Expression
term p = parens p <|> quantity

expression :: P Expression
expression = fix \p ->
  buildExprParser [ [ Infix (reservedOp "/" $> BinOp Div) AssocLeft ]
                  , [ Infix (reservedOp "*" $> BinOp Mul) AssocLeft ]
                  , [ Infix (reservedOp "-" $> BinOp Sub) AssocLeft ]
                  , [ Infix (reservedOp "+" $> BinOp Add) AssocLeft ]
                  ] (term p)

conversion :: P Statement
conversion = do
  expr <- expression
  whiteSpace
  reservedOp "->"
  targetUnit <- derivedUnit
  whiteSpace
  pure $ Conversion expr targetUnit


statement :: P Statement
statement =
  (
       try conversion
   <|> (Expression <$> expression)
  ) <* eof

parseInsect :: String -> Either ParseError Statement
parseInsect inp = runParser inp statement
