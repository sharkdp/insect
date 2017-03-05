-- | This module defines the parser for the Insect language.
module Insect.Parser
  ( parseInsect
  ) where

import Prelude hiding (degree)

import Control.Alt ((<|>))
import Control.Lazy (fix)

import Quantities (DerivedUnit, Unit, atto, bind, bit, byte, centi, day, deci,
                   degree, exa, femto, foot, giga, gram, hecto, hertz, hour,
                   id, inch, joule, kilo, mega, meter, micro, mile, milli,
                   minute, nano, newton, ounce, peta, pico, pound, pure,
                   radian, second, tera, unity, watt, week, yard, (./))

import Data.Either (Either)
import Data.Array (some, fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (fromCharArray, singleton)
import Global (readFloat, isFinite)

import Text.Parsing.Parser (Parser, ParseError, runParser, fail)
import Text.Parsing.Parser.Combinators (option, optionMaybe, try, (<?>))
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.String (string, char, eof, oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), LanguageDef, TokenParser,
                                  alphaNum, letter, makeTokenParser)

import Insect.Language (BinOp(..), Expression(..), Command(..), Statement(..))

-- | A type synonym for the main Parser type with `String` as input.
type P a = Parser String a

-- | The language definition for the `TokenParser`.
insectLanguage ∷ LanguageDef
insectLanguage = LanguageDef
  { commentStart: ""
  , commentEnd: ""
  , commentLine: "#"
  , nestedComments: false
  , identStart: letter
  , identLetter: alphaNum <|> char '_'
  , opStart: oneOf ['+', '-', '*', '·', '/', '^', '=']
  , opLetter: oneOf ['>', '*']
  , reservedNames: ["help", "?", "list", "ls", "reset", "clear"]
  , reservedOpNames: ["->", "+", "-", "*", "/", "^", "="]
  , caseSensitive: true
}

-- | The actual token parser.
token ∷ TokenParser
token = makeTokenParser insectLanguage

-- | Parse something, inside of parens.
parens ∷ ∀ a. P a → P a
parens = token.parens

-- | Parse one of the reserved operators.
reservedOp ∷ String → P Unit
reservedOp = token.reservedOp

-- | Parse a reserverd keyword.
reserved ∷ String → P Unit
reserved = token.reserved

-- | Parse zero or more whitespace characters.
whiteSpace ∷ P Unit
whiteSpace = token.whiteSpace

-- | Parse a number.
number ∷ P Number
number = do
  intPart ← digits

  mFracPart ← optionMaybe (append <$> string "." <*> digits)
  let fracPart = fromMaybe "" mFracPart

  mExpPart ← optionMaybe (append <$> string "e" <*> signAndDigits)
  let expPart = fromMaybe "" mExpPart

  whiteSpace

  let floatStr = intPart <> fracPart <> expPart
  let num = readFloat floatStr

  if isFinite num
    then pure num
    else fail $ "readFloat failed for input '" <> floatStr <> "'"

  where
    digits ∷ P String
    digits = do
      ds ← some $ oneOf ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] <?> "a digit"
      pure $ fromCharArray (fromFoldable ds)

    signAndDigits ∷ P String
    signAndDigits = do
      sign ← option '+' (oneOf ['+', '-'])
      intPart ← digits
      pure $ singleton sign <> intPart

-- | Parse a SI prefix like `µ` or `G`.
siPrefix ∷ P (DerivedUnit → DerivedUnit)
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

-- | Parse a normal (SI-conform) unit, i.e. non-imperical unit
normalUnit ∷ P DerivedUnit
normalUnit =
      (string "radians" *> pure radian)
  <|> (string "radian"  *> pure radian)
  <|> (string "rad"     *> pure radian)
  <|> (string "degrees" *> pure degree)
  <|> (string "degree"  *> pure degree)
  <|> (string "deg"     *> pure degree)
  <|> (string "°"       *> pure degree)
  <|> (string "hertz"   *> pure hertz)
  <|> (string "Hz"      *> pure hertz)
  <|> (string "newton"  *> pure newton)
  <|> (string "N"       *> pure newton)
  <|> (string "joules"  *> pure joule)
  <|> (string "joule"   *> pure joule)
  <|> (string "J"       *> pure joule)
  <|> (string "watts"   *> pure watt)
  <|> (string "watt"    *> pure watt)
  <|> (string "W"       *> pure watt)
  <|> (string "bytes"   *> pure byte)
  <|> (string "byte"    *> pure byte)
  <|> (string "bits"    *> pure bit)
  <|> (string "bit"     *> pure bit)
  <|> (string "bps"     *> pure (bit ./ second))
  <|> (string "b"       *> pure byte)
  <|> (string "seconds" *> pure second)
  <|> (string "second"  *> pure second)
  <|> (string "sec"     *> pure second)
  <|> (string "s"       *> pure second)
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
  <?> "unit"

-- | Parse a imperial unit like `ft`.
imperialUnit ∷ P DerivedUnit
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

-- | Parse a 'normal' unit with a SI prefix, like `km` or `Gb`.
unitWithSIPrefix ∷ P DerivedUnit
unitWithSIPrefix = do
  p ← siPrefix
  u ← normalUnit
  pure $ p u

-- | Parse a derived unit, like `km`, `ft`, or `s`.
derivedUnit ∷ P DerivedUnit
derivedUnit =
      try unitWithSIPrefix
  <|> imperialUnit
  <|> normalUnit

-- | Parse a physical quanity like `2.3e-7 km`.
quantity ∷ P Expression
quantity = whiteSpace *> (Q <$> number <*> (derivedUnit <|> pure unity)) <* whiteSpace

variable ∷ P Expression
variable = Variable <$> token.identifier

-- | Helper for the expression parser below.
term ∷ P Expression → P Expression
term p = parens p <|> quantity <|> variable

-- | Parse a full expression.
expression ∷ P Expression
expression = fix \p →
  buildExprParser
    [ [ Infix (powOp $> BinOp Pow) AssocLeft ]
    , [ Prefix (subOp $> Negate)             ]
    , [ Prefix (addOp $> id)                 ]
    , [ Infix (divOp $> BinOp Div) AssocLeft ]
    , [ Infix (mulOp $> BinOp Mul) AssocLeft ]
    , [ Infix (subOp $> BinOp Sub) AssocLeft ]
    , [ Infix (addOp $> BinOp Add) AssocLeft ]
    ] (term p)
  where
    powOp = reservedOp "^" <|> reservedOp "**"
    divOp = reservedOp "/"
    mulOp = reservedOp "*" <|> reservedOp "·"
    subOp = reservedOp "-"
    addOp = reservedOp "+"

-- | Parse a mathematical expression (or conversion) like `3m` or `3m->ft`.
expressionOrConversion ∷ P Statement
expressionOrConversion = do
  whiteSpace
  expr ← expression
  conv ← optionMaybe (reservedOp "->" *> derivedUnit <* whiteSpace)
  eof <?> "end of input"

  case conv of
    Just target → pure $ Conversion expr target
    Nothing     → pure $ Expression expr

-- | Parse an Insect-command
command ∷ P Command
command =
  (
        (reserved "help" <|> reserved "?") *> pure Help
    <|> (reserved "list" <|> reserved "ls") *> pure List
    <|> (reserved "reset") *> pure Reset
    <|> (reserved "clear") *> pure Clear
  ) <* eof

-- | Parse a variable assignment like `x = 3m*pi`
assignment ∷ P Statement
assignment = do
  var <- token.identifier
  reservedOp "="
  value <- expression
  pure $ Assignment var value

-- | Parse a statement in the Insect language.
statement ∷ P Statement
statement = (Command <$> command) <|> try assignment <|> expressionOrConversion

-- | Run the Insect-parser on a `String` input.
parseInsect ∷ String → Either ParseError Statement
parseInsect inp = runParser inp statement
