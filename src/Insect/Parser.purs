-- | This module defines the parser for the Insect language.
module Insect.Parser
  ( parseInsect
  ) where

import Prelude hiding (degree)

import Control.Alt ((<|>))
import Control.Lazy (fix)

import Quantities (DerivedUnit, atto, bit, byte, centi, day, deci, degree, exa,
                   femto, foot, giga, gram, hecto, hertz, hour, inch, joule,
                   kilo, mega, meter, micro, mile, milli, minute, nano,
                   newton, ounce, peta, pico, pound, radian, second, tera,
                   watt, week, yard, (./))

import Data.Array (some, fromFoldable)
import Data.Either (Either)
import Data.Foldable (foldr)
import Data.List (List, many, init, last)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, (:|), foldl1)
import Data.String (fromCharArray, singleton)
import Global (readFloat, isFinite)

import Text.Parsing.Parser (ParserT, Parser, ParseError, runParser, fail)
import Text.Parsing.Parser.Combinators (option, optionMaybe, try, (<?>),
                                        notFollowedBy)
import Text.Parsing.Parser.String (string, char, eof, oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), LanguageDef, TokenParser,
                                  digit, letter, makeTokenParser)

import Insect.Language (Func(..), BinOp(..), Expression(..), Command(..),
                        Statement(..))

-- | A type synonym for the main Parser type with `String` as input.
type P a = Parser String a

-- | Possibler characters for identifiers (not for the first character).
identLetter ∷ P Char
identLetter = letter <|> digit <|> char '_' <|> char '\''

-- | The language definition.
insectLanguage ∷ LanguageDef
insectLanguage = LanguageDef
  { commentStart: ""
  , commentEnd: ""
  , commentLine: "#"
  , nestedComments: false
  , identStart: letter <|> char '_'
  , identLetter: identLetter
  , opStart: oneOf ['+', '-', '*', '·', '×', '/', '÷', '^', '=', '²', '³']
  , opLetter: oneOf ['>', '*']
  , reservedNames: ["help", "?", "list", "ls", "reset", "clear"]
  , reservedOpNames: ["->", "+", "-", "*", "×", "/", "÷", "^", "**", "=", "²"]
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
  <|> (string "µ" *> pure micro) -- Micro sign U+00B5
  <|> (string "μ" *> pure micro) -- Greek small letter mu U+039C
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

-- | Parse a normal (SI-conform, non-imperical) unit, like `N` or `watt`.
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
  <|> (string "grams"   *> pure gram)
  <|> (string "gram"    *> pure gram)
  <|> (string "g"       *> pure gram)
  <|> (string "meters"  *> pure meter)
  <|> (string "meter"   *> pure meter)
  <|> (string "m"       *> pure meter)
  <?> "unit"

-- | Parse an imperial unit like `ft` of `mile`.
imperialUnit ∷ P DerivedUnit
imperialUnit =
      (string "miles"  *> pure mile)
  <|> (string "mile"   *> pure mile)
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

-- | Parse a 'normal' unit with SI prefix, like `km` or `Gb`.
unitWithSIPrefix ∷ P DerivedUnit
unitWithSIPrefix = do
  p ← siPrefix
  u ← normalUnit
  pure $ p u

-- | Parse a derived unit, like `km`, `ft`, or `s`.
derivedUnit ∷ P DerivedUnit
derivedUnit =
  (
        try unitWithSIPrefix
    <|> imperialUnit
    <|> normalUnit
  ) <* notFollowedBy identLetter <* whiteSpace

-- | Parse the name of a variable, like `my_variable'`.
variable ∷ P Expression
variable = Variable <$> token.identifier

-- | Parse the name of a mathematical function.
funcName ∷ P Func
funcName =
      (string "acosh" *> pure Acosh)
  <|> (string "acos"  *> pure Acos)
  <|> (string "asinh" *> pure Asinh)
  <|> (string "asin"  *> pure Asin)
  <|> (string "atanh" *> pure Atanh)
  <|> (string "atan"  *> pure Atan)
  <|> (string "ceil"  *> pure Ceil)
  <|> (string "cosh"  *> pure Cosh)
  <|> (string "cos"   *> pure Cos)
  <|> (string "exp"   *> pure Exp)
  <|> (string "floor" *> pure Floor)
  <|> (string "log10" *> pure Log10)
  <|> (string "log"   *> pure Ln)
  <|> (string "ln"    *> pure Ln)
  <|> (string "round" *> pure Round)
  <|> (string "sinh"  *> pure Sinh)
  <|> (string "sin"   *> pure Sin)
  <|> (string "sqrt"  *> pure Sqrt)
  <|> (string "tanh"  *> pure Tanh)
  <|> (string "tan"   *> pure Tan)

-- | A version of `sepBy1` that returns a `NonEmpty List`.
sepBy1 ∷ ∀ m s a sep. Monad m ⇒ ParserT s m a → ParserT s m sep → ParserT s m (NonEmpty List a)
sepBy1 p sep = do
  a ← p
  as ← many $ do
    sep
    p
  pure (a :| as)

-- | Fold a non-empty structure, collecting results using a binary operation.
foldr1 ∷ ∀ a. (a → a → a) → NonEmpty List a → a
foldr1 f (a :| xs) =
  case init xs, last xs of
    Just bs, Just b → f a (foldr f b bs)
    _, _ → a

-- | Parse a full mathematical expression.
expression ∷ P Expression
expression =
  fix \p →
    let
      atomic ∷ P Expression
      atomic = whiteSpace *> (
              parens p
          <|> (Scalar <$> number)
          <|> try (Unit <$> derivedUnit)
          <|> try (Apply <$> funcName <*> (parens p <* whiteSpace))
          <|> variable
          )

      suffixPow ∷ P Expression
      suffixPow = do
        a ← atomic
        mFn ← optionMaybe ((sqrOp *> pure square) <|> (cubOp *> pure cube))
        case mFn of
          Just fn → pure $ fn a
          Nothing → pure a

      sepByPow ∷ P Expression
      sepByPow = foldr1 (BinOp Pow) <$> suffixPow `sepBy1` powOp

      sepByMulImplicit ∷ P Expression
      sepByMulImplicit = foldl1 (BinOp Mul) <$> sepByPow `sepBy1` pure unit

      sepByDiv ∷ P Expression
      sepByDiv = foldl1 (BinOp Div) <$> sepByMulImplicit `sepBy1` divOp

      sepByMul ∷ P Expression
      sepByMul = foldl1 (BinOp Mul) <$> sepByDiv `sepBy1` mulOp

      prefixed ∷ P Expression
      prefixed = do
        prefixFn ← ((subOp *> pure Negate) <|> (addOp *> pure id) <|> pure id)
        prefixFn <$> sepByMul

      sepBySub ∷ P Expression
      sepBySub = foldl1 (BinOp Sub) <$> prefixed `sepBy1` subOp

      sepByAdd ∷ P Expression
      sepByAdd = foldl1 (BinOp Add) <$> sepBySub `sepBy1` addOp

      sepByConv ∷ P Expression
      sepByConv = foldl1 (BinOp ConvertTo) <$> sepByAdd `sepBy1` arrOp

    in sepByConv

  where

    powOp = reservedOp "^" <|> reservedOp "**"
    sqrOp = reservedOp "²"
    cubOp = reservedOp "³"
    divOp = reservedOp "/" <|> reservedOp "÷"
    mulOp = reservedOp "*" <|> reservedOp "·" <|> reservedOp "×"
    subOp = reservedOp "-"
    addOp = reservedOp "+"
    arrOp = reservedOp "->"

    square q = BinOp Pow q (Scalar 2.0)
    cube q = BinOp Pow q (Scalar 3.0)

-- | Parse a mathematical expression (or conversion) like `3m+2in -> cm`.
fullExpression ∷ P Expression
fullExpression = do
  whiteSpace
  expr ← expression
  eof <?> "end of input"

  pure $ expr

-- | Parse an Insect command.
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
  whiteSpace
  var ← token.identifier
  reservedOp "="
  value ← expression
  pure $ Assignment var value

-- | Parse a statement in the Insect language.
statement ∷ P Statement
statement =
      (Command <$> command)
  <|> try assignment
  <|> (Expression <$> fullExpression)

-- | Run the Insect-parser on a `String` input.
parseInsect ∷ String → Either ParseError Statement
parseInsect inp = runParser inp statement
