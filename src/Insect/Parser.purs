-- | This module defines the parser for the Insect language.
module Insect.Parser
  ( DictEntry(..)
  , (==>)
  , Dictionary(..)
  , siPrefixDict
  , normalUnitDict
  , imperialUnitDict
  , parseInsect
  ) where

import Prelude hiding (degree)

import Control.Alt ((<|>))
import Control.Lazy (fix)

import Quantities (DerivedUnit, atto, bit, byte, centi, day, deci, degree, exa,
                   femto, foot, giga, gram, hecto, hertz, hour, inch, joule,
                   kilo, mega, meter, micro, mile, milli, minute, nano,
                   newton, ounce, peta, pico, pound, radian, second, tera,
                   watt, week, yard, (./), pascal, coulomb, volt, farad, ohm,
                   siemens, weber, tesla, henry, lumen, lux, becquerel, gray,
                   sievert, katal, hectare, liter, tonne, electronvolt, ampere,
                   mole, kelvin, candela)

import Data.Array (some, fromFoldable)
import Data.Either (Either)
import Data.Foldable (foldr)
import Data.Foldable as F
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
  , reservedNames: ["help", "?", "list", "ls", "reset", "clear", "cls", "quit",
                    "exit"]
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

-- | A helper type for entries in the dictionary.
data DictEntry a = DictEntry a (Array String)

infix 4 DictEntry as ==>

-- | A dictionary of units and their abbreviations.
data Dictionary a = Dictionary (Array (DictEntry a))

-- | Build a parser from a Dictionary
buildDictParser ∷ ∀ a. Dictionary a → P a
buildDictParser (Dictionary dict) = F.oneOf $ entryParser <$> dict
  where
    entryParser (x ==> abbrevs) = F.oneOf $ abbrevParser x <$> abbrevs
    abbrevParser x abbrev = string abbrev *> pure x

siPrefixDict ∷ Dictionary (DerivedUnit → DerivedUnit)
siPrefixDict = Dictionary
  [ atto ==> ["a"]
  , femto ==> ["f"]
  , pico ==> ["p"]
  , nano ==> ["n"]
  , micro ==> [ "µ" -- Micro sign U+00B5
              , "μ" -- Greek small letter mu U+039C
              ]
  , milli ==> ["m"]
  , centi ==> ["c"]
  , deci ==> ["d"]
  , hecto ==> ["h"]
  , kilo ==> ["k"]
  , mega ==> ["M"]
  , giga ==> ["G"]
  , tera ==> ["T"]
  , peta ==> ["P"]
  , exa ==> ["E"]
  ]

-- | Parse a SI prefix like `µ` or `G`.
siPrefix ∷ P (DerivedUnit → DerivedUnit)
siPrefix = buildDictParser siPrefixDict <|> pure id

-- | Normal (SI-conform, non-imperial) units
normalUnitDict ∷ Dictionary DerivedUnit
normalUnitDict = Dictionary
  [ radian ==> ["radians", "radian", "rad"]
  , degree ==> ["degrees", "degree", "deg", "°"]
  , hertz ==> ["hertz", "Hz"]
  , newton ==> ["newton", "N"]
  , joule ==> ["joules", "joule", "J"]
  , pascal ==> ["pascal", "Pa"]
  , volt ==> ["volt", "V"]
  , farad ==> ["farad", "F"]
  , ohm ==> ["ohm", "Ω"]
  , sievert ==> ["sievert", "Sv"]
  , weber ==> ["weber", "Wb"]
  , tesla ==> ["tesla", "T"]
  , henry ==> ["henry", "H"]
  , coulomb ==> ["coulomb", "C"]
  , siemens ==> ["siemens", "S"]
  , lumen ==> ["lumen", "lm"]
  , lux ==> ["lux", "lx"]
  , becquerel ==> ["becquerel", "Bq"]
  , gray ==> ["gray", "Gy"]
  , katal ==> ["katal", "kat"]
  , hectare ==> ["hectare", "ha"]
  , liter ==> ["liters", "liter", "L"]
  , tonne ==> ["tonnes", "tonne", "tons", "ton"]
  , electronvolt ==> ["electronvolt", "eV"]
  , ampere ==> ["ampere", "A"]
  , mole ==> ["mole", "mol"]
  , kelvin ==> ["kelvin", "K"]
  , candela ==> ["candela", "cd"]
  , watt ==> ["watts", "watt", "W"]
  , byte ==> ["Bytes", "bytes", "Byte", "byte", "B"]
  , bit ==> ["bits", "bit"]
  , bit ./ second ==> ["bps"]
  , second ==> ["seconds", "second", "sec", "s"]
  , minute ==> ["minutes", "minute", "min"]
  , hour ==> ["hours", "hour", "h"]
  , day ==> ["days", "day", "d"]
  , week ==> ["weeks", "week", "w"]
  , gram ==> ["grams", "gram", "g"]
  , meter ==> ["meters", "meter", "m"]
  ]

-- | Parse a normal (SI-conform, non-imperical) unit, like `N` or `watt`.
normalUnit ∷ P DerivedUnit
normalUnit = buildDictParser normalUnitDict <?> "normal unit"

-- | Imperial units
imperialUnitDict ∷ Dictionary DerivedUnit
imperialUnitDict = Dictionary
  [ mile ==> ["miles", "mile"]
  , mile ./ hour ==> ["mph"]
  , inch ==> ["inches", "inch", "in"]
  , yard ==> ["yards", "yard", "yd"]
  , foot ==> ["feet", "foot", "ft"]
  , ounce ==> ["ounces", "ounce", "oz"]
  , pound ==> ["pounds", "pound", "lb"]
  ]

-- | Parse an imperial unit like `ft` of `mile`.
imperialUnit ∷ P DerivedUnit
imperialUnit = buildDictParser imperialUnitDict <?> "imperial unit"

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
    <|> (reserved "clear" <|> reserved "cls") *> pure Clear
    <|> (reserved "quit" <|> reserved "exit") *> pure Quit
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
