-- | This module defines the parser for the Insect language.
module Insect.Parser
  ( DictEntry(..)
  , (==>)
  , Dictionary(..)
  , commands
  , functions
  , siPrefixDict
  , normalUnitDict
  , imperialUnitDict
  , parseInsect
  ) where

import Prelude hiding (degree)

import Control.Alt ((<|>))
import Control.Lazy (fix)

import Quantities (DerivedUnit, (./))
import Quantities as Q

import Data.Array (some, fromFoldable)
import Data.Either (Either(..))
import Data.Decimal (Decimal, fromString, fromNumber, isFinite)
import Data.Foldable (foldr, foldMap)
import Data.Foldable as F
import Data.List (List, many, init, last)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, (:|), foldl1)
import Data.String (fromCharArray, singleton)
import Data.Tuple (Tuple(..))

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

-- | Possible characters for the first character of an identifier.
identStart ∷ P Char
identStart = letter <|> char '_'

-- | Possible characters for identifiers (not for the first character).
identLetter ∷ P Char
identLetter = letter <|> digit <|> char '_' <|> char '\''

-- | A list of allowed commands
commands ∷ Array String
commands = ["help", "?", "list", "ls", "ll", "reset", "clear", "cls", "quit", "exit"]

-- | The language definition.
insectLanguage ∷ LanguageDef
insectLanguage = LanguageDef
  { commentStart: ""
  , commentEnd: ""
  , commentLine: "#"
  , nestedComments: false
  , identStart: identStart
  , identLetter: identLetter
  , opStart: oneOf ['+', '-', '*', '·', '⋅', '×', '/', '÷', '^', '!', '→', '=']
  , opLetter: oneOf ['>', '*']
  , reservedNames: commands <> ["²", "³", "to", "per"]
  , reservedOpNames: ["->", "+", "-", "*", "·", "⋅", "×", "/", "÷", "^", "!",
                      "**", "="]
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
number ∷ P Decimal
number = do
  intPart ← digits

  mFracPart ← optionMaybe (append <$> string "." <*> digits)
  let fracPart = fromMaybe "" mFracPart

  mExpPart ← optionMaybe $ try do
    _ ← string "e"
    notFollowedBy identStart
    sad ← signAndDigits
    pure ("e" <> sad)
  let expPart = fromMaybe "" mExpPart

  whiteSpace

  let floatStr = intPart <> fracPart <> expPart

  case fromString floatStr of
    Just num →
      if isFinite num
      then pure num
      else fail "This number is too large"
    Nothing → fail $ "Parsing of number failed for input '" <> floatStr <> "'"

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
  [ Q.atto ==> ["atto", "a"]
  , Q.femto ==> ["femto", "f"]
  -- peta and mega have to be up here (before pico and milli) in order for the
  -- prefixes ('p' and 'm') not to be parsed as 'pico' or 'milli'.
  , Q.peta ==> ["peta"]
  , Q.mega ==> ["mega"]
  , Q.pico ==> ["pico", "p"]
  , Q.nano ==> ["nano", "n"]
  , Q.micro ==> [ "micro"
                , "µ" -- Micro sign U+00B5
                , "μ" -- Greek small letter mu U+039C
                ]
  , Q.milli ==> ["milli", "m"]
  , Q.centi ==> ["centi", "c"]
  , Q.deci ==> ["deci", "d"]
  , Q.hecto ==> ["hecto", "h"]
  , Q.kilo ==> ["kilo", "k"]
  , Q.mega ==> ["M"]
  , Q.giga ==> ["giga", "G"]
  , Q.tera ==> ["tera", "T"]
  , Q.peta ==> ["P"]
  , Q.exa ==> ["exa", "E"]
  ]

-- | Parse a SI prefix like `µ` or `G`.
siPrefix ∷ P (DerivedUnit → DerivedUnit)
siPrefix = buildDictParser siPrefixDict <|> pure id

-- | Normal (SI-conform, non-imperial) units
normalUnitDict ∷ Dictionary DerivedUnit
normalUnitDict = Dictionary
  [ Q.radian ==> ["radians", "radian", "rad"]
  , Q.degree ==> ["degrees", "degree", "deg", "°"]
  , Q.hertz ==> ["hertz", "Hz"]
  , Q.newton ==> ["newton", "N"]
  , Q.joule ==> ["joules", "joule", "J"]
  , Q.pascal ==> ["pascal", "Pa"]
  , Q.volt ==> ["volts", "volt", "V"]
  , Q.farad ==> ["farad", "F"]
  , Q.ohm ==> ["ohms", "ohm", "Ω"]
  , Q.sievert ==> ["sievert", "Sv"]
  , Q.weber ==> ["weber", "Wb"]
  , Q.tesla ==> ["tesla", "T"]
  , Q.henry ==> ["henry", "H"]
  , Q.coulomb ==> ["coulomb", "C"]
  , Q.siemens ==> ["siemens", "S"]
  , Q.lumen ==> ["lumen", "lm"]
  , Q.lux ==> ["lux", "lx"]
  , Q.becquerel ==> ["becquerel", "Bq"]
  , Q.gray ==> ["gray", "Gy"]
  , Q.katal ==> ["katal", "kat"]
  , Q.hectare ==> ["hectare", "ha"]
  , Q.liter ==> ["liters", "liter", "L"]
  , Q.tonne ==> ["tonnes", "tonne", "tons", "ton"]
  , Q.electronvolt ==> ["electronvolt", "eV"]
  , Q.ampere ==> ["amperes", "ampere", "A"]
  , Q.mole ==> ["mole", "mol"]
  , Q.kelvin ==> ["kelvin", "K"]
  , Q.candela ==> ["candela", "cd"]
  , Q.watt <> Q.hour ==> ["Wh"]
  , Q.watt ==> ["watts", "watt", "W"]
  , Q.byte ==> ["Bytes", "bytes", "Byte", "byte", "B"]
  , Q.bit ==> ["bits", "bit"]
  , Q.bit ./ Q.second ==> ["bps"]
  , Q.second ==> ["seconds", "second", "sec", "s"]
  , Q.minute ==> ["minutes", "minute", "min"]
  , Q.hour ==> ["hours", "hour", "h"]
  , Q.day ==> ["days", "day"]
  , Q.week ==> ["weeks", "week"]
  , Q.month ==> ["months", "month"]
  , Q.year ==> ["years", "year"]
  , Q.gram ==> ["grams", "gram", "g"]
  , Q.meter ==> ["meters", "meter", "m"]
  ]

-- | Parse a normal (SI-conform, non-imperical) unit, like `N` or `watt`.
normalUnit ∷ P DerivedUnit
normalUnit = buildDictParser normalUnitDict <?> "normal unit"

-- | Imperial units
imperialUnitDict ∷ Dictionary DerivedUnit
imperialUnitDict = Dictionary
  [ Q.mile ==> ["miles", "mile"]
  , Q.mile ./ Q.hour ==> ["mph"]
  , Q.inch ==> ["inches", "inch", "in"]
  , Q.yard ==> ["yards", "yard", "yd"]
  , Q.foot ==> ["feet", "foot", "ft"]
  , Q.ounce ==> ["ounces", "ounce", "oz"]
  , Q.pound ==> ["pounds", "pound", "lb"]
  , Q.gallon ==> ["gallons", "gallon", "gal"]
  , Q.pint ==> ["pints", "pint"]
  , Q.cup ==> ["cups", "cup"]
  , Q.tablespoon ==> ["tablespoons", "tablespoon", "tbsp"]
  , Q.teaspoon ==> ["teaspoons", "teaspoon", "tsp"]
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

specialCases ∷ P DerivedUnit
specialCases =
  -- The abbreviation 'd' for 'day' needs to be treated separately. Otherwise,
  -- 'cd' will be parsed as 'centi day' instead of 'candela'.
      string "d" *> pure Q.day
  -- Similarly, the abbreviation 't' for 'tonne' needs special treatment.
  -- Otherwise, 'ft' will be parsed as 'femto tonne' instead of 'feet'.
  <|> string "t" *> pure Q.tonne

-- | Parse a derived unit, like `km`, `ft`, or `s`.
derivedUnit ∷ P DerivedUnit
derivedUnit =
  (
        try (augment unitWithSIPrefix)
    <|> augment imperialUnit
    <|> augment normalUnit
    <|> augment specialCases
  ) <* whiteSpace
  where
    augment p = p <* notFollowedBy identLetter

-- | Parse the name of a variable, like `my_variable'`.
variable ∷ P Expression
variable = Variable <$> token.identifier

-- | Possible names for the mathematical functions.
funcNameDict ∷ Dictionary Func
funcNameDict = Dictionary
  [ Acosh ==> ["acosh"]
  , Acos ==> ["acos"]
  , Asinh ==> ["asinh"]
  , Asin ==> ["asin"]
  , Atanh ==> ["atanh"]
  , Atan ==> ["atan"]
  , Ceil ==> ["ceil"]
  , Cosh ==> ["cosh"]
  , Cos ==> ["cos"]
  , Exp ==> ["exp"]
  , Floor ==> ["floor"]
  , Gamma ==> ["gamma"]
  , Log10 ==> ["log10"]
  , Ln ==> ["log", "ln"]
  , Round ==> ["round"]
  , Sinh ==> ["sinh"]
  , Sin ==> ["sin"]
  , Sqrt ==> ["sqrt"]
  , Tanh ==> ["tanh"]
  , Tan ==> ["tan"]
  ]

-- | A list of all mathematical function names (for tab-completion).
functions ∷ Array String
functions =
  case funcNameDict of
    Dictionary dict → foldMap (\(_ ==> names) → names) dict

-- | Parse the name of a mathematical function.
funcName ∷ P Func
funcName = buildDictParser funcNameDict <?> "function name"

-- | A version of `sepBy1` that returns a `NonEmpty List`.
sepBy1 ∷ ∀ m s a sep. Monad m ⇒ ParserT s m a → ParserT s m sep → ParserT s m (NonEmpty List a)
sepBy1 p sep = do
  a ← p
  as ← many $ sep *> p
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

      suffixFac ∷ P Expression
      suffixFac = do
        a ← atomic
        mf ← optionMaybe (facOp *> pure Factorial)
        case mf of
          Just f → pure $ f a
          Nothing → pure a

      suffixPow ∷ P Expression
      suffixPow = do
        x ← suffixFac
        mFn ← optionMaybe ((sqrOp *> pure square) <|> (cubOp *> pure cube))
        case mFn of
          Just fn → pure $ fn x
          Nothing → pure x

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

    facOp = reservedOp "!"
    powOp = reservedOp "^" <|> reservedOp "**"
    -- these two need to be parsed as keywords in order to allow for other
    -- operators to follow (e.g. 3²*2)
    sqrOp = reserved "²"
    cubOp = reserved "³"
    divOp = reservedOp "/" <|> reservedOp "÷" <|> reserved "per"
    mulOp = reservedOp "*" <|> reservedOp "·" <|> reservedOp "⋅"
                           <|> reservedOp "×"
    subOp = reservedOp "-"
    addOp = reservedOp "+"
    arrOp = reservedOp "->" <|> reservedOp "→" <|> reserved "to"

    square q = BinOp Pow q (Scalar $ fromNumber 2.0)
    cube q = BinOp Pow q (Scalar $ fromNumber 3.0)

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
    <|> (reserved "list" <|> reserved "ls" <|> reserved "ll") *> pure List
    <|> (reserved "reset") *> pure Reset
    <|> (reserved "clear" <|> reserved "cls") *> pure Clear
    <|> (reserved "quit" <|> reserved "exit") *> pure Quit
  ) <* eof

-- | Parse a variable assignment like `x = 3m*pi`
assignment ∷ P (Tuple String Expression)
assignment = do
  whiteSpace
  var ← token.identifier
  reservedOp "="
  value ← expression
  eof
  pure $ Tuple var value

-- | Try to parse the identifier as a unit, and fail if it succeeds.
checkIdentifier ∷ Tuple String Expression → P Statement
checkIdentifier (Tuple var value) =
  case runParser var (derivedUnit <* eof) of
    Right _ →
      fail $ "'" <> var <> "' is reserved for a physical unit"
    Left _ →
      case runParser var (funcName <* eof) of
        Right _ →
          fail $ "'" <> var <> "' is reserved for a math. function"
        Left _ →
          pure $ Assignment var value

-- | Parse a statement in the Insect language.
statement ∷ P Statement
statement =
      (Command <$> command)
  <|> (try assignment >>= checkIdentifier)
  <|> (Expression <$> fullExpression)

-- | Run the Insect-parser on a `String` input.
parseInsect ∷ String → Either ParseError Statement
parseInsect inp = runParser inp statement
