-- | This module defines the parser for the Insect language.
module Insect.Parser
  ( DictEntry(..)
  , (==>)
  , Dictionary(..)
  , commands
  , prefixDict
  , normalUnitDict
  , imperialUnitDict
  , parseInsect
  ) where

import Prelude hiding (degree)

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (some, fromFoldable)
import Data.Decimal (Decimal, fromString, fromNumber, isFinite)
import Data.Either (Either, isRight)
import Data.Foldable (traverse_)
import Data.Foldable as F
import Data.List (List, many)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Semigroup.Foldable (foldl1, foldr1)
import Data.String (fromCodePointArray, codePointFromChar, singleton)
import Insect.Environment (Environment, StoredFunction(..))
import Insect.Language (BinOp(..), Expression(..), Command(..), Statement(..), Identifier)
import Quantities (DerivedUnit, (./))
import Quantities as Q
import Parsing (ParserT, Parser, ParseError, runParser, fail)
import Parsing.Combinators (option, optionMaybe, try, (<?>), notFollowedBy)
import Parsing.String (string, char, eof)
import Parsing.String.Basic (oneOf)
import Parsing.Token (GenLanguageDef(..), LanguageDef, TokenParser, digit, letter, makeTokenParser)

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
commands = ["help", "?", "list", "ls", "ll", "reset", "clear", "cls", "quit", "exit", "copy", "cp"]

-- | The language definition.
insectLanguage ∷ LanguageDef
insectLanguage = LanguageDef
  { commentStart: ""
  , commentEnd: ""
  , commentLine: "#"
  , nestedComments: false
  , identStart: identStart
  , identLetter: identLetter
  , opStart: oneOf ['+', '-', '*', '·', '⋅', '×', '/', '÷', '%', '^', '!', '→', '➞', '=']
  , opLetter: oneOf []
  , reservedNames: commands <> ["¹", "²", "³", "⁴", "⁵", "⁻¹", "⁻²", "⁻³", "⁻⁴", "⁻⁵", "to", "per"]
  , reservedOpNames: ["->", "+", "-", "*", "·", "⋅", "×", "/", "÷", "%", "^", "!",
                      "**", "=", ","]
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

-- | Parse a reserved keyword.
reserved ∷ String → P Unit
reserved = token.reserved

-- | Parse zero or more whitespace characters.
whiteSpace ∷ P Unit
whiteSpace = token.whiteSpace

-- | Parse a number.
number ∷ P Decimal
number = do
  decimalPart ← fractionalPart <|> do
    intPart ← digits
    mFracPart ← optionMaybe fractionalPart
    pure (intPart <> fromMaybe "" mFracPart)

  mExpPart ← optionMaybe $ try do
    _ ← string "e"
    notFollowedBy identStart
    sad ← signAndDigits
    pure ("e" <> sad)
  let expPart = fromMaybe "" mExpPart

  whiteSpace

  let floatStr = decimalPart <> expPart

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

    fractionalPart ∷ P String
    fractionalPart = (<>) <$> string "." <*> digits

    fromCharArray = fromCodePointArray <<< map codePointFromChar

    signAndDigits ∷ P String
    signAndDigits = do
      sign ← option '+' (oneOf ['+', '-'])
      intPart ← digits
      pure $ singleton (codePointFromChar sign) <> intPart

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

prefixDict ∷ Dictionary (DerivedUnit → DerivedUnit)
prefixDict = Dictionary
  [ Q.kibi ==> ["kibi", "Ki"]
  , Q.mebi ==> ["mebi", "Mi"]
  , Q.gibi ==> ["gibi", "Gi"]
  , Q.tebi ==> ["tebi", "Ti"]
  , Q.pebi ==> ["pebi", "Pi"]
  , Q.exbi ==> ["exbi", "Ei"]
  , Q.zebi ==> ["zebi", "Zi"]
  , Q.yobi ==> ["yobi", "Yi"]
  , Q.atto ==> ["atto", "a"]
  , Q.femto ==> ["femto", "f"]
  -- peta and mega have to be up here (before pico and milli) in order for the
  -- prefixes ('p' and 'm') not to be parsed as 'pico' or 'milli'.
  , Q.peta ==> ["peta"]
  , Q.mega ==> ["mega"]
  , Q.pico ==> ["pico", "p"]
  , Q.nano ==> ["nano", "n"]
  , Q.micro ==> [ "micro"
                , "u" -- u for micro
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

-- | Parse a SI or IEC prefix like `µ`, `G`, `pico` or `Ki`.
prefix ∷ P (DerivedUnit → DerivedUnit)
prefix = buildDictParser prefixDict <|> pure identity

-- | Normal (SI-conform, non-imperial) units
normalUnitDict ∷ Dictionary DerivedUnit
normalUnitDict = Dictionary
  [ Q.radian ==> ["radians", "radian", "rad"]
  , Q.degree ==> ["degrees", "degree", "deg", "°"]
  , Q.hertz ==> ["hertz", "Hz"]
  , Q.rpm ==> ["RPM", "rpm"]
  , Q.newton ==> ["newton", "N"]
  , Q.joule ==> ["joules", "joule", "J"]
  , Q.pascal ==> ["pascal", "Pa"]
  , Q.volt ==> ["volts", "volt", "V"]
  , Q.farad ==> ["farad", "F"]
  , Q.ohm ==> ["ohms", "ohm", "Ω"]
  , Q.sievert ==> ["sievert", "Sv"]
  , Q.weber ==> ["weber", "Wb"]
  , Q.tesla ==> ["tesla", "T"]
  , Q.henry ==> ["henrys", "henries", "henry", "H"]
  , Q.coulomb ==> ["coulomb", "C"]
  , Q.siemens ==> ["siemens", "S"]
  , Q.lumen ==> ["lumen", "lm"]
  , Q.lux ==> ["lux", "lx"]
  , Q.becquerel ==> ["becquerel", "Bq"]
  , Q.gray ==> ["gray", "Gy"]
  , Q.katal ==> ["katal", "kat"]
  , Q.hectare ==> ["hectare", "ha"]
  , Q.tonne ==> ["tonnes", "tonne", "tons", "ton"]
  , Q.electronvolt ==> ["electronvolt", "eV"]
  , Q.calorie ==> ["calories", "calorie", "cal"]
  , Q.bel ==> ["bel"]
  , Q.astronomicalUnit ==> ["AU", "au", "astronomicalunits", "astronomicalunit"]
  , Q.parsec ==> ["parsecs", "parsec", "pc"]
  , Q.lightyear ==> ["lightyears", "lightyear", "ly"]
  , Q.barn ==> ["barn"]
  , Q.bar ==> ["bar"]
  , Q.angstrom ==> ["angstrom", "Å"]
  , Q.gauss ==> ["gauss"]
  , Q.ampere ==> ["amperes", "ampere", "amps", "amp", "A"]
  , Q.mole ==> ["mole", "mol"]
  , Q.kelvin ==> ["kelvin", "K"]
  , Q.candela ==> ["candela", "cd"]
  , Q.watt <> Q.hour ==> ["Wh"]
  , Q.watt ==> ["watts", "watt", "W"]
  , Q.byte ==> ["Bytes", "bytes", "Byte", "byte", "B", "Octets", "octets", "Octet", "octet"]
  , Q.bit ==> ["bits", "bit"]
  , Q.bit ./ Q.second ==> ["bps"]
  , Q.second ==> ["seconds", "second", "sec", "s"]
  , Q.minute ==> ["minutes", "minute", "min"]
  , Q.hour ==> ["hours", "hour", "h"]
  , Q.day ==> ["days", "day"]
  , Q.week ==> ["weeks", "week"]
  , Q.fortnight ==> ["fortnights", "fortnight"]
  , Q.month ==> ["months", "month"]
  , Q.year ==> ["years", "year"]
  , Q.julianYear ==> ["julianYears", "julianYear"]
  , Q.gram ==> ["grammes", "gramme", "grams", "gram", "g"]
  , Q.meter ==> ["metres", "metre", "meters", "meter", "m"]
  , Q.liter ==> ["liters", "liter", "litres", "litre", "L", "l"]
  , Q.atm ==> ["atm"]
  , Q.pixel ==> ["pixels", "pixel", "px"]
  , Q.frame ==> ["frames", "frame"]
  , Q.frame ./ Q.second ==> ["fps"]
  , Q.dot ==> ["dots", "dot"]
  ]

-- | Parse a normal (SI-conform, non-imperial) unit, like `N` or `watt`.
normalUnit ∷ P DerivedUnit
normalUnit = buildDictParser normalUnitDict <?> "normal unit"

-- | Imperial units
imperialUnitDict ∷ Dictionary DerivedUnit
imperialUnitDict = Dictionary
  [ Q.percent ==> ["pct", "percent"]
  , Q.partsPerMillion ==> ["ppm"]
  , Q.partsPerBillion ==> ["ppb"]
  , Q.partsPerTrillion ==> ["ppt"]
  , Q.partsPerQuadrillion ==> ["ppq"]
  , Q.mile ==> ["miles", "mile"]
  , Q.mile ./ Q.hour ==> ["mph"]
  , Q.inch ==> ["inches", "inch", "in"]
  , Q.yard ==> ["yards", "yard", "yd"]
  , Q.foot ==> ["feet", "foot", "ft"]
  , Q.thou ==> ["thou", "mils", "mil"]
  , Q.ounce ==> ["ounces", "ounce", "oz"]
  , Q.lbf ==> ["pound_force", "lbf"]
  , Q.pound ==> ["pounds", "pound", "lb"]
  , Q.gallon ==> ["gallons", "gallon", "gal"]
  , Q.pint ==> ["pints", "pint"]
  , Q.cup ==> ["cups", "cup"]
  , Q.tablespoon ==> ["tablespoons", "tablespoon", "tbsp"]
  , Q.teaspoon ==> ["teaspoons", "teaspoon", "tsp"]
  , Q.fluidounce ==> ["fluidounces", "fluidounce", "floz"]
  , Q.furlong ==> ["furlong"]
  , Q.btu ==> ["BTU"]
  , Q.psi ==> ["psi"]
  , Q.mmHg ==> ["mmHg"]
  , Q.hogshead ==> ["hogsheads", "hogshead"]
  , Q.rod ==> ["rods", "rod"]
  , Q.pixel ./ Q.inch ==> ["ppi"]
  , Q.dot ./ Q.inch ==> ["dpi"]
  , Q.piece ==> ["pieces", "piece"]
  , Q.person ==> ["persons", "person", "people"]
  , Q.dollar ==> ["dollars", "dollar", "USD", "$"]
  , Q.euro ==> ["euros", "euro", "EUR", "€"]
  , Q.knot ==> ["knots", "knot", "kn", "kt"]
  , Q.nauticalMile ==> ["M", "NM", "nmi"]
  ]

-- | Parse an imperial unit like `ft` of `mile`.
imperialUnit ∷ P DerivedUnit
imperialUnit = buildDictParser imperialUnitDict <?> "imperial unit"

-- | Parse a 'normal' unit with SI prefix, like `km` or `Gb`.
unitWithSIPrefix ∷ P DerivedUnit
unitWithSIPrefix = do
  p ← prefix
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

-- | A version of `sepBy1` that returns a `NonEmpty List`.
sepBy1 ∷ ∀ m s a sep. ParserT s m a → ParserT s m sep → ParserT s m (NonEmpty List a)
sepBy1 p sep = do
  a ← p
  as ← many $ sep *> p
  pure (a :| as)

-- | Parse a function name and fail if it's not in the environment
function ∷ Environment → P Identifier
function env = do
  name ← token.identifier
  if name == "sum" || name == "product"
    then
      pure name
    else
      case lookup name env.functions of
        Just (StoredFunction _ _ _) → pure name
        Nothing → fail ("Unknown function '" <> name <> "'")

-- | Parse a full mathematical expression.
expression ∷ Environment → P Expression
expression env =
  fix \p →
    let
      atomic ∷ P Expression
      atomic = whiteSpace *> (
              parens p
          <|> (Scalar <$> number)
          <|> try (Unit <$> derivedUnit)
          <|> try (Apply <$> function env <*> parens (sepBy1 p commaOp))
          <|> variable
          )

      suffixFac ∷ P Expression
      suffixFac = do
        a ← atomic
        mf ← optionMaybe (facOp *> pure Factorial)
        pure case mf of
          Just f → f a
          Nothing → a

      suffixPow ∷ P Expression
      suffixPow = do
        x ← suffixFac
        mFn ← optionMaybe (     reservedOp "¹" *>  pure (powPos 1.0)
                            <|> reservedOp "²" *>  pure (powPos 2.0)
                            <|> reservedOp "³" *>  pure (powPos 3.0)
                            <|> reservedOp "⁴" *>  pure (powPos 4.0)
                            <|> reservedOp "⁵" *>  pure (powPos 5.0)
                            <|> reservedOp "⁻¹" *> pure (powNeg 1.0)
                            <|> reservedOp "⁻²" *> pure (powNeg 2.0)
                            <|> reservedOp "⁻³" *> pure (powNeg 3.0)
                            <|> reservedOp "⁻⁴" *> pure (powNeg 4.0)
                            <|> reservedOp "⁻⁵" *> pure (powNeg 5.0)
                          )
        pure case mFn of
          Just fn → fn x
          Nothing → x

      -- The power operator needs special treatment
      -- 1) it is right-associative => use foldr1 instead of foldl1
      -- 2) the unary minus on the *left* side of the power operator has
      --    a lower precedence, i.e. -2^3 = -(2^3). However, unary minus
      --    on the *right* side has a higher precedence: 2^-3 = 2^(-3).
      sepByPow ∷ P Expression
      sepByPow = fix \e → foldr1 (BinOp Pow) <$> list e
        where
          list e = do
            a ← suffixPow
            as ← many do
              powOp
              func ← (subOp *> pure Negate) <|> (addOp *> pure identity) <|> pure identity
              expr ← e
              pure (func expr)
            pure (a :| as)

      sepByMulImplicit ∷ P Expression
      sepByMulImplicit = foldl1 (BinOp Mul) <$> sepByPow `sepBy1` pure unit

      prefixed ∷ P Expression
      prefixed = fix \e →
            (subOp *> (Negate <$> e))
        <|> (addOp *> e)
        <|> sepByMulImplicit

      sepByMod ∷ P Expression
      sepByMod = foldl1 (BinOp Mod) <$> prefixed `sepBy1` modOp

      sepByPer ∷ P Expression
      sepByPer = foldl1 (BinOp Div) <$> sepByMod `sepBy1` perOp

      sepByDiv ∷ P Expression
      sepByDiv = foldl1 (BinOp Div) <$> sepByPer `sepBy1` divOp

      sepByMul ∷ P Expression
      sepByMul = foldl1 (BinOp Mul) <$> sepByDiv `sepBy1` mulOp

      sepBySub ∷ P Expression
      sepBySub = foldl1 (BinOp Sub) <$> sepByMul `sepBy1` subOp

      sepByAdd ∷ P Expression
      sepByAdd = foldl1 (BinOp Add) <$> sepBySub `sepBy1` addOp

      sepByConv ∷ P Expression
      sepByConv = foldl1 (BinOp ConvertTo) <$> sepByAdd `sepBy1` arrOp

    in sepByConv

  where

    commaOp = reservedOp ","
    facOp = reservedOp "!"
    powOp = reservedOp "^" <|> reservedOp "**"
    modOp = reservedOp "%"
    perOp = reserved "per"
    divOp = reservedOp "/" <|> reservedOp "÷"
    mulOp = reservedOp "*" <|> reservedOp "·" <|> reservedOp "⋅"
                           <|> reservedOp "×"
    subOp = reservedOp "-"
    addOp = reservedOp "+"
    arrOp = reservedOp "->" <|> reservedOp "→" <|> reservedOp "➞" <|> reserved "to"

    powPos s q | s == 1.0  = q
               | otherwise = BinOp Pow q (Scalar $ fromNumber s)
    powNeg s q = BinOp Pow q (Negate $ Scalar $ fromNumber s)

-- | Parse a mathematical expression (or conversion) like `3m+2in -> cm`.
fullExpression ∷ Environment → P Expression
fullExpression env = whiteSpace *> expression env <* (eof <?> "end of input")

-- | Parse an Insect command.
command ∷ P Command
command =
  (
        (reserved "help" <|> reserved "?") *> pure Help
    <|> (reserved "list" <|> reserved "ls" <|> reserved "ll") *> pure List
    <|> (reserved "reset") *> pure Reset
    <|> (reserved "clear" <|> reserved "cls") *> pure Clear
    <|> (reserved "copy" <|> reserved "cp") *> pure Copy
    <|> (reserved "quit" <|> reserved "exit") *> pure Quit
  ) <* eof

-- | Parse a variable- or function assignment.
assignment ∷ Environment → P Statement
assignment env = do
  { name, args, expr } ←
    try do
      whiteSpace
      name ← token.identifier
      args ← optionMaybe (parens (sepBy1 token.identifier (reservedOp ",")))
      reservedOp "="
      expr ← expression env
      eof

      pure { name, args, expr }

  failIfReserved name

  case args of
    Nothing → pure (VariableAssignment name expr)
    Just xs → do
      traverse_ failIfReserved xs
      pure (FunctionAssignment name xs expr)

  where
    failIfReserved n = do
      when (isRight $ runParser n (derivedUnit <* eof)) $
        fail ("'" <> n <> "' is reserved for a physical unit")

      when (n == "_" || n == "ans") $
        fail ("'" <> n <> "' is a reserved variable name")

-- | Parse a statement in the Insect language.
statement ∷ Environment → P Statement
statement env =
      (Command <$> command)
  <|> assignment env
  <|> (try (whiteSpace *> (PrettyPrintFunction <$> function env) <* eof))
  <|> (Expression <$> fullExpression env)

-- | Run the Insect-parser on a `String` input.
parseInsect ∷ Environment → String → Either ParseError Statement
parseInsect env inp = runParser inp (statement env)
