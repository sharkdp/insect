module Test.Main where

import Prelude hiding (degree)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)

import Data.Either(Either(..))
import Data.Foldable (traverse_)

import Test.Unit (suite, test, failure)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)

import Text.Parsing.Parser (parseErrorMessage, parseErrorPosition)
import Text.Parsing.Parser.Pos (Position(..))

import Quantities ((./), (.^), unity, milli, nano, meter, inch, hour, minute,
                   kilo, mile, gram, second, deci, tera, hertz, degree, radian)

import Insect.Language (BinOp(..), Expression(..), Statement(..))
import Insect.Parser (parseInsect)

shouldParseAs ∷ ∀ eff. Statement → String → Aff eff Unit
shouldParseAs expected input =
  case parseInsect input of
    Left err →
      case parseErrorPosition err of
        (Position pos) →
          failure $ "Parse error for input '" <> input <> "': "
                                <> parseErrorMessage err
                                <> " at position "
                                <> show pos.column
    Right output →
      unless (output == expected) $ do
        failure $ "Unexpected result:\n" <>
                  "Input:    '" <> input <> "'\n" <>
                  "Output:   " <> show output <> "\n" <>
                  "Expected: " <> show expected <> "\n"

allParseAs ∷ ∀ eff. Statement → Array String → Aff eff Unit
allParseAs expected = traverse_ (shouldParseAs expected)

shouldFail ∷ ∀ eff. String → Aff eff Unit
shouldFail input = do
  case parseInsect input of
   Left _ → pure unit
   Right output → failure $ "input is expected to throw a parse error: '" <> input <> "'"

main ∷ Eff (console ∷ CONSOLE, testOutput ∷ TESTOUTPUT, avar ∷ AVAR) Unit
main = runTest do
  suite "Parser - Numbers" do
    test "Simple numbers" do
      allParseAs (Expression (Q 1.0 unity))
        [ "1"
        , "1.0"
        , "  1  "
        , " 1.0000   "
        , " +1.0   "
        , "+1"
        ]

      allParseAs (Expression (Q 3.5 unity))
        [ "3.5"
        , "  3.5  "
        , "3.50"
        , "+3.50"
        ]

    test "Large numbers" do
      allParseAs (Expression (Q (1234567890000000.0) unity))
        [ "1234567890000000"
        , "1234567890000000.0"
        , "+1234567890000000.0"
        ]

    test "Negative numbers" do
      shouldParseAs (Expression (Negate (Q (123.45) unity)))
        "-123.45"

    test "Exponential notation" do
      shouldParseAs (Expression (Negate (Q 1.3e13 unity)))
        "-1.3e13"

      shouldParseAs (Expression (Q 2.7e-3 unity))
        "2.7e-3"

      shouldFail "2.7 e3"

      shouldFail "2.7e 3"


  suite "Parser - Quantities" do
    test "Simple" do
      allParseAs (Expression (Q 2.3 meter))
        [ "2.3m"
        , "  2.3 m "
        , "2.3meter"
        , "2.3 meter"
        , "2.3meters"
        ]

      allParseAs (Expression (Q 5.0 second))
        [ "5s"
        , "5second"
        , "5 seconds"
        ]

      allParseAs (Expression (Q 5.0 gram))
        [ "5g"
        , "5gram"
        , "5 grams"
        ]

      allParseAs (Expression (Q 10.0 mile))
        [ "10miles"
        , "10mile"
        ]

      allParseAs (Expression (Q 10.0 inch))
        [ "10inches"
        , "10inch"
        , "10in"
        ]

      allParseAs (Expression (Q 360.0 degree))
        [ "360degrees"
        , "360degree"
        , "360deg"
        , "360°"
        , "360.0°"
        ]

      allParseAs (Expression (Q 1.0 radian))
        [ "1.0"
        , "1.0rad"
        ]

      shouldFail "2.3yikes"

    test "SI prefixes" do
      allParseAs (Expression (Q 2.3 (kilo meter)))
        [ "2.3km"
        , "  2.3 km "
        , "  2.3 kmeter "
        , "  2.3 kmeters "
        ]

      allParseAs (Expression (Q 2.3 (milli meter)))
        [ "2.3mm"
        , "  2.3 mm "
        , "  2.3 mmeter "
        , "  2.3 mmeters "
        ]

      allParseAs (Expression (Q 2.3 (nano gram)))
        [ "2.3ng"
        , "  2.3 ng "
        ]

      allParseAs (Expression (Q 2.3 (kilo minute)))
        [ "2.3kmin"
        , "  2.3 kmin "
        ]

      allParseAs (Expression (Q 2.3 (deci meter)))
        [ "2.3dm"
        , "  2.3 dm "
        ]

      allParseAs (Expression (Q 42.3 (tera hertz)))
        [ "42.3THz"
        , "42.3Thertz"
        ]

      shouldFail "2.3k m"
      shouldFail "2.3m m"
      shouldFail "2.3n g"

    test "Divisions" do
      allParseAs (Expression (Q 2.3 (kilo meter ./ hour)))
        [ "2.3km/h"
        , "2.30km/h"
        ]


  suite "Parser - Operators" do
    test "Precedence" do
      allParseAs (Expression (BinOp Add (Q 5.0 meter) (BinOp Mul (Q 3.0 inch) (Q 7.0 unity)))) $
        [ "5m+3in*7"
        , "5m+(3in*7)"
        , "5m+(3in·7)"
        , "  5 m +   3  in * 7    "
        ]

      allParseAs (Expression (BinOp Mul (BinOp Add (Q 5.0 meter) (Q 3.0 inch)) (Q 7.0 unity))) $
        [ "(5m+3in)*7"
        , "((5m+3in))*7"
        , "(((((((5m))+((3in)))))))*((7))"
        , "  (  5 m + ( 3  in )  )  * 7    "
        ]

      shouldFail "3+*4"
      shouldFail "3*/4"
      shouldFail "(3+4"
      shouldFail "3+4)"
      shouldFail "3+("
      shouldFail "()"
      shouldFail "(3+)4"

    test "Multiple division" do
      allParseAs (Expression (BinOp Div (BinOp Div (Q 42.0 unity) (Q 7.0 unity)) (Q 3.0 unity))) $
        [ "42/7/3"
        , "(42/7)/3"
        ]

    test "Exponentiation" do
      allParseAs (Expression (BinOp Pow (Q 3.0 unity) (Q 4.0 unity))) $
        [ "3^4"
        , "3 ^ 4"
        , "3**4"
        , "3 ** 4"
        ]

      allParseAs (Expression (BinOp Pow (Q 3.0 unity) (Negate (Q 1.4 unity)))) $
        [ "3^-1.4"
        , "3**-1.4"
        , "3 ^ (-1.4)"
        , "3 ** (-1.4)"
        ]

      allParseAs (Expression (BinOp Pow (Q 3.0 meter) (Q 2.0 unity))) $
        [ "(3m)^2"
        , "(3.0m)^(2.0)"
        ]

      -- TODO
      --allParseAs (Expression (Q 3.0 (meter .^ 2.0))) $
      --  [ "3m^2"
      --  , "3.0(m)^(2.0)"
      --  ]

  suite "Parser - Conversions" do
    test "Simple" do
      allParseAs (Conversion (Q (2.3) meter) inch)
        [ "2.3m -> in"
        , "  2.3 meters->inches "
        , "  2.3 m  ->  in "
        ]

      allParseAs (Conversion (Q 120.0 minute) hour)
        [ "120min -> h"
        , "120minutes -> hours"
        ]

      shouldFail "2.3m->"
      shouldFail "2.3m->k"
      shouldFail "2.3m->yikes"

    test "Complex units" do
      allParseAs (Conversion (Q 36.0 (kilo meter ./ hour)) (mile ./ hour))
        [ "36km/h -> mph"
        ]
