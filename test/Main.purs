module Test.Main where

import Prelude hiding (degree)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)

import Data.StrMap (insert)
import Data.Either(Either(..))
import Data.Foldable (traverse_)

import Test.Unit (suite, test, failure)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)

import Text.Parsing.Parser (parseErrorMessage, parseErrorPosition)
import Text.Parsing.Parser.Pos (Position(..))

import Quantities ((./), (.*), milli, nano, meter, inch, hour, minute, kilo,
                   mile, gram, second, deci, tera, hertz, degree, radian)

import Insect.Language (Func(..), BinOp(..), Expression(..), Statement(..))
import Insect.Parser (parseInsect)
import Insect.Environment (Environment, initialEnvironment)
import Insect (repl)

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

expectOutput ∷ ∀ eff. Environment → String → String → Aff eff Unit
expectOutput env expected inp =
  let res = repl env inp
      out = res.msg
  in
    unless (out == expected) do
      failure $ "Unexpected result:\n" <>
                "Input:    '" <> inp <> "'\n" <>
                "Output:   '" <> out <> "'\n" <>
                "Expected: '" <> expected <> "'\n"


main ∷ Eff (console ∷ CONSOLE, testOutput ∷ TESTOUTPUT, avar ∷ AVAR) Unit
main = runTest do
  -- Helper to construct quantities
  let q s u = BinOp Mul (Scalar s) (Unit u)

  suite "Parser - Numbers" do
    test "Simple numbers" do
      allParseAs (Expression (Scalar 1.0))
        [ "1"
        , "1.0"
        , "  1  "
        , " 1.0000   "
        , " +1.0   "
        , "+1"
        ]

      allParseAs (Expression (Scalar 3.5))
        [ "3.5"
        , "  3.5  "
        , "3.50"
        , "+3.50"
        ]

      shouldFail "123.."
      shouldFail "0.."
      shouldFail ".0."
      shouldFail "."
      shouldFail ".2"

    test "Large numbers" do
      allParseAs (Expression (Scalar 1234567890000000.0))
        [ "1234567890000000"
        , "1234567890000000.0"
        , "+1234567890000000.0"
        ]

    test "Negative numbers" do
      shouldParseAs (Expression (Negate (Scalar 123.45)))
        "-123.45"

    test "Exponential notation" do
      shouldParseAs (Expression (Negate (Scalar 1.3e13)))
        "-1.3e13"

      shouldParseAs (Expression (Scalar 2.7e-3))
        "2.7e-3"

      shouldFail "2.7e 3"
      shouldFail "123e+"
      shouldFail "1223e++2"
      shouldFail "1223e+-2"

  suite "Parser - Units" do
    test "Simple" do
      allParseAs (Expression (Unit meter))
        [ "m"
        , "      m "
        , "meter"
        , "meters"
        ]

      allParseAs (Expression (Unit inch))
        [ "in"
        , "      in "
        , "inch"
        , "inches"
        ]

      allParseAs (Expression (Unit degree))
        [ "°"
        , "      ° "
        , "deg"
        , "degree"
        , "degrees"
        ]

  suite "Parser - Quantities" do
    test "Simple" do
      allParseAs (Expression (q 2.3 meter))
        [ "2.3*m"
        , " 2.3 * m "
        , "2.3m"
        , "  2.3 m "
        , "2.3meter"
        , "2.3 meter"
        , "2.3meters"
        ]

      allParseAs (Expression (q 5.0 second))
        [ "5s"
        , "5second"
        , "5 seconds"
        ]

      allParseAs (Expression (q 5.0 gram))
        [ "5g"
        , "5gram"
        , "5 grams"
        ]

      allParseAs (Expression (q 10.0 minute))
        [ "10minutes"
        , "10minute"
        , "10min"
        ]

      allParseAs (Expression (q 10.0 mile))
        [ "10miles"
        , "10mile"
        ]

      allParseAs (Expression (q 10.0 inch))
        [ "10inches"
        , "10inch"
        , "10in"
        ]

      allParseAs (Expression (q 360.0 degree))
        [ "360degrees"
        , "360degree"
        , "360deg"
        , "360°"
        , "360.0°"
        ]

      allParseAs (Expression (q 1.0 radian))
        [ "1.0rad"
        , "1.0 radian"
        , "1.0 radians"
        ]

    test "SI prefixes" do
      allParseAs (Expression (q 2.3 (kilo meter)))
        [ "2.3km"
        , "  2.3 km "
        , "  2.3 kmeter "
        , "  2.3 kmeters "
        ]

      allParseAs (Expression (q 2.3 (milli meter)))
        [ "2.3mm"
        , "  2.3 mm "
        , "  2.3 mmeter "
        , "  2.3 mmeters "
        ]

      allParseAs (Expression (q 2.3 (nano gram)))
        [ "2.3ng"
        , "  2.3 ng "
        ]

      allParseAs (Expression (q 2.3 (kilo minute)))
        [ "2.3kmin"
        , "  2.3 kmin "
        ]

      allParseAs (Expression (q 2.3 (deci meter)))
        [ "2.3dm"
        , "  2.3 dm "
        ]

      allParseAs (Expression (q 42.3 (tera hertz)))
        [ "42.3THz"
        , "42.3Thertz"
        ]

    test "Divisions" do
      allParseAs (Expression (BinOp Div (q 2.3 (kilo meter)) (Unit hour)))
        [ "2.3km/h"
        , "2.30km/h"
        , "2.30km÷h"
        ]

  suite "Parser - Operators" do
    test "Exponentiation" do
      allParseAs (Expression (BinOp Pow (Scalar 5.0) (Scalar 3.0))) $
        [ "5^3"
        , " 5 ^ 3 "
        , " ( 5 ) ^ ( 3 ) "
        , " ( ( 5 ) ^ ( 3 ) ) "
        , " ( 5 ^ 3 ) "
        , "5^(+3)"
        , "+5^3"
        ]

      shouldParseAs (Expression (BinOp Pow (Scalar 2.0) (BinOp Pow (Scalar 3.0) (BinOp Pow (Scalar 4.0) (Scalar 5.0))))) $
        "2^3^4^5"

      allParseAs (Expression (BinOp Pow (Scalar 4.0) (Scalar 3.0))) $
        [ "4^3"
        , "4 ^ 3"
        , "4**3"
        , "4 ** 3"
        , "4³"
        , "4  ³"
        ]

      allParseAs (Expression (BinOp Pow (Variable "pi") (Scalar 2.0))) $
        [ "pi^2"
        , "pi ^ 2"
        , "pi**2"
        , "pi ** 2"
        , "pi²"
        , "(pi)²"
        ]

      allParseAs (Expression (Negate (BinOp Pow (Scalar 3.0) (Scalar 4.0)))) $
        [ "-3^4"
        , "-3 ^ 4"
        , "-3**4"
        , "-3 ** 4"
        , "-(3^4)"
        ]

      allParseAs (Expression (BinOp Pow (Scalar 3.0) (Negate (Scalar 1.4)))) $
        [ "3 ^ (-1.4)"
        , "3 ** (-1.4)"
        ]

      shouldFail "³"
      shouldFail "³2"
      shouldFail "2^"
      shouldFail "^2"

    test "Multiplication" do
      allParseAs (Expression (BinOp Mul (Scalar 5.0) (Scalar 3.0))) $
        [ "5*3"
        , " 5 * 3 "
        , " ( 5 ) * ( 3 ) "
        , " ( 5 ) ( 3 ) "
        , " ( ( 5 ) * ( 3 ) ) "
        , " ( 5 * 3 ) "
        , "5(3)"
        , "(5)3"
        , "5(+3)"
        , "+5*3"
        ]

      allParseAs (Expression (BinOp Mul (Scalar 5.0) (Negate $ Scalar 3.0))) $
        [ "5*(-3)"
        , " 5 * (-3) "
        , " ( 5 ) * ( -3 ) "
        , " ( ( 5 ) * (-( 3 )) ) "
        , " ( 5 * (-3) ) "
        , "+5*(-3)"
        ]

      shouldFail "5*"

    test "Division" do
      allParseAs (Expression (BinOp Div (Scalar 5.0) (Scalar 3.0))) $
        [ "5/3"
        , " 5 / 3 "
        , " ( 5 ) / ( 3 ) "
        , " ( ( 5 ) / ( 3 ) ) "
        , " ( 5 / 3 ) "
        ]

      shouldFail "5/"

    test "Addition" do
      allParseAs (Expression (BinOp Add (Scalar 5.0) (Scalar 3.0))) $
        [ "5+3"
        , " 5 + 3 "
        , " ( 5 ) + ( 3 ) "
        , " ( ( 5 ) + ( 3 ) ) "
        , " ( 5 + 3 ) "
        ]

      shouldFail "3 + "
      shouldFail "3 + @"

    test "Subtraction" do
      allParseAs (Expression (BinOp Sub (Scalar 5.0) (Scalar 3.0))) $
        [ "5-3"
        , " 5 - 3 "
        , " ( 5 ) - ( 3 ) "
        , " ( ( 5 ) - ( 3 ) ) "
        , " ( 5 - 3 ) "
        ]

      shouldFail "3 - "

    test "Precedence" do
      allParseAs (Expression (BinOp Add (q 5.0 meter) (BinOp Mul (q 3.0 inch) (Scalar 7.0)))) $
        [ "5m+3in*7"
        , "5m+(3in*7)"
        , "5m+(3in·7)"
        , "5m+3in×7"
        , "  5 m +   3  in * 7    "
        ]

      allParseAs (Expression (BinOp Mul (BinOp Add (q 5.0 meter) (q 3.0 inch)) (Scalar 7.0))) $
        [ "(5m+3in)*7"
        , "((5m+3in))*7"
        , "(((((((5m))+((3in)))))))*((7))"
        , "  (  5 m + ( 3  in )  )  * 7    "
        ]

      allParseAs (Expression (BinOp Mul (BinOp Div (Scalar 5.0) (Scalar 3.0)) (Scalar 2.0)))
        [ "5/3*2"
        , "(5/3)*2"
        ]

      shouldFail "3+*4"
      shouldFail "3*/4"
      shouldFail "(3+4"
      shouldFail "3+4)"
      shouldFail "3+("
      shouldFail "()"
      shouldFail "(3+)4"

    test "Multiple division" do
      allParseAs (Expression (BinOp Div (BinOp Div (Scalar 42.0) (Scalar 7.0)) (Scalar 3.0))) $
        [ "42/7/3"
        , "(42/7)/3"
        ]

    test "Involving units" do
      allParseAs (Expression (BinOp Pow (q 3.0 meter) (Scalar 2.0))) $
        [ "(3m)^2"
        , "(3.0m)^(2.0)"
        ]

      allParseAs (Expression (BinOp Mul (Scalar 3.0) (BinOp Pow (Unit meter) (Scalar 2.0)))) $
        [ "3m^2"
        , "3.0(m)^(2.0)"
        , "3m²"
        , "3 m²"
        , "3·m²"
        , "3×m²"
        , "3·m^(2.0)"
        ]

      allParseAs (Expression (BinOp Div (q 3.0 meter) (Unit second))) $
        [ "3m/s"
        , "3 meter / second"
        , "(3m)/s"
        ]

      allParseAs (Expression (BinOp Mul (Scalar 3.0) (BinOp Div (Unit meter) (Unit second)))) $
        [ "3·m/s"
        , "3*meter / second"
        , "3*meter / sec"
        ]

      allParseAs (Expression (BinOp Pow (Unit meter) (Negate $ Scalar 1.0))) $
        [ "m^(-1)"
        , "m^(-1.0)"
        , "meter^(-1.0)"
        ]

  suite "Parser - Conversions" do
    test "Simple" do
      allParseAs (Expression (BinOp ConvertTo (q (2.3) meter) (Unit inch)))
        [ "2.3m -> in"
        , "  2.3 meters->inches "
        , "  2.3 m  ->  in "
        ]

      allParseAs (Expression (BinOp ConvertTo (q 120.0 minute) (Unit hour)))
        [ "120min -> h"
        , "120minutes -> hours"
        ]

      shouldFail "2.3m->"

    test "Complex units" do
      allParseAs (Expression (BinOp ConvertTo (BinOp Mul (Scalar 36.0) (BinOp Div (Unit (kilo meter)) (Unit hour))) (Unit (mile ./ hour))))
        [ "36·km/h -> mph"
        , " 36 · km / hour->mph"
        ]

      allParseAs (Expression (BinOp ConvertTo (BinOp Div (q 36.0 (kilo meter)) (Unit hour)) (Unit (mile ./ hour))))
        [ "36km/h -> mph"
        , " 36km / hour->mph "
        ]

  suite "Parser - Identifiers" do
    test "Valid and invalid names" do
      shouldParseAs (Expression (Variable "x")) "x"
      shouldParseAs (Expression (Variable "µ")) "µ"
      shouldParseAs (Expression (Variable "pi")) "pi"
      shouldParseAs (Expression (Variable "x_2")) "x_2"
      shouldParseAs (Expression (Variable "länge")) "länge"
      shouldParseAs (Expression (Variable "_prefixed")) "_prefixed"
      shouldParseAs (Expression (Variable "x'")) "x'"
      shouldParseAs (Expression (Variable "t''")) "t''"

      shouldFail "xs,as"
      shouldFail "hello$"

    test "Variables which begin like units" do
      shouldParseAs (Expression (Variable "myVariable")) "myVariable"
      shouldParseAs (Expression (Variable "density")) "density"

  suite "Parser - Functions" do
    test "Simple" do
      allParseAs (Expression (Apply Sin (q 30.0 degree)))
        [ "sin(30°)"
        , "  sin( 30° )  "
        , "  sin( +30° )  "
        ]

      allParseAs (Expression (Apply Sqrt (Scalar 2.0)))
        [ "sqrt(2)"
        , "  sqrt( 2.0 )  "
        , "  sqrt( +2.0 )  "
        ]

      allParseAs (Expression (Apply Exp (Negate $ Scalar 10.0)))
        [ "exp(-10)"
        , "  exp( -10 )  "
        ]

  suite "Parser - Assignments" do
    test "Simple" do
      allParseAs (Assignment "xyz_123" (Scalar 1.0)) $
        [ "xyz_123 = 1"
        , "xyz_123=1"
        , "  xyz_123  =  1  "
        ]

      shouldFail "x²=3"
      shouldFail "x+y=3"
      shouldFail "x+2=3"
      shouldFail "3=5"
      shouldFail "x="

  let expectOutput' = expectOutput initialEnvironment

  suite "Integration" do
    test "Simple" do
      expectOutput' "3m" "3m"
      expectOutput' "3m" " 3.0 meter  "

    test "Implicit multiplication" do
      let myEnv = insert "x" (5.0 .* meter) initialEnvironment
      expectOutput myEnv "5m" "x"
      expectOutput myEnv "10m" "2x"
      expectOutput myEnv "10m" "2 x"
      expectOutput myEnv "25m²" "x x"
      expectOutput myEnv "25m²" "x²"
      expectOutput myEnv "Unknown variable 'x2'" "x2"

    test "Function inverses" do
      expectOutput' "0.1234" "sin(asin(0.1234))"
      expectOutput' "0.1234" "cos(acos(0.1234))"
      expectOutput' "0.1234" "tan(atan(0.1234))"
      expectOutput' "0.1234" "sinh(asinh(0.1234))"
      expectOutput' "1.1234" "cosh(acosh(1.1234))"
      expectOutput' "0.1234" "tanh(atanh(0.1234))"
      expectOutput' "0.1234" "log(exp(0.1234))"

      expectOutput' "0.1234" "asin(sin(0.1234))"
      expectOutput' "0.1234" "acos(cos(0.1234))"
      expectOutput' "0.1234" "atan(tan(0.1234))"
      expectOutput' "0.1234" "asinh(sinh(0.1234))"
      expectOutput' "1.1234" "acosh(cosh(1.1234))"
      expectOutput' "0.1234" "atanh(tanh(0.1234))"
      expectOutput' "0.1234" "exp(log(0.1234))"

    test "Other functions" do
      expectOutput' "2" "sqrt(4)"
      expectOutput' "5" "log10(100000)"
      expectOutput' "15" "log(e^15)"
      expectOutput' "15" "ln(e^15)"
      expectOutput' "4" "ceil(3.1)"
      expectOutput' "3" "floor(3.9)"
      expectOutput' "4" "round(3.9)"
      expectOutput' "3" "round(3.1)"

    test "Simple" do
      expectOutput' "1080" "1920/16*9"
      expectOutput' "4294967296" "2^32"
      expectOutput' "0.512957" "sqrt(1.4^2 + 1.5^2) * cos(pi/3)^2"

      expectOutput' "2.5min" "2min + 30s"
      expectOutput' "150s" "2min + 30s -> sec"
      expectOutput' "904779000000km³" "4/3 * pi * (6000km)³"
      expectOutput' "58800kg·m·cm/s²" "40kg * 9.8m/s² * 150cm"
      expectOutput' "0.5" "sin(30°)"

      expectOutput' "26.8224m/s" "60mph -> m/s"
      expectOutput' "10km/h" "240km/day -> km/h"
      expectOutput' "0.0572958°" "1mrad -> °"
      expectOutput' "364d" "52weeks -> days"
      expectOutput' "73.66cm" "5in + 2ft -> cm"
      expectOutput' "8.53077°" "atan(30cm / 2m) -> °"

      expectOutput' "0.75" "3m/4m"
      expectOutput' "4" "4/2*2"
      expectOutput' "0.5s" "1/2 Hz -> s"

    test "Earth mass" do
      let env1 = initialEnvironment
          env2 = (repl env1 "r = 6000km").newEnv
          env3 = (repl env2 "vol = 4/3 * pi * r³").newEnv
          env4 = (repl env3 "density = 5g/cm³").newEnv

      expectOutput env4 "4.52389e+24kg" "vol * density -> kg"

    test "Pendulum" do
      let env1 = initialEnvironment
          env2 = (repl env1 "grav = 9.81m/s²").newEnv
          env3 = (repl env2 "L = 20cm").newEnv

      expectOutput env3 "897.14ms" "2pi*sqrt(L/grav) -> ms"

    test "Unicode" do
      expectOutput' "6.62607e-34J·s" "2π×ℏ"
