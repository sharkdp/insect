module Test.Main (main) where

import Prelude hiding (degree)

import Effect (Effect)
import Effect.Aff (Aff)

import Data.Decimal (fromNumber)
import Data.Either (Either(..))
import Data.Foldable (traverse_, for_, intercalate)
import Data.List (List(..), (:))
import Data.NonEmpty ((:|))
import Data.Map (insert, keys)

import Test.Unit (suite, test, failure)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)

import Parsing (Position(..), parseErrorMessage, parseErrorPosition)

import Quantities ((./), (.*), milli, nano, meter, inch, hour, minute, kilo,
                   mile, gram, second, deci, tera, hertz, degree, radian,
                   day, tonne, euro)

import Insect.Language (BinOp(..), Expression(..), Statement(..))
import Insect.Parser (Dictionary(..), DictEntry, (==>), prefixDict,
                      normalUnitDict, imperialUnitDict, parseInsect)
import Insect.Environment (StorageType(..), StoredValue(..), Environment,
                           initialEnvironment)
import Insect.Format (format, fmtPlain)
import Insect.PrettyPrint (pretty)
import Insect (repl)

shouldParseAs ∷ Statement → String → Aff Unit
shouldParseAs expected input =
  case parseInsect initialEnvironment input of
    Left err →
      case parseErrorPosition err of
        Position pos →
          failure $ "Parse error for input '" <> input <> "': "
                                <> parseErrorMessage err
                                <> " at position "
                                <> show pos.column
    Right output →
      unless (output == expected) do
        failure $ "Unexpected result:\n" <>
                  "Input:    '" <> input <> "'\n" <>
                  "Output:   " <> show output <> "\n" <>
                  "Expected: " <> show expected <> "\n"

allParseAs ∷ Statement → Array String → Aff Unit
allParseAs expected = traverse_ (shouldParseAs expected)

shouldFail ∷ String → Aff Unit
shouldFail input =
  case parseInsect initialEnvironment input of
   Left _ → pure unit
   Right _ → failure $ "input is expected to throw a parse error: '" <> input <> "'"

expectOutput ∷ Environment → String → String → Aff Unit
expectOutput env expected inp =
  let { msg: out } = repl fmtPlain env inp
  in
    unless (out == expected) do
      failure $ "Unexpected result:\n" <>
                "Input:    '" <> inp <> "'\n" <>
                "Output:   '" <> out <> "'\n" <>
                "Expected: '" <> expected <> "'\n"

prettyPrintCheck ∷ String → Aff Unit
prettyPrintCheck input =
  case parseInsect initialEnvironment input of
    Left err →
      case parseErrorPosition err of
        Position pos →
          failure $ "Parse error for input '" <> input <> "': "
                                <> parseErrorMessage err
                                <> " at position "
                                <> show pos.column
    Right output@(Expression expr) →
      shouldParseAs output (format fmtPlain (pretty expr))
    _ → failure "Input is not an expression"


main ∷ Effect Unit
main = runTest do
  -- Helper to construct scalars
  let scalar n = Scalar (fromNumber n)

  -- Helper to construct quantities
  let q s u = BinOp Mul (scalar s) (Unit u)

  suite "Parser - Numbers" do
    test "Simple numbers" do
      allParseAs (Expression (scalar 1.0))
        [ "1"
        , "1.0"
        , "  1  "
        , " 1.0000   "
        , " +1.0   "
        , "+1"
        ]

      allParseAs (Expression (scalar 3.5))
        [ "3.5"
        , "  3.5  "
        , "3.50"
        , "+3.50"
        ]

      allParseAs (Expression (scalar 0.2))
        [ "0.2"
        , "  0.2  "
        , "+0.2 "
        , ".2"
        , "+.2"
        ]

      allParseAs (Expression (Negate (scalar 0.61)))
        [ "-0.61"
        , "-.61"
        , "-   .61"
        ]

      shouldParseAs (Expression (scalar 0.05)) ".05"

      shouldFail "123.."
      shouldFail "0.."
      shouldFail ".0."
      shouldFail "."
      shouldFail ". 2"
      shouldFail "..2"

    test "Large numbers" do
      allParseAs (Expression (scalar 1234567890000000.0))
        [ "1234567890000000"
        , "1234567890000000.0"
        , "+1234567890000000.0"
        ]

    test "Negative numbers" do
      shouldParseAs (Expression (Negate (scalar 123.45)))
        "-123.45"

    test "Exponential notation" do
      shouldParseAs (Expression (Negate (scalar 1.3e13)))
        "-1.3e13"

      shouldParseAs (Expression (scalar 2.7e-3))
        "2.7e-3"

      shouldFail "123e+"
      shouldFail "123e-"

  suite "Parser - Units (this may take some time)" do
    test "Simple" do
      allParseAs (Expression (Unit meter))
        [ "m"
        , "      m "
        , "meter"
        , "meters"
        , "metre"
        , "metres"
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

    let
      unp ∷ ∀ a. Dictionary a → Array (DictEntry a)
      unp (Dictionary dict) = dict

    test "All imperial units" do
      for_ (unp imperialUnitDict) \(unit ==> unitStrs) →
        allParseAs (Expression (Unit  unit))
                   unitStrs

    test "All normal units" do
      for_ (unp normalUnitDict) \(unit ==> unitStrs) →
        allParseAs (Expression (Unit  unit))
                   unitStrs

    for_ (unp prefixDict) \(prefix ==> prefixStrs) →
      test ("Testing all units with prefix: " <> intercalate ", " prefixStrs) do
        for_ (unp normalUnitDict) \(unit ==> unitStrs) →
          allParseAs (Expression (Unit (prefix unit)))
                     ((<>) <$> prefixStrs <*> unitStrs)

    test "Special cases" do
      allParseAs (Expression (Unit day))
        [ "d"
        , " d "
        ]

      allParseAs (Expression (Unit tonne))
        [ "t"
        , " t "
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
        , "2.3 metre"
        , "2.3metres"
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
        , "  2.3 kmetre "
        , "  2.3 kmetres "
        ]

      allParseAs (Expression (q 2.3 (milli meter)))
        [ "2.3mm"
        , "  2.3 mm "
        , "  2.3 mmeter "
        , "  2.3 mmeters "
        , "  2.3 mmetre "
        , "  2.3 mmetres "
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
        , "2.30 kilometer per hour"
        , "2.30 kilometre per hour"
        , "2.30km per h"
        ]

  suite "Parser - Operators" do
    test "Factorial" do
      allParseAs (Expression (Factorial (scalar 4.0)))
        [ "4!"
        , "4.0!"
        , "4 !"
        , " 4 ! "
        , "(4)!"
        ]

      allParseAs (Expression (BinOp Pow (Factorial (scalar 5.0)) (scalar 3.0)))
        [ "5!^3"
        , "5!³"
        , "(5!)^3"
        ]

      allParseAs (Expression (Negate (Factorial (scalar 5.0))))
        [ "-5!"
        , "-(5!)"
        ]

    test "Exponentiation" do
      allParseAs (Expression (BinOp Pow (scalar 5.0) (scalar 3.0)))
        [ "5^3"
        , " 5 ^ 3 "
        , " ( 5 ) ^ ( 3 ) "
        , " ( ( 5 ) ^ ( 3 ) ) "
        , " ( 5 ^ 3 ) "
        , "5^(+3)"
        , "+5^3"
        ]

      shouldParseAs (Expression (BinOp Pow (scalar 2.0) (BinOp Pow (scalar 3.0) (BinOp Pow (scalar 4.0) (scalar 5.0)))))
        "2^3^4^5"

      allParseAs (Expression (BinOp Pow (scalar 4.0) (scalar 3.0)))
        [ "4^3"
        , "4 ^ 3"
        , "4**3"
        , "4 ** 3"
        , "4³"
        , "4  ³"
        ]

      allParseAs (Expression (BinOp Pow (Variable "pi") (scalar 2.0)))
        [ "pi^2"
        , "pi ^ 2"
        , "pi**2"
        , "pi ** 2"
        , "pi²"
        , "(pi)²"
        ]

      allParseAs (Expression (Negate (BinOp Pow (scalar 3.0) (scalar 4.0))))
        [ "-3^4"
        , "-3 ^ 4"
        , "-3**4"
        , "-3 ** 4"
        , "-(3^4)"
        ]

      allParseAs (Expression (BinOp Pow (scalar 3.0) (Negate (scalar 1.4))))
        [ "3 ^ (-1.4)"
        , "3 ** (-1.4)"
        ]

      shouldFail "³"
      shouldFail "³2"
      shouldFail "2^"
      shouldFail "^2"

    test "Multiplication" do
      allParseAs (Expression (BinOp Mul (scalar 5.0) (scalar 3.0)))
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

      allParseAs (Expression (BinOp Mul (scalar 5.0) (Negate $ scalar 3.0)))
        [ "5*(-3)"
        , " 5 * (-3) "
        , " ( 5 ) * ( -3 ) "
        , " ( ( 5 ) * (-( 3 )) ) "
        , " ( 5 * (-3) ) "
        , "+5*(-3)"
        ]

      shouldFail "5*"

    test "Division" do
      allParseAs (Expression (BinOp Div (scalar 5.0) (scalar 3.0)))
        [ "5/3"
        , "5 per 3"
        , " 5 / 3 "
        , " ( 5 ) / ( 3 ) "
        , " ( ( 5 ) / ( 3 ) ) "
        , " ( 5 / 3 ) "
        ]

      shouldFail "5/"
      shouldFail "5 per"

    test "Modulo" do
      allParseAs (Expression (BinOp Mod (scalar 5.0) (scalar 3.0)))
        [ "5%3"
        , "5 % 3"
        , " ( 5 ) % ( 3 ) "
        , " ( ( 5 ) % ( 3 ) ) "
        , " ( 5 % 3 ) "
        ]

      shouldFail "5%"
      shouldFail "%2"

    test "Addition" do
      allParseAs (Expression (BinOp Add (scalar 5.0) (scalar 3.0)))
        [ "5+3"
        , " 5 + 3 "
        , " ( 5 ) + ( 3 ) "
        , " ( ( 5 ) + ( 3 ) ) "
        , " ( 5 + 3 ) "
        ]

      shouldFail "3 + "
      shouldFail "3 + @"

    test "Subtraction" do
      allParseAs (Expression (BinOp Sub (scalar 5.0) (scalar 3.0)))
        [ "5-3"
        , " 5 - 3 "
        , " ( 5 ) - ( 3 ) "
        , " ( ( 5 ) - ( 3 ) ) "
        , " ( 5 - 3 ) "
        ]

      shouldFail "3 - "

    test "Unary operators" do
      shouldParseAs (Expression (Negate (Variable "x")))
        "-x"

      shouldParseAs (Expression (Variable "x"))
        "+x"

      shouldParseAs (Expression (Negate (Negate (Variable "x"))))
        "--x"

      shouldParseAs (Expression (Negate (Variable "x")))
        "++-x"

      shouldParseAs (Expression (BinOp Mul (Negate $ scalar 1.0) (Negate $ scalar 2.0)))
        "-1 * -2"

      shouldParseAs (Expression (BinOp Mul (scalar 1.0) (scalar 2.0)))
        "+1 * +2"

      shouldParseAs (Expression (BinOp Sub (Negate $ scalar 1.0) (Negate $ scalar 2.0)))
        "-1 - -2"

      shouldParseAs (Expression (BinOp Sub (scalar 1.0) (scalar 2.0)))
        "+1 - +2"

      shouldParseAs (Expression (Negate (BinOp Pow (scalar 2.0) (scalar 3.0))))
        "-2^3"

      shouldParseAs (Expression (Negate (BinOp Pow (scalar 2.0) (scalar 3.0))))
        "-2³"

      shouldParseAs (Expression (BinOp Pow (scalar 2.0) (Negate $ scalar 3.0)))
        "2^-3"

      shouldParseAs (Expression (BinOp Pow (scalar 2.0) (scalar 3.0)))
        "2^+3"

      shouldParseAs (Expression (BinOp Pow (scalar 2.0)
                                           (Negate (BinOp Pow (scalar 3.0)
                                                              (Negate (scalar 4.0))))))
        "2^-3^-4"

    test "Precedence" do
      allParseAs (Expression (BinOp Add (q 5.0 meter) (BinOp Mul (q 3.0 inch) (scalar 7.0))))
        [ "5m+3in*7"
        , "5m+(3in*7)"
        , "5m+(3in·7)"
        , "5m+(3in⋅7)"
        , "5m+3in×7"
        , "  5 m +   3  in * 7    "
        ]

      allParseAs (Expression (BinOp Mul (BinOp Add (q 5.0 meter) (q 3.0 inch)) (scalar 7.0)))
        [ "(5m+3in)*7"
        , "((5m+3in))*7"
        , "(((((((5m))+((3in)))))))*((7))"
        , "  (  5 m + ( 3  in )  )  * 7    "
        ]

      allParseAs (Expression (BinOp Mul (BinOp Div (scalar 5.0) (scalar 3.0)) (scalar 2.0)))
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
      allParseAs (Expression (BinOp Div (BinOp Div (scalar 42.0) (scalar 7.0)) (scalar 3.0)))
        [ "42/7/3"
        , "(42/7)/3"
        , "42 per 7 per 3"
        ]

    test "Multiple division - per precedence" do
      allParseAs (Expression (BinOp Div (Unit euro) (BinOp Div (Unit meter) (Unit second))))
        [ "euro / (meter / second)"
        , "euro / meter per second"
        , "euro per (meter per second)"
        ]

    test "Involving units" do
      allParseAs (Expression (BinOp Pow (q 3.0 meter) (scalar 2.0)))
        [ "(3m)^2"
        , "(3.0m)^(2.0)"
        ]

      allParseAs (Expression (BinOp Mul (scalar 3.0) (BinOp Pow (Unit meter) (scalar 2.0))))
        [ "3m^2"
        , "3.0(m)^(2.0)"
        , "3m²"
        , "3 m²"
        , "3·m²"
        , "3⋅m²"
        , "3×m²"
        , "3·m^(2.0)"
        ]

      allParseAs (Expression (BinOp Div (q 3.0 meter) (Unit second)))
        [ "3m/s"
        , "3 meter / second"
        , "3 meter per second"
        , "3 metre / second"
        , "3 metre per second"
        , "(3m)/s"
        ]

      allParseAs (Expression (BinOp Mul (scalar 3.0) (BinOp Div (Unit meter) (Unit second))))
        [ "3·m/s"
        , "3*meter / second"
        , "3*meter / sec"
        , "3*metre / second"
        , "3*metre / sec"
        ]

      allParseAs (Expression (BinOp Pow (Unit meter) (Negate $ scalar 1.0)))
        [ "m^(-1)"
        , "m^(-1.0)"
        , "meter^(-1.0)"
        , "metre^(-1.0)"
        ]

  suite "Parser - Conversions" do
    test "Simple" do
      allParseAs (Expression (BinOp ConvertTo (q (2.3) meter) (Unit inch)))
        [ "2.3m -> in"
        , "  2.3 meters->inches "
        , "  2.3 m  ->  in "
        , "2.3m→in"
        , "  2.3 m  →  in "
        , "2.3m to in"
        , "  2.3 m  to  in "
        ]

      allParseAs (Expression (BinOp ConvertTo (q 120.0 minute) (Unit hour)))
        [ "120min -> h"
        , "120minutes -> hours"
        ]

      shouldFail "2.3m->"
      shouldFail "-> km"
      shouldFail "2.3m →"
      shouldFail "2.3m to"
      shouldFail "to km"
      shouldFail "to"

    test "Complex units" do
      allParseAs (Expression (BinOp ConvertTo (BinOp Mul (scalar 36.0) (BinOp Div (Unit (kilo meter)) (Unit hour))) (Unit (mile ./ hour))))
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
      shouldParseAs (Expression (Variable "tau")) "tau"
      shouldParseAs (Expression (Variable "x_2")) "x_2"
      shouldParseAs (Expression (Variable "länge")) "länge"
      shouldParseAs (Expression (Variable "_prefixed")) "_prefixed"
      shouldParseAs (Expression (Variable "x'")) "x'"
      shouldParseAs (Expression (Variable "t''")) "t''"
      shouldParseAs (Expression (Variable "to_")) "to_"

      shouldFail "xs,as"
      shouldFail "hello`"

    test "Variables which begin like units" do
      shouldParseAs (Expression (Variable "myVariable")) "myVariable"
      shouldParseAs (Expression (Variable "density")) "density"
      shouldParseAs (Expression (Variable "µg2")) "µg2"
      shouldParseAs (Expression (Variable "in2")) "in2"

    test "Variables which begin with 'e'" do
      allParseAs (Expression (BinOp Mul (scalar 2.0) (Variable "ex")))
        [ "2ex"
        , "2.0ex"
        , "2 ex"
        ]

      allParseAs (Expression (BinOp Mul (scalar 2.0) (Variable "e")))
        [ "2e"
        , "2.0e"
        , "2 e"
        ]

    test "Variables before parenthesis" do
      allParseAs (Expression (BinOp Mul (Variable "pi") (scalar 2.0)))
        [ "pi(2)"
        , "pi*(2)"
        , "(pi)(2)"
        ]

    test "Initial environment" do
      for_ (keys initialEnvironment.values) \ident →
        shouldParseAs (Expression (Variable ident)) ident

  suite "Parser - Functions" do
    test "Simple" do
      allParseAs (Expression (Apply "sin" (q 30.0 degree :| Nil)))
        [ "sin(30°)"
        , "  sin( 30° )  "
        , "  sin( +30° )  "
        ]

      allParseAs (Expression (Apply "sqrt" (scalar 2.0 :| Nil)))
        [ "sqrt(2)"
        , "  sqrt( 2.0 )  "
        , "  sqrt( +2.0 )  "
        ]

      allParseAs (Expression (Apply "exp" (Negate (scalar 10.0) :| Nil)))
        [ "exp(-10)"
        , "  exp( -10 )  "
        ]

      allParseAs (Expression (Apply "exp" (scalar 1.0 :| scalar 2.0 : scalar 3.0 : Nil)))
        [ "exp(1,2,3)"
        , "  exp(  1  ,  2  ,  3   )  "
        ]

      allParseAs (Expression (BinOp Mul (Apply "exp" (scalar 1.0 :| Nil))
                                        (Apply "exp" (scalar 1.0 :| Nil))))
        [ "exp(1)exp(1)"
        , "  exp( 1 )  exp(  1  )  "
        ]

      shouldFail "exp(,)"
      shouldFail "exp(1,)"
      shouldFail "exp()"

  -- The first four are reserved units, 'list' is a reserved keyword, and '_'
  -- and 'ans' are reserved variables that refer to the value of the last
  -- evaluated line.
  let reservedNames = ["m", "meter", "kg", "kilogram", "list", "ans", "_"]

  suite "Parser - Variable Assignments" do
    test "Simple" do
      allParseAs (VariableAssignment "xyz_123" (scalar 1.0))
        [ "xyz_123 = 1"
        , "xyz_123=1"
        , "  xyz_123  =  1  "
        ]

      shouldFail "x²=3"
      shouldFail "x+y=3"
      shouldFail "x+2=3"
      shouldFail "3=5"
      shouldFail "x="
      shouldFail "x=3+"

    test "Reserved names" do
      for_ reservedNames \name -> shouldFail (name <> "=2")

  suite "Parser - Function Assignments" do
    test "Simple" do
      allParseAs (FunctionAssignment "xyz_123" ("x" :| Nil) (scalar 1.0))
        [ "xyz_123(x) = 1"
        , "xyz_123(x)=1"
        , "  xyz_123(x)  =  1  "
        ]

      shouldFail "f()=2"
      shouldFail "f(x,)=3"
      shouldFail "f(2)=3"
      shouldFail "f(x)="

    test "Multiple arguments" do
      allParseAs (FunctionAssignment "f'" ("x" :| "y" : "z" : Nil) (Variable "x"))
        [ "f'(x,y,z)=x"
        , " f'( x , y , z ) = x "
        ]

    test "Reserved names" do
      for_ reservedNames \name -> shouldFail (name <> "(x)=2")

  suite "Parser - Pretty print function" do
    test "Simple" do
      allParseAs (PrettyPrintFunction "cos")
        [ "cos"
        , "  cos"
        , "cos  "
        ]

  let pretty' str =
        case parseInsect initialEnvironment str of
          Right (Expression expr) → format fmtPlain (pretty expr)
          _ → "Error"

  let equalPretty out inp =
        equal out (pretty' inp)

  suite "Pretty printer" do
    test "Consistency" do
      prettyPrintCheck "-2.3e-12387"
      prettyPrintCheck "2.3e-12387"
      prettyPrintCheck "18379173"
      prettyPrintCheck "2+3"
      prettyPrintCheck "2+3*5"
      prettyPrintCheck "-3^4+2/(4+2*3)"
      prettyPrintCheck "1-2-3-4-(5-6-7)"
      prettyPrintCheck "1/2/3/4/(5/6/7)"
      prettyPrintCheck "kg"
      prettyPrintCheck "2m/s"
      prettyPrintCheck "a+b*c^d-e*f"
      prettyPrintCheck "sin(x)^3"
      prettyPrintCheck "sin(cos(atanh(x)+2))^3"
      prettyPrintCheck "2^3^4^5"
      prettyPrintCheck "(2^3)^(4^5)"
      prettyPrintCheck "sqrt(1.4^2 + 1.5^2) * cos(pi/3)^2"
      prettyPrintCheck "40kg * 9.8m/s^2 * 150cm"
      prettyPrintCheck "4/3 * pi * r³"
      prettyPrintCheck "vol * density -> kg"
      prettyPrintCheck "atan(30cm / 2m)"
      prettyPrintCheck "500km/day -> km/h"
      prettyPrintCheck "1mrad -> °"
      prettyPrintCheck "52weeks -> days"
      prettyPrintCheck "5in + 2ft -> cm"
      prettyPrintCheck "6Mbit/s * 1.5h -> GB"
      prettyPrintCheck "2d"
      prettyPrintCheck "5t"
      prettyPrintCheck "länge * x_2 * µ * _prefixed"
      prettyPrintCheck "2m^3"
      prettyPrintCheck "(2m)^3"
      prettyPrintCheck "(2m)^(3kg)"
      prettyPrintCheck "(2m)^(3kg)^((4in)^(5ft))^(6s)"
      prettyPrintCheck "-sqrt(-30m^3)"
      prettyPrintCheck "-3^4"
      prettyPrintCheck "(-3)^4"
      prettyPrintCheck "5!³"
      prettyPrintCheck "2^3!"
      prettyPrintCheck "-3!"
      prettyPrintCheck "(-3)!"
      prettyPrintCheck "sin(2,3,4)"

    test "Format" do
      equalPretty "2 + 3" "2+3"
      equalPretty "2 × 3" "2*3"
      equalPretty "2^3" "2^3"
      equalPretty "2 km" "2km"
      equalPretty "sin(30°)" "sin(30°)"
      equalPretty "2 × 3 × 4" "2*3*4"
      equalPretty "2 × 3 × 4" "2*(3*4)"
      equalPretty "2 + 3 + 4" "2+3+4"
      equalPretty "2 + 3 + 4" "2+(3+4)"
      equalPretty "atan(30 cm / 2 m)" "atan(30cm / 2m)"
      equalPretty "1 mrad ➞ °" "1mrad -> °"
      equalPretty "2 km + 2 cm ➞ in" "2km+2cm -> in"
      equalPretty "2^3 + 4^5" "2^3 + 4^5"
      equalPretty "2^3 - 4^5" "2^3 - 4^5"
      equalPretty "2^3 × 4^5" "2^3 * 4^5"
      equalPretty "2 × 3 + 4 × 5" "2 * 3 + 4 * 5"
      equalPretty "2 × (3 / 4)" "2 * 3 / 4"
      equalPretty "123.123 × km^2 / s^2" "123.123 km^2 / s^2"
      equalPretty "sin(2, 3, 4)" " sin(  2  ,  3  ,  4   )  "

  let expectOutput' = expectOutput initialEnvironment

  suite "Integration tests" do
    test "Simple input" do
      expectOutput' "3 m" "3m"
      expectOutput' "3 m" " 3.0 meter  "

    test "Square, cube and other exponentiation operators" do
      expectOutput' "18" "3²*2"
      expectOutput' "18" "3² 2"
      expectOutput' "18" "3²·2"
      expectOutput' "54" "3³*2"
      expectOutput' "54" "3³(2)"
      expectOutput' "9" "(1+2)²"
      expectOutput' "12.5664" "2²pi"
      expectOutput' "12.5664" "2² pi"
      expectOutput' "12.5664" "2²·pi"
      expectOutput' "500 cm·m" "5m² to cm·m"
      expectOutput' "32" "2⁵"
      expectOutput' "-4" "-4¹"
      expectOutput' "0.5" "2⁻¹"
      expectOutput' "0.25" "2⁻²"
      expectOutput' "0.00001" "10⁻⁵"

      shouldFail "32⁻"

    test "Conversions" do
      expectOutput' "5.08 cm" "2in to cm"
      expectOutput' "500 m·cm" "5m^2 -> m*cm"
      expectOutput' "500 cm·m" "5m^2 -> cm*m"
      expectOutput' "0.1 MB/s" "1 kB / 10 ms -> MB/s"

    test "Implicit multiplication" do
      let myEnv =
            { values: insert "x" (StoredValue UserDefined (5.0 .* meter)) initialEnvironment.values
            , functions: initialEnvironment.functions
            }
      expectOutput myEnv "5 m" "x"
      expectOutput myEnv "10 m" "2x"
      expectOutput myEnv "10 m" "2 x"
      expectOutput myEnv "25 m²" "x x"
      expectOutput myEnv "25 m²" "x²"
      expectOutput myEnv "Unknown identifier: x2" "x2"

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

    test "Temperature conversions" do
      expectOutput' "284.65 K" "fromCelsius(11.5)"
      expectOutput' "304.983 K" "fromFahrenheit(89.3)"
      expectOutput' "-273.15" "toCelsius(0 K)"
      expectOutput' "-405.67" "toFahrenheit(30 K)"
      expectOutput' "100" "toCelsius(fromCelsius(100))"
      expectOutput' "100" "toFahrenheit(fromFahrenheit(100))"
      expectOutput' "123 K" "fromCelsius(toCelsius(123 K))"
      expectOutput' "123 K" "fromFahrenheit(toFahrenheit(123 K))"

    test "Other functions" do
      expectOutput' "2" "sqrt(4)"
      expectOutput' "5" "log10(100000)"
      expectOutput' "15" "log(e^15)"
      expectOutput' "15" "ln(e^15)"
      expectOutput' "4" "ceil(3.1)"
      expectOutput' "3" "floor(3.9)"
      expectOutput' "4" "round(3.9)"
      expectOutput' "3" "round(3.1)"

    test "Unit simplification" do
      expectOutput' "36000 Mbit" "5Mbit/s * 2h"
      expectOutput' "2500 cm²" "5cm · 5m"
      expectOutput' "0.2 km" "120km/h*6s"

    test "'ans' and '_'" do
      let env1 = initialEnvironment
          env2 = (repl fmtPlain env1 "5").newEnv
          env3 = (repl fmtPlain env2 "_ * 20").newEnv

      expectOutput env3 "1" "ans / 100"

    test "Examples" do
      expectOutput' "1080" "1920/16*9"
      expectOutput' "4294967296" "2^32"
      expectOutput' "0.512957" "sqrt(1.4^2 + 1.5^2) * cos(pi/3)^2"

      expectOutput' "2.5 min" "2min + 30s"
      expectOutput' "150 s" "2min + 30s -> sec"
      expectOutput' "904779000000 km³" "4/3 * pi * (6000km)³"
      expectOutput' "588 m²·kg/s²" "40kg * 9.8m/s^2 * 150cm"
      expectOutput' "0.5" "sin(30°)"

      expectOutput' "26.8224 m/s" "60mph -> m/s"
      expectOutput' "10 km/h" "240km/day -> km/h"
      expectOutput' "0.0572958°" "1mrad -> °"
      expectOutput' "364 d" "52weeks -> days"
      expectOutput' "73.66 cm" "5in + 2ft -> cm"
      expectOutput' "8.53077°" "atan(30cm / 2m) -> °"
      expectOutput' "4.05 GB" "6Mbit/s * 1.5h -> GB"

      expectOutput' "0.75" "3m/4m"
      expectOutput' "4" "4/2*2"
      expectOutput' "0.5 s" "1/2 Hz -> s"

    test "Error messages" do
      expectOutput' "Parse error at position 4: Expected end of input"
                    "3kg+"
      expectOutput' "Parse error at position 5: 'kg' is reserved for a physical unit"
                    "kg=2"
      expectOutput' "Assignment error: 'pi' cannot be redefined."
                    "pi=3"
      expectOutput' "Assignment error: 'sin' cannot be redefined."
                    "sin=3"
      expectOutput' "Assignment error: 'pi' cannot be redefined."
                    "pi(x)=3"
      expectOutput' "Assignment error: 'sin' cannot be redefined."
                    "sin(x)=3"
      expectOutput' "Parse error at position 5: reserved word \"help\""
                    "help=3"
      expectOutput' "Wrong number of arguments:\n\n    The function 'sin' takes 1 argument (got 3)"
                    "sin(1,2,3)"
      expectOutput' "Unknown identifier: foo"
                    "foo"
      expectOutput' "Numerical error: division by zero or out-of-bounds error"
                    "1/0"

    test "Earth mass" do
      let env1 = initialEnvironment
          env2 = (repl fmtPlain env1 "r = 6000km").newEnv
          env3 = (repl fmtPlain env2 "vol = 4/3 * pi * r^3").newEnv
          env4 = (repl fmtPlain env3 "density = 5g/cm^3").newEnv

      expectOutput env4 "4.52389e+24 kg" "vol * density -> kg"

    test "Pendulum" do
      let env1 = initialEnvironment
          env2 = (repl fmtPlain env1 "len = 20cm").newEnv

      expectOutput env2 "897.294 ms" "2pi*sqrt(len/g0) -> ms"

    test "Unicode" do
      expectOutput' "6.62607e-34 J·s" "2π×ℏ"

      let env1 = initialEnvironment
          env2 = (repl fmtPlain env1 "λ = 2 × 300µm").newEnv
      expectOutput env2 "499.654 GHz" "ν = c/λ → GHz"
