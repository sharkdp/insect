module Test.Main where

import Prelude hiding (degree)

import Effect (Effect)
import Effect.Aff (Aff)

import Data.Decimal (fromNumber, Decimal)
import Data.Either(Either(..))
import Data.Foldable (traverse_, for_, intercalate)
import Data.List (List(..), (:))
import Data.NonEmpty ((:|))
import Data.Map (insert, keys)

import Test.Unit (suite, test, failure)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck (Result(), (===))

import Text.Parsing.Parser (parseErrorMessage, parseErrorPosition)
import Text.Parsing.Parser.Pos (Position(..))

import Quantities ((./), (.*), milli, nano, meter, inch, hour, minute, kilo,
                   mile, gram, second, deci, tera, hertz, degree, radian,
                   day, tonne, euro)

import Insect.Language (BinOp(..), Expression(..), Statement(..))
import Insect.Parser (Dictionary(..), DictEntry, (==>), prefixDict,
                      normalUnitDict, imperialUnitDict, parseInsect)
import Insect.Environment (StorageType(..), StoredValue(..), Environment,
                           initialEnvironment)
import Insect.Format (format, fmtPlain, fmtConsole, ident, unit, OutputType, FormatType)

import Insect.PrettyPrint (pretty)
import Insect (repl)

parser ∷ Statement → String → Aff Unit
parser expected input =
  case parseInsect initialEnvironment input of
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

prettyPrintCheck ∷ String → Aff Unit
prettyPrintCheck input =
  case parseInsect initialEnvironment input of
    Left err →
      case parseErrorPosition err of
        (Position pos) →
          failure $ "Parse error for input '" <> input <> "': "
                                <> parseErrorMessage err
                                <> " at position "
                                <> show pos.column
    Right output →
      case output of
        Expression expr →
          -- Pretty print the AST, parse it again and check against the
          -- original AST.
          parser output (format fmtPlain (pretty expr))
        _ →
          failure $ "Input is not an expression"


main = runTest do
  -- Helper to construct scalars
  let scalar n = Scalar (fromNumber n)

  -- Helper to construct quantities
  let q s u = BinOp Mul (scalar s) (Unit u)

  suite "Binary Operator with Scalar Testing" do
    test "Multiplication" do
      let x1 = (Expression (BinOp Mul (scalar 50.0) (scalar 653.0)))
      parser x1 "50 * 653"
    test "Division" do
      let x2 = (Expression (BinOp Div (scalar 99.0) (scalar 11.0)))
      parser x2 "99 / 11"
    test "Addition" do
      let x3 = (Expression (BinOp Add ((BinOp Add (scalar 420.0) (scalar 420.0))) (scalar 66.6)))
      parser x3 "420 + 420 + 66.6"
    test "Subtraction" do
      let x3 = (Expression (BinOp Sub ((BinOp Sub (scalar 420.0) (scalar 420.0))) (scalar 66.6)))
      parser x3 "420 - 420 - 66.6"
    test "Power" do
      let x4 = (Expression (BinOp Pow (BinOp Add (scalar 22.0) (scalar 22.0)) (scalar 410.0)))
      parser x4 "(22 + 22) ^ 410"
    test "Modulus" do
      let x5 =  (Expression (BinOp Pow (BinOp Mod (scalar 20.0) (scalar 10.0)) (scalar 2.0)))
      parser x5 "(20 % 10) ^ 2"
  suite "Variable Testing with Scalars" do
    test "Simple Assignment Tests" do
      let v1 = (Expression (BinOp Mul (BinOp Mul (Variable "ex") (scalar 2.0)) (scalar 2.0 )))
      let v2 = (Expression (Variable "sin"))
      parser v2 "sin"
      parser v1 "ex * 2 * 2"
    test "Expression Assignment Tests" do
      let v1 = (VariableAssignment "legendaryMember" (scalar 1.0))
      let v2 = (VariableAssignment "xyz_____testing" ((BinOp Add (scalar 2.0) (scalar 42.000434))))
      let v3 = (VariableAssignment "test1203030_________________________" ((BinOp Div (scalar 10.0) (scalar 51.0))))
      parser v1 "legendaryMember=1.0"
      parser v2 "xyz_____testing=2.0+42.000434"
      parser v3 "test1203030_________________________=10/51" 


