module Test.Main where

import Prelude hiding (degree)
import Effect (Effect)
import Effect.Aff (Aff)

import Data.Decimal (fromNumber)
import Data.Either(Either(..))
import Data.Foldable (traverse_)

import Test.Unit (suite, test, failure)
import Test.Unit.Main (runTest)

import Text.Parsing.Parser (parseErrorMessage, parseErrorPosition)
import Text.Parsing.Parser.Pos (Position(..))



import Insect.Language (BinOp(..), Expression(..), Statement(..))
import Insect.Parser (parseInsect)
import Insect.Environment (Environment,
                          initialEnvironment)
import Insect.Format (format, fmtPlain)
import Insect.PrettyPrint (pretty)
import Insect (repl)

shouldParseAs ∷ Statement → String → Aff Unit
shouldParseAs expected input =
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

allParseAs ∷ Statement → Array String → Aff Unit
allParseAs expected = traverse_ (shouldParseAs expected)

shouldFail ∷ String → Aff Unit
shouldFail input = do
 case parseInsect initialEnvironment input of
  Left _ → pure unit
  Right output → failure $ "input is expected to throw a parse error: '" <> input <> "'"

expectOutput ∷ Environment → String → String → Aff Unit
expectOutput env expected inp =
 let res = repl fmtPlain env inp
     out = res.msg
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
         shouldParseAs output (format fmtPlain (pretty expr))
       _ →
         failure $ "Input is not an expression"

main ∷ Effect Unit
main = runTest do
 -- Helper to construct scalars
 let scalar n = Scalar (fromNumber n)

 -- Helper to construct quantities
 let q s u = BinOp Mul (scalar s) (Unit u)

 let expectOutput' = expectOutput initialEnvironment

 suite "Function Test" do
    test "Square Root" do
      expectOutput' "3" "sqrt(9)"
      expectOutput' "1.22474" "sqrt(1.5)"
      expectOutput' "5" "sqrt(5^2)"
      expectOutput' "1.58114" "sqrt(5/2)"
      expectOutput' "1.41421" "sqrt(5-3)"
      expectOutput' "1.41421" "sqrt(1+1)"
      expectOutput' "3.16228" "sqrt(5*2)"

    test "Natural Logarithm " do
      expectOutput' "0" "ln(1)"
      expectOutput' "Numerical error: division by zero or out-of-bounds error" "ln(0)"
      expectOutput' "0.916291" "ln(5/2)"
      expectOutput' "2.30259" "ln(5*2)"
      expectOutput' "1.97408" "ln(7.2)"
      expectOutput' "1.38629" "ln(2^2)"
      expectOutput' "0" "ln(2-1)"
      expectOutput' "1.38629" "ln(2+2)"
