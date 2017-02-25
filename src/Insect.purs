module Insect (
  repl
  ) where

import Prelude

import Control.Monad.Eff (Eff)

import Control.Alt
import Control.Apply

import Data.Quantity
import Data.Units
import Data.Units.SI
import Data.Units.SI.Derived
import Data.Units.Imperial
import Data.Units.Time

import Text.Parsing.StringParser
import Text.Parsing.StringParser.Combinators
import Text.Parsing.StringParser.String
import Text.Parsing.StringParser.Expr
import Data.Either
import Data.Int (fromString, toNumber)
import Data.Foldable
import Data.String
import Data.Maybe
import Data.Array (fromFoldable)
import Partial.Unsafe
import Global (readFloat)

pNumber :: Parser Number
pNumber = do
  nums <- many1 anyDigit
  pure $ readFloat (fromCharArray (fromFoldable nums))

pPrefix :: Parser (DerivedUnit → DerivedUnit)
pPrefix =
      (string "µ" *> pure micro)
  <|> (string "m" *> pure milli)
  <|> (string "c" *> pure centi)
  <|> (string "h" *> pure hecto)
  <|> (string "k" *> pure kilo)
  <|> (string "M" *> pure mega)
  <|> (string "G" *> pure giga)
  <|> pure id

pImperialUnit :: Parser DerivedUnit
pImperialUnit =
      (string "miles" *> pure mile)
  <|> (string "mile"  *> pure mile)
  <|> (string "mph"   *> pure (mile ./ hour))
  <|> (string "in"    *> pure inch)
  <|> (string "ft"    *> pure foot)

pUnit :: Parser DerivedUnit
pUnit =
      (string "sec"   *> pure second)
  <|> (string "min"   *> pure minute)
  <|> (string "hours" *> pure hour)
  <|> (string "hour"  *> pure hour)
  <|> (string "h"     *> pure hour)
  <|> (string "days"  *> pure day)
  <|> (string "day"   *> pure day)
  <|> (string "d"     *> pure day)
  <|> (string "weeks" *> pure week)
  <|> (string "week"  *> pure week)
  <|> (string "m/h"   *> pure (meter ./ hour)) -- TODO
  <|> (string "m/s"   *> pure (meter ./ second)) -- TODO
  <|> (string "g"     *> pure gram)
  <|> (string "m"     *> pure meter)
  <|> (string "s"     *> pure second)
  <?> "Expected unit"


pUnitWithPrefix :: Parser DerivedUnit
pUnitWithPrefix = try withPrefix <|> try pUnit <|> pure unity
  where
    withPrefix = do
      p <- pPrefix
      u <- pUnit
      pure $ p u

pQuantity :: Parser Quantity
pQuantity = quantity <$> pNumber <*> pUnitWithPrefix


qS ea eb = do
  a <- ea
  b <- eb
  a ⊖ b

qA ea eb = do
  a <- ea
  b <- eb
  a ⊕ b


pExpr :: Parser (Either UnificationError Quantity)
pExpr = buildExprParser [ [ Infix (string "/" $> lift2 (⊘)) AssocRight ]
                        , [ Infix (string "*" $> lift2 (⊗)) AssocRight ]
                        , [ Infix (string "-" $> qS) AssocRight ]
                        , [ Infix (string "+" $> qA) AssocRight ]
                        ] (pure <$> pQuantity)

pInput :: Parser (Either UnificationError Quantity)
pInput = try (map toStandard <$> (pExpr <* eof))
  <|> conversion
  where
    conversion = do
      expr <- pExpr
      string " to "
      target <- pUnitWithPrefix
      eof
      pure (expr >>= (flip convertTo) target)

repl :: String -> { out :: String, divClass :: String }
repl inp =
  case runParser pInput inp of
    Left parseErr -> { out: show parseErr, divClass: "error" }
    Right res ->
      case res of
        Left unificationError -> { out: errorMessage unificationError, divClass: "error" }
        Right val -> { out: prettyPrint val, divClass: "value" }
