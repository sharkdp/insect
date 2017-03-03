module Insect (
  repl
  ) where

import Prelude

import Data.Either (Either(..))

import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser (parseErrorPosition, parseErrorMessage)

import Quantities (errorMessage, prettyPrint)

import Insect.Parser (parseInsect)
import Insect.Interpreter (runInsect)

-- | Run Insect, REPL-style.
repl ∷ String → { out ∷ String, divClass ∷ String }
repl userInput =
  case parseInsect userInput of
    Left pErr →
      let pos = parseErrorPosition pErr
      in case pos of
           (Position rec) →
             { out: "Parse error: " <> parseErrorMessage pErr <>
                    " at position " <> show rec.column
             , divClass: "error" }
    Right statement →
      case runInsect statement of
        Left unificationError →
          { out: errorMessage unificationError, divClass: "error" }
        Right val →
          { out: prettyPrint val, divClass: "value" }
