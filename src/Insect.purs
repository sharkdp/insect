module Insect (
  repl
  ) where

import Prelude

import Data.Either (Either(..))

import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser (parseErrorPosition, parseErrorMessage)

import Insect.Parser (parseInsect)
import Insect.Interpreter (runInsect, startEnv, MessageType(..), Message(..))

typeToClass ∷ MessageType → String
typeToClass Info  = "info"
typeToClass Error = "error"
typeToClass Value = "value"

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
      case runInsect startEnv statement of
        (Message msgType msg) → { divClass: typeToClass msgType, out: msg }
