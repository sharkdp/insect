module Insect
  ( repl
  , initialEnvironment
  ) where

import Prelude

import Data.Either (Either(..))

import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser (parseErrorPosition, parseErrorMessage)

import Insect.Parser (parseInsect)
import Insect.Interpreter (MessageType(..), Message(..), runInsect)
import Insect.Environment (Environment)
import Insect.Environment as E

-- | Re-export the initial environment
initialEnvironment ∷ Environment
initialEnvironment = E.initialEnvironment

-- | Convert a message type to the name of a CSS class.
typeToClass ∷ MessageType → String
typeToClass Info  = "info"
typeToClass Error = "error"
typeToClass Value = "value"

-- | Run Insect, REPL-style.
repl ∷ Environment → String → { out ∷ String
                              , divClass ∷ String
                              , newEnv ∷ Environment }
repl env userInput =
  case parseInsect userInput of
    Left pErr →
      let pos = parseErrorPosition pErr
      in case pos of
           (Position rec) →
             { out: "Parse error: " <> parseErrorMessage pErr <>
                    " at position " <> show rec.column
             , divClass: "error"
             , newEnv: env }
    Right statement → do
      let ans = runInsect env statement
      case ans.msg of
        (Message msgType msg) → { divClass: typeToClass msgType
                                , out: msg
                                , newEnv: ans.newEnv }
