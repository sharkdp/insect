module Insect
  ( repl
  , startEnv
  ) where

import Prelude

import Data.Either (Either(..))
import Data.StrMap (fromFoldable)
import Data.Tuple (Tuple(..))

import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser (parseErrorPosition, parseErrorMessage)

import Quantities (e, pi, speedOfLight, planckConstant, hbar)

import Insect.Parser (parseInsect)
import Insect.Interpreter (MessageType(..), Message(..), Environment, runInsect)

startEnv ∷ Environment
startEnv = fromFoldable
  [ Tuple "e"    e
  , Tuple "pi"   pi
  , Tuple "c"    speedOfLight
  , Tuple "h"    planckConstant
  , Tuple "hbar" hbar
  ]

typeToClass ∷ MessageType → String
typeToClass Info  = "info"
typeToClass Error = "error"
typeToClass Value = "value"

-- | Run Insect, REPL-style.
repl ∷ Environment → String → { out ∷ String, divClass ∷ String, newEnv ∷ Environment }
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
