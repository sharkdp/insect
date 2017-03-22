module Insect
  ( repl
  , initialEnvironment
  , supportedUnits
  ) where

import Prelude

import Data.Either (Either(..))

import Data.Array (sort)

import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser (parseErrorPosition, parseErrorMessage)

import Insect.Parser (Dictionary(..), (==>),
                      normalUnitDict, imperialUnitDict, parseInsect)
import Insect.Interpreter (MessageType(..), Message(..), runInsect)
import Insect.Environment (Environment)
import Insect.Environment as E

-- | Re-export the initial environment
initialEnvironment ∷ Environment
initialEnvironment = E.initialEnvironment

-- | List of all supported units
supportedUnits ∷ Array String
supportedUnits = sort $ toArray normalUnitDict <> toArray imperialUnitDict
  where
    toArray (Dictionary dict) = dict >>= toStrs
    toStrs (_ ==> strs) = strs

-- | Convert a message type to a string.
msgTypeToString ∷ MessageType → String
msgTypeToString Info     = "info"
msgTypeToString Error    = "error"
msgTypeToString Value    = "value"
msgTypeToString ValueSet = "value-set"
msgTypeToString Cmd      = "command"

-- | Run Insect, REPL-style.
repl ∷ Environment → String → { msg ∷ String
                              , msgType ∷ String
                              , newEnv ∷ Environment }
repl env userInput =
  case parseInsect userInput of
    Left pErr →
      let pos = parseErrorPosition pErr
      in case pos of
           (Position rec) →
             { msg: "Parse error: " <> parseErrorMessage pErr <>
                    " at position " <> show rec.column
             , msgType: "error"
             , newEnv: env }
    Right statement → do
      let ans = runInsect env statement
      case ans.msg of
        (Message msgType msg) → { msgType: msgTypeToString msgType
                                , msg: msg
                                , newEnv: ans.newEnv }
