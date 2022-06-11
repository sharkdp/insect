module Insect
  ( repl
  , initialEnvironment
  , supportedUnits
  , fmtPlain
  , fmtJqueryTerminal
  , fmtConsole
  , commands
  , functions
  , identifiers
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Map (lookup, keys)
import Data.Set (toUnfoldable)

import Data.Array (sort)
import Data.Maybe (maybe)

import Parsing (Position(..), parseErrorPosition, parseErrorMessage)

import Insect.Parser (Dictionary(..), (==>),
                      normalUnitDict, imperialUnitDict, parseInsect)
import Insect.Parser as P
import Insect.Interpreter (MessageType(..), Message(..), runInsect)
import Insect.Environment (Environment, StoredValue(..))
import Insect.Environment as E
import Insect.Format (Formatter, format)
import Insect.Format as F
import Insect.PrettyPrint (prettyQuantity)

-- | List of all supported units
supportedUnits ∷ Array String
supportedUnits = sort $ toArray normalUnitDict <> toArray imperialUnitDict
                        <> ["d", "t"] -- see Parser special cases
  where
    toArray (Dictionary dict) = dict >>= toStrs
    toStrs (_ ==> strs) = strs

-- | Convert a message type to a string.
msgTypeToString ∷ MessageType → String
msgTypeToString Info     = "info"
msgTypeToString Error    = "error"
msgTypeToString Value    = "value"
msgTypeToString ValueSet = "value-set"

-- | Run Insect, REPL-style.
repl ∷ Formatter → Environment → String → { msg ∷ String
                                          , newEnv ∷ Environment
                                          , msgType ∷ String     }
repl fmt env userInput =
  case parseInsect env userInput of
    Left pErr →
      let Position rec = parseErrorPosition pErr
      in
        { msg: format fmt
             [ F.optional (F.text "  ")
             , F.error $ "Parse error at position " <>
                         show rec.column <> ": "
             , F.text (parseErrorMessage pErr)
             ]
         , msgType: "error"
         , newEnv: env }
    Right statement →
      let ans = runInsect env statement
      in case ans.msg of
           Message msgType msg →
             { msgType: msgTypeToString msgType
             , msg: format fmt msg
             , newEnv: ans.newEnv }
           MQuit →
              { msgType: "quit"
              , msg: ""
              , newEnv: ans.newEnv }
           MCopy →
             { msgType: "copy"
             , msg: value
             , newEnv: ans.newEnv }
               where
                 storedQty (StoredValue _ q) = q
                 maybeStoredValue = lookup "ans" ans.newEnv.values
                 value = maybe "" (\sv → format fmtPlain (prettyQuantity $ storedQty sv)) maybeStoredValue
           MClear →
             { msgType: "clear"
             , msg: ""
             , newEnv: ans.newEnv }

-- | Re-export the initial environment
initialEnvironment ∷ Environment
initialEnvironment = E.initialEnvironment

-- | Re-export the plain formatter
fmtPlain ∷ Formatter
fmtPlain = F.fmtPlain

-- | Re-export the jquery terminal formatter
fmtJqueryTerminal ∷ Formatter
fmtJqueryTerminal = F.fmtJqueryTerminal

-- | Re-export the console formatter
fmtConsole ∷ Formatter
fmtConsole = F.fmtConsole

-- | Re-export the list of commands
commands ∷ Array String
commands = P.commands

functions ∷ Environment → Array String
functions env = toUnfoldable (keys env.functions) <> ["sum", "product"]

identifiers ∷ Environment → Array String
identifiers env = toUnfoldable (keys env.values)
