-- | A markup format for Insect.
module Insect.Format
  ( FormatType
  , FormattedString
  , Markup
  , Formatter
  , text
  , emph
  , error
  , val
  , ident
  , unit
  , whitespace
  , nl
  , format
  , fmtPlain
  , fmtJqueryTerminal
  , fmtConsole
  ) where

import Data.Array ((:))
import Data.Tuple (Tuple(..), uncurry)
import Data.Foldable (foldMap)

import Prelude ((<>))

data FormatType
  = FTText
  | FTEmphasized
  | FTError
  | FTValue
  | FTIdentifier
  | FTUnit
  | FTWhitespace

-- | A single piece of markup.
type FormattedString = Tuple FormatType String

-- | A formatted text.
type Markup = Array FormattedString

-- | Format as a plain string.
text ∷ String → FormattedString
text = Tuple FTText

-- | Emphasized text.
emph ∷ String → FormattedString
emph = Tuple FTEmphasized

-- | Error output.
error ∷ String → FormattedString
error = Tuple FTError

-- | Format as a value.
val ∷ String → FormattedString
val = Tuple FTValue

-- | Format as an identifier.
ident ∷ String → FormattedString
ident = Tuple FTIdentifier

-- | Format as a physical unit.
unit ∷ String → FormattedString
unit = Tuple FTUnit

-- | Beginning of a message
whitespace ∷ String → FormattedString
whitespace = Tuple FTWhitespace

-- | A newline character
nl ∷ FormattedString
nl = text "\n"

-- | A function that renders the markup to a string representation.
type Formatter = FormatType → String → String

-- | Format an output text with a given formatter.
format ∷ Formatter → Markup → String
format formatter m = foldMap (uncurry formatter) (whitespace "\n" : m)

-- | Formatter for plain text output on a command line.
fmtPlain ∷ Formatter
fmtPlain FTWhitespace _ = ""  -- ignore superfluous whitespace
fmtPlain _ s = s

jtColored ∷ String → String → String
jtColored col str = "[[;#" <> col <> ";]" <> str <> "]"

-- | Formatter for rich text output on jquery.terminal.
fmtJqueryTerminal ∷ Formatter
fmtJqueryTerminal FTText s = s
fmtJqueryTerminal FTEmphasized s = "[[b;;]" <> s <> "]"
fmtJqueryTerminal FTError s = jtColored "F92672" s
fmtJqueryTerminal FTValue s = jtColored "FFFFFF" s
fmtJqueryTerminal FTIdentifier s = jtColored "66D9EF" s
fmtJqueryTerminal FTUnit s = "[[i;#E6DB74;]" <> s <> "]"
fmtJqueryTerminal FTWhitespace s = s

consoleCode ∷ String → String → String
consoleCode code str = "\x1b[" <> code <> "m" <> str <> "\x1b[0m"

-- | Formatter for rich text output on a command line.
fmtConsole ∷ Formatter
fmtConsole FTText s = s
fmtConsole FTEmphasized s = consoleCode ansiBold s
  where ansiBold = "01"
fmtConsole FTError s = consoleCode ansiRed s
  where ansiRed = "31"
fmtConsole FTValue s = s
fmtConsole FTIdentifier s = consoleCode ansiYellow s
  where ansiYellow = "33"
fmtConsole FTUnit s = consoleCode ansiCyan s
  where ansiCyan = "36"
fmtConsole FTWhitespace s = s
