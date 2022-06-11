-- | A markup format for Insect.
module Insect.Format
  ( OutputType
  , FormatType
  , FormattedString
  , Markup
  , Formatter
  , text
  , emph
  , error
  , val
  , ident
  , function
  , unit
  , optional
  , nl
  , format
  , fmtPlain
  , fmtJqueryTerminal
  , fmtConsole
  ) where

import Prelude

import Data.Foldable (foldMap)

data FormatType
  = FTText
  | FTEmphasized
  | FTError
  | FTValue
  | FTIdentifier
  | FTFunction
  | FTUnit

data OutputType
  = Normal
  | Optional

-- | A single piece of markup.
data FormattedString = Formatted OutputType FormatType String

-- | A formatted text.
type Markup = Array FormattedString

-- | Format as a plain string.
text ∷ String → FormattedString
text = Formatted Normal FTText

-- | Emphasized text.
emph ∷ String → FormattedString
emph = Formatted Normal FTEmphasized

-- | Error output.
error ∷ String → FormattedString
error = Formatted Normal FTError

-- | Format as a value.
val ∷ String → FormattedString
val = Formatted Normal FTValue

-- | Format as an identifier.
ident ∷ String → FormattedString
ident = Formatted Normal FTIdentifier

-- | Format as a function name.
function ∷ String → FormattedString
function = Formatted Normal FTFunction

-- | Format as a physical unit.
unit ∷ String → FormattedString
unit = Formatted Normal FTUnit

-- | Optional output (whitespace, etc.).
optional ∷ FormattedString → FormattedString
optional (Formatted _ t s) = Formatted Optional t s

-- | A newline character
nl ∷ FormattedString
nl = text "\n"

-- | A function that renders the markup to a string representation.
type Formatter = OutputType → FormatType → String → String

-- | Uncurry a formatter.
uncurry ∷ Formatter → (FormattedString → String)
uncurry fmt (Formatted ot ft s) = fmt ot ft s

-- | Format an output text with a given formatter.
format ∷ Formatter → Markup → String
format formatter m = foldMap (uncurry formatter) ([ optional nl ] <> m)

-- | Formatter for plain text output on a command line.
fmtPlain ∷ Formatter
fmtPlain Normal   _ s = s
fmtPlain Optional _ _ = "" -- ignore optional output

jtClass ∷ String → String → String
jtClass _ "" = ""  -- do not emit formatting code for empty strings
jtClass name str = "[[;;;hl-" <> name <> "]" <> str <> "]"

-- | Formatter for rich text output on jquery.terminal.
fmtJqueryTerminal ∷ Formatter
fmtJqueryTerminal _ FTText = identity
fmtJqueryTerminal _ FTEmphasized = jtClass "emphasized"
fmtJqueryTerminal _ FTError = jtClass "error"
fmtJqueryTerminal _ FTValue = jtClass "value"
fmtJqueryTerminal _ FTIdentifier = jtClass "identifier"
fmtJqueryTerminal _ FTFunction = jtClass "function"
fmtJqueryTerminal _ FTUnit = jtClass "unit"

consoleCode ∷ String → String → String
consoleCode code str = "\x1b[" <> code <> "m" <> str <> "\x1b[0m"

-- | Formatter for rich text output on a command line.
fmtConsole ∷ Formatter
fmtConsole _ FTText s = s
fmtConsole _ FTEmphasized s = consoleCode ansiBold s
  where ansiBold = "01"
fmtConsole _ FTError s = consoleCode ansiRed s
  where ansiRed = "31"
fmtConsole _ FTValue s = consoleCode ansiCyan s
  where ansiCyan = "36"
fmtConsole _ FTIdentifier s = consoleCode ansiYellow s
  where ansiYellow = "33"
fmtConsole _ FTFunction s = consoleCode ansiItalic s
  where ansiItalic = "03"
fmtConsole _ FTUnit s = consoleCode ansiGreen s
  where ansiGreen = "32"
