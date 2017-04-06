-- | This module defines a pretty printer for Insect expressions.
module Insect.PrettyPrint
  ( pretty
  , prettyQuantity
  ) where

import Prelude

import Data.Array ((:))
import Data.Decimal as D
import Data.Tuple (Tuple(..))

import Quantities as Q

import Insect.Language (BinOp(..), Expression(..), Func(..))
import Insect.Format (Markup)
import Insect.Format as F

-- | Pretty print a single operator.
prettyOp ∷ BinOp → Markup
prettyOp op = [ F.text (opToStr op) ]
  where
    opToStr Add       = " + "
    opToStr Sub       = " - "
    opToStr Mul       = " × "
    opToStr Div       = " / "
    opToStr Pow       = "^"
    opToStr ConvertTo = " -> "

-- | Pretty print a scalar value.
prettyScalar ∷ D.Decimal → Markup
prettyScalar n = [ F.val (D.toString n) ]

-- | Pretty print a physical unit.
prettyUnit ∷ Q.DerivedUnit → Markup
prettyUnit u = [ F.unit (Q.toString u) ]

-- | Pretty print a physical quantity.
prettyQuantity ∷ Q.Quantity → Markup
prettyQuantity q =
  case Q.prettyPrint' q of
    Tuple v u → [ F.val v, F.unit u ]

-- | Construct and pretty-print a physical quantity
prettyQuantity' ∷ D.Decimal → Q.DerivedUnit → Markup
prettyQuantity' s u = prettyQuantity (Q.quantity' s u)

-- | Pretty print a variable name.
prettyVariable ∷ String → Markup
prettyVariable name = [ F.ident name ]

-- | Petty print a function application.
prettyApply ∷ Func → Expression → Markup
prettyApply fn x = [ F.function (funcToStr fn)
                   , F.text "(" ] <> pretty x <> [ F.text ")" ]
  where
    funcToStr ∷ Func → String
    funcToStr Acosh = "acosh"
    funcToStr Acos  = "acos"
    funcToStr Asinh = "asinh"
    funcToStr Asin  = "asin"
    funcToStr Atanh = "atanh"
    funcToStr Atan  = "atan"
    funcToStr Ceil  = "ceil"
    funcToStr Cosh  = "cosh"
    funcToStr Cos   = "cos"
    funcToStr Exp   = "exp"
    funcToStr Floor = "floor"
    funcToStr Log10 = "log10"
    funcToStr Ln    = "ln"
    funcToStr Round = "round"
    funcToStr Sinh  = "sinh"
    funcToStr Sin   = "sin"
    funcToStr Sqrt  = "sqrt"
    funcToStr Tanh  = "tanh"
    funcToStr Tan   = "tan"

-- | Add parenthesis.
parens ∷ Markup → Markup
parens m = F.text "(" : m <> [ F.text ")" ]

-- | Add parenthesis, if needed - conservative version for exponentiation.
withParens' ∷ Expression → Markup
withParens' u@(Unit _)     = pretty u
withParens' s@(Scalar _)   = pretty s
withParens' v@(Variable _) = pretty v
withParens' a@(Apply _ _)  = pretty a
withParens' x              = parens (pretty x)

-- | Add parenthesis, if needed - liberal version, can not be used for
-- | Exponentiation.
withParens ∷ Expression → Markup
withParens e@(BinOp Mul (Scalar s) (Unit u)) = pretty e
withParens e = withParens' e

-- | Pretty print an Insect expression.
pretty ∷ Expression → Markup
pretty (Scalar n)                      = prettyScalar n
pretty (Unit u)                        = prettyUnit u
pretty (Variable name)                 = prettyVariable name
pretty (Negate x)                      = F.text "-" : withParens x
pretty (Apply fn x)                    = prettyApply fn x
-- ConvertTo (->) never needs parens, it has the lowest precedence:
pretty (BinOp ConvertTo x y)           = pretty x <> prettyOp ConvertTo <> pretty y
-- Fuse multiplication of a scalar and a unit to a quantity:
pretty (BinOp Mul (Scalar s) (Unit u)) = prettyQuantity' s u
-- Leave out parens for multiplication, if possible:
pretty (BinOp Mul x y) = addP x <> prettyOp Mul <> addP y
  where
    addP ex = case ex of
                (BinOp Pow _ _) → pretty ex
                (BinOp Mul _ _) → pretty ex
                _               → withParens ex
-- Leave out parens for division, if possible:
pretty (BinOp Div x y) = addPLeft x <> prettyOp Div <> addPRight y
  where
    addPLeft ex = case ex of
                    (BinOp Pow _ _) → pretty ex
                    (BinOp Mul _ _) → pretty ex
                    _               → withParens ex
    addPRight ex = case ex of
                     (BinOp Pow _ _) → pretty ex
                     _               → withParens ex
-- Leave out parens for addition, if possible:
pretty (BinOp Add x y) = addP x <> prettyOp Add <> addP y
  where
    addP ex = case ex of
                (BinOp Pow _ _) → pretty ex
                (BinOp Mul _ _) → pretty ex
                (BinOp Add _ _) → pretty ex
                _               → withParens ex
-- Leave out parens for subtraction, if possible:
pretty (BinOp Sub x y) = addP x <> prettyOp Sub <> addP y
  where
    addP ex = case ex of
                (BinOp Pow _ _) → pretty ex
                (BinOp Mul _ _) → pretty ex
                _               → withParens ex
pretty (BinOp op x y) = withParens' x <> prettyOp op <> withParens' y
