{- |
Module      : Language.Egison.MathOutput
Licence     : MIT

This module provides translation from mathematical Egison expression into
other languages / format of other computer algebra systems.
-}

module Language.Egison.MathOutput
  ( changeOutputInLang
  ) where

import           Text.ParserCombinators.Parsec          (parse)

import           Language.Egison.PrettyMath.AST
import           Language.Egison.PrettyMath.AsciiMath
import           Language.Egison.PrettyMath.Latex
import           Language.Egison.PrettyMath.Mathematica
import           Language.Egison.PrettyMath.Maxima

changeOutputInLang :: String -> String -> String
changeOutputInLang lang input =
  -- 'lang' is either "asciimath", "latex", "mathematica" or "maxima"
  -- Other invalid options are rejected in Interpreter/egison.hs
  case parse parseExpr "math-expr" input of
    Left _ -> input
    Right val -> case showMathExpr lang val of
                   "undefined" -> "undefined"
                   output      -> "#" ++ lang ++ "|" ++ output ++ "|#"

showMathExpr :: String -> MathExpr -> String
showMathExpr "asciimath"   = showMathExprAsciiMath
showMathExpr "latex"       = showMathExprLatex
showMathExpr "mathematica" = showMathExprMathematica
showMathExpr "maxima"      = showMathExprMaxima
showMathExpr "haskell"     = show
showMathExpr _             = error "Unreachable"
