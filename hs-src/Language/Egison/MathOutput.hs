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
import qualified Language.Egison.PrettyMath.AsciiMath   as AsciiMath
import qualified Language.Egison.PrettyMath.Latex       as Latex
import qualified Language.Egison.PrettyMath.Mathematica as Mathematica
import qualified Language.Egison.PrettyMath.Maxima      as Maxima

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
showMathExpr "asciimath"   = AsciiMath.showMathExpr
showMathExpr "latex"       = Latex.showMathExpr
showMathExpr "mathematica" = Mathematica.showMathExpr
showMathExpr "maxima"      = Maxima.showMathExpr
showMathExpr "haskell"     = show
showMathExpr _             = error "Unreachable"
