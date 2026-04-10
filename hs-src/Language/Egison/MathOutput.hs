{- |
Module      : Language.Egison.MathOutput
Licence     : MIT

This module provides translation from mathematical Egison expression into
other languages / format of other computer algebra systems.
-}

module Language.Egison.MathOutput
  ( prettyMath
  ) where

import           Language.Egison.Data
import           Language.Egison.PrettyMath.AST
import qualified Language.Egison.PrettyMath.AsciiMath   as AsciiMath
import qualified Language.Egison.PrettyMath.Latex       as Latex
import qualified Language.Egison.PrettyMath.Mathematica as Mathematica
import qualified Language.Egison.PrettyMath.Maxima      as Maxima

prettyMath :: String -> EgisonValue -> String
prettyMath lang val =
  -- 'lang' is either "asciimath", "latex", "mathematica" or "maxima"
  -- Other invalid options are rejected in Interpreter/egison.hs
  case showMathValue lang (toMathValue val) of
    "undefined" -> "undefined"
    output      -> "#" ++ lang ++ "|" ++ output ++ "|#"

showMathValue :: String -> MathValue -> String
showMathValue "asciimath"   = AsciiMath.showMathValue
showMathValue "latex"       = Latex.showMathValue
showMathValue "mathematica" = Mathematica.showMathValue
showMathValue "maxima"      = Maxima.showMathValue
showMathValue "haskell"     = show
showMathValue _             = error "Unreachable"
