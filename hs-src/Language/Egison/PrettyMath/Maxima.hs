{- |
Module      : Language.Egison.PrettyMath.Maxima
Licence     : MIT
-}

module Language.Egison.PrettyMath.Maxima
  ( showMathValue
  ) where

import           Language.Egison.PrettyMath.AST

showMathValue :: MathValue -> String
showMathValue (Atom a []) = a
showMathValue (Partial _ _) = "undefined"
showMathValue (NegativeAtom a) = "-" ++ a
showMathValue (Plus []) = ""
showMathValue (Plus (x:xs)) = showMathValue x ++ showMathValueForPlus xs
 where
  showMathValueForPlus :: [MathValue] -> String
  showMathValueForPlus []                                  = ""
  showMathValueForPlus (NegativeAtom a:xs)                 = " - " ++ a ++ showMathValueForPlus xs
  showMathValueForPlus (Multiply (NegativeAtom "1":ys):xs) = " - " ++ showMathValue (Multiply ys) ++ showMathValueForPlus xs
  showMathValueForPlus (Multiply (NegativeAtom a:ys):xs)   = " - " ++ showMathValue (Multiply (Atom a []:ys)) ++ showMathValueForPlus xs
  showMathValueForPlus (x:xs)                              = " + " ++  showMathValue x ++ showMathValueForPlus xs
showMathValue (Multiply []) = ""
showMathValue (Multiply [x]) = showMathValue x
showMathValue (Multiply (Atom "1" []:xs)) = showMathValue (Multiply xs)
showMathValue (Multiply (NegativeAtom "1":xs)) = "-" ++ showMathValue (Multiply xs)
showMathValue (Multiply (x:xs)) = showMathValue' x ++ " * " ++ showMathValue (Multiply xs)
showMathValue (Div x y) = addBracket x ++ "/" ++ addBracket y
 where
   addBracket x@(Atom _ []) = showMathValue x
   addBracket x             = "(" ++ showMathValue x ++ ")"
showMathValue (Power lv1 lv2) = showMathValue lv1 ++ "^" ++ showMathValue lv2
showMathValue (Func (Atom "sqrt" []) [x]) = "sqrt(" ++ showMathValue x ++ ")"
showMathValue (Func (Atom "rt" []) [x, y]) = showMathValue y ++ "^(1/" ++ showMathValue x ++ ")"
showMathValue (Func (Atom "exp" []) [x]) = "exp(" ++ showMathValue x ++ ")"
showMathValue (Func f xs) = showMathValue f ++ "(" ++ showMathValueArg xs ++ ")"
showMathValue (Tensor _ _) = "undefined"
showMathValue (Tuple _) = "undefined"
showMathValue (Collection xs) = "[" ++ showMathValueArg xs ++ "]"
showMathValue (Quote x) = "(" ++ showMathValue x ++ ")"

showMathValue' :: MathValue -> String
showMathValue' x@(Plus _) = "(" ++ showMathValue x ++ ")"
showMathValue' x          = showMathValue x

showMathValueArg :: [MathValue] -> String
showMathValueArg []            = ""
showMathValueArg [Tensor _ []] = "undefined"
showMathValueArg [a]           = showMathValue a
showMathValueArg (lv:lvs)      = showMathValue lv ++ ", " ++ showMathValueArg lvs
