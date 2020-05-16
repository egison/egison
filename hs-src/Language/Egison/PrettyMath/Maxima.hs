{- |
Module      : Language.Egison.PrettyMath.Maxima
Licence     : MIT
-}

module Language.Egison.PrettyMath.Maxima
  ( showMathExprMaxima
  ) where

import           Language.Egison.PrettyMath.AST

showMathExprMaxima :: MathExpr -> String
showMathExprMaxima (Atom a []) = a
showMathExprMaxima (Partial _ _) = "undefined"
showMathExprMaxima (NegativeAtom a) = "-" ++ a
showMathExprMaxima (Plus []) = ""
showMathExprMaxima (Plus (x:xs)) = showMathExprMaxima x ++ showMathExprMaximaForPlus xs
 where
  showMathExprMaximaForPlus :: [MathExpr] -> String
  showMathExprMaximaForPlus [] = ""
  showMathExprMaximaForPlus (NegativeAtom a:xs) = " - " ++ a ++ showMathExprMaximaForPlus xs
  showMathExprMaximaForPlus (Multiply (NegativeAtom "1":ys):xs) = " - " ++ showMathExprMaxima (Multiply ys) ++ showMathExprMaximaForPlus xs
  showMathExprMaximaForPlus (Multiply (NegativeAtom a:ys):xs) = " - " ++ showMathExprMaxima (Multiply (Atom a []:ys)) ++ showMathExprMaximaForPlus xs
  showMathExprMaximaForPlus (x:xs) = " + " ++  showMathExprMaxima x ++ showMathExprMaximaForPlus xs
showMathExprMaxima (Multiply []) = ""
showMathExprMaxima (Multiply [x]) = showMathExprMaxima x
showMathExprMaxima (Multiply (Atom "1" []:xs)) = showMathExprMaxima (Multiply xs)
showMathExprMaxima (Multiply (NegativeAtom "1":xs)) = "-" ++ showMathExprMaxima (Multiply xs)
showMathExprMaxima (Multiply (x:xs)) = showMathExprMaxima' x ++ " * " ++ showMathExprMaxima (Multiply xs)
showMathExprMaxima (Power lv1 lv2) = showMathExprMaxima lv1 ++ "^" ++ showMathExprMaxima lv2
showMathExprMaxima (Func (Atom "sqrt" []) [x]) = "sqrt(" ++ showMathExprMaxima x ++ ")"
showMathExprMaxima (Func (Atom "rt" []) [x, y]) = showMathExprMaxima y ++ "^(1/" ++ showMathExprMaxima x ++ ")"
showMathExprMaxima (Func (Atom "/" []) [x, y]) = addBracket x ++ "/" ++ addBracket y
 where
   addBracket x@(Atom _ []) = showMathExprMaxima x
   addBracket x             = "(" ++ showMathExprMaxima x ++ ")"
showMathExprMaxima (Func f xs) = showMathExprMaxima f ++ "(" ++ showMathExprMaximaArg xs ++ ")"
showMathExprMaxima (Tensor _ _) = "undefined"
showMathExprMaxima (Tuple _) = "undefined"
showMathExprMaxima (Collection xs) = "[" ++ showMathExprMaximaArg xs ++ "]"
showMathExprMaxima (Exp x) = "exp(" ++ showMathExprMaxima x ++ ")"
showMathExprMaxima (Quote x) = "(" ++ showMathExprMaxima x ++ ")"

showMathExprMaxima' :: MathExpr -> String
showMathExprMaxima' x@(Plus _) = "(" ++ showMathExprMaxima x ++ ")"
showMathExprMaxima' x         = showMathExprMaxima x

showMathExprMaximaArg :: [MathExpr] -> String
showMathExprMaximaArg [] = ""
showMathExprMaximaArg [Tensor _ []] = "undefined"
showMathExprMaximaArg [a] = showMathExprMaxima a
showMathExprMaximaArg lvs = showMathExprMaxima (head lvs) ++ ", " ++ showMathExprMaximaArg (tail lvs)
