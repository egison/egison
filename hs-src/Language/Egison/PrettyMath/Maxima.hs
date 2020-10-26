{- |
Module      : Language.Egison.PrettyMath.Maxima
Licence     : MIT
-}

module Language.Egison.PrettyMath.Maxima
  ( showMathExpr
  ) where

import           Language.Egison.PrettyMath.AST

showMathExpr :: MathExpr -> String
showMathExpr (Atom a []) = a
showMathExpr (Partial _ _) = "undefined"
showMathExpr (NegativeAtom a) = "-" ++ a
showMathExpr (Plus []) = ""
showMathExpr (Plus (x:xs)) = showMathExpr x ++ showMathExprForPlus xs
 where
  showMathExprForPlus :: [MathExpr] -> String
  showMathExprForPlus []                                  = ""
  showMathExprForPlus (NegativeAtom a:xs)                 = " - " ++ a ++ showMathExprForPlus xs
  showMathExprForPlus (Multiply (NegativeAtom "1":ys):xs) = " - " ++ showMathExpr (Multiply ys) ++ showMathExprForPlus xs
  showMathExprForPlus (Multiply (NegativeAtom a:ys):xs)   = " - " ++ showMathExpr (Multiply (Atom a []:ys)) ++ showMathExprForPlus xs
  showMathExprForPlus (x:xs)                              = " + " ++  showMathExpr x ++ showMathExprForPlus xs
showMathExpr (Multiply []) = ""
showMathExpr (Multiply [x]) = showMathExpr x
showMathExpr (Multiply (Atom "1" []:xs)) = showMathExpr (Multiply xs)
showMathExpr (Multiply (NegativeAtom "1":xs)) = "-" ++ showMathExpr (Multiply xs)
showMathExpr (Multiply (x:xs)) = showMathExpr' x ++ " * " ++ showMathExpr (Multiply xs)
showMathExpr (Div x y) = addBracket x ++ "/" ++ addBracket y
 where
   addBracket x@(Atom _ []) = showMathExpr x
   addBracket x             = "(" ++ showMathExpr x ++ ")"
showMathExpr (Power lv1 lv2) = showMathExpr lv1 ++ "^" ++ showMathExpr lv2
showMathExpr (Func (Atom "sqrt" []) [x]) = "sqrt(" ++ showMathExpr x ++ ")"
showMathExpr (Func (Atom "rt" []) [x, y]) = showMathExpr y ++ "^(1/" ++ showMathExpr x ++ ")"
showMathExpr (Func (Atom "exp" []) [x]) = "exp(" ++ showMathExpr x ++ ")"
showMathExpr (Func f xs) = showMathExpr f ++ "(" ++ showMathExprArg xs ++ ")"
showMathExpr (Tensor _ _) = "undefined"
showMathExpr (Tuple _) = "undefined"
showMathExpr (Collection xs) = "[" ++ showMathExprArg xs ++ "]"
showMathExpr (Quote x) = "(" ++ showMathExpr x ++ ")"

showMathExpr' :: MathExpr -> String
showMathExpr' x@(Plus _) = "(" ++ showMathExpr x ++ ")"
showMathExpr' x          = showMathExpr x

showMathExprArg :: [MathExpr] -> String
showMathExprArg []            = ""
showMathExprArg [Tensor _ []] = "undefined"
showMathExprArg [a]           = showMathExpr a
showMathExprArg lvs           = showMathExpr (head lvs) ++ ", " ++ showMathExprArg (tail lvs)
