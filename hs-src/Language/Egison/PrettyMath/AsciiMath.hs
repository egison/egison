{- |
Module      : Language.Egison.PrettyMath.AsciiMath
Licence     : MIT
-}

module Language.Egison.PrettyMath.AsciiMath
  ( showMathExprAsciiMath
  ) where

import           Data.List                     (intercalate)

import           Language.Egison.PrettyMath.AST

showMathExprAsciiMath :: MathExpr -> String
showMathExprAsciiMath (Atom func []) = func
showMathExprAsciiMath (NegativeAtom func) = "-" ++ func
showMathExprAsciiMath (Plus []) = ""
showMathExprAsciiMath (Plus (x:xs)) = showMathExprAsciiMath x ++ showMathExprAsciiMathForPlus xs
 where
  showMathExprAsciiMathForPlus :: [MathExpr] -> String
  showMathExprAsciiMathForPlus [] = ""
  showMathExprAsciiMathForPlus (NegativeAtom a:xs) = " - " ++ a ++ showMathExprAsciiMathForPlus xs
  showMathExprAsciiMathForPlus (Multiply (NegativeAtom "1":ys):xs) = " - " ++ showMathExprAsciiMath (Multiply ys) ++ showMathExprAsciiMathForPlus xs
  showMathExprAsciiMathForPlus (Multiply (NegativeAtom a:ys):xs) = " - " ++ showMathExprAsciiMath (Multiply (Atom a []:ys)) ++ " " ++ showMathExprAsciiMathForPlus xs
  showMathExprAsciiMathForPlus (x:xs) = " + " ++ showMathExprAsciiMath x ++ showMathExprAsciiMathForPlus xs
showMathExprAsciiMath (Multiply []) = ""
showMathExprAsciiMath (Multiply [a]) = showMathExprAsciiMath a
showMathExprAsciiMath (Multiply (NegativeAtom "1":lvs)) = "-" ++ showMathExprAsciiMath (Multiply lvs)
showMathExprAsciiMath (Multiply lvs) = showMathExprAsciiMath' (head lvs) ++ " " ++ showMathExprAsciiMath (Multiply (tail lvs))
showMathExprAsciiMath (Power lv1 lv2) = showMathExprAsciiMath lv1 ++ "^" ++ showMathExprAsciiMath lv2
showMathExprAsciiMath (Func (Atom "sqrt" []) [x]) = "sqrt " ++ showMathExprAsciiMath x
showMathExprAsciiMath (Func (Atom "rt" []) [x, y]) = "root " ++ showMathExprAsciiMath x ++ " " ++ showMathExprAsciiMath y
showMathExprAsciiMath (Func (Atom "/" []) [x, y]) = "frac{" ++ showMathExprAsciiMath x ++ "}{" ++ showMathExprAsciiMath y ++ "}"
showMathExprAsciiMath (Func f lvs) = showMathExprAsciiMath f ++ "(" ++ showMathExprAsciiMathArg lvs ++ ")"
showMathExprAsciiMath (Tensor lvs mis)
  | null mis = "(" ++ showMathExprAsciiMathArg lvs ++ ")"
  | not (any isSub mis) = "(" ++ showMathExprAsciiMathArg lvs ++ ")^(" ++ showMathExprAsciiMathIndices mis ++ ")"
  | not (any (not . isSub) mis) = "(" ++ showMathExprAsciiMathArg lvs ++ ")_(" ++ showMathExprAsciiMathIndices mis ++ ")"
  | otherwise = "(" ++ showMathExprAsciiMathArg lvs ++ ")_(" ++ showMathExprAsciiMathIndices (filter isSub mis) ++ ")^(" ++ showMathExprAsciiMathIndices (filter (not . isSub) mis) ++ ")"
showMathExprAsciiMath (Tuple lvs) = "(" ++ showMathExprAsciiMathArg lvs ++ ")"
showMathExprAsciiMath (Collection lvs) = "{" ++ showMathExprAsciiMathArg lvs ++ "}"
showMathExprAsciiMath (Exp x) = "e^(" ++ showMathExprAsciiMath x ++ ")"

showMathExprAsciiMath' :: MathExpr -> String
showMathExprAsciiMath' (Plus lvs) = "(" ++ showMathExprAsciiMath (Plus lvs) ++ ")"
showMathExprAsciiMath' val = showMathExprAsciiMath val

showMathExprAsciiMathArg :: [MathExpr] -> String
showMathExprAsciiMathArg exprs = intercalate ", " $ map showMathExprAsciiMath exprs

showMathExprAsciiMathIndices :: [MathIndex] -> String
showMathExprAsciiMathIndices []  = error "unreachable"
showMathExprAsciiMathIndices lvs = concatMap showMathIndexAsciiMath lvs

showMathIndexAsciiMath :: MathIndex -> String
showMathIndexAsciiMath (Super a) = showMathExprAsciiMath a
showMathIndexAsciiMath (Sub a)   = showMathExprAsciiMath a
