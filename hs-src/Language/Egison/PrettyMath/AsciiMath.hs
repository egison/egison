{- |
Module      : Language.Egison.PrettyMath.AsciiMath
Licence     : MIT
-}

module Language.Egison.PrettyMath.AsciiMath
  ( showMathExpr
  ) where

import           Data.List                      (intercalate)

import           Language.Egison.PrettyMath.AST

showMathExpr :: MathExpr -> String
showMathExpr (Atom func []) = func
showMathExpr (NegativeAtom func) = "-" ++ func
showMathExpr (Plus []) = ""
showMathExpr (Plus (x:xs)) = showMathExpr x ++ showMathExprForPlus xs
 where
  showMathExprForPlus :: [MathExpr] -> String
  showMathExprForPlus []                                  = ""
  showMathExprForPlus (NegativeAtom a:xs)                 = " - " ++ a ++ showMathExprForPlus xs
  showMathExprForPlus (Multiply (NegativeAtom "1":ys):xs) = " - " ++ showMathExpr (Multiply ys) ++ showMathExprForPlus xs
  showMathExprForPlus (Multiply (NegativeAtom a:ys):xs)   = " - " ++ showMathExpr (Multiply (Atom a []:ys)) ++ " " ++ showMathExprForPlus xs
  showMathExprForPlus (x:xs)                              = " + " ++ showMathExpr x ++ showMathExprForPlus xs
showMathExpr (Multiply []) = ""
showMathExpr (Multiply [x]) = showMathExpr x
showMathExpr (Multiply (NegativeAtom "1":xs)) = "-" ++ showMathExpr (Multiply xs)
showMathExpr (Multiply (x:xs)) = showMathExpr' x ++ " " ++ showMathExpr (Multiply xs)
showMathExpr (Div x y) = "frac{" ++ showMathExpr x ++ "}{" ++ showMathExpr y ++ "}"
showMathExpr (Power lv1 lv2) = showMathExpr lv1 ++ "^" ++ showMathExpr lv2
showMathExpr (Func (Atom "sqrt" []) [x]) = "sqrt " ++ showMathExpr x
showMathExpr (Func (Atom "rt" []) [x, y]) = "root " ++ showMathExpr x ++ " " ++ showMathExpr y
showMathExpr (Func (Atom "exp" []) [x]) = "e^(" ++ showMathExpr x ++ ")"
showMathExpr (Func f lvs) = showMathExpr f ++ "(" ++ showMathExprArg lvs ++ ")"
showMathExpr (Tensor lvs mis)
  | null mis = "(" ++ showMathExprArg lvs ++ ")"
  | not (any isSub mis) = "(" ++ showMathExprArg lvs ++ ")^(" ++ showMathExprIndices mis ++ ")"
  | all isSub mis = "(" ++ showMathExprArg lvs ++ ")_(" ++ showMathExprIndices mis ++ ")"
  | otherwise = "(" ++ showMathExprArg lvs ++ ")_(" ++ showMathExprIndices (filter isSub mis) ++ ")^(" ++ showMathExprIndices (filter (not . isSub) mis) ++ ")"
showMathExpr (Tuple lvs) = "(" ++ showMathExprArg lvs ++ ")"
showMathExpr (Collection lvs) = "{" ++ showMathExprArg lvs ++ "}"

showMathExpr' :: MathExpr -> String
showMathExpr' (Plus lvs) = "(" ++ showMathExpr (Plus lvs) ++ ")"
showMathExpr' val        = showMathExpr val

showMathExprArg :: [MathExpr] -> String
showMathExprArg exprs = intercalate ", " $ map showMathExpr exprs

showMathExprIndices :: [MathIndex] -> String
showMathExprIndices []  = error "unreachable"
showMathExprIndices lvs = concatMap showMathIndex lvs

showMathIndex :: MathIndex -> String
showMathIndex (Super a) = showMathExpr a
showMathIndex (Sub a)   = showMathExpr a
