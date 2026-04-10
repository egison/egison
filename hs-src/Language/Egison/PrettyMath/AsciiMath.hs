{- |
Module      : Language.Egison.PrettyMath.AsciiMath
Licence     : MIT
-}

module Language.Egison.PrettyMath.AsciiMath
  ( showMathValue
  ) where

import           Data.List                      (intercalate)

import           Language.Egison.PrettyMath.AST

showMathValue :: MathValue -> String
showMathValue (Atom func []) = func
showMathValue (NegativeAtom func) = "-" ++ func
showMathValue (Plus []) = ""
showMathValue (Plus (x:xs)) = showMathValue x ++ showMathValueForPlus xs
 where
  showMathValueForPlus :: [MathValue] -> String
  showMathValueForPlus []                                  = ""
  showMathValueForPlus (NegativeAtom a:xs)                 = " - " ++ a ++ showMathValueForPlus xs
  showMathValueForPlus (Multiply (NegativeAtom "1":ys):xs) = " - " ++ showMathValue (Multiply ys) ++ showMathValueForPlus xs
  showMathValueForPlus (Multiply (NegativeAtom a:ys):xs)   = " - " ++ showMathValue (Multiply (Atom a []:ys)) ++ " " ++ showMathValueForPlus xs
  showMathValueForPlus (x:xs)                              = " + " ++ showMathValue x ++ showMathValueForPlus xs
showMathValue (Multiply []) = ""
showMathValue (Multiply [x]) = showMathValue x
showMathValue (Multiply (NegativeAtom "1":xs)) = "-" ++ showMathValue (Multiply xs)
showMathValue (Multiply (x:xs)) = showMathValue' x ++ " " ++ showMathValue (Multiply xs)
showMathValue (Div x y) = "frac{" ++ showMathValue x ++ "}{" ++ showMathValue y ++ "}"
showMathValue (Power lv1 lv2) = showMathValue lv1 ++ "^" ++ showMathValue lv2
showMathValue (Func (Atom "sqrt" []) [x]) = "sqrt " ++ showMathValue x
showMathValue (Func (Atom "rt" []) [x, y]) = "root " ++ showMathValue x ++ " " ++ showMathValue y
showMathValue (Func (Atom "exp" []) [x]) = "e^(" ++ showMathValue x ++ ")"
showMathValue (Func f lvs) = showMathValue f ++ "(" ++ showMathValueArg lvs ++ ")"
showMathValue (Tensor lvs mis)
  | null mis = "(" ++ showMathValueArg lvs ++ ")"
  | not (any isSub mis) = "(" ++ showMathValueArg lvs ++ ")^(" ++ showMathValueIndices mis ++ ")"
  | all isSub mis = "(" ++ showMathValueArg lvs ++ ")_(" ++ showMathValueIndices mis ++ ")"
  | otherwise = "(" ++ showMathValueArg lvs ++ ")_(" ++ showMathValueIndices (filter isSub mis) ++ ")^(" ++ showMathValueIndices (filter (not . isSub) mis) ++ ")"
showMathValue (Tuple lvs) = "(" ++ showMathValueArg lvs ++ ")"
showMathValue (Collection lvs) = "{" ++ showMathValueArg lvs ++ "}"

showMathValue' :: MathValue -> String
showMathValue' (Plus lvs) = "(" ++ showMathValue (Plus lvs) ++ ")"
showMathValue' val        = showMathValue val

showMathValueArg :: [MathValue] -> String
showMathValueArg exprs = intercalate ", " $ map showMathValue exprs

showMathValueIndices :: [MathIndex] -> String
showMathValueIndices []  = error "unreachable"
showMathValueIndices lvs = concatMap showMathIndex lvs

showMathIndex :: MathIndex -> String
showMathIndex (Super a) = showMathValue a
showMathIndex (Sub a)   = showMathValue a
