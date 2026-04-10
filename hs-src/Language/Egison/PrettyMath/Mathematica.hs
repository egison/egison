{- |
Module      : Language.Egison.PrettyMath.Mathematica
Licence     : MIT
-}

module Language.Egison.PrettyMath.Mathematica
  ( showMathValue
  ) where

import           Data.List                      (intercalate)

import           Language.Egison.PrettyMath.AST

showMathValue :: MathValue -> String
showMathValue (Atom a []) = a
showMathValue (Atom a xs) = a ++ showMathValueIndices xs
showMathValue (Partial f xs) = showMathValue f ++ "_" ++ showMathValues "_" xs
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
showMathValue (Multiply (x:xs)) = showMathValue' x ++ " " ++ showMathValue (Multiply xs)
showMathValue (Div x y) = addBracket x ++ "/" ++ addBracket y
 where
   addBracket x@(Atom _ []) = showMathValue x
   addBracket x             = "(" ++ showMathValue x ++ ")"
showMathValue (Power lv1 lv2) = showMathValue lv1 ++ "^" ++ showMathValue lv2
showMathValue (Func (Atom "sqrt" []) [x]) = "Sqrt[" ++ showMathValue x ++ "]"
showMathValue (Func (Atom "rt" []) [x, y]) = "Surd[" ++ showMathValue x ++ "," ++ showMathValue y ++ "]"
showMathValue (Func (Atom "exp" []) [x])= "e^(" ++ showMathValue x ++ ")"
showMathValue (Func f xs) = showMathValue f ++ "(" ++ showMathValueArg xs ++ ")"
showMathValue (Tensor lvs mis)
  | null mis = "{" ++ showMathValueArg lvs ++ "}"
  | not (any isSub mis) = "{" ++ showMathValueArg lvs ++ "}^(" ++ showMathValueIndices mis ++ ")"
  | all isSub mis = "{" ++ showMathValueArg lvs ++ "}_(" ++ showMathValueIndices mis ++ ")"
  | otherwise = "{" ++ showMathValueArg lvs ++ "}_(" ++ showMathValueIndices (filter isSub mis) ++ ")^(" ++ showMathValueIndices (filter (not . isSub) mis) ++ ")"
showMathValue (Tuple xs) = "(" ++ showMathValueArg xs ++ ")"
showMathValue (Collection xs) = "{" ++ showMathValueArg xs ++ "}"
showMathValue (Quote x) = "(" ++ showMathValue x ++ ")"

showMathValue' :: MathValue -> String
showMathValue' (Plus xs) = "(" ++ showMathValue (Plus xs) ++ ")"
showMathValue' x         = showMathValue x

showMathValues :: String -> [MathValue] -> String
showMathValues sep exprs = intercalate sep $ map showMathValue exprs

showMathValueArg :: [MathValue] -> String
showMathValueArg = showMathValues ", "

showMathValueIndices :: [MathIndex] -> String
showMathValueIndices []  = error "unreachable"
showMathValueIndices lvs = concatMap showMathIndex lvs

showMathIndex :: MathIndex -> String
showMathIndex (Super a) = showMathValue a
showMathIndex (Sub a)   = showMathValue a
