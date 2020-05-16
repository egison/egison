{- |
Module      : Language.Egison.PrettyMath.Mathematica
Licence     : MIT
-}

module Language.Egison.PrettyMath.Mathematica
  ( showMathExprMathematica
  ) where

import           Data.List                     (intercalate)

import           Language.Egison.PrettyMath.AST

showMathExprMathematica :: MathExpr -> String
showMathExprMathematica (Atom a []) = a
showMathExprMathematica (Partial f xs) = showMathExprMathematica f ++ "_" ++ showMathExprsMathematica "_" xs
showMathExprMathematica (NegativeAtom a) = "-" ++ a
showMathExprMathematica (Plus []) = ""
showMathExprMathematica (Plus (x:xs)) = showMathExprMathematica x ++ showMathExprMathematicaForPlus xs
 where
  showMathExprMathematicaForPlus :: [MathExpr] -> String
  showMathExprMathematicaForPlus [] = ""
  showMathExprMathematicaForPlus (NegativeAtom a:xs) = " - " ++ a ++ showMathExprMathematicaForPlus xs
  showMathExprMathematicaForPlus (Multiply (NegativeAtom "1":ys):xs) = " - " ++ showMathExprMathematica (Multiply ys) ++ showMathExprMathematicaForPlus xs
  showMathExprMathematicaForPlus (Multiply (NegativeAtom a:ys):xs) = " - " ++ showMathExprMathematica (Multiply (Atom a []:ys)) ++ showMathExprMathematicaForPlus xs
  showMathExprMathematicaForPlus (x:xs) = " + " ++  showMathExprMathematica x ++ showMathExprMathematicaForPlus xs
showMathExprMathematica (Multiply []) = ""
showMathExprMathematica (Multiply [x]) = showMathExprMathematica x
showMathExprMathematica (Multiply (Atom "1" []:xs)) = showMathExprMathematica (Multiply xs)
showMathExprMathematica (Multiply (NegativeAtom "1":xs)) = "-" ++ showMathExprMathematica (Multiply xs)
showMathExprMathematica (Multiply (x:xs)) = showMathExprMathematica' x ++ " " ++ showMathExprMathematica (Multiply xs)
showMathExprMathematica (Power lv1 lv2) = showMathExprMathematica lv1 ++ "^" ++ showMathExprMathematica lv2
showMathExprMathematica (Func (Atom "sqrt" []) [x]) = "Sqrt[" ++ showMathExprMathematica x ++ "]"
showMathExprMathematica (Func (Atom "rt" []) [x, y]) = "Surd[" ++ showMathExprMathematica x ++ "," ++ showMathExprMathematica y ++ "]"
showMathExprMathematica (Func (Atom "/" []) [x, y]) = addBracket x ++ "/" ++ addBracket y
 where
   addBracket x@(Atom _ []) = showMathExprMathematica x
   addBracket x             = "(" ++ showMathExprMathematica x ++ ")"
showMathExprMathematica (Func f xs) = showMathExprMathematica f ++ "(" ++ showMathExprMathematicaArg xs ++ ")"
showMathExprMathematica (Tensor lvs mis)
  | null mis = "{" ++ showMathExprMathematicaArg lvs ++ "}"
  | not (any isSub mis) = "{" ++ showMathExprMathematicaArg lvs ++ "}^(" ++ showMathExprMathematicaIndices mis ++ ")"
  | not (any (not . isSub) mis) = "{" ++ showMathExprMathematicaArg lvs ++ "}_(" ++ showMathExprMathematicaIndices mis ++ ")"
  | otherwise = "{" ++ showMathExprMathematicaArg lvs ++ "}_(" ++ showMathExprMathematicaIndices (filter isSub mis) ++ ")^(" ++ showMathExprMathematicaIndices (filter (not . isSub) mis) ++ ")"
showMathExprMathematica (Tuple xs) = "(" ++ showMathExprMathematicaArg xs ++ ")"
showMathExprMathematica (Collection xs) = "{" ++ showMathExprMathematicaArg xs ++ "}"
showMathExprMathematica (Exp x) = "e^(" ++ showMathExprMathematica x ++ ")"
showMathExprMathematica (Quote x) = "(" ++ showMathExprMathematica x ++ ")"

showMathExprMathematica' :: MathExpr -> String
showMathExprMathematica' (Plus xs) = "(" ++ showMathExprMathematica (Plus xs) ++ ")"
showMathExprMathematica' x = showMathExprMathematica x

showMathExprsMathematica :: String -> [MathExpr] -> String
showMathExprsMathematica sep exprs = intercalate sep $ map showMathExprMathematica exprs

showMathExprMathematicaArg :: [MathExpr] -> String
showMathExprMathematicaArg = showMathExprsMathematica ", "

showMathExprMathematicaIndices :: [MathIndex] -> String
showMathExprMathematicaIndices []  = error "unreachable"
showMathExprMathematicaIndices lvs = concatMap showMathIndexMathematica lvs

showMathIndexMathematica :: MathIndex -> String
showMathIndexMathematica (Super a) = showMathExprMathematica a
showMathIndexMathematica (Sub a)   = showMathExprMathematica a
