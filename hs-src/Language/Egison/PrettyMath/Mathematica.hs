{- |
Module      : Language.Egison.PrettyMath.Mathematica
Licence     : MIT
-}

module Language.Egison.PrettyMath.Mathematica
  ( showMathExpr
  ) where

import           Data.List                     (intercalate)

import           Language.Egison.PrettyMath.AST

showMathExpr :: MathExpr -> String
showMathExpr (Atom a []) = a
showMathExpr (Atom a xs) = a ++ showMathExprIndices xs
showMathExpr (Partial f xs) = showMathExpr f ++ "_" ++ showMathExprs "_" xs
showMathExpr (NegativeAtom a) = "-" ++ a
showMathExpr (Plus []) = ""
showMathExpr (Plus (x:xs)) = showMathExpr x ++ showMathExprForPlus xs
 where
  showMathExprForPlus :: [MathExpr] -> String
  showMathExprForPlus [] = ""
  showMathExprForPlus (NegativeAtom a:xs) = " - " ++ a ++ showMathExprForPlus xs
  showMathExprForPlus (Multiply (NegativeAtom "1":ys):xs) = " - " ++ showMathExpr (Multiply ys) ++ showMathExprForPlus xs
  showMathExprForPlus (Multiply (NegativeAtom a:ys):xs) = " - " ++ showMathExpr (Multiply (Atom a []:ys)) ++ showMathExprForPlus xs
  showMathExprForPlus (x:xs) = " + " ++  showMathExpr x ++ showMathExprForPlus xs
showMathExpr (Multiply []) = ""
showMathExpr (Multiply [x]) = showMathExpr x
showMathExpr (Multiply (Atom "1" []:xs)) = showMathExpr (Multiply xs)
showMathExpr (Multiply (NegativeAtom "1":xs)) = "-" ++ showMathExpr (Multiply xs)
showMathExpr (Multiply (x:xs)) = showMathExpr' x ++ " " ++ showMathExpr (Multiply xs)
showMathExpr (Div x y) = addBracket x ++ "/" ++ addBracket y
 where
   addBracket x@(Atom _ []) = showMathExpr x
   addBracket x             = "(" ++ showMathExpr x ++ ")"
showMathExpr (Power lv1 lv2) = showMathExpr lv1 ++ "^" ++ showMathExpr lv2
showMathExpr (Func (Atom "sqrt" []) [x]) = "Sqrt[" ++ showMathExpr x ++ "]"
showMathExpr (Func (Atom "rt" []) [x, y]) = "Surd[" ++ showMathExpr x ++ "," ++ showMathExpr y ++ "]"
showMathExpr (Func (Atom "exp" []) [x])= "e^(" ++ showMathExpr x ++ ")"
showMathExpr (Func f xs) = showMathExpr f ++ "(" ++ showMathExprArg xs ++ ")"
showMathExpr (Tensor lvs mis)
  | null mis = "{" ++ showMathExprArg lvs ++ "}"
  | not (any isSub mis) = "{" ++ showMathExprArg lvs ++ "}^(" ++ showMathExprIndices mis ++ ")"
  | all isSub mis = "{" ++ showMathExprArg lvs ++ "}_(" ++ showMathExprIndices mis ++ ")"
  | otherwise = "{" ++ showMathExprArg lvs ++ "}_(" ++ showMathExprIndices (filter isSub mis) ++ ")^(" ++ showMathExprIndices (filter (not . isSub) mis) ++ ")"
showMathExpr (Tuple xs) = "(" ++ showMathExprArg xs ++ ")"
showMathExpr (Collection xs) = "{" ++ showMathExprArg xs ++ "}"
showMathExpr (Quote x) = "(" ++ showMathExpr x ++ ")"

showMathExpr' :: MathExpr -> String
showMathExpr' (Plus xs) = "(" ++ showMathExpr (Plus xs) ++ ")"
showMathExpr' x = showMathExpr x

showMathExprs :: String -> [MathExpr] -> String
showMathExprs sep exprs = intercalate sep $ map showMathExpr exprs

showMathExprArg :: [MathExpr] -> String
showMathExprArg = showMathExprs ", "

showMathExprIndices :: [MathIndex] -> String
showMathExprIndices []  = error "unreachable"
showMathExprIndices lvs = concatMap showMathIndex lvs

showMathIndex :: MathIndex -> String
showMathIndex (Super a) = showMathExpr a
showMathIndex (Sub a)   = showMathExpr a
