{- |
Module      : Language.Egison.PrettyMath.Latex
Licence     : MIT
-}

module Language.Egison.PrettyMath.Latex
  ( showMathExpr
  ) where

import           Data.List                      (intercalate)

import           Language.Egison.PrettyMath.AST

showMathExpr :: MathExpr -> String
showMathExpr (Atom a []) = a
showMathExpr (Atom a xs) = a ++ showMathExprScript xs
showMathExpr (Partial f xs) = "\\frac{" ++ convertToPartial (f, length xs) ++ "}{" ++ showPartial xs ++ "}"
 where
  showPartial :: [MathExpr] -> String
  showPartial xs = let lx = elemCount xs in convertToPartial2 (head lx) ++ foldr (\x acc -> " " ++ convertToPartial2 x ++ acc) "" (tail lx)

  convertToPartial :: (MathExpr, Int) -> String
  convertToPartial (x, 1) = "\\partial " ++ showMathExpr x
  convertToPartial (x, n) = "\\partial^" ++ show n ++ " " ++ showMathExpr x

  convertToPartial2 :: (MathExpr, Int) -> String
  convertToPartial2 (x, 1) = "\\partial " ++ showMathExpr x
  convertToPartial2 (x, n) = "\\partial " ++ showMathExpr x ++ "^"  ++ show n
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
showMathExpr (Multiply (x:xs)) = showMathExpr' x ++ " " ++ showMathExpr (Multiply xs)
showMathExpr (Div x y) = "\\frac{" ++ showMathExpr x ++ "}{" ++ showMathExpr y ++ "}"
showMathExpr (Power lv1 lv2) = showMathExpr lv1 ++ "^" ++ showMathExpr lv2
showMathExpr (Func (Atom "sqrt" []) [x]) = "\\sqrt{" ++ showMathExpr x ++ "}"
showMathExpr (Func (Atom "rt" []) [x, y]) = "\\sqrt[" ++ showMathExpr x ++ "]{" ++ showMathExpr y ++ "}"
showMathExpr (Func (Atom "exp" []) [x]) = "e^{" ++ showMathExpr x ++ "}"
showMathExpr (Func f xs) = showMathExpr f ++ "(" ++ showMathExprArg xs ", " ++ ")"
showMathExpr (Tensor xs mis) = "\\begin{pmatrix} " ++ showMathExprVectors xs ++ "\\end{pmatrix}" ++ showMathExprScript mis
showMathExpr (Tuple xs) = "(" ++ showMathExprArg xs ", " ++ ")"
showMathExpr (Collection xs) = "\\{" ++ showMathExprArg xs ", " ++ "\\}"
showMathExpr (Quote x) = "(" ++ showMathExpr x ++ ")"

showMathExpr' :: MathExpr -> String
showMathExpr' (Plus xs) = "(" ++ showMathExpr (Plus xs) ++ ")"
showMathExpr' x         = showMathExpr x

showMathExprArg :: [MathExpr] -> String -> String
showMathExprArg exprs sep = intercalate sep $ map showMathExpr exprs

showMathExprSuper :: MathIndex -> String
showMathExprSuper (Super (Atom "#" [])) = "\\#"
showMathExprSuper (Super x)             = showMathExpr x
showMathExprSuper (Sub _)               = "\\;"

showMathExprSub :: MathIndex -> String
showMathExprSub (Sub (Atom "#" [])) = "\\#"
showMathExprSub (Sub x)             = showMathExpr x
showMathExprSub (Super _)           = "\\;"

showMathExprScript :: [MathIndex] -> String
showMathExprScript [] = ""
showMathExprScript is = "_{" ++ concatMap showMathExprSub is ++ "}^{" ++ concatMap showMathExprSuper is ++ "}"

showMathExprVectors :: [MathExpr] -> String
showMathExprVectors []                = ""
showMathExprVectors (Tensor lvs []:r) = showMathExprArg lvs " & " ++ " \\\\ " ++ showMathExprVectors r
showMathExprVectors lvs               = showMathExprArg lvs " \\\\ " ++ "\\\\ "

elemCount :: Eq a => [a] -> [(a, Int)]
elemCount []     = []
elemCount (x:xs) = (x, length (filter (== x) xs) + 1) : elemCount (filter (/= x) xs)
