{- |
Module      : Language.Egison.PrettyMath.Latex
Licence     : MIT
-}

module Language.Egison.PrettyMath.Latex
  ( showMathExprLatex
  ) where

import           Data.List                     (intercalate)

import           Language.Egison.PrettyMath.AST

showMathExprLatex :: MathExpr -> String
showMathExprLatex (Atom a []) = a
showMathExprLatex (Atom a xs) = a ++ showMathExprLatexScript xs
showMathExprLatex (Partial f xs) = "\\frac{" ++ convertToPartial (f, length xs) ++ "}{" ++ showPartial xs ++ "}"
 where
  showPartial :: [MathExpr] -> String
  showPartial xs = let lx = elemCount xs in convertToPartial2 (head lx) ++ foldr (\x acc -> " " ++ convertToPartial2 x ++ acc) "" (tail lx)

  convertToPartial :: (MathExpr, Int) -> String
  convertToPartial (x, 1) = "\\partial " ++ showMathExprLatex x
  convertToPartial (x, n) = "\\partial^" ++ show n ++ " " ++ showMathExprLatex x

  convertToPartial2 :: (MathExpr, Int) -> String
  convertToPartial2 (x, 1) = "\\partial " ++ showMathExprLatex x
  convertToPartial2 (x, n) = "\\partial " ++ showMathExprLatex x ++ "^"  ++ show n
showMathExprLatex (NegativeAtom a) = "-" ++ a
showMathExprLatex (Plus []) = ""
showMathExprLatex (Plus (x:xs)) = showMathExprLatex x ++ showMathExprLatexForPlus xs
 where
  showMathExprLatexForPlus :: [MathExpr] -> String
  showMathExprLatexForPlus [] = ""
  showMathExprLatexForPlus (NegativeAtom a:xs) = " - " ++ a ++ showMathExprLatexForPlus xs
  showMathExprLatexForPlus (Multiply (NegativeAtom "1":ys):xs) = " - " ++ showMathExprLatex (Multiply ys) ++ showMathExprLatexForPlus xs
  showMathExprLatexForPlus (Multiply (NegativeAtom a:ys):xs) = " - " ++ showMathExprLatex (Multiply (Atom a []:ys)) ++ showMathExprLatexForPlus xs
  showMathExprLatexForPlus (x:xs) = " + " ++  showMathExprLatex x ++ showMathExprLatexForPlus xs
showMathExprLatex (Multiply []) = ""
showMathExprLatex (Multiply [x]) = showMathExprLatex x
showMathExprLatex (Multiply (Atom "1" []:xs)) = showMathExprLatex (Multiply xs)
showMathExprLatex (Multiply (NegativeAtom "1":xs)) = "-" ++ showMathExprLatex (Multiply xs)
showMathExprLatex (Multiply (x:xs)) = showMathExprLatex' x ++ " " ++ showMathExprLatex (Multiply xs)
showMathExprLatex (Power lv1 lv2) = showMathExprLatex lv1 ++ "^" ++ showMathExprLatex lv2
showMathExprLatex (Func (Atom "sqrt" []) [x]) = "\\sqrt{" ++ showMathExprLatex x ++ "}"
showMathExprLatex (Func (Atom "rt" []) [x, y]) = "\\sqrt[" ++ showMathExprLatex x ++ "]{" ++ showMathExprLatex y ++ "}"
showMathExprLatex (Func (Atom "/" []) [x, y]) = "\\frac{" ++ showMathExprLatex x ++ "}{" ++ showMathExprLatex y ++ "}"
showMathExprLatex (Func f xs) = showMathExprLatex f ++ "(" ++ showMathExprLatexArg xs ", " ++ ")"
showMathExprLatex (Tensor xs mis) = "\\begin{pmatrix} " ++ showMathExprLatexVectors xs ++ "\\end{pmatrix}" ++ showMathExprLatexScript mis
showMathExprLatex (Tuple xs) = "(" ++ showMathExprLatexArg xs ", " ++ ")"
showMathExprLatex (Collection xs) = "\\{" ++ showMathExprLatexArg xs ", " ++ "\\}"
showMathExprLatex (Exp x) = "e^{" ++ showMathExprLatex x ++ "}"
showMathExprLatex (Quote x) = "(" ++ showMathExprLatex x ++ ")"

showMathExprLatex' :: MathExpr -> String
showMathExprLatex' (Plus xs) = "(" ++ showMathExprLatex (Plus xs) ++ ")"
showMathExprLatex' x         = showMathExprLatex x

showMathExprLatexArg :: [MathExpr] -> String -> String
showMathExprLatexArg exprs sep = intercalate sep $ map showMathExprLatex exprs

showMathExprLatexSuper :: MathIndex -> String
showMathExprLatexSuper (Super (Atom "#" [])) = "\\#"
showMathExprLatexSuper (Super x)             = showMathExprLatex x
showMathExprLatexSuper (Sub _)               = "\\;"

showMathExprLatexSub :: MathIndex -> String
showMathExprLatexSub (Sub (Atom "#" [])) = "\\#"
showMathExprLatexSub (Sub x)             = showMathExprLatex x
showMathExprLatexSub (Super _)           = "\\;"

showMathExprLatexScript :: [MathIndex] -> String
showMathExprLatexScript [] = ""
showMathExprLatexScript is = "_{" ++ concatMap showMathExprLatexSub is ++ "}^{" ++ concatMap showMathExprLatexSuper is ++ "}"

showMathExprLatexVectors :: [MathExpr] -> String
showMathExprLatexVectors [] = ""
showMathExprLatexVectors (Tensor lvs []:r) = showMathExprLatexArg lvs " & " ++ " \\\\ " ++ showMathExprLatexVectors r
showMathExprLatexVectors lvs = showMathExprLatexArg lvs " \\\\ " ++ "\\\\ "

elemCount :: Eq a => [a] -> [(a, Int)]
elemCount [] = []
elemCount (x:xs) = (x, length (filter (== x) xs) + 1) : elemCount (filter (/= x) xs)
