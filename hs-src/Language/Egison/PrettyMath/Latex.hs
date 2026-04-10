{- |
Module      : Language.Egison.PrettyMath.Latex
Licence     : MIT
-}

module Language.Egison.PrettyMath.Latex
  ( showMathValue
  ) where

import           Data.List                      (intercalate)

import           Language.Egison.PrettyMath.AST

showMathValue :: MathValue -> String
showMathValue (Atom a []) = a
showMathValue (Atom a xs) = a ++ showMathValueScript xs
showMathValue (Partial f xs) = "\\frac{" ++ convertToPartial (f, length xs) ++ "}{" ++ showPartial xs ++ "}"
 where
  showPartial :: [MathValue] -> String
  showPartial xs = let lx = elemCount xs in convertToPartial2 (head lx) ++ foldr (\x acc -> " " ++ convertToPartial2 x ++ acc) "" (tail lx)

  convertToPartial :: (MathValue, Int) -> String
  convertToPartial (x, 1) = "\\partial " ++ showMathValue x
  convertToPartial (x, n) = "\\partial^" ++ show n ++ " " ++ showMathValue x

  convertToPartial2 :: (MathValue, Int) -> String
  convertToPartial2 (x, 1) = "\\partial " ++ showMathValue x
  convertToPartial2 (x, n) = "\\partial " ++ showMathValue x ++ "^"  ++ show n
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
showMathValue (Div x y) = "\\frac{" ++ showMathValue x ++ "}{" ++ showMathValue y ++ "}"
showMathValue (Power lv1 lv2) = showMathValue lv1 ++ "^" ++ showMathValue lv2
showMathValue (Func (Atom "sqrt" []) [x]) = "\\sqrt{" ++ showMathValue x ++ "}"
showMathValue (Func (Atom "rt" []) [x, y]) = "\\sqrt[" ++ showMathValue x ++ "]{" ++ showMathValue y ++ "}"
showMathValue (Func (Atom "exp" []) [x]) = "e^{" ++ showMathValue x ++ "}"
showMathValue (Func f xs) = showMathValue f ++ "(" ++ showMathValueArg xs ", " ++ ")"
showMathValue (Tensor xs mis) = "\\begin{pmatrix} " ++ showMathValueVectors xs ++ "\\end{pmatrix}" ++ showMathValueScript mis
showMathValue (Tuple xs) = "(" ++ showMathValueArg xs ", " ++ ")"
showMathValue (Collection xs) = "\\{" ++ showMathValueArg xs ", " ++ "\\}"
showMathValue (Quote x) = "(" ++ showMathValue x ++ ")"

showMathValue' :: MathValue -> String
showMathValue' (Plus xs) = "(" ++ showMathValue (Plus xs) ++ ")"
showMathValue' x         = showMathValue x

showMathValueArg :: [MathValue] -> String -> String
showMathValueArg exprs sep = intercalate sep $ map showMathValue exprs

showMathValueSuper :: MathIndex -> String
showMathValueSuper (Super (Atom "#" [])) = "\\#"
showMathValueSuper (Super x)             = showMathValue x
showMathValueSuper (Sub _)               = "\\;"

showMathValueSub :: MathIndex -> String
showMathValueSub (Sub (Atom "#" [])) = "\\#"
showMathValueSub (Sub x)             = showMathValue x
showMathValueSub (Super _)           = "\\;"

showMathValueScript :: [MathIndex] -> String
showMathValueScript [] = ""
showMathValueScript is = "_{" ++ concatMap showMathValueSub is ++ "}^{" ++ concatMap showMathValueSuper is ++ "}"

showMathValueVectors :: [MathValue] -> String
showMathValueVectors []                = ""
showMathValueVectors (Tensor lvs []:r) = showMathValueArg lvs " & " ++ " \\\\ " ++ showMathValueVectors r
showMathValueVectors lvs               = showMathValueArg lvs " \\\\ " ++ "\\\\ "

elemCount :: Eq a => [a] -> [(a, Int)]
elemCount []     = []
elemCount (x:xs) = (x, length (filter (== x) xs) + 1) : elemCount (filter (/= x) xs)
