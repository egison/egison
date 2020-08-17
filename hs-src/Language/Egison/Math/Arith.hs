module Language.Egison.Math.Arith
  ( mathPlus
  , mathMinus
  , mathMult
  , mathDiv
  , mathPower
  , mathNumerator
  , mathDenominator
  ) where

import           Language.Egison.Math.Expr
import           Language.Egison.Math.Normalize

mathPlus :: ScalarData -> ScalarData -> ScalarData
mathPlus (Div m1 n1) (Div m2 n2) = mathNormalize' $ Div (mathPlusPoly (mathMultPoly m1 n2) (mathMultPoly m2 n1)) (mathMultPoly n1 n2)

mathPlusPoly :: PolyExpr -> PolyExpr -> PolyExpr
mathPlusPoly (Plus ts1) (Plus ts2) = Plus (ts1 ++ ts2)

mathMinus :: ScalarData -> ScalarData -> ScalarData
mathMinus s1 s2 = mathPlus s1 (mathNegate s2)

mathMult :: ScalarData -> ScalarData -> ScalarData
mathMult (Div m1 n1) (Div m2 n2) = mathNormalize' $ Div (mathMultPoly m1 m2) (mathMultPoly n1 n2)

mathMultPoly :: PolyExpr -> PolyExpr -> PolyExpr
mathMultPoly (Plus []) (Plus _) = Plus []
mathMultPoly (Plus _) (Plus []) = Plus []
mathMultPoly (Plus ts1) (Plus ts2) = foldl mathPlusPoly (Plus []) (map (\(Term a xs) -> Plus (map (\(Term b ys) -> Term (a * b) (xs ++ ys)) ts2)) ts1)

mathDiv :: ScalarData -> ScalarData -> ScalarData
mathDiv s (Div p1 p2) = mathMult s (Div p2 p1)

mathPower :: ScalarData -> Integer -> ScalarData
mathPower _ 0          = SingleTerm 1 []
mathPower s 1          = s
mathPower s n | n >= 2 = mathMult s (mathPower s (n - 1))

mathNumerator :: ScalarData -> ScalarData
mathNumerator (Div m _) = Div m (Plus [Term 1 []])

mathDenominator :: ScalarData -> ScalarData
mathDenominator (Div _ n) = Div n (Plus [Term 1 []])
