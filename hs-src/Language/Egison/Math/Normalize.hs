{-# LANGUAGE QuasiQuotes #-}

{- |
Module      : Language.Egison.Math.Expr
Licence     : MIT

This module implements the normalization of polynomials. Normalization rules
for particular mathematical functions (such as sqrt and sin/cos) are defined
in Rewrite.hs.
-}

module Language.Egison.Math.Normalize
  ( mathNormalize'
  , termsGcd
  , mathDivideTerm
  ) where

import           Control.Egison

import           Language.Egison.Math.Expr


mathNormalize' :: ScalarData -> ScalarData
mathNormalize' = mathDivide . mathRemoveZero . mathFold . mathRemoveZeroSymbol

termsGcd :: [TermExpr] -> TermExpr
termsGcd ts@(_:_) =
  foldl1 (\(Term a xs) (Term b ys) -> Term (gcd a b) (monoGcd xs ys)) ts
 where
  monoGcd :: Monomial -> Monomial -> Monomial
  monoGcd [] _ = []
  monoGcd ((x, n):xs) ys =
    case f (x, n) ys of
      (_, 0) -> monoGcd xs ys
      (z, m) -> (z, m) : monoGcd xs ys

  f :: (SymbolExpr, Integer) -> Monomial -> (SymbolExpr, Integer)
  f (x, _) [] = (x, 0)
  f (Quote x, n) ((Quote y, m):ys)
    | x == y            = (Quote x, min n m)
    | x == mathNegate y = (Quote x, min n m)
    | otherwise         = f (Quote x, n) ys
  f (x, n) ((y, m):ys)
    | x == y    = (x, min n m)
    | otherwise = f (x, n) ys

mathDivide :: ScalarData -> ScalarData
mathDivide mExpr@(Div (Plus _) (Plus [])) = mExpr
mathDivide mExpr@(Div (Plus []) (Plus _)) = mExpr
mathDivide (Div (Plus ts1) (Plus ts2)) =
  let z@(Term c zs) = termsGcd (ts1 ++ ts2) in
  case ts2 of
    [Term a _] | a < 0 -> Div (Plus (map (`mathDivideTerm` Term (-c) zs) ts1))
                              (Plus (map (`mathDivideTerm` Term (-c) zs) ts2))
    _                  -> Div (Plus (map (`mathDivideTerm` z) ts1))
                              (Plus (map (`mathDivideTerm` z) ts2))

mathDivideTerm :: TermExpr -> TermExpr -> TermExpr
mathDivideTerm (Term a xs) (Term b ys) =
  let (sgn, zs) = divMonomial xs ys in
  Term (sgn * div a b) zs
 where
  divMonomial :: Monomial -> Monomial -> (Integer, Monomial)
  divMonomial xs [] = (1, xs)
  divMonomial xs ((y, m):ys) =
    match dfs (y, xs) (Pair SymbolM (Multiset (Pair SymbolM Eql)))
      -- Because we've applied |mathFold|, we can only divide the first matching monomial
      [ [mc| (quote $s, ($x & negQuote #s, $n) : $xss) ->
               let (sgn, xs') = divMonomial xss ys in
               let sgn' = if even m then 1 else -1 in
               if n == m then (sgn * sgn', xs')
                         else (sgn * sgn', (x, n - m) : xs') |]
      , [mc| (_, (#y, $n) : $xss) ->
               let (sgn, xs') = divMonomial xss ys in
               if n == m then (sgn, xs') else (sgn, (y, n - m) : xs') |]
      , [mc| _ -> divMonomial xs ys |]
      ]

mathRemoveZeroSymbol :: ScalarData -> ScalarData
mathRemoveZeroSymbol (Div (Plus ts1) (Plus ts2)) =
  let ts1' = map (\(Term a xs) -> Term a (filter p xs)) ts1
      ts2' = map (\(Term a xs) -> Term a (filter p xs)) ts2
   in Div (Plus ts1') (Plus ts2')
  where
    p (_, 0) = False
    p _      = True

mathRemoveZero :: ScalarData -> ScalarData
mathRemoveZero (Div (Plus ts1) (Plus ts2)) =
  let ts1' = filter (\(Term a _) -> a /= 0) ts1 in
  let ts2' = filter (\(Term a _) -> a /= 0) ts2 in
    case ts1' of
      [] -> Div (Plus []) (Plus [Term 1 []])
      _  -> Div (Plus ts1') (Plus ts2')

mathFold :: ScalarData -> ScalarData
mathFold = mathTermFold . mathSymbolFold

-- x^2 y x -> x^3 y
mathSymbolFold :: ScalarData -> ScalarData
mathSymbolFold (Div (Plus ts1) (Plus ts2)) = Div (Plus (map f ts1)) (Plus (map f ts2))
 where
  f :: TermExpr -> TermExpr
  f (Term a xs) =
    let (sgn, ys) = g xs in Term (sgn * a) ys
  g :: Monomial -> (Integer, Monomial)
  g [] = (1, [])
  g ((x, m):xs) =
    match dfs (x, xs) (Pair SymbolM (Multiset (Pair SymbolM Eql)))
      [ [mc| (quote $s, (negQuote #s, $n) : $xs) ->
               let (sgn, ys) = g ((x, m + n) : xs) in
               if even n then (sgn, ys) else (- sgn, ys) |]
      , [mc| (_, (#x, $n) : $xs) -> g ((x, m + n) : xs) |]
      , [mc| _ -> let (sgn', ys) = g xs in (sgn', (x, m):ys) |]
      ]

-- x^2 y + x^2 y -> 2 x^2 y
mathTermFold :: ScalarData -> ScalarData
mathTermFold (Div (Plus ts1) (Plus ts2)) = Div (Plus (f ts1)) (Plus (f ts2))
 where
  f :: [TermExpr] -> [TermExpr]
  f [] = []
  f (t:ts) =
    match dfs (t, ts) (Pair TermM (Multiset TermM))
      [ [mc| (term $a $xs, term $b ($ys & equalMonomial $sgn #xs) : $tss) ->
               f (Term (sgn * a + b) ys : tss) |]
      , [mc| _ -> t : f ts |]
      ]
