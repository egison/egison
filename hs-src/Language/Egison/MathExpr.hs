{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

{- |
Module      : Language.Egison.MathExpr
Copyright   : Satoshi Egi
Licence     : MIT

This module contains functions for mathematical expressions.
-}

module Language.Egison.MathExpr
    (
    -- * MathExpr Data
      ScalarData (..)
    , PolyExpr (..)
    , TermExpr (..)
    , SymbolExpr (..)
    -- * Scalar
    , mathNormalize'
    , mathFold
    , mathSymbolFold
    , mathTermFold
    , mathRemoveZero
    , mathDivide
    , mathPlus
    , mathMult
    , mathNegate
    , mathNumerator
    , mathDenominator
    ) where

import           Prelude                   hiding (foldr, mappend, mconcat)
import           Data.List                 (any, elemIndex, splitAt)
import           Language.Egison.AST

--
-- Data
--


data ScalarData =
    Div PolyExpr PolyExpr
 deriving (Eq)

newtype PolyExpr =
    Plus [TermExpr]

data TermExpr =
    Term Integer [(SymbolExpr, Integer)]

data SymbolExpr =
    Symbol Id String [Index ScalarData]
  | Apply ScalarData [ScalarData]
  | Quote ScalarData
  | FunctionData ScalarData [ScalarData] [ScalarData] [Index ScalarData] -- fnname argnames args indices
 deriving (Eq)

type Id = String

instance Eq PolyExpr where
  (Plus []) == (Plus []) = True
  (Plus (x:xs)) == (Plus ys) =
    case elemIndex x ys of
      Just i -> let (hs, _:ts) = splitAt i ys in
                  Plus xs == Plus (hs ++ ts)
      Nothing -> False
  _ == _ = False

instance Eq TermExpr where
  (Term a []) == (Term b []) = a == b
  (Term a ((Quote x, n):xs)) == (Term b ys)
    | (a /= b) && (a /= negate b) = False
    | otherwise = case elemIndex (Quote x, n) ys of
                    Just i -> let (hs, _:ts) = splitAt i ys in
                                Term a xs == Term b (hs ++ ts)
                    Nothing -> case elemIndex (Quote (mathNegate x), n) ys of
                                 Just i -> let (hs, _:ts) = splitAt i ys in
                                             if even n
                                               then Term a xs == Term b (hs ++ ts)
                                               else Term (negate a) xs == Term b (hs ++ ts)
                                 Nothing -> False
  (Term a (x:xs)) == (Term b ys)
    | (a /= b) && (a /= negate b) = False
    | otherwise = case elemIndex x ys of
                    Just i -> let (hs, _:ts) = splitAt i ys in
                                Term a xs == Term b (hs ++ ts)
                    Nothing -> False
  _ == _ = False

--
-- Scalars
--

mathNormalize' :: ScalarData -> ScalarData
mathNormalize' mExpr = mathDivide (mathRemoveZero (mathFold (mathRemoveZeroSymbol mExpr)))

termsGcd :: [TermExpr] -> TermExpr
termsGcd (t:ts) = f t ts
 where
  f :: TermExpr -> [TermExpr] -> TermExpr
  f ret [] =  ret
  f (Term a xs) (Term b ys:ts) =
    f (Term (gcd a b) (g xs ys)) ts
  g :: [(SymbolExpr, Integer)] -> [(SymbolExpr, Integer)] -> [(SymbolExpr, Integer)]
  g [] ys = []
  g ((x, n):xs) ys = let (z, m) = h (x, n) ys in
    if m == 0 then g xs ys else (z, m):g xs ys
  h :: (SymbolExpr, Integer) -> [(SymbolExpr, Integer)] -> (SymbolExpr, Integer)
  h (x, n) [] = (x, 0)
  h (Quote x, n) ((Quote y, m):ys)
    | x == y = (Quote x, min n m)
    | x == mathNegate y = (Quote x, min n m)
    | otherwise = h (Quote x, n) ys
  h (x, n) ((y, m):ys) = if x == y
                         then (x, min n m)
                         else h (x, n) ys

mathDivide :: ScalarData -> ScalarData
mathDivide (Div (Plus ts1) (Plus [])) = Div (Plus ts1) (Plus [])
mathDivide (Div (Plus []) (Plus ts2)) = Div (Plus []) (Plus ts2)
mathDivide (Div (Plus ts1) (Plus ts2)) =
  let z = termsGcd (ts1 ++ ts2) in
  case z of
    (Term c zs) -> case ts2 of
      [Term a _] -> if a < 0
                      then Div (Plus (map (`mathDivideTerm` Term (-1 * c) zs) ts1)) (Plus (map (`mathDivideTerm` Term (-1 * c) zs) ts2))
                      else Div (Plus (map (`mathDivideTerm` z) ts1)) (Plus (map (`mathDivideTerm` z) ts2))
      _ -> Div (Plus (map (`mathDivideTerm` z) ts1)) (Plus (map (`mathDivideTerm` z) ts2))

mathDivideTerm :: TermExpr -> TermExpr -> TermExpr
mathDivideTerm (Term a xs) (Term b ys) =
  let (sgn, zs) = f 1 xs ys in
  Term (sgn * div a b) zs
 where
  f :: Integer -> [(SymbolExpr, Integer)] -> [(SymbolExpr, Integer)] -> (Integer, [(SymbolExpr, Integer)])
  f sgn xs [] = (sgn, xs)
  f sgn xs ((y, n):ys) =
    let (sgns, zs) = unzip (map (\(x, m) -> g (x, m) (y, n)) xs) in
    f (sgn * product sgns) zs ys
  g :: (SymbolExpr, Integer) -> (SymbolExpr, Integer) -> (Integer, (SymbolExpr, Integer))
  g (Quote x, n) (Quote y, m)
    | x == y = (1, (Quote x, n - m))
    | x == mathNegate y = if even m then (1, (Quote x, n - m)) else (-1, (Quote x, n - m))
    | otherwise = (1, (Quote x, n))
  g (x, n) (y, m) =
    if x == y
    then (1, (x, n - m))
    else (1, (x, n))

mathRemoveZeroSymbol :: ScalarData -> ScalarData
mathRemoveZeroSymbol (Div (Plus ts1) (Plus ts2)) =
  let p x = case x of
              (_, 0) -> False
              _      -> True in
  let ts1' = map (\(Term a xs) -> Term a (filter p xs)) ts1 in
  let ts2' = map (\(Term a xs) -> Term a (filter p xs)) ts2 in
    Div (Plus ts1') (Plus ts2')

mathRemoveZero :: ScalarData -> ScalarData
mathRemoveZero (Div (Plus ts1) (Plus ts2)) =
  let ts1' = filter (\(Term a _) -> a /= 0) ts1 in
  let ts2' = filter (\(Term a _) -> a /= 0) ts2 in
    case ts1' of
      [] -> Div (Plus []) (Plus [Term 1 []])
      _  -> Div (Plus ts1') (Plus ts2')

mathFold :: ScalarData -> ScalarData
mathFold mExpr = mathTermFold (mathSymbolFold (mathTermFold mExpr))

-- x^2 y x -> x^3 y
mathSymbolFold :: ScalarData -> ScalarData
mathSymbolFold (Div (Plus ts1) (Plus ts2)) = Div (Plus (map f ts1)) (Plus (map f ts2))
 where
  f :: TermExpr -> TermExpr
  f (Term a xs) = let (ys, sgns) = unzip $ g [] xs
                    in Term (product sgns * a) ys
  g :: [((SymbolExpr, Integer),Integer)] -> [(SymbolExpr, Integer)] -> [((SymbolExpr, Integer),Integer)]
  g ret [] = ret
  g ret ((x, n):xs) =
    if any (p (x, n)) ret
      then g (map (h (x, n)) ret) xs
      else g (ret ++ [((x, n), 1)]) xs
  p :: (SymbolExpr, Integer) -> ((SymbolExpr, Integer), Integer) -> Bool
  p (Quote x, _) ((Quote y, _),_) = (x == y) || (mathNegate x == y)
  p (x, _) ((y, _),_)             = x == y
  h :: (SymbolExpr, Integer) -> ((SymbolExpr, Integer), Integer) -> ((SymbolExpr, Integer), Integer)
  h (Quote x, n) ((Quote y, m), sgn)
    | x == y = ((Quote y, m + n), sgn)
    | x == mathNegate y = if even n then ((Quote y, m + n), sgn) else ((Quote y, m + n), -1 * sgn)
    | otherwise = ((Quote y, m), sgn)
  h (x, n) ((y, m), sgn) = if x == y
                             then ((y, m + n), sgn)
                             else ((y, m), sgn)

-- x^2 y + x^2 y -> 2 x^2 y
mathTermFold :: ScalarData -> ScalarData
mathTermFold (Div (Plus ts1) (Plus ts2)) = Div (Plus (f ts1)) (Plus (f ts2))
 where
  f :: [TermExpr] -> [TermExpr]
  f = f' []
  f' :: [TermExpr] -> [TermExpr] -> [TermExpr]
  f' ret [] = ret
  f' ret (Term a xs:ts) =
    if any (\(Term _ ys) -> fst (p 1 xs ys)) ret
      then f' (map (g (Term a xs)) ret) ts
      else f' (ret ++ [Term a xs]) ts
  g :: TermExpr -> TermExpr -> TermExpr
  g (Term a xs) (Term b ys) = let (c, sgn) = p 1 xs ys in
                                if c
                                  then Term ((sgn * a) + b) ys
                                  else Term b ys
  p :: Integer -> [(SymbolExpr, Integer)] -> [(SymbolExpr, Integer)] -> (Bool, Integer)
  p sgn [] [] = (True, sgn)
  p sgn [] _ = (False, 0)
  p sgn ((x, n):xs) ys =
    let (b, ys', sgn2) = q (x, n) [] ys in
      if b
        then p (sgn * sgn2) xs ys'
        else (False, 0)
  q :: (SymbolExpr, Integer) -> [(SymbolExpr, Integer)] -> [(SymbolExpr, Integer)] -> (Bool, [(SymbolExpr, Integer)], Integer)
  q _ _ [] = (False, [], 1)
  q (Quote x, n) ret ((Quote y, m):ys)
    | (x == y) && (n == m) = (True, ret ++ ys, 1)
    | (mathNegate x == y) && (n == m) = if even n then (True, ret ++ ys, 1) else (True, ret ++ ys, -1)
    | otherwise = q (Quote x, n) (ret ++ [(Quote y, m)]) ys
  q (Quote x, n) ret ((y,m):ys) = q (Quote x, n) (ret ++ [(y, m)]) ys
  q (x, n) ret ((y, m):ys) = if (x == y) && (n == m)
                               then (True, ret ++ ys, 1)
                               else q (x, n) (ret ++ [(y, m)]) ys

--
--  Arithmetic operations
--

mathPlus :: ScalarData -> ScalarData -> ScalarData
mathPlus (Div m1 n1) (Div m2 n2) = mathNormalize' $ Div (mathPlusPoly (mathMultPoly m1 n2) (mathMultPoly m2 n1)) (mathMultPoly n1 n2)

mathPlusPoly :: PolyExpr -> PolyExpr -> PolyExpr
mathPlusPoly (Plus ts1) (Plus ts2) = Plus (ts1 ++ ts2)

mathMult :: ScalarData -> ScalarData -> ScalarData
mathMult (Div m1 n1) (Div m2 n2) = mathNormalize' $ Div (mathMultPoly m1 m2) (mathMultPoly n1 n2)

mathMultPoly :: PolyExpr -> PolyExpr -> PolyExpr
mathMultPoly (Plus []) (Plus _) = Plus []
mathMultPoly (Plus _) (Plus []) = Plus []
mathMultPoly (Plus ts1) (Plus ts2) = foldl mathPlusPoly (Plus []) (map (\(Term a xs) -> Plus (map (\(Term b ys) -> Term (a * b) (xs ++ ys)) ts2)) ts1)

mathNegate :: ScalarData -> ScalarData
mathNegate (Div m n) = Div (mathNegate' m) n

mathNegate' :: PolyExpr -> PolyExpr
mathNegate' (Plus ts) = Plus (map (\(Term a xs) -> Term (negate a) xs) ts)

mathNumerator :: ScalarData -> ScalarData
mathNumerator (Div m _) = Div m (Plus [Term 1 []])

mathDenominator :: ScalarData -> ScalarData
mathDenominator (Div _ n) = Div n (Plus [Term 1 []])
