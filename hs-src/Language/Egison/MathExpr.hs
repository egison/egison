{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms   #-}

{- |
Module      : Language.Egison.MathExpr
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
    , pattern SingleTerm
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
import           Data.List                 (any, elemIndex, intercalate, splitAt)

import           Language.Egison.AST

--
-- Data
--


data ScalarData
  = Div PolyExpr PolyExpr
 deriving (Eq)

newtype PolyExpr
  = Plus [TermExpr]

data TermExpr
  = Term Integer Monomial

-- We choose the definition 'monomials' without its coefficients.
-- ex. 2 x^2 y^3 is *not* a monomial. x^2 t^3 is a monomial.
type Monomial = [(SymbolExpr, Integer)]

data SymbolExpr
  = Symbol Id String [Index ScalarData]
  | Apply ScalarData [ScalarData]
  | Quote ScalarData
  | FunctionData ScalarData [ScalarData] [ScalarData] [Index ScalarData] -- fnname argnames args indices
 deriving (Eq)

type Id = String

pattern SingleTerm :: Integer -> Monomial -> ScalarData
pattern SingleTerm coeff mono = Div (Plus [Term coeff mono]) (Plus [Term 1 []])

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
    | (a /= b) && (a /= -b) = False
    | otherwise = case elemIndex (Quote x, n) ys of
                    Just i -> let (hs, _:ts) = splitAt i ys in
                                Term a xs == Term b (hs ++ ts)
                    Nothing -> case elemIndex (Quote (mathNegate x), n) ys of
                                 Just i -> let (hs, _:ts) = splitAt i ys in
                                             if even n
                                               then Term a xs == Term b (hs ++ ts)
                                               else Term (-a) xs == Term b (hs ++ ts)
                                 Nothing -> False
  (Term a (x:xs)) == (Term b ys)
    | (a /= b) && (a /= -b) = False
    | otherwise = case elemIndex x ys of
                    Just i -> let (hs, _:ts) = splitAt i ys in
                                Term a xs == Term b (hs ++ ts)
                    Nothing -> False
  _ == _ = False

instance Show ScalarData where
  show (Div p1 (Plus [Term 1 []])) = show p1
  show (Div p1 p2)                 = show' p1 ++ " / " ++ show' p2
    where
      show' :: PolyExpr -> String
      show' p@(Plus [_]) = show p
      show' p            = "(" ++ show p ++ ")"

instance Show PolyExpr where
  show (Plus [])  = "0"
  show (Plus ts)  = intercalate " + " (map show ts)

instance Show TermExpr where
  show (Term a []) = show a
  show (Term 1 xs) = intercalate " * " (map showPoweredSymbol xs)
  show (Term a xs) = intercalate " * " (show a : map showPoweredSymbol xs)

showPoweredSymbol :: (SymbolExpr, Integer) -> String
showPoweredSymbol (x, 1) = show x
showPoweredSymbol (x, n) = show x ++ "^" ++ show n

instance Show SymbolExpr where
  show (Symbol _ (':':':':':':_) []) = "#"
  show (Symbol _ s []) = s
  show (Symbol _ s js) = s ++ concatMap show js
  show (Apply fn mExprs) = "(" ++ show fn ++ " " ++ unwords (map show mExprs) ++ ")"
  show (Quote mExprs) = "'(" ++ show mExprs ++ ")"
  show (FunctionData name _ _ js) = show name ++ concatMap show js

instance Show (Index ScalarData) where
  show (Superscript i)  = "~" ++ show i
  show (Subscript i)    = "_" ++ show i
  show (SupSubscript i) = "~_" ++ show i
  show (DFscript _ _)   = ""
  show (Userscript i)   = "|" ++ show i

--
-- Scalars
--

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
    | x == y            = (1, (Quote x, n - m))
    | x == mathNegate y = if even m then (1, (Quote x, n - m)) else (-1, (Quote x, n - m))
    | otherwise         = (1, (Quote x, n))
  g (x, n) (y, m)
    | x == y    = (1, (x, n - m))
    | otherwise = (1, (x, n))

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
mathFold = mathTermFold . mathSymbolFold . mathTermFold

-- x^2 y x -> x^3 y
mathSymbolFold :: ScalarData -> ScalarData
mathSymbolFold (Div (Plus ts1) (Plus ts2)) = Div (Plus (map f ts1)) (Plus (map f ts2))
 where
  f :: TermExpr -> TermExpr
  f (Term a xs) = let (ys, sgns) = unzip $ g [] xs
                   in Term (product sgns * a) ys
  g :: [((SymbolExpr, Integer),Integer)] -> [(SymbolExpr, Integer)] -> [((SymbolExpr, Integer),Integer)]
  g ret [] = ret
  g ret ((x, n):xs)
    | any (p (x, n)) ret = g (map (h (x, n)) ret) xs
    | otherwise          = g (ret ++ [((x, n), 1)]) xs
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
  p _   [] _  = (False, 0)
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
mathNegate' (Plus ts) = Plus (map (\(Term a xs) -> Term (-a) xs) ts)

mathNumerator :: ScalarData -> ScalarData
mathNumerator (Div m _) = Div m (Plus [Term 1 []])

mathDenominator :: ScalarData -> ScalarData
mathDenominator (Div _ n) = Div n (Plus [Term 1 []])
