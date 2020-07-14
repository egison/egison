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
    , Printable (..)
    , pattern ZeroExpr
    , pattern SingleSymbol
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
import           Data.List                 (elemIndex, intercalate)
import           Data.Maybe                (isJust)

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

pattern ZeroExpr :: ScalarData
pattern ZeroExpr = (Div (Plus []) (Plus [Term 1 []]))

pattern SingleSymbol :: SymbolExpr -> ScalarData
pattern SingleSymbol sym = Div (Plus [Term 1 [(sym, 1)]]) (Plus [Term 1 []])

-- Product of a coefficient and a monomial
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

class Printable a where
  isAtom :: a -> Bool
  pretty :: a -> String

pretty' :: Printable a => a -> String
pretty' e | isAtom e = pretty e
pretty' e            = "(" ++ pretty e ++ ")"

instance Printable ScalarData where
  isAtom (Div p (Plus [Term 1 []])) = isAtom p
  isAtom _                          = False

  pretty (Div p1 (Plus [Term 1 []])) = pretty p1
  pretty (Div p1 p2)                 = pretty'' p1 ++ " / " ++ pretty' p2
    where
      pretty'' :: PolyExpr -> String
      pretty'' p@(Plus [_]) = pretty p
      pretty'' p            = "(" ++ pretty p ++ ")"

instance Printable PolyExpr where
  isAtom (Plus [])           = True
  isAtom (Plus [Term _ []])  = True
  isAtom (Plus [Term 1 [_]]) = True
  isAtom _                   = False

  pretty (Plus []) = "0"
  pretty (Plus (t:ts)) = pretty t ++ concatMap withSign ts
    where
      withSign (Term a xs) | a < 0 = " - " ++ pretty (Term (- a) xs)
      withSign t                   = " + " ++ pretty t

instance Printable SymbolExpr where
  isAtom Symbol{}     = True
  isAtom (Apply _ []) = True
  isAtom _            = False

  pretty (Symbol _ (':':':':':':_) []) = "#"
  pretty (Symbol _ s []) = s
  pretty (Symbol _ s js) = s ++ concatMap show js
  pretty (Apply fn mExprs) = unwords (map pretty' (fn : mExprs))
  pretty (Quote mExprs) = "'" ++ pretty' mExprs
  pretty (FunctionData name _ _ js) = pretty name ++ concatMap show js

instance Printable TermExpr where
  isAtom (Term _ [])  = True
  isAtom (Term 1 [_]) = True
  isAtom _            = False

  pretty (Term a []) = show a
  pretty (Term 1 xs) = intercalate " * " (map prettyPoweredSymbol xs)
  pretty (Term (-1) xs) = "- " ++ intercalate " * " (map prettyPoweredSymbol xs)
  pretty (Term a xs) = intercalate " * " (show a : map prettyPoweredSymbol xs)

prettyPoweredSymbol :: (SymbolExpr, Integer) -> String
prettyPoweredSymbol (x, 1) = show x
prettyPoweredSymbol (x, n) = pretty' x ++ "^" ++ show n

instance Show ScalarData where
  show = pretty

instance Show PolyExpr where
  show = pretty

instance Show TermExpr where
  show = pretty

instance Show SymbolExpr where
  show = pretty

instance Show (Index ScalarData) where
  show (Superscript i)  = "~" ++ pretty' i
  show (Subscript i)    = "_" ++ pretty' i
  show (SupSubscript i) = "~_" ++ pretty' i
  show (DFscript _ _)   = ""
  show (Userscript i)   = "|" ++ pretty' i

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
  f :: Integer -> Monomial -> Monomial -> (Integer, Monomial)
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
mathFold = mathTermFold . mathSymbolFold

-- x^2 y x -> x^3 y
mathSymbolFold :: ScalarData -> ScalarData
mathSymbolFold (Div (Plus ts1) (Plus ts2)) = Div (Plus (map f ts1)) (Plus (map f ts2))
 where
  f :: TermExpr -> TermExpr
  f (Term a xs) = let (ys, sgns) = unzip $ g [] xs
                   in Term (product sgns * a) ys
  g :: [((SymbolExpr, Integer),Integer)] -> Monomial -> [((SymbolExpr, Integer),Integer)]
  g ret [] = ret
  g ret ((x, n):xs)
    | any (p x) ret = g (map (h (x, n)) ret) xs
    | otherwise     = g (ret ++ [((x, n), 1)]) xs
  p :: SymbolExpr -> ((SymbolExpr, Integer), Integer) -> Bool
  p (Quote x) ((Quote y, _),_) = x == y || mathNegate x == y
  p x         ((y, _),_)       = x == y
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
    if any (\(Term _ ys) -> isJust (p 1 xs ys)) ret
      then f' (map (g (Term a xs)) ret) ts
      else f' (ret ++ [Term a xs]) ts
  g :: TermExpr -> TermExpr -> TermExpr
  g (Term a xs) (Term b ys) =
    case p 1 xs ys of
      Just sgn -> Term ((sgn * a) + b) ys
      Nothing  -> Term b ys
  p :: Integer -> Monomial -> Monomial -> Maybe Integer
  p sgn [] [] = return sgn
  p _   [] _  = Nothing
  p sgn ((x, n):xs) ys =
    case q (x, n) [] ys of
      Just (ys', sgn2) -> p (sgn * sgn2) xs ys'
      Nothing          -> Nothing
  q :: (SymbolExpr, Integer) -> Monomial -> Monomial -> Maybe (Monomial, Integer)
  q _ _ [] = Nothing
  q (Quote x, n) ret ((Quote y, m):ys)
    | x == y && n == m            = return (ret ++ ys, 1)
    | mathNegate x == y && n == m = return $ if even n then (ret ++ ys, 1) else (ret ++ ys, -1)
    | otherwise                   = q (Quote x, n) (ret ++ [(Quote y, m)]) ys
  q (Quote x, n) ret ((y,m):ys) = q (Quote x, n) (ret ++ [(y, m)]) ys
  q (x, n) ret ((y, m):ys) =
    if x == y && n == m then return (ret ++ ys, 1)
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
