{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

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

import           Prelude                   hiding (div, foldr, mappend, mconcat)
import qualified Prelude                   as Prelude
import           Data.List                 (elemIndex, intercalate)
import           Data.Maybe                (isJust, fromJust)

import           Control.Monad             ( MonadPlus(..) )
import           Control.Egison

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

-- Matchers

data ScalarM = ScalarM
instance Matcher ScalarM ScalarData

div :: Pattern (PP PolyExpr, PP PolyExpr) ScalarM ScalarData (PolyExpr, PolyExpr)
div _ _ (Div p1 p2) = pure (p1, p2)
divM ScalarM _ = (Multiset PolyM, Multiset PolyM)

data PolyM = PolyM
instance Matcher PolyM PolyExpr

plus :: Pattern (PP [TermExpr]) PolyM PolyExpr [TermExpr]
plus _ _ (Plus ts) = pure ts
plusM PolyM _ = Multiset TermM

data TermM = TermM
instance Matcher TermM TermExpr

term :: Pattern (PP Integer, PP Monomial) TermM TermExpr (Integer, Monomial)
term _ _ (Term a mono) = pure (a, mono)
termM TermM _ = (Eql, Multiset (SymbolM, Eql))

data SymbolM = SymbolM
instance Matcher SymbolM SymbolExpr

quote :: Pattern (PP ScalarData) SymbolM SymbolExpr ScalarData
quote _ _ (Quote m) = pure m
quote _ _ _         = mzero
quoteM SymbolM _ = ScalarM

negQuote :: Pattern (PP ScalarData) SymbolM SymbolExpr ScalarData
negQuote _ _ (Quote m) = pure (mathNegate m)
negQuote _ _ _         = mzero
negQuoteM SymbolM _ = ScalarM

instance ValuePattern ScalarM ScalarData where
  value e () ScalarM v = if e == v then pure () else mzero

instance ValuePattern SymbolM SymbolExpr where
  value e () SymbolM v = if e == v then pure () else mzero

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
  Term (sgn * Prelude.div a b) zs
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
    -- TODO(momohatt): Can we write this without isJust and fromJust?
    match dfs (t, ts) (Pair TermM (Multiset TermM))
      [ [mc| (term $a $xs, term $b ($ys & ?(isJust . isEqualMonomial xs)) : $tss) ->
               let sgn = fromJust $ isEqualMonomial xs ys in
               f (Term (sgn * a + b) ys : tss) |]
      , [mc| _ -> t : f ts |]
      ]

  isEqualMonomial :: Monomial -> Monomial -> Maybe Integer
  isEqualMonomial xs ys =
    match dfs (xs, ys) (Pair (Multiset (Pair SymbolM Eql)) (Multiset (Pair SymbolM Eql)))
      [ [mc| ((quote $s, $n) : $xss, (negQuote #s, #n) : $yss) ->
               case isEqualMonomial xss yss of
                 Nothing -> Nothing
                 Just sgn -> return (if even n then sgn else - sgn) |]
      , [mc| (($x, $n) : $xss, (#x, #n) : $yss) -> isEqualMonomial xss yss |]
      , [mc| ([], []) -> return 1 |]
      , [mc| _ -> Nothing |]
      ]

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
