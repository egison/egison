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
    , mathPlus
    , mathMult
    , mathNegate
    , mathNumerator
    , mathDenominator
    ) where

import           Prelude                   hiding (foldr, mappend, mconcat)
import           Data.List                 (intercalate)

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

data TermM = TermM
instance Matcher TermM TermExpr

term :: Pattern (PP Integer, PP Monomial) TermM TermExpr (Integer, Monomial)
term _ _ (Term a mono) = pure (a, mono)
termM :: TermM -> TermExpr -> (Eql, Multiset (Pair SymbolM Eql))
termM TermM _ = (Eql, Multiset (Pair SymbolM Eql))

data SymbolM = SymbolM
instance Matcher SymbolM SymbolExpr

quote :: Pattern (PP ScalarData) SymbolM SymbolExpr ScalarData
quote _ _ (Quote m) = pure m
quote _ _ _         = mzero
quoteM :: SymbolM -> p -> ScalarM
quoteM SymbolM _ = ScalarM

negQuote :: Pattern (PP ScalarData) SymbolM SymbolExpr ScalarData
negQuote _ _ (Quote m) = pure (mathNegate m)
negQuote _ _ _         = mzero
negQuoteM :: SymbolM -> p -> ScalarM
negQuoteM SymbolM _ = ScalarM

equalMonomial :: Pattern (PP Integer, PP Monomial) (Multiset (Pair SymbolM Eql)) Monomial (Integer, Monomial)
equalMonomial (_, VP xs) _ ys = case isEqualMonomial xs ys of
                                  Just sgn -> pure (sgn, xs)
                                  Nothing  -> mzero
equalMonomial _ _ _ = mzero
equalMonomialM :: Multiset (Pair SymbolM Eql) -> p -> (Eql, Multiset (Pair SymbolM Eql))
equalMonomialM (Multiset (Pair SymbolM Eql)) _ = (Eql, Multiset (Pair SymbolM Eql))


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
  Plus xs == Plus ys =
    match dfs ys (Multiset Eql)
      [ [mc| #xs -> True |]
      , [mc| _   -> False |] ]

instance Eq TermExpr where
  Term a xs == Term b ys
    | a == b    = isEqualMonomial xs ys == Just 1
    | a == -b   = isEqualMonomial xs ys == Just (-1)
    | otherwise = False

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
               if even m then (sgn, (x, n - m) : xs') else (- sgn, (x, n - m) : xs') |]
      , [mc| (_, (#y, $n) : $xss) ->
               let (sgn, xs') = divMonomial xss ys in
               (sgn, (y, n - m) : xs') |]
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
