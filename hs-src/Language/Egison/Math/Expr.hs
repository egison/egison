{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuasiQuotes           #-}

{- |
Module      : Language.Egison.MathExpr
Licence     : MIT

This module contains functions for mathematical expressions.
-}

module Language.Egison.Math.Expr
    ( ScalarData (..)
    , PolyExpr (..)
    , TermExpr (..)
    , Monomial
    , SymbolExpr (..)
    , Printable (..)
    , pattern ZeroExpr
    , pattern SingleSymbol
    , pattern SingleTerm
    , ScalarM (..)
    , TermM (..)
    , SymbolM (..)
    , term
    , termM
    , quote
    , negQuote
    , negQuoteM
    , equalMonomial
    , equalMonomialM
    , mathNegate
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

data SymbolM = SymbolM
instance Matcher SymbolM SymbolExpr

term :: Pattern (PP Integer, PP Monomial) TermM TermExpr (Integer, Monomial)
term _ _ (Term a mono) = pure (a, mono)
termM :: TermM -> TermExpr -> (Eql, Multiset (Pair SymbolM Eql))
termM TermM _ = (Eql, Multiset (Pair SymbolM Eql))

quote :: Pattern (PP ScalarData) SymbolM SymbolExpr ScalarData
quote _ _ (Quote m) = pure m
quote _ _ _         = mzero

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

mathNegate :: ScalarData -> ScalarData
mathNegate (Div m n) = Div (mathNegate' m) n
  where
    mathNegate' (Plus ts) = Plus (map (\(Term a xs) -> Term (-a) xs) ts)

--
-- Pretty printing
--

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
