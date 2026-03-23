{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

{- |
Module      : Language.Egison.Primitives.Arith
Licence     : MIT

This module implements arithmetic primitive functions.
-}

module Language.Egison.Primitives.Arith
  ( primitiveArithFunctions
  ) where

import           Data.Ratio                       ((%))
import qualified Data.Sequence                    as Sq

import           Language.Egison.Data
import           Language.Egison.Math
import qualified Language.Egison.Math.CAS         as CAS
import           Language.Egison.Primitives.Utils

primitiveArithFunctions :: [(String, EgisonValue)]
primitiveArithFunctions =
  map (\(name, fn) -> (name, PrimitiveFunc (fn name))) strictPrimitives

strictPrimitives :: [(String, String -> PrimitiveFunc)]
strictPrimitives =
  [ ("i.+", plus)
  , ("i.-", minus)
  , ("i.*", multiply)
  , ("i./", divide)
  , ("f.+", floatBinaryOp (+))
  , ("f.-", floatBinaryOp (-))
  , ("f.*", floatBinaryOp (*))
  , ("f./", floatBinaryOp (/))
  , ("numerator",       numerator')
  , ("denominator",     denominator')
  , ("symbolNormalize", symbolNormalize)

  -- CAS matcher primitives
  , ("casTerms",       casTerms)
  , ("casFromTerms",   casFromTerms')
  , ("termCoeff",      termCoeff)
  , ("termMonomial",   termMonomial)

  , ("i.modulo",   integerBinaryOp mod)
  , ("i.quotient", integerBinaryOp quot)
  , ("i.%",        integerBinaryOp rem)
  , ("i.power",    integerBinaryOp (^))
  , ("i.abs",    integerUnaryOp abs)
  , ("i.neg",    integerUnaryOp negate)
  , ("f.abs",    floatUnaryOp abs)
  , ("f.neg",    floatUnaryOp negate)

  -- Primitive comparison aliases (to avoid type class method conflicts)
  , ("=",  eq)
  , ("i.<",  integerCompare (<))
  , ("i.<=", integerCompare (<=))
  , ("i.>",  integerCompare (>))
  , ("i.>=", integerCompare (>=))
  , ("f.<",  floatCompare (<))
  , ("f.<=", floatCompare (<=))
  , ("f.>",  floatCompare (>))
  , ("f.>=", floatCompare (>=))

  , ("round",    floatToIntegerOp round)
  , ("floor",    floatToIntegerOp floor)
  , ("ceiling",  floatToIntegerOp ceiling)
  , ("truncate", truncate')

  , ("f.sqrt",  floatUnaryOp sqrt)
  , ("f.sqrt'", floatUnaryOp sqrt)
  , ("f.exp",   floatUnaryOp exp)
  , ("f.log",   floatUnaryOp log)
  , ("f.sin",   floatUnaryOp sin)
  , ("f.cos",   floatUnaryOp cos)
  , ("f.tan",   floatUnaryOp tan)
  , ("f.asin",  floatUnaryOp asin)
  , ("f.acos",  floatUnaryOp acos)
  , ("f.atan",  floatUnaryOp atan)
  , ("f.sinh",  floatUnaryOp sinh)
  , ("f.cosh",  floatUnaryOp cosh)
  , ("f.tanh",  floatUnaryOp tanh)
  , ("f.asinh", floatUnaryOp asinh)
  , ("f.acosh", floatUnaryOp acosh)
  , ("f.atanh", floatUnaryOp atanh)
  ]


integerUnaryOp :: (Integer -> Integer) -> String -> PrimitiveFunc
integerUnaryOp = unaryOp

integerBinaryOp :: (Integer -> Integer -> Integer) -> String -> PrimitiveFunc
integerBinaryOp = binaryOp

floatUnaryOp :: (Double -> Double) -> String -> PrimitiveFunc
floatUnaryOp = unaryOp

floatBinaryOp :: (Double -> Double -> Double) -> String -> PrimitiveFunc
floatBinaryOp = binaryOp

floatToIntegerOp :: (Double -> Integer) -> String -> PrimitiveFunc
floatToIntegerOp = unaryOp

--
-- Arith
--

-- | Binary operation on CASValue
casBinaryOp :: (CASValue -> CASValue -> CASValue) -> String -> PrimitiveFunc
casBinaryOp op = twoArgs casBinaryOp'
 where
  casBinaryOp' (CASData c1) (CASData c2) = return $ CASData (op c1 c2)
  casBinaryOp' (CASData _)  val          = throwErrorWithTrace (TypeMismatch "number" (Value val))
  casBinaryOp' val          _            = throwErrorWithTrace (TypeMismatch "number" (Value val))

plus :: String -> PrimitiveFunc
plus = casBinaryOp casPlus

minus :: String -> PrimitiveFunc
minus = casBinaryOp casMinus

multiply :: String -> PrimitiveFunc
multiply = casBinaryOp casMult

divide :: String -> PrimitiveFunc
divide = casBinaryOp casDivide

numerator' :: String -> PrimitiveFunc
numerator' = oneArg numerator''
 where
  numerator'' (CASData c) = return $ CASData (casNumerator c)
  numerator'' val         = throwErrorWithTrace (TypeMismatch "rational" (Value val))

denominator' :: String -> PrimitiveFunc
denominator' = oneArg denominator''
 where
  denominator'' (CASData c) = return $ CASData (casDenominator c)
  denominator'' val         = throwErrorWithTrace (TypeMismatch "rational" (Value val))

symbolNormalize :: String -> PrimitiveFunc
symbolNormalize = oneArg $ \val ->
  case val of
    CASData c -> return $ CASData (casRewriteSymbol c)
    _         -> throwErrorWithTrace (TypeMismatch "math expression" (Value val))

-- CAS matcher primitives: decompose CASValue into terms, coefficients, monomials

-- | Convert CASValue to a list of single-term CASValues
casTerms :: String -> PrimitiveFunc
casTerms = oneArg $ \val ->
  case val of
    CASData cv ->
      let terms = casToTermsCAS cv
          termVals = map (\t -> CASData (termToCASValueCAS t)) terms
      in return $ Collection (Sq.fromList termVals)
    _ -> throwErrorWithTrace (TypeMismatch "math expression" (Value val))

-- | Reconstruct CASValue from a list of single-term CASValues
casFromTerms' :: String -> PrimitiveFunc
casFromTerms' = oneArg $ \val ->
  case val of
    Collection seq ->
      let extractTerm' (CASData (CAS.CASPoly [t])) = t
          extractTerm' (CASData (CAS.CASInteger n)) = CAS.CASTerm (CAS.CASInteger n) []
          extractTerm' (CASData (CAS.CASFactor sym)) = CAS.CASTerm (CAS.CASInteger 1) [(sym, 1)]
          extractTerm' _ = CAS.CASTerm (CAS.CASInteger 0) []
          terms = map extractTerm' (foldr (:) [] seq)
      in return $ CASData (CAS.casNormalizePoly terms)
    _ -> throwErrorWithTrace (TypeMismatch "collection" (Value val))

-- | Extract coefficient from a single-term CASValue
-- For multi-term polynomials or non-term values, returns the value as-is
-- (used in delegation chain where the value may not be a single term)
termCoeff :: String -> PrimitiveFunc
termCoeff = oneArg $ \val ->
  case val of
    CASData cv -> case extractTermCAS cv of
      Just (coeff, _) -> return $ CASData coeff
      Nothing         -> return val
    _ -> return val

-- | Extract monomial from a single-term CASValue as [(Factor, Integer)]
termMonomial :: String -> PrimitiveFunc
termMonomial = oneArg $ \val ->
  case val of
    CASData cv -> case extractTermCAS cv of
      Just (_, mono) ->
        let monoPairs = map (\(sym, expo) ->
              Tuple [CASData (CAS.CASPoly [CAS.CASTerm (CAS.CASInteger 1) [(sym, 1)]]),
                     CASData (CAS.CASInteger (fromIntegral expo))]) mono
        in return $ Collection (Sq.fromList monoPairs)
      Nothing -> throwErrorWithTrace (TypeMismatch "single term" (Value val))
    _ -> throwErrorWithTrace (TypeMismatch "math expression" (Value val))

-- Helper: Convert CASValue to list of CASTerms
casToTermsCAS :: CASValue -> [CAS.CASTerm]
casToTermsCAS (CAS.CASPoly terms) = terms
casToTermsCAS (CAS.CASInteger 0) = []
casToTermsCAS (CAS.CASInteger n) = [CAS.CASTerm (CAS.CASInteger n) []]
casToTermsCAS (CAS.CASFactor sym) = [CAS.CASTerm (CAS.CASInteger 1) [(sym, 1)]]
casToTermsCAS cv = [CAS.CASTerm cv []]

-- Helper: Convert CASTerm to CASValue (single-term polynomial or bare coefficient)
termToCASValueCAS :: CAS.CASTerm -> CASValue
termToCASValueCAS (CAS.CASTerm coeff []) = coeff
termToCASValueCAS t = CAS.CASPoly [t]

-- Helper: Extract coefficient and monomial from a single-term CASValue
extractTermCAS :: CASValue -> Maybe (CASValue, CAS.Monomial)
extractTermCAS (CAS.CASFactor sym) = Just (CAS.CASInteger 1, [(sym, 1)])
extractTermCAS (CAS.CASPoly [CAS.CASTerm coeff mono]) = Just (coeff, mono)
extractTermCAS (CAS.CASInteger n) = Just (CAS.CASInteger n, [])
extractTermCAS _ = Nothing

--
-- Pred
--
eq :: String -> PrimitiveFunc
eq = twoArgs' $ \val val' ->
  return $ Bool $ val == val'

integerCompare :: (forall a. Ord a => a -> a -> Bool) -> String -> PrimitiveFunc
integerCompare cmp = twoArgs' $ \val1 val2 ->
  case (val1, val2) of
    (CASData _, CASData _) -> do
      -- EgisonData instance handles the conversion
      r1 <- fromEgison val1 :: EvalM Rational
      r2 <- fromEgison val2 :: EvalM Rational
      return $ Bool (cmp r1 r2)
    (CASData _, _) -> throwErrorWithTrace (TypeMismatch "integer" (Value val2))
    _              -> throwErrorWithTrace (TypeMismatch "integer" (Value val1))

floatCompare :: (forall a. Ord a => a -> a -> Bool) -> String -> PrimitiveFunc
floatCompare cmp = twoArgs' $ \val1 val2 ->
  case (val1, val2) of
    (Float f1, Float f2) -> return $ Bool (cmp f1 f2)
    (Float _,      _) -> throwErrorWithTrace (TypeMismatch "float" (Value val2))
    _                 -> throwErrorWithTrace (TypeMismatch "float" (Value val1))

truncate' :: String -> PrimitiveFunc
truncate' = oneArg $ \val -> numberUnaryOp' val
 where
  numberUnaryOp' v = case v of
    CASData cv | Just r <- extractRationalCAS cv -> return $ toEgison (truncate r :: Integer)
    Float x -> return $ toEgison (truncate x :: Integer)
    _ -> throwErrorWithTrace (TypeMismatch "rational or float" (Value v))

  -- Extract a Rational from a CASValue if it represents a rational number
  extractRationalCAS :: CASValue -> Maybe Rational
  extractRationalCAS cv = case cv of
    CASInteger n -> Just (n % 1)
    CASPoly [] -> Just 0
    CASPoly [CASTerm coef []] -> extractRationalCAS coef
    CASDiv num den -> do
      n <- extractIntegerCAS num
      d <- extractIntegerCAS den
      if d == 0 then Nothing else Just (n % d)
    _ -> Nothing

  -- Extract an Integer from a CASValue
  extractIntegerCAS :: CASValue -> Maybe Integer
  extractIntegerCAS cv = case cv of
    CASInteger n -> Just n
    CASPoly [] -> Just 0
    CASPoly [CASTerm coef []] -> extractIntegerCAS coef
    _ -> Nothing
