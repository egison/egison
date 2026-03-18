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

import           Language.Egison.Data
import           Language.Egison.Math
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
  , ("fromMathExpr",    fromScalarData)
  , ("toMathExpr'",     toScalarData)
  , ("symbolNormalize", symbolNormalize)

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

-- | Binary operation on CASValue (direct, no ScalarData conversion)
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

fromScalarData :: String -> PrimitiveFunc
fromScalarData = oneArg fromScalarData'
 where
  fromScalarData' (CASData c) = return $ mathExprToEgison (casValueToScalarData c)
  fromScalarData' val         = throwErrorWithTrace (TypeMismatch "number" (Value val))

toScalarData :: String -> PrimitiveFunc
toScalarData = oneArg $ \val -> do
  s <- egisonToScalarData val
  return $ CASData (casNormalize' (scalarDataToCASValue s))

symbolNormalize :: String -> PrimitiveFunc
symbolNormalize = oneArg $ \val ->
  case val of
    CASData c -> return $ CASData (casRewriteSymbol c)
    _         -> throwErrorWithTrace (TypeMismatch "math expression" (Value val))

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
      -- ScalarData pattern synonym handles the conversion
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
  numberUnaryOp' v = case fromScalarVal v of
    Just (Div (Plus []) _)                           -> return $ toEgison (0 :: Integer)
    Just (Div (Plus [Term x []]) (Plus [Term y []])) -> return $ toEgison (quot x y)
    Nothing -> case v of
      Float x -> return $ toEgison (truncate x :: Integer)
      _       -> throwErrorWithTrace (TypeMismatch "rational or float" (Value v))
