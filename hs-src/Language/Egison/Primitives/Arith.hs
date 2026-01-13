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

  , ("modulo",   integerBinaryOp mod)
  , ("quotient", integerBinaryOp quot)
  , ("%",        integerBinaryOp rem)
  , ("b.abs",    rationalUnaryOp abs)
  , ("b.neg",    rationalUnaryOp negate)

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


rationalUnaryOp :: (Rational -> Rational) -> String -> PrimitiveFunc
rationalUnaryOp = unaryOp

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
scalarBinaryOp :: (ScalarData -> ScalarData -> ScalarData) -> String -> PrimitiveFunc
scalarBinaryOp mOp = twoArgs scalarBinaryOp'
 where
  scalarBinaryOp' (ScalarData m1) (ScalarData m2) = (return . ScalarData) (mOp m1 m2)
  scalarBinaryOp' (ScalarData _)  val             = throwErrorWithTrace (TypeMismatch "number" (Value val))
  scalarBinaryOp' val             _               = throwErrorWithTrace (TypeMismatch "number" (Value val))

plus :: String -> PrimitiveFunc
plus = scalarBinaryOp mathPlus

minus :: String -> PrimitiveFunc
minus = scalarBinaryOp (\m1 m2 -> mathPlus m1 (mathNegate m2))

multiply :: String -> PrimitiveFunc
multiply = scalarBinaryOp mathMult

divide :: String -> PrimitiveFunc
divide = scalarBinaryOp mathDiv

numerator' :: String -> PrimitiveFunc
numerator' = oneArg numerator''
 where
  numerator'' (ScalarData m) = return $ ScalarData (mathNumerator m)
  numerator'' val            = throwErrorWithTrace (TypeMismatch "rational" (Value val))

denominator' :: String -> PrimitiveFunc
denominator' = oneArg denominator''
 where
  denominator'' (ScalarData m) = return $ ScalarData (mathDenominator m)
  denominator'' val            = throwErrorWithTrace (TypeMismatch "rational" (Value val))

fromScalarData :: String -> PrimitiveFunc
fromScalarData = oneArg fromScalarData'
 where
  fromScalarData' (ScalarData m) = return $ mathExprToEgison m
  fromScalarData' val            = throwErrorWithTrace (TypeMismatch "number" (Value val))

toScalarData :: String -> PrimitiveFunc
toScalarData = oneArg $ \val ->
  ScalarData . mathNormalize' <$> egisonToScalarData val

symbolNormalize :: String -> PrimitiveFunc
symbolNormalize = oneArg $ \val ->
  case val of
    ScalarData s -> return $ ScalarData (rewriteSymbol s)
    _            -> throwErrorWithTrace (TypeMismatch "math expression" (Value val))

--
-- Pred
--
eq :: String -> PrimitiveFunc
eq = twoArgs' $ \val val' ->
  return $ Bool $ val == val'

integerCompare :: (forall a. Ord a => a -> a -> Bool) -> String -> PrimitiveFunc
integerCompare cmp = twoArgs' $ \val1 val2 ->
  case (val1, val2) of
    (ScalarData _, ScalarData _) -> do
      r1 <- fromEgison val1 :: EvalM Rational
      r2 <- fromEgison val2 :: EvalM Rational
      return $ Bool (cmp r1 r2)
    (ScalarData _, _) -> throwErrorWithTrace (TypeMismatch "integer" (Value val2))
    _                 -> throwErrorWithTrace (TypeMismatch "integer" (Value val1))

floatCompare :: (forall a. Ord a => a -> a -> Bool) -> String -> PrimitiveFunc
floatCompare cmp = twoArgs' $ \val1 val2 ->
  case (val1, val2) of
    (Float f1, Float f2) -> return $ Bool (cmp f1 f2)
    (Float _,      _) -> throwErrorWithTrace (TypeMismatch "float" (Value val2))
    _                 -> throwErrorWithTrace (TypeMismatch "float" (Value val1))

truncate' :: String -> PrimitiveFunc
truncate' = oneArg $ \val -> numberUnaryOp' val
 where
  numberUnaryOp' (ScalarData (Div (Plus []) _))                           = return $ toEgison (0 :: Integer)
  numberUnaryOp' (ScalarData (Div (Plus [Term x []]) (Plus [Term y []]))) = return $ toEgison (quot x y)
  numberUnaryOp' (Float x)                                                = return $ toEgison (truncate x :: Integer)
  numberUnaryOp' val                                                      = throwErrorWithTrace (TypeMismatch "rational or float" (Value val))
