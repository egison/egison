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

import           Control.Monad.Except
import           Language.Egison.Data
import           Language.Egison.EvalState        (MonadEval(..))
import           Language.Egison.Primitives.Utils
import           Language.Egison.Math

primitiveArithFunctions :: [(String, EgisonValue)]
primitiveArithFunctions =
  map (\(name, fn) -> (name, PrimitiveFunc (fn name))) strictPrimitives

strictPrimitives :: [(String, String -> PrimitiveFunc)]
strictPrimitives =
  [ ("b.+", plus)
  , ("b.-", minus)
  , ("b.*", multiply)
  , ("b./", divide)
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

  , ("=",  eq)
  , ("<",  scalarCompare (<))
  , ("<=", scalarCompare (<=))
  , (">",  scalarCompare (>))
  , (">=", scalarCompare (>=))

  , ("round",    floatToIntegerOp round)
  , ("floor",    floatToIntegerOp floor)
  , ("ceiling",  floatToIntegerOp ceiling)
  , ("truncate", truncate')

  , ("b.sqrt",  floatUnaryOp sqrt)
  , ("b.sqrt'", floatUnaryOp sqrt)
  , ("b.exp",   floatUnaryOp exp)
  , ("b.log",   floatUnaryOp log)
  , ("b.sin",   floatUnaryOp sin)
  , ("b.cos",   floatUnaryOp cos)
  , ("b.tan",   floatUnaryOp tan)
  , ("b.asin",  floatUnaryOp asin)
  , ("b.acos",  floatUnaryOp acos)
  , ("b.atan",  floatUnaryOp atan)
  , ("b.sinh",  floatUnaryOp sinh)
  , ("b.cosh",  floatUnaryOp cosh)
  , ("b.tanh",  floatUnaryOp tanh)
  , ("b.asinh", floatUnaryOp asinh)
  , ("b.acosh", floatUnaryOp acosh)
  , ("b.atanh", floatUnaryOp atanh)
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
  numerator'' val = throwErrorWithTrace (TypeMismatch "rational" (Value val))

denominator' :: String -> PrimitiveFunc
denominator' = oneArg denominator''
 where
  denominator'' (ScalarData m) = return $ ScalarData (mathDenominator m)
  denominator'' val = throwErrorWithTrace (TypeMismatch "rational" (Value val))

fromScalarData :: String -> PrimitiveFunc
fromScalarData = oneArg fromScalarData'
 where
  fromScalarData' (ScalarData m) = return $ mathExprToEgison m
  fromScalarData' val = throwErrorWithTrace (TypeMismatch "number" (Value val))

toScalarData :: String -> PrimitiveFunc
toScalarData = oneArg $ \val ->
  ScalarData . mathNormalize' <$> egisonToScalarData val

symbolNormalize :: String -> PrimitiveFunc
symbolNormalize = oneArg $ \val ->
  case val of
    ScalarData s -> return $ ScalarData (rewriteSymbol s)
    _ -> throwErrorWithTrace (TypeMismatch "math expression" (Value val))

--
-- Pred
--
eq :: String -> PrimitiveFunc
eq = twoArgs' $ \val val' ->
  return $ Bool $ val == val'

scalarCompare :: (forall a. Ord a => a -> a -> Bool) -> String -> PrimitiveFunc
scalarCompare cmp = twoArgs' $ \val1 val2 ->
  case (val1, val2) of
    (ScalarData _, ScalarData _) -> do
      r1 <- fromEgison val1 :: EvalM Rational
      r2 <- fromEgison val2 :: EvalM Rational
      return $ Bool (cmp r1 r2)
    (Float f1, Float f2) -> return $ Bool (cmp f1 f2)
    (ScalarData _, _) -> throwErrorWithTrace (TypeMismatch "number" (Value val2))
    (Float _,      _) -> throwErrorWithTrace (TypeMismatch "float"  (Value val2))
    _                 -> throwErrorWithTrace (TypeMismatch "number" (Value val1))

truncate' :: String -> PrimitiveFunc
truncate' = oneArg $ \val -> numberUnaryOp' val
 where
  numberUnaryOp' (ScalarData (Div (Plus []) _)) = return $ toEgison (0 :: Integer)
  numberUnaryOp' (ScalarData (Div (Plus [Term x []]) (Plus [Term y []]))) = return $ toEgison (quot x y)
  numberUnaryOp' (Float x)             = return $ toEgison (truncate x :: Integer)
  numberUnaryOp' val                   = throwErrorWithTrace (TypeMismatch "rational or float" (Value val))
