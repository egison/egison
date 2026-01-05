{- |
Module      : Language.Egison.Primitives.Types
Licence     : MIT

This module implements primitive functions that dynamically checks the types of
objects.
-}

module Language.Egison.Primitives.Types
  ( primitiveTypeFunctions
  ) where

import           Data.Char                        (chr, ord)
import           Data.Ratio                       ((%))

import           Language.Egison.Data
import           Language.Egison.Math
import           Language.Egison.Primitives.Utils

primitiveTypeFunctions :: [(String, EgisonValue)]
primitiveTypeFunctions =
  map (\(name, fn) -> (name, PrimitiveFunc (fn name))) strictPrimitives ++
    map (\(name, fn) -> (name, LazyPrimitiveFunc (fn name))) lazyPrimitives

strictPrimitives :: [(String, String -> PrimitiveFunc)]
strictPrimitives =
  [ ("itof", integerToFloat)
  , ("rtof", rationalToFloat)
  , ("ctoi", charToInteger)
  , ("itoc", integerToChar)
  ]

lazyPrimitives :: [(String, String -> LazyPrimitiveFunc)]
lazyPrimitives =
  [ ("isInteger",    lazyOneArg isInteger)
  , ("isRational",   lazyOneArg isRational)
  -- Note: Other type checking functions (isBool, isScalar, isFloat, isChar, isString,
  -- isCollection, isHash, isTensor, typeName) are removed because they are not needed
  -- with the static type system. isInteger and isRational are kept because
  -- MathExpr = Integer = Rational in Egison.
  ]

--
-- Typing
-- Note: Only isInteger and isRational are kept because MathExpr = Integer = Rational in Egison.
-- Other type checking functions are removed as they are not needed with the static type system.
--

isInteger :: WHNFData -> EvalM WHNFData
isInteger (Value (ScalarData (Div (Plus []) (Plus [Term 1 []]))))          = return . Value $ Bool True
isInteger (Value (ScalarData (Div (Plus [Term _ []]) (Plus [Term 1 []])))) = return . Value $ Bool True
isInteger _                                                                = return . Value $ Bool False

isRational :: WHNFData -> EvalM WHNFData
isRational (Value (ScalarData (Div (Plus []) (Plus [Term _ []]))))          = return . Value $ Bool True
isRational (Value (ScalarData (Div (Plus [Term _ []]) (Plus [Term _ []])))) = return . Value $ Bool True
isRational _                                                                = return . Value $ Bool False

--
-- Transform
--
integerToFloat :: String -> PrimitiveFunc
integerToFloat = rationalToFloat

rationalToFloat :: String -> PrimitiveFunc
rationalToFloat = oneArg $ \val ->
  case val of
    ScalarData (Div (Plus []) _)                           -> return $ Float 0
    ScalarData (Div (Plus [Term x []]) (Plus [Term y []])) -> return $ Float (fromRational (x % y))
    _                                                      -> throwErrorWithTrace (TypeMismatch "integer or rational number" (Value val))

charToInteger :: String -> PrimitiveFunc
charToInteger = unaryOp ctoi
  where
    ctoi :: Char -> Integer
    ctoi = fromIntegral . ord

integerToChar :: String -> PrimitiveFunc
integerToChar = unaryOp itoc
  where
    itoc :: Integer -> Char
    itoc = chr . fromIntegral

