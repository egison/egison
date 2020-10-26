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
  [ ("isBool",       lazyOneArg isBool)
  , ("isInteger",    lazyOneArg isInteger)
  , ("isRational",   lazyOneArg isRational)
  , ("isScalar",     lazyOneArg isScalar)
  , ("isFloat",      lazyOneArg isFloat)
  , ("isChar",       lazyOneArg isChar)
  , ("isString",     lazyOneArg isString)
  , ("isCollection", lazyOneArg isCollection)
  , ("isHash",       lazyOneArg isHash)
  , ("isTensor",     lazyOneArg isTensor)
  ]

--
-- Typing
--

isBool :: WHNFData -> EvalM WHNFData
isBool (Value (Bool _)) = return . Value $ Bool True
isBool _                = return . Value $ Bool False

isInteger :: WHNFData -> EvalM WHNFData
isInteger (Value (ScalarData (Div (Plus []) (Plus [Term 1 []]))))          = return . Value $ Bool True
isInteger (Value (ScalarData (Div (Plus [Term _ []]) (Plus [Term 1 []])))) = return . Value $ Bool True
isInteger _                                                                = return . Value $ Bool False

isRational :: WHNFData -> EvalM WHNFData
isRational (Value (ScalarData (Div (Plus []) (Plus [Term _ []]))))          = return . Value $ Bool True
isRational (Value (ScalarData (Div (Plus [Term _ []]) (Plus [Term _ []])))) = return . Value $ Bool True
isRational _                                                                = return . Value $ Bool False

isScalar :: WHNFData -> EvalM WHNFData
isScalar (Value (ScalarData _)) = return . Value $ Bool True
isScalar _                      = return . Value $ Bool False

isTensor :: WHNFData -> EvalM WHNFData
isTensor (Value (TensorData _)) = return . Value $ Bool True
isTensor (ITensor _)            = return . Value $ Bool True
isTensor _                      = return . Value $ Bool False

isFloat :: WHNFData -> EvalM WHNFData
isFloat (Value (Float _)) = return . Value $ Bool True
isFloat _                 = return . Value $ Bool False

isChar :: WHNFData -> EvalM WHNFData
isChar (Value (Char _)) = return . Value $ Bool True
isChar _                = return . Value $ Bool False

isString :: WHNFData -> EvalM WHNFData
isString (Value (String _)) = return . Value $ Bool True
isString _                  = return . Value $ Bool False

isCollection :: WHNFData -> EvalM WHNFData
isCollection (Value (Collection _)) = return . Value $ Bool True
isCollection (ICollection _)        = return . Value $ Bool True
isCollection _                      = return . Value $ Bool False

isHash :: WHNFData -> EvalM WHNFData
isHash (Value (IntHash _))  = return . Value $ Bool True
isHash (Value (CharHash _)) = return . Value $ Bool True
isHash (Value (StrHash _))  = return . Value $ Bool True
isHash (IIntHash _)         = return . Value $ Bool True
isHash (ICharHash _)        = return . Value $ Bool True
isHash (IStrHash _)         = return . Value $ Bool True
isHash _                    = return . Value $ Bool False

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
