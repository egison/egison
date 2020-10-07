{- |
Module      : Language.Egison.Primitives.Types
Licence     : MIT

This module implements primitive functions that dynamically checks the types of
objects.
-}

module Language.Egison.Primitives.Types
  ( primitiveTypeFunctions
  ) where

import           Control.Monad.Except

import           Data.Char                        (chr, ord)
import           Data.Ratio                       ((%))

import           Language.Egison.Data
import           Language.Egison.EvalState        (MonadEval(..))
import           Language.Egison.Math
import           Language.Egison.Primitives.Utils

primitiveTypeFunctions :: [(String, EgisonValue)]
primitiveTypeFunctions =
  map (\(name, fn) -> (name, PrimitiveFunc (fn name))) strictPrimitives

strictPrimitives :: [(String, String -> PrimitiveFunc)]
strictPrimitives =
  [ ("isBool",       oneArg' isBool)
  , ("isInteger",    oneArg' isInteger)
  , ("isRational",   oneArg' isRational)
  , ("isScalar",     oneArg' isScalar)
  , ("isFloat",      oneArg' isFloat)
  , ("isChar",       oneArg' isChar)
  , ("isString",     oneArg' isString)
  , ("isCollection", oneArg' isCollection)
  , ("isHash",       oneArg' isHash)
  , ("isTensor",     oneArg' isTensor)

  , ("itof", integerToFloat)
  , ("rtof", rationalToFloat)
  , ("ctoi", charToInteger)
  , ("itoc", integerToChar)
  ]

--
-- Typing
--

isBool :: EgisonValue -> EvalM EgisonValue
isBool (Bool _) = return $ Bool True
isBool _        = return $ Bool False

isInteger :: EgisonValue -> EvalM EgisonValue
isInteger (ScalarData (Div (Plus []) (Plus [Term 1 []])))          = return $ Bool True
isInteger (ScalarData (Div (Plus [Term _ []]) (Plus [Term 1 []]))) = return $ Bool True
isInteger _                                                        = return $ Bool False

isRational :: EgisonValue -> EvalM EgisonValue
isRational (ScalarData (Div (Plus []) (Plus [Term _ []])))          = return $ Bool True
isRational (ScalarData (Div (Plus [Term _ []]) (Plus [Term _ []]))) = return $ Bool True
isRational _                                                        = return $ Bool False

isScalar :: EgisonValue -> EvalM EgisonValue
isScalar (ScalarData _) = return $ Bool True
isScalar _              = return $ Bool False

isTensor :: EgisonValue -> EvalM EgisonValue
isTensor (TensorData _) = return $ Bool True
isTensor _              = return $ Bool False

isFloat :: EgisonValue -> EvalM EgisonValue
isFloat (Float _) = return $ Bool True
isFloat _         = return $ Bool False

isChar :: EgisonValue -> EvalM EgisonValue
isChar (Char _) = return $ Bool True
isChar _        = return $ Bool False

isString :: EgisonValue -> EvalM EgisonValue
isString (String _) = return $ Bool True
isString _          = return $ Bool False

isCollection :: EgisonValue -> EvalM EgisonValue
isCollection (Collection _) = return $ Bool True
isCollection _              = return $ Bool False

isHash :: EgisonValue -> EvalM EgisonValue
isHash (IntHash _)  = return $ Bool True
isHash (CharHash _) = return $ Bool True
isHash (StrHash _)  = return $ Bool True
isHash _            = return $ Bool False

--
-- Transform
--
integerToFloat :: String -> PrimitiveFunc
integerToFloat = rationalToFloat

rationalToFloat :: String -> PrimitiveFunc
rationalToFloat = oneArg $ \val ->
  case val of
    ScalarData (Div (Plus []) _) -> return $ Float 0
    ScalarData (Div (Plus [Term x []]) (Plus [Term y []])) -> return $ Float (fromRational (x % y))
    _ -> throwError =<< TypeMismatch "integer or rational number" (Value val) <$> getFuncNameStack

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
