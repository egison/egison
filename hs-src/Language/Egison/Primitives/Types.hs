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
isInteger (Value val) = case val of
  CASData (CASInteger _) -> return . Value $ Bool True
  CASData (CASPoly [CASTerm (CASInteger _) []]) -> return . Value $ Bool True
  _ -> return . Value $ Bool False
isInteger _ = return . Value $ Bool False

isRational :: WHNFData -> EvalM WHNFData
isRational (Value val) = case val of
  CASData cv | isRationalCAS cv -> return . Value $ Bool True
  _ -> return . Value $ Bool False
isRational _ = return . Value $ Bool False

-- | Check if a CASValue represents a rational number (integer or fraction of integers)
isRationalCAS :: CASValue -> Bool
isRationalCAS cv = case cv of
  CASInteger _ -> True
  CASPoly [] -> True  -- zero
  CASPoly [CASTerm coef []] -> isRationalCAS coef
  CASDiv num den -> isRationalCAS num && isRationalCAS den
  _ -> False

--
-- Transform
--
integerToFloat :: String -> PrimitiveFunc
integerToFloat = rationalToFloat

rationalToFloat :: String -> PrimitiveFunc
rationalToFloat = oneArg $ \val ->
  case val of
    CASData cv | Just r <- extractRational cv -> return $ Float (fromRational r)
    _ -> throwErrorWithTrace (TypeMismatch "integer or rational number" (Value val))

-- | Extract a Rational from a CASValue if it represents a rational number
extractRational :: CASValue -> Maybe Rational
extractRational cv = case cv of
  CASInteger n -> Just (n % 1)
  CASPoly [] -> Just 0
  CASPoly [CASTerm coef []] -> extractRational coef
  CASDiv num den -> do
    n <- extractInteger num
    d <- extractInteger den
    if d == 0 then Nothing else Just (n % d)
  _ -> Nothing

-- | Extract an Integer from a CASValue if it represents an integer
extractInteger :: CASValue -> Maybe Integer
extractInteger cv = case cv of
  CASInteger n -> Just n
  CASPoly [] -> Just 0
  CASPoly [CASTerm coef []] -> extractInteger coef
  _ -> Nothing

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

