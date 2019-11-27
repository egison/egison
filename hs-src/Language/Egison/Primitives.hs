{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}

{- |
Module      : Language.Egison.Primitives
Copyright   : Satoshi Egi
Licence     : MIT

This module provides primitive functions in Egison.
-}

module Language.Egison.Primitives
  (
  -- S-syntax version
    primitiveEnv
  , primitiveEnvNoIO
  -- Non-S syntax (Camel case) version
  , primitiveEnv'
  , primitiveEnvNoIO'
  ) where

import           Control.Monad.Except
import           Control.Monad.Trans.Maybe

import           Data.Foldable             (toList)
import           Data.IORef
import           Data.Ratio
import           Text.Regex.TDFA           ((=~~))

import           System.IO
import           System.Process            (readProcess)
import           System.Random             (getStdRandom, randomR)

import qualified Data.Sequence             as Sq
import qualified Data.Vector               as V

import           Data.Char                 (chr, ord)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T

 {--  -- for 'egison-sqlite'
import qualified Database.SQLite3 as SQLite
 --}  -- for 'egison-sqlite'

import           Language.Egison.AST
import           Language.Egison.Core
import           Language.Egison.Parser
import           Language.Egison.Pretty
import           Language.Egison.Types
import           Language.Egison.Tensor

primitiveEnv :: IO Env
primitiveEnv = do
  let ops = map (\(name, fn) -> (name, PrimitiveFunc name fn)) (primitives ++ ioPrimitives)
  bindings <- forM (constants ++ ops) $ \(name, op) -> do
    ref <- newIORef . WHNF $ Value op
    return (stringToVar name, ref)
  return $ extendEnv nullEnv bindings

primitiveEnvNoIO :: IO Env
primitiveEnvNoIO = do
  let ops = map (\(name, fn) -> (name, PrimitiveFunc name fn)) primitives
  bindings <- forM (constants ++ ops) $ \(name, op) -> do
    ref <- newIORef . WHNF $ Value op
    return (stringToVar name, ref)
  return $ extendEnv nullEnv bindings

-- primitive env for Non-S syntax
primitiveEnv' :: IO Env
primitiveEnv' = do
  let ops = map (\(name, fn) -> (name, PrimitiveFunc name fn)) (primitives' ++ ioPrimitives')
  bindings <- forM (constants ++ ops) $ \(name, op) -> do
    ref <- newIORef . WHNF $ Value op
    return (stringToVar name, ref)
  return $ extendEnv nullEnv bindings

primitiveEnvNoIO' :: IO Env
primitiveEnvNoIO' = do
  let ops = map (\(name, fn) -> (name, PrimitiveFunc name fn)) primitives'
  bindings <- forM (constants ++ ops) $ \(name, op) -> do
    ref <- newIORef . WHNF $ Value op
    return (stringToVar name, ref)
  return $ extendEnv nullEnv bindings

{-# INLINE noArg #-}
noArg :: EgisonM EgisonValue -> PrimitiveFunc
noArg f args = do
    args' <- tupleToList args
    case args' of
      [] -> Value <$> f
      _  -> throwError =<< ArgumentsNumPrimitive 0 (length args') <$> getFuncNameStack

{-# INLINE oneArg #-}
oneArg :: (EgisonValue -> EgisonM EgisonValue) -> PrimitiveFunc
oneArg f arg = do
  arg' <- evalWHNF arg
  case arg' of
    (TensorData (Tensor ns ds js)) -> do
      ds' <- V.mapM f ds
      Value <$> fromTensor (Tensor ns ds' js)
    _ -> Value <$> f arg'

{-# INLINE oneArg' #-}
oneArg' :: (EgisonValue -> EgisonM EgisonValue) -> PrimitiveFunc
oneArg' f arg = do
  arg' <- evalWHNF arg
  Value <$> f arg'

{-# INLINE twoArgs #-}
twoArgs :: (EgisonValue -> EgisonValue -> EgisonM EgisonValue) -> PrimitiveFunc
twoArgs f args = do
  args' <- tupleToList args
  case args' of
    [TensorData t1@Tensor{}, TensorData t2@Tensor{}] -> Value <$> (tProduct f t1 t2 >>= fromTensor)
    [TensorData(Tensor ns ds js), val] -> do
      ds' <- V.mapM (`f` val) ds
      Value <$> fromTensor (Tensor ns ds' js)
    [val, TensorData (Tensor ns ds js)] -> do
      ds' <- V.mapM (f val) ds
      Value <$> fromTensor (Tensor ns ds' js)
    [val, val'] -> Value <$> f val val'
    _ -> throwError =<< ArgumentsNumPrimitive 2 (length args') <$> getFuncNameStack

{-# INLINE twoArgs' #-}
twoArgs' :: (EgisonValue -> EgisonValue -> EgisonM EgisonValue) -> PrimitiveFunc
twoArgs' f args = do
  args' <- tupleToList args
  case args' of
    [val, val'] -> Value <$> f val val'
    _           -> throwError =<< ArgumentsNumPrimitive 2 (length args') <$> getFuncNameStack

{-# INLINE threeArgs' #-}
threeArgs' :: (EgisonValue -> EgisonValue -> EgisonValue -> EgisonM EgisonValue) -> PrimitiveFunc
threeArgs' f args = do
  args' <- tupleToList args
  case args' of
    [val, val', val''] -> Value <$> f val val' val''
    _                  -> throwError =<< ArgumentsNumPrimitive 3 (length args') <$> getFuncNameStack

--
-- Constants
--

constants :: [(String, EgisonValue)]
constants = [
              ("f.pi", Float 3.141592653589793)
             ,("f.e" , Float 2.718281828459045)
              ]

--
-- Primitives
--

primitives :: [(String, PrimitiveFunc)]
primitives = [ ("b.+", plus)
             , ("b.-", minus)
             , ("b.*", multiply)
             , ("b./", divide)
             , ("b.+'", plus)
             , ("b.-'", minus)
             , ("b.*'", multiply)
             , ("b./'", divide)
             , ("f.+", floatBinaryOp (+))
             , ("f.-", floatBinaryOp (-))
             , ("f.*", floatBinaryOp (*))
             , ("f./", floatBinaryOp (/))
             , ("numerator", numerator')
             , ("denominator", denominator')
             , ("from-math-expr", fromScalarData)
             , ("to-math-expr", toScalarData)
             , ("to-math-expr'", toScalarData)

             , ("modulo",    integerBinaryOp mod)
             , ("quotient",  integerBinaryOp quot)
             , ("remainder", integerBinaryOp rem)
             , ("b.abs", rationalUnaryOp abs)
             , ("b.neg", rationalUnaryOp negate)

             , ("eq?",  eq)
             , ("lt?",  scalarCompare (<))
             , ("lte?", scalarCompare (<=))
             , ("gt?",  scalarCompare (>))
             , ("gte?", scalarCompare (>=))

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

             , ("tensor-size", tensorSize')
             , ("tensor-to-list", tensorToList')
             , ("df-order", dfOrder')

             , ("itof", integerToFloat)
             , ("rtof", rationalToFloat)
             , ("ctoi", charToInteger)
             , ("itoc", integerToChar)

             , ("pack", pack)
             , ("unpack", unpack)
             , ("uncons-string", unconsString)
             , ("length-string", lengthString)
             , ("append-string", appendString)
             , ("split-string", splitString)
             , ("regex", regexString)
             , ("regex-cg", regexStringCaptureGroup)

             , ("add-prime", addPrime)
             , ("add-subscript", addSubscript)
             , ("add-superscript", addSuperscript)

             , ("read-process", readProcess')

             , ("read", read')
             , ("read-tsv", readTSV)
             , ("show", show')
             , ("show-tsv", showTSV')

             , ("empty?", isEmpty')
             , ("uncons", uncons')
             , ("unsnoc", unsnoc')

             , ("bool?", isBool')
             , ("integer?", isInteger')
             , ("rational?", isRational')
             , ("scalar?", isScalar')
             , ("float?", isFloat')
             , ("char?", isChar')
             , ("string?", isString')
             , ("collection?", isCollection')
             , ("array?", isArray')
             , ("hash?", isHash')
             , ("tensor?", isTensor')
             , ("tensor-with-index?", isTensorWithIndex')

             , ("assert", assert)
             , ("assert-equal", assertEqual)
             ]

-- for Non-S syntax
primitives' :: [(String, PrimitiveFunc)]
primitives' = [ ("b.+", plus)
             , ("b.-", minus)
             , ("b.*", multiply)
             , ("b./", divide)
             , ("b.+'", plus)
             , ("b.-'", minus)
             , ("b.*'", multiply)
             , ("b./'", divide)
             , ("f.+", floatBinaryOp (+))
             , ("f.-", floatBinaryOp (-))
             , ("f.*", floatBinaryOp (*))
             , ("f./", floatBinaryOp (/))
             , ("numerator", numerator')
             , ("denominator", denominator')
             , ("fromMathExpr", fromScalarData)
             , ("toMathExpr", toScalarData)
             , ("toMathExpr'", toScalarData)

             , ("modulo",    integerBinaryOp mod)
             , ("quotient",  integerBinaryOp quot)
             , ("remainder", integerBinaryOp rem)
             , ("b.abs", rationalUnaryOp abs)
             , ("b.neg", rationalUnaryOp negate)

             , ("eq?",  eq)
             , ("lt?",  scalarCompare (<))
             , ("lte?", scalarCompare (<=))
             , ("gt?",  scalarCompare (>))
             , ("gte?", scalarCompare (>=))

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

             , ("tensorSize", tensorSize')
             , ("tensorToList", tensorToList')
             , ("dfOrder", dfOrder')

             , ("itof", integerToFloat)
             , ("rtof", rationalToFloat)
             , ("ctoi", charToInteger)
             , ("itoc", integerToChar)

             , ("pack", pack)
             , ("unpack", unpack)
             , ("unconsString", unconsString)
             , ("lengthString", lengthString)
             , ("appendString", appendString)
             , ("splitString", splitString)
             , ("regex", regexString)
             , ("regexCg", regexStringCaptureGroup)

             , ("addPrime", addPrime)
             , ("addSubscript", addSubscript)
             , ("addSuperscript", addSuperscript)

             , ("readProcess", readProcess')

             , ("read", read')
             , ("readTsv", readTSV)
             , ("show", show')
             , ("showTsv", showTSV')

             , ("empty?", isEmpty')
             , ("uncons", uncons')
             , ("unsnoc", unsnoc')

             , ("bool?", isBool')
             , ("integer?", isInteger')
             , ("rational?", isRational')
             , ("scalar?", isScalar')
             , ("float?", isFloat')
             , ("char?", isChar')
             , ("string?", isString')
             , ("collection?", isCollection')
             , ("array?", isArray')
             , ("hash?", isHash')
             , ("tensor?", isTensor')
             , ("tensorWithIndex?", isTensorWithIndex')

             , ("assert", assert)
             , ("assertEqual", assertEqual)

             -- for old library compatibility
             , ("from-math-expr", fromScalarData)
             , ("to-math-expr", toScalarData)
             , ("to-math-expr'", toScalarData)
             , ("tensor-size", tensorSize')
             , ("tensor-to-list", tensorToList')
             , ("df-order", dfOrder')
             , ("uncons-string", unconsString)
             , ("length-string", lengthString)
             , ("append-string", appendString)
             , ("split-string", splitString)
             , ("regex-cg", regexStringCaptureGroup)
             , ("add-prime", addPrime)
             , ("add-subscript", addSubscript)
             , ("add-superscript", addSuperscript)
             , ("read-process", readProcess')
             , ("read-tsv", readTSV)
             , ("show-tsv", showTSV')
             , ("tensor-with-index?", isTensorWithIndex')
             , ("assert-equal", assertEqual)
             ]

unaryOp :: (EgisonData a, EgisonData b) => (a -> b) -> PrimitiveFunc
unaryOp op = oneArg $ \val -> do
  v <- fromEgison val
  return $ toEgison (op v)

binaryOp :: (EgisonData a, EgisonData b) => (a -> a -> b) -> PrimitiveFunc
binaryOp op = twoArgs $ \val val' -> do
  i <- fromEgison val
  i' <- fromEgison val'
  return $ toEgison (op i i')

rationalUnaryOp :: (Rational -> Rational) -> PrimitiveFunc
rationalUnaryOp = unaryOp

integerBinaryOp :: (Integer -> Integer -> Integer) -> PrimitiveFunc
integerBinaryOp = binaryOp

floatUnaryOp :: (Double -> Double) -> PrimitiveFunc
floatUnaryOp = unaryOp

floatBinaryOp :: (Double -> Double -> Double) -> PrimitiveFunc
floatBinaryOp = binaryOp

--
-- Arith
--

scalarBinaryOp :: (ScalarData -> ScalarData -> ScalarData) -> PrimitiveFunc
scalarBinaryOp mOp = twoArgs $ \val val' -> scalarBinaryOp' val val'
 where
  scalarBinaryOp' (ScalarData m1) (ScalarData m2) = (return . ScalarData . mathNormalize') (mOp m1 m2)
  scalarBinaryOp' (ScalarData _)  val             = throwError =<< TypeMismatch "number" (Value val) <$> getFuncNameStack
  scalarBinaryOp' val             _               = throwError =<< TypeMismatch "number" (Value val) <$> getFuncNameStack

plus :: PrimitiveFunc
plus = scalarBinaryOp mathPlus

minus :: PrimitiveFunc
minus = scalarBinaryOp (\m1 m2 -> mathPlus m1 (mathNegate m2))

multiply :: PrimitiveFunc
multiply = scalarBinaryOp mathMult

divide :: PrimitiveFunc
divide = scalarBinaryOp (\m1 (Div p1 p2) -> mathMult m1 (Div p2 p1))

numerator' :: PrimitiveFunc
numerator' =  oneArg numerator''
 where
  numerator'' (ScalarData m) = return $ ScalarData (mathNumerator m)
  numerator'' val = throwError =<< TypeMismatch "rational" (Value val) <$> getFuncNameStack

denominator' :: PrimitiveFunc
denominator' =  oneArg denominator''
 where
  denominator'' (ScalarData m) = return $ ScalarData (mathDenominator m)
  denominator'' val = throwError =<< TypeMismatch "rational" (Value val) <$> getFuncNameStack

fromScalarData :: PrimitiveFunc
fromScalarData = oneArg fromScalarData'
 where
  fromScalarData' (ScalarData m) = return $ mathExprToEgison m
  fromScalarData' val = throwError =<< TypeMismatch "number" (Value val) <$> getFuncNameStack

toScalarData :: PrimitiveFunc
toScalarData = oneArg toScalarData'
 where
  toScalarData' val = ScalarData . mathNormalize' <$> egisonToScalarData val

--
-- Pred
--
eq :: PrimitiveFunc
eq = twoArgs' $ \val val' ->
  return $ Bool $ val == val'

scalarCompare :: (forall a. Ord a => a -> a -> Bool) -> PrimitiveFunc
scalarCompare cmp = twoArgs' $ \val1 val2 ->
  case (val1, val2) of
    (ScalarData _, ScalarData _) -> do
      r1 <- fromEgison val1 :: EgisonM Rational
      r2 <- fromEgison val2 :: EgisonM Rational
      return $ Bool (cmp r1 r2)
    (Float f1, Float f2) -> return $ Bool (cmp f1 f2)
    (ScalarData _, _) -> throwError =<< TypeMismatch "number" (Value val2) <$> getFuncNameStack
    (Float _,      _) -> throwError =<< TypeMismatch "float"  (Value val2) <$> getFuncNameStack
    _                 -> throwError =<< TypeMismatch "number" (Value val1) <$> getFuncNameStack

truncate' :: PrimitiveFunc
truncate' = oneArg $ \val -> numberUnaryOp' val
 where
  numberUnaryOp' (ScalarData (Div (Plus []) _)) = return $ toEgison (0 :: Integer)
  numberUnaryOp' (ScalarData (Div (Plus [Term x []]) (Plus [Term y []]))) = return $ toEgison (quot x y)
  numberUnaryOp' (Float x)             = return $ toEgison (truncate x :: Integer)
  numberUnaryOp' val                   = throwError =<< TypeMismatch "rational or float" (Value val) <$> getFuncNameStack

--
-- Tensor
--

tensorSize' :: PrimitiveFunc
tensorSize' = oneArg' tensorSize''
 where
  tensorSize'' (TensorData (Tensor ns _ _)) = return . Collection . Sq.fromList $ map toEgison ns
  tensorSize'' _ = return . Collection $ Sq.fromList []

tensorToList' :: PrimitiveFunc
tensorToList' = oneArg' tensorToList''
 where
  tensorToList'' (TensorData (Tensor _ xs _)) = return . Collection . Sq.fromList $ V.toList xs
  tensorToList'' x = return . Collection $ Sq.fromList [x]

dfOrder' :: PrimitiveFunc
dfOrder' = oneArg' dfOrder''
 where
  dfOrder'' (TensorData (Tensor ns _ is)) = return (toEgison (fromIntegral (length ns - length is) :: Integer))
  dfOrder'' _ = return (toEgison (0 :: Integer))

--
-- Transform
--
numberToFloat' :: EgisonValue -> EgisonValue
numberToFloat' (ScalarData (Div (Plus []) _)) = Float 0
numberToFloat' (ScalarData (Div (Plus [Term x []]) (Plus [Term y []]))) = Float $ fromRational (x % y)

integerToFloat :: PrimitiveFunc
integerToFloat = rationalToFloat

rationalToFloat :: PrimitiveFunc
rationalToFloat = oneArg $ \val ->
  case val of
    (ScalarData (Div (Plus []) _)) -> return $ numberToFloat' val
    (ScalarData (Div (Plus [Term _ []]) (Plus [Term _ []]))) -> return $ numberToFloat' val
    _ -> throwError =<< TypeMismatch "integer or rational number" (Value val) <$> getFuncNameStack

charToInteger :: PrimitiveFunc
charToInteger = unaryOp ctoi
  where
    ctoi :: Char -> Integer
    ctoi = fromIntegral . ord

integerToChar :: PrimitiveFunc
integerToChar = unaryOp itoc
  where
    itoc :: Integer -> Char
    itoc = chr . fromIntegral

floatToIntegerOp :: (Double -> Integer) -> PrimitiveFunc
floatToIntegerOp = unaryOp

--
-- String
--
pack :: PrimitiveFunc
pack = oneArg $ \val -> do
  str <- packStringValue val
  return $ String str
  where
    packStringValue :: EgisonValue -> EgisonM Text
    packStringValue (Collection seq) = do
      let ls = toList seq
      str <- mapM fromEgison ls
      return $ T.pack str
    packStringValue (Tuple [val]) = packStringValue val
    packStringValue val = throwError =<< TypeMismatch "collection" (Value val) <$> getFuncNameStack

unpack :: PrimitiveFunc
unpack = unaryOp T.unpack

unconsString :: PrimitiveFunc
unconsString = oneArg $ \val -> do
  str <- fromEgison val
  case T.uncons str of
    Just (c, rest) -> return $ Tuple [Char c, String rest]
    Nothing -> throwError $ Default "Tried to unsnoc empty string"

lengthString :: PrimitiveFunc
lengthString = unaryOp (toInteger . T.length)

appendString :: PrimitiveFunc
appendString = binaryOp T.append

splitString :: PrimitiveFunc
splitString = twoArgs $ \pat src -> do
  patStr <- fromEgison pat
  srcStr <- fromEgison src
  return . Collection . Sq.fromList $ map String $ T.splitOn patStr srcStr

regexString :: PrimitiveFunc
regexString = twoArgs $ \pat src -> do
  patStr <- fromEgison pat
  srcStr <- fromEgison src
  case (T.unpack srcStr =~~ T.unpack patStr) :: (Maybe (String, String, String)) of
    Nothing -> return . Collection . Sq.fromList $ []
    Just (a,b,c) -> return . Collection . Sq.fromList $ [Tuple [String $ T.pack a, String $ T.pack b, String $ T.pack c]]

regexStringCaptureGroup :: PrimitiveFunc
regexStringCaptureGroup = twoArgs $ \pat src -> do
  patStr <- fromEgison pat
  srcStr <- fromEgison src
  case (T.unpack srcStr =~~ T.unpack patStr) :: (Maybe [[String]]) of
    Nothing -> return . Collection . Sq.fromList $ []
    Just ((x:xs):_) -> do let (a, c) = T.breakOn (T.pack x) srcStr
                          return . Collection . Sq.fromList $ [Tuple [String a, Collection (Sq.fromList (map (String . T.pack) xs)), String (T.drop (length x) c)]]

--regexStringMatch :: PrimitiveFunc
--regexStringMatch = twoArgs $ \pat src -> do
--  case (pat, src) of
--    (String patStr, String srcStr) -> return . Bool $ (((T.unpack srcStr) =~ (T.unpack patStr)) :: Bool)
--    (String _, _) -> throwError =<< TypeMismatch "string" (Value src) <$> getFuncNameStack
--    (_, _) -> throwError =<< TypeMismatch "string" (Value pat) <$> getFuncNameStack

addPrime :: PrimitiveFunc
addPrime = oneArg $ \sym ->
  case sym of
    ScalarData (Div (Plus [Term 1 [(Symbol id name is, 1)]]) (Plus [Term 1 []])) ->
      return (ScalarData (Div (Plus [Term 1 [(Symbol id (name ++ "'") is, 1)]]) (Plus [Term 1 []])))
    _ -> throwError =<< TypeMismatch "symbol" (Value sym) <$> getFuncNameStack

addSubscript :: PrimitiveFunc
addSubscript = twoArgs $ \fn sub ->
  case (fn, sub) of
    (ScalarData (Div (Plus [Term 1 [(Symbol id name is, 1)]]) (Plus [Term 1 []])),
     ScalarData s@(Div (Plus [Term 1 [(Symbol _ _ [], 1)]]) (Plus [Term 1 []]))) ->
       return (ScalarData (Div (Plus [Term 1 [(Symbol id name (is ++ [Subscript s]), 1)]]) (Plus [Term 1 []])))
    (ScalarData (Div (Plus [Term 1 [(Symbol id name is, 1)]]) (Plus [Term 1 []])),
     ScalarData s@(Div (Plus [Term _ []]) (Plus [Term 1 []]))) ->
       return (ScalarData (Div (Plus [Term 1 [(Symbol id name (is ++ [Subscript s]), 1)]]) (Plus [Term 1 []])))
    (ScalarData (Div (Plus [Term 1 [(Symbol{}, 1)]]) (Plus [Term 1 []])),
     _) -> throwError =<< TypeMismatch "symbol or integer" (Value sub) <$> getFuncNameStack
    _ -> throwError =<< TypeMismatch "symbol or integer" (Value fn) <$> getFuncNameStack

addSuperscript :: PrimitiveFunc
addSuperscript = twoArgs $ \fn sub ->
  case (fn, sub) of
    (ScalarData (Div (Plus [Term 1 [(Symbol id name is, 1)]]) (Plus [Term 1 []])),
     ScalarData s@(Div (Plus [Term 1 [(Symbol _ _ [], 1)]]) (Plus [Term 1 []]))) ->
       return (ScalarData (Div (Plus [Term 1 [(Symbol id name (is ++ [Superscript s]), 1)]]) (Plus [Term 1 []])))
    (ScalarData (Div (Plus [Term 1 [(Symbol id name is, 1)]]) (Plus [Term 1 []])),
     ScalarData s@(Div (Plus [Term _ []]) (Plus [Term 1 []]))) ->
       return (ScalarData (Div (Plus [Term 1 [(Symbol id name (is ++ [Superscript s]), 1)]]) (Plus [Term 1 []])))
    (ScalarData (Div (Plus [Term 1 [(Symbol{}, 1)]]) (Plus [Term 1 []])),
     _) -> throwError =<< TypeMismatch "symbol" (Value sub) <$> getFuncNameStack
    _ -> throwError =<< TypeMismatch "symbol" (Value fn) <$> getFuncNameStack

readProcess' :: PrimitiveFunc
readProcess' = threeArgs' $ \cmd args input ->
  case (cmd, args, input) of
    (String cmdStr, Collection argStrs, String inputStr) -> do
      let cmd' = T.unpack cmdStr
      let args' = map (\case String argStr -> T.unpack argStr) (toList argStrs)
      let input' = T.unpack inputStr
      outputStr <- liftIO $ readProcess cmd' args' input'
      return (String (T.pack outputStr))
    (_, _, _) -> throwError =<< TypeMismatch "(string, collection, string)" (Value (Tuple [cmd, args, input])) <$> getFuncNameStack

read' :: PrimitiveFunc
read'= oneArg' $ \val -> do
  str <- fromEgison val
  ast <- readExpr (T.unpack str)
  evalExprDeep nullEnv ast

readTSV :: PrimitiveFunc
readTSV= oneArg' $ \val -> do
  str   <- fromEgison val
  exprs <- readExprs (T.unpack str)
  rets  <- mapM (evalExprDeep nullEnv) exprs
  case rets of
    [ret] -> return ret
    _     -> return (Tuple rets)

show' :: PrimitiveFunc
show'= oneArg' $ \val -> return $ toEgison $ T.pack $ show val

showTSV' :: PrimitiveFunc
showTSV'= oneArg' $ \val -> return $ toEgison $ T.pack $ showTSV val

--
-- Collection
--
isEmpty' :: PrimitiveFunc
isEmpty' whnf = Value . Bool <$> isEmptyCollection whnf

uncons' :: PrimitiveFunc
uncons' whnf = do
  mRet <- runMaybeT (unconsCollection whnf)
  case mRet of
    Just (carObjRef, cdrObjRef) -> return $ Intermediate $ ITuple [carObjRef, cdrObjRef]
    Nothing -> throwError $ Default "cannot uncons collection"

unsnoc' :: PrimitiveFunc
unsnoc' whnf = do
  mRet <- runMaybeT (unsnocCollection whnf)
  case mRet of
    Just (racObjRef, rdcObjRef) -> return $ Intermediate $ ITuple [racObjRef, rdcObjRef]
    Nothing -> throwError $ Default "cannot unsnoc collection"

-- Test

assert ::  PrimitiveFunc
assert = twoArgs' $ \label test -> do
  test <- fromEgison test
  if test
    then return $ Bool True
    else throwError =<< Assertion (show label) <$> getFuncNameStack

assertEqual :: PrimitiveFunc
assertEqual = threeArgs' $ \label actual expected ->
  if actual == expected
     then return $ Bool True
     else throwError =<< Assertion
       (show label ++ "\n expected: " ++ show expected ++ "\n but found: " ++ show actual) <$> getFuncNameStack

--
-- IO Primitives
--

ioPrimitives :: [(String, PrimitiveFunc)]
ioPrimitives = [ ("return", return')
               , ("open-input-file", makePort ReadMode)
               , ("open-output-file", makePort WriteMode)
               , ("close-input-port", closePort)
               , ("close-output-port", closePort)
               , ("read-char", readChar)
               , ("read-line", readLine)
               , ("write-char", writeChar)
               , ("write", writeString)

               , ("read-char-from-port", readCharFromPort)
               , ("read-line-from-port", readLineFromPort)
               , ("write-char-to-port", writeCharToPort)
               , ("write-to-port", writeStringToPort)

               , ("eof?", isEOFStdin)
               , ("flush", flushStdout)
               , ("eof-port?", isEOFPort)
               , ("flush-port", flushPort)
               , ("read-file", readFile')

               , ("rand", randRange)
               , ("f.rand", randRangeDouble)
--               , ("sqlite", sqlite)
               ]

-- For Non-S syntax
ioPrimitives' :: [(String, PrimitiveFunc)]
ioPrimitives' = [ ("return", return')
               , ("openInputFile", makePort ReadMode)
               , ("openOutputFile", makePort WriteMode)
               , ("closeInputPort", closePort)
               , ("closeOutputPort", closePort)
               , ("readChar", readChar)
               , ("readLine", readLine)
               , ("writeChar", writeChar)
               , ("write", writeString)

               , ("readCharFromPort", readCharFromPort)
               , ("readLineFromPort", readLineFromPort)
               , ("writeCharToPort", writeCharToPort)
               , ("writeToPort", writeStringToPort)

               , ("eof?", isEOFStdin)
               , ("flush", flushStdout)
               , ("eofPort?", isEOFPort)
               , ("flushPort", flushPort)
               , ("readFile", readFile')

               , ("rand", randRange)
               , ("f.rand", randRangeDouble)

               -- for S-expression library compatibility
               , ("open-input-file", makePort ReadMode)
               , ("open-output-file", makePort WriteMode)
               , ("close-input-port", closePort)
               , ("close-output-port", closePort)
               , ("read-char", readChar)
               , ("read-line", readLine)
               , ("write-char", writeChar)
               , ("read-char-from-port", readCharFromPort)
               , ("read-line-from-port", readLineFromPort)
               , ("write-char-to-port", writeCharToPort)
               , ("write-to-port", writeStringToPort)
               , ("eof-port?", isEOFPort)
               , ("flush-port", flushPort)
               , ("read-file", readFile')
               ]

makeIO :: EgisonM EgisonValue -> EgisonValue
makeIO m = IOFunc $ fmap (Value . Tuple . (World :) . (:[])) m

makeIO' :: EgisonM () -> EgisonValue
makeIO' m = IOFunc $ m >> return (Value $ Tuple [World, Tuple []])

return' :: PrimitiveFunc
return' = oneArg' $ \val -> return $ makeIO $ return val

makePort :: IOMode -> PrimitiveFunc
makePort mode = oneArg' $ \val -> do
  filename <- fromEgison val
  port <- liftIO $ openFile (T.unpack filename) mode
  return $ makeIO $ return (Port port)

closePort :: PrimitiveFunc
closePort = oneArg' $ \val -> do
  port <- fromEgison val
  return $ makeIO' $ liftIO $ hClose port

writeChar :: PrimitiveFunc
writeChar = oneArg' $ \val -> do
  c <- fromEgison val
  return $ makeIO' $ liftIO $ putChar c

writeCharToPort :: PrimitiveFunc
writeCharToPort = twoArgs' $ \val val' -> do
  port <- fromEgison val
  c <- fromEgison val'
  return $ makeIO' $ liftIO $ hPutChar port c

writeString :: PrimitiveFunc
writeString = oneArg' $ \val -> do
  s <- fromEgison val
  return $ makeIO' $ liftIO $ T.putStr s

writeStringToPort :: PrimitiveFunc
writeStringToPort = twoArgs' $ \val val' -> do
  port <- fromEgison val
  s <- fromEgison val'
  return $ makeIO' $ liftIO $ T.hPutStr port s

flushStdout :: PrimitiveFunc
flushStdout = noArg $ return $ makeIO' $ liftIO $ hFlush stdout

flushPort :: PrimitiveFunc
flushPort = oneArg' $ \val -> do
  port <- fromEgison val
  return $ makeIO' $ liftIO $ hFlush port

readChar :: PrimitiveFunc
readChar = noArg $ return $ makeIO $ liftIO $ fmap Char getChar

readCharFromPort :: PrimitiveFunc
readCharFromPort = oneArg' $ \val -> do
  port <- fromEgison val
  c <- liftIO $ hGetChar port
  return $ makeIO $ return (Char c)

readLine :: PrimitiveFunc
readLine = noArg $ return $ makeIO $ liftIO $ fmap toEgison T.getLine

readLineFromPort :: PrimitiveFunc
readLineFromPort = oneArg' $ \val -> do
  port <- fromEgison val
  s <- liftIO $ T.hGetLine port
  return $ makeIO $ return $ toEgison s

readFile' :: PrimitiveFunc
readFile' =  oneArg' $ \val -> do
  filename <- fromEgison val
  s <- liftIO $ T.readFile $ T.unpack filename
  return $ makeIO $ return $ toEgison s

isEOFStdin :: PrimitiveFunc
isEOFStdin = noArg $ return $ makeIO $ liftIO $ fmap Bool isEOF

isEOFPort :: PrimitiveFunc
isEOFPort = oneArg' $ \val -> do
  port <- fromEgison val
  b <- liftIO $ hIsEOF port
  return $ makeIO $ return (Bool b)

randRange :: PrimitiveFunc
randRange = twoArgs' $ \val val' -> do
  i <- fromEgison val :: EgisonM Integer
  i' <- fromEgison val' :: EgisonM Integer
  n <- liftIO $ getStdRandom $ randomR (i, i')
  return $ makeIO $ return $ toEgison n

randRangeDouble :: PrimitiveFunc
randRangeDouble = twoArgs' $ \val val' -> do
  i <- fromEgison val :: EgisonM Double
  i' <- fromEgison val' :: EgisonM Double
  n <- liftIO $ getStdRandom $ randomR (i, i')
  return $ makeIO $ return $ toEgison n

 {-- -- for 'egison-sqlite'
sqlite :: PrimitiveFunc
sqlite  = twoArgs' $ \val val' -> do
  dbName <- fromEgison val
  qStr <- fromEgison val'
  ret <- liftIO $ query' (T.pack dbName) $ T.pack qStr
  return $ makeIO $ return $ Collection $ Sq.fromList $ map (\r -> Tuple (map toEgison r)) ret
 where
  query' :: T.Text -> T.Text -> IO [[String]]
  query' dbName q = do
    db <- SQLite.open dbName
    rowsRef <- newIORef []
    SQLite.execWithCallback db q (\_ _ mcs -> do
                                    row <- forM mcs (\mcol -> case mcol of
                                                              Just col ->  return $ T.unpack col
                                                              Nothing -> return "null")
                                    rows <- readIORef rowsRef
                                    writeIORef rowsRef (row:rows))
    SQLite.close db
    ret <- readIORef rowsRef
    return $ reverse ret
 --} -- for 'egison-sqlite'
