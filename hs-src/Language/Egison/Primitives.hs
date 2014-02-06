{-# Language FlexibleContexts #-}
module Language.Egison.Primitives (primitiveEnv, primitiveEnvNoIO) where

import Control.Arrow
import Control.Applicative
import Control.Monad.Error

import Data.IORef
import qualified Data.Array as A
import Data.Ratio

import System.IO
import System.Random

import qualified Data.Sequence as Sq

import System.IO.Unsafe
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

{--  -- for 'egison-sqlite'
import qualified Database.SQLite3 as SQLite
--}  -- for 'egison-sqlite'

{--  -- for 'egison-mysql'
import qualified Database.MySQL.Base as MySQL
--}  -- for 'egison-mysql'

import Control.Monad

import Language.Egison.Types
import Language.Egison.Parser
import Language.Egison.Core

primitiveEnv :: IO Env
primitiveEnv = do
  let ops = map (second PrimitiveFunc) (primitives ++ ioPrimitives)
  bindings <- forM (constants ++ ops) $ \(name, op) -> do
    ref <- newIORef . WHNF $ Value op
    return (name, ref)
  return $ extendEnv nullEnv bindings

primitiveEnvNoIO :: IO Env
primitiveEnvNoIO = do
  let ops = map (second PrimitiveFunc) primitives
  bindings <- forM (constants ++ ops) $ \(name, op) -> do
    ref <- newIORef . WHNF $ Value op
    return (name, ref)
  return $ extendEnv nullEnv bindings

{-# INLINE noArg #-}
noArg :: (MonadError EgisonError m) =>
         m EgisonValue ->
         EgisonValue -> m EgisonValue
noArg f = \args -> case fromTupleValue args of 
                     [] -> f
                     vals -> throwError $ ArgumentsNum 0 $ length vals

{-# INLINE oneArg #-}
oneArg :: (MonadError EgisonError m) =>
          (WHNFData -> m EgisonValue) ->
          EgisonValue -> m EgisonValue
oneArg f = \args -> f (Value args)

{-# INLINE twoArgs #-}
twoArgs :: (MonadError EgisonError m) =>
           (WHNFData -> WHNFData -> m EgisonValue) ->
           EgisonValue -> m EgisonValue
twoArgs f = \args -> case fromTupleValue args of 
                       [val, val'] -> f (Value val) (Value val')
                       vals -> throwError $ ArgumentsNum 2 $ length vals

{-# INLINE threeArgs #-}
threeArgs :: (MonadError EgisonError m) =>
             (WHNFData -> WHNFData -> WHNFData -> m EgisonValue) ->
             EgisonValue -> m EgisonValue
threeArgs f = \args -> case fromTupleValue args of 
                         [val, val', val''] -> f (Value val) (Value val') (Value val'')
                         vals -> throwError $ ArgumentsNum 3 $ length vals

--
-- Constants
--

constants :: [(String, EgisonValue)]
constants = [ ("pi", Float 3.141592653589793) ]

--
-- Primitives
--

primitives :: [(String, PrimitiveFunc)]
primitives = [ ("+", plus)
             , ("-", minus)
             , ("*", multiply)
             , ("/", divide)
             , ("/-inverse", divideInverse)
               
             , ("modulo",    integerBinaryOp mod)
             , ("quotient",   integerBinaryOp quot)
             , ("remainder", integerBinaryOp rem)
             , ("neg", integerUnaryOp negate)
             , ("abs", integerUnaryOp abs)
               
             , ("eq?",  eq)
             , ("lt?",  lt)
             , ("lte?", lte)
             , ("gt?",  gt)
             , ("gte?", gte)
               
             , ("round",    floatToIntegerOp round)
             , ("floor",    floatToIntegerOp floor)
             , ("ceiling",  floatToIntegerOp ceiling)
             , ("truncate", floatToIntegerOp truncate)
               
             , ("sqrt", floatUnaryOp sqrt)
             , ("exp", floatUnaryOp exp)
             , ("log", floatUnaryOp log)
             , ("sin", floatUnaryOp sin)
             , ("cos", floatUnaryOp cos)
             , ("tan", floatUnaryOp tan)
             , ("asin", floatUnaryOp asin)
             , ("acos", floatUnaryOp acos)
             , ("atan", floatUnaryOp atan)
             , ("sinh", floatUnaryOp sinh)
             , ("cosh", floatUnaryOp cosh)
             , ("tanh", floatUnaryOp tanh)
             , ("asinh", floatUnaryOp asinh)
             , ("acosh", floatUnaryOp acosh)
             , ("atanh", floatUnaryOp atanh)
               
             , ("itof", integerToFloat)
             , ("rtof", rationalToFloat)
               
             , ("stoi", stringToInteger)
               
             , ("read", read')
             , ("show", show')
               
             , ("assert", assert)
             , ("assert-equal", assertEqual)

{-- -- for 'egison-sqlite'
             , ("pure-sqlite", pureSQLite)
--} -- for 'egison-sqlite'

{-- -- for 'egison-mysql'
             , ("pure-mysql", pureMySQL)
--} -- for 'egison-mysql'
             ]

integerUnaryOp :: (Integer -> Integer) -> PrimitiveFunc
integerUnaryOp op = (liftError .) $ oneArg $ \val ->
  Integer . op <$> fromIntegerValue val

integerBinaryOp :: (Integer -> Integer -> Integer) -> PrimitiveFunc
integerBinaryOp op = (liftError .) $ twoArgs $ \val val' ->
  (Integer .) . op <$> fromIntegerValue val
                   <*> fromIntegerValue val'

integerBinaryPred :: (Integer -> Integer -> Bool) -> PrimitiveFunc
integerBinaryPred pred = (liftError .) $ twoArgs $ \val val' ->
  (Bool .) . pred <$> fromIntegerValue val
                  <*> fromIntegerValue val'

floatUnaryOp :: (Double -> Double) -> PrimitiveFunc
floatUnaryOp op = (liftError .) $ oneArg $ \val ->
  Float . op <$> fromFloatValue val

floatBinaryOp :: (Double -> Double -> Double) -> PrimitiveFunc
floatBinaryOp op = (liftError .) $ twoArgs $ \val val' ->
  (Float .) . op <$> fromFloatValue val
                 <*> fromFloatValue val'

floatBinaryPred :: (Double -> Double -> Bool) -> PrimitiveFunc
floatBinaryPred pred = (liftError .) $ twoArgs $ \val val' ->
  (Bool .) . pred <$> fromFloatValue val
                  <*> fromFloatValue val'

floatToIntegerOp :: (Double -> Integer) -> PrimitiveFunc
floatToIntegerOp op = (liftError .) $ oneArg $ \val ->
  Integer . op <$> fromFloatValue val

integerToFloat :: PrimitiveFunc
integerToFloat = (liftError .) $ oneArg $ \val ->
  Float . fromInteger <$> fromIntegerValue val

rationalToFloat :: PrimitiveFunc
rationalToFloat = (liftError .) $ oneArg $ \val ->
  Float . fromRational <$> fromRationalValue val

integerToString :: PrimitiveFunc
integerToString = (liftError .) $ oneArg $ \val ->
   makeEgisonString . show <$> fromIntegerValue val

stringToInteger :: PrimitiveFunc
stringToInteger = (liftError .) $ oneArg $ \val -> do
   numStr <- fromStringValue val
   return $ Integer (read numStr :: Integer)

show' :: PrimitiveFunc
show'= (liftError .) $ oneArg $ \val ->
   return $ makeEgisonString $ show val

eq :: PrimitiveFunc
eq = (liftError .) $ twoArgs $ \val val' ->
  (Bool .) . (==) <$> fromBuiltinValue val
                  <*> fromBuiltinValue val'

lt :: PrimitiveFunc
lt = (liftError .) $ twoArgs lt'
 where
  lt' (Value (Integer i)) (Value (Integer i')) = return $ Bool $ i < i'
  lt' (Value (Integer i)) (Value (Float f)) = return $ Bool $ fromInteger i < f
  lt' (Value (Float f)) (Value (Integer i)) = return $ Bool $ f < fromInteger i
  lt' (Value (Float f)) (Value (Float f')) = return $ Bool $ f < f'
  lt' (Value (Integer _)) val = throwError $ TypeMismatch "number" val
  lt' (Value (Float _)) val = throwError $ TypeMismatch "number" val
  lt' val _ = throwError $ TypeMismatch "number" val

lte :: PrimitiveFunc
lte = (liftError .) $ twoArgs lte'
 where
  lte' (Value (Integer i)) (Value (Integer i')) = return $ Bool $ i <= i'
  lte' (Value (Integer i)) (Value (Float f)) = return $ Bool $ fromInteger i <= f
  lte' (Value (Float f)) (Value (Integer i)) = return $ Bool $ f <= fromInteger i
  lte' (Value (Float f)) (Value (Float f')) = return $ Bool $ f <= f'
  lte' (Value (Integer _)) val = throwError $ TypeMismatch "number" val
  lte' (Value (Float _)) val = throwError $ TypeMismatch "number" val
  lte' val _ = throwError $ TypeMismatch "number" val

gt :: PrimitiveFunc
gt = (liftError .) $ twoArgs gt'
 where
  gt' (Value (Integer i)) (Value (Integer i')) = return $ Bool $ i > i'
  gt' (Value (Integer i)) (Value (Float f)) = return $ Bool $ fromInteger i > f
  gt' (Value (Float f)) (Value (Integer i)) = return $ Bool $ f > fromInteger i
  gt' (Value (Float f)) (Value (Float f')) = return $ Bool $ f > f'
  gt' (Value (Integer _)) val = throwError $ TypeMismatch "number" val
  gt' (Value (Float _)) val = throwError $ TypeMismatch "number" val
  gt' val _ = throwError $ TypeMismatch "number" val

gte :: PrimitiveFunc
gte = (liftError .) $ twoArgs gte'
 where
  gte' (Value (Integer i)) (Value (Integer i')) = return $ Bool $ i >= i'
  gte' (Value (Integer i)) (Value (Float f)) = return $ Bool $ fromInteger i >= f
  gte' (Value (Float f)) (Value (Integer i)) = return $ Bool $ f >= fromInteger i
  gte' (Value (Float f)) (Value (Float f')) = return $ Bool $ f >= f'
  gte' (Value (Integer _)) val = throwError $ TypeMismatch "number" val
  gte' (Value (Float _)) val = throwError $ TypeMismatch "number" val
  gte' val _ = throwError $ TypeMismatch "number" val

plus :: PrimitiveFunc
plus = (liftError .) $ twoArgs plus'
 where
  plus' (Value (Integer x)) (Value (Integer x')) = return $ Integer $ (x + x')
  plus' (Value (Integer i)) val = plus' (Value (Rational (i % 1))) val
  plus' val (Value (Integer i)) = plus' val (Value (Rational (i % 1)))
  plus' (Value (Rational x)) (Value (Rational x')) = let y = (x + x') in
                                                      if denominator y == 1
                                                        then return $ Integer $ numerator y
                                                        else return $ Rational y
  plus' (Value (Float f)) (Value (Float f')) = return $ Float $ f + f'
  plus' (Value (Rational i)) (Value (Float f)) = return $ Float $ (fromRational i) + f
  plus' (Value (Float f)) (Value (Rational i)) = return $ Float $ f + (fromRational i)
  plus' (Value (Rational _)) val = throwError $ TypeMismatch "number" val
  plus' (Value (Float _)) val = throwError $ TypeMismatch "number" val
  plus' val _ = throwError $ TypeMismatch "number" val

minus :: PrimitiveFunc
minus = (liftError .) $ twoArgs minus'
 where
  minus' (Value (Integer x)) (Value (Integer x')) = return $ Integer $ (x - x')
  minus' (Value (Integer i)) val = minus' (Value (Rational (i % 1))) val
  minus' val (Value (Integer i)) = minus' val (Value (Rational (i % 1)))
  minus' (Value (Rational x)) (Value (Rational x')) = let y = (x - x') in
                                                      if denominator y == 1
                                                        then return $ Integer $ numerator y
                                                        else return $ Rational y
  minus' (Value (Float f)) (Value (Float f')) = return $ Float $ f - f'
  minus' (Value (Rational i)) (Value (Float f)) = return $ Float $ (fromRational i) - f
  minus' (Value (Float f)) (Value (Rational i)) = return $ Float $ f - (fromRational i)
  minus' (Value (Rational _)) val = throwError $ TypeMismatch "number" val
  minus' (Value (Float _)) val = throwError $ TypeMismatch "number" val
  minus' val _ = throwError $ TypeMismatch "number" val

multiply :: PrimitiveFunc
multiply = (liftError .) $ twoArgs multiply'
 where
  multiply' (Value (Integer x)) (Value (Integer x')) = return $ Integer $ (x * x')
  multiply' (Value (Integer i)) val = multiply' (Value (Rational (i % 1))) val
  multiply' val (Value (Integer i)) = multiply' val (Value (Rational (i % 1)))
  multiply' (Value (Rational x)) (Value (Rational x')) = let y = (x * x') in
                                                      if denominator y == 1
                                                        then return $ Integer $ numerator y
                                                        else return $ Rational y
  multiply' (Value (Float f)) (Value (Float f')) = return $ Float $ f * f'
  multiply' (Value (Rational i)) (Value (Float f)) = return $ Float $ (fromRational i) * f
  multiply' (Value (Float f)) (Value (Rational i)) = return $ Float $ f * (fromRational i)
  multiply' (Value (Rational _)) val = throwError $ TypeMismatch "number" val
  multiply' (Value (Float _)) val = throwError $ TypeMismatch "number" val
  multiply' val _ = throwError $ TypeMismatch "number" val

divide :: PrimitiveFunc
divide = (liftError .) $ twoArgs divide'
 where
  divide' (Value (Integer x)) (Value (Integer x')) = return $ Rational $ (x % x')
  divide' (Value (Integer i)) val = divide' (Value (Rational (i % 1))) val
  divide' val (Value (Integer i)) = divide' val (Value (Rational (i % 1)))
  divide' (Value (Rational x)) (Value (Rational x')) =
    let m = numerator x' in
    let n = denominator x' in
    let y = (x * (n % m)) in
      if denominator y == 1
        then return $ Integer $ numerator y
        else return $ Rational y
  divide' (Value (Rational x)) (Value (Float f)) = return $ Float $ (fromRational x) / f
  divide' (Value (Float f)) (Value (Rational x)) = return $ Float $ f / (fromRational x)
  divide' (Value (Rational _)) val = throwError $ TypeMismatch "number" val
  divide' (Value (Float f)) (Value (Float f')) = return $ Float $ f / f'
  divide' (Value (Float _)) val = throwError $ TypeMismatch "number" val
  divide' val _ = throwError $ TypeMismatch "number" val

divideInverse :: PrimitiveFunc
divideInverse = (liftError .) $ oneArg $ divideInverse'
 where
  divideInverse' (Value (Rational rat)) = do
    return $ Tuple [Integer (numerator rat), Integer (denominator rat)]
  divideInverse' (Value (Integer x)) = do
    return $ Tuple [Integer x, Integer 1]
  divideInverse' val = throwError $ TypeMismatch "rational" val

assert ::  PrimitiveFunc
assert = (liftError .) $ twoArgs $ \label test -> do
  test <- fromBoolValue test
  if test
    then return $ Bool True
    else throwError $ Assertion $ show label

assertEqual :: PrimitiveFunc
assertEqual = threeArgs $ \label actual expected -> do
  actual <- evalDeep actual
  expected <- evalDeep expected
  if actual == expected
    then return $ Bool True
    else throwError $ Assertion $ show label ++ "\n expected: " ++ show expected ++
                                  "\n but found: " ++ show actual
{-- -- for 'egison-sqlite'
pureSQLite :: PrimitiveFunc
pureSQLite  = (liftError .) $ twoArgs $ \val val' -> do
  dbName <- fromStringValue val
  qStr <- fromStringValue val'
  let ret = unsafePerformIO $ query' (T.pack dbName) $ T.pack qStr
  return $ Collection $ Sq.fromList $ map (\r -> Tuple (map makeEgisonString r)) ret
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

{--  -- for 'egison-mysql'
pureMySQL :: PrimitiveFunc
pureMySQL = (liftError .) $ twoArgs $ \val val' -> do
  dbName <- fromStringValue val
  qStr <- fromStringValue val'
  let ret = unsafePerformIO $ query' dbName $ BC.pack qStr
  return $ Collection $ Sq.fromList $ map (\r -> Tuple (map makeEgisonString r)) ret
 where
  query' :: String -> ByteString -> IO [[String]]
  query' dbName q = do
    conn <- MySQL.connect MySQL.defaultConnectInfo { MySQL.connectDatabase = dbName }
    MySQL.query conn q
    ret <- MySQL.storeResult conn
    fetchAllRows ret
  fetchAllRows :: MySQL.Result -> IO [[String]]
  fetchAllRows ret = do
    row <- MySQL.fetchRow ret
    case row of
      [] -> return []
      _ -> do row' <- forM row (\mcol -> case mcol of
                                           Just col ->  return $ BC.unpack col
                                           Nothing -> return "null")
              rows' <- fetchAllRows ret
              return $ row':rows'
--}  -- for 'egison-mysql'

--
-- IO Primitives
--

ioPrimitives :: [(String, PrimitiveFunc)]
ioPrimitives = [
                 ("return", return')
               , ("open-input-file", makePort ReadMode)
               , ("open-output-file", makePort WriteMode)
               , ("close-input-port", closePort)
               , ("close-output-port", closePort)
               , ("read-char", readChar)
               , ("read-line", readLine)
               , ("write-char", writeChar)
               , ("write-string", writeString)
                 
               , ("read-char-from-port", readCharFromPort)
               , ("read-line-from-port", readLineFromPort)
               , ("write-char-to-port", writeCharToPort)
               , ("write-string-to-port", writeStringToPort)
               , ("write-to-port", writeToPort)
                 
               , ("eof?", isEOFStdin)
               , ("flush", flushStdout)
               , ("eof-port?", isEOFPort)
               , ("flush-port", flushPort)
               , ("read-file", readFile')
                 
               , ("rand", randRange)
               ]

makeIO :: EgisonM EgisonValue -> EgisonValue
makeIO m = IOFunc $ liftM (Value . Tuple . (World :) . (:[])) m

makeIO' :: EgisonM () -> EgisonValue
makeIO' m = IOFunc $ m >> return (Value $ Tuple [World, Tuple []])

return' :: PrimitiveFunc
return' = oneArg $ return . makeIO . evalDeep

makePort :: IOMode -> PrimitiveFunc
makePort mode = (liftError .) $ oneArg $ \val -> do
  filename <- fromStringValue val 
  return . makeIO . liftIO $ Port <$> openFile filename mode

closePort :: PrimitiveFunc
closePort = (liftError .) $ oneArg $ \val ->
  makeIO' . liftIO . hClose <$> fromPortValue val

writeChar :: PrimitiveFunc
writeChar = (liftError .) $ oneArg $ \val ->
  makeIO' . liftIO . putChar <$> fromCharValue val

writeString :: PrimitiveFunc
writeString = (liftError .) $ oneArg $ \val ->
  makeIO' . liftIO . putStr <$> fromStringValue val

writeToStdout :: PrimitiveFunc
writeToStdout = oneArg $ \val ->
  makeIO' . liftIO . putStr . show <$> evalDeep val

readChar :: PrimitiveFunc
readChar = noArg $ return $ makeIO $ liftIO $ liftM Char getChar

readLine :: PrimitiveFunc
readLine = noArg $ return $ makeIO $ liftIO $ liftM makeEgisonString getLine

flushStdout :: PrimitiveFunc
flushStdout = noArg $ return $ makeIO' $ liftIO $ hFlush stdout

isEOFStdin :: PrimitiveFunc
isEOFStdin = noArg $ return $ makeIO $ liftIO $ liftM Bool isEOF

writeCharToPort :: PrimitiveFunc
writeCharToPort = (liftError .) $ twoArgs $ \val val' ->
  ((makeIO' . liftIO) .) . hPutChar <$> fromPortValue val <*> fromCharValue val'

writeStringToPort :: PrimitiveFunc
writeStringToPort = (liftError .) $ twoArgs $ \val val' ->
  ((makeIO' . liftIO) .) . hPutStr <$> fromPortValue val <*> fromStringValue val'

readCharFromPort :: PrimitiveFunc
readCharFromPort = (liftError .) $ oneArg $ \val ->
  makeIO . liftIO . liftM Char . hGetChar <$> fromPortValue val

readLineFromPort :: PrimitiveFunc
readLineFromPort = (liftError .) $ oneArg $ \val ->
  makeIO . liftIO . liftM makeEgisonString . hGetLine <$> fromPortValue val

flushPort :: PrimitiveFunc
flushPort = (liftError .) $ oneArg $ \val ->
  makeIO' . liftIO . hFlush <$> fromPortValue val

isEOFPort :: PrimitiveFunc
isEOFPort = (liftError .) $ oneArg $ \val ->
  makeIO . liftIO . liftM Bool . hIsEOF <$> fromPortValue val

readFile' :: PrimitiveFunc
readFile' =  (liftError .) $ oneArg $ \val ->
  makeIO . liftIO . liftM makeEgisonString . readFile <$> fromStringValue val
  
randRange :: PrimitiveFunc
randRange = (liftError .) $ twoArgs $ \val val' ->
  return . makeIO . liftIO . liftM Integer . getStdRandom . randomR =<< liftM2 (,) (fromIntegerValue val) (fromIntegerValue val')
