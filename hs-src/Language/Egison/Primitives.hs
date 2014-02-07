{-# Language FlexibleContexts #-}

{- |
Module      : Language.Egison.Primitives
Copyright   : Satoshi Egi
Licence     : MIT

This module provides primitive functions in Egison.
-}

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
noArg :: EgisonM EgisonValue ->
         EgisonValue -> EgisonM EgisonValue
noArg f = \args -> case tupleToList args of 
                     [] -> f
                     vals -> throwError $ ArgumentsNum 0 $ length vals

{-# INLINE oneArg #-}
oneArg :: (EgisonValue -> EgisonM EgisonValue) ->
          EgisonValue -> EgisonM EgisonValue
oneArg f = \args -> case args of
                      (Tuple [val]) -> f val
                      _ -> f args

{-# INLINE twoArgs #-}
twoArgs :: (EgisonValue -> EgisonValue -> EgisonM EgisonValue) ->
           EgisonValue -> EgisonM EgisonValue
twoArgs f = \args -> case tupleToList args of 
                       [val, val'] -> f val val'
                       vals -> throwError $ ArgumentsNum 2 $ length vals

{-# INLINE threeArgs #-}
threeArgs :: (EgisonValue -> EgisonValue -> EgisonValue -> EgisonM EgisonValue) ->
             EgisonValue -> EgisonM EgisonValue
threeArgs f = \args -> case tupleToList args of 
                         [val, val', val''] -> f val val' val''
                         vals -> throwError $ ArgumentsNum 3 $ length vals

tupleToList :: EgisonValue -> [EgisonValue]
tupleToList (Tuple vals) = vals
tupleToList val = [val]

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
integerUnaryOp op = oneArg $ \val -> do
  i <- fromEgison val
  return $ Integer $ op i
  
integerBinaryOp :: (Integer -> Integer -> Integer) -> PrimitiveFunc
integerBinaryOp op = twoArgs $ \val val' -> do
  i <- fromEgison val
  i' <- fromEgison val'
  return $ Integer $ op i i'

integerBinaryPred :: (Integer -> Integer -> Bool) -> PrimitiveFunc
integerBinaryPred pred = twoArgs $ \val val' -> do
  i <- fromEgison val
  i' <- fromEgison val'
  return $ Bool $ pred i i'

floatUnaryOp :: (Double -> Double) -> PrimitiveFunc
floatUnaryOp op = oneArg $ \val -> do
  f <- fromEgison val
  return $ Float $ op f

floatBinaryOp :: (Double -> Double -> Double) -> PrimitiveFunc
floatBinaryOp op = twoArgs $ \val val' -> do
  f <- fromEgison val
  f' <- fromEgison val'
  return $ Float $ op f f'

floatBinaryPred :: (Double -> Double -> Bool) -> PrimitiveFunc
floatBinaryPred pred = twoArgs $ \val val' -> do
  f <- fromEgison val
  f' <- fromEgison val'
  return $ Bool $ pred f f'

--
-- Arith
--
plus :: PrimitiveFunc
plus = twoArgs $ \val val' -> numberBinaryOp' val val'
 where
  numberBinaryOp' (Integer i)  (Integer i')  = return $ Integer $ (+) i  i'
  numberBinaryOp' (Integer i)  val           = numberBinaryOp' (Rational (i % 1)) val
  numberBinaryOp' val          (Integer i)   = numberBinaryOp' val (Rational (i % 1)) 
  numberBinaryOp' (Rational r) (Rational r') = let y = (+) r r' in
                                                 if denominator y == 1
                                                   then return $ Integer $ numerator y
                                                   else return $ Rational y
  numberBinaryOp' (Rational r) (Float f)     = numberBinaryOp' (Float (fromRational r)) (Float f)
  numberBinaryOp' (Float f)    (Rational r)  = numberBinaryOp' (Float f) (Float (fromRational r))
  numberBinaryOp' (Float f)    (Float f')    = return $ Float $ (+) f f'
  numberBinaryOp' (Rational _) val           = throwError $ TypeMismatch "number" (Value val)
  numberBinaryOp' (Float _)    val           = throwError $ TypeMismatch "number" (Value val)
  numberBinaryOp' val          _             = throwError $ TypeMismatch "number" (Value val)

minus :: PrimitiveFunc
minus = twoArgs $ \val val' -> numberBinaryOp' val val'
 where
  numberBinaryOp' (Integer i)  (Integer i')  = return $ Integer $ (-) i  i'
  numberBinaryOp' (Integer i)  val           = numberBinaryOp' (Rational (i % 1)) val
  numberBinaryOp' val          (Integer i)   = numberBinaryOp' val (Rational (i % 1)) 
  numberBinaryOp' (Rational r) (Rational r') = let y = (-) r r' in
                                                 if denominator y == 1
                                                   then return $ Integer $ numerator y
                                                   else return $ Rational y
  numberBinaryOp' (Rational r) (Float f)     = numberBinaryOp' (Float (fromRational r)) (Float f)
  numberBinaryOp' (Float f)    (Rational r)  = numberBinaryOp' (Float f) (Float (fromRational r))
  numberBinaryOp' (Float f)    (Float f')    = return $ Float $ (-) f f'
  numberBinaryOp' (Rational _) val           = throwError $ TypeMismatch "number" (Value val)
  numberBinaryOp' (Float _)    val           = throwError $ TypeMismatch "number" (Value val)
  numberBinaryOp' val          _             = throwError $ TypeMismatch "number" (Value val)

multiply :: PrimitiveFunc
multiply = twoArgs $ \val val' -> numberBinaryOp' val val'
 where
  numberBinaryOp' (Integer i)  (Integer i')  = return $ Integer $ (*) i  i'
  numberBinaryOp' (Integer i)  val           = numberBinaryOp' (Rational (i % 1)) val
  numberBinaryOp' val          (Integer i)   = numberBinaryOp' val (Rational (i % 1)) 
  numberBinaryOp' (Rational r) (Rational r') = let y = (*) r r' in
                                                 if denominator y == 1
                                                   then return $ Integer $ numerator y
                                                   else return $ Rational y
  numberBinaryOp' (Rational r) (Float f)     = numberBinaryOp' (Float (fromRational r)) (Float f)
  numberBinaryOp' (Float f)    (Rational r)  = numberBinaryOp' (Float f) (Float (fromRational r))
  numberBinaryOp' (Float f)    (Float f')    = return $ Float $ (*) f f'
  numberBinaryOp' (Rational _) val           = throwError $ TypeMismatch "number" (Value val)
  numberBinaryOp' (Float _)    val           = throwError $ TypeMismatch "number" (Value val)
  numberBinaryOp' val          _             = throwError $ TypeMismatch "number" (Value val)

divide :: PrimitiveFunc
divide = twoArgs $ \val val' -> numberBinaryOp' val val'
 where
  numberBinaryOp' (Integer i)  (Integer i')  = return $ Rational $ (%) i  i'
  numberBinaryOp' (Integer i)  val           = numberBinaryOp' (Rational (i % 1)) val
  numberBinaryOp' val          (Integer i)   = numberBinaryOp' val (Rational (i % 1)) 
  numberBinaryOp' (Rational r) (Rational r') =
    let m = numerator r' in
    let n = denominator r' in
    let y = (r * (n % m)) in
      if denominator y == 1
        then return $ Integer $ numerator y
        else return $ Rational y
  numberBinaryOp' (Rational r) (Float f)    = numberBinaryOp' (Float (fromRational r)) (Float f)
  numberBinaryOp' (Float f)    (Rational r) = numberBinaryOp' (Float f) (Float (fromRational r))
  numberBinaryOp' (Float f)    (Float f')   = return $ Float $ (/) f f'
  numberBinaryOp' (Rational _) val          = throwError $ TypeMismatch "number" (Value val)
  numberBinaryOp' (Float _)    val          = throwError $ TypeMismatch "number" (Value val)
  numberBinaryOp' val          _            = throwError $ TypeMismatch "number" (Value val)

divideInverse :: PrimitiveFunc
divideInverse =  oneArg $ divideInverse'
 where
  divideInverse' (Rational rat) = do
    return $ Tuple [Integer (numerator rat), Integer (denominator rat)]
  divideInverse' (Integer x) = do
    return $ Tuple [Integer x, Integer 1]
  divideInverse' val = throwError $ TypeMismatch "rational" (Value val)

--
-- Pred
--
eq :: PrimitiveFunc
eq = twoArgs $ \val val' ->
  return $ Bool $ val == val'

lt :: PrimitiveFunc
lt = twoArgs $ \val val' -> numberBinaryPred' val val'
 where
  numberBinaryPred' (Integer i)  (Integer i')  = return $ Bool $ (<) i  i'
  numberBinaryPred' (Integer i)  val           = numberBinaryPred' (Rational (i % 1)) val
  numberBinaryPred' val          (Integer i)   = numberBinaryPred' val (Rational (i % 1)) 
  numberBinaryPred' (Rational r) (Rational r') = return $ Bool $ (<) r r'
  numberBinaryPred' (Rational r) (Float f)     = numberBinaryPred' (Float (fromRational r)) (Float f)
  numberBinaryPred' (Float f)    (Rational r)  = numberBinaryPred' (Float f) (Float (fromRational r))
  numberBinaryPred' (Float f)    (Float f')    = return $ Bool $ (<) f f'
  numberBinaryPred' (Rational _) val           = throwError $ TypeMismatch "number" (Value val)
  numberBinaryPred' (Float _)    val           = throwError $ TypeMismatch "number" (Value val)
  numberBinaryPred' val          _             = throwError $ TypeMismatch "number" (Value val)
  
lte :: PrimitiveFunc
lte = twoArgs $ \val val' -> numberBinaryPred' val val'
 where
  numberBinaryPred' (Integer i)  (Integer i')  = return $ Bool $ (<=) i  i'
  numberBinaryPred' (Integer i)  val           = numberBinaryPred' (Rational (i % 1)) val
  numberBinaryPred' val          (Integer i)   = numberBinaryPred' val (Rational (i % 1)) 
  numberBinaryPred' (Rational r) (Rational r') = return $ Bool $ (<=) r r'
  numberBinaryPred' (Rational r) (Float f)     = numberBinaryPred' (Float (fromRational r)) (Float f)
  numberBinaryPred' (Float f)    (Rational r)  = numberBinaryPred' (Float f) (Float (fromRational r))
  numberBinaryPred' (Float f)    (Float f')    = return $ Bool $ (<=) f f'
  numberBinaryPred' (Rational _) val           = throwError $ TypeMismatch "number" (Value val)
  numberBinaryPred' (Float _)    val           = throwError $ TypeMismatch "number" (Value val)
  numberBinaryPred' val          _             = throwError $ TypeMismatch "number" (Value val)
  
gt :: PrimitiveFunc
gt = twoArgs $ \val val' -> numberBinaryPred' val val'
 where
  numberBinaryPred' (Integer i)  (Integer i')  = return $ Bool $ (>) i  i'
  numberBinaryPred' (Integer i)  val           = numberBinaryPred' (Rational (i % 1)) val
  numberBinaryPred' val          (Integer i)   = numberBinaryPred' val (Rational (i % 1)) 
  numberBinaryPred' (Rational r) (Rational r') = return $ Bool $ (>) r r'
  numberBinaryPred' (Rational r) (Float f)     = numberBinaryPred' (Float (fromRational r)) (Float f)
  numberBinaryPred' (Float f)    (Rational r)  = numberBinaryPred' (Float f) (Float (fromRational r))
  numberBinaryPred' (Float f)    (Float f')    = return $ Bool $ (>) f f'
  numberBinaryPred' (Rational _) val           = throwError $ TypeMismatch "number" (Value val)
  numberBinaryPred' (Float _)    val           = throwError $ TypeMismatch "number" (Value val)
  numberBinaryPred' val          _             = throwError $ TypeMismatch "number" (Value val)
  
gte :: PrimitiveFunc
gte = twoArgs $ \val val' -> numberBinaryPred' val val'
 where
  numberBinaryPred' (Integer i)  (Integer i')  = return $ Bool $ (>=) i  i'
  numberBinaryPred' (Integer i)  val           = numberBinaryPred' (Rational (i % 1)) val
  numberBinaryPred' val          (Integer i)   = numberBinaryPred' val (Rational (i % 1)) 
  numberBinaryPred' (Rational r) (Rational r') = return $ Bool $ (>=) r r'
  numberBinaryPred' (Rational r) (Float f)     = numberBinaryPred' (Float (fromRational r)) (Float f)
  numberBinaryPred' (Float f)    (Rational r)  = numberBinaryPred' (Float f) (Float (fromRational r))
  numberBinaryPred' (Float f)    (Float f')    = return $ Bool $ (>=) f f'
  numberBinaryPred' (Rational _) val           = throwError $ TypeMismatch "number" (Value val)
  numberBinaryPred' (Float _)    val           = throwError $ TypeMismatch "number" (Value val)
  numberBinaryPred' val          _             = throwError $ TypeMismatch "number" (Value val)
  
--
-- Transform
--
integerToFloat :: PrimitiveFunc
integerToFloat = oneArg $ \val -> do
  i <- fromEgison val
  return $ Float $ fromInteger i

rationalToFloat :: PrimitiveFunc
rationalToFloat = oneArg $ \val -> do
  r <- fromEgison val
  return $ Float $ fromRational r

floatToIntegerOp :: (Double -> Integer) -> PrimitiveFunc
floatToIntegerOp op = oneArg $ \val -> do
  f <- fromEgison val
  return $ Integer $ op f

read' :: PrimitiveFunc
read'= oneArg $ \val -> fromStringValue val >>= readExpr >>= evalExprDeep nullEnv

show' :: PrimitiveFunc
show'= oneArg $ \val -> return $ toEgison $ show val

stringToInteger :: PrimitiveFunc
stringToInteger = oneArg $ \val -> do
  numStr <- fromEgison val
  return $ Integer (read numStr :: Integer)


assert ::  PrimitiveFunc
assert = twoArgs $ \label test -> do
  test <- fromEgison test
  if test
    then return $ Bool True
    else throwError $ Assertion $ show label

assertEqual :: PrimitiveFunc
assertEqual = threeArgs $ \label actual expected -> do
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
return' = oneArg $ \val -> return $ makeIO $ return val

makePort :: IOMode -> PrimitiveFunc
makePort mode = oneArg $ \val -> do
  filename <- fromEgison val
  port <- liftIO $ openFile filename mode
  return $ makeIO $ return (Port port)

closePort :: PrimitiveFunc
closePort = oneArg $ \val -> do
  port <- fromEgison val
  return $ makeIO' $ liftIO $ hClose port

writeChar :: PrimitiveFunc
writeChar = oneArg $ \val -> do
  c <- fromEgison val
  return $ makeIO' $ liftIO $ putChar c

writeCharToPort :: PrimitiveFunc
writeCharToPort = twoArgs $ \val val' -> do
  port <- fromEgison val
  c <- fromEgison val'
  return $ makeIO' $ liftIO $ hPutChar port c

writeString :: PrimitiveFunc
writeString = oneArg $ \val -> do
  s <- fromEgison val
  return $ makeIO' $ liftIO $ putStr s
  
writeStringToPort :: PrimitiveFunc
writeStringToPort = twoArgs $ \val val' -> do
  port <- fromEgison val
  s <- fromEgison val'
  return $ makeIO' $ liftIO $ hPutStr port s

flushStdout :: PrimitiveFunc
flushStdout = noArg $ return $ makeIO' $ liftIO $ hFlush stdout

flushPort :: PrimitiveFunc
flushPort = oneArg $ \val -> do
  port <- fromEgison val
  return $ makeIO' $ liftIO $ hFlush port

readChar :: PrimitiveFunc
readChar = noArg $ return $ makeIO $ liftIO $ liftM Char getChar

readCharFromPort :: PrimitiveFunc
readCharFromPort = oneArg $ \val -> do
  port <- fromEgison val
  c <- liftIO $ hGetChar port
  return $ makeIO $ return (Char c)

readLine :: PrimitiveFunc
readLine = noArg $ return $ makeIO $ liftIO $ liftM toEgison getLine

readLineFromPort :: PrimitiveFunc
readLineFromPort = oneArg $ \val -> do
  port <- fromEgison val
  s <- liftIO $ hGetLine port
  return $ makeIO $ return $ toEgison s

readFile' :: PrimitiveFunc
readFile' =  oneArg $ \val -> do
  filename <- fromEgison val
  s <- liftIO $ readFile filename
  return $ makeIO $ return $ toEgison s
  
isEOFStdin :: PrimitiveFunc
isEOFStdin = noArg $ return $ makeIO $ liftIO $ liftM Bool isEOF

isEOFPort :: PrimitiveFunc
isEOFPort = oneArg $ \val -> do
  port <- fromEgison val
  b <- liftIO $ hIsEOF port
  return $ makeIO $ return (Bool b)

randRange :: PrimitiveFunc
randRange = twoArgs $ \val val' -> do
  i <- fromEgison val
  i' <- fromEgison val'
  n <- liftIO $ getStdRandom $ randomR (i, i')
  return $ makeIO $ return (Integer n)
