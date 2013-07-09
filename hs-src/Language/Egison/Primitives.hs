{-# Language FlexibleContexts #-}
module Language.Egison.Primitives (primitiveEnv) where

import Control.Arrow
import Control.Applicative
import Control.Monad.Error

import Data.IORef
import qualified Data.Array as A

import System.IO
import System.Random

import Language.Egison.Types
import Language.Egison.Core

primitiveEnv :: IO Env
primitiveEnv = do
  let ops = map (second PrimitiveFunc) (primitives ++ ioPrimitives)
  bindings <- forM (constants ++ ops) $ \(name, op) -> do
    ref <- newIORef . WHNF $ Value op
    return (name, ref)
  return $ extendEnv nullEnv bindings

{-# INLINE noArg #-}
noArg :: (MonadError EgisonError m) =>
         m EgisonValue ->
         [WHNFData] -> m EgisonValue
noArg f = \vals -> case vals of 
                     [] -> f
                     _ -> throwError $ ArgumentsNum 0 $ length vals

{-# INLINE oneArg #-}
oneArg :: (MonadError EgisonError m) =>
          (WHNFData -> m EgisonValue) ->
          [WHNFData] -> m EgisonValue
oneArg f = \vals -> case vals of 
                      [val] -> f val
                      [] -> f $ Value $ Tuple []
                      _ -> throwError $ ArgumentsNum 1 $ length vals

{-# INLINE twoArgs #-}
twoArgs :: (MonadError EgisonError m) =>
           (WHNFData -> WHNFData -> m EgisonValue) ->
           [WHNFData] -> m EgisonValue
twoArgs f = \vals -> case vals of 
                       [val, val'] -> f val val'
                       _ -> throwError $ ArgumentsNum 2 $ length vals

{-# INLINE threeArgs #-}
threeArgs :: (MonadError EgisonError m) =>
             (WHNFData -> WHNFData -> WHNFData -> m EgisonValue) ->
             [WHNFData] -> m EgisonValue
threeArgs f = \vals -> case vals of 
                         [val, val', val''] -> f val val' val''
                         _ -> throwError $ ArgumentsNum 3 $ length vals

--
-- Constants
--

constants :: [(String, EgisonValue)]
constants = [ ("pi", Float 3.141592653589793) ]

--
-- Primitives
--

primitives :: [(String, PrimitiveFunc)]
primitives = [ ("+", integerBinaryOp (+)) 
             , ("-", integerBinaryOp (-))
             , ("*", integerBinaryOp (*))
             , ("modulo",    integerBinaryOp mod)
             , ("quotient",   integerBinaryOp quot)
             , ("remainder", integerBinaryOp rem)
             , ("eq-i?",  integerBinaryPred (==))
             , ("lt-i?",  integerBinaryPred (<))
             , ("lte-i?", integerBinaryPred (<=))
             , ("gt-i?",  integerBinaryPred (>))
             , ("gte-i?", integerBinaryPred (>=))
             , ("+f", floatBinaryOp (+))
             , ("-f", floatBinaryOp (-))
             , ("*f", floatBinaryOp (*))
             , ("/f", floatBinaryOp (/))
             , ("eq-f?",  floatBinaryPred (==))
             , ("lt-f?",  floatBinaryPred (<))
             , ("lte-f?", floatBinaryPred (<=))
             , ("gt-f?",  floatBinaryPred (>))
             , ("gte-f?", floatBinaryPred (>=))
             , ("neg", integerUnaryOp negate)
             , ("abs", integerUnaryOp abs)
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
             , ("round",    floatToIntegerOp round)
             , ("floor",    floatToIntegerOp floor)
             , ("ceiling",  floatToIntegerOp ceiling)
             , ("truncate", floatToIntegerOp truncate)
             , ("itof", integerToFloat)
             , ("eq?",  eq)
             , ("lt?",  lt)
             , ("lte?", lte)
             , ("gt?",  gt)
             , ("gte?", gte)
             , ("string-append", stringAppend)
             , ("assert", assert)
             , ("assert-equal", assertEqual) ]

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

eq :: PrimitiveFunc
eq = (liftError .) $ twoArgs $ \val val' ->
  (Bool .) . (==) <$> fromPrimitiveValue val
                  <*> fromPrimitiveValue val'

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



stringAppend :: PrimitiveFunc
stringAppend = (liftError .) $ twoArgs $ \val val' ->
  (String .) . (++) <$> fromStringValue val
                    <*> fromStringValue val'

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
--             , ("read", readFromStdin)
               , ("write-char", writeChar)
               , ("write-string", writeString)
               , ("write", write)
--             , ("print", writeStringLine)
               , ("eof?", isEOFStdin)
               , ("flush", flushStdout)
               , ("read-char-from-port", readCharFromPort)
               , ("read-line-from-port", readLineFromPort)
--             , ("read-from-port", readFromPort)
               , ("write-char-to-port", writeCharToPort)
               , ("write-string-to-port", writeStringToPort)
               , ("write-to-port", writeToPort)
               , ("eof-port?", isEOFPort)
--             , ("print-to-port", writeStringLineToPort)
               , ("flush-port", flushPort) 
--               , ("rand", rand)
--               , ("rand-range", randRange) ]
               , ("rand", randRange) ]                 
--             , ("get-lib-dir-name", getLibDirName) ]

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

write :: PrimitiveFunc
write = oneArg $ \val ->
  makeIO' . liftIO . putStr . show <$> evalDeep val

readChar :: PrimitiveFunc
readChar = noArg $ return $ makeIO $ liftIO $ liftM Char getChar

readLine :: PrimitiveFunc
readLine = noArg $ return $ makeIO $ liftIO $ liftM String getLine

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

writeToPort :: PrimitiveFunc
writeToPort = twoArgs $ \val val' -> do
  ((makeIO' . liftIO) .) . hPutStr <$> liftError (fromPortValue val)
                                   <*> (show <$> evalDeep val')

readCharFromPort :: PrimitiveFunc
readCharFromPort = (liftError .) $ oneArg $ \val ->
  makeIO . liftIO . liftM Char . hGetChar <$> fromPortValue val

readLineFromPort :: PrimitiveFunc
readLineFromPort = (liftError .) $ oneArg $ \val ->
  makeIO . liftIO . liftM String . hGetLine <$> fromPortValue val

flushPort :: PrimitiveFunc
flushPort = (liftError .) $ oneArg $ \val ->
  makeIO' . liftIO . hFlush <$> fromPortValue val

isEOFPort :: PrimitiveFunc
isEOFPort = (liftError .) $ oneArg $ \val ->
  makeIO . liftIO . liftM Bool . hIsEOF <$> fromPortValue val

--rand :: PrimitiveFunc
--rand = noArg $ return $ makeIO $ liftIO $ liftM Integer $ getStdRandom random

randRange :: PrimitiveFunc
randRange = (liftError .) $ twoArgs $ \val val' ->
  return . makeIO . liftIO . liftM Integer . getStdRandom . randomR =<< liftM2 (,) (fromIntegerValue val) (fromIntegerValue val')