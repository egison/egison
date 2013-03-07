{-# Language FlexibleContexts #-}
module Language.Egison.Primitives (primitiveEnv) where

import Control.Arrow
import Control.Applicative
import Control.Monad.Error

import Data.IORef

import System.IO

import Language.Egison.Types

primitiveEnv :: IO Env
primitiveEnv = do
  bindings <- forM (primitives ++ ioPrimitives) $ \(name, op) -> do
    ref <- newIORef . WHNF . Value $ PrimitiveFunc op
    return ((name, []), ref)
  return $ extendEnv nullEnv bindings

{-# INLINE noArg #-}
noArg :: (MonadError EgisonError m) =>
         m EgisonValue ->
         [WHNFData] -> m EgisonValue
noArg f = \vals -> case vals of 
                     [] -> f
                     _ -> throwError $ ArgumentsNum 0 vals

{-# INLINE oneArg #-}
oneArg :: (MonadError EgisonError m) =>
          (WHNFData -> m EgisonValue) ->
          [WHNFData] -> m EgisonValue
oneArg f = \vals -> case vals of 
                      [val] -> f val
                      _ -> throwError $ ArgumentsNum 1 vals

{-# INLINE twoArgs #-}
twoArgs :: (MonadError EgisonError m) =>
          (WHNFData -> WHNFData -> m EgisonValue) ->
          [WHNFData] -> m EgisonValue
twoArgs f = \vals -> case vals of 
                       [val, val'] -> f val val'
                       _ -> throwError $ ArgumentsNum 2 vals

--
-- Primitives
--

primitives :: [(String, PrimitiveFunc)]
primitives = [ ("+", add) 
             , ("-", sub)
             , ("*", mul)
             , ("eq?", eq) ]

add :: PrimitiveFunc
add [val, val'] = (Integer .) . (+) <$> fromIntegerValue val
                                    <*> fromIntegerValue val'
add vals = throwError $ ArgumentsNum 2 vals

sub :: PrimitiveFunc
sub [val, val'] = (Integer .) . (-) <$> fromIntegerValue val
                                    <*> fromIntegerValue val'
sub vals = throwError $ ArgumentsNum 2 vals

mul :: PrimitiveFunc
mul [val, val'] = (Integer .) . (*) <$> fromIntegerValue val
                                    <*> fromIntegerValue val'
mul vals = throwError $ ArgumentsNum 2 vals

eq :: PrimitiveFunc
eq [Value val, Value val'] = Bool <$> eq' val val'
 where
  eq' (Char c) (Char c') = return $ c == c'
  eq' (String s) (String s') = return $ s == s'
  eq' (Bool b) (Bool b') = return $ b == b'
  eq' (Integer i) (Integer i') = return $ i == i'
  eq' (Float f) (Float f') = return $ f == f'
  eq' _ _ = throwError $ TypeMismatch "immediate" $ Value val
eq [val, _] = throwError $ TypeMismatch "immediate" val
eq vals = throwError $ ArgumentsNum 2 vals

--
-- IO Primitives
--

ioPrimitives :: [(String, PrimitiveFunc)]
ioPrimitives = [ ("open-input-file", makePort ReadMode)
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
--             , ("flush", flushStdout)
               , ("read-char-from-port", readCharFromPort)
               , ("read-line-from-port", readLineFromPort)
--             , ("read-from-port", readFromPort)
               , ("write-char-to-port", writeCharToPort)
               , ("write-string-to-port", writeStringToPort)
               , ("write-to-port", writeToPort) ]
--             , ("print-to-port", writeStringLineToPort)
--             , ("flush-port", flushPort)
--             , ("get-lib-dir-name", getLibDirName) ]

makeIO :: IO EgisonValue -> EgisonValue
makeIO io = IOFunc . oneArg $ \val ->
  case val of
    Value World -> Tuple . (:) World . return <$> liftIO io
    _ -> throwError $ TypeMismatch "world" val

makeIO' :: IO () -> EgisonValue
makeIO' io = IOFunc . oneArg $ \val ->
  case val of
    Value World -> liftIO io >> return World
    _ -> throwError $ TypeMismatch "world" val

makePort :: IOMode -> PrimitiveFunc
makePort mode = oneArg $ \val -> do
  filename <- fromStringValue val 
  return . makeIO $ Port <$> openFile filename mode

closePort :: PrimitiveFunc
closePort = oneArg $ \val -> makeIO' . hClose <$> fromPortValue val

writeChar :: PrimitiveFunc
writeChar = oneArg $ \val -> makeIO' . putChar <$> fromCharValue val

writeString :: PrimitiveFunc
writeString = oneArg $ \val -> makeIO' . putStr <$> fromStringValue val

write :: PrimitiveFunc
write = oneArg $ Right . makeIO' . putStr . show

readChar :: PrimitiveFunc
readChar = noArg $ Right $ makeIO (liftM Char getChar)

readLine :: PrimitiveFunc
readLine = noArg $ Right $ makeIO (liftM String getLine)

writeCharToPort :: PrimitiveFunc
writeCharToPort = twoArgs $ \val val' ->
  (makeIO' .) . hPutChar <$> fromPortValue val <*> fromCharValue val'

writeStringToPort :: PrimitiveFunc
writeStringToPort = twoArgs $ \val val' ->
  (makeIO' .) . hPutStr <$> fromPortValue val <*> fromStringValue val'

writeToPort :: PrimitiveFunc
writeToPort = twoArgs $ \val val' ->
  makeIO' . flip hPutStr (show val') <$> fromPortValue val

readCharFromPort :: PrimitiveFunc
readCharFromPort = oneArg $ \val ->
  makeIO . liftM Char . hGetChar <$> fromPortValue val

readLineFromPort :: PrimitiveFunc
readLineFromPort = oneArg $ \val ->
  makeIO . liftM String . hGetLine <$> fromPortValue val
