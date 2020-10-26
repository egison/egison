{- |
Module      : Language.Egison.Primitives.IO
Licence     : MIT

This module implements primitive functions that performs IO operations.
-}

module Language.Egison.Primitives.IO
  ( ioPrimitives
  ) where

import           Control.Monad.Except

import           Data.IORef

import           System.IO
import           System.Process                   (readProcess)
import           System.Random                    (getStdRandom, randomR)

import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T

import           Language.Egison.Core             (evalWHNF)
import           Language.Egison.Data
import           Language.Egison.EvalState        (MonadEval(..))
import           Language.Egison.Primitives.Utils


--
-- IO Primitives
--

ioPrimitives :: [(String, EgisonValue)]
ioPrimitives =
  map (\(name, fn) -> (name, PrimitiveFunc (fn name))) ioStrictPrimitives ++
    map (\(name, fn) -> (name, LazyPrimitiveFunc (fn name))) ioLazyPrimitives

ioStrictPrimitives :: [(String, String -> PrimitiveFunc)]
ioStrictPrimitives =
  [ ("return",          return')
  , ("openInputFile",   makePort ReadMode)
  , ("openOutputFile",  makePort WriteMode)
  , ("closeInputPort",  closePort)
  , ("closeOutputPort", closePort)
  , ("readChar",        readChar)
  , ("readLine",        readLine)
  , ("writeChar",       writeChar)
  , ("write",           writeString)

  , ("readCharFromPort", readCharFromPort)
  , ("readLineFromPort", readLineFromPort)
  , ("writeCharToPort",  writeCharToPort)
  , ("writeToPort",      writeStringToPort)

  , ("isEof",     isEOFStdin)
  , ("flush",     flushStdout)
  , ("isEofPort", isEOFPort)
  , ("flushPort", flushPort)
  , ("readFile",  readFile')

  , ("rand",       randRange)
  , ("f.rand",     randRangeDouble)

  , ("newIORef",   newIORef')
  , ("writeIORef", writeIORef')
  , ("readIORef",  readIORef')

  , ("readProcess", readProcess')
  ]

ioLazyPrimitives :: [(String, String -> LazyPrimitiveFunc)]
ioLazyPrimitives =
  [ ("io", io)
  ]

makeIO :: EvalM EgisonValue -> EgisonValue
makeIO m = IOFunc $ fmap (Value . Tuple . (World :) . (:[])) m

makeIO' :: EvalM () -> EgisonValue
makeIO' m = IOFunc $ m >> return (Value $ Tuple [World, Tuple []])

return' :: String -> PrimitiveFunc
return' = oneArg' $ \val -> return $ makeIO $ return val

makePort :: IOMode -> String -> PrimitiveFunc
makePort mode = oneArg' $ \val -> do
  filename <- fromEgison val
  port <- liftIO $ openFile (T.unpack filename) mode
  return $ makeIO $ return (Port port)

closePort :: String -> PrimitiveFunc
closePort = oneArg' $ \val -> do
  port <- fromEgison val
  return $ makeIO' $ liftIO $ hClose port

writeChar :: String -> PrimitiveFunc
writeChar = oneArg' $ \val -> do
  c <- fromEgison val
  return $ makeIO' $ liftIO $ putChar c

writeCharToPort :: String -> PrimitiveFunc
writeCharToPort = twoArgs' $ \val val' -> do
  port <- fromEgison val
  c <- fromEgison val'
  return $ makeIO' $ liftIO $ hPutChar port c

writeString :: String -> PrimitiveFunc
writeString = oneArg' $ \val -> do
  s <- fromEgison val
  return $ makeIO' $ liftIO $ T.putStr s

writeStringToPort :: String -> PrimitiveFunc
writeStringToPort = twoArgs' $ \val val' -> do
  port <- fromEgison val
  s <- fromEgison val'
  return $ makeIO' $ liftIO $ T.hPutStr port s

flushStdout :: String -> PrimitiveFunc
flushStdout = noArg $ return $ makeIO' $ liftIO $ hFlush stdout

flushPort :: String -> PrimitiveFunc
flushPort = oneArg' $ \val -> do
  port <- fromEgison val
  return $ makeIO' $ liftIO $ hFlush port

readChar :: String -> PrimitiveFunc
readChar = noArg $ return $ makeIO $ liftIO (Char <$> getChar)

readCharFromPort :: String -> PrimitiveFunc
readCharFromPort = oneArg' $ \val -> do
  port <- fromEgison val
  return . makeIO $ liftIO (Char <$> hGetChar port)

readLine :: String -> PrimitiveFunc
readLine = noArg $ return $ makeIO $ liftIO (toEgison <$> T.getLine)

readLineFromPort :: String -> PrimitiveFunc
readLineFromPort = oneArg' $ \val -> do
  port <- fromEgison val
  return $ makeIO $ liftIO (toEgison <$> T.hGetLine port)

readFile' :: String -> PrimitiveFunc
readFile' =  oneArg' $ \val -> do
  filename <- fromEgison val
  return $ makeIO $ liftIO (toEgison <$> T.readFile (T.unpack filename))

isEOFStdin :: String -> PrimitiveFunc
isEOFStdin = noArg $ return $ makeIO $ liftIO (Bool <$> isEOF)

isEOFPort :: String -> PrimitiveFunc
isEOFPort = oneArg' $ \val -> do
  port <- fromEgison val
  return $ makeIO $ liftIO (Bool <$> hIsEOF port)

randRange :: String -> PrimitiveFunc
randRange = twoArgs' $ \val val' -> do
  i <- fromEgison val :: EvalM Integer
  i' <- fromEgison val' :: EvalM Integer
  return $ makeIO $ liftIO (toEgison <$> getStdRandom (randomR (i, i')))

randRangeDouble :: String -> PrimitiveFunc
randRangeDouble = twoArgs' $ \val val' -> do
  i <- fromEgison val :: EvalM Double
  i' <- fromEgison val' :: EvalM Double
  return $ makeIO $ liftIO (toEgison <$> getStdRandom (randomR (i, i')))

newIORef' :: String -> PrimitiveFunc
newIORef' = noArg $ do
  ref <- liftIO $ newIORef Undefined
  return $ makeIO $ return (RefBox ref)

writeIORef' :: String -> PrimitiveFunc
writeIORef' = twoArgs $ \ref val -> do
  ref' <- fromEgison ref
  return $ makeIO' $ liftIO $ writeIORef ref' val

readIORef' :: String -> PrimitiveFunc
readIORef' = oneArg $ \ref -> do
  ref' <- fromEgison ref
  return $ makeIO $ liftIO $ readIORef ref'

readProcess' :: String -> PrimitiveFunc
readProcess' = threeArgs' $ \cmd args input -> do
  cmd'   <- T.unpack <$> fromEgison cmd
  args'  <- map T.unpack <$> fromEgison args
  input' <- T.unpack <$> fromEgison input
  return $ makeIO $ do
    outputStr <- liftIO $ readProcess cmd' args' input'
    return (String (T.pack outputStr))

io :: String -> LazyPrimitiveFunc
io = lazyOneArg io'
  where
    io' (Value (IOFunc m)) = do
      val <- m >>= evalWHNF
      case val of
        Tuple [_, val'] -> return $ Value val'
        _ -> throwErrorWithTrace (TypeMismatch "io" (Value val))
    io' whnf = throwErrorWithTrace (TypeMismatch "io" whnf)
