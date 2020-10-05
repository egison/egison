{-# LANGUAGE LambdaCase #-}

module Language.Egison.Primitives.IO
  ( ioPrimitives
  ) where

import           Control.Monad.Except

import           Data.IORef

import           System.IO
import           System.Process            (readProcess)
import           System.Random             (getStdRandom, randomR)

import qualified Data.Text                 as T
import qualified Data.Text.IO              as T

import           Language.Egison.Data
import           Language.Egison.Primitives.Utils


--
-- IO Primitives
--

ioPrimitives :: [(String, String -> PrimitiveFunc)]
ioPrimitives =
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
readChar = noArg $ return $ makeIO $ liftIO $ fmap Char getChar

readCharFromPort :: String -> PrimitiveFunc
readCharFromPort = oneArg' $ \val -> do
  port <- fromEgison val
  c <- liftIO $ hGetChar port
  return $ makeIO $ return (Char c)

readLine :: String -> PrimitiveFunc
readLine = noArg $ return $ makeIO $ liftIO $ fmap toEgison T.getLine

readLineFromPort :: String -> PrimitiveFunc
readLineFromPort = oneArg' $ \val -> do
  port <- fromEgison val
  s <- liftIO $ T.hGetLine port
  return $ makeIO $ return $ toEgison s

readFile' :: String -> PrimitiveFunc
readFile' =  oneArg' $ \val -> do
  filename <- fromEgison val
  s <- liftIO $ T.readFile $ T.unpack filename
  return $ makeIO $ return $ toEgison s

isEOFStdin :: String -> PrimitiveFunc
isEOFStdin = noArg $ return $ makeIO $ liftIO $ fmap Bool isEOF

isEOFPort :: String -> PrimitiveFunc
isEOFPort = oneArg' $ \val -> do
  port <- fromEgison val
  b <- liftIO $ hIsEOF port
  return $ makeIO $ return (Bool b)

randRange :: String -> PrimitiveFunc
randRange = twoArgs' $ \val val' -> do
  i <- fromEgison val :: EvalM Integer
  i' <- fromEgison val' :: EvalM Integer
  n <- liftIO $ getStdRandom $ randomR (i, i')
  return $ makeIO $ return $ toEgison n

randRangeDouble :: String -> PrimitiveFunc
randRangeDouble = twoArgs' $ \val val' -> do
  i <- fromEgison val :: EvalM Double
  i' <- fromEgison val' :: EvalM Double
  n <- liftIO $ getStdRandom $ randomR (i, i')
  return $ makeIO $ return $ toEgison n

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
  val <- liftIO $ readIORef ref'
  return $ makeIO $ return val

readProcess' :: String -> PrimitiveFunc
readProcess' = threeArgs' $ \cmd args input -> do
  cmd'   <- T.unpack <$> fromEgison cmd
  args'  <- map T.unpack <$> fromEgison args
  input' <- T.unpack <$> fromEgison input
  return $ makeIO $ do
    outputStr <- liftIO $ readProcess cmd' args' input'
    return (String (T.pack outputStr))
