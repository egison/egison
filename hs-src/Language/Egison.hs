
module Language.Egison
       ( module Language.Egison.Types
       , module Language.Egison.Parser
       , module Language.Egison.Primitives
       , version
       , counter --danger
       , fromEgisonM
       , loadLibraries
       , loadPrimitives
       , loadEgisonFile
       , loadEgisonLibrary
       , evalEgisonExpr
       , evalEgisonTopExpr
       , evalEgisonTopExprs
       , runEgisonTopExpr
       , runEgisonTopExprs
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Data.Version
import qualified Paths_egison as P

import Language.Egison.Types
import Language.Egison.Parser
import Language.Egison.Primitives
import Language.Egison.Core

version :: Version
version = P.version

-- Unsafe
counter :: IORef Int
counter = unsafePerformIO (newIORef 0)

readCounter :: IO Int
readCounter = readIORef counter

updateCounter :: Int -> IO ()
updateCounter = writeIORef counter

modifyCounter :: FreshT IO a -> IO a
modifyCounter m = do
  seed <- readCounter
  (result, seed) <- runFreshT seed m 
  updateCounter seed
  return result  

loadLibraries :: Env -> IO Env
loadLibraries env = do
  seed <- readIORef counter
  (result, seed') <- runFreshT seed $ runEgisonM $ foldM evalTopExpr env (map Load libraries)
  writeIORef counter seed'
  case result of
    Left err -> do
      print . show $ err
      return env
    Right env' -> 
      return env'
  where
    libraries :: [String]
    libraries = [ "lib/core/base.egi"
                , "lib/core/collection.egi"
                , "lib/core/number.egi"
                 ]

fromEgisonM :: EgisonM a -> IO (Either EgisonError a)
fromEgisonM = modifyCounter . runEgisonM

loadPrimitives :: Env -> IO Env
loadPrimitives env = (++) <$> return env <*> primitiveEnv
  
loadEgisonFile :: Env -> FilePath -> IO (Either EgisonError Env)
loadEgisonFile env path = modifyCounter $ runEgisonM $ loadFile path >>= evalTopExprs env

loadEgisonLibrary :: Env -> FilePath -> IO (Either EgisonError Env)
loadEgisonLibrary env path = modifyCounter $ runEgisonM $ loadLibraryFile path >>= evalTopExprs env

evalEgisonExpr :: Env -> EgisonExpr -> IO (Either EgisonError EgisonValue)
evalEgisonExpr env expr = modifyCounter $ runEgisonM $ evalExpr' env expr

evalEgisonTopExpr :: Env -> EgisonTopExpr -> IO (Either EgisonError Env)
evalEgisonTopExpr env exprs = modifyCounter $ runEgisonM $ evalTopExpr env exprs

evalEgisonTopExprs :: Env -> [EgisonTopExpr] -> IO (Either EgisonError Env)
evalEgisonTopExprs env exprs = modifyCounter $ runEgisonM $ evalTopExprs env exprs

runEgisonTopExpr :: Env -> String -> IO (Either EgisonError Env)
runEgisonTopExpr env input = modifyCounter $ runEgisonM $ readTopExpr input >>= evalTopExpr env

runEgisonTopExprs :: Env -> String -> IO (Either EgisonError Env)
runEgisonTopExprs env input = modifyCounter $ runEgisonM $ readTopExprs input >>= evalTopExprs env
