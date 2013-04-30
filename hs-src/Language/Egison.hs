
module Language.Egison
       ( module Language.Egison.Types
       , module Language.Egison.Parser
       , module Language.Egison.Primitives
       , version
       , loadLibraries
       , loadPrimitives
       , runEgisonTopExpr
       , evalEgisonTopExpr
       , evalEgisonTopExprs
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Data.Version
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import qualified Paths_egison as P

import Language.Egison.Types
import Language.Egison.Core
import Language.Egison.Parser
import Language.Egison.Primitives

-- Unsafe
counter :: IORef Int
counter = unsafePerformIO (newIORef 0)


version :: Version
version = P.version

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
                , "lib/core/pattern.egi" ]
                
loadPrimitives :: Env -> IO Env
loadPrimitives env = (++) <$> return env <*> primitiveEnv

runEgisonTopExpr :: Env -> String -> IO (Either EgisonError Env)
runEgisonTopExpr env input = do
  seed <- readIORef counter
  (result, seed') <- runFreshT seed $ runEgisonM $ do 
    expr <- liftEgisonM $ readTopExpr input
    evalTopExpr env expr
  writeIORef counter seed'
  return result

evalEgisonTopExpr :: Env -> EgisonTopExpr -> IO (Either EgisonError Env)
evalEgisonTopExpr env expr = do
  seed <- readIORef counter
  (result, seed') <- runFreshT seed $ runEgisonM $ evalTopExpr env expr
  writeIORef counter seed'
  return result

evalEgisonTopExprs :: Env -> [EgisonTopExpr] -> IO (Either EgisonError Env)
evalEgisonTopExprs env exprs = do
  seed <- readIORef counter
  (result, seed') <- runFreshT seed $ runEgisonM $ foldM evalTopExpr env exprs
  writeIORef counter seed'
  return result
