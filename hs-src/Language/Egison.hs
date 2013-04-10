
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

version :: Version
version = P.version

loadLibraries :: Env -> IO Env
loadLibraries env = do
  result <- runEgisonM $ foldM evalTopExpr env (map Load libraries)
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
runEgisonTopExpr env input = runEgisonM $ do 
  expr <- liftError $ readTopExpr input
  evalTopExpr env expr

evalEgisonTopExpr :: Env -> EgisonTopExpr -> IO (Either EgisonError Env)
evalEgisonTopExpr env expr = runEgisonM $ evalTopExpr env expr

evalEgisonTopExprs :: Env -> [EgisonTopExpr] -> IO (Either EgisonError Env)
evalEgisonTopExprs env exprs = runEgisonM $ foldM evalTopExpr env exprs
