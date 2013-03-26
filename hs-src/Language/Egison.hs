
module Language.Egison
       ( module Language.Egison.Types
       , module Language.Egison.Parser
       , module Language.Egison.Core
       , module Language.Egison.Primitives
       , version
       , loadLibraries
       , loadPrimitives
       , runEgisonTopExpr
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

runParser' :: Parser a -> String -> Either EgisonError a
runParser' parser input = either (throwError . Parser) return $ parse parser "egison" (B.pack input)

runEgisonTopExpr :: Env -> String -> IO (Either EgisonError Env)
runEgisonTopExpr env input = runEgisonM $ do 
  expr <- liftError $ runParser' parseTopExpr input
  evalTopExpr env expr
