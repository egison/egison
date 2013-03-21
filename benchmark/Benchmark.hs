{-# LANGUAGE FlexibleInstances #-}
module Main where
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error
import Control.Applicative
import Control.DeepSeq (NFData(rnf))
import Language.Egison.Types
import Language.Egison.Core
import Language.Egison.Primitives
import Criterion.Main
import Text.Parsec
import Text.Parsec.ByteString

loadLibraries :: Env -> IO Env
loadLibraries env = do
  result <- runErrorT $ runEgisonM $ foldM evalTopExpr env (map Load libraries)
  case result of
    Left err -> do
      print . show $ err
      return env
    Right env' -> 
      return env'
  where
    libraries :: [String]
    libraries = [ "lib/core/base.egi"
                , "lib/core/number.egi"
                , "lib/core/collection.egi"
                , "lib/core/pattern.egi" ]

runEgisonFile :: String -> IO ()
runEgisonFile path = do
  env <- primitiveEnv >>= loadLibraries
  result <- runErrorT . runEgisonM $ evalTopExpr env (LoadFile path)
  either print (const $ return ()) $ result

main :: IO ()
main = defaultMain [
    bgroup "fact" [ bench "30000" $ runEgisonFile "benchmark/fact-30000.egi" ]
  ]

