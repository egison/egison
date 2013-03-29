{-# LANGUAGE FlexibleInstances #-}
module Main where
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error
import Control.Applicative
import Control.DeepSeq (NFData(rnf))
import Language.Egison
import Criterion.Main
import Text.Parsec
import Text.Parsec.ByteString

runEgisonFile :: String -> IO (Either EgisonError Env)
runEgisonFile path = do
  env <- loadPrimitives nullEnv >>= loadLibraries
  evalEgisonTopExpr env $ LoadFile path

main :: IO ()
main = defaultMain [
    bgroup "fact" [ bench "30000" $ runEgisonFile "benchmark/fact-30000.egi" ]
  ]

