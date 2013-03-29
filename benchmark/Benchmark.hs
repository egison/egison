{-# LANGUAGE FlexibleInstances #-}
module Main where
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error
import Control.Applicative
import Control.DeepSeq (NFData(rnf))
import Criterion.Main
import Criterion.Config
import Text.Parsec
import Text.Parsec.ByteString

import Language.Egison

runEgisonFile :: String -> IO (Either EgisonError Env)
runEgisonFile path = do
  env <- loadPrimitives nullEnv >>= loadLibraries
  evalEgisonTopExpr env $ LoadFile path

config :: Config
config = defaultConfig { cfgSamples   = ljust 3 
                       , cfgPerformGC = ljust True }

main :: IO ()
main = defaultMainWith config (return ()) [
    bgroup "fact" [ bench "30000" $ runEgisonFile "benchmark/fact-30000.egi" ]
  ]

