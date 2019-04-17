{-# LANGUAGE FlexibleInstances #-}
module Main where
import           Control.Applicative ((<$>), (<*>))
import           Control.Applicative
import           Control.DeepSeq     (NFData (rnf))
import           Control.Monad.Error
import           Criterion.Config
import           Criterion.Main
import           Language.Egison

runEgisonFile :: String -> IO (Either EgisonError Env)
runEgisonFile path = loadPrimitives nullEnv >>= loadLibraries >>= flip (loadEgisonFile defaultOption) path

config :: Config
config = defaultConfig { cfgSamples   = ljust 3
                       , cfgPerformGC = ljust True }

main :: IO ()
main = defaultMainWith config (return ())
       [ bgroup "fact" [ bench "30000" $ runEgisonFile "benchmark/fact-30000.egi" ]
       , bgroup "collection"
         [ bench "cons-bench" $ runEgisonFile "benchmark/collection-bench-cons.egi"
         , bench "cons-bench-large" $ runEgisonFile "benchmark/collection-bench-cons-large.egi"
         , bench "snoc-bench" $ runEgisonFile "benchmark/collection-bench-snoc.egi"
         ]
       ]

