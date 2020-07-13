module Main where

import           Criterion
import           Criterion.Main
import           Language.Egison

runEgisonFile :: String -> IO ()
runEgisonFile path = evalRuntimeT defaultOption $ do
  env <- initialEnv
  _ <- loadEgisonFile env path
  return ()

main :: IO ()
main = defaultMainWith defaultConfig
         [ bgroup "fact"
           [ bench "30000" $ nfIO $ runEgisonFile "benchmark/fact-30000.egi" ]
         , bgroup "collection"
           [ bench "cons-bench" $ nfIO $ runEgisonFile "benchmark/collection-bench-cons.egi"
           , bench "cons-bench-large" $ nfIO $ runEgisonFile "benchmark/collection-bench-cons-large.egi"
           , bench "snoc-bench" $ nfIO $ runEgisonFile "benchmark/collection-bench-snoc.egi"
           ]]

