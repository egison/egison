module Main where

import           Control.Monad.Trans.Class (lift)
import           Criterion
import           Criterion.Main

import           Language.Egison

runEgisonFile :: FilePath -> IO Env
runEgisonFile path = do
  res <- fromEvalM defaultOption $ do
    env <- lift (lift initialEnv)
    topExprs <- loadFile path
    evalTopExprsNoPrint env topExprs
  case res of
    Left err  -> do
      print err
      return nullEnv
    Right env -> return env

main :: IO ()
main = defaultMainWith defaultConfig
         [ bgroup "fact"
           [ bench "30000"            $ whnfIO $ runEgisonFile "benchmark/fact-30000.egi" ]
         , bgroup "collection"
           [ bench "cons-bench"       $ whnfIO $ runEgisonFile "benchmark/collection-bench-cons.egi"
           , bench "cons-bench-large" $ whnfIO $ runEgisonFile "benchmark/collection-bench-cons-large.egi"
           , bench "snoc-bench"       $ whnfIO $ runEgisonFile "benchmark/collection-bench-snoc.egi"
           ]
         ]
