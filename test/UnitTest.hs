module Main where

import Control.Applicative
import Control.Monad
import Data.IORef

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit
import System.FilePath.Glob (glob)

import Language.Egison.Types
import Language.Egison.Core
import Language.Egison.Primitives
import Language.Egison

main = do
  testCases <- glob "test/**/*.egi"
  defaultMain $ hUnitTestToTests $ test $ map runTestCase testCases

runTestCase :: FilePath -> Test
runTestCase file = TestLabel file . TestCase $ do 
  env <- primitiveEnv >>= loadLibraries
  seed <- readIORef counter
  (result, seed') <- runFreshT seed $ runEgisonM $ loadFile file >>= evalTopExprs' env
  writeIORef counter seed'
  assertString $ either show (const "") result

evalTopExprs' :: Env -> [EgisonTopExpr] -> EgisonM ()
evalTopExprs' env exprs = do
  let (bindings, tests) = foldr collectDefsAndTests ([], []) exprs
  env <- recursiveBind env bindings
  forM_ tests $ evalExpr' env
 where
  collectDefsAndTests (Define name expr) (bindings, tests) =
    ((name, expr) : bindings, tests)
  collectDefsAndTests (Test expr) (bindings, tests) =
    (bindings, expr : tests)
  collectDefsAndTests _ r = r
