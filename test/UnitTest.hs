module Main where

import Control.Monad

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit

import Language.Egison.Types
import Language.Egison.Core
import Language.Egison.Primitives
import Language.Egison

main = defaultMain $ hUnitTestToTests $ test $ map runTestCase testCases

testCases :: [FilePath]
testCases = [ "test/collection-test.egi"
            , "test/pattern-match-test.egi" ]

runTestCase :: FilePath -> Test
runTestCase file = TestLabel file . TestCase $ do 
  env <- primitiveEnv >>= loadLibraries
  result <- runEgisonM $ loadFile file >>= evalTopExprs' env
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
