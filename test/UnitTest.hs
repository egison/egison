module UnitTest ( runUnitTestCase ) where

import Control.Applicative
import Control.Monad
import Data.IORef

import Test.HUnit

import Language.Egison.Types
import Language.Egison.Core
import Language.Egison.Primitives
import Language.Egison

runUnitTestCase :: FilePath -> Test
runUnitTestCase file = TestLabel file . TestCase $ do
  env <- initialEnv
  assertEgisonM $ do
    exprs <- loadFile file
    let (bindings, tests) = foldr collectDefsAndTests ([], []) exprs
    env' <- recursiveBind env bindings
    forM_ tests $ evalExprDeep env'
      where
        assertEgisonM :: EgisonM a -> Assertion
        assertEgisonM m = fromEgisonM m >>= assertString . either show (const "")
    
        collectDefsAndTests (Define name expr) (bindings, tests) =
          ((name, expr) : bindings, tests)
        collectDefsAndTests (Test expr) (bindings, tests) =
          (bindings, expr : tests)
        collectDefsAndTests _ r = r
