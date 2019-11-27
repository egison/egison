module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.List

import           System.FilePath                (replaceDirectory, splitPath,
                                                 takeDirectory)
import           System.FilePath.Glob           (glob)
import           Test.Framework                 (defaultMain)
import           Test.Framework.Providers.HUnit (hUnitTestToTests)
import           Test.HUnit

import           Language.Egison
import           Language.Egison.Core
import           Language.Egison.CmdOptions
import qualified Language.Egison.Parser         as Parser
import qualified Language.Egison.ParserNonS     as ParserNonS
import           Language.Egison.Pretty
import           Language.Egison.Primitives
import           Language.Egison.Types

main :: IO ()
main =
  defaultMain . hUnitTestToTests . test $ nonSTests ++ sExprTests
  where
    sExprTests = map runTestCase     testCases
    nonSTests  = map runTestCaseNonS nonSTestCases

testCases :: [FilePath]
testCases =
  [ "test/syntax.egi"
  , "test/primitive.egi"
  , "test/lib/math/analysis.egi"
  , "test/lib/math/tensor.egi"
  , "test/lib/math/arithmetic.egi"
  , "test/lib/math/algebra.egi"
  , "test/lib/core/string.egi"
  , "test/lib/core/base.egi"
  , "test/lib/core/collection.egi"
  , "test/lib/core/order.egi"
  , "test/lib/core/number.egi"

  , "sample/mahjong.egi"
  , "sample/poker-hands-with-joker.egi"
  , "sample/poker-hands.egi"
  , "sample/primes.egi"
  , "sample/sat/cdcl.egi"
  , "sample/math/number/17th-root-of-unity.egi"
  , "sample/math/geometry/riemann-curvature-tensor-of-S2.egi"
  , "sample/math/geometry/riemann-curvature-tensor-of-T2.egi"
  , "sample/math/geometry/curvature-form.egi"
  ]

nonSTestCases :: [FilePath]
nonSTestCases =
  [ "nons-test/test/syntax.egi"
  , "nons-test/test/primitive.egi"
  , "nons-test/test/lib/core/base.egi"
  , "nons-test/test/lib/core/order.egi"
  ]

runTestCase :: FilePath -> Test
runTestCase file = TestLabel file . TestCase $ do
  env <- initialEnv defaultOption
  assertEgisonM $ do
    exprs <- Parser.loadFile file
    let (bindings, tests) = foldr collectDefsAndTests ([], []) exprs
    env' <- recursiveBind env bindings
    forM_ tests $ evalExprDeep env'
  where
    assertEgisonM :: EgisonM a -> Assertion
    assertEgisonM m = fromEgisonM m >>= assertString . either show (const "")

runTestCaseNonS :: FilePath -> Test
runTestCaseNonS file = TestLabel file . TestCase $ do
  env <- initialEnv (defaultOption { optSExpr = False })
  assertEgisonM $ do
    exprs <- ParserNonS.loadFile file
    let (bindings, tests) = foldr collectDefsAndTests ([], []) exprs
    env' <- recursiveBind env bindings
    forM_ tests $ evalExprDeep env'
  where
    assertEgisonM :: EgisonM a -> Assertion
    assertEgisonM m = fromEgisonM m >>= assertString . either show (const "")

collectDefsAndTests :: EgisonTopExpr -> ([(Var, EgisonExpr)], [EgisonExpr]) -> ([(Var, EgisonExpr)], [EgisonExpr])
collectDefsAndTests (Define name expr) (bindings, tests) =
  ((name, expr) : bindings, tests)
collectDefsAndTests (Test expr) (bindings, tests) =
  (bindings, expr : tests)
collectDefsAndTests _ r = r
