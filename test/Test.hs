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
  [ "test/syntax.segi"
  , "test/primitive.segi"
  , "test/lib/math/tensor.segi"

  , "sexpr-sample/poker-hands.segi"
  , "sexpr-sample/poker-hands-with-joker.segi"
  , "sexpr-sample/mahjong.segi" -- for testing pattern functions
  , "sexpr-sample/primes.segi" -- for testing pattern matching with infinitely many results
  , "sexpr-sample/sat/cdcl.segi" -- for testing a practical program using pattern matching
  , "sexpr-sample/math/number/17th-root-of-unity.segi" -- for testing rewriting of mathematical expressions
--  , "sexpr-sample/math/geometry/riemann-curvature-tensor-of-S2.segi" -- for testing tensor index notation
  , "sexpr-sample/math/geometry/riemann-curvature-tensor-of-T2.segi" -- for testing tensor index notation and math quote
--  , "sexpr-sample/math/geometry/curvature-form.segi" -- for testing differential form
--  , "sexpr-sample/math/geometry/hodge-laplacian-polar.segi" -- for testing "..." in tensor indices
  ]

nonSTestCases :: [FilePath]
nonSTestCases =
  [ "nons-test/test/syntax.egi"
  , "nons-test/test/primitive.egi"
  , "nons-test/test/lib/core/base.egi"
  , "nons-test/test/lib/core/collection.egi"
  , "nons-test/test/lib/core/maybe.egi"
  , "nons-test/test/lib/core/number.egi"
  , "nons-test/test/lib/core/order.egi"
  , "nons-test/test/lib/core/string.egi"
  , "nons-test/test/lib/math/algebra.egi"
  , "nons-test/test/lib/math/analysis.egi"
  , "nons-test/test/lib/math/arithmetic.egi"

  , "sample/math/geometry/riemann-curvature-tensor-of-S2.egi"
  , "sample/math/geometry/curvature-form.egi"
  , "sample/math/geometry/hodge-laplacian-polar.egi" -- for testing "..." in tensor indices
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
