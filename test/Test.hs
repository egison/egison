module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class      (lift)
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
import           Language.Egison.Parser
import           Language.Egison.Pretty
import           Language.Egison.Primitives
import           Language.Egison.Types

main :: IO ()
main =
  defaultMain . hUnitTestToTests . test $ map runTestCase testCases

testCases :: [FilePath]
testCases =
  [ "test/syntax.egi"
  , "test/primitive.egi"
  , "test/lib/core/assoc.egi"
  , "test/lib/core/base.egi"
  , "test/lib/core/collection.egi"
  , "test/lib/core/maybe.egi"
  , "test/lib/core/number.egi"
  , "test/lib/core/order.egi"
  , "test/lib/core/string.egi"
  , "test/lib/math/algebra.egi"
  , "test/lib/math/analysis.egi"
  , "test/lib/math/arithmetic.egi"
  , "test/lib/math/tensor.egi"

  , "sample/mahjong.egi" -- for testing pattern functions
  , "sample/primes.egi" -- for testing pattern matching with infinitely many results
  , "sample/sat/cdcl.egi" -- for testing a practical program using pattern matching
  , "sample/poker-hands.egi"
  , "sample/poker-hands-with-joker.egi"

  , "sample/math/geometry/riemann-curvature-tensor-of-S2.egi" -- for testing tensor index notation
  , "sample/math/geometry/riemann-curvature-tensor-of-T2.egi" -- for testing tensor index notation and math quote
  , "sample/math/geometry/curvature-form.egi" -- for testing differential form
  , "sample/math/geometry/hodge-laplacian-polar.egi" -- for testing "..." in tensor indices
  , "sample/math/number/17th-root-of-unity.egi" -- for testing rewriting of mathematical expressions
  ]

runTestCase :: FilePath -> Test
runTestCase file = TestLabel file . TestCase . assertEvalM $ do
  env <- lift $ lift initialEnv
  exprs <- loadFile file
  let (bindings, tests) = foldr collectDefsAndTests ([], []) exprs
  env' <- recursiveBind env bindings
  forM_ tests $ evalExprDeep env'
 where
  assertEvalM :: EvalM a -> Assertion
  assertEvalM m = fromEvalM defaultOption m >>= assertString . either show (const "")

collectDefsAndTests :: EgisonTopExpr -> ([(Var, EgisonExpr)], [EgisonExpr]) -> ([(Var, EgisonExpr)], [EgisonExpr])
collectDefsAndTests (Define name expr) (bindings, tests) =
  ((name, expr) : bindings, tests)
collectDefsAndTests (Test expr) (bindings, tests) =
  (bindings, expr : tests)
collectDefsAndTests _ r = r
