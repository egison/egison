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
import qualified Language.Egison.Parser         as Parser
import qualified Language.Egison.ParserNonS2    as ParserNonS2
import           Language.Egison.Primitives
import           Language.Egison.Types

main :: IO ()
main = do
  let unitTests     = map runUnitTestCase     unitTestCases
      unitNonSTests = map runUnitTestCaseNonS unitNonSTestCases
      sampleTests   = map runSampleTestCase   sampleTestCases
   in defaultMain . hUnitTestToTests . test $ unitNonSTests ++ unitTests ++ sampleTests

unitTestCases :: [FilePath]
unitTestCases =
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
  ]

unitNonSTestCases :: [FilePath]
unitNonSTestCases =
  [ "nons-test/test/syntax.egi"
  , "nons-test/test/primitive.egi"
  , "nons-test/test/lib/core/base.egi"
  , "nons-test/test/lib/core/order.egi"
  ]

sampleTestCases :: [FilePath]
sampleTestCases =
  [ "test/answer/sample/poker-hands-with-joker.egi"
  , "test/answer/sample/primes.egi"
  , "test/answer/sample/mahjong.egi"
  , "test/answer/sample/poker-hands.egi"
  , "test/answer/sample/math/geometry/riemann-curvature-tensor-of-S2.egi"
  , "test/answer/sample/math/number/17th-root-of-unity.egi"
  ]

runUnitTestCase :: FilePath -> Test
runUnitTestCase file = TestLabel file . TestCase $ do
  env <- initialEnv defaultOption
  assertEgisonM $ do
    exprs <- Parser.loadFile file
    let (bindings, tests) = foldr collectDefsAndTests ([], []) exprs
    env' <- recursiveBind env bindings
    forM_ tests $ evalExprDeep env'
  where
    assertEgisonM :: EgisonM a -> Assertion
    assertEgisonM m = fromEgisonM m >>= assertString . either show (const "")

runUnitTestCaseNonS :: FilePath -> Test
runUnitTestCaseNonS file = TestLabel file . TestCase $ do
  env <- initialEnv (defaultOption { optUseNonS2 = True })
  assertEgisonM $ do
    exprs <- ParserNonS2.loadFile file
    let (bindings, tests) = foldr collectDefsAndTests ([], []) exprs
    env' <- recursiveBind env bindings
    forM_ tests $ evalExprDeep env'
  where
    assertEgisonM :: EgisonM a -> Assertion
    assertEgisonM m = fromEgisonM m >>= assertString . either show (const "")

runSampleTestCase :: FilePath -> Test
runSampleTestCase file = TestLabel file . TestCase $ do
  env <- initialEnv defaultOption
  let directory_path = takeDirectory file
  answers <- readFile file
  assertEgisonM (lines answers) $ do
    exprs <- Parser.loadFile (replaceDirectory file $ concat $ drop 2 $ splitPath directory_path)
    let (bindings, tests) = foldr collectDefsAndTests ([], []) exprs
    env' <- recursiveBind env bindings
    vals <- forM tests (evalExprDeep env')
    return $ zip tests vals
  where
    assertEgisonM :: [String] -> EgisonM [(EgisonExpr, EgisonValue)] -> Assertion
    assertEgisonM answers m = fromEgisonM m >>= assertString . either show (f answers)

    f :: [String] -> [(EgisonExpr, EgisonValue)] -> String
    f answers ls = g answers ls 0
    g x y i = let (e, v) = unzip y in
              if (x !! i) == show (v !! i)
                 then (if i < (length y - 1) then g x y (i + 1)
                                             else "")
                 else "failed " ++ show (e !! i) ++ "\n expected: " ++ (x !! i) ++ "\n but found: " ++ show (v !! i)

collectDefsAndTests :: EgisonTopExpr -> ([(Var, EgisonExpr)], [EgisonExpr]) -> ([(Var, EgisonExpr)], [EgisonExpr])
collectDefsAndTests (Define name expr) (bindings, tests) =
  ((name, expr) : bindings, tests)
collectDefsAndTests (Test expr) (bindings, tests) =
  (bindings, expr : tests)
collectDefsAndTests _ r = r
