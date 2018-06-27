module Main where

import Control.Applicative
import Control.Monad
import Data.IORef

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit
import System.FilePath.Glob (glob)
import System.FilePath (takeDirectory, replaceDirectory)

import Language.Egison.Types
import Language.Egison.Core
import Language.Egison.Primitives
import Language.Egison

import UnitTest

main = do
  unitTestCases <- glob "test/**/*.egi"
  defaultMain $ hUnitTestToTests $ test $ map runUnitTestCase unitTestCases
  sampleTestCases <- glob "sample/*.egi"
  defaultMain $ hUnitTestToTests $ test $ map runSampleTestCase sampleTestCases

runSampleTestCase :: FilePath -> Test
runSampleTestCase file = TestLabel file . TestCase $ do
  env <- initialEnv
  let directory_path = takeDirectory file
  answers <- readFile (replaceDirectory file ("test/answer/" ++ directory_path))
  assertEgisonM (lines answers) $ do
    exprs <- loadFile file
    let (bindings, tests) = foldr collectDefsAndTests ([], []) exprs
    env' <- recursiveBind env bindings
    forM tests $ evalExprDeep env'
      where
        assertEgisonM :: [String] -> EgisonM [EgisonValue] -> Assertion
        assertEgisonM answers m = fromEgisonM m >>= assertString . either show (f answers)
    
        collectDefsAndTests (Define name expr) (bindings, tests) =
          ((stringToVar $ show name, expr) : bindings, tests)
        collectDefsAndTests (Test expr) (bindings, tests) =
          (bindings, expr : tests)
        collectDefsAndTests _ r = r

        f :: [String] -> [EgisonValue] -> String
        f answers ls = g answers ls 0
        g x y i = if (x !! i) == show (y !! i) 
                     then (if i < (length y - 1) then g x y (i + 1)
                                                 else "")
                     else "failed " ++ show i ++ "\n expected: " ++ (x !! i) ++ "\n but found: " ++ show (y !! i)
