module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.List

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit
import System.FilePath.Glob (glob)
import System.FilePath (takeDirectory, replaceDirectory, splitPath)

import Language.Egison.Types
import Language.Egison.Core
import Language.Egison.Primitives
import Language.Egison

import UnitTest

main = do
  unitTestCases <- glob "test/[^answer]**/*.egi"
  sampleTestCases <- glob "test/answer/**/*.egi"
  defaultMain $ hUnitTestToTests $ test $ map runUnitTestCase unitTestCases ++ map runSampleTestCase sampleTestCases

runSampleTestCase :: FilePath -> Test
runSampleTestCase file = TestLabel file . TestCase $ do
  env <- initialEnv
  let directory_path = takeDirectory file
  answers <- readFile file
  assertEgisonM (lines answers) $ do
    exprs <- loadFile (replaceDirectory file $ concat $ drop 2 $ splitPath directory_path)
    let (bindings, tests) = foldr collectDefsAndTests ([], []) exprs
    env' <- recursiveBind env bindings
    vals <- forM tests (evalExprDeep env')
    return $ zip tests vals
      where
        assertEgisonM :: [String] -> EgisonM [(EgisonExpr, EgisonValue)] -> Assertion
        assertEgisonM answers m = fromEgisonM m >>= assertString . either show (f answers)
    
        collectDefsAndTests (Define name expr) (bindings, tests) =
          ((name, expr) : bindings, tests)
        collectDefsAndTests (Test expr) (bindings, tests) =
          (bindings, expr : tests)
        collectDefsAndTests _ r = r

        f :: [String] -> [(EgisonExpr, EgisonValue)] -> String
        f answers ls = g answers ls 0
        g x y i = let (e, v) = unzip y in
                  if (x !! i) == show (v !! i)
                     then (if i < (length y - 1) then g x y (i + 1)
                                                 else "")
                     else "failed " ++ show (e !! i) ++ "\n expected: " ++ (x !! i) ++ "\n but found: " ++ show (v !! i)
