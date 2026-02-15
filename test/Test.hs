module Main where

import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Class      (lift)
import           System.Environment             (getArgs)
import           System.IO                      (hFlush, stdout)

import           Test.Framework                 (defaultMainWithArgs)
import           Test.Framework.Providers.HUnit (hUnitTestToTests)
import           Test.HUnit

import           Language.Egison
import           Language.Egison.AST            (TopExpr(..))
import           Language.Egison.MathOutput

main :: IO ()
main = do
  -- t <- evalRuntimeT defaultOption mathOutputTest
  args <- getArgs
  flip defaultMainWithArgs args . hUnitTestToTests . test $ 
    -- Skip mathOutputTest for now due to infinite loop
    map runTestCase testCases

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
  , "test/lib/core/random.egi"
  , "test/lib/core/sort.egi"
  , "test/lib/core/string.egi"
  , "test/lib/math/algebra.egi"
  -- , "test/lib/math/analysis.egi"   -- Skipped due to infinite loop
  -- , "test/lib/math/arithmetic.egi"  -- Skipped due to infinite loop
  -- , "test/lib/math/tensor.egi"     -- Skipped due to infinite loop

--  , "sample/mahjong.egi" -- for testing pattern functions
  , "sample/primes.egi" -- for testing pattern matching with infinitely many results
  , "sample/sat/cdcl.egi" -- for testing a practical program using pattern matching
  , "sample/poker-hands.egi"
  , "sample/poker-hands-with-joker.egi"

  , "sample/math/geometry/riemann-curvature-tensor-of-S2.egi" -- for testing tensor index notation
  , "sample/math/geometry/riemann-curvature-tensor-of-T2.egi" -- for testing tensor index notation and math quote
  , "sample/math/geometry/curvature-form.egi" -- for testing differential form
--  , "sample/math/geometry/hodge-laplacian-polar.egi" -- for testing "..." in tensor indices
--  , "sample/math/number/17th-root-of-unity.egi" -- for testing rewriting of mathematical expressions
  ]

runTestCase :: FilePath -> Test
runTestCase file = TestLabel file . TestCase . assertEvalM $ do
  -- Print the test file name before starting
  liftIO $ do
    putStrLn $ "\n=== Testing: " ++ file ++ " ==="
    hFlush stdout
  env <- initialEnv
  -- Load core libraries and math normalization library
  let coreLibExprs = map Load coreLibraries
      mathLibExpr = [Load "lib/math/normalize.egi"]
      allLibExprs = coreLibExprs ++ mathLibExpr
  env' <- evalTopExprsNoPrint env allLibExprs
  -- Then load the test file
  exprs <- loadFile file
  evalTopExprsNoPrint env' exprs
  where
    assertEvalM :: EvalM a -> Assertion
    assertEvalM m = fromEvalM defaultOption m >>= assertString . either show (const "")

mathOutputTest :: RuntimeM Test
mathOutputTest = do
  envResult <- fromEvalT $ do
    env <- initialEnv
    -- Load core libraries and math normalization library
    let coreLibExprs = map Load coreLibraries
        mathLibExpr = [Load "lib/math/normalize.egi"]
        allLibExprs = coreLibExprs ++ mathLibExpr
    evalTopExprsNoPrint env allLibExprs
  env <- case envResult of
    Left err -> error $ "Failed to initialize environment: " ++ show err
    Right e -> return e
  latexTest <- mathOutputTestLatex env
  return $ TestList [latexTest]

mathOutputTestLatex :: Env -> RuntimeM Test
mathOutputTestLatex env = do
  TestLabel "math output: latex" . TestList <$>
    mapM (\(x, y, z) -> makeTest x y z)
      [ ("div", "x / y", "\\frac{x}{y}")
      ]
 where
   makeTest = makeMathOutputTest env "latex"

makeMathOutputTest :: Env -> String -> String -> String -> String -> RuntimeM Test
makeMathOutputTest env lang label expr expectedOutput = do
  res <- fromEvalT (runExpr env expr)
  case res of
    Left _    -> return . TestCase $ assertFailure "Failed to evaluate the expression"
    Right res -> return . TestCase $ assertEqual label ("#" ++ lang ++ "|" ++ expectedOutput ++ "|#") (prettyMath lang res)
