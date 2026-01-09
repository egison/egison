module Main where

import           Control.Monad.Trans.Class      (lift)
import           System.Environment             (getArgs)

import           Test.Framework                 (defaultMainWithArgs)
import           Test.Framework.Providers.HUnit (hUnitTestToTests)
import           Test.HUnit

import           Language.Egison
import           Language.Egison.MathOutput

main :: IO ()
main = do
  t <- evalRuntimeT defaultOption mathOutputTest
  args <- getArgs
  flip defaultMainWithArgs args . hUnitTestToTests . test $ 
    t : runPatternEnvDumpTest : map runTestCase testCases

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
  , "test/lib/core/shell.egi"
  , "test/lib/core/sort.egi"
  , "test/lib/core/string.egi"
  , "test/lib/math/algebra.egi"
  , "test/lib/math/analysis.egi"
  , "test/lib/math/arithmetic.egi"
  , "test/lib/math/tensor.egi"
  , "test/lib/type/basic.egi"      -- for testing typed functions
  , "test/lib/type/infer.egi"      -- for testing type inference of various expressions
  , "test/lib/type/typeclass.egi"  -- for testing type class (class/instance declarations)
  , "test/lib/type/pattern-env.egi" -- for testing pattern inductive and pattern function environment

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
  evalTopExprsNoPrint env exprs
  where
    assertEvalM :: EvalM a -> Assertion
    assertEvalM m = fromEvalM defaultOption m >>= assertString . either show (const "")

-- Test case for pattern environment dump
runPatternEnvDumpTest :: Test
runPatternEnvDumpTest = TestLabel "pattern-env-dump" . TestCase . assertEvalMWithDump $ do
  env <- lift $ lift initialEnv
  exprs <- loadFile "mini-test/pattern-env-dump.egi"
  evalTopExprsNoPrint env exprs
  where
    assertEvalMWithDump :: EvalM a -> Assertion
    assertEvalMWithDump m = fromEvalM (defaultOption { optDumpEnv = True }) m >>= assertString . either show (const "")

mathOutputTest :: RuntimeM Test
mathOutputTest = do
  env <- initialEnv
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
