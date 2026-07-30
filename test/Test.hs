module Main where

import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Except           (catchError)
import           Data.List                      (isInfixOf, sort, (\\))
import qualified Data.Map.Strict                as Map
import           System.Environment             (getArgs)
import           System.FilePath.Glob           (glob)
import           System.IO                      (hFlush, stdout)

import           Test.Framework                 (defaultMainWithArgs)
import           Test.Framework.Providers.HUnit (hUnitTestToTests)
import           Test.HUnit

import           Language.Egison
import           Language.Egison.IExpr          (Var (..))
import qualified Language.Egison.Type.Capability as Capability
import           Language.Egison.Type.Env       (emptyPatternEnv,
                                                  extendPatternEnv,
                                                  lookupEnvExact)
import           Language.Egison.Type.Subst     (applyCapSubstToType,
                                                  applyTypeSubst,
                                                  singletonCapSubst,
                                                  singletonSubst)
import qualified Language.Egison.Type.ShapeSolver as ShapeSolver
import           Language.Egison.Type.Types     (CapVar (..),
                                                  Capability (..),
                                                  TypeScheme (..),
                                                  TyVar (..), Type (..),
                                                  mkTypeFormer)
import           Language.Egison.Type.Unify     (matchOneWay, unifyCapability)

main :: IO ()
main = do
  args <- getArgs
  libTests <- discoverLibTests
  mapM_ (\(f, why) -> putStrLn ("Skipping " ++ f ++ " (" ++ why ++ ")"))
        skippedLibTests
  flip defaultMainWithArgs args . hUnitTestToTests . test $
    p2CapabilityTests
      ++ [ matcherOneWayTests
         , strictPipelineTests
         , failedInferAtomicityTests
         , producerCyclePersistenceTests
         ]
      ++ map runTestCase (languageTests ++ libTests ++ sampleTests)

-- | Pure regressions for the two-sort P2 representation and evidence
-- calculus.  Language-level acceptance/rejection cases live in
-- test/lib/core/p2-capability.egi and test/type-error respectively.
p2CapabilityTests :: [Test]
p2CapabilityTests =
  [ TestLabel "P2: type substitution does not enter capability" . TestCase $ do
      let typeVariable = TyVar "a"
          capabilityVariable = MkCapVar "p"
          original =
            TMatcher
              (CapCon (mkTypeFormer "Collection" 1)
                [CapVar capabilityVariable])
              (TCollection (TVar typeVariable))
          substituted =
            applyTypeSubst
              (singletonSubst typeVariable TInt)
              original
      assertEqual
        "ordinary substitution must change only the matcher target"
        (TMatcher
          (CapCon (mkTypeFormer "Collection" 1)
            [CapVar capabilityVariable])
          (TCollection TInt))
        substituted

  , TestLabel "P2: capability substitution reaches nested matchers" . TestCase $ do
      let capabilityVariable = MkCapVar "p"
          original =
            TCollection
              (TFun TInt
                (TMatcher
                  (CapCon (mkTypeFormer "Maybe" 1)
                    [CapVar capabilityVariable])
                  (TInductive "Maybe" [TInt])))
          substituted =
            applyCapSubstToType
              (singletonCapSubst capabilityVariable CapNone)
              original
      assertEqual
        "capability substitution must traverse the complete ordinary type"
        (TCollection
          (TFun TInt
            (TMatcher
              (CapCon (mkTypeFormer "Maybe" 1) [CapNone])
              (TInductive "Maybe" [TInt]))))
        substituted

  , TestLabel "P2: exact evidence preserves variable identity" . TestCase $ do
      let p = CapVar (MkCapVar "p")
          q = CapVar (MkCapVar "q")
      assertEqual
        "the same producer variable is exact evidence"
        (Right (Capability.CapKnown p))
        (Capability.mergeCapEvidence
          (Capability.CapKnown p)
          (Capability.CapKnown p))
      case Capability.mergeCapEvidence
             (Capability.CapKnown p)
             (Capability.CapKnown q) of
        Left _  -> return ()
        Right _ ->
          assertFailure "different producer variables must not be unified by exact merge"

  , TestLabel "P2: observability uses the least declaration fixpoint" . TestCase $ do
      let a = TyVar "a"
          tree = TInductive "Tree" [TVar a]
          nodeOnly =
            extendPatternEnv "node"
              (Forall [] [a] [] (TFun tree tree))
              emptyPatternEnv
          withLeaf =
            extendPatternEnv "leaf"
              (Forall [] [a] [] (TFun (TVar a) tree))
              nodeOnly
          treeFormer = mkTypeFormer "Tree" 1
      nodeLookup <-
        either assertFailure return
          (Capability.observabilityLookup nodeOnly)
      leafLookup <-
        either assertFailure return
          (Capability.observabilityLookup withLeaf)
      assertEqual
        "a recursive-only occurrence is not an observability seed"
        (Just [False])
        (nodeLookup treeFormer)
      assertEqual
        "a direct field occurrence seeds the recursive parameter"
        (Just [True])
        (leafLookup treeFormer)

  , TestLabel "P2: signature projection follows result-slot order" . TestCase $ do
      let a = TyVar "a"
          b = TyVar "b"
          p = CapVar (MkCapVar "p")
          q = CapVar (MkCapVar "q")
          former = mkTypeFormer "Swap" 2
          observable requested
            | requested == former = Just [True, True]
            | otherwise           = Nothing
      projected <-
        either assertFailure return
          (Capability.projectConstructorEvidence
            observable
            [TVar a, TVar b]
            (TInductive "Swap" [TVar b, TVar a])
            [Capability.CapKnown p, Capability.CapKnown q])
      assertEqual
        "field order a,b must project into result order b,a"
        (Capability.CapConEvidence former
          [Capability.CapKnown q, Capability.CapKnown p])
        projected

  , TestLabel "P2: CapTargetOK is canonical and context-relative" . TestCase $ do
      let p = CapVar (MkCapVar "p")
          a = TVar (TyVar "a")
          collectionP =
            CapCon (mkTypeFormer "Collection" 1) [p]
      assertBool
        "an actual input matcher/slot supplies the open correspondence"
        (Capability.capTargetOK
          [(p, a)]
          collectionP
          (TCollection a))
      assertBool
        "a bare open pair has no correspondence without an input value"
        (not (Capability.capTargetOK
          []
          collectionP
          (TCollection a)))
      assertBool
        "CAS ground equivalence must not change canonical capability heads"
        (not (Capability.capTargetOK
          []
          (CapCon (mkTypeFormer "MathValue" 0) [])
          TFactor))

  , TestLabel "P2: malformed capability arity is rejected internally" .
      TestCase $ do
        let malformed =
              CapCon (mkTypeFormer "Collection" 1) []
        case unifyCapability malformed malformed of
          Left _ ->
            return ()
          Right _ ->
            assertFailure
              "identical malformed capabilities bypassed the kind invariant"

  , TestLabel "P2 D4: recursive shape solver computes least evidence" . TestCase $ do
      let producer = ShapeSolver.ShapeProducerId 0
          node = ShapeSolver.rootShapeNode producer
          p = CapVar (MkCapVar "p")
          equations =
            [ ShapeSolver.ShapeEquation node (ShapeSolver.ShapeRef node)
            , ShapeSolver.ShapeEquation node (ShapeSolver.ShapeKnown p)
            ]
      solution <-
        either (assertFailure . show) return
          (ShapeSolver.solveShapeEquations equations)
      assertEqual
        "a known seed must propagate through a self-copy cycle"
        (Just (ShapeSolver.SolvedKnown p))
        (Map.lookup node solution)

  , TestLabel "P2 D4: seedless copy cycle stays unseen" . TestCase $ do
      let node =
            ShapeSolver.rootShapeNode (ShapeSolver.ShapeProducerId 0)
      solution <-
        either (assertFailure . show) return
          (ShapeSolver.solveShapeEquations
            [ShapeSolver.ShapeEquation node (ShapeSolver.ShapeRef node)])
      assertEqual
        "a recursive reference is not a generation seed"
        (Just ShapeSolver.SolvedUnseen)
        (Map.lookup node solution)

  , TestLabel "P2 D4: mutual copy recursion propagates one seed" . TestCase $ do
      let left =
            ShapeSolver.rootShapeNode (ShapeSolver.ShapeProducerId 0)
          right =
            ShapeSolver.rootShapeNode (ShapeSolver.ShapeProducerId 1)
          p = CapVar (MkCapVar "p")
          equations =
            [ ShapeSolver.ShapeEquation left (ShapeSolver.ShapeRef right)
            , ShapeSolver.ShapeEquation right (ShapeSolver.ShapeRef left)
            , ShapeSolver.ShapeEquation right (ShapeSolver.ShapeKnown p)
            ]
      solution <-
        either (assertFailure . show) return
          (ShapeSolver.solveShapeEquations equations)
      assertEqual
        "the seed must reach every node in a mutual copy SCC"
        [ Just (ShapeSolver.SolvedKnown p)
        , Just (ShapeSolver.SolvedKnown p)
        ]
        [ Map.lookup left solution
        , Map.lookup right solution
        ]

  , TestLabel "P2 D4: mutual copy recursion rejects unequal seeds" .
      TestCase $ do
        let left =
              ShapeSolver.rootShapeNode (ShapeSolver.ShapeProducerId 0)
            right =
              ShapeSolver.rootShapeNode (ShapeSolver.ShapeProducerId 1)
            p = CapVar (MkCapVar "p")
            q = CapVar (MkCapVar "q")
            equations =
              [ ShapeSolver.ShapeEquation left (ShapeSolver.ShapeRef right)
              , ShapeSolver.ShapeEquation right (ShapeSolver.ShapeRef left)
              , ShapeSolver.ShapeEquation left (ShapeSolver.ShapeKnown p)
              , ShapeSolver.ShapeEquation right (ShapeSolver.ShapeKnown q)
              ]
        case ShapeSolver.solveShapeEquations equations of
          Left (ShapeSolver.ShapeExactMismatch _ _ _ _) ->
            return ()
          Left other ->
            assertFailure
              ("unexpected mutual Shape mismatch: " ++ show other)
          Right _ ->
            assertFailure
              "a mutual copy SCC with unequal seeds must fail exact merge"

  , TestLabel "P2 D4: an explicit path node can be projected acyclically" .
      TestCase $ do
        let sourceRoot =
              ShapeSolver.rootShapeNode (ShapeSolver.ShapeProducerId 0)
            projectedRoot =
              ShapeSolver.rootShapeNode (ShapeSolver.ShapeProducerId 1)
            collectionFormer = mkTypeFormer "Collection" 1
            sourceElement =
              ShapeSolver.conParameterNode sourceRoot collectionFormer 0
            p = CapVar (MkCapVar "p")
            equations =
              [ ShapeSolver.ShapeEquation sourceElement
                  (ShapeSolver.ShapeKnown p)
              , ShapeSolver.ShapeEquation projectedRoot
                  (ShapeSolver.ShapeRef sourceElement)
              ]
        solution <-
          either (assertFailure . show) return
            (ShapeSolver.solveShapeEquations equations)
        assertEqual
          "projection is expressed by referring to the canonical path node"
          (Just (ShapeSolver.SolvedKnown p))
          (Map.lookup projectedRoot solution)

  , TestLabel "P2 D4: expansive generation cycle is rejected" . TestCase $ do
      let node =
            ShapeSolver.rootShapeNode (ShapeSolver.ShapeProducerId 0)
          collectionFormer = mkTypeFormer "Collection" 1
          equations =
            [ ShapeSolver.ShapeEquation node
                (ShapeSolver.ShapeCon collectionFormer
                  [ShapeSolver.ShapeRef node])
            ]
      case ShapeSolver.solveShapeEquations equations of
        Left (ShapeSolver.ExpansiveShapeCycle _) ->
          return ()
        Left other ->
          assertFailure
            ("unexpected recursive Shape solver error: " ++ show other)
        Right _ ->
          assertFailure "g <- Collection g must fail the structural occurs check"

  , TestLabel "P2 D4: mutual expansive generation cycle is rejected" .
      TestCase $ do
        let left =
              ShapeSolver.rootShapeNode (ShapeSolver.ShapeProducerId 0)
            right =
              ShapeSolver.rootShapeNode (ShapeSolver.ShapeProducerId 1)
            collectionFormer = mkTypeFormer "Collection" 1
            equations =
              [ ShapeSolver.ShapeEquation left
                  (ShapeSolver.ShapeCon collectionFormer
                    [ShapeSolver.ShapeRef right])
              , ShapeSolver.ShapeEquation right (ShapeSolver.ShapeRef left)
              ]
        case ShapeSolver.solveShapeEquations equations of
          Left (ShapeSolver.ExpansiveShapeCycle _) ->
            return ()
          Left other ->
            assertFailure
              ("unexpected mutual Shape solver error: " ++ show other)
          Right _ ->
            assertFailure
              "a mutual cycle containing a structural edge must be rejected"

  , TestLabel "P2 D4: cyclic outward path projection is rejected" .
      TestCase $ do
        let root =
              ShapeSolver.rootShapeNode (ShapeSolver.ShapeProducerId 0)
            collectionFormer = mkTypeFormer "Collection" 1
            element =
              ShapeSolver.conParameterNode root collectionFormer 0
            equations =
              [ ShapeSolver.ShapeEquation root (ShapeSolver.ShapeRef element)
              , ShapeSolver.ShapeEquation element (ShapeSolver.ShapeRef root)
              ]
        case ShapeSolver.solveShapeEquations equations of
          Left (ShapeSolver.ExpansiveShapeCycle _) ->
            return ()
          Left other ->
            assertFailure
              ("unexpected path-cycle Shape solver error: " ++ show other)
          Right _ ->
            assertFailure
              "copying a producer root into its own child path must be rejected"
  ]

-- | The substitution domain belongs to the structural slot only.  In
-- particular, resolving a repeated slot variable to a matcher variable must
-- not make that matcher variable bindable at the next occurrence.
matcherOneWayTests :: Test
matcherOneWayTests =
  TestLabel "matchOneWay keeps matcher variables rigid" . TestCase $ do
    let slotVar = TyVar "slot"
        matcherLeft = TyVar "matcherLeft"
        matcherRight = TyVar "matcherRight"
        repeatedSlot = TTuple [TVar slotVar, TVar slotVar]
    case matchOneWay repeatedSlot
           (TTuple [TVar matcherLeft, TVar matcherRight]) of
      Nothing -> return ()
      Just _ ->
        assertFailure
          "a matcher-side variable was rebound while checking a repeated slot"
    case matchOneWay repeatedSlot
           (TTuple [TVar matcherLeft, TVar matcherLeft]) of
      Just _ -> return ()
      Nothing ->
        assertFailure
          "a consistently repeated rigid matcher variable should be accepted"

-- | Strict type checking must not silently feed an ill-typed definition to
-- the untyped evaluator.  Permissive mode retains that fallback for gradual
-- adoption, but the strict boundary is required for meaningful P2 checking.
strictPipelineTests :: Test
strictPipelineTests =
  TestLabel "strict mode stops before untyped fallback" . TestCase $ do
    result <- fromEvalM
      defaultOption
        { optNoPrelude = True
        , optTypeCheckStrict = True
        }
      $ do
          env <- initialEnv
          exprs <-
            readTopExprs
              "def p2StrictMustReject := p2DefinitelyUnbound"
          evalTopExprsNoPrint env exprs
    case result of
      Left err
        | "Type error:" `isInfixOf` show err ->
            return ()
        | otherwise ->
            assertFailure
              ("strict mode failed for an unexpected reason: " ++ show err)
      Right _ ->
        assertFailure
          "strict mode accepted an unbound definition through untyped fallback"

-- | A rejected top-level item must not publish the temporary recursive
-- placeholder that Infer installed while checking its RHS.
failedInferAtomicityTests :: Test
failedInferAtomicityTests =
  TestLabel "failed inference does not mutate the type environment" .
    TestCase $ do
      result <- fromEvalM
        defaultOption
          { optNoPrelude = True
          , optTypeCheckStrict = True
          }
        $ do
            env <- initialEnv
            exprs <-
              readTopExprs
                "def p2FailedBindingMustNotLeak := p2DefinitelyUnbound"
            rejected <-
              catchError
                (evalTopExprsNoPrint env exprs >> return False)
                (\_ -> return True)
            typeEnv <- getTypeEnv
            return
              ( rejected
              , lookupEnvExact
                  (Var "p2FailedBindingMustNotLeak" [])
                  typeEnv
              )
      case result of
        Right (True, Nothing) ->
          return ()
        Right other ->
          assertFailure
            ("failed inference leaked state: " ++ show other)
        Left err ->
          assertFailure
            ("atomicity regression failed unexpectedly: " ++ show err)

-- | Producer SCC summaries must survive the end of a load unit.  Otherwise
-- the same alias cycle rejected inside one batch becomes Known merely by
-- placing the consuming matcher in the next batch.
producerCyclePersistenceTests :: Test
producerCyclePersistenceTests =
  TestLabel "producer cycles remain unseen across load units" . TestCase $ do
    result <- fromEvalM
      defaultOption
        { optNoPrelude = True
        , optTypeCheckStrict = True
        }
      $ do
          env0 <- initialEnv
          declarations <- readTopExprs $ unlines
            [ "inductive P2CrossBatchTree a :="
            , "  | P2CrossBatchLeaf a"
            , "  | P2CrossBatchNode (P2CrossBatchTree a)"
            , "inductive pattern P2CrossBatchTree a :="
            , "  | p2CrossBatchLeaf a"
            , "  | p2CrossBatchNode (P2CrossBatchTree a)"
            , "def p2CrossBatchOpaque {a} : Matcher none a :="
            , "  matcher"
            , "    | $ as p2CrossBatchOpaque with"
            , "      | $tgt -> [tgt]"
            , "def p2CrossBatchLeft {a}"
            , "  : Matcher (P2CrossBatchTree none) (P2CrossBatchTree a) :="
            , "  p2CrossBatchRight"
            , "def p2CrossBatchRight {a}"
            , "  : Matcher (P2CrossBatchTree none) (P2CrossBatchTree a) :="
            , "  p2CrossBatchLeft"
            ]
          env1 <- evalTopExprsNoPrint env0 declarations
          consumer <- readTopExprs $ unlines
            [ "def p2CrossBatchUse {a}"
            , "  : Matcher (P2CrossBatchTree none) (P2CrossBatchTree a) :="
            , "  matcher"
            , "    | p2CrossBatchLeaf $ as p2CrossBatchOpaque with"
            , "      | P2CrossBatchLeaf $value -> [value]"
            , "      | _ -> []"
            , "    | p2CrossBatchNode $ as p2CrossBatchLeft with"
            , "      | P2CrossBatchNode $rest -> [rest]"
            , "      | _ -> []"
            , "    | $ as p2CrossBatchOpaque with"
            , "      | $tgt -> [tgt]"
            ]
          catchError
            (evalTopExprsNoPrint env1 consumer >> return False)
            (\err ->
              return
                ("producer/path Shape equations" `isInfixOf` show err))
    case result of
      Right True ->
        return ()
      Right False ->
        assertFailure
          "a completed producer cycle became Known in a later load unit"
      Left err ->
        assertFailure
          ("cross-batch producer regression failed unexpectedly: " ++ show err)

-- | Language-level tests: the surface syntax and the primitives.
languageTests :: [FilePath]
languageTests =
  [ "test/syntax.egi"
  , "test/primitive.egi"
  ]

-- | Library unit tests: every test/lib/**/*.egi is discovered, so a new
-- suite dropped there runs without editing this file.  To exclude one,
-- add it to skippedLibTests with the reason.
discoverLibTests :: IO [FilePath]
discoverLibTests = do
  files <- glob "test/lib/**/*.egi"
  return (sort files \\ map fst skippedLibTests)

-- | Discovered files excluded from the run, with the reason recorded
-- (printed at startup so the exclusion stays visible in the log).
skippedLibTests :: [(FilePath, String)]
skippedLibTests =
  [ ("test/lib/core/io.egi",    "interactive IO demos; its helper functions no longer exist")
  , ("test/lib/core/shell.egi", "loads lib/core/shell.egi, which was removed")
  ]

-- | Whole programs registered for the language features they exercise.
sampleTests :: [FilePath]
sampleTests =
  [ "sample/primes.egi"                 -- pattern matching with infinitely many results
  , "sample/sat/cdcl.egi"               -- a practical pattern-matching program
  , "sample/poker-hands.egi"
  , "sample/poker-hands-with-joker.egi"
  , "sample/math/geometry/riemann-curvature-tensor-of-S2.egi" -- tensor index notation
  , "sample/math/geometry/riemann-curvature-tensor-of-T2.egi" -- tensor indices and math quote
  , "sample/math/geometry/curvature-form.egi"                 -- differential forms
  , "sample/math/number/17th-root-of-unity.egi"               -- rewriting of mathematical expressions
  , "sample/math/geometry/hodge-laplacian-polar.egi"          -- "..." in tensor indices
  ]

runTestCase :: FilePath -> Test
runTestCase file = TestLabel file . TestCase . assertEvalM $ do
  -- Print the test file name before starting
  liftIO $ do
    putStrLn $ "\n=== Testing: " ++ file ++ " ==="
    hFlush stdout
  env <- initialEnv
  -- Load core libraries, the math normalization library, and the test
  -- file in ONE batch, mirroring the interpreter's initial load (see
  -- Interpreter/egison.hs: the test file is included in the initial
  -- load).  A separate batch would keep the library operators' closures
  -- pointing at the library-time mathNormalize, so rules declared in
  -- the test file (declare rule auto / declare ideal) would never fire.
  let coreLibExprs = map Load coreLibraries
      mathLibExpr = [Load "lib/math/normalize.egi"]
      allLibExprs = coreLibExprs ++ mathLibExpr
  exprs <- loadFile file
  evalTopExprsNoPrint env (allLibExprs ++ exprs)
  where
    assertEvalM :: EvalM a -> Assertion
    assertEvalM m = fromEvalM defaultOption m >>= assertString . either show (const "")
