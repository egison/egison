module Main where

import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Except           (catchError)
import           Data.List                      (isInfixOf, sort, (\\))
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set
import           Options.Applicative            (ParserResult (..), execParserPure,
                                                  prefs)
import           System.Environment             (getArgs)
import           System.FilePath.Glob           (glob)
import           System.IO                      (hFlush, stdout)

import           Test.Framework                 (defaultMainWithArgs)
import           Test.Framework.Providers.HUnit (hUnitTestToTests)
import           Test.HUnit
import           TensorInference                (tensorInferenceTests)

import           Language.Egison
import           Language.Egison.IExpr          (IExpr (..), IPattern (..),
                                                  ITopExpr (..),
                                                  TITopExpr (..),
                                                  Var (..), Index (..), tiExprType)
import qualified Language.Egison.Type.Env       as TypeEnv
import           Language.Egison.Type.Env       (emptyEnv,
                                                  emptyClassEnv,
                                                  emptyPatternEnv,
                                                  emptyPatternFunctionEnv,
                                                  extendPatternEnv,
                                                  extendPatternFunctionEnv,
                                                  lookupPatternFunctionEnv,
                                                  lookupPatternEnv,
                                                  lookupEnvExact)
import           Language.Egison.Type.Error     (TypeError (..), TypeWarning (..),
                                                  formatTypeWarning)
import           Language.Egison.Type.Infer     (InferConfig (..),
                                                  InferState (..),
                                                  defaultInferConfig,
                                                  inferIExpr,
                                                  inferITopExpr,
                                                  initialInferState,
                                                  initialInferStateWithConfig,
                                                  instantiatePatFuncSchemeInState,
                                                  runInferWithWarnings,
                                                  runInferWithWarningsAndState,
                                                  unifyTypes)
import           Language.Egison.Type.Subst     (applyCapSubstToType,
                                                  applyCapSubst,
                                                  applySubst,
                                                  applyTypeSubst,
                                                  emptySubst,
                                                  singletonCapSubst,
                                                  singletonSubst)
import           Language.Egison.Type.Types     (CapVar (..),
                                                  Capability (..),
                                                  RequirementPair (..), PatFuncScheme (..),
                                                  TypeScheme (..),
                                                  TyVar (..), Type (..),
                                                  patFuncSchemeTargetScheme,
                                                  mkDataType,
                                                  normalizeMatcherTuples,
                                                  tyVarName)
import           Language.Egison.Type.Unify     (unify, unifyCapability,
                                                  unifyWithConstraints)

main :: IO ()
main = do
  args <- getArgs
  libTests <- discoverLibTests
  mapM_ (\(f, why) -> putStrLn ("Skipping " ++ f ++ " (" ++ why ++ ")"))
        skippedLibTests
  flip defaultMainWithArgs args . hUnitTestToTests . test $
    canonicalMatcherTests
      ++ [ recursiveRootTests
         , coreConservativeExtensionTests
         , cliWarningFlagParsingTests
         , matchWithoutElseWarningTests
         , primitivePatternWarningTests
         , matcherStaticConditionTests
         , matcherEvidenceTypeErrorTests
         , dataConstructorPatternTypeErrorTests
         , patternFamilyTargetTypeErrorTests
         , patternFunctionSchemeTests
         , patternFunctionTypeErrorTests
         , matchElseTypeErrorTests
         , signatureBoundaryTypeErrorTests
         , closedFieldTypeErrorTests
         , strictPipelineTests
         , strictSelectedCoreTests
         , sequentialTypeErrorTests
         , annotationRigidityTests
         , capabilityMguTests
         , failedInferAtomicityTests
         , tensorInferenceTests
         ]
      ++ map runTestCase (languageTests ++ libTests ++ sampleTests)

cliWarningFlagParsingTests :: Test
cliWarningFlagParsingTests =
  TestLabel "outside-core warning CLI flags" . TestList $
    [ parses
        "general outside-core warning flag"
        "--outside-egison-core-warnings"
        (True, False, False, False)
    , parses
        "pattern-hole ordering warning flag"
        "--pattern-hole-before-primitive-value-pattern-warnings"
        (False, True, False, False)
    , parses
        "nested structured primitive-pattern warning flag"
        "--nested-structured-primitive-pattern-pattern-warnings"
        (False, False, True, False)
    , parses
        "match without else warning flag"
        "--match-without-else-warnings"
        (False, False, False, True)
    , TestLabel "TypePM metrics flag" . TestCase $
        case execParserPure (prefs mempty) cmdParser ["--type-pm-metrics"] of
          Success options ->
            assertBool "the TypePM metrics flag is enabled"
              (optTypePMMetrics options)
          Failure _ ->
            assertFailure "the CLI parser rejected --type-pm-metrics"
          CompletionInvoked _ ->
            assertFailure "the CLI parser requested completion for --type-pm-metrics"
    ]
  where
    parses label flag expected =
      TestLabel label . TestCase $
        case execParserPure (prefs mempty) cmdParser [flag] of
          Success options ->
            assertEqual
              "the CLI flag sets only its corresponding warning field"
              expected
              ( optOutsideEgisonCoreWarnings options
              , optPatternHoleBeforePrimitiveValuePatternWarnings options
              , optNestedStructuredPrimitivePatternPatternWarnings options
              , optMatchWithoutElseWarnings options
              )
          Failure _ ->
            assertFailure ("the CLI parser rejected " ++ flag)
          CompletionInvoked _ ->
            assertFailure ("the CLI parser requested completion for " ++ flag)

matchWithoutElseWarningTests :: Test
matchWithoutElseWarningTests =
  TestLabel "match without else warnings" . TestList $
    [ checksMode BFSMode
    , checksMode DFSMode
    ]
  where
    checksMode mode = TestLabel (show mode) . TestCase $ do
      let withoutElse =
            IMatchExpr
              mode
              (IConstantExpr (IntegerExpr 1))
              (IConstantExpr SomethingExpr)
              [(IWildCard, IConstantExpr (IntegerExpr 1))]
              Nothing
          withElse =
            IMatchExpr
              mode
              (IConstantExpr (IntegerExpr 1))
              (IConstantExpr SomethingExpr)
              [(IWildCard, IConstantExpr (IntegerExpr 1))]
              (Just (IConstantExpr (IntegerExpr 0)))
          offConfig = defaultInferConfig
          onConfig = defaultInferConfig
            { cfgMatchWithoutElseWarnings = True }
      (offResult, offWarnings) <-
        runInferWithWarnings
          (inferIExpr withoutElse)
          (initialInferStateWithConfig offConfig)
      (onResult, onWarnings) <-
        runInferWithWarnings
          (inferIExpr withoutElse)
          (initialInferStateWithConfig onConfig)
      (elseResult, elseWarnings) <-
        runInferWithWarnings
          (inferIExpr withElse)
          (initialInferStateWithConfig onConfig)
      assertBool "warning-off inference succeeds" (either (const False) (const True) offResult)
      assertBool "warning-on inference succeeds" (either (const False) (const True) onResult)
      assertBool "else inference succeeds" (either (const False) (const True) elseResult)
      assertEqual "the option does not alter warning-off behavior" [] offWarnings
      case onWarnings of
        [MatchWithoutElseWarning _] -> return ()
        other ->
          assertFailure
            ("expected one match-without-else warning, got " ++ show other)
      assertEqual "an explicit else does not warn" [] elseWarnings

primitivePatternWarningTests :: Test
primitivePatternWarningTests =
  TestLabel "primitive-pattern pattern warnings" . TestList $
    [ TestLabel "flat hole before primitive value pattern" . TestCase $ do
        let pattern =
              PPInductivePat "pair" [PPPatVar, PPValuePat "value"]
        (resultOff, warningsOff) <- inferMatcher pattern False False
        (resultOn, warningsOn) <- inferMatcher pattern True False
        assertAcceptedWithSameResult resultOff resultOn
        assertEqual "the ordering warning is silent when disabled" [] warningsOff
        case warningsOn of
          [warning@(PatternHoleBeforePrimitiveValuePatternWarning rendered _)] -> do
            assertEqual
              "the warning renders the flat constructor"
              "pair $ #$value"
              rendered
            assertBool "the warning identifies the core boundary"
              ("Egison core does not" `isInfixOf` formatTypeWarning warning)
          other ->
            assertFailure
              ("expected one hole-before-value warning, got " ++ show other)

    , TestLabel "primitive value pattern before hole" . TestCase $ do
        let pattern =
              PPInductivePat "pair" [PPValuePat "value", PPPatVar]
        (result, warnings) <- inferMatcher pattern True True
        case result of
          Right _ -> return ()
          Left err ->
            assertFailure
              ("the reverse-order primitive pattern failed: " ++ show err)
        assertEqual
          "a primitive value pattern to the left of every hole does not warn"
          [] warnings

    , TestLabel "primitive-data pattern bindings may shadow value-pattern bindings" . TestCase $ do
        let expression = IMatcherExpr
              [ ( PPValuePat "x"
                , ITupleExpr []
                , [(PDPatVar (Var "x" []), ICollectionExpr [ITupleExpr []])]
                )
              , ( PPPatVar
                , IConstantExpr SomethingExpr
                , [(PDPatVar (Var "target" []), ICollectionExpr [IVarExpr "target"])]
                )
              ]
        (result, warnings) <- runInferWithWarnings
          (inferIExpr expression)
          (initialInferStateWithConfig defaultInferConfig
            { cfgOutsideEgisonCoreWarnings = True })
        case result of
          Right _ -> return ()
          Left err -> assertFailure ("data-clause shadowing was rejected: " ++ show err)
        assertEqual "data-clause shadowing is ordinary core scope" [] warnings

    , TestLabel "nested structured pattern only" . TestCase $ do
        let pattern =
              PPInductivePat "join"
                [ PPValuePat "outer"
                , PPInductivePat "cons"
                    [PPValuePat "inner", PPPatVar]
                ]
        (result, warnings) <- inferMatcher pattern True True
        case result of
          Right _ -> return ()
          Left err ->
            assertFailure
              ("the accepted nested primitive pattern failed: " ++ show err)
        case warnings of
          [NestedStructuredPrimitivePatternPatternWarning rendered _] ->
            assertEqual
              "the nested diagnostic preserves the primitive-pattern tree"
              "join #$outer (cons #$inner $)"
              rendered
          other ->
            assertFailure
              ("expected one nested-structured warning, got " ++ show other)

    , TestLabel "nested pattern with hole before primitive value pattern" .
        TestCase $ do
          let pattern =
                PPInductivePat "join"
                  [ PPPatVar
                  , PPInductivePat "cons"
                      [PPValuePat "value", PPPatVar]
                  ]
          (result, warnings) <- inferMatcher pattern True True
          case result of
            Right _ -> return ()
            Left err ->
              assertFailure
                ("the accepted doubly diagnosed primitive pattern failed: " ++
                 show err)
          assertBool
            "the ordering category is reported"
            (any isOrderingWarning warnings)
          assertBool
            "the nested-structure category is reported"
            (any isNestedWarning warnings)
          assertEqual "the two independent categories each warn once" 2 (length warnings)
    ]
  where
    demoType = TInductive "NestedPPatDemo" []
    -- Every field has the declared pattern type, so every hole requires the
    -- matcher `demoMatcher : Matcher NestedPPatDemo NestedPPatDemo`.
    demoMatcherType =
      TMatcher (CapCon (mkDataType "NestedPPatDemo" 0) []) demoType
    constructorScheme =
      Forall [] [] [] (TFun demoType (TFun demoType demoType))
    pairScheme =
      Forall [] [] [] (TFun demoType (TFun demoType demoType))
    patternEnv =
      extendPatternEnv "pair" pairScheme $
        extendPatternEnv "join" constructorScheme $
          extendPatternEnv "cons" constructorScheme emptyPatternEnv

    inferMatcher pattern orderWarnings nestedWarnings =
      runInferWithWarnings
        (inferIExpr (matcherExpression pattern))
        ((initialInferStateWithConfig
            defaultInferConfig
              { cfgPatternHoleBeforePrimitiveValuePatternWarnings = orderWarnings
              , cfgNestedStructuredPrimitivePatternPatternWarnings = nestedWarnings
              })
          { inferPatternEnv = patternEnv
          , declaredSymbols =
              Map.fromList [("demoMatcher", demoMatcherType)]
          })

    matcherExpression pattern =
      IMatcherExpr
        [ ( pattern
          , nextMatchers (patternHoleCount pattern)
          , [(PDPatVar (Var "structuredTarget" []), ICollectionExpr [])]
          )
        , ( PPPatVar
          , IConstantExpr SomethingExpr
          , [ ( PDPatVar (Var "catchAllTarget" [])
              , ICollectionExpr [IVarExpr "catchAllTarget"]
              )
            ]
          )
        ]

    nextMatchers 1 = IVarExpr "demoMatcher"
    nextMatchers count =
      ITupleExpr (replicate count (IVarExpr "demoMatcher"))

    patternHoleCount pattern =
      case pattern of
        PPPatVar -> 1
        PPInductivePat _ children -> sum (map patternHoleCount children)
        PPTuplePat children -> sum (map patternHoleCount children)
        _ -> 0

    assertAcceptedWithSameResult resultOff resultOn =
      case (resultOff, resultOn) of
        (Right _, Right _) ->
          assertEqual
            "warning reporting must not change inference"
            (show resultOff)
            (show resultOn)
        (Left errorOff, Left errorOn) ->
          assertFailure
            ("the accepted primitive-pattern matcher failed with the option " ++
             "off/on: " ++ show errorOff ++ " / " ++ show errorOn)
        _ ->
          assertFailure
            "warning reporting changed whether the matcher was accepted"

    isOrderingWarning warning =
      case warning of
        PatternHoleBeforePrimitiveValuePatternWarning{} -> True
        _ -> False

    isNestedWarning warning =
      case warning of
        NestedStructuredPrimitivePatternPatternWarning{} -> True
        _ -> False

matcherStaticConditionTests :: Test
matcherStaticConditionTests =
  TestLabel "TypePM matcher static conditions" . TestList $
    [ TestLabel "catch-all primitive-data-match clauses may enumerate a complete ADT" . TestCase $ do
        (result, warnings) <-
          runInferWithWarnings
            (inferIExpr completeDataMatcher)
            dataState
        case result of
          Left err ->
            assertFailure
              ("complete constructor data clauses were rejected: " ++ show err)
          Right _ -> return ()
        assertEqual "hard static checks emit no warning" [] warnings

    , TestLabel "a matcher without constructor-rooted clauses has capability Any" .
        TestCase $ do
          (result, _) <-
            runInferWithWarnings
              (inferIExpr (dataMatcher [PDWildCard]))
              dataState
          case result of
            Right (typed, _) ->
              case tiExprType typed of
                TMatcher CapAny _ -> return ()
                other ->
                  assertFailure
                    ("EvidenceOK requires capability Any, got " ++ show other)
            Left err ->
              assertFailure
                ("a catch-all-only matcher was rejected: " ++ show err)

    , TestLabel "incomplete constructor data clauses are rejected" . TestCase $ do
        (result, _) <-
          runInferWithWarnings
            (inferIExpr incompleteDataMatcher)
            dataState
        case result of
          Left MatcherDataClausesNotExhaustive{} -> return ()
          Left err ->
            assertFailure
              ("incomplete data clauses failed unexpectedly: " ++ show err)
          Right _ ->
            assertFailure "an incomplete set of user-ADT data clauses was accepted"

    , TestLabel "RootCoverage warning uses the mentioned data types" .
        TestCase $ do
          let offState = patternState False
              onState = patternState True
          (offResult, offWarnings) <-
            runInferWithWarnings (inferIExpr partialPatternMatcher) offState
          (onResult, onWarnings) <-
            runInferWithWarnings (inferIExpr partialPatternMatcher) onState
          assertBool "warning-off matcher succeeds"
            (either (const False) (const True) offResult)
          assertBool "warning-on matcher succeeds"
            (either (const False) (const True) onResult)
          assertEqual "RootCoverage is silent by default" [] offWarnings
          case onWarnings of
            [MatcherCoverageWarning _ missing _] ->
              assertEqual "only the missing constructor is reported" ["pB"] missing
            other ->
              assertFailure
                ("expected one RootCoverage warning, got " ++ show other)
    ]
  where
    choiceType = TInductive "StaticChoice" []
    nullaryChoiceScheme = Forall [] [] [] choiceType
    dataEnvironment =
      TypeEnv.extendEnv (Var "ChoiceA" []) nullaryChoiceScheme $
        TypeEnv.extendEnv (Var "ChoiceB" []) nullaryChoiceScheme emptyEnv
    dataState =
      initialInferState
        { inferEnv = dataEnvironment
        , inferDataConstructorNames = Set.fromList ["ChoiceA", "ChoiceB"]
        }
    completeDataMatcher =
      dataMatcher
        [ PDInductivePat "ChoiceA" []
        , PDInductivePat "ChoiceB" []
        ]
    incompleteDataMatcher = dataMatcher [PDInductivePat "ChoiceA" []]
    dataMatcher pdPatterns =
      IMatcherExpr
        [ ( PPPatVar
          , IConstantExpr SomethingExpr
          , [ (pdPattern, ICollectionExpr []) | pdPattern <- pdPatterns ]
          )
        ]

    patternType = TInductive "StaticPattern" []
    patternEnvironment =
      extendPatternEnv "pA" (Forall [] [] [] patternType) $
        extendPatternEnv "pB" (Forall [] [] [] patternType) emptyPatternEnv
    patternState enabled =
      (initialInferStateWithConfig
        defaultInferConfig
          { cfgMatcherConsistencyWarnings = enabled })
        { inferPatternEnv = patternEnvironment }
    partialPatternMatcher =
      IMatcherExpr
        [ ( PPInductivePat "pA" []
          , ITupleExpr []
          , [(PDPatVar (Var "value" []), ICollectionExpr [])]
          )
        , ( PPPatVar
          , IConstantExpr SomethingExpr
          , [(PDWildCard, ICollectionExpr [])]
          )
        ]

-- | On the TypePM grammar, the Egison interpreter's equality must be exactly the
-- synchronized core relation.  Enabling extension diagnostics cannot turn a
-- core rejection into a warned success or change the core substitution.
coreConservativeExtensionTests :: Test
coreConservativeExtensionTests =
  TestLabel "TypePM: Egison inference is a conservative extension" . TestList $
    map checkCase cases
  where
    listCapability = CapCon (mkDataType "Collection" 1) [CapAny]
    cases =
      [ ( "nested target refinement"
        , TCollection (TMatcher CapAny (TVar (TyVar "target")))
        , TCollection (TMatcher CapAny TInt)
        )
      , ( "nested capability mismatch"
        , TCollection (TMatcher CapAny TInt)
        , TCollection (TMatcher listCapability TInt)
        )
      , ( "function result refinement"
        , TFun TInt (TVar (TyVar "result"))
        , TFun TInt TBool
        )
      ]

    checkCase (label, left, right) =
      TestLabel label . TestCase $ do
        let coreResult =
              fmap fst $
                unifyWithConstraints emptyClassEnv [] left right
            config =
              defaultInferConfig
                { cfgOutsideEgisonCoreWarnings = True }
        (productionResult, warnings) <-
          runInferWithWarnings
            (unifyTypes left right)
            (initialInferStateWithConfig config)
        assertEqual
          "a core constraint emits no extension warning"
          [] warnings
        case (coreResult, productionResult) of
          (Right coreSubst, Right productionSubst) ->
            assertEqual
              "interpreter and TypePM substitutions"
              coreSubst productionSubst
          (Left _, Left _) ->
            return ()
          (Left coreError, Right productionSubst) ->
            assertFailure
              ("the interpreter accepted a core rejection: " ++ show coreError ++
               "; substitution " ++ show productionSubst)
          (Right coreSubst, Left productionError) ->
            assertFailure
              ("the interpreter rejected a core success: " ++ show coreSubst ++
               "; error " ++ show productionError)

patternFunctionSchemeTests :: Test
patternFunctionSchemeTests =
  TestLabel "pattern-function scheme" . TestList $
    [ TestLabel "definition stores one correlated pattern-function scheme" . TestCase $ do
        let typeA = TyVar "a"
            typeB = TyVar "b"
            declaration =
              IPatternFunctionDecl
                "pairPattern"
                [typeA, typeB]
                [("left", TVar typeA), ("right", TVar typeB)]
                (TTuple [TVar typeA, TVar typeB])
                (ITuplePat [IVarPat "left", IVarPat "right"])
            config =
              defaultInferConfig
                { cfgOutsideEgisonCoreWarnings = True }
            ambientScheme =
              Forall [] [] [] (TTuple [TVar typeA, TVar typeB])
            headerScheme =
              Forall [] [typeA, typeB] []
                (TFun (TVar typeA)
                  (TFun (TVar typeB)
                    (TTuple [TVar typeA, TVar typeB])))
            initialState =
              (initialInferStateWithConfig config)
                { inferEnv =
                    TypeEnv.extendEnv
                      (Var "ambientTargets" []) ambientScheme emptyEnv
                , inferPatternFuncDeclEnv =
                    extendPatternEnv
                      "pairPattern" headerScheme emptyPatternEnv
                }

        (result, warnings, finalState) <-
          runInferWithWarningsAndState
            (inferITopExpr declaration)
            initialState

        assertEqual
          "a directly checked pattern-function definition is inside Egison core"
          []
          warnings
        case result of
          Right
            ( Just
                (TIPatternFunctionDecl
                  "pairPattern" typedScheme _parameters _resultType _body)
            , _substitution
            ) ->
              case lookupPatternFunctionEnv
                     "pairPattern" (inferPatternFuncEnv finalState) of
                Nothing ->
                  assertFailure
                    "the checked pattern-function scheme was not stored"
                Just storedScheme -> do
                  assertEqual
                    "the typed declaration and inference environment share one scheme"
                    typedScheme
                    storedScheme
                  assertCorrelatedPairScheme storedScheme
                  let targetProjection =
                        patFuncSchemeTargetScheme storedScheme
                  assertEqual
                    "the declaration environment stores the canonical target projection"
                    (Just targetProjection)
                    (lookupPatternEnv
                      "pairPattern" (inferPatternFuncDeclEnv finalState))
                  assertEqual
                    "the ordinary environment stores the same target projection"
                    (Just targetProjection)
                    (lookupEnvExact
                      (Var "pairPattern" []) (inferEnv finalState))
          Right other ->
            assertFailure
              ("unexpected typed pattern-function result: " ++ show other)
          Left err ->
            assertFailure
                  ("the correlated pattern-function definition failed: " ++ show err)

    , TestLabel "unshared inferred capability defaults to Any" . TestCase $ do
        let declaration =
              IPatternFunctionDecl
                "wildcardPattern"
                []
                []
                TInt
                IWildCard
        (result, warnings, finalState) <-
          runInferWithWarningsAndState
            (inferITopExpr declaration)
            (initialInferStateWithConfig defaultInferConfig)
        assertEqual "canonicalization emits no warning" [] warnings
        case result of
          Left err ->
            assertFailure
              ("the wildcard pattern function failed: " ++ show err)
          Right _ ->
            assertEqual
              "a capability occurring only in the result is ground Any"
              (Just (PatFuncScheme [] [] [] (RequirementPair CapAny TInt)))
              (lookupPatternFunctionEnv
                "wildcardPattern" (inferPatternFuncEnv finalState))

    , TestLabel "append patterns default their independent leaf to Any" .
        TestCase $ do
        let element = TyVar "a"
            collectionDataType = mkDataType "Collection" 1
            collection ty = TCollection ty
            collectionCapability capability =
              CapCon collectionDataType [capability]
            consScheme =
              Forall [] [element] []
                (TFun (TVar element)
                  (TFun (collection (TVar element))
                    (collection (TVar element))))
            joinScheme =
              Forall [] [element] []
                (TFun (collection (TVar element))
                  (TFun (collection (TVar element))
                    (collection (TVar element))))
            patternEnvironment =
              extendPatternEnv "::" consScheme
                (extendPatternEnv "++" joinScheme emptyPatternEnv)
            appendLeaf =
              IInductiveOrPApplyPat "++"
                [ IWildCard
                , IInductiveOrPApplyPat "::" [IPatVar "x", IWildCard]
                ]
            nestedAppendLeaf =
              IInductiveOrPApplyPat "++"
                [ IWildCard
                , IInductiveOrPApplyPat "::"
                    [ appendLeaf
                    , IWildCard
                    ]
                ]
            initialState =
              (initialInferStateWithConfig defaultInferConfig)
                { inferPatternEnv = patternEnvironment }
            check name target body expectedCapability = do
              let declaration =
                    IPatternFunctionDecl name [element] [] target body
              (result, warnings, finalState) <-
                runInferWithWarningsAndState
                  (inferITopExpr declaration)
                  initialState
              assertEqual (name ++ " emits no warning") [] warnings
              case result of
                Left err ->
                  assertFailure
                    (name ++ " failed inference: " ++ show err)
                Right _ ->
                  assertEqual
                    (name ++ " has the canonical Any capability")
                    (Just
                      (PatFuncScheme
                        []
                        [element]
                        []
                        (RequirementPair expectedCapability target)))
                    (lookupPatternFunctionEnv
                      name (inferPatternFuncEnv finalState))

        check
          "appendLeafPattern"
          (collection (TVar element))
          appendLeaf
          (collectionCapability CapAny)
        check
          "nestedAppendLeafPattern"
          (collection (collection (TVar element)))
          nestedAppendLeaf
          (collectionCapability (collectionCapability CapAny))

    , TestLabel "a singleton ambient capability remains free" . TestCase $ do
        let ambient = MkCapVar "ambient-capability"
            sourceName = "ambientPattern"
            aliasName = "ambientPatternAlias"
            sourceScheme =
              PatFuncScheme [] [] [] (RequirementPair (CapVar ambient) TInt)
            declaration =
              IPatternFunctionDecl
                aliasName
                []
                []
                TInt
                (IInductiveOrPApplyPat sourceName [])
            initialState =
              (initialInferStateWithConfig defaultInferConfig)
                { inferPatternFuncEnv =
                    extendPatternFunctionEnv
                      sourceName sourceScheme emptyPatternFunctionEnv
                }
        (result, warnings, finalState) <-
          runInferWithWarningsAndState
            (inferITopExpr declaration)
            initialState
        assertEqual "ambient preservation emits no warning" [] warnings
        case result of
          Left err ->
            assertFailure
              ("the ambient-capability alias failed: " ++ show err)
          Right _ ->
            assertEqual
              "a once-used ambient capability is neither defaulted nor quantified"
              (Just sourceScheme)
              (lookupPatternFunctionEnv
                aliasName (inferPatternFuncEnv finalState))

    , TestLabel "definition distinguishes recursion from a shadowed head" . TestCase $ do
        let declaration =
              IPatternFunctionDecl
                "selfPattern"
                []
                []
                TInt
                (IValuePat
                  (IMatchExpr
                    BFSMode
                    (IConstantExpr (IntegerExpr 1))
                    (IConstantExpr SomethingExpr)
                    [ (IPApplyPat (IVarExpr "selfPattern") []
                      , IConstantExpr (IntegerExpr 1)
                      )
                    ]
                    Nothing))
        (result, _warnings, _finalState) <-
          runInferWithWarningsAndState
            (inferITopExpr declaration)
            (initialInferStateWithConfig defaultInferConfig)
        case result of
          Left (RecursivePatternFunction "selfPattern" _) -> return ()
          Left err ->
            assertFailure
              ("the self call failed for an unexpected reason: " ++ show err)
          Right _ ->
            assertFailure
              "a nested direct pattern-function self call was accepted"

        let hiddenName = "selfCallAfterNotPattern"
            hiddenDeclaration =
              IPatternFunctionDecl
                hiddenName
                []
                []
                TInt
                (IAndPat
                  (INotPat (IPatVar hiddenName))
                  (IPApplyPat (IVarExpr hiddenName) []))
        (hiddenResult, _hiddenWarnings, _hiddenState) <-
          runInferWithWarningsAndState
            (inferITopExpr hiddenDeclaration)
            (initialInferStateWithConfig defaultInferConfig)
        case hiddenResult of
          Left (RecursivePatternFunction rejectedName _)
            | rejectedName == hiddenName -> return ()
          Left err ->
            assertFailure
              ("a self call after a non-exporting pattern failed unexpectedly: " ++
               show err)
          Right _ ->
            assertFailure
              "a not-pattern binder incorrectly hid a real self call"

        let shadowedName = "shadowedDefinitionHead"
            shadowedDeclaration =
              IPatternFunctionDecl
                shadowedName
                []
                []
                TInt
                (ILetPat
                  [ ( PDPatVar (Var shadowedName [])
                    , ILambdaExpr
                        Nothing
                        [Var "localValue" []]
                        (IVarExpr "localValue")
                    )
                  ]
                  (IPApplyPat
                    (IVarExpr shadowedName)
                    [IWildCard]))
        (shadowedResult, _shadowedWarnings, shadowedState) <-
          runInferWithWarningsAndState
            (inferITopExpr shadowedDeclaration)
            (initialInferStateWithConfig defaultInferConfig)
        case shadowedResult of
          Left err ->
            assertFailure
              ("a lexically shadowed explicit head was mistaken for recursion: " ++
               show err)
          Right _ ->
            case lookupPatternFunctionEnv
                   shadowedName (inferPatternFuncEnv shadowedState) of
              Just _ -> return ()
              Nothing ->
                assertFailure
                  "the accepted shadowed definition lost its pattern-function scheme"

        let patternBoundName = "patternBoundDefinitionHead"
            patternBoundDeclaration =
              IPatternFunctionDecl
                patternBoundName
                []
                []
                (TFun TInt TInt)
                (IAndPat
                  (IVarPat patternBoundName)
                  (IPApplyPat (IVarExpr patternBoundName) []))
        (patternBoundResult, _patternBoundWarnings, patternBoundState) <-
          runInferWithWarningsAndState
            (inferITopExpr patternBoundDeclaration)
            (initialInferStateWithConfig defaultInferConfig)
        case patternBoundResult of
          Left err ->
            assertFailure
              ("an exported IVarPat binding was mistaken for recursion: " ++
               show err)
          Right _ ->
            case lookupPatternFunctionEnv
                   patternBoundName (inferPatternFuncEnv patternBoundState) of
              Just _ -> return ()
              Nothing ->
                assertFailure
                  "the pattern-bound definition lost its pattern-function scheme"

    , TestLabel "definition rejects duplicate parameter names" . TestCase $ do
        let declaration =
              IPatternFunctionDecl
                "duplicateParameters"
                []
                [("same", TInt), ("same", TInt)]
                TInt
                (IVarPat "same")
        (result, _warnings, _finalState) <-
          runInferWithWarningsAndState
            (inferITopExpr declaration)
            (initialInferStateWithConfig defaultInferConfig)
        case result of
          Left
            (DuplicatePatternFunctionParameters
              "duplicateParameters" ["same"] _) -> return ()
          Left err ->
            assertFailure
              ("duplicate parameters failed for an unexpected reason: " ++
               show err)
          Right _ ->
            assertFailure
              "duplicate pattern-function parameter names were accepted"

    , TestLabel "definition reports an extended body before finalizing" .
        TestCase $ do
          let declaration =
                IPatternFunctionDecl
                  "predicateBody"
                  []
                  []
                  TInt
                  (IPredPat
                    (ILambdaExpr
                      Nothing
                      [Var "candidate" []]
                      (IConstantExpr (BoolExpr True))))
              config =
                defaultInferConfig
                  { cfgOutsideEgisonCoreWarnings = True }
          (result, warnings, finalState) <-
            runInferWithWarningsAndState
              (inferITopExpr declaration)
              (initialInferStateWithConfig config)
          case result of
            Left err ->
              assertFailure
                ("the extended pattern-function body failed: " ++ show err)
            Right _ ->
              case lookupPatternFunctionEnv
                     "predicateBody" (inferPatternFuncEnv finalState) of
                Nothing ->
                  assertFailure
                    "the extended body lost its inferred pattern-function scheme"
                Just _ -> return ()
          case warnings of
            [OutsideEgisonCoreWarning detail _] ->
              assertBool
                "the definition warning identifies the predicate-pattern boundary"
                ("predicate pattern" `isInfixOf` detail)
            other ->
              assertFailure
                ("expected one pattern-function body warning, got " ++ show other)

    , TestLabel "replacement masks an older pattern-function scheme before forward use" .
        TestCase $ do
          result <- fromEvalM
            defaultOption
              { optNoPrelude = True
              , optTypeCheckStrict = True
              }
            $ do
                env0 <- initialEnv
                oldDeclaration <- readTopExprs $ unlines
                  [ "def pattern replaceable"
                  , "  (left : Integer) (right : Integer)"
                  , "  : (Integer, Integer) := (~left, ~right)"
                  ]
                env1 <- evalTopExprsNoPrint env0 oldDeclaration
                before <-
                  fmap (fmap (length . patFuncParams)) $
                    lookupPatternFunctionEnv "replaceable" <$>
                      getPatternFuncEnv
                replacement <- readTopExprs $ unlines
                  [ "def useReplacement (target : Integer) : Integer :="
                  , "  match target as something with"
                  , "  | replaceable $captured -> captured"
                  , "def pattern replaceable"
                  , "  (value : Integer) : Integer := ~value"
                  ]
                _ <- evalTopExprsNoPrint env1 replacement
                after <-
                  fmap (fmap (length . patFuncParams)) $
                    lookupPatternFunctionEnv "replaceable" <$>
                      getPatternFuncEnv
                return (before, after)
          case result of
            Right counts ->
              assertEqual
                "the new header shadows the old body until replacement succeeds"
                (Just 2, Just 1)
                counts
            Left err ->
              assertFailure
                ("the replacement batch failed: " ++ show err)

    , TestLabel "failed permissive replacement cannot inherit an old scheme" .
        TestCase $ do
          result <- fromEvalM
            defaultOption
              { optNoPrelude = True
              , optTypeCheckStrict = False
              }
            $ do
                env0 <- initialEnv
                oldDeclaration <- readTopExprs
                  "def pattern replaceable (value : Integer) : Integer := ~value"
                env1 <- evalTopExprsNoPrint env0 oldDeclaration
                invalidReplacement <- readTopExprs
                  "def pattern replaceable (value : Integer) : Bool := ~value"
                _ <- evalTopExprsNoPrint env1 invalidReplacement
                finalized <-
                  lookupPatternFunctionEnv "replaceable" <$>
                    getPatternFuncEnv
                header <-
                  lookupPatternEnv "replaceable" <$>
                    getPatternFuncDeclEnv
                return (finalized, header)
          case result of
            Right pair ->
              assertEqual
                "an unchecked runtime replacement remains header-only"
                (Nothing, Just (Forall [] [] [] (TFun TInt TBool)))
                pair
            Left err ->
              assertFailure
                ("the permissive replacement failed: " ++ show err)

    , TestLabel "one batch rejects duplicate pattern-function names" .
        TestCase $ do
          result <- fromEvalM
            defaultOption
              { optNoPrelude = True
              , optTypeCheckStrict = True
              }
            $ do
                env <- initialEnv
                declarations <- readTopExprs $ unlines
                  [ "def pattern duplicated (value : Integer)"
                  , "  : Integer := ~value"
                  , "def pattern duplicated (value : Bool)"
                  , "  : Bool := ~value"
                  ]
                evalTopExprsNoPrint env declarations
          case result of
            Left err
              | "Duplicate pattern-function declaration(s)" `isInfixOf`
                  show err -> return ()
              | otherwise ->
                  assertFailure
                    ("duplicate declarations failed unexpectedly: " ++ show err)
            Right _ ->
              assertFailure
                "duplicate pattern-function declarations were accepted"

    , TestLabel "instantiation rejects duplicate binders" . TestCase $ do
        let capabilityBinder = MkCapVar "duplicateCapability"
            targetBinder = TyVar "duplicateTarget"
            malformedScheme =
              PatFuncScheme
                [capabilityBinder, capabilityBinder]
                [targetBinder, targetBinder]
                [RequirementPair (CapVar capabilityBinder) (TVar targetBinder)]
                (RequirementPair (CapVar capabilityBinder) (TVar targetBinder))
        (result, warnings, _finalState) <-
          runInferWithWarningsAndState
            (instantiatePatFuncSchemeInState malformedScheme)
            (initialInferStateWithConfig defaultInferConfig)
        assertEqual "malformed scheme validation emits no warning" [] warnings
        case result of
          Left (MatcherCapabilityError detail _)
            | "duplicate binder(s)" `isInfixOf` detail -> return ()
            | otherwise ->
                assertFailure
                  ("duplicate binders failed unexpectedly: " ++ detail)
          Left err ->
            assertFailure
              ("duplicate binders produced the wrong error: " ++ show err)
          Right _ ->
            assertFailure
              "duplicate pattern-function scheme binders were silently instantiated"

    , TestLabel "instantiation freshens both sorts together" . TestCase $ do
        let capLeft = MkCapVar "leftCapability"
            capRight = MkCapVar "rightCapability"
            typeLeft = TyVar "leftTarget"
            typeRight = TyVar "rightTarget"
            scheme =
              PatFuncScheme
                [capLeft, capRight]
                [typeLeft, typeRight]
                [ RequirementPair
                    (CapVar capLeft)
                    (TMatcher (CapVar capLeft) (TVar typeLeft))
                , RequirementPair
                    (CapVar capRight)
                    (TMatcher (CapVar capRight) (TVar typeRight))
                ]
                (RequirementPair
                  (CapTuple [CapVar capLeft, CapVar capRight])
                  (TTuple
                    [ TMatcher (CapVar capLeft) (TVar typeLeft)
                    , TMatcher (CapVar capRight) (TVar typeRight)
                    ]))
            instantiateTwice = do
              first <- instantiatePatFuncSchemeInState scheme
              second <- instantiatePatFuncSchemeInState scheme
              return (first, second)

        (result, warnings, _finalState) <-
          runInferWithWarningsAndState
            instantiateTwice
            (initialInferStateWithConfig defaultInferConfig)

        assertEqual "instantiation itself emits no warning" [] warnings
        case result of
          Left err ->
            assertFailure
              ("pattern-function scheme instantiation failed: " ++ show err)
          Right (first, second) ->
            case (correlatedPairImages first, correlatedPairImages second) of
              ( Just (firstCapLeft, firstCapRight,
                      firstTypeLeft, firstTypeRight)
                , Just (secondCapLeft, secondCapRight,
                        secondTypeLeft, secondTypeRight)
                ) -> do
                  let firstCapabilities = [firstCapLeft, firstCapRight]
                      secondCapabilities = [secondCapLeft, secondCapRight]
                      firstTargets = [firstTypeLeft, firstTypeRight]
                      secondTargets = [secondTypeLeft, secondTypeRight]
                  assertBool
                    "capability images within the first instance are distinct"
                    (firstCapLeft /= firstCapRight)
                  assertBool
                    "capability images within the second instance are distinct"
                    (secondCapLeft /= secondCapRight)
                  assertBool
                    "target images within the first instance are distinct"
                    (firstTypeLeft /= firstTypeRight)
                  assertBool
                    "target images within the second instance are distinct"
                    (secondTypeLeft /= secondTypeRight)
                  assertBool
                    "separate instances must not share capability images"
                    (all (`notElem` secondCapabilities) firstCapabilities)
                  assertBool
                    "separate instances must not share target images"
                    (all (`notElem` secondTargets) firstTargets)
              other ->
                assertFailure
                  ("instantiation lost an argument/result correlation: " ++
                   show other)
    , TestLabel "named applications distinguish finalized and header-only schemes" .
        TestCase $ do
          let typeVariable = TyVar "a"
              headerScheme =
                Forall [] [typeVariable] []
                  (TFun (TVar typeVariable) (TVar typeVariable))
              finalizedScheme =
                PatFuncScheme
                  []
                  [typeVariable]
                  [RequirementPair CapAny (TVar typeVariable)]
                  (RequirementPair CapAny (TVar typeVariable))
              namedApplication functionName =
                IMatchExpr
                  BFSMode
                  (IConstantExpr (IntegerExpr 1))
                  (IConstantExpr SomethingExpr)
                  [ ( IInductiveOrPApplyPat functionName [IPatVar "value"]
                    , IVarExpr "value"
                    )
                  ]
                  Nothing
              config enabled =
                defaultInferConfig
                  { cfgOutsideEgisonCoreWarnings = enabled }
              applicationState enabled functionName maybeFinalized =
                (initialInferStateWithConfig (config enabled))
                  { inferEnv =
                      TypeEnv.extendEnv
                        (Var functionName []) headerScheme emptyEnv
                  , inferPatternFuncDeclEnv =
                      extendPatternEnv
                        functionName headerScheme emptyPatternEnv
                  , inferPatternFuncEnv =
                      case maybeFinalized of
                        Just scheme ->
                          extendPatternFunctionEnv
                            functionName scheme emptyPatternFunctionEnv
                        Nothing -> emptyPatternFunctionEnv
                  }
              expressionHeadedApplication =
                IMatchExpr
                  BFSMode
                  (IConstantExpr (IntegerExpr 1))
                  (IConstantExpr SomethingExpr)
                  [ ( IPApplyPat
                        (IApplyExpr
                          (ILambdaExpr
                            Nothing
                            [Var "function" []]
                            (IVarExpr "function"))
                          [IVarExpr "headerIdentity"])
                        [IPatVar "value"]
                    , IVarExpr "value"
                    )
                  ]
                  Nothing
              shadowedName = "shadowedPatternFunction"
              shadowedScheme =
                PatFuncScheme [] [] [] (RequirementPair CapAny TInt)
              shadowedApplication =
                ILetExpr
                  [ ( PDPatVar (Var shadowedName [])
                    , ILambdaExpr
                        Nothing
                        [Var "localValue" []]
                        (IVarExpr "localValue")
                    )
                  ]
                  (IMatchExpr
                    BFSMode
                    (IConstantExpr (IntegerExpr 1))
                    (IConstantExpr SomethingExpr)
                    [ ( IPApplyPat
                          (IVarExpr shadowedName)
                          [IPatVar "value"]
                      , IVarExpr "value"
                      )
                    ]
                    Nothing)
              shadowedState enabled =
                let targetProjection =
                      patFuncSchemeTargetScheme shadowedScheme
                in (initialInferStateWithConfig (config enabled))
                    { inferEnv =
                        TypeEnv.extendEnv
                          (Var shadowedName []) targetProjection emptyEnv
                    , inferPatternFuncDeclEnv =
                        extendPatternEnv
                          shadowedName targetProjection emptyPatternEnv
                    , inferPatternFuncEnv =
                        extendPatternFunctionEnv
                          shadowedName shadowedScheme emptyPatternFunctionEnv
                    }

          (finalizedResult, finalizedWarnings) <-
            runInferWithWarnings
              (inferIExpr (namedApplication "finalizedIdentity"))
              (applicationState
                True "finalizedIdentity" (Just finalizedScheme))
          case finalizedResult of
            Right _ -> return ()
            Left err ->
              assertFailure
                ("the finalized named application failed: " ++ show err)
          assertEqual
            "a finalized named application has no outside-core warning"
            []
            finalizedWarnings

          (headerResultOff, headerWarningsOff) <-
            runInferWithWarnings
              (inferIExpr (namedApplication "headerIdentity"))
              (applicationState False "headerIdentity" Nothing)
          (headerResultOn, headerWarningsOn) <-
            runInferWithWarnings
              (inferIExpr (namedApplication "headerIdentity"))
              (applicationState True "headerIdentity" Nothing)
          assertEqual
            "warning reporting must not change header-only inference"
            (show headerResultOff)
            (show headerResultOn)
          case headerResultOn of
            Right _ -> return ()
            Left err ->
              assertFailure
                ("the header-only extension path failed: " ++ show err)
          assertEqual
            "the header-only path is silent when the option is disabled"
            []
            headerWarningsOff
          case headerWarningsOn of
            [OutsideEgisonCoreWarning detail _] -> do
              assertBool
                "the warning identifies the header-only function"
                ("`headerIdentity`" `isInfixOf` detail)
              assertBool
                "the warning explains that the pattern-function scheme is not finalized"
                ("uses only a header because its pattern-function scheme is not finalized"
                  `isInfixOf` detail)
            other ->
              assertFailure
                ("expected exactly one header-only outside-core warning, got " ++
                 show other)

          (expressionResultOff, expressionWarningsOff) <-
            runInferWithWarnings
              (inferIExpr expressionHeadedApplication)
              (applicationState False "headerIdentity" Nothing)
          (expressionResultOn, expressionWarningsOn) <-
            runInferWithWarnings
              (inferIExpr expressionHeadedApplication)
              (applicationState True "headerIdentity" Nothing)
          assertEqual
            "warning reporting must not change expression-headed inference"
            (show expressionResultOff)
            (show expressionResultOn)
          case expressionResultOn of
            Right _ -> return ()
            Left err ->
              assertFailure
                ("the expression-headed extension path failed: " ++ show err)
          assertEqual
            "the expression-headed path is silent when the option is disabled"
            []
            expressionWarningsOff
          case expressionWarningsOn of
            [OutsideEgisonCoreWarning detail _] ->
              assertBool
                "the expression-headed boundary is reported exactly once"
                ("expression-headed pattern application" `isInfixOf` detail)
            other ->
              assertFailure
                ("expected exactly one expression-headed warning, got " ++
                 show other)

          (shadowedResultOff, shadowedWarningsOff) <-
            runInferWithWarnings
              (inferIExpr shadowedApplication)
              (shadowedState False)
          (shadowedResultOn, shadowedWarningsOn) <-
            runInferWithWarnings
              (inferIExpr shadowedApplication)
              (shadowedState True)
          assertEqual
            "warning reporting must not change shadowed-head inference"
            (show shadowedResultOff)
            (show shadowedResultOn)
          case shadowedResultOn of
            Right _ -> return ()
            Left err ->
              assertFailure
                ("an explicit variable head ignored its lexical binding: " ++
                 show err)
          assertEqual
            "a shadowed explicit head is silent when warnings are disabled"
            []
            shadowedWarningsOff
          case shadowedWarningsOn of
            [OutsideEgisonCoreWarning detail _] ->
              assertBool
                "the shadowed variable still uses the expression-headed boundary"
                ("expression-headed pattern application" `isInfixOf` detail)
            other ->
              assertFailure
                ("expected one warning for the shadowed explicit head, got " ++
                 show other)
    ]
  where
    assertCorrelatedPairScheme scheme =
      case scheme of
        PatFuncScheme
          capabilityBinders
          targetBinders
          [ RequirementPair (CapVar leftCapability) (TVar leftTarget)
          , RequirementPair (CapVar rightCapability) (TVar rightTarget)
          ]
          (RequirementPair
            (CapTuple
              [CapVar resultLeftCapability, CapVar resultRightCapability])
            (TTuple [TVar resultLeftTarget, TVar resultRightTarget])) -> do
              assertEqual
                "the first result capability comes from the first argument"
                leftCapability
                resultLeftCapability
              assertEqual
                "the second result capability comes from the second argument"
                rightCapability
                resultRightCapability
              assertEqual
                "the first result target comes from the first argument"
                leftTarget
                resultLeftTarget
              assertEqual
                "the second result target comes from the second argument"
                rightTarget
                resultRightTarget
              assertEqual
                "the scheme quantifies exactly its capability images"
                (sort [leftCapability, rightCapability])
                (sort capabilityBinders)
              assertEqual
                "the scheme quantifies exactly its target images"
                (sort [leftTarget, rightTarget])
                (sort targetBinders)
        other ->
          assertFailure
            ("unexpected correlated pattern-function scheme: " ++ show other)

    correlatedPairImages instanceValue =
      case instanceValue of
        ( [ RequirementPair
              (CapVar leftCapability)
              (TMatcher (CapVar leftTargetCapability) (TVar leftTarget))
          , RequirementPair
              (CapVar rightCapability)
              (TMatcher (CapVar rightTargetCapability) (TVar rightTarget))
          ]
          , RequirementPair
              (CapTuple
                [CapVar resultLeftCapability, CapVar resultRightCapability])
              (TTuple
                [ TMatcher
                    (CapVar resultLeftTargetCapability)
                    (TVar resultLeftTarget)
                , TMatcher
                    (CapVar resultRightTargetCapability)
                    (TVar resultRightTarget)
                ])
          )
            | leftCapability == resultLeftCapability
            , rightCapability == resultRightCapability
            , leftCapability == leftTargetCapability
            , rightCapability == rightTargetCapability
            , leftCapability == resultLeftTargetCapability
            , rightCapability == resultRightTargetCapability
            , leftTarget == resultLeftTarget
            , rightTarget == resultRightTarget ->
                Just
                  (leftCapability, rightCapability, leftTarget, rightTarget)
        _ -> Nothing

-- | Unit regressions for the single matcher type of the paper: ordinary and
-- capability substitution, canonical matcher--tuple normalization, and the
-- head expansion of the two-sorted equality unifier.
canonicalMatcherTests :: [Test]
canonicalMatcherTests =
  [ TestLabel "TypePM: type substitution does not enter capability" . TestCase $ do
      let typeVariable = TyVar "a"
          capabilityVariable = MkCapVar "p"
          original =
            TMatcher
              (CapCon (mkDataType "Collection" 1)
                [CapVar capabilityVariable])
              (TCollection (TVar typeVariable))
          substituted =
            applyTypeSubst
              (singletonSubst typeVariable TInt)
              original
      assertEqual
        "ordinary substitution must change only the matcher target"
        (TMatcher
          (CapCon (mkDataType "Collection" 1)
            [CapVar capabilityVariable])
          (TCollection TInt))
        substituted

  , TestLabel "TypePM: capability substitution reaches nested matchers" . TestCase $ do
      let capabilityVariable = MkCapVar "p"
          original =
            TCollection
              (TFun TInt
                (TMatcher
                  (CapCon (mkDataType "Maybe" 1)
                    [CapVar capabilityVariable])
                  (TInductive "Maybe" [TInt])))
          substituted =
            applyCapSubstToType
              (singletonCapSubst capabilityVariable CapAny)
              original
      assertEqual
        "capability substitution must traverse the complete ordinary type"
        (TCollection
          (TFun TInt
            (TMatcher
              (CapCon (mkDataType "Maybe" 1) [CapAny])
              (TInductive "Maybe" [TInt]))))
        substituted

  , TestLabel "TypePM: a matcher over a tuple normalizes to a tuple of matchers" .
      TestCase $ do
        let original =
              TMatcher
                (CapTuple [CapAny, listAny])
                (TTuple [TInt, TCollection TInt])
        assertEqual
          "Matcher (Any, [Any]) (Integer, [Integer]) is the tuple of its components"
          (TTuple [TMatcher CapAny TInt, TMatcher listAny (TCollection TInt)])
          (normalizeMatcherTuples original)

  , TestLabel "TypePM: Matcher Any over a tuple does not distribute" .
      TestCase $ do
        let original = TMatcher CapAny (TTuple [TInt, TInt])
        assertEqual
          "a non-tuple capability keeps the matcher form"
          original
          (normalizeMatcherTuples original)

  , TestLabel "TypePM: unification expands a matcher head against a tuple" .
      TestCase $ do
        let p = MkCapVar "p"
            t = TyVar "t"
            head' = TMatcher (CapVar p) (TVar t)
            matcherTuple =
              TTuple [TMatcher CapAny TInt, TMatcher listAny (TCollection TInt)]
        substitution <-
          either (assertFailure . show) return (unify head' matcherTuple)
        assertEqual
          "the expanded head is the tuple"
          matcherTuple
          (applySubst substitution head')

  , TestLabel "TypePM: capabilities unify by equality only" . TestCase $
      case unify (TMatcher CapAny TInt) (TMatcher listAny TInt) of
        Left _ -> return ()
        Right _ -> assertFailure "Any and [Any] were unified"
  ]
  where
    listAny = CapCon (mkDataType "Collection" 1) [CapAny]

-- | Recursive top-level roots: a recursive lambda is accepted, a recursive
-- data root is rejected.
recursiveRootTests :: Test
recursiveRootTests =
  TestLabel "recursive definition roots" . TestList $
    [ TestLabel "recursive data root is rejected" . TestCase $ do
        let definition =
              IDefine
                (Var "cycle" [])
                (ICollectionExpr [IVarExpr "cycle"])
        (result, _) <-
          runInferWithWarnings (inferITopExpr definition) initialInferState
        case result of
          Left UnsupportedFeature{} -> return ()
          Left err -> assertFailure ("unexpected recursion error: " ++ show err)
          Right _ -> assertFailure "a recursive collection root was accepted"
    , TestLabel "recursive lambda root is accepted" . TestCase $ do
        let definition =
              IDefine
                (Var "loop" [])
                (ILambdaExpr Nothing [Var "x" []]
                  (IApplyExpr (IVarExpr "loop") [IVarExpr "x"]))
        (result, _) <-
          runInferWithWarnings (inferITopExpr definition) initialInferState
        case result of
          Right _ -> return ()
          Left err -> assertFailure ("recursive lambda failed: " ++ show err)
    ]
    ++ [ TestLabel label . TestCase $ do
           let scheme = Forall [] [] [] (TTensor TInt)
               env = foldr (\indices -> TypeEnv.extendEnv (Var "vi" indices) scheme)
                       (inferEnv initialInferState) overloads
               env' = if annotated
                        then TypeEnv.extendEnv (Var "vi" []) scheme env
                        else env
               state = initialInferState { inferEnv = env' }
               definition = IDefine (Var "vi" [])
                 (IIndexedExpr True (IVarExpr "vi") indices)
           (result, _) <- runInferWithWarnings (inferITopExpr definition) state
           case result of
             Right _ | accepted -> return ()
             Left UnsupportedFeature{} | not accepted -> return ()
             _ -> assertFailure ("unexpected indexed alias result: " ++ show result)
       | annotated <- [False, True]
       , (description, overloads, indices, accepted) <-
           [ ("exact indexed binding", [[Sub Nothing]], [Sub one], True)
           , ("longer indexed binding", [[Sub Nothing, Sub Nothing]], [Sub one], True)
           , ("shorter indexed binding", [[Sub Nothing]], [Sub one, Sub one], True)
           , ("bare fallback is recursive", [], [Sub one], False)
           , ("different variance falls back to bare", [[Sup Nothing]], [Sub one], False)
           , ("index expression still refers to self", [[Sub Nothing]],
                [Sub (IVarExpr "vi")], False)
           ]
       , let label = (if annotated then "annotated: " else "inferred: ") ++ description
       ]
  where
    one = IConstantExpr (IntegerExpr 1)

-- | Strict type checking must not silently feed an ill-typed definition to
-- the untyped evaluator.  Permissive mode retains that fallback for gradual
-- adoption, but the strict boundary is required for meaningful TypePM checking.
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
              "def coreStrictMustReject := definitelyUnbound"
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

-- | The matcher-capability surface regressions must type-check without relying on the
-- permissive fallback.  The base library still contains five MathValue
-- instances whose implementations normally arrive from the CAS layer.  Give
-- those names inert, correctly typed test definitions, then load only the
-- ordinary matcher/list/maybe surface needed by this regression.
strictSelectedCoreTests :: Test
strictSelectedCoreTests =
  TestLabel "TypePM: strict selected-library and language regressions" . TestCase $ do
    result <- fromEvalM
      defaultOption
        { optNoPrelude = True
        , optTypeCheckStrict = True
        }
      $ do
          env <- initialEnv
          casBridgeStubs <- readTopExprs $ unlines
            [ "def plusForMathValue (x : MathValue) (_ : MathValue)"
                ++ " : MathValue := x"
            , "def minusForMathValue (x : MathValue) (_ : MathValue)"
                ++ " : MathValue := x"
            , "def multForMathValue (x : MathValue) (_ : MathValue)"
                ++ " : MathValue := x"
            , "def divForMathValue (x : MathValue) (_ : MathValue)"
                ++ " : MathValue := x"
            , "def gcdForMathValue (x : MathValue) (_ : MathValue)"
                ++ " : MathValue := x"
            ]
          let selectedCoreLibraries =
                [ "lib/core/base.egi"
                , "lib/core/order.egi"
                , "lib/core/collection.egi"
                , "lib/core/maybe.egi"
                , "lib/core/number.egi"
                , "lib/core/random.egi"
                , "lib/core/assoc.egi"
                , "lib/core/string.egi"
                , "lib/core/io.egi"
                ]
          evalTopExprsNoPrint
            env
            (casBridgeStubs
              ++ map Load selectedCoreLibraries
              ++ [ LoadFile "test/lib/core/matcher-capability.egi"
                 , LoadFile "test/lib/core/pattern-function.egi"
                 , LoadFile "test/lib/core/closed-field-next-matcher.egi"
                 , LoadFile "test/lib/core/ar-recursive-matcher-strict.egi"
                 , LoadFile "test/lib/core/sequential-capability.egi"
                 ])
    case result of
      Left err ->
        assertFailure
          ("strict selected-library TypePM regression failed: " ++ show err)
      Right _ ->
        return ()

-- | Saved targets must agree between alternatives and carry both indices
-- into the following sequence stage. These use the inference entry.
sequentialTypeErrorTests :: Test
sequentialTypeErrorTests = TestLabel "sequential pattern rejection" . TestList $
  map rejects
    [ ("saved integer is not a pair", ISeqConsPat ILaterPatVar
        (ISeqConsPat (ITuplePat [IWildCard, IWildCard]) ISeqNilPat))
    , ("final stage must consume saved targets", ISeqConsPat ILaterPatVar ISeqNilPat)
    , ("alternatives save the same number", ISeqConsPat
        (IOrPat ILaterPatVar IWildCard) ISeqNilPat)
    , ("negation does not export a saved target", ISeqConsPat
        (INotPat ILaterPatVar)
        (ISeqConsPat (IValuePat (IConstantExpr (IntegerExpr 1))) ISeqNilPat))
    ]
  where
    rejects (label, pattern) = TestLabel label . TestCase $ do
      let expression = IMatchAllExpr BFSMode
            (IConstantExpr (IntegerExpr 1))
            (IConstantExpr SomethingExpr)
            [(pattern, IConstantExpr (IntegerExpr 1))]
      (result, _) <- runInferWithWarnings (inferIExpr expression) initialInferState
      case result of
        Left _ -> return ()
        Right typed -> assertFailure ("invalid sequence accepted: " ++ show typed)

-- | The standalone type-error corpus is normally checked by a separate
-- sweep.  Keep the two PatFuncScheme-specific rejection boundaries in the
-- ordinary HUnit run as well, and require their intended diagnostics so an
-- unrelated parse or linearity failure cannot satisfy the test accidentally.
patternFunctionTypeErrorTests :: Test
patternFunctionTypeErrorTests =
  TestLabel "pattern-function target and arity rejection" . TestList $
    map rejects
      [ ( "test/type-error/88-patfun-param-target.egi"
        , ["Type error:", "Integer", "Bool"]
        )
      , ( "test/type-error/89-patfun-exact-arity.egi"
        , ["Type error:", "expects 2 arguments, but got 1"]
        )
      ]
  where
    rejects (file, expectedFragments) =
      TestLabel file . TestCase $ do
        result <- fromEvalM
          defaultOption
            { optNoPrelude = True
            , optTypeCheckStrict = True
            }
          $ do
              env <- initialEnv
              evalTopExprsNoPrint env [LoadFile file]
        case result of
          Left err
            | all
                (\fragment -> fragment `isInfixOf` show err)
                expectedFragments ->
                return ()
            | otherwise ->
                assertFailure
                  ("pattern-function rejection failed for an unexpected reason: " ++
                   show err)
          Right _ ->
            assertFailure
              ("an invalid pattern function was accepted: " ++ file)

-- | Ordinary match clauses and the else expression share one result type, while
-- the else expression is checked outside the bindings of every ordinary match clause.
matchElseTypeErrorTests :: Test
matchElseTypeErrorTests =
  TestLabel "match else rejection" . TestList $
    map rejects
      [ ( "test/type-error/91-match-else-result.egi"
        , ["Type error:", "Integer", "Bool"]
        )
      , ( "test/type-error/92-match-else-scope.egi"
        , ["Type error:", "Unbound variable: x"]
        )
      ]
  where
    rejects (file, expectedFragments) =
      TestLabel file . TestCase $ do
        result <- fromEvalM
          defaultOption
            { optNoPrelude = True
            , optTypeCheckStrict = True
            }
          $ do
              env <- initialEnv
              evalTopExprsNoPrint env [LoadFile file]
        case result of
          Left err
            | all (`isInfixOf` show err) expectedFragments ->
                return ()
            | otherwise ->
                assertFailure
                  ("match else rejection failed for an unexpected reason: " ++
                   show err)
          Right _ ->
            assertFailure
              ("an invalid match else expression was accepted: " ++ file)

-- | A matcher expression without constructor-rooted clauses has capability Any
-- (EvidenceOK), so a pattern constructor cannot instantiate its capability.
matcherEvidenceTypeErrorTests :: Test
matcherEvidenceTypeErrorTests =
  TestLabel "TypePM matcher capability evidence" . TestList $
    map rejects
      [ ( "test/type-error/97-catch-all-matcher-constructor.egi"
        , "matcher capabilities do not unify"
        )
      ]
  where
    rejects (file, expectedFragment) =
      TestLabel file . TestCase $ do
        result <- fromEvalM
          defaultOption
            { optNoPrelude = True
            , optTypeCheckStrict = True
            }
          $ do
              env <- initialEnv
              evalTopExprsNoPrint env [LoadFile file]
        case result of
          Left err
            | expectedFragment `isInfixOf` show err -> return ()
            | otherwise ->
                assertFailure
                  ("capability evidence check failed unexpectedly: " ++ show err)
          Right _ ->
            assertFailure
              ("a constructor pattern used a matcher without capability evidence: " ++ file)

-- | A data constructor written as the head of a pattern is rejected: patterns
-- use the declared pattern constructors, as in the core.  The pattern used to
-- pass type checking and fail at run time.
dataConstructorPatternTypeErrorTests :: Test
dataConstructorPatternTypeErrorTests =
  TestLabel "TypePM data constructor in a pattern" . TestList $
    map rejects
      [ ( "test/type-error/98-data-constructor-pattern.egi"
        , "Data constructor `Just` cannot be used in a pattern"
        )
      ]
  where
    rejects (file, expectedFragment) =
      TestLabel file . TestCase $ do
        result <- fromEvalM
          defaultOption
            { optNoPrelude = True
            , optTypeCheckStrict = True
            }
          $ do
              env <- initialEnv
              evalTopExprsNoPrint env [LoadFile file]
        case result of
          Left err
            | expectedFragment `isInfixOf` show err -> return ()
            | otherwise ->
                assertFailure
                  ("data-constructor pattern check failed unexpectedly: " ++ show err)
          Right _ ->
            assertFailure
              ("a data constructor was accepted in a pattern: " ++ file)

-- | A pattern family declared with `for` names its target type separately.
-- Its capability differs from that of every other family over the same type,
-- and its declaration is rejected when the target is not a data type or a
-- built-in base type, when its name is that of a data type, when a type
-- parameter does not occur in the target, or when the target mentions a family
-- declared with `for`.
patternFamilyTargetTypeErrorTests :: Test
patternFamilyTargetTypeErrorTests =
  TestLabel "pattern families declared with for" . TestList $
    map rejects
      [ ( "test/type-error/99-for-capability-mismatch.egi"
        , "matcher capabilities do not unify"
        )
      , ( "test/type-error/100-for-tuple-target.egi"
        , "the target must be a data type or a built-in base type"
        )
      , ( "test/type-error/101-for-data-type-name.egi"
        , "needs a name different from every data type"
        )
      , ( "test/type-error/102-for-parameter-not-argument.egi"
        , "every type parameter must occur in the target"
        )
      , ( "test/type-error/103-for-target-mentions-family.egi"
        , "the target must not mention a pattern family declared with `for`"
        )
      ]
  where
    rejects (file, expectedFragment) =
      TestLabel file . TestCase $ do
        result <- fromEvalM
          defaultOption
            { optNoPrelude = True
            , optTypeCheckStrict = True
            }
          $ do
              env <- initialEnv
              evalTopExprsNoPrint env [LoadFile file]
        case result of
          Left err
            | expectedFragment `isInfixOf` show err -> return ()
            | otherwise ->
                assertFailure
                  ("pattern family rejection failed unexpectedly: " ++ show err)
          Right _ ->
            assertFailure
              ("an invalid pattern family was accepted: " ++ file)

signatureBoundaryTypeErrorTests :: Test
signatureBoundaryTypeErrorTests =
  TestLabel "TypePM signature boundaries" . TestList $
    map rejects
      [ ( "test/type-error/93-data-constructor-open-scheme.egi"
        , "undeclared type variable(s): a"
        )
      , ( "test/type-error/94-data-constructor-undetermined-capability.egi"
        , "every field type parameter must occur in the constructor result"
        )
      , ( "test/type-error/95-pattern-constructor-undetermined-capability.egi"
        , "every field type parameter must occur in the constructor result"
        )
      , ( "test/type-error/96-pattern-function-open-scheme.egi"
        , "undeclared type variable(s): a"
        )
      ]
  where
    rejects (file, expectedFragment) =
      TestLabel file . TestCase $ do
        result <- fromEvalM
          defaultOption
            { optNoPrelude = True
            , optTypeCheckStrict = True
            }
          $ do
              env <- initialEnv
              evalTopExprsNoPrint env [LoadFile file]
        case result of
          Left err
            | expectedFragment `isInfixOf` show err -> return ()
            | otherwise ->
                assertFailure
                  ("signature boundary failed unexpectedly: " ++ show err)
          Right _ ->
            assertFailure
              ("an invalid public signature was accepted: " ++ file)

-- | A hole of a declared list field requires the list capability; an
-- Any-capability next matcher, whatever its syntactic form, is rejected by
-- the capability equation.  Require the capability diagnostic so these cases
-- cannot pass because of an unrelated target or parser error.
closedFieldTypeErrorTests :: Test
closedFieldTypeErrorTests =
  TestLabel "closed constructor-field next-matcher rejection" . TestList $
    map rejects
      [ ( "test/type-error/24-patfun-nested-matcher-slot.egi"
        , "matcher capabilities do not unify"
        )
      , ( "test/type-error/59-next-matcher-bare-variable.egi"
        , "matcher capabilities do not unify"
        )
      , ( "test/type-error/60-next-matcher-bare-application.egi"
        , "matcher capabilities do not unify"
        )
      , ( "test/type-error/61-next-matcher-bare-lambda.egi"
        , "matcher capabilities do not unify"
        )
      , ( "test/type-error/90-closed-field-slot-application.egi"
        , "matcher capabilities do not unify"
        )
      ]
  where
    rejects (file, expectedFragment) =
      TestLabel file . TestCase $ do
        result <- fromEvalM
          defaultOption
            { optNoPrelude = True
            , optTypeCheckStrict = True
            }
          $ do
              env <- initialEnv
              -- The closed-field head is capability-visible only after the
              -- declared signature contains a Collection pattern constructor.
              collectionVisibility <-
                readTopExprs
                  "inductive pattern [a] := closedFieldVisibility"
              evalTopExprsNoPrint env
                (collectionVisibility ++ [LoadFile file])
        case result of
          Left err
            | "Type error:" `isInfixOf` show err
            , expectedFragment `isInfixOf` show err ->
                return ()
            | otherwise ->
                assertFailure
                  ("closed-field rejection failed for an unexpected reason: "
                   ++ show err)
          Right _ ->
            assertFailure
              ("an Any-capability next matcher filled a closed list field: "
               ++ file)

-- | Both sorts of binder in an explicit scheme are rigid for the duration of
-- checking.  These reject cases are wired into the normal HUnit suite so they
-- cannot silently regress behind the permissive command-line fallback used by
-- the standalone type-error corpus.
annotationRigidityTests :: Test
annotationRigidityTests =
  TestLabel "TypePM: annotation binders are rigid in both sorts" . TestList $
    map rejects
      [ ("test/type-error/83-ordinary-annotation-rigidity.egi", "only retain its identity")
      , ("test/type-error/84-nested-annotation-rigidity.egi", "only retain its identity")
      , ("test/type-error/85-pattern-function-annotation-rigidity.egi", "only retain its identity")
      , ("test/type-error/86-pattern-function-nested-annotation-rigidity.egi", "only retain its identity")
      , ("test/type-error/87-capability-annotation-rigidity.egi", "Matcher $skc")
      ]
  where
    rejects (file, expectedSkolem) =
      TestLabel file . TestCase $ do
        result <- fromEvalM
          defaultOption
            { optNoPrelude = True
            , optTypeCheckStrict = True
            }
          $ do
              env <- initialEnv
              evalTopExprsNoPrint env [LoadFile file]
        case result of
          Left err
            | "Type error:" `isInfixOf` show err
            , expectedSkolem `isInfixOf` show err ->
                return ()
            | otherwise ->
                assertFailure
                  ("rigid annotation failed for an unexpected reason: "
                    ++ show err)
          Right _ ->
            assertFailure
              "an over-general ordinary annotation was accepted"

-- | Executable regressions for TypePM's ordinary capability MGU.
capabilityMguTests :: Test
capabilityMguTests =
  TestLabel "TypePM: ordinary capability MGU" . TestList $
    [ TestLabel "a capability variable is bound by the capability MGU" .
        TestCase $ do
          let matcherVariable = MkCapVar "matcherCap"
              required = CapCon (mkDataType "Collection" 1) [CapAny]
          substitution <-
            either (assertFailure . show) return
              (unifyCapability (CapVar matcherVariable) required)
          assertEqual
            "the ordinary MGU specializes the matcher's capability"
            required
            (applyCapSubst substitution (CapVar matcherVariable))

    , TestLabel "a polymorphic matcher instance specializes at its use" .
        TestCase $ do
          result <- runSource matcherSpecializationSource
          case result of
            Right _ -> return ()
            Left err ->
              assertFailure
                ("ordinary capability specialization was rejected: " ++ show err)
    ]
  where
    runSource source = fromEvalM
      defaultOption
        { optNoPrelude = True
        , optTypeCheckStrict = True
        }
      $ do
          env <- initialEnv
          expressions <- readTopExprs source
          evalTopExprsNoPrint env expressions

    matcherSpecializationSource = unlines
      [ "def passMatcher {a}"
      , "  (m : Matcher p a)"
      , "  : Matcher p a := m"
      , ""
      , "def requireListFunction"
      , "  (f : Matcher [Any] Integer -> Matcher [Any] Integer)"
      , "  : Integer := 0"
      , ""
      , "def result := requireListFunction passMatcher"
      ]

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
                "def failedBindingMustNotLeak := definitelyUnbound"
            rejected <-
              catchError
                (evalTopExprsNoPrint env exprs >> return False)
                (\_ -> return True)
            typeEnv <- getTypeEnv
            return
              ( rejected
              , lookupEnvExact
                  (Var "failedBindingMustNotLeak" [])
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
  , "sample/sat/dp.egi"                 -- sequential patterns retain multiset matchers
  , "sample/sat/cdcl.egi"               -- a practical pattern-matching program
  , "sample/poker-hands.egi"
  , "sample/poker-hands-with-joker.egi"
  , "sample/tree.egi"                   -- loop patterns over a user-declared pattern family
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
    assertEvalM m = fromEvalM (defaultOption { optTypeCheckStrict = file `elem` ["sample/sat/dp.egi", "test/lib/core/type-pm-examples.egi", "test/lib/core/matcher-data-shadowing.egi", "test/lib/core/sequential-capability.egi", "test/lib/math/tensor-expected-result.egi"] }) m >>= assertString . either show (const "")
