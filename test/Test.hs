module Main where

import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Except           (catchError)
import           Data.List                      (isInfixOf, sort, (\\))
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set
import           System.Environment             (getArgs)
import           System.FilePath.Glob           (glob)
import           System.IO                      (hFlush, stdout)

import           Test.Framework                 (defaultMainWithArgs)
import           Test.Framework.Providers.HUnit (hUnitTestToTests)
import           Test.HUnit

import           Language.Egison
import           Language.Egison.IExpr          (IExpr (..), IPattern (..),
                                                  ITopExpr (..),
                                                  TITopExpr (..), Var (..))
import qualified Language.Egison.Type.Capability as Capability
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
                                                  initialInferStateWithConfig,
                                                  instantiateDualSchemeInState,
                                                  runInferWithWarnings,
                                                  runInferWithWarningsAndState)
import           Language.Egison.Type.Subst     (applyCapSubstToType,
                                                  applyCapSubst,
                                                  applyTypeSubst,
                                                  singletonCapSubst,
                                                  singletonSubst)
import qualified Language.Egison.Type.ShapeSolver as ShapeSolver
import           Language.Egison.Type.Types     (CapVar (..),
                                                  Capability (..),
                                                  Dual (..), DualScheme (..),
                                                  TypeScheme (..),
                                                  TyVar (..), Type (..),
                                                  dualSchemeTargetScheme,
                                                  mkTypeFormer)
import           Language.Egison.Type.Unify     (alignAtSlotWithConstraints,
                                                  matchCapability, matchOneWay,
                                                  unify, unifyCapability)

main :: IO ()
main = do
  args <- getArgs
  libTests <- discoverLibTests
  mapM_ (\(f, why) -> putStrLn ("Skipping " ++ f ++ " (" ++ why ++ ")"))
        skippedLibTests
  flip defaultMainWithArgs args . hUnitTestToTests . test $
    p2CapabilityTests
      ++ [ matcherOneWayTests
         , typePmCompatibilityWarningTests
         , patternFunctionDualSchemeTests
         , patternFunctionTypeErrorTests
         , strictPipelineTests
         , strictSelectedCoreTests
         , annotationRigidityTests
         , failedInferAtomicityTests
         , producerCyclePersistenceTests
         ]
      ++ map runTestCase (languageTests ++ libTests ++ sampleTests)

typePmCompatibilityWarningTests :: Test
typePmCompatibilityWarningTests =
  TestLabel "type-pm compatibility warnings" $ TestCase $ do
    let demoType = TInductive "NestedPPatDemo" []
        constructorScheme =
          Forall [] [] [] (TFun TInt (TFun demoType demoType))
        patternEnv =
          extendPatternEnv "join" constructorScheme $
            extendPatternEnv "cons" constructorScheme emptyPatternEnv
        nestedPrimitivePattern =
          PPInductivePat "join"
            [ PPPatVar
            , PPInductivePat "cons"
                [PPValuePat "val", PPPatVar]
            ]
        expression =
          IMatcherExpr
            [ ( nestedPrimitivePattern
              , ITupleExpr
                  [ IConstantExpr SomethingExpr
                  , IConstantExpr SomethingExpr
                  ]
              , [(PDPatVar (Var "nestedTarget" []), ICollectionExpr [])]
              )
            , ( PPPatVar
              , IConstantExpr SomethingExpr
              , [ ( PDPatVar (Var "fallbackTarget" [])
                  , ICollectionExpr [IVarExpr "fallbackTarget"]
                  )
                ]
              )
            ]
        config enabled =
          defaultInferConfig
            { cfgTypePmCompatibilityWarnings = enabled }
        state enabled =
          (initialInferStateWithConfig (config enabled))
            { inferPatternEnv = patternEnv }

    (resultOff, warningsOff) <-
      runInferWithWarnings
        (inferIExpr expression)
        (state False)
    (resultOn, warningsOn) <-
      runInferWithWarnings
        (inferIExpr expression)
        (state True)

    case (resultOff, resultOn) of
      (Right _, Right _) ->
        assertEqual
          "the reporting option must not change inference"
          (show resultOff)
          (show resultOn)
      (Left errorOff, Left errorOn) ->
        assertFailure
          ("the accepted nested primitive-pattern matcher failed with " ++
           "the option off/on: " ++ show errorOff ++ " / " ++ show errorOn)
      _ ->
        assertFailure
          "the reporting option changed whether the nested primitive-pattern matcher was accepted"
    assertEqual "the option is silent when disabled" [] warningsOff
    case warningsOn of
      [warning@(TypePmCompatibilityWarning detail _)] -> do
        assertBool
          "the warning identifies the nested primitive-pattern bridge"
          ("nested structured primitive-pattern pattern `join $ (cons #$val $)`"
            `isInfixOf` detail)
        let rendered = formatTypeWarning warning
        assertBool "the warning has the standard prefix"
          ("Warning:" `isInfixOf` rendered)
        assertBool "the warning explains that extended checking continues"
          ("Type checking continues through Egison's extension path"
            `isInfixOf` rendered)
      other ->
        assertFailure
          ("expected exactly one type-pm compatibility warning, got " ++
           show other)

    let slotExpression =
          IApplyExpr
            (IVarExpr "slotConsumer")
            [IVarExpr "opaqueMatcher"]
        slotState enabled =
          (initialInferStateWithConfig (config enabled))
            { declaredSymbols = Map.fromList
                [ ( "slotConsumer"
                  , TFun (TMatcherSlot CapNone TInt) TInt
                  )
                , ("opaqueMatcher", TAny)
                ]
            }

    (slotResultOff, slotWarningsOff) <-
      runInferWithWarnings
        (inferIExpr slotExpression)
        (slotState False)
    (slotResultOn, slotWarningsOn) <-
      runInferWithWarnings
        (inferIExpr slotExpression)
        (slotState True)

    assertEqual
      "slot warning reporting must not change inference"
      (show slotResultOff)
      (show slotResultOn)
    case slotResultOn of
      Right _ -> return ()
      Left err ->
        assertFailure
          ("the extended Any-to-slot path should remain accepted: " ++ show err)
    assertEqual
      "the Any-to-slot extension is silent when disabled"
      []
      slotWarningsOff
    case slotWarningsOn of
      [TypePmCompatibilityWarning detail _] ->
        assertBool
          "the Any-to-slot extension is reported at its explicit boundary"
          ("explicit slot use `_ <= MatcherSlot none Integer`"
            `isInfixOf` detail)
      other ->
        assertFailure
          ("expected exactly one Any-to-slot compatibility warning, got " ++
           show other)

patternFunctionDualSchemeTests :: Test
patternFunctionDualSchemeTests =
  TestLabel "pattern-function DualScheme" . TestList $
    [ TestLabel "definition stores one correlated DualScheme" . TestCase $ do
        let typeA = TyVar "a"
            typeB = TyVar "b"
            declaration =
              IPatternFunctionDecl
                "dualPair"
                [typeA, typeB]
                [("left", TVar typeA), ("right", TVar typeB)]
                (TTuple [TVar typeA, TVar typeB])
                (ITuplePat [IVarPat "left", IVarPat "right"])
            config =
              defaultInferConfig
                { cfgTypePmCompatibilityWarnings = True }
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
                      "dualPair" headerScheme emptyPatternEnv
                }

        (result, warnings, finalState) <-
          runInferWithWarningsAndState
            (inferITopExpr declaration)
            initialState

        assertEqual
          "a directly checked pattern-function definition is in the compatibility profile"
          []
          warnings
        case result of
          Right
            ( Just
                (TIPatternFunctionDecl
                  "dualPair" typedScheme _parameters _resultType _body)
            , _substitution
            ) ->
              case lookupPatternFunctionEnv
                     "dualPair" (inferPatternFuncEnv finalState) of
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
                        dualSchemeTargetScheme storedScheme
                  assertEqual
                    "the declaration environment stores the canonical target projection"
                    (Just targetProjection)
                    (lookupPatternEnv
                      "dualPair" (inferPatternFuncDeclEnv finalState))
                  assertEqual
                    "the ordinary environment stores the same target projection"
                    (Just targetProjection)
                    (lookupEnvExact
                      (Var "dualPair" []) (inferEnv finalState))
          Right other ->
            assertFailure
              ("unexpected typed pattern-function result: " ++ show other)
          Left err ->
            assertFailure
                  ("the correlated pattern-function definition failed: " ++ show err)

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
                    ]))
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
                  "the accepted shadowed definition lost its DualScheme"

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
                  "the pattern-bound definition lost its DualScheme"

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
                  { cfgTypePmCompatibilityWarnings = True }
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
                    "the extended body lost its inferred DualScheme"
                Just _ -> return ()
          case warnings of
            [TypePmCompatibilityWarning detail _] ->
              assertBool
                "the definition warning identifies the predicate-pattern boundary"
                ("predicate pattern" `isInfixOf` detail)
            other ->
              assertFailure
                ("expected one pattern-function body warning, got " ++ show other)

    , TestLabel "replacement masks an older DualScheme before forward use" .
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
                  fmap (fmap (length . dualArgs)) $
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
                  fmap (fmap (length . dualArgs)) $
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
              DualScheme
                [capabilityBinder, capabilityBinder]
                [targetBinder, targetBinder]
                [Dual (CapVar capabilityBinder) (TVar targetBinder)]
                (Dual (CapVar capabilityBinder) (TVar targetBinder))
        (result, warnings, _finalState) <-
          runInferWithWarningsAndState
            (instantiateDualSchemeInState malformedScheme)
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
              "duplicate DualScheme binders were silently instantiated"

    , TestLabel "instantiation freshens both sorts together" . TestCase $ do
        let capLeft = MkCapVar "leftCapability"
            capRight = MkCapVar "rightCapability"
            typeLeft = TyVar "leftTarget"
            typeRight = TyVar "rightTarget"
            scheme =
              DualScheme
                [capLeft, capRight]
                [typeLeft, typeRight]
                [ Dual
                    (CapVar capLeft)
                    (TMatcher (CapVar capLeft) (TVar typeLeft))
                , Dual
                    (CapVar capRight)
                    (TMatcher (CapVar capRight) (TVar typeRight))
                ]
                (Dual
                  (CapTuple [CapVar capLeft, CapVar capRight])
                  (TTuple
                    [ TMatcher (CapVar capLeft) (TVar typeLeft)
                    , TMatcher (CapVar capRight) (TVar typeRight)
                    ]))
            instantiateTwice = do
              first <- instantiateDualSchemeInState scheme
              second <- instantiateDualSchemeInState scheme
              return (first, second)

        (result, warnings, finalState) <-
          runInferWithWarningsAndState
            instantiateTwice
            (initialInferStateWithConfig defaultInferConfig)

        assertEqual "instantiation itself emits no warning" [] warnings
        case result of
          Left err ->
            assertFailure
              ("dual-scheme instantiation failed: " ++ show err)
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
                      allCapabilities =
                        firstCapabilities ++ secondCapabilities
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
                  assertBool
                    "fresh capability images are recorded as allocated"
                    (all
                      (\variable ->
                        Set.member variable
                          (inferAllocatedCapVars finalState))
                      allCapabilities)
                  assertBool
                    "fresh capability images are protected from strengthening"
                    (all
                      (\variable ->
                        Set.member variable (inferProtectedCaps finalState))
                      allCapabilities)
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
                DualScheme
                  []
                  [typeVariable]
                  [Dual CapNone (TVar typeVariable)]
                  (Dual CapNone (TVar typeVariable))
              namedApplication functionName =
                IMatchExpr
                  BFSMode
                  (IConstantExpr (IntegerExpr 1))
                  (IConstantExpr SomethingExpr)
                  [ ( IInductiveOrPApplyPat functionName [IPatVar "value"]
                    , IVarExpr "value"
                    )
                  ]
              config enabled =
                defaultInferConfig
                  { cfgTypePmCompatibilityWarnings = enabled }
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
              shadowedName = "shadowedPatternFunction"
              shadowedScheme =
                DualScheme [] [] [] (Dual CapNone TInt)
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
                    ])
              shadowedState enabled =
                let targetProjection =
                      dualSchemeTargetScheme shadowedScheme
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
            "a finalized named application has no compatibility warning"
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
            [TypePmCompatibilityWarning detail _] -> do
              assertBool
                "the warning identifies the header-only function"
                ("`headerIdentity`" `isInfixOf` detail)
              assertBool
                "the warning explains that the DualScheme is not finalized"
                ("uses only a header because its DualScheme is not finalized"
                  `isInfixOf` detail)
            other ->
              assertFailure
                ("expected exactly one header-only compatibility warning, got " ++
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
            [TypePmCompatibilityWarning detail _] ->
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
            [TypePmCompatibilityWarning detail _] ->
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
        DualScheme
          capabilityBinders
          targetBinders
          [ Dual (CapVar leftCapability) (TVar leftTarget)
          , Dual (CapVar rightCapability) (TVar rightTarget)
          ]
          (Dual
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
        ( [ Dual
              (CapVar leftCapability)
              (TMatcher (CapVar leftTargetCapability) (TVar leftTarget))
          , Dual
              (CapVar rightCapability)
              (TMatcher (CapVar rightTargetCapability) (TVar rightTarget))
          ]
          , Dual
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

  , TestLabel "P2: capability matching binds only the consumer" .
      TestCase $ do
        let producerVariable = MkCapVar "producer"
            consumerVariable = MkCapVar "consumer"
            collection capability =
              CapCon (mkTypeFormer "Collection" 1) [capability]
            producer = collection (CapVar producerVariable)
            consumer = collection (CapVar consumerVariable)
        substitution <-
          either (assertFailure . show) return
            (matchCapability producer consumer)
        assertEqual
          "the consumer variable must be solved to the producer capability"
          producer
          (applyCapSubst substitution consumer)
        assertEqual
          "the producer capability must remain unchanged"
          producer
          (applyCapSubst substitution producer)

  , TestLabel "P2: capability matching never strengthens a producer" .
      TestCase $ do
        let producer = CapVar (MkCapVar "producer")
            consumer =
              CapCon (mkTypeFormer "Collection" 1) [CapNone]
        case matchCapability producer consumer of
          Left _ ->
            return ()
          Right _ ->
            assertFailure
              "a consumer constructor bound the producer capability variable"

  , TestLabel "P2: a shared capability variable is not consumer-owned" .
      TestCase $ do
        let shared = MkCapVar "shared"
            producer =
              CapCon (mkTypeFormer "Collection" 1) [CapVar shared]
            consumer = CapVar shared
        case matchCapability producer consumer of
          Left _ ->
            return ()
          Right _ ->
            assertFailure
              "a shared variable was rebound through its consumer occurrence"

  , TestLabel "P2: target equality cannot strengthen the producer root" .
      TestCase $ do
        let producerVar = MkCapVar "producer"
            consumerVar = MkCapVar "consumer"
            listNone =
              CapCon (mkTypeFormer "Collection" 1) [CapNone]
            producer =
              TMatcher
                (CapVar producerVar)
                (TMatcher (CapVar producerVar) TInt)
            consumer =
              TMatcherSlot
                (CapVar consumerVar)
                (TMatcher listNone TInt)
        case alignAtSlotWithConstraints emptyClassEnv [] producer consumer of
          Left _ ->
            return ()
          Right _ ->
            assertFailure
              "target unification strengthened the producer capability"

  , TestLabel "P2: target capability bindings stay in consumer support" .
      TestCase $ do
        let targetVar = MkCapVar "target-only"
            listNone =
              CapCon (mkTypeFormer "Collection" 1) [CapNone]
            producer =
              TMatcher CapNone (TMatcher (CapVar targetVar) TInt)
            consumer =
              TMatcherSlot CapNone (TMatcher listNone TInt)
        case alignAtSlotWithConstraints emptyClassEnv [] producer consumer of
          Left _ ->
            return ()
          Right _ ->
            assertFailure
              "target unification introduced a capability binding outside the consumer"

  , TestLabel "P2: target ordinary variables remain specializable" .
      TestCase $ do
        let targetVar = TyVar "target"
            producer = TMatcher CapNone (TVar targetVar)
            consumer = TMatcherSlot CapNone TInt
        case alignAtSlotWithConstraints emptyClassEnv [] producer consumer of
          Left err ->
            assertFailure
              ("ordinary target specialization was rejected: " ++ show err)
          Right _ ->
            return ()

  , TestLabel "TypePM: generic equality does not perform slot coercion" .
      TestCase $ do
        case unify
          (TMatcher CapNone TInt)
          (TMatcherSlot CapNone TInt) of
          Left _ -> return ()
          Right _ ->
            assertFailure
              "generic equality accepted a producer-to-slot coercion"

  , TestLabel "TypePM: tuple matcher coercion requires an explicit slot use" .
      TestCase $ do
        let tupleMatcher =
              TTuple [TMatcher CapNone TInt, TMatcher CapNone TInt]
            productMatcher =
              TMatcher
                (CapTuple [CapNone, CapNone])
                (TTuple [TInt, TInt])
        case unify tupleMatcher productMatcher of
          Left _ -> return ()
          Right _ ->
            assertFailure
              "generic equality performed tuple-to-matcher coercion"

  , TestLabel "TypePM: core tuple coercion does not synthesize untracked duals" .
      TestCase $ do
        let unknownProducer = TTuple [TVar (TyVar "unknownProducer")]
            openConsumer =
              TMatcherSlot
                (CapVar (MkCapVar "openConsumerCap"))
                (TVar (TyVar "openConsumerTarget"))
        case alignAtSlotWithConstraints
          emptyClassEnv [] unknownProducer openConsumer of
          Left _ -> return ()
          Right _ ->
            assertFailure
              "core tuple coercion manufactured a dual outside InferState"

  , TestLabel "TypePM: Any cannot silently invent a matcher slot head" .
      TestCase $ do
        let consumer = TMatcherSlot CapNone TInt
        case alignAtSlotWithConstraints emptyClassEnv [] TAny consumer of
          Left _ -> return ()
          Right _ ->
            assertFailure
              "core slot alignment accepted Any as matcher evidence"

  , TestLabel "TypePM: tuple Any cannot silently invent a matcher slot head" .
      TestCase $ do
        let producers =
              TTuple [TAny, TMatcher CapNone TInt]
            consumer =
              TMatcherSlot
                (CapTuple [CapNone, CapNone])
                (TTuple [TInt, TInt])
        case alignAtSlotWithConstraints
          emptyClassEnv [] producers consumer of
          Left _ -> return ()
          Right _ ->
            assertFailure
              "core product-slot alignment accepted Any as matcher evidence"

  , TestLabel "P2: product slot coercion retains one shared witness" .
      TestCase $ do
        let shared = MkCapVar "shared"
            listNone =
              CapCon (mkTypeFormer "Collection" 1) [CapNone]
            producers =
              TTuple
                [ TMatcher (CapVar shared) TInt
                , TMatcher listNone TInt
                ]
            consumer =
              TMatcherSlot
                (CapTuple [CapVar shared, CapVar shared])
                (TTuple [TInt, TInt])
        case alignAtSlotWithConstraints emptyClassEnv [] producers consumer of
          Left _ ->
            return ()
          Right _ ->
            assertFailure
              "a later product component changed an earlier producer"

  , TestLabel "P2: one-way type matching keeps shared matcher variables rigid" .
      TestCase $ do
        let shared = TyVar "shared"
            slot = TTuple [TVar shared, TVar shared]
            matcher = TTuple [TVar shared, TInt]
        case matchOneWay slot matcher of
          Nothing ->
            return ()
          Just _ ->
            assertFailure
              "a slot occurrence rebound a variable shared with the matcher"

  , TestLabel "P2: one-way type matching shares one capability domain" .
      TestCase $ do
        let shared = MkCapVar "shared-capability"
            listNone =
              CapCon (mkTypeFormer "Collection" 1) [CapNone]
            slot =
              TTuple
                [ TMatcher (CapVar shared) TInt
                , TMatcher (CapVar shared) TInt
                ]
            matcher =
              TTuple
                [ TMatcher (CapVar shared) TInt
                , TMatcher listNone TInt
                ]
        case matchOneWay slot matcher of
          Nothing ->
            return ()
          Just _ ->
            assertFailure
              "a later component rebound a shared matcher capability"

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

-- | The P2 surface regressions must type-check without relying on the
-- permissive fallback.  The base library still contains five MathValue
-- instances whose implementations normally arrive from the CAS layer.  Give
-- those names inert, correctly typed test definitions, then load only the
-- ordinary matcher/list/maybe surface needed by this regression.
strictSelectedCoreTests :: Test
strictSelectedCoreTests =
  TestLabel "P2: strict selected-library and language regressions" . TestCase $ do
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
              ++ [ LoadFile "test/lib/core/p2-capability.egi"
                 , LoadFile "test/lib/core/pattern-function.egi"
                 ])
    case result of
      Left err ->
        assertFailure
          ("strict selected-library P2 regression failed: " ++ show err)
      Right _ ->
        return ()

-- | The standalone type-error corpus is normally checked by a separate
-- sweep.  Keep the two DualScheme-specific rejection boundaries in the
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

-- | Both sorts of binder in an explicit scheme are rigid for the duration of
-- checking.  These reject cases are wired into the normal HUnit suite so they
-- cannot silently regress behind the permissive command-line fallback used by
-- the standalone type-error corpus.
annotationRigidityTests :: Test
annotationRigidityTests =
  TestLabel "P2: annotation binders are rigid in both sorts" . TestList $
    map rejects
      [ ("test/type-error/83-p2-ordinary-annotation-rigidity.egi", "TSkolem")
      , ("test/type-error/84-p2-nested-annotation-rigidity.egi", "TSkolem")
      , ("test/type-error/85-p2-pattern-function-annotation-rigidity.egi", "TSkolem")
      , ("test/type-error/86-p2-pattern-function-nested-annotation-rigidity.egi", "TSkolem")
      , ("test/type-error/87-p2-capability-annotation-rigidity.egi", "Matcher $skc")
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
