{- |
Module      : Language.Egison.Type.Infer
Licence     : MIT

This module provides type inference for IExpr (Internal Expression).
This is the unified type inference module for Phase 5-6 of the Egison compiler:
  IExpr (Desugared, no types) → (Type, Subst)

This module consolidates all type inference functionality, including:
  - Hindley-Milner type inference
  - Type class constraint collection
  - Infer monad and state management
  - All helper functions

Note: This module only performs type inference and returns Type information.
The typed AST (TIExpr) is created in a separate phase by combining IExpr with Type.

Previous modules (Infer.hs for Expr, TypeInfer.hs for Expr→TypedExpr) are deprecated.
-}

module Language.Egison.Type.Infer
  ( -- * Type inference
    inferIExpr
  , inferITopExpr
  , inferITopExprs
    -- * Infer monad
  , Infer
  , InferState(..)
  , InferConfig(..)
  , initialInferState
  , initialInferStateWithConfig
  , defaultInferConfig
  , permissiveInferConfig
  , runInferWithWarnings
  , runInferWithWarningsAndState
    -- * Running inference
    -- * Helper functions
  , freshVar
  , instantiateDualSchemeInState
  , getEnv
  , setEnv
  , withEnv
  , unifyTypes
  , generalize
  , inferConstant
  , addWarning
  ) where

import           Control.Monad              (foldM, forM_, when, zipWithM, zipWithM_, unless)
import           Control.Monad.Except       (ExceptT, runExceptT, throwError, catchError)
import           Control.Monad.State.Strict (StateT, runStateT, get, gets, modify, put)
import           Data.List                  (isPrefixOf, nub, intercalate, zip4)
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import           Language.Egison.AST        (ConstantExpr (..), PrimitivePatPattern (..))
import           Language.Egison.IExpr      (IExpr (..), ITopExpr (..), TITopExpr (..)
                                            , TIExpr (..), TIExprNode (..)
                                            , IBindingExpr, TIBindingExpr
                                            , IMatchClause, TIMatchClause, IPatternDef, TIPatternDef
                                            , IPattern (..), ILoopRange (..)
                                            , TIPattern (..), TIPatternNode (..), TILoopRange (..)
                                            , IPrimitiveDataPattern, PDPatternBase (..)
                                            , extractNameFromVar, Var (..), Index (..), stringToVar
                                            , tiExprType, mapIExprTypes, mapIPatternTypes,
                                              mapTIExprChildren)
import           Language.Egison.Pretty     (prettyStr)
import           Language.Egison.Type.Env
import qualified Language.Egison.Type.Error as TE
import           Language.Egison.Type.Error (TypeError(..), TypeErrorContext(..), TypeWarning(..),
                                              emptyContext, withContext, withExpr)
import qualified Language.Egison.Type.Pretty as TP
import qualified Language.Egison.Type.Subtype as Subtype
import           Language.Egison.Type.Subst (Subst(..), applySubst, applySubstConstraint,
                                              applyCapSubst, applySubstDual,
                                              applySubstScheme, composeSubst, emptySubst,
                                              singletonSubst)
import           Language.Egison.Type.Tensor (normalizeTensorType)
import           Language.Egison.Type.Types
import qualified Language.Egison.Type.Types as Types
import           Language.Egison.Type.Unify as TU
import           Language.Egison.Type.Instance (findMatchingInstanceForType)

--------------------------------------------------------------------------------
-- * Infer Monad and State
--------------------------------------------------------------------------------

-- | Inference configuration
data InferConfig = InferConfig
  { cfgPermissive       :: Bool  -- ^ Treat unbound variables as warnings, not errors
  , cfgCollectWarnings  :: Bool  -- ^ Collect warnings during inference
  , cfgMatcherConsistencyWarnings :: Bool  -- ^ Emit matcher consistency warnings (paper Def 4.2):
                                 --   Coverage (4.2(3)).  Off by default, as the standard library
                                 --   has intentionally partial matchers (opt-in diagnostic;
                                 --   --matcher-consistency-warnings).  PP-Con (4.2(1a)) and arm
                                 --   exhaustiveness (4.2(1c)) are ordinary type errors.
  , cfgOutsideEgisonCoreWarnings :: Bool
                                 -- ^ Report uses that the production Egison checker accepts
                                 --   outside Egison core. This does not change typing or evaluation.
  , cfgPatternHoleBeforePrimitiveValuePatternWarnings :: Bool
                                 -- ^ Report primitive-pattern patterns whose DFS source order
                                 --   places a pattern hole before a primitive value pattern.
  , cfgNestedStructuredPrimitivePatternPatternWarnings :: Bool
                                 -- ^ Report structured primitive-pattern patterns nested inside
                                 --   another constructor or tuple.
  , cfgMatchWithoutElseWarnings :: Bool
                                 -- ^ Report a match or matchDFS expression whose final else
                                 --   branch is omitted.  This is diagnostic-only in Egison.
  }

instance Show InferConfig where
  show cfg = "InferConfig { cfgPermissive = " ++ show (cfgPermissive cfg)
           ++ ", cfgCollectWarnings = " ++ show (cfgCollectWarnings cfg)
           ++ ", cfgMatcherConsistencyWarnings = " ++ show (cfgMatcherConsistencyWarnings cfg)
           ++ ", cfgOutsideEgisonCoreWarnings = " ++ show (cfgOutsideEgisonCoreWarnings cfg)
           ++ ", cfgPatternHoleBeforePrimitiveValuePatternWarnings = " ++
                show (cfgPatternHoleBeforePrimitiveValuePatternWarnings cfg)
           ++ ", cfgNestedStructuredPrimitivePatternPatternWarnings = " ++
                show (cfgNestedStructuredPrimitivePatternPatternWarnings cfg)
           ++ ", cfgMatchWithoutElseWarnings = " ++
                show (cfgMatchWithoutElseWarnings cfg)
           ++ " }"

-- | Default configuration (strict mode)
defaultInferConfig :: InferConfig
defaultInferConfig = InferConfig
  { cfgPermissive = False
  , cfgCollectWarnings = False
  , cfgMatcherConsistencyWarnings = False
  , cfgOutsideEgisonCoreWarnings = False
  , cfgPatternHoleBeforePrimitiveValuePatternWarnings = False
  , cfgNestedStructuredPrimitivePatternPatternWarnings = False
  , cfgMatchWithoutElseWarnings = False
  }

-- | Permissive configuration (for gradual adoption)
permissiveInferConfig :: InferConfig
permissiveInferConfig = InferConfig
  { cfgPermissive = True
  , cfgCollectWarnings = True
  , cfgMatcherConsistencyWarnings = False
  , cfgOutsideEgisonCoreWarnings = False
  , cfgPatternHoleBeforePrimitiveValuePatternWarnings = False
  , cfgNestedStructuredPrimitivePatternPatternWarnings = False
  , cfgMatchWithoutElseWarnings = False
  }

-- | Inference state
data InferState = InferState
  { inferCounter     :: Int              -- ^ Fresh variable counter
  , inferEnv         :: TypeEnv          -- ^ Current type environment
  , inferWarnings    :: [TypeWarning]    -- ^ Collected warnings
  , inferConfig      :: InferConfig      -- ^ Configuration
  , inferClassEnv    :: ClassEnv         -- ^ Type class environment
  , inferPatternEnv  :: PatternTypeEnv   -- ^ Pattern constructor signatures only
  , inferPatternFuncDeclEnv :: PatternTypeEnv
                                          -- ^ Header-only target declarations used for name
                                          --   resolution before bodies have been checked.
  , inferPatternFuncEnv :: PatternFunctionEnv
                                          -- ^ Finalized capability/target DualSchemes.
  , inferPatfunParamDuals :: Map.Map String Dual
                                          -- ^ Pattern-parameter context while checking a definition
                                          --   body.  Each ~parameter carries its declared target and
                                          --   fresh capability together, as in TypePM.PatternCtx.
  , inferConstraints :: [Constraint]     -- ^ Accumulated type class constraints
  , declaredSymbols  :: Map.Map String Type  -- ^ Declared symbols with their types
  , inferGlobalSubst :: Subst             -- ^ The growing zonk substitution: every committed
                                          --   unification merges its result here, and the unify
                                          --   wrappers resolve both sides through it first.  Sibling
                                          --   subexpressions are inferred independently and their
                                          --   substitutions merged with the left-biased 'composeSubst',
                                          --   which on a conflicting binding silently keeps one side
                                          --   (e.g. two match sites committing the same lambda-bound
                                          --   matcher parameter to different matcher types).  Zonking
                                          --   makes the later unification see the earlier commitment,
                                          --   so the conflict is unified — and reported — instead of
                                          --   shadowed.  Reset per top-level item (a fresh InferState
                                          --   is seeded for each).
  , inferCasSubtypeEdges :: Subtype.SubtypeEnv
                                          -- ^ Declared `cas-subtype` edges (alias-expanded), seeded
                                          --   from EvalState per top-level item.  Consulted by the
                                          --   application-site CAS join: when two CAS operand types
                                          --   fail to unify, their unique join in the declared order
                                          --   becomes the promotion target and both operands are
                                          --   reshaped to it (elaboration inserts the coercion; the
                                          --   unifier itself never joins).
  , inferBatchDefNames :: Set.Set String  -- ^ Names of the definitions of the current load unit,
                                          --   seeded per batch by Eval.  An unbound variable that is
                                          --   in this set is a FORWARD reference (were it defined
                                          --   earlier it would be in the environment), so the warning
                                          --   can say how to fix it instead of "unbound".
  , inferDataConstructorNames :: Set.Set String
                                          -- ^ Value-level constructors currently registered by
                                          --   Eval.  Surface constructor applications desugar to
                                          --   ordinary variable applications, so this provenance
                                          --   selects structural scheme instantiation.
  , inferMatcherShapes :: Map.Map String [PrimitivePatPattern]
                                          -- ^ Clause pp shapes of top-level matcher definitions
                                          --   (name |-> pps of its (lambda-wrapped) matcher literal),
                                          --   harvested at IDefine. Consulted by the production
                                          --   use-site safeguard for outside-core primitive-pattern
                                          --   clauses to resolve matcher clause shapes statically.
  , inferMatchSiteCount :: Int
                                          -- ^ Number of match and matchAll expressions inferred.
  , inferMatcherLiteralCount :: Int
                                          -- ^ Number of matcher literals inferred.
  , inferMatcherClauseCount :: Int
                                          -- ^ Number of matcher-literal clauses inferred.
  , inferProductNextMatcherCount :: Int
                                          -- ^ Number of clauses with several holes whose next
                                          --   matcher is one product-typed expression rather
                                          --   than a syntactic tuple (canonical normalization).
  , inferCapabilityCombineCount :: Int
                                          -- ^ Number of capability equations between the two
                                          --   children of an and/or/forall/loop pattern.
  } deriving (Show)

-- | Initial inference state
initialInferState :: InferState
initialInferState = initialInferStateWithConfig defaultInferConfig

-- | Create initial state with config
initialInferStateWithConfig :: InferConfig -> InferState
initialInferStateWithConfig cfg = InferState
  { inferCounter = 0
  , inferEnv = emptyEnv
  , inferWarnings = []
  , inferConfig = cfg
  , inferClassEnv = emptyClassEnv
  , inferPatternEnv = emptyPatternEnv
  , inferPatternFuncDeclEnv = emptyPatternEnv
  , inferPatternFuncEnv = emptyPatternFunctionEnv
  , inferPatfunParamDuals = Map.empty
  , inferConstraints = []
  , declaredSymbols = Map.empty
  , inferGlobalSubst = emptySubst
  , inferCasSubtypeEdges = []
  , inferBatchDefNames = Set.empty
  , inferDataConstructorNames = Set.empty
  , inferMatcherShapes = Map.empty
  , inferMatchSiteCount = 0
  , inferMatcherLiteralCount = 0
  , inferMatcherClauseCount = 0
  , inferProductNextMatcherCount = 0
  , inferCapabilityCombineCount = 0
  }

-- | Inference monad (with IO for potential future extensions)
type Infer a = ExceptT TypeError (StateT InferState IO) a

-- | Run type inference and also return warnings
runInferWithWarnings :: Infer a -> InferState -> IO (Either TypeError a, [TypeWarning])
runInferWithWarnings m st = do
  (result, finalState) <- runStateT (runExceptT m) st
  return (result, inferWarnings finalState)

-- | Run inference and return result, warnings, and final state
runInferWithWarningsAndState :: Infer a -> InferState -> IO (Either TypeError a, [TypeWarning], InferState)
runInferWithWarningsAndState m st = do
  (result, finalState) <- runStateT (runExceptT m) st
  return (result, inferWarnings finalState, finalState)

--------------------------------------------------------------------------------
-- * Helper Functions
--------------------------------------------------------------------------------

-- | Add a warning
addWarning :: TypeWarning -> Infer ()
addWarning w = modify $ \st -> st { inferWarnings = w : inferWarnings st }

-- | Emit an opt-in diagnostic when production Egison accepts an extension
-- outside Egison core. The flag controls reporting only; inference always
-- follows the same extended-Egison path.
warnOutsideEgisonCore :: String -> TypeErrorContext -> Infer ()
warnOutsideEgisonCore detail ctx = do
  enabled <- cfgOutsideEgisonCoreWarnings <$> gets inferConfig
  when enabled $
    addWarning (OutsideEgisonCoreWarning detail ctx)

warnMatchWithoutElse :: TypeErrorContext -> Maybe IExpr -> Infer ()
warnMatchWithoutElse ctx fallback = do
  enabled <- cfgMatchWithoutElseWarnings <$> gets inferConfig
  when (enabled && maybe True (const False) fallback) $
    addWarning (MatchWithoutElseWarning ctx)

-- | The production bridge treats nested structured primitive-pattern patterns
-- as a dedicated opt-in diagnostic. Lean's core syntax can represent this
-- recursion; the warning records that its end-to-end Egison-to-core bridge has
-- not yet been validated.
hasNestedStructuredPPat :: PrimitivePatPattern -> Bool
hasNestedStructuredPPat pattern =
  case pattern of
    PPInductivePat _ children -> any isStructured children
    PPTuplePat children       -> any isStructured children
    _                         -> False
  where
    isStructured PPInductivePat{} = True
    isStructured PPTuplePat{}     = True
    isStructured _                = False

-- | Detect the core ordering boundary in depth-first, left-to-right source
-- order. Once a pattern hole has been visited, a later primitive value
-- pattern is outside Egison core, even when the two leaves are nested under
-- different structured nodes.
hasPatternHoleBeforePrimitiveValuePattern :: PrimitivePatPattern -> Bool
hasPatternHoleBeforePrimitiveValuePattern = snd . visit False
  where
    visit seenHole pattern =
      case pattern of
        PPWildCard -> (seenHole, False)
        PPPatVar -> (True, False)
        PPValuePat _ -> (seenHole, seenHole)
        PPInductivePat _ children -> visitMany seenHole children
        PPTuplePat children -> visitMany seenHole children

    visitMany seenHole [] = (seenHole, False)
    visitMany seenHole (pattern : patterns) =
      let (seenHole', violation) = visit seenHole pattern
          (seenHole'', laterViolation) = visitMany seenHole' patterns
      in (seenHole'', violation || laterViolation)

-- | Render a primitive-pattern pattern without losing its tree shape.
-- 'Pretty PrimitivePatPattern' historically omits parentheses around nested
-- constructors, which makes (for example) @join $ (cons #$val $)@ look flat
-- in a diagnostic. Outside-core warnings need a round-trippable account of
-- the boundary that was actually encountered.
renderPrimitivePatPattern :: PrimitivePatPattern -> String
renderPrimitivePatPattern pattern =
  case pattern of
    PPWildCard -> "_"
    PPPatVar -> "$"
    PPValuePat name -> "#$" ++ name
    PPInductivePat name children ->
      unwords (name : map renderChild children)
    PPTuplePat children ->
      "(" ++ intercalate ", " (map renderPrimitivePatPattern children) ++ ")"
  where
    renderChild child@PPInductivePat{} =
      "(" ++ renderPrimitivePatPattern child ++ ")"
    renderChild child@PPTuplePat{} =
      renderPrimitivePatPattern child
    renderChild child =
      renderPrimitivePatPattern child

primitivePatPatternBinders :: PrimitivePatPattern -> [String]
primitivePatPatternBinders pattern =
  case pattern of
    PPValuePat name          -> [name]
    PPInductivePat _ children -> concatMap primitivePatPatternBinders children
    PPTuplePat children      -> concatMap primitivePatPatternBinders children
    _                        -> []

duplicateNames :: [String] -> [String]
duplicateNames names =
  nub [name | name <- names, length (filter (== name) names) > 1]

-- | Report matcher well-formedness conditions enforced by the mechanized
-- checker but accepted by extended Egison.  Keeping these as diagnostics lets
-- existing programs run while making the boundary visible on demand.
warnMatcherCompatibility
  :: TypeErrorContext
  -> IPatternDef
  -> Infer ()
warnMatcherCompatibility ctx (pp, _, arms) = do
  when (hasNestedStructuredPPat pp) $ do
    enabled <- cfgNestedStructuredPrimitivePatternPatternWarnings <$>
      gets inferConfig
    when enabled $
      addWarning
        (NestedStructuredPrimitivePatternPatternWarning
          (renderPrimitivePatPattern pp) ctx)
  when (hasPatternHoleBeforePrimitiveValuePattern pp) $ do
    enabled <- cfgPatternHoleBeforePrimitiveValuePatternWarnings <$>
      gets inferConfig
    when enabled $
      addWarning
        (PatternHoleBeforePrimitiveValuePatternWarning
          (renderPrimitivePatPattern pp) ctx)
  let ppBinders = primitivePatPatternBinders pp
      duplicatePpBinders = duplicateNames ppBinders
  unless (null duplicatePpBinders) $
    warnOutsideEgisonCore
      ("primitive-pattern pattern `" ++ renderPrimitivePatPattern pp ++
       "` binds #$ name(s) more than once: " ++
       intercalate ", " duplicatePpBinders)
      ctx
  forM_ arms $ \(dataPattern, _) -> do
    let armBinders = primitivePatternNames dataPattern
        duplicateArmBinders = duplicateNames armBinders
        overlappingBinders = nub (filter (`elem` ppBinders) armBinders)
    unless (null duplicateArmBinders) $
      warnOutsideEgisonCore
        ("a data-pattern arm of primitive-pattern pattern `" ++
         renderPrimitivePatPattern pp ++
         "` binds name(s) more than once: " ++
         intercalate ", " duplicateArmBinders)
        ctx
    unless (null overlappingBinders) $
      warnOutsideEgisonCore
        ("a data-pattern arm of primitive-pattern pattern `" ++
         renderPrimitivePatPattern pp ++
         "` rebinds #$ name(s): " ++ intercalate ", " overlappingBinders)
        ctx

-- | Matcher-facing pattern forms still handled by Egison's extension layer
-- rather than the direct TypePM.Pattern bridge.  Supported children are
-- traversed so one match clause receives a compact, deduplicated set of
-- diagnostics.
typePmPatternExtensions :: IPattern -> [String]
typePmPatternExtensions = nub . go
  where
    go pattern =
      case pattern of
        IWildCard -> []
        IPatVar _ -> []
        IValuePat _ -> []
        IPredPat _ -> ["predicate pattern"]
        IIndexedPat child _ -> "indexed pattern" : go child
        ILetPat _ child -> "pattern-local let" : go child
        INotPat child -> "not-pattern" : go child
        IAndPat left right -> go left ++ go right
        IOrPat left right -> go left ++ go right
        IForallPat left right -> "forall-pattern" : go left ++ go right
        ITuplePat children -> concatMap go children
        IInductivePat _ children -> concatMap go children
        ILoopPat _ (ILoopRange _ _ rangePattern) body endPattern ->
          "loop pattern" :
            (go rangePattern ++ go body ++ go endPattern)
        IContPat -> ["continuation pattern"]
        -- The application branch itself emits the option-controlled warning.
        -- Keeping it out of this pre-pass avoids reporting the same explicit
        -- extension once during admissibility probing and again during the
        -- actual pattern inference.
        IPApplyPat _ children -> concatMap go children
        IVarPat _ -> []
        IInductiveOrPApplyPat _ children ->
          concatMap go children
        ISeqNilPat -> ["sequential-pattern terminator"]
        ISeqConsPat left right ->
          "sequential pattern" : go left ++ go right
        ILaterPatVar -> ["later pattern variable"]
        IDApplyPat function children ->
          "symbolic dynamic pattern application" :
            go function ++ concatMap go children

-- | Resolve the constructor-or-pattern-function surface node before the
-- speculative structural pass.  Warnings emitted inside that pass are rolled
-- back intentionally, so known pattern-function applications must be added to
-- the inventory here.
resolvedPatternBridgeExtensions :: IPattern -> Infer [String]
resolvedPatternBridgeExtensions pattern = do
  patternFunctionEnv <- getPatternFuncEnv
  patternFunctionDeclEnv <- getPatternFuncDeclEnv
  patternConstructorEnv <- getPatternEnv
  return . nub $
    go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv pattern
  where
    go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv current =
      case current of
        IWildCard -> []
        IPatVar _ -> []
        IValuePat _ -> []
        IPredPat _ -> []
        IIndexedPat child _ ->
          go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv child
        ILetPat _ child ->
          go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv child
        INotPat child ->
          go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv child
        IAndPat left right ->
          go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv left ++
          go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv right
        IOrPat left right ->
          go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv left ++
          go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv right
        IForallPat left right ->
          go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv left ++
          go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv right
        ITuplePat children ->
          concatMap
            (go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv)
            children
        IInductivePat name children ->
          (case lookupPatternEnv name patternConstructorEnv of
             Just _ -> []
             Nothing ->
               ["pattern constructor application without a frozen pattern signature"])
          ++ concatMap
               (go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv)
               children
        ILoopPat _ (ILoopRange _ _ rangePattern) body endPattern ->
          go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv rangePattern ++
          go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv body ++
          go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv endPattern
        IContPat -> []
        IPApplyPat _ children ->
          concatMap
            (go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv)
            children
        IVarPat _ -> []
        IInductiveOrPApplyPat name children ->
          (case ( lookupPatternFunctionEnv name patternFunctionEnv
                , lookupPatternEnv name patternFunctionDeclEnv
                ) of
             (Just _, _) -> []
             (Nothing, Just _) -> []
             (Nothing, Nothing) ->
               case lookupPatternEnv name patternConstructorEnv of
                 Just _ -> []
                 Nothing ->
                   ["pattern constructor application without a frozen pattern signature"])
          ++ concatMap
               (go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv)
               children
        ISeqNilPat -> []
        ISeqConsPat left right ->
          go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv left ++
          go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv right
        ILaterPatVar -> []
        IDApplyPat function children ->
          go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv function ++
          concatMap
            (go patternFunctionEnv patternFunctionDeclEnv patternConstructorEnv)
            children

-- | Context-shape conditions that TypePM records as an ordered, duplicate-free
-- output context.  Egison intentionally retains non-linear and order-insensitive
-- cases, so they are compatibility diagnostics rather than new hard errors.
patternContextCompatibilityIssues :: IPattern -> [String]
patternContextCompatibilityIssues pattern = nub (go pattern)
  where
    duplicateIssue current =
      case duplicateNames (outputBinders current) of
        [] -> []
        names ->
          ["pattern binds name(s) more than once: " ++ intercalate ", " names]

    go current =
      duplicateIssue current ++ case current of
        IOrPat left right ->
          let leftNames = outputBinders left
              rightNames = outputBinders right
              sameNames =
                all (`elem` rightNames) leftNames &&
                all (`elem` leftNames) rightNames
              orderIssue =
                [ "or-pattern branches bind the same names in different order: " ++
                  intercalate ", " leftNames ++ " / " ++
                  intercalate ", " rightNames
                | sameNames, leftNames /= rightNames
                ]
          in orderIssue ++ go left ++ go right
        IAndPat left right -> go left ++ go right
        IForallPat left right -> go left ++ go right
        ITuplePat children -> concatMap go children
        IInductivePat _ children -> concatMap go children
        IIndexedPat child _ -> go child
        ILetPat _ child -> go child
        INotPat child -> go child
        ILoopPat _ (ILoopRange _ _ rangePattern) body endPattern ->
          go rangePattern ++ go body ++ go endPattern
        IPApplyPat _ children -> concatMap go children
        IInductiveOrPApplyPat _ children -> concatMap go children
        ISeqConsPat left right -> go left ++ go right
        IDApplyPat function children -> go function ++ concatMap go children
        _ -> []

    outputBinders current =
      case current of
        IPatVar name -> [name]
        IIndexedPat child _ -> outputBinders child
        ILetPat _ child -> outputBinders child
        INotPat _ -> []
        IAndPat left right -> outputBinders left ++ outputBinders right
        IOrPat left _ -> outputBinders left
        IForallPat left right -> outputBinders left ++ outputBinders right
        ITuplePat children -> concatMap outputBinders children
        IInductivePat _ children -> concatMap outputBinders children
        ILoopPat _ (ILoopRange _ _ rangePattern) body endPattern ->
          outputBinders rangePattern ++ outputBinders body ++ outputBinders endPattern
        IPApplyPat _ children -> concatMap outputBinders children
        IVarPat name -> [name]
        IInductiveOrPApplyPat _ children -> concatMap outputBinders children
        ISeqConsPat left right -> outputBinders left ++ outputBinders right
        IDApplyPat function children ->
          outputBinders function ++ concatMap outputBinders children
        _ -> []

-- | The permissive-mode unbound-variable warning, upgraded to the
-- forward-reference variant when the name is a definition of the current
-- load unit: it must be defined LATER than this reference (an earlier
-- definition would already be in the environment), which has a concrete
-- fix (annotate it -- signatures are collected in a prepass).
warnUnboundVariable :: String -> TypeErrorContext -> Infer ()
warnUnboundVariable name ctx = do
  batchNames <- inferBatchDefNames <$> get
  if name `Set.member` batchNames
    then addWarning (ForwardReferenceWarning name ctx)
    else addWarning (UnboundVariableWarning name ctx)

-- | Add type class constraints (with deduplication and superclass propagation)
-- When adding a constraint like "Ord a", this also adds superclass constraints
-- (e.g., "Eq a") recursively, so that superclass methods are available.
addConstraints :: [Constraint] -> Infer ()
addConstraints cs = do
  classEnv <- getClassEnv
  let expanded = expandSuperclasses classEnv cs
  modify $ \st ->
    let existing = inferConstraints st
        newConstraints = filter (`notElem` existing) expanded
    in st { inferConstraints = existing ++ newConstraints }

-- | Expand a list of constraints by recursively adding superclass constraints.
-- e.g., [Ord a] -> [Ord a, Eq a]  (since Ord extends Eq)
expandSuperclasses :: ClassEnv -> [Constraint] -> [Constraint]
expandSuperclasses classEnv = go []
  where
    go seen [] = seen
    go seen (c:rest)
      | c `elem` seen = go seen rest
      | otherwise =
          let supers = case lookupClass (constraintClass c) classEnv of
                Nothing -> []
                -- Superclasses inherit ALL the type arguments from the subclass
                -- (e.g. `class AddSemigroup a` superclassed by `class AddMonoid a`).
                -- This generalizes correctly to multi-param classes if the
                -- superclass has the same type-parameter arity (which is the
                -- common case in Egison).
                Just info -> map (\superName -> Constraint superName (constraintTypes c))
                                 (classSupers info)
          in go (seen ++ [c]) (supers ++ rest)

-- | Get accumulated constraints
getConstraints :: Infer [Constraint]
getConstraints = inferConstraints <$> get

-- | Clear accumulated constraints
clearConstraints :: Infer ()
clearConstraints = modify $ \st -> st { inferConstraints = [] }

-- | Check if we're in permissive mode
isPermissive :: Infer Bool
isPermissive = cfgPermissive . inferConfig <$> get

-- | Generate a fresh type variable
freshVar :: String -> Infer Type
freshVar prefix = do
  st <- get
  let n = inferCounter st
  put st { inferCounter = n + 1 }
  return $ TVar $ TyVar $ prefix ++ show n

-- | Generate a fresh flexible capability variable.
freshCapability :: String -> Infer Capability
freshCapability prefix = do
  st <- get
  let n = inferCounter st
      variable = MkCapVar (prefix ++ show n)
  put st { inferCounter = n + 1 }
  return (CapVar variable)

-- | Instantiate a polymorphic scheme.  Capability variables are ordinary
-- unification variables; only annotation skolems are rigid.
instantiateSchemeInState :: TypeScheme -> Infer ([Constraint], Type)
instantiateSchemeInState scheme = do
  st <- get
  let (constraints, ty, nextCounter) = instantiate scheme (inferCounter st)
  put st { inferCounter = nextCounter }
  return (constraints, ty)

-- | Instantiate all capability and target binders of a pattern-function
-- scheme in one freshening step.  The same paired substitution is applied to
-- every argument and the result.
instantiateDualSchemeInState :: DualScheme -> Infer ([Dual], Dual)
instantiateDualSchemeInState = instantiateDualSchemeFresh

-- | A named pattern-function application creates pattern demands inside the
-- current match cut.  Its dual binders are therefore local consumer metas,
-- like constructor-local pattern templates, rather than exported producer
-- instances.
instantiateDualSchemeForPatternApplication
  :: DualScheme
  -> Infer ([Dual], Dual)
instantiateDualSchemeForPatternApplication =
  instantiateDualSchemeFresh

instantiateDualSchemeFresh :: DualScheme -> Infer ([Dual], Dual)
instantiateDualSchemeFresh scheme = do
  let duplicateCapabilities = duplicates (dualCapBinders scheme)
      duplicateTargets = duplicates (dualTyBinders scheme)
  unless (null duplicateCapabilities && null duplicateTargets) $
    throwError $ MatcherCapabilityError
      ("malformed pattern-function DualScheme: duplicate binder(s); " ++
       "capability = " ++ show duplicateCapabilities ++
       ", target = " ++ show duplicateTargets)
      emptyContext
  capabilityBindings <- mapM freshCapabilityBinding (dualCapBinders scheme)
  targetBindings <- mapM freshTargetBinding (dualTyBinders scheme)
  let substitution =
        Subst
          (Map.fromList targetBindings)
          (Map.fromList capabilityBindings)
  return
    ( map (applySubstDual substitution) (dualArgs scheme)
    , applySubstDual substitution (dualResult scheme)
    )
  where
    freshCapabilityBinding binder = do
      image <- freshCapability "dualCap"
      return (binder, image)
    freshTargetBinding binder = do
      image <- freshVar "dualTarget"
      return (binder, image)
    duplicates values =
      nub
        [ value
        | value <- values
        , length (filter (== value) values) > 1
        ]

-- | Generalize one complete list of pattern-argument/result duals relative to
-- the frozen constructor/function signatures and current expression context.
-- Capability and ordinary variables are quantified independently.  A
-- non-ambient capability variable with exactly one occurrence in the complete
-- argument/result payload carries no correlation, so it is canonicalized to
-- the ground capability 'CapAny'.  A literal 'CapAny' is a wildcard only in
-- producer-to-consumer matching.  Variables with two or more
-- occurrences remain quantified and preserve their sharing.  Explicit
-- annotation binders are lexical: they remain eligible for this canonical
-- generalization even when an unrelated ambient scheme happens to use the
-- same printed variable name.
generalizeDualSchemeInState
  :: [CapVar] -> [TyVar] -> [Dual] -> Dual -> Infer DualScheme
generalizeDualSchemeInState declaredCapabilities declaredTargets arguments result = do
  state <- get
  let payload = result : arguments
      payloadCaps = Set.unions (map freeCapVarsDual payload)
      payloadTypes = Set.unions (map freeTyVarsDual payload)
      lexicalCaps = Set.fromList declaredCapabilities
      lexicalTypes = Set.fromList declaredTargets
      ambientCaps =
        freeCapVarsInEnv (inferEnv state)
          `Set.union` patternTypeEnvFreeCaps (inferPatternEnv state)
          `Set.union` patternTypeEnvFreeCaps (inferPatternFuncDeclEnv state)
          `Set.union` patternFunctionEnvFreeCaps (inferPatternFuncEnv state)
      ambientTypes =
        freeVarsInEnv (inferEnv state)
          `Set.union` patternTypeEnvFreeTypes (inferPatternEnv state)
          `Set.union` patternTypeEnvFreeTypes (inferPatternFuncDeclEnv state)
          `Set.union` patternFunctionEnvFreeTypes (inferPatternFuncEnv state)
      generalizableCaps =
        payloadCaps `Set.difference` (ambientCaps `Set.difference` lexicalCaps)
      capabilityOccurrences :: Map.Map CapVar Int
      capabilityOccurrences =
        Map.unionsWith (+) (map dualCapabilityOccurrences payload)
      singletonCaps =
        Set.filter
          (\variable ->
            Map.findWithDefault 0 variable capabilityOccurrences == 1)
          generalizableCaps
      singletonDefault =
        Subst Map.empty
          (Map.fromList
            [ (variable, CapAny)
            | variable <- Set.toList singletonCaps
            ])
      arguments' = map (applySubstDual singletonDefault) arguments
      result' = applySubstDual singletonDefault result
  return DualScheme
    { dualCapBinders =
        Set.toList (generalizableCaps `Set.difference` singletonCaps)
    , dualTyBinders =
        Set.toList
          (payloadTypes `Set.difference` (ambientTypes `Set.difference` lexicalTypes))
    , dualArgs = arguments'
    , dualResult = result'
    }
  where
    dualCapabilityOccurrences (Dual capability target) =
      Map.unionWith (+)
        (capabilityOccurrencesInCapability capability)
        (capabilityOccurrencesInType target)

    capabilityOccurrencesInCapability capability =
      case capability of
        CapAny ->
          Map.empty
        CapVar variable ->
          Map.singleton variable 1
        CapSkolem _ ->
          Map.empty
        CapCon _ children ->
          Map.unionsWith (+)
            (map capabilityOccurrencesInCapability children)
        CapTuple components ->
          Map.unionsWith (+)
            (map capabilityOccurrencesInCapability components)

    capabilityOccurrencesInType ty =
      case ty of
        TTuple components ->
          combineTypes components
        TCollection element ->
          capabilityOccurrencesInType element
        TInductive _ arguments' ->
          combineTypes arguments'
        TTensor element ->
          capabilityOccurrencesInType element
        THash key value ->
          combineTypes [key, value]
        TMatcher capability target ->
          Map.unionWith (+)
            (capabilityOccurrencesInCapability capability)
            (capabilityOccurrencesInType target)
        TFun argument result' ->
          combineTypes [argument, result']
        TIO value ->
          capabilityOccurrencesInType value
        TIORef value ->
          capabilityOccurrencesInType value
        TTerm coefficient _ ->
          capabilityOccurrencesInType coefficient
        TFrac coefficient ->
          capabilityOccurrencesInType coefficient
        TPoly coefficient _ ->
          capabilityOccurrencesInType coefficient
        _ ->
          Map.empty
      where
        combineTypes =
          Map.unionsWith (+) . map capabilityOccurrencesInType

    patternTypeEnvFreeCaps environment =
      Set.unions
        [ schemeFreeCaps scheme
        | (_, scheme) <- patternEnvToList environment
        ]
    patternTypeEnvFreeTypes environment =
      Set.unions
        [ schemeFreeTypes scheme
        | (_, scheme) <- patternEnvToList environment
        ]
    patternFunctionEnvFreeCaps environment =
      Set.unions
        [ freeCapVarsDualScheme scheme
        | (_, scheme) <- patternFunctionEnvToList environment
        ]
    patternFunctionEnvFreeTypes environment =
      Set.unions
        [ freeTyVarsDualScheme scheme
        | (_, scheme) <- patternFunctionEnvToList environment
        ]
    schemeFreeCaps (Forall capBinders _ constraints target) =
      let constraintCaps =
            Set.unions
              [ freeCapVars ty
              | Constraint _ types <- constraints
              , ty <- types
              ]
      in (freeCapVars target `Set.union` constraintCaps)
          `Set.difference` Set.fromList capBinders
    schemeFreeTypes (Forall _ tyBinders constraints target) =
      let constraintTypes' =
            Set.unions
              [ freeTyVars ty
              | Constraint _ types <- constraints
              , ty <- types
              ]
      in (freeTyVars target `Set.union` constraintTypes')
          `Set.difference` Set.fromList tyBinders

-- | Get the current type environment
getEnv :: Infer TypeEnv
getEnv = inferEnv <$> get

-- | Set the type environment
setEnv :: TypeEnv -> Infer ()
setEnv env = modify $ \st -> st { inferEnv = env }

-- | Get the current pattern type environment
getPatternEnv :: Infer PatternTypeEnv
getPatternEnv = inferPatternEnv <$> get

-- | Get header-only pattern function declarations (for disambiguation and
-- warned forward-reference fallback).
getPatternFuncDeclEnv :: Infer PatternTypeEnv
getPatternFuncDeclEnv = inferPatternFuncDeclEnv <$> get

-- | Get finalized pattern-function DualSchemes.
getPatternFuncEnv :: Infer PatternFunctionEnv
getPatternFuncEnv = inferPatternFuncEnv <$> get

-- | Get the current class environment
getClassEnv :: Infer ClassEnv
getClassEnv = inferClassEnv <$> get

-- | Resolve a constraint based on available instances
-- If the constraint type is a Tensor type and no instance exists for it,
-- try to use the element type's instance instead
-- | Resolve constraints in a TIExpr recursively
resolveConstraintsInTIExpr :: ClassEnv -> Subst -> TIExpr -> TIExpr
resolveConstraintsInTIExpr classEnv subst
                           (TIExpr (Forall capVars vars constraints ty) node) =
  let resolvedConstraints = map (resolveConstraintWithInstances classEnv subst) constraints
      resolvedNode = mapTIExprChildren (resolveConstraintsInTIExpr classEnv subst) node
  in TIExpr (Forall capVars vars resolvedConstraints ty) resolvedNode

resolveConstraintWithInstances :: ClassEnv -> Subst -> Constraint -> Constraint
resolveConstraintWithInstances classEnv subst (Constraint className tyArgs) =
  let resolvedTypes = map (applySubst subst) tyArgs
      instances = lookupInstances className classEnv
      -- For multi-param constraints we apply Tensor unwrapping to the principal
      -- (first) type only; secondary types are passed through. This matches the
      -- existing semantics for single-param classes.
      resolvedFirst = case resolvedTypes of (t:_) -> t; [] -> TAny
      adjustFirst newFirst = case resolvedTypes of
                               (_:rest) -> newFirst : rest
                               []       -> [newFirst]
  in case resolvedFirst of
       TTensor elemType ->
         case findMatchingInstanceForType resolvedFirst instances of
           Just _  -> Constraint className resolvedTypes
           Nothing -> Constraint className (adjustFirst elemType)
       _ ->
         Constraint className resolvedTypes

-- | Strict signature-completeness check for annotated definitions.
-- Residual constraints (left in the inference state after checking the body)
-- that mention the signature's type variables must be entailed by the
-- (superclass-expanded) signature constraints; otherwise the signature is
-- missing a declaration the body relies on.  We reject such definitions
-- instead of silently emitting unresolvable dictionary references (the
-- declared signature is the contract; if the body needs {Ord a}, the
-- signature must say so).
checkResidualConstraints :: String -> [Constraint] -> Type -> Subst -> TypeErrorContext -> Infer ()
checkResidualConstraints defName sigConstraints finalType finalSubst ctx = do
  residual <- getConstraints
  classEnv <- getClassEnv
  let sigCs = map (applySubstConstraint finalSubst) sigConstraints
      sigVars = freeTyVars finalType
      sigSkolems = freeTySkolems finalType
      hasVar c =
        any
          (\ty -> not (Set.null (freeTyVars ty))
               || not (Set.null (freeTySkolems ty)))
          (constraintTypes c)
      mentionsSig c =
        any
          (\ty ->
            not (Set.null
              (freeTyVars ty `Set.intersection` sigVars))
            || not (Set.null
              (freeTySkolems ty `Set.intersection` sigSkolems)))
          (constraintTypes c)
      entailed c = any (\sc -> constraintClass sc == constraintClass c
                            && constraintTypes sc == constraintTypes c) sigCs

      -- Reduce a constraint by instance resolution: an instance matching the
      -- constraint stands for its context, e.g. {Eq (Tensor t)} with
      -- `instance {Eq a} Eq (Tensor a)` reduces to {Eq t} (which the
      -- signature may then entail).  A Tensor type with no instance defers
      -- the constraint to its element type, mirroring
      -- resolveConstraintWithInstances.  Instance matching is one-way: the
      -- instance head's variables are bound, the constraint's types are
      -- rigid (TU.matchOneWay).
      reduceC :: Int -> Constraint -> [Constraint]
      reduceC 0 c = [c]
      reduceC d c@(Constraint cls tys)
        | entailed c = []
        | otherwise =
            let insts = lookupInstances cls classEnv
                matches = [ (inst, th)
                          | inst <- insts
                          , length (instTypes inst) == length tys
                          , Just th <- [matchTypesOneWay (instTypes inst) tys] ]
            in case matches of
                 ((inst, th) : _) ->
                   concatMap (reduceC (d - 1))
                             (map (applySubstConstraint th) (instContext inst))
                 [] -> case tys of
                   (TTensor el : restT) -> reduceC (d - 1) (Constraint cls (el : restT))
                   _ -> [c]
      -- Match the complete multi-parameter head in one product judgment.
      -- Folding pairwise after applying the accumulated substitution to the
      -- next consumer erased whether a nested capability Any was literal or
      -- came from an earlier variable binding.
      matchTypesOneWay ps ts =
        TU.matchOneWay (TTuple ps) (TTuple ts)

      residual' = concatMap (reduceC 5 . applySubstConstraint finalSubst) residual
      missing = nub [ c | c <- residual', hasVar c, mentionsSig c, not (entailed c) ]
  when (not (null missing)) $
    throwError $ TE.MissingSignatureConstraint defName missing ctx

-- | Restore the declared binders after a successful rigid annotation check.
--
-- Ordinary and capability skolems are checking-only constants.  They must
-- not escape into the typed tree consumed by dictionary elaboration or into
-- the inference state used by a following member of an IDefineMany batch.
deskolemizeAnnotationType :: AnnotationSkolems -> Type -> Type
deskolemizeAnnotationType skolems =
  mapType replaceType . mapTypeCapabilities replaceCapability
  where
    replaceType (TSkolem fresh) =
      case declaredTypeBinder fresh of
        Just declared -> TVar declared
        Nothing       -> TSkolem fresh
    replaceType (TVar fresh) =
      case declaredTypeBinder fresh of
        Just declared -> TVar declared
        Nothing       -> TVar fresh
    replaceType ty = ty

    declaredTypeBinder fresh =
      case
        [ declared
        | (candidate, declared) <- annotationTySkolems skolems
        , tyVarName candidate == tyVarName fresh
        ] of
        declared : _ -> Just declared
        [] -> Nothing

    replaceCapability (CapSkolem fresh) =
      case lookup fresh (annotationCapSkolems skolems) of
        Just declared -> CapVar declared
        Nothing       -> CapSkolem fresh
    replaceCapability capability = capability

-- | Restore declared capability binders in a standalone capability value.
-- Unlike 'deskolemizeAnnotationType', this also covers the capability half of
-- a pattern-function dual, which is not embedded in an ordinary Type.
deskolemizeAnnotationCapability
  :: AnnotationSkolems
  -> Capability
  -> Capability
deskolemizeAnnotationCapability skolems =
  mapCapability $ \capability ->
    case capability of
      CapSkolem fresh ->
        case lookup fresh (annotationCapSkolems skolems) of
          Just declared -> CapVar declared
          Nothing       -> capability
      _ -> capability

deskolemizeAnnotationDual :: AnnotationSkolems -> Dual -> Dual
deskolemizeAnnotationDual skolems (Dual capability target) =
  Dual
    (deskolemizeAnnotationCapability skolems capability)
    (deskolemizeAnnotationType skolems target)

-- | Give source annotations nested inside an explicitly quantified
-- definition the same rigid interpretation as its declared top-level
-- scheme.  Desugaring preserves `(e : T)` as 'IReshape' metadata; without
-- this traversal, a textual binder such as @a@ inside the body would remain
-- a flexible 'TVar' even though the signature's @a@ had become a 'TSkolem'.
skolemizeAnnotationType
  :: AnnotationSkolems
  -> Type
  -> Type
skolemizeAnnotationType skolems ty =
  let withCapabilities =
        foldr
          (\(fresh, declared) accumulated ->
            substCapVarInType declared (CapSkolem fresh) accumulated)
          ty
          (annotationCapSkolems skolems)
  in foldr
      (\(fresh, declared) accumulated ->
        let name = tyVarName declared
            replacement = TVar fresh
        in substTyVar (TyVar name) replacement accumulated)
      withCapabilities
      (annotationTySkolems skolems)

skolemizeNestedAnnotations
  :: AnnotationSkolems
  -> IExpr
  -> IExpr
skolemizeNestedAnnotations skolems =
  mapIExprTypes (skolemizeAnnotationType skolems)

-- | Orient unconstrained inference aliases back toward the protected fresh
-- variable of an annotation.  This is only alpha-renaming: concrete images
-- are left for 'checkAnnotationIdentity' to reject.
closeAnnotatedTypeVariables
  :: AnnotationSkolems
  -> Subst
  -> TypeErrorContext
  -> Infer Subst
closeAnnotatedTypeVariables skolems initialSubstitution ctx =
  foldM closeOne initialSubstitution (annotationTySkolems skolems)
  where
    closeOne substitution (fresh, _) =
      case applySubst substitution (TVar fresh) of
        TVar image -> do
              let canonical = TyVar (tyVarName fresh)
              if image == canonical
                then return substitution
                else do
                  let renaming = singletonSubst image (TVar canonical)
                  recordGlobalSubst ctx renaming
                  return (composeSubst renaming substitution)
        _ -> return substitution

-- | Validate the protected type variables of an explicit annotation: each
-- annotation skolem must retain its identity in the final substitution.  A
-- concrete image or an unrelated variable is an annotation-rigidity error,
-- unless an explicit Egison extension is in use.
checkAnnotationIdentity
  :: AnnotationSkolems
  -> Subst
  -> Bool
  -> TypeErrorContext
  -> Infer ()
checkAnnotationIdentity skolems substitution allowExtension ctx =
  mapM_ inspect (annotationTySkolems skolems)
  where
    inspect (fresh, _declared) = do
      let image = applySubst substitution (TVar fresh)
      case image of
        TVar variable | tyVarName variable == tyVarName fresh -> return ()
        _ | allowExtension -> return ()
        _ ->
          throwError $
            TE.TypeMismatch
              (TVar fresh) image
              "an annotation type variable may only retain its identity"
              ctx

deskolemizeAnnotationConstraint
  :: AnnotationSkolems
  -> Constraint
  -> Constraint
deskolemizeAnnotationConstraint skolems (Constraint className types) =
  Constraint className (map (deskolemizeAnnotationType skolems) types)

deskolemizeAnnotationScheme
  :: AnnotationSkolems
  -> TypeScheme
  -> TypeScheme
deskolemizeAnnotationScheme skolems (Forall capVars tyVars constraints ty) =
  Forall capVars tyVars
    (map (deskolemizeAnnotationConstraint skolems) constraints)
    (deskolemizeAnnotationType skolems ty)

deskolemizeAnnotationSubst :: AnnotationSkolems -> Subst -> Subst
deskolemizeAnnotationSubst skolems (Subst types capabilities) =
  Subst
    (Map.fromList
      [ (deskolemizeKey variable, deskolemizeAnnotationType skolems ty)
      | (variable, ty) <- Map.toList types
      ])
    (Map.map replaceCapability capabilities)
  where
    deskolemizeKey variable =
      case
        [ declared
        | (fresh, declared) <- annotationTySkolems skolems
        , tyVarName fresh == tyVarName variable
        ] of
        declared : _ -> declared
        [] -> variable

    replaceCapability =
      mapCapability $ \capability ->
        case capability of
          CapSkolem fresh ->
            case lookup fresh (annotationCapSkolems skolems) of
              Just declared -> CapVar declared
              Nothing       -> capability
          _ ->
            capability

deskolemizeAnnotationTIExpr :: AnnotationSkolems -> TIExpr -> TIExpr
deskolemizeAnnotationTIExpr skolems (TIExpr scheme node) =
  TIExpr
    (deskolemizeAnnotationScheme skolems scheme)
    (deskolemizeNode node)
  where
    goExpr = deskolemizeAnnotationTIExpr skolems
    goPattern = deskolemizeAnnotationTIPattern skolems
    goBinding (pattern', expression) = (pattern', goExpr expression)
    goClause (pattern', expression) =
      (goPattern pattern', goExpr expression)

    deskolemizeNode (TIMatchExpr mode target matcher clauses fallback) =
      TIMatchExpr mode (goExpr target) (goExpr matcher) (map goClause clauses)
        (goExpr <$> fallback)
    deskolemizeNode (TIMatchAllExpr mode target matcher clauses) =
      TIMatchAllExpr mode (goExpr target) (goExpr matcher) (map goClause clauses)
    deskolemizeNode (TIMatcherExpr patternDefinitions) =
      TIMatcherExpr
        [ (patternPattern, goExpr expression, map goBinding bindings)
        | (patternPattern, expression, bindings) <- patternDefinitions
        ]
    deskolemizeNode (TIReshape ty inner) =
      TIReshape (deskolemizeAnnotationType skolems ty) (goExpr inner)
    deskolemizeNode
      (TIRuntimeDispatch className methodName candidates arguments) =
        TIRuntimeDispatch className methodName
          [ (deskolemizeAnnotationType skolems ty, dictionary)
          | (ty, dictionary) <- candidates
          ]
          (map goExpr arguments)
    deskolemizeNode other =
      mapTIExprChildren goExpr other

deskolemizeAnnotationTIPattern
  :: AnnotationSkolems
  -> TIPattern
  -> TIPattern
deskolemizeAnnotationTIPattern skolems (TIPattern scheme node) =
  TIPattern
    (deskolemizeAnnotationScheme skolems scheme)
    (deskolemizeNode node)
  where
    goExpr = deskolemizeAnnotationTIExpr skolems
    goPattern = deskolemizeAnnotationTIPattern skolems
    goBinding (pattern', expression) = (pattern', goExpr expression)

    deskolemizeNode TIWildCard = TIWildCard
    deskolemizeNode (TIPatVar name) = TIPatVar name
    deskolemizeNode (TIValuePat expression) = TIValuePat (goExpr expression)
    deskolemizeNode (TIPredPat expression) = TIPredPat (goExpr expression)
    deskolemizeNode (TIIndexedPat pattern' expressions) =
      TIIndexedPat (goPattern pattern') (map goExpr expressions)
    deskolemizeNode (TILetPat bindings pattern') =
      TILetPat (map goBinding bindings) (goPattern pattern')
    deskolemizeNode (TINotPat pattern') = TINotPat (goPattern pattern')
    deskolemizeNode (TIAndPat left right) =
      TIAndPat (goPattern left) (goPattern right)
    deskolemizeNode (TIOrPat left right) =
      TIOrPat (goPattern left) (goPattern right)
    deskolemizeNode (TIForallPat left right) =
      TIForallPat (goPattern left) (goPattern right)
    deskolemizeNode (TITuplePat patterns) =
      TITuplePat (map goPattern patterns)
    deskolemizeNode (TIInductivePat name patterns) =
      TIInductivePat name (map goPattern patterns)
    deskolemizeNode
      (TILoopPat name (TILoopRange start end rangePattern) body endPattern) =
        TILoopPat name
          (TILoopRange (goExpr start) (goExpr end) (goPattern rangePattern))
          (goPattern body)
          (goPattern endPattern)
    deskolemizeNode TIContPat = TIContPat
    deskolemizeNode (TIPApplyPat expression patterns) =
      TIPApplyPat (goExpr expression) (map goPattern patterns)
    deskolemizeNode (TIVarPat name) = TIVarPat name
    deskolemizeNode (TIInductiveOrPApplyPat name patterns) =
      TIInductiveOrPApplyPat name (map goPattern patterns)
    deskolemizeNode TISeqNilPat = TISeqNilPat
    deskolemizeNode (TISeqConsPat left right) =
      TISeqConsPat (goPattern left) (goPattern right)
    deskolemizeNode TILaterPatVar = TILaterPatVar
    deskolemizeNode (TIDApplyPat pattern' patterns) =
      TIDApplyPat (goPattern pattern') (map goPattern patterns)

-- | Enforce the local-meta boundary of explicit scheme checking.
--
-- Fresh inference variables created inside the definition may be solved to
-- an annotation skolem and are deskolemized with the typed tree.  Variables
-- already free in the surrounding environment are owned by that environment
-- and must remain pointwise fixed.  This is stronger than merely checking
-- that no skolem escapes: a ground annotation must not solve an environment
-- metavariable to a ground type or capability either.
checkAnnotationBoundary
  :: Subst
  -> TypeEnv
  -> Subst
  -> Bool
  -> TypeErrorContext
  -> Infer ()
checkAnnotationBoundary baselineSubst environment localSubst allowExtension context = do
  globalSubst <- gets inferGlobalSubst
  let committed = composeSubst globalSubst localSubst
      typeEscapes =
        [ (before, after)
        | variable <- Set.toList (freeVarsInEnv environment)
        , let before = applySubst baselineSubst (TVar variable)
        , let after = applySubst committed (TVar variable)
        , before /= after
        ]
      capabilityEscapes =
        [ (before, after)
        | variable <- Set.toList (freeCapVarsInEnv environment)
        , let before = applyCapSubst baselineSubst (CapVar variable)
        , let after = applyCapSubst committed (CapVar variable)
        , before /= after
        ]
      explicitlyExplainedTypeEscapes =
        all (uncurry explicitAnnotationExtensionChange) typeEscapes
      typeSkolemEscape = any (typeContainsSkolem . snd) typeEscapes
      capabilitySkolemEscape =
        any (capabilityContainsSkolem . snd) capabilityEscapes
  -- Annotation skolems are always rigid.  Ground specialization of an
  -- enclosing production metavariable is retained only as an explicit Egison
  -- reconstruction fallback; TypePM contexts themselves are fixed.
  when (typeSkolemEscape || capabilitySkolemEscape) $
    throwError (TE.AnnotationSkolemEscape context)
  unless (null capabilityEscapes) $
    warnOutsideEgisonCore
      (if allowExtension
        then "an Egison numeric/CAS/tensor annotation changes an enclosing capability metavariable"
        else "production annotation reconstruction specializes an enclosing capability metavariable")
      context
  unless (null typeEscapes) $
    if allowExtension && explicitlyExplainedTypeEscapes
      then
        warnOutsideEgisonCore
          "an Egison numeric/CAS/tensor annotation changes an enclosing ordinary-type metavariable"
          context
      else
        warnOutsideEgisonCore
          "production annotation reconstruction specializes an enclosing ordinary-type metavariable"
          context

typeContainsSkolem :: Type -> Bool
typeContainsSkolem ty =
  case ty of
    TSkolem _ -> True
    TTuple values -> any typeContainsSkolem values
    TCollection value -> typeContainsSkolem value
    TInductive _ values -> any typeContainsSkolem values
    THash key value -> typeContainsSkolem key || typeContainsSkolem value
    TMatcher capability target ->
      capabilityContainsSkolem capability || typeContainsSkolem target
    TFun argument result ->
      typeContainsSkolem argument || typeContainsSkolem result
    TIO value -> typeContainsSkolem value
    TIORef value -> typeContainsSkolem value
    TFactor -> False
    TTerm value _ -> typeContainsSkolem value
    TFrac value -> typeContainsSkolem value
    TPoly value _ -> typeContainsSkolem value
    TTensor value -> typeContainsSkolem value
    _ -> False

capabilityContainsSkolem :: Capability -> Bool
capabilityContainsSkolem capability =
  case capability of
    CapSkolem _ -> True
    CapCon _ children -> any capabilityContainsSkolem children
    CapTuple children -> any capabilityContainsSkolem children
    _ -> False

explicitAnnotationExtensionChange :: Type -> Type -> Bool
explicitAnnotationExtensionChange before after =
  extensionEndpoint before || extensionEndpoint after
  where
    extensionEndpoint ty =
      extensionRepresentationType ty ||
      case ty of
        TInt               -> True
        TTuple values      -> any extensionEndpoint values
        TCollection value  -> extensionEndpoint value
        TInductive name values ->
          egisonExtensionInductive name || any extensionEndpoint values
        THash key value    -> extensionEndpoint key || extensionEndpoint value
        TMatcher _ target  -> extensionEndpoint target
        TFun argument result ->
          extensionEndpoint argument || extensionEndpoint result
        TIO value          -> extensionEndpoint value
        TIORef value       -> extensionEndpoint value
        _                  -> False

typeSchemeUsesEgisonExtension :: TypeScheme -> Bool
typeSchemeUsesEgisonExtension (Forall _ _ constraints ty) =
  any constraintUsesExtension constraints || typeUsesExtension ty
  where
    constraintUsesExtension (Constraint _ types) =
      any typeUsesExtension types
    typeUsesExtension current =
      extensionRepresentationType current
        || case current of
             TTuple values       -> any typeUsesExtension values
             TCollection value   -> typeUsesExtension value
             TInductive name values ->
               egisonExtensionInductive name || any typeUsesExtension values
             THash key value     -> typeUsesExtension key || typeUsesExtension value
             TMatcher _ target   -> typeUsesExtension target
             TFun argument result ->
               typeUsesExtension argument || typeUsesExtension result
             TIO value           -> typeUsesExtension value
             TIORef value        -> typeUsesExtension value
             _                   -> False

-- | A small, explicit bridge inventory for extension operations whose types
-- can be hidden behind an otherwise ordinary matcher or function signature.
-- The AST-level warning inventory remains the authoritative user diagnostic;
-- this predicate only decides whether annotation reconstruction may use the
-- same compatibility path.
expressionUsesEgisonExtension :: IExpr -> Bool
expressionUsesEgisonExtension expression =
  any (`Set.member` extensionNames) (iexprVarRefs expression)
  where
    extensionNames = Set.fromList
      [ "mathValue", "termExpr", "poly", "frac", "indexExpr"
      , "tensorIndex", "matrix", "generateTensor", "tensorShape"
      , "subrefs", "suprefs", "userRefs", "contractWith"
      ]

deskolemizeAnnotationState :: AnnotationSkolems -> Infer ()
deskolemizeAnnotationState skolems =
  modify $ \state ->
    state
      { inferConstraints =
          map
            (deskolemizeAnnotationConstraint skolems)
            (inferConstraints state)
      , inferGlobalSubst =
          deskolemizeAnnotationSubst skolems (inferGlobalSubst state)
      }

-- | Extend the environment temporarily
withEnv :: [(String, TypeScheme)] -> Infer a -> Infer a
withEnv bindings action = do
  oldEnv <- getEnv
  setEnv $ extendEnvMany (map (\(name, scheme) -> (stringToVar name, scheme)) bindings) oldEnv
  restoreInferStateAfter (setEnv oldEnv) action

-- | Run a scoped inference action and restore its surrounding state whether
-- it succeeds or throws.  'Infer' keeps state underneath 'ExceptT', so a
-- caught error otherwise exposes the partially modified state to its handler.
restoreInferStateAfter :: Infer () -> Infer a -> Infer a
restoreInferStateAfter restore action = do
  outcome <-
    (Right <$> action)
      `catchError` (return . Left)
  restore
  either throwError return outcome

-- | Names bound by a primitive data pattern.
primitivePatternNames :: IPrimitiveDataPattern -> [String]
primitivePatternNames =
  foldr (\var names -> extractNameFromVar var : names) []

-- | Lookup variable and return type with constraints
lookupVarWithConstraints :: String -> Infer (Type, [Constraint])
lookupVarWithConstraints name = do
  env <- getEnv
  case lookupEnv (stringToVar name) env of
    Just scheme -> do
      (constraints, t) <- instantiateSchemeInState scheme
      -- Track constraints for type class resolution
      addConstraints constraints
      return (t, constraints)
    Nothing -> do
      -- Check if this is a declared symbol
      st <- get
      case Map.lookup name (declaredSymbols st) of
        Just ty -> return (ty, [])  -- Return the declared type without warning
        Nothing -> do
          permissive <- isPermissive
          if permissive
            then do
              -- In permissive mode, treat as a warning and return a fresh type variable
              warnUnboundVariable name emptyContext
              t <- freshVar "unbound"
              return (t, [])
            else throwError $ UnboundVariable name emptyContext

-- | Unify two types
unifyTypes :: Type -> Type -> Infer Subst
unifyTypes t1 t2 = unifyTypesWithContext t1 t2 emptyContext

-- | Unify two types with context information
-- This now uses the accumulated constraints from the Infer monad to properly
-- handle constraint-aware unification (e.g., ensuring {Num a} a doesn't unify with Tensor b)
-- | Error message for a capability mismatch.
matcherCapabilityMismatchMsg :: String
matcherCapabilityMismatchMsg =
  "matcher capabilities do not unify: the pattern demands one capability "
  ++ "and the matcher provides another."

-- | The expression under any lambda wrappers (the body a parameterized
-- definition is desugared to).  Also sees through the letrec produced by the
-- algebraicDataMatcher desugaring (a self-referencing matcher literal).
rhsCore :: IExpr -> IExpr
rhsCore (ILambdaExpr _ _ e) = rhsCore e
rhsCore (ILetRecExpr [(PDPatVar v, e@(IMatcherExpr _))] (IVarExpr name))
  | v == stringToVar name   = e
rhsCore e                   = e

-- | The core admits recursive values whose outer constructor delays or
-- packages evaluation: a lambda or a matcher literal.  The second shape is
-- the letrec wrapper emitted by algebraic-data-matcher desugaring.
recursiveValueRoot :: IExpr -> Bool
recursiveValueRoot ILambdaExpr{} = True
recursiveValueRoot IMatcherExpr{} = True
recursiveValueRoot
  (ILetRecExpr [(PDPatVar variable, IMatcherExpr _)] (IVarExpr name)) =
    variable == stringToVar name
recursiveValueRoot _ = False

checkRecursiveValueRoot :: String -> IExpr -> TypeErrorContext -> Infer ()
checkRecursiveValueRoot name expression ctx =
  checkRecursiveGroupValueRoot name (Set.singleton name) expression ctx

checkRecursiveGroupValueRoot
  :: String
  -> Set.Set String
  -> IExpr
  -> TypeErrorContext
  -> Infer ()
checkRecursiveGroupValueRoot name recursiveNames expression ctx =
  when
    (not (Set.null
      (recursiveNames `Set.intersection` iexprFreeVarRefs expression)) &&
      not (recursiveValueRoot expression)) $
    throwError $
      UnsupportedFeature
        ("recursive definition '" ++ name ++
          "' must have a lambda or matcher literal at its root")
        ctx

-- | Members of actual cycles in a recursive binding group.  This graph is
-- used only to enforce the recursive-value root restriction; it carries no
-- matcher-producer or capability evidence.
recursiveCycleMembers :: [IBindingExpr] -> Set.Set String
recursiveCycleMembers bindings =
  Set.filter (\name -> name `Set.member` reachableFrom name) allNames
  where
    entries =
      [ (name, rhs)
      | (pattern, rhs) <- bindings
      , name <- primitivePatternNames pattern
      ]
    allNames = Set.fromList (map fst entries)
    adjacency =
      Map.fromListWith Set.union
        [ (name, iexprFreeVarRefs rhs `Set.intersection` allNames)
        | (name, rhs) <- entries
        ]

    reachableFrom name =
      walk Set.empty
        (Set.toList (Map.findWithDefault Set.empty name adjacency))

    walk seen [] = seen
    walk seen (name : rest)
      | name `Set.member` seen = walk seen rest
      | otherwise =
          walk
            (Set.insert name seen)
            (Set.toList
              (Map.findWithDefault Set.empty name adjacency) ++ rest)

unifyTypesWithContext :: Type -> Type -> TypeErrorContext -> Infer Subst
unifyTypesWithContext t1 t2 ctx = do
  constraints <- getConstraints
  classEnv <- getClassEnv
  -- Zonk both sides through the global substitution first: a variable already
  -- committed by a sibling subexpression (whose local substitution this caller
  -- never saw) resolves to its committed type, so a conflicting second
  -- commitment is unified against the first instead of silently shadowing it
  -- in a later left-biased 'composeSubst'.
  (t1', t2') <- zonkPair t1 t2
  fst <$> solveTypes classEnv constraints t1' t2' ctx

-- | Resolve both unification operands through 'inferGlobalSubst' (with the
-- usual constraint-aware Tensor adjustment).  'applySubstWithConstraintsM'
-- routes every application through the global substitution, so the empty
-- local substitution suffices here.
zonkPair :: Type -> Type -> Infer (Type, Type)
zonkPair t1 t2 = do
  t1' <- applySubstWithConstraintsM emptySubst t1
  t2' <- applySubstWithConstraintsM emptySubst t2
  return (t1', t2')

-- | Merge a committed unifier into the global zonk substitution.
recordGlobalSubst :: TypeErrorContext -> Subst -> Infer ()
recordGlobalSubst _ substitution =
  modify $ \state -> state
    { inferGlobalSubst = composeSubst substitution (inferGlobalSubst state) }

-- | Solve ordinary equality with the synchronized TypePM relation.  Egison
-- extensions are selected only by a positive extension witness, such as a
-- numeric/CAS/tensor representation type; failure of the core relation is
-- never by itself permission to retry with a different matcher relation.
solveTypes
  :: ClassEnv
  -> [Constraint]
  -> Type
  -> Type
  -> TypeErrorContext
  -> Infer (Subst, Bool)
solveTypes classEnv constraints left right ctx = do
  case TU.unifyWithConstraints classEnv constraints left right of
    Right result -> commit result
    Left (TU.TypeMismatch mismatchLeft mismatchRight)
      | skolemExtensionMismatch constraints mismatchLeft mismatchRight -> do
          warnOutsideEgisonCore
            ("rigid annotation variable is used through Egison's extended " ++
             "numeric/CAS/tensor typing relation: `" ++
             TP.prettyType mismatchLeft ++ " ~ " ++
             TP.prettyType mismatchRight ++ "`")
            ctx
          commit (emptySubst, False)
    Left err -> throwUnifyError ctx err
  where
    commit result@(substitution, _) = do
      recordGlobalSubst ctx substitution
      return result

-- | Egison's production numeric, CAS, and tensor typing predates rigid
-- reconstruction.  These representation-level equalities are not core
-- equalities, so they are admitted only when an explicit extension type or
-- constraint positively selects the production rule.
skolemExtensionMismatch
  :: [Constraint]
  -> Type
  -> Type
  -> Bool
skolemExtensionMismatch constraints left right =
  skolemAgainst left right || skolemAgainst right left
  where
    skolemAgainst (TSkolem _) extensionType =
      extensionRepresentationType extensionType
        || (not (null constraints) && constrainedNumericType extensionType)
    skolemAgainst _ _ = False

    constrainedNumericType TInt = True
    constrainedNumericType TMathValue = True
    constrainedNumericType TFactor = True
    constrainedNumericType TTerm{} = True
    constrainedNumericType TFrac{} = True
    constrainedNumericType TPoly{} = True
    constrainedNumericType _ = False

extensionRepresentationType :: Type -> Bool
extensionRepresentationType ty =
  case ty of
    TMathValue  -> True
    TPolyExpr   -> True
    TTermExpr   -> True
    TSymbolExpr -> True
    TIndexExpr  -> True
    TTensor _   -> True
    TFactor     -> True
    TTerm _ _   -> True
    TFrac _     -> True
    TPoly _ _   -> True
    TInductive name _ -> egisonExtensionInductive name
    _           -> False

egisonExtensionInductive :: String -> Bool
egisonExtensionInductive name =
  name `elem` ["TensorIndex", "Matrix"]

throwUnifyError :: TypeErrorContext -> TU.UnifyError -> Infer a
throwUnifyError ctx err =
  case err of
    TU.OccursCheck variable ty ->
      throwError (OccursCheckError variable ty ctx)
    TU.TypeMismatch left right ->
      throwError (UnificationError left right ctx)
    TU.CapabilityMismatch left right ->
      throwError (TE.TypeMismatch left right matcherCapabilityMismatchMsg ctx)

-- | Unify two types with context, allowing Tensor a to unify with a
-- This is used only for top-level definitions with type annotations
-- According to type-tensor-simple.md: "Only for top-level tensor definitions, if Tensor a is unified with a, it becomes a."
unifyTypesWithTopLevel :: Type -> Type -> TypeErrorContext -> Infer Subst
unifyTypesWithTopLevel t1 t2 ctx = do
  (t1', t2') <- zonkPair t1 t2
  case TU.unifyWithTopLevel t1' t2' of
    Right s  -> recordGlobalSubst ctx s >> return s
    Left (TU.TypeMismatch mismatchLeft mismatchRight)
      | skolemExtensionMismatch [] mismatchLeft mismatchRight -> do
          warnOutsideEgisonCore
            ("top-level rigid annotation uses Egison's extended " ++
             "numeric/CAS/tensor typing relation: `" ++
             TP.prettyType mismatchLeft ++ " ~ " ++
             TP.prettyType mismatchRight ++ "`")
            ctx
          return emptySubst
    Left err -> throwUnifyError ctx err

-- | Unify two types with constraint-aware handling
-- This is crucial for unifying types when type variables have constraints
-- (e.g., {Num t0}) - the constraint affects how Tensor types are unified
unifyTypesWithConstraints :: [Constraint] -> Type -> Type -> TypeErrorContext -> Infer Subst
unifyTypesWithConstraints constraints t1 t2 ctx = do
  classEnv <- getClassEnv
  (t1', t2') <- zonkPair t1 t2
  fst <$> solveTypes classEnv constraints t1' t2' ctx

-- | Infer type for constants
inferConstant :: ConstantExpr -> Infer Type
inferConstant c = case c of
  CharExpr _    -> return TChar
  StringExpr _  -> return TString
  BoolExpr _    -> return TBool
  IntegerExpr _ -> return TInt
  FloatExpr _   -> return TFloat
  -- something : Matcher Any a
  SomethingExpr -> do
    elemType <- freshVar "a"
    return (TMatcher CapAny elemType)
  -- undefined has a fresh type variable (bottom-like, can be any type)
  UndefinedExpr -> freshVar "undefined"

--------------------------------------------------------------------------------
-- * Type Inference for IExpr
--------------------------------------------------------------------------------

-- | Helper: Create a TIExpr with a simple monomorphic type (no type variables, no constraints)
mkTIExpr :: Type -> TIExprNode -> TIExpr
mkTIExpr ty node = TIExpr (Forall [] [] [] ty) node

-- | Simplify Tensor constraints in type schemes
-- Rewrites C (Tensor a) to C a when C (Tensor a) has no instance but C a does
-- This enables correct type class expansion for higher-order functions with Tensor arguments
simplifyTensorConstraints :: ClassEnv -> [Constraint] -> [Constraint]
simplifyTensorConstraints classEnv = map simplifyConstraint
  where
    hasInstance :: String -> Type -> Bool
    hasInstance cls ty =
      case findMatchingInstanceForType ty (lookupInstances cls classEnv) of
        Just _  -> True
        Nothing -> False
    
    simplifyConstraint :: Constraint -> Constraint
    simplifyConstraint (Constraint cls tys) = Constraint cls (map (unwrapTensorInType cls) tys)
      where
        unwrapTensorInType :: String -> Type -> Type
        unwrapTensorInType cls' ty0 = case ty0 of
          TTensor inner
            | hasInstance cls' ty0   -> ty0           -- Tensor has instance, keep it
            | hasInstance cls' inner -> unwrapTensorInType cls' inner  -- Unwrap recursively
            | otherwise              -> ty0           -- No instance for either, keep original
          _ -> ty0

-- | Apply a substitution to a type scheme with class environment awareness
-- This adjusts the substitution based on type class constraints:
-- When {Num t0} t0 -> t0 is unified with Tensor t1, if Num (Tensor t1) has no instance,
-- the substitution is adjusted to t0 -> t1 (unwrapping the Tensor)
applySubstSchemeWithClassEnv :: ClassEnv -> Subst -> TypeScheme -> TypeScheme
applySubstSchemeWithClassEnv classEnv (Subst m capM) (Forall capVs vs cs t) =
  let m' = foldr Map.delete m vs
      capM' = foldr Map.delete capM capVs
      -- Adjust substitution based on constraints
      m'' = adjustSubstForConstraints classEnv cs m'
      s' = Subst m'' capM'
  in Forall capVs vs (map (applySubstConstraint s') cs) (applySubst s' t)
  where
    -- Adjust substitution to unwrap Tensor when constraint has no instance
    adjustSubstForConstraints :: ClassEnv -> [Constraint] -> Map.Map TyVar Type -> Map.Map TyVar Type
    adjustSubstForConstraints env constraints substMap =
      -- For each constraint, check if we need to adjust substitutions
      foldr (adjustForConstraint env substMap) substMap constraints

    adjustForConstraint :: ClassEnv -> Map.Map TyVar Type -> Constraint -> Map.Map TyVar Type -> Map.Map TyVar Type
    adjustForConstraint env originalSubst (Constraint cls constraintTys) currentSubst =
      -- Get all type variables across all constraint types (multi-param-friendly).
      let constraintVars = Set.toList $ Set.unions (map freeTyVars constraintTys)
      in foldr (adjustVarForClass env cls originalSubst) currentSubst constraintVars

    adjustVarForClass :: ClassEnv -> String -> Map.Map TyVar Type -> TyVar -> Map.Map TyVar Type -> Map.Map TyVar Type
    adjustVarForClass env cls originalSubst var currentSubst =
      case Map.lookup var originalSubst of
        Just replacementType@(TTensor _) ->
          -- This variable is being replaced with a Tensor type
          -- Check if the class has an instance for the Tensor type
          let instances = lookupInstances cls env
              hasTensorInstance = case findMatchingInstanceForType replacementType instances of
                                    Just _  -> True
                                    Nothing -> False
          in if hasTensorInstance
               then currentSubst  -- Keep the Tensor substitution
               else Map.insert var (unwrapTensorCompletely replacementType) currentSubst  -- Unwrap Tensor
        _ -> currentSubst  -- Not a Tensor substitution, keep as is

    -- Recursively unwrap Tensor to get the innermost type
    unwrapTensorCompletely :: Type -> Type
    unwrapTensorCompletely (TTensor inner) = unwrapTensorCompletely inner
    unwrapTensorCompletely ty = ty

-- | Apply a substitution to a TIExpr with ClassEnv awareness
-- This adjusts the substitution based on type class constraints
-- Example: {Num t0} t0 -> t0 with substitution t0 -> Tensor t1
--   If Num (Tensor t1) has no instance, the substitution is adjusted to t0 -> t1
applySubstToTIExprWithClassEnv :: ClassEnv -> Subst -> TIExpr -> TIExpr
applySubstToTIExprWithClassEnv classEnv s (TIExpr scheme node) =
  let updatedScheme = applySubstSchemeWithClassEnv classEnv s scheme
      updatedNode = applySubstToTIExprNodeWithClassEnv classEnv s node
  in TIExpr updatedScheme updatedNode

-- | Monadic version that uses ClassEnv to adjust substitutions based on constraints
-- Use this in type inference when you need to apply substitutions with constraint awareness
applySubstToTIExprM :: Subst -> TIExpr -> Infer TIExpr
applySubstToTIExprM s tiExpr = do
  classEnv <- getClassEnv
  g <- gets inferGlobalSubst
  -- Resolve through the global zonk substitution as well (see
  -- 'applySubstWithConstraintsM'): stored node schemes must not keep stale
  -- type variables that the global substitution has already committed.
  return $ applySubstToTIExprWithClassEnv classEnv (composeSubst g s) tiExpr

-- | Apply a substitution to a Type with constraint awareness
-- This is a monadic version that retrieves ClassEnv and constraints from the Infer monad
-- and adjusts the substitution based on type class constraints before applying it
applySubstWithConstraintsM :: Subst -> Type -> Infer Type
applySubstWithConstraintsM (Subst m capM) t = do
  classEnv <- getClassEnv
  constraints <- gets inferConstraints
  Subst gm globalCapM <- gets inferGlobalSubst
  -- Adjust substitution based on constraints using the same logic as applySubstSchemeWithClassEnv
  let m' = adjustSubstForConstraints classEnv constraints m
      s' = Subst m' capM
      -- Also resolve through the global zonk substitution: with zonking, each
      -- unifier is a delta relative to the global state, so a locally threaded
      -- substitution alone may leave already-committed variables unresolved
      -- (and code that case-analyzes the applied type would misread them).
      gm' = adjustSubstForConstraints classEnv constraints gm
  return $ applySubst (Subst gm' globalCapM) (applySubst s' t)
  where
    -- Adjust substitution to unwrap Tensor when constraint has no instance
    adjustSubstForConstraints :: ClassEnv -> [Constraint] -> Map.Map TyVar Type -> Map.Map TyVar Type
    adjustSubstForConstraints env cs substMap =
      foldr (adjustForConstraint env substMap) substMap cs

    adjustForConstraint :: ClassEnv -> Map.Map TyVar Type -> Constraint -> Map.Map TyVar Type -> Map.Map TyVar Type
    adjustForConstraint env originalSubst (Constraint cls constraintTys) currentSubst =
      let constraintVars = Set.toList $ Set.unions (map freeTyVars constraintTys)
      in foldr (adjustVarForClass env cls originalSubst) currentSubst constraintVars

    adjustVarForClass :: ClassEnv -> String -> Map.Map TyVar Type -> TyVar -> Map.Map TyVar Type -> Map.Map TyVar Type
    adjustVarForClass env cls originalSubst var currentSubst =
      case Map.lookup var originalSubst of
        Just replacementType@(TTensor _) ->
          let instances = lookupInstances cls env
              hasTensorInstance = case findMatchingInstanceForType replacementType instances of
                                    Just _  -> True
                                    Nothing -> False
          in if hasTensorInstance
               then currentSubst
               else Map.insert var (unwrapTensorCompletely replacementType) currentSubst
        _ -> currentSubst

    unwrapTensorCompletely :: Type -> Type
    unwrapTensorCompletely (TTensor inner) = unwrapTensorCompletely inner
    unwrapTensorCompletely ty = ty

-- | Resolve a capability through a local delta and the prevailing global
-- substitution without entering the ordinary target solver.
applyCapabilityM :: Subst -> Capability -> Infer Capability
applyCapabilityM local capability = do
  global <- gets inferGlobalSubst
  return (applyCapSubst global (applyCapSubst local capability))

-- | Resolve both components of a pattern dual under one prevailing paired
-- substitution.
applyDualM :: Subst -> Dual -> Infer Dual
applyDualM local (Dual capability target) =
  Dual <$> applyCapabilityM local capability
       <*> applySubstWithConstraintsM local target

-- | Apply a substitution to a TIExprNode recursively with ClassEnv awareness
applySubstToTIExprNodeWithClassEnv :: ClassEnv -> Subst -> TIExprNode -> TIExprNode
applySubstToTIExprNodeWithClassEnv env s node = case node of
  TIConstantExpr c -> TIConstantExpr c
  TIVarExpr name -> TIVarExpr name

  TILambdaExpr mVar params body ->
    TILambdaExpr mVar params (applySubstToTIExprWithClassEnv env s body)

  TIApplyExpr func args ->
    TIApplyExpr (applySubstToTIExprWithClassEnv env s func) (map (applySubstToTIExprWithClassEnv env s) args)

  TITupleExpr exprs ->
    TITupleExpr (map (applySubstToTIExprWithClassEnv env s) exprs)

  TICollectionExpr exprs ->
    TICollectionExpr (map (applySubstToTIExprWithClassEnv env s) exprs)

  TIConsExpr h t ->
    TIConsExpr (applySubstToTIExprWithClassEnv env s h) (applySubstToTIExprWithClassEnv env s t)

  TIJoinExpr l r ->
    TIJoinExpr (applySubstToTIExprWithClassEnv env s l) (applySubstToTIExprWithClassEnv env s r)

  TIIfExpr cond thenE elseE ->
    TIIfExpr (applySubstToTIExprWithClassEnv env s cond) (applySubstToTIExprWithClassEnv env s thenE) (applySubstToTIExprWithClassEnv env s elseE)

  TILetExpr bindings body ->
    TILetExpr (map (\(pat, expr) -> (pat, applySubstToTIExprWithClassEnv env s expr)) bindings)
              (applySubstToTIExprWithClassEnv env s body)

  TILetRecExpr bindings body ->
    TILetRecExpr (map (\(pat, expr) -> (pat, applySubstToTIExprWithClassEnv env s expr)) bindings)
                 (applySubstToTIExprWithClassEnv env s body)

  TISeqExpr e1 e2 ->
    TISeqExpr (applySubstToTIExprWithClassEnv env s e1) (applySubstToTIExprWithClassEnv env s e2)

  TIInductiveDataExpr name exprs ->
    TIInductiveDataExpr name (map (applySubstToTIExprWithClassEnv env s) exprs)

  TIMatcherExpr patDefs ->
    -- Substitute in the data-clause arm bodies too, not just the next-matcher
    -- expression: arm bodies contain ordinary expressions (e.g. class-method
    -- calls) whose node schemes must see the final substitution, otherwise
    -- TypeClassExpand later sees stale type variables in their constraints
    -- and emits unbound dictionary references (the method name then leaks
    -- into evaluation as a string index).
    TIMatcherExpr (map (\(pat, expr, bindings) ->
      (pat, applySubstToTIExprWithClassEnv env s expr,
       map (\(dp, e) -> (dp, applySubstToTIExprWithClassEnv env s e)) bindings)) patDefs)

  TIMatchExpr mode target matcher clauses fallback ->
    TIMatchExpr mode
                (applySubstToTIExprWithClassEnv env s target)
                (applySubstToTIExprWithClassEnv env s matcher)
                (map (\(pat, body) ->
                  ( applySubstToTIPatternWithClassEnv env s pat
                  , applySubstToTIExprWithClassEnv env s body
                  )) clauses)
                (applySubstToTIExprWithClassEnv env s <$> fallback)

  TIMatchAllExpr mode target matcher clauses ->
    TIMatchAllExpr mode
                   (applySubstToTIExprWithClassEnv env s target)
                   (applySubstToTIExprWithClassEnv env s matcher)
                   (map (\(pat, body) ->
                     ( applySubstToTIPatternWithClassEnv env s pat
                     , applySubstToTIExprWithClassEnv env s body
                     )) clauses)

  TIMemoizedLambdaExpr params body ->
    TIMemoizedLambdaExpr params (applySubstToTIExprWithClassEnv env s body)

  TIDoExpr bindings body ->
    TIDoExpr (map (\(pat, expr) -> (pat, applySubstToTIExprWithClassEnv env s expr)) bindings)
             (applySubstToTIExprWithClassEnv env s body)

  TICambdaExpr var body ->
    TICambdaExpr var (applySubstToTIExprWithClassEnv env s body)

  TIWithSymbolsExpr syms body ->
    TIWithSymbolsExpr syms (applySubstToTIExprWithClassEnv env s body)

  TIQuoteExpr e ->
    TIQuoteExpr (applySubstToTIExprWithClassEnv env s e)

  TIQuoteSymbolExpr e ->
    TIQuoteSymbolExpr (applySubstToTIExprWithClassEnv env s e)

  TIIndexedExpr override base indices ->
    TIIndexedExpr override (applySubstToTIExprWithClassEnv env s base) (fmap (applySubstToTIExprWithClassEnv env s) <$> indices)

  TISubrefsExpr override base ref ->
    TISubrefsExpr override (applySubstToTIExprWithClassEnv env s base) (applySubstToTIExprWithClassEnv env s ref)

  TISuprefsExpr override base ref ->
    TISuprefsExpr override (applySubstToTIExprWithClassEnv env s base) (applySubstToTIExprWithClassEnv env s ref)

  TIUserrefsExpr override base ref ->
    TIUserrefsExpr override (applySubstToTIExprWithClassEnv env s base) (applySubstToTIExprWithClassEnv env s ref)

  TIWedgeApplyExpr func args ->
    TIWedgeApplyExpr (applySubstToTIExprWithClassEnv env s func) (map (applySubstToTIExprWithClassEnv env s) args)

  TIFunctionExpr names ->
    TIFunctionExpr names

  TIVectorExpr exprs ->
    TIVectorExpr (map (applySubstToTIExprWithClassEnv env s) exprs)

  TIHashExpr pairs ->
    TIHashExpr (map (\(k, v) -> (applySubstToTIExprWithClassEnv env s k, applySubstToTIExprWithClassEnv env s v)) pairs)

  TIGenerateTensorExpr func shape ->
    TIGenerateTensorExpr (applySubstToTIExprWithClassEnv env s func) (applySubstToTIExprWithClassEnv env s shape)

  TITensorExpr shape elems ->
    TITensorExpr (applySubstToTIExprWithClassEnv env s shape) (applySubstToTIExprWithClassEnv env s elems)

  TITransposeExpr perm tensor ->
    TITransposeExpr (applySubstToTIExprWithClassEnv env s perm) (applySubstToTIExprWithClassEnv env s tensor)

  TIFlipIndicesExpr tensor ->
    TIFlipIndicesExpr (applySubstToTIExprWithClassEnv env s tensor)

  TITensorMapExpr func tensor ->
    TITensorMapExpr (applySubstToTIExprWithClassEnv env s func) (applySubstToTIExprWithClassEnv env s tensor)

  TITensorMap2Expr func t1 t2 ->
    TITensorMap2Expr (applySubstToTIExprWithClassEnv env s func) (applySubstToTIExprWithClassEnv env s t1) (applySubstToTIExprWithClassEnv env s t2)

  TITensorContractExpr tensor ->
    TITensorContractExpr (applySubstToTIExprWithClassEnv env s tensor)

  TIRuntimeDispatch className methodName candidates args ->
    TIRuntimeDispatch className methodName candidates (map (applySubstToTIExprWithClassEnv env s) args)

  TIReshape ty inner ->
    TIReshape (applySubst s ty) (applySubstToTIExprWithClassEnv env s inner)

-- | Apply a substitution throughout a typed pattern, including the ordinary
-- expressions embedded in value/predicate/application patterns.  Matcher
-- literals can occur in those expressions and their checks may refine types
-- after the pattern node was first constructed.
applySubstToTIPatternWithClassEnv :: ClassEnv -> Subst -> TIPattern -> TIPattern
applySubstToTIPatternWithClassEnv env s (TIPattern scheme node) =
  TIPattern
    (applySubstSchemeWithClassEnv env s scheme)
    (case node of
      TIWildCard -> TIWildCard
      TIPatVar name -> TIPatVar name
      TIValuePat expr ->
        TIValuePat (exprSubst expr)
      TIPredPat expr ->
        TIPredPat (exprSubst expr)
      TIIndexedPat pat indices ->
        TIIndexedPat (patSubst pat) (map exprSubst indices)
      TILetPat bindings pat ->
        TILetPat (map bindingSubst bindings) (patSubst pat)
      TINotPat pat ->
        TINotPat (patSubst pat)
      TIAndPat pat1 pat2 ->
        TIAndPat (patSubst pat1) (patSubst pat2)
      TIOrPat pat1 pat2 ->
        TIOrPat (patSubst pat1) (patSubst pat2)
      TIForallPat pat1 pat2 ->
        TIForallPat (patSubst pat1) (patSubst pat2)
      TITuplePat pats ->
        TITuplePat (map patSubst pats)
      TIInductivePat name pats ->
        TIInductivePat name (map patSubst pats)
      TILoopPat name (TILoopRange start end rangePat) pat1 pat2 ->
        TILoopPat name
          (TILoopRange (exprSubst start) (exprSubst end) (patSubst rangePat))
          (patSubst pat1)
          (patSubst pat2)
      TIContPat -> TIContPat
      TIPApplyPat func pats ->
        TIPApplyPat (exprSubst func) (map patSubst pats)
      TIVarPat name -> TIVarPat name
      TIInductiveOrPApplyPat name pats ->
        TIInductiveOrPApplyPat name (map patSubst pats)
      TISeqNilPat -> TISeqNilPat
      TISeqConsPat pat1 pat2 ->
        TISeqConsPat (patSubst pat1) (patSubst pat2)
      TILaterPatVar -> TILaterPatVar
      TIDApplyPat pat pats ->
        TIDApplyPat (patSubst pat) (map patSubst pats))
  where
    exprSubst = applySubstToTIExprWithClassEnv env s
    patSubst = applySubstToTIPatternWithClassEnv env s
    bindingSubst (pat, expr) = (pat, exprSubst expr)

-- | Monadic typed-pattern substitution, zonked through the global inference
-- substitution in the same way as 'applySubstToTIExprM'.
applySubstToTIPatternM :: Subst -> TIPattern -> Infer TIPattern
applySubstToTIPatternM s tiPattern = do
  classEnv <- getClassEnv
  g <- gets inferGlobalSubst
  return $
    applySubstToTIPatternWithClassEnv classEnv (composeSubst g s) tiPattern

-- | Infer type for IExpr
-- NEW: Returns TIExpr (typed expression) instead of (IExpr, Type, Subst)
-- This builds the recursive TIExpr structure directly during type inference
inferIExpr :: IExpr -> Infer (TIExpr, Subst)
inferIExpr expr = inferIExprWithContext expr emptyContext

-- | Infer type for IExpr with context information
-- NEW: Returns TIExpr (typed expression) with type information embedded
inferIExprWithContext :: IExpr -> TypeErrorContext -> Infer (TIExpr, Subst)
inferIExprWithContext expr ctx = case expr of
  -- Constants
  IConstantExpr c -> do
    ty <- inferConstant c
    let scheme = Forall [] [] [] ty
    return (TIExpr scheme (TIConstantExpr c), emptySubst)
  
  -- Variables
  IVarExpr name -> do
    -- Variables starting with ":::" are treated as Any type without warning
    if ":::" `isPrefixOf` name
      then do
        let scheme = Forall [] [] [] TAny
        return (TIExpr scheme (TIVarExpr name), emptySubst)
      else do
        (ty, constraints) <- lookupVarWithConstraints name
        let scheme = Forall [] [] constraints ty
        return (TIExpr scheme (TIVarExpr name), emptySubst)
  
  -- Tuples
  ITupleExpr elems -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    case elems of
      [] -> do
        -- Empty tuple: unit type ()
        let scheme = Forall [] [] [] (TTuple [])
        return (TIExpr scheme (TITupleExpr []), emptySubst)
      [single] -> do
        -- Single element tuple: same as the element itself (parentheses are just grouping)
        inferIExprWithContext single exprCtx
      _ -> do
        results <- mapM (\e -> inferIExprWithContext e exprCtx) elems
        let elemTIExprs = map fst results
            elemTypes = map (tiExprType . fst) results
            s = foldr composeSubst emptySubst (map snd results)
        appliedElemTypes <- mapM (applySubstWithConstraintsM s) elemTypes
        let resultType = TTuple appliedElemTypes
            scheme = Forall [] [] [] resultType
        return (TIExpr scheme (TITupleExpr elemTIExprs), s)
  
  -- Collections (Lists)
  ICollectionExpr elems -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    elemType <- freshVar "elem"
    (elemTIExprs, s) <- foldM (inferListElem elemType exprCtx) ([], emptySubst) elems
    elemType' <- applySubstWithConstraintsM s elemType
    let resultType = TCollection elemType'
    return (mkTIExpr resultType (TICollectionExpr (reverse elemTIExprs)), s)
    where
      inferListElem eType exprCtx (accExprs, s) e = do
        (tiExpr, s') <- inferIExprWithContext e exprCtx
        let t = tiExprType tiExpr
        eType' <- applySubstWithConstraintsM s eType
        s'' <- unifyTypesWithContext eType' t exprCtx
        return (tiExpr : accExprs, composeSubst s'' (composeSubst s' s))

  -- Cons
  IConsExpr headExpr tailExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (headTI, s1) <- inferIExprWithContext headExpr exprCtx
    (tailTI, s2) <- inferIExprWithContext tailExpr exprCtx
    let headType = tiExprType headTI
        tailType = tiExprType tailTI
        s12 = composeSubst s2 s1
    headType' <- applySubstWithConstraintsM s12 headType
    tailType' <- applySubstWithConstraintsM s12 tailType
    s3 <- unifyTypesWithContext (TCollection headType') tailType' exprCtx
    let finalS = composeSubst s3 s12
    resultType <- applySubstWithConstraintsM finalS tailType
    return (mkTIExpr resultType (TIConsExpr headTI tailTI), finalS)
  
  -- Join (list concatenation)
  IJoinExpr leftExpr rightExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (leftTI, s1) <- inferIExprWithContext leftExpr exprCtx
    (rightTI, s2) <- inferIExprWithContext rightExpr exprCtx
    let leftType = tiExprType leftTI
        rightType = tiExprType rightTI
        s12 = composeSubst s2 s1
    leftType' <- applySubstWithConstraintsM s12 leftType
    rightType' <- applySubstWithConstraintsM s12 rightType
    s3 <- unifyTypesWithContext leftType' rightType' exprCtx
    let finalS = composeSubst s3 s12
    resultType <- applySubstWithConstraintsM finalS leftType
    return (mkTIExpr resultType (TIJoinExpr leftTI rightTI), finalS)
  
  -- Hash (Map)
  IHashExpr pairs -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    keyType <- freshVar "hashKey"
    valType <- freshVar "hashVal"
    (pairTIs, s) <- foldM (inferHashPair keyType valType exprCtx) ([], emptySubst) pairs
    keyType' <- applySubstWithConstraintsM s keyType
    valType' <- applySubstWithConstraintsM s valType
    let resultType = THash keyType' valType'
    return (mkTIExpr resultType (TIHashExpr (reverse pairTIs)), s)
    where
      inferHashPair kType vType exprCtx (accPairs, s') (k, v) = do
        (kTI, s1) <- inferIExprWithContext k exprCtx
        (vTI, s2) <- inferIExprWithContext v exprCtx
        let kt = tiExprType kTI
            vt = tiExprType vTI
        kType' <- applySubstWithConstraintsM (composeSubst s2 s1) kType
        s3 <- unifyTypesWithContext kType' kt exprCtx
        vType' <- applySubstWithConstraintsM (composeSubst s3 (composeSubst s2 s1)) vType
        s4 <- unifyTypesWithContext vType' vt exprCtx
        return ((kTI, vTI) : accPairs, foldr composeSubst s' [s4, s3, s2, s1])
  
  -- Vector (Tensor)
  IVectorExpr elems -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    elemType <- freshVar "vecElem"
    (elemTIs, s) <- foldM (inferListElem elemType exprCtx) ([], emptySubst) elems
    elemType' <- applySubstWithConstraintsM s elemType
    let resultType = normalizeTensorType (TTensor elemType')
    return (mkTIExpr resultType (TIVectorExpr (reverse elemTIs)), s)
    where
      inferListElem eType exprCtx (accExprs, s) e = do
        (tiExpr, s') <- inferIExprWithContext e exprCtx
        let t = tiExprType tiExpr
        eType' <- applySubstWithConstraintsM s eType
        s'' <- unifyTypesWithContext eType' t exprCtx
        return (tiExpr : accExprs, composeSubst s'' (composeSubst s' s))

  -- Lambda
  ILambdaExpr mVar params body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    argTypes <- mapM (\_ -> freshVar "arg") params
    let bindings = zipWith makeBinding params argTypes
    -- Index-pattern bindings (tensor-paper5 "Pattern Matching for Tensor
    -- Indices"): a parameter written `T~(a_1)...~(a_r)_(b_1)..._(b_k)` binds,
    -- at application time (pmIndices), the hash `a`/`b` (position -> index
    -- symbol) and the counts `r`/`k`; a name index such as the `c` of
    -- `def ∇_c T... := ...` binds an index symbol.  Bring these variables
    -- into scope for the body so they do not surface as unbound-variable
    -- warnings (or hard errors in strict mode).
    let indexBindings = concatMap paramIndexBindings params
                     ++ maybe [] fnNameIndexBindings mVar
    (bodyTIExpr, s) <-
      withEnv (map toScheme (bindings ++ indexBindings)) $
        inferIExprWithContext body exprCtx
    let bodyType = tiExprType bodyTIExpr
    -- A parameter that carries index patterns is necessarily a tensor
    -- (makeBindings forces the argument to TensorData at runtime).
    s' <- foldM (\sAcc (param, argTy) ->
                   if hasIndexPattern param
                     then do
                       argTy' <- applySubstWithConstraintsM sAcc argTy
                       elemTy <- freshVar "idxParamElem"
                       sT <- unifyTypesWithContext argTy' (TTensor elemTy) exprCtx
                       return (composeSubst sT sAcc)
                     else return sAcc)
                s (zip params argTypes)
    resultBodyType <- applySubstWithConstraintsM emptySubst bodyType
    let finalSubst = s'
    finalArgTypes <- mapM (applySubstWithConstraintsM finalSubst) argTypes
    finalBodyType <- applySubstWithConstraintsM finalSubst resultBodyType
    let funType = foldr TFun finalBodyType finalArgTypes
    return (mkTIExpr funType (TILambdaExpr mVar params bodyTIExpr), finalSubst)
    where
      makeBinding var t = (extractNameFromVar var, t)
      toScheme (name, t) = (name, Forall [] [] [] t)
      hasIndexPattern (Var _ is) = not (null is)
      fnNameIndexBindings (Var _ is) = concatMap namedIndexBinding is
      paramIndexBindings (Var _ is) = concatMap indexPatternBinding is
      namedIndexBinding (Sub (Just (Var n []))) = [(n, TMathValue)]
      namedIndexBinding (Sup (Just (Var n []))) = [(n, TMathValue)]
      namedIndexBinding (SupSub (Just (Var n []))) = [(n, TMathValue)]
      namedIndexBinding _ = []
      indexPatternBinding (Sub (Just (Var n []))) = [(n, TMathValue)]
      indexPatternBinding (Sup (Just (Var n []))) = [(n, TMathValue)]
      indexPatternBinding (MultiSub (Just (Var a [])) _ (Just (Var e []))) =
        [(a, THash TInt TMathValue), (e, TInt)]
      indexPatternBinding (MultiSup (Just (Var a [])) _ (Just (Var e []))) =
        [(a, THash TInt TMathValue), (e, TInt)]
      indexPatternBinding _ = []
  
  -- Function Application
  IApplyExpr func args -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (funcTI, s1) <- inferIExprWithContext func exprCtx
    let funcType = tiExprType funcTI
    inferIApplicationWithContext funcTI funcType args s1 exprCtx

  -- Wedge apply expression (exterior product)
  IWedgeApplyExpr func args -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (funcTI, s1) <- inferIExprWithContext func exprCtx
    let funcType = tiExprType funcTI
    -- Wedge application is similar to normal application
    (resultTI, finalS) <- inferIApplicationWithContext funcTI funcType args s1 exprCtx
    -- Convert TIApplyExpr to TIWedgeApplyExpr to preserve wedge semantics
    let resultScheme = tiScheme resultTI
    case tiExprNode resultTI of
      TIApplyExpr funcTI' argTIs' ->
        return (TIExpr resultScheme (TIWedgeApplyExpr funcTI' argTIs'), finalS)
      _ -> return (resultTI, finalS)

  -- If expression
  IIfExpr cond thenExpr elseExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (condTI, s1) <- inferIExprWithContext cond exprCtx
    let condType = tiExprType condTI
    s2 <- unifyTypesWithContext condType TBool exprCtx
    let s12 = composeSubst s2 s1
    commonType <- freshVar "ifResult"
    (thenTI, s3) <- inferIExprWithContext thenExpr exprCtx
    thenType <- applySubstWithConstraintsM s3 (tiExprType thenTI)
    commonThen <- applySubstWithConstraintsM s3 commonType
    sThen <- unifyTypesWithContext thenType commonThen exprCtx
    (elseTI, sElse) <- inferIExprWithContext elseExpr exprCtx
    elseType <- applySubstWithConstraintsM sElse (tiExprType elseTI)
    commonElse <- applySubstWithConstraintsM sElse commonType
    s5 <- unifyTypesWithContext elseType commonElse exprCtx
    let finalS = foldr composeSubst emptySubst [s5, sElse, sThen, s3, s12]
    resultType <- applySubstWithConstraintsM finalS commonType
    return (mkTIExpr resultType (TIIfExpr condTI thenTI elseTI), finalS)
  
  -- Let expression
  ILetExpr bindings body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    env <- getEnv
    (bindingTIs, extendedEnv, s1) <- inferIBindingsWithContext bindings env emptySubst exprCtx
    (bodyTI, s2) <-
      withEnv extendedEnv $
        inferIExprWithContext body exprCtx
    let bodyType = tiExprType bodyTI
        finalS = composeSubst s2 s1
    resultType <- applySubstWithConstraintsM finalS bodyType
    return (mkTIExpr resultType (TILetExpr bindingTIs bodyTI), finalS)
  
  -- LetRec expression
  ILetRecExpr bindings body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    env <- getEnv
    (bindingTIs, extendedEnv, s1) <- inferIRecBindingsWithContext bindings env emptySubst exprCtx
    (bodyTI, s2) <-
      withEnv extendedEnv $
        inferIExprWithContext body exprCtx
    let bodyType = tiExprType bodyTI
        finalS = composeSubst s2 s1
    resultType <- applySubstWithConstraintsM finalS bodyType
    return (mkTIExpr resultType (TILetRecExpr bindingTIs bodyTI), finalS)
  
  -- Sequence expression
  ISeqExpr expr1 expr2 -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (expr1TI, s1) <- inferIExprWithContext expr1 exprCtx
    (expr2TI, s2) <- inferIExprWithContext expr2 exprCtx
    let t2 = tiExprType expr2TI
    return (mkTIExpr t2 (TISeqExpr expr1TI expr2TI), composeSubst s2 s1)
  
  -- Inductive Data Constructor
  IInductiveDataExpr name args -> do
    -- Look up constructor type in environment
    env <- getEnv
    case lookupEnv (stringToVar name) env of
      Just scheme -> do
        (constraints, constructorType) <-
          instantiateSchemeInState scheme
        addConstraints constraints
        -- Treat constructor as a function application
        inferIApplication name constructorType args emptySubst
      Nothing -> do
        -- Constructor not found in environment
        let exprCtx = withExpr (prettyStr expr) ctx
        permissive <- isPermissive
        if permissive
          then do
            -- In permissive mode, treat as a warning and return a fresh type variable
            addWarning $ UnboundVariableWarning name exprCtx
            resultType <- freshVar "ctor"
            return (mkTIExpr resultType (TIInductiveDataExpr name []), emptySubst)
          else throwError $ UnboundVariable name exprCtx
  
  -- Matchers (return Matcher type)
  IMatcherExpr patDefs -> do
    modify $ \state -> state
      { inferMatcherLiteralCount = inferMatcherLiteralCount state + 1
      , inferMatcherClauseCount =
          inferMatcherClauseCount state + length patDefs
      }
    let exprCtx = withExpr (prettyStr expr) ctx
        catchAlls =
          [ index
          | (index, (PPPatVar, _, _)) <- zip [0 :: Int ..] patDefs
          ]
    -- CatchAllLast constrains only clause headers: there is exactly one bare
    -- hole and it is final.  Its data arms are checked independently by
    -- ArmCoverage below.
    case catchAlls of
      [index]
        | index == length patDefs - 1 ->
            return ()
      [] ->
        throwError $ TE.TypeMismatch
          (TMatcher CapAny (TVar (TyVar "a")))
          (TMatcher CapAny (TVar (TyVar "a")))
          "a `matcher` must end with exactly one catch-all clause `$ as <matcher> with $tgt -> ...`"
          exprCtx
      _ ->
        throwError $ TE.TypeMismatch
          (TMatcher CapAny (TVar (TyVar "a")))
          (TMatcher CapAny (TVar (TyVar "a")))
          "the unique bare-hole catch-all must be the final matcher clause"
          exprCtx
    mapM_ (warnMatcherCompatibility exprCtx) patDefs
    -- G-Literal: one shared target type and one shared capability for the
    -- whole literal.  Every constructor or tuple clause header has exactly
    -- this capability and target; the catch-all's hole is unconstrained.
    sharedMatcherTarget <- freshVar "matcherTarget"
    sharedMatcherCapability <- freshCapability "matcherCap"
    results <-
      mapM
        (inferPatternDef exprCtx sharedMatcherTarget sharedMatcherCapability)
        patDefs
    let tiPatDefs = map fst results
        allSubst = foldr composeSubst emptySubst (concatMap snd results)
    covOn <- cfgMatcherConsistencyWarnings <$> gets inferConfig
    matchedTyFinal <- applySubstWithConstraintsM allSubst sharedMatcherTarget
    matcherCapability <- applyCapabilityM allSubst sharedMatcherCapability
    when covOn $ do
      patEnv <- getPatternEnv
      let declarations =
            [ (name, former, arity)
            | (name, scheme) <- patternEnvToList patEnv
            , Just (former, arity) <- [patternConstructorResult scheme]
            ]
          mentionedFormers = nub
            [ former
            | (PPInductivePat name _, _, _) <- patDefs
            , (declaredName, former, _) <- declarations
            , name == declaredName
            ]
          missing =
            [ name
            | (name, former, arity) <- declarations
            , former `elem` mentionedFormers
            , not (any (isGeneralConstructorClause name arity . firstOf3) patDefs)
            ]
      unless (null missing) $
        addWarning $
          MatcherCoverageWarning matchedTyFinal missing exprCtx
    -- TypePM ArmCoverage is a hard check: the final arm is irrefutable, or
    -- every arm is constructor-rooted and general arms cover all constructors
    -- of every mentioned data former.  A miss is a runtime primitive-pattern
    -- failure rather than graceful backtracking.  RootCoverage alone remains
    -- the opt-in diagnostic above.
    dataDeclarations <- dataConstructorShapes
    mapM_ (\(pp, _, dataClauses) ->
             when (not (pdArmsExhaustive dataDeclarations (map fst dataClauses))) $
               throwError $ MatcherDataArmsNotExhaustive (prettyStr pp) matchedTyFinal exprCtx)
          patDefs
    return
      ( mkTIExpr
          (TMatcher matcherCapability matchedTyFinal)
          (TIMatcherExpr tiPatDefs)
      , allSubst
      )
    where
      -- Infer one matcher clause (Q-Nil/Q-Cons/Q-Join generalized to every
      -- declared pattern constructor): the header fixes the literal's target
      -- and, for constructor and tuple headers, its capability; the next
      -- matcher expression has exactly the matcher type demanded by the holes;
      -- the data arms return the decompositions of the holes' targets.
      inferPatternDef
        :: TypeErrorContext
        -> Type
        -> Capability
        -> IPatternDef
        -> Infer (TIPatternDef, [Subst])
      inferPatternDef ctx sharedTarget sharedCapability
                      (ppPat, nextMatcherExpr, dataClauses) = do
        (matchedType, holes, headerCapability, ppBindings, sHeader0) <-
          inferPrimitivePatPattern ppPat ctx
        matchedTypeBeforeShared <-
          applySubstWithConstraintsM sHeader0 matchedType
        sharedTargetBeforeClause <-
          applySubstWithConstraintsM sHeader0 sharedTarget
        sShared <-
          unifyTypesWithContext
            matchedTypeBeforeShared sharedTargetBeforeClause ctx
        let sHeader1 = composeSubst sShared sHeader0
        sHeader <- case headerCapability of
          Just capability -> do
            capability' <- applyCapabilityM sHeader1 capability
            shared' <- applyCapabilityM sHeader1 sharedCapability
            sCap <- alignPatternCapabilities ctx capability' shared'
            return (composeSubst sCap sHeader1)
          Nothing -> return sHeader1
        -- The next matcher is an ordinary expression whose type is the
        -- matcher type demanded by the holes: one matcher for one hole and an
        -- ordinary tuple of matchers for several holes.  Canonical
        -- normalization lets a tuple-typed matcher expression fill several
        -- holes at once.
        (nextMatcherTI, sNext) <- inferIExprWithContext nextMatcherExpr ctx
        when (length holes >= 2 && not (isSyntacticTuple nextMatcherExpr)) $
          modify $ \state -> state
            { inferProductNextMatcherCount =
                inferProductNextMatcherCount state + 1 }
        let sBase = composeSubst sNext sHeader
        holeCapabilities <- mapM (applyCapabilityM sBase . fst) holes
        holeTargets <- mapM (applySubstWithConstraintsM sBase . snd) holes
        let demanded =
              case zipWith TMatcher holeCapabilities holeTargets of
                [single] -> single
                components -> TTuple components
        nextType <- applySubstWithConstraintsM sBase (tiExprType nextMatcherTI)
        sDemand <-
          unifyTypesWithContext demanded nextType ctx
            `catchError` \err ->
              case err of
                TE.TypeMismatch expected actual reason errCtx ->
                  throwError $
                    TE.TypeMismatch expected actual
                      (reason
                       ++ "\n  The next matcher of clause `" ++ prettyStr ppPat
                       ++ "` must have the matcher type demanded by its holes")
                      errCtx
                _ -> throwError err
        let sClause = composeSubst sDemand sBase
        matchedType' <- applySubstWithConstraintsM sClause matchedType
        nextMatcherInnerTypes <-
          mapM (applySubstWithConstraintsM sClause) holeTargets
        let ppBindings' =
              [ (var, applySubstScheme sClause scheme)
              | (var, scheme) <- ppBindings
              ]
        dataClauseResults <-
          withEnv ppBindings' $
            mapM
              (inferDataClauseWithCheck ctx nextMatcherInnerTypes matchedType')
              dataClauses
        let dataClauseTIs = map fst dataClauseResults
            sArms = foldr composeSubst emptySubst (map snd dataClauseResults)
        return ((ppPat, nextMatcherTI, dataClauseTIs), [sClause, sArms])

      -- Infer a primitive-pattern header.  Returns the matched (target)
      -- type, the holes as (capability, target) demands in source order, the
      -- header capability when the root is a constructor or tuple pattern,
      -- the captured bindings of value patterns (#$val), and the
      -- substitution.
      inferPrimitivePatPattern
        :: PrimitivePatPattern
        -> TypeErrorContext
        -> Infer ( Type, [(Capability, Type)], Maybe Capability
                 , [(String, TypeScheme)], Subst )
      inferPrimitivePatPattern ppPat ctx = do
        (matchedTy, holes, capability, bindings, s) <- inferHeader ppPat ctx
        let headerCapability =
              case ppPat of
                PPInductivePat _ _ -> Just capability
                PPTuplePat _       -> Just capability
                _                  -> Nothing
        return (matchedTy, holes, headerCapability, bindings, s)

      -- Every header has a capability: a hole's is its own fresh variable,
      -- bound by the enclosing constructor's field template; a constructor's
      -- is the projection of its declared signature onto fresh capability
      -- variables; a tuple's is the tuple of its components.
      inferHeader
        :: PrimitivePatPattern
        -> TypeErrorContext
        -> Infer ( Type, [(Capability, Type)], Capability
                 , [(String, TypeScheme)], Subst )
      inferHeader ppPat ctx = case ppPat of
        PPWildCard -> do
          matchedTy <- freshVar "matched"
          capability <- freshCapability "headerCap"
          return (matchedTy, [], capability, [], emptySubst)

        PPPatVar -> do
          matchedTy <- freshVar "matched"
          capability <- freshCapability "holeCap"
          return (matchedTy, [(capability, matchedTy)], capability, [], emptySubst)

        PPValuePat var -> do
          matchedTy <- freshVar "matched"
          capability <- freshCapability "headerCap"
          let binding = (var, Forall [] [] [] matchedTy)
          return (matchedTy, [], capability, [binding], emptySubst)

        PPTuplePat ppPats -> do
          results <- mapM (\pp -> inferHeader pp ctx) ppPats
          let s = foldr composeSubst emptySubst
                    [ sub | (_, _, _, _, sub) <- results ]
          matchedTypes <-
            mapM (applySubstWithConstraintsM s)
              [ mt | (mt, _, _, _, _) <- results ]
          holes <- mapM (substHole s) (concat [ hs | (_, hs, _, _, _) <- results ])
          capabilities <-
            mapM (applyCapabilityM s) [ c | (_, _, c, _, _) <- results ]
          let bindings = concat [ bs | (_, _, _, bs, _) <- results ]
          return (TTuple matchedTypes, holes, CapTuple capabilities, bindings, s)

        PPInductivePat name ppPats -> do
          patternEnv <- getPatternEnv
          (argTypes, resultType, fieldCapabilities, resultCapability) <-
            case lookupPatternEnv name patternEnv of
              Just scheme -> do
                (_constraints, ctorType) <- instantiateSchemeInState scheme
                let (argTypes0, resultType) = extractFunctionArgs ctorType
                when (length argTypes0 /= length ppPats) $
                  throwError $ TE.TypeMismatch
                    (foldr TFun resultType
                      (replicate (length ppPats) (TVar (TyVar "a"))))
                    ctorType
                    ("Pattern constructor " ++ name ++ " expects "
                     ++ show (length argTypes0)
                     ++ " arguments, but got " ++ show (length ppPats))
                    ctx
                let argTypes =
                      map (\ty -> case ty of
                                    TMatcher _ inner -> inner
                                    _ -> ty)
                          argTypes0
                    viewFormer =
                      case typeFormerOf resultType of
                        Just (former, _) -> legacyCasLeafFormer former
                        Nothing -> False
                if viewFormer
                  then do
                    -- Legacy CAS pattern view (outside the core rules): the
                    -- declaration names the runtime view only.  Its declared
                    -- field and result types are not target evidence, so the
                    -- holes and the matched type are fresh and are fixed by
                    -- the next matchers and the data arms.  The header keeps
                    -- the view's capability.
                    warnOutsideEgisonCore
                      ("pattern constructor `" ++ name ++
                       "` belongs to a legacy CAS pattern view; its declared field types are not used as target evidence")
                      ctx
                    viewTargets <- mapM (const (freshVar "viewField")) ppPats
                    viewCapabilities <-
                      mapM (const (freshCapability "viewFieldCap")) ppPats
                    viewMatched <- freshVar "viewTarget"
                    templates <- capabilityTemplates ctx [resultType]
                    resultCapability <- case templates of
                      [capability] -> return capability
                      _ -> throwError $ MatcherCapabilityError
                             "internal pattern capability projection lost its result"
                             ctx
                    return (viewTargets, viewMatched, viewCapabilities, resultCapability)
                  else do
                    templates <- capabilityTemplates ctx (argTypes ++ [resultType])
                    let (fieldCapabilities, resultTemplates) =
                          splitAt (length argTypes) templates
                    resultCapability <- case resultTemplates of
                      [capability] -> return capability
                      _ -> throwError $ MatcherCapabilityError
                             "internal pattern capability projection lost its result"
                             ctx
                    return (argTypes, resultType, fieldCapabilities, resultCapability)
              Nothing -> do
                warnOutsideEgisonCore
                  ("primitive-pattern constructor `" ++ name ++
                   "` has no declared pattern signature; using generic inference")
                  ctx
                argTypes <- mapM (const (freshVar "field")) ppPats
                fieldCapabilities <- mapM (const (freshCapability "fieldCap")) ppPats
                return ( argTypes
                       , TInductive name argTypes
                       , fieldCapabilities
                       , CapCon (mkTypeFormer name (length ppPats)) fieldCapabilities )
          results <- mapM (\pp -> inferHeader pp ctx) ppPats
          let s0 = foldr composeSubst emptySubst
                     [ sub | (_, _, _, _, sub) <- results ]
          -- Each sub-header has the field's target type and capability.
          s1 <- foldM
            (\acc ((subMatched, _, subCapability, _, _), (fieldType, fieldCapability)) -> do
              subMatched' <- applySubstWithConstraintsM acc subMatched
              fieldType' <- applySubstWithConstraintsM acc fieldType
              sType <- unifyTypesWithContext subMatched' fieldType' ctx
              let acc' = composeSubst sType acc
              subCapability' <- applyCapabilityM acc' subCapability
              fieldCapability' <- applyCapabilityM acc' fieldCapability
              sCap <- alignPatternCapabilities ctx subCapability' fieldCapability'
              return (composeSubst sCap acc'))
            s0
            (zip results (zip argTypes fieldCapabilities))
          resultType' <- applySubstWithConstraintsM s1 resultType
          resultCapability' <- applyCapabilityM s1 resultCapability
          holes <- mapM (substHole s1) (concat [ hs | (_, hs, _, _, _) <- results ])
          let bindings = concat [ bs | (_, _, _, bs, _) <- results ]
          return (resultType', holes, resultCapability', bindings, s1)
        where
          substHole s (capability, target) = do
            capability' <- applyCapabilityM s capability
            target' <- applySubstWithConstraintsM s target
            return (capability', target')

      isSyntacticTuple :: IExpr -> Bool
      isSyntacticTuple (ITupleExpr _) = True
      isSyntacticTuple _              = False

      -- Extract function argument types and result type
      -- e.g., a -> b -> c -> d  =>  ([a, b, c], d)
      extractFunctionArgs :: Type -> ([Type], Type)
      extractFunctionArgs (TFun arg rest) = 
        let (args, result) = extractFunctionArgs rest
        in (arg : args, result)
      extractFunctionArgs t = ([], t)
      
      -- Infer a data clause with type checking
      -- Check that the target expression returns a list of values with types matching next matcher inner types
      -- Also uses matched type for validation
      -- nextMatcherInnerTypes: inner types extracted from next matcher (already without TMatcher wrapper)
      inferDataClauseWithCheck :: TypeErrorContext -> [Type] -> Type -> (IPrimitiveDataPattern, IExpr) -> Infer ((IPrimitiveDataPattern, TIExpr), Subst)
      inferDataClauseWithCheck ctx nextMatcherInnerTypes matchedType (pdPat, targetExpr) = do
        -- Extract expected element type from next matcher inner types (the target type)
        -- This is the type of elements in the list returned by the target expression
        targetType <- case nextMatcherInnerTypes of
          [] -> return (TTuple [])  -- No pattern holes: empty tuple () case
          [single] -> return single  -- Single pattern hole: use inner type directly
          multiple -> return (TTuple multiple)  -- Multiple holes: tuple of inner types
        
        -- Infer PrimitiveDataPattern with matched type
        -- Primitive data pattern matches against values of the matched type
        -- and produces bindings and next targets
        (pdTargetType, bindings, s_pd) <- inferPrimitiveDataPattern pdPat matchedType ctx
        
        -- The primitive data pattern should match the matched type
        -- No need to unify pdTargetType with targetType - they serve different purposes
        -- pdTargetType: type of data that pdPat matches (should be matchedType)
        -- targetType: type of next targets returned by the target expression
        
        -- Verify that pdTargetType is consistent with matchedType
        pdTargetType' <- applySubstWithConstraintsM s_pd pdTargetType
        matchedType' <- applySubstWithConstraintsM s_pd matchedType
        s_match <- unifyTypesWithContext pdTargetType' matchedType' ctx
        let s_pd' = composeSubst s_match s_pd

        -- Infer the target expression with pattern variables in scope
        (targetTI, s1) <-
          withEnv bindings $
            inferIExprWithContext targetExpr ctx
        let exprType = tiExprType targetTI
            s_combined = composeSubst s1 s_pd'

        -- Unify with actual expression type
        -- Expected: [targetType]
        targetType' <- applySubstWithConstraintsM s_combined targetType
        let expectedType = TCollection targetType'

        exprType' <- applySubstWithConstraintsM s_combined exprType
        s2 <- unifyTypesWithContext exprType' expectedType ctx
        return ((pdPat, targetTI), composeSubst s2 s_combined)

      -- Helper to check if a pattern is a pattern variable
      isPDPatVar :: IPrimitiveDataPattern -> Bool
      isPDPatVar (PDPatVar _) = True
      isPDPatVar _ = False
      
      -- Infer PrimitiveDataPattern type
      -- Returns (inferred target type, variable bindings, substitution)
      -- This is similar to pattern matching in Haskell for algebraic data types
      inferPrimitiveDataPattern :: IPrimitiveDataPattern -> Type -> TypeErrorContext -> Infer (Type, [(String, TypeScheme)], Subst)
      inferPrimitiveDataPattern pdPat expectedType ctx = case pdPat of
        PDWildCard -> do
          -- Wildcard: matches any type, no bindings
          return (expectedType, [], emptySubst)
        
        PDPatVar var -> do
          -- Pattern variable: binds to the expected type
          let varName = extractNameFromVar var
          return (expectedType, [(varName, Forall [] [] [] expectedType)], emptySubst)
        
        PDConstantPat c -> do
          -- Constant pattern: must match the constant's type
          constTy <- inferConstant c
          s <- unifyTypesWithContext constTy expectedType ctx
          expectedType' <- applySubstWithConstraintsM s expectedType
          return (expectedType', [], s)
        
        PDTuplePat pats -> do
          -- Tuple pattern: expected type should be a tuple
          case expectedType of
            TTuple types | length types == length pats -> do
              -- Types match: infer each sub-pattern
              results <- zipWithM (\p t -> inferPrimitiveDataPattern p t ctx) pats types
              let (_, bindingsList, substs) = unzip3 results
                  allBindings = concat bindingsList
                  s = foldr composeSubst emptySubst substs
              expectedType' <- applySubstWithConstraintsM s expectedType
              return (expectedType', allBindings, s)
            
            TVar _ -> do
              -- Expected type is a type variable: create fresh types for each element
              elemTypes <- mapM (\_ -> freshVar "elem") pats
              let tupleTy = TTuple elemTypes
              s <- unifyTypesWithContext expectedType tupleTy ctx

              -- Recursively infer each sub-pattern
              elemTypes' <- mapM (applySubstWithConstraintsM s) elemTypes
              results <- zipWithM (\p t -> inferPrimitiveDataPattern p t ctx) pats elemTypes'
              let (_, bindingsList, substs) = unzip3 results
                  allBindings = concat bindingsList
                  s' = foldr composeSubst s substs
              tupleTy' <- applySubstWithConstraintsM s' tupleTy
              return (tupleTy', allBindings, s')
            
            _ -> do
              -- Type mismatch
              throwError $ TE.TypeMismatch
                (TTuple (replicate (length pats) (TVar (TyVar "a"))))
                expectedType
                "Tuple pattern but target is not a tuple type"
                ctx
        
        PDEmptyPat -> do
          -- Empty collection pattern: expected type should be [a] for some a
          elemTy <- freshVar "elem"
          s <- unifyTypesWithContext expectedType (TCollection elemTy) ctx
          collTy <- applySubstWithConstraintsM s (TCollection elemTy)
          return (collTy, [], s)
        
        PDConsPat p1 p2 -> do
          -- Cons pattern: expected type should be [a] for some a
          case expectedType of
            TCollection elemType -> do
              -- Infer head pattern with element type
              (_, bindings1, s1) <- inferPrimitiveDataPattern p1 elemType ctx
              -- Infer tail pattern with collection type
              expectedType' <- applySubstWithConstraintsM s1 expectedType
              (_, bindings2, s2) <- inferPrimitiveDataPattern p2 expectedType' ctx
              let s = composeSubst s2 s1
              expectedType'' <- applySubstWithConstraintsM s expectedType
              return (expectedType'', bindings1 ++ bindings2, s)
            
            TVar _ -> do
              -- Expected type is a type variable: constrain it to be a collection
              elemTy <- freshVar "elem"
              s <- unifyTypesWithContext expectedType (TCollection elemTy) ctx
              collTy <- applySubstWithConstraintsM s (TCollection elemTy)
              elemTy' <- applySubstWithConstraintsM s elemTy
              (_, bindings1, s1) <- inferPrimitiveDataPattern p1 elemTy' ctx
              collTy' <- applySubstWithConstraintsM s1 collTy
              (_, bindings2, s2) <- inferPrimitiveDataPattern p2 collTy' ctx
              let s' = composeSubst s2 (composeSubst s1 s)
              collTy'' <- applySubstWithConstraintsM s' collTy
              return (collTy'', bindings1 ++ bindings2, s')
            
            _ -> do
              throwError $ TE.TypeMismatch
                (TCollection (TVar (TyVar "a")))
                expectedType
                "Cons pattern but target is not a collection type"
                ctx
        
        PDSnocPat p1 p2 -> do
          -- Snoc pattern: similar to cons but reversed
          case expectedType of
            TCollection elemType -> do
              (_, bindings1, s1) <- inferPrimitiveDataPattern p1 expectedType ctx
              elemType' <- applySubstWithConstraintsM s1 elemType
              (_, bindings2, s2) <- inferPrimitiveDataPattern p2 elemType' ctx
              let s = composeSubst s2 s1
              expectedType' <- applySubstWithConstraintsM s expectedType
              return (expectedType', bindings1 ++ bindings2, s)
            
            TVar _ -> do
              elemTy <- freshVar "elem"
              s <- unifyTypesWithContext expectedType (TCollection elemTy) ctx
              collTy <- applySubstWithConstraintsM s (TCollection elemTy)
              elemTy' <- applySubstWithConstraintsM s elemTy
              (_, bindings1, s1) <- inferPrimitiveDataPattern p1 collTy ctx
              elemTy'' <- applySubstWithConstraintsM s1 elemTy'
              (_, bindings2, s2) <- inferPrimitiveDataPattern p2 elemTy'' ctx
              let s' = composeSubst s2 (composeSubst s1 s)
              collTy' <- applySubstWithConstraintsM s' collTy
              return (collTy', bindings1 ++ bindings2, s')
            
            _ -> do
              throwError $ TE.TypeMismatch
                (TCollection (TVar (TyVar "a")))
                expectedType
                "Snoc pattern but target is not a collection type"
                ctx
        
        PDInductivePat name pats -> do
          -- Inductive pattern: look up data constructor type from environment
          env <- getEnv
          case lookupEnv (stringToVar name) env of
            Just scheme -> do
              -- Found in environment: use the declared type
              (_constraints, ctorType) <- instantiateSchemeInState scheme
              
              -- Data constructor type: arg1 -> arg2 -> ... -> resultType
              let (argTypes, resultType) = extractFunctionArgs ctorType
              
              -- Check argument count matches
              if length argTypes /= length pats
                then throwError $ TE.TypeMismatch
                       (foldr TFun resultType (replicate (length pats) (TVar (TyVar "a"))))
                       ctorType
                       ("Data constructor " ++ name ++ " expects " ++ show (length argTypes) 
                        ++ " arguments, but got " ++ show (length pats))
                       ctx
                else do
                  -- Unify result type with expected type
                  s0 <- unifyTypesWithContext resultType expectedType ctx
                  resultType' <- applySubstWithConstraintsM s0 resultType
                  argTypes' <- mapM (applySubstWithConstraintsM s0) argTypes

                  -- Recursively infer each sub-pattern
                  results <- zipWithM (\p argTy -> inferPrimitiveDataPattern p argTy ctx) pats argTypes'
                  let (_, bindingsList, substs) = unzip3 results
                      allBindings = concat bindingsList
                      s = foldr composeSubst s0 substs

                  -- Return the result type, not expected type
                  resultType'' <- applySubstWithConstraintsM s resultType'
                  return (resultType'', allBindings, s)
            
            Nothing -> do
              -- Not found in environment: use generic inference
              argTypes <- mapM (\_ -> freshVar "arg") pats
              let resultType = TInductive name argTypes

              s0 <- unifyTypesWithContext resultType expectedType ctx
              resultType' <- applySubstWithConstraintsM s0 resultType

              argTypes' <- mapM (applySubstWithConstraintsM s0) argTypes
              results <- zipWithM (\p argTy -> inferPrimitiveDataPattern p argTy ctx) pats argTypes'
              let (_, bindingsList, substs) = unzip3 results
                  allBindings = concat bindingsList
                  s = foldr composeSubst s0 substs

              resultType'' <- applySubstWithConstraintsM s resultType'
              return (resultType'', allBindings, s)
        
        -- MathValue primitive patterns
        PDFracPat patNum patDen -> do
          -- Div: MathValue -> PolyExpr, PolyExpr
          -- However, if pattern is a pattern variable, it gets MathValue (auto-conversion)
          let polyExprTy = TPolyExpr
              mathValueTy = TMathValue
              numTy = if isPDPatVar patNum then mathValueTy else polyExprTy
              denTy = if isPDPatVar patDen then mathValueTy else polyExprTy
          (_, bindings1, s1) <- inferPrimitiveDataPattern patNum numTy ctx
          denTy' <- applySubstWithConstraintsM s1 denTy
          (_, bindings2, s2) <- inferPrimitiveDataPattern patDen denTy' ctx
          let s = composeSubst s2 s1
          expectedType' <- applySubstWithConstraintsM s expectedType
          return (expectedType', bindings1 ++ bindings2, s)
        
        PDPlusPat patTerms -> do
          -- Plus: PolyExpr -> [TermExpr]
          -- If pattern variable, it gets [MathValue]
          let termExprTy = TTermExpr
              mathValueTy = TMathValue
              termsTy = if isPDPatVar patTerms then TCollection mathValueTy else TCollection termExprTy
          (_, bindings, s) <- inferPrimitiveDataPattern patTerms termsTy ctx
          expectedType' <- applySubstWithConstraintsM s expectedType
          return (expectedType', bindings, s)
        
        PDTermPat patCoeff patMonomials -> do
          -- Term: TermExpr -> Integer, [(SymbolExpr, Integer)]
          -- If patMonomials is pattern variable, it gets [(MathValue, Integer)]
          let symbolExprTy = TSymbolExpr
              mathValueTy = TMathValue
              monomialsElemTy = if isPDPatVar patMonomials
                                then TTuple [mathValueTy, TInt]
                                else TTuple [symbolExprTy, TInt]
          (_, bindings1, s1) <- inferPrimitiveDataPattern patCoeff TInt ctx
          monomialsCollTy <- applySubstWithConstraintsM s1 (TCollection monomialsElemTy)
          (_, bindings2, s2) <- inferPrimitiveDataPattern patMonomials monomialsCollTy ctx
          let s = composeSubst s2 s1
          expectedType' <- applySubstWithConstraintsM s expectedType
          return (expectedType', bindings1 ++ bindings2, s)
        
        PDSymbolPat patName patIndices -> do
          -- Symbol: SymbolExpr -> String, [IndexExpr]
          -- patName and patIndices types don't change for pattern variables
          let indexExprTy = TIndexExpr
          (_, bindings1, s1) <- inferPrimitiveDataPattern patName TString ctx
          indicesCollTy <- applySubstWithConstraintsM s1 (TCollection indexExprTy)
          (_, bindings2, s2) <- inferPrimitiveDataPattern patIndices indicesCollTy ctx
          let s = composeSubst s2 s1
          expectedType' <- applySubstWithConstraintsM s expectedType
          return (expectedType', bindings1 ++ bindings2, s)
        
        PDApply1Pat patFn patArg -> do
          -- Apply1: SymbolExpr -> (MathValue -> MathValue), MathValue
          let mathValueTy = TMathValue
              fnTy = TFun mathValueTy mathValueTy
          (_, bindings1, s1) <- inferPrimitiveDataPattern patFn fnTy ctx
          mathValueTy' <- applySubstWithConstraintsM s1 mathValueTy
          (_, bindings2, s2) <- inferPrimitiveDataPattern patArg mathValueTy' ctx
          let s = composeSubst s2 s1
          expectedType' <- applySubstWithConstraintsM s expectedType
          return (expectedType', bindings1 ++ bindings2, s)
        
        PDApply2Pat patFn patArg1 patArg2 -> do
          let mathValueTy = TMathValue
              fnTy = TFun mathValueTy (TFun mathValueTy mathValueTy)
          (_, bindings1, s1) <- inferPrimitiveDataPattern patFn fnTy ctx
          mathValueTy1 <- applySubstWithConstraintsM s1 mathValueTy
          (_, bindings2, s2) <- inferPrimitiveDataPattern patArg1 mathValueTy1 ctx
          mathValueTy2 <- applySubstWithConstraintsM s2 mathValueTy
          (_, bindings3, s3) <- inferPrimitiveDataPattern patArg2 mathValueTy2 ctx
          let s = composeSubst s3 (composeSubst s2 s1)
          expectedType' <- applySubstWithConstraintsM s expectedType
          return (expectedType', bindings1 ++ bindings2 ++ bindings3, s)
        
        PDApply3Pat patFn patArg1 patArg2 patArg3 -> do
          let mathValueTy = TMathValue
              fnTy = TFun mathValueTy (TFun mathValueTy (TFun mathValueTy mathValueTy))
          (_, bindings1, s1) <- inferPrimitiveDataPattern patFn fnTy ctx
          mathValueTy1 <- applySubstWithConstraintsM s1 mathValueTy
          (_, bindings2, s2) <- inferPrimitiveDataPattern patArg1 mathValueTy1 ctx
          mathValueTy2 <- applySubstWithConstraintsM s2 mathValueTy
          (_, bindings3, s3) <- inferPrimitiveDataPattern patArg2 mathValueTy2 ctx
          mathValueTy3 <- applySubstWithConstraintsM s3 mathValueTy
          (_, bindings4, s4) <- inferPrimitiveDataPattern patArg3 mathValueTy3 ctx
          let s = composeSubst s4 (composeSubst s3 (composeSubst s2 s1))
          expectedType' <- applySubstWithConstraintsM s expectedType
          return (expectedType', bindings1 ++ bindings2 ++ bindings3 ++ bindings4, s)
        
        PDApply4Pat patFn patArg1 patArg2 patArg3 patArg4 -> do
          let mathValueTy = TMathValue
              fnTy = TFun mathValueTy (TFun mathValueTy (TFun mathValueTy (TFun mathValueTy mathValueTy)))
          (_, bindings1, s1) <- inferPrimitiveDataPattern patFn fnTy ctx
          mathValueTy1 <- applySubstWithConstraintsM s1 mathValueTy
          (_, bindings2, s2) <- inferPrimitiveDataPattern patArg1 mathValueTy1 ctx
          mathValueTy2 <- applySubstWithConstraintsM s2 mathValueTy
          (_, bindings3, s3) <- inferPrimitiveDataPattern patArg2 mathValueTy2 ctx
          mathValueTy3 <- applySubstWithConstraintsM s3 mathValueTy
          (_, bindings4, s4) <- inferPrimitiveDataPattern patArg3 mathValueTy3 ctx
          mathValueTy4 <- applySubstWithConstraintsM s4 mathValueTy
          (_, bindings5, s5) <- inferPrimitiveDataPattern patArg4 mathValueTy4 ctx
          let s = composeSubst s5 (composeSubst s4 (composeSubst s3 (composeSubst s2 s1)))
          expectedType' <- applySubstWithConstraintsM s expectedType
          return (expectedType', bindings1 ++ bindings2 ++ bindings3 ++ bindings4 ++ bindings5, s)
        
        PDQuotePat patExpr -> do
          -- Quote: SymbolExpr -> MathValue
          let mathValueTy = TMathValue
          (_, bindings, s) <- inferPrimitiveDataPattern patExpr mathValueTy ctx
          expectedType' <- applySubstWithConstraintsM s expectedType
          return (expectedType', bindings, s)
        
        PDFunctionPat patName patArgs -> do
          -- Function: SymbolExpr -> MathValue, [MathValue]
          let mathValueTy = TMathValue
          (_, bindings1, s1) <- inferPrimitiveDataPattern patName mathValueTy ctx
          argsCollTy <- applySubstWithConstraintsM s1 (TCollection mathValueTy)
          (_, bindings2, s2) <- inferPrimitiveDataPattern patArgs argsCollTy ctx
          expectedType' <- applySubstWithConstraintsM s2 expectedType
          return (expectedType', bindings1 ++ bindings2, s2)
        
        PDSubPat patExpr -> do
          -- Sub: IndexExpr -> MathValue
          let mathValueTy = TMathValue
          (_, bindings, s) <- inferPrimitiveDataPattern patExpr mathValueTy ctx
          expectedType' <- applySubstWithConstraintsM s expectedType
          return (expectedType', bindings, s)

        PDSupPat patExpr -> do
          -- Sup: IndexExpr -> MathValue
          let mathValueTy = TMathValue
          (_, bindings, s) <- inferPrimitiveDataPattern patExpr mathValueTy ctx
          expectedType' <- applySubstWithConstraintsM s expectedType
          return (expectedType', bindings, s)
        
        PDUserPat patExpr -> do
          -- User: IndexExpr -> MathValue
          let mathValueTy = TMathValue
          (_, bindings, s) <- inferPrimitiveDataPattern patExpr mathValueTy ctx
          expectedType' <- applySubstWithConstraintsM s expectedType
          return (expectedType', bindings, s)
  
  -- Match expressions (pattern matching)
  IMatchExpr mode target matcher clauses fallback -> do
    modify $ \state -> state
      { inferMatchSiteCount = inferMatchSiteCount state + 1 }
    let exprCtx = withExpr (prettyStr expr) ctx
    warnMatchWithoutElse exprCtx fallback
    (targetTI, s1) <- inferIExprWithContext target exprCtx
    (matcherTI, s2) <- inferIExprWithContext matcher exprCtx
    let targetType = tiExprType targetTI
        matcherType = tiExprType matcherTI
        s12 = composeSubst s2 s1
    commonResult <- freshVar "matchResult"

    -- T-MATCH: target, matcher, then each arm in source order.  An arm is
    -- checked exactly once as pattern -> matcher equality -> body.
    case clauses of
      [] -> do
        -- Surface syntax requires an ordinary arm, but keep the internal form
        -- total for generated expressions.
        sPattern <-
          checkMatcherAtPattern
            exprCtx matcher matcherTI matcherType targetType CapAny s12
        case fallback of
          Nothing -> do
            targetTI' <- applySubstToTIExprM sPattern targetTI
            matcherTI' <- applySubstToTIExprM sPattern matcherTI
            resultTy' <- applySubstWithConstraintsM sPattern commonResult
            return (mkTIExpr resultTy'
                      (TIMatchExpr mode targetTI' matcherTI' [] Nothing), sPattern)
          Just fallbackExpr -> do
            (fallbackTI, fallbackSubst) <-
              inferIExprWithContext fallbackExpr exprCtx
            let preUnifyS = composeSubst fallbackSubst sPattern
            expectedType <- applySubstWithConstraintsM preUnifyS commonResult
            fallbackType <-
              applySubstWithConstraintsM preUnifyS (tiExprType fallbackTI)
            resultSubst <-
              unifyTypesWithContext expectedType fallbackType exprCtx
            let finalS = composeSubst resultSubst preUnifyS
            targetTI' <- applySubstToTIExprM finalS targetTI
            matcherTI' <- applySubstToTIExprM finalS matcherTI
            fallbackTI' <- applySubstToTIExprM finalS fallbackTI
            resultTy' <- applySubstWithConstraintsM finalS commonResult
            return (mkTIExpr resultTy'
                      (TIMatchExpr mode targetTI' matcherTI' [] (Just fallbackTI')), finalS)
      _ -> do
        (resultTy, clauseTIs, clauseS) <-
          inferMatchClausesWithMatcher
            exprCtx matcher matcherTI matcherType targetType
            commonResult clauses s12
        (fallbackTI, finalS) <-
          case fallback of
            Nothing -> return (Nothing, clauseS)
            Just fallbackExpr -> do
              (rawFallbackTI, fallbackSubst) <-
                inferIExprWithContext fallbackExpr exprCtx
              let preUnifyS = composeSubst fallbackSubst clauseS
              expectedType <- applySubstWithConstraintsM preUnifyS resultTy
              fallbackType <-
                applySubstWithConstraintsM preUnifyS (tiExprType rawFallbackTI)
              resultSubst <-
                unifyTypesWithContext expectedType fallbackType exprCtx
              let combined = composeSubst resultSubst preUnifyS
              typedFallback <- applySubstToTIExprM combined rawFallbackTI
              return (Just typedFallback, combined)
        targetTI' <- applySubstToTIExprM finalS targetTI
        matcherTI' <- applySubstToTIExprM finalS matcherTI
        clauseTIs' <- mapM (applyMatchClauseSubst finalS) clauseTIs
        resultTy' <- applySubstWithConstraintsM finalS resultTy
        return (mkTIExpr resultTy'
                  (TIMatchExpr mode targetTI' matcherTI' clauseTIs' fallbackTI), finalS)
  
  -- MatchAll expressions
  IMatchAllExpr mode target matcher clauses -> do
    modify $ \state -> state
      { inferMatchSiteCount = inferMatchSiteCount state + 1 }
    let exprCtx = withExpr (prettyStr expr) ctx
    (targetTI, s1) <- inferIExprWithContext target exprCtx
    let targetType = tiExprType targetTI
    commonResult <- freshVar "matchAllElem"

    -- T-MATCHALL reads the first pattern before the matcher expression.  For
    -- the multi-arm Egison extension, later arms continue in source order.
    case clauses of
      [] -> do
        (matcherTI, s2) <- inferIExprWithContext matcher exprCtx
        let matcherType = tiExprType matcherTI
            s12 = composeSubst s2 s1
        finalS <-
          checkMatcherAtPattern
            exprCtx matcher matcherTI matcherType targetType CapAny s12
        targetTI' <- applySubstToTIExprM finalS targetTI
        matcherTI' <- applySubstToTIExprM finalS matcherTI
        resultElemTy' <- applySubstWithConstraintsM finalS commonResult
        return (mkTIExpr (TCollection resultElemTy')
                  (TIMatchAllExpr mode targetTI' matcherTI' []), finalS)
      firstClause : restClauses -> do
        firstPattern <-
          inferMatchPattern exprCtx targetType firstClause s1
        (matcherTI, s2) <- inferIExprWithContext matcher exprCtx
        let matcherType = tiExprType matcherTI
            firstS = composeSubst s2 (matchPatternSubst firstPattern)
        (firstTI, firstResult, firstFinalS) <-
          finishMatchClause
            exprCtx matcher matcherTI matcherType targetType
            commonResult firstPattern firstS
        (resultElemTy, reversedClauses, finalS) <-
          foldM
            (inferNextMatchClause
              exprCtx matcher matcherTI matcherType targetType)
            (firstResult, [firstTI], firstFinalS)
            restClauses
        targetTI' <- applySubstToTIExprM finalS targetTI
        matcherTI' <- applySubstToTIExprM finalS matcherTI
        clauseTIs <-
          mapM (applyMatchClauseSubst finalS) (reverse reversedClauses)
        resultElemTy' <- applySubstWithConstraintsM finalS resultElemTy
        return (mkTIExpr (TCollection resultElemTy')
                  (TIMatchAllExpr mode targetTI' matcherTI' clauseTIs), finalS)
  
  -- Memoized Lambda
  IMemoizedLambdaExpr args body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    argTypes <- mapM (\_ -> freshVar "memoArg") args
    let bindings = zip args argTypes  -- [(String, Type)]
        schemes = map (\(name, t) -> (name, Forall [] [] [] t)) bindings
    (bodyTI, s) <-
      withEnv schemes $
        inferIExprWithContext body exprCtx
    let bodyType = tiExprType bodyTI
    resultType <- applySubstWithConstraintsM emptySubst bodyType
    let finalSubst = s
    finalArgTypes <- mapM (applySubstWithConstraintsM finalSubst) argTypes
    finalResultType <- applySubstWithConstraintsM finalSubst resultType
    let funType = foldr TFun finalResultType finalArgTypes
    return
      ( mkTIExpr funType (TIMemoizedLambdaExpr args bodyTI)
      , finalSubst
      )
  
  -- Do expression
  IDoExpr bindings body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    -- Infer IO monad bindings: each binding should be of type IO a
    env <- getEnv
    (bindingTIs, bindingSchemes, s1) <- inferIOBindingsWithContext bindings env emptySubst exprCtx
    (bodyTI, s2) <-
      withEnv bindingSchemes $
        inferIExprWithContext body exprCtx
    let bodyType = tiExprType bodyTI
        finalS = composeSubst s2 s1
        
    -- Verify that body type is IO a
    bodyResultType <- freshVar "ioResult"
    bodyType' <- applySubstWithConstraintsM finalS bodyType
    s3 <- unifyTypesWithContext bodyType' (TIO bodyResultType) exprCtx
    resultType <- applySubstWithConstraintsM s3 (TIO bodyResultType)
    let finalS' = composeSubst s3 finalS
    return (mkTIExpr resultType (TIDoExpr bindingTIs bodyTI), finalS')
  
  -- Cambda (pattern matching lambda)
  ICambdaExpr var body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    argType <- freshVar "cambdaArg"
    (bodyTI, s) <- inferIExprWithContext body exprCtx
    let bodyType = tiExprType bodyTI
    resultType <- applySubstWithConstraintsM emptySubst bodyType
    let finalSubst = s
    argType' <- applySubstWithConstraintsM finalSubst argType
    resultType' <- applySubstWithConstraintsM finalSubst resultType
    return
      ( mkTIExpr (TFun argType' resultType') (TICambdaExpr var bodyTI)
      , finalSubst
      )
  
  -- With symbols
  IWithSymbolsExpr syms body -> do
    -- Add symbols to type environment as MathValue (TMathValue = TInt)
    -- Symbols introduced by withSymbols are mathematical symbols
    let symbolBindings = [(sym, Forall [] [] [] TMathValue) | sym <- syms]
    (bodyTI, s) <-
      withEnv symbolBindings $
        inferIExprWithContext body ctx
    let bodyType = tiExprType bodyTI
    return (mkTIExpr bodyType (TIWithSymbolsExpr syms bodyTI), s)
  
  -- Quote expressions (symbolic math)
  IQuoteExpr e -> do
    (eTI, s) <- inferIExprWithContext e ctx
    return (mkTIExpr TInt (TIQuoteExpr eTI), s)
  IQuoteSymbolExpr e -> do
    (eTI, s) <- inferIExprWithContext e ctx
    return (mkTIExpr (tiExprType eTI) (TIQuoteSymbolExpr eTI), s)
  
  -- Indexed expression (tensor indexing)
  IIndexedExpr override baseExpr indices -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    -- Special handling for IVarExpr: lookup with Var including index info
    -- Use the same strategy as refVar in Data.hs (Core.hs:235)
    (baseTI, s) <- case baseExpr of
      IVarExpr varName -> do
        -- Convert indices to index types (structure only, no content)
        -- Like: map (fmap (const Nothing)) indices in Core.hs
        let indexTypes = map (fmap (const Nothing)) indices
            varWithIndices = Var varName indexTypes
        env <- getEnv
        -- lookupEnv will try: Var "e" [Sub Nothing, Sub Nothing]
        --                 -> Var "e" [Sub Nothing]
        --                 -> Var "e" []
        case lookupEnv varWithIndices env of
          Just scheme -> do
            (constraints, t) <- instantiateSchemeInState scheme
            addConstraints constraints
            return (TIExpr (Forall [] [] constraints t) (TIVarExpr varName), emptySubst)
          Nothing -> do
            -- No variable found in type environment - fall back to normal inference
            -- This is necessary for lambda parameters, let-bound variables, etc.
            inferIExprWithContext baseExpr exprCtx
      _ -> inferIExprWithContext baseExpr exprCtx
    let baseType = tiExprType baseTI
    -- Infer indices as TIExpr
    indicesTI <- mapM (traverse (\idxExpr -> do
      (idxTI, _) <- inferIExprWithContext idxExpr exprCtx
      return idxTI)) indices
    -- Check if all indices are concrete (constants) or symbolic (variables)
    let isSymbolicIndex idx = case idx of
          Sub (TIExpr _ (TIVarExpr _)) -> True
          Sup (TIExpr _ (TIVarExpr _)) -> True
          SupSub (TIExpr _ (TIVarExpr _)) -> True
          User (TIExpr _ (TIVarExpr _)) -> True
          _ -> False
        hasSymbolicIndex = any isSymbolicIndex indicesTI
    -- For tensors with symbolic indices, keep the tensor type
    -- For concrete indices (numeric), return element type
    let resultType = case baseType of
          TTensor elemType -> 
            if hasSymbolicIndex
              then TTensor elemType  -- Symbolic index: keep tensor type
              else elemType           -- Concrete index: element access
          TCollection elemType -> elemType
          THash _keyType valType -> valType  -- Hash access returns value type
          _ -> baseType  -- Fallback: return base type
    return (mkTIExpr resultType (TIIndexedExpr override baseTI indicesTI), s)
  
  -- Subrefs expression (subscript references)
  ISubrefsExpr override baseExpr refExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (baseTI, s1) <- inferIExprWithContext baseExpr exprCtx
    (refTI, s2) <- inferIExprWithContext refExpr exprCtx
    let s12 = composeSubst s2 s1
    -- Constrain the base itself, not only the result.  Otherwise an
    -- unconstrained function parameter remains scalar and the later tensor
    -- elaboration maps the whole function elementwise before evaluation.
    elemType <- freshVar "subrefElem"
    baseType <- applySubstWithConstraintsM s12 (tiExprType baseTI)
    s3 <- unifyTypesWithContext baseType (TTensor elemType) exprCtx
    let finalS = composeSubst s3 s12
    finalElemType <- applySubstWithConstraintsM finalS elemType
    updatedBaseTI <- applySubstToTIExprM finalS baseTI
    updatedRefTI <- applySubstToTIExprM finalS refTI
    let resultType = normalizeTensorType (TTensor finalElemType)
    return (mkTIExpr resultType
              (TISubrefsExpr override updatedBaseTI updatedRefTI), finalS)
  
  -- Suprefs expression (superscript references)
  ISuprefsExpr override baseExpr refExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (baseTI, s1) <- inferIExprWithContext baseExpr exprCtx
    (refTI, s2) <- inferIExprWithContext refExpr exprCtx
    let s12 = composeSubst s2 s1
    elemType <- freshVar "suprefElem"
    baseType <- applySubstWithConstraintsM s12 (tiExprType baseTI)
    s3 <- unifyTypesWithContext baseType (TTensor elemType) exprCtx
    let finalS = composeSubst s3 s12
    finalElemType <- applySubstWithConstraintsM finalS elemType
    updatedBaseTI <- applySubstToTIExprM finalS baseTI
    updatedRefTI <- applySubstToTIExprM finalS refTI
    let resultType = normalizeTensorType (TTensor finalElemType)
    return (mkTIExpr resultType
              (TISuprefsExpr override updatedBaseTI updatedRefTI), finalS)
  
  -- Userrefs expression (user-defined references)
  IUserrefsExpr override baseExpr refExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (baseTI, s1) <- inferIExprWithContext baseExpr exprCtx
    (refTI, s2) <- inferIExprWithContext refExpr exprCtx
    let baseType = tiExprType baseTI
        finalS = composeSubst s2 s1
    -- TODO: Properly handle user-defined references
    return (mkTIExpr baseType (TIUserrefsExpr override baseTI refTI), finalS)

  -- Generate tensor expression
  IGenerateTensorExpr funcExpr shapeExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (funcTI, s1) <- inferIExprWithContext funcExpr exprCtx
    (shapeTI, s2) <- inferIExprWithContext shapeExpr exprCtx
    let funcType = tiExprType funcTI
    -- Extract element type from function result
    elemType <- case funcType of
      TFun _ resultType -> return resultType
      _ -> freshVar "tensorElem"
    let finalS = composeSubst s2 s1
    elemType' <- applySubstWithConstraintsM finalS elemType
    let resultType = normalizeTensorType (TTensor elemType')
    return (mkTIExpr resultType (TIGenerateTensorExpr funcTI shapeTI), finalS)
  
  -- Tensor expression
  ITensorExpr shapeExpr elemsExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (shapeTI, s1) <- inferIExprWithContext shapeExpr exprCtx
    (elemsTI, s2) <- inferIExprWithContext elemsExpr exprCtx
    let elemsType = tiExprType elemsTI
    -- Extract element type
    elemType <- case elemsType of
      TCollection t -> return t
      _ -> freshVar "tensorElem"
    let finalS = composeSubst s2 s1
    elemType' <- applySubstWithConstraintsM finalS elemType
    let resultType = normalizeTensorType (TTensor elemType')
    return (mkTIExpr resultType (TITensorExpr shapeTI elemsTI), finalS)
  
  -- Tensor contract expression
  ITensorContractExpr tensorExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (tensorTI, s1) <- inferIExprWithContext tensorExpr exprCtx
    let tensorType = tiExprType tensorTI
    
    -- contract : Tensor a -> [Tensor a]
    -- Ensure the argument is a Tensor type by unifying with TTensor elemType
    elemType <- freshVar "contractElem"
    tensorType' <- applySubstWithConstraintsM s1 tensorType
    s2 <- unifyTypesWithContext tensorType' (TTensor elemType) exprCtx

    let finalS = composeSubst s2 s1
    finalElemType <- applySubstWithConstraintsM finalS elemType
    let resultType = TCollection (TTensor finalElemType)
    updatedTensorTI <- applySubstToTIExprM finalS tensorTI

    return (mkTIExpr resultType (TITensorContractExpr updatedTensorTI), finalS)
  
  -- Tensor map expression
  ITensorMapExpr func tensorExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (funcTI, s1) <- inferIExprWithContext func exprCtx
    (tensorTI, s2) <- inferIExprWithContext tensorExpr exprCtx
    let funcType = tiExprType funcTI
        tensorType = tiExprType tensorTI
        s12 = composeSubst s2 s1
    -- Function maps elements: a -> b, tensor is Tensor a, result is Tensor b
    case tensorType of
      TTensor elemType -> do
        resultElemType <- freshVar "tmapElem"
        funcType' <- applySubstWithConstraintsM s12 funcType
        s3 <- unifyTypesWithContext funcType' (TFun elemType resultElemType) exprCtx
        let finalS = composeSubst s3 s12
        resultElemType' <- applySubstWithConstraintsM finalS resultElemType
        let resultType = normalizeTensorType (TTensor resultElemType')
        updatedFuncTI <- applySubstToTIExprM finalS funcTI
        updatedTensorTI <- applySubstToTIExprM finalS tensorTI
        return (mkTIExpr resultType (TITensorMapExpr updatedFuncTI updatedTensorTI), finalS)
      _ -> do
        updatedFuncTI <- applySubstToTIExprM s12 funcTI
        updatedTensorTI <- applySubstToTIExprM s12 tensorTI
        return (mkTIExpr tensorType (TITensorMapExpr updatedFuncTI updatedTensorTI), s12)
  
  -- Tensor map2 expression (binary map)
  ITensorMap2Expr func tensor1 tensor2 -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (funcTI, s1) <- inferIExprWithContext func exprCtx
    (tensor1TI, s2) <- inferIExprWithContext tensor1 exprCtx
    (tensor2TI, s3) <- inferIExprWithContext tensor2 exprCtx
    let funcType = tiExprType funcTI
        t1Type = tiExprType tensor1TI
        t2Type = tiExprType tensor2TI
        s123 = foldr composeSubst emptySubst [s3, s2, s1]
    -- Function: a -> b -> c, tensors are Tensor a and Tensor b, result is Tensor c
    case (t1Type, t2Type) of
      (TTensor elem1, TTensor elem2) -> do
        resultElemType <- freshVar "tmap2Elem"
        funcType' <- applySubstWithConstraintsM s123 funcType
        s4 <- unifyTypesWithContext funcType'
                (TFun elem1 (TFun elem2 resultElemType)) exprCtx
        let finalS = composeSubst s4 s123
        resultElemType' <- applySubstWithConstraintsM finalS resultElemType
        let resultType = normalizeTensorType (TTensor resultElemType')
        updatedFuncTI <- applySubstToTIExprM finalS funcTI
        updatedTensor1TI <- applySubstToTIExprM finalS tensor1TI
        updatedTensor2TI <- applySubstToTIExprM finalS tensor2TI
        return (mkTIExpr resultType (TITensorMap2Expr updatedFuncTI updatedTensor1TI updatedTensor2TI), finalS)
      _ -> do
        updatedFuncTI <- applySubstToTIExprM s123 funcTI
        updatedTensor1TI <- applySubstToTIExprM s123 tensor1TI
        updatedTensor2TI <- applySubstToTIExprM s123 tensor2TI
        return (mkTIExpr t1Type (TITensorMap2Expr updatedFuncTI updatedTensor1TI updatedTensor2TI), s123)
  
  -- Transpose expression
  -- ITransposeExpr takes (permutation, tensor) to match tTranspose signature
  ITransposeExpr permExpr tensorExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (permTI, s) <- inferIExprWithContext permExpr exprCtx
    let permType = tiExprType permTI
    -- Unify permutation type with [MathValue]
    permType' <- applySubstWithConstraintsM s permType
    s2 <- unifyTypesWithContext permType' (TCollection TMathValue) exprCtx
    (tensorTI, s3) <- inferIExprWithContext tensorExpr exprCtx
    let finalS = composeSubst s3 (composeSubst s2 s)
    updatedPermTI <- applySubstToTIExprM finalS permTI
    updatedTensorTI <- applySubstToTIExprM finalS tensorTI
    let tensorType = tiExprType updatedTensorTI
    -- Transpose preserves tensor type
    return (mkTIExpr (normalizeTensorType tensorType) (TITransposeExpr updatedPermTI updatedTensorTI), finalS)

  -- Flip indices expression
  IFlipIndicesExpr tensorExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (tensorTI, s) <- inferIExprWithContext tensorExpr exprCtx
    updatedTensorTI <- applySubstToTIExprM s tensorTI
    let tensorType = tiExprType updatedTensorTI
    -- Flipping indices preserves tensor type
    return (mkTIExpr (normalizeTensorType tensorType) (TIFlipIndicesExpr updatedTensorTI), s)
  
  -- Function symbol expression
  IFunctionExpr names -> do
    -- Function symbols are mathematical function symbols (e.g., f(x,y))
    -- They are represented as MathValue type
    return (mkTIExpr TMathValue (TIFunctionExpr names), emptySubst)

  -- Reshape: type-annotated expression `(e : T)` desugared by Desugar.hs.
  -- Infer e's type, subtype-unify with the annotation, return a TIReshape
  -- node typed as T. At eval time the runtime CAS structure is rewritten
  -- to fit T (or passes through unchanged for non-CAS types).
  IReshape ty inner -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    -- A nested Poly tower may contain at most one open atom set [..]:
    -- with two open slots the atom routing of the runtime reshape would
    -- be ambiguous (see Types.hasAmbiguousOpenTower).
    when (hasAmbiguousOpenTower ty) $
      throwError $ TE.UnsupportedFeature
        ("at most one open atom set [..] may appear in a nested Poly tower: "
         ++ TP.prettyType ty)
        exprCtx
    (innerTI, s) <- inferIExprWithContext inner exprCtx
    let innerType = tiExprType innerTI
    ty' <- applySubstWithConstraintsM s ty
    -- Representation-directive leniency (Phase gamma-prime of the
    -- extensible-tower plan): between CAS-family types the annotation
    -- selects a canonical form of the same value domain (trust the
    -- annotation; the runtime reshape is total on CAS values), so a
    -- structural mismatch such as a nested Poly re-annotated to its flat
    -- form — ((v : Poly (Poly Integer [i]) [x]) : Poly Integer [i, x]) —
    -- must not be a type error. Non-CAS mismatches keep failing.
    s2 <- unifyTypesWithContext innerType ty' exprCtx
            `catchError` \e ->
              if Subtype.isCasType innerType && Subtype.isCasType ty'
                then return emptySubst
                else throwError e
    let finalSubst = composeSubst s2 s
    finalTy <- applySubstWithConstraintsM finalSubst ty
    -- Apply the substitution to innerTI as well so the inner expression's
    -- scheme reflects the unified type. Without this, type-class methods like
    -- `(zero : MathValue)` keep the original `Forall [] [a] [AddMonoid a] a`
    -- scheme on `zero`, and TypeClassExpand goes down the TVar dispatch path
    -- (emitting a reference to the non-existent `dict_AddMonoid` parameter)
    -- instead of the concrete-instance path. At runtime that produces the
    -- "Expected CASData" / "Expected function" errors typical of unresolved
    -- dispatch.
    innerTI' <- applySubstToTIExprM finalSubst innerTI
    return (mkTIExpr finalTy (TIReshape finalTy innerTI'), finalSubst)

-- | Report every pattern-level boundary that is accepted by Egison but not by
-- the directly mechanized TypePM core.  This inventory is shared by ordinary
-- match sites and pattern-function definitions so a finalized DualScheme never
-- hides the fact that its body was checked through an extension path.
warnPatternCompatibility :: TypeErrorContext -> IPattern -> Infer ()
warnPatternCompatibility ctx pat = do
  resolvedExtensions <- resolvedPatternBridgeExtensions pat
  let extensionFeatures =
        nub (typePmPatternExtensions pat ++ resolvedExtensions)
  unless (null extensionFeatures) $
    warnOutsideEgisonCore
      ("pattern form(s) checked by Egison's extension layer: " ++
       intercalate ", " extensionFeatures)
      ctx
  mapM_
    (\issue ->
      warnOutsideEgisonCore
        ("pattern context accepted by Egison's extension layer: " ++ issue)
      ctx)
    (patternContextCompatibilityIssues pat)

-- | Snapshot of state that speculative structural inference must not leak.
-- Diagnostics are included so a failed probe is not observable.  The fresh counter is deliberately
-- absent so variables allocated by a discarded probe are never reused.
data ConstraintStateSnapshot = ConstraintStateSnapshot
  { snapshotConstraints :: [Constraint]
  , snapshotGlobalSubst :: Subst
  , snapshotWarnings :: [TypeWarning]
  }

saveConstraintState :: Infer ConstraintStateSnapshot
saveConstraintState = do
  state <- get
  return ConstraintStateSnapshot
    { snapshotConstraints = inferConstraints state
    , snapshotGlobalSubst = inferGlobalSubst state
    , snapshotWarnings = inferWarnings state
    }

restoreConstraintState :: ConstraintStateSnapshot -> Infer ()
restoreConstraintState snapshot =
  modify $ \state -> state
    { inferConstraints = snapshotConstraints snapshot
    , inferGlobalSubst = snapshotGlobalSubst snapshot
    , inferWarnings = snapshotWarnings snapshot
    }

-- | Align two pattern capabilities in the capability solver and publish the
-- resulting delta so target types containing the same capability variables
-- are zonked coherently.
alignPatternCapabilities
  :: TypeErrorContext -> Capability -> Capability -> Infer Subst
alignPatternCapabilities ctx left right = do
  left' <- applyCapabilityM emptySubst left
  right' <- applyCapabilityM emptySubst right
  case TU.unifyCapability left' right' of
    Left err -> throwUnifyError ctx err
    Right substitution -> do
      recordGlobalSubst ctx substitution
      return substitution

-- | Capability of an and/or/forall/loop/seq-cons pattern: both children
-- describe the same matched value and therefore carry one aligned demand.
capabilityCombine
  :: TypeErrorContext -> Capability -> Capability -> Infer Capability
capabilityCombine ctx left right = do
  modify $ \state -> state
    { inferCapabilityCombineCount = inferCapabilityCombineCount state + 1 }
  substitution <- alignPatternCapabilities ctx left right
  applyCapabilityM substitution left

-- | Type constructors whose pattern declarations are legacy CAS pattern
-- views: the declaration names the runtime view of a mathematical expression,
-- not the target type of the matcher, so its field types are not target
-- evidence.  This is an Egison extension outside the core rules and is
-- reported by the outside-core diagnostic.
legacyCasLeafFormer :: TypeFormer -> Bool
legacyCasLeafFormer former =
  former `elem`
    map (\name -> mkTypeFormer name 0)
      ["MathValue", "PolyExpr", "TermExpr", "SymbolExpr", "IndexExpr"]

-- | Convert a freshly instantiated structural type signature into capability
-- templates using one shared variable map.  This bridge is local to frozen
-- pattern-constructor projection; pattern inference itself remains in the
-- capability sort.
capabilityTemplates
  :: TypeErrorContext -> [Type] -> Infer [Capability]
capabilityTemplates ctx types = do
  let variables = Set.toList (Set.unions (map freeTyVars types))
  images <- mapM (const (freshCapability "patternTemplate")) variables
  patternEnv <- getPatternEnv
  let declaredFormers =
        [ former
        | (_, scheme) <- patternEnvToList patternEnv
        , Just (former, _) <- [patternConstructorResult scheme]
        ]
      declared former = former `elem` declaredFormers
      environment = Map.fromList (zip variables images)
      onVariable variable =
        Map.findWithDefault
          (CapVar (MkCapVar "internalMissingPatternTemplate"))
          variable
          environment
      convert ty =
        case capabilitySkeleton onVariable declared ty of
          Just capability -> return capability
          Nothing -> do
            -- Function/effect/matcher/CAS-view fields are outside the frozen
            -- core constructor projection.  They must not manufacture
            -- structure from their target type, so the conservative Egison
            -- extension is a fresh capability plus an opt-in diagnostic.
            warnOutsideEgisonCore
              ("pattern constructor field `" ++ TP.prettyType ty ++
               "` has no core capability projection; treating it as opaque")
              ctx
            freshCapability "opaquePatternTemplate"
  mapM convert types

-- | Collect the ~x pattern-variable references of an IPattern in left-to-right
-- order.  Used for the PATFUN-DEF linearity side condition: each pattern
-- function parameter must occur exactly once in the body, in declaration order.
patternVarRefsInOrder :: IPattern -> [String]
patternVarRefsInOrder pat = case pat of
  IVarPat name               -> [name]
  IWildCard                  -> []
  IPatVar _                  -> []
  IValuePat _                -> []
  IPredPat _                 -> []
  IIndexedPat p _            -> patternVarRefsInOrder p
  ILetPat _ p                -> patternVarRefsInOrder p
  INotPat p                  -> patternVarRefsInOrder p
  IAndPat p1 p2              -> patternVarRefsInOrder p1 ++ patternVarRefsInOrder p2
  IOrPat p1 p2               -> patternVarRefsInOrder p1 ++ patternVarRefsInOrder p2
  IForallPat p1 p2           -> patternVarRefsInOrder p1 ++ patternVarRefsInOrder p2
  ITuplePat ps               -> concatMap patternVarRefsInOrder ps
  IInductivePat _ ps         -> concatMap patternVarRefsInOrder ps
  ILoopPat _ (ILoopRange _ _ rp) p1 p2
                             -> patternVarRefsInOrder rp ++ patternVarRefsInOrder p1 ++ patternVarRefsInOrder p2
  IContPat                   -> []
  IPApplyPat _ ps            -> concatMap patternVarRefsInOrder ps
  IInductiveOrPApplyPat _ ps -> concatMap patternVarRefsInOrder ps
  ISeqNilPat                 -> []
  ISeqConsPat p1 p2          -> patternVarRefsInOrder p1 ++ patternVarRefsInOrder p2
  ILaterPatVar               -> []
  IDApplyPat p ps            -> concatMap patternVarRefsInOrder (p : ps)

-- | Collect the ~x pattern-variable references that occur under a branching or
-- repeating pattern (or-, loop-, not-, forall-pattern).  A pattern function
-- parameter in such a position may be expanded zero or several times along a
-- matching path, breaking the PATFUN-DEF binding contract, so PATFUN-DEF
-- rejects it.
patternVarRefsUnderBranch :: IPattern -> [String]
patternVarRefsUnderBranch = go False
  where
    go under pat = case pat of
      IVarPat name               -> [name | under]
      IWildCard                  -> []
      IPatVar _                  -> []
      IValuePat _                -> []
      IPredPat _                 -> []
      IIndexedPat p _            -> go under p
      ILetPat _ p                -> go under p
      INotPat p                  -> go True p
      IAndPat p1 p2              -> go under p1 ++ go under p2
      IOrPat p1 p2               -> go True p1 ++ go True p2
      IForallPat p1 p2           -> go True p1 ++ go True p2
      ITuplePat ps               -> concatMap (go under) ps
      IInductivePat _ ps         -> concatMap (go under) ps
      ILoopPat _ (ILoopRange _ _ rp) p1 p2
                                 -> go True rp ++ go True p1 ++ go True p2
      IContPat                   -> []
      IPApplyPat _ ps            -> concatMap (go under) ps
      IInductiveOrPApplyPat _ ps -> concatMap (go under) ps
      ISeqNilPat                 -> []
      ISeqConsPat p1 p2          -> go under p1 ++ go under p2
      ILaterPatVar               -> []
      IDApplyPat p ps            -> concatMap (go under) (p : ps)

-- | Named pattern-function calls occurring anywhere in a pattern, including
-- patterns nested in value/predicate expressions, match clauses, and matcher
-- arms.  The initial set contains PATFUN-DEF parameters.  A resolved
-- 'IInductiveOrPApplyPat' is always in the named pattern-function namespace;
-- an explicit 'IPApplyPat' variable head counts only when it is not shadowed
-- by an ordinary lexical binder.  Ordinary value references do not count.
patternFunctionCallHeads :: Set.Set String -> IPattern -> [String]
patternFunctionCallHeads initialBound = goPattern initialBound
  where
    goExpression bound expression = case expression of
      IConstantExpr _ -> []
      IVarExpr _ -> []
      IIndexedExpr _ inner indices ->
        goExpression bound inner ++ concatMap (goIndex bound) indices
      ISubrefsExpr _ left right ->
        goExpression bound left ++ goExpression bound right
      ISuprefsExpr _ left right ->
        goExpression bound left ++ goExpression bound right
      IUserrefsExpr _ left right ->
        goExpression bound left ++ goExpression bound right
      IInductiveDataExpr _ arguments ->
        concatMap (goExpression bound) arguments
      ITupleExpr values -> concatMap (goExpression bound) values
      ICollectionExpr values -> concatMap (goExpression bound) values
      IConsExpr headExpression tailExpression ->
        goExpression bound headExpression ++
          goExpression bound tailExpression
      IJoinExpr left right ->
        goExpression bound left ++ goExpression bound right
      IHashExpr pairs ->
        concatMap
          (\(key, value) ->
            goExpression bound key ++ goExpression bound value)
          pairs
      IVectorExpr values -> concatMap (goExpression bound) values
      ILambdaExpr _ parameters inner ->
        goExpression
          (bound `Set.union`
            Set.fromList (map extractNameFromVar parameters))
          inner
      IMemoizedLambdaExpr parameters inner ->
        goExpression (bound `Set.union` Set.fromList parameters) inner
      ICambdaExpr parameter inner ->
        goExpression (Set.insert parameter bound) inner
      IIfExpr condition yes no ->
        goExpression bound condition ++
          goExpression bound yes ++
          goExpression bound no
      ILetRecExpr bindings inner ->
        let groupNames =
              Set.fromList
                (concatMap (primitivePatternNames . fst) bindings)
            recursiveBound = bound `Set.union` groupNames
        in concatMap (goExpression recursiveBound . snd) bindings ++
             goExpression recursiveBound inner
      ILetExpr bindings inner ->
        goSequentialBindings bound bindings inner
      IWithSymbolsExpr symbols inner ->
        goExpression (bound `Set.union` Set.fromList symbols) inner
      IMatchExpr _ target matcher clauses fallback ->
        goExpression bound target ++
          goExpression bound matcher ++
          concatMap (goClause bound) clauses ++
          maybe [] (goExpression bound) fallback
      IMatchAllExpr _ target matcher clauses ->
        goExpression bound target ++
          goExpression bound matcher ++
          concatMap (goClause bound) clauses
      IMatcherExpr definitions ->
        concatMap (goDefinition bound) definitions
      IQuoteExpr inner -> goExpression bound inner
      IQuoteSymbolExpr inner -> goExpression bound inner
      IWedgeApplyExpr function arguments ->
        goExpression bound function ++
          concatMap (goExpression bound) arguments
      IDoExpr bindings inner ->
        goSequentialBindings bound bindings inner
      ISeqExpr first second ->
        goExpression bound first ++ goExpression bound second
      IApplyExpr function arguments ->
        goExpression bound function ++
          concatMap (goExpression bound) arguments
      IGenerateTensorExpr generator shape ->
        goExpression bound generator ++ goExpression bound shape
      ITensorExpr tensor indices ->
        goExpression bound tensor ++ goExpression bound indices
      ITensorContractExpr tensor -> goExpression bound tensor
      ITensorMapExpr function tensor ->
        goExpression bound function ++ goExpression bound tensor
      ITensorMap2Expr function left right ->
        goExpression bound function ++
          goExpression bound left ++
          goExpression bound right
      ITensorMap2WedgeExpr function left right ->
        goExpression bound function ++
          goExpression bound left ++
          goExpression bound right
      ITransposeExpr permutation tensor ->
        goExpression bound permutation ++ goExpression bound tensor
      IFlipIndicesExpr tensor -> goExpression bound tensor
      IFunctionExpr _ -> []
      IPatternFuncExpr parameters pattern ->
        goPattern (bound `Set.union` Set.fromList parameters) pattern
      IReshape _ inner -> goExpression bound inner
      IRuntimeDispatch _ _ _ arguments ->
        concatMap (goExpression bound) arguments

    goIndex bound index = case index of
      Sub expression -> goExpression bound expression
      Sup expression -> goExpression bound expression
      MultiSub first _ lastExpression ->
        goExpression bound first ++ goExpression bound lastExpression
      MultiSup first _ lastExpression ->
        goExpression bound first ++ goExpression bound lastExpression
      SupSub expression -> goExpression bound expression
      User expression -> goExpression bound expression
      DF _ _ -> []

    goClause bound (pattern, expression) =
      goPattern bound pattern ++
        goExpression
          (bound `Set.union` exportedVars pattern)
          expression

    goDefinition bound (_, nextMatcher, arms) =
      goExpression bound nextMatcher ++
        concatMap
          (\(pattern, expression) ->
            goExpression
              (bound `Set.union`
                Set.fromList (primitivePatternNames pattern))
              expression)
          arms

    goPattern bound pattern = case pattern of
      IWildCard -> []
      IPatVar _ -> []
      IValuePat expression -> goExpression bound expression
      IPredPat expression -> goExpression bound expression
      IIndexedPat inner indices ->
        goPattern bound inner ++
          concatMap (goExpression bound) indices
      ILetPat bindings inner ->
        goPatternBindings bound bindings inner
      INotPat inner -> goPattern bound inner
      IAndPat left right ->
        goPattern bound left ++
          goPattern
            (bound `Set.union` exportedVars left)
            right
      IOrPat left right ->
        goPattern bound left ++ goPattern bound right
      IForallPat left right ->
        goPattern bound left ++
          goPattern
            (bound `Set.union` exportedVars left)
            right
      ITuplePat patterns -> goPatternList bound patterns
      IInductivePat _ patterns -> goPatternList bound patterns
      ILoopPat loopVar (ILoopRange start end rangePattern) body rest ->
        let rangeCalls =
              goExpression bound start ++
                goExpression bound end ++
                goPattern bound rangePattern
            loopBound =
              Set.insert loopVar
                (bound `Set.union` exportedVars rangePattern)
            bodyCalls = goPattern loopBound body
            restBound =
              loopBound `Set.union` exportedVars body
        in rangeCalls ++ bodyCalls ++ goPattern restBound rest
      IContPat -> []
      IPApplyPat function patterns ->
        (case function of
           IVarExpr name
             | name `Set.member` bound -> []
             | otherwise -> [name]
           _ -> goExpression bound function) ++
        goPatternList bound patterns
      IVarPat _ -> []
      IInductiveOrPApplyPat name patterns ->
        name : goPatternList bound patterns
      ISeqNilPat -> []
      ISeqConsPat left right ->
        goPattern bound left ++
          goPattern
            (bound `Set.union` exportedVars left)
            right
      ILaterPatVar -> []
      IDApplyPat function patterns ->
        goPattern bound function ++
          goPatternList
            (bound `Set.union` exportedVars function)
            patterns

    goPatternList _ [] = []
    goPatternList bound (pattern : rest) =
      goPattern bound pattern ++
        goPatternList
          (bound `Set.union` exportedVars pattern)
          rest

    goSequentialBindings bound [] body =
      goExpression bound body
    goSequentialBindings bound ((pattern, rhs) : rest) body =
      let names = Set.fromList (primitivePatternNames pattern)
      in goExpression bound rhs ++
           goSequentialBindings (bound `Set.union` names) rest body

    goPatternBindings bound [] inner =
      goPattern bound inner
    goPatternBindings bound ((pattern, rhs) : rest) inner =
      let names = Set.fromList (primitivePatternNames pattern)
      in goExpression bound rhs ++
           goPatternBindings (bound `Set.union` names) rest inner

    -- Mirror the binding component returned by inferIPattern.  The broader
    -- ipatternVars inventory is intentionally unsuitable here: for example a
    -- not-pattern exports no bindings, while a non-parameter IVarPat does.
    exportedVars pattern = case pattern of
      IWildCard -> Set.empty
      IPatVar name -> Set.singleton name
      IValuePat _ -> Set.empty
      IPredPat _ -> Set.empty
      IIndexedPat inner _ -> exportedVars inner
      ILetPat _ inner -> exportedVars inner
      INotPat _ -> Set.empty
      IAndPat left right ->
        exportedVars left `Set.union` exportedVars right
      IOrPat left right ->
        -- A well-typed or-pattern exports the same names from both branches.
        -- Intersection is conservative before that equality check runs.
        exportedVars left `Set.intersection` exportedVars right
      IForallPat left right ->
        exportedVars left `Set.union` exportedVars right
      ITuplePat patterns -> Set.unions (map exportedVars patterns)
      IInductivePat _ patterns -> Set.unions (map exportedVars patterns)
      ILoopPat loopVar (ILoopRange _ _ rangePattern) body rest ->
        Set.insert loopVar
          (Set.unions
            [ exportedVars rangePattern
            , exportedVars body
            , exportedVars rest
            ])
      IContPat -> Set.empty
      IPApplyPat _ patterns -> Set.unions (map exportedVars patterns)
      IVarPat name
        | name `Set.member` initialBound -> Set.empty
        | otherwise -> Set.singleton name
      IInductiveOrPApplyPat _ patterns ->
        Set.unions (map exportedVars patterns)
      ISeqNilPat -> Set.empty
      ISeqConsPat left right ->
        exportedVars left `Set.union` exportedVars right
      ILaterPatVar -> Set.empty
      IDApplyPat base arguments ->
        Set.unions (map exportedVars (base : arguments))

capabilityFromCtor
  :: TypeErrorContext
  -> [Type]
  -> Type
  -> [Capability]
  -> Infer Capability
capabilityFromCtor ctx argumentTypes resultType children
  | length argumentTypes /= length children =
      throwError $ MatcherCapabilityError
        ("pattern constructor arity mismatch while deriving match-site "
         ++ "capability: expected " ++ show (length argumentTypes)
         ++ " children but found " ++ show (length children))
        ctx
  | otherwise = do
      templates <- capabilityTemplates ctx (argumentTypes ++ [resultType])
      let (expectedArguments, resultTemplateList) =
            splitAt (length argumentTypes) templates
      resultTemplate <- case resultTemplateList of
        [capability] -> return capability
        _ -> throwError $ MatcherCapabilityError
               "internal pattern capability projection lost its result"
               ctx
      substitution <- foldM
        (\acc (child, expected) -> do
          child' <- applyCapabilityM acc child
          expected' <- applyCapabilityM acc expected
          current <- alignPatternCapabilities ctx child' expected'
          return (composeSubst current acc))
        emptySubst
        (zip children expectedArguments)
      applyCapabilityM substitution resultTemplate

-- | Check the already-inferred matcher expression against the matcher type
-- demanded by one pattern: the pattern's capability and the target type are
-- related to the type of the matcher expression by an ordinary type
-- equality (rule G-Match of the paper).
checkMatcherAtPattern
  :: TypeErrorContext
  -> IExpr
  -> TIExpr
  -> Type
  -> Type
  -> Capability
  -> Subst
  -> Infer Subst
checkMatcherAtPattern
    ctx matcherSource matcherTyped matcherTy targetTy patternCap s0 = do
  matcherTy' <- applySubstWithConstraintsM s0 matcherTy
  patternCap' <- applyCapabilityM s0 patternCap
  targetTy' <- applySubstWithConstraintsM s0 targetTy
  rejectAnyMatcherCapabilityBypass ctx matcherTy' patternCap'
  classEnv <- getClassEnv
  constraints <- getConstraints
  (sPattern, _) <-
    solveApplicationArgument
      classEnv constraints matcherSource matcherTyped matcherTy'
      (TMatcher patternCap' targetTy') ctx
      `catchError` \err ->
        case err of
          TE.TypeMismatch expected actual reason errCtx ->
            throwError $
              TE.TypeMismatch expected actual
                (reason
                 ++ "\n  At this match clause, producer type "
                 ++ TP.prettyType matcherTy'
                 ++ " must satisfy pattern capability "
                 ++ TP.prettyCapability patternCap')
                errCtx
          _ -> throwError err
  return (composeSubst sPattern s0)

-- | 'Any' is a gradual escape hatch for ordinary values, not evidence that a
-- matcher supports a constructor or tuple pattern.  Keep unconstraining
-- variable/value-pattern requirements gradual, but reject every structured duty
-- before the ordinary unifier's catch-all TAny rule can erase it.
rejectAnyMatcherCapabilityBypass
  :: TypeErrorContext
  -> Type
  -> Capability
  -> Infer ()
rejectAnyMatcherCapabilityBypass ctx matcherType requiredCapability =
  case (matcherType, requiredCapability) of
    (TAny, capability)
      | capabilityRequiresProducerEvidence capability ->
          throwError $ MatcherCapabilityError
            "Any cannot witness a structured matcher capability"
            ctx
    (TTuple matcherTypes, CapTuple capabilities)
      | length matcherTypes == length capabilities ->
          zipWithM_
            (rejectAnyMatcherCapabilityBypass ctx)
            matcherTypes
            capabilities
    _ ->
      return ()
  where
    capabilityRequiresProducerEvidence capability =
      case capability of
        CapAny       -> False
        CapVar _      -> False
        CapSkolem _   -> True
        CapCon _ _    -> True
        CapTuple caps -> any capabilityRequiresProducerEvidence caps

-- | Frozen-signature information needed by capability-based Coverage.
patternConstructorResult :: TypeScheme -> Maybe (TypeFormer, Int)
patternConstructorResult (Forall _ _ _ ty) =
  let (arguments, result) = go ty
  in case typeFormerOf result of
       Just (former, _) -> Just (former, length arguments)
       Nothing          -> Nothing
  where
    go (TFun argument rest) =
      let (arguments, result) = go rest
      in (argument : arguments, result)
    go result = ([], result)

isGeneralConstructorClause
  :: String
  -> Int
  -> PrimitivePatPattern
  -> Bool
isGeneralConstructorClause expectedName expectedArity pattern =
  case pattern of
    PPInductivePat name arguments ->
      name == expectedName
        && length arguments == expectedArity
        && all isHole arguments
    _ -> False
  where
    isHole PPPatVar = True
    isHole _        = False

firstOf3 :: (a, b, c) -> a
firstOf3 (first, _, _) = first

-- | Declared value constructors with the former and curried arity of their
-- result.  The explicit constructor-name set prevents ordinary functions
-- with an inductive result from being mistaken for constructors.
dataConstructorShapes :: Infer [(String, TypeFormer, Int)]
dataConstructorShapes = do
  environment <- getEnv
  constructorNames <- gets inferDataConstructorNames
  return . nub $
    [ (name, former, arity)
    | (Var name indices, scheme) <- envToList environment
    , null indices
    , name `Set.member` constructorNames
    , Just (former, arity) <- [patternConstructorResult scheme]
    ]

-- | Strip lambda wrappers to find a matcher literal (for shape harvesting).
stripLambdasForShape :: IExpr -> IExpr
stripLambdasForShape (ILambdaExpr _ _ body) = stripLambdasForShape body
stripLambdasForShape e = e

-- | Statically resolved matcher clause shapes at a match site.
data VpShape
  = VpClauses [PrimitivePatPattern]  -- ^ a matcher with these clause pps
  | VpTuple [VpShape]                -- ^ a product matcher, componentwise
  | VpUnknown                        -- ^ opaque (e.g. a matcher-typed parameter)

-- | Resolve a match site's matcher expression to its clause shapes, when
-- statically known: a matcher literal, a tuple of matchers, or a (possibly
-- applied) top-level matcher definition harvested at its IDefine.
resolveVpShape :: IExpr -> Infer VpShape
resolveVpShape (IMatcherExpr patDefs) =
  return $ VpClauses (map (\(pp, _, _) -> pp) patDefs)
resolveVpShape (ITupleExpr es) = VpTuple <$> mapM resolveVpShape es
resolveVpShape (IVarExpr name) = do
  shapes <- gets inferMatcherShapes
  return $ maybe VpUnknown VpClauses (Map.lookup name shapes)
resolveVpShape (IApplyExpr f _) = resolveVpShape f
resolveVpShape _ = return VpUnknown

-- | Pattern variables a pattern can bind (conservative, all binders).
ipatternVars :: IPattern -> [String]
ipatternVars = go
  where
    go IWildCard                    = []
    go (IPatVar s)                  = [s]
    go (IValuePat _)                = []
    go (IPredPat _)                 = []
    go (IIndexedPat p _)            = go p
    go (ILetPat _ p)                = go p
    go (INotPat p)                  = go p
    go (IAndPat p q)                = go p ++ go q
    go (IOrPat p q)                 = go p ++ go q
    go (IForallPat p q)             = go p ++ go q
    go (ITuplePat ps)               = concatMap go ps
    go (IInductivePat _ ps)         = concatMap go ps
    go (ILoopPat s (ILoopRange _ _ pe) p q) = s : go pe ++ go p ++ go q
    go IContPat                     = []
    go (IPApplyPat _ ps)            = concatMap go ps
    go (IVarPat _)                  = []
    go (IInductiveOrPApplyPat _ ps) = concatMap go ps
    go ISeqNilPat                   = []
    go (ISeqConsPat p q)            = go p ++ go q
    go ILaterPatVar                 = []
    go (IDApplyPat p ps)            = go p ++ concatMap go ps

-- | Variable references of an expression (conservative over-approximation:
-- collects every IVarExpr occurrence; local rebinding is not subtracted).
-- Used only to intersect with candidate pattern variables, so global names
-- are harmless.
iexprVarRefs :: IExpr -> [String]
iexprVarRefs = go
  where
    go (IConstantExpr _)        = []
    go (IVarExpr s)             = [s]
    go (IIndexedExpr _ e is)    = go e ++ concatMap goIdx is
    go (ISubrefsExpr _ e1 e2)   = go e1 ++ go e2
    go (ISuprefsExpr _ e1 e2)   = go e1 ++ go e2
    go (IUserrefsExpr _ e1 e2)  = go e1 ++ go e2
    go (IInductiveDataExpr _ es) = concatMap go es
    go (ITupleExpr es)          = concatMap go es
    go (ICollectionExpr es)     = concatMap go es
    go (IConsExpr e1 e2)        = go e1 ++ go e2
    go (IJoinExpr e1 e2)        = go e1 ++ go e2
    go (IHashExpr prs)          = concatMap (\(a, b) -> go a ++ go b) prs
    go (IVectorExpr es)         = concatMap go es
    go (ILambdaExpr _ _ b)      = go b
    go (IMemoizedLambdaExpr _ b) = go b
    go (ICambdaExpr _ b)        = go b
    go (IIfExpr c t e)          = go c ++ go t ++ go e
    go (ILetRecExpr bs b)       = concatMap (go . snd) bs ++ go b
    go (ILetExpr bs b)          = concatMap (go . snd) bs ++ go b
    go (IWithSymbolsExpr _ b)   = go b
    go (IMatchExpr _ t m cls fallback) =
      go t ++ go m ++ concatMap goClause cls ++ maybe [] go fallback
    go (IMatchAllExpr _ t m cls) = go t ++ go m ++ concatMap goClause cls
    go (IMatcherExpr defs)      = concatMap goDef defs
    go (IQuoteExpr e)           = go e
    go (IQuoteSymbolExpr e)     = go e
    go (IWedgeApplyExpr f es)   = go f ++ concatMap go es
    go (IDoExpr bs b)           = concatMap (go . snd) bs ++ go b
    go (ISeqExpr e1 e2)         = go e1 ++ go e2
    go (IApplyExpr f es)        = go f ++ concatMap go es
    go (IGenerateTensorExpr e1 e2) = go e1 ++ go e2
    go (ITensorExpr e1 e2)      = go e1 ++ go e2
    go (ITensorContractExpr e)  = go e
    go (ITensorMapExpr e1 e2)   = go e1 ++ go e2
    go (ITensorMap2Expr e1 e2 e3) = go e1 ++ go e2 ++ go e3
    go (ITensorMap2WedgeExpr e1 e2 e3) = go e1 ++ go e2 ++ go e3
    go (ITransposeExpr e1 e2)   = go e1 ++ go e2
    go (IFlipIndicesExpr e)     = go e
    go (IFunctionExpr _)        = []
    go (IPatternFuncExpr _ p)   = goPat p
    go (IReshape _ e)           = go e
    go (IRuntimeDispatch _ _ _ es) = concatMap go es
    goIdx idx = case idx of
      Sub e    -> go e
      Sup e    -> go e
      _        -> []
    goClause (p, b) = goPat p ++ go b
    goDef (_, m, arms) = go m ++ concatMap (go . snd) arms
    goPat p = case p of
      IValuePat e        -> go e
      IPredPat e         -> go e
      IIndexedPat q es   -> goPat q ++ concatMap go es
      ILetPat bs q       -> concatMap (go . snd) bs ++ goPat q
      INotPat q          -> goPat q
      IAndPat q r        -> goPat q ++ goPat r
      IOrPat q r         -> goPat q ++ goPat r
      IForallPat q r     -> goPat q ++ goPat r
      ITuplePat qs       -> concatMap goPat qs
      IInductivePat _ qs -> concatMap goPat qs
      ILoopPat _ (ILoopRange e1 e2 pe) q r ->
        go e1 ++ go e2 ++ goPat pe ++ goPat q ++ goPat r
      IPApplyPat e qs    -> go e ++ concatMap goPat qs
      IInductiveOrPApplyPat _ qs -> concatMap goPat qs
      ISeqConsPat q r    -> goPat q ++ goPat r
      IDApplyPat q qs    -> goPat q ++ concatMap goPat qs
      _                  -> []

-- | Lexically scoped free variables of an expression.  Unlike
-- 'iexprVarRefs', this respects lambda parameters and local pattern bindings.
-- It is used only to identify actual cycles for the recursive-value root
-- restriction; it does not contribute matcher capability evidence.
iexprFreeVarRefs :: IExpr -> Set.Set String
iexprFreeVarRefs = go Set.empty
  where
    go bound expression = case expression of
      IConstantExpr _ ->
        Set.empty
      IVarExpr name
        | name `Set.member` bound ->
            Set.empty
        | otherwise ->
            Set.singleton name
      IIndexedExpr _ base indices ->
        Set.unions (go bound base : map (goIndex bound) indices)
      ISubrefsExpr _ left right ->
        go bound left `Set.union` go bound right
      ISuprefsExpr _ left right ->
        go bound left `Set.union` go bound right
      IUserrefsExpr _ left right ->
        go bound left `Set.union` go bound right
      IInductiveDataExpr _ args ->
        Set.unions (map (go bound) args)
      ITupleExpr elems ->
        Set.unions (map (go bound) elems)
      ICollectionExpr elems ->
        Set.unions (map (go bound) elems)
      IConsExpr headExpr tailExpr ->
        go bound headExpr `Set.union` go bound tailExpr
      IJoinExpr left right ->
        go bound left `Set.union` go bound right
      IHashExpr pairs ->
        Set.unions
          [ go bound key `Set.union` go bound value
          | (key, value) <- pairs
          ]
      IVectorExpr elems ->
        Set.unions (map (go bound) elems)
      ILambdaExpr _ params body ->
        go
          (bound `Set.union`
            Set.fromList (map extractNameFromVar params))
          body
      IMemoizedLambdaExpr params body ->
        go (bound `Set.union` Set.fromList params) body
      ICambdaExpr param body ->
        go (Set.insert param bound) body
      IIfExpr condition yes no ->
        Set.unions [go bound condition, go bound yes, go bound no]
      ILetRecExpr bindings body ->
        let groupNames =
              Set.fromList
                (concatMap (primitivePatternNames . fst) bindings)
            recursiveBound = bound `Set.union` groupNames
        in Set.unions
             (go recursiveBound body :
               map (go recursiveBound . snd) bindings)
      ILetExpr bindings body ->
        freeSequentialBindings bound bindings body
      IWithSymbolsExpr symbols body ->
        go (bound `Set.union` Set.fromList symbols) body
      IMatchExpr _ target matcher clauses fallback ->
        Set.unions
          [ go bound target
          , go bound matcher
          , Set.unions (map (goClause bound) clauses)
          , maybe Set.empty (go bound) fallback
          ]
      IMatchAllExpr _ target matcher clauses ->
        Set.unions
          [ go bound target
          , go bound matcher
          , Set.unions (map (goClause bound) clauses)
          ]
      IMatcherExpr definitions ->
        Set.unions (map (goDefinition bound) definitions)
      IQuoteExpr inner ->
        go bound inner
      IQuoteSymbolExpr inner ->
        go bound inner
      IWedgeApplyExpr function args ->
        Set.unions (go bound function : map (go bound) args)
      IDoExpr bindings body ->
        freeSequentialBindings bound bindings body
      ISeqExpr first second ->
        go bound first `Set.union` go bound second
      IApplyExpr function args ->
        Set.unions (go bound function : map (go bound) args)
      IGenerateTensorExpr generator shape ->
        go bound generator `Set.union` go bound shape
      ITensorExpr tensor indices ->
        go bound tensor `Set.union` go bound indices
      ITensorContractExpr tensor ->
        go bound tensor
      ITensorMapExpr function tensor ->
        go bound function `Set.union` go bound tensor
      ITensorMap2Expr function left right ->
        Set.unions [go bound function, go bound left, go bound right]
      ITensorMap2WedgeExpr function left right ->
        Set.unions [go bound function, go bound left, go bound right]
      ITransposeExpr permutation tensor ->
        go bound permutation `Set.union` go bound tensor
      IFlipIndicesExpr tensor ->
        go bound tensor
      IFunctionExpr _ ->
        Set.empty
      IPatternFuncExpr params pattern ->
        goPattern (bound `Set.union` Set.fromList params) pattern
      IReshape _ inner ->
        go bound inner
      IRuntimeDispatch _ _ _ args ->
        Set.unions (map (go bound) args)

    goIndex bound index = case index of
      Sub expression ->
        go bound expression
      Sup expression ->
        go bound expression
      MultiSub first _ lastExpr ->
        go bound first `Set.union` go bound lastExpr
      MultiSup first _ lastExpr ->
        go bound first `Set.union` go bound lastExpr
      SupSub expression ->
        go bound expression
      User expression ->
        go bound expression
      DF _ _ ->
        Set.empty

    freeSequentialBindings bound [] body =
      go bound body
    freeSequentialBindings bound ((pat, rhs) : rest) body =
      let names = Set.fromList (primitivePatternNames pat)
      in go bound rhs `Set.union`
           freeSequentialBindings (bound `Set.union` names) rest body

    goClause bound (pattern, body) =
      goPattern bound pattern `Set.union`
        go
          (bound `Set.union` Set.fromList (ipatternVars pattern))
          body

    goDefinition bound (_, nextMatcher, arms) =
      go bound nextMatcher `Set.union`
        Set.unions
          [ go
              (bound `Set.union`
                Set.fromList (primitivePatternNames pat))
              body
          | (pat, body) <- arms
          ]

    goPattern bound pattern = case pattern of
      IWildCard ->
        Set.empty
      IPatVar _ ->
        Set.empty
      IValuePat expression ->
        go bound expression
      IPredPat expression ->
        go bound expression
      IIndexedPat inner indices ->
        goPattern bound inner `Set.union`
          Set.unions (map (go bound) indices)
      ILetPat bindings inner ->
        freePatternBindings bound bindings inner
      INotPat inner ->
        goPattern bound inner
      IAndPat left right ->
        goPattern bound left `Set.union`
          goPattern
            (bound `Set.union` Set.fromList (ipatternVars left))
            right
      IOrPat left right ->
        goPattern bound left `Set.union` goPattern bound right
      IForallPat left right ->
        goPattern bound left `Set.union`
          goPattern
            (bound `Set.union` Set.fromList (ipatternVars left))
            right
      ITuplePat patterns ->
        goPatternList bound patterns
      IInductivePat _ patterns ->
        goPatternList bound patterns
      ILoopPat loopVar (ILoopRange start end rangePattern) loopBody rest ->
        let rangeRefs =
              Set.unions
                [ go bound start
                , go bound end
                , goPattern bound rangePattern
                ]
            loopBound =
              Set.insert loopVar
                (bound `Set.union`
                  Set.fromList (ipatternVars rangePattern))
            bodyRefs = goPattern loopBound loopBody
            restBound =
              loopBound `Set.union`
                Set.fromList (ipatternVars loopBody)
        in Set.unions
             [rangeRefs, bodyRefs, goPattern restBound rest]
      IContPat ->
        Set.empty
      IPApplyPat function patterns ->
        go bound function `Set.union` goPatternList bound patterns
      IVarPat _ ->
        Set.empty
      IInductiveOrPApplyPat _ patterns ->
        goPatternList bound patterns
      ISeqNilPat ->
        Set.empty
      ISeqConsPat left right ->
        goPattern bound left `Set.union`
          goPattern
            (bound `Set.union` Set.fromList (ipatternVars left))
            right
      ILaterPatVar ->
        Set.empty
      IDApplyPat base args ->
        goPattern bound base `Set.union`
          goPatternList
            (bound `Set.union` Set.fromList (ipatternVars base))
            args

    goPatternList _ [] =
      Set.empty
    goPatternList bound (pattern : rest) =
      goPattern bound pattern `Set.union`
        goPatternList
          (bound `Set.union` Set.fromList (ipatternVars pattern))
          rest

    freePatternBindings bound [] inner =
      goPattern bound inner
    freePatternBindings bound ((pat, rhs) : rest) inner =
      let names = Set.fromList (primitivePatternNames pat)
      in go bound rhs `Set.union`
           freePatternBindings (bound `Set.union` names) rest inner

-- | Align a clause's pp with a match-site pattern, threading the pattern
-- variables bound to the left (within the same atom's pattern) and
-- collecting each captured value pattern's expression with the variables
-- forbidden for it.  Nothing = shape mismatch (the clause is not selected;
-- no obligation).
vpAlign :: [String] -> PrimitivePatPattern -> IPattern
        -> Maybe ([(IExpr, [String])], [String])
vpAlign acc pp p = case (pp, p) of
  (PPWildCard, IWildCard)       -> Just ([], acc)
  (PPPatVar, q)                 -> Just ([], acc ++ ipatternVars q)
  (PPValuePat _, IValuePat e)   -> Just ([(e, acc)], acc)
  (PPInductivePat n pps, IInductivePat n' ps)
    | n == n' && length pps == length ps -> vpAlignList acc pps ps
  (PPInductivePat n pps, IInductiveOrPApplyPat n' ps)
    | n == n' && length pps == length ps -> vpAlignList acc pps ps
  (PPTuplePat pps, ITuplePat ps)
    | length pps == length ps   -> vpAlignList acc pps ps
  _                             -> Nothing

vpAlignList :: [String] -> [PrimitivePatPattern] -> [IPattern]
            -> Maybe ([(IExpr, [String])], [String])
vpAlignList acc [] [] = Just ([], acc)
vpAlignList acc (pp : pps) (p : ps) = do
  (caps, acc')  <- vpAlign acc pp p
  (caps', acc'') <- vpAlignList acc' pps ps
  return (caps ++ caps', acc'')
vpAlignList _ _ _ = Nothing

-- | Production use-site safeguard for primitive-pattern patterns that Egison
-- accepts beyond the core's PPatCoreOrder restriction. At a match site whose
-- matcher clause shapes are statically known, a value pattern captured by a
-- #\$x may not reference pattern variables bound to its left within the same
-- clause pattern (bindings made before the atom are available; those of the
-- same clause's holes are not yet made). Core-admissible clauses cannot have
-- such a capture after a hole. The check is componentwise for a tuple of
-- matchers (each component is its own atom, so earlier components' bindings
-- are pre-atom for later ones). Opaque production matchers are not checked.
checkVpScope :: TypeErrorContext -> IExpr -> [IMatchClause] -> Infer ()
checkVpScope ctx matcherExpr clauses = do
  shape <- resolveVpShape matcherExpr
  mapM_ (\(pat, _) -> walkShape shape pat) clauses
  where
    walkShape (VpTuple shapes) (ITuplePat ps)
      | length shapes == length ps = zipWithM_ walkShape shapes ps
    walkShape (VpClauses pps) p = mapM_ (checkClause p) pps
    walkShape _ _ = return ()
    checkClause p pp = case vpAlign [] pp p of
      Nothing -> return ()
      Just (caps, _) ->
        mapM_ (\(e, forbidden) -> do
          let bad = nub (filter (`elem` forbidden) (iexprVarRefs e))
          unless (null bad) $
            throwError $ MatchCapturedValuePatScope bad (prettyStr pp) ctx)
          caps

data DataArmRoot
  = DeclaredDataRoot TypeFormer
  | CollectionDataRoot
  | BoolDataRoot
  deriving (Eq)

-- | TypePM ArmCoverage, extended only with Egison's built-in collection and
-- Boolean pattern syntax.  Either the final arm is a variable/wildcard, or
-- every arm is constructor-rooted and every constructor of every mentioned
-- former has a general arm.
pdArmsExhaustive
  :: [(String, TypeFormer, Int)]
  -> [IPrimitiveDataPattern]
  -> Bool
pdArmsExhaustive declarations arms =
     armsCatchAllLast
  || (not (null arms)
      && all (maybe False (const True) . armRoot) arms
      && all declaredConstructorCovered mentionedDeclarations
      && (not mentionsCollection || collectionCovered)
      && (not mentionsBool || boolCovered))
  where
    armsCatchAllLast =
      case reverse arms of
        finalArm : _ -> pdIrrefutable finalArm
        []           -> False

    armRoot (PDInductivePat name _) =
      DeclaredDataRoot <$> lookupFormer name
    armRoot PDEmptyPat = Just CollectionDataRoot
    armRoot PDConsPat{} = Just CollectionDataRoot
    armRoot PDSnocPat{} = Just CollectionDataRoot
    armRoot (PDConstantPat (BoolExpr _)) = Just BoolDataRoot
    armRoot _ = Nothing

    lookupFormer name =
      case [former | (declaredName, former, _) <- declarations,
                     declaredName == name] of
        former : _ -> Just former
        []         -> Nothing

    mentionedRoots = [root | Just root <- map armRoot arms]
    mentionedDeclarations =
      [ declaration
      | declaration@(_, former, _) <- declarations
      , DeclaredDataRoot former `elem` mentionedRoots
      ]
    declaredConstructorCovered (name, _, arity) =
      any (isGeneralDataConstructor name arity) arms
    isGeneralDataConstructor name arity pattern =
      case pattern of
        PDInductivePat actual fields ->
          actual == name && length fields == arity && all pdIrrefutable fields
        _ -> False

    mentionsCollection = CollectionDataRoot `elem` mentionedRoots
    collectionCovered =
      any isEmptyArm arms && any completeUncons arms
    isEmptyArm PDEmptyPat = True
    isEmptyArm _          = False
    completeUncons (PDConsPat p1 p2) = pdIrrefutable p1 && pdIrrefutable p2
    completeUncons (PDSnocPat p1 p2) = pdIrrefutable p1 && pdIrrefutable p2
    completeUncons _                 = False

    mentionsBool = BoolDataRoot `elem` mentionedRoots
    boolCovered =
      any (isBoolArm True) arms && any (isBoolArm False) arms
    isBoolArm b (PDConstantPat (BoolExpr b')) = b == b'
    isBoolArm _ _                             = False

-- | TypePM's irrefutable arm headers are exactly a variable or wildcard.
pdIrrefutable :: IPrimitiveDataPattern -> Bool
pdIrrefutable PDWildCard      = True
pdIrrefutable (PDPatVar _)    = True
pdIrrefutable _               = False

applyMatchClauseSubst :: Subst -> TIMatchClause -> Infer TIMatchClause
applyMatchClauseSubst subst (pattern', body) =
  (,) <$> applySubstToTIPatternM subst pattern'
      <*> applySubstToTIExprM subst body

-- | The result of the pattern phase of one match arm.  Keeping it separate
-- lets T-MATCHALL infer its first pattern before synthesizing the matcher.
data InferredMatchPattern = InferredMatchPattern
  { matchPatternSource :: IPattern
  , matchPatternTyped :: TIPattern
  , matchPatternBindings :: [(String, Type)]
  , matchPatternCapability :: Capability
  , matchPatternBody :: IExpr
  , matchPatternSubst :: Subst
  }

inferMatchPattern
  :: TypeErrorContext
  -> Type
  -> IMatchClause
  -> Subst
  -> Infer InferredMatchPattern
inferMatchPattern ctx matchedType (pattern, bodyExpr) initSubst = do
  warnPatternCompatibility ctx pattern
  matchedType' <- applySubstWithConstraintsM initSubst matchedType
  (typedPattern, bindings, patternSubst, capability) <-
    inferIPattern pattern matchedType' ctx
  return InferredMatchPattern
    { matchPatternSource = pattern
    , matchPatternTyped = typedPattern
    , matchPatternBindings = bindings
    , matchPatternCapability = capability
    , matchPatternBody = bodyExpr
    , matchPatternSubst = composeSubst patternSubst initSubst
    }

finishMatchClause
  :: TypeErrorContext
  -> IExpr
  -> TIExpr
  -> Type
  -> Type
  -> Type
  -> InferredMatchPattern
  -> Subst
  -> Infer (TIMatchClause, Type, Subst)
finishMatchClause
    ctx matcherSource matcherTyped matcherType matchedType commonResult
    inferredPattern initSubst = do
  checkVpScope
    ctx matcherSource
    [(matchPatternSource inferredPattern, matchPatternBody inferredPattern)]
  patternSubst <-
    checkMatcherAtPattern
      ctx matcherSource matcherTyped matcherType matchedType
      (matchPatternCapability inferredPattern) initSubst
  bindings <-
    mapM
      (\(name, ty) -> do
        ty' <- applySubstWithConstraintsM patternSubst ty
        return (name, Forall [] [] [] ty'))
      (matchPatternBindings inferredPattern)
  (bodyTI, s2) <-
    withEnv bindings $
      inferIExprWithContext (matchPatternBody inferredPattern) ctx
  let preResultSubst = composeSubst s2 patternSubst
  bodyType <-
    applySubstWithConstraintsM preResultSubst (tiExprType bodyTI)
  expectedType <-
    applySubstWithConstraintsM preResultSubst commonResult
  resultSubst <- unifyTypesWithContext expectedType bodyType ctx
  let finalSubst = composeSubst resultSubst preResultSubst
  finalResult <- applySubstWithConstraintsM finalSubst commonResult
  return
    ( (matchPatternTyped inferredPattern, bodyTI)
    , finalResult
    , finalSubst
    )

inferNextMatchClause
  :: TypeErrorContext
  -> IExpr
  -> TIExpr
  -> Type
  -> Type
  -> (Type, [TIMatchClause], Subst)
  -> IMatchClause
  -> Infer (Type, [TIMatchClause], Subst)
inferNextMatchClause
    ctx matcherSource matcherTyped matcherType matchedType
    (commonResult, reversedClauses, initSubst) clause = do
  inferredPattern <- inferMatchPattern ctx matchedType clause initSubst
  (typedClause, finalResult, finalSubst) <-
    finishMatchClause
      ctx matcherSource matcherTyped matcherType matchedType commonResult
      inferredPattern (matchPatternSubst inferredPattern)
  return (finalResult, typedClause : reversedClauses, finalSubst)

inferMatchClausesWithMatcher
  :: TypeErrorContext
  -> IExpr
  -> TIExpr
  -> Type
  -> Type
  -> Type
  -> [IMatchClause]
  -> Subst
  -> Infer (Type, [TIMatchClause], Subst)
inferMatchClausesWithMatcher
    ctx matcherSource matcherTyped matcherType matchedType commonResult
    clauses initSubst =
  case clauses of
    [] -> return (commonResult, [], initSubst)
    firstClause : restClauses -> do
      inferredPattern <-
        inferMatchPattern ctx matchedType firstClause initSubst
      (firstTyped, firstResult, firstSubst) <-
        finishMatchClause
          ctx matcherSource matcherTyped matcherType matchedType commonResult
          inferredPattern (matchPatternSubst inferredPattern)
      (finalResult, reversedClauses, finalSubst) <-
        foldM
          (inferNextMatchClause
            ctx matcherSource matcherTyped matcherType matchedType)
          (firstResult, [firstTyped], firstSubst)
          restClauses
      typedClauses <-
        mapM (applyMatchClauseSubst finalSubst) (reverse reversedClauses)
      return (finalResult, typedClauses, finalSubst)

-- | Infer multiple patterns left-to-right, making left bindings available to right patterns
-- This enables non-linear patterns like ($p, #(p + 1))
-- Returns (list of TIPattern, accumulated bindings, substitution)
-- The final @[Capability]@ component is the sub-patterns' structural demands,
-- in order, used by the parent constructor/tuple to derive its own capability.
inferPatternsLeftToRight :: [IPattern] -> [Type] -> [(String, Type)] -> Subst -> TypeErrorContext
                         -> Infer ([TIPattern], [(String, Type)], Subst, [Capability])
inferPatternsLeftToRight [] [] accBindings accSubst _ctx =
  return ([], accBindings, accSubst, [])
inferPatternsLeftToRight (p:ps) (t:ts) accBindings accSubst ctx = do
  -- Add accumulated bindings to environment for this pattern
  let schemes = [(var, Forall [] [] [] ty) | (var, ty) <- accBindings]

  -- Infer this pattern with left bindings in scope
  t' <- applySubstWithConstraintsM accSubst t
  (tipat, newBindings, s, capability) <-
    withEnv schemes $
      inferIPattern p t' ctx

  -- Compose substitutions
  let accSubst' = composeSubst s accSubst

  -- Apply substitution to accumulated bindings
  accBindings'' <- mapM (\(v, ty) -> do
      ty' <- applySubstWithConstraintsM s ty
      return (v, ty')) accBindings
  let accBindings' = accBindings'' ++ newBindings

  -- Continue with remaining patterns
  (restTipats, finalBindings, finalSubst, restCapabilities) <- inferPatternsLeftToRight ps ts accBindings' accSubst' ctx
  return (tipat : restTipats, finalBindings, finalSubst, capability : restCapabilities)
inferPatternsLeftToRight _ _ accBindings accSubst _ =
  return ([], accBindings, accSubst, [])  -- Mismatched lengths

-- | Type a syntactically named pattern-function application through the
-- mechanized PAT-APP path.  The caller has already resolved the surface
-- 'IInductiveOrPApplyPat' node against the pattern-function namespace, so a
-- lexically shadowing expression variable cannot accidentally select this
-- branch.
inferNamedPatternFunctionApplication
  :: String
  -> DualScheme
  -> [IPattern]
  -> Type
  -> TypeErrorContext
  -> Infer (TIPattern, [(String, Type)], Subst, Capability)
inferNamedPatternFunctionApplication
  functionName scheme argPats expectedType ctx = do
    -- Instantiate the complete scheme once so capability and target images
    -- remain correlated across every argument and the result.
    (expectedArguments, resultDual) <-
      instantiateDualSchemeForPatternApplication scheme
    let expectedArity = length expectedArguments
        actualArity = length argPats
        functionType =
          foldr TFun
            (dualTarget resultDual)
            (map dualTarget expectedArguments)
    when (expectedArity /= actualArity) $
      throwError $ TE.TypeMismatch
        functionType
        functionType
        ("Pattern function " ++ functionName ++ " expects " ++
         show expectedArity ++ " arguments, but got " ++
         show actualArity)
        ctx

    resultSubst <-
      unifyTypesWithContext
        (dualTarget resultDual)
        expectedType
        ctx
    argumentTargets <-
      mapM
        (applySubstWithConstraintsM resultSubst . dualTarget)
        expectedArguments
    (typedArguments, allBindings, patternSubst, actualCapabilities) <-
      inferPatternsLeftToRight
        argPats argumentTargets [] resultSubst ctx
    capabilitySubst <-
      foldM
        (\acc (actual, expectedArgument) -> do
          actual' <- applyCapabilityM acc actual
          expected' <-
            applyCapabilityM acc (dualCapability expectedArgument)
          current <- alignPatternCapabilities ctx actual' expected'
          return (composeSubst current acc))
        emptySubst
        (zip actualCapabilities expectedArguments)
    let finalSubst = composeSubst capabilitySubst patternSubst
        functionTI0 =
          TIExpr
            (Forall [] [] [] functionType)
            (TIVarExpr functionName)
    functionTI <- applySubstToTIExprM finalSubst functionTI0
    typedArguments' <-
      mapM (applySubstToTIPatternM finalSubst) typedArguments
    finalBindings <-
      mapM
        (\(name, ty) -> do
          ty' <- applySubstWithConstraintsM finalSubst ty
          return (name, ty'))
        allBindings
    finalType <-
      applySubstWithConstraintsM finalSubst expectedType
    finalCapability <-
      applyCapabilityM finalSubst (dualCapability resultDual)
    let typedPattern =
          TIPattern
            (Forall [] [] [] finalType)
            (TIPApplyPat functionTI typedArguments')
    return
      (typedPattern, finalBindings, finalSubst, finalCapability)

-- | Preserve Egison's target-only application path at an explicit extension
-- boundary.  This helper always infers the head as an expression, including a
-- variable head, so ordinary lexical shadowing is respected.
inferTargetOnlyPatternApplication
  :: String
  -> IExpr
  -> [IPattern]
  -> Type
  -> TypeErrorContext
  -> Infer (TIPattern, [(String, Type)], Subst, Capability)
inferTargetOnlyPatternApplication
  detail funcExpr argPats expectedType ctx = do
    warnOutsideEgisonCore detail ctx
    (functionTI, functionSubst) <-
      inferIExprWithContext funcExpr ctx
    argumentTypes <- mapM (const (freshVar "parg")) argPats
    functionType' <-
      applySubstWithConstraintsM functionSubst (tiExprType functionTI)
    expectedType' <-
      applySubstWithConstraintsM functionSubst expectedType
    applicationSubst <-
      unifyTypesWithContext
        functionType'
        (foldr TFun expectedType' argumentTypes)
        ctx
    let initialSubst = composeSubst applicationSubst functionSubst
    argumentTypes' <-
      mapM (applySubstWithConstraintsM initialSubst) argumentTypes
    (typedArguments, allBindings, finalSubst, _) <-
      inferPatternsLeftToRight
        argPats argumentTypes' [] initialSubst ctx
    finalBindings <-
      mapM
        (\(name, ty) -> do
          ty' <- applySubstWithConstraintsM finalSubst ty
          return (name, ty'))
        allBindings
    finalType <-
      applySubstWithConstraintsM finalSubst expectedType
    functionTI' <- applySubstToTIExprM finalSubst functionTI
    typedArguments' <-
      mapM (applySubstToTIPatternM finalSubst) typedArguments
    let typedPattern =
          TIPattern
            (Forall [] [] [] finalType)
            (TIPApplyPat functionTI' typedArguments')
    capability <- freshCapability "patternApplication"
    return (typedPattern, finalBindings, finalSubst, capability)

-- | Infer an IPattern's types and extract its pattern-variable bindings.  The
-- fourth component is the capability-sort half of the paper's dual judgment
-- @Γ;Δ ⊢ p : Pattern κ ▷ τ ; Δ'@.
--   * τ_t (the *target* type) and Δ' (bindings) are computed exactly as before — coherently,
--     top-down, threading one substitution (τ_t is read from the TIPattern / @expectedType@);
--   * κ is built in the capability sort with a fresh variable at every
--     variable, wildcard, and value/predicate leaf.
-- Keeping κ's variables disjoint from τ leaves matcher demand independent
-- from ordinary target specialization.
inferIPattern :: IPattern -> Type -> TypeErrorContext -> Infer (TIPattern, [(String, Type)], Subst, Capability)
inferIPattern pat expectedType ctx = case pat of
  IWildCard -> do
    -- Wildcard: no bindings and a fresh unconstraining capability.
    let tipat = TIPattern (Forall [] [] [] expectedType) TIWildCard
    capability <- freshCapability "pattern"
    return (tipat, [], emptySubst, capability)

  IPatVar name -> do
    -- Pattern variable: bind the target and leave capability unconstrained.
    let tipat = TIPattern (Forall [] [] [] expectedType) (TIPatVar name)
    capability <- freshCapability "pattern"
    return (tipat, [(name, expectedType)], emptySubst, capability)

  IValuePat expr -> do
    -- Value pattern: infer the expression target and unify it with the expected
    -- target.  Its fresh capability imposes no fixed structural duty.
    (exprTI, s) <- inferIExprWithContext expr ctx
    let exprType = tiExprType exprTI
    exprType' <- applySubstWithConstraintsM s exprType
    expectedType' <- applySubstWithConstraintsM s expectedType
    s' <- unifyTypesWithContext exprType' expectedType' ctx
    let finalS = composeSubst s' s
    exprTI' <- applySubstToTIExprM finalS exprTI
    finalType <- applySubstWithConstraintsM finalS expectedType
    let tipat = TIPattern (Forall [] [] [] finalType) (TIValuePat exprTI')
    capability <- freshCapability "pattern"
    return (tipat, [], finalS, capability)

  IPredPat expr -> do
    -- Predicate pattern: infer the predicate expression.  Its fresh capability
    -- imposes no fixed structural duty.
    let predicateType = TFun expectedType TBool
    (exprTI, s) <- inferIExprWithContext expr ctx
    -- Unify with expected predicate type to concretize type variables
    exprType' <- applySubstWithConstraintsM s (tiExprType exprTI)
    predicateType' <- applySubstWithConstraintsM s predicateType
    s' <- unifyTypesWithContext exprType' predicateType' ctx
    let finalS = composeSubst s' s
    exprTI' <- applySubstToTIExprM finalS exprTI
    finalType <- applySubstWithConstraintsM finalS expectedType
    let tipat = TIPattern (Forall [] [] [] finalType) (TIPredPat exprTI')
    capability <- freshCapability "pattern"
    return (tipat, [], finalS, capability)
  
  ITuplePat pats -> do
    -- Tuple pattern: decompose expected type
    case expectedType of
      TTuple types | length types == length pats -> do
        -- Types match: infer each sub-pattern left-to-right
        -- Left patterns' bindings are available for right patterns (for non-linear patterns)
        (tipats, allBindings, s, childCapabilities) <- inferPatternsLeftToRight pats types [] emptySubst ctx
        finalType <- applySubstWithConstraintsM s expectedType
        let tipat = TIPattern (Forall [] [] [] finalType) (TITuplePat tipats)
        return (tipat, allBindings, s, CapTuple childCapabilities)

      TVar _ -> do
        -- Expected type is a type variable: create tuple type
        elemTypes <- mapM (\_ -> freshVar "elem") pats
        let tupleTy = TTuple elemTypes
        s <- unifyTypesWithContext expectedType tupleTy ctx

        -- Recursively infer each sub-pattern left-to-right
        elemTypes' <- mapM (applySubstWithConstraintsM s) elemTypes
        (tipats, allBindings, s', childCapabilities) <- inferPatternsLeftToRight pats elemTypes' [] s ctx
        finalType <- applySubstWithConstraintsM s' expectedType
        let tipat = TIPattern (Forall [] [] [] finalType) (TITuplePat tipats)
        return (tipat, allBindings, s', CapTuple childCapabilities)
      
      _ -> do
        -- Type mismatch
        throwError $ TE.TypeMismatch
          (TTuple (replicate (length pats) (TVar (TyVar "a"))))
          expectedType
          "Tuple pattern but matched type is not a tuple"
          ctx
  
  IInductivePat name pats -> do
    -- Inductive pattern: look up pattern constructor type from pattern environment
    patternEnv <- getPatternEnv
    case lookupPatternEnv name patternEnv of
      Just scheme -> do
        -- Found in pattern environment: use the declared type
        (_constraints, ctorType) <- instantiateSchemeInState scheme
        
        -- Pattern constructor type: arg1 -> arg2 -> ... -> resultType
        let (argTypes, resultType) = extractFunctionArgs ctorType
        
        -- Check argument count matches
        if length argTypes /= length pats
          then throwError $ TE.TypeMismatch
                 (foldr TFun resultType (replicate (length pats) (TVar (TyVar "a"))))
                 ctorType
                 ("Pattern constructor " ++ name ++ " expects " ++ show (length argTypes) 
                  ++ " arguments, but got " ++ show (length pats))
                 ctx
          else do
            -- Unify result type with expected type
            s0 <- unifyTypesWithContext resultType expectedType ctx
            argTypes' <- mapM (applySubstWithConstraintsM s0) argTypes

            -- Recursively infer each sub-pattern left-to-right
            -- Left patterns' bindings are available for right patterns
            (tipats, allBindings, s, childCapabilities) <-
              inferPatternsLeftToRight pats argTypes' [] s0 ctx
            finalType <- applySubstWithConstraintsM s expectedType
            -- Derive the constructor capability from a fresh structural
            -- projection, independently of target specialization.
            (_csP, ctorTypeP) <- instantiateSchemeInState scheme
            let (argTypesP, resultTypeP) = extractFunctionArgs ctorTypeP
            capability <-
              capabilityFromCtor ctx argTypesP resultTypeP childCapabilities
            let tipat = TIPattern (Forall [] [] [] finalType) (TIInductivePat name tipats)
            return (tipat, allBindings, s, capability)
      
      Nothing -> do
        -- Not found in pattern environment: try data constructor from value environment
        -- This handles data constructors used as patterns
        env <- getEnv
        case lookupEnv (stringToVar name) env of
          Just scheme -> do
            (_constraints, ctorType) <- instantiateSchemeInState scheme
            
            let (argTypes, resultType) = extractFunctionArgs ctorType
            
            if length argTypes /= length pats
              then throwError $ TE.TypeMismatch
                     (foldr TFun resultType (replicate (length pats) (TVar (TyVar "a"))))
                     ctorType
                     ("Constructor " ++ name ++ " expects " ++ show (length argTypes) 
                      ++ " arguments, but got " ++ show (length pats))
                     ctx
              else do
                s0 <- unifyTypesWithContext resultType expectedType ctx
                argTypes' <- mapM (applySubstWithConstraintsM s0) argTypes

                -- Recursively infer each sub-pattern left-to-right
                (tipats, allBindings, s, childCapabilities) <-
                  inferPatternsLeftToRight pats argTypes' [] s0 ctx
                finalType <- applySubstWithConstraintsM s expectedType
                -- Derive the constructor capability from a fresh structural
                -- projection, independently of target specialization.
                (_csP, ctorTypeP) <- instantiateSchemeInState scheme
                let (argTypesP, resultTypeP) = extractFunctionArgs ctorTypeP
                capability <-
                  capabilityFromCtor ctx argTypesP resultTypeP childCapabilities
                let tipat = TIPattern (Forall [] [] [] finalType) (TIInductivePat name tipats)
                return (tipat, allBindings, s, capability)

          Nothing -> do
            -- Not found: generic inference
            argTypes <- mapM (\_ -> freshVar "arg") pats
            let resultType = TInductive name argTypes

            s0 <- unifyTypesWithContext resultType expectedType ctx
            argTypes' <- mapM (applySubstWithConstraintsM s0) argTypes

            -- Recursively infer each sub-pattern left-to-right
            (tipats, allBindings, s, childCapabilities) <-
              inferPatternsLeftToRight pats argTypes' [] s0 ctx
            finalType <- applySubstWithConstraintsM s expectedType
            -- An undeclared constructor remains an Egison extension, but its
            -- structural demand still stays in the capability sort.
            let tipat = TIPattern (Forall [] [] [] finalType) (TIInductivePat name tipats)
                capability =
                  CapCon (mkTypeFormer name (length childCapabilities))
                    childCapabilities
            return (tipat, allBindings, s, capability)
  
  IIndexedPat p indices -> do
    -- Indexed pattern: infer base pattern and index expressions
    -- For $x_i pattern, x should have type Hash keyType expectedType
    -- where expectedType is the type of the indexed result
    
    -- First, infer the index expressions to determine their types
    indexTypes <- mapM (\_ -> freshVar "idx") indices
    (indexTIs, s1) <- foldM (\(accTIs, accS) (idx, idxType) -> do
      (idxTI, idxS) <- inferIExprWithContext idx ctx
      let actualIdxType = tiExprType idxTI
      actualIdxType' <- applySubstWithConstraintsM idxS actualIdxType
      idxType' <- applySubstWithConstraintsM idxS idxType
      s' <- unifyTypesWithContext actualIdxType' idxType' ctx
      let finalS = composeSubst s' (composeSubst idxS accS)
      return (accTIs ++ [idxTI], finalS)) ([], emptySubst) (zip indices indexTypes)

    -- Construct the base type: Hash indexType expectedType
    -- For simplicity, assume single index access and use THash
    indexType <- case indexTypes of
                   [t] -> applySubstWithConstraintsM s1 t
                   _ -> return TInt  -- Multiple indices: fallback to Int
    let baseType = THash indexType expectedType

    -- Infer base pattern with Hash type
    baseType' <- applySubstWithConstraintsM s1 baseType
    (tipat, bindings, s2, _) <- inferIPattern p baseType' ctx

    let finalS = composeSubst s2 s1
    finalType <- applySubstWithConstraintsM finalS expectedType
    let tiIndexedPat = TIPattern (Forall [] [] [] finalType) (TIIndexedPat tipat indexTIs)
    -- An indexed access is variable-like and contributes no fixed structure.
    capability <- freshCapability "pattern"
    return (tiIndexedPat, bindings, finalS, capability)
  
  ILetPat bindings p -> do
    -- Let pattern: infer bindings and then the pattern
    -- Infer bindings first
    env <- getEnv
    (bindingTIs, bindingSchemes, s1) <- inferIBindingsWithContext bindings env emptySubst ctx

    -- Infer pattern with bindings in scope
    expectedType' <- applySubstWithConstraintsM s1 expectedType
    (tipat, patBindings, s2, innerCapability) <-
      withEnv bindingSchemes $
        inferIPattern p expectedType' ctx

    let s = composeSubst s2 s1
    finalType <- applySubstWithConstraintsM s expectedType
    let tiLetPat = TIPattern (Forall [] [] [] finalType) (TILetPat bindingTIs tipat)
    -- Let bindings are not exported; the inner pattern carries the demand.
    return (tiLetPat, patBindings, s, innerCapability)

  INotPat p -> do
    -- Not pattern: infer the sub-pattern but do not export its bindings.
    (tipat, _, s, innerCapability) <- inferIPattern p expectedType ctx
    finalType <- applySubstWithConstraintsM s expectedType
    let tiNotPat = TIPattern (Forall [] [] [] finalType) (TINotPat tipat)
    return (tiNotPat, [], s, innerCapability)
  
  IAndPat p1 p2 -> do
    -- And pattern: both patterns must match the same type
    -- Left bindings should be available to right pattern
    (tipat1, bindings1, s1, capability1) <- inferIPattern p1 expectedType ctx
    let schemes1 = [(var, Forall [] [] [] ty) | (var, ty) <- bindings1]
    expectedType' <- applySubstWithConstraintsM s1 expectedType
    (tipat2, bindings2, s2, capability2) <-
      withEnv schemes1 $
        inferIPattern p2 expectedType' ctx
    let s = composeSubst s2 s1
    -- Apply substitution to left bindings
    bindings1'' <- mapM (\(v, ty) -> do
        ty' <- applySubstWithConstraintsM s2 ty
        return (v, ty')) bindings1
    finalType <- applySubstWithConstraintsM s expectedType
    capability <- capabilityCombine ctx capability1 capability2
    let bindings1' = bindings1''
        tiAndPat = TIPattern (Forall [] [] [] finalType) (TIAndPat tipat1 tipat2)
    return (tiAndPat, bindings1' ++ bindings2, s, capability)
  
  IOrPat p1 p2 -> do
    -- Or pattern (paper PAT-OR): the two branches are alternatives over the same input
    -- context, so they are typed independently and must produce the SAME output bindings Δ'
    -- — the same variable names, at unifiable types.
    (tipat1, bindings1, s1, capability1) <- inferIPattern p1 expectedType ctx
    expectedType' <- applySubstWithConstraintsM s1 expectedType
    (tipat2, bindings2, s2, capability2) <-
      inferIPattern p2 expectedType' ctx
    let s12 = composeSubst s2 s1
        vars1 = nub (map fst bindings1)
        vars2 = nub (map fst bindings2)
        sameVars = all (`elem` vars2) vars1 && all (`elem` vars1) vars2
    if not sameVars
      then throwError $ TE.TypeMismatch
             (TTuple (map snd bindings1))
             (TTuple (map snd bindings2))
             ("or-pattern (`|`) branches must bind the same variables, but the left binds {"
               ++ intercalate ", " vars1 ++ "} and the right binds {"
               ++ intercalate ", " vars2 ++ "}")
             ctx
      else do
        -- Unify the type of each shared variable across the two branches.
        sVars <- foldM (\accS (v, ty1) ->
            case lookup v bindings2 of
              Just ty2 -> do
                ty1' <- applySubstWithConstraintsM accS ty1
                ty2' <- applySubstWithConstraintsM accS ty2
                s' <- unifyTypesWithContext ty1' ty2' ctx
                return (composeSubst s' accS)
              Nothing -> return accS
          ) s12 bindings1
        finalBindings <- mapM (\(v, ty) -> do
            ty' <- applySubstWithConstraintsM sVars ty
            return (v, ty')) bindings1
        finalType <- applySubstWithConstraintsM sVars expectedType
        let tiOrPat = TIPattern (Forall [] [] [] finalType) (TIOrPat tipat1 tipat2)
        capability <- capabilityCombine ctx capability1 capability2
        return (tiOrPat, finalBindings, sVars, capability)
  
  IForallPat p1 p2 -> do
    -- Forall [] pattern: similar to and pattern
    -- Left bindings should be available to right pattern
    (tipat1, bindings1, s1, capability1) <- inferIPattern p1 expectedType ctx
    let schemes1 = [(var, Forall [] [] [] ty) | (var, ty) <- bindings1]
    expectedType' <- applySubstWithConstraintsM s1 expectedType
    (tipat2, bindings2, s2, capability2) <-
      withEnv schemes1 $
        inferIPattern p2 expectedType' ctx
    let s = composeSubst s2 s1
    -- Apply substitution to left bindings
    bindings1'' <- mapM (\(v, ty) -> do
        ty' <- applySubstWithConstraintsM s2 ty
        return (v, ty')) bindings1
    finalType <- applySubstWithConstraintsM s expectedType
    capability <- capabilityCombine ctx capability1 capability2
    let bindings1' = bindings1''
        tiForallPat = TIPattern (Forall [] [] [] finalType) (TIForallPat tipat1 tipat2)
    return (tiForallPat, bindings1' ++ bindings2, s, capability)
  
  ILoopPat var range p1 p2 -> do
    -- Loop pattern: $var is the loop variable (Integer), range contains pattern
    -- First, infer the range pattern (third element of ILoopRange)
    let ILoopRange startExpr endExpr rangePattern = range
    (tiRangePat, rangeBindings, s_range, _) <- inferIPattern rangePattern TInt ctx
    
    -- Infer start and end expressions
    (startTI, s_start) <- inferIExprWithContext startExpr ctx
    (endTI, s_end) <- inferIExprWithContext endExpr ctx
    let tiLoopRange = TILoopRange startTI endTI tiRangePat
    
    -- Add loop variable binding (always Integer for loop index)
    let loopVarBinding = (var, TInt)
        initialBindings = loopVarBinding : rangeBindings
        schemes0 = [(v, Forall [] [] [] ty) | (v, ty) <- initialBindings]
        s_combined = foldr composeSubst emptySubst [s_end, s_start, s_range]

    -- Infer p1 with loop variable and range bindings in scope
    expectedType1 <- applySubstWithConstraintsM s_combined expectedType
    (tipat1, bindings1, s1, capability1) <-
      withEnv schemes0 $
        inferIPattern p1 expectedType1 ctx

    -- Infer p2 with all previous bindings in scope
    allPrevBindings' <- mapM (\(v, ty) -> do
        ty' <- applySubstWithConstraintsM s1 ty
        return (v, ty')) initialBindings
    let allPrevBindings = allPrevBindings' ++ bindings1
        schemes1 = [(v, Forall [] [] [] ty) | (v, ty) <- allPrevBindings]
    expectedType2 <- applySubstWithConstraintsM s1 expectedType
    (tipat2, bindings2, s2, capability2) <-
      withEnv schemes1 $
        inferIPattern p2 expectedType2 ctx

    let s = foldr composeSubst emptySubst [s2, s1, s_combined]
    -- Apply final substitution to all bindings
    finalBindings' <- mapM (\(v, ty) -> do
        ty' <- applySubstWithConstraintsM s ty
        return (v, ty')) (loopVarBinding : rangeBindings ++ bindings1 ++ bindings2)
    finalType <- applySubstWithConstraintsM s expectedType
    capability <- capabilityCombine ctx capability1 capability2
    let finalBindings = finalBindings'
        tiLoopPat = TIPattern (Forall [] [] [] finalType) (TILoopPat var tiLoopRange tipat1 tipat2)

    return (tiLoopPat, finalBindings, s, capability)

  IContPat -> do
    -- Continuation pattern: no bindings
    let tipat = TIPattern (Forall [] [] [] expectedType) TIContPat
    capability <- freshCapability "pattern"
    return (tipat, [], emptySubst, capability)
  
  IPApplyPat funcExpr argPats ->
    -- Explicit PApply syntax is expression-headed even when its head happens
    -- to be a variable with the same spelling as a top-level pattern
    -- function.  Infer that expression normally so lexical shadowing wins.
    inferTargetOnlyPatternApplication
      "expression-headed pattern application has no directly mechanized DualScheme dispatch"
      funcExpr argPats expectedType ctx

  IVarPat name -> do
    -- A ~parameter carries the complete dual assigned by PATFUN-DEF.  In
    -- particular, its declared target is checked here instead of being
    -- silently replaced by the body's expected target.
    parameterDuals <- inferPatfunParamDuals <$> get
    case Map.lookup name parameterDuals of
      Just parameterDual -> do
        substitution <-
          unifyTypesWithContext
            (dualTarget parameterDual)
            expectedType
            ctx
        finalType <-
          applySubstWithConstraintsM substitution expectedType
        capability <-
          applyCapabilityM substitution (dualCapability parameterDual)
        let typedPattern =
              TIPattern
                (Forall [] [] [] finalType)
                (TIVarPat name)
        return
          (typedPattern, [], substitution, capability)
      Nothing -> do
        capability <- freshCapability "pattern"
        let typedPattern =
              TIPattern
                (Forall [] [] [] expectedType)
                (TIVarPat name)
        return
          (typedPattern, [(name, expectedType)], emptySubst, capability)
  
  IInductiveOrPApplyPat name pats -> do
    -- Could be either inductive pattern or pattern application
    -- Check pattern function environment to distinguish
    -- Pattern functions are ONLY in patternFuncEnv, pattern constructors are NOT
    patternFunctionEnv <- getPatternFuncEnv
    patternFunctionDeclEnv <- getPatternFuncDeclEnv
    case ( lookupPatternFunctionEnv name patternFunctionEnv
         , lookupPatternEnv name patternFunctionDeclEnv
         ) of
      (Just scheme, _) ->
        -- The resolved named surface form is the unambiguous PAT-APP path.
        inferNamedPatternFunctionApplication
          name scheme pats expectedType ctx
      (Nothing, Just _) ->
        -- A forward or mutually recursive header is a target-only extension.
        inferTargetOnlyPatternApplication
          ("pattern-function application `" ++ name ++
           "` uses only a header because its DualScheme is not finalized")
          (IVarExpr name) pats expectedType ctx
      (Nothing, Nothing) -> do
        -- It's an inductive pattern constructor (or not found, will be handled later)
        (tipat, bindings, s, capability) <-
          inferIPattern (IInductivePat name pats) expectedType ctx
        -- Wrap it as InductiveOrPApplyPat (if it's actually an inductive pattern)
        case tipPatternNode tipat of
          TIInductivePat _ tipats -> do
            let scheme = tipScheme tipat
                tiInductiveOrPApplyPat = TIPattern scheme (TIInductiveOrPApplyPat name tipats)
            return (tiInductiveOrPApplyPat, bindings, s, capability)
          _ ->
            -- Not an inductive pattern (e.g., already processed as pattern application)
            return (tipat, bindings, s, capability)
  
  ISeqNilPat -> do
    -- Sequence nil: no bindings
    let tipat = TIPattern (Forall [] [] [] expectedType) TISeqNilPat
    capability <- freshCapability "pattern"
    return (tipat, [], emptySubst, capability)

  ISeqConsPat p1 p2 -> do
    -- Sequence cons: infer both patterns
    -- Left bindings should be available to right pattern
    (tipat1, bindings1, s1, capability1) <-
      inferIPattern p1 expectedType ctx
    let schemes1 = [(var, Forall [] [] [] ty) | (var, ty) <- bindings1]
    expectedType' <- applySubstWithConstraintsM s1 expectedType
    (tipat2, bindings2, s2, capability2) <-
      withEnv schemes1 $
        inferIPattern p2 expectedType' ctx
    let s = composeSubst s2 s1
    -- Apply substitution to left bindings
    bindings1'' <- mapM (\(v, ty) -> do
        ty' <- applySubstWithConstraintsM s2 ty
        return (v, ty')) bindings1
    finalType <- applySubstWithConstraintsM s expectedType
    capability <- capabilityCombine ctx capability1 capability2
    let bindings1' = bindings1''
        tipat = TIPattern (Forall [] [] [] finalType) (TISeqConsPat tipat1 tipat2)
    return (tipat, bindings1' ++ bindings2, s, capability)

  ILaterPatVar -> do
    -- Later pattern variable: no immediate binding
    let tipat = TIPattern (Forall [] [] [] expectedType) TILaterPatVar
    capability <- freshCapability "pattern"
    return (tipat, [], emptySubst, capability)
  
  IDApplyPat p pats -> do
    -- D-apply pattern: infer base pattern and argument patterns
    -- Base pattern bindings should be available to argument patterns
    (tipat, bindings1, s1, baseCapability) <-
      inferIPattern p expectedType ctx

    -- Infer argument patterns left-to-right with base pattern bindings in scope
    argTypes <- mapM (\_ -> freshVar "darg") pats
    let schemes1 = [(var, Forall [] [] [] ty) | (var, ty) <- bindings1]
    (tipats, argBindings, s2, _) <-
      withEnv schemes1 $
        inferPatternsLeftToRight pats argTypes [] s1 ctx

    let s = composeSubst s2 s1
    -- Apply substitution to base bindings
    bindings1'' <- mapM (\(v, ty) -> do
        ty' <- applySubstWithConstraintsM s2 ty
        return (v, ty')) bindings1
    finalType <- applySubstWithConstraintsM s expectedType
    let bindings1' = bindings1''
        tiDApplyPat = TIPattern (Forall [] [] [] finalType) (TIDApplyPat tipat tipats)
    return (tiDApplyPat, bindings1' ++ argBindings, s, baseCapability)
  where
    -- Extract function argument types and result type
    -- e.g., a -> b -> c -> d  =>  ([a, b, c], d)
    extractFunctionArgs :: Type -> ([Type], Type)
    extractFunctionArgs (TFun arg rest) = 
      let (args, result) = extractFunctionArgs rest
      in (arg : args, result)
    extractFunctionArgs t = ([], t)

-- | Infer application (helper)
-- NEW: Returns TIExpr instead of (IExpr, Type, Subst)
inferIApplication :: String -> Type -> [IExpr] -> Subst -> Infer (TIExpr, Subst)
inferIApplication funcName funcType args initSubst = do
  let funcTI = mkTIExpr funcType (TIVarExpr funcName)
  inferIApplicationWithContext funcTI funcType args initSubst emptyContext

-- | Check one already-synthesized application argument against its expected
-- type by ordinary equality.
solveApplicationArgument
  :: ClassEnv
  -> [Constraint]
  -> IExpr
  -> TIExpr
  -> Type
  -> Type
  -> TypeErrorContext
  -> Infer (Subst, Bool)
solveApplicationArgument classEnv constraints _source _typed inferred expected ctx =
  solveTypes classEnv constraints inferred expected ctx

-- TensorMap insertion logic has been moved to Language.Egison.Type.TensorMapInsertion
-- This keeps type inference focused on type checking only

-- | Infer application (helper) with context
-- NEW: Returns TIExpr instead of (IExpr, Type, Subst)
-- TensorMap insertion has been moved to Phase 7 (TensorMapInsertion module)
-- This function now only performs type inference and unification
-- When a Tensor argument is passed to a scalar parameter, the result type is wrapped in Tensor
--
-- The ordinary path fixes the function spine first and closes arguments in
-- source order.  CAS joining remains a separately marked extension retry.
inferIApplicationWithContext :: TIExpr -> Type -> [IExpr] -> Subst -> TypeErrorContext -> Infer (TIExpr, Subst)
inferIApplicationWithContext funcTIExpr funcType args initSubst ctx = do
  snapshot <- saveConstraintState
  inferIApplicationSequential funcTIExpr funcType args initSubst ctx
    `catchError` \e -> case e of
      UnificationError t1 t2 _
        | Subtype.isCasType t1, Subtype.isCasType t2 -> do
            edges <- gets inferCasSubtypeEdges
            case Subtype.joinTypesWith edges t1 t2 of
              Just j -> do
                restoreConstraintState snapshot
                argResults <-
                  mapM (\arg -> inferIExprWithContext arg ctx) args
                let argTIExprs = map fst argResults
                    argTypes = map (tiExprType . fst) argResults
                    argSubst =
                      foldr composeSubst initSubst (map snd argResults)
                let promote ti at
                      | Subtype.isCasType at, at /= j, Subtype.isSubtypeWith edges at j =
                          (TIExpr (Forall [] [] [] j) (TIReshape j ti), j)
                      | otherwise = (ti, at)
                    (argTIExprs', argTypes') = unzip (zipWith promote argTIExprs argTypes)
                inferIApplicationUnifyPhase funcTIExpr funcType args argTIExprs' argTypes' argSubst ctx
                  `catchError` \_ -> do
                    restoreConstraintState snapshot
                    throwError e
              Nothing -> throwError e
      _ -> throwError e

-- | The ordinary application path fixes the A-parameter/R-result spine
-- before visiting any argument, then closes each argument check before
-- synthesizing the next source argument.
inferIApplicationSequential
  :: TIExpr
  -> Type
  -> [IExpr]
  -> Subst
  -> TypeErrorContext
  -> Infer (TIExpr, Subst)
inferIApplicationSequential funcTIExpr funcType args initSubst ctx = do
  paramVars <-
    mapM (\index -> freshVar ("param" ++ show index)) [1 .. length args]
  resultType <- freshVar "result"
  let expectedFuncType = foldr TFun resultType paramVars
      Forall _ _ funcConstraints _ = tiScheme funcTIExpr
  appliedFuncType <- applySubstWithConstraintsM initSubst funcType
  classEnv <- getClassEnv
  contextConstraints <- getConstraints
  let constraints = funcConstraints ++ contextConstraints
  initialUnifier <-
    (Just <$> solveTypes
      classEnv constraints appliedFuncType expectedFuncType ctx)
      `catchError` \_ -> return Nothing
  case initialUnifier of
    Just (shapeSubst, shapeFlag) -> do
      let initial =
            ( []
            , composeSubst shapeSubst initSubst
            , shapeFlag
            )
      (reversedArgs, finalSubst, tensorFlag) <-
        foldM
          (inferAndCheck classEnv constraints)
          initial
          (zip args paramVars)
      finishApplication
        classEnv funcConstraints resultType funcTIExpr
        (reverse reversedArgs) finalSubst tensorFlag
    Nothing ->
      case appliedFuncType of
        TMathValue -> do
          argResults <- mapM (\arg -> inferIExprWithContext arg ctx) args
          let argTIExprs = map fst argResults
              finalSubst =
                foldr composeSubst initSubst (map snd argResults)
              resultScheme = Forall [] [] [] TMathValue
              updatedFuncTI =
                applySubstToTIExprWithClassEnv
                  classEnv finalSubst funcTIExpr
              updatedArgTIs =
                map
                  (applySubstToTIExprWithClassEnv classEnv finalSubst)
                  argTIExprs
          return
            ( TIExpr resultScheme (TIApplyExpr updatedFuncTI updatedArgTIs)
            , finalSubst
            )
        _ -> throwError $ UnificationError appliedFuncType expectedFuncType ctx
  where
    inferAndCheck classEnv constraints
                  (typedArgs, substitution, flag)
                  (sourceArg, paramType) = do
      (typedArg, argSubst) <- inferIExprWithContext sourceArg ctx
      let substitution' = composeSubst argSubst substitution
      inferredType <-
        applySubstWithConstraintsM substitution' (tiExprType typedArg)
      expectedType <-
        applySubstWithConstraintsM substitution' paramType
      let outerConstraints =
            map (applySubstConstraint substitution') constraints
          Forall _ _ argConstraints _ = tiScheme typedArg
          allConstraints =
            outerConstraints ++
            map (applySubstConstraint substitution') argConstraints
      (checkingSubst, argumentFlag) <-
        solveApplicationArgument
          classEnv allConstraints sourceArg typedArg
          inferredType expectedType ctx
      return
        ( typedArg : typedArgs
        , composeSubst checkingSubst substitution'
        , flag || argumentFlag
        )

-- | Build the typed application after all argument checks have closed.
finishApplication
  :: ClassEnv
  -> [Constraint]
  -> Type
  -> TIExpr
  -> [TIExpr]
  -> Subst
  -> Bool
  -> Infer (TIExpr, Subst)
finishApplication classEnv funcConstraints resultType funcTIExpr
                  argTIExprs finalSubst tensorFlag = do
  baseResultType <- applySubstWithConstraintsM finalSubst resultType
  let finalType
        | tensorFlag && not (Types.isTensorType baseResultType) =
            TTensor baseResultType
        | otherwise = baseResultType
      updatedFuncConstraints =
        map (applySubstConstraint finalSubst) funcConstraints
      simplifiedFuncConstraints =
        simplifyTensorConstraints classEnv updatedFuncConstraints
      deduplicatedConstraints = nub simplifiedFuncConstraints
      isTypeVarConstraint constraint =
        any isTypeVarType (constraintTypes constraint)
      isTypeVarType (TVar _) = True
      isTypeVarType (TSkolem _) = True
      isTypeVarType _ = False
      typeVarConstraints =
        filter isTypeVarConstraint deduplicatedConstraints
      resultConstraints = case finalType of
        TFun _ _ -> typeVarConstraints
        _        -> []
      resultScheme = Forall [] [] resultConstraints finalType
      updatedFuncTI =
        applySubstToTIExprWithClassEnv classEnv finalSubst funcTIExpr
      updatedArgTIs =
        map
          (applySubstToTIExprWithClassEnv classEnv finalSubst)
          argTIExprs
  return
    ( TIExpr resultScheme (TIApplyExpr updatedFuncTI updatedArgTIs)
    , finalSubst
    )

-- | The unification half of application inference: fresh parameter/result
-- variables, function-shape unification, then argument/parameter
-- unification in source order. Factored out so the
-- CAS-join retry above can re-run it with reshaped arguments.
inferIApplicationUnifyPhase :: TIExpr -> Type -> [IExpr] -> [TIExpr] -> [Type] -> Subst -> TypeErrorContext -> Infer (TIExpr, Subst)
inferIApplicationUnifyPhase funcTIExpr funcType args argTIExprs argTypes argSubst ctx = do
  -- Create fresh type variables for parameters and result
  paramVars <- mapM (\i -> freshVar ("param" ++ show i)) [1..length argTypes]
  resultType <- freshVar "result"
  let expectedFuncType = foldr TFun resultType paramVars
  appliedFuncType <- applySubstWithConstraintsM argSubst funcType


  -- First unify function type structure to get parameter bindings
  let funcScheme = tiScheme funcTIExpr
      (Forall _capVars _tvs funcConstraints _) = funcScheme
  classEnv <- getClassEnv
  -- Include constraints from both the function being applied AND the inference context
  -- The context constraints include constraints from outer scopes (e.g., {Num a} from (.) definition)
  contextConstraints <- getConstraints
  let constraints = funcConstraints ++ contextConstraints
  initialUnifier <-
    (Just <$> solveTypes
      classEnv constraints appliedFuncType expectedFuncType ctx)
      `catchError` \_ -> return Nothing
  case initialUnifier of
    Just (s1, flag1) -> do
      -- Check arguments in source order.  This is part of the inference
      -- relation: each argument's constraints are closed before the next
      -- argument is checked.
      paramTypesRaw <- mapM (applySubstWithConstraintsM s1) paramVars
      let indexedArgs = zip4 args argTIExprs argTypes paramTypesRaw
      (s3, flag3) <- foldM (\(s, flagAcc) (sourceArg, typedArg, at, pt) -> do
                     at' <- applySubstWithConstraintsM s at
                     pt' <- applySubstWithConstraintsM s pt
                     let -- Get constraints from both the outer function and the argument itself
                         outerCs = map (applySubstConstraint s) constraints
                         argScheme = tiScheme typedArg
                         (Forall _ _ argConstraints _) = argScheme
                         argCs = map (applySubstConstraint s) argConstraints
                         allCs = outerCs ++ argCs
                     (s', flag') <-
                       solveApplicationArgument
                         classEnv allCs sourceArg typedArg at' pt' ctx
                     return (composeSubst s' s, flagAcc || flag')
                  ) (s1, flag1) indexedArgs

      let finalS = composeSubst s3 argSubst
      finishApplication
        classEnv funcConstraints resultType funcTIExpr
        argTIExprs finalS flag3

    Nothing ->
      -- Special case: if function has type MathValue, allow application returning MathValue
      -- (handles FunctionData application, e.g. f 0 where f := function (x))
      case appliedFuncType of
        TMathValue -> do
          classEnv' <- getClassEnv
          let resultScheme = Forall [] [] [] TMathValue
              updatedFuncTI = applySubstToTIExprWithClassEnv classEnv' argSubst funcTIExpr
              updatedArgTIs = map (applySubstToTIExprWithClassEnv classEnv' argSubst) argTIExprs
          return (TIExpr resultScheme (TIApplyExpr updatedFuncTI updatedArgTIs), argSubst)
        _ -> throwError $ UnificationError appliedFuncType expectedFuncType ctx
-- | Infer let bindings (non-recursive)

-- | Infer let bindings (non-recursive) with context
-- NEW: Returns TIBindingExpr instead of IBindingExpr
-- Infer IO bindings for do expressions
inferIOBindingsWithContext :: [IBindingExpr] -> TypeEnv -> Subst -> TypeErrorContext -> Infer ([TIBindingExpr], [(String, TypeScheme)], Subst)
inferIOBindingsWithContext [] _env s _ctx = return ([], [], s)
inferIOBindingsWithContext ((pat, expr):bs) env s ctx = do
  -- Infer the type of the expression
  (exprTI, s1) <- inferIExprWithContext expr ctx
  let exprType = tiExprType exprTI

  -- The expression should be of type IO a
  innerType <- freshVar "ioInner"
  exprType' <- applySubstWithConstraintsM s1 exprType
  s2 <- unifyTypesWithContext exprType' (TIO innerType) ctx
  let s12 = composeSubst s2 s1
  actualInnerType <- applySubstWithConstraintsM s12 innerType

  -- Create expected type from pattern and unify with inner type
  (patternType, s3) <- inferPatternType pat
  let s123 = composeSubst s3 s12
  actualInnerType' <- applySubstWithConstraintsM s123 actualInnerType
  patternType' <- applySubstWithConstraintsM s123 patternType
  s4 <- unifyTypesWithContext actualInnerType' patternType' ctx

  -- Apply all substitutions and extract bindings with inner type
  let finalS = composeSubst s4 s123
  finalInnerType <- applySubstWithConstraintsM finalS actualInnerType
  let bindings = extractIBindingsFromPattern pat finalInnerType
      s' = composeSubst finalS s

  _env' <- getEnv
  let extendedEnvList = bindings  -- Already a list of (String, TypeScheme)
  (restBindingTIs, restBindings, s2') <-
    withEnv extendedEnvList $
      inferIOBindingsWithContext bs env s' ctx
  return ((pat, exprTI) : restBindingTIs, bindings ++ restBindings, s2')
  where
    -- Infer the type that a pattern expects
    inferPatternType :: IPrimitiveDataPattern -> Infer (Type, Subst)
    inferPatternType PDWildCard = do
      t <- freshVar "wild"
      return (t, emptySubst)
    inferPatternType (PDPatVar _) = do
      t <- freshVar "patvar"
      return (t, emptySubst)
    inferPatternType (PDTuplePat pats) = do
      results <- mapM inferPatternType pats
      let types = map fst results
          substs = map snd results
          s = foldr composeSubst emptySubst substs
      return (TTuple types, s)
    inferPatternType PDEmptyPat = return (TCollection (TVar (TyVar "a")), emptySubst)
    inferPatternType (PDConsPat _ _) = do
      elemType <- freshVar "elem"
      return (TCollection elemType, emptySubst)
    inferPatternType (PDSnocPat _ _) = do
      elemType <- freshVar "elem"
      return (TCollection elemType, emptySubst)
    inferPatternType (PDInductivePat name pats) = do
      results <- mapM inferPatternType pats
      let types = map fst results
          substs = map snd results
          s = foldr composeSubst emptySubst substs
      return (TInductive name types, s)
    inferPatternType (PDConstantPat c) = do
      ty <- inferConstant c
      return (ty, emptySubst)
    -- MathValue primitive patterns
    inferPatternType (PDFracPat _ _) = return (TMathValue, emptySubst)
    inferPatternType (PDPlusPat _) = return (TPolyExpr, emptySubst)
    inferPatternType (PDTermPat _ _) = return (TTermExpr, emptySubst)
    inferPatternType (PDSymbolPat _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDApply1Pat _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDApply2Pat _ _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDApply3Pat _ _ _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDApply4Pat _ _ _ _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDQuotePat _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDFunctionPat _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDSubPat _) = return (TIndexExpr, emptySubst)
    inferPatternType (PDSupPat _) = return (TIndexExpr, emptySubst)
    inferPatternType (PDUserPat _) = return (TIndexExpr, emptySubst)

-- | Fully zonk a type through the local and globally committed
-- substitutions.  'applySubst' follows acyclic substitution chains to a
-- genuine fixed point, so no arbitrary iteration bound is needed here.
applySubstRecursively :: Subst -> Type -> Infer Type
applySubstRecursively = applySubstWithConstraintsM

inferIBindingsWithContext :: [IBindingExpr] -> TypeEnv -> Subst -> TypeErrorContext -> Infer ([TIBindingExpr], [(String, TypeScheme)], Subst)
inferIBindingsWithContext [] _env s _ctx = return ([], [], s)
inferIBindingsWithContext ((pat, expr):bs) env s ctx = do
  -- Infer the type of the expression
  (exprTI, s1) <- inferIExprWithContext expr ctx
  exprType <- case pat of
    -- This is the core let form.  Wildcard and destructuring bindings are
    -- also used internally to elaborate function-parameter patterns; their
    -- RHS type is taken as it is.
    PDPatVar _ -> applySubstWithConstraintsM emptySubst (tiExprType exprTI)
    _ -> return (tiExprType exprTI)
  let s1' = s1

  -- Create expected type from pattern and unify with expression type
  -- This helps resolve type variables in the expression type
  (patternType, s2) <- inferPatternType pat
  let s12 = composeSubst s2 s1'
  exprType' <- applySubstWithConstraintsM s12 exprType
  patternType' <- applySubstWithConstraintsM s12 patternType
  s3 <- unifyTypesWithContext exprType' patternType' ctx

  -- Apply all substitutions recursively until fixed point
  -- This ensures nested type variables are fully resolved (e.g., for sortWithSign)
  let finalS = composeSubst s3 s12
  finalExprType <- applySubstRecursively finalS exprType

  -- Let-generalization (paper T-LET, standard Hindley-Milner): quantify the
  -- binding over its type variables that occur neither in the environment nor
  -- in the accumulated class constraints.  The environment's free variables
  -- are zonked first: a lambda-bound variable already committed by the global
  -- substitution stands for its image's variables, which a stale entry does
  -- not mention.  Constrained variables stay monomorphic (a deliberate
  -- restriction): dictionaries are threaded at top-level definitions only, so
  -- generalizing a constrained local binding would outrun the runtime's
  -- dictionary passing.  Matcher-typed bindings (e.g. `let m := something`)
  -- are constraint-free and generalize fully, so a let-bound matcher may
  -- serve differently-typed match sites (matcher polymorphism), unlike a
  -- lambda-bound, monomorphic one.
  -- Only a single-variable binding (the paper's `let x = e1 in e2` form) is
  -- generalized; destructuring bindings keep monomorphic components.
  let rhsFreeCaps = freeCapVars finalExprType
      rhsFree = freeTyVars finalExprType
  bindings <-
    case pat of
      PDPatVar _
        | not (Set.null rhsFreeCaps) || not (Set.null rhsFree) -> do
        envNow <- getEnv
        envFreeImages <- mapM (applySubstWithConstraintsM emptySubst . TVar)
                              (Set.toList (freeVarsInEnv envNow))
        constraintsNow <- getConstraints
        let envFreeCapZ = freeCapVarsInEnv envNow
            envFreeZ = Set.unions (map freeTyVars envFreeImages)
            consFreeCaps =
              Set.unions [ freeCapVars t | c <- constraintsNow
                                         , t <- constraintTypes c ]
            consFree = Set.unions [ freeTyVars t | c <- constraintsNow, t <- constraintTypes c ]
            genCapSet =
              rhsFreeCaps `Set.difference`
                (envFreeCapZ `Set.union` consFreeCaps)
            genSet = rhsFree `Set.difference` (envFreeZ `Set.union` consFree)
            regeneralize (n, Forall _ _ _ t) =
              ( n
              , Forall
                  (Set.toList (freeCapVars t `Set.intersection` genCapSet))
                  (Set.toList (freeTyVars t `Set.intersection` genSet))
                  []
                  t
              )
        return (map regeneralize (extractIBindingsFromPattern pat finalExprType))
      _ -> return (extractIBindingsFromPattern pat finalExprType)
  let s' = composeSubst finalS s

  _env' <- getEnv
  let extendedEnvList = bindings  -- Already a list of (String, TypeScheme)
  (restBindingTIs, restBindings, s2') <-
    withEnv extendedEnvList $
      inferIBindingsWithContext bs env s' ctx
  return ((pat, exprTI) : restBindingTIs, bindings ++ restBindings, s2')
  where
    -- Infer the type that a pattern expects
    inferPatternType :: IPrimitiveDataPattern -> Infer (Type, Subst)
    inferPatternType PDWildCard = do
      t <- freshVar "wild"
      return (t, emptySubst)
    inferPatternType (PDPatVar _) = do
      t <- freshVar "patvar"
      return (t, emptySubst)
    inferPatternType (PDTuplePat pats) = do
      results <- mapM inferPatternType pats
      let types = map fst results
          substs = map snd results
          s = foldr composeSubst emptySubst substs
      return (TTuple types, s)
    inferPatternType PDEmptyPat = return (TCollection (TVar (TyVar "a")), emptySubst)
    inferPatternType (PDConsPat _ _) = do
      elemType <- freshVar "elem"
      return (TCollection elemType, emptySubst)
    inferPatternType (PDSnocPat _ _) = do
      elemType <- freshVar "elem"
      return (TCollection elemType, emptySubst)
    inferPatternType (PDInductivePat name pats) = do
      results <- mapM inferPatternType pats
      let types = map fst results
          substs = map snd results
          s = foldr composeSubst emptySubst substs
      return (TInductive name types, s)
    inferPatternType (PDConstantPat c) = do
      ty <- inferConstant c
      return (ty, emptySubst)
    -- MathValue primitive patterns
    inferPatternType (PDFracPat _ _) = return (TMathValue, emptySubst)
    inferPatternType (PDPlusPat _) = return (TPolyExpr, emptySubst)
    inferPatternType (PDTermPat _ _) = return (TTermExpr, emptySubst)
    inferPatternType (PDSymbolPat _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDApply1Pat _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDApply2Pat _ _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDApply3Pat _ _ _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDApply4Pat _ _ _ _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDQuotePat _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDFunctionPat _ _) = return (TSymbolExpr, emptySubst)
    inferPatternType (PDSubPat _) = return (TIndexExpr, emptySubst)
    inferPatternType (PDSupPat _) = return (TIndexExpr, emptySubst)
    inferPatternType (PDUserPat _) = return (TIndexExpr, emptySubst)

-- | Infer letrec bindings (recursive)

-- | Infer letrec bindings (recursive) with context
-- NEW: Returns TIBindingExpr instead of IBindingExpr
inferIRecBindingsWithContext :: [IBindingExpr] -> TypeEnv -> Subst -> TypeErrorContext -> Infer ([TIBindingExpr], [(String, TypeScheme)], Subst)
inferIRecBindingsWithContext bindings _env s ctx = do
  -- Create placeholders with fresh type variables
  placeholders <- mapM (\(pat, _) -> do
    (patternType, s1) <- inferPatternType pat
    return (pat, patternType, s1)) bindings
  
  let placeholderTypes = map (\(_, ty, _) -> ty) placeholders
      placeholderSubsts = map (\(_, _, s) -> s) placeholders
      s0 = foldr composeSubst s placeholderSubsts
  
  -- Extract bindings from placeholders
  let placeholderBindings = concat $ zipWith (\(pat, _, _) ty -> extractIBindingsFromPattern pat ty) placeholders placeholderTypes
      cycleMembers = recursiveCycleMembers bindings
      inferRecursiveBinding (pat, expr) =
        let boundNames = primitivePatternNames pat
            owner = case boundNames of
              [name] -> Just name
              _      -> Nothing
        in do
          case owner of
            Just name
              | name `Set.member` cycleMembers ->
                  checkRecursiveGroupValueRoot
                    name cycleMembers expr ctx
            _ -> return ()
          inferIExprWithContext expr ctx
  
  -- Infer expressions in extended environment
  results <-
    withEnv placeholderBindings $
      mapM inferRecursiveBinding bindings
  
  let exprTIs = map fst results
      exprTypes = map (tiExprType . fst) results
      substList = map snd results
      s1 = foldr composeSubst s0 substList
  
  -- Unify placeholder types with inferred expression types
  unifySubsts <- zipWithM (\placeholderTy exprTy -> do
    placeholderTy' <- applySubstWithConstraintsM s1 placeholderTy
    exprTy' <- applySubstWithConstraintsM s1 exprTy
    unifyTypesWithContext exprTy' placeholderTy' ctx
      `catchError` \err ->
        case err of
          TE.TypeMismatch expected actual reason errCtx ->
            throwError $ TE.TypeMismatch expected actual
              (reason ++
               "\n  While tying a recursive binding: body type " ++
               TP.prettyType exprTy' ++ ", placeholder type " ++
               TP.prettyType placeholderTy')
              errCtx
          _ -> throwError err) placeholderTypes exprTypes
  
  let finalS = foldr composeSubst s1 unifySubsts

  -- Re-extract bindings with fully resolved types
  exprTypes' <- mapM (applySubstRecursively finalS) exprTypes
  -- Let-generalization (paper T-LET, standard Hindley-Milner; the surface
  -- `let` parses as letrec, so this is the binding form the paper's
  -- let-generalization claim refers to).  Quantify each single-variable
  -- binding over its type variables that occur neither in the (zonked)
  -- environment nor in the accumulated class constraints:
  --   * the environment's free variables are zonked first — a lambda-bound
  --     variable already committed by the global substitution stands for its
  --     image's variables, which the stale entry does not mention;
  --   * constrained variables stay monomorphic (dictionaries are threaded at
  --     top-level definitions only, so a generalized constrained local
  --     binding would outrun the runtime's dictionary passing);
  --   * a matcher-literal binding (the desugarer wraps `matcher` definitions
  --     in a letrec) stays monomorphic: generalizing it would make the body's
  --     variable reference instantiate a fresh copy, severing the clause
  --     trees' type variables from the definition's final type.
  -- A let-bound matcher VALUE (e.g. `let m := something`) thus generalizes
  -- and may serve differently-typed match sites (matcher polymorphism),
  -- unlike a lambda-bound, monomorphic one.
  let groupFreeCaps = Set.unions (map freeCapVars exprTypes')
      groupFree = Set.unions (map freeTyVars exprTypes')
      monoExtract = concat $ zipWith (\(pat, _, _) ty -> extractIBindingsFromPattern pat ty) placeholders exprTypes'
  finalBindings <-
    if Set.null groupFreeCaps && Set.null groupFree
      then return monoExtract
      else do
        envNow <- getEnv
        envFreeImages <- mapM (applySubstWithConstraintsM emptySubst . TVar)
                              (Set.toList (freeVarsInEnv envNow))
        constraintsNow <- getConstraints
        let envFreeCapZ = freeCapVarsInEnv envNow
            envFreeZ = Set.unions (map freeTyVars envFreeImages)
            consFreeCaps =
              Set.unions [ freeCapVars t | c <- constraintsNow
                                         , t <- constraintTypes c ]
            consFree = Set.unions [ freeTyVars t | c <- constraintsNow, t <- constraintTypes c ]
            genCapSet =
              groupFreeCaps `Set.difference`
                (envFreeCapZ `Set.union` consFreeCaps)
            genSet = groupFree `Set.difference` (envFreeZ `Set.union` consFree)
            genOne (pat, _, _) ty _ = case pat of
              PDPatVar _ ->
                map (\(n, Forall _ _ _ t) ->
                       ( n
                       , Forall
                           (Set.toList
                             (freeCapVars t `Set.intersection` genCapSet))
                           (Set.toList
                             (freeTyVars t `Set.intersection` genSet))
                           []
                           t
                       ))
                    (extractIBindingsFromPattern pat ty)
              _ -> extractIBindingsFromPattern pat ty
        return (concat (zipWith3 genOne placeholders exprTypes' bindings))
  let transformedBindings = zipWith (\(pat, _) exprTI -> (pat, exprTI)) bindings exprTIs

  return (transformedBindings, finalBindings, finalS)
  where
    -- Infer the type that a pattern expects (same as in inferIBindingsWithContext)
    inferPatternType :: IPrimitiveDataPattern -> Infer (Type, Subst)
    inferPatternType PDWildCard = do
      t <- freshVar "wild"
      return (t, emptySubst)
    inferPatternType (PDPatVar _) = do
      t <- freshVar "rec"
      return (t, emptySubst)
    inferPatternType (PDTuplePat pats) = do
      results <- mapM inferPatternType pats
      let types = map fst results
          substs = map snd results
          s = foldr composeSubst emptySubst substs
      return (TTuple types, s)
    inferPatternType PDEmptyPat = return (TCollection (TVar (TyVar "a")), emptySubst)
    inferPatternType (PDConsPat _ _) = do
      elemType <- freshVar "elem"
      return (TCollection elemType, emptySubst)
    inferPatternType (PDSnocPat _ _) = do
      elemType <- freshVar "elem"
      return (TCollection elemType, emptySubst)
    inferPatternType (PDInductivePat name pats) = do
      results <- mapM inferPatternType pats
      let types = map fst results
          substs = map snd results
          s = foldr composeSubst emptySubst substs
      return (TInductive name types, s)
    inferPatternType (PDConstantPat c) = do
      ty <- inferConstant c
      return (ty, emptySubst)
    -- Add other cases as needed
    inferPatternType _ = do
      t <- freshVar "rec"
      return (t, emptySubst)

-- | Extract bindings from pattern
-- This function extracts variable bindings from a primitive data pattern
-- given the type that the pattern should match against
-- Helper to check if a pattern is a pattern variable
isPatVarPat :: IPrimitiveDataPattern -> Bool
isPatVarPat (PDPatVar _) = True
isPatVarPat _ = False

extractIBindingsFromPattern :: IPrimitiveDataPattern -> Type -> [(String, TypeScheme)]
extractIBindingsFromPattern pat ty = case pat of
  PDWildCard -> []
  PDPatVar var -> [(extractNameFromVar var, Forall [] [] [] ty)]
  PDInductivePat _ pats -> concatMap (\p -> extractIBindingsFromPattern p ty) pats
  PDTuplePat pats -> 
    case ty of
      TTuple tys | length pats == length tys -> 
        -- Types match: bind each pattern variable to corresponding type
        concat $ zipWith extractIBindingsFromPattern pats tys
      _ -> 
        -- Type is not a resolved tuple (might be type variable or mismatch)
        -- Extract pattern variables but assign them the full tuple type for now
        -- This is imprecise but allows variables to be in scope
        -- The actual element types will be determined during later unification
        concatMap (\p -> extractIBindingsFromPattern p ty) pats
  PDEmptyPat -> []
  PDConsPat p1 p2 ->
    case ty of
      TCollection elemTy -> extractIBindingsFromPattern p1 elemTy ++ extractIBindingsFromPattern p2 ty
      _ -> []
  PDSnocPat p1 p2 ->
    case ty of
      TCollection elemTy -> extractIBindingsFromPattern p1 ty ++ extractIBindingsFromPattern p2 elemTy
      _ -> []
  -- MathValue primitive patterns
  PDFracPat p1 p2 ->
    let polyExprTy = TPolyExpr
        mathValueTy = TMathValue
        p1Ty = if isPatVarPat p1 then mathValueTy else polyExprTy
        p2Ty = if isPatVarPat p2 then mathValueTy else polyExprTy
    in extractIBindingsFromPattern p1 p1Ty ++ extractIBindingsFromPattern p2 p2Ty
  PDPlusPat p ->
    let termExprTy = TTermExpr
        mathValueTy = TMathValue
        pTy = if isPatVarPat p then TCollection mathValueTy else TCollection termExprTy
    in extractIBindingsFromPattern p pTy
  PDTermPat p1 p2 ->
    let symbolExprTy = TSymbolExpr
        mathValueTy = TMathValue
        p2Ty = if isPatVarPat p2
               then TCollection (TTuple [mathValueTy, TInt])
               else TCollection (TTuple [symbolExprTy, TInt])
    in extractIBindingsFromPattern p1 TInt ++ extractIBindingsFromPattern p2 p2Ty
  PDSymbolPat p1 p2 ->
    let indexExprTy = TIndexExpr
    in extractIBindingsFromPattern p1 TString ++ extractIBindingsFromPattern p2 (TCollection indexExprTy)
  PDApply1Pat p1 p2 ->
    let mathValueTy = TMathValue
        fnTy = TFun mathValueTy mathValueTy
    in extractIBindingsFromPattern p1 fnTy ++ extractIBindingsFromPattern p2 mathValueTy
  PDApply2Pat p1 p2 p3 ->
    let mathValueTy = TMathValue
        fnTy = TFun mathValueTy (TFun mathValueTy mathValueTy)
    in extractIBindingsFromPattern p1 fnTy ++ extractIBindingsFromPattern p2 mathValueTy ++ extractIBindingsFromPattern p3 mathValueTy
  PDApply3Pat p1 p2 p3 p4 ->
    let mathValueTy = TMathValue
        fnTy = TFun mathValueTy (TFun mathValueTy (TFun mathValueTy mathValueTy))
    in extractIBindingsFromPattern p1 fnTy ++ extractIBindingsFromPattern p2 mathValueTy ++ extractIBindingsFromPattern p3 mathValueTy ++ extractIBindingsFromPattern p4 mathValueTy
  PDApply4Pat p1 p2 p3 p4 p5 ->
    let mathValueTy = TMathValue
        fnTy = TFun mathValueTy (TFun mathValueTy (TFun mathValueTy (TFun mathValueTy mathValueTy)))
    in extractIBindingsFromPattern p1 fnTy ++ extractIBindingsFromPattern p2 mathValueTy ++ extractIBindingsFromPattern p3 mathValueTy ++ extractIBindingsFromPattern p4 mathValueTy ++ extractIBindingsFromPattern p5 mathValueTy
  PDQuotePat p ->
    let mathValueTy = TMathValue
    in extractIBindingsFromPattern p mathValueTy
  PDFunctionPat p1 p2 ->
    let mathValueTy = TMathValue
    in extractIBindingsFromPattern p1 mathValueTy ++ extractIBindingsFromPattern p2 (TCollection mathValueTy)
  PDSubPat p ->
    let mathValueTy = TMathValue
    in extractIBindingsFromPattern p mathValueTy
  PDSupPat p ->
    let mathValueTy = TMathValue
    in extractIBindingsFromPattern p mathValueTy
  PDUserPat p ->
    let mathValueTy = TMathValue
    in extractIBindingsFromPattern p mathValueTy
  _ -> []

-- | Infer top-level IExpr and return TITopExpr directly
-- | Warn when a top-level definition reuses a class method name.  Such a
-- def replaces the dispatching binding for the rest of the program, which
-- surfaces as baffling type errors far from the definition.  Class and
-- instance declarations lower to IDefineMany (registry, wrappers,
-- dictionaries), never to an IDefine of the bare method name, so a name
-- match at IDefine is always user shadowing.
warnOnClassMethodShadow :: Var -> Infer ()
warnOnClassMethodShadow (Var defName _) = do
  classEnv <- getClassEnv
  case [ cls | (cls, info) <- classEnvToList classEnv
             , defName `elem` map fst (Types.classMethods info) ] of
    (cls:_) -> addWarning (ClassMethodShadowWarning defName cls emptyContext)
    []      -> return ()

inferITopExpr :: ITopExpr -> Infer (Maybe TITopExpr, Subst)
inferITopExpr topExpr = case topExpr of
  IDefine var expr -> do
    warnOnClassMethodShadow var
    -- Harvest matcher clause shapes for the production use-site safeguard:
    -- a top-level definition whose (lambda-wrapped) body is a matcher literal
    -- records its clause pps under its name.
    case stripLambdasForShape expr of
      IMatcherExpr patDefs ->
        modify (\st -> st { inferMatcherShapes =
          Map.insert (extractNameFromVar var)
                     (map (\(pp, _, _) -> pp) patDefs)
                     (inferMatcherShapes st) })
      _ -> return ()
    env <- getEnv
    -- Check if there's an explicit type signature in the environment
    -- (added by EnvBuilder from DefineWithType).
    -- Use the exact-index lookup: a definition with index patterns
    -- (`def ∇_c T... := ...`, key ∇ [Sub Nothing]) must not adopt the
    -- signature of a same-name index-less variable (e.g. stdlib ∇) that
    -- lookupEnv's suffix fallback would return.
    case lookupEnvExact var env of
      Just existingScheme -> do
        -- There's an explicit type signature: check that the inferred type matches
        st <- get
        baselineGlobalSubst <- gets inferGlobalSubst
        classEnv <- getClassEnv
        let (instConstraints0, expectedType, annotationSkolems, newCounter) =
              skolemizeAnnotation existingScheme (inferCounter st)
            -- Expand superclass constraints so that superclass methods are available
            -- e.g., {Ord a} -> {Ord a, Eq a} since Ord extends Eq
            instConstraints = expandSuperclasses classEnv instConstraints0
        modify $ \s -> s { inferCounter = newCounter }
        -- Add instantiated constraints to the inference context
        -- This is crucial for constraint-aware unification inside the definition body
        -- e.g., when (.) has {Num a}, this constraint must be visible when type-checking t1 * t2
        clearConstraints  -- Start fresh
        addConstraints instConstraints
        -- Recursive uses of an annotated definition are monomorphic within
        -- the definition.  Shadow the polymorphic declaration with this
        -- single skolemized instance so lookup cannot instantiate a fresh
        -- variable of either sort at each recursive occurrence.  Retain the instantiated
        -- class constraints: recursive constrained functions must receive
        -- the same dictionary parameters as their enclosing definition.
        modify $ \state ->
          state
            { inferEnv =
                extendEnv var
                  (Forall [] [] instConstraints expectedType)
                  (inferEnv state)
            }

        -- Infer the expression type
        let owner = extractNameFromVar var
            checkedExpr =
              skolemizeNestedAnnotations annotationSkolems expr
        case var of
          Var _ [] ->
            checkRecursiveValueRoot
              owner checkedExpr (withExpr (prettyStr expr) emptyContext)
          -- Indexed definitions share a base spelling across overloads.  An
          -- indexed reference to an earlier overload is not a self-reference
          -- to this exact binding.
          Var _ _ -> return ()
        (exprTI, subst1) <- inferIExpr checkedExpr
        let exprType = tiExprType exprTI

        -- Unify inferred type with expected type using constraint-aware unification
        -- This is crucial for cases like (.) where type variables have constraints
        -- The constraints from the type signature affect how Tensor types are unified
        let exprCtx = withExpr (prettyStr expr) emptyContext
            -- Apply substitution to constraints to get current state
            currentConstraints = map (applySubstConstraint subst1) instConstraints
        exprType' <- applySubstWithConstraintsM subst1 exprType
        expectedType' <- applySubstWithConstraintsM subst1 expectedType
        subst2 <-
          unifyTypesWithConstraints currentConstraints exprType' expectedType' exprCtx
        let preHoleSubst = composeSubst subst2 subst1
        let finalSubst0 = preHoleSubst
        finalSubst <-
          closeAnnotatedTypeVariables
            annotationSkolems finalSubst0 exprCtx

        -- Reject the definition if its body needs constraints (on the
        -- signature's type variables) that the signature does not declare
        finalTypeChk <- applySubstWithConstraintsM finalSubst expectedType
        let Var defNameStr _ = var
        checkResidualConstraints defNameStr instConstraints finalTypeChk finalSubst exprCtx
        let usesExtension =
              typeSchemeUsesEgisonExtension existingScheme
                || expressionUsesEgisonExtension expr
        checkAnnotationBoundary
          baselineGlobalSubst env finalSubst
          usesExtension
          exprCtx
        checkAnnotationIdentity
          annotationSkolems finalSubst usesExtension exprCtx

        -- Apply final substitution to exprTI to resolve all type variables
        -- IMPORTANT: Use applySubstToTIExprM to adjust substitution based on constraints
        exprTI' <- applySubstToTIExprM finalSubst exprTI

        -- Resolve constraints in exprTI' (Tensor t0 -> t0)
        classEnv <- getClassEnv
        let exprTI'' =
              deskolemizeAnnotationTIExpr annotationSkolems
                (resolveConstraintsInTIExpr classEnv finalSubst exprTI')
            finalSubst' =
              deskolemizeAnnotationSubst annotationSkolems finalSubst
        
        -- Reconstruct type scheme from exprTI'' to match actual type variables
        -- Use instantiated constraints and apply final substitution
        -- When there's an explicit type annotation, use the expected type
        -- (with substitutions applied) as the final type, not the inferred type.
        -- This ensures that Tensor types are preserved when explicitly annotated.
        let Forall declaredCapVars declaredTyVars declaredConstraints declaredType =
              existingScheme
            updatedScheme =
              Forall declaredCapVars declaredTyVars
                (expandSuperclasses classEnv declaredConstraints)
                declaredType
        
        -- Update the environment with the expanded scheme
        -- This is important so that call sites see the full constraints
        -- (including superclass-expanded ones) and pass all needed dictionaries
        modify $ \s ->
          s
            { inferEnv =
                extendEnv var updatedScheme
                  (removeFromEnv var (inferEnv s))
            }
        deskolemizeAnnotationState annotationSkolems
        return (Just (TIDefine updatedScheme var exprTI''), finalSubst')
      
      Nothing -> do
        -- No explicit type signature: infer and generalize as before
        clearConstraints  -- Start with fresh constraints for this expression
        -- Monomorphic recursion: bind the definition's own name to a fresh
        -- type variable before inferring the body, so recursive calls
        -- constrain it.  (Previously the name was unbound in its own body:
        -- the calls warned, fell to Any, and could fail at runtime.  The
        -- accident that hid this: a def shadowing a class method looked
        -- its own name up in the METHOD's scheme.)  The body's type is
        -- unified with the placeholder below, so polymorphic recursion
        -- still needs an explicit signature, as in ML.
        selfTy <- freshVar "rec"
        modify $ \s -> s { inferEnv = extendEnv var (Forall [] [] [] selfTy) (inferEnv s) }
        let owner = extractNameFromVar var
        case var of
          Var _ [] ->
            checkRecursiveValueRoot
              owner expr (withExpr (prettyStr expr) emptyContext)
          Var _ _ -> return ()
        (exprTI, subst1) <- inferIExpr expr
        -- Tie the placeholder to the inferred body type.  For a
        -- non-recursive body the placeholder is still free and the
        -- unification just discharges it; for a recursive one this is
        -- where the recursive uses meet the definition.  This must precede
        -- the deferred hole checks: recursive uses can be the last source of
        -- information that resolves a hole target's structural head.
        selfTy' <- applySubstWithConstraintsM subst1 selfTy
        exprType' <- applySubstWithConstraintsM subst1 (tiExprType exprTI)
        let Var selfName _ = var
            recCtx = withContext
              ("in the definition of '" ++ selfName ++
               "': the type of a recursive use does not match the body" ++
               " (polymorphic recursion needs an explicit type signature)")
              (withExpr (prettyStr expr) emptyContext)
        subst2 <- unifyTypesWithContext selfTy' exprType' recCtx
        let preHoleSubst = composeSubst subst2 subst1
        let subst = preHoleSubst

        -- Apply the complete substitution to the stored expression, exactly
        -- as the signature branch does.  Without this, node schemes inside
        -- the expression (notably matcher data-clause arms) keep stale type
        -- variables in their constraints, and the generalized scheme below
        -- is built from variables that no longer match the nodes —
        -- TypeClassExpand then emits unbound dictionary references.
        exprTI' <- applySubstToTIExprM subst exprTI
        let exprType = tiExprType exprTI'
        constraints <- getConstraints  -- Collect constraints from type inference

        -- Resolve constraints based on available instances
        classEnv <- getClassEnv
        let exprTI'' = resolveConstraintsInTIExpr classEnv subst exprTI'
            updatedConstraints = map (resolveConstraintWithInstances classEnv subst) constraints
            -- Filter out constraints on concrete types (non-type-variables)
            -- Concrete constraints don't need to be generalized since the type is already determined
            isTypeVarConstraint c = any isTypeVarType' (constraintTypes c)
            isTypeVarType' (TVar _) = True
            isTypeVarType' (TSkolem _) = True
            isTypeVarType' _        = False
            -- Deduplicate constraints (e.g., {Num a, Num a} -> {Num a})
            generalizedConstraints = nub $ filter isTypeVarConstraint updatedConstraints

        -- Generalize with filtered constraints (only type variables)
        let envFreeCapVars = freeCapVarsInEnv env
            envFreeVars = freeVarsInEnv env
            typeFreeCapVars = freeCapVars exprType
            typeFreeVars = freeTyVars exprType
            genCapVars =
              Set.toList $ typeFreeCapVars `Set.difference` envFreeCapVars
            genVars = Set.toList $ typeFreeVars `Set.difference` envFreeVars
            scheme = Forall genCapVars genVars generalizedConstraints exprType

        -- Add to environment using the Var directly (preserves index info)
        modify $ \s -> s { inferEnv = extendEnv var scheme (inferEnv s) }

        return (Just (TIDefine scheme var exprTI''), subst)
  
  ITest expr -> do
    clearConstraints  -- Start with fresh constraints
    (exprTI, finalSubst) <- inferIExpr expr
    exprTI' <- applySubstToTIExprM finalSubst exprTI
    -- Constraints are now in state, will be retrieved by Eval.hs
    return (Just (TITest exprTI'), finalSubst)
  
  IExecute expr -> do
    clearConstraints  -- Start with fresh constraints
    (exprTI, finalSubst) <- inferIExpr expr
    exprTI' <- applySubstToTIExprM finalSubst exprTI
    -- Constraints are now in state, will be retrieved by Eval.hs
    return (Just (TIExecute exprTI'), finalSubst)
  
  ILoadFile _path -> return (Nothing, emptySubst)
  ILoad _lib -> return (Nothing, emptySubst)

  IDefineMany bindings -> do
    -- Process each binding in the list
    env <- getEnv
    let recursiveBindings =
          [ (PDPatVar var, expression)
          | (var, expression) <- bindings
          ]
        cycleMembers = recursiveCycleMembers recursiveBindings
    results <-
      mapM (inferBinding env cycleMembers) bindings
    let bindingsTI = map fst results
        substs = map snd results
        combinedSubst = foldr composeSubst emptySubst substs
    return (Just (TIDefineMany bindingsTI), combinedSubst)
    where
      inferBinding env cycleMembers binding@(var, expression) = do
          let owner = extractNameFromVar var
          when (owner `Set.member` cycleMembers) $
            case var of
              Var _ [] ->
                checkRecursiveGroupValueRoot
                  owner cycleMembers expression emptyContext
              Var _ _ -> return ()
          inferBindingInScope env binding

      -- An IDefineMany hash-literal binding is, by construction, a type-class
      -- instance dictionary (Desugar's makeDictDef; the other IDefineMany
      -- producers bind lambdas).  A dictionary is a heterogeneous record:
      -- method entries have the methods' own types (e.g. Ord's
      -- compare : a -> a -> Ordering next to (<) : a -> a -> Bool) and
      -- __super_ entries hold superclass dictionaries.  It is consumed through
      -- type-class expansion (dictionary passing / runtime dispatch), never
      -- through the hash's value type, so the entry types are deliberately not
      -- unified with each other — only keys are checked (String) — and the
      -- node is typed Hash String v, v fresh.  (EnvBuilder registers the
      -- dictionary's env scheme separately, approximating the value type by
      -- the first method's; we leave that scheme as is and skip checking the
      -- literal against it.)
      inferBindingInScope _env (var, IHashExpr pairs@(_:_)) = do
        clearConstraints
        (pairTIs, finalSubst) <- foldM dictPair ([], emptySubst) pairs
        v <- freshVar "dictVal"
        let exprTI = TIExpr (Forall [] [] [] (THash TString v)) (TIHashExpr (reverse pairTIs))
        exprTI' <- applySubstToTIExprM finalSubst exprTI
        return ((var, exprTI'), finalSubst)
        where
          dictPair (acc, s) (k, vE) = do
            (kTI, s1) <- inferIExprWithContext k emptyContext
            sk <- unifyTypesWithContext (tiExprType kTI) TString emptyContext
            (vTI, s2) <- inferIExprWithContext vE emptyContext
            return ((kTI, vTI) : acc, foldr composeSubst s [s2, sk, s1])
      inferBindingInScope env (var, expr) = do
        -- Check if there's an existing type signature (exact index match;
        -- see the IDefine case for why the prefix/suffix fallbacks must not
        -- be used at definition sites)
        case lookupEnvExact var env of
          Just existingScheme -> do
            -- With type signature: check type
            st <- get
            baselineGlobalSubst <- gets inferGlobalSubst
            classEnvForSig <- getClassEnv
            let (instCs0, expectedType, annotationSkolems, newCounter) =
                  skolemizeAnnotation existingScheme (inferCounter st)
                instCsMany = expandSuperclasses classEnvForSig instCs0
            modify $ \s -> s { inferCounter = newCounter }

            clearConstraints
            -- Make the signature's constraints visible while checking the
            -- body (parity with the IDefine signature branch)
            addConstraints instCsMany
            modify $ \state ->
              state
                { inferEnv =
                    extendEnv var
                      (Forall [] [] instCsMany expectedType)
                      (inferEnv state)
                }
            let checkedExpr =
                  skolemizeNestedAnnotations annotationSkolems expr
            (exprTI, subst1) <- inferIExpr checkedExpr
            let exprType = tiExprType exprTI
            exprType' <- applySubstWithConstraintsM subst1 exprType
            expectedType' <- applySubstWithConstraintsM subst1 expectedType
            -- An annotated matcher literal (possibly lambda-wrapped) is
            -- checked by ordinary equality with the signature's constraints
            -- not yet in scope; other definitions use the top-level unifier.
            subst2 <- case rhsCore checkedExpr of
              IMatcherExpr _ ->
                unifyTypesWithConstraints [] exprType' expectedType' emptyContext
              _ -> unifyTypesWithTopLevel exprType' expectedType' emptyContext
            let finalSubst0 = composeSubst subst2 subst1
            finalSubst <-
              closeAnnotatedTypeVariables
                annotationSkolems finalSubst0 emptyContext
            -- Reject if the body needs constraints the signature lacks
            finalTypeChk <- applySubstWithConstraintsM finalSubst expectedType
            let Var defNameStr _ = var
            checkResidualConstraints defNameStr instCsMany finalTypeChk finalSubst emptyContext
            let usesExtension =
                  typeSchemeUsesEgisonExtension existingScheme
                    || expressionUsesEgisonExtension expr
            checkAnnotationBoundary
              baselineGlobalSubst env finalSubst
              usesExtension
              emptyContext
            checkAnnotationIdentity
              annotationSkolems finalSubst usesExtension emptyContext
            exprTI' <- applySubstToTIExprM finalSubst exprTI
            -- The monomorphic entry above is scoped to checking this body.
            -- Restore the declared polymorphic scheme for later definitions
            -- and for dictionary elaboration of this IDefineMany batch.
            let Forall declaredCapVars declaredTyVars declaredConstraints declaredType =
                  existingScheme
                updatedScheme =
                  Forall declaredCapVars declaredTyVars
                    (expandSuperclasses classEnvForSig declaredConstraints)
                    declaredType
                exprTI'' =
                  deskolemizeAnnotationTIExpr annotationSkolems exprTI'
                finalSubst' =
                  deskolemizeAnnotationSubst annotationSkolems finalSubst
            modify $ \state ->
              state
                { inferEnv =
                    extendEnv var updatedScheme
                      (removeFromEnv var (inferEnv state))
                }
            deskolemizeAnnotationState annotationSkolems
            return ((var, exprTI''), finalSubst')
          
          Nothing -> do
            -- Without type signature: infer and generalize
            clearConstraints
            (exprTI, finalSubst) <- inferIExpr expr
            -- Apply the substitution to the stored expression (same as the
            -- IDefine no-signature branch): node schemes must not keep stale
            -- type variables in their constraints, or TypeClassExpand emits
            -- unbound dictionary references (e.g. inside `declare rule`
            -- generated bodies, which arrive here via IDefineMany).
            exprTI' <- applySubstToTIExprM finalSubst exprTI
            let exprType = tiExprType exprTI'
            constraints <- getConstraints

            -- Resolve constraints based on available instances
            classEnv <- getClassEnv
            let exprTI'' = resolveConstraintsInTIExpr classEnv finalSubst exprTI'
                updatedConstraints = map (resolveConstraintWithInstances classEnv finalSubst) constraints
                -- Filter out constraints on concrete types (non-type-variables)
                isTypeVarConstraint c = any isTypeVarT (constraintTypes c)
                isTypeVarT (TVar _) = True
                isTypeVarT (TSkolem _) = True
                isTypeVarT _        = False
                -- Deduplicate constraints (e.g., {Num a, Num a} -> {Num a})
                generalizedConstraints = nub $ filter isTypeVarConstraint updatedConstraints

            -- Generalize the type
            let envFreeCapVars = freeCapVarsInEnv env
                envFreeVars = freeVarsInEnv env
                typeFreeCapVars = freeCapVars exprType
                typeFreeVars = freeTyVars exprType
                genCapVars =
                  Set.toList $ typeFreeCapVars `Set.difference` envFreeCapVars
                genVars = Set.toList $ typeFreeVars `Set.difference` envFreeVars
                scheme = Forall genCapVars genVars generalizedConstraints exprType

            -- Add to environment for subsequent bindings using Var directly
            modify $ \s -> s { inferEnv = extendEnv var scheme (inferEnv s) }

            return ((var, exprTI''), finalSubst)
  
  IPatternFunctionDecl name tyVars params retType body -> do
    -- Pattern function type checking follows TypePM PATFUN-DEF.  Parameter
    -- capabilities and targets remain paired throughout body inference and
    -- are generalized with the result into one canonical DualScheme.
    let paramTypes = map snd params
        funcType = foldr TFun retType paramTypes
        declaredCapVars = Set.toList (freeCapVars funcType)
        typeScheme = Forall declaredCapVars tyVars [] funcType
    st <- get
    baselineGlobalSubst <- gets inferGlobalSubst
    outerEnv <- getEnv
    let (_checkedConstraints, _checkedFuncType,
          annotationSkolems, newCounter) =
            skolemizePatternAnnotation typeScheme (inferCounter st)
        checkedParams =
          [ (paramName,
              skolemizeAnnotationType annotationSkolems paramType)
          | (paramName, paramType) <- params
          ]
        checkedRetType =
          skolemizeAnnotationType annotationSkolems retType
        checkedBody =
          mapIPatternTypes
            (skolemizeAnnotationType annotationSkolems)
            body
    modify $ \state -> state { inferCounter = newCounter }

    clearConstraints  -- Start fresh

    let ctx = TypeErrorContext
                { errorLocation = Nothing
                , errorExpr = Just ("Pattern function: " ++ name)
                , errorContext = Just ("Expected type: " ++ show retType)
                }
        paramNames = map fst params
        paramUses = filter (`elem` paramNames) (patternVarRefsInOrder body)
        duplicateParameters = duplicateNames paramNames
    unless (null duplicateParameters) $
      throwError $ TE.DuplicatePatternFunctionParameters
        name duplicateParameters ctx
    when
      (name `elem`
        patternFunctionCallHeads (Set.fromList paramNames) body) $
      throwError $ TE.RecursivePatternFunction name ctx
    -- Linearity (PATFUN-DEF side condition): exactly one use of each parameter,
    -- in declaration order.  This is what lets MS-MNODE-VARPAT expand each
    -- argument pattern exactly once, left to right, so argument bindings appear
    -- exactly as promised and value patterns in later arguments can refer to
    -- variables bound by earlier ones.
    when (paramUses /= paramNames) $
      throwError $ TE.PatternFunctionLinearityError name paramNames paramUses ctx
    -- A parameter under an or-, loop-, not-, or forall-pattern may be expanded
    -- zero or several times along a matching path even when it occurs exactly
    -- once syntactically, so it is rejected as well.
    let branchedUses = filter (`elem` paramNames) (patternVarRefsUnderBranch body)
    when (not (null branchedUses)) $
      throwError $ TE.PatternFunctionParamUnderBranchError name branchedUses ctx

    -- The body may still use an Egison pattern form outside the mechanized
    -- core.  Preserve its inferred DualScheme, but surface that proof boundary
    -- now; later named applications intentionally use the finalized scheme
    -- without repeating the same warning.
    warnPatternCompatibility ctx body

    -- Add parameters to environment for type checking the body
    -- Note: Parameter types don't need Pattern wrapper (design/pattern.md)
    let paramBindings =
          map
            (\(pname, pty) -> (pname, Forall [] [] [] pty))
            checkedParams
    parameterCapabilities <-
      mapM (const (freshCapability "patternParameter")) params
    let checkedParameterDuals =
          zipWith
            (\capability (_, target) -> Dual capability target)
            parameterCapabilities
            checkedParams
        parameterDualMap =
          Map.fromList (zip paramNames checkedParameterDuals)
    previousParameterDuals <- inferPatfunParamDuals <$> get
    modify $ \state ->
      state { inferPatfunParamDuals = parameterDualMap }
    bodyOutcome <-
      (Right <$> withEnv paramBindings
        (inferIPattern checkedBody checkedRetType ctx))
        `catchError` (return . Left)
    modify $ \state ->
      state { inferPatfunParamDuals = previousParameterDuals }
    (typedBody, _bodyBindings, bodySubst, bodyCapability) <-
      either throwError return bodyOutcome
    let finalSubst0 = bodySubst
    finalSubst <-
      closeAnnotatedTypeVariables annotationSkolems finalSubst0 ctx
    typedBody' <- applySubstToTIPatternM finalSubst typedBody
    checkedParameterDuals' <-
      mapM (applyDualM finalSubst) checkedParameterDuals
    checkedResultDual' <-
      applyDualM finalSubst (Dual bodyCapability checkedRetType)
    checkAnnotationBoundary
      baselineGlobalSubst outerEnv finalSubst
      (typeSchemeUsesEgisonExtension typeScheme)
      ctx
    checkAnnotationIdentity
      annotationSkolems finalSubst
      (typeSchemeUsesEgisonExtension typeScheme) ctx

    let parameterDuals =
          map
            (deskolemizeAnnotationDual annotationSkolems)
            checkedParameterDuals'
        resultDual =
          deskolemizeAnnotationDual annotationSkolems checkedResultDual'
        typedBody'' =
          deskolemizeAnnotationTIPattern annotationSkolems typedBody'
        finalSubst' =
          deskolemizeAnnotationSubst annotationSkolems finalSubst
    deskolemizeAnnotationState annotationSkolems

    let Forall _ strengthenedTyVars _ _ = typeScheme
    dualScheme <-
      generalizeDualSchemeInState
        declaredCapVars strengthenedTyVars parameterDuals resultDual
    let targetScheme = dualSchemeTargetScheme dualScheme
    modify $ \state -> state
      { inferPatternFuncEnv =
          extendPatternFunctionEnv
            name dualScheme (inferPatternFuncEnv state)
      , inferPatternFuncDeclEnv =
          extendPatternEnv
            name targetScheme (inferPatternFuncDeclEnv state)
      , inferEnv =
          extendEnv
            (stringToVar name) targetScheme (inferEnv state)
      }

    return
      ( Just
          (TIPatternFunctionDecl
            name dualScheme params retType typedBody'')
      , finalSubst'
      )
  
  IDeclareSymbol names mType -> do
    -- Register declared symbols with their types
    let ty = case mType of
               Just t  -> t
               Nothing -> TInt  -- Default to Integer (MathValue)
    -- Add symbols to declared symbols map
    modify $ \s -> s { declaredSymbols = 
                        foldr (\name m -> Map.insert name ty m) 
                              (declaredSymbols s) 
                              names }
    -- Also add to type environment so they can be used in subsequent expressions
    let scheme = Forall [] [] [] ty
    modify $ \s -> s { inferEnv = 
                        foldr (\name e -> extendEnv (stringToVar name) scheme e) 
                              (inferEnv s) 
                              names }
    -- Return the typed declaration
    return (Just (TIDeclareSymbol names ty), emptySubst)

-- | Infer multiple top-level IExprs
inferITopExprs :: [ITopExpr] -> Infer ([Maybe TITopExpr], Subst)
inferITopExprs [] = return ([], emptySubst)
inferITopExprs (e:es) = do
  (tyE, s1) <- inferITopExpr e
  (tyEs, s2) <- inferITopExprs es
  return (tyE : tyEs, composeSubst s2 s1)

--------------------------------------------------------------------------------
-- * Running Inference
--------------------------------------------------------------------------------

