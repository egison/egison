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
  , runInfer
  , runInferWithWarnings
  , runInferWithWarningsAndState
    -- * Running inference
  , runInferI
  , runInferIWithEnv
    -- * Helper functions
  , freshVar
  , getEnv
  , setEnv
  , withEnv
  , lookupVar
  , unifyTypes
  , generalize
  , inferConstant
  , addWarning
  , clearWarnings
  ) where

import           Control.Monad              (foldM, when, zipWithM)
import           Control.Monad.Except       (ExceptT, runExceptT, throwError, catchError)
import           Control.Monad.State.Strict (StateT, evalStateT, runStateT, get, gets, modify, put)
import           Data.List                  (isPrefixOf, nub, partition, intercalate)
import           Data.Maybe                  (catMaybes, fromMaybe)
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
                                            , tiExprType, mapTIExprChildren)
import           Language.Egison.Pretty     (prettyStr)
import           Language.Egison.Type.Env
import qualified Language.Egison.Type.Error as TE
import           Language.Egison.Type.Error (TypeError(..), TypeErrorContext(..), TypeWarning(..),
                                              emptyContext, withExpr)
import qualified Language.Egison.Type.Subtype as Subtype
import           Language.Egison.Type.Subst (Subst(..), applySubst, applySubstConstraint,
                                              applySubstScheme, composeSubst, emptySubst,
                                              singletonSubst)
import           Language.Egison.Type.Tensor (normalizeTensorType)
import           Language.Egison.Type.Types
import qualified Language.Egison.Type.Types as Types
import           Language.Egison.Type.Unify as TU
import qualified Language.Egison.Type.Unify as Unify
import           Language.Egison.Type.Instance (findMatchingInstanceForType)

--------------------------------------------------------------------------------
-- * Infer Monad and State
--------------------------------------------------------------------------------

-- | Inference configuration
data InferConfig = InferConfig
  { cfgPermissive       :: Bool  -- ^ Treat unbound variables as warnings, not errors
  , cfgCollectWarnings  :: Bool  -- ^ Collect warnings during inference
  , cfgMatcherConsistencyWarnings :: Bool  -- ^ Emit matcher consistency warnings (paper Def 4.2):
                                 --   Coverage (4.2(3)) and PP-Con (4.2(1a)).  Off by default, as
                                 --   the standard library has intentionally partial / non-strict
                                 --   matchers (opt-in diagnostic; --matcher-consistency-warnings).
                                 --   Arm exhaustiveness (4.2(1c)) is not gated here: it is an
                                 --   ordinary type error (see pdArmsExhaustive).
  }

instance Show InferConfig where
  show cfg = "InferConfig { cfgPermissive = " ++ show (cfgPermissive cfg)
           ++ ", cfgCollectWarnings = " ++ show (cfgCollectWarnings cfg)
           ++ ", cfgMatcherConsistencyWarnings = " ++ show (cfgMatcherConsistencyWarnings cfg)
           ++ " }"

-- | Default configuration (strict mode)
defaultInferConfig :: InferConfig
defaultInferConfig = InferConfig
  { cfgPermissive = False
  , cfgCollectWarnings = False
  , cfgMatcherConsistencyWarnings = False
  }

-- | Permissive configuration (for gradual adoption)
permissiveInferConfig :: InferConfig
permissiveInferConfig = InferConfig
  { cfgPermissive = True
  , cfgCollectWarnings = True
  , cfgMatcherConsistencyWarnings = False
  }

-- | Inference state
data InferState = InferState
  { inferCounter     :: Int              -- ^ Fresh variable counter
  , inferEnv         :: TypeEnv          -- ^ Current type environment
  , inferWarnings    :: [TypeWarning]    -- ^ Collected warnings
  , inferConfig      :: InferConfig      -- ^ Configuration
  , inferClassEnv    :: ClassEnv         -- ^ Type class environment
  , inferPatternEnv  :: PatternTypeEnv   -- ^ Pattern constructor environment (merged)
  , inferPatternFuncEnv :: PatternTypeEnv  -- ^ Pattern function environment (for disambiguation)
  , inferPatternFuncStructEnv :: PatternTypeEnv
                                          -- ^ Pattern function structural signatures (paper PATFUN-DEF):
                                          --   name |-> scheme of beta_1 -> ... -> beta_k -> tau_p_body,
                                          --   where beta_i is parameter i's structural index and
                                          --   tau_p_body the body's structural index.  PAT-APP
                                          --   instantiates this to compute an application's tau_p.
  , inferPatfunParamTaup :: Map.Map String Type
                                          -- ^ While inferring a pattern function body: parameter name
                                          --   |-> its structural index beta_i, consulted by IVarPat
                                          --   (the ~param embedding).  Empty outside such bodies.
  , inferPatfunTaupEqs :: Maybe [(Type, Type)]
                                          -- ^ While inferring a pattern function body (Just):
                                          --   the structural equations solved locally (and then
                                          --   discarded) by taupCombine / taupFromCtor.  The
                                          --   per-node solvers keep only each node's RESULT type,
                                          --   so links binding a parameter's beta_i can be lost to
                                          --   unifier direction; PATFUN-DEF re-solves the recorded
                                          --   equations jointly and applies the solution to the
                                          --   structural signature, recovering every link.
  , inferConstraints :: [Constraint]     -- ^ Accumulated type class constraints
  , declaredSymbols  :: Map.Map String Type  -- ^ Declared symbols with their types
  , inferInMatcherBody :: Bool           -- ^ True while inferring a `matcher` body.  The match-site
                                          --   admissibility check (T-MATCHALL) runs normally here —
                                          --   match-sites nested in a matcher body are genuinely
                                          --   type-checked.  This flag only suppresses matcher-
                                          --   Coverage (Def 4.2(3)) warnings for nested / generated
                                          --   matchers, whose constructor set is an implementation
                                          --   detail rather than a user-facing matcher.
  , inferDeferredHoleChecks :: [(Type, HoleCompShape, String, TypeErrorContext)]
                                          -- ^ Matcher-definition hole admissibility checks deferred
                                          --   to the end of the top-level expression (paper PP-Con,
                                          --   Def 4.2(1a)): the hole's TARGET type may be pinned only
                                          --   by the definition's annotation, so the structural test
                                          --   runs after the final substitution.  (holeTy, shape of
                                          --   the next-matcher component, error context).
  , inferGlobalSubst :: Subst             -- ^ The growing zonk substitution: every committed
                                          --   unification merges its result here, and the unify
                                          --   wrappers resolve both sides through it first.  Sibling
                                          --   subexpressions are inferred independently and their
                                          --   substitutions merged with the left-biased 'composeSubst',
                                          --   which on a conflicting binding silently keeps one side
                                          --   (e.g. two match sites committing the same lambda-bound
                                          --   matcher parameter to different slot types).  Zonking
                                          --   makes the later unification see the earlier commitment,
                                          --   so the conflict is unified — and reported — instead of
                                          --   shadowed.  Reset per top-level item (a fresh InferState
                                          --   is seeded for each).
  , inferCasSubtypeEdges :: Subtype.SubtypeEnv
                                          -- ^ Declared `cas-subtype` edges (alias-expanded), seeded
                                          --   from EvalState per top-level item.  Consulted by the
                                          --   application-site CAS join: when two CAS operand types
                                          --   fail to unify, their unique join in the declared order
                                          --   (D1) becomes the promotion target and both operands are
                                          --   reshaped to it (elaboration inserts the coercion; the
                                          --   unifier itself never joins).
  } deriving (Show)

-- | Shape classification of a matcher-clause hole's next-matcher component,
-- recorded at clause-inference time (before the hole/target unification ties
-- its type to the hole) for the deferred admissibility check:
--   * HCSlot: a slot-typed parameter — committed to the hole by index
--     unification (Def 4.2(1a) parameter route), nothing left to check.
--   * HCBareVar: a bare-variable matcher value (eq / something) — admissible
--     only at a variable-headed or function-typed hole.
--   * HCShape t: a structured/concrete matcher value — its (freshened)
--     intrinsic inner type must one-way match the hole's structural index
--     (same head, fresh leaves).
data HoleCompShape = HCSlot | HCBareVar Type | HCShape Type
  deriving (Show)

-- | Initial inference state
initialInferState :: InferState
initialInferState = InferState 0 emptyEnv [] defaultInferConfig emptyClassEnv emptyPatternEnv emptyPatternEnv emptyPatternEnv Map.empty Nothing [] Map.empty False [] emptySubst []

-- | Create initial state with config
initialInferStateWithConfig :: InferConfig -> InferState
initialInferStateWithConfig cfg = InferState 0 emptyEnv [] cfg emptyClassEnv emptyPatternEnv emptyPatternEnv emptyPatternEnv Map.empty Nothing [] Map.empty False [] emptySubst []

-- | Inference monad (with IO for potential future extensions)
type Infer a = ExceptT TypeError (StateT InferState IO) a

-- | Run type inference
runInfer :: Infer a -> InferState -> IO (Either TypeError a)
runInfer m st = evalStateT (runExceptT m) st

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

-- | Clear all accumulated warnings
clearWarnings :: Infer ()
clearWarnings = modify $ \st -> st { inferWarnings = [] }

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

-- | Get the current type environment
getEnv :: Infer TypeEnv
getEnv = inferEnv <$> get

-- | Set the type environment
setEnv :: TypeEnv -> Infer ()
setEnv env = modify $ \st -> st { inferEnv = env }

-- | Get the current pattern type environment
getPatternEnv :: Infer PatternTypeEnv
getPatternEnv = inferPatternEnv <$> get

-- | Get the current pattern function environment (for disambiguation)
getPatternFuncEnv :: Infer PatternTypeEnv
getPatternFuncEnv = inferPatternFuncEnv <$> get

-- | Get the pattern function structural-signature environment (paper PATFUN-DEF/PAT-APP)
getPatternFuncStructEnvI :: Infer PatternTypeEnv
getPatternFuncStructEnvI = inferPatternFuncStructEnv <$> get

-- | Get the current class environment
getClassEnv :: Infer ClassEnv
getClassEnv = inferClassEnv <$> get

-- | Resolve a constraint based on available instances
-- If the constraint type is a Tensor type and no instance exists for it,
-- try to use the element type's instance instead
-- | Resolve constraints in a TIExpr recursively
resolveConstraintsInTIExpr :: ClassEnv -> Subst -> TIExpr -> TIExpr
resolveConstraintsInTIExpr classEnv subst (TIExpr (Forall vars constraints ty) node) =
  let resolvedConstraints = map (resolveConstraintWithInstances classEnv subst) constraints
      resolvedNode = mapTIExprChildren (resolveConstraintsInTIExpr classEnv subst) node
  in TIExpr (Forall vars resolvedConstraints ty) resolvedNode

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
      hasVar c = any (not . Set.null . freeTyVars) (constraintTypes c)
      mentionsSig c =
        any (\t -> not (Set.null (freeTyVars t `Set.intersection` sigVars))) (constraintTypes c)
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
      matchTypesOneWay ps ts = foldM step emptySubst (zip ps ts)
        where step acc (p, t) = do
                s <- TU.matchOneWay (applySubst acc p) t
                return (composeSubst s acc)

      residual' = concatMap (reduceC 5 . applySubstConstraint finalSubst) residual
      missing = nub [ c | c <- residual', hasVar c, mentionsSig c, not (entailed c) ]
  when (not (null missing)) $
    throwError $ TE.MissingSignatureConstraint defName missing ctx

-- | Queue a matcher-definition hole admissibility check for the end of the
-- current top-level expression (see 'inferDeferredHoleChecks').
deferHoleCheck :: Type -> HoleCompShape -> String -> TypeErrorContext -> Infer ()
deferHoleCheck holeTy shape ppStr ctx =
  modify $ \s -> s { inferDeferredHoleChecks = (holeTy, shape, ppStr, ctx) : inferDeferredHoleChecks s }

-- | Drop all queued hole checks (called at the start of each top-level
-- expression, alongside clearConstraints).
clearDeferredHoleChecks :: Infer ()
clearDeferredHoleChecks = modify $ \s -> s { inferDeferredHoleChecks = [] }

-- | Run the queued matcher-hole admissibility checks against the final
-- substitution (paper PP-Con / Def 4.2(1a)).
flushDeferredHoleChecks :: Subst -> Infer ()
flushDeferredHoleChecks finalSubst = do
  checks <- gets inferDeferredHoleChecks
  clearDeferredHoleChecks
  classEnv <- getClassEnv
  mapM_ (runCheck classEnv) (reverse checks)
  where
    runCheck classEnv (holeTy0, shape, ppStr, ctx) = do
      holeTy <- applySubstWithConstraintsM finalSubst holeTy0
      case shape of
        HCSlot -> return ()
        HCBareVar compTy -> case holeTy of
          TVar _   -> return ()
          -- A function-typed hole admits a bare-variable matcher: function
          -- types can never own pattern constructors (the declaration
          -- grammar attaches them to type constructors only), so only
          -- value patterns / variables / wildcards can reach it.
          TFun _ _ -> return ()
          _ ->
                throwError $ TE.TypeMismatch
                  (TMatcherSlot holeTy holeTy)
                  compTy
                  ("the next matcher of clause `" ++ ppStr ++ "` is a bare-variable matcher, not structurally admissible at its hole's resolved type (paper PP-Con, Def 4.2(1a)); use a concrete matcher for that hole's type")
                  ctx
        HCShape inner0 -> case normalizeInductiveTypes (normalizeTensorType holeTy) of
          TVar _ -> return ()
          holeTyN -> do
            let inner = normalizeInductiveTypes (normalizeTensorType inner0)
            taup <- freshLeavesOf holeTyN
            -- The shape's variables are fresh copies (binding-independent),
            -- so a full unification realizes the one-way instance check while
            -- staying aware of the CAS tower (Term/Frac/Poly vs MathValue).
            case TU.unifyWithConstraints classEnv [] taup inner of
              Right _ -> return ()
              Left _  ->
                throwError $ TE.TypeMismatch
                  (TMatcherSlot holeTy holeTy)
                  (TMatcher inner)
                  ("the hole's next matcher is not structurally admissible at this hole (paper PP-Con, Def 4.2(1a)): its intrinsic type does not match the hole's structural index")
                  ctx

-- | The hole's structural index: same head as the (resolved) hole target
-- type, fresh leaves (paper PP-Con's fresh instantiation).
freshLeavesOf :: Type -> Infer Type
freshLeavesOf ty = case ty of
  TCollection _    -> TCollection <$> freshVar "leaf"
  TTuple ts        -> TTuple <$> mapM (const (freshVar "leaf")) ts
  TInductive n ts  -> TInductive n <$> mapM (const (freshVar "leaf")) ts
  TTensor _        -> TTensor <$> freshVar "leaf"
  THash _ _        -> THash <$> freshVar "leaf" <*> freshVar "leaf"
  _                -> return ty

-- | A copy of a type with all its free variables renamed fresh (binding
-- independence for the deferred structural check: the copy must not be
-- touched by the definition's ongoing unifications).
freshenTypeVars :: Type -> Infer Type
freshenTypeVars ty = do
  let vs = Set.toList (freeTyVars ty)
  pairs <- mapM (\v -> do { fv <- freshVar "fr"; return (v, fv) }) vs
  let s = foldr (\(v, fv) acc -> composeSubst (singletonSubst v fv) acc) emptySubst pairs
  return (applySubst s ty)

-- | Extend the environment temporarily
withEnv :: [(String, TypeScheme)] -> Infer a -> Infer a
withEnv bindings action = do
  oldEnv <- getEnv
  setEnv $ extendEnvMany (map (\(name, scheme) -> (stringToVar name, scheme)) bindings) oldEnv
  result <- action
  setEnv oldEnv
  return result

-- | Look up a variable's type
lookupVar :: String -> Infer Type
lookupVar name = do
  env <- getEnv
  case lookupEnv (stringToVar name) env of
    Just scheme -> do
      st <- get
      let (constraints, t, newCounter) = instantiate scheme (inferCounter st)
      -- Track constraints for type class resolution
      modify $ \s -> s { inferCounter = newCounter }
      addConstraints constraints
      return t
    Nothing -> do
      -- Check if this is a declared symbol
      st <- get
      case Map.lookup name (declaredSymbols st) of
        Just ty -> return ty  -- Return the declared type without warning
        Nothing -> do
          permissive <- isPermissive
          if permissive
            then do
              -- In permissive mode, treat as a warning and return a fresh type variable
              addWarning $ UnboundVariableWarning name emptyContext
              freshVar "unbound"
            else throwError $ UnboundVariable name emptyContext

-- | Lookup variable and return type with constraints
lookupVarWithConstraints :: String -> Infer (Type, [Constraint])
lookupVarWithConstraints name = do
  env <- getEnv
  case lookupEnv (stringToVar name) env of
    Just scheme -> do
      st <- get
      let (constraints, t, newCounter) = instantiate scheme (inferCounter st)
      -- Track constraints for type class resolution
      modify $ \s -> s { inferCounter = newCounter }
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
              addWarning $ UnboundVariableWarning name emptyContext
              t <- freshVar "unbound"
              return (t, [])
            else throwError $ UnboundVariable name emptyContext

-- | Unify two types
unifyTypes :: Type -> Type -> Infer Subst
unifyTypes t1 t2 = unifyTypesWithContext t1 t2 emptyContext

-- | Unify two types with context information
-- This now uses the accumulated constraints from the Infer monad to properly
-- handle constraint-aware unification (e.g., ensuring {Num a} a doesn't unify with Tensor b)
-- | Error message for a matcher-rigidity violation (TU.MatcherRigidity): two
-- distinct Matcher types may not be unified (see the TMatcher/TMatcher case
-- in Type.Unify for the soundness argument).
matcherRigidityMsg :: String
matcherRigidityMsg =
  "matcher types are rigid: a matcher value's structural capability is fixed by its definition, "
  ++ "so two different Matcher types never unify (e.g. `something' cannot be specialized to a "
  ++ "concrete matcher type by context).  Pass the intended matcher directly; matcher-consuming "
  ++ "function parameters should be slot-typed (m : MatcherSlot a a)"

-- | Bind a fresh inner variable to a type's Matcher component.  Matcher types
-- are rigid (the TMatcher/TMatcher case of unifyG), so an already-Matcher type
-- is destructured directly -- binding only the fresh variable, a pure
-- extraction, not a semantic merge of two matcher types; any other type
-- (a yet-undetermined variable, a slot, a tuple of matchers) is constrained by
-- unification with @Matcher fresh@ as before.
bindMatcherInner :: TypeErrorContext -> Type -> Type -> Infer Subst
bindMatcherInner ctx ty freshInner = case ty of
  TMatcher inner -> unifyTypesWithContext freshInner inner ctx
  _              -> unifyTypesWithContext ty (TMatcher freshInner) ctx

-- | The expression under any lambda wrappers (the body a parameterized
-- definition is desugared to).  Also sees through the letrec produced by the
-- algebraicDataMatcher desugaring (a self-referencing matcher literal).
rhsCore :: IExpr -> IExpr
rhsCore (ILambdaExpr _ _ e) = rhsCore e
rhsCore (ILetRecExpr [(PDPatVar v, e@(IMatcherExpr _))] (IVarExpr name))
  | v == stringToVar name   = e
rhsCore e                   = e

-- | T-MATCHER checking mode: unify the inferred and declared types of an
-- annotated matcher-literal definition (possibly parameterized, i.e.
-- lambda-wrapped).  This is the one context where two Matcher types may have
-- their parameters unified: the literal's structural capability is derived by
-- the clause checks at the declared type -- the matcher value is being
-- DEFINED here, not re-typed -- so matcher rigidity does not apply at the
-- result position of the definition's type.  Parameter positions of the TFun
-- spine are unified normally (they are slots or ordinary types).
unifyMatcherDefType :: [Constraint] -> Type -> Type -> TypeErrorContext -> Infer Subst
unifyMatcherDefType cs (TFun a1 r1) (TFun a2 r2) ctx = do
  s1 <- unifyTypesWithConstraints cs a1 a2 ctx
  r1' <- applySubstWithConstraintsM s1 r1
  r2' <- applySubstWithConstraintsM s1 r2
  s2 <- unifyMatcherDefType (map (applySubstConstraint s1) cs) r1' r2' ctx
  return (composeSubst s2 s1)
unifyMatcherDefType cs (TMatcher t1) (TMatcher t2) ctx =
  unifyTypesWithConstraints cs t1 t2 ctx
unifyMatcherDefType cs t1 t2 ctx = unifyTypesWithConstraints cs t1 t2 ctx

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
  case TU.unifyWithConstraints classEnv constraints t1' t2' of
    Right (s, _)  -> recordGlobalSubst s >> return s  -- Discard flag in basic unification
    Left err -> case err of
      TU.OccursCheck v t -> throwError $ OccursCheckError v t ctx
      TU.TypeMismatch a b -> throwError $ UnificationError a b ctx
      TU.MatcherRigidity a b -> throwError $ TE.TypeMismatch a b matcherRigidityMsg ctx

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
recordGlobalSubst :: Subst -> Infer ()
recordGlobalSubst s =
  modify $ \st -> st { inferGlobalSubst = composeSubst s (inferGlobalSubst st) }

-- | Unify two types with context, allowing Tensor a to unify with a
-- This is used only for top-level definitions with type annotations
-- According to type-tensor-simple.md: "Only for top-level tensor definitions, if Tensor a is unified with a, it becomes a."
unifyTypesWithTopLevel :: Type -> Type -> TypeErrorContext -> Infer Subst
unifyTypesWithTopLevel t1 t2 ctx = do
  (t1', t2') <- zonkPair t1 t2
  case TU.unifyWithTopLevel t1' t2' of
    Right s  -> recordGlobalSubst s >> return s
    Left err -> case err of
      TU.OccursCheck v t -> throwError $ OccursCheckError v t ctx
      TU.TypeMismatch a b -> throwError $ UnificationError a b ctx
      TU.MatcherRigidity a b -> throwError $ TE.TypeMismatch a b matcherRigidityMsg ctx

-- | Unify two types with constraint-aware handling
-- This is crucial for unifying types when type variables have constraints
-- (e.g., {Num t0}) - the constraint affects how Tensor types are unified
unifyTypesWithConstraints :: [Constraint] -> Type -> Type -> TypeErrorContext -> Infer Subst
unifyTypesWithConstraints constraints t1 t2 ctx = do
  classEnv <- getClassEnv
  (t1', t2') <- zonkPair t1 t2
  case TU.unifyWithConstraints classEnv constraints t1' t2' of
    Right (s, _)  -> recordGlobalSubst s >> return s  -- Discard flag in basic unification
    Left err -> case err of
      TU.OccursCheck v t -> throwError $ OccursCheckError v t ctx
      TU.TypeMismatch a b -> throwError $ UnificationError a b ctx
      TU.MatcherRigidity a b -> throwError $ TE.TypeMismatch a b matcherRigidityMsg ctx

-- | Infer type for constants
inferConstant :: ConstantExpr -> Infer Type
inferConstant c = case c of
  CharExpr _    -> return TChar
  StringExpr _  -> return TString
  BoolExpr _    -> return TBool
  IntegerExpr _ -> return TInt
  FloatExpr _   -> return TFloat
  -- something : Matcher a (polymorphic matcher that matches any type)
  SomethingExpr -> do
    elemType <- freshVar "a"
    return (TMatcher elemType)
  -- undefined has a fresh type variable (bottom-like, can be any type)
  UndefinedExpr -> freshVar "undefined"

--------------------------------------------------------------------------------
-- * Type Inference for IExpr
--------------------------------------------------------------------------------

-- | Helper: Create a TIExpr with a simple monomorphic type (no type variables, no constraints)
mkTIExpr :: Type -> TIExprNode -> TIExpr
mkTIExpr ty node = TIExpr (Forall [] [] ty) node

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
applySubstSchemeWithClassEnv classEnv (Subst m) (Forall vs cs t) =
  let m' = foldr Map.delete m vs
      -- Adjust substitution based on constraints
      m'' = adjustSubstForConstraints classEnv cs m'
      s' = Subst m''
  in Forall vs (map (applySubstConstraint s') cs) (applySubst s' t)
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
applySubstWithConstraintsM (Subst m) t = do
  classEnv <- getClassEnv
  constraints <- gets inferConstraints
  Subst gm <- gets inferGlobalSubst
  -- Adjust substitution based on constraints using the same logic as applySubstSchemeWithClassEnv
  let m' = adjustSubstForConstraints classEnv constraints m
      s' = Subst m'
      -- Also resolve through the global zonk substitution: with zonking, each
      -- unifier is a delta relative to the global state, so a locally threaded
      -- substitution alone may leave already-committed variables unresolved
      -- (and code that case-analyzes the applied type would misread them).
      gm' = adjustSubstForConstraints classEnv constraints gm
  return $ applySubst (Subst gm') (applySubst s' t)
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

  TIMatchExpr mode target matcher clauses ->
    TIMatchExpr mode
                (applySubstToTIExprWithClassEnv env s target)
                (applySubstToTIExprWithClassEnv env s matcher)
                (map (\(pat, body) -> (pat, applySubstToTIExprWithClassEnv env s body)) clauses)

  TIMatchAllExpr mode target matcher clauses ->
    TIMatchAllExpr mode
                   (applySubstToTIExprWithClassEnv env s target)
                   (applySubstToTIExprWithClassEnv env s matcher)
                   (map (\(pat, body) -> (pat, applySubstToTIExprWithClassEnv env s body)) clauses)

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
    let scheme = Forall [] [] ty
    return (TIExpr scheme (TIConstantExpr c), emptySubst)
  
  -- Variables
  IVarExpr name -> do
    -- Variables starting with ":::" are treated as Any type without warning
    if ":::" `isPrefixOf` name
      then do
        let scheme = Forall [] [] TAny
        return (TIExpr scheme (TIVarExpr name), emptySubst)
      else do
        (ty, constraints) <- lookupVarWithConstraints name
        let scheme = Forall [] constraints ty
        return (TIExpr scheme (TIVarExpr name), emptySubst)
  
  -- Tuples
  ITupleExpr elems -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    case elems of
      [] -> do
        -- Empty tuple: unit type ()
        let scheme = Forall [] [] (TTuple [])
        return (TIExpr scheme (TITupleExpr []), emptySubst)
      [single] -> do
        -- Single element tuple: same as the element itself (parentheses are just grouping)
        inferIExprWithContext single exprCtx
      _ -> do
        results <- mapM (\e -> inferIExprWithContext e exprCtx) elems
        let elemTIExprs = map fst results
            elemTypes = map (tiExprType . fst) results
            s = foldr composeSubst emptySubst (map snd results)
        
        -- Check if all elements are Matcher types
        -- If so, return Matcher (Tuple ...) instead of (Matcher ..., Matcher ...)
        appliedElemTypes <- mapM (applySubstWithConstraintsM s) elemTypes
        let matcherTypes = catMaybes (map extractMatcherType appliedElemTypes)
        
        if length matcherTypes == length appliedElemTypes && not (null appliedElemTypes)
          then do
            -- All elements are matchers: return Matcher (Tuple ...)
            let tupleType = TTuple matcherTypes
                resultType = TMatcher tupleType
                scheme = Forall [] [] resultType
            return (TIExpr scheme (TITupleExpr elemTIExprs), s)
          else do
            -- Not all elements are matchers: return regular tuple
            let resultType = TTuple appliedElemTypes
                scheme = Forall [] [] resultType
            return (TIExpr scheme (TITupleExpr elemTIExprs), s)
        where
          -- Extract the inner type from Matcher a -> Just a, otherwise Nothing
          extractMatcherType :: Type -> Maybe Type
          extractMatcherType (TMatcher t) = Just t
          extractMatcherType _ = Nothing
  
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
    (bodyTIExpr, s) <- withEnv (map toScheme bindings) $ inferIExprWithContext body exprCtx
    let bodyType = tiExprType bodyTIExpr
    finalArgTypes <- mapM (applySubstWithConstraintsM s) argTypes
    let funType = foldr TFun bodyType finalArgTypes
    return (mkTIExpr funType (TILambdaExpr mVar params bodyTIExpr), s)
    where
      makeBinding var t = (extractNameFromVar var, t)
      toScheme (name, t) = (name, Forall [] [] t)
  
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
    (thenTI, s3) <- inferIExprWithContext thenExpr exprCtx
    (elseTI, s4) <- inferIExprWithContext elseExpr exprCtx
    let thenType = tiExprType thenTI
        elseType = tiExprType elseTI
    thenType' <- applySubstWithConstraintsM s4 thenType
    s5 <- unifyTypesWithContext thenType' elseType exprCtx
    let finalS = foldr composeSubst emptySubst [s5, s4, s3, s12]
    resultType <- applySubstWithConstraintsM finalS elseType
    return (mkTIExpr resultType (TIIfExpr condTI thenTI elseTI), finalS)
  
  -- Let expression
  ILetExpr bindings body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    env <- getEnv
    (bindingTIs, extendedEnv, s1) <- inferIBindingsWithContext bindings env emptySubst exprCtx
    (bodyTI, s2) <- withEnv extendedEnv $ inferIExprWithContext body exprCtx
    let bodyType = tiExprType bodyTI
        finalS = composeSubst s2 s1
    resultType <- applySubstWithConstraintsM finalS bodyType
    return (mkTIExpr resultType (TILetExpr bindingTIs bodyTI), finalS)
  
  -- LetRec expression
  ILetRecExpr bindings body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    env <- getEnv
    (bindingTIs, extendedEnv, s1) <- inferIRecBindingsWithContext bindings env emptySubst exprCtx
    (bodyTI, s2) <- withEnv extendedEnv $ inferIExprWithContext body exprCtx
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
        -- Instantiate the type scheme
        st <- get
        let (_constraints, constructorType, newCounter) = instantiate scheme (inferCounter st)
        modify $ \s -> s { inferCounter = newCounter }
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
    let exprCtx = withExpr (prettyStr expr) ctx
    -- Paper Def 4.2(2) (Reachability / catch-all): a matcher must contain a catch-all clause
    -- `$ as M with $tgt -> N` (its primitive-pattern pattern is a bare hole `$`), so that
    -- variable and wildcard patterns are handled (delegating, typically to `something`) rather
    -- than getting stuck.  (Coverage, Def 4.2(3), is not enforced — it needs the full set of
    -- pattern constructors for the matched type, which Egison does not require to be declared;
    -- see design/matcher-slot.md, Stage 4, gap C.)
    if any (\(pp, _, _) -> case pp of PPPatVar -> True; _ -> False) patDefs
      then return ()
      else throwError $ TE.TypeMismatch
             (TMatcher (TVar (TyVar "a")))
             (TMatcher (TVar (TyVar "a")))
             "a `matcher` must contain a catch-all clause `$ as <matcher> with $tgt -> ...` (e.g. `$ as something`) so that variable and wildcard patterns are handled"
             exprCtx
    -- Infer type of each pattern definition (matcher clause)
    -- Each clause has: (PrimitivePatPattern, nextMatcherExpr, [(primitiveDataPat, targetExpr)])
    -- Mark that we are inside a matcher body.  Match-sites nested here are still fully checked
    -- for admissibility (T-MATCHALL); this flag only suppresses matcher-Coverage warnings for
    -- the nested / generated matchers a body may build (see `inferInMatcherBody`).
    savedInMB <- gets inferInMatcherBody
    modify $ \st -> st { inferInMatcherBody = True }
    results <- mapM (inferPatternDef exprCtx) patDefs
    modify $ \st -> st { inferInMatcherBody = savedInMB }
    
    -- Collect TIPatternDefs and substitutions
    let tiPatDefs = map fst results
        substs = concatMap (snd . snd) results  -- Extract [Subst] from (TIPatternDef, (Type, [Subst]))
        finalSubst = foldr composeSubst emptySubst substs
    
    -- All clauses should agree on the matched type
    -- Unify all matched types from each pattern definition
    matchedTypes <- mapM (\(_, (ty, _)) -> applySubstWithConstraintsM finalSubst ty) results
    (matchedTy, s_matched) <- case matchedTypes of
      [] -> do
        ty <- freshVar "matched"
        return (ty, emptySubst)
      (firstTy:restTys) -> do
        -- Unify all matched types
        s <- foldM (\accS ty -> do
            firstTy' <- applySubstWithConstraintsM accS firstTy
            ty' <- applySubstWithConstraintsM accS ty
            s' <- unifyTypesWithContext firstTy' ty' exprCtx
            return $ composeSubst s' accS
          ) emptySubst restTys
        resultTy <- applySubstWithConstraintsM s firstTy
        return (resultTy, s)
    
    let allSubst = composeSubst s_matched finalSubst
    -- Paper Def 4.2(3) Coverage (warning-level diagnostic, non-fatal): report any pattern
    -- constructor of the matched type that has no general clause `c $..$` (such a pattern
    -- would get stuck at runtime).  Coverage holds vacuously for a polymorphic matched type
    -- or one with no inductive pattern declaration (no constructors).  Suppressed inside a
    -- matcher body (generated / nested matchers).
    covOn <- cfgMatcherConsistencyWarnings <$> gets inferConfig
    inMB <- gets inferInMatcherBody
    matchedTyFinal <- applySubstWithConstraintsM allSubst matchedTy
    case (covOn && not inMB, matcherTypeHead matchedTyFinal) of
      (True, Just hd) -> do
        patEnv <- getPatternEnv
        let allCtors     = [ name | (name, sch) <- patternEnvToList patEnv, ctorResultHead sch == Just hd ]
            coveredCtors  = [ c | (pp, _, _) <- patDefs, Just c <- [generalClauseCtor pp] ]
            missing       = filter (`notElem` coveredCtors) allCtors
        if not (null allCtors) && not (null missing)
          then addWarning $ MatcherCoverageWarning matchedTyFinal missing exprCtx
          else return ()
      _ -> return ()
    -- Arm exhaustiveness (paper Def 4.2(1c), part of matcher consistency): once a clause's
    -- pp matches the pattern, the target is matched against that clause's data-pattern arms
    -- alone, and a miss there is a runtime failure ("Primitive data pattern match failed"),
    -- not a graceful backtrack — an arm that cannot decompose its target must say so by
    -- returning [].  Reject any clause whose arm set is not syntactically exhaustive (a
    -- conservative approximation of Def 4.2(1c); see pdArmsExhaustive); the standard-library
    -- convention of a final `| _ -> []` / `| $tgt -> ...` arm always passes.  Unlike the
    -- Coverage diagnostic this is an ordinary type error, not gated behind
    -- --matcher-consistency-warnings; it stays suppressed inside matcher bodies
    -- (generated / nested matchers), as for Coverage.
    when (not inMB) $
      mapM_ (\(pp, _, dataClauses) ->
               when (not (pdArmsExhaustive (map fst dataClauses))) $
                 throwError $ MatcherDataArmsNotExhaustive (prettyStr pp) matchedTyFinal exprCtx)
            patDefs
    return (mkTIExpr (TMatcher matchedTy) (TIMatcherExpr tiPatDefs), allSubst)
    where
      -- Infer a single pattern definition (matcher clause)
      -- Returns (TIPatternDef, (matched type, [substitutions]))
      inferPatternDef :: TypeErrorContext -> IPatternDef -> Infer (TIPatternDef, (Type, [Subst]))
      inferPatternDef ctx (ppPat, nextMatcherExpr, dataClauses) = do
        -- Infer the type of next matcher expression
        -- It should be a Matcher type (possibly Matcher of tuple, like Matcher (a, b))
        -- Note: (integer, integer) is inferred as Matcher (Integer, Integer), not (Matcher Integer, Matcher Integer)
        (nextMatcherTI, s1) <- inferIExprWithContext nextMatcherExpr ctx
        let nextMatcherType = tiExprType nextMatcherTI
        
        -- nextMatcherType must be a Matcher type
        -- Constrain it to `Matcher inner` / extract its inner type.  Matcher
        -- types are rigid (the TMatcher/TMatcher case of unifyG), so an
        -- already-Matcher type is destructured directly -- binding only the
        -- fresh inner variable, a pure extraction, not a semantic merge of two
        -- matcher types -- instead of being unified with `Matcher fresh`.
        matcherInnerTy <- freshVar "matcherInner"
        nextMatcherType' <- applySubstWithConstraintsM s1 nextMatcherType
        s1' <- bindMatcherInner ctx nextMatcherType' matcherInnerTy
        nextMatcherType'' <- applySubstWithConstraintsM s1' nextMatcherType
        
        -- Infer PrimitivePatPattern type to get matched type, pattern hole types, and variable bindings
        (matchedType, patternHoleTypes, ppBindings, s_pp) <- inferPrimitivePatPattern ppPat ctx
        let s1'' = composeSubst s_pp s1'
        matchedType' <- applySubstWithConstraintsM s1'' matchedType
        let -- Apply substitution to variable bindings
            ppBindings' = [(var, applySubstScheme s1'' scheme) | (var, scheme) <- ppBindings]

        -- Apply substitution to pattern hole types (keep as inner types)
        patternHoleTypes' <- mapM (applySubstWithConstraintsM s1'') patternHoleTypes

        -- Extract inner type(s) from next matcher type
        -- If multiple pattern holes, combine them into a tuple to match ITupleExpr behavior
        nextMatcherInnerTypes <- extractInnerTypesFromMatcher nextMatcherType'' (length patternHoleTypes') ctx

        -- Paper PP-Con / COERCE-MATCHER-TO-SLOT (Def 4.2(1a)) — matcher-definition-time structural
        -- admissibility, a HARD ERROR.  In the paper each hole gives a pair (τ_p ▷ τ_t): the target
        -- τ_t is the declared argument type σ_l (= `holeTy` below) and the structural index τ_p is a
        -- *fresh instantiation* of σ_l (PP-Con's device — same head, fresh leaves; NO fusion
        -- τ_p = τ_t, NO separate fresh_rename, uniform with PAT-CON at a match site).  The next
        -- matcher is consumed at the slot @MatcherSlot τ_p σ_l@, whose structural half is the
        -- one-way match τ_m ⊑ τ_p: a bare-variable matcher fills only a variable-headed or
        -- function-typed hole (functions admit no pattern constructors); a constructor-/concrete-
        -- headed hole rejects it (the paper's `weird`: it cannot decompose the hole, so a pattern
        -- routed through it gets stuck).
        --
        -- The test runs in TWO STAGES, because a hole's target type may be resolved only by the
        -- enclosing definition's final substitution (e.g. the annotation `: Matcher [Integer]`
        -- pinning the matched variable):
        --   * EAGER (here): the literal `something` (T-SOME) at a hole whose type is already
        --     constructor-/concrete-headed — rejected immediately, with the clause in context.
        --     A slot-typed parameter (`m : MatcherSlot a a`) or a structured next matcher
        --     (`list m`/`multiset m`) is never literal `something`, so it is not flagged here;
        --     an undetermined parameter's inner is fixed by the target unification below
        --     (`checkPatternHoleConsistency`) and must not be rejected prematurely.
        --   * DEFERRED (recorded below, run by `flushDeferredHoleChecks` at the end of the
        --     top-level expression): the general check at the RESOLVED hole types — components
        --     classified as slot / bare-variable value / shaped value (`HoleCompShape`), the
        --     shaped case checked against PP-Con's fresh-leaves structural index.
        -- (Coverage, Def 4.2(3), stays an opt-in warning — partial matchers are intentional.)
        let comps = case nextMatcherExpr of { ITupleExpr es -> es; e -> [e] }
        mapM_ (\(holeTy, comp) -> case (holeTy, comp) of
                 (TVar _, _)                      -> return ()
                 (TFun _ _, _)                    -> return ()
                 (_, IConstantExpr SomethingExpr) ->
                   throwError $ TE.TypeMismatch
                     (TMatcherSlot holeTy holeTy)
                     (TMatcher (TVar (TyVar "a")))
                     ("the next matcher `" ++ prettyStr comp ++ "` is a bare-variable matcher, " ++
                      "not structurally admissible at a constructor-headed hole (paper PP-Con, " ++
                      "Def 4.2(1a)); use a concrete matcher for that hole's type")
                     ctx
                 _                                -> return ())
              (zip patternHoleTypes' comps)
        -- Deferred (post-annotation) admissibility: a hole's target type may
        -- be pinned only by the enclosing definition's final substitution
        -- (e.g. the annotation `: Matcher [Integer]` resolving the matched
        -- variable), so the structural check above can be vacuous (TVar) at
        -- this point and yet fail at the resolved type.  Record each
        -- constructor-/tuple-pp hole's (target type, next-matcher shape) and
        -- re-check at the end of the top-level expression
        -- (flushDeferredHoleChecks).  PP-Hole (a bare `$` pp, the catch-all)
        -- gives its hole a FRESH VARIABLE structural index regardless of the
        -- target type, so nothing is deferred for it.
        let compTIs = case tiExprNode nextMatcherTI of
              TITupleExpr es -> es
              _              -> [nextMatcherTI]
        case ppPat of
          PPPatVar -> return ()
          _ | length compTIs == length patternHoleTypes' -> do
                -- Classify each component from its own inferred (intrinsic)
                -- type — BEFORE the hole/target unification ties it to the
                -- hole — so a slot-typed parameter is recognized as a slot
                -- and a bare-variable matcher value as bare.
                compTys <- mapM (applySubstWithConstraintsM s1'' . tiExprType) compTIs
                mapM_ (\(holeTy, compTI, compTy) -> do
                  mshape <- case (tiExprNode compTI, compTy) of
                    (_, TMatcherSlot _ _) -> return (Just HCSlot)
                    -- A bare-variable matcher VALUE: the literal `something`,
                    -- or a variable referring to one (eq, a polymorphic
                    -- matcher alias).  Only these are classified bare — an
                    -- APPLICATION whose result type is still an unresolved
                    -- variable is not (its shape follows from its own
                    -- definition's checking) and is skipped below.
                    (TIConstantExpr SomethingExpr, _) ->
                      return (Just (HCBareVar compTy))
                    (TIVarExpr vname, TMatcher (TVar _)) -> do
                      envHere <- getEnv
                      case lookupEnv (stringToVar vname) envHere of
                        -- Only a GENERALIZED bare matcher value (its scheme
                        -- quantifies the Matcher parameter: eq, a polymorphic
                        -- alias) is bare.  A monomorphically bound variable
                        -- (a lambda parameter, whose type is still being
                        -- discovered and whose slot-ness is revealed by the
                        -- signature) is the parameter route — checked at its
                        -- call sites by COERCE-MATCHER-TO-SLOT, not here.
                        Just (Forall qs _ (TMatcher (TVar v)))
                          | v `elem` qs -> return (Just (HCBareVar compTy))
                        _ -> return Nothing
                    (_, TMatcher (TVar _)) -> return Nothing
                    (_, TMatcher inner) -> Just . HCShape <$> freshenTypeVars inner
                    _ -> return Nothing
                  mapM_ (\shape -> deferHoleCheck holeTy shape (prettyStr ppPat) ctx) mshape)
                  (zip3 patternHoleTypes' compTIs compTys)
            | otherwise -> return ()  -- a single matcher covering several holes: skip

        -- Unify pattern hole types (inner types) with next matcher inner types
        s_unify <- checkPatternHoleConsistency patternHoleTypes' nextMatcherInnerTypes ctx
        let s1''' = composeSubst s_unify s1''
        
        -- Infer the type of data clauses with pp variables in scope, building
        -- the typed arms (TIBindingExpr) in the SAME pass that checks them.
        -- A single inference per arm matters: the arm TIExprs stored in the
        -- TIMatcherExpr node and the constraints recorded in the inference
        -- state must come from the same instantiation, and the clause
        -- substitutions must be returned, so that the definition's final
        -- substitution can resolve the arm nodes' constraint variables
        -- (e.g. the `a` of `==`'s {Eq a}).  A separate re-inference used to
        -- leave orphaned constraint variables behind, and TypeClassExpand
        -- then emitted unbound dictionary references for the arm's method
        -- calls (the method name leaked into evaluation as a string).
        dataClauseResults <- withEnv ppBindings' $
          mapM (inferDataClauseWithCheck ctx nextMatcherInnerTypes matchedType') dataClauses
        let dataClauseTIs = map fst dataClauseResults
            s2 = foldr composeSubst emptySubst (map snd dataClauseResults)

        let tiPatDef = (ppPat, nextMatcherTI, dataClauseTIs)

        return (tiPatDef, (matchedType', [s1''', s2]))
      
      -- Infer PrimitivePatPattern type
      -- Returns (matched type, pattern hole types, variable bindings, substitution)
      -- Pattern hole types are the inner types (without TMatcher wrapper)
      -- The caller should wrap them with TMatcher when unifying with next matcher types
      -- Variable bindings are for PPValuePat variables (#$val)
      -- Note: Pattern hole types are determined by the pattern constructor, not by external context
      inferPrimitivePatPattern :: PrimitivePatPattern -> TypeErrorContext -> Infer (Type, [Type], [(String, TypeScheme)], Subst)
      inferPrimitivePatPattern ppPat ctx = case ppPat of
        PPWildCard -> do
          -- Wildcard pattern: no pattern holes, no bindings
          matchedTy <- freshVar "matched"
          return (matchedTy, [], [], emptySubst)
        
        PPPatVar -> do
          -- Pattern variable ($): one pattern hole, no binding
          -- Returns the matched type as the pattern hole type
          -- The caller will wrap it with TMatcher when unifying with next matcher type
          matchedTy <- freshVar "matched"
          return (matchedTy, [matchedTy], [], emptySubst)
        
        PPValuePat var -> do
          -- Value pattern (#$val): no pattern holes, binds variable to matched type
          matchedTy <- freshVar "matched"
          let binding = (var, Forall [] [] matchedTy)
          return (matchedTy, [], [binding], emptySubst)
        
        PPTuplePat ppPats -> do
          -- Tuple pattern: ($p1, $p2, ...)
          -- Recursively infer each sub-pattern
          results <- mapM (\pp -> inferPrimitivePatPattern pp ctx) ppPats
          let matchedTypes = [mt | (mt, _, _, _) <- results]
              patternHoleLists = [phs | (_, phs, _, _) <- results]
              bindingLists = [bs | (_, _, bs, _) <- results]
              substs = [s | (_, _, _, s) <- results]
              allPatternHoles = concat patternHoleLists
              allBindings = concat bindingLists
              finalSubst = foldr composeSubst emptySubst substs
          
          -- Matched type is tuple of matched types
          matchedTypes' <- mapM (applySubstWithConstraintsM finalSubst) matchedTypes
          allPatternHoles' <- mapM (applySubstWithConstraintsM finalSubst) allPatternHoles
          let matchedTy = TTuple matchedTypes'
          return (matchedTy, allPatternHoles', allBindings, finalSubst)
        
        PPInductivePat name ppPats -> do
          -- Inductive pattern: look up pattern constructor type from pattern environment
          patternEnv <- getPatternEnv
          case lookupPatternEnv name patternEnv of
            Just scheme -> do
              -- Found in pattern environment: use the declared type
              st <- get
              let (_constraints, ctorType, newCounter) = instantiate scheme (inferCounter st)
              modify $ \s -> s { inferCounter = newCounter }
              
              -- Pattern constructor type: arg1 -> arg2 -> ... -> resultType
              -- Extract argument types and result type
              let (argTypes, resultType) = extractFunctionArgs ctorType
              
              -- Check argument count matches
              if length argTypes /= length ppPats
                then throwError $ TE.TypeMismatch
                       (foldr TFun resultType (replicate (length ppPats) (TVar (TyVar "a"))))
                       ctorType
                       ("Pattern constructor " ++ name ++ " expects " ++ show (length argTypes) 
                        ++ " arguments, but got " ++ show (length ppPats))
                       ctx
                else do
                  -- Recursively infer each sub-pattern
                  results <- mapM (\pp -> inferPrimitivePatPattern pp ctx) ppPats
                  
                  let matchedTypes = [mt | (mt, _, _, _) <- results]
                      patternHoleLists = [phs | (_, phs, _, _) <- results]
                      bindingLists = [bs | (_, _, bs, _) <- results]
                      substs = [s | (_, _, _, s) <- results]
                      allPatternHoles = concat patternHoleLists
                      allBindings = concat bindingLists
                      s = foldr composeSubst emptySubst substs
                  
                  -- Verify that inferred matched types match expected argument types
                  -- Extract inner types from Matcher types in argTypes
                  let expectedMatchedTypes = map (\ty -> case ty of
                        TMatcher inner -> inner
                        _ -> ty) argTypes
                  s' <- foldM (\accS (inferredTy, expectedTy) -> do
                      inferredTy' <- applySubstWithConstraintsM accS inferredTy
                      expectedTy' <- applySubstWithConstraintsM accS expectedTy
                      s'' <- unifyTypesWithContext inferredTy' expectedTy' ctx
                      return $ composeSubst s'' accS
                    ) s (zip matchedTypes expectedMatchedTypes)

                  resultType' <- applySubstWithConstraintsM s' resultType
                  allPatternHoles' <- mapM (applySubstWithConstraintsM s') allPatternHoles
                  return (resultType', allPatternHoles', allBindings, s')
            
            Nothing -> do
              -- Not found in pattern environment: use generic inference
              -- This is for backward compatibility
              results <- mapM (\pp -> inferPrimitivePatPattern pp ctx) ppPats
              let matchedTypes = [mt | (mt, _, _, _) <- results]
                  patternHoleLists = [phs | (_, phs, _, _) <- results]
                  bindingLists = [bs | (_, _, bs, _) <- results]
                  substs = [s | (_, _, _, s) <- results]
                  allPatternHoles = concat patternHoleLists
                  allBindings = concat bindingLists
                  s = foldr composeSubst emptySubst substs
              
              -- Result type is inductive type
              matchedTypes' <- mapM (applySubstWithConstraintsM s) matchedTypes
              allPatternHoles' <- mapM (applySubstWithConstraintsM s) allPatternHoles
              let resultType = TInductive name matchedTypes'
              return (resultType, allPatternHoles', allBindings, s)
      
      -- Extract function argument types and result type
      -- e.g., a -> b -> c -> d  =>  ([a, b, c], d)
      extractFunctionArgs :: Type -> ([Type], Type)
      extractFunctionArgs (TFun arg rest) = 
        let (args, result) = extractFunctionArgs rest
        in (arg : args, result)
      extractFunctionArgs t = ([], t)
      
      -- Extract matched type from Matcher type
      -- Check consistency between pattern hole types and next matcher types
      checkPatternHoleConsistency :: [Type] -> [Type] -> TypeErrorContext -> Infer Subst
      checkPatternHoleConsistency [] [] _ctx = return emptySubst
      checkPatternHoleConsistency patternHoles nextMatchers ctx
        | length patternHoles /= length nextMatchers = 
            throwError $ TE.TypeMismatch
              (TTuple nextMatchers)
              (TTuple patternHoles)
              ("Inconsistent number of pattern holes (" ++ show (length patternHoles) 
               ++ ") and next matchers (" ++ show (length nextMatchers) ++ ")")
              ctx
        | otherwise = do
            -- Unify each pattern hole type with corresponding next matcher type
            foldM (\accS (holeTy, matcherTy) -> do
                holeTy' <- applySubstWithConstraintsM accS holeTy
                matcherTy' <- applySubstWithConstraintsM accS matcherTy
                s <- unifyTypesWithContext holeTy' matcherTy' ctx
                return $ composeSubst s accS
              ) emptySubst (zip patternHoles nextMatchers)
      
      -- Extract inner types from next matcher type
      -- Given Matcher a, returns [a]
      -- Given Matcher (a, b, ...) and n pattern holes, returns [a, b, ...] if n > 1, or [(a, b, ...)] if n = 1
      -- Special case: (Matcher a, Matcher b, ...) should be converted to Matcher (a, b, ...) first
      -- Note: Even when numHoles = 0, we extract inner types to detect mismatches in checkPatternHoleConsistency
      extractInnerTypesFromMatcher :: Type -> Int -> TypeErrorContext -> Infer [Type]
      extractInnerTypesFromMatcher matcherType numHoles ctx = case numHoles of
        0 -> case matcherType of
          -- No pattern holes, but extract inner type to allow error detection
          TMatcher innerType -> return [innerType]
          -- A MatcherSlot used as a next-matcher: its target component is the inner type.
          TMatcherSlot _ tt -> return [tt]
          TTuple types -> do
            let matcherInners = mapM extractMatcherInner types
            case matcherInners of
              Just inners -> return inners
              Nothing -> return []  -- Not matcher types, return empty
          _ -> return []  -- Not a matcher type
        1 -> case matcherType of
          TMatcher innerType -> return [innerType]  -- Single hole: return inner type as-is
          TMatcherSlot _ tt -> return [tt]          -- A slot next-matcher: target component
          -- Special case: (Matcher a, Matcher b, ...) from ITupleExpr that failed to convert
          -- This can happen when matcher parameters are used before ITupleExpr conversion
          TTuple types -> do
            let matcherInners = mapM extractMatcherInner types
            case matcherInners of
              Just inners -> return [TTuple inners]  -- Return as single tuple type
              Nothing -> throwError $ TE.TypeMismatch
                           (TMatcher (TVar (TyVar "a")))
                           matcherType
                           "Expected Matcher type or tuple of Matcher types"
                           ctx
          _ -> throwError $ TE.TypeMismatch
                 (TMatcher (TVar (TyVar "a")))
                 matcherType
                 "Expected Matcher type"
                 ctx
        n -> case matcherType of
          -- Multiple holes: expect Matcher (tuple) and extract each element
          TMatcher (TTuple innerTypes) ->
            if length innerTypes == n
              then return innerTypes
              else throwError $ TE.TypeMismatch
                     (TMatcher (TTuple (replicate n (TVar (TyVar "a")))))
                     matcherType
                     ("Expected Matcher with tuple of " ++ show n ++ " elements, but got " ++ show (length innerTypes))
                     ctx
          -- A product MatcherSlot used as a next-matcher: target tuple component.
          TMatcherSlot _ (TTuple innerTypes) ->
            if length innerTypes == n
              then return innerTypes
              else throwError $ TE.TypeMismatch
                     (TMatcher (TTuple (replicate n (TVar (TyVar "a")))))
                     matcherType
                     ("Expected Matcher with tuple of " ++ show n ++ " elements, but got " ++ show (length innerTypes))
                     ctx
          -- Special case: (Matcher a, Matcher b, ...) - extract inner types directly
          TTuple types -> do
            let matcherInners = mapM extractMatcherInner types
            case matcherInners of
              Just inners | length inners == n -> return inners
              _ -> throwError $ TE.TypeMismatch
                     (TMatcher (TTuple (replicate n (TVar (TyVar "a")))))
                     matcherType
                     "Expected tuple of Matcher types with correct count"
                     ctx
          _ -> throwError $ TE.TypeMismatch
                 (TMatcher (TTuple (replicate n (TVar (TyVar "a")))))
                 matcherType
                 ("Expected Matcher of tuple with " ++ show n ++ " elements")
                 ctx
      
      -- Helper: Extract inner type from Matcher a -> Just a, otherwise Nothing
      extractMatcherInner :: Type -> Maybe Type
      extractMatcherInner (TMatcher t) = Just t
      extractMatcherInner (TMatcherSlot _ tt) = Just tt
      extractMatcherInner _ = Nothing
      
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
        (targetTI, s1) <- withEnv bindings $ inferIExprWithContext targetExpr ctx
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
          return (expectedType, [(varName, Forall [] [] expectedType)], emptySubst)
        
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
              st <- get
              let (_constraints, ctorType, newCounter) = instantiate scheme (inferCounter st)
              modify $ \s -> s { inferCounter = newCounter }
              
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
  IMatchExpr mode target matcher clauses -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (targetTI, s1) <- inferIExprWithContext target exprCtx
    (matcherTI, s2) <- inferIExprWithContext matcher exprCtx
    let targetType = tiExprType targetTI
        matcherType = tiExprType matcherTI

    -- Matcher should be TMatcher a, (TMatcher a, ...) which becomes TMatcher (a, ...), or a
    -- MatcherSlot (a committed parameter / a stdlib slot-typed matcher).
    let s12 = composeSubst s2 s1
    -- Paper T-MATCHALL / COERCE-MATCHER-TO-SLOT: derive each clause pattern's structural type
    -- τ_p independently and require the matcher to fill MatcherSlot τ_p τ_t.  Run on the raw
    -- matcher type (before the Matcher/tuple normalization below), so an unannotated matcher
    -- parameter — a bare type variable — is committed to a slot type rather than forced into
    -- `Matcher <var>` (indistinguishable from `something`).  Rejects structurally inadmissible
    -- matchers (e.g. `something` at a constructor or concrete-value pattern, even nested).
    sAdm <- checkMatcherAdmissibility exprCtx matcherType targetType clauses s12
    appliedMatcherType <- applySubstWithConstraintsM sAdm matcherType

    -- Normalize the matcher type to extract the matched inner type.
    (_normalizedMatcherType, matchedInnerType, s3) <- case appliedMatcherType of
      TTuple elemTypes -> do
        -- Each tuple element is a Matcher (extract its inner) or a MatcherSlot (its target).
        (finalInnerTypes, s_elems) <- foldM (\(acc, accS) elemTy -> do
          appliedElemTy <- applySubstWithConstraintsM accS elemTy
          case appliedElemTy of
            TMatcherSlot _ tt -> return (acc ++ [tt], accS)
            _ -> do
              innerTy <- freshVar "matched"
              s' <- bindMatcherInner exprCtx appliedElemTy innerTy
              innerTy' <- applySubstWithConstraintsM s' innerTy
              return (acc ++ [innerTy'], composeSubst s' accS)
          ) ([], emptySubst) elemTypes
        -- The tuple as a whole becomes Matcher (a1, a2, ...)
        let tupleInnerType = TTuple finalInnerTypes
        return (TMatcher tupleInnerType, tupleInnerType, s_elems)
      -- A MatcherSlot (committed parameter, or a stdlib slot-typed matcher): the matched inner
      -- type is its target component.
      TMatcherSlot _ tt -> return (TMatcher tt, tt, emptySubst)
      _ -> do
        -- Single matcher: TMatcher a
        matchedTy <- freshVar "matched"
        s' <- bindMatcherInner exprCtx appliedMatcherType matchedTy
        finalMatchedTy <- applySubstWithConstraintsM s' matchedTy
        return (TMatcher finalMatchedTy, finalMatchedTy, s')

    let s123 = composeSubst s3 sAdm
    targetType' <- applySubstWithConstraintsM s123 targetType
    matchedInnerType' <- applySubstWithConstraintsM s123 matchedInnerType
    s4 <- unifyTypesWithContext targetType' matchedInnerType' exprCtx
    
    -- Infer match clauses result type
    let s1234 = composeSubst s4 sAdm
    case clauses of
      [] -> do
        -- No clauses: this should not happen, but handle gracefully
        resultTy <- freshVar "matchResult"
        targetTI' <- applySubstToTIExprM s1234 targetTI
        matcherTI' <- applySubstToTIExprM s1234 matcherTI
        resultTy' <- applySubstWithConstraintsM s1234 resultTy
        return (mkTIExpr resultTy' (TIMatchExpr mode targetTI' matcherTI' []), s1234)
      _ -> do
        -- Infer type of each clause and unify them
        matchedInnerType' <- applySubstWithConstraintsM s1234 matchedInnerType
        (resultTy, clauseTIs, clauseSubst) <- inferMatchClauses exprCtx matchedInnerType' clauses s1234
        let finalS = composeSubst clauseSubst s1234
        targetTI' <- applySubstToTIExprM finalS targetTI
        matcherTI' <- applySubstToTIExprM finalS matcherTI
        resultTy' <- applySubstWithConstraintsM finalS resultTy
        return (mkTIExpr resultTy' (TIMatchExpr mode targetTI' matcherTI' clauseTIs), finalS)
  
  -- MatchAll expressions
  IMatchAllExpr mode target matcher clauses -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (targetTI, s1) <- inferIExprWithContext target exprCtx
    (matcherTI, s2) <- inferIExprWithContext matcher exprCtx
    let targetType = tiExprType targetTI
        matcherType = tiExprType matcherTI
    
    -- Matcher should be TMatcher a, (TMatcher a, ...) which becomes TMatcher (a, ...), or a
    -- MatcherSlot (a committed parameter / a stdlib slot-typed matcher).
    let s12 = composeSubst s2 s1
    -- Paper T-MATCHALL / COERCE-MATCHER-TO-SLOT: derive each clause pattern's structural type
    -- τ_p independently and require the matcher to fill MatcherSlot τ_p τ_t.  Run on the raw
    -- matcher type (before the Matcher/tuple normalization below), so an unannotated matcher
    -- parameter — a bare type variable — is committed to a slot type rather than forced into
    -- `Matcher <var>` (indistinguishable from `something`).  Rejects structurally inadmissible
    -- matchers (e.g. `something` at a constructor or concrete-value pattern, even nested).
    sAdm <- checkMatcherAdmissibility exprCtx matcherType targetType clauses s12
    appliedMatcherType <- applySubstWithConstraintsM sAdm matcherType

    -- Normalize the matcher type to extract the matched inner type.
    (_normalizedMatcherType, matchedInnerType, s3) <- case appliedMatcherType of
      TTuple elemTypes -> do
        -- Each tuple element is a Matcher (extract its inner) or a MatcherSlot (its target).
        (finalInnerTypes, s_elems) <- foldM (\(acc, accS) elemTy -> do
          appliedElemTy <- applySubstWithConstraintsM accS elemTy
          case appliedElemTy of
            TMatcherSlot _ tt -> return (acc ++ [tt], accS)
            _ -> do
              innerTy <- freshVar "matched"
              s' <- bindMatcherInner exprCtx appliedElemTy innerTy
              innerTy' <- applySubstWithConstraintsM s' innerTy
              return (acc ++ [innerTy'], composeSubst s' accS)
          ) ([], emptySubst) elemTypes
        -- The tuple as a whole becomes Matcher (a1, a2, ...)
        let tupleInnerType = TTuple finalInnerTypes
        return (TMatcher tupleInnerType, tupleInnerType, s_elems)
      -- A MatcherSlot (committed parameter, or a stdlib slot-typed matcher): the matched inner
      -- type is its target component.
      TMatcherSlot _ tt -> return (TMatcher tt, tt, emptySubst)
      _ -> do
        -- Single matcher: TMatcher a
        matchedTy <- freshVar "matched"
        s' <- bindMatcherInner exprCtx appliedMatcherType matchedTy
        finalMatchedTy <- applySubstWithConstraintsM s' matchedTy
        return (TMatcher finalMatchedTy, finalMatchedTy, s')

    let s123 = composeSubst s3 sAdm
    targetType' <- applySubstWithConstraintsM s123 targetType
    matchedInnerType' <- applySubstWithConstraintsM s123 matchedInnerType
    s4 <- unifyTypesWithContext targetType' matchedInnerType' exprCtx
    
    -- MatchAll returns a collection of results from match clauses
    let s1234 = composeSubst s4 sAdm
    case clauses of
      [] -> do
        -- No clauses: return empty collection type
        resultElemTy <- freshVar "matchAllElem"
        targetTI' <- applySubstToTIExprM s1234 targetTI
        matcherTI' <- applySubstToTIExprM s1234 matcherTI
        resultElemTy' <- applySubstWithConstraintsM s1234 resultElemTy
        return (mkTIExpr (TCollection resultElemTy') (TIMatchAllExpr mode targetTI' matcherTI' []), s1234)
      _ -> do
        -- Infer type of each clause (they should all have the same type)
        matchedInnerType' <- applySubstWithConstraintsM s1234 matchedInnerType
        (resultElemTy, clauseTIs, clauseSubst) <- inferMatchClauses exprCtx matchedInnerType' clauses s1234
        let finalS = composeSubst clauseSubst s1234
        targetTI' <- applySubstToTIExprM finalS targetTI
        matcherTI' <- applySubstToTIExprM finalS matcherTI
        resultElemTy' <- applySubstWithConstraintsM finalS resultElemTy
        return (mkTIExpr (TCollection resultElemTy') (TIMatchAllExpr mode targetTI' matcherTI' clauseTIs), finalS)
  
  -- Memoized Lambda
  IMemoizedLambdaExpr args body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    argTypes <- mapM (\_ -> freshVar "memoArg") args
    let bindings = zip args argTypes  -- [(String, Type)]
        schemes = map (\(name, t) -> (name, Forall [] [] t)) bindings
    (bodyTI, s) <- withEnv schemes $ inferIExprWithContext body exprCtx
    let bodyType = tiExprType bodyTI
    finalArgTypes <- mapM (applySubstWithConstraintsM s) argTypes
    let funType = foldr TFun bodyType finalArgTypes
    return (mkTIExpr funType (TIMemoizedLambdaExpr args bodyTI), s)
  
  -- Do expression
  IDoExpr bindings body -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    -- Infer IO monad bindings: each binding should be of type IO a
    env <- getEnv
    (bindingTIs, bindingSchemes, s1) <- inferIOBindingsWithContext bindings env emptySubst exprCtx
    (bodyTI, s2) <- withEnv bindingSchemes $ inferIExprWithContext body exprCtx
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
    return (mkTIExpr (TFun argType bodyType) (TICambdaExpr var bodyTI), s)
  
  -- With symbols
  IWithSymbolsExpr syms body -> do
    -- Add symbols to type environment as MathValue (TMathValue = TInt)
    -- Symbols introduced by withSymbols are mathematical symbols
    let symbolBindings = [(sym, Forall [] [] TMathValue) | sym <- syms]
    (bodyTI, s) <- withEnv symbolBindings $ inferIExprWithContext body ctx
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
            st <- get
            let (constraints, t, newCounter) = instantiate scheme (inferCounter st)
            modify $ \s' -> s' { inferCounter = newCounter }
            addConstraints constraints
            return (TIExpr (Forall [] constraints t) (TIVarExpr varName), emptySubst)
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
    let baseType = tiExprType baseTI
        finalS = composeSubst s2 s1
        -- Subrefs requires base to be a Tensor type
        -- Force base type to be Tensor if not already
        tensorBaseType = case baseType of
          TTensor elemType -> TTensor elemType  -- Already Tensor
          otherType -> TTensor otherType  -- Wrap non-Tensor in Tensor
        -- Result is also a Tensor type
        resultType = tensorBaseType
    return (mkTIExpr resultType (TISubrefsExpr override baseTI refTI), finalS)
  
  -- Suprefs expression (superscript references)
  ISuprefsExpr override baseExpr refExpr -> do
    let exprCtx = withExpr (prettyStr expr) ctx
    (baseTI, s1) <- inferIExprWithContext baseExpr exprCtx
    (refTI, s2) <- inferIExprWithContext refExpr exprCtx
    let baseType = tiExprType baseTI
        finalS = composeSubst s2 s1
        -- Suprefs requires base to be a Tensor type
        -- Force base type to be Tensor if not already
        tensorBaseType = case baseType of
          TTensor elemType -> TTensor elemType  -- Already Tensor
          otherType -> TTensor otherType  -- Wrap non-Tensor in Tensor
        -- Result is also a Tensor type
        resultType = tensorBaseType
    return (mkTIExpr resultType (TISuprefsExpr override baseTI refTI), finalS)
  
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
    -- `(zero : MathValue)` keep the original `Forall [a] [AddMonoid a] a`
    -- scheme on `zero`, and TypeClassExpand goes down the TVar dispatch path
    -- (emitting a reference to the non-existent `dict_AddMonoid` parameter)
    -- instead of the concrete-instance path. At runtime that produces the
    -- "Expected CASData" / "Expected function" errors typical of unresolved
    -- dispatch.
    innerTI' <- applySubstToTIExprM finalSubst innerTI
    return (mkTIExpr finalTy (TIReshape finalTy innerTI'), finalSubst)

-- | Dual pattern typing for match-site admissibility (paper T-MATCHALL / T-MATCH): a single
-- 'inferIPattern' traversal yields BOTH of the rule's pattern types — τ_t (the target type, from
-- the ordinary inference) and τ_p (the structural type, the 4th component, built from the
-- sub-patterns' own structural types with fresh leaves).  τ_p shares no variable with τ_t (every
-- leaf is fresh and value patterns never contribute their value's type to τ_p), so later unifying
-- τ_t with the target never concretizes τ_p — keeping the structural slot matcher-independent (a
-- bare variable pattern stays admissible for ANY matcher, e.g. `something`).  Run as a
-- side-effect-free probe (type-class constraints and the global zonk substitution
-- snapshotted/restored); any failure falls back to
-- fresh variables (a variable-headed slot admits any matcher — never a false rejection).
patternDualType :: IPattern -> TypeErrorContext -> Infer (Type, Type)
patternDualType pat ctx = do
  saved <- getConstraints
  savedG <- gets inferGlobalSubst
  result <- (do tv <- freshVar "taut"
                (_, _, st, taup) <- inferIPattern pat tv ctx
                taut  <- applySubstWithConstraintsM st tv
                taup' <- applySubstWithConstraintsM st taup
                return (taup', taut))
              `catchError` \_ -> (,) <$> freshVar "taup" <*> freshVar "taut"
  modify $ \st -> st { inferConstraints = saved, inferGlobalSubst = savedG }
  return result

-- | Run an action but discard its effect on the type-class constraint store and the global
-- zonk substitution.  Used for the structural-type τ_p reassembly unifications, which involve
-- only fresh variables and must never leak into the main (τ_t / binding) inference — including
-- speculative unifications whose failure is swallowed by a 'catchError' fallback.
withIsolatedConstraints :: Infer a -> Infer a
withIsolatedConstraints act = do
  saved <- getConstraints
  savedG <- gets inferGlobalSubst
  r <- act
  modify $ \st -> st { inferConstraints = saved, inferGlobalSubst = savedG }
  return r

-- | τ_p of an and/or/forall/loop/seq-cons pattern: the two sub-patterns describe the same value,
-- so the matcher must support both shapes — unify their structural types (all fresh-headed, so the
-- occurs check cannot fire; isolated so it leaves the main inference untouched).
taupCombine :: TypeErrorContext -> Type -> Type -> Infer Type
taupCombine ctx ta tb = do
  r <- withIsolatedConstraints $
    (do s <- unifyTypesWithContext ta tb ctx
        Just <$> applySubstWithConstraintsM s ta)
      `catchError` \_ -> return Nothing
  case r of
    Just t  -> recordPatfunTaupEqs [(ta, tb)] >> return t
    Nothing -> freshVar "taup"

-- | While inferring a pattern function body, record the structural equations a
-- node-local solver discharged, so PATFUN-DEF can re-solve them jointly for the
-- structural signature (the node-local substitutions themselves are discarded).
-- A no-op outside pattern function bodies.
recordPatfunTaupEqs :: [(Type, Type)] -> Infer ()
recordPatfunTaupEqs eqs =
  modify $ \st -> st { inferPatfunTaupEqs = fmap (eqs ++) (inferPatfunTaupEqs st) }

-- | τ_p of a constructor application: reassemble the constructor's result type from the
-- sub-patterns' structural types @childrenTaup@ at the (freshly instantiated) argument positions
-- @argTypes@ / @resultType@.  Every argument is fresh-headed, so the occurs check can never fire;
-- isolated so the reassembly never leaks into the main inference.
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

taupFromCtor :: TypeErrorContext -> [Type] -> Type -> [Type] -> Infer Type
taupFromCtor ctx argTypes resultType childrenTaup
  | length argTypes /= length childrenTaup = freshVar "taup"
  | otherwise = do
      r <- withIsolatedConstraints $
        (do s <- foldM (\acc (x, y) -> do
                     x' <- applySubstWithConstraintsM acc x
                     y' <- applySubstWithConstraintsM acc y
                     s' <- unifyTypesWithContext x' y' ctx
                     return (composeSubst s' acc)) emptySubst (zip argTypes childrenTaup)
            Just <$> applySubstWithConstraintsM s resultType)
          `catchError` \_ -> return Nothing
      case r of
        Just t  -> recordPatfunTaupEqs (zip argTypes childrenTaup) >> return t
        Nothing -> freshVar "taup"

-- | Match-site structural admissibility (paper T-MATCHALL / T-MATCH via COERCE-MATCHER-TO-SLOT).
-- For each clause a single traversal ('patternDualType') derives BOTH pattern types at once:
--   * τ_p — the *structural* type, the slot the matcher must fill (one-way @⊑@); a value/predicate
--     pattern contributes only a fresh variable here, so it imposes no structural duty;
--   * τ_t — the *target* type (value patterns contribute their value's type), unified with the
--     target type.
-- The matcher must fill @MatcherSlot τ_p τ_t@.  Routing value-pattern types into τ_t (target)
-- rather than τ_p (structural) means a value pattern `#e` — matched by structural equality @≡@,
-- which every matcher supports — imposes only a target-type constraint.  So `multiset eq with #1`
-- and `something with #1` are admissible, while a *constructor* pattern still demands a
-- structurally-capable matcher (`something` / a bare `Matcher a` at `$x :: $xs` is still rejected).
checkMatcherAdmissibility :: TypeErrorContext -> Type -> Type -> [IMatchClause] -> Subst -> Infer Subst
checkMatcherAdmissibility ctx matcherTy targetTy clauses s0 = foldM step s0 clauses
  where
    step accS (pat, _body) = do
      (tau_p, tau_t) <- patternDualType pat ctx
      targetTy'  <- applySubstWithConstraintsM accS targetTy
      tau_t'     <- applySubstWithConstraintsM accS tau_t
      -- value-pattern-informed type unifies with the actual target type
      sTgt       <- unifyTypesWithContext tau_t' targetTy' ctx
      let acc1   =  composeSubst sTgt accS
      matcherTy' <- applySubstWithConstraintsM acc1 matcherTy
      tau_p'     <- applySubstWithConstraintsM acc1 tau_p
      targetTy'' <- applySubstWithConstraintsM acc1 targetTy
      sSlot <- unifyTypesWithContext matcherTy' (TMatcherSlot tau_p' targetTy'') ctx
      return (composeSubst sSlot acc1)

-- | Head type-former name under which a type's pattern constructors are grouped (paper
-- Coverage, Def 4.2(3)).  Polymorphic / tuple / function matched types have no declared
-- pattern constructors, so they yield 'Nothing' (Coverage holds vacuously).
matcherTypeHead :: Type -> Maybe String
matcherTypeHead t = case t of
  TInt           -> Just "Integer"
  TMathValue     -> Just "MathValue"
  TBool          -> Just "Bool"
  TString        -> Just "String"
  TChar          -> Just "Char"
  TFloat         -> Just "Float"
  TCollection _  -> Just "[]"
  TInductive n _ -> Just n
  _              -> Nothing

-- | The result type-former of a pattern-constructor scheme (walks the @arg -> … -> result@
-- chain), used to group constructors by the type they construct.
ctorResultHead :: TypeScheme -> Maybe String
ctorResultHead (Forall _ _ ty) = matcherTypeHead (resultOf ty)
  where resultOf (TFun _ r) = resultOf r
        resultOf t          = t

-- | The constructor a matcher clause is a *general* clause for (paper's @c $..$@): an
-- inductive primitive-pattern pattern whose arguments are all bare holes @$@.  Refinement
-- clauses (holes mixed with @_@ or @#$x@), value-pattern clauses, and the catch-all are not
-- general clauses and do not contribute to Coverage.
generalClauseCtor :: PrimitivePatPattern -> Maybe String
generalClauseCtor (PPInductivePat name args)
  | all isHole args = Just name
  where isHole PPPatVar = True
        isHole _        = False
generalClauseCtor _ = Nothing

-- | Conservative exhaustiveness of a matcher clause's primitive-data-pattern arms
-- (arm exhaustiveness, paper Def 4.2(1c); enforced as an ordinary type error).
-- An arm set is deemed exhaustive if
-- some arm is irrefutable, or the arms complete a built-in closed shape: the
-- empty-collection pattern together with a cons/snoc pattern with irrefutable components
-- (every collection is empty or uncons-able), or the constants True and False (every Bool).
-- Purely syntactic — no data-constructor enumeration (the type env does not distinguish
-- constructors from functions of the same result type) — so a refutable-but-complete arm
-- set over a user ADT (each constructor enumerated, no final catch-all) is conservatively
-- rejected; add a final `| _ -> []` arm.
pdArmsExhaustive :: [IPrimitiveDataPattern] -> Bool
pdArmsExhaustive arms =
     any pdIrrefutable arms
  || (any isEmptyArm arms && any completeUncons arms)
  || (any (isBoolArm True) arms && any (isBoolArm False) arms)
  where
    isEmptyArm PDEmptyPat = True
    isEmptyArm _          = False
    completeUncons (PDConsPat p1 p2) = pdIrrefutable p1 && pdIrrefutable p2
    completeUncons (PDSnocPat p1 p2) = pdIrrefutable p1 && pdIrrefutable p2
    completeUncons _                 = False
    isBoolArm b (PDConstantPat (BoolExpr b')) = b == b'
    isBoolArm _ _                             = False

-- | An irrefutable primitive data pattern: one that matches every well-typed target.
-- A tuple of irrefutable components is irrefutable because the arm's expected type is
-- unified with the corresponding tuple type, so the target is always a tuple of that
-- arity in a well-typed program.
pdIrrefutable :: IPrimitiveDataPattern -> Bool
pdIrrefutable PDWildCard      = True
pdIrrefutable (PDPatVar _)    = True
pdIrrefutable (PDTuplePat ps) = all pdIrrefutable ps
pdIrrefutable _               = False

-- | Infer match clauses type
-- All clauses should return the same type
-- NEW: Returns TIMatchClause list in addition to type and subst
inferMatchClauses :: TypeErrorContext -> Type -> [IMatchClause] -> Subst -> Infer (Type, [TIMatchClause], Subst)
inferMatchClauses ctx matchedType clauses initSubst = do
  case clauses of
    [] -> do
      -- No clauses (should not happen)
      ty <- freshVar "clauseResult"
      return (ty, [], initSubst)
    (firstClause:restClauses) -> do
      -- Infer first clause
      (firstTI, firstType, s1) <- inferMatchClause ctx matchedType firstClause initSubst
      
      -- Infer rest clauses and unify with first
      (finalType, clauseTIs, finalSubst) <- foldM (inferAndUnifyClause ctx matchedType) (firstType, [firstTI], s1) restClauses
      return (finalType, reverse clauseTIs, finalSubst)
  where
    inferAndUnifyClause :: TypeErrorContext -> Type -> (Type, [TIMatchClause], Subst) -> IMatchClause -> Infer (Type, [TIMatchClause], Subst)
    inferAndUnifyClause ctx' matchedTy (expectedType, accClauses, accSubst) clause = do
      matchedTy' <- applySubstWithConstraintsM accSubst matchedTy
      (clauseTI, clauseType, s1) <- inferMatchClause ctx' matchedTy' clause accSubst
      expectedType' <- applySubstWithConstraintsM s1 expectedType
      s2 <- unifyTypesWithContext expectedType' clauseType ctx'
      let finalS = composeSubst s2 (composeSubst s1 accSubst)
      finalExpectedType <- applySubstWithConstraintsM finalS expectedType
      return (finalExpectedType, clauseTI : accClauses, finalS)

-- | Infer a single match clause
-- NEW: Returns TIMatchClause in addition to type and subst
inferMatchClause :: TypeErrorContext -> Type -> IMatchClause -> Subst -> Infer (TIMatchClause, Type, Subst)
inferMatchClause ctx matchedType (pattern, bodyExpr) initSubst = do
  -- Infer pattern type and extract pattern variable bindings
  -- Use pattern constructor and pattern function type information
  (tiPattern, bindings, s_pat, _) <- inferIPattern pattern matchedType ctx
  let s1 = composeSubst s_pat initSubst
  
  -- Convert bindings to TypeScheme format
  let schemes = [(var, Forall [] [] ty) | (var, ty) <- bindings]
  
  -- Infer body expression type with pattern variables in scope
  (bodyTI, s2) <- withEnv schemes $ inferIExprWithContext bodyExpr ctx
  let bodyType = tiExprType bodyTI
      finalS = composeSubst s2 s1
  finalBodyType <- applySubstWithConstraintsM finalS bodyType
  return ((tiPattern, bodyTI), finalBodyType, finalS)

-- | Infer multiple patterns left-to-right, making left bindings available to right patterns
-- This enables non-linear patterns like ($p, #(p + 1))
-- Returns (list of TIPattern, accumulated bindings, substitution)
-- The final @[Type]@ component is the sub-patterns' structural types τ_p, in order, used by the
-- parent constructor/tuple to reassemble its own τ_p (paper T-MATCHALL dual judgment).
inferPatternsLeftToRight :: [IPattern] -> [Type] -> [(String, Type)] -> Subst -> TypeErrorContext
                         -> Infer ([TIPattern], [(String, Type)], Subst, [Type])
inferPatternsLeftToRight [] [] accBindings accSubst _ctx =
  return ([], accBindings, accSubst, [])
inferPatternsLeftToRight (p:ps) (t:ts) accBindings accSubst ctx = do
  -- Add accumulated bindings to environment for this pattern
  let schemes = [(var, Forall [] [] ty) | (var, ty) <- accBindings]

  -- Infer this pattern with left bindings in scope
  t' <- applySubstWithConstraintsM accSubst t
  (tipat, newBindings, s, taup) <- withEnv schemes $ inferIPattern p t' ctx

  -- Compose substitutions
  let accSubst' = composeSubst s accSubst

  -- Apply substitution to accumulated bindings
  accBindings'' <- mapM (\(v, ty) -> do
      ty' <- applySubstWithConstraintsM s ty
      return (v, ty')) accBindings
  let accBindings' = accBindings'' ++ newBindings

  -- Continue with remaining patterns
  (restTipats, finalBindings, finalSubst, restTaups) <- inferPatternsLeftToRight ps ts accBindings' accSubst' ctx
  return (tipat : restTipats, finalBindings, finalSubst, taup : restTaups)
inferPatternsLeftToRight _ _ accBindings accSubst _ =
  return ([], accBindings, accSubst, [])  -- Mismatched lengths

-- | Infer an IPattern's types and extract its pattern-variable bindings.  Returns
-- (TIPattern, bindings, substitution, τ_p) — realizing the paper's dual judgment
-- @Γ;Δ ⊢ p : Pattern τ_p ▷ τ_t ; Δ'@ in one traversal:
--   * τ_t (the *target* type) and Δ' (bindings) are computed exactly as before — coherently,
--     top-down, threading one substitution (τ_t is read from the TIPattern / @expectedType@);
--   * τ_p (the *structural* type, the 4th component) is built up from the sub-patterns' own τ_p
--     with a FRESH variable at every leaf (variable, wildcard, and value/predicate position).
-- Keeping τ_p's variables disjoint from τ_t's (the leaves are fresh, and value patterns never
-- contribute their value's type to τ_p) is what lets the later τ_t-with-target unification leave
-- τ_p untouched — so the structural slot stays matcher-independent — and is also what stops τ_p
-- from tangling with outer bindings into an infinite type.  The few τ_p reassembly unifications
-- (constructor/and/or/…) are run with 'withIsolatedConstraints' so they never leak into the main
-- (τ_t / binding) inference.
inferIPattern :: IPattern -> Type -> TypeErrorContext -> Infer (TIPattern, [(String, Type)], Subst, Type)
inferIPattern pat expectedType ctx = case pat of
  IWildCard -> do
    -- Wildcard: no bindings; τ_p is a fresh variable (no structural duty)
    let tipat = TIPattern (Forall [] [] expectedType) TIWildCard
    taup <- freshVar "taup"
    return (tipat, [], emptySubst, taup)

  IPatVar name -> do
    -- Pattern variable: bind to expected type; τ_p a fresh variable, independent of expectedType
    let tipat = TIPattern (Forall [] [] expectedType) (TIPatVar name)
    taup <- freshVar "taup"
    return (tipat, [(name, expectedType)], emptySubst, taup)

  IValuePat expr -> do
    -- Value pattern: infer expression type and unify with expected type.  τ_p is a fresh variable
    -- — matched by structural equality, the value imposes no structural duty (its type goes to τ_t)
    (exprTI, s) <- inferIExprWithContext expr ctx
    let exprType = tiExprType exprTI
    exprType' <- applySubstWithConstraintsM s exprType
    expectedType' <- applySubstWithConstraintsM s expectedType
    s' <- unifyTypesWithContext exprType' expectedType' ctx
    let finalS = composeSubst s' s
    exprTI' <- applySubstToTIExprM finalS exprTI
    finalType <- applySubstWithConstraintsM finalS expectedType
    let tipat = TIPattern (Forall [] [] finalType) (TIValuePat exprTI')
    taup <- freshVar "taup"
    return (tipat, [], finalS, taup)

  IPredPat expr -> do
    -- Predicate pattern: infer predicate expression.  τ_p is a fresh variable (a boolean test
    -- imposes no structural duty)
    let predicateType = TFun expectedType TBool
    (exprTI, s) <- inferIExprWithContext expr ctx
    -- Unify with expected predicate type to concretize type variables
    exprType' <- applySubstWithConstraintsM s (tiExprType exprTI)
    predicateType' <- applySubstWithConstraintsM s predicateType
    s' <- unifyTypesWithContext exprType' predicateType' ctx
    let finalS = composeSubst s' s
    exprTI' <- applySubstToTIExprM finalS exprTI
    finalType <- applySubstWithConstraintsM finalS expectedType
    let tipat = TIPattern (Forall [] [] finalType) (TIPredPat exprTI')
    taup <- freshVar "taup"
    return (tipat, [], finalS, taup)
  
  ITuplePat pats -> do
    -- Tuple pattern: decompose expected type
    case expectedType of
      TTuple types | length types == length pats -> do
        -- Types match: infer each sub-pattern left-to-right
        -- Left patterns' bindings are available for right patterns (for non-linear patterns)
        (tipats, allBindings, s, childrenTaup) <- inferPatternsLeftToRight pats types [] emptySubst ctx
        finalType <- applySubstWithConstraintsM s expectedType
        let tipat = TIPattern (Forall [] [] finalType) (TITuplePat tipats)
        return (tipat, allBindings, s, TTuple childrenTaup)

      TVar _ -> do
        -- Expected type is a type variable: create tuple type
        elemTypes <- mapM (\_ -> freshVar "elem") pats
        let tupleTy = TTuple elemTypes
        s <- unifyTypesWithContext expectedType tupleTy ctx

        -- Recursively infer each sub-pattern left-to-right
        elemTypes' <- mapM (applySubstWithConstraintsM s) elemTypes
        (tipats, allBindings, s', childrenTaup) <- inferPatternsLeftToRight pats elemTypes' [] s ctx
        finalType <- applySubstWithConstraintsM s' expectedType
        let tipat = TIPattern (Forall [] [] finalType) (TITuplePat tipats)
        return (tipat, allBindings, s', TTuple childrenTaup)
      
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
        st <- get
        let (_constraints, ctorType, newCounter) = instantiate scheme (inferCounter st)
        modify $ \s -> s { inferCounter = newCounter }
        
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
            (tipats, allBindings, s, childrenTaup) <- inferPatternsLeftToRight pats argTypes' [] s0 ctx
            finalType <- applySubstWithConstraintsM s expectedType
            -- τ_p: reassemble from a FRESH instantiation (untied to expectedType) + children τ_p
            stP <- get
            let (_csP, ctorTypeP, ctrP) = instantiate scheme (inferCounter stP)
            modify $ \z -> z { inferCounter = ctrP }
            let (argTypesP, resultTypeP) = extractFunctionArgs ctorTypeP
            taup <- taupFromCtor ctx argTypesP resultTypeP childrenTaup
            let tipat = TIPattern (Forall [] [] finalType) (TIInductivePat name tipats)
            return (tipat, allBindings, s, taup)
      
      Nothing -> do
        -- Not found in pattern environment: try data constructor from value environment
        -- This handles data constructors used as patterns
        env <- getEnv
        case lookupEnv (stringToVar name) env of
          Just scheme -> do
            st <- get
            let (_constraints, ctorType, newCounter) = instantiate scheme (inferCounter st)
            modify $ \s -> s { inferCounter = newCounter }
            
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
                (tipats, allBindings, s, childrenTaup) <- inferPatternsLeftToRight pats argTypes' [] s0 ctx
                finalType <- applySubstWithConstraintsM s expectedType
                -- τ_p: reassemble from a FRESH instantiation (untied to expectedType) + children τ_p
                stP <- get
                let (_csP, ctorTypeP, ctrP) = instantiate scheme (inferCounter stP)
                modify $ \z -> z { inferCounter = ctrP }
                let (argTypesP, resultTypeP) = extractFunctionArgs ctorTypeP
                taup <- taupFromCtor ctx argTypesP resultTypeP childrenTaup
                let tipat = TIPattern (Forall [] [] finalType) (TIInductivePat name tipats)
                return (tipat, allBindings, s, taup)

          Nothing -> do
            -- Not found: generic inference
            argTypes <- mapM (\_ -> freshVar "arg") pats
            let resultType = TInductive name argTypes

            s0 <- unifyTypesWithContext resultType expectedType ctx
            argTypes' <- mapM (applySubstWithConstraintsM s0) argTypes

            -- Recursively infer each sub-pattern left-to-right
            (tipats, allBindings, s, childrenTaup) <- inferPatternsLeftToRight pats argTypes' [] s0 ctx
            finalType <- applySubstWithConstraintsM s expectedType
            -- τ_p: an undeclared constructor is treated generically — same head, children's τ_p
            let tipat = TIPattern (Forall [] [] finalType) (TIInductivePat name tipats)
            return (tipat, allBindings, s, TInductive name childrenTaup)
  
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
    let tiIndexedPat = TIPattern (Forall [] [] finalType) (TIIndexedPat tipat indexTIs)
    -- τ_p: an indexed access ($x_i) is variable-like — no structural duty
    taup <- freshVar "taup"
    return (tiIndexedPat, bindings, finalS, taup)
  
  ILetPat bindings p -> do
    -- Let pattern: infer bindings and then the pattern
    -- Infer bindings first
    env <- getEnv
    (bindingTIs, bindingSchemes, s1) <- inferIBindingsWithContext bindings env emptySubst ctx

    -- Infer pattern with bindings in scope
    expectedType' <- applySubstWithConstraintsM s1 expectedType
    (tipat, patBindings, s2, innerTaup) <- withEnv bindingSchemes $ inferIPattern p expectedType' ctx

    let s = composeSubst s2 s1
    finalType <- applySubstWithConstraintsM s expectedType
    let tiLetPat = TIPattern (Forall [] [] finalType) (TILetPat bindingTIs tipat)
    -- Let bindings are not exported, only pattern bindings; τ_p is the inner pattern's
    return (tiLetPat, patBindings, s, innerTaup)

  INotPat p -> do
    -- Not pattern: infer the sub-pattern but don't use its bindings; τ_p is the inner pattern's
    (tipat, _, s, innerTaup) <- inferIPattern p expectedType ctx
    finalType <- applySubstWithConstraintsM s expectedType
    let tiNotPat = TIPattern (Forall [] [] finalType) (TINotPat tipat)
    return (tiNotPat, [], s, innerTaup)
  
  IAndPat p1 p2 -> do
    -- And pattern: both patterns must match the same type
    -- Left bindings should be available to right pattern
    (tipat1, bindings1, s1, taup1) <- inferIPattern p1 expectedType ctx
    let schemes1 = [(var, Forall [] [] ty) | (var, ty) <- bindings1]
    expectedType' <- applySubstWithConstraintsM s1 expectedType
    (tipat2, bindings2, s2, taup2) <- withEnv schemes1 $ inferIPattern p2 expectedType' ctx
    let s = composeSubst s2 s1
    -- Apply substitution to left bindings
    bindings1'' <- mapM (\(v, ty) -> do
        ty' <- applySubstWithConstraintsM s2 ty
        return (v, ty')) bindings1
    finalType <- applySubstWithConstraintsM s expectedType
    -- τ_p: the matcher must support both conjuncts' shapes
    taup <- taupCombine ctx taup1 taup2
    let bindings1' = bindings1''
        tiAndPat = TIPattern (Forall [] [] finalType) (TIAndPat tipat1 tipat2)
    return (tiAndPat, bindings1' ++ bindings2, s, taup)
  
  IOrPat p1 p2 -> do
    -- Or pattern (paper PAT-OR): the two branches are alternatives over the same input
    -- context, so they are typed independently and must produce the SAME output bindings Δ'
    -- — the same variable names, at unifiable types.
    (tipat1, bindings1, s1, taup1) <- inferIPattern p1 expectedType ctx
    expectedType' <- applySubstWithConstraintsM s1 expectedType
    (tipat2, bindings2, s2, taup2) <- inferIPattern p2 expectedType' ctx
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
        let tiOrPat = TIPattern (Forall [] [] finalType) (TIOrPat tipat1 tipat2)
        -- τ_p: the matcher must support either alternative's shape
        taup <- taupCombine ctx taup1 taup2
        return (tiOrPat, finalBindings, sVars, taup)
  
  IForallPat p1 p2 -> do
    -- Forall pattern: similar to and pattern
    -- Left bindings should be available to right pattern
    (tipat1, bindings1, s1, taup1) <- inferIPattern p1 expectedType ctx
    let schemes1 = [(var, Forall [] [] ty) | (var, ty) <- bindings1]
    expectedType' <- applySubstWithConstraintsM s1 expectedType
    (tipat2, bindings2, s2, taup2) <- withEnv schemes1 $ inferIPattern p2 expectedType' ctx
    let s = composeSubst s2 s1
    -- Apply substitution to left bindings
    bindings1'' <- mapM (\(v, ty) -> do
        ty' <- applySubstWithConstraintsM s2 ty
        return (v, ty')) bindings1
    finalType <- applySubstWithConstraintsM s expectedType
    taup <- taupCombine ctx taup1 taup2
    let bindings1' = bindings1''
        tiForallPat = TIPattern (Forall [] [] finalType) (TIForallPat tipat1 tipat2)
    return (tiForallPat, bindings1' ++ bindings2, s, taup)
  
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
        schemes0 = [(v, Forall [] [] ty) | (v, ty) <- initialBindings]
        s_combined = foldr composeSubst emptySubst [s_end, s_start, s_range]

    -- Infer p1 with loop variable and range bindings in scope
    expectedType1 <- applySubstWithConstraintsM s_combined expectedType
    (tipat1, bindings1, s1, taup1) <- withEnv schemes0 $ inferIPattern p1 expectedType1 ctx

    -- Infer p2 with all previous bindings in scope
    allPrevBindings' <- mapM (\(v, ty) -> do
        ty' <- applySubstWithConstraintsM s1 ty
        return (v, ty')) initialBindings
    let allPrevBindings = allPrevBindings' ++ bindings1
        schemes1 = [(v, Forall [] [] ty) | (v, ty) <- allPrevBindings]
    expectedType2 <- applySubstWithConstraintsM s1 expectedType
    (tipat2, bindings2, s2, taup2) <- withEnv schemes1 $ inferIPattern p2 expectedType2 ctx

    let s = foldr composeSubst emptySubst [s2, s1, s_combined]
    -- Apply final substitution to all bindings
    finalBindings' <- mapM (\(v, ty) -> do
        ty' <- applySubstWithConstraintsM s ty
        return (v, ty')) (loopVarBinding : rangeBindings ++ bindings1 ++ bindings2)
    finalType <- applySubstWithConstraintsM s expectedType
    -- τ_p: the iterated body and the rest both describe the matched value
    taup <- taupCombine ctx taup1 taup2
    let finalBindings = finalBindings'
        tiLoopPat = TIPattern (Forall [] [] finalType) (TILoopPat var tiLoopRange tipat1 tipat2)

    return (tiLoopPat, finalBindings, s, taup)

  IContPat -> do
    -- Continuation pattern: no bindings
    let tipat = TIPattern (Forall [] [] expectedType) TIContPat
    taup <- freshVar "taup"
    return (tipat, [], emptySubst, taup)
  
  IPApplyPat funcExpr argPats -> do
    -- Pattern application (paper PAT-APP), the same device as IInductivePat:
    -- the target side unifies the pattern function's type with the argument
    -- target types and the expected (target) type; the structural side
    -- instantiates the function's recorded structural signature and unifies
    -- the arguments' structural indices into it.
    (funcTI, s1) <- inferIExprWithContext funcExpr ctx

    -- Target side: f : tau_1 -> ... -> tau_k -> tau (inner types, no Pattern
    -- wrapper; design/pattern.md), so unify it with parg_1 -> ... -> parg_k ->
    -- expectedType and check the argument patterns against the resolved
    -- parameter types (top-down, keeping tau_t coherent).
    argTypes <- mapM (\_ -> freshVar "parg") argPats
    let funcType = tiExprType funcTI
    funcType' <- applySubstWithConstraintsM s1 funcType
    expectedType1 <- applySubstWithConstraintsM s1 expectedType
    s0 <- unifyTypesWithContext funcType' (foldr TFun expectedType1 argTypes) ctx
    let s10 = composeSubst s0 s1
    argTypes' <- mapM (applySubstWithConstraintsM s10) argTypes
    (tipats, allBindings, s2, childrenTaup) <- inferPatternsLeftToRight argPats argTypes' [] s10 ctx

    finalType <- applySubstWithConstraintsM s2 expectedType
    let tipat = TIPattern (Forall [] [] finalType) (TIPApplyPat funcTI tipats)

    -- Structural side: instantiate the structural signature
    -- beta_1 -> ... -> beta_k -> tau_p_body recorded at the definition
    -- (a FRESH instantiation, untied to the target side, as in IInductivePat),
    -- unify the arguments' structural indices with the beta_i, and return the
    -- resulting instance of tau_p_body.  Without a recorded signature the
    -- application is structurally unconstrained (fresh), the pre-fix behavior.
    taup <- case funcExpr of
      IVarExpr fname -> do
        structEnv <- getPatternFuncStructEnvI
        case lookupPatternEnv fname structEnv of
          Just structScheme -> do
            stP <- get
            let (_csP, structTy, ctrP) = instantiate structScheme (inferCounter stP)
            modify $ \z -> z { inferCounter = ctrP }
            let (argTaups, resultTaup) = extractFunctionArgs structTy
            taupFromCtor ctx argTaups resultTaup childrenTaup
          Nothing -> freshVar "taup"
      _ -> freshVar "taup"
    return (tipat, allBindings, s2, taup)

  IVarPat name -> do
    -- Variable pattern (with ~): bind to expected type.
    -- τ_p: inside a pattern function body, a parameter embedding ~x_i carries the
    -- parameter's structural index beta_i (paper PATFUN-DEF/PAT-EMBED), so the body's
    -- structural index records where each argument's structure flows; otherwise fresh.
    let tipat = TIPattern (Forall [] [] expectedType) (TIVarPat name)
    paramTaups <- inferPatfunParamTaup <$> get
    taup <- case Map.lookup name paramTaups of
              Just beta -> return beta
              Nothing   -> freshVar "taup"
    return (tipat, [(name, expectedType)], emptySubst, taup)
  
  IInductiveOrPApplyPat name pats -> do
    -- Could be either inductive pattern or pattern application
    -- Check pattern function environment to distinguish
    -- Pattern functions are ONLY in patternFuncEnv, pattern constructors are NOT
    patternFuncEnv <- getPatternFuncEnv
    case lookupPatternEnv name patternFuncEnv of
      Just _ -> do
        -- It's a pattern function: treat as pattern application
        (tipat, bindings, s, taup) <- inferIPattern (IPApplyPat (IVarExpr name) pats) expectedType ctx
        return (tipat, bindings, s, taup)
      Nothing -> do
        -- It's an inductive pattern constructor (or not found, will be handled later)
        (tipat, bindings, s, taup) <- inferIPattern (IInductivePat name pats) expectedType ctx
        -- Wrap it as InductiveOrPApplyPat (if it's actually an inductive pattern)
        case tipPatternNode tipat of
          TIInductivePat _ tipats -> do
            let scheme = tipScheme tipat
                tiInductiveOrPApplyPat = TIPattern scheme (TIInductiveOrPApplyPat name tipats)
            return (tiInductiveOrPApplyPat, bindings, s, taup)
          _ ->
            -- Not an inductive pattern (e.g., already processed as pattern application)
            return (tipat, bindings, s, taup)
  
  ISeqNilPat -> do
    -- Sequence nil: no bindings
    let tipat = TIPattern (Forall [] [] expectedType) TISeqNilPat
    taup <- freshVar "taup"
    return (tipat, [], emptySubst, taup)

  ISeqConsPat p1 p2 -> do
    -- Sequence cons: infer both patterns
    -- Left bindings should be available to right pattern
    (tipat1, bindings1, s1, taup1) <- inferIPattern p1 expectedType ctx
    let schemes1 = [(var, Forall [] [] ty) | (var, ty) <- bindings1]
    expectedType' <- applySubstWithConstraintsM s1 expectedType
    (tipat2, bindings2, s2, taup2) <- withEnv schemes1 $ inferIPattern p2 expectedType' ctx
    let s = composeSubst s2 s1
    -- Apply substitution to left bindings
    bindings1'' <- mapM (\(v, ty) -> do
        ty' <- applySubstWithConstraintsM s2 ty
        return (v, ty')) bindings1
    finalType <- applySubstWithConstraintsM s expectedType
    taup <- taupCombine ctx taup1 taup2
    let bindings1' = bindings1''
        tipat = TIPattern (Forall [] [] finalType) (TISeqConsPat tipat1 tipat2)
    return (tipat, bindings1' ++ bindings2, s, taup)

  ILaterPatVar -> do
    -- Later pattern variable: no immediate binding
    let tipat = TIPattern (Forall [] [] expectedType) TILaterPatVar
    taup <- freshVar "taup"
    return (tipat, [], emptySubst, taup)
  
  IDApplyPat p pats -> do
    -- D-apply pattern: infer base pattern and argument patterns
    -- Base pattern bindings should be available to argument patterns
    (tipat, bindings1, s1, baseTaup) <- inferIPattern p expectedType ctx

    -- Infer argument patterns left-to-right with base pattern bindings in scope
    argTypes <- mapM (\_ -> freshVar "darg") pats
    let schemes1 = [(var, Forall [] [] ty) | (var, ty) <- bindings1]
    (tipats, argBindings, s2, _) <- withEnv schemes1 $ inferPatternsLeftToRight pats argTypes [] s1 ctx

    let s = composeSubst s2 s1
    -- Apply substitution to base bindings
    bindings1'' <- mapM (\(v, ty) -> do
        ty' <- applySubstWithConstraintsM s2 ty
        return (v, ty')) bindings1
    finalType <- applySubstWithConstraintsM s expectedType
    let bindings1' = bindings1''
        tiDApplyPat = TIPattern (Forall [] [] finalType) (TIDApplyPat tipat tipats)
    -- τ_p: the d-apply's structural shape is its base pattern's
    return (tiDApplyPat, bindings1' ++ argBindings, s, baseTaup)
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

-- TensorMap insertion logic has been moved to Language.Egison.Type.TensorMapInsertion
-- This keeps type inference focused on type checking only

-- | Infer application (helper) with context
-- NEW: Returns TIExpr instead of (IExpr, Type, Subst)
-- TensorMap insertion has been moved to Phase 7 (TensorMapInsertion module)
-- This function now only performs type inference and unification
-- When a Tensor argument is passed to a scalar parameter, the result type is wrapped in Tensor
--
-- IMPORTANT: Non-function arguments are unified first to let data types (like lists)
-- constrain type variables before callback function types are unified.
-- This ensures that foldl (+) 0 [t1, t2] properly infers a = Tensor Integer from the list
-- before trying to match the callback type.
inferIApplicationWithContext :: TIExpr -> Type -> [IExpr] -> Subst -> TypeErrorContext -> Infer (TIExpr, Subst)
inferIApplicationWithContext funcTIExpr funcType args initSubst ctx = do
  -- Infer argument types (once; shared by the main attempt and the CAS-join retry)
  argResults <- mapM (\arg -> inferIExprWithContext arg ctx) args
  let argTIExprs = map fst argResults
      argTypes = map (tiExprType . fst) argResults
      argSubst = foldr composeSubst initSubst (map snd argResults)

  -- Application-site CAS join (design/type-cas-tower.md, join table): when
  -- unifying two CAS operand types fails (in practice: closed atom sets or
  -- canonical forms that unification deliberately keeps unrelated, e.g.
  -- Poly Integer [i] vs Poly Integer [sqrt2]), compute their unique join in
  -- the declared order and retry once with every CAS argument below the
  -- join reshaped to it. Promotion is thus a coercion inserted at the
  -- application site (D5: casReshapeAs only) — the unifier itself never
  -- joins, so type errors outside the CAS order are unaffected. On retry
  -- failure the ORIGINAL mismatch is reported.
  savedCs <- getConstraints
  savedG  <- gets inferGlobalSubst
  inferIApplicationUnifyPhase funcTIExpr funcType argTIExprs argTypes argSubst ctx
    `catchError` \e -> case e of
      UnificationError t1 t2 _
        | Subtype.isCasType t1, Subtype.isCasType t2 -> do
            edges <- gets inferCasSubtypeEdges
            case Subtype.joinTypesWith edges t1 t2 of
              Just j -> do
                modify $ \st -> st { inferConstraints = savedCs, inferGlobalSubst = savedG }
                let promote ti at
                      | Subtype.isCasType at, at /= j, Subtype.isSubtypeWith edges at j =
                          (TIExpr (Forall [] [] j) (TIReshape j ti), j)
                      | otherwise = (ti, at)
                    (argTIExprs', argTypes') = unzip (zipWith promote argTIExprs argTypes)
                inferIApplicationUnifyPhase funcTIExpr funcType argTIExprs' argTypes' argSubst ctx
                  `catchError` \_ -> throwError e
              Nothing -> throwError e
      _ -> throwError e

-- | The unification half of application inference: fresh parameter/result
-- variables, function-shape unification, then argument/parameter
-- unification (data arguments before callbacks). Factored out so the
-- CAS-join retry above can re-run it with reshaped arguments.
inferIApplicationUnifyPhase :: TIExpr -> Type -> [TIExpr] -> [Type] -> Subst -> TypeErrorContext -> Infer (TIExpr, Subst)
inferIApplicationUnifyPhase funcTIExpr funcType argTIExprs argTypes argSubst ctx = do
  -- Create fresh type variables for parameters and result
  paramVars <- mapM (\i -> freshVar ("param" ++ show i)) [1..length argTypes]
  resultType <- freshVar "result"
  let expectedFuncType = foldr TFun resultType paramVars
  appliedFuncType <- applySubstWithConstraintsM argSubst funcType


  -- First unify function type structure to get parameter bindings
  let funcScheme = tiScheme funcTIExpr
      (Forall _tvs funcConstraints _) = funcScheme
  classEnv <- getClassEnv
  -- Include constraints from both the function being applied AND the inference context
  -- The context constraints include constraints from outer scopes (e.g., {Num a} from (.) definition)
  contextConstraints <- getConstraints
  let constraints = funcConstraints ++ contextConstraints
  case Unify.unifyWithConstraints classEnv constraints appliedFuncType expectedFuncType of
    Right (s1, flag1) -> do
      -- Now unify argument types with parameter types
      -- Key: Unify non-function arguments FIRST to let data types constrain type variables
      paramTypesRaw <- mapM (applySubstWithConstraintsM s1) paramVars
      let indexedArgs = zip3 [0..] argTypes paramTypesRaw

      -- Classify arguments: non-functions first, then functions
      -- A type is considered a function if it's TFun
          isArgFunction (TFun _ _) = True
          isArgFunction _ = False
          (funcArgsList, nonFuncArgsList) = partition (\(_, at, _) -> isArgFunction at) indexedArgs

      -- Unify non-function arguments first (data types like lists)
      -- IMPORTANT: Apply substitution to constraints so that constraint checking works correctly
      (s2, flag2) <- foldM (\(s, flagAcc) (_, at, pt) -> do
                     at' <- applySubstWithConstraintsM s at
                     pt' <- applySubstWithConstraintsM s pt
                     let cs' = map (applySubstConstraint s) constraints
                     case Unify.unifyWithConstraints classEnv cs' at' pt' of
                       Right (s', flag') -> return (composeSubst s' s, flagAcc || flag')
                       Left _ -> throwError $ UnificationError at' pt' ctx
                  ) (s1, flag1) nonFuncArgsList

      -- Then unify function arguments (callbacks)
      -- IMPORTANT: Include constraints from the argument's type scheme (e.g., {Num t} from (+))
      -- so that constraint checking works correctly for the argument's type variables
      (s3, flag3) <- foldM (\(s, flagAcc) (idx, at, pt) -> do
                     at' <- applySubstWithConstraintsM s at
                     pt' <- applySubstWithConstraintsM s pt
                     let -- Get constraints from both the outer function and the argument itself
                         outerCs = map (applySubstConstraint s) constraints
                         argScheme = tiScheme (argTIExprs !! idx)
                         (Forall _ argConstraints _) = argScheme
                         argCs = map (applySubstConstraint s) argConstraints
                         allCs = outerCs ++ argCs
                     case Unify.unifyWithConstraints classEnv allCs at' pt' of
                       Right (s', flag') -> return (composeSubst s' s, flagAcc || flag')
                       Left _ -> throwError $ UnificationError at' pt' ctx
                  ) (s2, flag2) funcArgsList

      let finalS = composeSubst s3 argSubst
      baseResultType <- applySubstWithConstraintsM finalS resultType

      -- Check if Tensor was unwrapped during unification (flag3)
      -- If so, wrap the result type in Tensor
      -- This handles cases like sum : {Num a} [a] -> a with [Tensor Integer]
      -- where a unifies with Tensor Integer but gets unwrapped to Integer
      let needsTensorWrap = flag3
          finalType = if needsTensorWrap && not (Types.isTensorType baseResultType)
                      then TTensor baseResultType
                      else baseResultType

      -- Apply substitution to constraints and simplify Tensor constraints
      -- This rewrites C (Tensor a) to C a when appropriate, while keeping types as Tensor a
      -- IMPORTANT: Only use funcConstraints for the result scheme, not contextConstraints
      -- contextConstraints are from outer scopes and should not be propagated to sub-expressions
      let updatedFuncConstraints = map (applySubstConstraint finalS) funcConstraints
          simplifiedFuncConstraints = simplifyTensorConstraints classEnv updatedFuncConstraints
          -- Deduplicate constraints
          deduplicatedConstraints = nub simplifiedFuncConstraints
          -- Filter out constraints on concrete types (only keep constraints on type variables)
          -- This prevents constraints like {Num (Tensor t0)} from appearing in result types
          -- Multi-param-aware: keep constraints if at least one of the type
          -- arguments is still a type variable (these still need dictionary
          -- threading at higher up).
          isTypeVarConstraint c = any isTypeVarType (constraintTypes c)
          isTypeVarType (TVar _) = True
          isTypeVarType _        = False
          typeVarConstraints = filter isTypeVarConstraint deduplicatedConstraints
          -- Result constraints: functions (partial applications) keep constraints,
          -- but values (fully applied) don't need them
          resultConstraints = case finalType of
                                TFun _ _ -> typeVarConstraints  -- Partial application
                                _ -> []  -- Fully applied: no constraints needed
          resultScheme = Forall [] resultConstraints finalType

          -- Update function and argument TIExprs
          -- IMPORTANT: Use applySubstToTIExprWithClassEnv to adjust substitution based on constraints
          -- When {Num t0} t0 -> t0 is unified with Tensor t1, if Num (Tensor t1) has no instance,
          -- the substitution is adjusted to t0 -> t1 (unwrapping the Tensor)
          updatedFuncTI = applySubstToTIExprWithClassEnv classEnv finalS funcTIExpr
          updatedArgTIs = map (applySubstToTIExprWithClassEnv classEnv finalS) argTIExprs

      return (TIExpr resultScheme (TIApplyExpr updatedFuncTI updatedArgTIs), finalS)

    Left _ ->
      -- Special case: if function has type MathValue, allow application returning MathValue
      -- (handles FunctionData application, e.g. f 0 where f := function (x))
      case appliedFuncType of
        TMathValue -> do
          classEnv' <- getClassEnv
          let resultScheme = Forall [] [] TMathValue
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
  (restBindingTIs, restBindings, s2') <- withEnv extendedEnvList $ inferIOBindingsWithContext bs env s' ctx
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

-- | Apply substitution recursively until a fixed point is reached
-- This ensures that nested type variables are fully resolved
-- For example, if s = {t1 -> (Integer, t2), t2 -> [Integer]}, then
-- applySubstRecursively s t1 will return (Integer, [Integer])
-- instead of (Integer, t2)
applySubstRecursively :: Subst -> Type -> Infer Type
applySubstRecursively s t = applySubstRecursively' s t 5  -- Max 5 iterations (reduced from 10)
  where
    applySubstRecursively' :: Subst -> Type -> Int -> Infer Type
    applySubstRecursively' _ t 0 = return t  -- Stop after max iterations
    applySubstRecursively' s t n = do
      t' <- applySubstWithConstraintsM s t
      if t' == t
        then return t
        else applySubstRecursively' s t' (n - 1)

inferIBindingsWithContext :: [IBindingExpr] -> TypeEnv -> Subst -> TypeErrorContext -> Infer ([TIBindingExpr], [(String, TypeScheme)], Subst)
inferIBindingsWithContext [] _env s _ctx = return ([], [], s)
inferIBindingsWithContext ((pat, expr):bs) env s ctx = do
  -- Infer the type of the expression
  (exprTI, s1) <- inferIExprWithContext expr ctx
  let exprType = tiExprType exprTI

  -- Create expected type from pattern and unify with expression type
  -- This helps resolve type variables in the expression type
  (patternType, s2) <- inferPatternType pat
  let s12 = composeSubst s2 s1
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
  let rhsFree = freeTyVars finalExprType
  bindings <-
    case pat of
      PDPatVar _ | not (Set.null rhsFree) -> do
        envNow <- getEnv
        envFreeImages <- mapM (applySubstWithConstraintsM emptySubst . TVar)
                              (Set.toList (freeVarsInEnv envNow))
        constraintsNow <- getConstraints
        let envFreeZ = Set.unions (map freeTyVars envFreeImages)
            consFree = Set.unions [ freeTyVars t | c <- constraintsNow, t <- constraintTypes c ]
            genSet = rhsFree `Set.difference` (envFreeZ `Set.union` consFree)
            regeneralize (n, Forall _ _ t) =
              (n, Forall (Set.toList (freeTyVars t `Set.intersection` genSet)) [] t)
        return (map regeneralize (extractIBindingsFromPattern pat finalExprType))
      _ -> return (extractIBindingsFromPattern pat finalExprType)
  let s' = composeSubst finalS s

  _env' <- getEnv
  let extendedEnvList = bindings  -- Already a list of (String, TypeScheme)
  (restBindingTIs, restBindings, s2') <- withEnv extendedEnvList $ inferIBindingsWithContext bs env s' ctx
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
  
  -- Infer expressions in extended environment
  results <- withEnv placeholderBindings $ mapM (\(_, expr) -> inferIExprWithContext expr ctx) bindings
  
  let exprTIs = map fst results
      exprTypes = map (tiExprType . fst) results
      substList = map snd results
      s1 = foldr composeSubst s0 substList
  
  -- Unify placeholder types with inferred expression types
  unifySubsts <- zipWithM (\placeholderTy exprTy -> do
    placeholderTy' <- applySubstWithConstraintsM s1 placeholderTy
    exprTy' <- applySubstWithConstraintsM s1 exprTy
    unifyTypesWithContext exprTy' placeholderTy' ctx) placeholderTypes exprTypes
  
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
  let groupFree = Set.unions (map freeTyVars exprTypes')
      monoExtract = concat $ zipWith (\(pat, _, _) ty -> extractIBindingsFromPattern pat ty) placeholders exprTypes'
  finalBindings <-
    if Set.null groupFree
      then return monoExtract
      else do
        envNow <- getEnv
        envFreeImages <- mapM (applySubstWithConstraintsM emptySubst . TVar)
                              (Set.toList (freeVarsInEnv envNow))
        constraintsNow <- getConstraints
        let envFreeZ = Set.unions (map freeTyVars envFreeImages)
            consFree = Set.unions [ freeTyVars t | c <- constraintsNow, t <- constraintTypes c ]
            genSet = groupFree `Set.difference` (envFreeZ `Set.union` consFree)
            genOne (pat, _, _) ty (_, rhs) = case (pat, rhs) of
              (PDPatVar _, IMatcherExpr _) -> extractIBindingsFromPattern pat ty
              (PDPatVar _, _) ->
                map (\(n, Forall _ _ t) ->
                       (n, Forall (Set.toList (freeTyVars t `Set.intersection` genSet)) [] t))
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
  PDPatVar var -> [(extractNameFromVar var, Forall [] [] ty)]
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
inferITopExpr :: ITopExpr -> Infer (Maybe TITopExpr, Subst)
inferITopExpr topExpr = case topExpr of
  IDefine var expr -> do
    env <- getEnv
    -- Check if there's an explicit type signature in the environment
    -- (added by EnvBuilder from DefineWithType)
    case lookupEnv var env of
      Just existingScheme -> do
        -- There's an explicit type signature: check that the inferred type matches
        st <- get
        classEnv <- getClassEnv
        let (instConstraints0, expectedType, newCounter) = instantiate existingScheme (inferCounter st)
            -- Expand superclass constraints so that superclass methods are available
            -- e.g., {Ord a} -> {Ord a, Eq a} since Ord extends Eq
            instConstraints = expandSuperclasses classEnv instConstraints0
        modify $ \s -> s { inferCounter = newCounter }
        -- Add instantiated constraints to the inference context
        -- This is crucial for constraint-aware unification inside the definition body
        -- e.g., when (.) has {Num a}, this constraint must be visible when type-checking t1 * t2
        clearConstraints  -- Start fresh
        clearDeferredHoleChecks
        addConstraints instConstraints

        -- Infer the expression type
        (exprTI, subst1) <- inferIExpr expr
        let exprType = tiExprType exprTI

        -- Unify inferred type with expected type using constraint-aware unification
        -- This is crucial for cases like (.) where type variables have constraints
        -- The constraints from the type signature affect how Tensor types are unified
        let exprCtx = withExpr (prettyStr expr) emptyContext
            -- Apply substitution to constraints to get current state
            currentConstraints = map (applySubstConstraint subst1) instConstraints
        exprType' <- applySubstWithConstraintsM subst1 exprType
        expectedType' <- applySubstWithConstraintsM subst1 expectedType
        -- Matcher-rigidity exception: a matcher LITERAL (possibly behind the
        -- lambdas of a parameterized definition) checked against its
        -- annotation (paper T-MATCHER's checking mode).  The literal's
        -- structural capability is derived by the clause checks at whatever
        -- type the annotation names, so unifying the Matcher parameters at
        -- the result position is sound; rigidity guards already-existing
        -- matcher values, not the literal being defined.
        subst2 <- case rhsCore expr of
          IMatcherExpr _ ->
            unifyMatcherDefType currentConstraints exprType' expectedType' exprCtx
          _ -> unifyTypesWithConstraints currentConstraints exprType' expectedType' exprCtx
        let finalSubst = composeSubst subst2 subst1

        -- Reject the definition if its body needs constraints (on the
        -- signature's type variables) that the signature does not declare
        finalTypeChk <- applySubstWithConstraintsM finalSubst expectedType
        let Var defNameStr _ = var
        checkResidualConstraints defNameStr instConstraints finalTypeChk finalSubst exprCtx

        -- Deferred matcher-hole admissibility (paper PP-Con) at the final types
        flushDeferredHoleChecks finalSubst

        -- Apply final substitution to exprTI to resolve all type variables
        -- IMPORTANT: Use applySubstToTIExprM to adjust substitution based on constraints
        exprTI' <- applySubstToTIExprM finalSubst exprTI

        -- Resolve constraints in exprTI' (Tensor t0 -> t0)
        classEnv <- getClassEnv
        let exprTI'' = resolveConstraintsInTIExpr classEnv finalSubst exprTI'
        
        -- Reconstruct type scheme from exprTI'' to match actual type variables
        -- Use instantiated constraints and apply final substitution
        -- When there's an explicit type annotation, use the expected type
        -- (with substitutions applied) as the final type, not the inferred type.
        -- This ensures that Tensor types are preserved when explicitly annotated.
        finalType <- applySubstWithConstraintsM finalSubst expectedType
        let constraints' = map (applySubstConstraint finalSubst) instConstraints
            envFreeVars = freeVarsInEnv env
            typeFreeVars = freeTyVars finalType
            genVars = Set.toList $ typeFreeVars `Set.difference` envFreeVars
            updatedScheme = Forall genVars constraints' finalType
        
        -- Update the environment with the expanded scheme
        -- This is important so that call sites see the full constraints
        -- (including superclass-expanded ones) and pass all needed dictionaries
        modify $ \s -> s { inferEnv = extendEnv var updatedScheme (inferEnv s) }
        return (Just (TIDefine updatedScheme var exprTI''), finalSubst)
      
      Nothing -> do
        -- No explicit type signature: infer and generalize as before
        clearConstraints  -- Start with fresh constraints for this expression
        clearDeferredHoleChecks
        (exprTI, subst) <- inferIExpr expr
        -- Deferred matcher-hole admissibility (paper PP-Con) at the final types
        flushDeferredHoleChecks subst
        -- Apply the substitution to the stored expression, exactly as the
        -- signature branch does.  Without this, node schemes inside the
        -- expression (notably matcher data-clause arms) keep stale type
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
            isTypeVarType' _        = False
            -- Deduplicate constraints (e.g., {Num a, Num a} -> {Num a})
            generalizedConstraints = nub $ filter isTypeVarConstraint updatedConstraints

        -- Generalize with filtered constraints (only type variables)
        let envFreeVars = freeVarsInEnv env
            typeFreeVars = freeTyVars exprType
            genVars = Set.toList $ typeFreeVars `Set.difference` envFreeVars
            scheme = Forall genVars generalizedConstraints exprType

        -- Add to environment using the Var directly (preserves index info)
        modify $ \s -> s { inferEnv = extendEnv var scheme (inferEnv s) }

        return (Just (TIDefine scheme var exprTI''), subst)
  
  ITest expr -> do
    clearConstraints  -- Start with fresh constraints
    clearDeferredHoleChecks
    (exprTI, subst) <- inferIExpr expr
    flushDeferredHoleChecks subst
    -- Constraints are now in state, will be retrieved by Eval.hs
    return (Just (TITest exprTI), subst)
  
  IExecute expr -> do
    clearConstraints  -- Start with fresh constraints
    clearDeferredHoleChecks
    (exprTI, subst) <- inferIExpr expr
    flushDeferredHoleChecks subst
    -- Constraints are now in state, will be retrieved by Eval.hs
    return (Just (TIExecute exprTI), subst)
  
  ILoadFile _path -> return (Nothing, emptySubst)
  ILoad _lib -> return (Nothing, emptySubst)

  IDefineMany bindings -> do
    -- Process each binding in the list
    env <- getEnv
    results <- mapM (inferBinding env) bindings
    let bindingsTI = map fst results
        substs = map snd results
        combinedSubst = foldr composeSubst emptySubst substs
    return (Just (TIDefineMany bindingsTI), combinedSubst)
    where
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
      inferBinding _env (var, IHashExpr pairs@(_:_)) = do
        clearConstraints
        clearDeferredHoleChecks
        (pairTIs, s) <- foldM dictPair ([], emptySubst) pairs
        v <- freshVar "dictVal"
        let exprTI = TIExpr (Forall [] [] (THash TString v)) (TIHashExpr (reverse pairTIs))
        exprTI' <- applySubstToTIExprM s exprTI
        return ((var, exprTI'), s)
        where
          dictPair (acc, s) (k, vE) = do
            (kTI, s1) <- inferIExprWithContext k emptyContext
            sk <- unifyTypesWithContext (tiExprType kTI) TString emptyContext
            (vTI, s2) <- inferIExprWithContext vE emptyContext
            return ((kTI, vTI) : acc, foldr composeSubst s [s2, sk, s1])
      inferBinding env (var, expr) = do
        -- Check if there's an existing type signature
        case lookupEnv var env of
          Just existingScheme -> do
            -- With type signature: check type
            st <- get
            classEnvForSig <- getClassEnv
            let (instCs0, expectedType, newCounter) = instantiate existingScheme (inferCounter st)
                instCsMany = expandSuperclasses classEnvForSig instCs0
            modify $ \s -> s { inferCounter = newCounter }

            clearConstraints
            clearDeferredHoleChecks
            -- Make the signature's constraints visible while checking the
            -- body (parity with the IDefine signature branch)
            addConstraints instCsMany
            (exprTI, subst1) <- inferIExpr expr
            let exprType = tiExprType exprTI
            exprType' <- applySubstWithConstraintsM subst1 exprType
            expectedType' <- applySubstWithConstraintsM subst1 expectedType
            -- Matcher-rigidity exception for an annotated matcher literal,
            -- possibly lambda-wrapped (see the IDefine signature branch)
            subst2 <- case rhsCore expr of
              IMatcherExpr _ ->
                unifyMatcherDefType [] exprType' expectedType' emptyContext
              _ -> unifyTypesWithTopLevel exprType' expectedType' emptyContext
            let finalSubst = composeSubst subst2 subst1
            -- Reject if the body needs constraints the signature lacks
            finalTypeChk <- applySubstWithConstraintsM finalSubst expectedType
            let Var defNameStr _ = var
            checkResidualConstraints defNameStr instCsMany finalTypeChk finalSubst emptyContext
            flushDeferredHoleChecks finalSubst
            exprTI' <- applySubstToTIExprM finalSubst exprTI
            return ((var, exprTI'), finalSubst)
          
          Nothing -> do
            -- Without type signature: infer and generalize
            clearConstraints
            clearDeferredHoleChecks
            (exprTI, subst) <- inferIExpr expr
            flushDeferredHoleChecks subst
            -- Apply the substitution to the stored expression (same as the
            -- IDefine no-signature branch): node schemes must not keep stale
            -- type variables in their constraints, or TypeClassExpand emits
            -- unbound dictionary references (e.g. inside `declare rule`
            -- generated bodies, which arrive here via IDefineMany).
            exprTI' <- applySubstToTIExprM subst exprTI
            let exprType = tiExprType exprTI'
            constraints <- getConstraints

            -- Resolve constraints based on available instances
            classEnv <- getClassEnv
            let exprTI'' = resolveConstraintsInTIExpr classEnv subst exprTI'
                updatedConstraints = map (resolveConstraintWithInstances classEnv subst) constraints
                -- Filter out constraints on concrete types (non-type-variables)
                isTypeVarConstraint c = any isTypeVarT (constraintTypes c)
                isTypeVarT (TVar _) = True
                isTypeVarT _        = False
                -- Deduplicate constraints (e.g., {Num a, Num a} -> {Num a})
                generalizedConstraints = nub $ filter isTypeVarConstraint updatedConstraints

            -- Generalize the type
            let envFreeVars = freeVarsInEnv env
                typeFreeVars = freeTyVars exprType
                genVars = Set.toList $ typeFreeVars `Set.difference` envFreeVars
                scheme = Forall genVars generalizedConstraints exprType

            -- Add to environment for subsequent bindings using Var directly
            modify $ \s -> s { inferEnv = extendEnv var scheme (inferEnv s) }

            return ((var, exprTI''), subst)
  
  IPatternFunctionDecl name tyVars params retType body -> do
    -- Pattern function type checking (paper PATFUN-DEF):
    -- 1. Linearity side condition: each parameter occurs exactly once in the
    --    body, in declaration order
    -- 2. Add parameters to environment for type checking
    -- 3. Infer body pattern with expected return type, giving each parameter a
    --    fresh structural index beta_i and capturing the body's structural index
    -- 4. Create type scheme with type parameters, and record the structural
    --    signature beta_1 -> ... -> beta_k -> tau_p_body for PAT-APP

    clearConstraints  -- Start fresh

    let ctx = TypeErrorContext
                { errorLocation = Nothing
                , errorExpr = Just ("Pattern function: " ++ name)
                , errorContext = Just ("Expected type: " ++ show retType)
                }
        paramNames = map fst params
        paramUses = filter (`elem` paramNames) (patternVarRefsInOrder body)
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

    -- Add parameters to environment for type checking the body
    -- Note: Parameter types don't need Pattern wrapper (design/pattern.md)
    let paramBindings = map (\(pname, pty) -> (pname, Forall [] [] pty)) params
    withEnv paramBindings $ do
      -- Structural side: a fresh structural index beta_i per parameter; the
      -- ~param embeddings in the body return it (IVarPat), so the body's
      -- structural index records where each argument's structure flows.
      betas <- mapM (\_ -> freshVar "beta") params
      let paramTaupMap = Map.fromList (zip paramNames betas)
      oldParamTaups <- inferPatfunParamTaup <$> get
      oldTaupEqs <- inferPatfunTaupEqs <$> get
      modify $ \z -> z { inferPatfunParamTaup = paramTaupMap, inferPatfunTaupEqs = Just [] }
      bodyResult <- (Right <$> inferIPattern body retType ctx) `catchError` (return . Left)
      taupEqs <- (fromMaybe [] . inferPatfunTaupEqs) <$> get
      modify $ \z -> z { inferPatfunParamTaup = oldParamTaups, inferPatfunTaupEqs = oldTaupEqs }
      (tiBody, _bodyBindings, subst, bodyTaup) <- either throwError return bodyResult

      -- Note: Pattern variables that reference parameters (using ~param) will appear in bodyBindings
      -- but they are NOT conflicts - they are references to the parameters themselves.
      -- Only NEW variable bindings (using $var) would be actual conflicts.
      -- Since the pattern body uses ~p1 and ~p2 (pattern variable references),
      -- not $p1 and $p2 (new bindings), we don't need to check for conflicts here.
      -- The existing semantics already handle this correctly during pattern matching.

      -- Create type scheme with type parameters
      -- Pattern function type: param1 -> param2 -> ... -> retType
      let paramTypes = map snd params
          funcType = foldr TFun retType paramTypes
          typeScheme = Forall tyVars [] funcType

      -- Structural signature beta_1 -> ... -> beta_k -> tau_p_body.  The body's
      -- node-local structural solvers keep only each node's result type, so the
      -- recorded equations are re-solved jointly here and the solution applied
      -- to the signature — recovering the links between the beta_i and the body
      -- skeleton (e.g. pair's beta_1 at the element position of its cons body).
      -- Generalized over all its variables so every application site
      -- instantiates it freshly (PAT-APP, independent of the target side).
      let structSig0 = foldr TFun bodyTaup betas
      structSig <- withIsolatedConstraints $
        (do sEq <- foldM (\acc (x, y) -> do
                       x' <- applySubstWithConstraintsM acc x
                       y' <- applySubstWithConstraintsM acc y
                       s' <- unifyTypesWithContext x' y' ctx
                       return (composeSubst s' acc)) emptySubst taupEqs
            applySubstWithConstraintsM sEq structSig0)
          `catchError` \_ -> return structSig0
      let structScheme = Forall (Set.toList (freeTyVars structSig)) [] structSig

      -- Add pattern function to inferPatternFuncEnv, inferEnv, and the
      -- structural-signature environment
      -- This allows the type checker to recognize it in subsequent declarations
      modify $ \s -> s {
        inferPatternFuncEnv = extendPatternEnv name typeScheme (inferPatternFuncEnv s),
        inferEnv = extendEnv (stringToVar name) typeScheme (inferEnv s),
        inferPatternFuncStructEnv = extendPatternEnv name structScheme (inferPatternFuncStructEnv s)
      }

      return (Just (TIPatternFunctionDecl name typeScheme params retType tiBody), subst)
  
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
    let scheme = Forall [] [] ty
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

-- | Run type inference on IExpr
runInferI :: InferConfig -> TypeEnv -> IExpr -> IO (Either TypeError (Type, Subst, [TypeWarning]))
runInferI cfg env expr = do
  let initState = (initialInferStateWithConfig cfg) { inferEnv = env }
  (result, warnings) <- runInferWithWarnings (inferIExpr expr) initState
  return $ case result of
    Left err -> Left err
    Right (tiExpr, subst) -> Right (tiExprType tiExpr, subst, warnings)

-- | Run type inference on IExpr with initial environment
runInferIWithEnv :: InferConfig -> TypeEnv -> IExpr -> IO (Either TypeError (Type, Subst, TypeEnv, [TypeWarning]))
runInferIWithEnv cfg env expr = do
  let initState = (initialInferStateWithConfig cfg) { inferEnv = env }
  (result, warnings, finalState) <- runInferWithWarningsAndState (inferIExpr expr) initState
  return $ case result of
    Left err -> Left err
    Right (tiExpr, subst) -> Right (tiExprType tiExpr, subst, inferEnv finalState, warnings)
