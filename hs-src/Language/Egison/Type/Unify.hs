{- |
Module      : Language.Egison.Type.Unify
Licence     : MIT

This module provides type unification for the Egison type system.

Three unification modes are supported via 'TensorHandling':
  - 'TensorStrict': Tensor a does NOT unify with a (for TensorMapInsertion)
  - 'TensorTopLevel': Tensor a freely unifies with a (for top-level annotations)
  - 'TensorConstraintAware': Constraint-aware Tensor handling (for general inference)
-}

module Language.Egison.Type.Unify
  ( unify
  , unifyStrict
  , unifyStrictWithConstraints
  , unifyWithTopLevel
  , unifyWithConstraints
  , alignAtSlotWithConstraints
  , unifyExtendedWithTopLevel
  , unifyExtendedWithConstraints
  , unifyMany
  , unifyCapability
  , matchCapability
  , matchOneWay
  , UnifyError(..)
  ) where

import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set

import           Language.Egison.Type.Subst  (Subst(..), applySubst, composeSubst,
                                              emptySubst, singletonSubst,
                                              singletonCapSubst, applyCapSubst,
                                              applySubstConstraint)
import           Language.Egison.Type.Tensor (normalizeTensorType)
import           Language.Egison.Type.Types  (Capability (..), CapVar (..),
                                              TypeFormer (..), TyVar (..), Type (..),
                                              freeCapVars, freeCapVarsCapability,
                                              freeTyVars,
                                              normalizeInductiveTypes,
                                              Constraint(..), SymbolSet(..))
import           Language.Egison.Type.Env    (ClassEnv, lookupInstances, emptyClassEnv)
import           Language.Egison.Type.Types  (instType)

-- | Unification errors
data UnifyError
  = OccursCheck TyVar Type        -- ^ Infinite type detected
  | TypeMismatch Type Type        -- ^ Types cannot be unified
  | MatcherRigidity Type Type     -- ^ Capability mismatch (kept as a constructor
                                  --   name for the existing diagnostic boundary)
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Tensor Handling Modes
--------------------------------------------------------------------------------

-- | Controls how Tensor types interact with non-Tensor types during unification.
data TensorHandling
  = TensorStrict
    -- ^ Tensor a does NOT unify with a. Used for type class instance checking
    -- in TensorMapInsertion to distinguish Tensor types from scalar types.
  | TensorTopLevel
    -- ^ Tensor a freely unifies with a at top-level definitions.
    -- According to type-tensor-simple.md: only for top-level tensor definitions,
    -- Tensor a unifying with a yields a.
  | TensorConstraintAware
    -- ^ Constraint-aware: if type variable has constraints and Tensor lacks
    -- instances for them, prefer binding to the element type instead.
  deriving (Eq)

--------------------------------------------------------------------------------
-- Public API (signatures unchanged)
--------------------------------------------------------------------------------

-- | Unify two types, returning a substitution if successful.
-- Discards the unwrap flag since it's not needed in basic unification.
unify :: Type -> Type -> Either UnifyError Subst
unify t1 t2 = fmap fst (unifyWithConstraints emptyClassEnv [] t1 t2)

-- | Strict unification that does NOT allow Tensor a to unify with a.
-- Used for checking type class instances in TensorMapInsertion.
unifyStrict :: Type -> Type -> Either UnifyError Subst
unifyStrict = unifyStrictWithConstraints emptyClassEnv []

-- | Strict unification with type class constraints.
-- IMPORTANT: This does NOT allow Tensor a to unify with a (strict unification).
unifyStrictWithConstraints :: ClassEnv -> [Constraint] -> Type -> Type -> Either UnifyError Subst
unifyStrictWithConstraints classEnv constraints t1 t2 =
  fmap fst $ unifyNormalized TensorStrict classEnv constraints t1 t2

-- | Unify two types, allowing Tensor a to unify with a at top-level definitions.
-- According to type-tensor-simple.md: only for top-level tensor definitions.
unifyWithTopLevel :: Type -> Type -> Either UnifyError Subst
unifyWithTopLevel t1 t2 =
  fmap fst $ unifyNormalized TensorTopLevel emptyClassEnv [] t1 t2

-- | Unify two types while considering type class constraints.
-- Returns (Subst, Bool) where Bool indicates if Tensor was unwrapped.
unifyWithConstraints :: ClassEnv -> [Constraint] -> Type -> Type -> Either UnifyError (Subst, Bool)
unifyWithConstraints = unifyNormalized TensorConstraintAware

-- | TypePM-style producer-to-consumer alignment at an explicit MatcherSlot
-- use site.  Generic equality deliberately does not perform this coercion.
-- The first operand is the inferred producer and the second is the expected
-- consumer.  A raw tuple is accepted only in this role-aware entry point and
-- is checked componentwise against a product slot.
alignAtSlotWithConstraints
  :: ClassEnv
  -> [Constraint]
  -> Type
  -> Type
  -> Either UnifyError (Subst, Bool)
alignAtSlotWithConstraints classEnv constraints inferred expected =
  let inferred' =
        normalizeInductiveTypes (normalizeTensorType inferred)
      expected' =
        normalizeInductiveTypes (normalizeTensorType expected)
  in alignAtSlotG
       TensorConstraintAware classEnv constraints inferred' expected'

-- | Egison-extension fallback for top-level inference.  Unlike the core API,
-- this retains the historical behavior of aligning matcher capabilities at
-- every recursive type position.
unifyExtendedWithTopLevel :: Type -> Type -> Either UnifyError Subst
unifyExtendedWithTopLevel t1 t2 =
  fmap fst $ unifyExtendedNormalized TensorTopLevel emptyClassEnv [] t1 t2

-- | Egison-extension fallback for constraint-aware inference.  Callers should
-- try 'unifyWithConstraints' first and use this only at an explicitly warned
-- non-core boundary.
unifyExtendedWithConstraints
  :: ClassEnv
  -> [Constraint]
  -> Type
  -> Type
  -> Either UnifyError (Subst, Bool)
unifyExtendedWithConstraints =
  unifyExtendedNormalized TensorConstraintAware

-- | Unify a list of type pairs.
unifyMany :: [Type] -> [Type] -> Either UnifyError Subst
unifyMany ts1 ts2 =
  fmap fst $ unifyManyRootG TensorConstraintAware emptyClassEnv [] ts1 ts2

--------------------------------------------------------------------------------
-- Capability unification
--------------------------------------------------------------------------------

-- | Unify two capabilities using only capability constructors and capability
-- variables.  No ordinary type normalization, CAS equivalence, subtyping, or
-- type-class relation is visible at this boundary.
unifyCapability :: Capability -> Capability -> Either UnifyError Subst
unifyCapability cap1 cap2
  | not (wellFormedCapability cap1) =
      Left (capabilityMismatch cap1 CapAny)
  | not (wellFormedCapability cap2) =
      Left (capabilityMismatch cap2 CapAny)
  | otherwise =
      go [(cap1, cap2)] emptySubst
  where
    go [] acc = Right acc
    go ((left0, right0) : rest) acc =
      let left  = applyCapSubst acc left0
          right = applyCapSubst acc right0
      in if left == right
           then go rest acc
           else case (left, right) of
             (CapVar v, cap) -> bind v cap rest acc
             (cap, CapVar v) -> bind v cap rest acc
             (CapCon f xs, CapCon g ys)
               | f == g
               , length xs == length ys ->
                   go (zip xs ys ++ rest) acc
             (CapTuple xs, CapTuple ys)
               | length xs == length ys ->
                   go (zip xs ys ++ rest) acc
             _ -> Left (capabilityMismatch left right)

    bind v cap rest acc
      | CapVar v == cap = go rest acc
      | v `Set.member` freeCapVarsCapability cap =
          Left (capabilityMismatch (CapVar v) cap)
      | otherwise =
          let s = singletonCapSubst v cap
          in go rest (composeSubst s acc)

-- | Producer-to-consumer capability matching.
--
-- Only flexible variables belonging exclusively to the original consumer
-- form the substitution domain.  Producer variables are rigid for this
-- judgment, including a variable shared syntactically by producer and
-- consumer.  The stable domain is captured before decomposition so a
-- producer variable copied into a consumer position never becomes bindable
-- later in the same match.  A literal consumer 'CapAny' is a wildcard, but a
-- consumer variable previously bound to 'CapAny' is not: later occurrences
-- of that variable must agree strictly with the saved ground capability.
matchCapability :: Capability -> Capability -> Either UnifyError Subst
matchCapability producer0 consumer0 =
  matchCapabilityWithDomain
      True
      (freeCapVarsCapability consumer0
        `Set.difference` freeCapVarsCapability producer0)
      producer0
      consumer0
      emptySubst

-- | Continue producer-to-consumer capability matching with one stable
-- consumer-owned domain and an existing paired substitution.  The Boolean
-- records whether literal 'CapAny' nodes come from the original consumer
-- shape.  It is false for a type obtained by expanding an earlier consumer
-- variable binding, because an @Any@ stored in that binding is a rigid value,
-- not a fresh wildcard occurrence.
matchCapabilityWithDomain
  :: Bool
  -> Set.Set CapVar
  -> Capability
  -> Capability
  -> Subst
  -> Either UnifyError Subst
matchCapabilityWithDomain literalAnyIsWildcard bindable producer0 consumer0 initialSubst
  | not (wellFormedCapability producer0) =
      Left (capabilityMismatch producer0 CapAny)
  | not (wellFormedCapability consumer0) =
      Left (capabilityMismatch consumer0 CapAny)
  | otherwise =
      go [(producer0, consumer0)] initialSubst
  where
    go [] acc = Right acc
    go ((producer, originalConsumer) : rest) acc =
      let consumer = applyCapSubst acc originalConsumer
      in if producer == consumer
           then go rest acc
           else case (producer, originalConsumer) of
             -- Only a literal Any in the declared consumer shape is a
             -- wildcard.  Inspecting the original node here is essential:
             -- applying the accumulated substitution first would make a
             -- repeated variable bound to Any indistinguishable from this
             -- literal case.
             (_, CapAny)
               | literalAnyIsWildcard ->
                   go rest acc
             (cap, CapVar variable)
               | variable `Set.member` bindable
               , Map.notMember variable (unCapSubst acc) ->
                   bindConsumer variable cap rest acc
             (CapCon producerFormer producerChildren,
              CapCon consumerFormer originalChildren)
               | producerFormer == consumerFormer
               , length producerChildren == length originalChildren ->
                   go (zip producerChildren originalChildren ++ rest) acc
             (CapTuple producerComponents, CapTuple originalComponents)
               | length producerComponents == length originalComponents ->
                   go (zip producerComponents originalComponents ++ rest) acc
             _ ->
               Left (capabilityMismatch producer consumer)

    bindConsumer variable capability rest acc
      | CapVar variable == capability =
          go rest acc
      | variable `Set.member` freeCapVarsCapability capability =
          Left (capabilityMismatch capability (CapVar variable))
      | otherwise =
          let substitution = singletonCapSubst variable capability
          in go rest (composeSubst substitution acc)

wellFormedCapability :: Capability -> Bool
wellFormedCapability capability =
  case capability of
    CapAny ->
      True
    CapVar _ ->
      True
    CapSkolem _ ->
      True
    CapTuple components ->
      all wellFormedCapability components
    CapCon former arguments ->
      length arguments == typeFormerArity former
        && all wellFormedCapability arguments

capabilityMismatch :: Capability -> Capability -> UnifyError
capabilityMismatch left right =
  MatcherRigidity (TMatcher left TAny) (TMatcher right TAny)

--------------------------------------------------------------------------------
-- Normalization Entry Point
--------------------------------------------------------------------------------

-- | Trivial success: empty substitution, no Tensor unwrapping.
ok :: Either UnifyError (Subst, Bool)
ok = Right (emptySubst, False)

-- | Normalize types and align the two outermost types.  Capability variables
-- are solved symmetrically only when both outer constructors are 'TMatcher'
-- or both are 'TMatcherSlot'.  Once their target components are entered,
-- matcher capability annotations are rigid ordinary-type data.
unifyNormalized :: TensorHandling -> ClassEnv -> [Constraint] -> Type -> Type -> Either UnifyError (Subst, Bool)
unifyNormalized mode classEnv constraints t1 t2 =
  let t1' = normalizeInductiveTypes (normalizeTensorType t1)
      t2' = normalizeInductiveTypes (normalizeTensorType t2)
  in alignRootG mode classEnv constraints t1' t2'

-- | Normalize a type component below the outer alignment boundary.  This is
-- the executable counterpart of the target-only MGU in TypePM: nested
-- matcher/slot capabilities must already be equal and are never refined here.
unifyNestedNormalized
  :: TensorHandling
  -> ClassEnv
  -> [Constraint]
  -> Type
  -> Type
  -> Either UnifyError (Subst, Bool)
unifyNestedNormalized mode classEnv constraints t1 t2 =
  let t1' = normalizeInductiveTypes (normalizeTensorType t1)
      t2' = normalizeInductiveTypes (normalizeTensorType t2)
  in unifyG mode classEnv constraints t1' t2'

-- | Normalize and use the Egison-extension solver.  Recursive calls re-enter
-- this same entry point, intentionally restoring the pre-core behavior in
-- which each nested matcher/slot node is an alignment boundary.
unifyExtendedNormalized
  :: TensorHandling
  -> ClassEnv
  -> [Constraint]
  -> Type
  -> Type
  -> Either UnifyError (Subst, Bool)
unifyExtendedNormalized mode classEnv constraints t1 t2 =
  let t1' = normalizeInductiveTypes (normalizeTensorType t1)
      t2' = normalizeInductiveTypes (normalizeTensorType t2)
  in unifyExtendedG mode classEnv constraints t1' t2'

-- | Root equality corresponding to TypePM's @alignTypesCore@.  Slot-use
-- coercion and product matcher conveniences live exclusively in
-- 'alignAtSlotG' or the explicitly extended solver below.
alignRootG
  :: TensorHandling
  -> ClassEnv
  -> [Constraint]
  -> Type
  -> Type
  -> Either UnifyError (Subst, Bool)
alignRootG _ _ _ left@(TMatcher _ _) right@(TTuple _) =
  Left (MatcherRigidity left right)
alignRootG _ _ _ left@(TTuple _) right@(TMatcher _ _) =
  Left (MatcherRigidity left right)
alignRootG _ _ _ left@(TMatcherSlot _ _) right@(TTuple _) =
  Left (MatcherRigidity left right)
alignRootG _ _ _ left@(TTuple _) right@(TMatcherSlot _ _) =
  Left (MatcherRigidity left right)
alignRootG mode ce cs (TMatcher cap1 target1) (TMatcher cap2 target2) = do
  capSubst <- unifyCapability cap1 cap2
  let cs' = map (applySubstConstraint capSubst) cs
  (targetSubst, flag) <-
    unifyNestedNormalized mode ce cs'
      (applySubst capSubst target1)
      (applySubst capSubst target2)
  Right (composeSubst targetSubst capSubst, flag)
alignRootG mode ce cs (TMatcherSlot cap1 target1) (TMatcherSlot cap2 target2) = do
  capSubst <- unifyCapability cap1 cap2
  let cs' = map (applySubstConstraint capSubst) cs
  (targetSubst, flag) <-
    unifyNestedNormalized mode ce cs'
      (applySubst capSubst target1)
      (applySubst capSubst target2)
  Right (composeSubst targetSubst capSubst, flag)
alignRootG _ _ _ left@(TMatcher _ _) right@(TMatcherSlot _ _) =
  Left (MatcherRigidity left right)
alignRootG _ _ _ left@(TMatcherSlot _ _) right@(TMatcher _ _) =
  Left (MatcherRigidity left right)
alignRootG mode ce cs t1 t2 =
  unifyG mode ce cs t1 t2

-- | Role-aware counterpart of TypePM's @alignAtSlot@.  Only this boundary
-- admits producer-to-slot coercion; all other pairs fall back to core root
-- equality.  In particular, reversing Matcher and MatcherSlot is rejected.
alignAtSlotG
  :: TensorHandling
  -> ClassEnv
  -> [Constraint]
  -> Type
  -> Type
  -> Either UnifyError (Subst, Bool)
alignAtSlotG mode ce cs
             (TMatcher producerCap producerTarget)
             (TMatcherSlot consumerCap consumerTarget) =
  coerceMatcherToSlot
    mode ce cs producerCap producerTarget consumerCap consumerTarget
-- 'Any' carries no evidence that its value is a matcher.  Letting the
-- catch-all rule in 'unifyG' accept it here would silently manufacture the
-- Matcher/MatcherSlot head (and therefore a capability witness).  Production
-- Egison may retain that gradual behavior only through the warned extension
-- solver used by Infer.
alignAtSlotG _ _ _ inferred@TAny expected@(TMatcherSlot _ _) =
  Left (MatcherRigidity inferred expected)
alignAtSlotG TensorConstraintAware ce cs
             (TTuple producers)
             (TMatcherSlot consumerCap consumerTarget) =
  coerceSlotTuple
    TensorConstraintAware ce cs consumerCap consumerTarget producers
alignAtSlotG mode ce cs inferred expected =
  alignRootG mode ce cs inferred expected

--------------------------------------------------------------------------------
-- Generic Core Unification
--------------------------------------------------------------------------------

-- | Target-only core unification parametrized by TensorHandling mode.
-- Recursive calls remain below the root alignment boundary, so capability
-- annotations nested in ordinary type structure are compared rigidly.
unifyG :: TensorHandling -> ClassEnv -> [Constraint] -> Type -> Type -> Either UnifyError (Subst, Bool)

-- Same types unify trivially
unifyG _ _ _ TInt TInt = ok
unifyG _ _ _ TMathValue TMathValue = ok
unifyG _ _ _ TPolyExpr TPolyExpr = ok
unifyG _ _ _ TTermExpr TTermExpr = ok
unifyG _ _ _ TSymbolExpr TSymbolExpr = ok
unifyG _ _ _ TIndexExpr TIndexExpr = ok
unifyG _ _ _ TFloat TFloat = ok
unifyG _ _ _ TBool TBool = ok
unifyG _ _ _ TChar TChar = ok
unifyG _ _ _ TString TString = ok
unifyG _ _ _ (TSkolem v1) (TSkolem v2)
  | v1 == v2 = ok

-- Special rule: TInt and TMathValue unify
unifyG _ _ _ TInt TMathValue = ok
unifyG _ _ _ TMathValue TInt = ok

-- Phase 5.5 (simplified subtype unification): all CAS-family types
-- (Factor / Frac / Poly) unify with MathValue and with TInt (=MathValue).
-- This is the "every CAS type is a subtype of MathValue" relationship from
-- the design's type-inclusion graph. The runtime values are all CASValue
-- so this is sound at the value level; full Embed/coerce machinery (with
-- runtime checks) is still pending.
unifyG _ _ _ TMathValue TFactor   = ok
unifyG _ _ _ TFactor    TMathValue = ok
unifyG _ _ _ TInt        TFactor   = ok
unifyG _ _ _ TFactor     TInt      = ok
unifyG _ _ _ TMathValue (TTerm _ _) = ok
unifyG _ _ _ (TTerm _ _) TMathValue = ok
unifyG _ _ _ TInt       (TTerm _ _) = ok
unifyG _ _ _ (TTerm _ _) TInt       = ok
unifyG _ _ _ TMathValue (TFrac _)  = ok
unifyG _ _ _ (TFrac _)  TMathValue = ok
unifyG _ _ _ TInt       (TFrac _)  = ok
unifyG _ _ _ (TFrac _)  TInt       = ok
unifyG _ _ _ TMathValue (TPoly _ _) = ok
unifyG _ _ _ (TPoly _ _) TMathValue = ok
unifyG _ _ _ TInt        (TPoly _ _) = ok
unifyG _ _ _ (TPoly _ _) TInt        = ok
-- Cross-level widening: any Frac chain unifies with any Poly chain via reshape
-- (e.g. `def e2 : Poly (Frac Integer) [..] := e1` where e1 : Frac Integer).
unifyG _ _ _ (TFrac _)   (TPoly _ _) = ok
unifyG _ _ _ (TPoly _ _) (TFrac _)   = ok
-- Factor widening into Frac/Poly chains (e.g. `def e4 : Frac (Poly Integer [x]) := x`).
unifyG _ _ _ TFactor     (TFrac _)   = ok
unifyG _ _ _ (TFrac _)   TFactor     = ok
unifyG _ _ _ TFactor     (TPoly _ _) = ok
unifyG _ _ _ (TPoly _ _) TFactor     = ok

-- Type variables: delegated to mode-specific handler
unifyG mode ce cs (TVar v) t = unifyVarG mode ce cs v t
unifyG mode ce cs t (TVar v) = unifyVarG mode ce cs v t

-- Tuples
unifyG mode ce cs (TTuple ts1) (TTuple ts2)
  | length ts1 == length ts2 = unifyManyG mode ce cs ts1 ts2
  | otherwise = Left $ TypeMismatch (TTuple ts1) (TTuple ts2)

-- Collections
unifyG mode ce cs (TCollection t1) (TCollection t2) =
  unifyNestedNormalized mode ce cs t1 t2

-- Inductive types
unifyG mode ce cs (TInductive n1 ts1) (TInductive n2 ts2)
  | n1 == n2 && length ts1 == length ts2 = unifyManyG mode ce cs ts1 ts2
  | otherwise = Left $ TypeMismatch (TInductive n1 ts1) (TInductive n2 ts2)

-- Hash types (two components with substitution threading)
unifyG mode ce cs (THash k1 v1) (THash k2 v2) = do
  (s1, f1) <- unifyNestedNormalized mode ce cs k1 k2
  let cs' = map (applySubstConstraint s1) cs
  (s2, f2) <-
    unifyNestedNormalized mode ce cs'
      (applySubst s1 v1)
      (applySubst s1 v2)
  Right (composeSubst s2 s1, f1 || f2)

-- Nested matcher and slot annotations are rigid.  Symmetric outer capability
-- equality is available through 'alignRootG'; producer-to-slot coercion is
-- available only through the role-aware 'alignAtSlotG'.
unifyG mode ce cs left@(TMatcher cap1 target1)
                          right@(TMatcher cap2 target2)
  | cap1 == cap2 =
      unifyNestedNormalized mode ce cs target1 target2
  | otherwise =
      Left (MatcherRigidity left right)
unifyG mode ce cs left@(TMatcherSlot cap1 target1)
                          right@(TMatcherSlot cap2 target2)
  | cap1 == cap2 =
      unifyNestedNormalized mode ce cs target1 target2
  | otherwise =
      Left (MatcherRigidity left right)
unifyG _ _ _ left@(TMatcher _ _) right@(TMatcherSlot _ _) =
  Left (MatcherRigidity left right)
unifyG _ _ _ left@(TMatcherSlot _ _) right@(TMatcher _ _) =
  Left (MatcherRigidity left right)
unifyG TensorConstraintAware _ _ left@(TMatcher _ _) right@(TTuple _) =
  Left (MatcherRigidity left right)
unifyG TensorConstraintAware _ _ left@(TTuple _) right@(TMatcher _ _) =
  Left (MatcherRigidity left right)
unifyG TensorConstraintAware _ _ left@(TMatcherSlot _ _) right@(TTuple _) =
  Left (MatcherRigidity left right)
unifyG TensorConstraintAware _ _ left@(TTuple _) right@(TMatcherSlot _ _) =
  Left (MatcherRigidity left right)

-- Function types (two components with substitution threading)
unifyG mode ce cs (TFun a1 r1) (TFun a2 r2) = do
  (s1, f1) <- unifyNestedNormalized mode ce cs a1 a2
  let cs' = map (applySubstConstraint s1) cs
  (s2, f2) <-
    unifyNestedNormalized mode ce cs'
      (applySubst s1 r1)
      (applySubst s1 r2)
  Right (composeSubst s2 s1, f1 || f2)

-- IO types
unifyG mode ce cs (TIO t1) (TIO t2) =
  unifyNestedNormalized mode ce cs t1 t2

-- IORef types
unifyG mode ce cs (TIORef t1) (TIORef t2) =
  unifyNestedNormalized mode ce cs t1 t2

-- Port type
unifyG _ _ _ TPort TPort = ok

-- CAS types
unifyG _ _ _ TFactor TFactor = ok

unifyG mode ce cs (TTerm t1 ss1) (TTerm t2 ss2) = do
  (s1, f1) <- unifyNestedNormalized mode ce cs t1 t2
  case unifySymbolSets ss1 ss2 of
    Just _  -> Right (s1, f1)
    Nothing -> Left $ TypeMismatch (TTerm t1 ss1) (TTerm t2 ss2)

unifyG mode ce cs (TFrac t1) (TFrac t2) =
  unifyNestedNormalized mode ce cs t1 t2

unifyG mode ce cs (TPoly t1 ss1) (TPoly t2 ss2) = do
  -- First unify the coefficient types
  (s1, f1) <- unifyNestedNormalized mode ce cs t1 t2
  -- Then unify the symbol sets
  case unifySymbolSets ss1 ss2 of
    Just _  -> Right (s1, f1)
    Nothing -> Left $ TypeMismatch (TPoly t1 ss1) (TPoly t2 ss2)

-- Tensor types: both Tensor — same for all modes
unifyG mode ce cs (TTensor t1) (TTensor t2) =
  unifyNestedNormalized mode ce cs t1 t2

-- Tensor vs non-Tensor: TopLevel allows unwrapping
unifyG TensorTopLevel _ _ (TTensor t1) t2 = do
  (s, _) <- unifyNestedNormalized TensorTopLevel emptyClassEnv [] t1 t2
  Right (s, True)
unifyG TensorTopLevel _ _ t1 (TTensor t2) = do
  (s, _) <- unifyNestedNormalized TensorTopLevel emptyClassEnv [] t1 t2
  Right (s, True)

-- Tensor vs non-Tensor: ConstraintAware uses constraint-aware logic
unifyG TensorConstraintAware ce cs (TTensor t1) t2 =
  unifyTensorWithConstraints ce cs t1 t2
unifyG TensorConstraintAware ce cs t1 (TTensor t2) =
  unifyTensorWithConstraints ce cs t2 t1

-- TensorStrict: Tensor vs non-Tensor falls through to mismatch below

-- TAny unifies with anything
unifyG _ _ _ TAny _ = ok
unifyG _ _ _ _ TAny = ok

-- Mismatched types
unifyG _ _ _ t1 t2 = Left $ TypeMismatch t1 t2

--------------------------------------------------------------------------------
-- Egison-extension recursive alignment
--------------------------------------------------------------------------------

-- | Historical Egison unification used every recursively visited type node as
-- a matcher-alignment boundary.  This solver is deliberately separate from
-- 'unifyG': inference may fall back to it after reporting that a term is
-- outside the mechanized core, while the core APIs retain rigid annotations.
unifyExtendedG
  :: TensorHandling
  -> ClassEnv
  -> [Constraint]
  -> Type
  -> Type
  -> Either UnifyError (Subst, Bool)

-- Preserve the core solver's variable precedence over structural rules.
unifyExtendedG mode ce cs (TVar v) t =
  unifyVarG mode ce cs v t
unifyExtendedG mode ce cs t (TVar v) =
  unifyVarG mode ce cs v t

unifyExtendedG mode ce cs (TTuple ts1) (TTuple ts2)
  | length ts1 == length ts2 =
      unifyManyExtendedG mode ce cs ts1 ts2
  | otherwise =
      Left $ TypeMismatch (TTuple ts1) (TTuple ts2)

unifyExtendedG mode ce cs (TCollection t1) (TCollection t2) =
  unifyExtendedNormalized mode ce cs t1 t2

unifyExtendedG mode ce cs (TInductive n1 ts1) (TInductive n2 ts2)
  | n1 == n2 && length ts1 == length ts2 =
      unifyManyExtendedG mode ce cs ts1 ts2
  | otherwise =
      Left $ TypeMismatch (TInductive n1 ts1) (TInductive n2 ts2)

unifyExtendedG mode ce cs (THash k1 v1) (THash k2 v2) = do
  (s1, f1) <- unifyExtendedNormalized mode ce cs k1 k2
  let cs' = map (applySubstConstraint s1) cs
  (s2, f2) <-
    unifyExtendedNormalized mode ce cs'
      (applySubst s1 v1)
      (applySubst s1 v2)
  Right (composeSubst s2 s1, f1 || f2)

unifyExtendedG TensorConstraintAware ce cs
                 (TMatcher cap target) (TTuple ts) =
  unifyMatcherWithTupleExtendedG ce cs cap target ts
unifyExtendedG TensorConstraintAware ce cs
                 (TTuple ts) (TMatcher cap target) =
  unifyMatcherWithTupleExtendedG ce cs cap target ts

unifyExtendedG TensorConstraintAware ce cs
                 (TMatcherSlot cap target) (TTuple tys) =
  coerceSlotTupleExtended TensorConstraintAware ce cs cap target tys
unifyExtendedG TensorConstraintAware ce cs
                 (TTuple tys) (TMatcherSlot cap target) =
  coerceSlotTupleExtended TensorConstraintAware ce cs cap target tys

unifyExtendedG mode ce cs
                 (TMatcher cap1 target1) (TMatcher cap2 target2) = do
  capSubst <- unifyCapability cap1 cap2
  let cs' = map (applySubstConstraint capSubst) cs
  (targetSubst, flag) <-
    unifyExtendedNormalized mode ce cs'
      (applySubst capSubst target1)
      (applySubst capSubst target2)
  Right (composeSubst targetSubst capSubst, flag)

unifyExtendedG mode ce cs
                 (TMatcherSlot cap1 target1)
                 (TMatcherSlot cap2 target2) = do
  capSubst <- unifyCapability cap1 cap2
  let cs' = map (applySubstConstraint capSubst) cs
  (targetSubst, flag) <-
    unifyExtendedNormalized mode ce cs'
      (applySubst capSubst target1)
      (applySubst capSubst target2)
  Right (composeSubst targetSubst capSubst, flag)

unifyExtendedG mode ce cs
                 (TMatcher matcherCap matcherTarget)
                 (TMatcherSlot slotCap slotTarget) =
  coerceMatcherToSlotExtended
    mode ce cs matcherCap matcherTarget slotCap slotTarget
unifyExtendedG mode ce cs
                 (TMatcherSlot slotCap slotTarget)
                 (TMatcher matcherCap matcherTarget) =
  coerceMatcherToSlotExtended
    mode ce cs matcherCap matcherTarget slotCap slotTarget

unifyExtendedG mode ce cs (TFun a1 r1) (TFun a2 r2) = do
  (s1, f1) <- unifyExtendedNormalized mode ce cs a1 a2
  let cs' = map (applySubstConstraint s1) cs
  (s2, f2) <-
    unifyExtendedNormalized mode ce cs'
      (applySubst s1 r1)
      (applySubst s1 r2)
  Right (composeSubst s2 s1, f1 || f2)

unifyExtendedG mode ce cs (TIO t1) (TIO t2) =
  unifyExtendedNormalized mode ce cs t1 t2
unifyExtendedG mode ce cs (TIORef t1) (TIORef t2) =
  unifyExtendedNormalized mode ce cs t1 t2

unifyExtendedG mode ce cs (TTerm t1 ss1) (TTerm t2 ss2) = do
  (s1, f1) <- unifyExtendedNormalized mode ce cs t1 t2
  case unifySymbolSets ss1 ss2 of
    Just _  -> Right (s1, f1)
    Nothing -> Left $ TypeMismatch (TTerm t1 ss1) (TTerm t2 ss2)

unifyExtendedG mode ce cs (TFrac t1) (TFrac t2) =
  unifyExtendedNormalized mode ce cs t1 t2

unifyExtendedG mode ce cs (TPoly t1 ss1) (TPoly t2 ss2) = do
  (s1, f1) <- unifyExtendedNormalized mode ce cs t1 t2
  case unifySymbolSets ss1 ss2 of
    Just _  -> Right (s1, f1)
    Nothing -> Left $ TypeMismatch (TPoly t1 ss1) (TPoly t2 ss2)

unifyExtendedG mode ce cs (TTensor t1) (TTensor t2) =
  unifyExtendedNormalized mode ce cs t1 t2

unifyExtendedG TensorTopLevel _ _ (TTensor t1) t2 = do
  (s, _) <-
    unifyExtendedNormalized TensorTopLevel emptyClassEnv [] t1 t2
  Right (s, True)
unifyExtendedG TensorTopLevel _ _ t1 (TTensor t2) = do
  (s, _) <-
    unifyExtendedNormalized TensorTopLevel emptyClassEnv [] t1 t2
  Right (s, True)

unifyExtendedG TensorConstraintAware ce cs (TTensor t1) t2 =
  unifyTensorWithConstraintsExtended ce cs t1 t2
unifyExtendedG TensorConstraintAware ce cs t1 (TTensor t2) =
  unifyTensorWithConstraintsExtended ce cs t2 t1

-- Base types, CAS widening, skolems, Any, and ordinary mismatches are shared
-- with the core solver because none of them recursively aligns capabilities.
unifyExtendedG mode ce cs t1 t2 =
  unifyG mode ce cs t1 t2

--------------------------------------------------------------------------------
-- Generic Unify-Many
--------------------------------------------------------------------------------

-- | Public list alignment: each list element is an outer constraint boundary.
unifyManyRootG
  :: TensorHandling
  -> ClassEnv
  -> [Constraint]
  -> [Type]
  -> [Type]
  -> Either UnifyError (Subst, Bool)
unifyManyRootG _ _ _ [] [] = ok
unifyManyRootG mode ce cs (t1:ts1) (t2:ts2) = do
  (s1, f1) <- unifyNormalized mode ce cs t1 t2
  let cs' = map (applySubstConstraint s1) cs
  (s2, f2) <-
    unifyManyRootG mode ce cs'
      (map (applySubst s1) ts1)
      (map (applySubst s1) ts2)
  Right (composeSubst s2 s1, f1 || f2)
unifyManyRootG _ _ _ _ _ =
  Left $ TypeMismatch (TTuple []) (TTuple [])

-- | Unify child lists below an ordinary type constructor.  Matcher
-- capabilities in each child are therefore rigid.
unifyManyG :: TensorHandling -> ClassEnv -> [Constraint] -> [Type] -> [Type] -> Either UnifyError (Subst, Bool)
unifyManyG _ _ _ [] [] = ok
unifyManyG mode ce cs (t1:ts1) (t2:ts2) = do
  (s1, f1) <- unifyNestedNormalized mode ce cs t1 t2
  let cs' = map (applySubstConstraint s1) cs
  (s2, f2) <-
    unifyManyG mode ce cs'
      (map (applySubst s1) ts1)
      (map (applySubst s1) ts2)
  Right (composeSubst s2 s1, f1 || f2)
unifyManyG _ _ _ _ _ = Left $ TypeMismatch (TTuple []) (TTuple [])

-- | Extension-mode child-list unification.  Each child recursively regains
-- the historical matcher-alignment behavior.
unifyManyExtendedG
  :: TensorHandling
  -> ClassEnv
  -> [Constraint]
  -> [Type]
  -> [Type]
  -> Either UnifyError (Subst, Bool)
unifyManyExtendedG _ _ _ [] [] = ok
unifyManyExtendedG mode ce cs (t1:ts1) (t2:ts2) = do
  (s1, f1) <- unifyExtendedNormalized mode ce cs t1 t2
  let cs' = map (applySubstConstraint s1) cs
  (s2, f2) <-
    unifyManyExtendedG mode ce cs'
      (map (applySubst s1) ts1)
      (map (applySubst s1) ts2)
  Right (composeSubst s2 s1, f1 || f2)
unifyManyExtendedG _ _ _ _ _ =
  Left $ TypeMismatch (TTuple []) (TTuple [])

--------------------------------------------------------------------------------
-- Variable Unification (mode-specific dispatch)
--------------------------------------------------------------------------------

-- | Unify a type variable with a type, delegating to mode-specific logic.
unifyVarG :: TensorHandling -> ClassEnv -> [Constraint] -> TyVar -> Type -> Either UnifyError (Subst, Bool)
unifyVarG TensorStrict ce cs v t =
  fmap (\s -> (s, False)) $ unifyVarStrict ce cs v t
unifyVarG TensorTopLevel _ _ v t =
  fmap (\s -> (s, False)) $ unifyVarSimple v t
unifyVarG TensorConstraintAware ce cs v t =
  unifyVarConstraintAware ce cs v t

-- | Simple variable unification (no constraint or Tensor logic).
unifyVarSimple :: TyVar -> Type -> Either UnifyError Subst
unifyVarSimple v t
  | TVar v == t = Right emptySubst
  | v `Set.member` freeTyVars t = Left $ OccursCheck v t
  | otherwise = Right $ singletonSubst v t

-- | Strict variable unification with constraints.
-- Tensor a does NOT unify with a unless all constraints are satisfied by Tensor.
unifyVarStrict :: ClassEnv -> [Constraint] -> TyVar -> Type -> Either UnifyError Subst
unifyVarStrict classEnv constraints v t
  | TVar v == t = Right emptySubst
  | otherwise = case t of
      TTensor elemType ->
        let varConstraints = filter (\c -> TVar v `elem` constraintTypes c) constraints
        in if null varConstraints
           then occursCheckAndBind v t
           else if all (hasInstanceForTensorType classEnv elemType) varConstraints
                then occursCheckAndBind v t
                else Left $ TypeMismatch (TVar v) t
      _ -> occursCheckAndBind v t

-- | Constraint-aware variable unification.
-- Returns (Subst, Bool) where Bool indicates if Tensor was unwrapped.
unifyVarConstraintAware :: ClassEnv -> [Constraint] -> TyVar -> Type -> Either UnifyError (Subst, Bool)
unifyVarConstraintAware classEnv constraints v t
  | TVar v == t = Right (emptySubst, False)
  | otherwise = case t of
      TTensor elemType ->
        let varConstraints = filter (\c -> TVar v `elem` constraintTypes c) constraints
        in if null varConstraints
           then fmap (\s -> (s, False)) $ occursCheckAndBind v t
           else if all (hasInstanceForTensorType classEnv elemType) varConstraints
                then fmap (\s -> (s, False)) $ occursCheckAndBind v t
                else
                  -- Some constraint lacks Tensor instance, bind to element type instead.
                  -- This allows tensorMap to handle the Tensor -> scalar conversion.
                  if TVar v == elemType
                  then Right (emptySubst, True)
                  else if v `Set.member` freeTyVars elemType
                       then Left $ OccursCheck v elemType
                       else Right (singletonSubst v elemType, True)
      _ ->
        fmap (\s -> (s, False)) $ occursCheckAndBind v t

-- | Occurs check and variable binding (shared helper).
occursCheckAndBind :: TyVar -> Type -> Either UnifyError Subst
occursCheckAndBind v t
  | v `Set.member` freeTyVars t = Left $ OccursCheck v t
  | otherwise = Right $ singletonSubst v t

--------------------------------------------------------------------------------
-- Tensor-Specific Helpers (ConstraintAware mode only)
--------------------------------------------------------------------------------

-- | Unify Tensor elemType with a non-Tensor type, considering constraints.
unifyTensorWithConstraints :: ClassEnv -> [Constraint] -> Type -> Type -> Either UnifyError (Subst, Bool)
unifyTensorWithConstraints classEnv constraints elemType otherType =
  case otherType of
    TVar v ->
      unifyVarConstraintAware classEnv constraints v (TTensor elemType)
    _ -> do
      (s, _) <-
        unifyNestedNormalized
          TensorConstraintAware classEnv constraints elemType otherType
      Right (s, True)

-- | Constraint-aware Tensor recursion for extension fallback.
unifyTensorWithConstraintsExtended
  :: ClassEnv
  -> [Constraint]
  -> Type
  -> Type
  -> Either UnifyError (Subst, Bool)
unifyTensorWithConstraintsExtended classEnv constraints elemType otherType =
  case otherType of
    TVar v ->
      unifyVarConstraintAware classEnv constraints v (TTensor elemType)
    _ -> do
      (s, _) <-
        unifyExtendedNormalized
          TensorConstraintAware classEnv constraints elemType otherType
      Right (s, True)

-- | Check if there's an instance for Constraint (Tensor elemType).
hasInstanceForTensorType :: ClassEnv -> Type -> Constraint -> Bool
hasInstanceForTensorType classEnv elemType (Constraint className _) =
  let tensorType = TTensor elemType
      instances = lookupInstances className classEnv
  in any (\inst -> case unifyStrict (instType inst) tensorType of
                     Right _ -> True
                     Left _  -> False
         ) instances

-- | Extension-mode variant whose synthesized product matcher is aligned with
-- recursively extensible targets.
unifyMatcherWithTupleExtendedG
  :: ClassEnv
  -> [Constraint]
  -> Capability
  -> Type
  -> [Type]
  -> Either UnifyError (Subst, Bool)
unifyMatcherWithTupleExtendedG classEnv constraints cap target ts = do
  (parts, s1, flag1) <-
    unifyEachAsMatcherExtended classEnv constraints ts emptySubst
  let tupleCap = CapTuple (map fst parts)
      tupleTarget = TTuple (map snd parts)
      constraints' = map (applySubstConstraint s1) constraints
  (s2, flag2) <-
    unifyExtendedNormalized TensorConstraintAware classEnv constraints'
      (applySubst s1 (TMatcher cap target))
      (applySubst s1 (TMatcher tupleCap tupleTarget))
  Right (composeSubst s2 s1, flag1 || flag2)

-- | Treat each element of a raw tuple as a matcher and extract its capability
-- and target, threading both substitutions.
unifyEachAsMatcherExtended
  :: ClassEnv
  -> [Constraint]
  -> [Type]
  -> Subst
  -> Either UnifyError ([(Capability, Type)], Subst, Bool)
unifyEachAsMatcherExtended _ _ [] s = Right ([], s, False)
unifyEachAsMatcherExtended env cons (t:rest) s = do
  let t' = applySubst s t
      cons' = map (applySubstConstraint s) cons
  (part, s1, flag1) <- case t' of
    TMatcher cap target -> Right ((cap, target), emptySubst, False)
    -- A MatcherSlot element (e.g. a slot-typed parameter used in a next-matcher
    -- tuple like @(m, list m)@) retains both of its components.
    TMatcherSlot cap target -> Right ((cap, target), emptySubst, False)
    TVar v -> do
      let innerVar = TyVar (getTyVarName v ++ "'")
          innerTy = TVar innerVar
          cap = CapVar (MkCapVar (getTyVarName v ++ "'cap"))
      (s', flag) <-
        unifyNormalized TensorConstraintAware env cons' t'
          (TMatcher cap innerTy)
      case applySubst s' (TMatcher cap innerTy) of
        TMatcher cap' target' -> Right ((cap', target'), s', flag)
        _ -> Left $ TypeMismatch (TMatcher cap innerTy) t'
    _ -> Left $ TypeMismatch (TMatcher CapAny TAny) t'

  let s2 = composeSubst s1 s
      cons'' = map (applySubstConstraint s2) cons
  (restParts, s3, flag2) <-
    unifyEachAsMatcherExtended env cons'' rest s2
  let (cap, target) = part
  Right ((applyCapSubst s3 cap, applySubst s3 target) : restParts,
         s3,
         flag1 || flag2)

-- | Core tuple coercion accepts only components whose complete matcher/slot
-- dual is already known.  Manufacturing fresh variables from a 'TyVar' by
-- string concatenation would bypass InferState's allocation/protection trace;
-- that historical behavior is confined to the warned extended solver above.
unifyEachKnownMatcher
  :: ClassEnv
  -> [Constraint]
  -> [Type]
  -> Subst
  -> Either UnifyError ([(Capability, Type)], Subst, Bool)
unifyEachKnownMatcher _ _ [] substitution =
  Right ([], substitution, False)
unifyEachKnownMatcher env constraints (ty : rest) substitution = do
  let resolved = applySubst substitution ty
      resolvedConstraints =
        map (applySubstConstraint substitution) constraints
  part <- case resolved of
    TMatcher capability target -> Right (capability, target)
    TMatcherSlot capability target -> Right (capability, target)
    _ -> Left (MatcherRigidity resolved (TMatcher CapAny TAny))
  (restParts, finalSubstitution, flag) <-
    unifyEachKnownMatcher
      env resolvedConstraints rest substitution
  let (capability, target) = part
  Right
    ( ( applyCapSubst finalSubstitution capability
      , applySubst finalSubstitution target
      ) : restParts
    , finalSubstitution
    , flag
    )

type TuplePartsUnifier =
  ClassEnv
  -> [Constraint]
  -> [Type]
  -> Subst
  -> Either UnifyError ([(Capability, Type)], Subst, Bool)

getTyVarName :: TyVar -> String
getTyVarName (TyVar name) = name

--------------------------------------------------------------------------------
-- COERCE-MATCHER-TO-SLOT (paper: one-way Matcher -> MatcherSlot coercion)
--------------------------------------------------------------------------------

type RecursiveUnifier =
  TensorHandling
  -> ClassEnv
  -> [Constraint]
  -> Type
  -> Type
  -> Either UnifyError (Subst, Bool)

-- | Coerce a matcher producer into a complete consumer slot.  Capability
-- matching is solved first in the capability sort; target equality is then
-- solved independently in the ordinary type sort.
coerceMatcherToSlot
  :: TensorHandling
  -> ClassEnv
  -> [Constraint]
  -> Capability
  -> Type
  -> Capability
  -> Type
  -> Either UnifyError (Subst, Bool)
coerceMatcherToSlot mode ce cs matcherCap matcherTarget slotCap slotTarget =
  coerceMatcherToSlotWithin
    (freeCapVarsCapability slotCap)
    mode ce cs matcherCap matcherTarget slotCap slotTarget

-- | Explicit extension fallback: keep one-way capability matching at the
-- outer slot, but recursively use extension alignment for the target.  The
-- complete consumer type owns the permitted capability-substitution domain;
-- capability variables already present in the producer remain rigid.
coerceMatcherToSlotExtended
  :: TensorHandling
  -> ClassEnv
  -> [Constraint]
  -> Capability
  -> Type
  -> Capability
  -> Type
  -> Either UnifyError (Subst, Bool)
coerceMatcherToSlotExtended
    mode ce cs matcherCap matcherTarget slotCap slotTarget =
  coerceMatcherToSlotWithinUsing
    unifyExtendedNormalized
    (freeCapVars (TMatcherSlot slotCap slotTarget)
      `Set.difference`
     freeCapVars (TMatcher matcherCap matcherTarget))
    mode ce cs matcherCap matcherTarget slotCap slotTarget

-- | Scalar matcher-to-slot coercion under the stable capability-variable
-- domain of the enclosing consumer.  For a scalar coercion this is just the
-- slot root's free variables; product coercion passes the whole raw product
-- domain to every component so all components share one witness.
coerceMatcherToSlotWithin
  :: Set.Set CapVar
  -> TensorHandling
  -> ClassEnv
  -> [Constraint]
  -> Capability
  -> Type
  -> Capability
  -> Type
  -> Either UnifyError (Subst, Bool)
coerceMatcherToSlotWithin =
  coerceMatcherToSlotWithinUsing unifyNestedNormalized

coerceMatcherToSlotWithinUsing
  :: RecursiveUnifier
  -> Set.Set CapVar
  -> TensorHandling
  -> ClassEnv
  -> [Constraint]
  -> Capability
  -> Type
  -> Capability
  -> Type
  -> Either UnifyError (Subst, Bool)
coerceMatcherToSlotWithinUsing targetUnifier allowedSupport
                             mode ce cs
                             matcherCap matcherTarget slotCap slotTarget = do
  capSubst <- matchCapability matcherCap slotCap
  let cs' = map (applySubstConstraint capSubst) cs
  (targetSubst, flag) <-
    targetUnifier mode ce cs'
      (applySubst capSubst matcherTarget)
      (applySubst capSubst slotTarget)
  let combined = composeSubst targetSubst capSubst
      producerStable =
        applyCapSubst combined matcherCap == matcherCap
      supportWithinConsumer =
        Map.keysSet (unCapSubst combined)
          `Set.isSubsetOf` allowedSupport
  if producerStable && supportWithinConsumer
    then Right (combined, flag)
    else Left
      (MatcherRigidity
        (TMatcher matcherCap matcherTarget)
        (TMatcherSlot slotCap slotTarget))

-- | COERCE-SLOT-TUPLE: a tuple of matchers @(m1, ..., mk)@ filling a product slot
-- @MatcherSlot tp tt@ (structural index @tp@ = τ_p, target index @tt@ = τ_t).
--
-- When the slot's structural and target indices are themselves @k@-tuples, decompose the
-- product slot into component slots and check each tuple-matcher component against its own
-- @MatcherSlot σ_i τ_i@ (the paper's COERCE-SLOT-TUPLE).  This *defers* a component that is a
-- matcher parameter (committing it to a component slot) rather than folding it into a bare
-- @Matcher@ — so e.g. @\\m -> matchAll (xs, n) as (m, integer) with ($x :: $xs, $n) -> ...@
-- commits @m@ to a list-headed component slot instead of rejecting it, while still rejecting
-- @something@ there.
--
-- Otherwise (a variable-headed slot, or a non-tuple target) fold the tuple of matchers into a
-- single product @Matcher@ and apply the standard COERCE-MATCHER-TO-SLOT dual check.  This is
-- what lets a matcher constructor whose element parameter is a slot (e.g.
-- @list (m : MatcherSlot a a)@) still accept a tuple matcher such as @(m, integer)@.
coerceSlotTuple :: TensorHandling -> ClassEnv -> [Constraint]
                -> Capability -> Type -> [Type]
                -> Either UnifyError (Subst, Bool)
coerceSlotTuple mode ce cs cap target tys =
  coerceSlotTupleWithin
    (freeCapVarsCapability cap)
    mode ce cs cap target tys

-- | Product-slot coercion for the warned Egison-extension fallback.  Recursive
-- extension alignment may solve capability variables in the consumer target,
-- so the stable support is the complete consumer type minus the raw producer.
coerceSlotTupleExtended
  :: TensorHandling
  -> ClassEnv
  -> [Constraint]
  -> Capability
  -> Type
  -> [Type]
  -> Either UnifyError (Subst, Bool)
coerceSlotTupleExtended mode ce cs cap target tys =
  coerceSlotTupleWithinUsing
    unifyExtendedNormalized
    unifyExtendedNormalized
    unifyEachAsMatcherExtended
    (freeCapVars (TMatcherSlot cap target)
      `Set.difference`
     freeCapVars (TTuple tys))
    mode ce cs cap target tys

-- | Product slot coercion using one capability witness and one stable support
-- set for the complete raw consumer.  The aggregate post-check is essential:
-- a substitution learned by a later component must not retroactively change
-- an earlier component's producer capability.
coerceSlotTupleWithin
  :: Set.Set CapVar
  -> TensorHandling
  -> ClassEnv
  -> [Constraint]
  -> Capability
  -> Type
  -> [Type]
  -> Either UnifyError (Subst, Bool)
coerceSlotTupleWithin =
  coerceSlotTupleWithinUsing
    unifyNestedNormalized unifyNormalized unifyEachKnownMatcher

coerceSlotTupleWithinUsing
  :: RecursiveUnifier
  -> RecursiveUnifier
  -> TuplePartsUnifier
  -> Set.Set CapVar
  -> TensorHandling
  -> ClassEnv
  -> [Constraint]
  -> Capability
  -> Type
  -> [Type]
  -> Either UnifyError (Subst, Bool)
coerceSlotTupleWithinUsing targetUnifier boundaryUnifier tuplePartsUnifier
                           allowedSupport mode ce cs cap target tys
  | CapTuple caps <- cap, TTuple targets <- target
  , length caps == length tys, length targets == length tys = do
      result@(finalSubst, _) <-
        goComponents (zip3 tys caps targets) emptySubst False
      validateAggregate finalSubst
      Right result
  | otherwise = do
      (parts, s1, flag1) <- tuplePartsUnifier ce cs tys emptySubst
      let matcherCap = CapTuple (map fst parts)
          matcherTarget = TTuple (map snd parts)
          cs' = map (applySubstConstraint s1) cs
      (s2, flag2) <-
        coerceMatcherToSlotWithinUsing targetUnifier allowedSupport mode ce cs'
                       (applyCapSubst s1 matcherCap)
                       (applySubst s1 matcherTarget)
                       (applyCapSubst s1 cap)
                       (applySubst s1 target)
      let finalSubst = composeSubst s2 s1
      validateAggregate finalSubst
      Right (finalSubst, flag1 || flag2)
  where
    goComponents [] acc anyFlag = Right (acc, anyFlag)
    goComponents ((ty, expectedCap, expectedTarget) : rest) acc anyFlag = do
      let cs' = map (applySubstConstraint acc) cs
          ty' = applySubst acc ty
          cap' = applyCapSubst acc expectedCap
          target' = applySubst acc expectedTarget
      (s', f') <- case ty' of
        TMatcher matcherCap matcherTarget ->
          coerceMatcherToSlotWithinUsing targetUnifier allowedSupport mode ce cs'
            matcherCap matcherTarget cap' target'
        TTuple nested ->
          coerceSlotTupleWithinUsing
            targetUnifier boundaryUnifier tuplePartsUnifier
            allowedSupport mode ce cs' cap' target' nested
        -- As at the scalar slot boundary, a raw Any component is not core
        -- evidence for a matcher head.  The extension-mode tuple solver keeps
        -- the historical gradual acceptance and is selected only after Infer
        -- reports an outside-core warning.
        TAny ->
          Left
            (MatcherRigidity
              TAny
              (TMatcherSlot cap' target'))
        _ ->
          boundaryUnifier mode ce cs' ty' (TMatcherSlot cap' target')
      goComponents rest (composeSubst s' acc) (anyFlag || f')

    rawPairs = collectRawPairs tys cap
    rawProducerRoots = concatMap collectRawProducers tys
    rawOuterPair =
      case mapM extractRawProducer tys of
        Just producers ->
          Just (CapTuple producers, cap)
        Nothing ->
          Nothing

    collectRawPairs :: [Type] -> Capability -> [(Capability, Capability)]
    collectRawPairs types (CapTuple capabilities)
      | length types == length capabilities =
          concat
            (zipWith collectRawPair types capabilities)
    collectRawPairs _ _ = []

    collectRawPair :: Type -> Capability -> [(Capability, Capability)]
    collectRawPair (TMatcher producer _) consumer =
      [(producer, consumer)]
    collectRawPair (TTuple components) consumer =
      collectRawPairs components consumer
    collectRawPair _ _ =
      []

    collectRawProducers :: Type -> [Capability]
    collectRawProducers (TMatcher producer _) =
      [producer]
    collectRawProducers (TTuple components) =
      concatMap collectRawProducers components
    collectRawProducers _ =
      []

    extractRawProducer :: Type -> Maybe Capability
    extractRawProducer (TMatcher producer _) =
      Just producer
    extractRawProducer (TTuple components) =
      CapTuple <$> mapM extractRawProducer components
    extractRawProducer _ =
      Nothing

    validateAggregate :: Subst -> Either UnifyError ()
    validateAggregate finalSubst =
      let supportOK =
            Map.keysSet (unCapSubst finalSubst)
              `Set.isSubsetOf` allowedSupport
          producersStable =
            all
              (\producer ->
                applyCapSubst finalSubst producer == producer)
              rawProducerRoots
          pairOK (producer, originalConsumer) =
            applyCapSubst finalSubst producer == producer
              && consumerSatisfied producer originalConsumer
          outerOK =
            maybe True pairOK rawOuterPair
      in if supportOK
            && producersStable
            && all pairOK rawPairs
            && outerOK
           then Right ()
           else Left
             (MatcherRigidity
               (TTuple tys)
               (TMatcherSlot cap target))
      where
        -- Recheck the complete product under the final shared bindings without
        -- erasing consumer provenance.  Literal Any nodes in the raw consumer
        -- remain wildcards.  A consumer variable whose saved image happens to
        -- be Any takes the CapVar branch and is therefore compared strictly.
        consumerSatisfied producer originalConsumer =
          case originalConsumer of
            CapAny ->
              True
            CapVar _ ->
              applyCapSubst finalSubst originalConsumer == producer
            CapSkolem _ ->
              originalConsumer == producer
            CapCon consumerFormer consumerChildren ->
              case producer of
                CapCon producerFormer producerChildren
                  | producerFormer == consumerFormer
                  , length producerChildren == length consumerChildren ->
                      and
                        (zipWith
                          consumerSatisfied
                          producerChildren
                          consumerChildren)
                _ ->
                  False
            CapTuple consumerComponents ->
              case producer of
                CapTuple producerComponents
                  | length producerComponents == length consumerComponents ->
                      and
                        (zipWith
                          consumerSatisfied
                          producerComponents
                          consumerComponents)
                _ ->
                  False

-- | One-way matching: is there a substitution over @slot@'s type variables making
-- @slot == matcher@, with @matcher@ rigid (its variables are never bound)?
-- A variable-headed @slot@ admits any matcher (bind the variable); a constructor- or
-- concrete-headed @slot@ rejects a bare-variable matcher (e.g. @something@). Repeated
-- slot variables are matched consistently (resolved via the accumulated substitution).
matchOneWay :: Type -> Type -> Maybe Subst
matchOneWay slot0 matcher0 =
  matchOneWayWithDomain
    (freeTyVars slot0 `Set.difference` freeTyVars matcher0)
    (freeCapVars slot0 `Set.difference` freeCapVars matcher0)
    slot0 matcher0

-- | One-way matching with an explicit, stable binding domain.  The domain is
-- captured from the original structural slot and must be preserved across a
-- recursively decomposed product coercion.  Variables introduced by the
-- matcher side therefore remain rigid even if an earlier equality substitutes
-- one of them into a later slot position.  Worklist entries retain the raw
-- consumer node plus a provenance bit; expanding a saved type-variable image
-- clears that bit so capability Any inside the image is checked strictly.
matchOneWayWithDomain
  :: Set.Set TyVar
  -> Set.Set CapVar
  -> Type
  -> Type
  -> Maybe Subst
matchOneWayWithDomain bindable bindableCapabilities slot0 matcher0 =
  go [(True, slot0, matcher0)] emptySubst
  where
    go [] acc
      | applySubst acc matcher0 == matcher0
      , Map.keysSet (unSubst acc) `Set.isSubsetOf` bindable
      , Map.keysSet (unCapSubst acc)
          `Set.isSubsetOf` bindableCapabilities =
          Just acc
      | otherwise =
          Nothing
    go ((fromOriginalConsumer, slot, matcher) : rest) acc =
      case slot of
        TVar variable
          | variable `Set.member` bindable ->
              case Map.lookup variable (unSubst acc) of
                Just _ ->
                  -- A repeated type variable reuses its saved image rigidly.
                  -- In particular, a nested capability Any inside that image
                  -- did not occur literally at this consumer position.
                  matchStruct
                    False
                    (applySubst acc (TVar variable))
                    matcher
                    rest
                    acc
                Nothing
                  | TVar variable == matcher ->
                      go rest acc
                  | variable `Set.member` freeTyVars matcher ->
                      Nothing
                  | otherwise ->
                      go rest
                        (composeSubst
                          (singletonSubst variable matcher)
                          acc)
          | otherwise ->
              matchStruct
                fromOriginalConsumer
                (TVar variable)
                matcher
                rest
                acc
        _ ->
          matchStruct fromOriginalConsumer slot matcher rest acc

    descend provenance pairs rest acc =
      go
        ([ (provenance, consumer, producer)
         | (consumer, producer) <- pairs
         ] ++ rest)
        acc

    matchStruct provenance (TCollection a) (TCollection b) rest acc =
      descend provenance [(a, b)] rest acc
    matchStruct provenance (TTuple as) (TTuple bs) rest acc
      | length as == length bs =
          descend provenance (zip as bs) rest acc
    matchStruct provenance (TInductive n as) (TInductive m bs) rest acc
      | n == m && length as == length bs =
          descend provenance (zip as bs) rest acc
    matchStruct provenance (TTensor a) (TTensor b) rest acc =
      descend provenance [(a, b)] rest acc
    matchStruct provenance (THash k1 v1) (THash k2 v2) rest acc =
      descend provenance [(k1, k2), (v1, v2)] rest acc
    matchStruct provenance (TFun a1 r1) (TFun a2 r2) rest acc =
      descend provenance [(a1, a2), (r1, r2)] rest acc
    matchStruct provenance
                (TMatcher consumerCap consumerTarget)
                (TMatcher producerCap producerTarget)
                rest acc =
      case matchCapabilityWithDomain
             provenance
             bindableCapabilities
             producerCap
             consumerCap
             acc of
        Right acc' ->
          descend provenance [(consumerTarget, producerTarget)] rest acc'
        Left _ ->
          Nothing
    matchStruct provenance
                (TMatcherSlot consumerCap consumerTarget)
                (TMatcherSlot producerCap producerTarget)
                rest acc =
      case matchCapabilityWithDomain
             provenance
             bindableCapabilities
             producerCap
             consumerCap
             acc of
        Right acc' ->
          descend provenance [(consumerTarget, producerTarget)] rest acc'
        Left _ ->
          Nothing
    matchStruct provenance (TIO a) (TIO b) rest acc =
      descend provenance [(a, b)] rest acc
    matchStruct provenance (TIORef a) (TIORef b) rest acc =
      descend provenance [(a, b)] rest acc
    matchStruct _ a b rest acc
      | a == b          = go rest acc   -- base types match exactly
      | groundEquiv a b = go rest acc   -- CAS ground equivalence (Integer ~ MathValue ~ Factor/Term/Frac/Poly)
      | otherwise       = Nothing

-- | CAS ground-type equivalence: the closed, ClassEnv-free subtype/widening
-- rules of 'unifyG' (Integer, MathValue, Factor, Term, Frac, Poly are mutually
-- equivalent at the ground level).  This lets 'matchOneWay' admit a concrete CAS
-- matcher at a concrete CAS slot — e.g. @integer : Matcher Integer@ filling the
-- @MatcherSlot MathValue MathValue@ that the body of @term@/@poly@/@frac@ pins.
groundEquiv :: Type -> Type -> Bool
groundEquiv a b = isCASGround a && isCASGround b
  where
    isCASGround TInt        = True
    isCASGround TMathValue  = True
    isCASGround TFactor     = True
    isCASGround (TTerm _ _) = True
    isCASGround (TFrac _)   = True
    isCASGround (TPoly _ _) = True
    isCASGround _           = False

--------------------------------------------------------------------------------
-- CAS Symbol Set Unification
--------------------------------------------------------------------------------

-- | Unify two symbol sets, returning the unified symbol set if compatible.
-- Rules:
--   - Open [..] unifies with anything, resulting in the more specific one
--   - Closed [x, y] unifies with Closed [x, y] if they're equal (or one is subset)
--   - SymbolSetVar can unify with concrete symbol sets
unifySymbolSets :: SymbolSet -> SymbolSet -> Maybe SymbolSet
unifySymbolSets SymbolSetOpen ss = Just ss
unifySymbolSets ss SymbolSetOpen = Just ss
unifySymbolSets (SymbolSetClosed s1) (SymbolSetClosed s2)
  | s1 == s2 = Just (SymbolSetClosed s1)
  -- Subset checking: unify to the larger set
  | all (`elem` s2) s1 = Just (SymbolSetClosed s2)  -- s1 ⊆ s2
  | all (`elem` s1) s2 = Just (SymbolSetClosed s1)  -- s2 ⊆ s1
  | otherwise = Nothing  -- No subset relationship
unifySymbolSets (SymbolSetVar v1) (SymbolSetVar v2)
  | v1 == v2 = Just (SymbolSetVar v1)
  | otherwise = Just (SymbolSetVar v1)  -- Arbitrary choice; needs substitution tracking
unifySymbolSets (SymbolSetVar _) ss = Just ss
unifySymbolSets ss (SymbolSetVar _) = Just ss
