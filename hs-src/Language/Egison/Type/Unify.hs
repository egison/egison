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
  , unifyCapability
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
                                              TypeFormer (..),
                                              TyVar (..), Type (..),
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
  | CapabilityMismatch Type Type  -- ^ Matcher capability equation has no solution
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


-- | Capability component matching used only inside type-class
-- 'matchOneWay'.  It keeps the quantified-instance domain stable and is not
-- the symmetric capability unification of type equality.  The Boolean
-- records whether literal 'CapAny' nodes come from the original consumer
-- shape.  It is false for a type obtained by expanding an earlier consumer
-- variable binding, because an @Any@ stored in that binding is a rigid value,
-- not a fresh wildcard occurrence.
matchCapabilityOneWayWithDomain
  :: Bool
  -> Set.Set CapVar
  -> Capability
  -> Capability
  -> Subst
  -> Either UnifyError Subst
matchCapabilityOneWayWithDomain literalAnyIsWildcard bindable producer0 consumer0 initialSubst
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
  CapabilityMismatch (TMatcher left TAny) (TMatcher right TAny)

--------------------------------------------------------------------------------
-- Normalization Entry Point
--------------------------------------------------------------------------------

-- | Trivial success: empty substitution, no Tensor unwrapping.
ok :: Either UnifyError (Subst, Bool)
ok = Right (emptySubst, False)

-- | Normalize types and align the two outermost types.  Capability variables
-- are solved symmetrically when both outer constructors are 'TMatcher';
-- capability equations at every nested occurrence use the same ordinary
-- capability MGU.
unifyNormalized :: TensorHandling -> ClassEnv -> [Constraint] -> Type -> Type -> Either UnifyError (Subst, Bool)
unifyNormalized mode classEnv constraints t1 t2 =
  let t1' = normalizeInductiveTypes (normalizeTensorType t1)
      t2' = normalizeInductiveTypes (normalizeTensorType t2)
  in alignRootG mode classEnv constraints t1' t2'

-- | Normalize a type component below the outer alignment boundary.  Generic
-- equality recurses through nested matcher types with the same rules.
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

-- | Root equality corresponding to TypePM's @alignTypesCore@.  It is
-- ordinary equality; the matcher/product head expansion lives in
-- 'unifyMatcherProductG'.
alignRootG
  :: TensorHandling
  -> ClassEnv
  -> [Constraint]
  -> Type
  -> Type
  -> Either UnifyError (Subst, Bool)
alignRootG = unifyG

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

-- Matcher types use ordinary equality recursively: both the capability and
-- target components contribute equations.
unifyG mode ce cs (TMatcher cap1 target1) (TMatcher cap2 target2) = do
  capSubst <- unifyCapability cap1 cap2
  let cs' = map (applySubstConstraint capSubst) cs
  (targetSubst, flag) <-
    unifyNestedNormalized mode ce cs'
      (applySubst capSubst target1)
      (applySubst capSubst target2)
  Right (composeSubst targetSubst capSubst, flag)
unifyG mode ce cs (TMatcher cap target) (TTuple components) =
  unifyMatcherProductG mode ce cs cap target components
unifyG mode ce cs (TTuple components) (TMatcher cap target) =
  unifyMatcherProductG mode ce cs cap target components

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

-- | Unify child lists below an ordinary type constructor, including ordinary
-- capability equations inside nested matcher types.
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

-- | The matcher/product head expansion of the canonical equality: a matcher
-- equal to a tuple of @n@ components has a tuple capability and a tuple
-- target of arity @n@, and its components are the component matchers.
-- Variable indices are decomposed into fresh components named after the
-- variable; the rigid capability @Any@ and constructor capabilities never
-- distribute.
unifyMatcherProductG
  :: TensorHandling
  -> ClassEnv
  -> [Constraint]
  -> Capability
  -> Type
  -> [Type]
  -> Either UnifyError (Subst, Bool)
unifyMatcherProductG mode ce cs cap target components = do
  let arity = length components
      mismatch = Left (CapabilityMismatch (TMatcher cap target) (TTuple components))
  (capSubst, caps) <-
    case cap of
      CapTuple caps
        | length caps == arity -> Right (emptySubst, caps)
        | otherwise -> mismatch
      CapVar (MkCapVar name) ->
        let caps = [ CapVar (MkCapVar (name ++ "." ++ show i)) | i <- [1 .. arity] ]
        in Right (singletonCapSubst (MkCapVar name) (CapTuple caps), caps)
      _ -> mismatch
  let target' = applySubst capSubst target
  (targetSubst, targets) <-
    case target' of
      TTuple targets
        | length targets == arity -> Right (emptySubst, targets)
        | otherwise -> mismatch
      TVar (TyVar name) ->
        let targets = [ TVar (TyVar (name ++ "." ++ show i)) | i <- [1 .. arity] ]
        in Right (singletonSubst (TyVar name) (TTuple targets), targets)
      _ -> mismatch
  let headSubst = composeSubst targetSubst capSubst
      cs' = map (applySubstConstraint headSubst) cs
      componentMatchers = zipWith TMatcher caps targets
  (componentSubst, flag) <-
    unifyManyG mode ce cs'
      (map (applySubst headSubst) componentMatchers)
      (map (applySubst headSubst) components)
  Right (composeSubst componentSubst headSubst, flag)

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
unifyVarSimple = occursCheckAndBind

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
                  fmap (\substitution -> (substitution, True)) $
                    occursCheckAndBind v elemType
      _ ->
        fmap (\s -> (s, False)) $ occursCheckAndBind v t

-- | Occurs check and variable binding (shared helper).
occursCheckAndBind :: TyVar -> Type -> Either UnifyError Subst
occursCheckAndBind variable ty
  | TVar variable == ty = Right emptySubst
  | variable `Set.member` freeTyVars ty = Left $ OccursCheck variable ty
  | otherwise = Right $ singletonSubst variable ty

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

-- | Check if there's an instance for Constraint (Tensor elemType).
hasInstanceForTensorType :: ClassEnv -> Type -> Constraint -> Bool
hasInstanceForTensorType classEnv elemType (Constraint className _) =
  let tensorType = TTensor elemType
      instances = lookupInstances className classEnv
  in any (\inst -> case unifyStrict (instType inst) tensorType of
                     Right _ -> True
                     Left _  -> False
         ) instances

--------------------------------------------------------------------------------
-- One-way matching for type-class instance selection
--------------------------------------------------------------------------------


-- | One-way matching: is there a substitution over the first type's variables
-- making it equal to the second, with the second rigid (its variables are
-- never bound)?  A variable-headed first type admits anything (bind the
-- variable); a constructor- or concrete-headed one rejects a bare variable.
-- Repeated variables are matched consistently (resolved via the accumulated
-- substitution).  Used for type-class instance selection only.
matchOneWay :: Type -> Type -> Maybe Subst
matchOneWay quantified0 rigid0 =
  matchOneWayWithDomain
    (freeTyVars quantified0 `Set.difference` freeTyVars rigid0)
    (freeCapVars quantified0 `Set.difference` freeCapVars rigid0)
    quantified0 rigid0

-- | One-way matching with an explicit, stable binding domain.  The domain is
-- captured from the original quantified type and must be preserved across a
-- recursively decomposed product.  Variables introduced by the rigid side
-- therefore remain rigid even if an earlier equality substitutes one of them
-- into a later position.  Worklist entries retain the raw quantified node
-- plus a provenance bit; expanding a saved type-variable image clears that
-- bit so capability Any inside the image is checked strictly.
matchOneWayWithDomain
  :: Set.Set TyVar
  -> Set.Set CapVar
  -> Type
  -> Type
  -> Maybe Subst
matchOneWayWithDomain bindable bindableCapabilities quantified0 rigid0 =
  go [(True, quantified0, rigid0)] emptySubst
  where
    go [] acc
      | applySubst acc rigid0 == rigid0
      , Map.keysSet (unSubst acc) `Set.isSubsetOf` bindable
      , Map.keysSet (unCapSubst acc)
          `Set.isSubsetOf` bindableCapabilities =
          Just acc
      | otherwise =
          Nothing
    go ((fromOriginalConsumer, quantified, rigid) : rest) acc =
      case quantified of
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
                    rigid
                    rest
                    acc
                Nothing
                  | TVar variable == rigid ->
                      go rest acc
                  | variable `Set.member` freeTyVars rigid ->
                      Nothing
                  | otherwise ->
                      go rest
                        (composeSubst
                          (singletonSubst variable rigid)
                          acc)
          | otherwise ->
              matchStruct
                fromOriginalConsumer
                (TVar variable)
                rigid
                rest
                acc
        _ ->
          matchStruct fromOriginalConsumer quantified rigid rest acc

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
      case matchCapabilityOneWayWithDomain
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
-- equivalent at the ground level).  This lets 'matchOneWay' identify the CAS
-- ground types, e.g. @Integer@ against @MathValue@ in the bodies of
-- @term@/@poly@/@frac@.
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
