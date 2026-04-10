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
  , unifyMany
  , UnifyError(..)
  ) where

import qualified Data.Set                    as Set

import           Language.Egison.Type.Subst  (Subst, applySubst, composeSubst,
                                              emptySubst, singletonSubst, applySubstConstraint)
import           Language.Egison.Type.Tensor (normalizeTensorType)
import           Language.Egison.Type.Types  (TyVar (..), Type (..), freeTyVars, normalizeInductiveTypes,
                                              Constraint(..), SymbolSet(..))
import           Language.Egison.Type.Env    (ClassEnv, lookupInstances, InstanceInfo(..), emptyClassEnv)

-- | Unification errors
data UnifyError
  = OccursCheck TyVar Type        -- ^ Infinite type detected
  | TypeMismatch Type Type        -- ^ Types cannot be unified
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

-- | Unify a list of type pairs.
unifyMany :: [Type] -> [Type] -> Either UnifyError Subst
unifyMany ts1 ts2 =
  fmap fst $ unifyManyG TensorConstraintAware emptyClassEnv [] ts1 ts2

--------------------------------------------------------------------------------
-- Normalization Entry Point
--------------------------------------------------------------------------------

-- | Trivial success: empty substitution, no Tensor unwrapping.
ok :: Either UnifyError (Subst, Bool)
ok = Right (emptySubst, False)

-- | Normalize types and delegate to core unification.
unifyNormalized :: TensorHandling -> ClassEnv -> [Constraint] -> Type -> Type -> Either UnifyError (Subst, Bool)
unifyNormalized mode classEnv constraints t1 t2 =
  let t1' = normalizeInductiveTypes (normalizeTensorType t1)
      t2' = normalizeInductiveTypes (normalizeTensorType t2)
  in unifyG mode classEnv constraints t1' t2'

--------------------------------------------------------------------------------
-- Generic Core Unification
--------------------------------------------------------------------------------

-- | Core unification function parametrized by TensorHandling mode.
-- All public unification variants delegate to this single function, eliminating
-- the previous code duplication across three nearly-identical implementations.
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

-- Special rule: TInt and TMathValue unify
unifyG _ _ _ TInt TMathValue = ok
unifyG _ _ _ TMathValue TInt = ok

-- Type variables: delegated to mode-specific handler
unifyG mode ce cs (TVar v) t = unifyVarG mode ce cs v t
unifyG mode ce cs t (TVar v) = unifyVarG mode ce cs v t

-- Tuples
unifyG mode ce cs (TTuple ts1) (TTuple ts2)
  | length ts1 == length ts2 = unifyManyG mode ce cs ts1 ts2
  | otherwise = Left $ TypeMismatch (TTuple ts1) (TTuple ts2)

-- Collections
unifyG mode ce cs (TCollection t1) (TCollection t2) =
  unifyNormalized mode ce cs t1 t2

-- Inductive types
unifyG mode ce cs (TInductive n1 ts1) (TInductive n2 ts2)
  | n1 == n2 && length ts1 == length ts2 = unifyManyG mode ce cs ts1 ts2
  | otherwise = Left $ TypeMismatch (TInductive n1 ts1) (TInductive n2 ts2)

-- Hash types (two components with substitution threading)
unifyG mode ce cs (THash k1 v1) (THash k2 v2) = do
  (s1, f1) <- unifyNormalized mode ce cs k1 k2
  let cs' = map (applySubstConstraint s1) cs
  (s2, f2) <- unifyNormalized mode ce cs' (applySubst s1 v1) (applySubst s1 v2)
  Right (composeSubst s2 s1, f1 || f2)

-- Matcher-Tuple special rule (ConstraintAware mode only)
unifyG TensorConstraintAware ce cs (TMatcher b) (TTuple ts) =
  unifyMatcherWithTupleG ce cs b ts
unifyG TensorConstraintAware ce cs (TTuple ts) (TMatcher b) =
  unifyMatcherWithTupleG ce cs b ts

-- Matcher types
unifyG mode ce cs (TMatcher t1) (TMatcher t2) =
  unifyNormalized mode ce cs t1 t2

-- Function types (two components with substitution threading)
unifyG mode ce cs (TFun a1 r1) (TFun a2 r2) = do
  (s1, f1) <- unifyNormalized mode ce cs a1 a2
  let cs' = map (applySubstConstraint s1) cs
  (s2, f2) <- unifyNormalized mode ce cs' (applySubst s1 r1) (applySubst s1 r2)
  Right (composeSubst s2 s1, f1 || f2)

-- IO types
unifyG mode ce cs (TIO t1) (TIO t2) =
  unifyNormalized mode ce cs t1 t2

-- IORef types
unifyG mode ce cs (TIORef t1) (TIORef t2) =
  unifyNormalized mode ce cs t1 t2

-- Port type
unifyG _ _ _ TPort TPort = ok

-- CAS types
unifyG _ _ _ TFactor TFactor = ok

unifyG mode ce cs (TFrac t1) (TFrac t2) =
  unifyNormalized mode ce cs t1 t2

unifyG mode ce cs (TPoly t1 ss1) (TPoly t2 ss2) = do
  -- First unify the coefficient types
  (s1, f1) <- unifyNormalized mode ce cs t1 t2
  -- Then unify the symbol sets
  case unifySymbolSets ss1 ss2 of
    Just _  -> Right (s1, f1)
    Nothing -> Left $ TypeMismatch (TPoly t1 ss1) (TPoly t2 ss2)

-- Tensor types: both Tensor — same for all modes
unifyG mode ce cs (TTensor t1) (TTensor t2) =
  unifyNormalized mode ce cs t1 t2

-- Tensor vs non-Tensor: TopLevel allows unwrapping
unifyG TensorTopLevel _ _ (TTensor t1) t2 = do
  (s, _) <- unifyNormalized TensorTopLevel emptyClassEnv [] t1 t2
  Right (s, True)
unifyG TensorTopLevel _ _ t1 (TTensor t2) = do
  (s, _) <- unifyNormalized TensorTopLevel emptyClassEnv [] t1 t2
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

-- | Unify multiple type pairs generically.
unifyManyG :: TensorHandling -> ClassEnv -> [Constraint] -> [Type] -> [Type] -> Either UnifyError (Subst, Bool)
unifyManyG _ _ _ [] [] = ok
unifyManyG mode ce cs (t1:ts1) (t2:ts2) = do
  (s1, f1) <- unifyNormalized mode ce cs t1 t2
  let cs' = map (applySubstConstraint s1) cs
  (s2, f2) <- unifyManyG mode ce cs' (map (applySubst s1) ts1) (map (applySubst s1) ts2)
  Right (composeSubst s2 s1, f1 || f2)
unifyManyG _ _ _ _ _ = Left $ TypeMismatch (TTuple []) (TTuple [])

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
        let varConstraints = filter (\(Constraint _ constraintType) -> constraintType == TVar v) constraints
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
        let varConstraints = filter (\(Constraint _ constraintType) -> constraintType == TVar v) constraints
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
      (s, _) <- unifyNormalized TensorConstraintAware classEnv constraints elemType otherType
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
-- Matcher-Tuple Unification (ConstraintAware mode only)
--------------------------------------------------------------------------------

-- | Unify Matcher b with (t1, t2, ...) by treating each ti as Matcher ci.
-- Result: b = (c1, c2, ...) where ti unifies with Matcher ci.
unifyMatcherWithTupleG :: ClassEnv -> [Constraint] -> Type -> [Type] -> Either UnifyError (Subst, Bool)
unifyMatcherWithTupleG classEnv constraints b ts = do
  (innerTypes, s1, flag1) <- unifyEachAsMatcher classEnv constraints ts emptySubst
  let tupleType = TTuple innerTypes
      constraints' = map (applySubstConstraint s1) constraints
  (s2, flag2) <- unifyNormalized TensorConstraintAware classEnv constraints' (applySubst s1 b) tupleType
  Right (composeSubst s2 s1, flag1 || flag2)
  where
    unifyEachAsMatcher :: ClassEnv -> [Constraint] -> [Type] -> Subst -> Either UnifyError ([Type], Subst, Bool)
    unifyEachAsMatcher _ _ [] s = Right ([], s, False)
    unifyEachAsMatcher env cons (t:rest) s = do
      let t' = applySubst s t
          cons' = map (applySubstConstraint s) cons
      (innerType, s1, flag1) <- case t' of
        TMatcher inner -> Right (inner, emptySubst, False)
        TVar v -> do
          let innerVar = TyVar (getTyVarName v ++ "'")
              innerTy = TVar innerVar
          (s', flag) <- unifyNormalized TensorConstraintAware env cons' t' (TMatcher innerTy)
          Right (applySubst s' innerTy, s', flag)
        _ -> Left $ TypeMismatch (TMatcher (TVar (TyVar "?"))) t'

      let s2 = composeSubst s1 s
          cons'' = map (applySubstConstraint s2) cons
      (restInnerTypes, s3, flag2) <- unifyEachAsMatcher env cons'' rest s2
      Right (applySubst s3 innerType : restInnerTypes, s3, flag1 || flag2)

    getTyVarName :: TyVar -> String
    getTyVarName (TyVar name) = name

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
