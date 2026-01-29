{- |
Module      : Language.Egison.Type.Unify
Licence     : MIT

This module provides type unification for the Egison type system.
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
                                              Constraint(..))
import           Language.Egison.Type.Env    (ClassEnv, lookupInstances, InstanceInfo(..), emptyClassEnv)

-- | Unification errors
data UnifyError
  = OccursCheck TyVar Type        -- ^ Infinite type detected
  | TypeMismatch Type Type        -- ^ Types cannot be unified
  deriving (Eq, Show)

-- | Unify two types, returning a substitution if successful
-- This is a wrapper around unifyWithConstraints with empty constraints
unify :: Type -> Type -> Either UnifyError Subst
unify = unifyWithConstraints emptyClassEnv []


-- | Strict unification that does NOT allow Tensor a to unify with a
-- This is a wrapper around unifyStrictWithConstraints with empty constraints
-- This is used for checking type class instances in TensorMapInsertion
-- to ensure that Tensor types are properly distinguished from scalar types
unifyStrict :: Type -> Type -> Either UnifyError Subst
unifyStrict = unifyStrictWithConstraints emptyClassEnv []


-- | Strict unification with type class constraints
-- This is like unifyStrict but considers type class constraints when unifying type variables.
-- IMPORTANT: This does NOT allow Tensor a to unify with a (strict unification).
-- When unifying a constrained type variable with Tensor type, it checks if Tensor
-- has instances for all the constraints.
unifyStrictWithConstraints :: ClassEnv -> [Constraint] -> Type -> Type -> Either UnifyError Subst
unifyStrictWithConstraints classEnv constraints t1 t2 =
  let t1' = normalizeInductiveTypes (normalizeTensorType t1)
      t2' = normalizeInductiveTypes (normalizeTensorType t2)
  in unifyStrictWithConstraints' classEnv constraints t1' t2'

unifyStrictWithConstraints' :: ClassEnv -> [Constraint] -> Type -> Type -> Either UnifyError Subst
-- Same types unify trivially
unifyStrictWithConstraints' _ _ TInt TInt = Right emptySubst
unifyStrictWithConstraints' _ _ TMathExpr TMathExpr = Right emptySubst
unifyStrictWithConstraints' _ _ TPolyExpr TPolyExpr = Right emptySubst
unifyStrictWithConstraints' _ _ TTermExpr TTermExpr = Right emptySubst
unifyStrictWithConstraints' _ _ TSymbolExpr TSymbolExpr = Right emptySubst
unifyStrictWithConstraints' _ _ TIndexExpr TIndexExpr = Right emptySubst
unifyStrictWithConstraints' _ _ TFloat TFloat = Right emptySubst
unifyStrictWithConstraints' _ _ TBool TBool = Right emptySubst
unifyStrictWithConstraints' _ _ TChar TChar = Right emptySubst
unifyStrictWithConstraints' _ _ TString TString = Right emptySubst

-- Special rule: TInt and TMathExpr unify
unifyStrictWithConstraints' _ _ TInt TMathExpr = Right emptySubst
unifyStrictWithConstraints' _ _ TMathExpr TInt = Right emptySubst

-- Type variables - use constraint-aware strict unification
unifyStrictWithConstraints' classEnv constraints (TVar v) t =
  unifyVarStrictWithConstraints classEnv constraints v t
unifyStrictWithConstraints' classEnv constraints t (TVar v) =
  unifyVarStrictWithConstraints classEnv constraints v t

unifyStrictWithConstraints' classEnv constraints (TTuple ts1) (TTuple ts2)
  | length ts1 == length ts2 = unifyManyStrictWithConstraints classEnv constraints ts1 ts2
  | otherwise = Left $ TypeMismatch (TTuple ts1) (TTuple ts2)

unifyStrictWithConstraints' classEnv constraints (TCollection t1) (TCollection t2) =
  unifyStrictWithConstraints classEnv constraints t1 t2

-- Inductive types
unifyStrictWithConstraints' classEnv constraints (TInductive n1 ts1) (TInductive n2 ts2)
  | n1 == n2 && length ts1 == length ts2 = unifyManyStrictWithConstraints classEnv constraints ts1 ts2
  | otherwise = Left $ TypeMismatch (TInductive n1 ts1) (TInductive n2 ts2)

unifyStrictWithConstraints' classEnv constraints (THash k1 v1) (THash k2 v2) = do
  s1 <- unifyStrictWithConstraints classEnv constraints k1 k2
  let constraints' = map (applySubstConstraint s1) constraints
  s2 <- unifyStrictWithConstraints classEnv constraints' (applySubst s1 v1) (applySubst s1 v2)
  Right $ composeSubst s2 s1

unifyStrictWithConstraints' classEnv constraints (TMatcher t1) (TMatcher t2) =
  unifyStrictWithConstraints classEnv constraints t1 t2

unifyStrictWithConstraints' classEnv constraints (TFun a1 r1) (TFun a2 r2) = do
  s1 <- unifyStrictWithConstraints classEnv constraints a1 a2
  let constraints' = map (applySubstConstraint s1) constraints
  s2 <- unifyStrictWithConstraints classEnv constraints' (applySubst s1 r1) (applySubst s1 r2)
  Right $ composeSubst s2 s1

unifyStrictWithConstraints' classEnv constraints (TIO t1) (TIO t2) =
  unifyStrictWithConstraints classEnv constraints t1 t2

unifyStrictWithConstraints' classEnv constraints (TIORef t1) (TIORef t2) =
  unifyStrictWithConstraints classEnv constraints t1 t2

unifyStrictWithConstraints' _ _ TPort TPort = Right emptySubst

-- Tensor types - STRICT: Tensor a does NOT unify with a
unifyStrictWithConstraints' classEnv constraints (TTensor t1) (TTensor t2) =
  unifyStrictWithConstraints classEnv constraints t1 t2

-- TAny unifies with anything
unifyStrictWithConstraints' _ _ TAny _ = Right emptySubst
unifyStrictWithConstraints' _ _ _ TAny = Right emptySubst

-- Mismatched types
unifyStrictWithConstraints' _ _ t1 t2 = Left $ TypeMismatch t1 t2

-- | Unify a type variable with a type using strict unification with constraints
-- IMPORTANT: This is STRICT - Tensor a does NOT unify with a
unifyVarStrictWithConstraints :: ClassEnv -> [Constraint] -> TyVar -> Type -> Either UnifyError Subst
unifyVarStrictWithConstraints classEnv constraints v t
  | TVar v == t = Right emptySubst
  | otherwise = case t of
      -- Tensor type: check if the type variable's constraints allow Tensor
      TTensor elemType ->
        let varConstraints = filter (\(Constraint _ constraintType) -> constraintType == TVar v) constraints
        in if null varConstraints
           then
             -- No constraints: can bind to Tensor (with occurs check)
             if v `Set.member` freeTyVars t
             then Left $ OccursCheck v t
             else Right $ singletonSubst v t
           else
             -- Has constraints: check if Tensor has instances for ALL of them
             if all (hasInstanceForTensorType classEnv elemType) varConstraints
             then
               -- All constraints satisfied: can bind to Tensor
               if v `Set.member` freeTyVars t
               then Left $ OccursCheck v t
               else Right $ singletonSubst v t
             else
               -- Some constraint not satisfied by Tensor: cannot unify (strict)
               Left $ TypeMismatch (TVar v) t
      _ ->
        -- Non-Tensor type: regular occurs check and bind
        if v `Set.member` freeTyVars t
        then Left $ OccursCheck v t
        else Right $ singletonSubst v t

-- | Unify multiple type pairs with strict unification and constraints
unifyManyStrictWithConstraints :: ClassEnv -> [Constraint] -> [Type] -> [Type] -> Either UnifyError Subst
unifyManyStrictWithConstraints _ _ [] [] = Right emptySubst
unifyManyStrictWithConstraints classEnv constraints (t1:ts1) (t2:ts2) = do
  s1 <- unifyStrictWithConstraints classEnv constraints t1 t2
  let constraints' = map (applySubstConstraint s1) constraints
  s2 <- unifyManyStrictWithConstraints classEnv constraints' (map (applySubst s1) ts1) (map (applySubst s1) ts2)
  Right $ composeSubst s2 s1
unifyManyStrictWithConstraints _ _ _ _ = Left $ TypeMismatch (TTuple []) (TTuple [])

-- | Unify a type variable with a type
unifyVar :: TyVar -> Type -> Either UnifyError Subst
unifyVar v t
  | TVar v == t = Right emptySubst
  | occursIn v t = Left $ OccursCheck v t
  | otherwise = Right $ singletonSubst v t

-- | Occurs check: ensure a type variable doesn't occur in a type
-- This prevents infinite types like a = [a]
occursIn :: TyVar -> Type -> Bool
occursIn v t = v `Set.member` freeTyVars t

-- | Unify Matcher b with (t1, t2, ...) by treating each ti as Matcher ci
-- Result: b = (c1, c2, ...) where ti unifies with Matcher ci
unifyMatcherWithTuple :: Type -> [Type] -> Either UnifyError Subst
unifyMatcherWithTuple b ts = do
  -- Process each element: extract inner type or create constraint
  (innerTypes, s1) <- unifyEachAsMatcher ts emptySubst
  -- Now unify b with (c1, c2, ...)
  let tupleType = TTuple innerTypes
  s2 <- unify (applySubst s1 b) tupleType
  Right $ composeSubst s2 s1
  where
    -- Unify each type in the tuple with Matcher ci, extracting ci
    unifyEachAsMatcher :: [Type] -> Subst -> Either UnifyError ([Type], Subst)
    unifyEachAsMatcher [] s = Right ([], s)
    unifyEachAsMatcher (t:rest) s = do
      let t' = applySubst s t
      (innerType, s1) <- case t' of
        -- If already Matcher c, extract c
        TMatcher inner -> Right (inner, emptySubst)
        -- If type variable, unify it with Matcher (fresh variable)
        TVar v -> do
          -- Generate a new variable name for the inner type
          let innerVar = TyVar (getTyVarName v ++ "'")
              innerType = TVar innerVar
          s' <- unify t' (TMatcher innerType)
          Right (applySubst s' innerType, s')
        -- Other types cannot be unified with Matcher
        _ -> Left $ TypeMismatch (TMatcher (TVar (TyVar "?"))) t'
      
      let s2 = composeSubst s1 s
      (restInnerTypes, s3) <- unifyEachAsMatcher rest s2
      Right (applySubst s3 innerType : restInnerTypes, s3)
    
    getTyVarName :: TyVar -> String
    getTyVarName (TyVar name) = name

-- | Unify two types, allowing Tensor a to unify with a at top-level definitions
-- This is used only for top-level definitions with type annotations
-- According to type-tensor-simple.md: "トップレベル定義のテンソルについてのみ、Tensor a型が a型とunifyするとa型になる。"
unifyWithTopLevel :: Type -> Type -> Either UnifyError Subst
unifyWithTopLevel t1 t2 =
  let t1' = normalizeInductiveTypes (normalizeTensorType t1)
      t2' = normalizeInductiveTypes (normalizeTensorType t2)
  in unifyWithTopLevel' t1' t2'

unifyWithTopLevel' :: Type -> Type -> Either UnifyError Subst
-- Same types unify trivially
unifyWithTopLevel' TInt TInt = Right emptySubst
unifyWithTopLevel' TMathExpr TMathExpr = Right emptySubst
unifyWithTopLevel' TPolyExpr TPolyExpr = Right emptySubst
unifyWithTopLevel' TTermExpr TTermExpr = Right emptySubst
unifyWithTopLevel' TSymbolExpr TSymbolExpr = Right emptySubst
unifyWithTopLevel' TIndexExpr TIndexExpr = Right emptySubst
unifyWithTopLevel' TFloat TFloat = Right emptySubst
unifyWithTopLevel' TBool TBool = Right emptySubst
unifyWithTopLevel' TChar TChar = Right emptySubst
unifyWithTopLevel' TString TString = Right emptySubst

-- Special rule: TInt and TMathExpr unify to TMathExpr
unifyWithTopLevel' TInt TMathExpr = Right emptySubst
unifyWithTopLevel' TMathExpr TInt = Right emptySubst

-- Type variables
unifyWithTopLevel' (TVar v) t = unifyVar v t
unifyWithTopLevel' t (TVar v) = unifyVar v t

unifyWithTopLevel' (TTuple ts1) (TTuple ts2)
  | length ts1 == length ts2 = unifyManyWithTopLevel ts1 ts2
  | otherwise = Left $ TypeMismatch (TTuple ts1) (TTuple ts2)

unifyWithTopLevel' (TCollection t1) (TCollection t2) = unifyWithTopLevel t1 t2

-- Inductive types
unifyWithTopLevel' (TInductive n1 ts1) (TInductive n2 ts2)
  | n1 == n2 && length ts1 == length ts2 = unifyManyWithTopLevel ts1 ts2
  | otherwise = Left $ TypeMismatch (TInductive n1 ts1) (TInductive n2 ts2)

unifyWithTopLevel' (THash k1 v1) (THash k2 v2) = do
  s1 <- unifyWithTopLevel k1 k2
  s2 <- unifyWithTopLevel (applySubst s1 v1) (applySubst s1 v2)
  Right $ composeSubst s2 s1

unifyWithTopLevel' (TMatcher t1) (TMatcher t2) = unifyWithTopLevel t1 t2

unifyWithTopLevel' (TFun a1 r1) (TFun a2 r2) = do
  s1 <- unifyWithTopLevel a1 a2
  s2 <- unifyWithTopLevel (applySubst s1 r1) (applySubst s1 r2)
  Right $ composeSubst s2 s1

unifyWithTopLevel' (TIO t1) (TIO t2) = unifyWithTopLevel t1 t2

unifyWithTopLevel' (TIORef t1) (TIORef t2) = unifyWithTopLevel t1 t2

unifyWithTopLevel' TPort TPort = Right emptySubst

-- TAny unifies with anything
unifyWithTopLevel' TAny _ = Right emptySubst
unifyWithTopLevel' _ TAny = Right emptySubst

-- Tensor types
-- Tensor a and Tensor b unify if a and b unify
unifyWithTopLevel' (TTensor t1) (TTensor t2) = unifyWithTopLevel t1 t2
-- Tensor a and a can unify as a (only at top-level definitions)
-- Tensor MathExpr can unifies with MathExpr as MathExpr
unifyWithTopLevel' (TTensor t1) t2 = do
  s <- unifyWithTopLevel t1 t2
  -- Return substitution that unifies t1 with t2, result type is t2 (scalar)
  Right s

unifyWithTopLevel' t1 (TTensor t2) = do
  s <- unifyWithTopLevel t1 t2
  -- Return substitution that unifies t1 with t2, result type is t1 (scalar)
  Right s

-- Mismatched types
unifyWithTopLevel' t1 t2 = Left $ TypeMismatch t1 t2

-- | Unify a list of type pairs with top-level tensor unification
unifyManyWithTopLevel :: [Type] -> [Type] -> Either UnifyError Subst
unifyManyWithTopLevel [] [] = Right emptySubst
unifyManyWithTopLevel (t1:ts1) (t2:ts2) = do
  s1 <- unifyWithTopLevel t1 t2
  s2 <- unifyManyWithTopLevel (map (applySubst s1) ts1) (map (applySubst s1) ts2)
  Right $ composeSubst s2 s1
unifyManyWithTopLevel _ _ = Left $ TypeMismatch (TTuple []) (TTuple [])  -- Length mismatch

-- | Unify a list of type pairs
unifyMany :: [Type] -> [Type] -> Either UnifyError Subst
unifyMany [] [] = Right emptySubst
unifyMany (t1:ts1) (t2:ts2) = do
  s1 <- unify t1 t2
  s2 <- unifyMany (map (applySubst s1) ts1) (map (applySubst s1) ts2)
  Right $ composeSubst s2 s1
unifyMany _ _ = Left $ TypeMismatch (TTuple []) (TTuple [])  -- Length mismatch

--------------------------------------------------------------------------------
-- Constraint-Aware Unification
--------------------------------------------------------------------------------

-- | Unify two types while considering type class constraints
-- This function chooses unifiers that satisfy type class constraints
-- Specifically, when unifying Tensor a with a constrained type variable t:
--   - If C t constraint exists and C (Tensor a) is not satisfiable,
--     prefer t = a over t = Tensor a
unifyWithConstraints :: ClassEnv -> [Constraint] -> Type -> Type -> Either UnifyError Subst
unifyWithConstraints classEnv constraints t1 t2 =
  let t1' = normalizeInductiveTypes (normalizeTensorType t1)
      t2' = normalizeInductiveTypes (normalizeTensorType t2)
  in unifyWithConstraints' classEnv constraints t1' t2'

unifyWithConstraints' :: ClassEnv -> [Constraint] -> Type -> Type -> Either UnifyError Subst
-- Same types unify trivially
unifyWithConstraints' _ _ TInt TInt = Right emptySubst
unifyWithConstraints' _ _ TMathExpr TMathExpr = Right emptySubst
unifyWithConstraints' _ _ TPolyExpr TPolyExpr = Right emptySubst
unifyWithConstraints' _ _ TTermExpr TTermExpr = Right emptySubst
unifyWithConstraints' _ _ TSymbolExpr TSymbolExpr = Right emptySubst
unifyWithConstraints' _ _ TIndexExpr TIndexExpr = Right emptySubst
unifyWithConstraints' _ _ TFloat TFloat = Right emptySubst
unifyWithConstraints' _ _ TBool TBool = Right emptySubst
unifyWithConstraints' _ _ TChar TChar = Right emptySubst
unifyWithConstraints' _ _ TString TString = Right emptySubst

-- Special rule: TInt and TMathExpr unify to TMathExpr
unifyWithConstraints' _ _ TInt TMathExpr = Right emptySubst
unifyWithConstraints' _ _ TMathExpr TInt = Right emptySubst

-- Type variables - with constraint-aware Tensor handling
unifyWithConstraints' classEnv constraints (TVar v) t =
  unifyVarWithConstraints classEnv constraints v t
unifyWithConstraints' classEnv constraints t (TVar v) =
  unifyVarWithConstraints classEnv constraints v t

unifyWithConstraints' classEnv constraints (TTuple ts1) (TTuple ts2)
  | length ts1 == length ts2 = unifyManyWithConstraints classEnv constraints ts1 ts2
  | otherwise = Left $ TypeMismatch (TTuple ts1) (TTuple ts2)

unifyWithConstraints' classEnv constraints (TCollection t1) (TCollection t2) =
  unifyWithConstraints classEnv constraints t1 t2

-- Inductive types
unifyWithConstraints' classEnv constraints (TInductive n1 ts1) (TInductive n2 ts2)
  | n1 == n2 && length ts1 == length ts2 = unifyManyWithConstraints classEnv constraints ts1 ts2
  | otherwise = Left $ TypeMismatch (TInductive n1 ts1) (TInductive n2 ts2)

unifyWithConstraints' classEnv constraints (THash k1 v1) (THash k2 v2) = do
  s1 <- unifyWithConstraints classEnv constraints k1 k2
  s2 <- unifyWithConstraints classEnv (map (applySubstConstraint s1) constraints) (applySubst s1 v1) (applySubst s1 v2)
  Right $ composeSubst s2 s1

-- Special rule: Matcher b unifies with (t1, t2, ...) 
-- by treating each ti as Matcher ci, resulting in b = (c1, c2, ...)
unifyWithConstraints' classEnv constraints (TMatcher b) (TTuple ts) =
  unifyMatcherWithTupleWithConstraints classEnv constraints b ts
unifyWithConstraints' classEnv constraints (TTuple ts) (TMatcher b) =
  unifyMatcherWithTupleWithConstraints classEnv constraints b ts

unifyWithConstraints' classEnv constraints (TMatcher t1) (TMatcher t2) =
  unifyWithConstraints classEnv constraints t1 t2

unifyWithConstraints' classEnv constraints (TFun a1 r1) (TFun a2 r2) = do
  s1 <- unifyWithConstraints classEnv constraints a1 a2
  s2 <- unifyWithConstraints classEnv (map (applySubstConstraint s1) constraints) (applySubst s1 r1) (applySubst s1 r2)
  Right $ composeSubst s2 s1

unifyWithConstraints' classEnv constraints (TIO t1) (TIO t2) =
  unifyWithConstraints classEnv constraints t1 t2

unifyWithConstraints' classEnv constraints (TIORef t1) (TIORef t2) =
  unifyWithConstraints classEnv constraints t1 t2

unifyWithConstraints' _ _ TPort TPort = Right emptySubst

-- Tensor types - both Tensor
unifyWithConstraints' classEnv constraints (TTensor t1) (TTensor t2) =
  unifyWithConstraints classEnv constraints t1 t2

-- IMPORTANT: Constraint-aware handling for Tensor <-> non-Tensor
-- When unifying Tensor a with non-Tensor, prefer non-Tensor if it satisfies constraints
unifyWithConstraints' classEnv constraints (TTensor t1) t2 =
  unifyTensorWithConstraints classEnv constraints t1 t2
unifyWithConstraints' classEnv constraints t1 (TTensor t2) =
  unifyTensorWithConstraints classEnv constraints t2 t1

-- TAny unifies with anything
unifyWithConstraints' _ _ TAny _ = Right emptySubst
unifyWithConstraints' _ _ _ TAny = Right emptySubst

-- Mismatched types
unifyWithConstraints' _ _ t1 t2 = Left $ TypeMismatch t1 t2

-- | Unify type variable with another type, considering constraints
-- Note: occurs check is deferred to handle cases like unifying t0 with Tensor t0
-- when t0 has constraints (e.g., {Num t0}) and there's no Num (Tensor t0) instance.
-- In such cases, we bind t0 to the element type (t0 itself), which is identity.
unifyVarWithConstraints :: ClassEnv -> [Constraint] -> TyVar -> Type -> Either UnifyError Subst
unifyVarWithConstraints classEnv constraints v t
  | TVar v == t = Right emptySubst
  | otherwise = case t of
      -- Special handling for Tensor types with constraints
      TTensor elemType ->
        -- Check if the type variable has constraints
        let varConstraints = filter (\(Constraint _ constraintType) -> constraintType == TVar v) constraints
        in if null varConstraints
           then
             -- No constraints on this variable, bind to Tensor (need occurs check)
             if v `Set.member` freeTyVars t
             then Left $ OccursCheck v t
             else Right $ singletonSubst v t
           else
             -- Has constraints: check if Tensor has instances for all of them
             if all (hasInstanceForTensorType classEnv elemType) varConstraints
             then
               -- All constraints have Tensor instances, bind to Tensor (need occurs check)
               if v `Set.member` freeTyVars t
               then Left $ OccursCheck v t
               else Right $ singletonSubst v t
             else
               -- Some constraint lacks Tensor instance, bind to element type instead
               -- This allows tensorMap to handle the Tensor -> scalar conversion
               -- Special case: if v == elemType (e.g., t0 with Tensor t0), return identity
               if TVar v == elemType
               then Right emptySubst
               else if v `Set.member` freeTyVars elemType
                    then Left $ OccursCheck v elemType
                    else Right $ singletonSubst v elemType
      _ ->
        -- Non-Tensor type, regular occurs check
        if v `Set.member` freeTyVars t
        then Left $ OccursCheck v t
        else Right $ singletonSubst v t

-- | Check if there's an instance for Constraint (Tensor elemType)
-- e.g., check if Num (Tensor Integer) exists given elemType = Integer and constraint = Num
hasInstanceForTensorType :: ClassEnv -> Type -> Constraint -> Bool
hasInstanceForTensorType classEnv elemType (Constraint className _) =
  let tensorType = TTensor elemType
      instances = lookupInstances className classEnv
  in any (\inst -> case unifyStrict (instType inst) tensorType of
                     Right _ -> True
                     Left _  -> False
         ) instances

-- | Unify Tensor elemType with a non-Tensor type, considering constraints
unifyTensorWithConstraints :: ClassEnv -> [Constraint] -> Type -> Type -> Either UnifyError Subst
unifyTensorWithConstraints classEnv constraints elemType otherType =
  case otherType of
    TVar v ->
      -- Symmetric case: handled by unifyVarWithConstraints
      unifyVarWithConstraints classEnv constraints v (TTensor elemType)
    _ ->
      -- Normal unification: Tensor elemType with otherType means elemType = otherType
      unifyWithConstraints classEnv constraints elemType otherType

-- | Unify multiple type pairs with constraints
unifyManyWithConstraints :: ClassEnv -> [Constraint] -> [Type] -> [Type] -> Either UnifyError Subst
unifyManyWithConstraints _ _ [] [] = Right emptySubst
unifyManyWithConstraints classEnv constraints (t1:ts1) (t2:ts2) = do
  s1 <- unifyWithConstraints classEnv constraints t1 t2
  let constraints' = map (applySubstConstraint s1) constraints
  s2 <- unifyManyWithConstraints classEnv constraints' (map (applySubst s1) ts1) (map (applySubst s1) ts2)
  Right $ composeSubst s2 s1
unifyManyWithConstraints _ _ _ _ = Left $ TypeMismatch (TTuple []) (TTuple [])

-- | Unify Matcher b with (t1, t2, ...) using constraint-aware unification
-- Result: b = (c1, c2, ...) where ti unifies with Matcher ci
unifyMatcherWithTupleWithConstraints :: ClassEnv -> [Constraint] -> Type -> [Type] -> Either UnifyError Subst
unifyMatcherWithTupleWithConstraints classEnv constraints b ts = do
  -- Process each element: extract inner type or create constraint
  (innerTypes, s1) <- unifyEachAsMatcherWithConstraints classEnv constraints ts emptySubst
  -- Now unify b with (c1, c2, ...)
  let tupleType = TTuple innerTypes
      constraints' = map (applySubstConstraint s1) constraints
  s2 <- unifyWithConstraints classEnv constraints' (applySubst s1 b) tupleType
  Right $ composeSubst s2 s1
  where
    -- Unify each type in the tuple with Matcher ci, extracting ci
    unifyEachAsMatcherWithConstraints :: ClassEnv -> [Constraint] -> [Type] -> Subst -> Either UnifyError ([Type], Subst)
    unifyEachAsMatcherWithConstraints _ _ [] s = Right ([], s)
    unifyEachAsMatcherWithConstraints env cons (t:rest) s = do
      let t' = applySubst s t
          cons' = map (applySubstConstraint s) cons
      (innerType, s1) <- case t' of
        -- If already Matcher c, extract c
        TMatcher inner -> Right (inner, emptySubst)
        -- If type variable, unify it with Matcher (fresh variable)
        TVar v -> do
          -- Generate a new variable name for the inner type
          let innerVar = TyVar (getTyVarName v ++ "'")
              innerType = TVar innerVar
          s' <- unifyWithConstraints env cons' t' (TMatcher innerType)
          Right (applySubst s' innerType, s')
        -- Other types cannot be unified with Matcher
        _ -> Left $ TypeMismatch (TMatcher (TVar (TyVar "?"))) t'
      
      let s2 = composeSubst s1 s
          cons'' = map (applySubstConstraint s2) cons
      (restInnerTypes, s3) <- unifyEachAsMatcherWithConstraints env cons'' rest s2
      Right (applySubst s3 innerType : restInnerTypes, s3)
    
    getTyVarName :: TyVar -> String
    getTyVarName (TyVar name) = name

