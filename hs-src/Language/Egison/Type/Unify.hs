{- |
Module      : Language.Egison.Type.Unify
Licence     : MIT

This module provides type unification for the Egison type system.
-}

module Language.Egison.Type.Unify
  ( unify
  , unifyMany
  , UnifyError(..)
  ) where

import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import           Language.Egison.Type.Index  (IndexSpec)
import           Language.Egison.Type.Subst  (Subst, applySubst, composeSubst,
                                              emptySubst, singletonSubst)
import           Language.Egison.Type.Tensor (normalizeTensorType)
import           Language.Egison.Type.Types  (TensorShape (..), TyVar (..),
                                              Type (..), freeTyVars)

-- | Unification errors
data UnifyError
  = OccursCheck TyVar Type        -- ^ Infinite type detected
  | TypeMismatch Type Type        -- ^ Types cannot be unified
  | ShapeMismatch TensorShape TensorShape  -- ^ Tensor shapes don't match
  | IndexMismatch IndexSpec IndexSpec      -- ^ Tensor indices don't match
  deriving (Eq, Show)

-- | Unify two types, returning a substitution if successful
unify :: Type -> Type -> Either UnifyError Subst
-- Normalize tensor types first
unify t1 t2 =
  unify' (normalizeTensorType t1) (normalizeTensorType t2)

unify' :: Type -> Type -> Either UnifyError Subst
-- Same types unify trivially
unify' TInt TInt = Right emptySubst
unify' TFloat TFloat = Right emptySubst
unify' TBool TBool = Right emptySubst
unify' TChar TChar = Right emptySubst
unify' TString TString = Right emptySubst
unify' TUnit TUnit = Right emptySubst

-- Type variables
unify' (TVar v) t = unifyVar v t
unify' t (TVar v) = unifyVar v t

-- TAny unifies with anything
unify' TAny _ = Right emptySubst
unify' _ TAny = Right emptySubst

-- Compound types
unify' (TList t1) (TList t2) = unify t1 t2

unify' (TTuple ts1) (TTuple ts2)
  | length ts1 == length ts2 = unifyMany ts1 ts2
  | otherwise = Left $ TypeMismatch (TTuple ts1) (TTuple ts2)

unify' (TFun a1 r1) (TFun a2 r2) = do
  s1 <- unify a1 a2
  s2 <- unify (applySubst s1 r1) (applySubst s1 r2)
  Right $ composeSubst s2 s1

unify' (TMatcher t1) (TMatcher t2) = unify t1 t2
unify' (TPattern t1) (TPattern t2) = unify t1 t2
unify' (TPatternFunc args1 ret1) (TPatternFunc args2 ret2)
  | length args1 == length args2 = do
    s1 <- unifyMany args1 args2
    s2 <- unify (applySubst s1 ret1) (applySubst s1 ret2)
    Right $ composeSubst s2 s1
  | otherwise = Left $ TypeMismatch (TPatternFunc args1 ret1) (TPatternFunc args2 ret2)
unify' (TCollection t1) (TCollection t2) = unify t1 t2
unify' (THash k1 v1) (THash k2 v2) = do
  s1 <- unify k1 k2
  s2 <- unify (applySubst s1 v1) (applySubst s1 v2)
  Right $ composeSubst s2 s1
unify' (TIORef t1) (TIORef t2) = unify t1 t2

-- Tensor types
unify' (TTensor t1 sh1 is1) (TTensor t2 sh2 is2) = do
  s1 <- unify t1 t2
  s2 <- unifyShape sh1 sh2
  s3 <- unifyIndices is1 is2
  Right $ composeSubst s3 (composeSubst s2 s1)
  where
    unifyShape :: TensorShape -> TensorShape -> Either UnifyError Subst
    unifyShape ShapeUnknown _ = Right emptySubst
    unifyShape _ ShapeUnknown = Right emptySubst
    unifyShape (ShapeLit d1) (ShapeLit d2)
      | d1 == d2 = Right emptySubst
      | otherwise = Left $ ShapeMismatch (ShapeLit d1) (ShapeLit d2)
    unifyShape (ShapeVar _) _ = Right emptySubst  -- Shape polymorphism
    unifyShape _ (ShapeVar _) = Right emptySubst

    unifyIndices :: IndexSpec -> IndexSpec -> Either UnifyError Subst
    -- For now, indices must match exactly or one is empty (placeholder)
    unifyIndices [] _ = Right emptySubst
    unifyIndices _ [] = Right emptySubst
    unifyIndices i1 i2
      | i1 == i2 = Right emptySubst
      | otherwise = Left $ IndexMismatch i1 i2

-- Mismatched types
unify' t1 t2 = Left $ TypeMismatch t1 t2

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

-- | Unify a list of type pairs
unifyMany :: [Type] -> [Type] -> Either UnifyError Subst
unifyMany [] [] = Right emptySubst
unifyMany (t1:ts1) (t2:ts2) = do
  s1 <- unify t1 t2
  s2 <- unifyMany (map (applySubst s1) ts1) (map (applySubst s1) ts2)
  Right $ composeSubst s2 s1
unifyMany _ _ = Left $ TypeMismatch TUnit TUnit  -- Length mismatch

