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

import qualified Data.Set                    as Set

import           Language.Egison.Type.Subst  (Subst, applySubst, composeSubst,
                                              emptySubst, singletonSubst)
import           Language.Egison.Type.Tensor (normalizeTensorType)
import           Language.Egison.Type.Types  (TyVar (..), Type (..), freeTyVars)

-- | Unification errors
data UnifyError
  = OccursCheck TyVar Type        -- ^ Infinite type detected
  | TypeMismatch Type Type        -- ^ Types cannot be unified
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
-- Unit type is represented as empty tuple TTuple []

-- Type variables
unify' (TVar v) t = unifyVar v t
unify' t (TVar v) = unifyVar v t

unify' (TTuple ts1) (TTuple ts2)
  | length ts1 == length ts2 = unifyMany ts1 ts2
  | otherwise = Left $ TypeMismatch (TTuple ts1) (TTuple ts2)

unify' (TCollection t1) (TCollection t2) = unify t1 t2

-- Inductive types
unify' (TInductive n1 ts1) (TInductive n2 ts2)
  | n1 == n2 && length ts1 == length ts2 = unifyMany ts1 ts2
  | otherwise = Left $ TypeMismatch (TInductive n1 ts1) (TInductive n2 ts2)

-- Tensor types
-- Tensor a and Tensor b unify if a and b unify
unify' (TTensor t1) (TTensor t2) = unify t1 t2

unify' (THash k1 v1) (THash k2 v2) = do
  s1 <- unify k1 k2
  s2 <- unify (applySubst s1 v1) (applySubst s1 v2)
  Right $ composeSubst s2 s1

unify' (TMatcher t1) (TMatcher t2) = unify t1 t2

unify' (TFun a1 r1) (TFun a2 r2) = do
  s1 <- unify a1 a2
  s2 <- unify (applySubst s1 r1) (applySubst s1 r2)
  Right $ composeSubst s2 s1

unify' (TIO t1) (TIO t2) = unify t1 t2

unify' (TIORef t1) (TIORef t2) = unify t1 t2

-- TAny unifies with anything
unify' TAny _ = Right emptySubst
unify' _ TAny = Right emptySubst

-- Tensor a and a unify as a (according to type-tensor-simple.md)
-- Tensor MathExpr unifies with MathExpr as MathExpr
unify' (TTensor t1) t2 = do
  s <- unify t1 t2
  -- Return substitution that unifies t1 with t2, result type is t2 (scalar)
  Right s

unify' t1 (TTensor t2) = do
  s <- unify t1 t2
  -- Return substitution that unifies t1 with t2, result type is t1 (scalar)
  Right s

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
unifyMany _ _ = Left $ TypeMismatch (TTuple []) (TTuple [])  -- Length mismatch

