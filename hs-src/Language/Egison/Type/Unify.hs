{- |
Module      : Language.Egison.Type.Unify
Licence     : MIT

This module provides type unification for the Egison type system.
-}

module Language.Egison.Type.Unify
  ( unify
  , unifyWithTopLevel
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

unify' (THash k1 v1) (THash k2 v2) = do
  s1 <- unify k1 k2
  s2 <- unify (applySubst s1 v1) (applySubst s1 v2)
  Right $ composeSubst s2 s1

-- Special rule: Matcher b unifies with (t1, t2, ...) 
-- by treating each ti as Matcher ci, resulting in b = (c1, c2, ...)
unify' (TMatcher b) (TTuple ts) = unifyMatcherWithTuple b ts
unify' (TTuple ts) (TMatcher b) = unifyMatcherWithTuple b ts

unify' (TMatcher t1) (TMatcher t2) = unify t1 t2

unify' (TFun a1 r1) (TFun a2 r2) = do
  s1 <- unify a1 a2
  s2 <- unify (applySubst s1 r1) (applySubst s1 r2)
  Right $ composeSubst s2 s1

unify' (TIO t1) (TIO t2) = unify t1 t2

unify' (TIORef t1) (TIORef t2) = unify t1 t2

-- Tensor types
-- Tensor a and Tensor b unify if a and b unify
unify' (TTensor t1) (TTensor t2) = unify t1 t2
unify' (TTensor t1) t2 = unify t1 t2
unify' t1 (TTensor t2) = unify t1 t2

-- TAny unifies with anything
unify' TAny _ = Right emptySubst
unify' _ TAny = Right emptySubst

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
  unifyWithTopLevel' (normalizeTensorType t1) (normalizeTensorType t2)

unifyWithTopLevel' :: Type -> Type -> Either UnifyError Subst
-- Same types unify trivially
unifyWithTopLevel' TInt TInt = Right emptySubst
unifyWithTopLevel' TFloat TFloat = Right emptySubst
unifyWithTopLevel' TBool TBool = Right emptySubst
unifyWithTopLevel' TChar TChar = Right emptySubst
unifyWithTopLevel' TString TString = Right emptySubst

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

