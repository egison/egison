{- |
Module      : Language.Egison.Type.Instance
Licence     : MIT

This module provides utilities for matching type class instances.
-}

module Language.Egison.Type.Instance
  ( findMatchingInstanceForType
  , findMatchingInstanceForTypes
  ) where

import           Language.Egison.Type.Types (Type, InstanceInfo, instType, instTypes)
import           Language.Egison.Type.Unify (unifyStrict)
import           Language.Egison.Type.Subst (Subst, emptySubst, composeSubst, applySubst)

-- | Single-type lookup. Used by sites that intentionally check only the
-- principal class type (e.g. Tensor unwrapping in `Type/Infer.hs`).
-- Type-class dispatch through `Type/TypeClassExpand.hs` uses the multi-type
-- variant `findMatchingInstanceForTypes` instead.
-- IMPORTANT: uses `unifyStrict` so `Tensor a` does NOT unify with `a`.
findMatchingInstanceForType :: Type -> [InstanceInfo] -> Maybe InstanceInfo
findMatchingInstanceForType targetType = go
  where
    go [] = Nothing
    go (inst:rest) = case unifyStrict (instType inst) targetType of
      Right _ -> Just inst
      Left _  -> go rest

-- | Multi-parameter dispatch (e.g. `Coerce a b`). Each target type is
-- unified pairwise with the corresponding instance type, and substitutions
-- are composed across pairs so that repeated type variables in the instance
-- (e.g. `instance Coerce a a`) are required to unify with identical targets.
-- Single-param classes pass `[t]` and get the same result as
-- `findMatchingInstanceForType t`.
findMatchingInstanceForTypes :: [Type] -> [InstanceInfo] -> Maybe InstanceInfo
findMatchingInstanceForTypes targetTypes = go
  where
    go [] = Nothing
    go (inst:rest)
      | length (instTypes inst) /= length targetTypes = go rest
      | otherwise = case unifyAll emptySubst (zip (instTypes inst) targetTypes) of
          Right _ -> Just inst
          Left _  -> go rest

    -- Carry substitution forward so consistent type variables across pairs
    -- are enforced (e.g. `[a, a]` against `[Int, Float]` fails).
    unifyAll :: Subst -> [(Type, Type)] -> Either String Subst
    unifyAll s [] = Right s
    unifyAll s ((it, tt) : rs) =
      case unifyStrict (applySubst s it) (applySubst s tt) of
        Right s' -> unifyAll (s' `composeSubst` s) rs
        Left e   -> Left (show e)
