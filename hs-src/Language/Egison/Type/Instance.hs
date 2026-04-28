{- |
Module      : Language.Egison.Type.Instance
Licence     : MIT

This module provides utilities for matching type class instances.
-}

module Language.Egison.Type.Instance
  ( findMatchingInstanceForType
  , findMatchingInstanceForTypes
  ) where

import           Language.Egison.Type.Types (Type(..), TyVar(..), InstanceInfo(..), instType, instTypes, freeTyVars)
import           Language.Egison.Type.Unify (unifyStrict)

-- | Find a matching instance for a given target type
-- This searches through a list of instances and returns the first one that unifies with the target type
-- Used by both type inference (Infer.hs) and type class expansion (TypeClassExpand.hs)
-- IMPORTANT: Uses unifyStrict to ensure Tensor a does NOT unify with a
-- This prevents incorrectly matching scalar instances as tensor instances
findMatchingInstanceForType :: Type -> [InstanceInfo] -> Maybe InstanceInfo
findMatchingInstanceForType targetType instances = go instances
  where
    go [] = Nothing
    go (inst:rest) =
      -- Try to unify the instance type with the target type using strict unification
      case unifyStrict (instType inst) targetType of
        Right _ -> Just inst  -- Successfully unified
        Left _  -> go rest    -- Unification failed, try next instance

-- | Find a matching instance for multi-parameter type classes (e.g. `Coerce a b`).
-- Each target type is unified pairwise with the corresponding instance type.
-- Returns the first instance whose ALL types unify with the targets.
--
-- NOTE: For full Coerce-style dispatch, the call site needs to plumb through
-- BOTH the source type (`a`) and the result type (`b`). Currently most
-- dispatch sites only know the constraint's first type because `Constraint`
-- holds a single `Type`. Generalizing this is the remaining work for §2.1.
findMatchingInstanceForTypes :: [Type] -> [InstanceInfo] -> Maybe InstanceInfo
findMatchingInstanceForTypes targetTypes instances = go instances
  where
    go [] = Nothing
    go (inst:rest)
      | length (instTypes inst) /= length targetTypes = go rest
      | otherwise =
          case unifyAll (zip (instTypes inst) targetTypes) of
            Right () -> Just inst
            Left _   -> go rest

    -- | Try to unify each pair; succeeds only if all unify (no carry-forward
    -- substitution between pairs is performed here, matching the existing
    -- single-type behavior).
    unifyAll :: [(Type, Type)] -> Either String ()
    unifyAll [] = Right ()
    unifyAll ((it, tt) : rest) = case unifyStrict it tt of
      Right _ -> unifyAll rest
      Left e  -> Left (show e)
