{- |
Module      : Language.Egison.Type.Instance
Licence     : MIT

This module provides utilities for matching type class instances.
-}

module Language.Egison.Type.Instance
  ( findMatchingInstanceForType
  ) where

import           Language.Egison.Type.Types (Type(..), TyVar(..), InstanceInfo(..), freeTyVars)
import           Language.Egison.Type.Unify (unify)

-- | Find a matching instance for a given target type
-- This searches through a list of instances and returns the first one that unifies with the target type
-- Used by both type inference (IInfer.hs) and type class expansion (TypeClassExpand.hs)
findMatchingInstanceForType :: Type -> [InstanceInfo] -> Maybe InstanceInfo
findMatchingInstanceForType targetType instances = go instances
  where
    go [] = Nothing
    go (inst:rest) =
      -- Try to unify the instance type with the target type using proper unification
      case unify (instType inst) targetType of
        Right _ -> Just inst  -- Successfully unified
        Left _  -> go rest    -- Unification failed, try next instance
