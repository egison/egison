{- |
Module      : Language.Egison.Type.Instance
Licence     : MIT

This module provides utilities for matching type class instances.
-}

module Language.Egison.Type.Instance
  ( findMatchingInstanceForType
  , findMatchingInstanceForTypes
  , findMostSpecificInstance
  , findMostSpecificInstanceForTypes
  , AmbiguityError(..)
  ) where

import qualified Data.Set                   as Set
import           Language.Egison.Type.Types (Type, InstanceInfo, instType, instTypes,
                                              freeTyVars)
import           Language.Egison.Type.Unify (unifyStrict)
import           Language.Egison.Type.Subst (Subst, emptySubst, composeSubst, applySubst)
import           Language.Egison.Type.Join  (isSubtype)

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

-- | Errors raised by `findMostSpecificInstance`.
-- See design/runtime-type-dispatch.md §4 for the full rationale.
data AmbiguityError
  = NoMatchingInstance
    -- ^ No candidate instance accepts the target type.
  | AmbiguousIncomparable
    -- ^ Multiple candidates exist and at least two are not comparable
    -- via the subtype partial order, so no unique most-specific exists.
  | AmbiguousMultipleMaxima
    -- ^ Multiple incomparable minima (most-specific) candidates exist.
    -- Should be unreachable when instances are pairwise distinct;
    -- kept defensively.
  deriving (Eq, Show)

-- | Pick the most specific instance accepting `target`.
--
-- An instance is a candidate iff the target type is a subtype of the
-- instance's principal type (or unifies with it for type-variable instances).
-- Among candidates, the most specific is the one whose principal type is a
-- subtype of every other candidate's principal type (= the minimum element
-- in the subtype partial order). If no unique minimum exists, returns an
-- ambiguity error.
findMostSpecificInstance :: Type -> [InstanceInfo] -> Either AmbiguityError InstanceInfo
findMostSpecificInstance target insts =
  case filter (\inst -> isCompatible target (instType inst)) insts of
    []  -> Left NoMatchingInstance
    [x] -> Right x
    cs  ->
      let isMostSpecific x =
            all (\y -> instType y == instType x
                       || isSubtype (instType x) (instType y))
                cs
          mostSpecific = filter isMostSpecific cs
      in case mostSpecific of
           [unique] -> Right unique
           []       -> Left AmbiguousIncomparable
           _        -> Left AmbiguousMultipleMaxima

-- | A target is compatible with an instance type. The check is mode-aware:
-- if the instance type has free type variables (e.g. `Eq a`, `Coerce a [a]`),
-- we fall back to strict unification so type-variable instances can match.
-- Otherwise the instance is fully concrete and we require the target to be
-- a subtype of it (no cross-CAS-type slippage from unifyStrict's broad rules).
isCompatible :: Type -> Type -> Bool
isCompatible target inst
  | not (Set.null (freeTyVars inst)) = case unifyStrict inst target of
      Right _ -> True
      Left _  -> False
  | otherwise = isSubtype target inst

-- | Multi-parameter version of `findMostSpecificInstance`.
-- Generalises the single-target specificity rule pairwise: instance A is
-- "more specific" than instance B if A's type list is pointwise a subtype
-- of B's type list (and A != B). Single-param classes (one target type) get
-- the same behaviour as `findMostSpecificInstance`.
findMostSpecificInstanceForTypes :: [Type] -> [InstanceInfo] -> Either AmbiguityError InstanceInfo
findMostSpecificInstanceForTypes targets insts =
  case filter (\inst -> isCompatibleAll targets (instTypes inst)) insts of
    []  -> Left NoMatchingInstance
    [x] -> Right x
    cs  ->
      let isMostSpecific x =
            all (\y -> instTypes y == instTypes x
                       || isSubtypeList (instTypes x) (instTypes y))
                cs
          mostSpecific = filter isMostSpecific cs
      in case mostSpecific of
           [unique] -> Right unique
           []       -> Left AmbiguousIncomparable
           _        -> Left AmbiguousMultipleMaxima
  where
    isCompatibleAll ts is
      | length ts /= length is = False
      | otherwise = and (zipWith isCompatible ts is)
    isSubtypeList xs ys
      | length xs /= length ys = False
      | otherwise = and (zipWith isSubtype xs ys)
