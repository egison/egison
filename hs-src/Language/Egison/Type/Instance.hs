{- |
Module      : Language.Egison.Type.Instance
Licence     : MIT

This module provides utilities for matching type class instances.
-}

module Language.Egison.Type.Instance
  ( findMatchingInstanceForType
  , findMatchingInstanceForTypes
  , findMostSpecificInstanceForTypes
  , selectMostSpecific
  , AmbiguityError(..)
  ) where

import qualified Data.Set                   as Set
import           Language.Egison.Type.Types (Type, InstanceInfo, instType, instTypes,
                                              freeTyVars)
import           Language.Egison.Type.Unify (unifyStrict)
import           Language.Egison.Type.Subst (Subst, emptySubst, composeSubst, applySubst)
import           Language.Egison.Type.Subtype (SubtypeEnv, isSubtypeWith, sameCasHead)

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

-- | Errors raised by most-specific instance selection.
-- See design/runtime-type-dispatch.md §4 for the full rationale.
data AmbiguityError
  = NoMatchingInstance
    -- ^ No candidate instance accepts the target type.
  | AmbiguousIncomparable
    -- ^ Multiple candidates exist and at least two are not comparable
    -- via the subtype partial order, so no unique most-specific exists.
  | AmbiguousMultipleMaxima
    -- ^ Multiple most-specific candidates remain (order-equivalent
    -- types — mutual embeddings at the MathValue-coefficient level)
    -- and the representation-head tie-break could not single one out.
  deriving (Eq, Show)

-- | Most-specific selection among candidates that are already known to
-- accept the targets. `proj` projects a candidate to its type list.
--
-- The most specific candidate is the one whose projected list is
-- pointwise below every other's in the declared CAS order (complete
-- skeleton plus `declare cas-subtype` edges), so user-declared
-- embeddings participate in dispatch the same way built-in tower
-- embeddings do. Among order-EQUIVALENT survivors (the same value set
-- in different canonical forms, e.g. Poly MathValue [..] vs
-- Frac MathValue) the tie is broken by the targets' representation
-- heads — per the design principle, types select representations, so
-- dispatch follows the target's own head.
selectMostSpecific :: SubtypeEnv -> (c -> [Type]) -> [Type] -> [c] -> Either AmbiguityError c
selectMostSpecific edges proj targets cands =
  case cands of
    []  -> Left NoMatchingInstance
    [x] -> Right x
    cs  ->
      let leqList xs ys = length xs == length ys
                          && and (zipWith (isSubtypeWith edges) xs ys)
          isMostSpecific x =
            all (\y -> proj y == proj x || leqList (proj x) (proj y)) cs
          sameHeads x = length (proj x) == length targets
                        && and (zipWith sameCasHead targets (proj x))
      in case filter isMostSpecific cs of
           [unique] -> Right unique
           []       -> Left AmbiguousIncomparable
           several  ->
             case filter sameHeads several of
               [unique] -> Right unique
               _        -> Left AmbiguousMultipleMaxima

-- | A target is compatible with an instance type. The check is mode-aware:
-- if the instance type has free type variables (e.g. `Eq a`, `Coerce a [a]`),
-- we fall back to strict unification so type-variable instances can match.
-- Otherwise the instance is fully concrete and we require the target to be
-- a subtype of it (no cross-CAS-type slippage from unifyStrict's broad rules).
isCompatible :: SubtypeEnv -> Type -> Type -> Bool
isCompatible edges target inst
  | not (Set.null (freeTyVars inst)) = case unifyStrict inst target of
      Right _ -> True
      Left _  -> False
  | otherwise = isSubtypeWith edges target inst

-- | Pick the most specific instance accepting the targets (pointwise).
-- Single-param classes pass `[t]`. An instance is a candidate iff each
-- target is compatible with the corresponding instance type
-- ('isCompatible'); selection is 'selectMostSpecific'.
findMostSpecificInstanceForTypes :: SubtypeEnv -> [Type] -> [InstanceInfo] -> Either AmbiguityError InstanceInfo
findMostSpecificInstanceForTypes edges targets insts =
  selectMostSpecific edges instTypes targets
    (filter (isCompatibleAll targets . instTypes) insts)
  where
    isCompatibleAll ts is
      | length ts /= length is = False
      | otherwise = and (zipWith (isCompatible edges) ts is)
