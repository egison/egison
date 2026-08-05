{- |
Module      : Language.Egison.Type.Subst
Licence     : MIT

This module provides type substitution operations for the type system.
-}

{-# LANGUAGE DeriveGeneric #-}

module Language.Egison.Type.Subst
  ( Subst(..)
  , emptySubst
  , singletonSubst
  , singletonCapSubst
  , composeSubst
  , applySubst
  , applyTypeSubst
  , applyCapSubst
  , applyCapSubstToType
  , applySubstScheme
  , applySubstDual
  , applySubstConstraint
  , SubstIndex
  , emptySubstIndex
  , singletonSubstIndex
  , applySubstIndex
  ) where

import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import           GHC.Generics               (Generic)

import           Language.Egison.Type.Index (Index (..), IndexSpec, IndexTyVar (..))
import           Language.Egison.Type.Types (Capability (..), CapVar, TyVar,
                                             Type (..), TypeScheme (..),
                                             Dual (..),
                                             Constraint (..), SymbolSet (..),
                                             mapTypeCapabilities)

-- | A pair of independent substitutions, one for each sort.
--
-- 'singletonSubst' can only populate the ordinary type component and
-- 'singletonCapSubst' can only populate the capability component.  This
-- representation prevents target unification from accidentally refining a
-- matcher capability.
data Subst = Subst
  { unSubst    :: Map TyVar Type
  , unCapSubst :: Map CapVar Capability
  }
  deriving (Eq, Show, Generic)

-- | Empty substitution
emptySubst :: Subst
emptySubst = Subst Map.empty Map.empty

-- | Create a substitution with a single binding
singletonSubst :: TyVar -> Type -> Subst
singletonSubst v t = Subst (Map.singleton v t) Map.empty

-- | Create a substitution with a single flexible capability binding.
singletonCapSubst :: CapVar -> Capability -> Subst
singletonCapSubst v cap = Subst Map.empty (Map.singleton v cap)

-- | Compose two substitutions (s2 after s1)
-- (s2 `composeSubst` s1) x = s2 (s1 x)
composeSubst :: Subst -> Subst -> Subst
composeSubst s2@(Subst tys2 caps2) (Subst tys1 caps1) =
  Subst
    (Map.map (applySubst s2) tys1 `Map.union` tys2)
    (Map.map (applyCapSubst s2) caps1 `Map.union` caps2)

-- | Apply both components of a substitution to a type.
--
-- The ordinary component is applied first so capability variables contained
-- in a substituted type range are subsequently reached by the capability
-- component.
applySubst :: Subst -> Type -> Type
applySubst s = applyCapSubstToType s . applyTypeSubst s

-- | Apply only the ordinary type component of a substitution.
--
-- In particular, matcher capabilities are copied verbatim.
applyTypeSubst :: Subst -> Type -> Type
applyTypeSubst substitution@(Subst types _) = go Set.empty
  where
    -- Follow substitution chains to a genuine fixed point.  Keeping a visited
    -- set makes the operation total even if an invalid cyclic substitution is
    -- presented by a diagnostic/recovery path.
    go seen type' = case type' of
      TVar variable
        | variable `Set.member` seen ->
            TVar variable
        | Just replacement <- Map.lookup variable types ->
            go (Set.insert variable seen) replacement
        | otherwise ->
            TVar variable
      TTuple values ->
        TTuple (map (go seen) values)
      TCollection value ->
        TCollection (go seen value)
      TInductive name arguments ->
        TInductive name (map (go seen) arguments)
      TTensor value ->
        TTensor (go seen value)
      THash key value ->
        THash (go seen key) (go seen value)
      TMatcher capability target ->
        TMatcher capability (go seen target)
      TMatcherSlot capability target ->
        TMatcherSlot capability (go seen target)
      TFun argument result ->
        TFun (go seen argument) (go seen result)
      TIO value ->
        TIO (go seen value)
      TIORef value ->
        TIORef (go seen value)
      TTerm value symbols ->
        TTerm (go seen value) (applySubstSymbolSet substitution symbols)
      TFrac value ->
        TFrac (go seen value)
      TPoly value symbols ->
        TPoly (go seen value) (applySubstSymbolSet substitution symbols)
      leaf ->
        leaf

-- | Apply only the capability component to a capability.
--
-- 'CapSkolem' is not a flexible variable and therefore cannot be replaced.
applyCapSubst :: Subst -> Capability -> Capability
applyCapSubst (Subst _ caps) = go Set.empty
  where
    go seen capability = case capability of
      CapVar variable
        | variable `Set.member` seen ->
            CapVar variable
        | Just replacement <- Map.lookup variable caps ->
            go (Set.insert variable seen) replacement
        | otherwise ->
            CapVar variable
      CapCon former children ->
        CapCon former (map (go seen) children)
      CapTuple components ->
        CapTuple (map (go seen) components)
      leaf ->
        leaf

-- | Apply only the capability component throughout a type.
applyCapSubstToType :: Subst -> Type -> Type
applyCapSubstToType s = mapTypeCapabilities replace
  where
    replace cap@(CapVar _) = applyCapSubst s cap
    replace cap            = cap

-- | Apply a substitution to a SymbolSet
applySubstSymbolSet :: Subst -> SymbolSet -> SymbolSet
applySubstSymbolSet _ ss@(SymbolSetClosed _) = ss
applySubstSymbolSet _ SymbolSetOpen = SymbolSetOpen
applySubstSymbolSet (Subst tys _) ss@(SymbolSetVar v) =
  case Map.lookup v tys of
    Just (TPoly _ ss') -> ss'  -- If variable maps to a Poly type, extract its symbol set
    _                  -> ss   -- Otherwise keep as variable

-- | Apply a substitution to a type scheme
applySubstScheme :: Subst -> TypeScheme -> TypeScheme
applySubstScheme (Subst tys caps) (Forall capVars tyVars cs t) =
  let tys' = foldr Map.delete tys tyVars
      caps' = foldr Map.delete caps capVars
      s' = Subst tys' caps'
  in Forall capVars tyVars
       (map (applySubstConstraint s') cs)
       (applySubst s' t)

-- | Apply one paired substitution to both sorts of a pattern dual.
applySubstDual :: Subst -> Dual -> Dual
applySubstDual substitution (Dual capability target) =
  Dual
    (applyCapSubst substitution capability)
    (applySubst substitution target)

-- | Apply a substitution to a constraint
applySubstConstraint :: Subst -> Constraint -> Constraint
applySubstConstraint s (Constraint cls tys) = Constraint cls (map (applySubst s) tys)

-- | Index substitution: mapping from index variables to indices
newtype SubstIndex = SubstIndex { unSubstIndex :: Map IndexTyVar Index }
  deriving (Eq, Show, Generic)

-- | Empty index substitution
emptySubstIndex :: SubstIndex
emptySubstIndex = SubstIndex Map.empty

-- | Create an index substitution with a single binding
singletonSubstIndex :: IndexTyVar -> Index -> SubstIndex
singletonSubstIndex v i = SubstIndex $ Map.singleton v i

-- | Apply an index substitution to an index specification
applySubstIndex :: SubstIndex -> IndexSpec -> IndexSpec
applySubstIndex (SubstIndex m) = map apply
  where
    apply i@(IndexVar s) = Map.findWithDefault i (IndexTyVar s) m
    apply i = i
