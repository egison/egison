{- |
Module      : Language.Egison.Type.Subst
Licence     : MIT

This module provides type substitution operations for the type system.
-}

{-# LANGUAGE DeriveGeneric #-}

module Language.Egison.Type.Subst
  ( Subst
  , emptySubst
  , singletonSubst
  , composeSubst
  , applySubst
  , applySubstScheme
  , applySubstConstraint
  , SubstIndex
  , emptySubstIndex
  , singletonSubstIndex
  , applySubstIndex
  ) where

import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           GHC.Generics               (Generic)

import           Language.Egison.Type.Index (Index (..), IndexSpec, IndexTyVar (..))
import           Language.Egison.Type.Types (TyVar (..), Type (..), TypeScheme (..), Constraint(..))

-- | Type substitution: a mapping from type variables to types
newtype Subst = Subst { unSubst :: Map TyVar Type }
  deriving (Eq, Show, Generic)

-- | Empty substitution
emptySubst :: Subst
emptySubst = Subst Map.empty

-- | Create a substitution with a single binding
singletonSubst :: TyVar -> Type -> Subst
singletonSubst v t = Subst $ Map.singleton v t

-- | Compose two substitutions (s2 after s1)
-- (s2 `composeSubst` s1) x = s2 (s1 x)
composeSubst :: Subst -> Subst -> Subst
composeSubst s2@(Subst m2) (Subst m1) =
  Subst $ Map.map (applySubst s2) m1 `Map.union` m2

-- | Apply a substitution to a type
applySubst :: Subst -> Type -> Type
applySubst _ TInt             = TInt
applySubst _ TFloat           = TFloat
applySubst _ TBool            = TBool
applySubst _ TChar            = TChar
applySubst _ TString          = TString
applySubst (Subst m) t@(TVar v) = Map.findWithDefault t v m
applySubst s (TTuple ts)      = TTuple (map (applySubst s) ts)
applySubst s (TCollection t)  = TCollection (applySubst s t)
applySubst s (TInductive name ts) = TInductive name (map (applySubst s) ts)
applySubst s (TTensor t)      = TTensor (applySubst s t)
applySubst s (THash k v)      = THash (applySubst s k) (applySubst s v)
applySubst s (TMatcher t)     = TMatcher (applySubst s t)
applySubst s (TFun t1 t2)     = TFun (applySubst s t1) (applySubst s t2)
applySubst s (TIO t)          = TIO (applySubst s t)
applySubst s (TIORef t)       = TIORef (applySubst s t)
applySubst _ TAny             = TAny

-- | Apply a substitution to a type scheme
applySubstScheme :: Subst -> TypeScheme -> TypeScheme
applySubstScheme (Subst m) (Forall vs cs t) =
  let m' = foldr Map.delete m vs
      s' = Subst m'
  in Forall vs (map (applySubstConstraint s') cs) (applySubst s' t)

-- | Apply a substitution to a constraint
applySubstConstraint :: Subst -> Constraint -> Constraint
applySubstConstraint s (Constraint cls ty) = Constraint cls (applySubst s ty)

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

