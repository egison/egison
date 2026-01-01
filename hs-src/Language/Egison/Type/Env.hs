{- |
Module      : Language.Egison.Type.Env
Licence     : MIT

This module provides type environment for the Egison type system.
-}

module Language.Egison.Type.Env
  ( TypeEnv
  , emptyEnv
  , extendEnv
  , extendEnvMany
  , lookupEnv
  , removeFromEnv
  , envToList
  , freeVarsInEnv
  , generalize
  , instantiate
  ) where

import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set

import           Language.Egison.Type.Subst (Subst, applySubst, applySubstScheme)
import           Language.Egison.Type.Types (TyVar (..), Type (..), TypeScheme (..),
                                             freeTyVars, freshTyVar)

-- | Type environment: maps variable names to type schemes
newtype TypeEnv = TypeEnv { unTypeEnv :: Map String TypeScheme }
  deriving (Eq, Show)

-- | Empty type environment
emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

-- | Extend the environment with a new binding
extendEnv :: String -> TypeScheme -> TypeEnv -> TypeEnv
extendEnv name scheme (TypeEnv env) = TypeEnv $ Map.insert name scheme env

-- | Extend the environment with multiple bindings
extendEnvMany :: [(String, TypeScheme)] -> TypeEnv -> TypeEnv
extendEnvMany bindings env = foldr (uncurry extendEnv) env bindings

-- | Look up a variable in the environment
lookupEnv :: String -> TypeEnv -> Maybe TypeScheme
lookupEnv name (TypeEnv env) = Map.lookup name env

-- | Remove a variable from the environment
removeFromEnv :: String -> TypeEnv -> TypeEnv
removeFromEnv name (TypeEnv env) = TypeEnv $ Map.delete name env

-- | Convert environment to list
envToList :: TypeEnv -> [(String, TypeScheme)]
envToList (TypeEnv env) = Map.toList env

-- | Get free type variables in the environment
freeVarsInEnv :: TypeEnv -> Set TyVar
freeVarsInEnv (TypeEnv env) = Set.unions $ map freeVarsInScheme $ Map.elems env
  where
    freeVarsInScheme (Forall vs t) = freeTyVars t `Set.difference` Set.fromList vs

-- | Generalize a type to a type scheme
-- Generalize all free type variables that are not in the environment
generalize :: TypeEnv -> Type -> TypeScheme
generalize env t =
  let envFreeVars = freeVarsInEnv env
      typeFreeVars = freeTyVars t
      genVars = Set.toList $ typeFreeVars `Set.difference` envFreeVars
  in Forall genVars t

-- | Instantiate a type scheme with fresh type variables
-- Returns a tuple of (instantiated type, fresh variable counter)
instantiate :: TypeScheme -> Int -> (Type, Int)
instantiate (Forall vs t) counter =
  let freshVars = zipWith (\v i -> (v, TVar (freshTyVar "t" (counter + i)))) vs [0..]
      subst = foldr (\(old, new) acc -> substVar old new acc) t freshVars
  in (subst, counter + length vs)
  where
    substVar :: TyVar -> Type -> Type -> Type
    substVar _ _ TInt = TInt
    substVar _ _ TFloat = TFloat
    substVar _ _ TBool = TBool
    substVar _ _ TChar = TChar
    substVar _ _ TString = TString
    substVar _ _ TUnit = TUnit
    substVar _ _ TAny = TAny
    substVar old new (TVar v)
      | v == old = new
      | otherwise = TVar v
    substVar old new (TList t') = TList (substVar old new t')
    substVar old new (TTuple ts) = TTuple (map (substVar old new) ts)
    substVar old new (TFun t1 t2) = TFun (substVar old new t1) (substVar old new t2)
    substVar old new (TMatcher t') = TMatcher (substVar old new t')
    substVar old new (TPattern t') = TPattern (substVar old new t')
    substVar old new (TTensor t' sh is) = TTensor (substVar old new t') sh is
    substVar old new (TCollection t') = TCollection (substVar old new t')
    substVar old new (THash k v) = THash (substVar old new k) (substVar old new v)
    substVar old new (TIORef t') = TIORef (substVar old new t')
    substVar old new (TIO t') = TIO (substVar old new t')
    substVar old new (TPatternFunc argTs retT) =
      TPatternFunc (map (substVar old new) argTs) (substVar old new retT)

-- | Apply a substitution to the type environment
applySubstEnv :: Subst -> TypeEnv -> TypeEnv
applySubstEnv s (TypeEnv env) = TypeEnv $ Map.map (applySubstScheme s) env

