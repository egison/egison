{- |
Module      : Language.Egison.Type.Env
Licence     : MIT

This module provides type environment for the Egison type system.
-}

module Language.Egison.Type.Env
  ( TypeEnv(..)
  , emptyEnv
  , extendEnv
  , extendEnvMany
  , lookupEnv
  , removeFromEnv
  , envToList
  , freeVarsInEnv
  , generalize
  , instantiate
  -- * Class environment
  , ClassEnv(..)
  , ClassInfo(..)
  , InstanceInfo(..)
  , emptyClassEnv
  , addClass
  , addInstance
  , lookupClass
  , lookupInstances
  , classEnvToList
  , mergeClassEnv
  -- * Pattern type environment
  , PatternTypeEnv(..)
  , emptyPatternEnv
  , extendPatternEnv
  , lookupPatternEnv
  , patternEnvToList
  ) where

import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set

import           Language.Egison.Type.Types (TyVar (..), Type (..), TypeScheme (..),
                                             Constraint(..), ClassInfo(..), InstanceInfo(..),
                                             freeTyVars, freshTyVar)

-- | Type environment: maps variable names to type schemes
newtype TypeEnv = TypeEnv { unTypeEnv :: Map String TypeScheme }
  deriving (Eq, Show)

-- | Pattern type environment: maps pattern function names to type schemes
-- This is separate from the value type environment
newtype PatternTypeEnv = PatternTypeEnv { unPatternTypeEnv :: Map String TypeScheme }
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
    freeVarsInScheme (Forall vs _ t) = freeTyVars t `Set.difference` Set.fromList vs

-- | Generalize a type to a type scheme (without constraints)
-- Generalize all free type variables that are not in the environment
generalize :: TypeEnv -> Type -> TypeScheme
generalize env t =
  let envFreeVars = freeVarsInEnv env
      typeFreeVars = freeTyVars t
      genVars = Set.toList $ typeFreeVars `Set.difference` envFreeVars
  in Forall genVars [] t

-- | Instantiate a type scheme with fresh type variables
-- Returns a tuple of (constraints, instantiated type, fresh variable counter)
instantiate :: TypeScheme -> Int -> ([Constraint], Type, Int)
instantiate (Forall vs cs t) counter =
  let freshVars = zipWith (\v i -> (v, TVar (freshTyVar "t" (counter + i)))) vs [0..]
      substType = foldr (\(old, new) acc -> substVar old new acc) t freshVars
      substCs = map (substConstraint freshVars) cs
  in (substCs, substType, counter + length vs)
  where
    substConstraint :: [(TyVar, Type)] -> Constraint -> Constraint
    substConstraint vars (Constraint cls ty) =
      Constraint cls (foldr (\(old, new) acc -> substVar old new acc) ty vars)
    substVar :: TyVar -> Type -> Type -> Type
    substVar _ _ TInt = TInt
    substVar _ _ TFloat = TFloat
    substVar _ _ TBool = TBool
    substVar _ _ TChar = TChar
    substVar _ _ TString = TString
    substVar old new (TVar v)
      | v == old = new
      | otherwise = TVar v
    substVar old new (TTuple ts) = TTuple (map (substVar old new) ts)
    substVar old new (TCollection t') = TCollection (substVar old new t')
    substVar old new (TInductive name ts) = TInductive name (map (substVar old new) ts)
    substVar old new (TTensor t') = TTensor (substVar old new t')
    substVar old new (THash k v) = THash (substVar old new k) (substVar old new v)
    substVar old new (TMatcher t') = TMatcher (substVar old new t')
    substVar old new (TFun t1 t2) = TFun (substVar old new t1) (substVar old new t2)
    substVar old new (TIO t') = TIO (substVar old new t')
    substVar old new (TIORef t') = TIORef (substVar old new t')
    substVar _ _ TAny = TAny

--------------------------------------------------------------------------------
-- Class Environment
--------------------------------------------------------------------------------

-- | Class environment: maps class names to class info and instances
data ClassEnv = ClassEnv
  { classEnvClasses   :: Map String ClassInfo      -- ^ Class definitions
  , classEnvInstances :: Map String [InstanceInfo] -- ^ Instances per class
  } deriving (Eq, Show)

-- | Empty class environment
emptyClassEnv :: ClassEnv
emptyClassEnv = ClassEnv Map.empty Map.empty

-- | Add a class to the environment
addClass :: String -> ClassInfo -> ClassEnv -> ClassEnv
addClass name info (ClassEnv classes insts) =
  ClassEnv (Map.insert name info classes) insts

-- | Add an instance to the environment
addInstance :: String -> InstanceInfo -> ClassEnv -> ClassEnv
addInstance className inst (ClassEnv classes insts) =
  ClassEnv classes (Map.insertWith (++) className [inst] insts)

-- | Look up a class definition
lookupClass :: String -> ClassEnv -> Maybe ClassInfo
lookupClass name (ClassEnv classes _) = Map.lookup name classes

-- | Look up instances for a class
lookupInstances :: String -> ClassEnv -> [InstanceInfo]
lookupInstances name (ClassEnv _ insts) = Map.findWithDefault [] name insts

-- | Convert class environment to list
classEnvToList :: ClassEnv -> [(String, ClassInfo)]
classEnvToList (ClassEnv classes _) = Map.toList classes

-- | Merge two class environments
-- The second environment's definitions take precedence in case of conflicts
mergeClassEnv :: ClassEnv -> ClassEnv -> ClassEnv
mergeClassEnv (ClassEnv classes1 insts1) (ClassEnv classes2 insts2) =
  ClassEnv
    (Map.union classes2 classes1)  -- classes2 takes precedence
    (Map.unionWith (++) insts2 insts1)  -- Combine instance lists

--------------------------------------------------------------------------------
-- Pattern Type Environment
--------------------------------------------------------------------------------

-- | Empty pattern type environment
emptyPatternEnv :: PatternTypeEnv
emptyPatternEnv = PatternTypeEnv Map.empty

-- | Extend the pattern type environment with a new binding
extendPatternEnv :: String -> TypeScheme -> PatternTypeEnv -> PatternTypeEnv
extendPatternEnv name scheme (PatternTypeEnv env) = PatternTypeEnv $ Map.insert name scheme env

-- | Look up a pattern constructor/function in the environment
lookupPatternEnv :: String -> PatternTypeEnv -> Maybe TypeScheme
lookupPatternEnv name (PatternTypeEnv env) = Map.lookup name env

-- | Convert pattern type environment to list
patternEnvToList :: PatternTypeEnv -> [(String, TypeScheme)]
patternEnvToList (PatternTypeEnv env) = Map.toList env

