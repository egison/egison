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
  , lookupEnvExact
  , removeFromEnv
  , envToList
  , freeVarsInEnv
  , freeCapVarsInEnv
  , generalize
  , instantiate
  , AnnotationSkolems(..)
  , skolemizeAnnotation
  , skolemizePatternAnnotation
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
  -- * Checked pattern-function dual environment
  , PatternFunctionEnv(..)
  , emptyPatternFunctionEnv
  , extendPatternFunctionEnv
  , removePatternFunctionEnv
  , lookupPatternFunctionEnv
  , patternFunctionEnvToList
  ) where

import           Data.List                  (sortBy, sortOn)
import           Data.Ord                   (Down(..))
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set

import           Language.Egison.IExpr      (Var(..), Index(..))
import           Language.Egison.VarEntry   (VarEntry(..))
import           Language.Egison.Type.Types (Capability (..), CapVar, TyVar,
                                             Type (..), TypeScheme (..),
                                             DualScheme,
                                             Constraint(..), ClassInfo(..),
                                             InstanceInfo(..), freeCapVars,
                                             freeTyVars, freshCapVar,
                                             freshTyVarLike,
                                             substCapVarInType, substTyVar)

-- | Type environment: uses same data structure as evaluation environment
-- Maps base variable names to all bindings with that name
-- VarEntry list is sorted by index length (shortest first) for efficient prefix matching
newtype TypeEnv = TypeEnv { unTypeEnv :: Map String [VarEntry TypeScheme] }
  deriving (Eq, Show)

-- | Target-only signatures used for frozen pattern constructors or for
-- pattern-function headers, depending on the owning state field.  Finalized
-- pattern functions use the separate two-sorted 'PatternFunctionEnv'.
newtype PatternTypeEnv = PatternTypeEnv { unPatternTypeEnv :: Map String TypeScheme }
  deriving (Eq, Show)

-- | Fully checked pattern-function signatures.  Header-only declarations are
-- intentionally kept in 'PatternTypeEnv' until their bodies have produced a
-- complete capability/target 'DualScheme'.
newtype PatternFunctionEnv = PatternFunctionEnv
  { unPatternFunctionEnv :: Map String DualScheme
  } deriving (Eq, Show)

-- | Empty type environment
emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

-- | Extend the environment with a new binding
extendEnv :: Var -> TypeScheme -> TypeEnv -> TypeEnv
extendEnv (Var name indices) scheme (TypeEnv env) =
  let entry = VarEntry indices scheme
      newEntries = case Map.lookup name env of
        Nothing -> [entry]
        Just existingEntries -> sortOn (length . veIndices) (entry : existingEntries)
  in TypeEnv $ Map.insert name newEntries env

-- | Extend the environment with multiple bindings
extendEnvMany :: [(Var, TypeScheme)] -> TypeEnv -> TypeEnv
extendEnvMany bindings env = foldr (uncurry extendEnv) env bindings

-- | Look up a variable requiring the index structure to match exactly.
-- Used for definition-site signature detection (IDefine): a definition
-- `def ∇_c T... := ...` must not pick up the signature of an index-less
-- binding of the same name (e.g. the stdlib `∇ : Tensor MathValue -> ...`)
-- through the prefix/suffix fallbacks of 'lookupEnv'; the two are distinct
-- variables (the runtime environment also keeps them separate).
lookupEnvExact :: Var -> TypeEnv -> Maybe TypeScheme
lookupEnvExact (Var name indices) (TypeEnv env) =
  case Map.lookup name env of
    Nothing -> Nothing
    Just entries ->
      case [veValue e | e <- entries, veIndices e == indices] of
        (scheme:_) -> Just scheme
        []         -> Nothing

-- | Look up a variable in the environment
-- Search algorithm (same as refVar in Data.hs):
--   1. Try exact match
--   2. Try prefix match (find longer indices and auto-complete with #)
--   3. Try suffix removal (find shorter indices, pick longest match)
-- No recursion is used; all matching is done in a single pass to avoid infinite loops.
lookupEnv :: Var -> TypeEnv -> Maybe TypeScheme
lookupEnv (Var name targetIndices) (TypeEnv env) =
  case Map.lookup name env of
    Nothing -> Nothing
    Just entries ->
      -- 1. Try exact match first
      case findExactMatch targetIndices entries of
        Just scheme -> Just scheme
        Nothing ->
          -- 2. Try prefix matching (e_a matches e_i_j)
          case findPrefixMatch targetIndices entries of
            Just scheme -> Just scheme
            Nothing ->
              -- 3. Try suffix removal (e_i_j_k matches e_i_j, pick longest)
              findSuffixMatch targetIndices entries
  where
    -- Exact match: same length and same indices
    findExactMatch :: [Index (Maybe Var)] -> [VarEntry TypeScheme] -> Maybe TypeScheme
    findExactMatch indices entries =
      case [veValue e | e <- entries, veIndices e == indices] of
        (scheme:_) -> Just scheme
        [] -> Nothing
    
    -- Prefix matching: find shortest entry where target indices are a prefix
    -- Example: target [a] matches [i, j] in e_i_j (shortest match)
    findPrefixMatch :: [Index (Maybe Var)] -> [VarEntry TypeScheme] -> Maybe TypeScheme
    findPrefixMatch indices entries =
      -- entries are sorted by index length (ascending), so first match is shortest
      case [veValue e | e <- entries, isPrefixOfIndices indices (veIndices e)] of
        (scheme:_) -> Just scheme
        [] -> Nothing
    
    -- Suffix removal: find longest entry where stored indices are a prefix of target
    -- Example: target [i,j,k] matches e_i_j (stored [i,j]); prefer e_i_j over e_i
    -- Single pass, no recursion - safe from infinite loops
    findSuffixMatch :: [Index (Maybe Var)] -> [VarEntry TypeScheme] -> Maybe TypeScheme
    findSuffixMatch targetIndices entries =
      let suffixMatches = [e | e <- entries, storedIsPrefixOfTarget (veIndices e) targetIndices]
      in case sortByIndexLengthDesc suffixMatches of
        (e:_) -> Just (veValue e)
        [] -> Nothing
    
    -- stored is prefix of target: stored has fewer indices, first part of target matches
    storedIsPrefixOfTarget :: [Index (Maybe Var)] -> [Index (Maybe Var)] -> Bool
    storedIsPrefixOfTarget stored target =
      not (null target) &&
      length stored < length target &&
      stored == take (length stored) target
    
    -- Sort by descending index length, preserving insertion order for equal lengths
    -- so that local bindings (added later via extendEnv) come before global ones
    sortByIndexLengthDesc :: [VarEntry TypeScheme] -> [VarEntry TypeScheme]
    sortByIndexLengthDesc = sortBy (\a b -> compare (Down (length (veIndices a))) (Down (length (veIndices b))))
    
    -- Check if target is a prefix of candidate (for prefix matching)
    -- Example: [a] is prefix of [i, j]
    -- IMPORTANT: target must be non-empty to avoid matching everything
    isPrefixOfIndices :: [Index (Maybe Var)] -> [Index (Maybe Var)] -> Bool
    isPrefixOfIndices target candidate =
      not (null target) &&
      length target < length candidate &&
      target == take (length target) candidate

-- | Remove a variable from the environment
removeFromEnv :: Var -> TypeEnv -> TypeEnv
removeFromEnv (Var name indices) (TypeEnv env) =
  case Map.lookup name env of
    Nothing -> TypeEnv env
    Just entries ->
      let newEntries = [e | e <- entries, veIndices e /= indices]
      in if null newEntries
         then TypeEnv $ Map.delete name env
         else TypeEnv $ Map.insert name newEntries env

-- | Convert environment to list
envToList :: TypeEnv -> [(Var, TypeScheme)]
envToList (TypeEnv env) =
  [ (Var name (veIndices entry), veValue entry)
  | (name, entries) <- Map.toList env
  , entry <- entries
  ]

-- | Get free type variables in the environment
freeVarsInEnv :: TypeEnv -> Set TyVar
freeVarsInEnv (TypeEnv env) = 
  Set.unions $ map freeVarsInScheme $ concat $ Map.elems env
  where
    freeVarsInScheme entry = 
      let Forall _ vs cs t = veValue entry
          vars = freeTyVars t `Set.union`
                 Set.unions (map (Set.unions . map freeTyVars . constraintTypes) cs)
      in vars `Set.difference` Set.fromList vs

-- | Get free capability variables in the environment.
freeCapVarsInEnv :: TypeEnv -> Set CapVar
freeCapVarsInEnv (TypeEnv env) =
  Set.unions $ map freeVarsInScheme $ concat $ Map.elems env
  where
    freeVarsInScheme entry =
      let Forall capVars _ cs t = veValue entry
          vars = freeCapVars t `Set.union`
                 Set.unions (map (Set.unions . map freeCapVars . constraintTypes) cs)
      in vars `Set.difference` Set.fromList capVars

-- | Generalize a type to a type scheme (without constraints)
-- Generalize all free type variables that are not in the environment
generalize :: TypeEnv -> Type -> TypeScheme
generalize env t =
  let envFreeCapVars = freeCapVarsInEnv env
      envFreeVars = freeVarsInEnv env
      typeFreeCapVars = freeCapVars t
      typeFreeVars = freeTyVars t
      genCapVars = Set.toList $ typeFreeCapVars `Set.difference` envFreeCapVars
      genVars = Set.toList $ typeFreeVars `Set.difference` envFreeVars
  in Forall genCapVars genVars [] t

-- | Instantiate a type scheme with fresh variables of both sorts.
-- Returns a tuple of (constraints, instantiated type, fresh variable counter)
instantiate :: TypeScheme -> Int -> ([Constraint], Type, Int)
instantiate = instantiateWith CapVar "c" TVar "t"

-- | Fresh rigid names introduced while checking an explicit type scheme.
--
-- Each pair is @(fresh skolem, declared binder)@.  Successful checking uses
-- this map to restore the declared binders in the typed tree before it leaves
-- the annotation boundary.  The fresh prefixes start with @$@, which surface
-- type and capability variable parsers cannot produce, and the shared
-- inference counter prevents collisions between nested checks.
data AnnotationSkolems = AnnotationSkolems
  { annotationCapSkolems :: [(CapVar, CapVar)]
  , annotationTySkolems  :: [(TyVar, TyVar)]
  } deriving (Eq, Show)

-- | Instantiate every quantified capability of an explicit scheme as a rigid
-- skolem.  Type binders use protected fresh A/R variables: inference may
-- strengthen an A binder to R, but the annotation boundary later rejects any
-- other image.  Thus source annotations may omit inferred A/R flags without
-- permitting ordinary type specialization.
skolemizeAnnotation
  :: TypeScheme
  -> Int
  -> ([Constraint], Type, AnnotationSkolems, Int)
skolemizeAnnotation = skolemizeAnnotationWith True

-- | Pattern targets are inputs to matching rather than returned values, so
-- their quantified variables retain the source A/R classes.
skolemizePatternAnnotation
  :: TypeScheme
  -> Int
  -> ([Constraint], Type, AnnotationSkolems, Int)
skolemizePatternAnnotation = skolemizeAnnotationWith False

skolemizeAnnotationWith
  :: Bool
  -> TypeScheme
  -> Int
  -> ([Constraint], Type, AnnotationSkolems, Int)
skolemizeAnnotationWith requireResult sourceScheme counter =
  let (constraints, ty, finalCounter) =
        instantiateWith CapSkolem "$skc" TVar "$skt" scheme counter
      capFresh =
        [ freshCapVar "$skc" (counter + index)
        | index <- [0 .. length capVars - 1]
        ]
      typeCounter = counter + length capVars
      tyFresh =
        zipWith
          (\binder index ->
            freshTyVarLike binder "$skt" (typeCounter + index))
          tyVars
          [0 ..]
      skolems =
        AnnotationSkolems
          (zip capFresh capVars)
          (zip tyFresh tyVars)
  in (constraints, ty, skolems, finalCounter)
  where
    scheme@(Forall capVars tyVars _ _) = sourceScheme

instantiateWith
  :: (CapVar -> Capability)
  -> String
  -> (TyVar -> Type)
  -> String
  -> TypeScheme
  -> Int
  -> ([Constraint], Type, Int)
instantiateWith capNode capPrefix typeNode typePrefix
                (Forall capVars tyVars cs t) counter =
  let freshCaps =
        zipWith
          (\v i -> (v, capNode (freshCapVar capPrefix (counter + i))))
          capVars
          [0..]
      typeCounter = counter + length capVars
      freshTypes =
        zipWith
          (\v i ->
            (v, typeNode (freshTyVarLike v typePrefix (typeCounter + i))))
          tyVars
          [0..]
      substType = substituteBoth freshCaps freshTypes t
      substCs = map (substConstraint freshCaps freshTypes) cs
      finalCounter = typeCounter + length tyVars
  in (substCs, substType, finalCounter)
  where
    substituteBoth capBindings typeBindings =
      applyTypeBindings typeBindings . applyCapBindings capBindings

    applyCapBindings bindings ty =
      foldr (\(old, new) acc -> substCapVarInType old new acc) ty bindings

    applyTypeBindings bindings ty =
      foldr (\(old, new) acc -> substTyVar old new acc) ty bindings

    substConstraint capBindings typeBindings (Constraint cls tys) =
      Constraint cls (map (substituteBoth capBindings typeBindings) tys)

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

-- | Empty checked pattern-function environment.
emptyPatternFunctionEnv :: PatternFunctionEnv
emptyPatternFunctionEnv = PatternFunctionEnv Map.empty

-- | Add or replace a finalized pattern-function dual scheme.
extendPatternFunctionEnv
  :: String -> DualScheme -> PatternFunctionEnv -> PatternFunctionEnv
extendPatternFunctionEnv name scheme (PatternFunctionEnv env) =
  PatternFunctionEnv (Map.insert name scheme env)

-- | Remove a finalized pattern-function scheme.  Environment prepasses use
-- this when a new declaration shadows an older checked body: the new header
-- remains available for forward references, but the old body contract must
-- not be reused before the replacement body has itself been checked.
removePatternFunctionEnv :: String -> PatternFunctionEnv -> PatternFunctionEnv
removePatternFunctionEnv name (PatternFunctionEnv env) =
  PatternFunctionEnv (Map.delete name env)

-- | Look up a finalized pattern-function dual scheme.
lookupPatternFunctionEnv :: String -> PatternFunctionEnv -> Maybe DualScheme
lookupPatternFunctionEnv name (PatternFunctionEnv env) = Map.lookup name env

-- | Convert a finalized pattern-function environment to a stable list.
patternFunctionEnvToList :: PatternFunctionEnv -> [(String, DualScheme)]
patternFunctionEnvToList (PatternFunctionEnv env) = Map.toList env
