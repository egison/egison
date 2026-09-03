{- |
Module      : Language.Egison.Type.Types
Licence     : MIT

This module defines the type system for Egison.
-}

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Egison.Type.Types
  ( Type(..)
  , Capability(..)
  , CapVar(..)
  , TypeFormer(..)
  , TypeFormerId(..)
  , SymbolSet(..)
  , TypeAtom(..)
  , prettyTypeAtomValue
  , TypeScheme(..)
  , Dual(..)
  , DualScheme(..)
  , TyVar(..)
  , tyVarName
  , freshTyVarLike
  , TensorShape(..)
  , ShapeDimType(..)
  , Constraint(..)
  , constraintType  -- backward-compat: head of constraintTypes
  , ClassInfo(..)
  , classParam
  , InstanceInfo(..)
  , instType
  , freshTyVar
  , freshCapVar
  , freeTyVars
  , freeTySkolems
  , freeCapVars
  , freeCapVarsCapability
  , freeCapVarsDual
  , freeTyVarsDual
  , freeCapVarsDualScheme
  , freeTyVarsDualScheme
  , freeCapSkolems
  , freeCapSkolemsCapability
  , mapCapability
  , mapTypeCapabilities
  , substCapVar
  , substCapVarInType
  , mkTypeFormer
  , typeFormerOf
  , capabilitySkeleton
  , capExprToCapability
  , isTensorType
  , isScalarType
  , isCASType
  , isSubsetSymbolSet
  , hasAmbiguousOpenTower
  , mapType
  , substTyVar
  , typeAtomExprToTypeAtom
  , typeToName
  , typeConstructorName
  , sanitizeMethodName
  , typeExprToType
  , normalizeInductiveTypes
  , normalizeMatcherProducts
  , dualSchemeTargetType
  , dualSchemeTargetScheme
  , expandTypeAliases
  , reservedCasTypeNames
  , capitalizeFirst
  , lowerFirst
  ) where

import           Data.Char        (toLower, toUpper)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable    (Hashable)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           GHC.Generics     (Generic)

import           Language.Egison.AST        (CapabilityExpr(..), TypeExpr(..),
                                             SymbolSetExpr(..), TypeAtomExpr(..))
import           Language.Egison.Type.Index ()

-- | The admissible images of an ordinary type variable.  Result variables
-- are more constrained: a value returned by a function may not contain a
-- matcher slot, except in a parameter position of a returned function.
-- | Ordinary type variable.  There is one sort of ordinary variables; the
-- string is the variable identity.
data TyVar
  = TyVar String
  deriving (Eq, Ord, Show, Generic, Hashable)

tyVarName :: TyVar -> String
tyVarName (TyVar name) = name

-- | Capability variable.  Its constructor is intentionally distinct from
-- the flexible 'CapVar' node of 'Capability'.
newtype CapVar = MkCapVar String
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Hashable

-- | Canonical identity of a type former in the frozen signature
-- environment.  Surface synonyms are removed before an ID is constructed.
newtype TypeFormerId = TypeFormerId String
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Hashable

-- | Canonical type former together with its kind-level arity.
--
-- Keeping arity in the identity prevents malformed applications of the same
-- spelling from being considered equal by capability matching.
data TypeFormer = TypeFormer
  { typeFormerId    :: TypeFormerId
  , typeFormerArity :: Int
  } deriving (Eq, Ord, Show, Generic, Hashable)

-- | Structural capability carried by a matcher.
--
-- 'CapAny' is a ground capability.  It is rigid under symmetric capability
-- equality, but a literal 'CapAny' in a consumer position is the one-way
-- catch-all accepted by matcher-to-slot checking.
--
-- Flexible variables and rigid skolems are separate constructors so an
-- annotation can be checked without its quantified capability being seeded
-- by the implementation being checked.
data Capability
  = CapAny
  | CapVar CapVar
  | CapSkolem CapVar
  | CapCon TypeFormer [Capability]
  | CapTuple [Capability]
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | Shape dimension (can be concrete or variable)
data ShapeDimType
  = DimLit Integer        -- ^ Concrete dimension, e.g., 2
  | DimVar String         -- ^ Dimension variable, e.g., n
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | Tensor shape (dimension sizes)
data TensorShape
  = ShapeLit [Integer]        -- ^ Concrete shape, e.g., [2, 2]
  | ShapeVar String           -- ^ Shape variable, e.g., ns in zeroTensor
  | ShapeMixed [ShapeDimType] -- ^ Mixed shape, e.g., [n, m, 2]
  | ShapeUnknown              -- ^ To be inferred
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | A type-level atom inside a closed symbol set.
-- Mirrors `TypeAtomExpr` from the AST; conversion is handled by
-- `typeAtomExprToTypeAtom`.
data TypeAtom
  = TANameAtom String             -- ^ A plain identifier, e.g. `x`, `i`
  | TAApplyAtom String [TypeAtom] -- ^ A function applied to atom args, e.g. `sin x`
  | TAIntAtom  Integer            -- ^ An integer literal in atom position
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | Pretty print a TypeAtom in canonical form.
prettyTypeAtomValue :: TypeAtom -> String
prettyTypeAtomValue (TANameAtom s)        = s
prettyTypeAtomValue (TAIntAtom n)         = show n
prettyTypeAtomValue (TAApplyAtom fn args) = unwords (fn : map prettyAtomArg args)
  where
    prettyAtomArg a@(TAApplyAtom _ _) = "(" ++ prettyTypeAtomValue a ++ ")"
    prettyAtomArg a                   = prettyTypeAtomValue a

-- | Convert an AST-level atom to a Type-level atom (1-to-1 correspondence).
typeAtomExprToTypeAtom :: TypeAtomExpr -> TypeAtom
typeAtomExprToTypeAtom (TAEName s)       = TANameAtom s
typeAtomExprToTypeAtom (TAEInt n)        = TAIntAtom n
typeAtomExprToTypeAtom (TAEApp fn args)  = TAApplyAtom fn (map typeAtomExprToTypeAtom args)

-- | Symbol set for polynomial types
-- Used to specify the indeterminates in a polynomial type
data SymbolSet
  = SymbolSetClosed [TypeAtom]  -- ^ Fixed symbol set, e.g., [x, y, sqrt 2]
  | SymbolSetOpen               -- ^ Open symbol set, e.g., [..] in Poly Integer [..]
  | SymbolSetVar TyVar          -- ^ Symbol set variable (for unification with open sets)
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | Egison types
data Type
  = TInt                              -- ^ Integer
  | TMathValue                         -- ^ MathValue (mathematical expression, unifies with Integer)
  -- The four *Expr types below type the views of the internal
  -- mathematical-expression data: primitive data patterns in matcher
  -- definitions (Plus / Term / Symbol / Apply1..4 / Quote / Function, as in
  -- lib/math/expression.egi) are given these types by the inference.
  | TPolyExpr                         -- ^ PolyExpr (a polynomial view: Plus)
  | TTermExpr                         -- ^ TermExpr (a term view: Term)
  | TSymbolExpr                       -- ^ SymbolExpr (a factor view: Symbol, Apply1..4, Quote, Function)
  | TIndexExpr                        -- ^ IndexExpr (an index view: Sub/Sup/User; also a surface type name)
  | TFloat                            -- ^ Float (Double)
  | TBool                             -- ^ Bool
  | TChar                             -- ^ Char
  | TString                           -- ^ String
  | TVar TyVar                        -- ^ Type variable, e.g., a
  | TSkolem TyVar                     -- ^ Rigid ordinary type variable used
                                      --   only while checking an annotation
  | TTuple [Type]                     -- ^ Tuple type, e.g., (a, b). Unit type () is TTuple []
  | TCollection Type                  -- ^ Collection type, e.g., [a]
  | TInductive String [Type]          -- ^ Inductive data type with type arguments
  | TTensor Type                      -- ^ Tensor type (only element type is kept). Vector and Matrix are aliases for Tensor
  | THash Type Type                   -- ^ Hash map type
  | TMatcher Capability Type          -- ^ Matcher type, e.g., Matcher (List p) [a]
  | TFun Type Type                    -- ^ Function type, e.g., a -> b
  | TIO Type                          -- ^ IO type (for IO actions)
  | TIORef Type                       -- ^ IORef type
  | TPort                             -- ^ Port type (file handles)
  | TAny                              -- ^ Any type (for gradual typing)
  -- New CAS types (Phase 2)
  | TFactor                           -- ^ Factor type (atomic mathematical factor from quote ')
  | TTerm Type SymbolSet               -- ^ Term type, e.g., Term Integer [x] = monomials over Integer with atom set [x]
  | TFrac Type                         -- ^ Frac type, e.g., Frac Integer = rationals
  | TPoly Type SymbolSet              -- ^ Poly type, e.g., Poly Integer [x, y] or Poly Integer [..]
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | Type scheme with separately quantified capability and type variables.
--
-- The two binder lists are intentionally distinct: instantiating a target
-- type variable must never instantiate or refine a capability variable.
data TypeScheme = Forall [CapVar] [TyVar] [Constraint] Type
  deriving (Eq, Show, Generic)

-- | The two independently substituted components of a pattern type.
--
-- This is the production counterpart of @TypePM.Dual@: the capability is in
-- the capability sort, while the target is an ordinary Egison type (which may
-- itself contain matcher capabilities).
data Dual = Dual
  { dualCapability :: Capability
  , dualTarget     :: Type
  } deriving (Eq, Show, Generic, Hashable)

-- | A pattern-function type quantified in both solver sorts.
--
-- All argument duals and the result dual are instantiated together.  Keeping
-- them in one scheme preserves capability/target correlations across one
-- pattern-function application.
data DualScheme = DualScheme
  { dualCapBinders :: [CapVar]
  , dualTyBinders  :: [TyVar]
  , dualArgs       :: [Dual]
  , dualResult     :: Dual
  } deriving (Eq, Show, Generic, Hashable)

-- | Capability variables occurring in both components of a dual.
freeCapVarsDual :: Dual -> Set CapVar
freeCapVarsDual (Dual capability target) =
  freeCapVarsCapability capability `Set.union` freeCapVars target

-- | Ordinary variables occur only in the target component.
freeTyVarsDual :: Dual -> Set TyVar
freeTyVarsDual = freeTyVars . dualTarget

-- | Free capability variables of a dual scheme, excluding its binders.
freeCapVarsDualScheme :: DualScheme -> Set CapVar
freeCapVarsDualScheme scheme =
  let variables =
        Set.unions
          (map freeCapVarsDual (dualResult scheme : dualArgs scheme))
  in variables `Set.difference` Set.fromList (dualCapBinders scheme)

-- | Free ordinary variables of a dual scheme, excluding its binders.
freeTyVarsDualScheme :: DualScheme -> Set TyVar
freeTyVarsDualScheme scheme =
  let variables =
        Set.unions
          (map freeTyVarsDual (dualResult scheme : dualArgs scheme))
  in variables `Set.difference` Set.fromList (dualTyBinders scheme)

-- | Erase pattern capabilities and expose the ordinary function type used by
-- expression elaboration.  This is a projection of the canonical dual scheme,
-- never an independently generalized signature.
dualSchemeTargetType :: DualScheme -> Type
dualSchemeTargetType scheme =
  foldr TFun
    (dualTarget (dualResult scheme))
    (map dualTarget (dualArgs scheme))

-- | Quantified ordinary projection of a dual scheme.
dualSchemeTargetScheme :: DualScheme -> TypeScheme
dualSchemeTargetScheme scheme =
  Forall
    (dualCapBinders scheme)
    (dualTyBinders scheme)
    []
    (dualSchemeTargetType scheme)

-- | Type class constraint. May carry multiple type arguments for
-- multi-param classes (e.g. `Coerce a b` → `Constraint "Coerce" [a, b]`).
-- Single-param classes use a one-element list.
data Constraint = Constraint
  { constraintClass :: String  -- ^ Class name, e.g., "Eq"
  , constraintTypes :: [Type]  -- ^ Type arguments. For single-param classes,
                                -- a singleton list `[t]`. For multi-param, all
                                -- class type parameters in declaration order.
  } deriving (Eq, Show, Generic)

-- | Accessor for the principal (first) constraint type.
-- For single-param classes this is the only type. Multi-param call sites
-- that need the full type list should use `constraintTypes` directly.
constraintType :: Constraint -> Type
constraintType c = case constraintTypes c of
  (t:_) -> t
  []    -> error "constraintType: constraint with no types"

-- | Information about a type class
data ClassInfo = ClassInfo
  { classSupers  :: [String]           -- ^ Superclass names
  , classParams  :: [TyVar]            -- ^ Type parameters (e.g. ['a'] in "class Eq a"; ['a','b'] in "class Embed a b")
  , classMethods :: [(String, Type)]   -- ^ Method names and their types
  } deriving (Eq, Show, Generic)

-- | Backward-compatible accessor for the first (or only) class parameter.
-- Many existing call sites assume a single-parameter class; multi-param classes
-- (Phase 5.5 Embed) need to call `classParams` directly.
classParam :: ClassInfo -> TyVar
classParam ci = case classParams ci of
  (p:_) -> p
  []    -> error "classParam: class with no type parameters"

-- | Information about a type class instance
data InstanceInfo = InstanceInfo
  { instContext :: [Constraint]        -- ^ Instance context (e.g., "Eq a" in "{Eq a} Eq [a]")
  , instClass   :: String              -- ^ Class name
  , instTypes   :: [Type]              -- ^ Instance types (e.g. [Integer] or [Integer, Frac Integer] for multi-param)
  , instMethods :: [(String, ())]      -- ^ Method implementations (placeholder for now)
  } deriving (Eq, Show, Generic)

-- | Backward-compatible accessor for the first (or only) instance type.
instType :: InstanceInfo -> Type
instType ii = case instTypes ii of
  (t:_) -> t
  []    -> error "instType: instance with no types"

-- | Generate a fresh type variable with a given prefix
freshTyVar :: String -> Int -> TyVar
freshTyVar prefix n = TyVar (prefix ++ show n)

-- | Generate a fresh variable for a quantified binder.
freshTyVarLike :: TyVar -> String -> Int -> TyVar
freshTyVarLike _ prefix n = TyVar (prefix ++ show n)

-- | Generate a fresh capability variable with a given prefix.
freshCapVar :: String -> Int -> CapVar
freshCapVar prefix n = MkCapVar (prefix ++ show n)

-- | Get free type variables from a type
freeTyVars :: Type -> Set TyVar
freeTyVars TInt             = Set.empty
freeTyVars TMathValue        = Set.empty
freeTyVars TPolyExpr        = Set.empty
freeTyVars TTermExpr        = Set.empty
freeTyVars TSymbolExpr      = Set.empty
freeTyVars TIndexExpr       = Set.empty
freeTyVars TFloat           = Set.empty
freeTyVars TBool            = Set.empty
freeTyVars TChar            = Set.empty
freeTyVars TString          = Set.empty
freeTyVars (TVar v)         = Set.singleton v
freeTyVars (TSkolem _)      = Set.empty
freeTyVars (TTuple ts)      = Set.unions (map freeTyVars ts)
freeTyVars (TCollection t)  = freeTyVars t
freeTyVars (TInductive _ ts) = Set.unions (map freeTyVars ts)
freeTyVars (TTensor t)      = freeTyVars t
freeTyVars (THash k v)      = freeTyVars k `Set.union` freeTyVars v
freeTyVars (TMatcher _ t)     = freeTyVars t
freeTyVars (TFun t1 t2)     = freeTyVars t1 `Set.union` freeTyVars t2
freeTyVars (TIO t)          = freeTyVars t
freeTyVars (TIORef t)       = freeTyVars t
freeTyVars TPort            = Set.empty
freeTyVars TAny             = Set.empty
-- New CAS types
freeTyVars TFactor          = Set.empty
freeTyVars (TTerm t ss)      = freeTyVars t `Set.union` freeTyVarsSymbolSet ss
freeTyVars (TFrac t)         = freeTyVars t
freeTyVars (TPoly t ss)     = freeTyVars t `Set.union` freeTyVarsSymbolSet ss

-- | Get rigid ordinary type skolems from a type.
--
-- Skolems are deliberately separate from 'freeTyVars': they are constants
-- during annotation checking and must never enter an ordinary substitution
-- or be generalized.  The checker uses this traversal for the no-escape
-- boundary before it deskolemizes a successful typed tree.
freeTySkolems :: Type -> Set TyVar
freeTySkolems TInt               = Set.empty
freeTySkolems TMathValue         = Set.empty
freeTySkolems TPolyExpr          = Set.empty
freeTySkolems TTermExpr          = Set.empty
freeTySkolems TSymbolExpr        = Set.empty
freeTySkolems TIndexExpr         = Set.empty
freeTySkolems TFloat             = Set.empty
freeTySkolems TBool              = Set.empty
freeTySkolems TChar              = Set.empty
freeTySkolems TString            = Set.empty
freeTySkolems (TVar _)           = Set.empty
freeTySkolems (TSkolem v)        = Set.singleton v
freeTySkolems (TTuple ts)        = Set.unions (map freeTySkolems ts)
freeTySkolems (TCollection t)    = freeTySkolems t
freeTySkolems (TInductive _ ts)  = Set.unions (map freeTySkolems ts)
freeTySkolems (TTensor t)        = freeTySkolems t
freeTySkolems (THash k v)        = freeTySkolems k `Set.union` freeTySkolems v
freeTySkolems (TMatcher _ t)     = freeTySkolems t
freeTySkolems (TFun t1 t2)       = freeTySkolems t1 `Set.union` freeTySkolems t2
freeTySkolems (TIO t)            = freeTySkolems t
freeTySkolems (TIORef t)         = freeTySkolems t
freeTySkolems TPort              = Set.empty
freeTySkolems TAny               = Set.empty
freeTySkolems TFactor            = Set.empty
freeTySkolems (TTerm t _)        = freeTySkolems t
freeTySkolems (TFrac t)          = freeTySkolems t
freeTySkolems (TPoly t _)        = freeTySkolems t

-- | Free type variables in a SymbolSet (used by both Term and Poly).
freeTyVarsSymbolSet :: SymbolSet -> Set TyVar
freeTyVarsSymbolSet (SymbolSetClosed _) = Set.empty
freeTyVarsSymbolSet SymbolSetOpen       = Set.empty
freeTyVarsSymbolSet (SymbolSetVar v)    = Set.singleton v

-- | Get free flexible capability variables from a capability.
--
-- Rigid skolems are constants, not free variables, and are therefore
-- deliberately excluded.
freeCapVarsCapability :: Capability -> Set CapVar
freeCapVarsCapability CapAny         = Set.empty
freeCapVarsCapability (CapVar v)      = Set.singleton v
freeCapVarsCapability (CapSkolem _)   = Set.empty
freeCapVarsCapability (CapCon _ caps) = Set.unions (map freeCapVarsCapability caps)
freeCapVarsCapability (CapTuple caps) = Set.unions (map freeCapVarsCapability caps)

-- | Get free capability variables from every matcher occurrence in a type.
freeCapVars :: Type -> Set CapVar
freeCapVars TInt               = Set.empty
freeCapVars TMathValue         = Set.empty
freeCapVars TPolyExpr          = Set.empty
freeCapVars TTermExpr          = Set.empty
freeCapVars TSymbolExpr        = Set.empty
freeCapVars TIndexExpr         = Set.empty
freeCapVars TFloat             = Set.empty
freeCapVars TBool              = Set.empty
freeCapVars TChar              = Set.empty
freeCapVars TString            = Set.empty
freeCapVars (TVar _)           = Set.empty
freeCapVars (TSkolem _)        = Set.empty
freeCapVars (TTuple ts)        = Set.unions (map freeCapVars ts)
freeCapVars (TCollection t)    = freeCapVars t
freeCapVars (TInductive _ ts)  = Set.unions (map freeCapVars ts)
freeCapVars (TTensor t)        = freeCapVars t
freeCapVars (THash k v)        = freeCapVars k `Set.union` freeCapVars v
freeCapVars (TMatcher cap t) =
  freeCapVarsCapability cap `Set.union` freeCapVars t
freeCapVars (TFun t1 t2)       = freeCapVars t1 `Set.union` freeCapVars t2
freeCapVars (TIO t)            = freeCapVars t
freeCapVars (TIORef t)         = freeCapVars t
freeCapVars TPort              = Set.empty
freeCapVars TAny               = Set.empty
freeCapVars TFactor            = Set.empty
freeCapVars (TTerm t _)        = freeCapVars t
freeCapVars (TFrac t)          = freeCapVars t
freeCapVars (TPoly t _)        = freeCapVars t

-- | Get rigid capability skolems from a capability.
freeCapSkolemsCapability :: Capability -> Set CapVar
freeCapSkolemsCapability CapAny         = Set.empty
freeCapSkolemsCapability (CapVar _)      = Set.empty
freeCapSkolemsCapability (CapSkolem v)   = Set.singleton v
freeCapSkolemsCapability (CapCon _ caps) =
  Set.unions (map freeCapSkolemsCapability caps)
freeCapSkolemsCapability (CapTuple caps) =
  Set.unions (map freeCapSkolemsCapability caps)

-- | Get rigid capability skolems from every matcher occurrence in a type.
freeCapSkolems :: Type -> Set CapVar
freeCapSkolems TInt               = Set.empty
freeCapSkolems TMathValue         = Set.empty
freeCapSkolems TPolyExpr          = Set.empty
freeCapSkolems TTermExpr          = Set.empty
freeCapSkolems TSymbolExpr        = Set.empty
freeCapSkolems TIndexExpr         = Set.empty
freeCapSkolems TFloat             = Set.empty
freeCapSkolems TBool              = Set.empty
freeCapSkolems TChar              = Set.empty
freeCapSkolems TString            = Set.empty
freeCapSkolems (TVar _)           = Set.empty
freeCapSkolems (TSkolem _)        = Set.empty
freeCapSkolems (TTuple ts)        = Set.unions (map freeCapSkolems ts)
freeCapSkolems (TCollection t)    = freeCapSkolems t
freeCapSkolems (TInductive _ ts)  = Set.unions (map freeCapSkolems ts)
freeCapSkolems (TTensor t)        = freeCapSkolems t
freeCapSkolems (THash k v)        = freeCapSkolems k `Set.union` freeCapSkolems v
freeCapSkolems (TMatcher cap t) =
  freeCapSkolemsCapability cap `Set.union` freeCapSkolems t
freeCapSkolems (TFun t1 t2)       = freeCapSkolems t1 `Set.union` freeCapSkolems t2
freeCapSkolems (TIO t)            = freeCapSkolems t
freeCapSkolems (TIORef t)         = freeCapSkolems t
freeCapSkolems TPort              = Set.empty
freeCapSkolems TAny               = Set.empty
freeCapSkolems TFactor            = Set.empty
freeCapSkolems (TTerm t _)        = freeCapSkolems t
freeCapSkolems (TFrac t)          = freeCapSkolems t
freeCapSkolems (TPoly t _)        = freeCapSkolems t

-- | Bottom-up transformation of a capability.
mapCapability :: (Capability -> Capability) -> Capability -> Capability
mapCapability f = go
  where
    go cap = f (descend cap)
    descend (CapCon former caps) = CapCon former (map go caps)
    descend (CapTuple caps)      = CapTuple (map go caps)
    descend leaf                 = leaf

-- | Bottom-up transformation of all capability occurrences in a type.
--
-- Ordinary type nodes and type variables are retained verbatim.  The walker
-- still descends through them so nested matcher types are reached.
mapTypeCapabilities :: (Capability -> Capability) -> Type -> Type
mapTypeCapabilities f = go
  where
    go (TTuple ts)          = TTuple (map go ts)
    go (TCollection t)      = TCollection (go t)
    go (TInductive n ts)    = TInductive n (map go ts)
    go (TTensor t)          = TTensor (go t)
    go (THash k v)          = THash (go k) (go v)
    go (TMatcher cap t)     = TMatcher (mapCapability f cap) (go t)
    go (TFun a b)           = TFun (go a) (go b)
    go (TIO t)              = TIO (go t)
    go (TIORef t)           = TIORef (go t)
    go (TFrac t)            = TFrac (go t)
    go (TTerm t ss)         = TTerm (go t) ss
    go (TPoly t ss)         = TPoly (go t) ss
    go leaf                 = leaf

-- | Substitute one flexible capability variable.
--
-- Skolems are rigid and are never replaced.
substCapVar :: CapVar -> Capability -> Capability -> Capability
substCapVar old new = mapCapability replace
  where
    replace (CapVar v) | v == old = new
    replace cap                    = cap

-- | Substitute one capability variable throughout the matcher occurrences of
-- a type, without changing any ordinary type variable.
substCapVarInType :: CapVar -> Capability -> Type -> Type
substCapVarInType old new = mapTypeCapabilities replace
  where
    replace (CapVar v) | v == old = new
    replace cap                    = cap

-- | Bottom-up transformation of a type: rebuild every composite node from
-- its transformed children, then apply @f@ to the rebuilt node (so @f@ sees
-- leaves as-is and composite nodes with already-transformed children).
-- Nodes produced by @f@ are not re-visited. Symbol sets of Poly/Term are
-- left untouched; a transformation that needs to rewrite them can do so in
-- its @f@ at the TPoly/TTerm node.
--
-- This is the single recursion used by the type-variable substitution
-- walkers (Env.instantiate, EnvBuilder.substituteTypeVar,
-- TypeClassExpand.applySubstsToType), which only differ in their leaf
-- function.
mapType :: (Type -> Type) -> Type -> Type
mapType f = go
  where
    go t = f (descend t)
    descend (TTuple ts)        = TTuple (map go ts)
    descend (TCollection t)    = TCollection (go t)
    descend (TInductive n ts)  = TInductive n (map go ts)
    descend (TTensor t)        = TTensor (go t)
    descend (THash k v)        = THash (go k) (go v)
    descend (TMatcher cap t)       = TMatcher cap (go t)
    descend (TFun a b)         = TFun (go a) (go b)
    descend (TIO t)            = TIO (go t)
    descend (TIORef t)         = TIORef (go t)
    descend (TFrac t)          = TFrac (go t)
    descend (TTerm t ss)       = TTerm (go t) ss
    descend (TPoly t ss)       = TPoly (go t) ss
    descend leaf               = leaf

-- | Substitute a single type variable, leaving symbol sets untouched.
-- Shared leaf function for the substitution walkers built on 'mapType'.
substTyVar :: TyVar -> Type -> Type -> Type
substTyVar old new = mapType replace
  where
    replace (TVar v) | v == old = new
    replace t                   = t

-- | Check if a type is a tensor type
isTensorType :: Type -> Bool
isTensorType (TTensor _) = True
isTensorType _           = False

-- | Check if a type is a scalar (non-tensor) type
isScalarType :: Type -> Bool
isScalarType = not . isTensorType

-- | Check if a type is a CAS type (Factor, Term, Frac, or Poly)
isCASType :: Type -> Bool
isCASType TFactor     = True
isCASType (TTerm _ _) = True
isCASType (TFrac _)    = True
isCASType (TPoly _ _) = True
isCASType _           = False

-- | Check if one symbol set is a subset of another
-- Used for coercive subtyping: Poly a [x] can be embedded into Poly a [x, y]
isSubsetSymbolSet :: SymbolSet -> SymbolSet -> Bool
-- Open is a superset of everything
isSubsetSymbolSet _ SymbolSetOpen = True
-- Open is only subset of itself
isSubsetSymbolSet SymbolSetOpen _ = False
-- Closed is subset if all elements are contained
isSubsetSymbolSet (SymbolSetClosed s1) (SymbolSetClosed s2) =
  all (`elem` s2) s1
-- Variables require unification
isSubsetSymbolSet (SymbolSetVar _) _ = False
isSubsetSymbolSet _ (SymbolSetVar _) = False

-- | Restriction on open atom sets: a nested Poly tower — the chain of
-- Poly/Term coefficient nesting, descending through Frac — may contain at
-- most one open symbol set @[..]@. With a single open slot every atom's
-- destination level is uniquely determined (closed sets route their atoms,
-- the open slot takes the rest), whereas a second open slot would make the
-- routing ambiguous, so reshape could not be defined. Components outside a
-- tower (tuple fields, function arguments, and so on) are separate towers,
-- each allowed its own @[..]@. Returns True when some tower in the type
-- violates the restriction.
hasAmbiguousOpenTower :: Type -> Bool
hasAmbiguousOpenTower ty = case ty of
  TPoly {} -> towerViolation ty
  TTerm {} -> towerViolation ty
  TFrac {} -> towerViolation ty
  _        -> anyComponent ty
  where
    towerViolation t =
      let (opens, base) = walk t (0 :: Int)
      in opens >= 2 || hasAmbiguousOpenTower base
    walk (TPoly inner ss) n = walk inner (n + openCount ss)
    walk (TTerm inner ss) n = walk inner (n + openCount ss)
    walk (TFrac inner)    n = walk inner n
    walk base             n = (n, base)
    openCount SymbolSetOpen = 1
    openCount _             = 0
    anyComponent t = case t of
      TTuple ts        -> any hasAmbiguousOpenTower ts
      TCollection t1   -> hasAmbiguousOpenTower t1
      TInductive _ ts  -> any hasAmbiguousOpenTower ts
      TTensor t1       -> hasAmbiguousOpenTower t1
      THash k v        -> hasAmbiguousOpenTower k || hasAmbiguousOpenTower v
      TMatcher _ t1      -> hasAmbiguousOpenTower t1
      TFun a b         -> hasAmbiguousOpenTower a || hasAmbiguousOpenTower b
      TIO t1           -> hasAmbiguousOpenTower t1
      TIORef t1        -> hasAmbiguousOpenTower t1
      _                -> False

-- | Convert a Type to a string name for dictionary and method naming
-- This is used for generating instance dictionary names and method names
-- E.g., TInt -> "Integer", TTensor TInt -> "TensorInteger"
-- | Render a type into a flat name slug used for instance-dictionary names.
-- Recursively includes inner type parameters so that e.g.
-- `Frac (Poly Integer [..])` and `Frac Integer` produce distinct names.
-- Type variables are dropped (rendered as ""), so polymorphic instances like
-- `instance Eq [a]` yield `eqCollection` rather than `eqCollectiona`.
-- Note: Integer and MathValue produce DIFFERENT names so that
-- `instance Coerce Integer Integer` and `instance Coerce MathValue MathValue`
-- are distinct (subtyping Integer ⊂ MathValue is handled separately).
typeToName :: Type -> String
typeToName TInt = "Integer"
typeToName TMathValue = "MathValue"
typeToName TFloat = "Float"
typeToName TBool = "Bool"
typeToName TChar = "Char"
typeToName TString = "String"
typeToName (TVar _) = ""  -- type variables omitted from dict names
typeToName (TSkolem _) = ""  -- annotation skolems are abstract variables
typeToName (TInductive name _) = name
typeToName (TCollection t) = "Collection" ++ typeToName t
typeToName (TTuple ts) = "Tuple" ++ concatMap typeToName ts
typeToName (TTensor t) = "Tensor" ++ typeToName t
typeToName TFactor = "Factor"
typeToName (TTerm t ss) = "Term" ++ typeToName t ++ symbolSetToName ss
typeToName (TFrac t) = "Frac" ++ typeToName t
typeToName (TPoly t ss) = "Poly" ++ typeToName t ++ symbolSetToName ss
typeToName _ = "Unknown"

-- | Render a SymbolSet into a flat name slug for dictionary lookup.
-- E.g. [sqrt 2, x] -> "_sqrt_2_x", [..] -> "_Open".
symbolSetToName :: SymbolSet -> String
symbolSetToName (SymbolSetClosed syms) =
  concatMap (\a -> '_' : sanitize (prettyTypeAtomValue a)) syms
  where
    sanitize = map (\c -> if c == ' ' || c == '(' || c == ')' then '_' else c)
symbolSetToName SymbolSetOpen = "_Open"
symbolSetToName (SymbolSetVar v) = "_" ++ tyVarName v

-- | Get the type constructor name only, without type parameters
-- Used for generating instance dictionary names (e.g., "eqCollection" not "eqCollectiona")
typeConstructorName :: Type -> String
-- Note: TInt is normalized to "MathValue" because Integer = MathValue in Egison
-- and all type class instances are defined for MathValue, not Integer
typeConstructorName TInt = "MathValue"  -- Integer = MathValue, use MathValue for dictionary names
typeConstructorName TMathValue = "MathValue"
typeConstructorName TPolyExpr = "PolyExpr"
typeConstructorName TTermExpr = "TermExpr"
typeConstructorName TSymbolExpr = "SymbolExpr"
typeConstructorName TIndexExpr = "IndexExpr"
typeConstructorName TFloat = "Float"
typeConstructorName TBool = "Bool"
typeConstructorName TChar = "Char"
typeConstructorName TString = "String"
typeConstructorName (TVar _) = ""  -- Type variables are ignored
typeConstructorName (TSkolem _) = ""  -- Annotation skolems are ignored
typeConstructorName (TInductive name _) = name  -- Type arguments are ignored
typeConstructorName (TCollection _) = "Collection"  -- Element type is ignored
typeConstructorName (TTuple _) = "Tuple"
typeConstructorName (TTensor _) = "Tensor"
typeConstructorName (THash _ _) = "Hash"
typeConstructorName (TMatcher _ _) = "Matcher"
typeConstructorName (TFun _ _) = "Fun"
typeConstructorName (TIO _) = "IO"
typeConstructorName (TIORef _) = "IORef"
typeConstructorName TPort = "Port"
typeConstructorName TAny = "Any"
-- New CAS types
typeConstructorName TFactor = "Factor"
typeConstructorName (TTerm _ _) = "Term"
typeConstructorName (TFrac _) = "Frac"
typeConstructorName (TPoly _ _) = "Poly"

-- | Sanitize method names for use in identifiers
-- Converts operator symbols to alphanumeric names
-- E.g., "==" -> "eq", "+" -> "plus"
sanitizeMethodName :: String -> String
sanitizeMethodName "==" = "eq"
sanitizeMethodName "/=" = "neq"
sanitizeMethodName "<"  = "lt"
sanitizeMethodName "<=" = "le"
sanitizeMethodName ">"  = "gt"
sanitizeMethodName ">=" = "ge"
sanitizeMethodName "+"  = "plus"
sanitizeMethodName "-"  = "minus"
sanitizeMethodName "*"  = "times"
sanitizeMethodName "/"  = "div"
sanitizeMethodName name = name

-- | Construct a canonical type former from a surface spelling and arity.
--
-- This is the deliberately small D5-core allowlist: it removes only fixed
-- surface synonyms.  It does not consult aliases, CAS equivalence, subtyping,
-- tensor normalization, or type-class instances.
mkTypeFormer :: String -> Int -> TypeFormer
mkTypeFormer surfaceName arity =
  TypeFormer (TypeFormerId (canonicalName surfaceName)) arity
  where
    canonicalName "List"     = "Collection"
    canonicalName "Vector"   = "Tensor"
    canonicalName "Matrix"   = "Tensor"
    canonicalName "DiffForm" = "Tensor"
    canonicalName name       = name

-- | Decompose a canonical core type former and its ordinary type arguments.
--
-- Tuple products are represented by 'CapTuple' and therefore return
-- 'Nothing'.  Function, effect, matcher, and gradual types are opaque
-- barriers.  CAS nodes are exposed only as raw canonical heads here; callers
-- constructing certified CAS capabilities must additionally use the
-- target-indexed virtual pattern signatures required by D5-CAS.
typeFormerOf :: Type -> Maybe (TypeFormer, [Type])
typeFormerOf TInt              = Just (mkTypeFormer "Integer" 0, [])
typeFormerOf TMathValue        = Just (mkTypeFormer "MathValue" 0, [])
typeFormerOf TPolyExpr         = Just (mkTypeFormer "PolyExpr" 0, [])
typeFormerOf TTermExpr         = Just (mkTypeFormer "TermExpr" 0, [])
typeFormerOf TSymbolExpr       = Just (mkTypeFormer "SymbolExpr" 0, [])
typeFormerOf TIndexExpr        = Just (mkTypeFormer "IndexExpr" 0, [])
typeFormerOf TFloat            = Just (mkTypeFormer "Float" 0, [])
typeFormerOf TBool             = Just (mkTypeFormer "Bool" 0, [])
typeFormerOf TChar             = Just (mkTypeFormer "Char" 0, [])
typeFormerOf TString           = Just (mkTypeFormer "String" 0, [])
typeFormerOf (TCollection t)   = Just (mkTypeFormer "Collection" 1, [t])
typeFormerOf (TInductive n ts) = Just (mkTypeFormer n (length ts), ts)
typeFormerOf (TTensor t)       = Just (mkTypeFormer "Tensor" 1, [t])
typeFormerOf (THash k v)       = Just (mkTypeFormer "Hash" 2, [k, v])
typeFormerOf TFactor           = Just (mkTypeFormer "Factor" 0, [])
typeFormerOf (TTerm t _)       = Just (mkTypeFormer "Term" 1, [t])
typeFormerOf (TFrac t)         = Just (mkTypeFormer "Frac" 1, [t])
typeFormerOf (TPoly t _)       = Just (mkTypeFormer "Poly" 1, [t])
typeFormerOf _                 = Nothing

-- | Build a structural capability template from a core result type.
--
-- The caller supplies the mapping for ordinary type variables and the set of
-- type constructors with declared pattern constructors, making the boundary
-- between the two sorts explicit.  The helper is conservative:
-- effectful, function, matcher, gradual, and CAS-view types are opaque and
-- return 'Nothing'.  In particular it never invokes ordinary type equality or
-- CAS ground equivalence.  Its input must already be a matcher-independent
-- constructor-derived type, not an ordinary target specialization.
-- Consequently a closed argument such as @Atom@ in @Collection Atom@ is retained:
-- it is evidence contributed by a nested constructor pattern, not structure
-- manufactured by target specialization.
capabilitySkeleton
  :: (TyVar -> Capability) -> (TypeFormer -> Bool) -> Type -> Maybe Capability
capabilitySkeleton onVar declared = go
  where
    go (TVar v)         = Just (onVar v)
    -- A target annotation is a consumer constraint, never producer evidence.
    -- In particular its rigid ordinary skolem must not be converted into a
    -- capability witness.
    go (TSkolem _)      = Nothing
    -- Tuple components are structural roots in their own right.  Unlike a
    -- type-former argument, a closed component such as `Ordering` must retain
    -- its constructor head; otherwise `(less, less)` would incorrectly ask
    -- for `(Any, Any)`.
    go (TTuple ts)      = CapTuple <$> mapM go ts
    go TFactor          = Nothing
    go TTerm {}         = Nothing
    go TFrac {}         = Nothing
    go TPoly {}         = Nothing
    -- A declared pattern type projects to its own capability constructor
    -- applied to the projections of its parameters.  A type without declared
    -- pattern constructors admits no constructor pattern, so every matcher for
    -- it has capability Any and a field of that type demands exactly Any.
    go ty = do
      (former, args) <- typeFormerOf ty
      if declared former
        then CapCon former <$> mapM go args
        else Just CapAny

-- | Convert a source capability expression to the internal capability sort.
--
-- Name/kind elaboration is responsible for distinguishing variables from
-- former names before this conversion.  Surface aliases are intentionally not
-- accepted here; 'mkTypeFormer' only erases the fixed core synonyms.
capExprToCapability :: CapabilityExpr -> Capability
capExprToCapability CEAny = CapAny
capExprToCapability (CEVar name) = CapVar (MkCapVar name)
capExprToCapability (CECon name args) =
  let caps = map capExprToCapability args
  in CapCon (mkTypeFormer name (length caps)) caps
capExprToCapability (CEList cap) =
  CapCon (mkTypeFormer "Collection" 1) [capExprToCapability cap]
capExprToCapability (CETuple caps) =
  CapTuple (map capExprToCapability caps)

-- | Convert TypeExpr (from AST) to Type (internal representation)
typeExprToType :: TypeExpr -> Type
typeExprToType TEInt = TInt
typeExprToType TEMathValue = TMathValue  -- MathValue is a primitive type
typeExprToType TEFloat = TFloat
typeExprToType TEBool = TBool
typeExprToType TEChar = TChar
typeExprToType TEString = TString
typeExprToType (TEVar "Port") = TPort
typeExprToType (TEVar name) = TVar (TyVar name)
typeExprToType (TETuple ts) = TTuple (map typeExprToType ts)
typeExprToType (TEList t) = TCollection (typeExprToType t)
typeExprToType (TEApp t1 ts) = 
  case typeExprToType t1 of
    TVar variable ->
      let name = tyVarName variable
      in
      -- Special case: convert inductive type names to primitive types
      case (name, ts) of
        ("MathValue", [])   -> TMathValue
        ("PolyExpr", [])   -> TPolyExpr
        ("TermExpr", [])   -> TTermExpr
        ("SymbolExpr", []) -> TSymbolExpr
        ("IndexExpr", [])  -> TIndexExpr
        ("Port", [])       -> TPort
        _                  -> TInductive name (map typeExprToType ts)
    TInductive name existingTs -> TInductive name (existingTs ++ map typeExprToType ts)
    baseType -> baseType  -- Can't apply to non-inductive types
typeExprToType (TETensor elemT) = TTensor (typeExprToType elemT)
typeExprToType (TEVector elemT) = TTensor (typeExprToType elemT)  -- Vector is an alias for Tensor
typeExprToType (TEMatrix elemT) = TTensor (typeExprToType elemT)  -- Matrix is an alias for Tensor
typeExprToType (TEDiffForm elemT) = TTensor (typeExprToType elemT)  -- DiffForm is an alias for Tensor
typeExprToType (TEMatcher cap t) =
  TMatcher (capExprToCapability cap) (typeExprToType t)
typeExprToType (TEFun t1 t2) = TFun (typeExprToType t1) (typeExprToType t2)
typeExprToType (TEIO t) = TIO (typeExprToType t)
typeExprToType (TEConstrained _ t) = typeExprToType t  -- Ignore constraints
typeExprToType (TEPattern t) = TInductive "Pattern" [typeExprToType t]
-- New CAS types
typeExprToType TEFactor = TFactor
typeExprToType (TETerm t ss) = TTerm (typeExprToType t) (symbolSetExprToSymbolSet ss)
typeExprToType (TEFrac t) = TFrac (typeExprToType t)
typeExprToType (TEPoly t ss) = TPoly (typeExprToType t) (symbolSetExprToSymbolSet ss)

-- | Convert AST-level SymbolSetExpr to internal SymbolSet.
symbolSetExprToSymbolSet :: SymbolSetExpr -> SymbolSet
symbolSetExprToSymbolSet (SSEClosed atoms) =
  SymbolSetClosed (map typeAtomExprToTypeAtom atoms)
symbolSetExprToSymbolSet SSEOpen = SymbolSetOpen

-- | Builtin type names that user-facing CAS declarations (`declare
-- cas-type` aliases, `declare cas-quotient` types) must not shadow.
-- Single source of truth for the name-clash validations.
reservedCasTypeNames :: Set String
reservedCasTypeNames = Set.fromList
  [ "Integer", "MathValue", "Float", "Bool", "Char", "String"
  , "Factor", "Term", "Frac", "Poly", "Tensor", "Vector", "Matrix"
  , "DiffForm", "Matcher", "Pattern", "IO", "Symbol"
  , "PolyExpr", "TermExpr", "SymbolExpr", "IndexExpr" ]

-- | Canonical matcher--tuple normalization: a matcher whose capability and
-- target are both tuples of the same arity is the tuple of the component
-- matchers (the definitional equality of the paper).  Applied bottom-up
-- after every substitution.
normalizeMatcherProducts :: Type -> Type
normalizeMatcherProducts = go
  where
    go ty = case ty of
      TMatcher cap target ->
        case (cap, go target) of
          (CapTuple caps, TTuple targets)
            | length caps == length targets, length caps >= 2 ->
                TTuple (zipWith (\c t -> go (TMatcher c t)) caps targets)
          (_, target') -> TMatcher cap target'
      TTuple ts -> TTuple (map go ts)
      TCollection t -> TCollection (go t)
      TInductive n ts -> TInductive n (map go ts)
      TTensor t -> TTensor (go t)
      THash k v -> THash (go k) (go v)
      TFun a b -> TFun (go a) (go b)
      TIO t -> TIO (go t)
      TIORef t -> TIORef (go t)
      TTerm t ss -> TTerm (go t) ss
      TFrac t -> TFrac (go t)
      TPoly t ss -> TPoly (go t) ss
      leaf -> leaf

-- | Expand `declare cas-type` transparent aliases inside a Type (Phase alpha
-- of the extensible CAS tower; see
-- design/type-cas-tower-implementation.md section 2).
-- A capitalized name in type position parses to `TVar (TyVar name)` (or is
-- concretized to `TInductive name []`), so we substitute both forms.
-- Alias bodies stored in the map are already fully expanded, hence a single
-- substitution pass suffices; we do not recurse into substituted bodies.
expandTypeAliases :: HashMap String Type -> Type -> Type
expandTypeAliases aliases ty
  | HashMap.null aliases = ty
  | otherwise = go ty
  where
    go t@(TVar variable)    = HashMap.lookupDefault t (tyVarName variable) aliases
    go t@(TInductive n []) = HashMap.lookupDefault t n aliases
    go (TInductive n ts)   = TInductive n (map go ts)
    go (TTuple ts)         = TTuple (map go ts)
    go (TCollection t)     = TCollection (go t)
    go (TTensor t)         = TTensor (go t)
    go (THash k v)         = THash (go k) (go v)
    go (TMatcher cap t)       = TMatcher cap (go t)
    go (TFun a b)          = TFun (go a) (go b)
    go (TIO t)             = TIO (go t)
    go (TIORef t)          = TIORef (go t)
    go (TTerm t ss)        = TTerm (go t) ss
    go (TFrac t)           = TFrac (go t)
    go (TPoly t ss)        = TPoly (go t) ss
    go t                   = t

-- | Normalize inductive type names to primitive types if applicable
-- This is used to convert TInductive "MathValue" [] to TMathValue, etc.
normalizeInductiveTypes :: Type -> Type
normalizeInductiveTypes (TInductive name []) = case name of
  "MathValue"   -> TMathValue
  "PolyExpr"   -> TPolyExpr
  "TermExpr"   -> TTermExpr
  "SymbolExpr" -> TSymbolExpr
  "IndexExpr"  -> TIndexExpr
  "Port"       -> TPort
  "Factor"     -> TFactor  -- New CAS type
  _            -> TInductive name []
-- Normalize Div to TFrac
normalizeInductiveTypes (TInductive "Frac" [t]) = TFrac (normalizeInductiveTypes t)
normalizeInductiveTypes (TInductive "Term" [t]) = TTerm (normalizeInductiveTypes t) SymbolSetOpen
-- Convert TInductive "Vector", "Matrix", and "DiffForm" to Tensor (they are aliases)
normalizeInductiveTypes (TInductive "Vector" [t]) = TTensor (normalizeInductiveTypes t)
normalizeInductiveTypes (TInductive "Matrix" [t]) = TTensor (normalizeInductiveTypes t)
normalizeInductiveTypes (TInductive "DiffForm" [t]) = TTensor (normalizeInductiveTypes t)
normalizeInductiveTypes (TInductive name ts) = TInductive name (map normalizeInductiveTypes ts)
normalizeInductiveTypes (TTuple ts) = TTuple (map normalizeInductiveTypes ts)
normalizeInductiveTypes (TCollection t) = TCollection (normalizeInductiveTypes t)
normalizeInductiveTypes (THash k v) = THash (normalizeInductiveTypes k) (normalizeInductiveTypes v)
normalizeInductiveTypes (TMatcher cap t) =
  TMatcher cap (normalizeInductiveTypes t)
normalizeInductiveTypes (TFun arg ret) = TFun (normalizeInductiveTypes arg) (normalizeInductiveTypes ret)
normalizeInductiveTypes (TIO t) = TIO (normalizeInductiveTypes t)
normalizeInductiveTypes (TIORef t) = TIORef (normalizeInductiveTypes t)
normalizeInductiveTypes (TTensor t) = TTensor (normalizeInductiveTypes t)
-- New CAS types
normalizeInductiveTypes (TTerm t ss) = TTerm (normalizeInductiveTypes t) ss
normalizeInductiveTypes (TFrac t) = TFrac (normalizeInductiveTypes t)
normalizeInductiveTypes (TPoly t ss) = TPoly (normalizeInductiveTypes t) ss
normalizeInductiveTypes t = t  -- Other types remain unchanged

-- | Capitalize first character
capitalizeFirst :: String -> String
capitalizeFirst []     = []
capitalizeFirst (c:cs) = toUpper c : cs

-- | Lowercase first character
lowerFirst :: String -> String
lowerFirst []     = []
lowerFirst (c:cs) = toLower c : cs
