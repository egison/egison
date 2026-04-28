{- |
Module      : Language.Egison.Type.Types
Licence     : MIT

This module defines the type system for Egison.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Egison.Type.Types
  ( Type(..)
  , SymbolSet(..)
  , TypeAtom(..)
  , prettyTypeAtomValue
  , TypeScheme(..)
  , TyVar(..)
  , TensorShape(..)
  , ShapeDimType(..)
  , Constraint(..)
  , constraintType  -- backward-compat: head of constraintTypes
  , mkConstraint    -- backward-compat: build single-type constraint
  , ClassInfo(..)
  , classParam
  , InstanceInfo(..)
  , instType
  , freshTyVar
  , freeTyVars
  , isTensorType
  , isScalarType
  , isCASType
  , isSubsetSymbolSet
  , typeAtomExprToTypeAtom
  , typeToName
  , typeConstructorName
  , sanitizeMethodName
  , typeExprToType
  , normalizeInductiveTypes
  , capitalizeFirst
  , lowerFirst
  ) where

import           Data.Char        (toLower, toUpper)
import           Data.Hashable    (Hashable)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           GHC.Generics     (Generic)

import           Language.Egison.AST        (TypeExpr(..), SymbolSetExpr(..), TypeAtomExpr(..))
import           Language.Egison.Type.Index ()

-- | Type variable
newtype TyVar = TyVar String
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
  | TPolyExpr                         -- ^ PolyExpr (polynomial expression) - legacy
  | TTermExpr                         -- ^ TermExpr (term in polynomial) - legacy
  | TSymbolExpr                       -- ^ SymbolExpr (symbolic variable) - legacy
  | TIndexExpr                        -- ^ IndexExpr (subscript/superscript index) - legacy
  | TFloat                            -- ^ Float (Double)
  | TBool                             -- ^ Bool
  | TChar                             -- ^ Char
  | TString                           -- ^ String
  | TVar TyVar                        -- ^ Type variable, e.g., a
  | TTuple [Type]                     -- ^ Tuple type, e.g., (a, b). Unit type () is TTuple []
  | TCollection Type                  -- ^ Collection type, e.g., [a]
  | TInductive String [Type]          -- ^ Inductive data type with type arguments
  | TTensor Type                      -- ^ Tensor type (only element type is kept). Vector and Matrix are aliases for Tensor
  | THash Type Type                   -- ^ Hash map type
  | TMatcher Type                     -- ^ Matcher type, e.g., Matcher a
  | TFun Type Type                    -- ^ Function type, e.g., a -> b
  | TIO Type                          -- ^ IO type (for IO actions)
  | TIORef Type                       -- ^ IORef type
  | TPort                             -- ^ Port type (file handles)
  | TAny                              -- ^ Any type (for gradual typing)
  -- New CAS types (Phase 2)
  | TFactor                           -- ^ Factor type (atomic mathematical factor from quote ')
  | TFrac Type                         -- ^ Frac type, e.g., Frac Integer = rationals
  | TPoly Type SymbolSet              -- ^ Poly type, e.g., Poly Integer [x, y] or Poly Integer [..]
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | Type alias: MathValue = Integer in Egison
-- Both names refer to the same type (TInt)
tMathValue :: Type
tMathValue = TInt

-- | Type scheme for polymorphic types (∀a. C a => Type)
-- Includes type constraints for type class support
data TypeScheme = Forall [TyVar] [Constraint] Type
  deriving (Eq, Show, Generic)

-- | Type class constraint. May carry multiple type arguments for
-- multi-param classes (e.g. `Coerce a b` → `Constraint "Coerce" [a, b]`).
-- Single-param classes use a one-element list.
data Constraint = Constraint
  { constraintClass :: String  -- ^ Class name, e.g., "Eq"
  , constraintTypes :: [Type]  -- ^ Type arguments. For single-param classes,
                                -- a singleton list `[t]`. For multi-param, all
                                -- class type parameters in declaration order.
  } deriving (Eq, Show, Generic)

-- | Backward-compat accessor: returns the first (or only) constraint type.
-- Use this where the call site assumes a single-param class.
constraintType :: Constraint -> Type
constraintType c = case constraintTypes c of
  (t:_) -> t
  []    -> error "constraintType: constraint with no types"

-- | Backward-compat smart constructor for single-param constraints.
-- New multi-param call sites should use `Constraint cls [t1, t2, ...]` directly.
mkConstraint :: String -> Type -> Constraint
mkConstraint cls ty = Constraint cls [ty]

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
freeTyVars (TTuple ts)      = Set.unions (map freeTyVars ts)
freeTyVars (TCollection t)  = freeTyVars t
freeTyVars (TInductive _ ts) = Set.unions (map freeTyVars ts)
freeTyVars (TTensor t)      = freeTyVars t
freeTyVars (THash k v)      = freeTyVars k `Set.union` freeTyVars v
freeTyVars (TMatcher t)     = freeTyVars t
freeTyVars (TFun t1 t2)     = freeTyVars t1 `Set.union` freeTyVars t2
freeTyVars (TIO t)          = freeTyVars t
freeTyVars (TIORef t)       = freeTyVars t
freeTyVars TPort            = Set.empty
freeTyVars TAny             = Set.empty
-- New CAS types
freeTyVars TFactor          = Set.empty
freeTyVars (TFrac t)         = freeTyVars t
freeTyVars (TPoly t ss)     = freeTyVars t `Set.union` freeTyVarsSymbolSet ss
  where
    freeTyVarsSymbolSet (SymbolSetClosed _) = Set.empty
    freeTyVarsSymbolSet SymbolSetOpen       = Set.empty
    freeTyVarsSymbolSet (SymbolSetVar v)    = Set.singleton v

-- | Check if a type is a tensor type
isTensorType :: Type -> Bool
isTensorType (TTensor _) = True
isTensorType _           = False

-- | Check if a type is a scalar (non-tensor) type
isScalarType :: Type -> Bool
isScalarType = not . isTensorType

-- | Check if a type is a CAS type (Factor, Div, or Poly)
isCASType :: Type -> Bool
isCASType TFactor     = True
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

-- | Convert a Type to a string name for dictionary and method naming
-- This is used for generating instance dictionary names and method names
-- E.g., TInt -> "Integer", TTensor TInt -> "TensorInteger"
typeToName :: Type -> String
-- Note: TInt is normalized to "MathValue" because Integer = MathValue in Egison
typeToName TInt = "MathValue"  -- Integer = MathValue, use MathValue for dictionary names
typeToName TMathValue = "MathValue"
typeToName TFloat = "Float"
typeToName TBool = "Bool"
typeToName TChar = "Char"
typeToName TString = "String"
typeToName (TVar (TyVar v)) = v
typeToName (TInductive name _) = name
typeToName (TCollection t) = "Collection" ++ typeToName t
typeToName (TTuple ts) = "Tuple" ++ concatMap typeToName ts
typeToName (TTensor t) = "Tensor" ++ typeToName t
typeToName TFactor = "Factor"
typeToName (TFrac t) = "Frac" ++ typeToName t
typeToName (TPoly t ss) = "Poly" ++ typeToName t ++ symbolSetToName ss
  where
    -- Render atoms into a flat slug, e.g. [sqrt 2, x] -> "_sqrt_2_x".
    -- Spaces are replaced with underscores so the slug is a valid identifier
    -- piece for dictionary lookup.
    symbolSetToName (SymbolSetClosed syms) = concatMap (\a -> '_' : sanitize (prettyTypeAtomValue a)) syms
    symbolSetToName SymbolSetOpen = "_Open"
    symbolSetToName (SymbolSetVar (TyVar v)) = "_" ++ v
    sanitize = map (\c -> if c == ' ' || c == '(' || c == ')' then '_' else c)
typeToName _ = "Unknown"

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
typeConstructorName (TInductive name _) = name  -- Type arguments are ignored
typeConstructorName (TCollection _) = "Collection"  -- Element type is ignored
typeConstructorName (TTuple _) = "Tuple"
typeConstructorName (TTensor _) = "Tensor"
typeConstructorName (THash _ _) = "Hash"
typeConstructorName (TMatcher _) = "Matcher"
typeConstructorName (TFun _ _) = "Fun"
typeConstructorName (TIO _) = "IO"
typeConstructorName (TIORef _) = "IORef"
typeConstructorName TPort = "Port"
typeConstructorName TAny = "Any"
-- New CAS types
typeConstructorName TFactor = "Factor"
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

-- | Convert TypeExpr (from AST) to Type (internal representation)
typeExprToType :: TypeExpr -> Type
typeExprToType TEInt = TInt
typeExprToType TEMathValue = TMathValue  -- MathValue is a primitive type
typeExprToType TEFloat = TFloat
typeExprToType TEBool = TBool
typeExprToType TEChar = TChar
typeExprToType TEString = TString
typeExprToType (TEVar name) = TVar (TyVar name)
typeExprToType (TETuple ts) = TTuple (map typeExprToType ts)
typeExprToType (TEList t) = TCollection (typeExprToType t)
typeExprToType (TEApp t1 ts) = 
  case typeExprToType t1 of
    TVar (TyVar name) -> 
      -- Special case: convert inductive type names to primitive types
      case (name, ts) of
        ("MathValue", [])   -> TMathValue
        ("PolyExpr", [])   -> TPolyExpr
        ("TermExpr", [])   -> TTermExpr
        ("SymbolExpr", []) -> TSymbolExpr
        ("IndexExpr", [])  -> TIndexExpr
        _                  -> TInductive name (map typeExprToType ts)
    TInductive name existingTs -> TInductive name (existingTs ++ map typeExprToType ts)
    baseType -> baseType  -- Can't apply to non-inductive types
typeExprToType (TETensor elemT) = TTensor (typeExprToType elemT)
typeExprToType (TEVector elemT) = TTensor (typeExprToType elemT)  -- Vector is an alias for Tensor
typeExprToType (TEMatrix elemT) = TTensor (typeExprToType elemT)  -- Matrix is an alias for Tensor
typeExprToType (TEDiffForm elemT) = TTensor (typeExprToType elemT)  -- DiffForm is an alias for Tensor
typeExprToType (TEMatcher t) = TMatcher (typeExprToType t)
typeExprToType (TEFun t1 t2) = TFun (typeExprToType t1) (typeExprToType t2)
typeExprToType (TEIO t) = TIO (typeExprToType t)
typeExprToType (TEConstrained _ t) = typeExprToType t  -- Ignore constraints
typeExprToType (TEPattern t) = TInductive "Pattern" [typeExprToType t]
-- New CAS types
typeExprToType TEFactor = TFactor
typeExprToType (TEFrac t) = TFrac (typeExprToType t)
typeExprToType (TEPoly t ss) = TPoly (typeExprToType t) (symbolSetExprToSymbolSet ss)
  where
    symbolSetExprToSymbolSet (SSEClosed atoms) =
      SymbolSetClosed (map typeAtomExprToTypeAtom atoms)
    symbolSetExprToSymbolSet SSEOpen = SymbolSetOpen

-- | Normalize inductive type names to primitive types if applicable
-- This is used to convert TInductive "MathValue" [] to TMathValue, etc.
normalizeInductiveTypes :: Type -> Type
normalizeInductiveTypes (TInductive name []) = case name of
  "MathValue"   -> TMathValue
  "PolyExpr"   -> TPolyExpr
  "TermExpr"   -> TTermExpr
  "SymbolExpr" -> TSymbolExpr
  "IndexExpr"  -> TIndexExpr
  "Factor"     -> TFactor  -- New CAS type
  _            -> TInductive name []
-- Normalize Div to TFrac
normalizeInductiveTypes (TInductive "Frac" [t]) = TFrac (normalizeInductiveTypes t)
-- Convert TInductive "Vector", "Matrix", and "DiffForm" to Tensor (they are aliases)
normalizeInductiveTypes (TInductive "Vector" [t]) = TTensor (normalizeInductiveTypes t)
normalizeInductiveTypes (TInductive "Matrix" [t]) = TTensor (normalizeInductiveTypes t)
normalizeInductiveTypes (TInductive "DiffForm" [t]) = TTensor (normalizeInductiveTypes t)
normalizeInductiveTypes (TInductive name ts) = TInductive name (map normalizeInductiveTypes ts)
normalizeInductiveTypes (TTuple ts) = TTuple (map normalizeInductiveTypes ts)
normalizeInductiveTypes (TCollection t) = TCollection (normalizeInductiveTypes t)
normalizeInductiveTypes (THash k v) = THash (normalizeInductiveTypes k) (normalizeInductiveTypes v)
normalizeInductiveTypes (TMatcher t) = TMatcher (normalizeInductiveTypes t)
normalizeInductiveTypes (TFun arg ret) = TFun (normalizeInductiveTypes arg) (normalizeInductiveTypes ret)
normalizeInductiveTypes (TIO t) = TIO (normalizeInductiveTypes t)
normalizeInductiveTypes (TIORef t) = TIORef (normalizeInductiveTypes t)
normalizeInductiveTypes (TTensor t) = TTensor (normalizeInductiveTypes t)
-- New CAS types
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

