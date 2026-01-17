{- |
Module      : Language.Egison.Type.Types
Licence     : MIT

This module defines the type system for Egison.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Egison.Type.Types
  ( Type(..)
  , TypeScheme(..)
  , TyVar(..)
  , TensorShape(..)
  , ShapeDimType(..)
  , Constraint(..)
  , ClassInfo(..)
  , InstanceInfo(..)
  , freshTyVar
  , freeTyVars
  , isTensorType
  , isScalarType
  , typeToName
  , typeConstructorName
  , sanitizeMethodName
  , typeExprToType
  , normalizeInductiveTypes
  , capitalizeFirst
  , lowerFirst
  , findMatchingInstanceForType
  ) where

import           Data.Char        (toLower, toUpper)
import           Data.Hashable    (Hashable)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           GHC.Generics     (Generic)

import           Language.Egison.AST        (TypeExpr(..))
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

-- | Egison types
data Type
  = TInt                              -- ^ Integer
  | TMathExpr                         -- ^ MathExpr (mathematical expression, unifies with Integer)
  | TPolyExpr                         -- ^ PolyExpr (polynomial expression)
  | TTermExpr                         -- ^ TermExpr (term in polynomial)
  | TSymbolExpr                       -- ^ SymbolExpr (symbolic variable)
  | TIndexExpr                        -- ^ IndexExpr (subscript/superscript index)
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
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | Type alias: MathExpr = Integer in Egison
-- Both names refer to the same type (TInt)
tMathExpr :: Type
tMathExpr = TInt

-- | Type scheme for polymorphic types (∀a. C a => Type)
-- Includes type constraints for type class support
data TypeScheme = Forall [TyVar] [Constraint] Type
  deriving (Eq, Show, Generic)

-- | Type class constraint, e.g., "Eq a"
data Constraint = Constraint
  { constraintClass :: String  -- ^ Class name, e.g., "Eq"
  , constraintType  :: Type    -- ^ Type argument, e.g., TVar "a"
  } deriving (Eq, Show, Generic)

-- | Information about a type class
data ClassInfo = ClassInfo
  { classSupers  :: [String]           -- ^ Superclass names
  , classParam   :: TyVar              -- ^ Type parameter (e.g., 'a' in "class Eq a")
  , classMethods :: [(String, Type)]   -- ^ Method names and their types
  } deriving (Eq, Show, Generic)

-- | Information about a type class instance
data InstanceInfo = InstanceInfo
  { instContext :: [Constraint]        -- ^ Instance context (e.g., "Eq a" in "{Eq a} Eq [a]")
  , instClass   :: String              -- ^ Class name
  , instType    :: Type                -- ^ Instance type
  , instMethods :: [(String, ())]      -- ^ Method implementations (placeholder for now)
  } deriving (Eq, Show, Generic)

-- | Generate a fresh type variable with a given prefix
freshTyVar :: String -> Int -> TyVar
freshTyVar prefix n = TyVar (prefix ++ show n)

-- | Get free type variables from a type
freeTyVars :: Type -> Set TyVar
freeTyVars TInt             = Set.empty
freeTyVars TMathExpr        = Set.empty
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

-- | Check if a type is a tensor type
isTensorType :: Type -> Bool
isTensorType (TTensor _) = True
isTensorType _           = False

-- | Check if a type is a scalar (non-tensor) type
isScalarType :: Type -> Bool
isScalarType = not . isTensorType

-- | Convert a Type to a string name for dictionary and method naming
-- This is used for generating instance dictionary names and method names
-- E.g., TInt -> "Integer", TTensor TInt -> "TensorInteger"
typeToName :: Type -> String
typeToName TInt = "Integer"
typeToName TFloat = "Float"
typeToName TBool = "Bool"
typeToName TChar = "Char"
typeToName TString = "String"
typeToName (TVar (TyVar v)) = v
typeToName (TInductive name _) = name
typeToName (TCollection t) = "Collection" ++ typeToName t
typeToName (TTuple ts) = "Tuple" ++ concatMap typeToName ts
typeToName (TTensor t) = "Tensor" ++ typeToName t
typeToName _ = "Unknown"

-- | Get the type constructor name only, without type parameters
-- Used for generating instance dictionary names (e.g., "eqCollection" not "eqCollectiona")
typeConstructorName :: Type -> String
typeConstructorName TInt = "Integer"
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
typeExprToType TEMathExpr = TMathExpr  -- MathExpr is a primitive type
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
        ("MathExpr", [])   -> TMathExpr
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
typeExprToType (TEMatcher t) = TMatcher (typeExprToType t)
typeExprToType (TEFun t1 t2) = TFun (typeExprToType t1) (typeExprToType t2)
typeExprToType (TEIO t) = TIO (typeExprToType t)
typeExprToType (TEConstrained _ t) = typeExprToType t  -- Ignore constraints
typeExprToType (TEPattern t) = TInductive "Pattern" [typeExprToType t]

-- | Normalize inductive type names to primitive types if applicable
-- This is used to convert TInductive "MathExpr" [] to TMathExpr, etc.
normalizeInductiveTypes :: Type -> Type
normalizeInductiveTypes (TInductive name []) = case name of
  "MathExpr"   -> TMathExpr
  "PolyExpr"   -> TPolyExpr
  "TermExpr"   -> TTermExpr
  "SymbolExpr" -> TSymbolExpr
  "IndexExpr"  -> TIndexExpr
  _            -> TInductive name []
-- Convert TInductive "Vector" and "Matrix" to Tensor (they are aliases)
normalizeInductiveTypes (TInductive "Vector" [t]) = TTensor (normalizeInductiveTypes t)
normalizeInductiveTypes (TInductive "Matrix" [t]) = TTensor (normalizeInductiveTypes t)
normalizeInductiveTypes (TInductive name ts) = TInductive name (map normalizeInductiveTypes ts)
normalizeInductiveTypes (TTuple ts) = TTuple (map normalizeInductiveTypes ts)
normalizeInductiveTypes (TCollection t) = TCollection (normalizeInductiveTypes t)
normalizeInductiveTypes (THash k v) = THash (normalizeInductiveTypes k) (normalizeInductiveTypes v)
normalizeInductiveTypes (TMatcher t) = TMatcher (normalizeInductiveTypes t)
normalizeInductiveTypes (TFun arg ret) = TFun (normalizeInductiveTypes arg) (normalizeInductiveTypes ret)
normalizeInductiveTypes (TIO t) = TIO (normalizeInductiveTypes t)
normalizeInductiveTypes (TIORef t) = TIORef (normalizeInductiveTypes t)
normalizeInductiveTypes (TTensor t) = TTensor (normalizeInductiveTypes t)
normalizeInductiveTypes t = t  -- Other types remain unchanged

-- | Capitalize first character
capitalizeFirst :: String -> String
capitalizeFirst []     = []
capitalizeFirst (c:cs) = toUpper c : cs

-- | Lowercase first character
lowerFirst :: String -> String
lowerFirst []     = []
lowerFirst (c:cs) = toLower c : cs

-- | Find a matching instance for a given target type
-- This searches through a list of instances and returns the first one that unifies with the target type
-- Used by both type inference (IInfer.hs) and type class expansion (TypeClassExpand.hs)
findMatchingInstanceForType :: Type -> [InstanceInfo] -> Maybe InstanceInfo
findMatchingInstanceForType targetType instances = go instances
  where
    go [] = Nothing
    go (inst:rest) =
      -- Try to unify the instance type with the target type
      -- We need to import unify from Type.Unify, but to avoid circular dependency,
      -- we'll use a simple structural equality check for now
      -- and rely on the caller to use unify externally
      if canMatch (instType inst) targetType
        then Just inst
        else go rest
    
    -- Simple structural matching (approximate)
    -- For precise matching, the caller should use unify
    canMatch :: Type -> Type -> Bool
    canMatch t1 t2
      | t1 == t2 = True  -- Exact match
      | otherwise = case (t1, t2) of
          (TVar _, _) -> True  -- Type variable matches anything
          (_, TVar _) -> True  -- Type variable matches anything
          (TTensor e1, TTensor e2) -> canMatch e1 e2
          (TCollection e1, TCollection e2) -> canMatch e1 e2
          (TFun a1 r1, TFun a2 r2) -> canMatch a1 a2 && canMatch r1 r2
          (TTuple ts1, TTuple ts2) 
            | length ts1 == length ts2 -> all (uncurry canMatch) (zip ts1 ts2)
          (TInductive n1 ts1, TInductive n2 ts2)
            | n1 == n2 && length ts1 == length ts2 -> all (uncurry canMatch) (zip ts1 ts2)
          _ -> False

