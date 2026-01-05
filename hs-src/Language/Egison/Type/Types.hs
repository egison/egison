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
  ) where

import           Data.Hashable    (Hashable)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           GHC.Generics     (Generic)

import           Language.Egison.Type.Index (IndexSpec, IndexKind, Index)

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
  = TInt                              -- ^ Integer (= MathExpr in Egison)
  | TFloat                            -- ^ Float (Double)
  | TBool                             -- ^ Bool
  | TChar                             -- ^ Char
  | TString                           -- ^ String
  | TUnit                             -- ^ Unit type ()
  | TVar TyVar                        -- ^ Type variable, e.g., a
  | TTuple [Type]                     -- ^ Tuple type, e.g., (a, b)
  | TCollection Type                  -- ^ Collection type, e.g., [a]
  | TInductive String [Type]          -- ^ Inductive data type with type arguments
  | TTensor Type                      -- ^ Tensor type (only element type is kept)
  | THash Type Type                   -- ^ Hash map type
  | TMatcher Type                     -- ^ Matcher type, e.g., Matcher a
  | TFun Type Type                    -- ^ Function type, e.g., a -> b
  | TIO Type                          -- ^ IO type (for IO actions)
  | TIORef Type                       -- ^ IORef type
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
freeTyVars TFloat           = Set.empty
freeTyVars TBool            = Set.empty
freeTyVars TChar            = Set.empty
freeTyVars TString          = Set.empty
freeTyVars TUnit            = Set.empty
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
freeTyVars TAny             = Set.empty

-- | Check if a type is a tensor type
isTensorType :: Type -> Bool
isTensorType (TTensor _) = True
isTensorType _           = False

-- | Check if a type is a scalar (non-tensor) type
isScalarType :: Type -> Bool
isScalarType = not . isTensorType

