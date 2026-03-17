{- |
Module      : Language.Egison.Type.Join
Licence     : MIT

This module provides the join operation for CAS types.
The join computes the least upper bound (LUB) of two types in the
CAS type lattice, used for binary operations and conditional expressions.

Type lattice for CAS types:
  Integer ⊂ Factor ⊂ Poly Integer [s]
  Integer ⊂ Div Integer
  Integer ⊂ Poly Integer [s]
  Div Integer ⊂ Poly (Div Integer) [s]
  Poly Integer [s] ⊂ Poly (Div Integer) [s]

Symbol set inclusion:
  Poly a [x] ⊂ Poly a [x, y]     -- [x] ⊆ [x, y]
  Poly a [S₁] ⊂ Poly a [S₂]     -- when S₁ ⊆ S₂
  Poly a [S] ⊂ Poly a [..]      -- closed → open
-}

module Language.Egison.Type.Join
  ( joinTypes
  , JoinError(..)
  , isSubtype
  , symbolSetSubset
  ) where

import           Data.List                  (sort)
import           Language.Egison.Type.Types (SymbolSet (..), Type (..))

-- | Errors that can occur during join computation
data JoinError
  = IncompatibleSymbolSets SymbolSet SymbolSet
    -- ^ Symbol sets have no inclusion relationship
  | IncompatibleTypes Type Type
    -- ^ Types cannot be joined (e.g., String and Integer)
  deriving (Eq, Show)

-- | Compute the join (least upper bound) of two types.
-- Returns the LUB type if it exists, or an error if the types
-- cannot be joined in the CAS type lattice.
joinTypes :: Type -> Type -> Either JoinError Type

-- Same types join trivially
joinTypes t1 t2 | t1 == t2 = Right t1

-- Integer joins
joinTypes TInt (TDiv t) = Right (TDiv (joinCoeff TInt t))
joinTypes (TDiv t) TInt = Right (TDiv (joinCoeff t TInt))
joinTypes TInt (TPoly t ss) = Right (TPoly (joinCoeff TInt t) ss)
joinTypes (TPoly t ss) TInt = Right (TPoly (joinCoeff t TInt) ss)
joinTypes TInt TFactor = Right TFactor
joinTypes TFactor TInt = Right TFactor

-- Factor joins
joinTypes TFactor (TPoly t ss) = Right (TPoly (joinCoeff TInt t) ss)
joinTypes (TPoly t ss) TFactor = Right (TPoly (joinCoeff t TInt) ss)

-- Div joins
joinTypes (TDiv t1) (TDiv t2) = Right (TDiv (joinCoeff t1 t2))
joinTypes (TDiv t) (TPoly pt ss) =
  -- Div a with Poly b [S] -> Div (Poly (join a b) [S])
  Right (TDiv (TPoly (joinCoeff t pt) ss))
joinTypes (TPoly pt ss) (TDiv t) =
  Right (TDiv (TPoly (joinCoeff pt t) ss))

-- Poly joins - most complex case
joinTypes (TPoly t1 ss1) (TPoly t2 ss2) =
  case joinSymbolSets ss1 ss2 of
    Just ss -> Right (TPoly (joinCoeff t1 t2) ss)
    Nothing -> Left (IncompatibleSymbolSets ss1 ss2)

-- Open Poly with non-Poly
joinTypes (TPoly t ss@SymbolSetOpen) other =
  Right (TPoly (joinCoeff t (extractCoeff other)) ss)
joinTypes other (TPoly t ss@SymbolSetOpen) =
  Right (TPoly (joinCoeff (extractCoeff other) t) ss)

-- Any type
joinTypes TAny t = Right t
joinTypes t TAny = Right t

-- MathExpr compatibility (legacy)
joinTypes TInt TMathExpr = Right TMathExpr
joinTypes TMathExpr TInt = Right TMathExpr
joinTypes TMathExpr TMathExpr = Right TMathExpr

-- Default: types are incompatible
joinTypes t1 t2 = Left (IncompatibleTypes t1 t2)

-- | Join coefficient types (simplified version for recursive calls)
joinCoeff :: Type -> Type -> Type
joinCoeff TInt TInt = TInt
joinCoeff TInt (TDiv t) = TDiv (joinCoeff TInt t)
joinCoeff (TDiv t) TInt = TDiv (joinCoeff t TInt)
joinCoeff (TDiv t1) (TDiv t2) = TDiv (joinCoeff t1 t2)
joinCoeff t1 t2 | t1 == t2 = t1
joinCoeff _ _ = TAny  -- Fallback for incompatible coefficients

-- | Extract coefficient type from a type (for Poly integration)
extractCoeff :: Type -> Type
extractCoeff TInt = TInt
extractCoeff (TDiv t) = TDiv t
extractCoeff TFactor = TInt
extractCoeff (TPoly t _) = t
extractCoeff _ = TAny

-- | Join two symbol sets, if possible.
-- Returns Just the unified symbol set, or Nothing if incompatible.
joinSymbolSets :: SymbolSet -> SymbolSet -> Maybe SymbolSet

-- Open symbol set absorbs everything
joinSymbolSets SymbolSetOpen _ = Just SymbolSetOpen
joinSymbolSets _ SymbolSetOpen = Just SymbolSetOpen

-- Same closed sets
joinSymbolSets ss1@(SymbolSetClosed s1) (SymbolSetClosed s2)
  | sort s1 == sort s2 = Just ss1
  -- Check subset relationships
  | all (`elem` s2) s1 = Just (SymbolSetClosed s2)  -- s1 ⊆ s2
  | all (`elem` s1) s2 = Just (SymbolSetClosed s1)  -- s2 ⊆ s1
  | otherwise = Nothing  -- No inclusion relationship

-- Symbol set variables
joinSymbolSets (SymbolSetVar v1) (SymbolSetVar v2)
  | v1 == v2 = Just (SymbolSetVar v1)
  | otherwise = Just SymbolSetOpen  -- Different vars → open
joinSymbolSets (SymbolSetVar _) ss = Just ss
joinSymbolSets ss (SymbolSetVar _) = Just ss

-- | Check if one symbol set is a subset of another.
symbolSetSubset :: SymbolSet -> SymbolSet -> Bool
symbolSetSubset _ SymbolSetOpen = True  -- Everything is subset of open
symbolSetSubset SymbolSetOpen (SymbolSetClosed _) = False  -- Open not subset of closed
symbolSetSubset (SymbolSetClosed s1) (SymbolSetClosed s2) = all (`elem` s2) s1
symbolSetSubset (SymbolSetVar _) _ = True  -- Assume vars are subset (will be resolved)
symbolSetSubset _ (SymbolSetVar _) = True

-- | Check if one type is a subtype of another in the CAS lattice.
isSubtype :: Type -> Type -> Bool

-- Reflexivity
isSubtype t1 t2 | t1 == t2 = True

-- Integer is subtype of many types
isSubtype TInt (TDiv TInt) = True
isSubtype TInt TFactor = True
isSubtype TInt (TPoly TInt _) = True
isSubtype TInt TMathExpr = True

-- Factor subtypes
isSubtype TFactor (TPoly TInt _) = True

-- Div subtypes
isSubtype (TDiv t1) (TDiv t2) = isSubtype t1 t2

-- Poly subtypes
isSubtype (TPoly t1 ss1) (TPoly t2 ss2) =
  isSubtype t1 t2 && symbolSetSubset ss1 ss2

-- Any is supertype of everything
isSubtype _ TAny = True

-- Default
isSubtype _ _ = False
