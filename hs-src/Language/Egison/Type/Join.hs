{- |
Module      : Language.Egison.Type.Join
Licence     : MIT

This module provides the join operation for CAS types.
The join computes the least upper bound (LUB) of two types in the
CAS type lattice, used for binary operations and conditional expressions.

Type lattice for CAS types:
  Integer ⊂ Factor ⊂ Poly Integer [s]
  Integer ⊂ Frac Integer
  Integer ⊂ Poly Integer [s]
  Frac Integer ⊂ Poly (Frac Integer) [s]
  Poly Integer [s] ⊂ Poly (Frac Integer) [s]

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
joinTypes TInt (TFrac t) = Right (TFrac (joinCoeff TInt t))
joinTypes (TFrac t) TInt = Right (TFrac (joinCoeff t TInt))
joinTypes TInt (TPoly t ss) = Right (TPoly (joinCoeff TInt t) ss)
joinTypes (TPoly t ss) TInt = Right (TPoly (joinCoeff t TInt) ss)
joinTypes TInt (TTerm t ss) = Right (TTerm (joinCoeff TInt t) ss)
joinTypes (TTerm t ss) TInt = Right (TTerm (joinCoeff t TInt) ss)
joinTypes TInt TFactor = Right TFactor
joinTypes TFactor TInt = Right TFactor

-- Factor joins
joinTypes TFactor (TPoly t ss) = Right (TPoly (joinCoeff TInt t) ss)
joinTypes (TPoly t ss) TFactor = Right (TPoly (joinCoeff t TInt) ss)
joinTypes TFactor (TTerm t ss) = Right (TTerm (joinCoeff TInt t) ss)
joinTypes (TTerm t ss) TFactor = Right (TTerm (joinCoeff t TInt) ss)

-- Term joins
joinTypes (TTerm t1 ss1) (TTerm t2 ss2) =
  case joinSymbolSets ss1 ss2 of
    Just ss -> Right (TTerm (joinCoeff t1 t2) ss)
    Nothing -> Left (IncompatibleSymbolSets ss1 ss2)
joinTypes (TTerm t ss1) (TPoly pt ss2) =
  -- Term a [S1] with Poly b [S2] -> Poly (join a b) (joinSS S1 S2)
  case joinSymbolSets ss1 ss2 of
    Just ss -> Right (TPoly (joinCoeff t pt) ss)
    Nothing -> Left (IncompatibleSymbolSets ss1 ss2)
joinTypes (TPoly pt ss1) (TTerm t ss2) =
  case joinSymbolSets ss1 ss2 of
    Just ss -> Right (TPoly (joinCoeff pt t) ss)
    Nothing -> Left (IncompatibleSymbolSets ss1 ss2)

-- Frac joins
joinTypes (TFrac t1) (TFrac t2) = Right (TFrac (joinCoeff t1 t2))
joinTypes (TFrac t) (TPoly pt ss) =
  -- Frac a with Poly b [S] -> Frac (Poly (join a b) [S])
  Right (TFrac (TPoly (joinCoeff t pt) ss))
joinTypes (TPoly pt ss) (TFrac t) =
  Right (TFrac (TPoly (joinCoeff pt t) ss))
joinTypes (TFrac t) (TTerm pt ss) =
  Right (TFrac (TTerm (joinCoeff t pt) ss))
joinTypes (TTerm pt ss) (TFrac t) =
  Right (TFrac (TTerm (joinCoeff pt t) ss))

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

-- MathValue compatibility (legacy)
joinTypes TInt TMathValue = Right TMathValue
joinTypes TMathValue TInt = Right TMathValue
joinTypes TMathValue TMathValue = Right TMathValue

-- Default: types are incompatible
joinTypes t1 t2 = Left (IncompatibleTypes t1 t2)

-- | Join coefficient types (simplified version for recursive calls)
joinCoeff :: Type -> Type -> Type
joinCoeff TInt TInt = TInt
joinCoeff TInt (TFrac t) = TFrac (joinCoeff TInt t)
joinCoeff (TFrac t) TInt = TFrac (joinCoeff t TInt)
joinCoeff (TFrac t1) (TFrac t2) = TFrac (joinCoeff t1 t2)
joinCoeff t1 t2 | t1 == t2 = t1
joinCoeff _ _ = TAny  -- Fallback for incompatible coefficients

-- | Extract coefficient type from a type (for Poly integration)
extractCoeff :: Type -> Type
extractCoeff TInt = TInt
extractCoeff (TFrac t) = TFrac t
extractCoeff TFactor = TInt
extractCoeff (TTerm t _) = t
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
-- The CAS hierarchy is (more specific → more general):
--   Integer ⊂ Factor ⊂ Term a ⊂ Poly a [s] ⊂ Frac a ⊂ MathValue
-- Nested types are covariant within depth ≤ 2.
isSubtype :: Type -> Type -> Bool

-- Reflexivity
isSubtype t1 t2 | t1 == t2 = True

-- Integer subtypes
isSubtype TInt (TFrac TInt) = True
isSubtype TInt TFactor = True
isSubtype TInt (TTerm TInt _) = True
isSubtype TInt (TPoly TInt _) = True
isSubtype TInt TMathValue = True

-- Factor subtypes
isSubtype TFactor (TTerm TInt _) = True
isSubtype TFactor (TPoly TInt _) = True
isSubtype TFactor (TFrac TInt) = True
isSubtype TFactor TMathValue = True

-- Term subtypes (covariant in coefficient AND atom-set inclusion, like Poly)
isSubtype (TTerm t1 ss1) (TTerm t2 ss2) =
  isSubtype t1 t2 && symbolSetSubset ss1 ss2
isSubtype (TTerm t1 _) (TPoly t2 _) = isSubtype t1 t2
isSubtype (TTerm t1 _) (TFrac t2) = isSubtype t1 t2
isSubtype (TTerm _ _) TMathValue = True

-- Poly subtypes
isSubtype (TPoly t1 ss1) (TPoly t2 ss2) =
  isSubtype t1 t2 && symbolSetSubset ss1 ss2
isSubtype (TPoly t1 _) (TFrac t2) = isSubtype t1 t2
isSubtype (TPoly _ _) TMathValue = True

-- Frac subtypes
isSubtype (TFrac t1) (TFrac t2) = isSubtype t1 t2
isSubtype (TFrac _) TMathValue = True

-- Any is supertype of everything
isSubtype _ TAny = True

-- Default
isSubtype _ _ = False
