{- |
Module      : Language.Egison.Type.Join
Licence     : MIT

The legacy structural subtype relation for CAS types, used by instance
resolution (Type.Instance, Core's runtime dispatch).

Note (Phase beta of the extensible tower): the design-complete skeleton
relation and the join operation live in "Language.Egison.Type.Subtype"
('skeletonSubtype' / 'skeletonJoin' / 'isSubtypeWith' / 'joinTypesWith').
This module deliberately keeps the older, partial 'isSubtype' because
instance resolution depends on its exact extent — switching it to the
complete skeleton would widen which instances match and must be done
with a dedicated dispatch-impact assessment (see
design/type-cas-tower-implementation.md §7). The former 'joinTypes' here
(which disagreed with the design's tower rule, returning level 5 for
Poly ⊔ Frac) had no callers and has been removed in favor of
'Subtype.skeletonJoin'.

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
  ( isSubtype
  , symbolSetSubset
  ) where

import           Language.Egison.Type.Types (SymbolSet (..), Type (..))

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
