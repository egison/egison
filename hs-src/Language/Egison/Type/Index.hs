{- |
Module      : Language.Egison.Type.Index
Licence     : MIT

This module defines tensor index types for the Egison type system.
Indices can be superscript (contravariant, ~i) or subscript (covariant, _i).
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Egison.Type.Index
  ( IndexKind(..)
  , Index(..)
  , IndexSpec
  , IndexTyVar(..)
  , isSupSubPair
  , isSuperscript
  , isSubscript
  , isPlaceholder
  , indexSymbol
  , flipIndexKind
  ) where

import           Data.Hashable (Hashable)
import           GHC.Generics (Generic)

-- | The kind of tensor index
data IndexKind
  = Superscript    -- ^ Contravariant index, written as ~i
  | Subscript      -- ^ Covariant index, written as _i
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | A tensor index
data Index
  = IndexSym IndexKind String      -- ^ Named index, e.g., _i, ~j
  | IndexPlaceholder IndexKind     -- ^ Placeholder index, e.g., _#, ~#
  | IndexVar String                -- ^ Index variable (for type-level computation)
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | A sequence of indices
type IndexSpec = [Index]

-- | Index variable for type schemes (type-level)
newtype IndexTyVar = IndexTyVar String
  deriving (Eq, Ord, Show, Generic)

-- | Check if two indices form a superscript-subscript pair (for contraction)
-- For example, ~i and _i form a pair
isSupSubPair :: Index -> Index -> Bool
isSupSubPair (IndexSym Superscript s1) (IndexSym Subscript s2) = s1 == s2
isSupSubPair (IndexSym Subscript s1) (IndexSym Superscript s2) = s1 == s2
isSupSubPair _ _ = False

-- | Check if an index is a superscript
isSuperscript :: Index -> Bool
isSuperscript (IndexSym Superscript _)   = True
isSuperscript (IndexPlaceholder Superscript) = True
isSuperscript _                          = False

-- | Check if an index is a subscript
isSubscript :: Index -> Bool
isSubscript (IndexSym Subscript _)   = True
isSubscript (IndexPlaceholder Subscript) = True
isSubscript _                        = False

-- | Check if an index is a placeholder
isPlaceholder :: Index -> Bool
isPlaceholder (IndexPlaceholder _) = True
isPlaceholder _                    = False

-- | Get the symbol name from an index (if it has one)
indexSymbol :: Index -> Maybe String
indexSymbol (IndexSym _ s) = Just s
indexSymbol (IndexVar s)   = Just s
indexSymbol _              = Nothing

-- | Flip the kind of an index (superscript <-> subscript)
flipIndexKind :: IndexKind -> IndexKind
flipIndexKind Superscript = Subscript
flipIndexKind Subscript   = Superscript

