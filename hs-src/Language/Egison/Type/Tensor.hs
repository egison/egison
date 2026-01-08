{- |
Module      : Language.Egison.Type.Tensor
Licence     : MIT

This module provides tensor-specific type normalization for the Egison type system.
-}

module Language.Egison.Type.Tensor
  ( -- * Type normalization
    normalizeTensorType
  ) where

import           Language.Egison.Type.Types

-- | Normalize tensor types
-- According to type-tensor-simple.md, Tensor MathExpr unifies with MathExpr
-- So Tensor a can unify with a (scalar)
normalizeTensorType :: Type -> Type
normalizeTensorType t = t

