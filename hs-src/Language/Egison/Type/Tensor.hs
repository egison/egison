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
-- For example,
-- Tensor a -> Tensor a
-- Tensor (Tensor a) -> Tensor a
-- Tensor (Tensor (Tensor a)) -> Tensor a
-- [Tensor (Tensor a)] -> [Tensor a]
normalizeTensorType :: Type -> Type
normalizeTensorType (TTensor (TTensor t)) = normalizeTensorType (TTensor t)
normalizeTensorType (TTensor t) = TTensor (normalizeTensorType t)
normalizeTensorType (TTuple ts) = TTuple (map normalizeTensorType ts)
normalizeTensorType (TCollection t) = TCollection (normalizeTensorType t)
normalizeTensorType (TInductive name ts) = TInductive name (map normalizeTensorType ts)
normalizeTensorType (THash k v) = THash (normalizeTensorType k) (normalizeTensorType v)
normalizeTensorType (TMatcher t) = TMatcher (normalizeTensorType t)
normalizeTensorType (TFun a r) = TFun (normalizeTensorType a) (normalizeTensorType r)
normalizeTensorType (TIO t) = TIO (normalizeTensorType t)
normalizeTensorType (TIORef t) = TIORef (normalizeTensorType t)
normalizeTensorType t = t  -- TInt, TMathExpr, TPolyExpr, TTermExpr, TSymbolExpr, TIndexExpr, TFloat, TBool, TChar, TString, TVar, TAny
