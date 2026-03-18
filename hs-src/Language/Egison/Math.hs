{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : Language.Egison.Math
Licence     : MIT

This module provides the interface of Egison's computer algebra system.
The public API is based on CASValue. ScalarData is an internal implementation detail.
-}

module Language.Egison.Math
  ( -- * CAS Public API (preferred)
    CASValue (..)
  , CASTerm (..)
  , casNormalize'
  , casRewriteSymbol
  , casPlus
  , casMinus
  , casMult
  , casDivide
  , casPower
  , casNumerator
  , casDenominator
  , casNegate
  , casIsZero
    -- * Internal types (for pattern matching in Core.hs etc.)
    -- These are exposed for backward compatibility but should be migrated to CAS types
  , ScalarData (..)
  , PolyExpr (..)
  , TermExpr (..)
  , E.Monomial
  , E.SymbolExpr (..)
  , E.Printable (..)
  , pattern ZeroExpr
  , pattern SingleSymbol
  , pattern SingleTerm
    -- * Conversion functions (for migration)
  , scalarDataToCASValue
  , casValueToScalarData
    -- * Deprecated ScalarData operations (use CAS versions instead)
  , mathNormalize'
  , rewriteSymbol
  , mathPlus
  , mathMult
  , mathDiv
  , mathNumerator
  , mathDenominator
  , E.mathNegate
  , E.makeApplyExpr
  ) where

import           Language.Egison.Math.Arith
import           Language.Egison.Math.CAS hiding (SymbolExpr(..), Monomial, makeApplyExpr)
import qualified Language.Egison.Math.Expr as E
import           Language.Egison.Math.Expr (ScalarData(..), PolyExpr(..), TermExpr(..), pattern ZeroExpr, pattern SingleSymbol, pattern SingleTerm)
import           Language.Egison.Math.Normalize
import           Language.Egison.Math.Rewrite hiding (casRewriteSymbol)
import qualified Language.Egison.Math.Rewrite as R

-- | Normalize a CASValue (CAS version of mathNormalize')
casNormalize' :: CASValue -> CASValue
casNormalize' = scalarDataToCASValue . mathNormalize' . casValueToScalarData

-- | Apply rewrite rules to a CASValue (CAS version of rewriteSymbol)
casRewriteSymbol :: CASValue -> CASValue
casRewriteSymbol = R.casRewriteSymbol
