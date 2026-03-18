{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : Language.Egison.Math
Licence     : MIT

This module provides the interface of Egison's computer algebra system.
This module provides the public API for Egison's computer algebra system based on CASValue.
-}

module Language.Egison.Math
  ( -- * CAS Public API
    CASValue (..)
  , CASTerm (..)
  , casNormalize
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
  , casIsAtom
    -- ** Pretty printing
  , prettyCAS
    -- ** CAS Pattern Synonyms
  , pattern CASZero
  , pattern CASSingleSymbol
  , pattern CASSingleTerm
    -- ** CAS Pattern Matching (control-egison)
  , CASM (..)
  , CASTermM (..)
  , CASSymbolM (..)
  , casTerm'
  , casTerm'M
  , casTermM
  , casSymbol
  , casSymbolM
  , casFunc
  , casFuncM
  , casApply1
  , casApply1M
  , casApply2
  , casApply2M
  , casApply3
  , casApply3M
  , casApply4
  , casApply4M
  , casQuote
  , casNegQuote
  , casNegQuoteM
  , casQuoteFunction
  , casQuoteFunctionM
  , casEqualMonomial
  , casEqualMonomialM
  , casZero
  , casZeroM
  , casSingleTerm
  , casSingleTermM
  ) where

import           Language.Egison.Math.CAS hiding (SymbolExpr(..), Monomial, makeApplyExpr)
import qualified Language.Egison.Math.Rewrite as R

-- | Apply rewrite rules to a CASValue (CAS version of rewriteSymbol)
casRewriteSymbol :: CASValue -> CASValue
casRewriteSymbol = R.casRewriteSymbol
