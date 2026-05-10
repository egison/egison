{-# LANGUAGE QuasiQuotes #-}

{- |
Module      : Language.Egison.Math.Rewrite
Licence     : MIT

Residual mathematical rewrite rules kept in Haskell.

Remaining function:
  - casRewriteDd : merge polynomial terms with same FunctionData factor.
    The equivalent declare-rule poly pattern is too expensive for complex
    differential-form samples (e.g. riemann-curvature-tensor-of-S2xS3).

Migrated to declare rule (and removed):
  - casRewriteI, casRewriteW, casRewriteLog, casRewriteExp
  - casRewritePower, casRewriteRt, casRewriteRtu, casRewriteSqrt
-}

module Language.Egison.Math.Rewrite
  ( casRewriteSymbol
  ) where

import           Control.Egison

import           Language.Egison.Math.CAS

-- | Apply rewrite rules to a CASValue.
casRewriteSymbol :: CASValue -> CASValue
casRewriteSymbol = casRewriteDd

-- | Rewrite dd (differential): merge polynomial terms whose monomial shares
-- the same FunctionData factor (same `g`, `args`, exponent, and rest of
-- monomial). Kept in Haskell because the equivalent
-- `declare rule auto poly` (multi-term, same-binding) is too expensive for
-- complex differential-form samples (e.g. riemann-curvature-tensor-of-S2xS3).
casRewriteDd :: CASValue -> CASValue
casRewriteDd (CASFrac num denom) =
  CASFrac (casNormalizePoly (rewriteDdPoly (extractTerms num)))
         (casNormalizePoly (rewriteDdPoly (extractTerms denom)))
 where
  extractTerms (CASPoly ts) = ts
  extractTerms (CASInteger n) = [CASTerm (CASInteger n) []]
  extractTerms _ = []
casRewriteDd (CASPoly ts) = casNormalizePoly (rewriteDdPoly ts)
casRewriteDd v = v

rewriteDdPoly :: [CASTerm] -> [CASTerm]
rewriteDdPoly poly =
  match dfs poly (Multiset CASTermM)
    [ [mc| casTerm' $a (($f & casFunc $g $args, $n) : $mr) :
           casTerm' $b ((casFunc #g #args, #n) : #mr) : $pr ->
             rewriteDdPoly (CASTerm (casPlus a b) ((f, n) : mr) : pr) |]
    , [mc| _ -> poly |]
    ]
