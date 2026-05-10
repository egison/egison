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
--
-- Fast path: if the value contains no FunctionData factor anywhere, the
-- pattern can't fire — return the value unchanged. This avoids the
-- per-mathNormalize-call overhead of `rewriteDdPoly`'s multi-term matcher
-- on the vast majority of values that have no `Function _ _` factors.
casRewriteDd :: CASValue -> CASValue
casRewriteDd v
  | not (casHasFunctionData v) = v
casRewriteDd (CASFrac num denom) =
  CASFrac (casNormalizePoly (rewriteDdPoly (extractTerms num)))
         (casNormalizePoly (rewriteDdPoly (extractTerms denom)))
 where
  extractTerms (CASPoly ts) = ts
  extractTerms (CASInteger n) = [CASTerm (CASInteger n) []]
  extractTerms _ = []
casRewriteDd (CASPoly ts) = casNormalizePoly (rewriteDdPoly ts)
casRewriteDd v = v

-- | True if the CASValue contains any `FunctionData` factor anywhere in
-- its tree.
casHasFunctionData :: CASValue -> Bool
casHasFunctionData = goV
 where
  goV (CASInteger _)  = False
  goV (CASFactor sym) = goSym sym
  goV (CASPoly terms) = any goTerm terms
  goV (CASFrac n d)   = goV n || goV d
  goTerm (CASTerm coeff mono) = goV coeff || any (goSym . fst) mono
  goSym (FunctionData {})  = True
  goSym (Apply1 f a)        = goV f || goV a
  goSym (Apply2 f a b)      = goV f || goV a || goV b
  goSym (Apply3 f a b c)    = goV f || goV a || goV b || goV c
  goSym (Apply4 f a b c d)  = goV f || goV a || goV b || goV c || goV d
  goSym (Quote v)           = goV v
  goSym (Symbol _ _ _)      = False
  goSym (QuoteFunction _)   = False

rewriteDdPoly :: [CASTerm] -> [CASTerm]
rewriteDdPoly poly =
  match dfs poly (Multiset CASTermM)
    [ [mc| casTerm' $a (($f & casFunc $g $args, $n) : $mr) :
           casTerm' $b ((casFunc #g #args, #n) : #mr) : $pr ->
             rewriteDdPoly (CASTerm (casPlus a b) ((f, n) : mr) : pr) |]
    , [mc| _ -> poly |]
    ]
