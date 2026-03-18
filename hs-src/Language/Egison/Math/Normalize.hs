{-# LANGUAGE QuasiQuotes #-}

{- |
Module      : Language.Egison.Math.Normalize
Licence     : MIT

This module implements the normalization of polynomials. Normalization rules
for particular mathematical functions (such as sqrt and sin/cos) are defined
in Rewrite.hs.
-}

module Language.Egison.Math.Normalize
  ( casNormalize
  , casNormalizePoly
  , casTermsGcd
  , casDivideTerm
  ) where

import           Control.Egison

import           Language.Egison.Math.CAS


-- | Divide a CASTerm by another CASTerm (for GCD reduction)
-- Returns (sign, resulting_term) where sign accounts for negQuote handling
casDivideTerm :: CASTerm -> CASTerm -> (Integer, CASTerm)
casDivideTerm (CASTerm c1 xs) (CASTerm c2 ys) =
  let c' = casDivideCoeff c1 c2
      (sgn, zs) = divMonomial xs ys
  in (sgn, CASTerm c' zs)
 where
  divMonomial :: Monomial -> Monomial -> (Integer, Monomial)
  divMonomial m [] = (1, m)
  divMonomial m ((y, n):rest) =
    match dfs (y, m) (CASSymbolM, Multiset (CASSymbolM, Eql))
      -- Handle negQuote case: (-x)^n / (-x)^m where we match negated quotes
      [ [mc| (casQuote $s, (casNegQuote #s, $k) : $mss) ->
               let (sgn, m') = divMonomial mss rest in
               let sgn' = if even n then 1 else -1 in
               if k == n then (sgn * sgn', m')
                         else (sgn * sgn', (y, k - n) : m') |]
      , [mc| (_, (#y, $k) : $mss) ->
               let (sgn, m') = divMonomial mss rest in
               if k == n then (sgn, m') else (sgn, (y, k - n) : m') |]
      , [mc| _ -> divMonomial m rest |]
      ]

  -- Divide two CASValue coefficients
  casDivideCoeff :: CASValue -> CASValue -> CASValue
  casDivideCoeff (CASInteger a) (CASInteger b) = CASInteger (a `div` b)
  casDivideCoeff a b = casDivide a b
