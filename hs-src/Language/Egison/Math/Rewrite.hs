{-# LANGUAGE QuasiQuotes #-}

{- |
Module      : Language.Egison.Math.Rewrite
Licence     : MIT

Residual mathematical rewrite rules that are kept in Haskell because their
declare-rule equivalents are too expensive in practice for complex samples
(e.g. riemann-curvature-tensor-of-S2xS3).

Remaining functions:
  - casRewriteSqrt  : `sqrt a * sqrt b = sqrt(a*b)` plus GCD pair merge
  - casRewriteDd    : merge polynomial terms with same FunctionData factor

Migrated to declare rule (and removed):
  - casRewriteI, casRewriteW, casRewriteLog, casRewriteExp
  - casRewritePower, casRewriteRt, casRewriteRtu
  - casRewriteSqrt's `(sqrt x)^k` (k > 1) reduction
-}

module Language.Egison.Math.Rewrite
  ( casRewriteSymbol
  ) where

import           Control.Egison

import           Language.Egison.Math.CAS
import           Language.Egison.Math.Normalize (casDivideTerm)
import {-# SOURCE #-} Language.Egison.Data (WHNFData)


-- | Apply rewrite rules to a CASValue.
casRewriteSymbol :: CASValue -> CASValue
casRewriteSymbol = casRewriteDd . casRewriteSqrt

-- | Helper: Map a function over all terms, returning CASValue
mapCASTerms' :: (CASTerm -> CASValue) -> CASValue -> CASValue
mapCASTerms' f (CASPoly ts) = foldr casPlus (CASInteger 0) (map f ts)
mapCASTerms' f (CASFrac num denom) = casNormalize (CASFrac (mapCASTerms' f num) (mapCASTerms' f denom))
mapCASTerms' _ v = v

-- | Helper: Create an Apply symbol from WHNF and CASValue arguments
cassMakeApply :: WHNFData -> [CASValue] -> SymbolExpr
cassMakeApply whnf args =
  makeApplyExpr (CASPoly [CASTerm (CASInteger 1) [(QuoteFunction whnf, 1)]]) args

-- | Helper: Create a single term CASValue
casSingleTermVal :: Integer -> Monomial -> CASValue
casSingleTermVal coeff mono = CASPoly [CASTerm (CASInteger coeff) mono]

-- | Determine if a CASValue is definitely negative.
casIsNegative :: CASValue -> Maybe Bool
casIsNegative (CASInteger n)
  | n < 0     = Just True
  | otherwise = Just False
casIsNegative (CASPoly terms)
  | all isNegTerm terms = Just True
  | all isPosTerm terms = Just False
 where
  isNegTerm (CASTerm (CASInteger c) _) = c < 0
  isNegTerm _ = False
  isPosTerm (CASTerm (CASInteger c) _) = c > 0
  isPosTerm _ = False
casIsNegative (CASFrac num (CASInteger d))
  | d > 0     = casIsNegative num
  | d < 0     = fmap not (casIsNegative num)
casIsNegative (CASFrac num (CASPoly [CASTerm (CASInteger d) []]))
  | d > 0     = casIsNegative num
  | d < 0     = fmap not (casIsNegative num)
casIsNegative _ = Nothing

casFindSqrtPairToMerge :: Monomial -> Maybe (WHNFData, CASValue, Monomial, Integer)
casFindSqrtPairToMerge xs =
  case results of
    (r:_) -> Just r
    []    -> Nothing
 where
  results =
    [ (whnf, simplified, xss, sign)
    | (whnf, x, y, xss) <- matchAll dfs xs (Multiset (CASSymbolM, Eql))
        [ [mc| (casApply1 #"sqrt" $whnf $x, #1) :
               (casApply1 #"sqrt" _ $y, #1) : $xss ->
                 (whnf, x, y, xss) |] ]
    , let simplified = casRewriteSqrt (casMult x y)
    , isSingleTermCAS simplified
    , let sign = case (casIsNegative x, casIsNegative y) of
                   (Just True, Just True) -> -1
                   _                      -> 1
    ]
  isSingleTermCAS (CASInteger _) = True
  isSingleTermCAS (CASPoly [_])  = True
  isSingleTermCAS _              = False

-- | Rewrite sqrt: sqrt(x) * sqrt(y) merge with GCD extraction.
-- The `(sqrt x)^k` (k > 1) reduction was migrated to declare rule auto.
casRewriteSqrt :: CASValue -> CASValue
casRewriteSqrt = mapCASTerms' f
 where
  f (CASTerm coeff xs) =
    match dfs xs (Multiset (CASSymbolM, Eql))
      [ [mc| (casApply1 #"sqrt" $sqrtWhnf (casSingleTerm $n #1 $x), #1) :
               (casApply1 #"sqrt" _ (casSingleTerm $m #1 $y), #1) : $xss ->
             let gcdTerm = casTermsGcd [CASTerm (CASInteger n) x, CASTerm (CASInteger m) y]
                 (_, CASTerm (CASInteger n') x') = casDivideTerm (CASTerm (CASInteger n) x) gcdTerm
                 (_, CASTerm (CASInteger m') y') = casDivideTerm (CASTerm (CASInteger m) y) gcdTerm
                 (CASInteger c) = casTermCoeff gcdTerm
                 z = casTermMono gcdTerm
                 in case (n' * m', n', m', x', y') of
                      (1, _, _, [], []) -> casMult (casSingleTermVal c z) coeff
                      (_, _, _, _, _) -> casMult (casSingleTermVal c z)
                                                 (casMult coeff
                                                          (casSingleTermVal 1 ((cassMakeApply sqrtWhnf [casSingleTermVal (n' * m') (x' ++ y')], 1) : xss))) |]
      , [mc| _ -> case casFindSqrtPairToMerge xs of
                    Just (whnf, product, remaining, sign) ->
                      casRewriteSqrt (casMult coeff
                                              (casSingleTermVal sign ((cassMakeApply whnf [product], 1) : remaining)))
                    Nothing -> casMult coeff (casSingleTermVal 1 xs) |]
      ]
  casTermCoeff (CASTerm c _) = c
  casTermMono (CASTerm _ m) = m

-- | Rewrite dd (differential): merge polynomial terms whose monomial shares
-- the same FunctionData factor.
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
