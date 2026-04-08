{-# LANGUAGE QuasiQuotes #-}

{- |
Module      : Language.Egison.Math.Rewrite
Licence     : MIT

This module implements rewrite rules for common mathematical functions.
Uses CASValue as the primary representation.
-}

module Language.Egison.Math.Rewrite
  ( casRewriteSymbol
  ) where

import           Control.Egison

import           Language.Egison.Math.CAS
import           Language.Egison.Math.Normalize (casDivideTerm)
import {-# SOURCE #-} Language.Egison.Data (WHNFData)


-- | Apply rewrite rules to a CASValue
casRewriteSymbol :: CASValue -> CASValue
casRewriteSymbol =
  foldl1 (\acc f -> f . acc)
    [ casRewriteI
    , casRewriteW
    , casRewriteLog
--    , casRewriteSinCos
    , casRewriteExp
    , casRewritePower
    , casRewriteSqrt
    , casRewriteRt
    , casRewriteRtu
    , casRewriteDd
    ]


-- | Helper: Map a function over all terms in a CASValue
mapCASTerms :: (CASTerm -> CASTerm) -> CASValue -> CASValue
mapCASTerms f (CASPoly ts) = casNormalizePoly (map f ts)
mapCASTerms f (CASFrac num denom) = casNormalize (CASFrac (mapCASTerms f num) (mapCASTerms f denom))
mapCASTerms _ v = v

-- | Helper: Map a function over all terms, returning CASValue
mapCASTerms' :: (CASTerm -> CASValue) -> CASValue -> CASValue
mapCASTerms' f (CASPoly ts) = foldr casPlus (CASInteger 0) (map f ts)
mapCASTerms' f (CASFrac num denom) = casNormalize (CASFrac (mapCASTerms' f num) (mapCASTerms' f denom))
mapCASTerms' _ v = v

-- | Helper: Map over both polys in a CASFrac (or single poly)
mapCASPolys :: ([CASTerm] -> [CASTerm]) -> CASValue -> CASValue
mapCASPolys f (CASPoly ts) = casNormalizePoly (f ts)
mapCASPolys f (CASFrac num denom) = casNormalize (CASFrac (mapCASPolys f num) (mapCASPolys f denom))
mapCASPolys _ v = v

-- | Helper: Create an Apply symbol from WHNF and CASValue arguments
cassMakeApply :: WHNFData -> [CASValue] -> SymbolExpr
cassMakeApply whnf args =
  makeApplyExpr (CASPoly [CASTerm (CASInteger 1) [(QuoteFunction whnf, 1)]]) args

-- | Helper: Create a single term CASValue
casSingleTermVal :: Integer -> Monomial -> CASValue
casSingleTermVal coeff mono = CASPoly [CASTerm (CASInteger coeff) mono]


--------------------------------------------------------------------------------
-- Rewrite Rules
--------------------------------------------------------------------------------

-- | Rewrite i (imaginary unit): i^2 = -1
casRewriteI :: CASValue -> CASValue
casRewriteI = mapCASTerms f
 where
  f term@(CASTerm coeff xs) =
    match dfs xs (Multiset (CASSymbolM, Eql))
      [ [mc| (casSymbol #"i", $k) : $xss ->
              case coeff of
                CASInteger a ->
                  if even k
                    then CASTerm (CASInteger (a * (-1) ^ (quot k 2))) xss
                    else CASTerm (CASInteger (a * (-1) ^ (quot k 2))) ((Symbol "" "i" [], 1) : xss)
                _ -> term |]
      , [mc| _ -> term |]
      ]

-- | Rewrite w (cube root of unity): w^3 = 1
casRewriteW :: CASValue -> CASValue
casRewriteW = mapCASPolys g . mapCASTerms f
 where
  f term@(CASTerm coeff xs) =
    match dfs xs (Multiset (CASSymbolM, Eql))
      [ [mc| (casSymbol #"w", $k & ?(>= 3)) : $xss ->
               CASTerm coeff ((Symbol "" "w" [], k `mod` 3) : xss) |]
      , [mc| _ -> term |]
      ]
  g ts@(poly) =
    match dfs poly (Multiset CASTermM)
      [ [mc| casTerm' $a ((casSymbol #"w", #2) : $mr) :
             casTerm' $b ((casSymbol #"w", #1) : #mr) : $pr ->
               g (CASTerm (casNegate a) mr :
                  CASTerm (casMinus b a) ((Symbol "" "w" [], 1) : mr) : pr) |]
      , [mc| _ -> poly |]
      ]

-- | Rewrite log: log(1) = 0, log(e^n) = n
casRewriteLog :: CASValue -> CASValue
casRewriteLog = mapCASTerms f
 where
  f term@(CASTerm coeff xs) =
    match dfs xs (Multiset (CASSymbolM, Eql))
      [ [mc| (casApply1 #"log" _ casZero, _) : _ -> CASTerm (CASInteger 0) [] |]
      , [mc| (casApply1 #"log" _ (casSingleTerm _ #1 [(casSymbol #"e", $n)]), _) : $xss ->
              CASTerm (casMult (CASInteger n) coeff) xss |]
      , [mc| _ -> term |]
      ]

-- | Rewrite exp: exp(0) = 1, exp(1) = e, exp(n*i*pi) = (-1)^n
casRewriteExp :: CASValue -> CASValue
casRewriteExp = mapCASTerms f
 where
  f term@(CASTerm coeff xs) =
    match dfs xs (Multiset (CASSymbolM, Eql))
      [ [mc| (casApply1 #"exp" _ casZero, _) : $xss ->
               f (CASTerm coeff xss) |]
      , [mc| (casApply1 #"exp" _ (casSingleTerm #1 #1 []), _) : $xss ->
               f (CASTerm coeff ((Symbol "" "e" [], 1) : xss)) |]
      , [mc| (casApply1 #"exp" _ (casSingleTerm $n #1 [(casSymbol #"i", #1), (casSymbol #"π", #1)]), _) : $xss ->
               f (CASTerm (casMult coeff (CASInteger ((-1) ^ n))) xss) |]
      , [mc| (casApply1 #"exp" $expWhnf $x, $n & ?(>= 2)) : $xss ->
               f (CASTerm coeff ((cassMakeApply expWhnf [casMult (CASInteger n) x], 1) : xss)) |]
      , [mc| (casApply1 #"exp" $expWhnf $x, #1) : (casApply1 #"exp" _ $y, #1) : $xss ->
               f (CASTerm coeff ((cassMakeApply expWhnf [casPlus x y], 1) : xss)) |]
      , [mc| _ -> term |]
      ]

-- | Rewrite power: x^0 = 1, x^n * x^m = x^(n+m)
casRewritePower :: CASValue -> CASValue
casRewritePower = mapCASTerms f
 where
  f term@(CASTerm coeff xs) =
    match dfs xs (Multiset (CASSymbolM, Eql))
      [ [mc| (casApply1 #"^" _ (casSingleTerm #1 #1 []), _) : $xss -> f (CASTerm coeff xss) |]
      , [mc| (casApply2 #"^" $powerWhnf $x $y, $n & ?(>= 2)) : $xss ->
               f (CASTerm coeff ((cassMakeApply powerWhnf [x, casMult (CASInteger n) y], 1) : xss)) |]
      , [mc| (casApply2 #"^" $powerWhnf $x $y, #1) : (casApply2 #"^" _ #x $z, #1) : $xss ->
               f (CASTerm coeff ((cassMakeApply powerWhnf [x, casPlus y z], 1) : xss)) |]
      , [mc| _ -> term |]
      ]

-- | Determine if a CASValue is definitely negative
-- Returns Just True if negative, Just False if non-negative, Nothing if unknown
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

-- | Find a pair of sqrts in a monomial whose product simplifies to a single term.
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

-- | Rewrite sqrt: sqrt(x)^2 = x, sqrt(x) * sqrt(y) = sqrt(x*y)
casRewriteSqrt :: CASValue -> CASValue
casRewriteSqrt = mapCASTerms' f
 where
  f (CASTerm coeff xs) =
    match dfs xs (Multiset (CASSymbolM, Eql))
      [ [mc| (casApply1 #"sqrt" $sqrtWhnf $x, ?(> 1) & $k) : $xss ->
               casRewriteSqrt
                 (casMult (casSingleTermVal 1 ((cassMakeApply sqrtWhnf [x], k `mod` 2) : xss))
                          (casMult (casCoeffToVal coeff) (casPower x (div k 2)))) |]
      , [mc| (casApply1 #"sqrt" $sqrtWhnf (casSingleTerm $n #1 $x), #1) :
               (casApply1 #"sqrt" _ (casSingleTerm $m #1 $y), #1) : $xss ->
             let gcdTerm = casTermsGcd [CASTerm (CASInteger n) x, CASTerm (CASInteger m) y]
                 (_, CASTerm (CASInteger n') x') = casDivideTerm (CASTerm (CASInteger n) x) gcdTerm
                 (_, CASTerm (CASInteger m') y') = casDivideTerm (CASTerm (CASInteger m) y) gcdTerm
                 (CASInteger c) = casTermCoeff gcdTerm
                 z = casTermMono gcdTerm
                 in case (n' * m', n', m', x', y') of
                      (1, _, _, [], []) -> casMult (casSingleTermVal c z) (casCoeffToVal coeff)
                      (_, _, _, _, _) -> casMult (casSingleTermVal c z)
                                                 (casMult (casCoeffToVal coeff)
                                                          (casSingleTermVal 1 ((cassMakeApply sqrtWhnf [casSingleTermVal (n' * m') (x' ++ y')], 1) : xss))) |]
      , [mc| _ -> case casFindSqrtPairToMerge xs of
                    Just (whnf, product, remaining, sign) ->
                      casRewriteSqrt (casMult (casCoeffToVal coeff)
                                              (casSingleTermVal sign ((cassMakeApply whnf [product], 1) : remaining)))
                    Nothing -> casMult (casCoeffToVal coeff) (casSingleTermVal 1 xs) |]
      ]

  -- Helper to extract coefficient from a term
  casTermCoeff (CASTerm c _) = c
  -- Helper to extract monomial from a term
  casTermMono (CASTerm _ m) = m
  -- Helper to convert CASValue coefficient to CASValue
  casCoeffToVal c = c

-- | Rewrite rt (nth root): rt(n,x)^n = x
casRewriteRt :: CASValue -> CASValue
casRewriteRt = mapCASTerms' f
 where
  f (CASTerm coeff xs) =
    match dfs xs (Multiset (CASSymbolM, Eql))
      [ [mc| (casApply2 #"rt" _ (casSingleTerm $n #1 []) $x & $rtnx, ?(>= n) & $k) : $xss ->
               casMult (casMult (casCoeffToVal coeff) (casSingleTermVal 1 ((rtnx, k `mod` n) : xss)))
                       (casPower x (div k n)) |]
      , [mc| _ -> casMult (casCoeffToVal coeff) (casSingleTermVal 1 xs) |]
      ]
   where
    casCoeffToVal c = c

-- | Rewrite rtu (nth root of unity)
casRewriteRtu :: CASValue -> CASValue
casRewriteRtu = mapCASTerms' g . mapCASTerms f
 where
  f term@(CASTerm coeff xs) =
    match dfs xs (Multiset (CASSymbolM, Eql))
      [ [mc| (casApply1 #"rtu" _ (casSingleTerm $n #1 []) & $rtun, ?(>= n) & $k) : $r ->
               CASTerm coeff ((rtun, k `mod` n) : r) |]
      , [mc| _ -> term |]
      ]
  g (CASTerm coeff xs) =
    match dfs xs (Multiset (CASSymbolM, Eql))
      [ [mc| (casApply1 #"rtu" _ (casSingleTerm $n #1 []) & $rtun, ?(== n - 1)) : $mr ->
               casMult
                 (foldr casMinus (casSingleTermVal (-1) []) (map (\k -> casSingleTermVal 1 [(rtun, k)]) [1..(n-2)]))
                 (g (CASTerm coeff mr)) |]
      , [mc| _ -> casMult (casCoeffToVal coeff) (casSingleTermVal 1 xs) |]
      ]
   where
    casCoeffToVal c = c

-- | Rewrite dd (differential)
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
