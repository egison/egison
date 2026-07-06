{-# LANGUAGE QuasiQuotes #-}

{- |
Module      : Language.Egison.Math.Rewrite
Licence     : MIT

Residual mathematical rewrite rules kept in Haskell.

Remaining functions:
  - casRewriteDd : merge polynomial terms with same FunctionData factor.
    The equivalent declare-rule poly pattern is too expensive for complex
    differential-form samples (e.g. riemann-curvature-tensor-of-S2xS3).
  - casRewriteSqrt : sqrt power reduction and sqrt pair merging.
    Ported back from declare rule (G6 of design/cas-simplification.md):
    the Egison-level term rules paid a pattern-match attempt on every
    term of every sqrt-carrying value per normalization, making
    arithmetic on such values ~20x slower (thurston.egi's bottleneck).
  - casRewriteExp : exp power reduction and exp product merging
    ((exp a)^n -> exp (n a), exp a * exp b -> exp (a+b)), ported back
    for the same reason (an identical 60-operation fold on
    exp-carrying operands measured 20.7s under the declare rules).
    The value rules (exp 0 = 1, exp 1 = e, exp (n i pi) = (-1)^n)
    stay in the library.

Migrated to declare rule (and removed):
  - casRewriteI, casRewriteW, casRewriteLog
  - casRewritePower, casRewriteRt, casRewriteRtu
-}

module Language.Egison.Math.Rewrite
  ( casRewriteSymbol
  ) where

import           Control.Egison

import           Language.Egison.Math.CAS

-- | Apply rewrite rules to a CASValue.
casRewriteSymbol :: CASValue -> CASValue
casRewriteSymbol = casRewriteDd . casRewriteSqrt . casRewriteExp

-- | Rewrite sqrt factors of every term (top level of the value, the
-- same scope as casRewriteDd; inner values were normalized when they
-- were constructed):
--
--   1. Power reduction: (sqrt a)^n with |n| >= 2 becomes
--      a^q * (sqrt a)^r with q = quot n 2 and r = n - 2q in {-1,0,1}
--      (matching the power-evaluation path's normal form).
--   2. Pair merge: sqrt a * sqrt b (both with exponent 1) merges to
--      sqrt (a*b), and the square part of a single-term product is
--      extracted: sqrt 2 * sqrt 8 = sqrt 16 = 4,
--      sqrt (2x) * sqrt (2y) = 2 * sqrt (x y),
--      sqrt x * sqrt (x y^2) = x y.  Multi-term (polynomial) products
--      stay under the sqrt: sqrt (x+1) * sqrt (x-1) = sqrt (x^2-1).
--
-- Fast path: values none of whose terms need sqrt work (see
-- termNeedsSqrtWork) are returned unchanged, without any rebuilding.
casRewriteSqrt :: CASValue -> CASValue
casRewriteSqrt = go (100 :: Int)
 where
  -- Iterate to a fixpoint (like the old applyRuleFix): one rewrite
  -- pass can create new reducible shapes -- merging produces products
  -- whose casMult combines equal atoms into powers, and content
  -- splitting produces pairs that reproduce themselves identically
  -- (sqrt 2 * sqrt(-sqrt 5 - 5) merges and re-splits to the same
  -- pair, which the equality check turns into a fixpoint).
  go 0 v = v
  go fuel v =
    let v' = rewriteSqrtOnce v
    in if v' == v then v else go (fuel - 1) v'

rewriteSqrtOnce :: CASValue -> CASValue
rewriteSqrtOnce (CASFrac num denom)
  | valueNeedsSqrtWork num || valueNeedsSqrtWork denom =
      casDivide (rewriteSqrtPart num) (rewriteSqrtPart denom)
rewriteSqrtOnce v@(CASPoly _)
  | valueNeedsSqrtWork v = rewriteSqrtPart v
rewriteSqrtOnce v = v

-- | Fast path: a term needs sqrt work if it has a sqrt factor with
-- |exponent| >= 2, at least two sqrt factors with exponent 1, or a
-- nested value (an application argument, a quoted expression, or a
-- level-4 coefficient) that needs work itself -- the old declare-rule
-- versions recursed into those via mapTermAll, and nested-radical
-- reductions such as sqrt 5 * sqrt(-5-2 sqrt 5) * sqrt(-5+2 sqrt 5) = 5
-- depend on it.  Terms with a lone top-level (sqrt a)^(+-1) factor and
-- quiet insides -- the common shape in curvature-style values -- are
-- skipped without any rebuilding, which is the point of the port.
termNeedsSqrtWork :: CASTerm -> Bool
termNeedsSqrtWork (CASTerm c mono) = go 0 mono || valueNeedsSqrtWork c
 where
  go ones ((sym, n) : rest) = case sqrtRadicand sym of
    Just _
      | abs n >= 2       -> True
      | n == 1 && ones >= 1 -> True
      | n == 1           -> symNeedsSqrtWork sym || go 1 rest
      | otherwise        -> symNeedsSqrtWork sym || go ones rest
    Nothing              -> symNeedsSqrtWork sym || go ones rest
  go _ [] = False

symNeedsSqrtWork :: SymbolExpr -> Bool
symNeedsSqrtWork = symNeedsWorkWith valueNeedsSqrtWork

valueNeedsSqrtWork :: CASValue -> Bool
valueNeedsSqrtWork (CASFrac n d) = valueNeedsSqrtWork n || valueNeedsSqrtWork d
valueNeedsSqrtWork (CASPoly ts)  = any termNeedsSqrtWork ts
valueNeedsSqrtWork _             = False

-- | Rewrite the terms that need it and re-combine.  Combination goes
-- through casPlus/casDivide (not a raw CASPoly rebuild) because a
-- negative radicand power over a polynomial radicand produces a
-- genuine fraction: (sqrt (x+1))^-2 = 1/(x+1).
rewriteSqrtPart :: CASValue -> CASValue
rewriteSqrtPart (CASPoly ts) =
  let changed   = filter termNeedsSqrtWork ts
      unchanged = filter (not . termNeedsSqrtWork) ts
  in foldl casPlus (casNormalizePoly unchanged) (map rewriteSqrtTerm changed)
rewriteSqrtPart v = v

-- | The argument of a unary application factor of the named function.
applyArg1 :: String -> SymbolExpr -> Maybe CASValue
applyArg1 name (Apply1 fh a) = case fh of
  CASFactor (QuoteFunction w)
    | prettyFunctionName w == Just name -> Just a
  CASPoly [CASTerm (CASInteger 1) [(QuoteFunction w, 1)]]
    | prettyFunctionName w == Just name -> Just a
  _ -> Nothing
applyArg1 _ _ = Nothing

-- | The radicand of a sqrt application factor, if it is one.
sqrtRadicand :: SymbolExpr -> Maybe CASValue
sqrtRadicand = applyArg1 "sqrt"

-- | Generic traversals shared by the factor rewriters: does any
-- nested value (application argument, quoted expression) satisfy the
-- check / rewrite every nested value.  Each rewriter recurses only
-- into itself, matching the old per-rule mapTermAll recursion.
symNeedsWorkWith :: (CASValue -> Bool) -> SymbolExpr -> Bool
symNeedsWorkWith p (Apply1 _ a)       = p a
symNeedsWorkWith p (Apply2 _ a b)     = p a || p b
symNeedsWorkWith p (Apply3 _ a b c)   = p a || p b || p c
symNeedsWorkWith p (Apply4 _ a b c d) = p a || p b || p c || p d
symNeedsWorkWith p (Quote q)          = p q
symNeedsWorkWith _ _                  = False

rewriteInsideSymWith :: (CASValue -> CASValue) -> SymbolExpr -> SymbolExpr
rewriteInsideSymWith f sym = case sym of
  Apply1 g a       -> Apply1 g (f a)
  Apply2 g a b     -> Apply2 g (f a) (f b)
  Apply3 g a b c   -> Apply3 g (f a) (f b) (f c)
  Apply4 g a b c d -> Apply4 g (f a) (f b) (f c) (f d)
  Quote q          -> Quote (f q)
  _                -> sym

-- | Rewrite the sqrt factors of one term.  Returns a CASValue because
-- extracted radicand powers can be polynomials.  Nested values
-- (application arguments, quoted expressions, level-4 coefficients)
-- are rewritten first, bottom-up, matching the old mapTermAll
-- recursion of the declare-rule versions.
rewriteSqrtTerm :: CASTerm -> CASValue
rewriteSqrtTerm (CASTerm c0 mono0) =
  let c    = casRewriteSqrt c0
      mono = [ (rewriteInsideSym sym, n) | (sym, n) <- mono0 ]
      -- 1. power reduction on every factor with |n| >= 2
      (powerOuts, mono1) = foldr powerStep ([], []) mono
      powerStep (sym, n) (outs, ms) = case sqrtRadicand sym of
        Just a | abs n >= 2 ->
          let q = n `quot` 2
              r = n - 2 * q
          in (casPower a q : outs, if r == 0 then ms else (sym, r) : ms)
        _ -> (outs, (sym, n) : ms)
      -- 2. pair merge on the remaining exponent-1 sqrt factors
      (sqrtOnes, others) = partitionSqrtOnes mono1
      base = CASPoly [CASTerm c others]
      merged = case sqrtOnes of
        ((sym0, _) : _ : _) ->
          let radicands = map snd sqrtOnes
              product'  = casRewriteSqrt (foldr1 casMult radicands)
              (outside, insides) = splitSquarePart product'
              sqrtAtom r = case sym0 of
                Apply1 fh _ -> CASPoly [CASTerm (CASInteger 1) [(Apply1 fh r, 1)]]
                _           -> error "rewriteSqrtTerm: non-Apply1 sqrt factor"
          in outside : map sqrtAtom insides
        [(sym0, _)] -> [CASPoly [CASTerm (CASInteger 1) [(sym0, 1)]]]
        []          -> []
  in foldl casMult base (powerOuts ++ merged)
 where
  partitionSqrtOnes = foldr step ([], [])
   where
    step (sym, 1) (sq, rest) = case sqrtRadicand sym of
      Just a  -> ((sym, a) : sq, rest)
      Nothing -> (sq, (sym, 1) : rest)
    step f (sq, rest) = (sq, f : rest)

  rewriteInsideSym = rewriteInsideSymWith casRewriteSqrt

-- | Split a merged radicand into (outside, inside radicands) with
-- value = outside^2 * product(insides), extracting square content and
-- canonicalizing the atom forms:
--
--   * single-term radicand: extract the square part of the integer
--     coefficient and the even exponents (sqrt 16 = 4,
--     sqrt (4 x^2 y) = 2 x sqrt y); the remainder stays as ONE atom.
--   * multi-term radicand: extract the integer content's square part,
--     and split the squarefree content off as its OWN integer sqrt
--     atom, leaving a content-free polynomial radicand.  This matches
--     the stable form the old declare-rule + lib-sqrt round trip
--     converged to (sqrt 5 * sqrt(-10 sqrt 5 - 50) =
--     5 * sqrt(-2 sqrt 5 - 10)); without it, algebraically related
--     atoms appear in several content forms and sums that should
--     cancel (5th roots of unity, mini-test 117) do not.
--
-- Anything unsupported stays entirely inside (value-safe).
splitSquarePart :: CASValue -> (CASValue, [CASValue])
splitSquarePart (CASPoly [CASTerm (CASInteger m) mono])
  | m >= 0 =
      let (s, m') = integerSquarePart m
          outsideFactors = [ (sym, k `div` 2) | (sym, k) <- mono, k `div` 2 /= 0 ]
          insideFactors  = [ (sym, k `mod` 2) | (sym, k) <- mono, k `mod` 2 /= 0 ]
          outside = CASPoly [CASTerm (CASInteger s) outsideFactors]
          inside  = casNormalizePoly [CASTerm (CASInteger m') insideFactors]
      in (outside, if inside == CASInteger 1 then [] else [inside])
splitSquarePart (CASInteger m)
  | m >= 0 =
      let (s, m') = integerSquarePart m
      in (CASInteger s, if m' == 1 then [] else [CASInteger m'])
splitSquarePart (CASPoly ts@(_ : _ : _))
  | Just coeffs <- mapM intCoeff ts
  , let g = foldr1 gcd (map abs coeffs)
  , g > 1 =
      let (s, g') = integerSquarePart g
          prim = casNormalizePoly
                   [ CASTerm (CASInteger (c `div` g)) mono | CASTerm (CASInteger c) mono <- ts ]
      in ( CASInteger s
         , (if g' == 1 then [] else [CASInteger g']) ++ [prim] )
 where
  intCoeff (CASTerm (CASInteger c) _) = Just c
  intCoeff _                          = Nothing
splitSquarePart v = (CASInteger 1, [v])

-- | Largest s with s^2 dividing m (m >= 0), by trial division with a
-- divisor cap: square factors hiding behind primes above the cap are
-- left inside (value-safe, just less extraction).
integerSquarePart :: Integer -> (Integer, Integer)
integerSquarePart m0 = go m0 2
 where
  cap = 1000000
  go m p
    | p > cap || p * p > m = (1, m)
    | m `mod` p == 0 =
        let (e, m')   = strip m p 0
            (s, rest) = go m' (p + 1)
        in (p ^ (e `div` 2) * s, p ^ (e `mod` 2) * rest)
    | otherwise = go m (p + 1)
  strip m p e | m `mod` p == 0 = strip (m `div` p) p (e + 1)
              | otherwise      = (e, m)

-- | Rewrite exp factors of every term:
--
--   1. Power reduction: (exp a)^n with n /= 1 becomes exp (n a).
--   2. Product merge: all exp factors of a term merge into one,
--      exp a * exp b = exp (a+b); a zero total collapses to 1
--      (exp x * exp (-x) = 1).
--
-- One pass per term suffices (the merge is closed: it produces at
-- most one exp factor with exponent 1).  Nested values are rewritten
-- first, like casRewriteSqrt.  The value rules exp 0 = 1, exp 1 = e,
-- and exp (n i pi) = (-1)^n stay in the library and fire on the
-- merged atom as before.
casRewriteExp :: CASValue -> CASValue
casRewriteExp (CASFrac num denom)
  | valueNeedsExpWork num || valueNeedsExpWork denom =
      casDivide (rewriteExpPart num) (rewriteExpPart denom)
casRewriteExp v@(CASPoly _)
  | valueNeedsExpWork v = rewriteExpPart v
casRewriteExp v = v

expArg :: SymbolExpr -> Maybe CASValue
expArg = applyArg1 "exp"

-- | A term needs exp work if it has an exp factor with exponent /= 1,
-- at least two exp factors, or nested work.
termNeedsExpWork :: CASTerm -> Bool
termNeedsExpWork (CASTerm c mono) = go 0 mono || valueNeedsExpWork c
 where
  go ones ((sym, n) : rest) = case expArg sym of
    Just _
      | n /= 1    -> True
      | ones >= 1 -> True
      | otherwise -> symNeedsWorkWith valueNeedsExpWork sym || go 1 rest
    Nothing       -> symNeedsWorkWith valueNeedsExpWork sym || go ones rest
  go _ [] = False

valueNeedsExpWork :: CASValue -> Bool
valueNeedsExpWork (CASFrac n d) = valueNeedsExpWork n || valueNeedsExpWork d
valueNeedsExpWork (CASPoly ts)  = any termNeedsExpWork ts
valueNeedsExpWork _             = False

rewriteExpPart :: CASValue -> CASValue
rewriteExpPart (CASPoly ts) =
  let changed   = filter termNeedsExpWork ts
      unchanged = filter (not . termNeedsExpWork) ts
  in foldl casPlus (casNormalizePoly unchanged) (map rewriteExpTerm changed)
rewriteExpPart v = v

rewriteExpTerm :: CASTerm -> CASValue
rewriteExpTerm (CASTerm c0 mono0) =
  let c    = casRewriteExp c0
      mono = [ (rewriteInsideSymWith casRewriteExp sym, n) | (sym, n) <- mono0 ]
      (exps, others) = foldr step ([], []) mono
      step (sym, n) (es, ms) = case expArg sym of
        Just a  -> ((sym, casMult (CASInteger n) a) : es, ms)
        Nothing -> (es, (sym, n) : ms)
      base = CASPoly [CASTerm c others]
  in case exps of
       [] -> base
       ((sym0, _) : _) ->
         let total = foldr1 casPlus (map snd exps)
             atom = case sym0 of
               Apply1 fh _ -> CASPoly [CASTerm (CASInteger 1) [(Apply1 fh total, 1)]]
               _           -> error "rewriteExpTerm: non-Apply1 exp factor"
         in if casIsZero total
              then base
              else casMult base atom

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
