{- |
Module      : Language.Egison.Type.Subtype
Licence     : MIT

Phase beta of the extensible CAS tower (design/type-cas-tower.md D1/D5,
design/type-cas-tower-implementation.md section 3).

This module implements the dynamic subtype order: a structural skeleton
(the design document's inclusion table, complete with coefficient
propagation) extended with user-declared `declare cas-subtype A ⊂ B`
edges, plus the D1 declare-time join-semilattice check:

  * an edge already derivable from the order is redundant (warning; it is
    still stored so its endpoints participate in later checks)
  * an edge whose reverse already holds would collapse the order (error)
  * an edge that leaves some pair of nodes with two incomparable minimal
    upper bounds is ambiguous (error, with suggested completing edges)
  * an edge that places the target strictly below an existing join
    refines that join (warning; refinement monotonicity — values never
    change, static types only become more precise)

The pair enumeration runs over declared nodes (all endpoints ever
declared, including redundant edges) — a finite approximation of the
full scheme-level check, to be refined counterexample-driven
(implementation plan, section 7).

Note: `Type.Join.isSubtype` (used by instance resolution) intentionally
still implements the older, partial skeleton; switching instance
resolution to this module's complete skeleton is deferred until its
dispatch impact is assessed.
-}

module Language.Egison.Type.Subtype
  ( SubtypeEdge
  , SubtypeEnv
  , isCasType
  , skeletonSubtype
  , skeletonJoin
  , isSubtypeWith
  , joinTypesWith
  , EdgeCheck(..)
  , checkEdgeAddition
  ) where

import           Data.List                  (nub)

import           Language.Egison.Type.Join  (symbolSetSubset)
import           Language.Egison.Type.Types (SymbolSet (..), Type (..))

-- | A declared subtype edge (lhs ⊂ rhs), with cas-type aliases expanded.
type SubtypeEdge = (Type, Type)

-- | All declared edges, in declaration order (redundant ones included).
type SubtypeEnv = [SubtypeEdge]

-- | The types the CAS order talks about. Everything else only relates
-- to itself (reflexivity).
isCasType :: Type -> Bool
isCasType TInt         = True
isCasType TMathValue   = True
isCasType TFactor      = True
isCasType (TTerm t _)  = isCasType t
isCasType (TPoly t _)  = isCasType t
isCasType (TFrac t)    = isCasType t
isCasType _            = False

--------------------------------------------------------------------------------
-- Structural skeleton
--------------------------------------------------------------------------------

-- | The structural skeleton subtype relation: the design document's
-- inclusion table (type-cas.md 型の包含関係) with its propagation rules,
-- applied recursively through coefficients:
--
--   * Integer embeds into Factor and into any Frac/Poly/Term whose
--     coefficient admits Integer
--   * Factor embeds where Integer-coefficient polys live
--   * Frac a embeds into Poly b s when Frac a embeds into the
--     coefficient b (constant embedding, e.g. Frac Integer ⊂ Poly (Frac
--     Integer) s)
--   * Term/Poly are covariant in the coefficient with atom-set inclusion
--   * Poly/Term a s ⊂ Frac b when they embed into b (numerator embedding)
--
-- Deliberately NOT included: relating nested and flat canonical forms
-- (e.g. Poly (Poly Integer [i]) [x] vs Poly Integer [i, x]). Those are
-- different canonical forms of the same value set; placing them in the
-- order is exactly what user-declared edges are for (usecase 04/08).
skeletonSubtype :: Type -> Type -> Bool
skeletonSubtype a b | a == b = True
skeletonSubtype a TMathValue = isCasType a
-- Integer
skeletonSubtype TInt TFactor     = True
skeletonSubtype TInt (TFrac t)   = t == TInt || skeletonSubtype TInt t
skeletonSubtype TInt (TPoly t _) = t == TInt || skeletonSubtype TInt t
skeletonSubtype TInt (TTerm t _) = t == TInt || skeletonSubtype TInt t
-- Factor (atomic element; lives wherever Integer coefficients do)
skeletonSubtype TFactor (TPoly t _) = t == TInt || skeletonSubtype TInt t
skeletonSubtype TFactor (TTerm t _) = t == TInt || skeletonSubtype TInt t
skeletonSubtype TFactor (TFrac t)   = skeletonSubtype TFactor t
-- Term
skeletonSubtype (TTerm t1 s1) (TTerm t2 s2) =
  skeletonSubtype t1 t2 && symbolSetSubset s1 s2
skeletonSubtype (TTerm t1 s1) (TPoly t2 s2) =
  skeletonSubtype t1 t2 && symbolSetSubset s1 s2
skeletonSubtype (TTerm t1 s1) (TFrac t2) = skeletonSubtype (TTerm t1 s1) t2
-- Poly
skeletonSubtype (TPoly t1 s1) (TPoly t2 s2) =
  skeletonSubtype t1 t2 && symbolSetSubset s1 s2
skeletonSubtype (TPoly t1 s1) (TFrac t2) = skeletonSubtype (TPoly t1 s1) t2
-- Frac
skeletonSubtype (TFrac t1) (TFrac t2)   = skeletonSubtype t1 t2
skeletonSubtype (TFrac t1) (TPoly t2 _) = skeletonSubtype (TFrac t1) t2
skeletonSubtype (TFrac t1) (TTerm t2 _) = skeletonSubtype (TFrac t1) t2
skeletonSubtype _ _ = False

-- | Skeleton join, following the design join table (type-cas.md
-- join の計算規則), including the documented tower rule
-- level 2 ⊔ level 3 = level 4:
--
--   join(Poly a s, Frac X) = Poly (join a (Frac X)) s   -- X non-Poly
--   join(Poly a s, Frac (Poly b s')) = Frac (Poly (join a b) (s ∪ s'))
--
-- (The legacy 'Type.Join.joinTypes', which returned level 5 for the first
-- case in disagreement with the design, had no callers and was removed.)
skeletonJoin :: Type -> Type -> Maybe Type
skeletonJoin a b
  | not (isCasType a) || not (isCasType b) = Nothing
  | skeletonSubtype a b = Just b
  | skeletonSubtype b a = Just a
skeletonJoin (TTerm t1 s1) (TTerm t2 s2) =
  TTerm <$> skeletonJoin t1 t2 <*> unionSymbolSets s1 s2
skeletonJoin (TTerm t1 s1) p@(TPoly _ _) = skeletonJoin (TPoly t1 s1) p
skeletonJoin p@(TPoly _ _) (TTerm t2 s2) = skeletonJoin p (TPoly t2 s2)
skeletonJoin (TTerm t1 s1) f@(TFrac _) = skeletonJoin (TPoly t1 s1) f
skeletonJoin f@(TFrac _) (TTerm t2 s2) = skeletonJoin f (TPoly t2 s2)
skeletonJoin (TPoly t1 s1) (TPoly t2 s2) =
  TPoly <$> skeletonJoin t1 t2 <*> unionSymbolSets s1 s2
skeletonJoin (TPoly t1 s1) (TFrac t2) = joinPolyFrac t1 s1 t2
skeletonJoin (TFrac t1) (TPoly t2 s2) = joinPolyFrac t2 s2 t1
skeletonJoin (TFrac t1) (TFrac t2) = TFrac <$> skeletonJoin t1 t2
skeletonJoin _ _ = Nothing

-- | join(Poly a s, Frac x): level 4 when x is a scalar domain, level 5
-- when x is itself a Poly (rational-function field).
joinPolyFrac :: Type -> SymbolSet -> Type -> Maybe Type
joinPolyFrac a s (TPoly b s') =
  TFrac <$> (TPoly <$> skeletonJoin a b <*> unionSymbolSets s s')
joinPolyFrac a s x = do
  coeff <- skeletonJoin a (TFrac x)
  return (TPoly coeff s)

-- | Union of atom sets: open absorbs, closed sets take the ordered union.
unionSymbolSets :: SymbolSet -> SymbolSet -> Maybe SymbolSet
unionSymbolSets SymbolSetOpen _ = Just SymbolSetOpen
unionSymbolSets _ SymbolSetOpen = Just SymbolSetOpen
unionSymbolSets (SymbolSetClosed s1) (SymbolSetClosed s2) =
  Just (SymbolSetClosed (nub (s1 ++ s2)))
unionSymbolSets (SymbolSetVar v1) (SymbolSetVar v2)
  | v1 == v2 = Just (SymbolSetVar v1)
  | otherwise = Just SymbolSetOpen
unionSymbolSets (SymbolSetVar _) ss = Just ss
unionSymbolSets ss (SymbolSetVar _) = Just ss

--------------------------------------------------------------------------------
-- Order with declared edges
--------------------------------------------------------------------------------

-- | Subtype in the declared order: skeleton, or a path that climbs
-- through declared edges (entering an edge from anything skeleton-below
-- its source). Worklist over edge targets; terminates because the
-- visited set only grows within the finite edge-target set.
isSubtypeWith :: SubtypeEnv -> Type -> Type -> Bool
isSubtypeWith edges a b = go [a] [a]
  where
    go [] _ = False
    go (t:rest) visited
      | skeletonSubtype t b = True
      | otherwise =
          let nexts = [ y | (x, y) <- edges
                          , skeletonSubtype t x || t == x
                          , y `notElem` visited ]
          in go (rest ++ nexts) (visited ++ nexts)

-- | Join in the declared order: the unique minimal upper bound among the
-- skeleton join and the declared nodes. Ambiguity should be prevented by
-- the declare-time check; if it happens anyway we prefer the skeleton
-- join, and give up otherwise.
joinTypesWith :: SubtypeEnv -> Type -> Type -> Maybe Type
joinTypesWith edges a b =
  case minimals of
    [j] -> Just j
    []  -> Nothing
    js  -> case [ j | j <- js, Just j == skel ] of
             [j] -> Just j
             _   -> Nothing
  where
    skel = skeletonJoin a b
    nodes = nub (concatMap (\(x, y) -> [x, y]) edges)
    uppers = nub ( [ j | Just j <- [skel] ]
                ++ [ n | n <- nodes
                       , isSubtypeWith edges a n
                       , isSubtypeWith edges b n ] )
    minimals = [ u | u <- uppers, not (any (strictlyBelow u) uppers) ]
    strictlyBelow u v =
      v /= u && isSubtypeWith edges v u && not (isSubtypeWith edges u v)

--------------------------------------------------------------------------------
-- D1 declare-time check
--------------------------------------------------------------------------------

-- | Result of checking a new edge against the current order.
data EdgeCheck
  = EdgeRedundant
    -- ^ Already derivable; harmless (store it, warn).
  | EdgeCycle
    -- ^ The reverse relation already holds; adding the edge would
    -- collapse the two types into one order point (rejected).
  | EdgeAmbiguous [(Type, Type, Type)]
    -- ^ Pairs (x, y, oldJoin) whose minimal upper bounds would split
    -- into {oldJoin, target}; suggest declaring oldJoin ⊂ target.
  | EdgeRefines [(Type, Type, Type)]
    -- ^ Pairs (x, y, oldJoin) whose join refines from oldJoin down to
    -- the target (accepted with a warning).
  | EdgeOk
  deriving (Eq, Show)

-- | D1 join-semilattice check for adding edge (a ⊂ b).
-- Pairs are drawn from declared nodes below b in the extended order.
checkEdgeAddition :: SubtypeEnv -> SubtypeEdge -> EdgeCheck
checkEdgeAddition edges (a, b)
  | isSubtypeWith edges a b = EdgeRedundant
  | isSubtypeWith edges b a = EdgeCycle
  | not (null ambiguous)    = EdgeAmbiguous ambiguous
  | not (null refines)      = EdgeRefines refines
  | otherwise               = EdgeOk
  where
    edges' = (a, b) : edges
    nodes = nub ([a, b] ++ concatMap (\(x, y) -> [x, y]) edges)
    below = [ n | n <- nodes, n /= b, isSubtypeWith edges' n b ]
    pairs = [ (x, y) | x <- below, y <- below, x < y ]
    classified = [ (p, c) | p <- pairs, Just c <- [classify p] ]
    ambiguous = [ (x, y, j) | ((x, y), Right j) <- classified ]
    refines   = [ (x, y, j) | ((x, y), Left j)  <- classified ]
    -- Right = ambiguous (old join incomparable with b),
    -- Left  = refinement (b strictly below old join).
    classify (x, y) =
      case joinTypesWith edges x y of
        Nothing -> Nothing            -- no old join: b becomes one (fine)
        Just j
          | isSubtypeWith edges' j b -> Nothing   -- old join stays minimal
          | isSubtypeWith edges' b j -> Just (Left j)
          | otherwise                -> Just (Right j)
