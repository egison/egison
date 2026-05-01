{- |
Module      : Language.Egison.Type.RuntimeType
Licence     : MIT

Shallow runtime-type computation for CAS values, used by the runtime-type
dispatch mechanism (see design/runtime-type-dispatch.md).

The shallow `runtimeTypeOfCAS` walks at most two levels of the value: the
outer constructor and one level of inner coefficient/numerator/denominator.
This keeps the operation O(1) at the cost of representing nested
`Frac (Poly Integer [..])` etc. as `Frac MathValue`. Instances that need
finer-grained dispatch should pattern match on the value inside their body.
-}

module Language.Egison.Type.RuntimeType
  ( runtimeTypeOfCAS
  , shallowTypeOfCAS
  ) where

import           Data.List                 (nub, sort)
import           Language.Egison.Math.CAS  (CASTerm (..), CASValue (..),
                                            SymbolExpr (..), prettyCAS)
import           Language.Egison.Type.Types (SymbolSet (..), Type (..),
                                             TypeAtom (..))

-- | Compute the shallow runtime type of a CAS value.
-- Looks at the outer constructor and one level deeper. Inner Poly/Frac are
-- abstracted to TMathValue. See design/runtime-type-dispatch.md §3.
runtimeTypeOfCAS :: CASValue -> Type
runtimeTypeOfCAS (CASInteger _) = TInt
runtimeTypeOfCAS (CASFactor _)  = TFactor
runtimeTypeOfCAS (CASPoly [])   = TInt
runtimeTypeOfCAS (CASPoly [CASTerm c []]) | isCASInteger c = TInt
runtimeTypeOfCAS (CASPoly [term@(CASTerm coef _)]) =
  let atoms = extractAtomsAsTypeAtoms [term]
  in TTerm (shallowTypeOfCAS coef) (SymbolSetClosed atoms)
runtimeTypeOfCAS (CASPoly terms) =
  let coefType = shallowJoinCoefs terms
      atoms    = extractAtomsAsTypeAtoms terms
  in TPoly coefType (SymbolSetClosed atoms)
runtimeTypeOfCAS (CASFrac n d) =
  TFrac (shallowJoinTypes (shallowTypeOfCAS n) (shallowTypeOfCAS d))

-- | Shallow type: return the outer constructor type only. Nested Poly/Frac
-- collapse to TMathValue (the design's depth cap).
shallowTypeOfCAS :: CASValue -> Type
shallowTypeOfCAS (CASInteger _) = TInt
shallowTypeOfCAS (CASFactor _)  = TFactor
shallowTypeOfCAS (CASPoly _)    = TMathValue
shallowTypeOfCAS (CASFrac _ _)  = TMathValue

-- | Predicate: is this CAS value the integer constructor?
isCASInteger :: CASValue -> Bool
isCASInteger (CASInteger _) = True
isCASInteger _              = False

-- | Compute the join of the coefficient types of a list of terms, using
-- shallow types only. Used when the polynomial has multiple terms.
shallowJoinCoefs :: [CASTerm] -> Type
shallowJoinCoefs []    = TInt
shallowJoinCoefs terms =
  foldr1 shallowJoinTypes [shallowTypeOfCAS c | CASTerm c _ <- terms]

-- | Shallow join: combine two outer types. Anything beyond Integer/Factor
-- widens to MathValue, matching the design's coarse-grained join.
shallowJoinTypes :: Type -> Type -> Type
shallowJoinTypes t1 t2
  | t1 == t2                          = t1
  | t1 == TInt && t2 == TFactor       = TFactor
  | t1 == TFactor && t2 == TInt       = TFactor
  | otherwise                         = TMathValue

-- | Collect the distinct atoms appearing in the monomials of a term list,
-- as TypeAtoms suitable for embedding in `SymbolSetClosed`.
extractAtomsAsTypeAtoms :: [CASTerm] -> [TypeAtom]
extractAtomsAsTypeAtoms terms =
  let atoms = [symbolToTypeAtom s | CASTerm _ mono <- terms, (s, _) <- mono]
  in nub (sort atoms)

-- | Convert a SymbolExpr to a TypeAtom for use in symbol sets.
-- Symbols become TANameAtom; Apply1-4 become TAApplyAtom; everything else
-- falls back to TANameAtom of its pretty form.
symbolToTypeAtom :: SymbolExpr -> TypeAtom
symbolToTypeAtom (Symbol _ name _) = TANameAtom name
symbolToTypeAtom (Apply1 fn a1) =
  TAApplyAtom (extractFnName fn) [casValueToTypeAtom a1]
symbolToTypeAtom (Apply2 fn a1 a2) =
  TAApplyAtom (extractFnName fn) [casValueToTypeAtom a1, casValueToTypeAtom a2]
symbolToTypeAtom (Apply3 fn a1 a2 a3) =
  TAApplyAtom (extractFnName fn)
    [casValueToTypeAtom a1, casValueToTypeAtom a2, casValueToTypeAtom a3]
symbolToTypeAtom (Apply4 fn a1 a2 a3 a4) =
  TAApplyAtom (extractFnName fn)
    [casValueToTypeAtom a1, casValueToTypeAtom a2, casValueToTypeAtom a3, casValueToTypeAtom a4]
symbolToTypeAtom other = TANameAtom (show other)

-- | Extract a function name from a CASValue head: typically a Symbol factor.
extractFnName :: CASValue -> String
extractFnName (CASFactor (Symbol _ name _)) = name
extractFnName v                              = prettyCAS v

-- | Convert an arbitrary CASValue argument to a TypeAtom.
-- Integers become TAIntAtom; symbols become TANameAtom; complex values
-- fall back to TANameAtom of their pretty form.
casValueToTypeAtom :: CASValue -> TypeAtom
casValueToTypeAtom (CASInteger n)              = TAIntAtom n
casValueToTypeAtom (CASFactor (Symbol _ s _))  = TANameAtom s
casValueToTypeAtom v                           = TANameAtom (prettyCAS v)
