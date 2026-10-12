{-# LANGUAGE TemplateHaskell #-}

-- | The type that GHC inferred for the unannotated 'unconsWith', read at
-- compile time with Template Haskell.
module InferredType (unconsWithType) where

import           Data.List                  (permutations, sort)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (lift)

import           Control.Egison             (CollectionPattern, ElemT, Matcher)
import           Definitions                (unconsWith)

-- | An expression of type (String, Maybe String): the printed inferred type
-- of 'unconsWith' and, unless the type is
--
-- > forall m t. (Matcher m t, CollectionPattern m t) => m -> t -> [(ElemT t, t)]
--
-- up to the names and order of its type variables and constraints, a
-- description of the difference.
unconsWithType :: Q Exp
unconsWithType = do
  inferred <- reifyType 'unconsWith
  lift (pprint inferred, difference inferred)

difference :: Type -> Maybe String
difference inferred@(ForallT binders context body)
  | any expected (permutations (map binderName binders)) = Nothing
  | otherwise = Just ("unexpected inferred type: " ++ show inferred)
  where
    expected [m, t] =
      sort (map show context) == sort (map show [constraint ''Matcher m t, constraint ''CollectionPattern m t])
        && withArrows body == arrow (VarT m) (arrow (VarT t) resultType)
      where
        resultType = AppT ListT (AppT (AppT (TupleT 2) (AppT (ConT ''ElemT) (VarT t))) (VarT t))
    expected _ = False
    constraint name m t = AppT (AppT (ConT name) (VarT m)) (VarT t)
    arrow a b = AppT (AppT ArrowT a) b
    binderName (PlainTV name _)    = name
    binderName (KindedTV name _ _) = name
difference inferred = Just ("the inferred type is not polymorphic: " ++ show inferred)

-- | Write unrestricted function arrows with 'ArrowT', whichever form GHC
-- reports them in.
withArrows :: Type -> Type
withArrows (AppT (AppT (AppT MulArrowT (PromotedT many)) a) b)
  | nameBase many == "Many" = AppT (AppT ArrowT (withArrows a)) (withArrows b)
withArrows (AppT a b) = AppT (withArrows a) (withArrows b)
withArrows t = t
