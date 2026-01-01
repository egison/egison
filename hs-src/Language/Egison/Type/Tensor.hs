{- |
Module      : Language.Egison.Type.Tensor
Licence     : MIT

This module provides tensor-specific type rules for the Egison type system.
Including:
  - Index contraction (RemoveSupSubPairs)
  - Tensor normalization (Tensor a [] = a)
  - Scalar function lifting to tensors
  - Index merging for tensor products
-}

module Language.Egison.Type.Tensor
  ( -- * Type normalization
    normalizeTensorType
    -- * Index operations
  , mergeIndices
  , removeSupSubPairs
  , renameIndices
  , countPlaceholders
    -- * Shape operations
  , mergeShapes
  , removeShapeDims
    -- * Type rules for tensor operations
  , typeOfContractWith
  , typeOfTensorDot
  , typeOfTensorProduct
  , typeOfDistinctProduct
  , liftScalarBinOp
  , liftScalarUnaryOp
  ) where

import           Data.List                  (partition)

import           Language.Egison.Type.Index
import           Language.Egison.Type.Types

-- | Normalize tensor types
-- Key rule: Tensor a [] = a (0-rank tensor is a scalar)
normalizeTensorType :: Type -> Type
normalizeTensorType (TTensor a (ShapeLit []) []) = a
normalizeTensorType t = t

-- | Merge two index specifications (for tensor product)
mergeIndices :: IndexSpec -> IndexSpec -> IndexSpec
mergeIndices is1 is2 = is1 ++ is2

-- | Remove superscript-subscript pairs from index specification (for contraction)
-- e.g., [~i, _j, _i] -> [_j] (removes ~i and _i pair)
removeSupSubPairs :: IndexSpec -> IndexSpec
removeSupSubPairs [] = []
removeSupSubPairs (i:is) =
  case findAndRemovePair i is of
    Just is' -> removeSupSubPairs is'
    Nothing  -> i : removeSupSubPairs is
  where
    findAndRemovePair :: Index -> IndexSpec -> Maybe IndexSpec
    findAndRemovePair _ [] = Nothing
    findAndRemovePair idx (x:xs)
      | isSupSubPair idx x = Just xs
      | otherwise = (x :) <$> findAndRemovePair idx xs

-- | Rename indices to make them distinct (for !* operator)
-- e.g., [_i, _j] -> [_i', _j']
renameIndices :: IndexSpec -> IndexSpec
renameIndices = map rename
  where
    rename (IndexSym k s) = IndexSym k (s ++ "'")
    rename i = i

-- | Count placeholders in an index specification
countPlaceholders :: IndexSpec -> Int
countPlaceholders = length . filter isPlaceholder

-- | Merge two tensor shapes
mergeShapes :: TensorShape -> TensorShape -> TensorShape
mergeShapes (ShapeLit d1) (ShapeLit d2) = ShapeLit (d1 ++ d2)
mergeShapes (ShapeVar v) _ = ShapeVar v  -- Preserve variable
mergeShapes _ (ShapeVar v) = ShapeVar v
mergeShapes _ _ = ShapeUnknown

-- | Remove dimensions from shape corresponding to contracted indices
removeShapeDims :: TensorShape -> Int -> Int -> TensorShape
removeShapeDims (ShapeLit dims) originalLen newLen =
  let numRemoved = originalLen - newLen
  in ShapeLit $ take (length dims - numRemoved) dims  -- Simplified
removeShapeDims s _ _ = s

-- | Type rule for contractWith
-- contractWith : (a -> a -> a) -> Tensor a is -> Tensor a (RemoveSupSubPairs is)
typeOfContractWith :: Type -> Type -> Either String Type
typeOfContractWith opTy tensorTy = case (opTy, tensorTy) of
  (TFun a1 (TFun a2 a3), TTensor elemTy shape indices)
    | a1 == a2 && a2 == a3 && a1 == elemTy ->
        let newIndices = removeSupSubPairs indices
            newShape = removeShapeDims shape (length indices) (length newIndices)
        in Right $ normalizeTensorType $ TTensor elemTy newShape newIndices
  _ -> Left "contractWith: type mismatch - expected (a -> a -> a) and Tensor a is"

-- | Type rule for tensor dot product (.)
-- (.) : Tensor a is1 -> Tensor a is2 -> Tensor a (RemoveSupSubPairs (mergeIndices is1 is2))
typeOfTensorDot :: Type -> Type -> Either String Type
typeOfTensorDot t1 t2 = case (t1, t2) of
  (TTensor a1 sh1 is1, TTensor a2 sh2 is2)
    | a1 == a2 ->
        let merged = mergeIndices is1 is2
            contracted = removeSupSubPairs merged
            -- Calculate new shape based on contraction
            numContracted = (length merged - length contracted) `div` 2
            newShape = contractedShape sh1 sh2 numContracted
        in Right $ normalizeTensorType $ TTensor a1 newShape contracted
  _ -> Left "(.): type mismatch - expected two tensors of the same element type"
  where
    contractedShape (ShapeLit d1) (ShapeLit d2) n =
      ShapeLit $ take (length d1 - n) d1 ++ take (length d2 - n) d2
    contractedShape _ _ _ = ShapeUnknown

-- | Type rule for tensor product (*)
-- When indices are the same: element-wise multiplication
-- When indices differ: outer product
typeOfTensorProduct :: Type -> Type -> Either String Type
typeOfTensorProduct t1 t2 = case (t1, t2) of
  (TTensor a1 sh1 is1, TTensor a2 sh2 is2)
    | a1 == a2 ->
        if is1 == is2
          then Right $ TTensor a1 sh1 is1  -- Element-wise
          else Right $ TTensor a1 (mergeShapes sh1 sh2) (mergeIndices is1 is2)  -- Outer product
  -- Scalar * Tensor
  (t, TTensor a sh is) | t == a -> Right $ TTensor a sh is
  (TTensor a sh is, t) | t == a -> Right $ TTensor a sh is
  _ -> Left "(*): type mismatch for tensor product"

-- | Type rule for distinct product (!*)
-- !* : Tensor a is1 -> Tensor a is2 -> Tensor a (is1' ++ is2)
-- where is1' has renamed indices
typeOfDistinctProduct :: Type -> Type -> Either String Type
typeOfDistinctProduct t1 t2 = case (t1, t2) of
  (TTensor a1 sh1 is1, TTensor a2 sh2 is2)
    | a1 == a2 ->
        let renamedIs1 = renameIndices is1
        in Right $ TTensor a1 (mergeShapes sh1 sh2) (renamedIs1 ++ is2)
  _ -> Left "(!*): type mismatch - expected two tensors of the same element type"

-- | Lift a scalar binary operation to tensors
-- If op : a -> a -> a, then:
--   op : Tensor a is -> Tensor a is -> Tensor a is (element-wise)
--   op : Tensor a is1 -> Tensor a is2 -> Tensor a (merge is1 is2) (broadcast)
liftScalarBinOp :: Type -> Type -> Type -> Either String Type
liftScalarBinOp opTy t1 t2 = case opTy of
  TFun a (TFun b c) | a == b && b == c -> liftBinOp a t1 t2
  _ -> Left "liftScalarBinOp: expected binary operator type"
  where
    liftBinOp elemTy (TTensor a1 sh1 is1) (TTensor a2 sh2 is2)
      | a1 == a2 && a1 == elemTy =
          if is1 == is2
            then Right $ TTensor a1 sh1 is1
            else Right $ TTensor a1 (mergeShapes sh1 sh2) (mergeIndices is1 is2)
    liftBinOp elemTy (TTensor a sh is) scalar
      | a == elemTy && scalar == elemTy = Right $ TTensor a sh is
    liftBinOp elemTy scalar (TTensor a sh is)
      | a == elemTy && scalar == elemTy = Right $ TTensor a sh is
    liftBinOp _ x y
      | x == y = Right x
      | otherwise = Left "liftScalarBinOp: type mismatch"

-- | Lift a scalar unary operation to tensors
-- If op : a -> b, then op : Tensor a is -> Tensor b is
liftScalarUnaryOp :: Type -> Type -> Either String Type
liftScalarUnaryOp opTy argTy = case opTy of
  TFun a b -> case argTy of
    TTensor elemA sh is
      | elemA == a -> Right $ TTensor b sh is
    t | t == a -> Right b
    _ -> Left "liftScalarUnaryOp: argument type mismatch"
  _ -> Left "liftScalarUnaryOp: expected unary function type"

