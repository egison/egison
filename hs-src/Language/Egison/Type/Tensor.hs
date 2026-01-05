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
-- According to type-tensor-simple.md, Tensor MathExpr unifies with MathExpr
-- So Tensor a can unify with a (scalar)
normalizeTensorType :: Type -> Type
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
-- contractWith : (a -> a -> a) -> Tensor a -> Tensor a (or a if contracted to scalar)
-- According to type-tensor-simple.md, Tensor MathExpr unifies with MathExpr
typeOfContractWith :: Type -> Type -> Either String Type
typeOfContractWith opTy tensorTy = case (opTy, tensorTy) of
  (TFun a1 (TFun a2 a3), TTensor elemTy)
    | a1 == a2 && a2 == a3 && a1 == elemTy ->
        -- Contracted result can be Tensor a or a (scalar)
        -- Return Tensor a, unification will handle Tensor a => a
        Right $ TTensor elemTy
  _ -> Left "contractWith: type mismatch - expected (a -> a -> a) and Tensor a"

-- | Type rule for tensor dot product (.)
-- (.) : Tensor a -> Tensor a -> Tensor a (or a if contracted to scalar)
-- According to type-tensor-simple.md, Tensor MathExpr unifies with MathExpr
typeOfTensorDot :: Type -> Type -> Either String Type
typeOfTensorDot t1 t2 = case (t1, t2) of
  (TTensor a1, TTensor a2)
    | a1 == a2 ->
        -- Contracted result can be Tensor a or a (scalar)
        -- Return Tensor a, unification will handle Tensor a => a
        Right $ TTensor a1
  _ -> Left "(.): type mismatch - expected two tensors of the same element type"

-- | Type rule for tensor product (*)
-- (*) : Tensor a -> Tensor a -> Tensor a
-- (*) : a -> Tensor a -> Tensor a (scalar * tensor)
-- (*) : Tensor a -> a -> Tensor a (tensor * scalar)
typeOfTensorProduct :: Type -> Type -> Either String Type
typeOfTensorProduct t1 t2 = case (t1, t2) of
  (TTensor a1, TTensor a2)
    | a1 == a2 -> Right $ TTensor a1
  -- Scalar * Tensor
  (t, TTensor a) | t == a -> Right $ TTensor a
  (TTensor a, t) | t == a -> Right $ TTensor a
  _ -> Left "(*): type mismatch for tensor product"

-- | Type rule for distinct product (!*)
-- !* : Tensor a -> Tensor a -> Tensor a
typeOfDistinctProduct :: Type -> Type -> Either String Type
typeOfDistinctProduct t1 t2 = case (t1, t2) of
  (TTensor a1, TTensor a2)
    | a1 == a2 -> Right $ TTensor a1
  _ -> Left "(!*): type mismatch - expected two tensors of the same element type"

-- | Lift a scalar binary operation to tensors
-- If op : a -> a -> a, then:
--   op : Tensor a -> Tensor a -> Tensor a
--   op : a -> Tensor a -> Tensor a (scalar * tensor)
--   op : Tensor a -> a -> Tensor a (tensor * scalar)
liftScalarBinOp :: Type -> Type -> Type -> Either String Type
liftScalarBinOp opTy t1 t2 = case opTy of
  TFun a (TFun b c) | a == b && b == c -> liftBinOp a t1 t2
  _ -> Left "liftScalarBinOp: expected binary operator type"
  where
    liftBinOp elemTy (TTensor a1) (TTensor a2)
      | a1 == a2 && a1 == elemTy = Right $ TTensor a1
    liftBinOp elemTy (TTensor a) scalar
      | a == elemTy && scalar == elemTy = Right $ TTensor a
    liftBinOp elemTy scalar (TTensor a)
      | a == elemTy && scalar == elemTy = Right $ TTensor a
    liftBinOp _ x y
      | x == y = Right x
      | otherwise = Left "liftScalarBinOp: type mismatch"

-- | Lift a scalar unary operation to tensors
-- If op : a -> b, then op : Tensor a -> Tensor b
liftScalarUnaryOp :: Type -> Type -> Either String Type
liftScalarUnaryOp opTy argTy = case opTy of
  TFun a b -> case argTy of
    TTensor elemA
      | elemA == a -> Right $ TTensor b
    t | t == a -> Right b
    _ -> Left "liftScalarUnaryOp: argument type mismatch"
  _ -> Left "liftScalarUnaryOp: expected unary function type"

