{- |
Module      : Language.Egison.Type.TypedDesugar
Licence     : MIT

This module implements Phase 8 of the processing flow: TypedDesugar.
It performs type-driven transformations on TIExpr (Typed Internal Expressions).

Type-Driven Transformations (Phase 8):
  1. tensorMap automatic insertion
     - Detect mismatches between Tensor MathExpr and MathExpr
     - Insert tensorMap at appropriate positions
  2. Type class dictionary passing (via TypeClassExpand)
     - Instance selection based on types
     - Method call concretization
  3. Type information optimization and embedding
     - Preserve type info for better error messages during evaluation
     - Each node in TIExpr contains its type

Type information is preserved throughout desugaring, enabling:
  - Better runtime error messages with type information
  - Type-based dispatch during evaluation
  - Debugging support with type annotations
-}

module Language.Egison.Type.TypedDesugar
  ( desugarTypedExprT
  , desugarTypedTopExprT
  ) where

import           Language.Egison.Data       (EvalM)
import           Language.Egison.IExpr      (TIExpr(..), TITopExpr(..))

-- | Desugar a typed expression (TIExpr) with type-driven transformations
-- For now, this is a placeholder that returns the input unchanged.
-- TODO: Implement tensorMap insertion and type class dictionary passing.
desugarTypedExprT :: TIExpr -> EvalM TIExpr
desugarTypedExprT tiexpr = 
  -- For now, just return the input unchanged
  -- Future work: 
  --   1. Traverse tiExpr and detect tensor type mismatches
  --   2. Insert tensorMap where needed
  --   3. Call TypeClassExpand for dictionary passing
  return tiexpr

-- | Desugar a top-level typed expression (TITopExpr)
-- This is the main entry point for Phase 8 transformations.
desugarTypedTopExprT :: TITopExpr -> EvalM (Maybe TITopExpr)
desugarTypedTopExprT topExpr = case topExpr of
  TIDefine scheme var tiexpr -> do
    tiexpr' <- desugarTypedExprT tiexpr
    return $ Just (TIDefine scheme var tiexpr')
  
  TITest tiexpr -> do
    tiexpr' <- desugarTypedExprT tiexpr
    return $ Just (TITest tiexpr')
  
  TIExecute tiexpr -> do
    tiexpr' <- desugarTypedExprT tiexpr
    return $ Just (TIExecute tiexpr')
  
  TILoadFile path -> 
    return $ Just (TILoadFile path)
  
  TILoad lib -> 
    return $ Just (TILoad lib)
  
  TIDefineMany bindings -> do
    bindings' <- mapM (\(var, tiexpr) -> do
      tiexpr' <- desugarTypedExprT tiexpr
      return (var, tiexpr')) bindings
    return $ Just (TIDefineMany bindings')
