{- |
Module      : Language.Egison.Type.TypedDesugar
Licence     : MIT

This module implements Phase 8 of the processing flow: TypedDesugar.
It orchestrates type-driven transformations on TIExpr (Typed Internal Expressions)
by calling specialized expansion modules.

Type-Driven Transformations (Phase 8):
  1. Type class dictionary passing (via TypeClassExpand)
     - Instance selection based on types
     - Method call concretization
  2. Type information optimization and embedding
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
import           Language.Egison.Type.TypeClassExpand (expandTypeClassMethodsT)

-- | Desugar a typed expression (TIExpr) with type-driven transformations
-- This function orchestrates the transformation pipeline:
--   1. Expand tensor applications (tensorMap insertion)
--   2. Expand type class methods (dictionary passing)
-- 
-- The order matters: tensor expansion should happen before type class expansion
-- because type class methods might operate on tensor types.
desugarTypedExprT :: TIExpr -> EvalM TIExpr
desugarTypedExprT tiexpr = do
  -- Step 1: Expand type class methods (dictionary passing)
  tiexpr'' <- expandTypeClassMethodsT tiexpr
  
  return tiexpr''

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

