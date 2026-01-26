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
import           Language.Egison.EvalState  (MonadEval(..))
import           Language.Egison.IExpr      (TIExpr(..), TITopExpr(..), extractNameFromVar, stringToVar)
import           Language.Egison.Type.Env   (lookupEnv)
import           Language.Egison.Type.TensorMapInsertion (insertTensorMaps)
import           Language.Egison.Type.TypeClassExpand (expandTypeClassMethodsT, addDictionaryParametersT, applyConcreteConstraintDictionaries)

-- | Desugar a typed expression (TIExpr) with type-driven transformations
-- This function orchestrates the transformation pipeline:
--   1. Insert tensorMap where needed (TensorMapInsertion)
--   2. Expand type class methods (dictionary passing)
--
-- The order matters: tensorMap insertion should happen before type class expansion
-- because after tensorMap insertion, argument types (scalar vs tensor) are determined,
-- which allows type class expansion to use unifyStrict for instance selection.
desugarTypedExprT :: TIExpr -> EvalM TIExpr
desugarTypedExprT tiexpr = do
  -- Step 1: Insert tensorMap where needed
  tiexpr' <- insertTensorMaps tiexpr

  -- Step 2: Expand type class methods (dictionary passing)
  tiexpr'' <- expandTypeClassMethodsT tiexpr'

  return tiexpr''

-- | Desugar a top-level typed expression (TITopExpr)
-- This is the main entry point for Phase 8 transformations.
desugarTypedTopExprT :: TITopExpr -> EvalM (Maybe TITopExpr)
desugarTypedTopExprT topExpr = case topExpr of
  TIDefine scheme var tiexpr -> do
    tiexpr' <- desugarTypedExprT tiexpr
    -- Apply dictionaries to right-hand side if it has concrete type constraints
    tiexpr'' <- applyConcreteConstraintDictionaries tiexpr'
    -- Add dictionary parameters for constrained functions
    tiexpr''' <- addDictionaryParametersT scheme tiexpr''
    return $ Just (TIDefine scheme var tiexpr''')
  
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
      -- Add dictionary parameters using the variable's type scheme from TypeEnv
      -- This is important for dictionary definitions where the expression (hash)
      -- may not have constraints, but the variable has constraints in its type scheme
      typeEnv <- getTypeEnv
      let varName = extractNameFromVar var
          scheme = case lookupEnv (stringToVar varName) typeEnv of
                     Just ts -> ts  -- Use type scheme from environment
                     Nothing -> tiScheme tiexpr'  -- Fallback to expression's scheme
      tiexpr'' <- addDictionaryParametersT scheme tiexpr'
      return (var, tiexpr'')) bindings
    return $ Just (TIDefineMany bindings')
  
  TIDeclareSymbol names ty ->
    -- Symbol declarations don't need type-driven transformations
    return $ Just (TIDeclareSymbol names ty)

