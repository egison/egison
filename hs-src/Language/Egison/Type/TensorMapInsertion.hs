{- |
Module      : Language.Egison.Type.TensorMapInsertion
Licence     : MIT

This module implements automatic tensorMap insertion for Phase 8 of the Egison compiler.
When a function expects a scalar type (e.g., Integer) but receives a Tensor type,
this module automatically inserts tensorMap to apply the function element-wise.

According to type-tensor-simple.md:
- If a parameter type cannot unify with Tensor a, and the argument is Tensor a,
  then tensorMap is automatically inserted.
- Type class instances are checked: if Tensor has an instance, no tensorMap is needed.

Example:
  def f (x : Integer) : Integer := x
  def t1 := [| 1, 2 |]
  f t1  --=>  tensorMap (\t1e -> f t1e) t1
-}

module Language.Egison.Type.TensorMapInsertion
  ( insertTensorMaps
  , insertTensorMapsInTopExpr
  ) where

import           Language.Egison.Data       (EvalM)
import           Language.Egison.EvalState  (MonadEval(..))
import           Language.Egison.IExpr      (TIExpr(..), TIExprNode(..), TITopExpr(..), 
                                             Var(..), tiExprType, tiScheme, tiExprNode)
import           Language.Egison.Type.Env   (ClassEnv, lookupInstances, InstanceInfo(..))
import           Language.Egison.Type.Tensor (normalizeTensorType)
import           Language.Egison.Type.Types (Type(..), TypeScheme(..), Constraint(..))
import           Language.Egison.Type.Unify as Unify

--------------------------------------------------------------------------------
-- * TensorMap Insertion Decision Logic
--------------------------------------------------------------------------------

-- | Check if tensorMap should be inserted for an argument
-- This implements the type-tensor-simple.md specification
-- 
-- Arguments:
--   ClassEnv     : The current type class environment (holds available type class instances).
--   [Constraint] : The set of type class constraints in scope (e.g., Num a, Eq a).
--   Type         : The type of the argument being applied to the function.
--   Type         : The type of the parameter as expected by the function (i.e., declared type).
shouldInsertTensorMap :: ClassEnv -> [Constraint] -> Type -> Type -> Bool
shouldInsertTensorMap classEnv constraints argType paramType = case argType of
  TTensor elemType -> case paramType of
    -- Tensor matched with Tensor → no insertion needed
    TTensor _ -> False
    
    -- Tensor matched with type variable → check type class instances
    TVar tyVar -> 
      -- Find constraints on this type variable
      let relevantConstraints = filter (\(Constraint _ t) -> t == TVar tyVar) constraints
      in case relevantConstraints of
           -- No constraints → 0-rank tensor interpretation, no insertion needed
           [] -> False
           -- Has constraints → check if Tensor instance exists for ALL constraints
           cs -> not (all (hasInstanceForTensor classEnv elemType) cs)
                 -- If any constraint lacks a Tensor instance, we need tensorMap
    
    -- Tensor matched with concrete non-tensor type → insertion needed
    _ -> True
    
  -- Non-tensor argument → no insertion needed
  _ -> False

-- | Check if a type class has an instance for Tensor elemType
-- Uses unify to check if an instance type matches the query type
hasInstanceForTensor :: ClassEnv -> Type -> Constraint -> Bool
hasInstanceForTensor classEnv elemType (Constraint className _tyVar) =
  let tensorType = TTensor elemType
      instances = lookupInstances className classEnv
  in any (\inst -> case Unify.unify (instType inst) tensorType of
                     Right _ -> True   -- Instance type unifies with Tensor type
                     Left _  -> False  -- No match
         ) instances

-- | Get the parameter type at the specified index from a function type
-- Example: (a -> b -> c) at index 0 → Just a, at index 1 → Just b
getParamType :: Type -> Int -> Maybe Type
getParamType (TFun param _) 0 = Just param
getParamType (TFun _ rest) n 
  | n > 0 = getParamType rest (n - 1)
getParamType _ _ = Nothing

-- | Apply one argument to a function type
-- Example: (a -> b -> c) → (b -> c)
applyOneArgType :: Type -> Type
applyOneArgType (TFun _ rest) = rest
applyOneArgType t = t  -- No more arguments

--------------------------------------------------------------------------------
-- * TensorMap Insertion Implementation
--------------------------------------------------------------------------------

-- | Insert tensorMap expressions where needed in a TIExpr
-- This is the main entry point for tensorMap insertion
insertTensorMaps :: TIExpr -> EvalM TIExpr
insertTensorMaps tiExpr = do
  classEnv <- getClassEnv
  let scheme = tiScheme tiExpr
  insertTensorMapsInExpr classEnv scheme tiExpr

-- | Insert tensorMap in a top-level expression
insertTensorMapsInTopExpr :: TITopExpr -> EvalM TITopExpr
insertTensorMapsInTopExpr topExpr = case topExpr of
  TIDefine scheme var tiexpr -> do
    tiexpr' <- insertTensorMaps tiexpr
    return $ TIDefine scheme var tiexpr'
  
  TITest tiexpr -> do
    tiexpr' <- insertTensorMaps tiexpr
    return $ TITest tiexpr'
  
  TIExecute tiexpr -> do
    tiexpr' <- insertTensorMaps tiexpr
    return $ TIExecute tiexpr'
  
  TILoadFile path -> 
    return $ TILoadFile path
  
  TILoad lib -> 
    return $ TILoad lib
  
  TIDefineMany bindings -> do
    bindings' <- mapM (\(var, tiexpr) -> do
      tiexpr' <- insertTensorMaps tiexpr
      return (var, tiexpr')) bindings
    return $ TIDefineMany bindings'

-- | Insert tensorMap in a TIExpr with type scheme information
insertTensorMapsInExpr :: ClassEnv -> TypeScheme -> TIExpr -> EvalM TIExpr
insertTensorMapsInExpr classEnv scheme tiExpr = do
  let (Forall _vars constraints _ty) = scheme
  expandedNode <- insertInNode classEnv constraints (tiExprNode tiExpr)
  return $ TIExpr scheme expandedNode
  where
    -- Process a TIExprNode
    insertInNode :: ClassEnv -> [Constraint] -> TIExprNode -> EvalM TIExprNode
    insertInNode env cs node = case node of
      -- Constants and variables: no change needed
      TIConstantExpr c -> return $ TIConstantExpr c
      TIVarExpr name -> return $ TIVarExpr name
      
      -- Lambda expressions: process body
      TILambdaExpr mVar params body -> do
        let (Forall _ bodyConstraints _) = tiScheme body
            allConstraints = cs ++ bodyConstraints
        body' <- insertTensorMapsWithConstraints env allConstraints body
        return $ TILambdaExpr mVar params body'
      
      -- Function application: check if tensorMap is needed
      TIApplyExpr func args -> do
        -- First, recursively process function and arguments
        func' <- insertTensorMapsWithConstraints env cs func
        args' <- mapM (insertTensorMapsWithConstraints env cs) args
        
        -- Check if tensorMap insertion is needed
        let funcType = tiExprType func'
            argTypes = map tiExprType args'
            -- Get constraints from the function's type scheme
            (Forall _ funcConstraints _) = tiScheme func'
            -- Combine parent scope constraints with function constraints
            allConstraints = cs ++ funcConstraints
        
        -- Try to insert tensorMap if needed, using all available constraints
        result <- wrapWithTensorMapIfNeeded env allConstraints func' funcType args' argTypes
        case result of
          Just wrappedNode -> return wrappedNode
          Nothing -> return $ TIApplyExpr func' args'
      
      -- Collections
      TITupleExpr exprs -> do
        exprs' <- mapM (insertTensorMapsWithConstraints env cs) exprs
        return $ TITupleExpr exprs'
      
      TICollectionExpr exprs -> do
        exprs' <- mapM (insertTensorMapsWithConstraints env cs) exprs
        return $ TICollectionExpr exprs'
      
      TIConsExpr h t -> do
        h' <- insertTensorMapsWithConstraints env cs h
        t' <- insertTensorMapsWithConstraints env cs t
        return $ TIConsExpr h' t'
      
      TIJoinExpr l r -> do
        l' <- insertTensorMapsWithConstraints env cs l
        r' <- insertTensorMapsWithConstraints env cs r
        return $ TIJoinExpr l' r'
      
      TIHashExpr pairs -> do
        pairs' <- mapM (\(k, v) -> do
          k' <- insertTensorMapsWithConstraints env cs k
          v' <- insertTensorMapsWithConstraints env cs v
          return (k', v')) pairs
        return $ TIHashExpr pairs'
      
      TIVectorExpr exprs -> do
        exprs' <- mapM (insertTensorMapsWithConstraints env cs) exprs
        return $ TIVectorExpr exprs'
      
      -- Control flow
      TIIfExpr cond thenExpr elseExpr -> do
        cond' <- insertTensorMapsWithConstraints env cs cond
        thenExpr' <- insertTensorMapsWithConstraints env cs thenExpr
        elseExpr' <- insertTensorMapsWithConstraints env cs elseExpr
        return $ TIIfExpr cond' thenExpr' elseExpr'
      
      -- Let bindings
      TILetExpr bindings body -> do
        bindings' <- mapM (\(v, e) -> do
          e' <- insertTensorMapsWithConstraints env cs e
          return (v, e')) bindings
        body' <- insertTensorMapsWithConstraints env cs body
        return $ TILetExpr bindings' body'
      
      TILetRecExpr bindings body -> do
        bindings' <- mapM (\(v, e) -> do
          e' <- insertTensorMapsWithConstraints env cs e
          return (v, e')) bindings
        body' <- insertTensorMapsWithConstraints env cs body
        return $ TILetRecExpr bindings' body'
      
      TISeqExpr e1 e2 -> do
        e1' <- insertTensorMapsWithConstraints env cs e1
        e2' <- insertTensorMapsWithConstraints env cs e2
        return $ TISeqExpr e1' e2'
      
      -- Pattern matching
      TIMatchExpr mode target matcher clauses -> do
        target' <- insertTensorMapsWithConstraints env cs target
        matcher' <- insertTensorMapsWithConstraints env cs matcher
        clauses' <- mapM (\(pat, body) -> do
          body' <- insertTensorMapsWithConstraints env cs body
          return (pat, body')) clauses
        return $ TIMatchExpr mode target' matcher' clauses'
      
      TIMatchAllExpr mode target matcher clauses -> do
        target' <- insertTensorMapsWithConstraints env cs target
        matcher' <- insertTensorMapsWithConstraints env cs matcher
        clauses' <- mapM (\(pat, body) -> do
          body' <- insertTensorMapsWithConstraints env cs body
          return (pat, body')) clauses
        return $ TIMatchAllExpr mode target' matcher' clauses'
      
      -- More lambda-like constructs
      TIMemoizedLambdaExpr vars body -> do
        body' <- insertTensorMapsWithConstraints env cs body
        return $ TIMemoizedLambdaExpr vars body'
      
      TICambdaExpr var body -> do
        body' <- insertTensorMapsWithConstraints env cs body
        return $ TICambdaExpr var body'
      
      TIWithSymbolsExpr syms body -> do
        body' <- insertTensorMapsWithConstraints env cs body
        return $ TIWithSymbolsExpr syms body'
      
      TIDoExpr bindings body -> do
        bindings' <- mapM (\(v, e) -> do
          e' <- insertTensorMapsWithConstraints env cs e
          return (v, e')) bindings
        body' <- insertTensorMapsWithConstraints env cs body
        return $ TIDoExpr bindings' body'
      
      -- Tensor operations
      TITensorMapExpr func tensor -> do
        func' <- insertTensorMapsWithConstraints env cs func
        tensor' <- insertTensorMapsWithConstraints env cs tensor
        return $ TITensorMapExpr func' tensor'
      
      TITensorMap2Expr func t1 t2 -> do
        func' <- insertTensorMapsWithConstraints env cs func
        t1' <- insertTensorMapsWithConstraints env cs t1
        t2' <- insertTensorMapsWithConstraints env cs t2
        return $ TITensorMap2Expr func' t1' t2'
      
      TIGenerateTensorExpr shape func -> do
        shape' <- insertTensorMapsWithConstraints env cs shape
        func' <- insertTensorMapsWithConstraints env cs func
        return $ TIGenerateTensorExpr shape' func'
      
      TITensorExpr shape elems -> do
        shape' <- insertTensorMapsWithConstraints env cs shape
        elems' <- insertTensorMapsWithConstraints env cs elems
        return $ TITensorExpr shape' elems'
      
      TITensorContractExpr tensor -> do
        tensor' <- insertTensorMapsWithConstraints env cs tensor
        return $ TITensorContractExpr tensor'
      
      TITransposeExpr tensor perm -> do
        tensor' <- insertTensorMapsWithConstraints env cs tensor
        perm' <- insertTensorMapsWithConstraints env cs perm
        return $ TITransposeExpr tensor' perm'
      
      TIFlipIndicesExpr tensor -> do
        tensor' <- insertTensorMapsWithConstraints env cs tensor
        return $ TIFlipIndicesExpr tensor'
      
      -- Quote expressions
      TIQuoteExpr e -> do
        e' <- insertTensorMapsWithConstraints env cs e
        return $ TIQuoteExpr e'
      
      TIQuoteSymbolExpr e -> do
        e' <- insertTensorMapsWithConstraints env cs e
        return $ TIQuoteSymbolExpr e'
      
      -- Indexed expressions
      TISubrefsExpr b base ref -> do
        base' <- insertTensorMapsWithConstraints env cs base
        ref' <- insertTensorMapsWithConstraints env cs ref
        return $ TISubrefsExpr b base' ref'
      
      TISuprefsExpr b base ref -> do
        base' <- insertTensorMapsWithConstraints env cs base
        ref' <- insertTensorMapsWithConstraints env cs ref
        return $ TISuprefsExpr b base' ref'
      
      TIUserrefsExpr b base ref -> do
        base' <- insertTensorMapsWithConstraints env cs base
        ref' <- insertTensorMapsWithConstraints env cs ref
        return $ TIUserrefsExpr b base' ref'
      
      -- Other cases
      TIInductiveDataExpr name exprs -> do
        exprs' <- mapM (insertTensorMapsWithConstraints env cs) exprs
        return $ TIInductiveDataExpr name exprs'
      
      TIMatcherExpr patDefs -> return $ TIMatcherExpr patDefs
      
      TIIndexedExpr b base indices -> do
        base' <- insertTensorMapsWithConstraints env cs base
        return $ TIIndexedExpr b base' indices
      
      TIWedgeApplyExpr func args -> do
        func' <- insertTensorMapsWithConstraints env cs func
        args' <- mapM (insertTensorMapsWithConstraints env cs) args
        return $ TIWedgeApplyExpr func' args'
      
      TIFunctionExpr names -> return $ TIFunctionExpr names

-- | Helper to insert tensorMaps in a TIExpr with constraints
insertTensorMapsWithConstraints :: ClassEnv -> [Constraint] -> TIExpr -> EvalM TIExpr
insertTensorMapsWithConstraints env _cs expr = do
  insertTensorMapsInExpr env (tiScheme expr) expr

-- | Wrap function application with tensorMap if needed
-- Returns Just wrappedNode if tensorMap was inserted, Nothing otherwise
wrapWithTensorMapIfNeeded :: ClassEnv -> [Constraint] -> TIExpr -> Type -> [TIExpr] -> [Type] -> EvalM (Maybe TIExprNode)
wrapWithTensorMapIfNeeded classEnv constraints func funcType args argTypes = do
  -- Check if any argument needs tensorMap
  let checks = zipWith (\argType idx -> 
                 case getParamType funcType idx of
                   Just paramType -> shouldInsertTensorMap classEnv constraints argType paramType
                   Nothing -> False
               ) argTypes [0..]
  
  if or checks
    then do
      -- Need to insert tensorMap - use recursive wrapping
      wrapped <- wrapWithTensorMapRecursive classEnv constraints func funcType args argTypes
      return $ Just wrapped
    else
      -- No tensorMap needed
      return Nothing

-- | Recursively wrap function application with tensorMap where needed
-- Process arguments from left to right, building nested tensorMap expressions
wrapWithTensorMapRecursive :: 
    ClassEnv
    -> [Constraint]
    -> TIExpr          -- Current function expression (possibly partially applied)
    -> Type            -- Current function type
    -> [TIExpr]        -- Remaining argument expressions  
    -> [Type]          -- Remaining argument types
    -> EvalM TIExprNode
wrapWithTensorMapRecursive _classEnv _constraints currentFunc _currentType [] [] = do
  -- All arguments processed - return the application
  return $ tiExprNode currentFunc

wrapWithTensorMapRecursive classEnv constraints currentFunc currentType (arg:restArgs) (argType:restArgTypes) = do
  -- Get the expected parameter type
  case getParamType currentType 0 of
    Nothing -> return $ TIApplyExpr currentFunc (arg : restArgs)
    Just paramType -> do
      if shouldInsertTensorMap classEnv constraints argType paramType
        then do
          -- TensorMap insertion needed
          -- Generate a variable name for the lambda (use a simple counter-based name)
          -- For now, use a fixed pattern since we don't have easy access to a counter
          let varName = "tmapVar" ++ show (length restArgs)  -- Use remaining args as counter
              var = Var varName []
              
              -- Extract element type from tensor
              elemType = case argType of
                           TTensor t -> t
                           _ -> argType
              
              varScheme = Forall [] [] elemType
              varTIExpr = TIExpr varScheme (TIVarExpr varName)
              
              -- Build inner expression (recursive call)
              innerType = applyOneArgType currentType
              funcScheme = tiScheme currentFunc
              (Forall _ funcConstraints _) = funcScheme
              innerFuncScheme = Forall [] funcConstraints innerType
              innerFuncTI = TIExpr innerFuncScheme (TIApplyExpr currentFunc [varTIExpr])
          
          -- Process remaining arguments
          innerNode <- wrapWithTensorMapRecursive classEnv constraints innerFuncTI innerType restArgs restArgTypes
          let innerTIExpr = TIExpr innerFuncScheme innerNode
              finalType = tiExprType innerTIExpr
              
              -- Get constraints from inner expression
              (Forall _ innerConstraints _) = tiScheme innerTIExpr
          
          -- Build lambda: \varName -> innerTIExpr
          let lambdaType = TFun elemType finalType
              lambdaScheme = Forall [] innerConstraints lambdaType
              lambdaTI = TIExpr lambdaScheme (TILambdaExpr Nothing [var] innerTIExpr)
          
          return $ TITensorMapExpr lambdaTI arg
        
        else do
          -- No tensorMap needed, normal application
          let appliedType = applyOneArgType currentType
              appliedScheme = Forall [] constraints appliedType
              appliedTI = TIExpr appliedScheme (TIApplyExpr currentFunc [arg])
          
          -- Process remaining arguments (recursive call)
          wrapWithTensorMapRecursive classEnv constraints appliedTI appliedType restArgs restArgTypes

wrapWithTensorMapRecursive _classEnv _constraints currentFunc _currentType _args _argTypes = 
  return $ TIApplyExpr currentFunc []
