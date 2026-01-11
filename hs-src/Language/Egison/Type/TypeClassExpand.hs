{- |
Module      : Language.Egison.Type.TypeClassExpand
Licence     : MIT

This module expands type class method calls using type information from TIExpr.
It transforms TIExpr to TIExpr, replacing type class method calls with
dictionary-based dispatch.

Pipeline: Phase 8 (TypedDesugar) - TypeClassExpand (first step)
This is executed before TensorMapInsertion to resolve type class methods
to concrete functions first.

For example, if we have:
  class Eq a where (==) : a -> a -> Bool
  instance Eq Integer where (==) x y := x = y

Then a call like:
  autoEq 1 2  (with type constraint: Eq Integer)
becomes:
  eqIntegerEq 1 2  (dictionary-based dispatch)

This eliminates the need for runtime dispatch functions like resolveEq.
-}

module Language.Egison.Type.TypeClassExpand
  ( expandTypeClassMethodsT
  , addDictionaryParametersT
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Char                  (toLower, toUpper)
import           Data.List                  (find)
import           Data.Text                  (pack)

import           Language.Egison.AST        (ConstantExpr(..))
import           Language.Egison.Data       (EvalM)
import           Language.Egison.EvalState  (MonadEval(..))
import           Language.Egison.IExpr      (TIExpr(..), TIExprNode(..), IExpr(..), IPattern(..), Var(..), stringToVar,
                                             IBindingExpr, IMatchClause, ILoopRange(..),
                                             Index(..), tiExprConstraints, tiExprType, tiScheme, stripType, tiExprNode)
import           Language.Egison.Type.Env  (ClassEnv(..), ClassInfo(..), InstanceInfo(..),
                                             lookupInstances, lookupClass, generalize, classEnvToList, lookupEnv, TypeEnv)
import           Language.Egison.Type.IInfer (runInferI, defaultInferConfig)
import           Language.Egison.Type.Types (Type(..), TyVar(..), TypeScheme(..), Constraint(..), typeToName, sanitizeMethodName,
                                            capitalizeFirst, lowerFirst, findMatchingInstanceForType, InstanceInfo(..))
import           Language.Egison.Type.Unify (unify)

-- | Expand type class method calls in a typed expression (TIExpr)
-- This function recursively processes TIExpr and replaces type class method calls
-- with dictionary-based dispatch.
expandTypeClassMethodsT :: TIExpr -> EvalM TIExpr
expandTypeClassMethodsT tiExpr = do
  classEnv <- getClassEnv
  let scheme = tiScheme tiExpr
  -- Recursively process the TIExprNode with constraint information
  expandedNode <- expandTIExprNodeWithConstraints classEnv scheme (tiExprNode tiExpr)
  return $ TIExpr scheme expandedNode
  where
    -- Expand TIExprNode with constraint information from TypeScheme
    expandTIExprNodeWithConstraints :: ClassEnv -> TypeScheme -> TIExprNode -> EvalM TIExprNode
    expandTIExprNodeWithConstraints classEnv' (Forall _vars constraints _ty) node =
      expandTIExprNodeWithConstraintList classEnv' constraints node
    
    -- Expand TIExprNode with a list of constraints
    expandTIExprNodeWithConstraintList :: ClassEnv -> [Constraint] -> TIExprNode -> EvalM TIExprNode
    expandTIExprNodeWithConstraintList classEnv' cs node = case node of
      -- Constants and variables: no expansion needed at node level
      -- (TIVarExpr expansion is handled at TIExpr level in expandTIExprWithConstraints)
      TIConstantExpr c -> return $ TIConstantExpr c
      TIVarExpr name -> return $ TIVarExpr name
      
      -- Lambda expressions: process body with constraints
      TILambdaExpr mVar params body -> do
        -- Merge constraints from body's scheme with parent constraints
        let (Forall _ bodyConstraints _) = tiScheme body
            allConstraints = cs ++ bodyConstraints
        body' <- expandTIExprWithConstraints classEnv' allConstraints body
        return $ TILambdaExpr mVar params body'
      
      -- Application: check if it's a method call or constrained function call
      TIApplyExpr func args -> do
        -- First, expand the arguments
        args' <- mapM (expandTIExprWithConstraints classEnv' cs) args
        
        case tiExprNode func of
          TIVarExpr methodName -> do
            -- Try to resolve if func is a method call
            resolved <- tryResolveMethodCall classEnv' cs methodName args'
            case resolved of
              Just result -> return result
              Nothing -> do
                -- Not a method call - process recursively
                -- Note: Dictionary application for constrained functions
                -- is handled in TIVarExpr case of expandTIExprWithConstraints
                func' <- expandTIExprWithConstraints classEnv' cs func
                return $ TIApplyExpr func' args'
          _ -> do
            -- Not a simple variable: process recursively
            func' <- expandTIExprWithConstraints classEnv' cs func
            return $ TIApplyExpr func' args'
      
      -- Collections
      TITupleExpr exprs -> do
        exprs' <- mapM (expandTIExprWithConstraints classEnv' cs) exprs
        return $ TITupleExpr exprs'
      
      TICollectionExpr exprs -> do
        exprs' <- mapM (expandTIExprWithConstraints classEnv' cs) exprs
        return $ TICollectionExpr exprs'
      
      -- Control flow
      TIIfExpr cond thenExpr elseExpr -> do
        cond' <- expandTIExprWithConstraints classEnv' cs cond
        thenExpr' <- expandTIExprWithConstraints classEnv' cs thenExpr
        elseExpr' <- expandTIExprWithConstraints classEnv' cs elseExpr
        return $ TIIfExpr cond' thenExpr' elseExpr'
      
      -- Let bindings
      TILetExpr bindings body -> do
        bindings' <- mapM (\(v, e) -> do
          e' <- expandTIExprWithConstraints classEnv' cs e
          return (v, e')) bindings
        body' <- expandTIExprWithConstraints classEnv' cs body
        return $ TILetExpr bindings' body'
      
      TILetRecExpr bindings body -> do
        bindings' <- mapM (\(v, e) -> do
          e' <- expandTIExprWithConstraints classEnv' cs e
          return (v, e')) bindings
        body' <- expandTIExprWithConstraints classEnv' cs body
        return $ TILetRecExpr bindings' body'
      
      TISeqExpr e1 e2 -> do
        e1' <- expandTIExprWithConstraints classEnv' cs e1
        e2' <- expandTIExprWithConstraints classEnv' cs e2
        return $ TISeqExpr e1' e2'
      
      -- Collections
      TIConsExpr h t -> do
        h' <- expandTIExprWithConstraints classEnv' cs h
        t' <- expandTIExprWithConstraints classEnv' cs t
        return $ TIConsExpr h' t'
      
      TIJoinExpr l r -> do
        l' <- expandTIExprWithConstraints classEnv' cs l
        r' <- expandTIExprWithConstraints classEnv' cs r
        return $ TIJoinExpr l' r'
      
      TIHashExpr pairs -> do
        pairs' <- mapM (\(k, v) -> do
          k' <- expandTIExprWithConstraints classEnv' cs k
          v' <- expandTIExprWithConstraints classEnv' cs v
          return (k', v')) pairs
        return $ TIHashExpr pairs'
      
      TIVectorExpr exprs -> do
        exprs' <- mapM (expandTIExprWithConstraints classEnv' cs) exprs
        return $ TIVectorExpr exprs'
      
      -- More lambda-like constructs
      TIMemoizedLambdaExpr vars body -> do
        body' <- expandTIExprWithConstraints classEnv' cs body
        return $ TIMemoizedLambdaExpr vars body'
      
      TICambdaExpr var body -> do
        body' <- expandTIExprWithConstraints classEnv' cs body
        return $ TICambdaExpr var body'
      
      TIWithSymbolsExpr syms body -> do
        body' <- expandTIExprWithConstraints classEnv' cs body
        return $ TIWithSymbolsExpr syms body'
      
      TIDoExpr bindings body -> do
        bindings' <- mapM (\(v, e) -> do
          e' <- expandTIExprWithConstraints classEnv' cs e
          return (v, e')) bindings
        body' <- expandTIExprWithConstraints classEnv' cs body
        return $ TIDoExpr bindings' body'
      
      -- Pattern matching
      TIMatchExpr mode target matcher clauses -> do
        target' <- expandTIExprWithConstraints classEnv' cs target
        matcher' <- expandTIExprWithConstraints classEnv' cs matcher
        clauses' <- mapM (\(pat, body) -> do
          body' <- expandTIExprWithConstraints classEnv' cs body
          return (pat, body')) clauses
        return $ TIMatchExpr mode target' matcher' clauses'
      
      TIMatchAllExpr mode target matcher clauses -> do
        target' <- expandTIExprWithConstraints classEnv' cs target
        matcher' <- expandTIExprWithConstraints classEnv' cs matcher
        clauses' <- mapM (\(pat, body) -> do
          body' <- expandTIExprWithConstraints classEnv' cs body
          return (pat, body')) clauses
        return $ TIMatchAllExpr mode target' matcher' clauses'
      
      -- Tensor operations
      TITensorMapExpr func tensor -> do
        func' <- expandTIExprWithConstraints classEnv' cs func
        tensor' <- expandTIExprWithConstraints classEnv' cs tensor
        return $ TITensorMapExpr func' tensor'
      
      TITensorMap2Expr func t1 t2 -> do
        func' <- expandTIExprWithConstraints classEnv' cs func
        t1' <- expandTIExprWithConstraints classEnv' cs t1
        t2' <- expandTIExprWithConstraints classEnv' cs t2
        return $ TITensorMap2Expr func' t1' t2'
      
      TIGenerateTensorExpr shape func -> do
        shape' <- expandTIExprWithConstraints classEnv' cs shape
        func' <- expandTIExprWithConstraints classEnv' cs func
        return $ TIGenerateTensorExpr shape' func'
      
      TITensorExpr shape elems -> do
        shape' <- expandTIExprWithConstraints classEnv' cs shape
        elems' <- expandTIExprWithConstraints classEnv' cs elems
        return $ TITensorExpr shape' elems'
      
      TITensorContractExpr tensor -> do
        tensor' <- expandTIExprWithConstraints classEnv' cs tensor
        return $ TITensorContractExpr tensor'
      
      TITransposeExpr tensor perm -> do
        tensor' <- expandTIExprWithConstraints classEnv' cs tensor
        perm' <- expandTIExprWithConstraints classEnv' cs perm
        return $ TITransposeExpr tensor' perm'
      
      TIFlipIndicesExpr tensor -> do
        tensor' <- expandTIExprWithConstraints classEnv' cs tensor
        return $ TIFlipIndicesExpr tensor'
      
      -- Quote expressions
      TIQuoteExpr e -> do
        e' <- expandTIExprWithConstraints classEnv' cs e
        return $ TIQuoteExpr e'
      
      TIQuoteSymbolExpr e -> do
        e' <- expandTIExprWithConstraints classEnv' cs e
        return $ TIQuoteSymbolExpr e'
      
      -- Indexed expressions
      TISubrefsExpr b base ref -> do
        base' <- expandTIExprWithConstraints classEnv' cs base
        ref' <- expandTIExprWithConstraints classEnv' cs ref
        return $ TISubrefsExpr b base' ref'
      
      TISuprefsExpr b base ref -> do
        base' <- expandTIExprWithConstraints classEnv' cs base
        ref' <- expandTIExprWithConstraints classEnv' cs ref
        return $ TISuprefsExpr b base' ref'
      
      TIUserrefsExpr b base ref -> do
        base' <- expandTIExprWithConstraints classEnv' cs base
        ref' <- expandTIExprWithConstraints classEnv' cs ref
        return $ TIUserrefsExpr b base' ref'
      
      -- Other cases: return unchanged for now
      TIInductiveDataExpr name exprs -> do
        exprs' <- mapM (expandTIExprWithConstraints classEnv' cs) exprs
        return $ TIInductiveDataExpr name exprs'
      
      TIMatcherExpr patDefs -> return $ TIMatcherExpr patDefs  -- TODO: Process matcher definitions
      TIIndexedExpr b base indices -> do
        base' <- expandTIExprWithConstraints classEnv' cs base
        return $ TIIndexedExpr b base' indices  -- TODO: Process indices
      
      TIWedgeApplyExpr func args -> do
        func' <- expandTIExprWithConstraints classEnv' cs func
        args' <- mapM (expandTIExprWithConstraints classEnv' cs) args
        return $ TIWedgeApplyExpr func' args'
      
      TIFunctionExpr names -> return $ TIFunctionExpr names  -- Built-in function, no expansion needed
    
    -- Helper: expand a TIExpr with constraints
    expandTIExprWithConstraints :: ClassEnv -> [Constraint] -> TIExpr -> EvalM TIExpr
    expandTIExprWithConstraints classEnv' cs expr = do
      let scheme = tiScheme expr
          (Forall _ exprConstraints _) = scheme
          -- Merge parent constraints with expression's own constraints
          -- This ensures that nested expressions like "contract (* x y)" can resolve
          -- the method call (*) using the constraints from the inner expression
          allConstraints = cs ++ exprConstraints
      
      -- Special handling for TIVarExpr: eta-expand methods or apply dictionaries
      expandedNode <- case tiExprNode expr of
        TIVarExpr varName -> do
          -- Check if this is a type class method
          case findConstraintForMethod classEnv' varName allConstraints of
            Just (Constraint className tyArg) -> do
              -- Get method type to determine arity
              typeEnv <- getTypeEnv
              case lookupEnv varName typeEnv of
                Just (Forall _ _ ty) -> do
                  let arity = getMethodArity ty
                      paramNames = ["etaVar" ++ show i | i <- [1..arity]]
                      paramVars = map stringToVar paramNames
                      paramExprs = map (\n -> TIExpr (Forall [] [] (TVar (TyVar "eta"))) (TIVarExpr n)) paramNames
                      methodKey = sanitizeMethodName varName
                  
                  -- Determine dictionary name based on type
                  case tyArg of
                    TVar (TyVar v) -> do
                      -- Type variable: use dictionary parameter name
                      let dictParamName = "dict_" ++ className ++ "_" ++ v
                          dictType = ty
                          dictExpr = TIExpr (Forall [] [] dictType) (TIVarExpr dictParamName)
                          dictAccess = TIExpr (Forall [] [] dictType) $
                                       TIIndexedExpr False dictExpr
                                         [Sub (IConstantExpr (StringExpr (pack methodKey)))]
                          body = TIExpr (Forall [] [] ty) (TIApplyExpr dictAccess paramExprs)
                      return $ TILambdaExpr Nothing paramVars body
                    _ -> do
                      -- Concrete type: find matching instance
                      let instances = lookupInstances className classEnv'
                      case findMatchingInstanceForType tyArg instances of
                        Just inst -> do
                          -- Found instance: eta-expand with concrete dictionary
                          let instTypeName = typeToName (instType inst)
                              dictName = lowerFirst className ++ instTypeName
                              dictType = ty
                              dictExpr = TIExpr (Forall [] [] dictType) (TIVarExpr dictName)
                              dictAccess = TIExpr (Forall [] [] dictType) $
                                           TIIndexedExpr False dictExpr
                                             [Sub (IConstantExpr (StringExpr (pack methodKey)))]
                              body = TIExpr (Forall [] [] ty) (TIApplyExpr dictAccess paramExprs)
                          return $ TILambdaExpr Nothing paramVars body
                        Nothing -> checkConstrainedVariable
                Nothing -> checkConstrainedVariable
            Nothing -> checkConstrainedVariable
          where
            -- Check if this is a constrained variable (not a method)
            checkConstrainedVariable = do
              if not (null exprConstraints)
                then do
                  -- Check if all constraints are on concrete types
                  let hasOnlyConcreteConstraints = all isConcreteConstraint exprConstraints
                  if hasOnlyConcreteConstraints
                    then do
                      -- This is a constrained variable with concrete types - apply dictionaries
                      dictArgs <- mapM (resolveDictionaryArg classEnv') exprConstraints
                      -- Create application: varName dict1 dict2 ...
                      let varExpr = TIExpr scheme (TIVarExpr varName)
                      return $ TIApplyExpr varExpr dictArgs
                    else
                      -- Has type variable constraints - leave as-is (polymorphic)
                      expandTIExprNodeWithConstraintList classEnv' allConstraints (tiExprNode expr)
                else
                  -- Regular variable, no constraints
                  expandTIExprNodeWithConstraintList classEnv' allConstraints (tiExprNode expr)
            
            isConcreteConstraint (Constraint _ (TVar _)) = False
            isConcreteConstraint _ = True
        _ -> expandTIExprNodeWithConstraintList classEnv' allConstraints (tiExprNode expr)
      
      return $ TIExpr scheme expandedNode
    
    -- Helper to get method arity
    getMethodArity :: Type -> Int
    getMethodArity (TFun _ t2) = 1 + getMethodArity t2
    getMethodArity _ = 0
    
    -- Try to resolve a method call using type class constraints
    -- Dictionary passing: convert method calls to dictionary access
    tryResolveMethodCall :: ClassEnv -> [Constraint] -> String -> [TIExpr] -> EvalM (Maybe TIExprNode)
    tryResolveMethodCall classEnv' cs methodName expandedArgs = do
      -- Find a constraint that provides this method
      case findConstraintForMethod classEnv' methodName cs of
        Nothing -> return Nothing
        Just (Constraint className tyArg) -> do
          -- Look up the class to check if methodName is a method
          case lookupClass className classEnv' of
            Just classInfo -> do
              if methodName `elem` map fst (classMethods classInfo)
                then do
                  -- Try to find an instance for the specific type
                  let instances = lookupInstances className classEnv'
                  -- Use actual argument type if constraint type is a type variable
                  let argTypes = map tiExprType expandedArgs
                      actualType = case (tyArg, argTypes) of
                        (TVar _, (t:_)) -> t  -- Use first argument's type
                        _ -> tyArg
                  -- Skip if actualType is still a type variable
                  case actualType of
                    TVar _ -> return Nothing  -- Cannot resolve with type variable
                    _ -> case findMatchingInstanceForType actualType instances of
                      Just inst -> do
                        -- Found an instance: generate dictionary access
                        -- e.g., numInteger_"plus" for Num Integer instance
                        let instTypeName = typeToName (instType inst)
                            dictName = lowerFirst className ++ instTypeName
                            methodKey = sanitizeMethodName methodName
                        -- Create dictionary access: dictName_"methodKey"
                        let dictType = tiExprType (head expandedArgs)  -- Approximate
                            dictExpr = TIExpr (Forall [] [] dictType) (TIVarExpr dictName)
                            dictAccess = TIExpr (Forall [] [] dictType) $
                                         TIIndexedExpr False dictExpr
                                           [Sub (IConstantExpr (StringExpr (pack methodKey)))]
                        -- Apply arguments: dictAccess arg1 arg2 ...
                        return $ Just $ TIApplyExpr dictAccess expandedArgs
                      Nothing -> return Nothing
                else return Nothing
            Nothing -> return Nothing
    
    -- Find a constraint that provides the given method
    findConstraintForMethod :: ClassEnv -> String -> [Constraint] -> Maybe Constraint
    findConstraintForMethod env methodName cs = 
      find (\(Constraint className _) ->
        case lookupClass className env of
          Just classInfo -> methodName `elem` map fst (classMethods classInfo)
          Nothing -> False
      ) cs
    
    -- Resolve a constraint to a dictionary argument
    resolveDictionaryArg :: ClassEnv -> Constraint -> EvalM TIExpr
    resolveDictionaryArg classEnv (Constraint className tyArg) = do
      -- Only resolve if the type is concrete (not a type variable)
      case tyArg of
        TVar _ -> do
          -- Type variable: cannot determine instance yet
          -- This should not happen if constraints are properly resolved
          -- Return a placeholder that will fail at runtime
          return $ TIExpr (Forall [] [] (TVar (TyVar "error"))) (TIVarExpr ("dict_" ++ className ++ "_unresolved"))
        _ -> do
          -- Concrete type: try to find matching instance
          let instances = lookupInstances className classEnv
          case findMatchingInstanceForType tyArg instances of
            Just inst -> do
              -- Found instance: generate dictionary name (e.g., "numInteger")
              let instTypeName = typeToName (instType inst)
                  dictName = lowerFirst className ++ instTypeName
                  -- Create a reference to the dictionary variable
                  -- Use a generic type for now
                  dictType = TVar (TyVar "dict")
              return $ TIExpr (Forall [] [] dictType) (TIVarExpr dictName)
            Nothing -> do
              -- No instance found - this is an error, but return a dummy for now
              return $ TIExpr (Forall [] [] (TVar (TyVar "error"))) (TIVarExpr "undefined")
    
    -- Check if a name is a type class method
    isTypeClassMethod :: String -> ClassEnv -> Bool
    isTypeClassMethod methName env =
      any (hasMethod methName) (map snd (classEnvToList env))
      where
        hasMethod :: String -> ClassInfo -> Bool
        hasMethod name classInfo = name `elem` map fst (classMethods classInfo)
    
    -- Helper: lowercase first character
    lowerFirst :: String -> String
    lowerFirst [] = []
    lowerFirst (c:cs) = toLower c : cs

{- OLD CODE - DISABLED TEMPORARILY
  where
    -- Helper function to expand IExpr with constraint information
    expandTIExprWithConstraints :: ClassEnv -> TypeScheme -> IExpr -> EvalM IExpr
    expandTIExprWithConstraints classEnv' (Forall _vars constraints _ty) e = 
      expandTIExprWithConstraintList classEnv' constraints e
    
    -- Helper function to expand IExpr with a list of constraints
    expandTIExprWithConstraintList :: ClassEnv -> [Constraint] -> IExpr -> EvalM IExpr
    expandTIExprWithConstraintList classEnv' cs e = case e of
      -- Lambda expressions: pass constraints to body
      ILambdaExpr mVar params body -> do
        body' <- expandTIExprWithConstraintList classEnv' cs body
        return $ ILambdaExpr mVar params body'
      
      -- TensorMap: process lambda body with constraints
      -- Need special handling to infer lambda parameter type from tensor argument
      ITensorMapExpr lambda tensorArg -> do
        -- First, expand the tensor argument
        tensorArg' <- expandTIExprWithConstraintList classEnv' cs tensorArg
        
        -- Infer the type of the tensor argument to extract element type
        typeEnv <- getTypeEnv
        tensorArgTypeM <- liftIO $ runInferI defaultInferConfig typeEnv tensorArg'
        
        case tensorArgTypeM of
          Right (TTensor elemType, _, _) -> do
            -- Now we know lambda parameter has type elemType
            -- Process lambda with this knowledge
            lambda' <- expandLambdaInTensorMap classEnv' cs elemType lambda
            return $ ITensorMapExpr lambda' tensorArg'
          _ -> do
            -- Can't infer type, process normally
            lambda' <- expandTIExprWithConstraintList classEnv' cs lambda
            return $ ITensorMapExpr lambda' tensorArg'
      
      -- TensorMap2: process lambda body with constraints for two tensor arguments
      ITensorMap2Expr lambda tensorArg1 tensorArg2 -> do
        -- Expand tensor arguments
        tensorArg1' <- expandTIExprWithConstraintList classEnv' cs tensorArg1
        tensorArg2' <- expandTIExprWithConstraintList classEnv' cs tensorArg2
        
        -- Infer element types from tensor arguments
        typeEnv <- getTypeEnv
        tensorArg1TypeM <- liftIO $ runInferI defaultInferConfig typeEnv tensorArg1'
        
        case tensorArg1TypeM of
          Right (TTensor elemType, _, _) -> do
            -- Process lambda knowing first parameter has type elemType
            lambda' <- expandLambdaInTensorMap classEnv' cs elemType lambda
            return $ ITensorMap2Expr lambda' tensorArg1' tensorArg2'
          _ -> do
            -- Can't infer type, process normally
            lambda' <- expandTIExprWithConstraintList classEnv' cs lambda
            return $ ITensorMap2Expr lambda' tensorArg1' tensorArg2'
      
      -- TensorContract: process expression with constraints
      ITensorContractExpr tensorExpr -> do
        tensorExpr' <- expandTIExprWithConstraintList classEnv' cs tensorExpr
        return $ ITensorContractExpr tensorExpr'
      
      -- Method call: try to resolve using constraints
      IApplyExpr (IVarExpr methodName) args -> do
        -- Try to resolve method call using constraints
        typeEnv <- getTypeEnv
        let tryResolve = do
              -- Find a constraint that provides this method
              let matchingConstraint = findConstraintForMethod methodName cs
              case matchingConstraint of
                Nothing -> return Nothing
                Just (Constraint className _) -> do
                  if null args
                    then return Nothing
                    else do
                      -- Infer the first argument's type
                      argResult <- liftIO $ runInferI defaultInferConfig typeEnv (head args)
                      -- Get argument type: either from inference or from constraint
                      let argTypeM = case argResult of
                            Right (argType, _, _) -> Just argType
                            Left _ -> 
                              -- If inference fails (e.g., unbound variable in lambda),
                              -- we can't resolve polymorphic types without full dictionary passing
                              Nothing
                      case argTypeM of
                        Nothing -> return Nothing
                        Just argType -> do
                          -- Find matching instance
                          let instances = lookupInstances className classEnv'
                          case findMatchingInst argType instances of
                            Just inst -> do
                              -- Generate the instance method name using sanitization
                              let sanitized = sanitizeMethodName methodName
                                  instTypeName = typeToName (instType inst)
                                  instanceMethodName = lowerFirst className ++ instTypeName ++ capitalizeFirst sanitized
                              -- Recursively expand arguments
                              args' <- mapM (expandTIExprWithConstraintList classEnv' cs) args
                              return $ Just $ IApplyExpr (IVarExpr instanceMethodName) args'
                            Nothing -> return Nothing
        resolved <- tryResolve
        case resolved of
          Just resolvedExpr -> return resolvedExpr
          Nothing -> do
            -- Not a method call, check if it's a constrained function call
            -- This is where we insert dictionaries for calls like `double 2`
            case lookupEnv methodName typeEnv of
              Just (Forall _vars funcConstraints _ty) | not (null funcConstraints) -> do
                -- Constrained function: insert dictionary arguments
                dictArgs <- mapM (resolveDictionary classEnv' args) funcConstraints
                case sequence dictArgs of
                  Just dicts -> do
                    -- Successfully resolved all dictionaries
                    args' <- mapM (expandTIExprWithConstraintList classEnv' cs) args
                    return $ IApplyExpr (IVarExpr methodName) (dicts ++ args')
                  Nothing -> do
                    -- Could not resolve dictionaries, process normally
                    func' <- expandTIExprWithConstraintList classEnv' cs (IVarExpr methodName)
                    args' <- mapM (expandTIExprWithConstraintList classEnv' cs) args
                    return $ IApplyExpr func' args'
              _ -> do
                -- Fall back to normal processing
                func' <- expandTIExprWithConstraintList classEnv' cs (IVarExpr methodName)
                args' <- mapM (expandTIExprWithConstraintList classEnv' cs) args
                return $ IApplyExpr func' args'
      
      -- Other expressions: use original expandTIExpr
      _ -> expandTIExpr classEnv' e
      where
        -- Helper to expand lambda in tensorMap context where we know parameter type
        expandLambdaInTensorMap :: ClassEnv -> [Constraint] -> Type -> IExpr -> EvalM IExpr
        expandLambdaInTensorMap env cs paramType lambdaExpr = case lambdaExpr of
          ILambdaExpr mVar params body -> do
            -- Process body, knowing that the first parameter has type paramType
            -- When processing function calls in body, we can resolve dictionaries
            body' <- expandBodyWithParamType env cs paramType body
            return $ ILambdaExpr mVar params body'
          _ -> expandTIExprWithConstraintList env cs lambdaExpr
        
        -- Helper to expand lambda body where first parameter has known type
        expandBodyWithParamType :: ClassEnv -> [Constraint] -> Type -> IExpr -> EvalM IExpr
        expandBodyWithParamType env cs paramType body = case body of
          -- Function application: check if it's a method or constrained function
          IApplyExpr (IVarExpr funcName) args -> do
            typeEnv <- getTypeEnv
            
            -- First, check if it's a type class method
            let isMethod = any (constraintHasMeth funcName) cs
            
            if isMethod && not (null args)
              then do
                -- It's a method call - use paramType for first argument
                let className = case filter (constraintHasMeth funcName) cs of
                      (Constraint cn _:_) -> cn
                      [] -> ""
                let instances = lookupInstances className env
                case findMatchingInstanceForType paramType instances of
                  Just inst -> do
                    -- Generate instance method name
                    let sanitized = sanitizeMethodName funcName
                        instTypeName = typeToName (instType inst)
                        instanceMethodName = lowerFirst className ++ instTypeName ++ capitalizeFirst sanitized
                    -- Recursively expand arguments
                    args' <- mapM (expandBodyWithParamType env cs paramType) args
                    return $ IApplyExpr (IVarExpr instanceMethodName) args'
                  Nothing -> do
                    -- Can't resolve, try as constrained function
                    tryConstrainedFunction
              else
                -- Not a method, try as constrained function
                tryConstrainedFunction
            where
              tryConstrainedFunction = do
                typeEnv <- getTypeEnv
                case lookupEnv funcName typeEnv of
                  Just (Forall _vars funcConstraints _ty) | not (null funcConstraints) -> do
                    -- This is a constrained function, resolve dictionaries
                    dictArgs <- mapM (resolveDictForArg env args paramType) funcConstraints
                    case sequence dictArgs of
                      Just dicts -> do
                        args' <- mapM (expandBodyWithParamType env cs paramType) args
                        return $ IApplyExpr (IVarExpr funcName) (dicts ++ args')
                      Nothing -> do
                        -- Fall back to normal processing
                        expandTIExprWithConstraintList env cs body
                  _ -> do
                    -- Not constrained, but recursively process arguments
                    args' <- mapM (expandBodyWithParamType env cs paramType) args
                    return $ IApplyExpr (IVarExpr funcName) args'
          
          -- Nested application: recursively process
          IApplyExpr func args -> do
            func' <- expandBodyWithParamType env cs paramType func
            args' <- mapM (expandBodyWithParamType env cs paramType) args
            return $ IApplyExpr func' args'
          
          -- TensorMap: process recursively
          ITensorMapExpr lambda tensorArg -> do
            lambda' <- expandBodyWithParamType env cs paramType lambda
            tensorArg' <- expandBodyWithParamType env cs paramType tensorArg
            return $ ITensorMapExpr lambda' tensorArg'
          
          -- Lambda: process body recursively
          ILambdaExpr mVar params lambdaBody -> do
            lambdaBody' <- expandBodyWithParamType env cs paramType lambdaBody
            return $ ILambdaExpr mVar params lambdaBody'
          
          _ -> expandTIExprWithConstraintList env cs body
        
        -- Resolve dictionary for argument, using known parameter type if available
        resolveDictForArg :: ClassEnv -> [IExpr] -> Type -> Constraint -> EvalM (Maybe IExpr)
        resolveDictForArg env args knownParamType (Constraint className _) = do
          -- Check if any argument is a variable (lambda parameter), use known type
          typeEnv <- getTypeEnv
          let argType = case args of
                -- Single argument that's a variable
                [IVarExpr _] -> Just knownParamType
                -- Multiple arguments - check if any is a variable, use known type
                _ | any isVarExpr args -> Just knownParamType
                _ -> Nothing
          
          case argType of
            Just t -> do
              let instances = lookupInstances className env
              case findMatchingInstanceForType t instances of
                Just inst -> do
                  let instTypeName = typeToName (instType inst)
                      dictName = lowerFirst className ++ instTypeName
                  return $ Just $ IVarExpr dictName
                Nothing -> return Nothing
            Nothing -> return Nothing
        
        isVarExpr :: IExpr -> Bool
        isVarExpr (IVarExpr _) = True
        isVarExpr _ = False
        
        findConstraintForMethod :: String -> [Constraint] -> Maybe Constraint
        findConstraintForMethod methName constraints = 
          case filter (constraintHasMeth methName) constraints of
            (c:_) -> Just c
            [] -> Nothing
        
        constraintHasMeth :: String -> Constraint -> Bool
        constraintHasMeth methName (Constraint clsName _) =
          case lookupClass clsName classEnv' of
            Just classInfo -> methName `elem` map fst (classMethods classInfo)
            Nothing -> False
        
        findMatchingInst :: Type -> [InstanceInfo] -> Maybe InstanceInfo
        findMatchingInst _ [] = Nothing
        findMatchingInst targetType (inst:insts) =
          case unify (instType inst) targetType of
            Right _ -> Just inst
            Left _ -> findMatchingInst targetType insts
        
-- Helper function to recursively process IExpr and return IExpr
    expandTIExpr :: ClassEnv -> IExpr -> EvalM IExpr
    expandTIExpr classEnv' e = case e of
      -- Constants and variables (no sub-expressions)
      IConstantExpr c -> return $ IConstantExpr c
      IVarExpr name -> return $ IVarExpr name
        -- Note: Eta-expansion of type class methods is handled in addDictionaryParametersT
        -- via replaceMethodCallsWithDictAccess, not here to avoid infinite loops
      
      -- Indexed expressions
      IIndexedExpr b expr1 indices -> do
        expr1' <- expandTIExpr classEnv' expr1
        indices' <- mapM (expandIndex classEnv') indices
        return $ IIndexedExpr b expr1' indices'
        where
          expandIndex :: ClassEnv -> Index IExpr -> EvalM (Index IExpr)
          expandIndex _ (DF i1 i2) = return $ DF i1 i2
          expandIndex env (Sub e) = do
            e' <- expandTIExpr env e
            return $ Sub e'
          expandIndex env (Sup e) = do
            e' <- expandTIExpr env e
            return $ Sup e'
          expandIndex env (MultiSub e1 n e2) = do
            e1' <- expandTIExpr env e1
            e2' <- expandTIExpr env e2
            return $ MultiSub e1' n e2'
          expandIndex env (MultiSup e1 n e2) = do
            e1' <- expandTIExpr env e1
            e2' <- expandTIExpr env e2
            return $ MultiSup e1' n e2'
          expandIndex env (SupSub e) = do
            e' <- expandTIExpr env e
            return $ SupSub e'
          expandIndex env (User e) = do
            e' <- expandTIExpr env e
            return $ User e'
      
      ISubrefsExpr b expr1 expr2 -> do
        expr1' <- expandTIExpr classEnv' expr1
        expr2' <- expandTIExpr classEnv' expr2
        return $ ISubrefsExpr b expr1' expr2'
      
      ISuprefsExpr b expr1 expr2 -> do
        expr1' <- expandTIExpr classEnv' expr1
        expr2' <- expandTIExpr classEnv' expr2
        return $ ISuprefsExpr b expr1' expr2'
      
      IUserrefsExpr b expr1 expr2 -> do
        expr1' <- expandTIExpr classEnv' expr1
        expr2' <- expandTIExpr classEnv' expr2
        return $ IUserrefsExpr b expr1' expr2'
      
      -- Data constructors
      IInductiveDataExpr name exprs -> do
        exprs' <- mapM (expandTIExpr classEnv') exprs
        return $ IInductiveDataExpr name exprs'
  
  -- Collections
      ITupleExpr exprs -> do
        exprs' <- mapM (expandTIExpr classEnv') exprs
        return $ ITupleExpr exprs'
      
      ICollectionExpr exprs -> do
        exprs' <- mapM (expandTIExpr classEnv') exprs
        return $ ICollectionExpr exprs'
      
      IConsExpr expr1 expr2 -> do
        expr1' <- expandTIExpr classEnv' expr1
        expr2' <- expandTIExpr classEnv' expr2
        return $ IConsExpr expr1' expr2'
      
      IJoinExpr expr1 expr2 -> do
        expr1' <- expandTIExpr classEnv' expr1
        expr2' <- expandTIExpr classEnv' expr2
        return $ IJoinExpr expr1' expr2'
      
      IHashExpr pairs -> do
        pairs' <- mapM (\(k, v) -> do
          k' <- expandTIExpr classEnv' k
          v' <- expandTIExpr classEnv' v
          return (k', v')) pairs
        return $ IHashExpr pairs'
      
      IVectorExpr exprs -> do
        exprs' <- mapM (expandTIExpr classEnv') exprs
        return $ IVectorExpr exprs'
  
  -- Lambda expressions
      ILambdaExpr mVar params body -> do
        body' <- expandTIExpr classEnv' body
        return $ ILambdaExpr mVar params body'
      
      IMemoizedLambdaExpr args body -> do
        body' <- expandTIExpr classEnv' body
        return $ IMemoizedLambdaExpr args body'
      
      ICambdaExpr var body -> do
        body' <- expandTIExpr classEnv' body
        return $ ICambdaExpr var body'
  
  -- Control flow
      IIfExpr cond thenExpr elseExpr -> do
        cond' <- expandTIExpr classEnv' cond
        thenExpr' <- expandTIExpr classEnv' thenExpr
        elseExpr' <- expandTIExpr classEnv' elseExpr
        return $ IIfExpr cond' thenExpr' elseExpr'
      
      -- Let expressions
      ILetRecExpr bindings body -> do
        bindings' <- mapM (expandBinding classEnv') bindings
        body' <- expandTIExpr classEnv' body
        return $ ILetRecExpr bindings' body'
      
      ILetExpr bindings body -> do
        bindings' <- mapM (expandBinding classEnv') bindings
        body' <- expandTIExpr classEnv' body
        return $ ILetExpr bindings' body'
      
      IWithSymbolsExpr syms body -> do
        body' <- expandTIExpr classEnv' body
        return $ IWithSymbolsExpr syms body'
  
  -- Pattern matching
      IMatchExpr mode target matcher clauses -> do
        target' <- expandTIExpr classEnv' target
        matcher' <- expandTIExpr classEnv' matcher
        clauses' <- mapM (expandMatchClause classEnv') clauses
        return $ IMatchExpr mode target' matcher' clauses'
      
      IMatchAllExpr mode target matcher clauses -> do
        target' <- expandTIExpr classEnv' target
        matcher' <- expandTIExpr classEnv' matcher
        clauses' <- mapM (expandMatchClause classEnv') clauses
        return $ IMatchAllExpr mode target' matcher' clauses'
      
      IMatcherExpr patDefs -> do
        patDefs' <- mapM (\(pat, expr, bindings) -> do
          expr' <- expandTIExpr classEnv' expr
          bindings' <- mapM (expandBinding classEnv') bindings
          return (pat, expr', bindings')) patDefs
        return $ IMatcherExpr patDefs'
      
      -- Quote expressions
      IQuoteExpr expr1 -> do
        expr1' <- expandTIExpr classEnv' expr1
        return $ IQuoteExpr expr1'
      
      IQuoteSymbolExpr expr1 -> do
        expr1' <- expandTIExpr classEnv' expr1
        return $ IQuoteSymbolExpr expr1'
      
      -- Application
      IWedgeApplyExpr func args -> do
        func' <- expandTIExpr classEnv' func
        args' <- mapM (expandTIExpr classEnv') args
        return $ IWedgeApplyExpr func' args'
      
      IDoExpr bindings body -> do
        bindings' <- mapM (expandBinding classEnv') bindings
        body' <- expandTIExpr classEnv' body
        return $ IDoExpr bindings' body'
      
      ISeqExpr expr1 expr2 -> do
        expr1' <- expandTIExpr classEnv' expr1
        expr2' <- expandTIExpr classEnv' expr2
        return $ ISeqExpr expr1' expr2'
      
      -- Function application
      IApplyExpr func args -> do
        func' <- expandTIExpr classEnv' func
        args' <- mapM (expandTIExpr classEnv') args
        return $ IApplyExpr func' args'
  
  -- Tensor operations
      IGenerateTensorExpr expr1 expr2 -> do
        expr1' <- expandTIExpr classEnv' expr1
        expr2' <- expandTIExpr classEnv' expr2
        return $ IGenerateTensorExpr expr1' expr2'
      
      ITensorExpr expr1 expr2 -> do
        expr1' <- expandTIExpr classEnv' expr1
        expr2' <- expandTIExpr classEnv' expr2
        return $ ITensorExpr expr1' expr2'
      
      ITensorContractExpr expr1 -> do
        expr1' <- expandTIExpr classEnv' expr1
        return $ ITensorContractExpr expr1'
      
      ITensorMapExpr expr1 expr2 -> do
        expr1' <- expandTIExpr classEnv' expr1
        expr2' <- expandTIExpr classEnv' expr2
        return $ ITensorMapExpr expr1' expr2'
      
      ITensorMap2Expr expr1 expr2 expr3 -> do
        expr1' <- expandTIExpr classEnv' expr1
        expr2' <- expandTIExpr classEnv' expr2
        expr3' <- expandTIExpr classEnv' expr3
        return $ ITensorMap2Expr expr1' expr2' expr3'
      
      ITransposeExpr expr1 expr2 -> do
        expr1' <- expandTIExpr classEnv' expr1
        expr2' <- expandTIExpr classEnv' expr2
        return $ ITransposeExpr expr1' expr2'
      
      IFlipIndicesExpr expr1 -> do
        expr1' <- expandTIExpr classEnv' expr1
        return $ IFlipIndicesExpr expr1'
      
      -- Function reference
      IFunctionExpr names -> return $ IFunctionExpr names
      where
        -- Check if a name is a type class method
        isTypeClassMethod :: String -> ClassEnv -> Bool
        isTypeClassMethod methName env =
          any (hasMethod methName) (map snd (classEnvToList env))
          where
            hasMethod :: String -> ClassInfo -> Bool
            hasMethod name classInfo = name `elem` map fst (classMethods classInfo)
        
        -- Eta-expand a type class method: + → \x y -> x + y
        -- The generated lambda will be processed by existing lambda handling code
        etaExpandMethod :: String -> TypeScheme -> EvalM IExpr
        etaExpandMethod methName (Forall _ _ ty) = do
          let arity = getTypeArity ty
              paramNames = ["etaVar" ++ show i | i <- [1..arity]]
              paramVars = map stringToVar paramNames
              paramExprs = map (IVarExpr . show) paramVars
              -- Create: methName etaVar1 etaVar2 ... etaVarN
              -- Important: IApplyExpr takes a list of all arguments at once
              body = IApplyExpr (IVarExpr methName) paramExprs
              -- Create: \etaVar1 etaVar2 ... etaVarN -> body
              lambda = ILambdaExpr Nothing paramVars body
          -- Expand the lambda (this will handle dictionary insertion)
          expandTIExpr classEnv' lambda
        
        -- Get the arity (number of parameters) of a function type
        getTypeArity :: Type -> Int
        getTypeArity (TFun _ t2) = 1 + getTypeArity t2
        getTypeArity _ = 0
    
    -- Helper function to process bindings
    expandBinding :: ClassEnv -> IBindingExpr -> EvalM IBindingExpr
    expandBinding classEnv' (pat, expr) = do
      expr' <- expandTIExpr classEnv' expr
      return (pat, expr')
    
    -- Helper function to process match clauses
    expandMatchClause :: ClassEnv -> IMatchClause -> EvalM IMatchClause
    expandMatchClause classEnv' (pat, expr) = do
      pat' <- expandPattern classEnv' pat
      expr' <- expandTIExpr classEnv' expr
      return (pat', expr')
    
    -- Helper function to process patterns
    expandPattern :: ClassEnv -> IPattern -> EvalM IPattern
    expandPattern classEnv' pat = case pat of
      IValuePat expr -> do
        expr' <- expandTIExpr classEnv' expr
        return $ IValuePat expr'
      IPredPat expr -> do
        expr' <- expandTIExpr classEnv' expr
        return $ IPredPat expr'
      IIndexedPat pat1 exprs -> do
        pat1' <- expandPattern classEnv' pat1
        exprs' <- mapM (expandTIExpr classEnv') exprs
        return $ IIndexedPat pat1' exprs'
      ILetPat bindings pat1 -> do
        bindings' <- mapM (expandBinding classEnv') bindings
        pat1' <- expandPattern classEnv' pat1
        return $ ILetPat bindings' pat1'
      INotPat pat1 -> do
        pat1' <- expandPattern classEnv' pat1
        return $ INotPat pat1'
      IAndPat pat1 pat2 -> do
        pat1' <- expandPattern classEnv' pat1
        pat2' <- expandPattern classEnv' pat2
        return $ IAndPat pat1' pat2'
      IOrPat pat1 pat2 -> do
        pat1' <- expandPattern classEnv' pat1
        pat2' <- expandPattern classEnv' pat2
        return $ IOrPat pat1' pat2'
      IForallPat pat1 pat2 -> do
        pat1' <- expandPattern classEnv' pat1
        pat2' <- expandPattern classEnv' pat2
        return $ IForallPat pat1' pat2'
      ITuplePat pats -> do
        pats' <- mapM (expandPattern classEnv') pats
        return $ ITuplePat pats'
      IInductivePat name pats -> do
        pats' <- mapM (expandPattern classEnv') pats
        return $ IInductivePat name pats'
      ILoopPat name range pat1 pat2 -> do
        range' <- expandLoopRange classEnv' range
        pat1' <- expandPattern classEnv' pat1
        pat2' <- expandPattern classEnv' pat2
        return $ ILoopPat name range' pat1' pat2'
      IPApplyPat expr pats -> do
        expr' <- expandTIExpr classEnv' expr
        pats' <- mapM (expandPattern classEnv') pats
        return $ IPApplyPat expr' pats'
      IDApplyPat pat1 pats -> do
        pat1' <- expandPattern classEnv' pat1
        pats' <- mapM (expandPattern classEnv') pats
        return $ IDApplyPat pat1' pats'
      -- Patterns without IExpr
      IWildCard -> return IWildCard
      IPatVar name -> return $ IPatVar name
      IContPat -> return IContPat
      IVarPat name -> return $ IVarPat name
      IInductiveOrPApplyPat name pats -> do
        pats' <- mapM (expandPattern classEnv') pats
        return $ IInductiveOrPApplyPat name pats'
      ISeqNilPat -> return ISeqNilPat
      ISeqConsPat pat1 pat2 -> do
        pat1' <- expandPattern classEnv' pat1
        pat2' <- expandPattern classEnv' pat2
        return $ ISeqConsPat pat1' pat2'
      ILaterPatVar -> return ILaterPatVar
    
    -- Helper function to process loop ranges
    expandLoopRange :: ClassEnv -> ILoopRange -> EvalM ILoopRange
    expandLoopRange classEnv' (ILoopRange expr1 expr2 pat) = do
      expr1' <- expandTIExpr classEnv' expr1
      expr2' <- expandTIExpr classEnv' expr2
      pat' <- expandPattern classEnv' pat
      return $ ILoopRange expr1' expr2' pat'

-- | Resolve a dictionary for a constraint given the function arguments
-- Returns the dictionary expression (e.g., IVarExpr "numInteger")
resolveDictionary :: ClassEnv -> [IExpr] -> Constraint -> EvalM (Maybe IExpr)
resolveDictionary classEnv args (Constraint className constraintType) = do
  -- Infer argument types to determine the concrete type
  typeEnv <- getTypeEnv
  if null args
    then return Nothing
    else do
      -- Infer the first argument's type
      argResult <- liftIO $ runInferI defaultInferConfig typeEnv (head args)
      case argResult of
        Left _ -> return Nothing
        Right (argType, _, _) -> do
          -- Find matching instance
          let instances = lookupInstances className classEnv
          case findMatchingInstanceForType argType instances of
            Just inst -> do
              -- Generate dictionary name: e.g., "numInteger" for Num Integer
              let instTypeName = typeToName (instType inst)
                  dictName = lowerFirst className ++ instTypeName
              return $ Just $ IVarExpr dictName
            Nothing -> return Nothing
-}

-- | Add dictionary parameters to a function based on its type scheme constraints
-- This transforms constrained functions into dictionary-passing style
addDictionaryParametersT :: TypeScheme -> TIExpr -> EvalM TIExpr
addDictionaryParametersT (Forall _vars constraints _ty) tiExpr
  | null constraints = return tiExpr  -- No constraints, no change
  | otherwise = do
      classEnv <- getClassEnv
      -- Resolve constraints using instance information
      -- This handles the case where Tensor T needs to use T's instance
      let resolvedConstraints = resolveConstraints classEnv constraints
      addDictParamsToTIExpr classEnv resolvedConstraints tiExpr
  where
    -- Resolve constraints based on available instances
    resolveConstraints :: ClassEnv -> [Constraint] -> [Constraint]
    resolveConstraints env cs = map (resolveConstraint env) cs
    
    resolveConstraint :: ClassEnv -> Constraint -> Constraint
    resolveConstraint env c@(Constraint className ty) = case ty of
      TTensor elemType ->
        let instances = lookupInstances className env
        in case findMatchingInstanceForType ty instances of
             Just _ -> c  -- Tensor instance exists, use it
             Nothing -> 
               -- No Tensor instance, try element type
               case findMatchingInstanceForType elemType instances of
                 Just _ -> Constraint className elemType
                 Nothing -> c
      _ -> c
    
    -- Add dictionary parameters to a TIExpr
    addDictParamsToTIExpr :: ClassEnv -> [Constraint] -> TIExpr -> EvalM TIExpr
    addDictParamsToTIExpr env cs expr = case tiExprNode expr of
      -- Lambda: add dictionary parameters before regular parameters
      TILambdaExpr mVar params body -> do
        let dictParams = map constraintToDictParam cs
            dictVars = map stringToVar dictParams
        -- Replace method calls in body with dictionary access
        body' <- replaceMethodCallsWithDictAccessT env cs body
        let newNode = TILambdaExpr mVar (dictVars ++ params) body'
        return $ TIExpr (tiScheme expr) newNode
      
      -- Not a lambda: wrap in a lambda with dictionary parameters
      _ -> do
        let dictParams = map constraintToDictParam cs
            dictVars = map stringToVar dictParams
        expr' <- replaceMethodCallsWithDictAccessT env cs expr
        let wrapperType = tiExprType expr
            newNode = TILambdaExpr Nothing dictVars expr'
            newScheme = Forall [] [] wrapperType
        return $ TIExpr newScheme newNode
    
    -- Generate dictionary parameter name from constraint
    constraintToDictParam :: Constraint -> String
    constraintToDictParam (Constraint className constraintType) =
      "dict_" ++ className ++ "_" ++ typeSuffix constraintType
    
    typeSuffix :: Type -> String
    typeSuffix (TVar (TyVar v)) = v
    typeSuffix TInt = "Integer"
    typeSuffix TFloat = "Float"
    typeSuffix (TTensor t) = "Tensor" ++ typeSuffix t
    typeSuffix _ = "t"
    
    -- Replace method calls with dictionary access in TIExpr
    replaceMethodCallsWithDictAccessT :: ClassEnv -> [Constraint] -> TIExpr -> EvalM TIExpr
    replaceMethodCallsWithDictAccessT env cs tiExpr = do
      let scheme = tiScheme tiExpr
      newNode <- replaceMethodCallsInNode env cs (tiExprNode tiExpr)
      return $ TIExpr scheme newNode
    
    -- Replace method calls in TIExprNode
    replaceMethodCallsInNode :: ClassEnv -> [Constraint] -> TIExprNode -> EvalM TIExprNode
    replaceMethodCallsInNode env cs node = case node of
      -- Standalone method reference: eta-expand
      TIVarExpr methodName -> do
        case findConstraintForMethodInList env methodName cs of
          Just constraint -> do
            -- Get method type to determine arity
            typeEnv <- getTypeEnv
            case lookupEnv methodName typeEnv of
              Just (Forall _ _ ty) -> do
                let arity = getMethodArity ty
                    paramNames = ["etaVar" ++ show i | i <- [1..arity]]
                    paramVars = map stringToVar paramNames
                    paramExprs = map (\n -> TIExpr (Forall [] [] (TVar (TyVar "eta"))) (TIVarExpr n)) paramNames
                    -- Create dictionary access
                    dictParam = constraintToDictParam constraint
                    dictAccess = TIExpr (Forall [] [] ty) $
                                 TIIndexedExpr False 
                                   (TIExpr (Forall [] [] ty) (TIVarExpr dictParam))
                                   [Sub (IConstantExpr (StringExpr (pack (sanitizeMethodName methodName))))]
                    -- Create: dictAccess etaVar1 etaVar2 ... etaVarN
                    body = TIExpr (Forall [] [] ty) (TIApplyExpr dictAccess paramExprs)
                return $ TILambdaExpr Nothing paramVars body
              Nothing -> return $ TIVarExpr methodName
          Nothing -> return $ TIVarExpr methodName
      
      -- Method call: replace with dictionary access
      TIApplyExpr func args -> do
        case tiExprNode func of
          TIVarExpr methodName -> do
            case findConstraintForMethodInList env methodName cs of
              Just constraint -> do
                -- Replace with dictionary access
                let dictParam = constraintToDictParam constraint
                    funcType = tiExprType func
                    dictAccessNode = TIIndexedExpr False 
                                     (TIExpr (Forall [] [] funcType) (TIVarExpr dictParam))
                                     [Sub (IConstantExpr (StringExpr (pack (sanitizeMethodName methodName))))]
                    dictAccess = TIExpr (Forall [] [] funcType) dictAccessNode
                -- Recursively process arguments
                args' <- mapM (replaceMethodCallsWithDictAccessT env cs) args
                return $ TIApplyExpr dictAccess args'
              Nothing -> do
                -- Not a method, process recursively
                func' <- replaceMethodCallsWithDictAccessT env cs func
                args' <- mapM (replaceMethodCallsWithDictAccessT env cs) args
                return $ TIApplyExpr func' args'
          _ -> do
            -- Not a simple variable, process recursively
            func' <- replaceMethodCallsWithDictAccessT env cs func
            args' <- mapM (replaceMethodCallsWithDictAccessT env cs) args
            return $ TIApplyExpr func' args'
      
      -- Lambda: recursively process body
      TILambdaExpr mVar params body -> do
        body' <- replaceMethodCallsWithDictAccessT env cs body
        return $ TILambdaExpr mVar params body'
      
      -- If: recursively process
      TIIfExpr cond thenExpr elseExpr -> do
        cond' <- replaceMethodCallsWithDictAccessT env cs cond
        thenExpr' <- replaceMethodCallsWithDictAccessT env cs thenExpr
        elseExpr' <- replaceMethodCallsWithDictAccessT env cs elseExpr
        return $ TIIfExpr cond' thenExpr' elseExpr'
      
      -- Let: recursively process
      TILetExpr bindings body -> do
        bindings' <- mapM (\(pat, e) -> do
          e' <- replaceMethodCallsWithDictAccessT env cs e
          return (pat, e')) bindings
        body' <- replaceMethodCallsWithDictAccessT env cs body
        return $ TILetExpr bindings' body'
      
      -- LetRec: recursively process
      TILetRecExpr bindings body -> do
        bindings' <- mapM (\(pat, e) -> do
          e' <- replaceMethodCallsWithDictAccessT env cs e
          return (pat, e')) bindings
        body' <- replaceMethodCallsWithDictAccessT env cs body
        return $ TILetRecExpr bindings' body'
      
      -- Other expressions: return as-is for now
      _ -> return node
    
    -- Find constraint that provides a method
    findConstraintForMethodInList :: ClassEnv -> String -> [Constraint] -> Maybe Constraint
    findConstraintForMethodInList _ _ [] = Nothing
    findConstraintForMethodInList env methodName (c@(Constraint className _):cs) =
      case lookupClass className env of
        Just classInfo ->
          if methodName `elem` map fst (classMethods classInfo)
            then Just c
            else findConstraintForMethodInList env methodName cs
        Nothing -> findConstraintForMethodInList env methodName cs
    
    -- Get method arity from type
    getMethodArity :: Type -> Int
    getMethodArity (TFun _ t2) = 1 + getMethodArity t2
    getMethodArity _ = 0

{- OLD CODE - DISABLED TEMPORARILY
  where
    -- Add dictionary parameters to the expression (typically a Lambda)
    addDictParamsToExpr :: ClassEnv -> [Constraint] -> IExpr -> EvalM IExpr
    addDictParamsToExpr classEnv cs expr = case expr of
      -- Lambda: add dictionary parameters before regular parameters
      ILambdaExpr mVar params body -> do
        -- Generate dictionary parameter names
        let dictParams = map constraintToDictParam cs
            dictVars = map stringToVar dictParams
        -- Replace method calls in body with dictionary access
        body' <- replaceMethodCallsWithDictAccess classEnv cs body
        -- Create new lambda with dict params + regular params
        return $ ILambdaExpr mVar (dictVars ++ params) body'
      
      -- Not a lambda: wrap in a lambda with dictionary parameters
      _ -> do
        let dictParams = map constraintToDictParam cs
            dictVars = map stringToVar dictParams
        body' <- replaceMethodCallsWithDictAccess classEnv cs expr
        return $ ILambdaExpr Nothing dictVars body'
    
    -- Generate dictionary parameter name from constraint
    -- E.g., Constraint "Num" (TVar "a") -> "dict_Num_a"
    constraintToDictParam :: Constraint -> String
    constraintToDictParam (Constraint className constraintType) =
      "dict_" ++ className ++ "_" ++ typeSuffix constraintType
    
    typeSuffix :: Type -> String
    typeSuffix (TVar (TyVar v)) = v
    typeSuffix TInt = "Integer"
    typeSuffix TFloat = "Float"
    typeSuffix _ = "t"
    
    -- Replace method calls with dictionary access
    replaceMethodCallsWithDictAccess :: ClassEnv -> [Constraint] -> IExpr -> EvalM IExpr
    replaceMethodCallsWithDictAccess classEnv cs iexpr = case iexpr of
      -- Standalone method reference: eta-expand then process
      IVarExpr methodName -> do
        case findConstraintForMethod classEnv methodName cs of
          Just constraint -> do
            -- This is a method used as a value - eta-expand it
            -- Get method type to determine arity
            typeEnv <- getTypeEnv
            case lookupEnv methodName typeEnv of
              Just (Forall _ _ ty) -> do
                let arity = getMethodArity ty
                    paramNames = ["etaVar" ++ show i | i <- [1..arity]]
                    paramVars = map stringToVar paramNames
                    paramExprs = map IVarExpr paramNames  -- Use paramNames directly, not show of paramVars
                    -- Create dictionary access
                    dictParam = constraintToDictParam constraint
                    dictAccess = IIndexedExpr False (IVarExpr dictParam)
                                  [Sub (IConstantExpr (StringExpr (pack (sanitizeMethodName methodName))))]
                    -- Create: dictAccess etaVar1 etaVar2 ... etaVarN
                    body = IApplyExpr dictAccess paramExprs
                    -- Create: \etaVar1 etaVar2 ... etaVarN -> body
                return $ ILambdaExpr Nothing paramVars body
              Nothing -> return $ IVarExpr methodName
          Nothing -> return $ IVarExpr methodName
      
      -- Method call: replace with dictionary access
      IApplyExpr (IVarExpr methodName) args -> do
        case findConstraintForMethod classEnv methodName cs of
          Just constraint -> do
            -- Replace with dictionary access
            let dictParam = constraintToDictParam constraint
                dictAccess = IIndexedExpr False (IVarExpr dictParam)
                              [Sub (IConstantExpr (StringExpr (pack (sanitizeMethodName methodName))))]
            -- Recursively process arguments
            args' <- mapM (replaceMethodCallsWithDictAccess classEnv cs) args
            return $ IApplyExpr dictAccess args'
          Nothing -> do
            -- Not a method from constraints, process recursively
            args' <- mapM (replaceMethodCallsWithDictAccess classEnv cs) args
            return $ IApplyExpr (IVarExpr methodName) args'
      
      -- Lambda: recursively process body
      ILambdaExpr mVar params body -> do
        body' <- replaceMethodCallsWithDictAccess classEnv cs body
        return $ ILambdaExpr mVar params body'
      
      -- Application: recursively process
      IApplyExpr func args -> do
        func' <- replaceMethodCallsWithDictAccess classEnv cs func
        args' <- mapM (replaceMethodCallsWithDictAccess classEnv cs) args
        return $ IApplyExpr func' args'
      
      -- If: recursively process
      IIfExpr cond thenExpr elseExpr -> do
        cond' <- replaceMethodCallsWithDictAccess classEnv cs cond
        thenExpr' <- replaceMethodCallsWithDictAccess classEnv cs thenExpr
        elseExpr' <- replaceMethodCallsWithDictAccess classEnv cs elseExpr
        return $ IIfExpr cond' thenExpr' elseExpr'
      
      -- Let: recursively process
      ILetExpr bindings body -> do
        bindings' <- mapM (\(pat, e) -> do
          e' <- replaceMethodCallsWithDictAccess classEnv cs e
          return (pat, e')) bindings
        body' <- replaceMethodCallsWithDictAccess classEnv cs body
        return $ ILetExpr bindings' body'
      
      -- LetRec: recursively process
      ILetRecExpr bindings body -> do
        bindings' <- mapM (\(pat, e) -> do
          e' <- replaceMethodCallsWithDictAccess classEnv cs e
          return (pat, e')) bindings
        body' <- replaceMethodCallsWithDictAccess classEnv cs body
        return $ ILetRecExpr bindings' body'
      
      -- Other expressions: return as-is for now
      -- (Can add more cases as needed)
      _ -> return iexpr
    
    findConstraintForMethod :: ClassEnv -> String -> [Constraint] -> Maybe Constraint
    findConstraintForMethod _ _ [] = Nothing
    findConstraintForMethod env methodName (c@(Constraint className _):cs) =
      case lookupClass className env of
        Just classInfo ->
          if methodName `elem` map fst (classMethods classInfo)
            then Just c
            else findConstraintForMethod env methodName cs
        Nothing -> findConstraintForMethod env methodName cs
    
    getMethodArity :: Type -> Int
    getMethodArity (TFun _ t2) = 1 + getMethodArity t2
    getMethodArity _ = 0
-}
