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

import           Data.Char                  (toLower)
import           Data.List                  (find)
import           Data.Text                  (pack)

import           Language.Egison.AST        (ConstantExpr(..))
import           Language.Egison.Data       (EvalM)
import           Language.Egison.EvalState  (MonadEval(..))
import           Language.Egison.IExpr      (TIExpr(..), TIExprNode(..), IExpr(..), stringToVar,
                                             Index(..), tiExprType, tiScheme, tiExprNode)
import           Language.Egison.Type.Env  (ClassEnv(..), ClassInfo(..), InstanceInfo(..),
                                             lookupInstances, lookupClass, lookupEnv)
import           Language.Egison.Type.Types (Type(..), TyVar(..), TypeScheme(..), Constraint(..), typeToName, sanitizeMethodName,
                                            findMatchingInstanceForType)

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
                    else do
                      -- Has type variable constraints - pass dictionary parameters
                      -- This handles recursive calls in polymorphic functions
                      -- Generate dictionary argument expressions for each constraint
                      let makeDict c =
                            let dictName = constraintToDictParam c
                                dictType = TVar (TyVar "dict")
                            in TIExpr (Forall [] [] dictType) (TIVarExpr dictName)
                          dictArgs = map makeDict exprConstraints
                          varExpr = TIExpr scheme (TIVarExpr varName)
                      return $ TIApplyExpr varExpr dictArgs
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
    
    -- Helper: lowercase first character
    lowerFirst :: String -> String
    lowerFirst [] = []
    lowerFirst (c:cs) = toLower c : cs

-- | Generate dictionary parameter name from constraint
-- Used for both dictionary parameter generation and dictionary argument passing
constraintToDictParam :: Constraint -> String
constraintToDictParam (Constraint className constraintType) =
  "dict_" ++ className ++ "_" ++ typeSuffix constraintType

-- | Generate type suffix for dictionary parameter names
typeSuffix :: Type -> String
typeSuffix (TVar (TyVar v)) = v
typeSuffix TInt = "Integer"
typeSuffix TFloat = "Float"
typeSuffix TString = "String"
typeSuffix TBool = "Bool"
typeSuffix TChar = "Char"
typeSuffix (TTensor t) = "Tensor" ++ typeSuffix t
typeSuffix (TCollection t) = "Collection" ++ typeSuffix t
typeSuffix _ = "t"

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
