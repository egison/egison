{- |
Module      : Language.Egison.Type.TypeClassExpand
Licence     : MIT

This module expands type class method calls using type information from TIExpr.
It transforms TIExpr to TIExpr, replacing type class method calls with
dictionary-based dispatch.

Pipeline: Phase 8 (TypedDesugar) - TypeClassExpand

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
import           Data.Text                  (pack)

import           Language.Egison.AST        (ConstantExpr(..))
import           Language.Egison.Data       (EvalM)
import           Language.Egison.EvalState  (MonadEval(..))
import           Language.Egison.IExpr      (TIExpr(..), IExpr(..), IPattern(..), Var(..), stringToVar,
                                             IBindingExpr, IMatchClause, ILoopRange(..),
                                             Index(..), tiExprConstraints, tiExprType, tiScheme, stripType)
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
  -- TODO: Implement proper type class expansion with new TIExprNode structure
  -- For now, return the original TIExpr unchanged
  -- This means type class dictionary passing is disabled temporarily
  return tiExpr

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
      -- TODO: Implement proper dictionary parameter addition with new TIExprNode structure
      -- For now, return the original TIExpr unchanged
      return tiExpr

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
