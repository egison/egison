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
                                             Index(..), tiExprConstraints, tiExprType)
import           Language.Egison.Type.Env  (ClassEnv(..), ClassInfo(..), InstanceInfo(..),
                                             lookupInstances, lookupClass, generalize, classEnvToList, lookupEnv)
import           Language.Egison.Type.IInfer (runInferI, defaultInferConfig)
import           Language.Egison.Type.Types (Type(..), TyVar(..), TypeScheme(..), Constraint(..), typeToName, sanitizeMethodName,
                                            capitalizeFirst, lowerFirst, findMatchingInstanceForType, InstanceInfo(..))
import           Language.Egison.Type.Unify (unify)

-- | Expand type class method calls in a typed expression (TIExpr)
-- This function recursively processes TIExpr and replaces type class method calls
-- with dictionary-based dispatch.
expandTypeClassMethodsT :: TIExpr -> EvalM TIExpr
expandTypeClassMethodsT (TIExpr scheme expr) = do
  classEnv <- getClassEnv
  -- Recursively process the expression tree with constraint information
  expr' <- expandTIExprWithConstraints classEnv scheme expr
  -- For now, preserve the original type scheme
  -- TODO: Update type scheme after dictionary passing (remove constraints)
  return $ TIExpr scheme expr'
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
      ITensorMapExpr lambda tensorArg -> do
        lambda' <- expandTIExprWithConstraintList classEnv' cs lambda
        tensorArg' <- expandTIExprWithConstraintList classEnv' cs tensorArg
        return $ ITensorMapExpr lambda' tensorArg'
      
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
    

-- | Add dictionary parameters to a function based on its type scheme constraints
-- This transforms constrained functions into dictionary-passing style
addDictionaryParametersT :: TypeScheme -> TIExpr -> EvalM TIExpr
addDictionaryParametersT (Forall _vars constraints _ty) tiExpr
  | null constraints = return tiExpr  -- No constraints, no change
  | otherwise = do
      classEnv <- getClassEnv
      -- Transform the expression to add dictionary parameters
      let TIExpr scheme expr = tiExpr
      expr' <- addDictParamsToExpr classEnv constraints expr
      return $ TIExpr scheme expr'
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
    
