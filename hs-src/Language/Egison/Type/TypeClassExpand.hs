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
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Char                  (toLower, toUpper)

import           Language.Egison.Data       (EvalM)
import           Language.Egison.EvalState  (MonadEval(..))
import           Language.Egison.IExpr      (TIExpr(..), IExpr(..), IPattern(..),
                                             IBindingExpr, IMatchClause, ILoopRange(..),
                                             Index(..), tiExprConstraints, tiExprType)
import           Language.Egison.Type.Env  (ClassEnv(..), ClassInfo(..), InstanceInfo(..),
                                             lookupInstances, lookupClass, generalize, classEnvToList)
import           Language.Egison.Type.IInfer (runInferI, defaultInferConfig)
import           Language.Egison.Type.Types (Type(..), TyVar(..), TypeScheme(..), Constraint(..))
import           Language.Egison.Type.Unify (unify)

-- | Expand type class method calls in a typed expression (TIExpr)
-- This function recursively processes TIExpr and replaces type class method calls
-- with dictionary-based dispatch.
expandTypeClassMethodsT :: TIExpr -> EvalM TIExpr
expandTypeClassMethodsT (TIExpr scheme expr) = do
  classEnv <- getClassEnv
  -- Recursively process the expression tree
  expr' <- expandTIExpr classEnv expr
  -- For now, preserve the original type scheme
  -- TODO: Update type scheme after dictionary passing (remove constraints)
  return $ TIExpr scheme expr'
  where
    -- Helper function to infer type for an IExpr and process it as TIExpr
    inferAndExpand :: ClassEnv -> IExpr -> EvalM TIExpr
    inferAndExpand classEnv' e = do
      typeEnv <- getTypeEnv
      -- Run type inference for the sub-expression
      result <- liftIO $ runInferI defaultInferConfig typeEnv e
      case result of
        Left _err -> do
          -- If type inference fails, create a TIExpr with TAny type
          let fallbackScheme = Forall [] [] TAny
          e' <- expandTIExpr classEnv' e
          return $ TIExpr fallbackScheme e'
        Right (ty, _subst, _warnings) -> do
          -- Generalize the type to create a type scheme
          let typeScheme = generalize typeEnv ty
          e' <- expandTIExpr classEnv' e
          return $ TIExpr typeScheme e'
    
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
      
      -- Application - This is where we expand type class methods!
      IWedgeApplyExpr func args -> do
        -- Process with type information to enable type class expansion
        funcTI <- inferAndExpand classEnv' func
        argsTI <- mapM (inferAndExpand classEnv') args
        -- Try to expand type class method calls
        expanded <- tryExpandTypeClassCallT classEnv' funcTI argsTI
        case expanded of
          Just expandedExpr -> return expandedExpr
          Nothing -> do
            func' <- expandTIExpr classEnv' (tiExpr funcTI)
            args' <- mapM (expandTIExpr classEnv' . tiExpr) argsTI
            return $ IWedgeApplyExpr func' args'
      
      IDoExpr bindings body -> do
        bindings' <- mapM (expandBinding classEnv') bindings
        body' <- expandTIExpr classEnv' body
        return $ IDoExpr bindings' body'
      
      ISeqExpr expr1 expr2 -> do
        expr1' <- expandTIExpr classEnv' expr1
        expr2' <- expandTIExpr classEnv' expr2
        return $ ISeqExpr expr1' expr2'
      
      -- Function application - This is where we expand type class methods!
      IApplyExpr func args -> do
        -- Check if this is a type class method call
        case func of
          IVarExpr methodName -> do
            -- Try to expand as type class method
            expanded <- tryExpandTypeClassMethod classEnv' methodName args
            case expanded of
              Just expandedExpr -> return expandedExpr
              Nothing -> do
                -- Not a type class method, process recursively
                func' <- expandTIExpr classEnv' func
                args' <- mapM (expandTIExpr classEnv') args
                return $ IApplyExpr func' args'
          _ -> do
            -- Function is not a simple variable, process recursively
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

-- | Try to expand a type class method call by inferring argument types
-- This is simpler than using TIExpr constraints
tryExpandTypeClassMethod :: ClassEnv -> String -> [IExpr] -> EvalM (Maybe IExpr)
tryExpandTypeClassMethod classEnv methodName args = do
  -- Check if this method name belongs to any type class
  let allClasses = classEnvToList classEnv
      matchingClasses = filter (\(_, classInfo) -> 
                                  methodName `elem` map fst (classMethods classInfo)) allClasses
  
  case matchingClasses of
    [] -> return Nothing  -- Not a type class method
    ((className, _):_) -> do
      -- Infer argument types to determine the instance
      typeEnv <- getTypeEnv
      result <- liftIO $ runInferI defaultInferConfig typeEnv (IApplyExpr (IVarExpr methodName) args)
      case result of
        Left _ -> return Nothing
        Right (_, _, warnings) -> do
          -- Try to infer the first argument's type to determine the instance
          if null args
            then return Nothing
            else do
              argResult <- liftIO $ runInferI defaultInferConfig typeEnv (head args)
              case argResult of
                Left _ -> return Nothing
                Right (argType, _, _) -> do
                  -- Find matching instance for this type
                  let instances = lookupInstances className classEnv
                  case findMatchingInstanceForType argType instances of
                    Just inst -> do
                      let sanitizedMethod = sanitizeMethodName methodName
                          instTypeName = typeToName (instType inst)
                          dictName = resolveMethodName className sanitizedMethod instTypeName
                      return $ Just $ IApplyExpr (IVarExpr dictName) args
                    Nothing -> return Nothing
  where
    findMatchingInstanceForType :: Type -> [InstanceInfo] -> Maybe InstanceInfo
    findMatchingInstanceForType _ [] = Nothing
    findMatchingInstanceForType targetType (inst:insts) =
      case unify (instType inst) targetType of
        Right _ -> Just inst
        Left _ -> findMatchingInstanceForType targetType insts
    
    sanitizeMethodName :: String -> String
    sanitizeMethodName "==" = "eq"
    sanitizeMethodName "/=" = "neq"
    sanitizeMethodName "<"  = "lt"
    sanitizeMethodName "<=" = "le"
    sanitizeMethodName ">"  = "gt"
    sanitizeMethodName ">=" = "ge"
    sanitizeMethodName "+"  = "plus"
    sanitizeMethodName "-"  = "minus"
    sanitizeMethodName "*"  = "times"
    sanitizeMethodName "/"  = "div"
    sanitizeMethodName name = name
    
    typeToName :: Type -> String
    typeToName TInt = "Integer"
    typeToName TFloat = "Float"
    typeToName TBool = "Bool"
    typeToName TChar = "Char"
    typeToName TString = "String"
    typeToName (TVar (TyVar v)) = v
    typeToName (TInductive name _) = name
    typeToName (TCollection t) = "List" ++ typeToName t
    typeToName (TTuple ts) = "Tuple" ++ concatMap typeToName ts
    typeToName _ = "Unknown"

-- | Try to expand a type class method call using type information
-- This function uses the type information from TIExpr to select the appropriate instance
-- and replace the method call with a dictionary-based dispatch.
tryExpandTypeClassCallT :: ClassEnv -> TIExpr -> [TIExpr] -> EvalM (Maybe IExpr)
tryExpandTypeClassCallT classEnv funcTI argsTI = do
  -- Check if the function is a variable (method call)
  case tiExpr funcTI of
    IVarExpr methodName -> do
      -- Check if this method name corresponds to a type class method
      -- by looking at the function's type constraints
      let constraints = tiExprConstraints funcTI
          argTypes = map tiExprType argsTI
      
      -- If there are no constraints, this is not a type class method
      if null constraints
        then return Nothing
        else do
          -- Try to find a matching constraint for this method
          matchingConstraint <- findMatchingConstraint classEnv methodName constraints argTypes
          case matchingConstraint of
            Just dictName -> do
              -- Replace with dictionary call: dictName args...
              let args' = map tiExpr argsTI
              return $ Just $ IApplyExpr (IVarExpr dictName) args'
            Nothing -> return Nothing
    
    _ -> return Nothing
  where
    -- Find a constraint that matches the method name and argument types
    findMatchingConstraint :: ClassEnv -> String -> [Constraint] -> [Type] -> EvalM (Maybe String)
    findMatchingConstraint _ _ [] _ = return Nothing
    findMatchingConstraint env methodName (Constraint className constraintType : cs) argTypes = do
      -- Look up the class to get method information
      case lookupClass className env of
        Just classInfo -> do
          -- Check if the method name is in this class
          if methodName `elem` map fst (classMethods classInfo)
            then do
              -- Resolve the constraint type using argument types
              resolvedType <- return $ resolveConstraintType constraintType argTypes
              
              -- Try to find a matching instance
              let instances = lookupInstances className env
              matchingInstance <- findMatchingInstance resolvedType instances
              case matchingInstance of
                Just inst -> do
                  -- Generate dictionary method name
                  let sanitizedMethod = sanitizeMethodName methodName
                      dictName = resolveMethodName className sanitizedMethod (typeName (instType inst))
                  return $ Just dictName
                Nothing -> findMatchingConstraint env methodName cs argTypes
            else findMatchingConstraint env methodName cs argTypes
        Nothing -> findMatchingConstraint env methodName cs argTypes
    
    -- Resolve constraint type using argument types
    -- If constraint type is a type variable, try to unify with argument types
    resolveConstraintType :: Type -> [Type] -> Type
    resolveConstraintType (TVar _) (argType:_) = argType  -- Use first arg type
    resolveConstraintType t _ = t  -- Already concrete
    
    -- Find an instance that matches the given type
    findMatchingInstance :: Type -> [InstanceInfo] -> EvalM (Maybe InstanceInfo)
    findMatchingInstance _ [] = return Nothing
    findMatchingInstance targetType (inst : insts) = do
      case unify (instType inst) targetType of
        Right _ -> return $ Just inst
        Left _ -> findMatchingInstance targetType insts
    
    -- Sanitize method name for dictionary lookup
    sanitizeMethodName :: String -> String
    sanitizeMethodName "==" = "eq"
    sanitizeMethodName "/=" = "neq"
    sanitizeMethodName "<"  = "lt"
    sanitizeMethodName "<=" = "le"
    sanitizeMethodName ">"  = "gt"
    sanitizeMethodName ">=" = "ge"
    sanitizeMethodName "+"  = "plus"
    sanitizeMethodName "-"  = "minus"
    sanitizeMethodName "*"  = "times"
    sanitizeMethodName "/"  = "div"
    sanitizeMethodName name = name
    
    -- Extract type name from Type for dictionary naming
    typeName :: Type -> String
    typeName TInt = "Integer"
    typeName TFloat = "Float"
    typeName TBool = "Bool"
    typeName TChar = "Char"
    typeName TString = "String"
    typeName (TVar (TyVar v)) = v
    typeName (TInductive name _) = name
    typeName (TCollection t) = "List" ++ typeName t
    typeName (TTuple ts) = "Tuple" ++ concatMap typeName ts
    typeName _ = "Unknown"

-- | Resolve a type class method name for a specific type
-- e.g., resolveMethodName "Eq" "eq" "Integer" -> "eqIntegerEq"
resolveMethodName :: String -> String -> String -> String
resolveMethodName className methodName typeName =
  lowerFirst className ++ typeName ++ capitalizeFirst methodName

-- | Capitalize first character
capitalizeFirst :: String -> String
capitalizeFirst []     = []
capitalizeFirst (c:cs) = toUpper c : cs

-- | Lowercase first character
lowerFirst :: String -> String
lowerFirst []     = []
lowerFirst (c:cs) = toLower c : cs
