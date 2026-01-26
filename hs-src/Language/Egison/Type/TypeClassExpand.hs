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
  , applyConcreteConstraintDictionaries
  ) where

import           Data.Char                  (toLower)
import           Data.List                  (find)
import           Data.Maybe                 (mapMaybe)
import           Data.Text                  (pack)
import           Control.Monad              (mplus)
import qualified Data.Set                   as Set

import           Language.Egison.AST        (ConstantExpr(..))
import           Language.Egison.Data       (EvalM)
import           Language.Egison.EvalState  (MonadEval(..))
import           Language.Egison.IExpr      (TIExpr(..), TIExprNode(..), IExpr(..), stringToVar,
                                             Index(..), tiExprType, tiScheme, tiExprNode,
                                             TIPattern(..), TIPatternNode(..), TILoopRange(..))
import           Language.Egison.Type.Env  (ClassEnv(..), ClassInfo(..), InstanceInfo(..),
                                             lookupInstances, lookupClass, lookupEnv)
import qualified Language.Egison.Type.Types as Types
import           Language.Egison.Type.Types (Type(..), TyVar(..), TypeScheme(..), Constraint(..), typeToName, typeConstructorName,
                                            sanitizeMethodName, freeTyVars)
import           Language.Egison.Type.Instance (findMatchingInstanceForType)

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
        -- Dictionary hashes: process keys but NOT values
        -- Values should remain as simple method references
        pairs' <- mapM (\(k, v) -> do
          k' <- expandTIExprWithConstraints classEnv' cs k
          -- Do NOT process v - dictionary values should not be expanded
          return (k', v)) pairs
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
          pat' <- expandTIPattern classEnv' cs pat
          body' <- expandTIExprWithConstraints classEnv' cs body
          return (pat', body')) clauses
        return $ TIMatchExpr mode target' matcher' clauses'
      
      TIMatchAllExpr mode target matcher clauses -> do
        target' <- expandTIExprWithConstraints classEnv' cs target
        matcher' <- expandTIExprWithConstraints classEnv' cs matcher
        clauses' <- mapM (\(pat, body) -> do
          pat' <- expandTIPattern classEnv' cs pat
          body' <- expandTIExprWithConstraints classEnv' cs body
          return (pat', body')) clauses
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
      
      TIGenerateTensorExpr func shape -> do
        func' <- expandTIExprWithConstraints classEnv' cs func
        shape' <- expandTIExprWithConstraints classEnv' cs shape
        return $ TIGenerateTensorExpr func' shape'
      
      TITensorExpr shape elems -> do
        shape' <- expandTIExprWithConstraints classEnv' cs shape
        elems' <- expandTIExprWithConstraints classEnv' cs elems
        return $ TITensorExpr shape' elems'
      
      TITensorContractExpr tensor -> do
        tensor' <- expandTIExprWithConstraints classEnv' cs tensor
        return $ TITensorContractExpr tensor'
      
      TITransposeExpr perm tensor -> do
        perm' <- expandTIExprWithConstraints classEnv' cs perm
        tensor' <- expandTIExprWithConstraints classEnv' cs tensor
        return $ TITransposeExpr perm' tensor'
      
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
      
      TIMatcherExpr patDefs -> do
        -- Expand expressions inside matcher definitions
        -- patDefs is a list of (PrimitivePatPattern, TIExpr, [TIBindingExpr])
        -- where TIBindingExpr is (IPrimitiveDataPattern, TIExpr)
        -- Note: cs contains the constraints from the matcher definition itself (e.g., {Eq a})
        patDefs' <- mapM (\(pat, matcherExpr, bindings) -> do
          -- Expand the next-matcher expression with parent constraints
          matcherExpr' <- expandTIExprWithConstraints classEnv' cs matcherExpr
          -- Expand expressions in primitive-data-match clauses with parent constraints
          -- These expressions may use type class methods from the matcher's constraints
          bindings' <- mapM (\(dp, expr) -> do
            expr' <- expandTIExprWithConstraints classEnv' cs expr
            return (dp, expr')) bindings
          return (pat, matcherExpr', bindings')) patDefs
        return $ TIMatcherExpr patDefs'
      TIIndexedExpr override base indices -> do
        base' <- expandTIExprWithConstraints classEnv' cs base
        -- Expand indices (which are already typed as TIExpr)
        indices' <- mapM (traverse (\tiexpr -> expandTIExprWithConstraints classEnv' cs tiexpr)) indices
        return $ TIIndexedExpr override base' indices'
      
      TIWedgeApplyExpr func args -> do
        func' <- expandTIExprWithConstraints classEnv' cs func
        args' <- mapM (expandTIExprWithConstraints classEnv' cs) args
        return $ TIWedgeApplyExpr func' args'
      
      TIFunctionExpr names -> return $ TIFunctionExpr names  -- Built-in function, no expansion needed
    
    -- Helper: expand a TIExpr with constraints
    expandTIExprWithConstraints :: ClassEnv -> [Constraint] -> TIExpr -> EvalM TIExpr
    expandTIExprWithConstraints classEnv' cs expr = do
      let scheme@(Forall _ exprConstraints exprType) = tiScheme expr
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
              case lookupEnv (stringToVar varName) typeEnv of
                Just (Forall _ _ _ty) -> do
                  -- Use the expression's actual type (exprType) instead of the method's declared type (ty)
                  -- because eta-expansion should create parameters matching the expected usage context
                  let arity = getMethodArity exprType
                      paramTypes = getParamTypes exprType
                      paramNames = ["etaVar" ++ show i | i <- [1..arity]]
                      paramVars = map stringToVar paramNames
                      paramExprs = zipWith (\n t -> TIExpr (Forall [] [] t) (TIVarExpr n)) paramNames paramTypes
                      methodKey = sanitizeMethodName varName
                  
                  -- Determine dictionary name based on type
                  case tyArg of
                    TVar (TyVar _v) -> do
                      -- Type variable: use dictionary parameter name (without type parameter)
                      typeEnv <- getTypeEnv
                      let dictParamName = "dict_" ++ className
                      -- Look up dictionary type from type environment
                      dictHashType <- case lookupEnv (stringToVar dictParamName) typeEnv of
                        Just (Forall _ _ dictType) -> return dictType
                        Nothing -> return $ THash TString TAny  -- Fallback
                      -- Get method type from ClassEnv instead of dictHashType
                      let methodType = getMethodTypeFromClass classEnv' className methodKey tyArg
                          methodConstraint = Constraint className tyArg
                          methodScheme = Forall (Set.toList $ freeTyVars tyArg) [methodConstraint] methodType
                          dictExpr = TIExpr (Forall [] [] dictHashType) (TIVarExpr dictParamName)
                          indexExpr = TIExpr (Forall [] [] TString) 
                                            (TIConstantExpr (StringExpr (pack methodKey)))
                          dictAccess = TIExpr methodScheme $
                                       TIIndexedExpr False dictExpr [Sub indexExpr]
                          body = TIExpr methodScheme (TIApplyExpr dictAccess paramExprs)
                      return $ TILambdaExpr Nothing paramVars body
                    _ -> do
                      -- Concrete type: find matching instance
                      let instances = lookupInstances className classEnv'
                      case findMatchingInstanceForType tyArg instances of
                        Just inst -> do
                          -- Found instance: eta-expand with concrete dictionary
                          typeEnv <- getTypeEnv
                          let instTypeName = typeConstructorName (instType inst)
                              dictName = lowerFirst className ++ instTypeName

                          -- Look up dictionary type from type environment
                          dictHashType <- case lookupEnv (stringToVar dictName) typeEnv of
                            Just (Forall _ _ dictType) -> return dictType
                            Nothing -> return $ THash TString TAny  -- Fallback

                          -- Get method type from ClassEnv instead of dictHashType
                          let methodType = getMethodTypeFromClass classEnv' className methodKey tyArg
                              methodConstraint = Constraint className tyArg
                              methodScheme = Forall (Set.toList $ freeTyVars tyArg) [methodConstraint] methodType

                          -- Check if instance has nested constraints
                          dictExprBase <- if null (instContext inst)
                            then do
                              -- No constraints: dictionary is a simple hash
                              return $ TIExpr (Forall [] [] dictHashType) (TIVarExpr dictName)
                            else do
                              -- Has constraints: dictionary is a function that returns a hash
                              -- Get the result type (should be the hash type after applying arguments)
                              let dictFuncType = case dictHashType of
                                    TFun _ resultType -> TFun dictHashType resultType
                                    _ -> TFun (THash TString TAny) dictHashType
                                  dictFuncExpr = TIExpr (Forall [] [] dictFuncType) (TIVarExpr dictName)
                              dictArgs <- mapM (resolveDictionaryArg classEnv') (instContext inst)
                              return $ TIExpr (Forall [] [] dictHashType) (TIApplyExpr dictFuncExpr dictArgs)

                          let indexExpr = TIExpr (Forall [] [] TString) 
                                               (TIConstantExpr (StringExpr (pack methodKey)))
                              dictAccess = TIExpr methodScheme $
                                           TIIndexedExpr False dictExprBase [Sub indexExpr]
                              body = TIExpr methodScheme (TIApplyExpr dictAccess paramExprs)
                          return $ TILambdaExpr Nothing paramVars body
                        Nothing -> checkConstrainedVariable
                Nothing -> checkConstrainedVariable
            Nothing -> checkConstrainedVariable
          where
            -- Check if this is a constrained variable (not a method)
            -- IMPORTANT: Only apply dictionaries if the variable was DEFINED with constraints,
            -- not just if the expression has propagated constraints from usage context.
            checkConstrainedVariable = do
              typeEnv <- getTypeEnv
              -- Look up the variable's original type scheme from TypeEnv
              case lookupEnv (stringToVar varName) typeEnv of
                Just (Forall _ originalConstraints _)
                  | not (null originalConstraints) -> do
                      -- Variable was defined with constraints - apply dictionaries
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
                _ ->
                  -- Variable was defined without constraints, or not found in TypeEnv
                  -- Don't apply dictionaries - just process normally
                  expandTIExprNodeWithConstraintList classEnv' allConstraints (tiExprNode expr)

            isConcreteConstraint (Constraint _ (TVar _)) = False
            isConcreteConstraint _ = True
        _ -> expandTIExprNodeWithConstraintList classEnv' allConstraints (tiExprNode expr)
      
      return $ TIExpr scheme expandedNode
    
    -- Expand type class methods in patterns (especially loop patterns)
    expandTIPattern :: ClassEnv -> [Constraint] -> TIPattern -> EvalM TIPattern
    expandTIPattern classEnv' cs (TIPattern scheme node) = do
      node' <- expandTIPatternNode classEnv' cs node
      return $ TIPattern scheme node'
    
    -- Expand pattern nodes recursively
    expandTIPatternNode :: ClassEnv -> [Constraint] -> TIPatternNode -> EvalM TIPatternNode
    expandTIPatternNode classEnv' cs node = case node of
      -- Loop pattern: expand the loop range expressions
      TILoopPat var loopRange pat1 pat2 -> do
        loopRange' <- expandTILoopRange classEnv' cs loopRange
        pat1' <- expandTIPattern classEnv' cs pat1
        pat2' <- expandTIPattern classEnv' cs pat2
        return $ TILoopPat var loopRange' pat1' pat2'
      
      -- Recursive pattern constructors
      TIAndPat pat1 pat2 -> do
        pat1' <- expandTIPattern classEnv' cs pat1
        pat2' <- expandTIPattern classEnv' cs pat2
        return $ TIAndPat pat1' pat2'
      
      TIOrPat pat1 pat2 -> do
        pat1' <- expandTIPattern classEnv' cs pat1
        pat2' <- expandTIPattern classEnv' cs pat2
        return $ TIOrPat pat1' pat2'
      
      TIForallPat pat1 pat2 -> do
        pat1' <- expandTIPattern classEnv' cs pat1
        pat2' <- expandTIPattern classEnv' cs pat2
        return $ TIForallPat pat1' pat2'
      
      TINotPat pat -> do
        pat' <- expandTIPattern classEnv' cs pat
        return $ TINotPat pat'
      
      TITuplePat pats -> do
        pats' <- mapM (expandTIPattern classEnv' cs) pats
        return $ TITuplePat pats'
      
      TIInductivePat name pats -> do
        pats' <- mapM (expandTIPattern classEnv' cs) pats
        return $ TIInductivePat name pats'
      
      TIIndexedPat pat exprs -> do
        pat' <- expandTIPattern classEnv' cs pat
        exprs' <- mapM (expandTIExprWithConstraints classEnv' cs) exprs
        return $ TIIndexedPat pat' exprs'
      
      TILetPat bindings pat -> do
        pat' <- expandTIPattern classEnv' cs pat
        return $ TILetPat bindings pat'  -- TODO: Expand binding expressions
      
      TIPApplyPat funcExpr argPats -> do
        funcExpr' <- expandTIExprWithConstraints classEnv' cs funcExpr
        argPats' <- mapM (expandTIPattern classEnv' cs) argPats
        return $ TIPApplyPat funcExpr' argPats'
      
      TIDApplyPat pat pats -> do
        pat' <- expandTIPattern classEnv' cs pat
        pats' <- mapM (expandTIPattern classEnv' cs) pats
        return $ TIDApplyPat pat' pats'
      
      TISeqConsPat pat1 pat2 -> do
        pat1' <- expandTIPattern classEnv' cs pat1
        pat2' <- expandTIPattern classEnv' cs pat2
        return $ TISeqConsPat pat1' pat2'
      
      TISeqNilPat -> return TISeqNilPat
      
      TIVarPat name -> return $ TIVarPat name
      
      TIInductiveOrPApplyPat name pats -> do
        pats' <- mapM (expandTIPattern classEnv' cs) pats
        return $ TIInductiveOrPApplyPat name pats'
      
      -- Leaf patterns: no expansion needed
      TIWildCard -> return TIWildCard
      TIPatVar name -> return $ TIPatVar name
      TIValuePat expr -> do
        expr' <- expandTIExprWithConstraints classEnv' cs expr
        return $ TIValuePat expr'
      TIPredPat pred -> do
        pred' <- expandTIExprWithConstraints classEnv' cs pred
        return $ TIPredPat pred'
      TIContPat -> return TIContPat
      TILaterPatVar -> return TILaterPatVar
    
    -- Expand loop range expressions
    expandTILoopRange :: ClassEnv -> [Constraint] -> TILoopRange -> EvalM TILoopRange
    expandTILoopRange classEnv' cs (TILoopRange start end rangePat) = do
      start' <- expandTIExprWithConstraints classEnv' cs start
      end' <- expandTIExprWithConstraints classEnv' cs end
      rangePat' <- expandTIPattern classEnv' cs rangePat
      return $ TILoopRange start' end' rangePat'
    
    -- Helper to get method arity
    getMethodArity :: Type -> Int
    getMethodArity (TFun _ t2) = 1 + getMethodArity t2
    getMethodArity _ = 0
    
    -- Helper to get parameter types from function type
    getParamTypes :: Type -> [Type]
    getParamTypes (TFun t1 t2) = t1 : getParamTypes t2
    getParamTypes _ = []
    
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
                  let methodKey = sanitizeMethodName methodName
                  -- Check if this is a type variable constraint
                  case tyArg of
                    TVar (TyVar _v) -> do
                      -- Type variable: use dictionary parameter
                      -- e.g., for {Eq a}, use dict_Eq (without type parameter)
                      typeEnv <- getTypeEnv
                      let dictParamName = "dict_" ++ className
                      -- Look up dictionary type from type environment
                      dictHashType <- case lookupEnv (stringToVar dictParamName) typeEnv of
                        Just (Forall _ _ dictType) -> return dictType
                        Nothing -> return $ THash TString TAny  -- Fallback
                      -- Get method type from ClassEnv instead of dictHashType
                      let methodType = getMethodTypeFromClass classEnv' className methodKey tyArg
                          methodConstraint = Constraint className tyArg
                          methodScheme = Forall (Set.toList $ freeTyVars tyArg) [methodConstraint] methodType
                          dictExpr = TIExpr (Forall [] [] dictHashType) (TIVarExpr dictParamName)
                          indexExpr = TIExpr (Forall [] [] TString) 
                                            (TIConstantExpr (StringExpr (pack methodKey)))
                          dictAccess = TIExpr methodScheme $
                                       TIIndexedExpr False dictExpr [Sub indexExpr]
                      -- Apply arguments: dictAccess arg1 arg2 ...
                      return $ Just $ TIApplyExpr dictAccess expandedArgs
                    _ -> do
                      -- Concrete type: try to find matching instance
                      let instances = lookupInstances className classEnv'
                      -- Use actual argument type if needed
                      let argTypes = map tiExprType expandedArgs
                          actualType = case (tyArg, argTypes) of
                            (TVar _, (t:_)) -> t  -- Use first argument's type
                            _ -> tyArg
                      -- Check if actualType is still a type variable
                      case actualType of
                        TVar (TyVar _v') -> do
                          -- Still a type variable: use dictionary parameter
                          typeEnv <- getTypeEnv
                          let dictParamName = "dict_" ++ className
                          -- Look up dictionary type from type environment
                          dictHashType <- case lookupEnv (stringToVar dictParamName) typeEnv of
                            Just (Forall _ _ dictType) -> return dictType
                            Nothing -> return $ THash TString TAny  -- Fallback
                          -- Get method type from ClassEnv instead of dictHashType
                          let methodType = getMethodTypeFromClass classEnv' className methodKey actualType
                              methodConstraint = Constraint className actualType
                              methodScheme = Forall (Set.toList $ freeTyVars actualType) [methodConstraint] methodType
                              dictExpr = TIExpr (Forall [] [] dictHashType) (TIVarExpr dictParamName)
                              indexExpr = TIExpr (Forall [] [] TString) 
                                                (TIConstantExpr (StringExpr (pack methodKey)))
                              dictAccess = TIExpr methodScheme $
                                           TIIndexedExpr False dictExpr [Sub indexExpr]
                          -- Apply arguments: dictAccess arg1 arg2 ...
                          return $ Just $ TIApplyExpr dictAccess expandedArgs
                        _ -> case findMatchingInstanceForType actualType instances of
                          Just inst -> do
                            -- Found an instance: generate dictionary access
                            -- e.g., numInteger_"plus" for Num Integer instance
                            typeEnv <- getTypeEnv
                            let instTypeName = typeConstructorName (instType inst)
                                dictName = lowerFirst className ++ instTypeName

                            -- Look up dictionary type from type environment
                            dictHashType <- case lookupEnv (stringToVar dictName) typeEnv of
                              Just (Forall _ _ dictType) -> return dictType
                              Nothing -> return $ THash TString TAny  -- Fallback

                            -- Get method type from ClassEnv instead of dictHashType
                            let methodType = getMethodTypeFromClass classEnv' className methodKey actualType
                                methodConstraint = Constraint className actualType
                                methodScheme = Forall (Set.toList $ freeTyVars actualType) [methodConstraint] methodType
                            
                            -- Check if instance has nested constraints
                            -- If so, dictionary is a function that takes dict parameters
                            dictExprBase <- if null (instContext inst)
                                  then do
                                    -- No constraints: dictionary is a simple hash
                                    let dictExpr = TIExpr (Forall [] [] dictHashType) (TIVarExpr dictName)
                                    return dictExpr
                                  else do
                                    -- Has constraints: dictionary is a function
                                    -- Need to resolve constraint arguments and apply them
                                    -- e.g., eqCollection eqInteger
                                    let dictFuncType = case dictHashType of
                                          TFun _ resultType -> TFun dictHashType resultType
                                          _ -> TFun (THash TString TAny) dictHashType
                                        dictFuncExpr = TIExpr (Forall [] [] dictFuncType) (TIVarExpr dictName)

                                    -- Substitute type variables in constraints with actual types
                                    -- e.g., for instance {Eq a} Eq [a] matched with [Integer]
                                    -- instType inst = [a], actualType = [Integer]
                                    -- constraint {Eq a} should become {Eq Integer}
                                    -- Substitute type variables in constraints
                                    -- e.g., instance {Eq a} Eq [a] matched with [[Integer]]
                                    -- instType = [a], actualType = [[Integer]]
                                    -- Extract a -> [Integer], apply to {Eq a} -> {Eq [Integer]}
                                    let substitutedConstraints = substituteInstanceConstraints (instType inst) actualType (instContext inst)
                                    -- Resolve each substituted constraint (depth is managed internally)
                                    dictArgs <- mapM (resolveDictionaryArg classEnv') substitutedConstraints
                                    -- Apply dictionary function to constraint dictionaries
                                    return $ TIExpr (Forall [] [] dictHashType) (TIApplyExpr dictFuncExpr dictArgs)

                                -- Now index into the dictionary (which is now a hash)
                            let indexExpr = TIExpr (Forall [] [] TString) 
                                                  (TIConstantExpr (StringExpr (pack methodKey)))
                                dictAccess = TIExpr methodScheme $
                                             TIIndexedExpr False dictExprBase [Sub indexExpr]
                            -- Apply arguments: dictAccess arg1 arg2 ...
                            return $ Just $ TIApplyExpr dictAccess expandedArgs
                          Nothing -> return Nothing
                else return Nothing
            Nothing -> return Nothing
    
    -- Substitute type variables in instance constraints based on actual type
    -- e.g., for instance {Eq a} Eq [a] matched with [[Integer]]
    -- instType = [a], actualType = [[Integer]]
    -- Extract: a -> [Integer], then apply to constraints {Eq a} -> {Eq [Integer]}
    substituteInstanceConstraints :: Type -> Type -> [Constraint] -> [Constraint]
    substituteInstanceConstraints instType actualType constraints =
      let substs = extractAllTypeSubsts instType actualType
      in map (applySubstsToConstraint substs) constraints
      where
        -- Extract all type variable substitutions
        extractAllTypeSubsts :: Type -> Type -> [(TyVar, Type)]
        extractAllTypeSubsts instTy actualTy = go instTy actualTy
          where
            go (TVar v) actual = [(v, actual)]
            go (TCollection instElem) (TCollection actualElem) = go instElem actualElem
            go (TTuple instTypes) (TTuple actualTypes)
              | length instTypes == length actualTypes =
                  concatMap (\(i, a) -> go i a) (zip instTypes actualTypes)
            go (TInductive _ instArgs) (TInductive _ actualArgs)
              | length instArgs == length actualArgs =
                  concatMap (\(i, a) -> go i a) (zip instArgs actualArgs)
            go (TTensor instElem) (TTensor actualElem) = go instElem actualElem
            go (TFun instArg instRet) (TFun actualArg actualRet) =
              go instArg actualArg ++ go instRet actualRet
            go _ _ = []
        
        -- Apply multiple substitutions to a constraint
        applySubstsToConstraint :: [(TyVar, Type)] -> Constraint -> Constraint
        applySubstsToConstraint substs (Constraint cName cType) =
          Constraint cName (applySubstsToType substs cType)
        
        -- Apply multiple substitutions to a type
        applySubstsToType :: [(TyVar, Type)] -> Type -> Type
        applySubstsToType substs = go
          where
            go t@(TVar v) = case lookup v substs of
                              Just newType -> newType
                              Nothing -> t
            go TInt = TInt
            go TFloat = TFloat
            go TBool = TBool
            go TChar = TChar
            go TString = TString
            go (TCollection t) = TCollection (go t)
            go (TTuple ts) = TTuple (map go ts)
            go (TInductive name ts) = TInductive name (map go ts)
            go (TTensor t) = TTensor (go t)
            go (THash k v) = THash (go k) (go v)
            go (TMatcher t) = TMatcher (go t)
            go (TFun t1 t2) = TFun (go t1) (go t2)
            go (TIO t) = TIO (go t)
            go (TIORef t) = TIORef (go t)
            go TPort = TPort
            go TAny = TAny
    
    
    -- Find a constraint that provides the given method
    findConstraintForMethod :: ClassEnv -> String -> [Constraint] -> Maybe Constraint
    findConstraintForMethod env methodName cs = 
      find (\(Constraint className _) ->
        case lookupClass className env of
          Just classInfo -> methodName `elem` map fst (classMethods classInfo)
          Nothing -> False
      ) cs
    
    -- Resolve a constraint to a dictionary argument (with depth limit to prevent infinite recursion)
    resolveDictionaryArg :: ClassEnv -> Constraint -> EvalM TIExpr
    resolveDictionaryArg classEnv constraint = resolveDictionaryArgWithDepth classEnv 50 constraint
    
    resolveDictionaryArgWithDepth :: ClassEnv -> Int -> Constraint -> EvalM TIExpr
    resolveDictionaryArgWithDepth _ 0 (Constraint className _) = do
      -- Depth limit reached, return error placeholder
      return $ TIExpr (Forall [] [] (TVar (TyVar "error"))) (TIVarExpr ("dict_" ++ className ++ "_TOO_DEEP"))
    
    resolveDictionaryArgWithDepth classEnv depth (Constraint className tyArg) = do
      case tyArg of
        TVar (TyVar _v) -> do
          -- Type variable: use dictionary parameter name (without type parameter)
          -- e.g., for {Eq a}, return dict_Eq
          let dictParamName = "dict_" ++ className
              dictType = TVar (TyVar "dict")
          return $ TIExpr (Forall [] [] dictType) (TIVarExpr dictParamName)
        _ -> do
          -- Concrete type: try to find matching instance
          let instances = lookupInstances className classEnv
          case findMatchingInstanceForType tyArg instances of
            Just inst -> do
              -- Found instance: generate dictionary name (e.g., "numInteger", "eqCollection")
              let instTypeName = typeConstructorName (instType inst)
                  dictName = lowerFirst className ++ instTypeName
                  dictType = TVar (TyVar "dict")
                  dictExpr = TIExpr (Forall [] [] dictType) (TIVarExpr dictName)
              
              -- Check if this instance has nested constraints
              -- e.g., instance {Eq a} Eq [a] has constraint {Eq a}
              if null (instContext inst)
                then do
                  -- No constraints: return simple dictionary reference
                  return dictExpr
                else do
                  -- Has constraints: need to resolve them and apply to dictionary
                  -- e.g., for Eq [Integer], resolve {Eq Integer} -> eqInteger
                  -- then return: eqCollection eqInteger
                  
                  -- Substitute type variables in constraints with actual types
                  -- e.g., for instance {Eq a} Eq [a] matched with [[Integer]]
                  -- instType inst = [a], tyArg = [[Integer]]
                  -- Extract: a -> [Integer]
                  -- Apply to constraints: {Eq a} -> {Eq [Integer]}
                  let substs = extractTypeSubstitutions (instType inst) tyArg
                      substitutedConstraints = map (applyTypeSubstsToConstraint substs) (instContext inst)
                  
                  -- Recursively resolve each constraint with reduced depth
                  dictArgs <- mapM (resolveDictionaryArgWithDepth classEnv (depth - 1)) substitutedConstraints
                  
                  -- Apply dictionary function to resolved dictionaries
                  -- e.g., eqCollection eqInteger (when resolving Eq [Integer])
                  --       eqCollection (eqCollection eqInteger) (when resolving Eq [[Integer]])
                  return $ TIExpr (Forall [] [] dictType) (TIApplyExpr dictExpr dictArgs)
            Nothing -> do
              -- No instance found - this is an error, but return a dummy for now
              return $ TIExpr (Forall [] [] (TVar (TyVar "error"))) (TIVarExpr "undefined")
      where
        -- Extract type variable substitutions
        -- e.g., [a] -> [[Integer]] gives [(a, [Integer])]
        extractTypeSubstitutions :: Type -> Type -> [(TyVar, Type)]
        extractTypeSubstitutions instTy actualTy = go instTy actualTy
          where
            go (TVar v) actual = [(v, actual)]
            go (TCollection instElem) (TCollection actualElem) = go instElem actualElem
            go (TTuple instTypes) (TTuple actualTypes)
              | length instTypes == length actualTypes =
                  concatMap (\(i, a) -> go i a) (zip instTypes actualTypes)
            go (TInductive _ instArgs) (TInductive _ actualArgs)
              | length instArgs == length actualArgs =
                  concatMap (\(i, a) -> go i a) (zip instArgs actualArgs)
            go (TTensor instElem) (TTensor actualElem) = go instElem actualElem
            go (TFun instArg instRet) (TFun actualArg actualRet) =
              go instArg actualArg ++ go instRet actualRet
            go (THash instK instV) (THash actualK actualV) =
              go instK actualK ++ go instV actualV
            go (TMatcher instT) (TMatcher actualT) = go instT actualT
            go (TIO instT) (TIO actualT) = go instT actualT
            go (TIORef instT) (TIORef actualT) = go instT actualT
            go TPort TPort = []
            go _ _ = []
        
        -- Apply type substitutions to a constraint
        applyTypeSubstsToConstraint :: [(TyVar, Type)] -> Constraint -> Constraint
        applyTypeSubstsToConstraint substs (Constraint cName cType) =
          Constraint cName (applyTypeSubstsToType substs cType)
        
        -- Apply type substitutions to a type
        applyTypeSubstsToType :: [(TyVar, Type)] -> Type -> Type
        applyTypeSubstsToType substs = go
          where
            go t@(TVar v) = case lookup v substs of
                              Just newType -> newType
                              Nothing -> t
            go TInt = TInt
            go TFloat = TFloat
            go TBool = TBool
            go TChar = TChar
            go TString = TString
            go (TCollection t) = TCollection (go t)
            go (TTuple ts) = TTuple (map go ts)
            go (TInductive name ts) = TInductive name (map go ts)
            go (TTensor t) = TTensor (go t)
            go (THash k v) = THash (go k) (go v)
            go (TMatcher t) = TMatcher (go t)
            go (TFun t1 t2) = TFun (go t1) (go t2)
            go (TIO t) = TIO (go t)
            go (TIORef t) = TIORef (go t)
            go TPort = TPort
            go TAny = TAny
    
    -- Helper: lowercase first character
    lowerFirst :: String -> String
    lowerFirst [] = []
    lowerFirst (c:cs) = toLower c : cs

-- | Generate dictionary parameter name from constraint
-- Used for both dictionary parameter generation and dictionary argument passing
-- Type parameters are not included in the dictionary parameter name
constraintToDictParam :: Constraint -> String
constraintToDictParam (Constraint className _constraintType) =
  "dict_" ++ className

-- | Get method type from ClassEnv
-- This retrieves the method type from the class definition and substitutes type variables
-- Note: methodKey is the sanitized name (e.g., "plus"), but classMethods uses original names (e.g., "+")
-- We need to try both the sanitized and original names
getMethodTypeFromClass :: ClassEnv -> String -> String -> Type -> Type
getMethodTypeFromClass classEnv className methodKey constraintType =
  case lookupClass className classEnv of
    Just classInfo ->
      -- Try to find the method by sanitized name first, then try unsanitizing
      case lookup methodKey (classMethods classInfo) `mplus` lookupUnsanitized methodKey (classMethods classInfo) of
        Just classMethodType ->
          -- Substitute class type parameter with actual constraint type
          -- e.g., class Num a has plus : a -> a -> a
          --       constraint Num t0 → plus : t0 -> t0 -> t0
          applyTypeSubstsToType [(classParam classInfo, constraintType)] classMethodType
        Nothing -> TAny  -- Method not found in class
    Nothing -> TAny  -- Class not found
  where
    -- Lookup by unsanitizing the method key (reverse of sanitizeMethodName)
    -- e.g., "plus" -> "+", "times" -> "*"
    lookupUnsanitized :: String -> [(String, a)] -> Maybe a
    lookupUnsanitized key methods =
      case unsanitizeMethodName key of
        Just originalName -> lookup originalName methods
        Nothing -> Nothing
    
    -- Reverse of sanitizeMethodName
    unsanitizeMethodName :: String -> Maybe String
    unsanitizeMethodName "eq" = Just "=="
    unsanitizeMethodName "neq" = Just "/="
    unsanitizeMethodName "lt" = Just "<"
    unsanitizeMethodName "le" = Just "<="
    unsanitizeMethodName "gt" = Just ">"
    unsanitizeMethodName "ge" = Just ">="
    unsanitizeMethodName "plus" = Just "+"
    unsanitizeMethodName "minus" = Just "-"
    unsanitizeMethodName "times" = Just "*"
    unsanitizeMethodName "div" = Just "/"
    unsanitizeMethodName _ = Nothing
    
    -- Apply type substitutions to a type
    applyTypeSubstsToType :: [(TyVar, Type)] -> Type -> Type
    applyTypeSubstsToType substs = go
      where
        go t@(TVar v) = case lookup v substs of
                          Just newType -> newType
                          Nothing -> t
        go TInt = TInt
        go TFloat = TFloat
        go TBool = TBool
        go TChar = TChar
        go TString = TString
        go (TCollection t) = TCollection (go t)
        go (TTuple ts) = TTuple (map go ts)
        go (TInductive name ts) = TInductive name (map go ts)
        go (TTensor t) = TTensor (go t)
        go (THash k v) = THash (go k) (go v)
        go (TMatcher t) = TMatcher (go t)
        go (TFun t1 t2) = TFun (go t1) (go t2)
        go (TIO t) = TIO (go t)
        go (TIORef t) = TIORef (go t)
        go TPort = TPort
        go TAny = TAny

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
        -- BUT: if body is a hash (dictionary), don't process it
        body' <- case tiExprNode body of
                   TIHashExpr _ -> return body  -- Dictionary body, don't process
                   _ -> replaceMethodCallsWithDictAccessT env cs body
        let newNode = TILambdaExpr mVar (dictVars ++ params) body'
        return $ TIExpr (tiScheme expr) newNode
      
      -- Hash (dictionary definition): wrap in lambda AND apply dict params to methods
      -- Dictionary values are method references that need dictionary parameters
      TIHashExpr pairs -> do
        let dictParams = map constraintToDictParam cs
            dictVars = map stringToVar dictParams
            wrapperType = tiExprType expr
        
        -- For each value in the hash (which is a method reference),
        -- if it has constraints, apply dictionary parameters to it
        pairs' <- mapM (\(k, v) -> do
          -- Check if the value (method) has constraints
          typeEnv <- getTypeEnv
          let vNode = tiExprNode v
          case vNode of
            TIVarExpr methodName -> do
              case lookupEnv (stringToVar methodName) typeEnv of
                Just (Forall _ vConstraints _) | not (null vConstraints) -> do
                  -- Method has constraints, apply dictionary parameters
                  let dictArgExprs = map (\p -> TIExpr (Forall [] [] (TVar (TyVar "dict"))) (TIVarExpr p)) dictParams
                      vApplied = TIExpr (tiScheme v) (TIApplyExpr v dictArgExprs)
                  return (k, vApplied)
                _ -> return (k, v)  -- No constraints, keep as-is
            _ -> return (k, v)  -- Not a variable, keep as-is
          ) pairs
        
        let hashExpr' = TIExpr (tiScheme expr) (TIHashExpr pairs')
            newNode = TILambdaExpr Nothing dictVars hashExpr'
            newScheme = Forall [] [] wrapperType
        return $ TIExpr newScheme newNode
      
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
      let scheme@(Forall _ exprConstraints exprType) = tiScheme tiExpr
      newNode <- replaceMethodCallsInNode env cs exprConstraints exprType (tiExprNode tiExpr)
      return $ TIExpr scheme newNode
    
    -- Replace method calls in TIExprNode
    replaceMethodCallsInNode :: ClassEnv -> [Constraint] -> [Constraint] -> Type -> TIExprNode -> EvalM TIExprNode
    replaceMethodCallsInNode env cs exprConstraints exprType node = case node of
      -- Standalone method reference: eta-expand
      TIVarExpr methodName -> do
        case findConstraintForMethodInList env methodName cs of
          Just constraint -> do
            -- Get method type to determine arity
            typeEnv <- getTypeEnv
            case lookupEnv (stringToVar methodName) typeEnv of
              Just (Forall _ _ _ty) -> do
                -- Use the expression's actual type (exprType) instead of the method's declared type (ty)
                -- because eta-expansion should create parameters matching the expected usage context
                let arity = getMethodArity exprType
                    paramTypes = getParamTypes exprType
                    paramNames = ["etaVar" ++ show i | i <- [1..arity]]
                    paramVars = map stringToVar paramNames
                    paramExprs = zipWith (\n t -> TIExpr (Forall [] [] t) (TIVarExpr n)) paramNames paramTypes
                    -- Create dictionary access
                    dictParam = constraintToDictParam constraint
                    Constraint className tyArg = constraint
                -- Look up dictionary type from type environment
                dictHashType <- case lookupEnv (stringToVar dictParam) typeEnv of
                  Just (Forall _ _ dictType) -> return dictType
                  Nothing -> return $ THash TString TAny  -- Fallback
                -- Get method type from ClassEnv instead of dictHashType
                let methodType = getMethodTypeFromClass env className (sanitizeMethodName methodName) tyArg
                    methodConstraint = Constraint className tyArg
                    methodScheme = Forall (Set.toList $ freeTyVars tyArg) [methodConstraint] methodType
                    indexExpr = TIExpr (Forall [] [] TString) 
                                      (TIConstantExpr (StringExpr (pack (sanitizeMethodName methodName))))
                    dictAccess = TIExpr methodScheme $
                                 TIIndexedExpr False
                                   (TIExpr (Forall [] [] dictHashType) (TIVarExpr dictParam))
                                   [Sub indexExpr]
                    -- Create: dictAccess etaVar1 etaVar2 ... etaVarN
                    body = TIExpr methodScheme (TIApplyExpr dictAccess paramExprs)
                return $ TILambdaExpr Nothing paramVars body
              Nothing -> return $ TIVarExpr methodName
          Nothing -> do
            -- Not a method - check if it's a constrained variable (e.g., dotProduct)
            -- and the expression itself has constraints that match the parent constraints
            if not (null exprConstraints)
              then do
                -- Check which constraints from exprConstraints match parent constraints cs
                let matchingConstraints = filter (\(Constraint eName _) ->
                      any (\(Constraint pName _) -> eName == pName) cs) exprConstraints
                if null matchingConstraints
                  then return $ TIVarExpr methodName
                  else do
                    -- Apply matching dictionary parameters
                    let dictParams = map constraintToDictParam matchingConstraints
                        dictArgExprs = map (\p -> TIExpr (Forall [] [] (TVar (TyVar "dict"))) (TIVarExpr p)) dictParams
                        varExpr = TIExpr (Forall [] exprConstraints (TVar (TyVar "var"))) (TIVarExpr methodName)
                    return $ TIApplyExpr varExpr dictArgExprs
              else return $ TIVarExpr methodName
      
      -- Method call: replace with dictionary access
      TIApplyExpr func args -> do
        case tiExprNode func of
          TIVarExpr methodName -> do
            case findConstraintForMethodInList env methodName cs of
              Just constraint -> do
                -- Replace with dictionary access
                typeEnv <- getTypeEnv
                let dictParam = constraintToDictParam constraint
                    Constraint className tyArg = constraint
                -- Look up dictionary type from type environment
                dictHashType <- case lookupEnv (stringToVar dictParam) typeEnv of
                  Just (Forall _ _ dictType) -> return dictType
                  Nothing -> return $ THash TString TAny  -- Fallback
                -- Get method type from ClassEnv instead of dictHashType
                let methodType = getMethodTypeFromClass env className (sanitizeMethodName methodName) tyArg
                    methodConstraint = Constraint className tyArg
                    methodScheme = Forall (Set.toList $ freeTyVars tyArg) [methodConstraint] methodType
                    indexExpr = TIExpr (Forall [] [] TString) 
                                      (TIConstantExpr (StringExpr (pack (sanitizeMethodName methodName))))
                    dictAccessNode = TIIndexedExpr False
                                     (TIExpr (Forall [] [] dictHashType) (TIVarExpr dictParam))
                                     [Sub indexExpr]
                    dictAccess = TIExpr methodScheme dictAccessNode
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
      
      -- Hash: do NOT process values inside dictionary hashes
      -- Dictionary values should remain as simple references
      -- e.g., {| ("eq", eqCollectionEq), ... |} not {| ("eq", eqCollectionEq dict_Eq), ... |}
      -- We return the node as-is without recursively processing the pairs
      TIHashExpr pairs -> do
        -- Process only keys, not values (values should remain as method references)
        pairs' <- mapM (\(k, v) -> do
          k' <- replaceMethodCallsWithDictAccessT env cs k
          -- Do NOT process v - keep it as a simple reference
          return (k', v)) pairs
        return $ TIHashExpr pairs'
      
      -- Matcher: recursively process expressions inside matcher definitions
      TIMatcherExpr patDefs -> do
        patDefs' <- mapM (\(pat, matcherExpr, bindings) -> do
          -- Process the next-matcher expression
          matcherExpr' <- replaceMethodCallsWithDictAccessT env cs matcherExpr
          -- Process expressions in primitive-data-match clauses
          bindings' <- mapM (\(dp, expr) -> do
            expr' <- replaceMethodCallsWithDictAccessT env cs expr
            return (dp, expr')) bindings
          return (pat, matcherExpr', bindings')) patDefs
        return $ TIMatcherExpr patDefs'
      
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
    
    -- Get parameter types from function type
    getParamTypes :: Type -> [Type]
    getParamTypes (TFun t1 t2) = t1 : getParamTypes t2
    getParamTypes _ = []

-- | Apply dictionaries to expressions with concrete type constraints
-- This is used for top-level definitions like: def integer : Matcher Integer := eq
-- where the right-hand side (eq) has concrete type constraints {Eq Integer}
applyConcreteConstraintDictionaries :: TIExpr -> EvalM TIExpr
applyConcreteConstraintDictionaries expr = do
  classEnv <- getClassEnv
  let scheme@(Forall _ constraints _) = tiScheme expr
  
  -- Check if all constraints are on concrete types
  let isConcreteConstraint (Constraint _ (TVar _)) = False
      isConcreteConstraint _ = True
      hasOnlyConcreteConstraints = not (null constraints) && all isConcreteConstraint constraints
  
  if hasOnlyConcreteConstraints
    then do
      -- Apply dictionaries for concrete constraints
      dictArgs <- mapM (resolveDictionaryForConstraint classEnv) constraints
      -- Create application: expr dict1 dict2 ...
      let resultType = tiExprType expr
          -- Update scheme to remove constraints since they are now applied
          newScheme = Forall [] [] resultType
      return $ TIExpr newScheme (TIApplyExpr expr dictArgs)
    else
      -- No concrete constraints, return as-is
      return expr
  where
    -- Resolve dictionary for a concrete constraint
    resolveDictionaryForConstraint :: ClassEnv -> Constraint -> EvalM TIExpr
    resolveDictionaryForConstraint classEnv (Constraint className tyArg) = do
      let instances = lookupInstances className classEnv
          lowerFirstChar [] = []
          lowerFirstChar (c:cs) = toLower c : cs
      case findMatchingInstanceForType tyArg instances of
        Just inst -> do
          -- Generate dictionary name (e.g., "eqInteger", "numInteger")
          let instTypeName = typeConstructorName (instType inst)
              dictName = lowerFirstChar className ++ instTypeName
              dictType = TVar (TyVar "dict")
              dictExpr = TIExpr (Forall [] [] dictType) (TIVarExpr dictName)
          
          -- Check if instance has nested constraints
          if null (instContext inst)
            then do
              -- No constraints: return simple dictionary reference
              return dictExpr
            else do
              -- Has constraints: need to resolve them recursively
              nestedDictArgs <- mapM (resolveDictionaryForConstraint classEnv) (instContext inst)
              return $ TIExpr (Forall [] [] dictType) (TIApplyExpr dictExpr nestedDictArgs)
        Nothing -> do
          -- No instance found - return dummy dictionary
          let dictName = "dict_" ++ className ++ "_NOT_FOUND"
              dictType = TVar (TyVar "dict")
          return $ TIExpr (Forall [] [] dictType) (TIVarExpr dictName)
