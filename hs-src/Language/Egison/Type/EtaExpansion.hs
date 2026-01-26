{- |
Module      : Language.Egison.Type.EtaExpansion
Licence     : MIT

This module implements eta expansion for type class methods.
It runs BEFORE TensorMapInsertion and TypeClassExpand in Phase 8.

The purpose is to transform type class method references into explicit lambda expressions:
  (+)  becomes  \etaVar1 etaVar2 -> (+) etaVar1 etaVar2

This allows TensorMapInsertion to properly wrap binary functions with tensorMap2,
and TypeClassExpand to later replace the method reference with dictionary access.

Processing order in Phase 8:
  1. EtaExpansion (this module) - eta-expand type class methods
  2. TensorMapInsertion - insert tensorMap/tensorMap2 where needed
  3. TypeClassExpand - replace method references with dictionary access
-}

module Language.Egison.Type.EtaExpansion
  ( etaExpandTypeClassMethods
  , etaExpandTopExpr
  ) where

import           Language.Egison.Data       (EvalM)
import           Language.Egison.EvalState  (MonadEval(..))
import           Language.Egison.IExpr      (TIExpr(..), TIExprNode(..), TITopExpr(..),
                                             TIPattern(..), TIPatternNode(..), TILoopRange(..),
                                             Index(..), stringToVar, tiExprType, tiScheme, tiExprNode)
import           Language.Egison.Type.Env   (ClassEnv(..), ClassInfo(..), lookupClass)
import           Language.Egison.Type.Types (Type(..), TypeScheme(..), Constraint(..))

--------------------------------------------------------------------------------
-- * Eta Expansion of Type Class Methods
--------------------------------------------------------------------------------

-- | Eta-expand type class methods in a TIExpr
-- This transforms standalone method references like (+) into
-- lambda expressions like \x y -> (+) x y
etaExpandTypeClassMethods :: TIExpr -> EvalM TIExpr
etaExpandTypeClassMethods tiExpr = do
  classEnv <- getClassEnv
  let scheme = tiScheme tiExpr
  expandedNode <- etaExpandNode classEnv scheme (tiExprNode tiExpr)
  return $ TIExpr scheme expandedNode

-- | Eta-expand type class methods in a top-level expression
etaExpandTopExpr :: TITopExpr -> EvalM TITopExpr
etaExpandTopExpr topExpr = case topExpr of
  TIDefine scheme var tiexpr -> do
    tiexpr' <- etaExpandTypeClassMethods tiexpr
    return $ TIDefine scheme var tiexpr'

  TITest tiexpr -> do
    tiexpr' <- etaExpandTypeClassMethods tiexpr
    return $ TITest tiexpr'

  TIExecute tiexpr -> do
    tiexpr' <- etaExpandTypeClassMethods tiexpr
    return $ TIExecute tiexpr'

  TILoadFile path ->
    return $ TILoadFile path

  TILoad lib ->
    return $ TILoad lib

  TIDefineMany bindings -> do
    bindings' <- mapM (\(var, tiexpr) -> do
      tiexpr' <- etaExpandTypeClassMethods tiexpr
      return (var, tiexpr')) bindings
    return $ TIDefineMany bindings'

  TIDeclareSymbol names ty ->
    return $ TIDeclareSymbol names ty

--------------------------------------------------------------------------------
-- * Internal Implementation
--------------------------------------------------------------------------------

-- | Expand a TIExprNode, handling type class method eta expansion
etaExpandNode :: ClassEnv -> TypeScheme -> TIExprNode -> EvalM TIExprNode
etaExpandNode classEnv scheme@(Forall _ constraints _) node = case node of
  -- Constants: no expansion needed
  TIConstantExpr c -> return $ TIConstantExpr c

  -- Variables: check if it's a type class method that needs eta expansion
  TIVarExpr name -> do
    let exprType = schemeType scheme
    case findConstraintForMethod classEnv name constraints of
      Just _ -> do
        -- This is a type class method - eta expand it
        let arity = getMethodArity exprType
        if arity > 0
          then return $ etaExpandMethod name exprType arity
          else return $ TIVarExpr name
      Nothing ->
        -- Not a type class method - leave as is
        return $ TIVarExpr name

  -- Lambda expressions: recursively process body
  TILambdaExpr mVar params body -> do
    body' <- etaExpandExprWithConstraints classEnv constraints body
    return $ TILambdaExpr mVar params body'

  TIMemoizedLambdaExpr names body -> do
    body' <- etaExpandExprWithConstraints classEnv constraints body
    return $ TIMemoizedLambdaExpr names body'

  TICambdaExpr name body -> do
    body' <- etaExpandExprWithConstraints classEnv constraints body
    return $ TICambdaExpr name body'

  -- Application: recursively process function and arguments
  TIApplyExpr func args -> do
    -- Check if func is a type class method being applied
    -- If it's already being applied, don't eta-expand it
    func' <- case tiExprNode func of
      TIVarExpr methodName -> do
        -- Check if this is a type class method
        case findConstraintForMethod classEnv methodName constraints of
          Just _ ->
            -- It's a method being applied - keep it as is (will be resolved by TypeClassExpand)
            return func
          Nothing ->
            -- Not a method - process normally
            etaExpandExprWithConstraints classEnv constraints func
      _ ->
        etaExpandExprWithConstraints classEnv constraints func
    args' <- mapM (etaExpandExprWithConstraints classEnv constraints) args
    return $ TIApplyExpr func' args'

  -- Collections
  TITupleExpr exprs -> do
    exprs' <- mapM (etaExpandExprWithConstraints classEnv constraints) exprs
    return $ TITupleExpr exprs'

  TICollectionExpr exprs -> do
    exprs' <- mapM (etaExpandExprWithConstraints classEnv constraints) exprs
    return $ TICollectionExpr exprs'

  TIConsExpr h t -> do
    h' <- etaExpandExprWithConstraints classEnv constraints h
    t' <- etaExpandExprWithConstraints classEnv constraints t
    return $ TIConsExpr h' t'

  TIJoinExpr l r -> do
    l' <- etaExpandExprWithConstraints classEnv constraints l
    r' <- etaExpandExprWithConstraints classEnv constraints r
    return $ TIJoinExpr l' r'

  TIHashExpr pairs -> do
    pairs' <- mapM (\(k, v) -> do
      k' <- etaExpandExprWithConstraints classEnv constraints k
      v' <- etaExpandExprWithConstraints classEnv constraints v
      return (k', v')) pairs
    return $ TIHashExpr pairs'

  TIVectorExpr exprs -> do
    exprs' <- mapM (etaExpandExprWithConstraints classEnv constraints) exprs
    return $ TIVectorExpr exprs'

  -- Control flow
  TIIfExpr cond thenExpr elseExpr -> do
    cond' <- etaExpandExprWithConstraints classEnv constraints cond
    thenExpr' <- etaExpandExprWithConstraints classEnv constraints thenExpr
    elseExpr' <- etaExpandExprWithConstraints classEnv constraints elseExpr
    return $ TIIfExpr cond' thenExpr' elseExpr'

  -- Let bindings
  TILetExpr bindings body -> do
    bindings' <- mapM (\(v, e) -> do
      e' <- etaExpandExprWithConstraints classEnv constraints e
      return (v, e')) bindings
    body' <- etaExpandExprWithConstraints classEnv constraints body
    return $ TILetExpr bindings' body'

  TILetRecExpr bindings body -> do
    bindings' <- mapM (\(v, e) -> do
      e' <- etaExpandExprWithConstraints classEnv constraints e
      return (v, e')) bindings
    body' <- etaExpandExprWithConstraints classEnv constraints body
    return $ TILetRecExpr bindings' body'

  TIWithSymbolsExpr syms body -> do
    body' <- etaExpandExprWithConstraints classEnv constraints body
    return $ TIWithSymbolsExpr syms body'

  TISeqExpr e1 e2 -> do
    e1' <- etaExpandExprWithConstraints classEnv constraints e1
    e2' <- etaExpandExprWithConstraints classEnv constraints e2
    return $ TISeqExpr e1' e2'

  -- Match expressions
  TIMatchExpr strategy target matcher clauses -> do
    target' <- etaExpandExprWithConstraints classEnv constraints target
    matcher' <- etaExpandExprWithConstraints classEnv constraints matcher
    clauses' <- mapM (\(pat, body) -> do
      pat' <- etaExpandPattern classEnv constraints pat
      body' <- etaExpandExprWithConstraints classEnv constraints body
      return (pat', body')) clauses
    return $ TIMatchExpr strategy target' matcher' clauses'

  TIMatchAllExpr strategy target matcher clauses -> do
    target' <- etaExpandExprWithConstraints classEnv constraints target
    matcher' <- etaExpandExprWithConstraints classEnv constraints matcher
    clauses' <- mapM (\(pat, body) -> do
      pat' <- etaExpandPattern classEnv constraints pat
      body' <- etaExpandExprWithConstraints classEnv constraints body
      return (pat', body')) clauses
    return $ TIMatchAllExpr strategy target' matcher' clauses'

  TIMatcherExpr defs -> do
    defs' <- mapM (\(ppat, matcherBody, bindings) -> do
      bindings' <- mapM (\(p, e) -> do
        e' <- etaExpandExprWithConstraints classEnv constraints e
        return (p, e')) bindings
      return (ppat, matcherBody, bindings')) defs
    return $ TIMatcherExpr defs'

  -- Inductive data
  TIInductiveDataExpr name exprs -> do
    exprs' <- mapM (etaExpandExprWithConstraints classEnv constraints) exprs
    return $ TIInductiveDataExpr name exprs'

  -- Quote expressions
  TIQuoteExpr e -> do
    e' <- etaExpandExprWithConstraints classEnv constraints e
    return $ TIQuoteExpr e'

  TIQuoteSymbolExpr e -> do
    e' <- etaExpandExprWithConstraints classEnv constraints e
    return $ TIQuoteSymbolExpr e'

  -- Indexed expressions
  TIIndexedExpr b expr indices -> do
    expr' <- etaExpandExprWithConstraints classEnv constraints expr
    indices' <- mapM (etaExpandIndex classEnv constraints) indices
    return $ TIIndexedExpr b expr' indices'

  TISubrefsExpr b expr ref -> do
    expr' <- etaExpandExprWithConstraints classEnv constraints expr
    ref' <- etaExpandExprWithConstraints classEnv constraints ref
    return $ TISubrefsExpr b expr' ref'

  TISuprefsExpr b expr ref -> do
    expr' <- etaExpandExprWithConstraints classEnv constraints expr
    ref' <- etaExpandExprWithConstraints classEnv constraints ref
    return $ TISuprefsExpr b expr' ref'

  TIUserrefsExpr b expr ref -> do
    expr' <- etaExpandExprWithConstraints classEnv constraints expr
    ref' <- etaExpandExprWithConstraints classEnv constraints ref
    return $ TIUserrefsExpr b expr' ref'

  -- Application variants
  TIWedgeApplyExpr e args -> do
    e' <- etaExpandExprWithConstraints classEnv constraints e
    args' <- mapM (etaExpandExprWithConstraints classEnv constraints) args
    return $ TIWedgeApplyExpr e' args'

  -- Do expressions
  TIDoExpr bindings body -> do
    bindings' <- mapM (\(v, e) -> do
      e' <- etaExpandExprWithConstraints classEnv constraints e
      return (v, e')) bindings
    body' <- etaExpandExprWithConstraints classEnv constraints body
    return $ TIDoExpr bindings' body'

  -- Tensor operations
  TIGenerateTensorExpr func size -> do
    func' <- etaExpandExprWithConstraints classEnv constraints func
    size' <- etaExpandExprWithConstraints classEnv constraints size
    return $ TIGenerateTensorExpr func' size'

  TITensorExpr e1 e2 -> do
    e1' <- etaExpandExprWithConstraints classEnv constraints e1
    e2' <- etaExpandExprWithConstraints classEnv constraints e2
    return $ TITensorExpr e1' e2'

  TITensorContractExpr tensor -> do
    tensor' <- etaExpandExprWithConstraints classEnv constraints tensor
    return $ TITensorContractExpr tensor'

  TITensorMapExpr func arg -> do
    func' <- etaExpandExprWithConstraints classEnv constraints func
    arg' <- etaExpandExprWithConstraints classEnv constraints arg
    return $ TITensorMapExpr func' arg'

  TITensorMap2Expr func arg1 arg2 -> do
    func' <- etaExpandExprWithConstraints classEnv constraints func
    arg1' <- etaExpandExprWithConstraints classEnv constraints arg1
    arg2' <- etaExpandExprWithConstraints classEnv constraints arg2
    return $ TITensorMap2Expr func' arg1' arg2'

  TITransposeExpr mapping tensor -> do
    mapping' <- etaExpandExprWithConstraints classEnv constraints mapping
    tensor' <- etaExpandExprWithConstraints classEnv constraints tensor
    return $ TITransposeExpr mapping' tensor'

  TIFlipIndicesExpr tensor -> do
    tensor' <- etaExpandExprWithConstraints classEnv constraints tensor
    return $ TIFlipIndicesExpr tensor'

  -- Function reference
  TIFunctionExpr args -> return $ TIFunctionExpr args

-- | Expand a TIExpr with constraint information
etaExpandExprWithConstraints :: ClassEnv -> [Constraint] -> TIExpr -> EvalM TIExpr
etaExpandExprWithConstraints classEnv parentConstraints expr = do
  let scheme@(Forall _ exprConstraints _) = tiScheme expr
      allConstraints = parentConstraints ++ exprConstraints
  expandedNode <- etaExpandNode classEnv (Forall [] allConstraints (tiExprType expr)) (tiExprNode expr)
  return $ TIExpr scheme expandedNode

-- | Eta-expand a type class method into a lambda expression
-- Example: (+) with type a -> a -> a becomes \etaVar1 etaVar2 -> (+) etaVar1 etaVar2
etaExpandMethod :: String -> Type -> Int -> TIExprNode
etaExpandMethod methodName funcType arity =
  let paramTypes = getParamTypes funcType
      paramNames = ["etaVar" ++ show i | i <- [1..arity]]
      paramVars = map stringToVar paramNames
      paramExprs = zipWith (\n t -> TIExpr (Forall [] [] t) (TIVarExpr n)) paramNames paramTypes
      resultType = getResultType funcType
      methodExpr = TIExpr (Forall [] [] funcType) (TIVarExpr methodName)
      bodyScheme = Forall [] [] resultType
      body = TIExpr bodyScheme (TIApplyExpr methodExpr paramExprs)
  in TILambdaExpr Nothing paramVars body

-- | Expand indices
etaExpandIndex :: ClassEnv -> [Constraint] -> Index TIExpr -> EvalM (Index TIExpr)
etaExpandIndex classEnv constraints idx = case idx of
  Sub e -> do
    e' <- etaExpandExprWithConstraints classEnv constraints e
    return $ Sub e'
  Sup e -> do
    e' <- etaExpandExprWithConstraints classEnv constraints e
    return $ Sup e'
  SupSub e -> do
    e' <- etaExpandExprWithConstraints classEnv constraints e
    return $ SupSub e'
  User e -> do
    e' <- etaExpandExprWithConstraints classEnv constraints e
    return $ User e'
  DF n1 n2 -> return $ DF n1 n2

-- | Expand patterns
etaExpandPattern :: ClassEnv -> [Constraint] -> TIPattern -> EvalM TIPattern
etaExpandPattern classEnv constraints (TIPattern scheme node) = do
  node' <- etaExpandPatternNode classEnv constraints node
  return $ TIPattern scheme node'

-- | Expand pattern nodes
etaExpandPatternNode :: ClassEnv -> [Constraint] -> TIPatternNode -> EvalM TIPatternNode
etaExpandPatternNode classEnv cs node = case node of
  TILoopPat var loopRange pat1 pat2 -> do
    loopRange' <- etaExpandLoopRange classEnv cs loopRange
    pat1' <- etaExpandPattern classEnv cs pat1
    pat2' <- etaExpandPattern classEnv cs pat2
    return $ TILoopPat var loopRange' pat1' pat2'

  TIAndPat pat1 pat2 -> do
    pat1' <- etaExpandPattern classEnv cs pat1
    pat2' <- etaExpandPattern classEnv cs pat2
    return $ TIAndPat pat1' pat2'

  TIOrPat pat1 pat2 -> do
    pat1' <- etaExpandPattern classEnv cs pat1
    pat2' <- etaExpandPattern classEnv cs pat2
    return $ TIOrPat pat1' pat2'

  TIForallPat pat1 pat2 -> do
    pat1' <- etaExpandPattern classEnv cs pat1
    pat2' <- etaExpandPattern classEnv cs pat2
    return $ TIForallPat pat1' pat2'

  TINotPat pat -> do
    pat' <- etaExpandPattern classEnv cs pat
    return $ TINotPat pat'

  TIInductivePat name pats -> do
    pats' <- mapM (etaExpandPattern classEnv cs) pats
    return $ TIInductivePat name pats'

  TITuplePat pats -> do
    pats' <- mapM (etaExpandPattern classEnv cs) pats
    return $ TITuplePat pats'

  TIPApplyPat func pats -> do
    func' <- etaExpandExprWithConstraints classEnv cs func
    pats' <- mapM (etaExpandPattern classEnv cs) pats
    return $ TIPApplyPat func' pats'

  TIDApplyPat func pats -> do
    func' <- etaExpandPattern classEnv cs func
    pats' <- mapM (etaExpandPattern classEnv cs) pats
    return $ TIDApplyPat func' pats'

  TIInductiveOrPApplyPat name pats -> do
    pats' <- mapM (etaExpandPattern classEnv cs) pats
    return $ TIInductiveOrPApplyPat name pats'

  TIIndexedPat pat indices -> do
    pat' <- etaExpandPattern classEnv cs pat
    indices' <- mapM (etaExpandExprWithConstraints classEnv cs) indices
    return $ TIIndexedPat pat' indices'

  TISeqNilPat -> return TISeqNilPat
  TISeqConsPat pat1 pat2 -> do
    pat1' <- etaExpandPattern classEnv cs pat1
    pat2' <- etaExpandPattern classEnv cs pat2
    return $ TISeqConsPat pat1' pat2'

  TILetPat bindings pat -> do
    bindings' <- mapM (\(v, e) -> do
      e' <- etaExpandExprWithConstraints classEnv cs e
      return (v, e')) bindings
    pat' <- etaExpandPattern classEnv cs pat
    return $ TILetPat bindings' pat'

  -- Leaf patterns: no expansion needed
  TIWildCard -> return TIWildCard
  TIPatVar name -> return $ TIPatVar name
  TIVarPat name -> return $ TIVarPat name
  TIValuePat expr -> do
    expr' <- etaExpandExprWithConstraints classEnv cs expr
    return $ TIValuePat expr'
  TIPredPat pred -> do
    pred' <- etaExpandExprWithConstraints classEnv cs pred
    return $ TIPredPat pred'
  TIContPat -> return TIContPat
  TILaterPatVar -> return TILaterPatVar

-- | Expand loop range
etaExpandLoopRange :: ClassEnv -> [Constraint] -> TILoopRange -> EvalM TILoopRange
etaExpandLoopRange classEnv cs (TILoopRange start end rangePat) = do
  start' <- etaExpandExprWithConstraints classEnv cs start
  end' <- etaExpandExprWithConstraints classEnv cs end
  rangePat' <- etaExpandPattern classEnv cs rangePat
  return $ TILoopRange start' end' rangePat'

--------------------------------------------------------------------------------
-- * Helper Functions
--------------------------------------------------------------------------------

-- | Get the type from a TypeScheme
schemeType :: TypeScheme -> Type
schemeType (Forall _ _ ty) = ty

-- | Get the arity of a method (number of arguments)
getMethodArity :: Type -> Int
getMethodArity (TFun _ t2) = 1 + getMethodArity t2
getMethodArity _ = 0

-- | Get the parameter types from a function type
getParamTypes :: Type -> [Type]
getParamTypes (TFun t1 t2) = t1 : getParamTypes t2
getParamTypes _ = []

-- | Get the result type of a function type
getResultType :: Type -> Type
getResultType (TFun _ t2) = getResultType t2
getResultType t = t

-- | Find a constraint that provides a given method
findConstraintForMethod :: ClassEnv -> String -> [Constraint] -> Maybe Constraint
findConstraintForMethod classEnv methodName constraints =
  let matchingConstraints = filter (constraintProvidesMethod classEnv methodName) constraints
  in case matchingConstraints of
       (c:_) -> Just c
       []    -> Nothing

-- | Check if a constraint provides a given method
constraintProvidesMethod :: ClassEnv -> String -> Constraint -> Bool
constraintProvidesMethod classEnv methodName (Constraint className _) =
  case lookupClass className classEnv of
    Just classInfo -> methodName `elem` map fst (classMethods classInfo)
    Nothing -> False
