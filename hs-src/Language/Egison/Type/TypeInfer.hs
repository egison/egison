{- |
Module      : Language.Egison.Type.TypeInfer
Licence     : MIT

This module provides type inference that produces typed AST.
It transforms untyped Expr into TypedExpr with inferred types.
-}

{-# LANGUAGE LambdaCase #-}

module Language.Egison.Type.TypeInfer
  ( -- * Type inference producing typed AST
    inferTypedExpr
  , inferTypedTopExpr
  , inferTypedPattern
    -- * Running inference
  , runTypedInfer
  , runTypedInferTopExpr
  , runTypedInferTopExprWithEnv
  , TypedInferResult(..)
  ) where

import           Control.Monad              (forM)
import           Control.Monad.Except       (throwError, catchError)
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Language.Egison.AST
import           Language.Egison.Type.Env
import           Language.Egison.Type.Error
import           Language.Egison.Type.Infer (Infer, InferState(..), InferConfig(..),
                                             freshVar, getEnv, setEnv, withEnv, 
                                             runInferWithWarnings, runInferWithWarningsAndState, typeExprToType,
                                             typedParamToType, extractTypedParamBindings,
                                             extractLetPatternBindings, generalize,
                                             inferConstant, lookupVar, unifyTypes,
                                             defaultInferConfig)
import qualified Language.Egison.Type.Infer as Infer
import           Language.Egison.Type.Subst (Subst, emptySubst, composeSubst, applySubst)
import           Language.Egison.Type.TypedAST
import           Language.Egison.Type.Types

-- | Result of typed inference
data TypedInferResult = TypedInferResult
  { tirExpr     :: TypedExpr
  , tirType     :: Type
  , tirWarnings :: [TypeWarning]
  } deriving (Show)

-- | Run typed inference on an expression
runTypedInfer :: InferState -> Expr -> IO (Either TypeError TypedInferResult, [TypeWarning])
runTypedInfer state expr = do
  (result, warnings) <- runInferWithWarnings (inferTypedExpr expr) state
  return $ case result of
    Left err -> (Left err, warnings)
    Right (texpr, _subst) -> (Right $ TypedInferResult texpr (texprType texpr) warnings, warnings)

-- | Run typed inference on a top expression
-- Returns Maybe TypedTopExpr (Nothing for declarations that don't produce code)
-- Note: Uses permissive mode if the Bool parameter is True
-- Load/LoadFile should already be expanded before calling this
runTypedInferTopExpr :: Bool -> TopExpr -> IO (Either TypeError (Maybe TypedTopExpr), [TypeWarning])
runTypedInferTopExpr permissive topExpr = do
  (result, warnings, _, _) <- runTypedInferTopExprWithEnv permissive topExpr emptyEnv emptyClassEnv
  return (result, warnings)

-- | Run type inference with initial type and class environments
-- Returns (result, warnings, updated type env, updated class env)
runTypedInferTopExprWithEnv :: Bool -> TopExpr -> TypeEnv -> ClassEnv -> IO (Either TypeError (Maybe TypedTopExpr), [TypeWarning], TypeEnv, ClassEnv)
runTypedInferTopExprWithEnv permissive topExpr initialTypeEnv initialClassEnv = do
  let inferCfg = defaultInferConfig { cfgPermissive = permissive }
      initialState = InferState 
        { inferCounter = 0
        , inferEnv = initialTypeEnv
        , inferClassEnv = initialClassEnv
        , inferWarnings = []
        , inferConfig = inferCfg
        }
  (result, warnings, finalState) <- runInferWithWarningsAndState (inferTypedTopExpr topExpr) initialState
  let updatedTypeEnv = inferEnv finalState
      updatedClassEnv = inferClassEnv finalState
  return $ case result of
    Left err -> (Left err, warnings, updatedTypeEnv, updatedClassEnv)
    Right mTypedTop -> (Right mTypedTop, warnings, updatedTypeEnv, updatedClassEnv)

-- | Infer type and produce typed expression
inferTypedExpr :: Expr -> Infer (TypedExpr, Subst)
inferTypedExpr expr = case expr of
  
  -- Constants
  ConstantExpr c -> do
    (ty, s) <- Infer.inferConstant c
    return (TypedExpr ty (TConstantExpr c), s)
  
  -- Variables
  VarExpr name -> do
    ty <- Infer.lookupVar name
    return (TypedExpr ty (TVarExpr name), emptySubst)
  
  -- Indexed expressions
  IndexedExpr override base indices -> do
    (baseTyped, s1) <- inferTypedExpr base
    indicesTyped <- forM indices $ \idx -> do
      case idx of
        Subscript e -> do
          (te, _) <- inferTypedExpr e
          return $ Subscript te
        Superscript e -> do
          (te, _) <- inferTypedExpr e
          return $ Superscript te
        SupSubscript e -> do
          (te, _) <- inferTypedExpr e
          return $ SupSubscript te
        Userscript e -> do
          (te, _) <- inferTypedExpr e
          return $ Userscript te
        MultiSubscript e1 e2 -> do
          (te1, _) <- inferTypedExpr e1
          (te2, _) <- inferTypedExpr e2
          return $ MultiSubscript te1 te2
        MultiSuperscript e1 e2 -> do
          (te1, _) <- inferTypedExpr e1
          (te2, _) <- inferTypedExpr e2
          return $ MultiSuperscript te1 te2
    -- Result type depends on base type
    let resultType = texprType baseTyped  -- Simplified: actual type depends on index
    return (TypedExpr resultType (TIndexedExpr override baseTyped indicesTyped), s1)
  
  -- Tuples
  TupleExpr es -> do
    results <- mapM inferTypedExpr es
    let typedExprs = map fst results
        types = map texprType typedExprs
        subst = foldr composeSubst emptySubst (map snd results)
    return (TypedExpr (TTuple types) (TTupleExpr typedExprs), subst)
  
  -- Collections
  CollectionExpr exprs -> do
    results <- mapM inferTypedExpr exprs
    let typedExprs = map fst results
    elemType <- if null typedExprs
                then freshVar "elem"
                else return $ texprType (head typedExprs)
    let subst = foldr composeSubst emptySubst (map snd results)
    return (TypedExpr (TList elemType) (TCollectionExpr typedExprs), subst)
  
  -- Hash
  HashExpr pairs -> do
    results <- forM pairs $ \(k, v) -> do
      (tk, s1) <- inferTypedExpr k
      (tv, s2) <- inferTypedExpr v
      return ((tk, tv), composeSubst s2 s1)
    let typedPairs = map fst results
        subst = foldr composeSubst emptySubst (map snd results)
    (keyType, valType) <- if null typedPairs
                          then do
                            k <- freshVar "key"
                            v <- freshVar "val"
                            return (k, v)
                          else let (tk, tv) = head typedPairs
                               in return (texprType tk, texprType tv)
    return (TypedExpr (THash keyType valType) (THashExpr typedPairs), subst)
  
  -- Lambda
  -- Preserve ScalarArg/TensorArg info for proper desugaring
  LambdaExpr args body -> do
    paramTypes <- mapM (\_ -> freshVar "param") args
    let paramNames = map extractArgName args
        argParams = map convertArg args  -- Preserve arg type info
        paramBindings = zipWith (\n t -> (n, Forall [] [] t)) paramNames paramTypes
    (bodyTyped, s) <- withEnv paramBindings $ inferTypedExpr body
    let funType = foldr TFun (texprType bodyTyped) (map (applySubst s) paramTypes)
    return (TypedExpr funType (TLambdaExpr argParams bodyTyped), s)
    where
      extractArgName (ScalarArg (APPatVar (VarWithIndices n _))) = n
      extractArgName (TensorArg (APPatVar (VarWithIndices n _))) = n
      extractArgName (InvertedScalarArg (APPatVar (VarWithIndices n _))) = n
      extractArgName _ = "_"
      
      convertArg (ScalarArg (APPatVar (VarWithIndices n _))) = ScalarArg n
      convertArg (TensorArg (APPatVar (VarWithIndices n _))) = TensorArg n
      convertArg (InvertedScalarArg (APPatVar (VarWithIndices n _))) = InvertedScalarArg n
      convertArg _ = TensorArg "_"
  
  -- Typed Lambda
  TypedLambdaExpr params retTypeExpr body -> do
    let paramTypes = map (typeExprToType . snd) params
        paramBindings = zipWith (\(n, _) t -> (n, Forall [] [] t)) params paramTypes
        retType = typeExprToType retTypeExpr
    (bodyTyped, s) <- withEnv paramBindings $ inferTypedExpr body
    s' <- unifyTypes (applySubst s (texprType bodyTyped)) retType
    let finalS = composeSubst s' s
        finalParamTypes = map (applySubst finalS) paramTypes
        funType = foldr TFun (applySubst finalS retType) finalParamTypes
        typedParams = zipWith (\(n, _) t -> (n, t)) params finalParamTypes
    return (TypedExpr funType (TTypedLambdaExpr typedParams (applySubst finalS retType) bodyTyped), finalS)
  
  -- Function application
  ApplyExpr func args -> do
    (funcTyped, s1) <- inferTypedExpr func
    argsResults <- mapM inferTypedExpr args
    let argsTyped = map fst argsResults
        argsSubst = foldr composeSubst s1 (map snd argsResults)
    resultType <- freshVar "result"
    let expectedFuncType = foldr TFun resultType (map texprType argsTyped)
    s2 <- unifyTypes (applySubst argsSubst (texprType funcTyped)) expectedFuncType
    let finalS = composeSubst s2 argsSubst
        finalResultType = applySubst finalS resultType
    return (TypedExpr finalResultType (TApplyExpr funcTyped argsTyped), finalS)
  
  -- If expression
  IfExpr cond thenE elseE -> do
    (condTyped, s1) <- inferTypedExpr cond
    s2 <- unifyTypes (texprType condTyped) TBool
    let s12 = composeSubst s2 s1
    (thenTyped, s3) <- inferTypedExpr thenE
    (elseTyped, s4) <- inferTypedExpr elseE
    s5 <- unifyTypes (applySubst s4 (texprType thenTyped)) (texprType elseTyped)
    let finalS = foldr composeSubst emptySubst [s5, s4, s3, s12]
        resultType = applySubst finalS (texprType elseTyped)
    return (TypedExpr resultType (TIfExpr condTyped thenTyped elseTyped), finalS)
  
  -- Let expression
  LetExpr bindings body -> do
    (typedBindings, s1) <- inferTypedBindings bindings
    (bodyTyped, s2) <- inferTypedExpr body
    let finalS = composeSubst s2 s1
    return (TypedExpr (texprType bodyTyped) (TLetExpr typedBindings bodyTyped), finalS)
  
  -- Match expressions
  MatchExpr mode target matcher clauses -> do
    (targetTyped, s1) <- inferTypedExpr target
    (matcherTyped, s2) <- inferTypedExpr matcher
    let s12 = composeSubst s2 s1
    resultType <- freshVar "matchResult"
    typedClauses <- forM clauses $ \(pat, clauseBody) -> do
      -- Extract pattern variable bindings and add to environment
      patBindings <- extractPatternVarBindings pat (texprType targetTyped)
      typedPat <- inferTypedPattern pat (texprType targetTyped)
      (bodyTyped, _) <- withEnv patBindings $ inferTypedExpr clauseBody
      return (typedPat, bodyTyped)
    let finalResultType = if null typedClauses
                          then resultType
                          else texprType (snd (head typedClauses))
    return (TypedExpr finalResultType (TMatchExpr mode targetTyped matcherTyped typedClauses), s12)
  
  MatchAllExpr mode target matcher clauses -> do
    (targetTyped, s1) <- inferTypedExpr target
    (matcherTyped, s2) <- inferTypedExpr matcher
    let s12 = composeSubst s2 s1
    typedClauses <- forM clauses $ \(pat, clauseBody) -> do
      -- Extract pattern variable bindings and add to environment
      patBindings <- extractPatternVarBindings pat (texprType targetTyped)
      typedPat <- inferTypedPattern pat (texprType targetTyped)
      (bodyTyped, _) <- withEnv patBindings $ inferTypedExpr clauseBody
      return (typedPat, bodyTyped)
    let elemType = if null typedClauses
                   then TVar (TyVar "a")
                   else texprType (snd (head typedClauses))
    return (TypedExpr (TList elemType) (TMatchAllExpr mode targetTyped matcherTyped typedClauses), s12)
  
  -- Quote
  QuoteExpr e -> do
    (te, s) <- inferTypedExpr e
    return (TypedExpr TInt (TQuoteExpr te), s)
  
  QuoteSymbolExpr e -> do
    (te, s) <- inferTypedExpr e
    return (TypedExpr TInt (TQuoteSymbolExpr te), s)
  
  -- IO
  DoExpr bindings body -> do
    (typedBindings, s1) <- inferTypedDoBindings bindings
    (bodyTyped, s2) <- inferTypedExpr body
    let finalS = composeSubst s2 s1
    return (TypedExpr (TIO (texprType bodyTyped)) (TDoExpr typedBindings bodyTyped), finalS)
  
  SeqExpr e1 e2 -> do
    (te1, s1) <- inferTypedExpr e1
    (te2, s2) <- inferTypedExpr e2
    let finalS = composeSubst s2 s1
    return (TypedExpr (texprType te2) (TSeqExpr te1 te2), finalS)
  
  -- Generate tensor
  GenerateTensorExpr gen shape -> do
    (genTyped, s1) <- inferTypedExpr gen
    (shapeTyped, s2) <- inferTypedExpr shape
    let s12 = composeSubst s2 s1
    elemType <- freshVar "tensorElem"
    return (TypedExpr (TTensor elemType ShapeUnknown []) (TGenerateTensorExpr genTyped shapeTyped), s12)
  
  -- Tensor operations
  TensorExpr dataE shapeE -> do
    (dataTyped, s1) <- inferTypedExpr dataE
    (shapeTyped, s2) <- inferTypedExpr shapeE
    let s12 = composeSubst s2 s1
    elemType <- case texprType dataTyped of
      TList e -> return e
      _ -> freshVar "tensorElem"
    return (TypedExpr (TTensor elemType ShapeUnknown []) (TTensorExpr dataTyped shapeTyped), s12)
  
  TensorContractExpr e -> do
    (te, s) <- inferTypedExpr e
    return (TypedExpr (texprType te) (TTensorContractExpr te), s)
  
  TensorMapExpr f t -> do
    (fTyped, s1) <- inferTypedExpr f
    (tTyped, s2) <- inferTypedExpr t
    let finalS = composeSubst s2 s1
    return (TypedExpr (texprType tTyped) (TTensorMapExpr fTyped tTyped), finalS)
  
  TensorMap2Expr f t1 t2 -> do
    (fTyped, s1) <- inferTypedExpr f
    (t1Typed, s2) <- inferTypedExpr t1
    (t2Typed, s3) <- inferTypedExpr t2
    let finalS = foldr composeSubst emptySubst [s3, s2, s1]
    return (TypedExpr (texprType t1Typed) (TTensorMap2Expr fTyped t1Typed t2Typed), finalS)
  
  TransposeExpr indices tensor -> do
    (indicesTyped, s1) <- inferTypedExpr indices
    (tensorTyped, s2) <- inferTypedExpr tensor
    let finalS = composeSubst s2 s1
    return (TypedExpr (texprType tensorTyped) (TTransposeExpr indicesTyped tensorTyped), finalS)
  
  -- Section expressions
  SectionExpr op mLeft mRight -> do
    mLeftTyped <- traverse inferTypedExpr mLeft
    mRightTyped <- traverse inferTypedExpr mRight
    resultType <- freshVar "section"
    let leftTyped = fmap fst mLeftTyped
        rightTyped = fmap fst mRightTyped
    return (TypedExpr resultType (TSectionExpr op leftTyped rightTyped), emptySubst)
  
  -- Infix expressions (e.g., 1 + 2)
  InfixExpr op e1 e2 -> do
    (e1Typed, s1) <- inferTypedExpr e1
    (e2Typed, s2) <- inferTypedExpr e2
    let s12 = composeSubst s2 s1
    -- The operator is a function, look up its type
    opType <- lookupVar (repr op)
    resultType <- freshVar "infixResult"
    -- Unify: opType should be a -> b -> resultType
    let expectedType = TFun (texprType e1Typed) (TFun (texprType e2Typed) resultType)
    s3 <- unifyTypes (applySubst s12 opType) expectedType
    let finalS = composeSubst s3 s12
    return (TypedExpr (applySubst finalS resultType) (TInfixExpr op e1Typed e2Typed), finalS)
  
  -- Anonymous parameters
  AnonParamFuncExpr arity body -> do
    (bodyTyped, s) <- inferTypedExpr body
    return (TypedExpr (texprType bodyTyped) (TAnonParamFuncExpr (fromIntegral arity) bodyTyped), s)
  
  AnonParamExpr n -> do
    ty <- freshVar "anonParam"
    return (TypedExpr ty (TAnonParamExpr (fromIntegral n)), emptySubst)
  
  -- Prefix operator
  PrefixExpr op e -> do
    (te, s) <- inferTypedExpr e
    return (TypedExpr (texprType te) (TPrefixExpr op te), s)
  
  -- Fresh variable
  FreshVarExpr -> do
    ty <- freshVar "fresh"
    return (TypedExpr ty TFreshVarExpr, emptySubst)
  
  -- WithSymbols
  WithSymbolsExpr syms body -> do
    (bodyTyped, s) <- inferTypedExpr body
    return (TypedExpr (texprType bodyTyped) (TWithSymbolsExpr syms bodyTyped), s)
  
  -- Pattern function
  PatternFunctionExpr params body -> do
    typedBody <- inferTypedPattern body (TVar (TyVar "a"))
    let paramTypes = replicate (length params) (TPattern (TVar (TyVar "a")))
        resultType = TPatternFunc paramTypes (TVar (TyVar "a"))
    return (TypedExpr resultType (TPatternFunctionExpr params typedBody), emptySubst)
  
  -- Function expression (for symbolic computation)
  FunctionExpr args -> do
    return (TypedExpr TInt (TFunctionExpr args), emptySubst)
  
  -- Matcher expressions
  MatcherExpr patDefs -> do
    typedPatDefs <- forM patDefs $ \(patPat, nextMatcher, clauses) -> do
      -- Extract bindings from primitive pattern pattern (e.g., "val", "tgt")
      patPatBindings <- extractPrimPatPatBindings patPat
      (nextMatcherTyped, _) <- withEnv patPatBindings $ inferTypedExpr nextMatcher
      typedClauses <- forM clauses $ \(dataPat, clauseBody) -> do
        -- Extract bindings from data pattern (e.g., pattern variables)
        dataPatBindings <- extractPrimDataPatBindings dataPat
        let allBindings = patPatBindings ++ dataPatBindings
        (bodyTyped, _) <- withEnv allBindings $ inferTypedExpr clauseBody
        return (dataPat, bodyTyped)
      return (patPat, nextMatcherTyped, typedClauses)
    elemType <- freshVar "matcherElem"
    return (TypedExpr (TMatcher elemType) (TMatcherExpr typedPatDefs), emptySubst)
  
  AlgebraicDataMatcherExpr constructors -> do
    typedConstructors <- forM constructors $ \(name, argExprs) -> do
      typedArgs <- forM argExprs $ \argE -> fst <$> inferTypedExpr argE
      return (name, typedArgs)
    elemType <- freshVar "adtElem"
    return (TypedExpr (TMatcher elemType) (TAlgebraicDataMatcherExpr typedConstructors), emptySubst)
  
  -- Memoized lambda
  MemoizedLambdaExpr params body -> do
    paramTypes <- mapM (\p -> freshVar ("memoParam_" ++ p)) params
    let paramBindings = zipWith (\n t -> (n, Forall [] [] t)) params paramTypes
    (bodyTyped, s) <- withEnv paramBindings $ inferTypedExpr body
    let funType = foldr TFun (texprType bodyTyped) (map (applySubst s) paramTypes)
    return (TypedExpr funType (TMemoizedLambdaExpr params bodyTyped), s)
  
  -- Cons/Join
  ConsExpr h t -> do
    (hTyped, s1) <- inferTypedExpr h
    (tTyped, s2) <- inferTypedExpr t
    let s12 = composeSubst s2 s1
    s3 <- unifyTypes (TList (texprType hTyped)) (texprType tTyped)
    let finalS = composeSubst s3 s12
    return (TypedExpr (applySubst finalS (texprType tTyped)) (TConsExpr hTyped tTyped), finalS)
  
  JoinExpr l r -> do
    (lTyped, s1) <- inferTypedExpr l
    (rTyped, s2) <- inferTypedExpr r
    let s12 = composeSubst s2 s1
    s3 <- unifyTypes (texprType lTyped) (texprType rTyped)
    let finalS = composeSubst s3 s12
    return (TypedExpr (applySubst finalS (texprType lTyped)) (TJoinExpr lTyped rTyped), finalS)
  
  -- CApply (curried apply)
  CApplyExpr f a -> do
    (fTyped, s1) <- inferTypedExpr f
    (aTyped, s2) <- inferTypedExpr a
    let s12 = composeSubst s2 s1
    resultType <- freshVar "capply"
    s3 <- unifyTypes (applySubst s12 (texprType fTyped)) (TFun (texprType aTyped) resultType)
    let finalS = composeSubst s3 s12
    return (TypedExpr (applySubst finalS resultType) (TCApplyExpr fTyped aTyped), finalS)
  
  -- Match lambda
  MatchLambdaExpr matcher clauses -> do
    (matcherTyped, s1) <- inferTypedExpr matcher
    typedClauses <- forM clauses $ \(pat, clauseBody) -> do
      elemType <- freshVar "matchElem"
      typedPat <- inferTypedPattern pat elemType
      (bodyTyped, _) <- inferTypedExpr clauseBody
      return (typedPat, bodyTyped)
    let resultType = if null typedClauses
                     then TVar (TyVar "a")
                     else texprType (snd (head typedClauses))
    argType <- freshVar "matchArg"
    return (TypedExpr (TFun argType resultType) (TMatchLambdaExpr matcherTyped typedClauses), s1)
  
  MatchAllLambdaExpr matcher clauses -> do
    (matcherTyped, s1) <- inferTypedExpr matcher
    typedClauses <- forM clauses $ \(pat, clauseBody) -> do
      elemType <- freshVar "matchElem"
      typedPat <- inferTypedPattern pat elemType
      (bodyTyped, _) <- inferTypedExpr clauseBody
      return (typedPat, bodyTyped)
    let elemType = if null typedClauses
                   then TVar (TyVar "a")
                   else texprType (snd (head typedClauses))
    argType <- freshVar "matchArg"
    return (TypedExpr (TFun argType (TList elemType)) (TMatchAllLambdaExpr matcherTyped typedClauses), s1)
  
  -- Default: use fallback for unhandled expressions
  _ -> do
    result <- catchError (Right <$> Infer.inferExpr expr) (\_ -> return $ Left ())
    case result of
      Right (ty, s) -> do
        fallback <- makeFallbackTypedExpr expr
        return (fallback { texprType = ty }, s)
      Left _ -> do
        fallback <- makeFallbackTypedExpr expr
        return (fallback, emptySubst)

-- | Create a fallback typed expression when type inference fails
-- This allows the desugaring to continue with TAny type
-- The fallback uses the same structure but with TAny types
makeFallbackTypedExpr :: Expr -> Infer TypedExpr
makeFallbackTypedExpr expr = case expr of
  VarExpr name -> return $ TypedExpr TAny (TVarExpr name)
  ConstantExpr c -> return $ TypedExpr TAny (TConstantExpr c)
  LambdaExpr args body -> do
    let argParams = map convertArg args
    bodyTyped <- makeFallbackTypedExpr body
    return $ TypedExpr TAny (TLambdaExpr argParams bodyTyped)
    where
      convertArg (ScalarArg (APPatVar (VarWithIndices n _))) = ScalarArg n
      convertArg (TensorArg (APPatVar (VarWithIndices n _))) = TensorArg n
      convertArg (InvertedScalarArg (APPatVar (VarWithIndices n _))) = InvertedScalarArg n
      convertArg _ = TensorArg "_"
  ApplyExpr f argList -> do
    fTyped <- makeFallbackTypedExpr f
    argsTyped <- mapM makeFallbackTypedExpr argList
    return $ TypedExpr TAny (TApplyExpr fTyped argsTyped)
  IfExpr c t e -> do
    cTyped <- makeFallbackTypedExpr c
    tTyped <- makeFallbackTypedExpr t
    eTyped <- makeFallbackTypedExpr e
    return $ TypedExpr TAny (TIfExpr cTyped tTyped eTyped)
  InfixExpr op e1 e2 -> do
    e1Typed <- makeFallbackTypedExpr e1
    e2Typed <- makeFallbackTypedExpr e2
    return $ TypedExpr TAny (TInfixExpr op e1Typed e2Typed)
  SectionExpr op mLeft mRight -> do
    mLeftTyped <- traverse makeFallbackTypedExpr mLeft
    mRightTyped <- traverse makeFallbackTypedExpr mRight
    return $ TypedExpr TAny (TSectionExpr op mLeftTyped mRightTyped)
  TupleExpr elems -> do
    elemsTyped <- mapM makeFallbackTypedExpr elems
    return $ TypedExpr TAny (TTupleExpr elemsTyped)
  CollectionExpr elems -> do
    elemsTyped <- mapM makeFallbackTypedExpr elems
    return $ TypedExpr TAny (TCollectionExpr elemsTyped)
  LetExpr bindings body -> do
    bindingsTyped <- mapM makeFallbackBinding bindings
    bodyTyped <- makeFallbackTypedExpr body
    return $ TypedExpr TAny (TLetExpr bindingsTyped bodyTyped)
  _ -> return $ TypedExpr TAny (TVarExpr "<fallback>")
  where
    makeFallbackBinding (Bind pat e) = do
      eTyped <- makeFallbackTypedExpr e
      return $ TBind pat eTyped
    makeFallbackBinding (BindWithIndices (VarWithIndices name _) e) = do
      eTyped <- makeFallbackTypedExpr e
      return $ TBindWithType name TAny eTyped
    makeFallbackBinding (BindWithType _ e) = do
      eTyped <- makeFallbackTypedExpr e
      return $ TBind PDWildCard eTyped

-- | Infer typed bindings for let expressions
inferTypedBindings :: [BindingExpr] -> Infer ([TypedBinding], Subst)
inferTypedBindings [] = return ([], emptySubst)
inferTypedBindings (b:bs) = case b of
  Bind pat e -> do
    (eTyped, s1) <- inferTypedExpr e
    -- Add pattern bindings to environment
    patBindings <- extractLetPatternBindings (texprType eTyped) pat
    env <- getEnv
    setEnv $ extendEnvMany patBindings env
    (restTyped, s2) <- inferTypedBindings bs
    let finalS = composeSubst s2 s1
    return (TBind pat eTyped : restTyped, finalS)
  
  BindWithIndices (VarWithIndices name _) e -> do
    (eTyped, s1) <- inferTypedExpr e
    env <- getEnv
    let scheme = generalize env (texprType eTyped)
    setEnv $ extendEnv name scheme env
    (restTyped, s2) <- inferTypedBindings bs
    let finalS = composeSubst s2 s1
    return (TBindWithType name (texprType eTyped) eTyped : restTyped, finalS)
  
  BindWithType _ e -> do
    (eTyped, s1) <- inferTypedExpr e
    (restTyped, s2) <- inferTypedBindings bs
    let finalS = composeSubst s2 s1
    return (TBind PDWildCard eTyped : restTyped, finalS)

-- | Infer typed bindings for do expressions
inferTypedDoBindings :: [BindingExpr] -> Infer ([TypedBinding], Subst)
inferTypedDoBindings [] = return ([], emptySubst)
inferTypedDoBindings (b:bs) = case b of
  Bind pat e -> do
    (eTyped, s1) <- inferTypedExpr e
    -- Add pattern bindings to environment
    patBindings <- extractLetPatternBindings (texprType eTyped) pat
    env <- getEnv
    setEnv $ extendEnvMany patBindings env
    (restTyped, s2) <- inferTypedDoBindings bs
    let finalS = composeSubst s2 s1
    return (TBind pat eTyped : restTyped, finalS)
  
  BindWithIndices (VarWithIndices name _) e -> do
    (eTyped, s1) <- inferTypedExpr e
    env <- getEnv
    let scheme = generalize env (texprType eTyped)
    setEnv $ extendEnv name scheme env
    (restTyped, s2) <- inferTypedDoBindings bs
    let finalS = composeSubst s2 s1
    return (TBindWithType name (texprType eTyped) eTyped : restTyped, finalS)
  
  BindWithType _ e -> do
    (eTyped, s1) <- inferTypedExpr e
    (restTyped, s2) <- inferTypedDoBindings bs
    let finalS = composeSubst s2 s1
    return (TBind PDWildCard eTyped : restTyped, finalS)

-- | Infer typed pattern
inferTypedPattern :: Pattern -> Type -> Infer TypedPattern
inferTypedPattern pat ty = case pat of
  WildCard -> return $ TypedPattern ty TPWildCard
  PatVar name -> return $ TypedPattern ty (TPPatVar name)
  ValuePat e -> do
    (eTyped, _) <- inferTypedExpr e
    return $ TypedPattern ty (TPValuePat eTyped)
  PredPat e -> do
    (eTyped, _) <- inferTypedExpr e
    return $ TypedPattern ty (TPPredPat eTyped)
  IndexedPat p idxs -> do
    pTyped <- inferTypedPattern p ty
    idxsTyped <- mapM (\e -> fst <$> inferTypedExpr e) idxs
    return $ TypedPattern ty (TPIndexedPat pTyped idxsTyped)
  NotPat p -> do
    pTyped <- inferTypedPattern p ty
    return $ TypedPattern ty (TPNotPat pTyped)
  AndPat p1 p2 -> do
    p1Typed <- inferTypedPattern p1 ty
    p2Typed <- inferTypedPattern p2 ty
    return $ TypedPattern ty (TPAndPat p1Typed p2Typed)
  OrPat p1 p2 -> do
    p1Typed <- inferTypedPattern p1 ty
    p2Typed <- inferTypedPattern p2 ty
    return $ TypedPattern ty (TPOrPat p1Typed p2Typed)
  TuplePat ps -> do
    elemTypes <- case ty of
      TTuple ts | length ts == length ps -> return ts
      _ -> mapM (\_ -> freshVar "tupPat") ps
    psTyped <- zipWithM inferTypedPattern ps elemTypes
    return $ TypedPattern ty (TPTuplePat psTyped)
  InductivePat name ps -> do
    elemType <- case ty of
      TList e -> return e
      _ -> freshVar "indElem"
    psTyped <- mapM (\p -> inferTypedPattern p elemType) ps
    return $ TypedPattern ty (TPInductivePat name psTyped)
  InfixPat op p1 p2 -> do
    elemType <- case ty of
      TList e -> return e
      _ -> freshVar "infixElem"
    p1Typed <- inferTypedPattern p1 elemType
    p2Typed <- inferTypedPattern p2 ty
    return $ TypedPattern ty (TPInfixPat op p1Typed p2Typed)
  VarPat name -> return $ TypedPattern ty (TPVarPat name)
  ContPat -> return $ TypedPattern ty TPContPat
  SeqNilPat -> return $ TypedPattern ty TPSeqNilPat
  SeqConsPat p1 p2 -> do
    elemType <- case ty of
      TList e -> return e
      _ -> freshVar "seqElem"
    p1Typed <- inferTypedPattern p1 elemType
    p2Typed <- inferTypedPattern p2 ty
    return $ TypedPattern ty (TPSeqConsPat p1Typed p2Typed)
  LaterPatVar -> return $ TypedPattern ty TPLaterPatVar
  _ -> return $ TypedPattern ty TPWildCard  -- Fallback

-- | zipWithM for patterns
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = mapM (uncurry f) (zip xs ys)

-- | Extract variable bindings from a Pattern (used in match/matchAll expressions)
-- This extracts all pattern variables and assigns them fresh types
extractPatternVarBindings :: Pattern -> Type -> Infer [(String, TypeScheme)]
extractPatternVarBindings pat targetType = go pat targetType
  where
    go :: Pattern -> Type -> Infer [(String, TypeScheme)]
    go WildCard _ = return []
    go (PatVar name) ty = return [(name, Forall [] [] ty)]
    go (ValuePat _) _ = return []
    go (PredPat _) _ = return []
    go (NotPat p) ty = go p ty
    go (AndPat p1 p2) ty = (++) <$> go p1 ty <*> go p2 ty
    go (OrPat p1 p2) ty = (++) <$> go p1 ty <*> go p2 ty
    go (ForallPat p1 p2) ty = (++) <$> go p1 ty <*> go p2 ty
    go (TuplePat ps) ty = do
      elemTypes <- case ty of
        TTuple ts | length ts == length ps -> return ts
        _ -> mapM (\_ -> freshVar "tupleElem") ps
      concat <$> zipWithM go ps elemTypes
    go (InductivePat "cons" [h, t]) ty = do
      elemTy <- case ty of
        TList e -> return e
        _ -> freshVar "consElem"
      hBindings <- go h elemTy
      tBindings <- go t ty
      return (hBindings ++ tBindings)
    go (InductivePat _ ps) ty = do
      elemTy <- case ty of
        TList e -> return e
        _ -> freshVar "indElem"
      concat <$> mapM (\p -> go p elemTy) ps
    go (InfixPat _ p1 p2) ty = do
      elemTy <- case ty of
        TList e -> return e
        _ -> freshVar "infixElem"
      (++) <$> go p1 elemTy <*> go p2 ty
    go (LetPat _ p) ty = go p ty
    go (IndexedPat p _) ty = go p ty
    go (DApplyPat p ps) ty = do
      pBindings <- go p ty
      psBindings <- concat <$> mapM (\p' -> go p' TAny) ps
      return (pBindings ++ psBindings)
    go (PApplyPat _ ps) ty = do
      elemTy <- case ty of
        TList e -> return e
        _ -> freshVar "papplyElem"
      concat <$> mapM (\p -> go p elemTy) ps
    go (InductiveOrPApplyPat _ ps) ty = do
      elemTy <- case ty of
        TList e -> return e
        _ -> freshVar "iopElem"
      concat <$> mapM (\p -> go p elemTy) ps
    go (VarPat name) ty = return [(name, Forall [] [] ty)]
    go (LoopPat _ range p1 p2) ty = do
      -- Loop patterns are complex; extract from subpatterns
      rangeBindings <- extractLoopRangeBindings range
      p1Bindings <- go p1 ty
      p2Bindings <- go p2 ty
      return (rangeBindings ++ p1Bindings ++ p2Bindings)
    go ContPat _ = return []
    go SeqNilPat _ = return []
    go (SeqConsPat p1 p2) ty = do
      elemTy <- case ty of
        TList e -> return e
        _ -> freshVar "seqElem"
      (++) <$> go p1 elemTy <*> go p2 ty
    go LaterPatVar _ = return []

    extractLoopRangeBindings :: LoopRange -> Infer [(String, TypeScheme)]
    extractLoopRangeBindings (LoopRange _ _ p) = go p TInt

-- | Extract bindings from PrimitivePatPattern (used in matcher definitions)
-- e.g., PPValuePat "val" -> [("val", Forall [] [] TAny)]
extractPrimPatPatBindings :: PrimitivePatPattern -> Infer [(String, TypeScheme)]
extractPrimPatPatBindings ppp = case ppp of
  PPWildCard -> return []
  PPPatVar -> return []  -- PatVar doesn't bind a named variable here
  PPValuePat name -> return [(name, Forall [] [] TAny)]
  PPInductivePat _ pats -> concat <$> mapM extractPrimPatPatBindings pats
  PPTuplePat pats -> concat <$> mapM extractPrimPatPatBindings pats

-- | Extract bindings from PrimitiveDataPattern (used in matcher clauses)
-- e.g., PDPatVar "tgt" -> [("tgt", Forall [] [] TAny)]
extractPrimDataPatBindings :: PrimitiveDataPattern -> Infer [(String, TypeScheme)]
extractPrimDataPatBindings pdp = case pdp of
  PDWildCard -> return []
  PDEmptyPat -> return []
  PDPatVar name -> return [(name, Forall [] [] TAny)]
  PDConstantPat _ -> return []
  PDTuplePat pats -> concat <$> mapM extractPrimDataPatBindings pats
  PDConsPat h t -> do
    hBindings <- extractPrimDataPatBindings h
    tBindings <- extractPrimDataPatBindings t
    return (hBindings ++ tBindings)
  PDSnocPat i l -> do
    iBindings <- extractPrimDataPatBindings i
    lBindings <- extractPrimDataPatBindings l
    return (iBindings ++ lBindings)
  PDInductivePat _ pats -> concat <$> mapM extractPrimDataPatBindings pats

-- | Infer typed top-level expression
-- Uses catchError to continue even if type inference fails for sub-expressions
inferTypedTopExpr :: TopExpr -> Infer (Maybe TypedTopExpr)
inferTypedTopExpr topExpr = case topExpr of
  Define (VarWithIndices name _) e -> do
    result <- catchError (Right <$> inferTypedExpr e) (\_ -> return $ Left ())
    case result of
      Left _ -> do
        -- Type inference failed, register with TAny and continue
        env <- getEnv
        setEnv $ extendEnv name (Forall [] [] TAny) env
        -- Use fallback typed expression
        fallbackTyped <- makeFallbackTypedExpr e
        return $ Just $ TDefine name fallbackTyped
      Right (eTyped, _) -> do
        env <- getEnv
        let scheme = generalize env (texprType eTyped)
        setEnv $ extendEnv name scheme env
        return $ Just $ TDefine name eTyped
  
  DefineWithType (TypedVarWithIndices name _ params retType) body -> do
    let paramTypes = map typedParamToType params
        paramBindings = extractTypedParamBindings params
        retTy = typeExprToType retType
    (bodyTyped, s) <- withEnv paramBindings $ inferTypedExpr body
    s' <- unifyTypes (applySubst s (texprType bodyTyped)) retTy
    let finalS = composeSubst s' s
        finalParamTypes = map (applySubst finalS) paramTypes
        funType = foldr TFun (applySubst finalS retTy) finalParamTypes
        scheme = generalize emptyEnv funType
        typedParams = extractTypedParamBindingsWithTypes params
    env <- getEnv
    setEnv $ extendEnv name scheme env
    return $ Just $ TDefineWithType name typedParams (applySubst finalS retTy) bodyTyped
  
  Test e -> do
    (eTyped, _) <- inferTypedExpr e
    return $ Just $ TTest eTyped
  
  Execute e -> do
    (eTyped, _) <- inferTypedExpr e
    return $ Just $ TExecute eTyped
  
  LoadFile path -> do
    -- Also run type inference on the loaded file
    _ <- Infer.inferTopExpr topExpr
    return $ Just $ TLoadFile path
  Load lib -> do
    -- Also run type inference on the loaded library
    _ <- Infer.inferTopExpr topExpr
    return $ Just $ TLoad lib
  
  InductiveDecl typeName typeParams constructors -> do
    -- Register inductive type in environment
    _ <- Infer.inferTopExpr topExpr
    let typedConstrs = map (\(InductiveConstructor n ts) -> (n, map typeExprToType ts)) constructors
    return $ Just $ TInductiveDecl typeName typeParams typedConstrs
  
  ClassDeclExpr (ClassDecl className typeParams supers methods) -> do
    _ <- Infer.inferTopExpr topExpr
    let typedMethods = map convertMethod methods
    return $ Just $ TClassDecl className typeParams (map constraintToString supers) typedMethods
    where
      convertMethod (ClassMethod name params retType _) =
        (name, map (\tp -> (extractParamName tp, typedParamToType tp)) params, typeExprToType retType)
      extractParamName (TPVar n _) = n
      extractParamName (TPUntypedVar n) = n
      extractParamName _ = "_"
      constraintToString (ConstraintExpr cls _) = cls
  
  InstanceDeclExpr (InstanceDecl constraints className instTypes methods) -> do
    _ <- Infer.inferTopExpr topExpr
    typedMethods <- forM methods $ \(InstanceMethod name params body) -> do
      (bodyTyped, _) <- inferTypedExpr body
      return (name, params, bodyTyped)
    let contextTypes = map (\(ConstraintExpr _ ts) -> map typeExprToType ts) constraints
    return $ Just $ TInstanceDecl (concat contextTypes) className (map typeExprToType instTypes) typedMethods
  
  _ -> do
    -- For other top expressions, just run the regular inference
    _ <- Infer.inferTopExpr topExpr
    return Nothing

-- | Helper to extract typed param bindings with types
extractTypedParamBindingsWithTypes :: [TypedParam] -> [(String, Type)]
extractTypedParamBindingsWithTypes = concatMap go
  where
    go (TPVar name ty) = [(name, typeExprToType ty)]
    go (TPTuple elems) = concatMap go elems
    go (TPWildcard _) = []
    go (TPUntypedVar name) = [(name, TAny)]
    go TPUntypedWildcard = []

