{- |
Module      : Language.Egison.Type.Infer
Licence     : MIT

This module provides type inference for the Egison type system.
Uses a Hindley-Milner style algorithm with extensions for:
  - Tensor types with indices
  - Matcher types
  - Gradual typing (TAny)
-}

module Language.Egison.Type.Infer
  ( -- * Type inference
    inferExpr
  , inferTopExpr
  , Infer
  , InferState(..)
  , InferConfig(..)
  , initialInferState
  , defaultInferConfig
  , permissiveInferConfig
  , runInfer
  , runInferWithWarnings
    -- * Type conversion
  , typeExprToType
  , typeToTypeExpr
  ) where

import           Control.Monad              (foldM, when, zipWithM)
import           Control.Monad.Except       (ExceptT, runExceptT, throwError, catchError)
import           Control.Monad.State.Strict (State, evalState, runState, get, modify, put)
import           Data.IORef                 (IORef)

import           Language.Egison.AST hiding (Subscript, Superscript)
import qualified Language.Egison.AST as AST
import           Language.Egison.Type.Env
import           Language.Egison.Type.Error
import           Language.Egison.Type.Index as TI
import           Language.Egison.Type.Subst
import           Language.Egison.Type.Tensor
import           Language.Egison.Type.Types
import           Language.Egison.Type.Unify as TU

-- | Inference configuration
data InferConfig = InferConfig
  { cfgPermissive     :: Bool   -- ^ Treat unbound variables as warnings, not errors
  , cfgCollectWarnings :: Bool  -- ^ Collect warnings during inference
  } deriving (Show)

-- | Default configuration (strict mode)
defaultInferConfig :: InferConfig
defaultInferConfig = InferConfig
  { cfgPermissive = False
  , cfgCollectWarnings = False
  }

-- | Permissive configuration (for gradual adoption)
permissiveInferConfig :: InferConfig
permissiveInferConfig = InferConfig
  { cfgPermissive = True
  , cfgCollectWarnings = True
  }

-- | Inference state
data InferState = InferState
  { inferCounter  :: Int              -- ^ Fresh variable counter
  , inferEnv      :: TypeEnv          -- ^ Current type environment
  , inferWarnings :: [TypeWarning]    -- ^ Collected warnings
  , inferConfig   :: InferConfig      -- ^ Configuration
  } deriving (Show)

-- | Initial inference state
initialInferState :: InferState
initialInferState = InferState 0 emptyEnv [] defaultInferConfig

-- | Create initial state with config
initialInferStateWithConfig :: InferConfig -> InferState
initialInferStateWithConfig cfg = InferState 0 emptyEnv [] cfg

-- | Inference monad
type Infer a = ExceptT TypeError (State InferState) a

-- | Run type inference
runInfer :: Infer a -> InferState -> Either TypeError a
runInfer m st = evalState (runExceptT m) st

-- | Run type inference and also return warnings
runInferWithWarnings :: Infer a -> InferState -> (Either TypeError a, [TypeWarning])
runInferWithWarnings m st =
  let (result, finalState) = runState (runExceptT m) st
  in (result, inferWarnings finalState)

-- | Add a warning
addWarning :: TypeWarning -> Infer ()
addWarning w = modify $ \st -> st { inferWarnings = w : inferWarnings st }

-- | Check if we're in permissive mode
isPermissive :: Infer Bool
isPermissive = cfgPermissive . inferConfig <$> get

-- | Generate a fresh type variable
freshVar :: String -> Infer Type
freshVar prefix = do
  st <- get
  let n = inferCounter st
  put st { inferCounter = n + 1 }
  return $ TVar $ TyVar $ prefix ++ show n

-- | Get the current type environment
getEnv :: Infer TypeEnv
getEnv = inferEnv <$> get

-- | Set the type environment
setEnv :: TypeEnv -> Infer ()
setEnv env = modify $ \st -> st { inferEnv = env }

-- | Extend the environment temporarily
withEnv :: [(String, TypeScheme)] -> Infer a -> Infer a
withEnv bindings action = do
  oldEnv <- getEnv
  setEnv $ extendEnvMany bindings oldEnv
  result <- action
  setEnv oldEnv
  return result

-- | Look up a variable's type
lookupVar :: String -> Infer Type
lookupVar name = do
  env <- getEnv
  case lookupEnv name env of
    Just scheme -> do
      st <- get
      let (t, newCounter) = instantiate scheme (inferCounter st)
      modify $ \s -> s { inferCounter = newCounter }
      return t
    Nothing -> do
      permissive <- isPermissive
      if permissive
        then do
          -- In permissive mode, treat as a warning and return a fresh type variable
          addWarning $ UnboundVariableWarning name emptyContext
          freshVar "unbound"
        else throwError $ UnboundVariable name emptyContext

-- | Alias for emptyContext for backwards compatibility
emptyCtx :: TypeErrorContext
emptyCtx = emptyContext

-- | Make error context with expression (for future use)
_exprCtx :: String -> TypeErrorContext
_exprCtx expr = TypeErrorContext Nothing (Just expr) Nothing

-- | Unify two types
unifyTypes :: Type -> Type -> Infer Subst
unifyTypes t1 t2 = case unify t1 t2 of
  Right s  -> return s
  Left err -> case err of
    TU.OccursCheck v t -> throwError $ OccursCheckError v t emptyCtx
    TU.TypeMismatch a b -> throwError $ UnificationError a b emptyCtx
    TU.ShapeMismatch s1 s2 -> throwError $ TensorShapeMismatch s1 s2 emptyCtx
    TU.IndexMismatch i1 i2 -> throwError $ TensorIndexMismatch i1 i2 emptyCtx

-- | Infer the type of an expression
inferExpr :: Expr -> Infer (Type, Subst)
inferExpr expr = case expr of
  -- Constants
  ConstantExpr c -> inferConstant c

  -- Variables
  VarExpr name -> do
    t <- lookupVar name
    return (t, emptySubst)

  -- Collections
  TupleExpr es -> do
    results <- mapM inferExpr es
    let ts = map fst results
        s = foldr composeSubst emptySubst (map snd results)
    return (TTuple ts, s)

  CollectionExpr es -> do
    elemType <- freshVar "elem"
    s <- foldM (inferListElem elemType) emptySubst es
    return (TList (applySubst s elemType), s)
    where
      inferListElem eType s e = do
        (t, s') <- inferExpr e
        s'' <- unifyTypes (applySubst s eType) t
        return $ composeSubst s'' (composeSubst s' s)

  -- Lambda expressions
  LambdaExpr args body -> do
    argTypes <- mapM (const $ freshVar "arg") args
    let bindings = zipWith makeBinding (extractArgNames args) argTypes
    (bodyType, s) <- withEnv (map toScheme bindings) $ inferExpr body
    let finalArgTypes = map (applySubst s) argTypes
        funType = foldr TFun bodyType finalArgTypes
    return (funType, s)
    where
      extractArgNames = map extractArgName
      extractArgName (ScalarArg ap)         = extractFromArgPattern ap
      extractArgName (InvertedScalarArg ap) = extractFromArgPattern ap
      extractArgName (TensorArg ap)         = extractFromArgPattern ap
      extractFromArgPattern APWildCard = "_"
      extractFromArgPattern (APPatVar (VarWithIndices n _)) = n
      extractFromArgPattern _ = "_"
      makeBinding name t = (name, t)
      toScheme (name, t) = (name, Forall [] t)

  -- Typed lambda expressions
  TypedLambdaExpr params retTypeExpr body -> do
    let paramTypes = map (typeExprToType . snd) params
        paramBindings = zipWith (\(n, _) t -> (n, Forall [] t)) params paramTypes
    (bodyType, s) <- withEnv paramBindings $ inferExpr body
    let expectedRetType = typeExprToType retTypeExpr
    s' <- unifyTypes (applySubst s bodyType) expectedRetType
    let finalS = composeSubst s' s
        finalArgTypes = map (applySubst finalS) paramTypes
        funType = foldr TFun (applySubst finalS expectedRetType) finalArgTypes
    return (funType, finalS)

  -- Function application
  ApplyExpr func args -> do
    (funcType, s1) <- inferExpr func
    inferApplication funcType args s1

  -- If expression
  IfExpr cond thenE elseE -> do
    (condType, s1) <- inferExpr cond
    s2 <- unifyTypes condType TBool
    let s12 = composeSubst s2 s1
    (thenType, s3) <- inferExpr thenE
    (elseType, s4) <- inferExpr elseE
    s5 <- unifyTypes (applySubst s4 thenType) elseType
    let finalS = foldr composeSubst emptySubst [s5, s4, s3, s12]
    return (applySubst finalS elseType, finalS)

  -- Let expression
  LetExpr bindings body -> do
    s <- foldM inferBinding emptySubst bindings
    (bodyType, s') <- inferExpr body
    return (bodyType, composeSubst s' s)
    where
      inferBinding s (Bind pat e) = do
        (t, s') <- inferExpr e
        -- Extract pattern bindings and add to environment
        patBindings <- extractLetPatternBindings (applySubst s' t) pat
        env <- getEnv
        setEnv $ extendEnvMany patBindings env
        return $ composeSubst s' s
      inferBinding s (BindWithIndices (VarWithIndices name _) e) = do
        (t, s') <- inferExpr e
        env <- getEnv
        let scheme = generalize env (applySubst s' t)
        setEnv $ extendEnv name scheme env
        return $ composeSubst s' s

  -- Match expressions
  MatchExpr _ target matcher clauses -> inferMatch target matcher clauses
  MatchAllExpr _ target matcher clauses -> do
    (t, s) <- inferMatch target matcher clauses
    return (TList t, s)

  -- Type annotation
  TypeAnnotation e typeExpr -> do
    (inferredType, s) <- inferExpr e
    let annotatedType = typeExprToType typeExpr
    s' <- unifyTypes (applySubst s inferredType) annotatedType
    return (applySubst s' annotatedType, composeSubst s' s)

  -- Tensor expressions
  VectorExpr es -> do
    elemType <- freshVar "tElem"
    results <- mapM inferExpr es
    let ts = map fst results
    s <- foldM (\acc t -> do
      s' <- unifyTypes (applySubst acc elemType) t
      return $ composeSubst s' acc) emptySubst ts
    let n = length es
    return (TTensor (applySubst s elemType) (ShapeLit [fromIntegral n]) [], s)

  -- Indexed expressions (tensor indexing)
  -- e.g., g_i_j, X~i, A_i~j
  IndexedExpr _ base indices -> do
    (baseType, s) <- inferExpr base
    -- Convert AST indices to type system indices
    let typeIndices = map convertIndexExpr indices
    case baseType of
      TTensor elemTy shape existingIndices ->
        -- Combine or replace indices
        let newIndices = if null existingIndices
                           then typeIndices
                           else existingIndices ++ typeIndices
            contracted = removeSupSubPairs newIndices
            numContracted = (length newIndices - length contracted) `div` 2
            newShape = case shape of
                         ShapeLit dims -> ShapeLit (drop numContracted dims)
                         _ -> shape
        in return (normalizeTensorType $ TTensor elemTy newShape contracted, s)
      _ ->
        -- If base is not a tensor, create a tensor type with the indices
        return (baseType, s)

  -- Infix expressions
  InfixExpr op left right -> do
    (leftType, s1) <- inferExpr left
    (rightType, s2) <- inferExpr right
    let s = composeSubst s2 s1
        lt = applySubst s leftType
        rt = applySubst s rightType
    -- Handle tensor operations specially
    case repr op of
      -- Tensor dot product (contraction)
      "." -> case typeOfTensorDot lt rt of
        Right t -> return (t, s)
        Left _  -> do
          resultType <- freshVar "result"
          return (resultType, s)
      -- Tensor product (same indices -> element-wise, different -> outer product)
      "*" -> case typeOfTensorProduct lt rt of
        Right t -> return (t, s)
        Left _  -> inferScalarLiftedBinOp TInt lt rt s  -- Fall back to scalar lift
      -- Distinct product (always outer product with renamed indices)
      "!*" -> case typeOfDistinctProduct lt rt of
        Right t -> return (t, s)
        Left _  -> do
          resultType <- freshVar "result"
          return (resultType, s)
      -- Scalar operations that lift to tensors
      "+" -> inferScalarLiftedBinOp TInt lt rt s
      "-" -> inferScalarLiftedBinOp TInt lt rt s
      "/" -> inferScalarLiftedBinOp TFloat lt rt s
      -- Other operators
      _ -> do
        resultType <- freshVar "result"
        return (resultType, s)

  -- Matcher expression (Phase 3: Full type checking of pattern definitions)
  -- e.g., matcher | [] as () with ... | $ :: $ as (a, list a) with ...
  -- Structure:
  --   PatternDef = (PrimitivePatPattern, Expr, [(PrimitiveDataPattern, Expr)])
  --   - PrimitivePatPattern: pattern against patterns (e.g., `pair $ $`, `$ :: $`)
  --   - Expr: next matcher expression tuple (e.g., `(a, list a)`)
  --   - [(PrimitiveDataPattern, Expr)]: primitive match clauses
  --     - PrimitiveDataPattern: pattern against target (e.g., `($x, $y)`, `$x :: $xs`)
  --     - Expr: body returning list of next targets
  MatcherExpr patternDefs -> do
    -- First, infer the target type from data patterns
    targetType <- inferMatcherTargetType patternDefs
    -- Then, type check each pattern definition
    mapM_ (checkPatternDef targetType) patternDefs
    return (TMatcher targetType, emptySubst)

  -- Algebraic data matcher expression
  -- e.g., algebraicDataMatcher | leaf | node algebraicDataMatcher algebraicDataMatcher
  AlgebraicDataMatcherExpr constructors -> do
    -- Create a fresh type for the algebraic data type
    dataType <- freshVar "algData"
    -- Type check each constructor
    mapM_ (checkAlgebraicConstructor dataType) constructors
    return (TMatcher dataType, emptySubst)

  -- Pattern function expression
  -- e.g., \pat1 pat2 => ($pat & ~pat1) :: #pat :: ~pat2
  -- Type: Pattern a -> Pattern [a] -> Pattern [a]
  PatternFunctionExpr argNames _bodyPat -> do
    -- Each argument is a pattern, so its type is Pattern t for some t
    argTypes <- mapM (\_ -> freshVar "patArg") argNames
    -- The result type is also a pattern type
    resultType <- freshVar "patResult"
    -- Construct the pattern function type
    let patFuncType = TPatternFunc argTypes resultType
    return (patFuncType, emptySubst)

  -- Hash expression: {| (k1, v1), (k2, v2), ... |}
  -- Type: Hash k v
  HashExpr pairs -> do
    keyType <- freshVar "hashKey"
    valType <- freshVar "hashVal"
    s <- foldM inferHashPair emptySubst pairs
    return (THash (applySubst s keyType) (applySubst s valType), s)
    where
      inferHashPair s (k, v) = do
        (kt, s1) <- inferExpr k
        (vt, s2) <- inferExpr v
        s3 <- unifyTypes (applySubst (composeSubst s2 s1) keyType) kt
        s4 <- unifyTypes (applySubst (composeSubst s3 (composeSubst s2 s1)) valType) vt
        return $ foldr composeSubst s [s4, s3, s2, s1]
      keyType = TVar (TyVar "hashKey0")
      valType = TVar (TyVar "hashVal0")

  -- Sequence expression: expr1 ; expr2
  -- Type: the type of expr2 (expr1 is evaluated for side effects)
  SeqExpr expr1 expr2 -> do
    (_, s1) <- inferExpr expr1
    (t2, s2) <- inferExpr expr2
    return (t2, composeSubst s2 s1)

  -- Cons expression: x :: xs
  -- Type: a -> [a] -> [a]
  ConsExpr headExpr tailExpr -> do
    (headType, s1) <- inferExpr headExpr
    (tailType, s2) <- inferExpr tailExpr
    let s12 = composeSubst s2 s1
    s3 <- unifyTypes (TList (applySubst s12 headType)) (applySubst s12 tailType)
    let finalS = composeSubst s3 s12
    return (applySubst finalS tailType, finalS)

  -- Join expression: xs ++ ys
  -- Type: [a] -> [a] -> [a]
  JoinExpr leftExpr rightExpr -> do
    (leftType, s1) <- inferExpr leftExpr
    (rightType, s2) <- inferExpr rightExpr
    let s12 = composeSubst s2 s1
    s3 <- unifyTypes (applySubst s12 leftType) (applySubst s12 rightType)
    let finalS = composeSubst s3 s12
    return (applySubst finalS leftType, finalS)

  -- Section expression: (+ 1), (1 +), (+)
  -- Type depends on the operator and provided arguments
  SectionExpr op mleft mright -> case (mleft, mright) of
    -- Full section: (+) => a -> a -> a
    (Nothing, Nothing) -> do
      argType <- freshVar "secArg"
      return (TFun argType (TFun argType argType), emptySubst)
    -- Right section: (+ 1) => a -> a
    (Nothing, Just right) -> do
      (rightType, s1) <- inferExpr right
      resultType <- freshVar "secResult"
      return (TFun rightType resultType, s1)
    -- Left section: (1 +) => a -> a
    (Just left, Nothing) -> do
      (leftType, s1) <- inferExpr left
      resultType <- freshVar "secResult"
      return (TFun leftType resultType, s1)
    -- This case shouldn't happen (both provided)
    (Just _, Just _) -> do
      t <- freshVar "section"
      return (t, emptySubst)

  -- Quote expression: 'expr
  -- Type: MathExpr (= Integer in our type system)
  QuoteExpr _ -> return (TInt, emptySubst)

  -- Quote symbol expression: `expr
  QuoteSymbolExpr _ -> return (TInt, emptySubst)

  -- LetRec expression (recursive let)
  -- Similar to Let but allows recursive definitions
  LetRecExpr bindings body -> do
    -- First, add all bindings with fresh type variables
    freshTypes <- mapM (\_ -> freshVar "letrec") bindings
    let bindingNames = map extractBindingName bindings
        initialBindings = zip bindingNames (map (Forall []) freshTypes)
    -- Infer types with recursive bindings in scope
    s <- withEnv initialBindings $ foldM inferRecBinding emptySubst (zip bindings freshTypes)
    -- Infer body type
    (bodyType, s') <- withEnv initialBindings $ inferExpr body
    return (bodyType, composeSubst s' s)
    where
      extractBindingName (Bind (PDPatVar name) _) = name
      extractBindingName (BindWithIndices (VarWithIndices name _) _) = name
      extractBindingName _ = "_"
      
      inferRecBinding s (Bind _ e, expectedType) = do
        (t, s') <- inferExpr e
        s'' <- unifyTypes (applySubst s' t) (applySubst s' expectedType)
        return $ composeSubst s'' (composeSubst s' s)
      inferRecBinding s (BindWithIndices _ e, expectedType) = do
        (t, s') <- inferExpr e
        s'' <- unifyTypes (applySubst s' t) (applySubst s' expectedType)
        return $ composeSubst s'' (composeSubst s' s)

  -- Match lambda expression: \x -> match x as matcher with ...
  -- Equivalent to a lambda that immediately matches
  MatchLambdaExpr matcher clauses -> do
    argType <- freshVar "matchLamArg"
    (matcherType, s1) <- inferExpr matcher
    -- Check matcher compatibility
    (elemType, s2) <- checkMatcherTargetCompatibility matcherType argType
    let s12 = composeSubst s2 s1
    -- Infer result type from clauses
    resultType <- freshVar "matchLamResult"
    clauseSubsts <- mapM (inferMatchLambdaClause argType matcherType elemType resultType) clauses
    let finalS = foldr composeSubst s12 clauseSubsts
    return (TFun (applySubst finalS argType) (applySubst finalS resultType), finalS)
    where
      inferMatchLambdaClause tgtType mType elemType expectedType (pat, body) = do
        patBindings <- extractPatternBindings tgtType mType elemType pat
        withEnv patBindings $ do
          (bodyType, s) <- inferExpr body
          s' <- unifyTypes (applySubst s bodyType) expectedType
          return $ composeSubst s' s

  -- MatchAll lambda expression
  MatchAllLambdaExpr matcher clauses -> do
    argType <- freshVar "matchAllLamArg"
    (matcherType, s1) <- inferExpr matcher
    (elemType, s2) <- checkMatcherTargetCompatibility matcherType argType
    let s12 = composeSubst s2 s1
    resultType <- freshVar "matchAllLamResult"
    clauseSubsts <- mapM (inferMatchLambdaClause argType matcherType elemType resultType) clauses
    let finalS = foldr composeSubst s12 clauseSubsts
    -- matchAll returns a list
    return (TFun (applySubst finalS argType) (TList (applySubst finalS resultType)), finalS)
    where
      inferMatchLambdaClause tgtType mType elemType expectedType (pat, body) = do
        patBindings <- extractPatternBindings tgtType mType elemType pat
        withEnv patBindings $ do
          (bodyType, s) <- inferExpr body
          s' <- unifyTypes (applySubst s bodyType) expectedType
          return $ composeSubst s' s

  -- Anonymous parameter function: 1#(%1 + 1), 2#(%1 + %2)
  -- Type: a1 -> a2 -> ... -> result
  AnonParamFuncExpr arity body -> do
    argTypes <- mapM (\i -> freshVar ("anonArg" ++ show i)) [1..arity]
    -- We need to track anonymous parameters in the body
    -- For now, just infer the body type
    (bodyType, s) <- inferExpr body
    let funType = foldr TFun bodyType argTypes
    return (funType, s)

  -- Anonymous tuple parameter function: 1##(fst %1, snd %1)
  AnonTupleParamFuncExpr arity body -> do
    argTypes <- mapM (\i -> freshVar ("anonTupArg" ++ show i)) [1..arity]
    (bodyType, s) <- inferExpr body
    let funType = foldr TFun bodyType argTypes
    return (funType, s)

  -- Anonymous list parameter function
  AnonListParamFuncExpr arity body -> do
    argTypes <- mapM (\i -> freshVar ("anonListArg" ++ show i)) [1..arity]
    (bodyType, s) <- inferExpr body
    let funType = foldr TFun bodyType argTypes
    return (funType, s)

  -- Anonymous parameter: %1, %2, etc.
  -- Type: depends on context (handled by AnonParamFuncExpr)
  AnonParamExpr _n -> do
    t <- freshVar "anonParam"
    return (t, emptySubst)

  -- WithSymbols expression: withSymbols [x, y] expr
  -- The symbols are bound as type variables in the expression
  WithSymbolsExpr _symbols body -> inferExpr body

  -- Generate tensor expression: generateTensor f shape
  GenerateTensorExpr genFunc shapeExpr -> do
    (funcType, s1) <- inferExpr genFunc
    (shapeType, s2) <- inferExpr shapeExpr
    let s12 = composeSubst s2 s1
    -- shapeExpr should be [Integer]
    s3 <- unifyTypes (applySubst s12 shapeType) (TList TInt)
    -- funcType should be [Integer] -> a
    elemType <- freshVar "tensorElem"
    s4 <- unifyTypes (applySubst (composeSubst s3 s12) funcType) (TFun (TList TInt) elemType)
    let finalS = foldr composeSubst emptySubst [s4, s3, s2, s1]
    return (TTensor (applySubst finalS elemType) ShapeUnknown [], finalS)

  -- Tensor contract expression
  TensorContractExpr tensor -> do
    (tensorType, s) <- inferExpr tensor
    case tensorType of
      TTensor elemTy shape indices ->
        let contracted = removeSupSubPairs indices
            newShape = case shape of
                         ShapeLit dims -> ShapeLit (drop ((length indices - length contracted) `div` 2) dims)
                         _ -> shape
        in return (normalizeTensorType $ TTensor elemTy newShape contracted, s)
      _ -> return (tensorType, s)

  -- Tensor map expression: tensorMap f tensor
  TensorMapExpr funcExpr tensorExpr -> do
    (funcType, s1) <- inferExpr funcExpr
    (tensorType, s2) <- inferExpr tensorExpr
    let s12 = composeSubst s2 s1
    case (applySubst s12 funcType, applySubst s12 tensorType) of
      (TFun argTy resultTy, TTensor elemTy shape indices) -> do
        s3 <- unifyTypes argTy elemTy
        let finalS = composeSubst s3 s12
        return (TTensor (applySubst finalS resultTy) shape indices, finalS)
      _ -> do
        resultType <- freshVar "tensorMapResult"
        return (resultType, s12)

  -- Tensor map2 expression: tensorMap2 f tensor1 tensor2
  TensorMap2Expr funcExpr tensor1Expr tensor2Expr -> do
    (funcType, s1) <- inferExpr funcExpr
    (tensor1Type, s2) <- inferExpr tensor1Expr
    (tensor2Type, s3) <- inferExpr tensor2Expr
    let s123 = foldr composeSubst emptySubst [s3, s2, s1]
    case applySubst s123 funcType of
      TFun arg1Ty (TFun arg2Ty resultTy) ->
        case (applySubst s123 tensor1Type, applySubst s123 tensor2Type) of
          (TTensor elem1Ty shape1 indices1, TTensor elem2Ty _shape2 _indices2) -> do
            s4 <- unifyTypes arg1Ty elem1Ty
            s5 <- unifyTypes arg2Ty elem2Ty
            let finalS = foldr composeSubst s123 [s5, s4]
            return (TTensor (applySubst finalS resultTy) shape1 indices1, finalS)
          _ -> do
            resultType <- freshVar "tensorMap2Result"
            return (resultType, s123)
      _ -> do
        resultType <- freshVar "tensorMap2Result"
        return (resultType, s123)

  -- Transpose expression
  TransposeExpr indicesExpr tensorExpr -> do
    (_, s1) <- inferExpr indicesExpr
    (tensorType, s2) <- inferExpr tensorExpr
    let s12 = composeSubst s2 s1
    -- Transpose preserves the tensor type but reorders indices
    return (applySubst s12 tensorType, s12)

  -- Flip indices expression
  FlipIndicesExpr tensorExpr -> do
    (tensorType, s) <- inferExpr tensorExpr
    -- Flipping indices swaps superscript and subscript
    case tensorType of
      TTensor elemTy shape indices ->
        let flippedIndices = map flipIndex indices
        in return (TTensor elemTy shape flippedIndices, s)
      _ -> return (tensorType, s)
    where
      flipIndex (IndexSym Subscript name) = IndexSym Superscript name
      flipIndex (IndexSym Superscript name) = IndexSym Subscript name
      flipIndex (IndexPlaceholder Subscript) = IndexPlaceholder Superscript
      flipIndex (IndexPlaceholder Superscript) = IndexPlaceholder Subscript
      flipIndex idx = idx

  -- CApply expression (collection apply)
  CApplyExpr funcExpr argExpr -> do
    (funcType, s1) <- inferExpr funcExpr
    (argType, s2) <- inferExpr argExpr
    let s12 = composeSubst s2 s1
    resultType <- freshVar "capplyResult"
    -- CApply applies a function to all elements of a collection
    case applySubst s12 argType of
      TList elemTy -> do
        s3 <- unifyTypes (applySubst s12 funcType) (TFun (TList elemTy) resultType)
        let finalS = composeSubst s3 s12
        return (applySubst finalS resultType, finalS)
      _ -> do
        s3 <- unifyTypes (applySubst s12 funcType) (TFun (applySubst s12 argType) resultType)
        let finalS = composeSubst s3 s12
        return (applySubst finalS resultType, finalS)

  -- Wedge apply expression (exterior algebra)
  WedgeApplyExpr funcExpr argExprs -> do
    (funcType, s1) <- inferExpr funcExpr
    results <- mapM inferExpr argExprs
    let argTypes = map fst results
        argSubsts = map snd results
        combinedS = foldr composeSubst s1 argSubsts
    resultType <- freshVar "wedgeResult"
    let expectedFuncType = foldr TFun resultType argTypes
    s' <- unifyTypes (applySubst combinedS funcType) expectedFuncType
    let finalS = composeSubst s' combinedS
    return (applySubst finalS resultType, finalS)

  -- Do expression (monadic sequencing)
  DoExpr bindings body -> do
    s <- foldM inferDoBinding emptySubst bindings
    (bodyType, s') <- inferExpr body
    return (bodyType, composeSubst s' s)
    where
      inferDoBinding s (Bind pat e) = do
        (t, s') <- inferExpr e
        patBindings <- extractLetPatternBindings (applySubst s' t) pat
        env <- getEnv
        setEnv $ extendEnvMany patBindings env
        return $ composeSubst s' s
      inferDoBinding s (BindWithIndices (VarWithIndices name _) e) = do
        (t, s') <- inferExpr e
        env <- getEnv
        let scheme = generalize env (applySubst s' t)
        setEnv $ extendEnv name scheme env
        return $ composeSubst s' s

  -- Prefix expression (unary operators like -)
  PrefixExpr op expr -> do
    (exprType, s) <- inferExpr expr
    case op of
      "-" -> do
        -- Negation: works on Int or Float
        s' <- unifyTypes exprType TInt
        return (TInt, composeSubst s' s)
      "!" -> do
        -- Logical not or wedge prefix (handled elsewhere)
        return (exprType, s)
      _ -> return (exprType, s)

  -- Fresh variable expression
  FreshVarExpr -> return (TInt, emptySubst)  -- Fresh vars are typically symbols (MathExpr)

  -- Function expression (for defining functions with indices)
  FunctionExpr _args -> return (TInt, emptySubst)  -- Returns a MathExpr

  -- Subrefs/Suprefs/Userrefs expressions (index manipulation)
  SubrefsExpr _ base _ -> inferExpr base
  SuprefsExpr _ base _ -> inferExpr base
  UserrefsExpr _ base _ -> inferExpr base

  -- TensorExpr (raw tensor construction)
  TensorExpr dataExpr shapeExpr -> do
    (dataType, s1) <- inferExpr dataExpr
    (_, s2) <- inferExpr shapeExpr
    let s12 = composeSubst s2 s1
    case applySubst s12 dataType of
      TList elemTy -> return (TTensor elemTy ShapeUnknown [], s12)
      _ -> return (TTensor (applySubst s12 dataType) ShapeUnknown [], s12)

  -- Default case for any remaining unhandled expressions
  _ -> do
    t <- freshVar "unknown"
    return (t, emptySubst)

-- | Type check a pattern definition (matcher clause)
-- PatternDef = (PrimitivePatPattern, Expr, [(PrimitiveDataPattern, Expr)])
--   - primPatPat: primitive pattern-pattern (e.g., `pair $ $`)
--   - nextMatcherExpr: tuple of next matchers (e.g., `(a, list a)`)
--   - primMatchClauses: [(data pattern, body)]
-- 
-- Type rules:
--   1. The number of pattern holes ($) in primPatPat = tuple size of nextMatcherExpr
--   2. Data pattern variables are typed based on target type
--   3. Body must return a list of next targets: [nextMatcherType]
checkPatternDef :: Type -> PatternDef -> Infer ()
checkPatternDef targetType (primPatPat, nextMatcherExpr, primMatchClauses) = do
  -- Count pattern holes in primitive pattern-pattern
  let holeCount = countPatternHoles primPatPat
  
  -- Infer the next matcher expression type
  (nextMatcherType, _) <- inferExpr nextMatcherExpr
  
  -- Verify hole count matches next matcher tuple size
  let expectedHoles = case nextMatcherType of
        TTuple ts -> length ts
        TUnit     -> 0
        _         -> 1
  
  -- We don't fail hard here, just validate silently
  -- (Could add a warning system later)
  when (holeCount /= expectedHoles && holeCount > 0 && expectedHoles > 0) $
    return ()  -- Mismatch, but we continue
  
  -- Type check each primitive match clause
  mapM_ (checkPrimMatchClause targetType nextMatcherType) primMatchClauses

-- | Count pattern holes ($) in a primitive pattern-pattern
countPatternHoles :: PrimitivePatPattern -> Int
countPatternHoles PPWildCard = 0
countPatternHoles PPPatVar = 1  -- $ is a pattern hole
countPatternHoles (PPValuePat _) = 0
countPatternHoles (PPInductivePat _ args) = sum (map countPatternHoles args)
countPatternHoles (PPTuplePat args) = sum (map countPatternHoles args)

-- | Type check a primitive match clause
-- (PrimitiveDataPattern, Expr) where:
--   - data pattern matches against target
--   - body returns list of next targets
checkPrimMatchClause :: Type -> Type -> (PrimitiveDataPattern, Expr) -> Infer ()
checkPrimMatchClause targetType nextMatcherType (dataPat, body) = do
  -- Extract variable bindings from data pattern
  bindings <- extractDataPatternBindings targetType dataPat
  
  -- Infer body type with data pattern variables in scope
  withEnv bindings $ do
    (bodyType, _) <- inferExpr body
    
    -- Body should return [nextMatcherType] (list of next targets)
    -- For example: [(x, y), (y, x)] for next matcher type (a, a)
    let expectedBodyType = TList nextMatcherType
    _ <- unifyTypes bodyType expectedBodyType
    return ()

-- | Extract variable bindings from a primitive data pattern
-- Similar to extractLetPatternBindings but for primitive data patterns
extractDataPatternBindings :: Type -> PrimitiveDataPattern -> Infer [(String, TypeScheme)]
extractDataPatternBindings t pat = go t pat
  where
    go :: Type -> PrimitiveDataPattern -> Infer [(String, TypeScheme)]
    go _ PDWildCard = return []
    go ty (PDPatVar var) = return [(var, Forall [] ty)]
    go _ PDEmptyPat = return []
    go _ (PDConstantPat _) = return []
    go ty (PDTuplePat pats) = do
      -- Create fresh types for each tuple element
      elemTypes <- case ty of
        TTuple ts | length ts == length pats -> return ts
        _ -> mapM (\_ -> freshVar "tupleElem") pats
      concat <$> zipWithM go elemTypes pats
    go ty (PDConsPat headPat tailPat) = do
      elemTy <- case ty of
        TList e -> return e
        _ -> freshVar "consElem"
      headBindings <- go elemTy headPat
      tailBindings <- go ty tailPat
      return (headBindings ++ tailBindings)
    go ty (PDSnocPat initPat lastPat) = do
      elemTy <- case ty of
        TList e -> return e
        _ -> freshVar "snocElem"
      initBindings <- go ty initPat
      lastBindings <- go elemTy lastPat
      return (initBindings ++ lastBindings)
    go ty (PDInductivePat "cons" [headPat, tailPat]) = do
      elemTy <- case ty of
        TList e -> return e
        _ -> freshVar "consElem"
      headBindings <- go elemTy headPat
      tailBindings <- go ty tailPat
      return (headBindings ++ tailBindings)
    go _ (PDInductivePat _ pats) = do
      -- For other inductive patterns, use fresh types
      freshTypes <- mapM (\_ -> freshVar "indElem") pats
      concat <$> zipWithM go freshTypes pats

-- | Type check an algebraic data matcher constructor
checkAlgebraicConstructor :: Type -> (String, [Expr]) -> Infer ()
checkAlgebraicConstructor _dataType (_name, argExprs) = do
  -- Infer types of each argument expression
  -- Each argument should be a matcher type
  mapM_ inferExpr argExprs

-- | Infer the target type of a matcher from its pattern definitions
-- Pattern definition: (PrimitivePatPattern, Expr, [(PrimitiveDataPattern, Expr)])
-- The third element contains data patterns that reveal the target type
inferMatcherTargetType :: [PatternDef] -> Infer Type
inferMatcherTargetType [] = freshVar "matcherTarget"
inferMatcherTargetType patternDefs = do
  -- Look at the data patterns (PrimitiveDataPattern) to infer the target type
  -- If any data pattern uses cons (::), the target is a list type
  -- Example: | $ :: $ as (a, list a) with
  --            | $x :: $xs -> [(x, xs)]  <- this data pattern has ::
  
  targetType <- freshVar "target"
  
  -- Extract all data patterns from pattern definitions
  let allDataPatterns = concatMap getDataPatterns patternDefs
      hasConsPat = any hasConsDataPattern allDataPatterns
  
  if hasConsPat
    then do
      -- Data pattern uses ::, so target is a list type
      elemType <- freshVar "elem"
      return $ TList elemType
    else
      return targetType
  where
    -- Get data patterns from a pattern definition
    getDataPatterns :: PatternDef -> [PrimitiveDataPattern]
    getDataPatterns (_, _, dataPats) = map fst dataPats
    
    -- Check if a data pattern contains cons (::)
    hasConsDataPattern :: PrimitiveDataPattern -> Bool
    hasConsDataPattern (PDConsPat _ _) = True
    hasConsDataPattern (PDSnocPat _ _) = True  -- snoc also implies list
    hasConsDataPattern (PDInductivePat "cons" _) = True
    hasConsDataPattern (PDInductivePat "snoc" _) = True
    hasConsDataPattern (PDTuplePat pats) = any hasConsDataPattern pats
    hasConsDataPattern _ = False

-- | Infer type of a constant
inferConstant :: ConstantExpr -> Infer (Type, Subst)
inferConstant c = return $ case c of
  CharExpr _    -> (TChar, emptySubst)
  StringExpr _  -> (TString, emptySubst)
  BoolExpr _    -> (TBool, emptySubst)
  IntegerExpr _ -> (TInt, emptySubst)
  FloatExpr _   -> (TFloat, emptySubst)
  SomethingExpr -> (TAny, emptySubst)
  UndefinedExpr -> (TAny, emptySubst)

-- | Infer type for scalar operation lifted to tensors
-- For operators like +, -, *, / when applied to tensors
inferScalarLiftedBinOp :: Type -> Type -> Type -> Subst -> Infer (Type, Subst)
inferScalarLiftedBinOp scalarType leftType rightType s = do
  let opType = TFun scalarType (TFun scalarType scalarType)
  case liftScalarBinOp opType leftType rightType of
    Right t -> return (t, s)
    Left _ -> do
      -- Fall back: try to unify with scalar type
      resultType <- freshVar "result"
      return (resultType, s)

-- | Infer function application
inferApplication :: Type -> [Expr] -> Subst -> Infer (Type, Subst)
inferApplication funcType args s = do
  results <- mapM inferExpr args
  let argTypes = map fst results
      argSubsts = map snd results
      combinedS = foldr composeSubst s argSubsts
  resultType <- freshVar "result"
  let expectedFuncType = foldr TFun resultType argTypes
  s' <- unifyTypes (applySubst combinedS funcType) expectedFuncType
  let finalS = composeSubst s' combinedS
  return (applySubst finalS resultType, finalS)

-- | Infer match expression
inferMatch :: Expr -> Expr -> [MatchClause] -> Infer (Type, Subst)
inferMatch target matcher clauses = do
  (targetType, s1) <- inferExpr target
  (matcherType, s2) <- inferExpr matcher
  -- Check that matcher type is compatible with target type
  -- e.g., Matcher [Integer] should match target type [Integer]
  (elemType, s3) <- checkMatcherTargetCompatibility matcherType targetType
  let combinedS = foldr composeSubst emptySubst [s3, s2, s1]
  resultType <- freshVar "matchResult"
  -- Pass both target type and matcher type for pattern inference
  clauseResults <- mapM (inferClause targetType matcherType elemType resultType) clauses
  let clauseSubsts = map snd clauseResults
      finalS = foldr composeSubst combinedS clauseSubsts
  return (applySubst finalS resultType, finalS)
  where
    inferClause tgtType mType elemType expectedType (pat, body) = do
      -- Extract pattern variables and their types
      -- Pass matcher type so pattern constructors can be interpreted correctly
      patBindings <- extractPatternBindings tgtType mType elemType pat
      -- Infer body type with pattern variables in scope
      withEnv patBindings $ do
        (bodyType, s) <- inferExpr body
        s' <- unifyTypes (applySubst s bodyType) expectedType
        return ((), composeSubst s' s)

-- | Check that matcher type is compatible with target type
-- Returns the element type for pattern matching and a substitution
-- e.g., Matcher [Integer] with target [Integer] -> (Integer, subst)
checkMatcherTargetCompatibility :: Type -> Type -> Infer (Type, Subst)
checkMatcherTargetCompatibility matcherType targetType = case matcherType of
  -- Matcher [a] should match [a]
  TMatcher (TList elemTy) -> do
    s <- unifyTypes (TList elemTy) targetType
    return (applySubst s elemTy, s)

  -- Matcher a should match a
  TMatcher innerTy -> do
    s <- unifyTypes innerTy targetType
    return (applySubst s innerTy, s)

  -- If matcher type is a type variable, generate constraints
  TVar _ -> do
    elemTy <- freshVar "matchElem"
    s <- unifyTypes matcherType (TMatcher targetType)
    return (applySubst s elemTy, s)

  -- For any other type, fall back to extracting element type without strict checking
  _ -> return (extractElementType matcherType targetType, emptySubst)

-- | Extract element type from matcher type
-- e.g., Matcher (List a) with target [Integer] -> Integer
extractElementType :: Type -> Type -> Type
extractElementType (TMatcher (TList elemTy)) _ = elemTy
extractElementType (TMatcher ty) _ = ty
extractElementType _ (TList elemTy) = elemTy
extractElementType _ ty = ty

-- | Extract variable bindings from a pattern
-- Takes target type, matcher type, and element type
-- The matcher type is used to interpret pattern constructors correctly
-- (e.g., "cons" means different things for list vs multiset)
extractPatternBindings :: Type -> Type -> Type -> Pattern -> Infer [(String, TypeScheme)]
extractPatternBindings targetType matcherType elemType pat = go targetType matcherType elemType pat
  where
    -- Helper to check if matcher is a list-like matcher
    isListMatcher :: Type -> Bool
    isListMatcher (TMatcher (TList _)) = True
    isListMatcher _ = False
    
    -- Helper to get element type from matcher
    getMatcherElemType :: Type -> Maybe Type
    getMatcherElemType (TMatcher (TList e)) = Just e
    getMatcherElemType (TMatcher t) = Just t
    getMatcherElemType _ = Nothing

    go _tgt _m ty WildCard = return []

    go _tgt _m ty (PatVar name) = return [(name, Forall [] ty)]

    go _tgt _m _ (ValuePat _) = return []

    go _tgt _m _ (PredPat _) = return []

    go tgt m ty (IndexedPat p _) = go tgt m ty p

    -- LetPat: let bindings in pattern
    -- The bindings introduce variables that can be used in the pattern
    go tgt m ty (LetPat bindings p) = do
      -- Extract bindings from let expressions and add to environment
      letBindings <- extractLetPatBindings bindings
      -- Process the inner pattern with the let bindings in scope
      withEnv letBindings $ do
        patBindings <- go tgt m ty p
        return (letBindings ++ patBindings)
      where
        -- Extract variable bindings from LetPat's binding list
        extractLetPatBindings :: [BindingExpr] -> Infer [(String, TypeScheme)]
        extractLetPatBindings [] = return []
        extractLetPatBindings (Bind pat' expr : rest) = do
          -- Infer the type of the expression
          (exprType, _) <- inferExpr expr
          -- Extract pattern bindings
          patBinds <- extractLetPatternBindings exprType pat'
          -- Process remaining bindings
          restBinds <- extractLetPatBindings rest
          return (patBinds ++ restBinds)
        extractLetPatBindings (BindWithIndices (VarWithIndices name _) expr : rest) = do
          -- Infer the type of the expression
          (exprType, _) <- inferExpr expr
          env <- getEnv
          let binding = (name, generalize env exprType)
          -- Process remaining bindings
          restBinds <- extractLetPatBindings rest
          return (binding : restBinds)

    go _tgt _m _ty (NotPat _) = return []

    go tgt m ty (AndPat p1 p2) = do
      b1 <- go tgt m ty p1
      b2 <- go tgt m ty p2
      return (b1 ++ b2)

    go tgt m ty (OrPat p1 p2) = do
      b1 <- go tgt m ty p1
      b2 <- go tgt m ty p2
      return (b1 ++ b2)

    go tgt m ty (ForallPat p1 p2) = do
      b1 <- go tgt m ty p1
      b2 <- go tgt m ty p2
      return (b1 ++ b2)

    go _tgt _m ty (TuplePat pats) = do
      case ty of
        TTuple tys | length tys == length pats ->
          concat <$> zipWithM (\t p -> go t TAny t p) tys pats
        _ -> do
          -- Generate fresh types for each element
          freshTypes <- mapM (\_ -> freshVar "tup") pats
          concat <$> zipWithM (\t p -> go t TAny t p) freshTypes pats

    -- Pattern constructors are interpreted based on matcher type
    go tgt m ty (InductivePat name pats) = do
      -- For cons/join patterns, check if the matcher is list-like
      case name of
        "cons" | [headPat, tailPat] <- pats -> do
          -- If matcher is list-like, cons splits into (element, list)
          -- Otherwise, use fresh types
          case tgt of
            TList elemTy -> do
              headBindings <- go elemTy m elemTy headPat
              -- For tail, matcher becomes same matcher (list a -> list a)
              tailBindings <- go tgt m tgt tailPat
              return (headBindings ++ tailBindings)
            _ -> do
              -- Not a list - use fresh types for each sub-pattern
              freshTypes <- mapM (\_ -> freshVar "cons") pats
              concat <$> zipWithM (\t p -> go t m t p) freshTypes pats
              
        "join" | [leftPat, rightPat] <- pats -> do
          -- Join splits a collection into two parts of the same type
          case tgt of
            TList _ -> do
              leftBindings <- go tgt m ty leftPat
              rightBindings <- go tgt m ty rightPat
              return (leftBindings ++ rightBindings)
            _ -> do
              freshTypes <- mapM (\_ -> freshVar "join") pats
              concat <$> zipWithM (\t p -> go t m t p) freshTypes pats
              
        "nil" -> return []
        
        "snoc" | [initPat, lastPat] <- pats -> do
          case tgt of
            TList elemTy -> do
              initBindings <- go tgt m tgt initPat
              lastBindings <- go elemTy m elemTy lastPat
              return (initBindings ++ lastBindings)
            _ -> do
              freshTypes <- mapM (\_ -> freshVar "snoc") pats
              concat <$> zipWithM (\t p -> go t m t p) freshTypes pats
              
        _ -> do
          -- For unknown pattern constructors, use fresh types
          freshTypes <- mapM (\_ -> freshVar "ind") pats
          concat <$> zipWithM (\t p -> go t m t p) freshTypes pats

    go tgt m ty (InfixPat op p1 p2) = do
      -- Infix patterns: interpret based on target type
      case repr op of
        "::" -> do
          -- cons pattern - only list-like if target is list
          case tgt of
            TList elemTy -> do
              b1 <- go elemTy m elemTy p1
              b2 <- go tgt m tgt p2
              return (b1 ++ b2)
            _ -> do
              -- Not a list - use element type for both
              b1 <- go ty m ty p1
              b2 <- go ty m ty p2
              return (b1 ++ b2)
        "++" -> do
          -- join pattern - both sides have target type
          b1 <- go tgt m ty p1
          b2 <- go tgt m ty p2
          return (b1 ++ b2)
        _ -> do
          b1 <- go ty m ty p1
          b2 <- go ty m ty p2
          return (b1 ++ b2)

    -- LoopPat: loop $i (start, end) bodyPat contPat
    -- - Loop index variable ($i): Integer
    -- - Indexed pattern variables ($h_i): Hash Integer elemType
    -- - Range expressions (start, end): Integer
    -- - End pattern in LoopRange: matched against Integer
    -- - Body pattern (p1): matched against target (typically starts with :: ...)
    -- - Continuation pattern (p2): matched against target (the whole list/collection type)
    go tgt m ty (LoopPat indexVar (LoopRange _startExpr _endExpr endPat) bodyPat contPat) = do
      -- Loop index variable is Integer
      let indexBinding = (indexVar, Forall [] TInt)
      
      -- Extract bindings from end pattern (matched against Integer)
      endBindings <- go TInt m TInt endPat
      
      -- Extract bindings from body pattern with index variable in scope
      -- Note: indexed pattern variables like $h_i become Hash Integer elemType
      bodyBindings <- withEnv [indexBinding] $ do
        extractLoopBodyBindings tgt m ty bodyPat
      
      -- Extract bindings from continuation pattern
      -- The continuation pattern matches against the whole target type (e.g., [Integer])
      -- not the element type
      contBindings <- go tgt m tgt contPat
      
      return (indexBinding : endBindings ++ bodyBindings ++ contBindings)
      where
        -- Helper for extracting bindings with indexed pattern variable support
        extractLoopBodyBindings :: Type -> Type -> Type -> Pattern -> Infer [(String, TypeScheme)]
        extractLoopBodyBindings tgt' m' ty' pat' = case pat' of
          -- Indexed pattern variable: $h_i -> Hash Integer elemType
          IndexedPat (PatVar name) _indices -> do
            -- Indexed pattern variables are hashes from Integer to element type
            return [(name, Forall [] (THash TInt ty'))]
          
          -- Recurse for other patterns
          IndexedPat p _ -> extractLoopBodyBindings tgt' m' ty' p
          
          AndPat p1 p2 -> do
            b1 <- extractLoopBodyBindings tgt' m' ty' p1
            b2 <- extractLoopBodyBindings tgt' m' ty' p2
            return (b1 ++ b2)
          
          OrPat p1 p2 -> do
            b1 <- extractLoopBodyBindings tgt' m' ty' p1
            b2 <- extractLoopBodyBindings tgt' m' ty' p2
            return (b1 ++ b2)
          
          InfixPat op' p1 p2 -> do
            -- Handle :: and ++ with proper types
            case repr op' of
              "::" -> do
                case tgt' of
                  TList elemTy -> do
                    b1 <- extractLoopBodyBindings elemTy m' elemTy p1
                    b2 <- extractLoopBodyBindings tgt' m' tgt' p2
                    return (b1 ++ b2)
                  _ -> do
                    b1 <- extractLoopBodyBindings ty' m' ty' p1
                    b2 <- extractLoopBodyBindings ty' m' ty' p2
                    return (b1 ++ b2)
              "++" -> do
                b1 <- extractLoopBodyBindings tgt' m' ty' p1
                b2 <- extractLoopBodyBindings tgt' m' ty' p2
                return (b1 ++ b2)
              _ -> do
                b1 <- extractLoopBodyBindings ty' m' ty' p1
                b2 <- extractLoopBodyBindings ty' m' ty' p2
                return (b1 ++ b2)
          
          TuplePat pats -> do
            case ty' of
              TTuple tys | length tys == length pats ->
                concat <$> zipWithM (\t' p -> extractLoopBodyBindings t' m' t' p) tys pats
              _ -> do
                freshTypes <- mapM (\_ -> freshVar "tup") pats
                concat <$> zipWithM (\t' p -> extractLoopBodyBindings t' m' t' p) freshTypes pats
          
          InductivePat name pats -> do
            case name of
              "cons" | [headPat, tailPat] <- pats -> do
                case tgt' of
                  TList elemTy -> do
                    headBindings <- extractLoopBodyBindings elemTy m' elemTy headPat
                    tailBindings <- extractLoopBodyBindings tgt' m' tgt' tailPat
                    return (headBindings ++ tailBindings)
                  _ -> do
                    freshTypes <- mapM (\_ -> freshVar "cons") pats
                    concat <$> zipWithM (\t' p -> extractLoopBodyBindings t' m' t' p) freshTypes pats
              _ -> do
                freshTypes <- mapM (\_ -> freshVar "ind") pats
                concat <$> zipWithM (\t' p -> extractLoopBodyBindings t' m' t' p) freshTypes pats
          
          -- For simple pattern variables (not indexed), use normal type
          PatVar name -> return [(name, Forall [] ty')]
          VarPat name -> return [(name, Forall [] ty')]
          
          -- Other patterns delegate to go
          _ -> go tgt' m' ty' pat'

    go _tgt _m _ ContPat = return []

    -- PApplyPat: pattern function application
    -- e.g., twin $x (shuntsu $y rest)
    go tgt m _ty (PApplyPat funcExpr argPats) = do
      -- Infer the type of the pattern function
      (funcType, _) <- inferExpr funcExpr
      -- Extract argument types from the pattern function type
      case funcType of
        TPatternFunc argTypes _resultType
          | length argTypes == length argPats -> do
            -- Use the declared argument types (inner types of Pattern a)
            concat <$> zipWithM (\t p -> go t m t p) argTypes argPats
        _ -> do
          -- Type unknown or mismatch - use fresh types
          freshTypes <- mapM (\_ -> freshVar "papp") argPats
          concat <$> zipWithM (\t p -> go t m t p) freshTypes argPats

    go _tgt _m ty (VarPat name) = return [(name, Forall [] ty)]

    go tgt m ty (InductiveOrPApplyPat name pats) = do
      -- Similar to InductivePat - interpret based on target type
      case name of
        "cons" | [headPat, tailPat] <- pats -> do
          case tgt of
            TList elemTy -> do
              headBindings <- go elemTy m elemTy headPat
              tailBindings <- go tgt m tgt tailPat
              return (headBindings ++ tailBindings)
            _ -> do
              freshTypes <- mapM (\_ -> freshVar "iopa") pats
              concat <$> zipWithM (\t p -> go t m t p) freshTypes pats
        _ -> do
          freshTypes <- mapM (\_ -> freshVar "iopa") pats
          concat <$> zipWithM (\t p -> go t m t p) freshTypes pats

    go _tgt _m _ SeqNilPat = return []

    go tgt m ty (SeqConsPat p1 p2) = do
      case tgt of
        TList elemTy -> do
          b1 <- go elemTy m elemTy p1
          b2 <- go tgt m tgt p2
          return (b1 ++ b2)
        _ -> do
          b1 <- go ty m ty p1
          b2 <- go (TList ty) m (TList ty) p2
          return (b1 ++ b2)

    go _tgt _m _ LaterPatVar = return []

    go tgt m ty (DApplyPat p pats) = do
      b1 <- go tgt m ty p
      freshTypes <- mapM (\_ -> freshVar "dapp") pats
      bs <- concat <$> zipWithM (\t p' -> go t m t p') freshTypes pats
      return (b1 ++ bs)

-- | Extract variable bindings from a primitive data pattern (used in let)
extractLetPatternBindings :: Type -> PrimitiveDataPattern -> Infer [(String, TypeScheme)]
extractLetPatternBindings ty pat = do
  env <- getEnv
  bindings <- go ty pat
  return $ map (\(n, t) -> (n, generalize env t)) bindings
  where
    go :: Type -> PrimitiveDataPattern -> Infer [(String, Type)]
    go _ PDWildCard = return []
    go _ PDEmptyPat = return []
    go _ (PDConstantPat _) = return []
    go t (PDPatVar name) = return [(name, t)]
    go t (PDTuplePat pats) = case t of
      TTuple ts | length ts == length pats ->
        concat <$> zipWithM go ts pats
      _ -> do
        freshTypes <- mapM (\_ -> freshVar "letTup") pats
        concat <$> zipWithM go freshTypes pats
    go t (PDConsPat headPat tailPat) = do
      elemTy <- case t of
        TList e -> return e
        _ -> freshVar "letElem"
      headBindings <- go elemTy headPat
      tailBindings <- go t tailPat
      return (headBindings ++ tailBindings)
    go t (PDSnocPat initPat lastPat) = do
      elemTy <- case t of
        TList e -> return e
        _ -> freshVar "letElem"
      initBindings <- go t initPat
      lastBindings <- go elemTy lastPat
      return (initBindings ++ lastBindings)
    go t (PDInductivePat "cons" [headPat, tailPat]) = do
      elemTy <- case t of
        TList e -> return e
        _ -> freshVar "letElem"
      headBindings <- go elemTy headPat
      tailBindings <- go t tailPat
      return (headBindings ++ tailBindings)
    go _ (PDInductivePat _ pats) = do
      -- For other inductive patterns, use fresh types
      freshTypes <- mapM (\_ -> freshVar "letInd") pats
      concat <$> zipWithM go freshTypes pats

-- | Infer top-level expression
inferTopExpr :: TopExpr -> Infer ()
inferTopExpr topExpr = case topExpr of
  Define (VarWithIndices name varIndices) expr -> do
    (t, _) <- inferExpr expr
    -- If variable has indices, wrap the type in a tensor type
    let finalType = if null varIndices
                      then t
                      else addIndicesToType t (map convertVarIndex varIndices)
    env <- getEnv
    let scheme = generalize env finalType
    setEnv $ extendEnv name scheme env

  DefineWithType typedVar expr -> do
    let name = typedVarName typedVar
        paramTypes = map (typeExprToType . snd) (typedVarParams typedVar)
        retType = typeExprToType (typedVarRetType typedVar)
        expectedType = foldr TFun retType paramTypes
    (inferredType, s) <- inferExpr expr
    _ <- unifyTypes (applySubst s inferredType) expectedType
    env <- getEnv
    let scheme = generalize env expectedType
    setEnv $ extendEnv name scheme env

  Test _ -> return ()
  Execute _ -> return ()
  LoadFile _ -> return ()
  Load _ -> return ()
  InfixDecl _ _ -> return ()

-- | Convert TypeExpr (AST) to Type (internal representation)
typeExprToType :: TypeExpr -> Type
typeExprToType TEInt = TInt
typeExprToType TEMathExpr = TInt  -- MathExpr = Integer
typeExprToType TEFloat = TFloat
typeExprToType TEBool = TBool
typeExprToType TEChar = TChar
typeExprToType TEString = TString
typeExprToType (TEVar v) = TVar (TyVar v)
typeExprToType (TEList t) = TList (typeExprToType t)
typeExprToType (TETuple ts) = TTuple (map typeExprToType ts)
typeExprToType (TEFun t1 t2) =
  -- Check if this is a pattern function type: Pattern a -> Pattern b -> Pattern c
  case tryParsePatternFuncType (TEFun t1 t2) of
    Just (argTypes, retType) -> TPatternFunc argTypes retType
    Nothing -> TFun (typeExprToType t1) (typeExprToType t2)
typeExprToType (TEMatcher t) = TMatcher (typeExprToType t)
typeExprToType (TEPattern t) = TPattern (typeExprToType t)
typeExprToType (TETensor t shape indices) =
  TTensor (typeExprToType t) (shapeExprToShape shape) (indexExprsToIndices indices)
typeExprToType (TEApp constr args) =
  -- Handle common type constructors
  case (constr, args) of
    (TEVar "List", [t]) -> TList (typeExprToType t)
    (TEVar "Maybe", [t]) -> TList (typeExprToType t)  -- Approximate Maybe as List
    (TEVar "Hash", [k, v]) -> THash (typeExprToType k) (typeExprToType v)
    (TEVar "IORef", [t]) -> TIORef (typeExprToType t)
    (TEVar "Matcher", [t]) -> TMatcher (typeExprToType t)
    (TEVar "Pattern", [t]) -> TPattern (typeExprToType t)
    _ -> TAny  -- Unsupported type application

-- | Try to parse a function type as a pattern function type
-- Pattern a -> Pattern b -> Pattern c => Just ([a, b], c)
-- Returns Nothing if not a pattern function type
tryParsePatternFuncType :: TypeExpr -> Maybe ([Type], Type)
tryParsePatternFuncType te = go te []
  where
    go :: TypeExpr -> [Type] -> Maybe ([Type], Type)
    -- Base case: return type is a Pattern
    go (TEPattern innerType) accArgs =
      if null accArgs
        then Nothing  -- Just Pattern a, not a function
        else Just (reverse accArgs, typeExprToType innerType)
    -- Recursive case: Pattern a -> rest
    go (TEFun (TEPattern argType) rest) accArgs =
      go rest (typeExprToType argType : accArgs)
    -- Not a pattern function type
    go _ _ = Nothing

-- | Convert TensorShapeExpr to TensorShape
shapeExprToShape :: TensorShapeExpr -> TensorShape
shapeExprToShape (TSLit dims) = ShapeLit dims
shapeExprToShape (TSVar v) = ShapeVar v
shapeExprToShape (TSMixed dims) = ShapeMixed (map shapeDimToType dims)

-- | Convert ShapeDim to ShapeDimType
shapeDimToType :: ShapeDim -> ShapeDimType
shapeDimToType (SDLit n) = DimLit n
shapeDimToType (SDVar v) = DimVar v

-- | Convert TensorIndexExpr list to IndexSpec
indexExprsToIndices :: [TensorIndexExpr] -> IndexSpec
indexExprsToIndices = map convert
  where
    convert (TISub s) = IndexSym TI.Subscript s
    convert (TISup s) = IndexSym TI.Superscript s
    convert TIPlaceholderSub = IndexPlaceholder TI.Subscript
    convert TIPlaceholderSup = IndexPlaceholder TI.Superscript

-- | Convert Type to TypeExpr (for error messages, etc.)
typeToTypeExpr :: Type -> TypeExpr
typeToTypeExpr TInt = TEInt
typeToTypeExpr TFloat = TEFloat
typeToTypeExpr TBool = TEBool
typeToTypeExpr TChar = TEChar
typeToTypeExpr TString = TEString
typeToTypeExpr TUnit = TETuple []
typeToTypeExpr TAny = TEVar "_"
typeToTypeExpr (TVar (TyVar v)) = TEVar v
typeToTypeExpr (TList t) = TEList (typeToTypeExpr t)
typeToTypeExpr (TTuple ts) = TETuple (map typeToTypeExpr ts)
typeToTypeExpr (TFun t1 t2) = TEFun (typeToTypeExpr t1) (typeToTypeExpr t2)
typeToTypeExpr (TMatcher t) = TEMatcher (typeToTypeExpr t)
typeToTypeExpr (TTensor t shape indices) =
  TETensor (typeToTypeExpr t) (shapeToShapeExpr shape) (indicesToIndexExprs indices)
typeToTypeExpr (TCollection t) = TEList (typeToTypeExpr t)
typeToTypeExpr (THash k v) = TEApp (TEVar "Hash") [typeToTypeExpr k, typeToTypeExpr v]
typeToTypeExpr (TPattern t) = TEPattern (typeToTypeExpr t)
typeToTypeExpr (TPatternFunc argTypes retType) =
  -- Convert Pattern a -> Pattern b -> Pattern c to TEFun form
  foldr TEFun (TEPattern (typeToTypeExpr retType)) (map (\t -> TEPattern (typeToTypeExpr t)) argTypes)
typeToTypeExpr (TIORef t) = TEApp (TEVar "IORef") [typeToTypeExpr t]

-- | Convert TensorShape to TensorShapeExpr
shapeToShapeExpr :: TensorShape -> TensorShapeExpr
shapeToShapeExpr (ShapeLit dims) = TSLit dims
shapeToShapeExpr (ShapeVar v) = TSVar v
shapeToShapeExpr (ShapeMixed dims) = TSMixed (map shapeDimTypeToExpr dims)
shapeToShapeExpr ShapeUnknown = TSVar "?"

-- | Convert ShapeDimType to ShapeDim
shapeDimTypeToExpr :: ShapeDimType -> ShapeDim
shapeDimTypeToExpr (DimLit n) = SDLit n
shapeDimTypeToExpr (DimVar v) = SDVar v

-- | Convert IndexSpec to TensorIndexExpr list
indicesToIndexExprs :: IndexSpec -> [TensorIndexExpr]
indicesToIndexExprs = map convert
  where
    convert (IndexSym TI.Subscript s) = TISub s
    convert (IndexSym TI.Superscript s) = TISup s
    convert (IndexPlaceholder TI.Subscript) = TIPlaceholderSub
    convert (IndexPlaceholder TI.Superscript) = TIPlaceholderSup
    convert (IndexVar s) = TISub s  -- Default to subscript

-- | Convert AST IndexExpr to type system Index
convertIndexExpr :: IndexExpr a -> Index
convertIndexExpr (AST.Subscript _)      = IndexPlaceholder TI.Subscript
convertIndexExpr (AST.Superscript _)    = IndexPlaceholder TI.Superscript
convertIndexExpr (AST.SupSubscript _)   = IndexPlaceholder TI.Subscript  -- Treat as subscript
convertIndexExpr (AST.MultiSubscript _ _)   = IndexPlaceholder TI.Subscript
convertIndexExpr (AST.MultiSuperscript _ _) = IndexPlaceholder TI.Superscript
convertIndexExpr (AST.Userscript _)     = IndexPlaceholder TI.Subscript

-- | Convert VarIndex (from variable definitions) to type system Index
convertVarIndex :: VarIndex -> Index
convertVarIndex (VSubscript s)      = IndexSym TI.Subscript s
convertVarIndex (VSuperscript s)    = IndexSym TI.Superscript s
convertVarIndex (VMultiSubscript s _ _)   = IndexSym TI.Subscript s
convertVarIndex (VMultiSuperscript s _ _) = IndexSym TI.Superscript s
convertVarIndex (VGroupScripts _)   = IndexPlaceholder TI.Subscript  -- Simplified
convertVarIndex (VSymmScripts _)    = IndexPlaceholder TI.Subscript
convertVarIndex (VAntiSymmScripts _) = IndexPlaceholder TI.Subscript

-- | Add indices to a type, creating a tensor type if necessary
-- e.g., addIndicesToType (Tensor Integer [2,2] []) [_i, _j] = Tensor Integer [2,2] [_i, _j]
-- e.g., addIndicesToType Integer [_i] = (keep as Integer, indices recorded separately)
addIndicesToType :: Type -> IndexSpec -> Type
addIndicesToType t [] = t
addIndicesToType (TTensor elemTy shape existingIndices) newIndices =
  TTensor elemTy shape (existingIndices ++ newIndices)
addIndicesToType t indices =
  -- For non-tensor types with indices, we keep the indices attached
  -- This is a simplification - ideally we'd track indices separately
  TTensor t ShapeUnknown indices
