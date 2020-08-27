{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE MultiWayIf      #-}

{- |
Module      : Language.Egison.Core
Licence     : MIT

This module provides functions to evaluate various objects.
-}

module Language.Egison.Core
    -- * Egison code evaluation
    ( evalExprShallow
    , evalExprDeep
    , evalWHNF
    -- * Environment
    , recursiveBind
    -- * Pattern matching
    , patternMatch
    ) where

import           Prelude                     hiding (mapM, mappend, mconcat)

import           Control.Arrow
import           Control.Monad.Except        (throwError)
import           Control.Monad.State         hiding (mapM, join)
import           Control.Monad.Trans.Maybe

import           Data.Char                   (isUpper)
import           Data.IORef
import           Data.List                   (partition)
import           Data.Maybe
import qualified Data.Sequence               as Sq
import           Data.Traversable            (mapM)

import qualified Data.HashMap.Lazy           as HL
import qualified Data.Vector                 as V

import           Language.Egison.AST
import           Language.Egison.Data
import           Language.Egison.Data.Collection
import           Language.Egison.Data.Utils
import           Language.Egison.EvalState   (MonadEval(..), mLabelFuncName)
import           Language.Egison.Match
import           Language.Egison.Math
import           Language.Egison.MList
import           Language.Egison.Pretty
import           Language.Egison.RState
import           Language.Egison.Tensor


evalExprShallow :: Env -> Expr -> EvalM WHNFData
evalExprShallow _ (CharExpr c)    = return . Value $ Char c
evalExprShallow _ (StringExpr s)  = return . Value $ toEgison s
evalExprShallow _ (BoolExpr b)    = return . Value $ Bool b
evalExprShallow _ (IntegerExpr x) = return . Value $ toEgison x
evalExprShallow _ (FloatExpr x)   = return . Value $ Float x

evalExprShallow env (QuoteExpr expr) = do
  whnf <- evalExprShallow env expr
  case whnf of
    Value (ScalarData s) -> return . Value . ScalarData $ SingleTerm 1 [(Quote s, 1)]
    _ -> throwError =<< TypeMismatch "scalar in quote" whnf <$> getFuncNameStack

evalExprShallow env (QuoteSymbolExpr expr) = do
  whnf <- evalExprShallow env expr
  case whnf of
    Value (Func (Just name) _ _ _) -> return . Value $ symbolScalarData "" name
    Value (ScalarData _) -> return whnf
    _ -> throwError =<< TypeMismatch "value in quote-function" whnf <$> getFuncNameStack

evalExprShallow env (VarExpr var@(Var _ [])) =
  case refVar env var of
    Nothing | isUpper (head (show var)) ->
      return $ Value (InductiveData (show var) [])
    Nothing  -> return $ Value (symbolScalarData "" $ prettyStr var)
    Just ref -> evalRef ref

evalExprShallow _ (TupleExpr []) = return . Value $ Tuple []
evalExprShallow env (TupleExpr [expr]) = evalExprShallow env expr
evalExprShallow env (TupleExpr exprs) = Intermediate . ITuple <$> mapM (newThunkRef env) exprs

evalExprShallow _ (CollectionExpr []) = return . Value $ Collection Sq.empty

evalExprShallow env (CollectionExpr inners) = do
  inners' <- mapM ((IElement <$>) . newThunkRef env) inners
  innersSeq <- liftIO $ newIORef $ Sq.fromList inners'
  return $ Intermediate $ ICollection innersSeq

evalExprShallow env (ConsExpr x xs) = do
  x' <- newThunkRef env x
  xs' <- newThunkRef env xs
  innersSeq <- liftIO $ newIORef $ Sq.fromList [IElement x', ISubCollection xs']
  return $ Intermediate $ ICollection innersSeq

evalExprShallow env (JoinExpr xs ys) = do
  xs' <- newThunkRef env xs
  ys' <- newThunkRef env ys
  innersSeq <- liftIO $ newIORef $ Sq.fromList [ISubCollection xs', ISubCollection ys']
  return $ Intermediate $ ICollection innersSeq

evalExprShallow env@(Env frame maybe_vwi) (VectorExpr exprs) = do
  let n = toInteger (length exprs)
  whnfs <- zipWithM evalWithIndex exprs [1..]
  case whnfs of
    Intermediate (ITensor Tensor{}):_ ->
      mapM toTensor (zipWith f whnfs [1..]) >>= tConcat' >>= fromTensor
    _ -> fromTensor (Tensor [n] (V.fromList whnfs) [])
  where
    evalWithIndex :: Expr -> Integer -> EvalM WHNFData
    evalWithIndex expr index = evalExprShallow env' expr
      where
        env' = case maybe_vwi of
          Nothing -> env
          Just (VarWithIndices name indices) ->
            Env frame (Just (VarWithIndices name (zipWith changeIndex indices [toEgison index])))
    f (Intermediate (ITensor (Tensor ns xs indices))) i =
      Intermediate (ITensor (Tensor ns xs' indices))
      where
        xs' = V.fromList $ zipWith g (V.toList xs) $ map (\ms -> map toEgison (i:ms)) $ enumTensorIndices ns
    f x _ = x
    g (Value (ScalarData (Div (Plus [Term 1 [(FunctionData fn argnames args js, 1)]]) p))) ms =
      Value (ScalarData (Div (Plus [Term 1 [(FunctionData fn' argnames args js, 1)]]) p))
      where
        fn' = case maybe_vwi of
          Nothing -> fn
          Just (VarWithIndices name indices) ->
            symbolScalarData' $ prettyStr (VarWithIndices name (zipWith changeIndex indices ms))
    g x _ = x

evalExprShallow env (TensorExpr nsExpr xsExpr) = do
  nsWhnf <- evalExprShallow env nsExpr
  ns <- (collectionToRefs nsWhnf >>= fromMList >>= mapM evalRef >>= mapM fromWHNF) :: EvalM [Integer]
  xsWhnf <- evalExprShallow env xsExpr
  xs <- collectionToRefs xsWhnf >>= fromMList >>= mapM evalRef
  if product ns == toInteger (length xs)
    then fromTensor (initTensor ns xs)
    else throwError =<< InconsistentTensorShape <$> getFuncNameStack

evalExprShallow env (HashExpr assocs) = do
  let (keyExprs, exprs) = unzip assocs
  keyWhnfs <- mapM (evalExprShallow env) keyExprs
  keys <- mapM makeHashKey keyWhnfs
  refs <- mapM (newThunkRef env) exprs
  case keys of
    CharKey _ : _ -> do
      let keys' = map (\case CharKey c -> c) keys
      return . Intermediate . ICharHash $ HL.fromList $ zip keys' refs
    StrKey _ : _ -> do
      let keys' = map (\case StrKey s -> s) keys
      return . Intermediate . IStrHash $ HL.fromList $ zip keys' refs
    _ -> do
      let keys' = map (\case IntKey i -> i) keys
      return . Intermediate . IIntHash $ HL.fromList $ zip keys' refs
 where
  makeHashKey :: WHNFData -> EvalM EgisonHashKey
  makeHashKey (Value val) =
    case val of
      ScalarData _ -> IntKey <$> fromEgison val
      Char c       -> return (CharKey c)
      String str   -> return (StrKey str)
      _ -> throwError =<< TypeMismatch "integer or string" (Value val) <$> getFuncNameStack
  makeHashKey whnf = throwError =<< TypeMismatch "integer or string" whnf <$> getFuncNameStack

evalExprShallow env (IndexedExpr override expr indices) = do
  -- Tensor or hash
  tensor <- case expr of
              VarExpr (Var xs is) -> do
                let mObjRef = refVar env (Var xs $ is ++ map (const () <$>) indices)
                case mObjRef of
                  Just objRef -> evalRef objRef
                  Nothing     -> evalExprShallow env expr
              _ -> evalExprShallow env expr
  case tensor of
    Value (ScalarData (SingleTerm 1 [(Symbol id name [], 1)])) -> do
      js2 <- mapM evalIndexToScalar indices
      return $ Value (ScalarData (SingleTerm 1 [(Symbol id name js2, 1)]))
    Value (ScalarData (SingleTerm 1 [(Symbol id name js', 1)])) -> do
      js2 <- mapM evalIndexToScalar indices
      return $ Value (ScalarData (SingleTerm 1 [(Symbol id name (js' ++ js2), 1)]))
    Value (TensorData t@Tensor{}) -> do
      js <- mapM evalIndex indices
      Value <$> refTensorWithOverride override js t
    Intermediate (ITensor t@Tensor{}) -> do
      js <- mapM evalIndex indices
      refTensorWithOverride override js t
    _ -> do
      js <- mapM evalIndex indices
      refHash tensor (map extractIndex js)
 where
  evalIndex :: Index Expr -> EvalM (Index EgisonValue)
  evalIndex index = traverse (evalExprDeep env) index

  evalIndexToScalar :: Index Expr -> EvalM (Index ScalarData)
  evalIndexToScalar index = traverse ((extractScalar =<<) . evalExprDeep env) index

evalExprShallow env (SubrefsExpr override expr jsExpr) = do
  js <- map Subscript <$> (evalExprDeep env jsExpr >>= collectionToList)
  tensor <- case expr of
              VarExpr (Var xs is) -> do
                let mObjRef = refVar env (Var xs $ is ++ map (\_ -> Subscript ()) js)
                case mObjRef of
                  Just objRef -> evalRef objRef
                  Nothing     -> evalExprShallow env expr
              _ -> evalExprShallow env expr
  case tensor of
    Value (ScalarData _)              -> return tensor
    Value (TensorData t@Tensor{})     -> Value <$> refTensorWithOverride override js t
    Intermediate (ITensor t@Tensor{}) -> refTensorWithOverride override js t
    _ -> throwError =<< NotImplemented "subrefs" <$> getFuncNameStack

evalExprShallow env (SuprefsExpr override expr jsExpr) = do
  js <- map Superscript <$> (evalExprDeep env jsExpr >>= collectionToList)
  tensor <- case expr of
              VarExpr (Var xs is) -> do
                let mObjRef = refVar env (Var xs $ is ++ map (\_ -> Superscript ()) js)
                case mObjRef of
                  Just objRef -> evalRef objRef
                  Nothing     -> evalExprShallow env expr
              _ -> evalExprShallow env expr
  case tensor of
    Value (ScalarData _)              -> return tensor
    Value (TensorData t@Tensor{})     -> Value <$> refTensorWithOverride override js t
    Intermediate (ITensor t@Tensor{}) -> refTensorWithOverride override js t
    _ -> throwError =<< NotImplemented "suprefs" <$> getFuncNameStack

evalExprShallow env (UserrefsExpr _ expr jsExpr) = do
  val <- evalExprDeep env expr
  js <- map Userscript <$> (evalExprDeep env jsExpr >>= collectionToList >>= mapM extractScalar)
  case val of
    ScalarData (SingleTerm 1 [(Symbol id name is, 1)]) ->
      return $ Value (ScalarData (SingleTerm 1 [(Symbol id name (is ++ js), 1)]))
    ScalarData (SingleTerm 1 [(FunctionData name argnames args is, 1)]) ->
      return $ Value (ScalarData (SingleTerm 1 [(FunctionData name argnames args (is ++ js), 1)]))
    _ -> throwError =<< NotImplemented "user-refs" <$> getFuncNameStack

evalExprShallow env (LambdaExpr fnname names expr) = do
  names' <- mapM (\case
                     TensorArg name' -> return name'
                     ScalarArg _ -> throwError =<< EgisonBug "scalar-arg remained" <$> getFuncNameStack) names
  return . Value $ Func fnname env names' expr

evalExprShallow env (MemoizedLambdaExpr names body) = do
  hashRef <- liftIO $ newIORef HL.empty
  return . Value $ MemoizedFunc hashRef env names body

evalExprShallow env (CambdaExpr name expr) = return . Value $ CFunc env name expr

evalExprShallow env (PatternFunctionExpr names pattern) = return . Value $ PatternFunc env names pattern

evalExprShallow (Env _ Nothing) (FunctionExpr _) = throwError $ Default "function symbol is not bound to a variable"

evalExprShallow env@(Env _ (Just name)) (FunctionExpr args) = do
  args' <- mapM (evalExprDeep env) args >>= mapM extractScalar
  return . Value $ ScalarData (SingleTerm 1 [(FunctionData (symbolScalarData' (prettyStr name)) (map (symbolScalarData' . prettyStr') args) args' [], 1)])

evalExprShallow env (IfExpr test expr expr') = do
  test <- evalExprShallow env test >>= fromWHNF
  evalExprShallow env $ if test then expr else expr'

evalExprShallow env (LetExpr bindings expr) = do
  binding <- concat <$> mapM extractBindings bindings
  evalExprShallow (extendEnv env binding) expr
 where
  extractBindings :: BindingExpr -> EvalM [Binding]
  extractBindings (PDPatVar name, expr@FunctionExpr{}) =
    let Env frame _ = env
     in makeBindings [name] . (:[]) <$> newThunkRef (Env frame (Just $ varToVarWithIndices name)) expr
  extractBindings (pdp, expr) = do
    thunk <- newThunkRef env expr
    bindPrimitiveDataPattern pdp thunk

evalExprShallow env (LetRecExpr bindings expr) = do
  env' <- recursiveBind env bindings
  evalExprShallow env' expr

evalExprShallow env (TransposeExpr vars expr) = do
  syms <- evalExprDeep env vars >>= collectionToList
  whnf <- evalExprShallow env expr
  case whnf of
    Intermediate (ITensor t) -> Intermediate . ITensor <$> tTranspose' syms t
    Value (TensorData t)     -> Value . TensorData <$> tTranspose' syms t
    _                        -> return whnf

evalExprShallow env (FlipIndicesExpr expr) = do
  whnf <- evalExprShallow env expr
  case whnf of
    Intermediate (ITensor t) -> Intermediate . ITensor <$> tFlipIndices t
    Value (TensorData t)     -> Value . TensorData <$> tFlipIndices t
    _                        -> return whnf

evalExprShallow env (WithSymbolsExpr vars expr) = do
  symId <- fresh
  syms <- mapM (newEvaluatedObjectRef . Value . symbolScalarData symId) vars
  whnf <- evalExprShallow (extendEnv env (makeBindings' vars syms)) expr
  case whnf of
    Value (TensorData t@Tensor{}) ->
      Value . TensorData <$> removeTmpScripts symId t
    Intermediate (ITensor t@Tensor{}) ->
      Intermediate . ITensor <$> removeTmpScripts symId t
    _ -> return whnf
 where
  isTmpSymbol :: String -> Index EgisonValue -> Bool
  isTmpSymbol symId index = symId == getSymId (extractIndex index)

  removeTmpScripts :: HasTensor a => String -> Tensor a -> EvalM (Tensor a)
  removeTmpScripts symId (Tensor s xs is) = do
    let (ds, js) = partition (isTmpSymbol symId) is
    Tensor s ys _ <- tTranspose (js ++ ds) (Tensor s xs is)
    return (Tensor s ys js)


evalExprShallow env (DoExpr bindings expr) = return $ Value $ IOFunc $ do
  let body = foldr genLet (ApplyExpr expr $ TupleExpr [stringToVarExpr "#1"]) bindings
  applyFunc env (Value $ Func Nothing env ["#1"] body) $ Value World
 where
  genLet (names, expr) expr' =
    LetExpr [(PDTuplePat (map (PDPatVar . stringToVar) ["#1", "#2"]), ApplyExpr expr $ TupleExpr [stringToVarExpr "#1"])] $
    LetExpr [(names, stringToVarExpr "#2")] expr'

evalExprShallow env (IoExpr expr) = do
  io <- evalExprShallow env expr
  case io of
    Value (IOFunc m) -> do
      val <- m >>= evalWHNF
      case val of
        Tuple [_, val'] -> return $ Value val'
    _ -> throwError =<< TypeMismatch "io" io <$> getFuncNameStack

evalExprShallow env (MatchAllExpr pmmode target matcher clauses) = do
  target <- evalExprShallow env target
  matcher <- evalExprShallow env matcher >>= evalMatcherWHNF
  f matcher target >>= fromMList
 where
  fromMList :: MList EvalM WHNFData -> EvalM WHNFData
  fromMList MNil = return . Value $ Collection Sq.empty
  fromMList (MCons val m) = do
    head <- IElement <$> newEvaluatedObjectRef val
    tail <- ISubCollection <$> (liftIO . newIORef . Thunk $ m >>= fromMList)
    seqRef <- liftIO . newIORef $ Sq.fromList [head, tail]
    return . Intermediate $ ICollection seqRef
  f matcher target = do
      let tryMatchClause (pattern, expr) results = do
            result <- patternMatch pmmode env pattern target matcher
            mmap (flip evalExprShallow expr . extendEnv env) result >>= (`mappend` results)
      mfoldr tryMatchClause (return MNil) (fromList clauses)

evalExprShallow env (MatchExpr pmmode target matcher clauses) = do
  target <- evalExprShallow env target
  matcher <- evalExprShallow env matcher >>= evalMatcherWHNF
  f matcher target
 where
  f matcher target = do
      let tryMatchClause (pattern, expr) cont = do
            result <- patternMatch pmmode env pattern target matcher
            case result of
              MCons bindings _ -> evalExprShallow (extendEnv env bindings) expr
              MNil             -> cont
      callstack <- getFuncNameStack
      foldr tryMatchClause (throwError $ MatchFailure callstack) clauses

evalExprShallow env (SeqExpr expr1 expr2) = do
  _ <- evalExprDeep env expr1
  evalExprShallow env expr2

evalExprShallow env (CApplyExpr func arg) = do
  func <- evalExprShallow env func
  args <- evalExprDeep env arg >>= collectionToList
  case func of
    Value (MemoizedFunc hashRef env names body) -> do
      indices <- mapM fromEgison args
      hash <- liftIO $ readIORef hashRef
      case HL.lookup indices hash of
        Just whnf -> return whnf
        Nothing -> do
          whnf <- applyFunc env (Value (Func Nothing env names body)) (Value (makeTuple args))
          liftIO $ modifyIORef hashRef (HL.insert indices whnf)
          return whnf
    _ -> applyFunc env func (Value (makeTuple args))

evalExprShallow env (ApplyExpr func arg) = do
  func <- evalExprShallow env func >>= appendDFscripts 0
  case func of
    Value (InductiveData name []) ->
      case arg of
        TupleExpr exprs ->
          Intermediate . IInductiveData name <$> mapM (newThunkRef env) exprs
        _ -> throwError $ Default "argument is not a tuple"
    Value (TensorData t@Tensor{}) -> do
      arg <- evalExprShallow env arg
      Value <$> (tMap (\f -> applyFunc env (Value f) arg >>= evalWHNF) t >>= fromTensor) >>= removeDFscripts
    Intermediate (ITensor t@Tensor{}) -> do
      arg <- evalExprShallow env arg
      tMap (\f -> applyFunc env f arg) t >>= fromTensor
    Value (MemoizedFunc hashRef env' names body) -> do
      arg <- evalExprShallow env arg
      indices' <- evalWHNF arg
      indices <- mapM fromEgison $ tupleToList indices'
      hash <- liftIO $ readIORef hashRef
      case HL.lookup indices hash of
        Just whnf -> return whnf
        Nothing -> do
          whnf <- applyFunc env' (Value (Func Nothing env' names body)) arg
          liftIO $ modifyIORef hashRef (HL.insert indices whnf)
          return whnf
    _ -> do
      arg <- evalExprShallow env arg
      applyFunc env func arg >>= removeDFscripts

evalExprShallow env (WedgeApplyExpr func arg) = do
  func <- evalExprShallow env func >>= appendDFscripts 0
  arg <- evalExprShallow env arg >>= tupleToListWHNF
  arg <- zipWithM appendDFscripts [1..] arg >>= makeITuple
  case func of
    Value (TensorData t@Tensor{}) ->
      Value <$> (tMap (\f -> applyFunc env (Value f) arg >>= evalWHNF) t >>= fromTensor)
    Intermediate (ITensor t@Tensor{}) ->
      tMap (\f -> applyFunc env f arg) t >>= fromTensor
    Value (MemoizedFunc hashRef env names body) -> do
      indices <- evalWHNF arg
      indices <- mapM fromEgison $ tupleToList indices
      hash <- liftIO $ readIORef hashRef
      case HL.lookup indices hash of
        Just whnf -> return whnf
        Nothing -> do
          whnf <- applyFunc env (Value (Func Nothing env names body)) arg
          liftIO $ modifyIORef hashRef (HL.insert indices whnf)
          return whnf
    _ -> applyFunc env func arg >>= removeDFscripts

evalExprShallow env (MatcherExpr info) = return $ Value $ UserMatcher env info

evalExprShallow env (GenerateTensorExpr fnExpr shapeExpr) = do
  shape <- evalExprDeep env shapeExpr >>= collectionToList
  ns    <- mapM fromEgison shape :: EvalM Shape
  xs    <- mapM (indexToWHNF env . map toEgison) (enumTensorIndices ns)
  fromTensor (Tensor ns (V.fromList xs) [])
 where
  indexToWHNF :: Env -> [EgisonValue] {- index -} -> EvalM WHNFData
  indexToWHNF (Env frame maybe_vwi) ms = do
    let env' = maybe env (\(VarWithIndices name indices) -> Env frame $ Just $ VarWithIndices name $ zipWith changeIndex indices ms) maybe_vwi
    fn <- evalExprShallow env' fnExpr
    applyFunc env fn $ Value $ makeTuple ms

evalExprShallow env (TensorContractExpr tExpr) = do
  whnf <- evalExprShallow env tExpr
  case whnf of
    Intermediate (ITensor t@Tensor{}) -> do
      ts <- tContract t
      makeICollection (map tensorToWHNF ts)
    Value (TensorData t@Tensor{}) -> do
      ts <- tContract t
      return $ Value $ Collection $ Sq.fromList $ map tensorToValue ts
    _ -> makeICollection [whnf]

evalExprShallow env (TensorMapExpr fnExpr tExpr) = do
  fn <- evalExprShallow env fnExpr
  whnf <- evalExprShallow env tExpr
  case whnf of
    Intermediate (ITensor t) ->
      tMap (applyFunc env fn) t >>= fromTensor
    Value (TensorData t) ->
      Value <$> (tMap (applyFunc' env fn) t >>= fromTensor)
    _ -> applyFunc env fn whnf

evalExprShallow env (TensorMap2Expr fnExpr t1Expr t2Expr) = do
  fn <- evalExprShallow env fnExpr
  whnf1 <- evalExprShallow env t1Expr
  whnf2 <- evalExprShallow env t2Expr
  case (whnf1, whnf2) of
    -- both of arguments are tensors
    (Intermediate (ITensor t1), Intermediate (ITensor t2)) ->
      tMap2 (applyFunc'' env fn) t1 t2 >>= fromTensor
    (Intermediate (ITensor t), Value (TensorData (Tensor ns xs js))) -> do
      let xs' = V.map Value xs
      tMap2 (applyFunc'' env fn) t (Tensor ns xs' js) >>= fromTensor
    (Value (TensorData (Tensor ns xs js)), Intermediate (ITensor t)) -> do
      let xs' = V.map Value xs
      tMap2 (applyFunc'' env fn) (Tensor ns xs' js) t >>= fromTensor
    (Value (TensorData t1), Value (TensorData t2)) ->
      Value <$> (tMap2 (\x y -> applyFunc' env fn (Tuple [x, y])) t1 t2 >>= fromTensor)
    -- an argument is scalar
    (Intermediate (ITensor (Tensor ns xs js)), whnf) -> do
      ys <- V.mapM (\x -> applyFunc'' env fn x whnf) xs
      return $ Intermediate (ITensor (Tensor ns ys js))
    (whnf, Intermediate (ITensor (Tensor ns xs js))) -> do
      ys <- V.mapM (applyFunc'' env fn whnf) xs
      return $ Intermediate (ITensor (Tensor ns ys js))
    (Value (TensorData (Tensor ns xs js)), whnf) -> do
      ys <- V.mapM (\x -> applyFunc'' env fn (Value x) whnf) xs
      return $ Intermediate (ITensor (Tensor ns ys js))
    (whnf, Value (TensorData (Tensor ns xs js))) -> do
      ys <- V.mapM (applyFunc'' env fn whnf . Value) xs
      return $ Intermediate (ITensor (Tensor ns ys js))
    _ -> applyFunc'' env fn whnf1 whnf2
 where
  applyFunc'' :: Env -> WHNFData -> WHNFData -> WHNFData -> EvalM WHNFData
  applyFunc'' env fn x y = do
    xRef <- newEvaluatedObjectRef x
    yRef <- newEvaluatedObjectRef y
    applyFunc env fn (Intermediate (ITuple [xRef, yRef]))

evalExprShallow _ SomethingExpr = return $ Value Something
evalExprShallow _ UndefinedExpr = return $ Value Undefined
evalExprShallow _ expr = throwError =<< NotImplemented ("evalExprShallow for " ++ show expr) <$> getFuncNameStack

evalExprDeep :: Env -> Expr -> EvalM EgisonValue
evalExprDeep env expr = evalExprShallow env expr >>= evalWHNF

evalRefDeep :: ObjectRef -> EvalM EgisonValue
evalRefDeep ref = do
  obj <- liftIO $ readIORef ref
  case obj of
    WHNF (Value val) -> return val
    WHNF val -> do
      val <- evalWHNF val
      writeObjectRef ref $ Value val
      return val
    Thunk thunk -> do
      val <- thunk >>= evalWHNF
      writeObjectRef ref $ Value val
      return val

evalWHNF :: WHNFData -> EvalM EgisonValue
evalWHNF (Value val) = return val
evalWHNF (Intermediate (IInductiveData name refs)) =
  InductiveData name <$> mapM evalRefDeep refs
evalWHNF (Intermediate (IIntHash refs)) = do
  refs' <- mapM evalRefDeep refs
  return $ IntHash refs'
evalWHNF (Intermediate (ICharHash refs)) = do
  refs' <- mapM evalRefDeep refs
  return $ CharHash refs'
evalWHNF (Intermediate (IStrHash refs)) = do
  refs' <- mapM evalRefDeep refs
  return $ StrHash refs'
evalWHNF (Intermediate (ITuple [ref])) = evalRefDeep ref
evalWHNF (Intermediate (ITuple refs)) = Tuple <$> mapM evalRefDeep refs
evalWHNF (Intermediate (ITensor (Tensor ns whnfs js))) = do
  vals <- mapM evalWHNF (V.toList whnfs)
  return $ TensorData $ Tensor ns (V.fromList vals) js
evalWHNF coll = Collection <$> (collectionToRefs coll >>= fromMList >>= mapM evalRefDeep . Sq.fromList)

addscript :: (Index EgisonValue, Tensor a) -> Tensor a
addscript (subj, Tensor s t i) = Tensor s t (i ++ [subj])

valuetoTensor2 :: WHNFData -> Tensor WHNFData
valuetoTensor2 (Intermediate (ITensor t)) = t

applyFunc :: Env -> WHNFData -> WHNFData -> EvalM WHNFData
applyFunc env (Value (TensorData (Tensor s1 t1 i1))) tds = do
  tds <- tupleToListWHNF tds
  if length s1 > length i1 && all (\(Intermediate (ITensor (Tensor s _ i))) -> length s - length i == 1) tds
    then do
      symId <- fresh
      let argnum = length tds
          subjs = map (Subscript . symbolScalarData symId . show) [1 .. argnum]
          supjs = map (Superscript . symbolScalarData symId . show) [1 .. argnum]
      dot <- evalExprShallow env (stringToVarExpr ".")
      makeITuple (Value (TensorData (Tensor s1 t1 (i1 ++ supjs))):map (Intermediate . ITensor . addscript) (zip subjs $ map valuetoTensor2 tds)) >>= applyFunc env dot
    else throwError $ Default "applyfunc"

applyFunc env (Intermediate (ITensor (Tensor s1 t1 i1))) tds = do
  tds <- tupleToListWHNF tds
  if length s1 > length i1 && all (\(Intermediate (ITensor (Tensor s _ i))) -> length s - length i == 1) tds
    then do
      symId <- fresh
      let argnum = length tds
          subjs = map (Subscript . symbolScalarData symId . show) [1 .. argnum]
          supjs = map (Superscript . symbolScalarData symId . show) [1 .. argnum]
      dot <- evalExprShallow env (stringToVarExpr ".")
      makeITuple (map Intermediate (ITensor (Tensor s1 t1 (i1 ++ supjs)):map (ITensor . addscript) (zip subjs $ map valuetoTensor2 tds))) >>= applyFunc env dot
    else throwError $ Default "applyfunc"

applyFunc _ (Value (Func mFuncName env [name] body)) arg =
  mLabelFuncName mFuncName $ do
    ref <- newEvaluatedObjectRef arg
    evalExprShallow (extendEnv env $ makeBindings' [name] [ref]) body
applyFunc _ (Value (Func mFuncName env names body)) arg =
  mLabelFuncName mFuncName $ do
    refs <- tupleToRefs arg
    if | length names == length refs ->
         evalExprShallow (extendEnv env (makeBindings' names refs)) body
       | length names > length refs -> -- Currying
         let (bound, rest) = splitAt (length refs) names
          in return . Value $ Func mFuncName (extendEnv env (makeBindings' bound refs)) rest body
       | otherwise ->
           throwError =<< ArgumentsNumWithNames names (length names) (length refs) <$> getFuncNameStack
applyFunc _ (Value (CFunc env name body)) arg = do
  refs <- tupleToRefs arg
  seqRef <- liftIO . newIORef $ Sq.fromList (map IElement refs)
  col <- liftIO . newIORef $ WHNF $ Intermediate $ ICollection seqRef
  if not (null refs)
    then evalExprShallow (extendEnv env $ makeBindings' [name] [col]) body
    else throwError =<< ArgumentsNumWithNames [name] 1 0 <$> getFuncNameStack
applyFunc _ (Value (PrimitiveFunc func)) arg = func arg
applyFunc _ (Value (IOFunc m)) arg =
  case arg of
     Value World -> m
     _           -> throwError =<< TypeMismatch "world" arg <$> getFuncNameStack
applyFunc _ (Value (ScalarData fn@(SingleTerm 1 [(Symbol{}, 1)]))) arg = do
  args <- tupleToList <$> evalWHNF arg
  mExprs <- mapM (\arg -> case arg of
                            ScalarData _ -> extractScalar arg
                            _ -> throwError =<< EgisonBug "to use undefined functions, you have to use ScalarData args" <$> getFuncNameStack) args
  return (Value (ScalarData (SingleTerm 1 [(Apply fn mExprs, 1)])))
applyFunc _ whnf _ = throwError =<< TypeMismatch "function" whnf <$> getFuncNameStack

applyFunc' :: Env -> WHNFData -> EgisonValue -> EvalM EgisonValue
applyFunc' env fn x = applyFunc env fn (Value x) >>= evalWHNF

refHash :: WHNFData -> [EgisonValue] -> EvalM WHNFData
refHash val [] = return val
refHash val (index:indices) =
  case val of
    Value (IntHash hash)  -> refHash' hash
    Value (CharHash hash) -> refHash' hash
    Value (StrHash hash)  -> refHash' hash
    Intermediate (IIntHash hash)  -> irefHash hash
    Intermediate (ICharHash hash) -> irefHash hash
    Intermediate (IStrHash hash)  -> irefHash hash
    _ -> throwError =<< TypeMismatch "hash" val <$> getFuncNameStack
 where
  refHash' hash = do
    key <- fromEgison index
    case HL.lookup key hash of
      Just val -> refHash (Value val) indices
      Nothing  -> return $ Value Undefined

  irefHash hash = do
    key <- fromEgison index
    case HL.lookup key hash of
      Just ref -> evalRef ref >>= flip refHash indices
      Nothing  -> return $ Value Undefined

updateHash :: [Integer] -> WHNFData -> WHNFData -> EvalM WHNFData
updateHash [index] tgt (Intermediate (IIntHash hash)) = do
  targetRef <- newEvaluatedObjectRef tgt
  return . Intermediate . IIntHash $ HL.insert index targetRef hash
updateHash (index:indices) tgt (Intermediate (IIntHash hash)) = do
  val <- maybe (return $ Intermediate $ IIntHash HL.empty) evalRef $ HL.lookup index hash
  ref <- updateHash indices tgt val >>= newEvaluatedObjectRef
  return . Intermediate . IIntHash $ HL.insert index ref hash
updateHash indices tgt (Value (IntHash hash)) = do
  let keys = HL.keys hash
  vals <- mapM (newEvaluatedObjectRef . Value) $ HL.elems hash
  updateHash indices tgt (Intermediate $ IIntHash $ HL.fromList $ zip keys vals)
updateHash _ _ v = throwError $ Default $ "expected hash value: " ++ show v

subst :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
subst k nv ((k', v'):xs) | k == k'   = (k', nv):subst k nv xs
                         | otherwise = (k', v'):subst k nv xs
subst _ _ [] = []

newThunk :: Env -> Expr -> Object
newThunk env expr = Thunk $ evalExprShallow env expr

newThunkRef :: Env -> Expr -> EvalM ObjectRef
newThunkRef env expr = liftIO . newIORef $ newThunk env expr

recursiveBind :: Env -> [BindingExpr] -> EvalM Env
recursiveBind env bindings = do
  -- List of variables defined in |bindings|
  let names = concatMap (\(pd, _) -> collectNames pd) bindings
  -- Create dummy bindings for |names| first. Since this is a reference,
  -- it can be overwritten later.
  binds <- mapM (\name -> (name, ) <$> newThunkRef nullEnv UndefinedExpr) names
  let env'@(Env frame _) = extendEnv env binds
  forM_ bindings $ \(pd, expr) -> do
    -- Modify |env'| for some cases
    let env'' =
          case (pd, expr) of
            (PDPatVar var, FunctionExpr{}) -> Env frame (Just (varToVarWithIndices var))
            (PDPatVar var@(Var _ is), _) | not (null is) -> Env frame (Just (varToVarWithIndices var))
            _ -> env'
    thunk <- newThunkRef env'' expr
    binds <- bindPrimitiveDataPattern pd thunk
    forM_ binds $ \(var, objref) -> do
      -- |obj| is an Object being bound to |var|.
      obj <- liftIO $ readIORef objref
      let ref = fromJust (refVar env' var)
      liftIO $ writeIORef ref obj
  return env'
 where
  collectNames :: PrimitiveDataPattern -> [Var]
  collectNames (PDPatVar var) = [var]
  collectNames (PDInductivePat _ ps) = concatMap collectNames ps
  collectNames (PDTuplePat ps) = concatMap collectNames ps
  collectNames (PDConsPat p1 p2) = collectNames p1 ++ collectNames p2
  collectNames (PDSnocPat p1 p2) = collectNames p1 ++ collectNames p2
  collectNames _ = []

--
-- Pattern Match
--

patternMatch :: PMMode -> Env -> Pattern -> WHNFData -> Matcher -> EvalM (MList EvalM Match)
patternMatch pmmode env pattern target matcher =
  case pmmode of
    DFSMode -> processMStatesAllDFS (msingleton initMState)
    BFSMode -> processMStatesAll [msingleton initMState]
  where
    initMState = MState { mStateEnv      = env
                        , loopPatCtx     = []
                        , seqPatCtx      = []
                        , mStateBindings = []
                        , mTrees         = [MAtom pattern target matcher]
                        }

processMStatesAllDFS :: MList EvalM MatchingState -> EvalM (MList EvalM Match)
processMStatesAllDFS MNil = return MNil
processMStatesAllDFS (MCons (MState _ _ [] bindings []) ms) = MCons bindings . processMStatesAllDFS <$> ms
processMStatesAllDFS (MCons mstate ms) = processMState mstate >>= (`mappend` ms) >>= processMStatesAllDFS

processMStatesAllDFSForall :: MList EvalM MatchingState -> EvalM (MList EvalM MatchingState)
processMStatesAllDFSForall MNil = return MNil
processMStatesAllDFSForall (MCons mstate@(MState _ _ (ForallPatContext _ _ : _) _ []) ms) = MCons mstate . processMStatesAllDFSForall <$> ms
processMStatesAllDFSForall (MCons mstate ms) = processMState mstate >>= (`mappend` ms) >>= processMStatesAllDFSForall

processMStatesAll :: [MList EvalM MatchingState] -> EvalM (MList EvalM Match)
processMStatesAll [] = return MNil
processMStatesAll streams = do
  (matches, streams') <- mapM processMStates streams >>= extractMatches . concat
  mappend (fromList matches) $ processMStatesAll streams'

processMStates :: MList EvalM MatchingState -> EvalM [MList EvalM MatchingState]
processMStates MNil = return []
processMStates (MCons state stream) = (\x y -> [x, y]) <$> processMState state <*> stream

extractMatches :: [MList EvalM MatchingState] -> EvalM ([Match], [MList EvalM MatchingState])
extractMatches = extractMatches' ([], [])
 where
  extractMatches' :: ([Match], [MList EvalM MatchingState]) -> [MList EvalM MatchingState] -> EvalM ([Match], [MList EvalM MatchingState])
  extractMatches' (xs, ys) [] = return (xs, ys)
  extractMatches' (xs, ys) (MCons (gatherBindings -> Just bindings) states : rest) = do
    states' <- states
    extractMatches' (xs ++ [bindings], ys ++ [states']) rest
  extractMatches' (xs, ys) (stream:rest) = extractMatches' (xs, ys ++ [stream]) rest

gatherBindings :: MatchingState -> Maybe [Binding]
gatherBindings MState{ seqPatCtx = [], mStateBindings = b, mTrees = [] } = return b
gatherBindings _ = Nothing

processMState :: MatchingState -> EvalM (MList EvalM MatchingState)
processMState state | nullMState state = processMState' state
processMState state =
  case splitMState state of
    (1, state1, state2) -> do
      result <- processMStatesAllDFS (msingleton state1)
      case result of
        MNil -> return $ msingleton state2
        _    -> return MNil
    (0, MState e l s b [MAtom (ForallPat p1 p2) m t], MState{ mTrees = trees }) -> do
      states <- processMStatesAllDFSForall (msingleton (MState e l (ForallPatContext [] []:s) b [MAtom p1 m t]))
      statess' <- mmap (\(MState e' l' (ForallPatContext ms ts:s') b' []) -> do
                            let mat' = makeTuple ms
                            tgt' <- makeITuple ts
                            processMStatesAllDFSForall (msingleton (MState e' l' (ForallPatContext [] []:s') b' [MAtom p2 tgt' mat']))) states
      b <- mAny (\case
                   MNil -> return True
                   _ -> return False) statess'
      if b
        then return MNil
--        else return MNil
        else do nstatess <- mmap (\states' -> mmap (\(MState e' l' (ForallPatContext [] []:s') b' []) -> return $ MState e' l' s' b' trees) states') statess'
                mconcat nstatess
    _ -> processMState' state
 where
  splitMState :: MatchingState -> (Integer, MatchingState, MatchingState)
  splitMState mstate@MState{ mTrees = MAtom (NotPat pattern) target matcher : trees } =
    (1, mstate { seqPatCtx = [],  mTrees = [MAtom pattern target matcher] }, mstate { mTrees = trees })
  splitMState mstate@MState{ mTrees = MAtom pattern target matcher : trees } =
    (0, mstate { mTrees = [MAtom pattern target matcher] }, mstate { mTrees = trees })
  splitMState mstate@MState{ mTrees = MNode penv state' : trees } =
    (f, mstate { mTrees = [MNode penv state1] }, mstate { mTrees = MNode penv state2 : trees })
      where (f, state1, state2) = splitMState state'

processMState' :: MatchingState -> EvalM (MList EvalM MatchingState)
--processMState' MState{ seqPatCtx = [], mTrees = [] } = throwError =<< EgisonBug "should not reach here (empty matching-state)" <$> getFuncNameStack
processMState' mstate@MState{ seqPatCtx = [], mTrees = [] } = return . msingleton $ mstate -- for forall pattern used in matchAll (not matchAllDFS)

-- Sequential patterns and forall pattern
processMState' mstate@MState{ seqPatCtx = SeqPatContext stack SeqNilPat [] []:seqs, mTrees = [] } =
  return . msingleton $ mstate { seqPatCtx = seqs, mTrees = stack }
processMState' mstate@MState{ seqPatCtx = SeqPatContext stack seqPat mats tgts:seqs, mTrees = [] } = do
  let mat' = makeTuple mats
  tgt' <- makeITuple tgts
  return . msingleton $ mstate { seqPatCtx = seqs, mTrees = MAtom seqPat tgt' mat' : stack }
processMState' mstate@MState{ seqPatCtx = ForallPatContext _ _:_, mTrees = [] } =
  return . msingleton $ mstate

-- Matching Nodes
--processMState' MState{ mTrees = MNode _ MState{ mStateBindings = [], mTrees = [] }:_ } = throwError =<< EgisonBug "should not reach here (empty matching-node)" <$> getFuncNameStack
processMState' mstate@MState{ mTrees = MNode _ MState{ seqPatCtx = [], mTrees = [] }:trees } = return . msingleton $ mstate { mTrees = trees }

processMState' ms1@MState{ mTrees = MNode penv ms2@MState{ mTrees = MAtom (VarPat name) target matcher:trees' }:trees } =
  case lookup name penv of
    Just pattern ->
      case trees' of
        [] -> return . msingleton $ ms1 { mTrees = MAtom pattern target matcher:trees }
        _  -> return . msingleton $ ms1 { mTrees = MAtom pattern target matcher:MNode penv (ms2 { mTrees = trees' }):trees }
    Nothing -> throwError =<< UnboundVariable name <$> getFuncNameStack

processMState' ms1@(MState _ _ _ bindings (MNode penv ms2@(MState env' loops' _ _ (MAtom (IndexedPat (VarPat name) indices) target matcher:trees')):trees)) =
  case lookup name penv of
    Just pattern -> do
      let env'' = extendEnvForNonLinearPatterns env' bindings loops'
      indices <- mapM (evalExprShallow env'' >=> fmap fromInteger . fromWHNF) indices
      let pattern' = IndexedPat pattern $ map IntegerExpr indices
      case trees' of
        [] -> return . msingleton $ ms1 { mTrees = MAtom pattern' target matcher:trees }
        _  -> return . msingleton $ ms1 { mTrees = MAtom pattern' target matcher:MNode penv (ms2 { mTrees = trees' }):trees }
    Nothing -> throwError =<< UnboundVariable name <$> getFuncNameStack

processMState' mstate@MState{ mTrees = MNode penv state:trees } =
  processMState' state >>= mmap (\state' -> case state' of
--egi                                              MState { mTrees = [] } -> return $ mstate { mTrees = trees }
                                              _ -> return $ mstate { mTrees = MNode penv state':trees })

-- Matching Atoms
processMState' mstate@(MState env loops seqs bindings (MAtom pattern target matcher:trees)) =
  let env' = extendEnvForNonLinearPatterns env bindings loops in
  case pattern of
    InductiveOrPApplyPat name args ->
      case refVar env (stringToVar name) of
        Nothing -> processMState' (mstate { mTrees = MAtom (InductivePat name args) target matcher:trees })
        Just ref -> do
          whnf <- evalRef ref
          case whnf of
            Value PatternFunc{} ->
              processMState' (mstate { mTrees = MAtom (PApplyPat (VarExpr (stringToVar name)) args) target matcher:trees })
            _                   ->
              processMState' (mstate { mTrees = MAtom (InductivePat name args) target matcher:trees })

    NotPat _ -> throwError =<< EgisonBug "should not reach here (not-pattern)" <$> getFuncNameStack
    VarPat _ -> throwError $ Default $ "cannot use variable except in pattern function:" ++ prettyStr pattern

    LetPat bindings' pattern' -> do
      b <- concat <$> mapM extractBindings bindings'
      return . msingleton $ mstate { mStateBindings = b ++ bindings, mTrees = MAtom pattern' target matcher:trees }
        where
          extractBindings (pdp, expr) = do
            thunk <- newThunkRef (extendEnv env bindings) expr
            bindPrimitiveDataPattern pdp thunk

    PredPat predicate -> do
      func <- evalExprShallow env' predicate
      result <- applyFunc env func target >>= fromWHNF
      if result then return . msingleton $ mstate { mTrees = trees }
                else return MNil

    PApplyPat func args -> do
      func' <- evalExprShallow env' func
      case func' of
        Value (PatternFunc env'' names expr) ->
          return . msingleton $ mstate { mTrees = MNode penv (MState env'' [] [] [] [MAtom expr target matcher]) : trees }
            where penv = zip names args
        _ -> throwError =<< TypeMismatch "pattern constructor" func' <$> getFuncNameStack

    DApplyPat func args ->
      return . msingleton $ mstate { mTrees = MAtom (InductivePat "apply" [func, toListPat args]) target matcher:trees }

    LoopPat name (LoopRange start ends endPat) pat pat' -> do
      startNum    <- evalExprShallow env' start >>= fromWHNF :: (EvalM Integer)
      startNumRef <- newEvaluatedObjectRef $ Value $ toEgison (startNum - 1)
      ends'       <- evalExprShallow env' ends
      case ends' of
        Value (ScalarData _) -> do -- the case when the end numbers are an integer
          endsRef  <- newEvaluatedObjectRef ends'
          inners   <- liftIO . newIORef $ Sq.fromList [IElement endsRef]
          endsRef' <- liftIO $ newIORef (WHNF (Intermediate (ICollection inners)))
          return . msingleton $ mstate { loopPatCtx = LoopPatContext (name, startNumRef) endsRef' endPat pat pat':loops
                                       , mTrees = MAtom ContPat target matcher:trees }
        _ -> do -- the case when the end numbers are a collection
          endsRef <- newEvaluatedObjectRef ends'
          return . msingleton $ mstate { loopPatCtx = LoopPatContext (name, startNumRef) endsRef endPat pat pat':loops
                                       , mTrees = MAtom ContPat target matcher:trees }
    ContPat ->
      case loops of
        [] -> throwError $ Default "cannot use cont pattern except in loop pattern"
        LoopPatContext (name, startNumRef) endsRef endPat pat pat' : loops' -> do
          startNumWhnf <- evalRef startNumRef
          startNum <- fromWHNF startNumWhnf :: (EvalM Integer)
          nextNumRef <- newEvaluatedObjectRef $ Value $ toEgison (startNum + 1)
          ends <- evalRef endsRef
          b <- isEmptyCollection ends
          if b
            then return MNil
            else do
              (carEndsRef, cdrEndsRef) <- fromJust <$> runMaybeT (unconsCollection ends)
              b2 <- evalRef cdrEndsRef >>= isEmptyCollection
              carEndsNum <- evalRef carEndsRef >>= fromWHNF
              return $ if
                | startNum >  carEndsNum -> MNil
                | startNum == carEndsNum && b2 ->
                  fromList [mstate { loopPatCtx = loops', mTrees = MAtom endPat startNumWhnf Something:MAtom pat' target matcher:trees }]
                | startNum == carEndsNum ->
                  fromList [mstate { loopPatCtx = loops', mTrees = MAtom endPat startNumWhnf Something:MAtom pat' target matcher:trees },
                            mstate { loopPatCtx = LoopPatContext (name, nextNumRef) cdrEndsRef endPat pat pat':loops', mTrees = MAtom pat target matcher:trees }]
                | otherwise ->
                  fromList [mstate { loopPatCtx = LoopPatContext (name, nextNumRef) endsRef endPat pat pat':loops', mTrees = MAtom pat target matcher:trees }]
    SeqNilPat -> throwError =<< EgisonBug "should not reach here (seq nil pattern)" <$> getFuncNameStack
    SeqConsPat pattern pattern' -> return . msingleton $ MState env loops (SeqPatContext trees pattern' [] []:seqs) bindings [MAtom pattern target matcher]
    LaterPatVar ->
      case seqs of
        [] -> throwError $ Default "cannot use # out of seq patterns"
        SeqPatContext stack pat mats tgts:seqs ->
          return . msingleton $ MState env loops (SeqPatContext stack pat (mats ++ [matcher]) (tgts ++ [target]):seqs) bindings trees
        ForallPatContext mats tgts:seqs ->
          return . msingleton $ MState env loops (ForallPatContext (mats ++ [matcher]) (tgts ++ [target]):seqs) bindings trees
    AndPat pat1 pat2 ->
      let trees' = [MAtom pat1 target matcher, MAtom pat2 target matcher] ++ trees
       in return . msingleton $ mstate { mTrees = trees' }
    OrPat pat1 pat2 ->
      return $ fromList [mstate { mTrees = MAtom pat1 target matcher : trees }, mstate { mTrees = MAtom pat2 target matcher : trees }]

    _ ->
      case matcher of
        UserMatcher{} -> do
          (patterns, targetss, matchers) <- inductiveMatch env' pattern target matcher
          case length patterns of
            1 ->
              mfor targetss $ \ref -> do
                targets <- evalRef ref >>= (\x -> return [x])
                let trees' = zipWith3 MAtom patterns targets matchers ++ trees
                return $ mstate { mTrees = trees' }
            _ ->
              mfor targetss $ \ref -> do
                targets <- evalRef ref >>= tupleToListWHNF
                let trees' = zipWith3 MAtom patterns targets matchers ++ trees
                return $ mstate { mTrees = trees' }

        Tuple matchers ->
          case pattern of
            ValuePat _ -> return . msingleton $ mstate { mTrees = MAtom pattern target Something:trees }
            WildCard   -> return . msingleton $ mstate { mTrees = MAtom pattern target Something:trees }
            PatVar _   -> return . msingleton $ mstate { mTrees = MAtom pattern target Something:trees }
            IndexedPat _ _ -> return . msingleton $ mstate { mTrees = MAtom pattern target Something:trees }
            TuplePat patterns -> do
              targets <- tupleToListWHNF target
              when (length patterns /= length targets) $ throwError =<< TupleLength (length patterns) (length targets) <$> getFuncNameStack
              when (length patterns /= length matchers) $ throwError =<< TupleLength (length patterns) (length matchers) <$> getFuncNameStack
              let trees' = zipWith3 MAtom patterns targets matchers ++ trees
              return . msingleton $ mstate { mTrees = trees' }
            _ ->  throwError $ Default $ "should not reach here. matcher: " ++ show matcher ++ ", pattern:  " ++ show pattern

        Something ->
          case pattern of
            ValuePat valExpr -> do
              val <- evalExprDeep env' valExpr
              tgtVal <- evalWHNF target
              if val == tgtVal
                then return . msingleton $ mstate { mTrees = trees }
                else return MNil
            WildCard -> return . msingleton $ mstate { mTrees = trees }
            PatVar name -> do
              targetRef <- newEvaluatedObjectRef target
              return . msingleton $ mstate { mStateBindings = (name, targetRef):bindings, mTrees = trees }
            IndexedPat (PatVar name) indices -> do
              indices <- mapM (evalExprShallow env' >=> fmap fromInteger . fromWHNF) indices
              case lookup name bindings of
                Just ref -> do
                  obj <- evalRef ref >>= updateHash indices target >>= newEvaluatedObjectRef
                  return . msingleton $ mstate { mStateBindings = subst name obj bindings, mTrees = trees }
                Nothing  -> do
                  obj <- updateHash indices target (Intermediate . IIntHash $ HL.empty) >>= newEvaluatedObjectRef
                  return . msingleton $ mstate { mStateBindings = (name,obj):bindings, mTrees = trees }
            IndexedPat pattern _ -> throwError $ Default ("invalid indexed-pattern: " ++ prettyStr pattern)
            TuplePat patterns -> do
              targets <- tupleToListWHNF target
              when (length patterns /= length targets) $ throwError =<< TupleLength (length patterns) (length targets) <$> getFuncNameStack
              let trees' = zipWith3 MAtom patterns targets (map (const Something) patterns) ++ trees
              return . msingleton $ mstate { mTrees = trees' }
            _ -> throwError $ Default $ "something can only match with a pattern variable. not: " ++ prettyStr pattern
        _ ->  throwError =<< EgisonBug ("should not reach here. matcher: " ++ show matcher ++ ", pattern:  " ++ show pattern) <$> getFuncNameStack

inductiveMatch :: Env -> Pattern -> WHNFData -> Matcher ->
                  EvalM ([Pattern], MList EvalM ObjectRef, [Matcher])
inductiveMatch env pattern target (UserMatcher matcherEnv clauses) =
  foldr tryPPMatchClause failPPPatternMatch clauses
 where
  tryPPMatchClause (pat, matchers, clauses) cont = do
    result <- runMaybeT $ primitivePatPatternMatch env pat pattern
    case result of
      Just ([pattern], bindings) -> do
        targetss <- foldr (tryPDMatchClause bindings) failPDPatternMatch clauses
        matcher <- evalExprShallow matcherEnv matchers >>= evalMatcherWHNF
        return ([pattern], targetss, [matcher])
      Just (patterns, bindings) -> do
        targetss <- foldr (tryPDMatchClause bindings) failPDPatternMatch clauses
        matchers <- tupleToList <$> (evalExprShallow matcherEnv matchers >>= evalMatcherWHNF)
        return (patterns, targetss, matchers)
      _ -> cont
  tryPDMatchClause bindings (pat, expr) cont = do
    ref <- newEvaluatedObjectRef target
    result <- runMaybeT $ primitiveDataPatternMatch pat ref
    case result of
      Just bindings' -> do
        let env = extendEnv matcherEnv $ bindings ++ bindings'
        evalExprShallow env expr >>= collectionToRefs
      _ -> cont
  failPPPatternMatch = throwError $ Default "failed primitive pattern pattern match"
  failPDPatternMatch = throwError $ Default "failed primitive data pattern match"

primitivePatPatternMatch :: Env -> PrimitivePatPattern -> Pattern ->
                            MatchM ([Pattern], [Binding])
primitivePatPatternMatch _ PPWildCard WildCard = return ([], [])
primitivePatPatternMatch _ PPPatVar pattern = return ([pattern], [])
primitivePatPatternMatch env (PPValuePat name) (ValuePat expr) = do
  ref <- lift $ newThunkRef env expr
  return ([], [(stringToVar name, ref)])
primitivePatPatternMatch env (PPInductivePat name patterns) (InductivePat name' exprs)
  | name == name' && length patterns == length exprs =
    (concat *** concat) . unzip <$> zipWithM (primitivePatPatternMatch env) patterns exprs
  | otherwise = matchFail
primitivePatPatternMatch env (PPTuplePat patterns) (TuplePat exprs)
  | length patterns == length exprs =
    (concat *** concat) . unzip <$> zipWithM (primitivePatPatternMatch env) patterns exprs
  | otherwise = matchFail
primitivePatPatternMatch _ _ _ = matchFail

bindPrimitiveDataPattern :: PrimitiveDataPattern -> ObjectRef -> EvalM [Binding]
bindPrimitiveDataPattern pdp ref = do
  r <- runMaybeT $ primitiveDataPatternMatch pdp ref
  case r of
    Nothing -> throwError $ Default "failed primitive data pattern match"
    Just binding -> return binding

primitiveDataPatternMatch :: PrimitiveDataPattern -> ObjectRef -> MatchM [Binding]
primitiveDataPatternMatch PDWildCard _        = return []
primitiveDataPatternMatch (PDPatVar name) ref = return [(name, ref)]
primitiveDataPatternMatch (PDInductivePat name patterns) ref = do
  whnf <- lift $ evalRef ref
  case whnf of
    Intermediate (IInductiveData name' refs) | name == name' ->
      concat <$> zipWithM primitiveDataPatternMatch patterns refs
    Value (InductiveData name' vals) | name == name' -> do
      whnfs <- lift $ mapM (newEvaluatedObjectRef . Value) vals
      concat <$> zipWithM primitiveDataPatternMatch patterns whnfs
    _ -> matchFail
primitiveDataPatternMatch (PDTuplePat patterns) ref = do
  whnf <- lift $ evalRef ref
  case whnf of
    Intermediate (ITuple refs) -> do
      concat <$> zipWithM primitiveDataPatternMatch patterns refs
    Value (Tuple vals) -> do
      whnfs <- lift $ mapM (newEvaluatedObjectRef . Value) vals
      concat <$> zipWithM primitiveDataPatternMatch patterns whnfs
    _ -> matchFail
primitiveDataPatternMatch PDEmptyPat ref = do
  whnf <- lift $ evalRef ref
  isEmpty <- lift $ isEmptyCollection whnf
  if isEmpty then return [] else matchFail
primitiveDataPatternMatch (PDConsPat pattern pattern') ref = do
  whnf <- lift $ evalRef ref
  (head, tail) <- unconsCollection whnf
  (++) <$> primitiveDataPatternMatch pattern head
       <*> primitiveDataPatternMatch pattern' tail
primitiveDataPatternMatch (PDSnocPat pattern pattern') ref = do
  whnf <- lift $ evalRef ref
  (init, last) <- unsnocCollection whnf
  (++) <$> primitiveDataPatternMatch pattern init
       <*> primitiveDataPatternMatch pattern' last
primitiveDataPatternMatch (PDConstantPat expr) ref = do
  whnf <- lift $ evalRef ref
  case whnf of
    Value val -> do
      val' <- lift $ evalExprDeep nullEnv expr
      if val == val' then return [] else matchFail
    _ -> matchFail

extendEnvForNonLinearPatterns :: Env -> [Binding] -> [LoopPatContext] -> Env
extendEnvForNonLinearPatterns env bindings loops =  extendEnv env $ bindings ++ map (\(LoopPatContext binding _ _ _ _) -> binding) loops

evalMatcherWHNF :: WHNFData -> EvalM Matcher
evalMatcherWHNF (Value matcher@Something) = return matcher
evalMatcherWHNF (Value matcher@UserMatcher{}) = return matcher
evalMatcherWHNF (Value (Tuple ms)) = Tuple <$> mapM (evalMatcherWHNF . Value) ms
evalMatcherWHNF (Intermediate (ITuple refs)) = do
  whnfs <- mapM evalRef refs
  ms <- mapM evalMatcherWHNF whnfs
  return $ Tuple ms
evalMatcherWHNF whnf = throwError =<< TypeMismatch "matcher" whnf <$> getFuncNameStack

--
-- Util
--
toListPat :: [Pattern] -> Pattern
toListPat []         = InductivePat "nil" []
toListPat (pat:pats) = InductivePat "::" [pat, toListPat pats]

-- Refer the specified tensor index with potential overriding of the index.
refTensorWithOverride :: HasTensor a => Bool -> [Index EgisonValue] -> Tensor a -> EvalM a
refTensorWithOverride override js (Tensor ns xs is) =
  tref js' (Tensor ns xs js') >>= toTensor >>= tContract' >>= fromTensor
    where
      js' = if override then js else is ++ js
