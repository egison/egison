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
    (
    -- * Egison code evaluation
      collectDefs
    , evalTopExpr'
    , evalExpr
    , evalExprDeep
    , evalRef
    , evalRefDeep
    , evalWHNF
    , applyFunc
    -- * Array
    , refArray
    , arrayBounds
    -- * Environment
    , recursiveBind
    -- * Pattern matching
    , patternMatch
    -- * Collection
    , isEmptyCollection
    , unconsCollection
    , unsnocCollection
    -- * Tuple, Collection
    , tupleToList
    , collectionToList
    ) where

import           Prelude                     hiding (mapM, mappend, mconcat)

import           Control.Arrow
import           Control.Monad               (when)
import           Control.Monad.Except        (throwError)
import           Control.Monad.State         hiding (mapM)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State   (evalStateT, withStateT)

import           Data.Char                   (isUpper)
import           Data.Foldable               (toList)
import           Data.IORef
import           Data.List                   (partition)
import           Data.Maybe
import           Data.Sequence               (Seq, ViewL (..), ViewR (..), (><))
import qualified Data.Sequence               as Sq
import           Data.Traversable            (mapM)

import qualified Data.Array                  as Array
import qualified Data.HashMap.Lazy           as HL
import qualified Data.Vector                 as V

import           Language.Egison.AST
import           Language.Egison.CmdOptions
import           Language.Egison.Data
import           Language.Egison.MathExpr
import           Language.Egison.Parser      as Parser
import           Language.Egison.ParserNonS  as ParserNonS
import           Language.Egison.Pretty
import           Language.Egison.Types
import           Language.Egison.Tensor

--
-- Evaluator
--

collectDefs :: EgisonOpts -> [EgisonTopExpr] -> [(Var, EgisonExpr)] -> [EgisonTopExpr] -> EgisonM ([(Var, EgisonExpr)], [EgisonTopExpr])
collectDefs opts (expr:exprs) bindings rest =
  case expr of
    Define name expr -> collectDefs opts exprs ((name, expr) : bindings) rest
    DefineWithIndices{} -> throwError =<< EgisonBug "should not reach here (desugared)" <$> getFuncNameStack
    Redefine _ _ -> collectDefs opts exprs bindings $ if optTestOnly opts then expr : rest else rest
    Test _ -> collectDefs opts exprs bindings $ if optTestOnly opts then expr : rest else rest
    Execute _ -> collectDefs opts exprs bindings $ if optTestOnly opts then rest else expr : rest
    LoadFile file ->
      if optNoIO opts
         then throwError $ Default "No IO support"
         else do exprs' <- if optSExpr opts then Parser.loadFile file
                                            else ParserNonS.loadFile file
                 collectDefs opts (exprs' ++ exprs) bindings rest
    Load file ->
      if optNoIO opts
         then throwError $ Default "No IO support"
         else do exprs' <- if optSExpr opts then Parser.loadLibraryFile file
                                            else ParserNonS.loadLibraryFile file
                 collectDefs opts (exprs' ++ exprs) bindings rest
    InfixDecl{} -> collectDefs opts exprs bindings rest
collectDefs _ [] bindings rest = return (bindings, reverse rest)

evalTopExpr' :: EgisonOpts -> StateT [(Var, EgisonExpr)] EgisonM Env -> EgisonTopExpr -> EgisonM (Maybe String, StateT [(Var, EgisonExpr)] EgisonM Env)
evalTopExpr' _ st (Define name expr) = return (Nothing, withStateT (\defines -> (name, expr):defines) st)
evalTopExpr' _ _ DefineWithIndices{} = throwError =<< EgisonBug "should not reach here (desugared)" <$> getFuncNameStack
evalTopExpr' _ st (Redefine name expr) = return (Nothing, mapStateT (>>= \(env, defines) -> (, defines) <$> recursiveRebind env (name, expr)) st)
evalTopExpr' opts st (Test expr) = do
  pushFuncName "<stdin>"
  val <- evalStateT st [] >>= flip evalExprDeep expr
  popFuncName
  case (optSExpr opts, optMathExpr opts) of
    (False, Nothing) -> return (Just (show val), st)
    _  -> return (Just (prettyS val), st)
evalTopExpr' _ st (Execute expr) = do
  pushFuncName "<stdin>"
  io <- evalStateT st [] >>= flip evalExpr expr
  case io of
    Value (IOFunc m) -> m >> popFuncName >> return (Nothing, st)
    _                -> throwError =<< TypeMismatch "io" io <$> getFuncNameStack
evalTopExpr' opts st (Load file) = do
  exprs <- if optSExpr opts then Parser.loadLibraryFile file else ParserNonS.loadLibraryFile file
  (bindings, _) <- collectDefs opts exprs [] []
  return (Nothing, withStateT (\defines -> bindings ++ defines) st)
evalTopExpr' opts st (LoadFile file) = do
  exprs <- if optSExpr opts then Parser.loadFile file else ParserNonS.loadFile file
  (bindings, _) <- collectDefs opts exprs [] []
  return (Nothing, withStateT (\defines -> bindings ++ defines) st)
evalTopExpr' _ st InfixDecl{} = return (Nothing, st)

evalExpr :: Env -> EgisonExpr -> EgisonM WHNFData
evalExpr _ (CharExpr c)    = return . Value $ Char c
evalExpr _ (StringExpr s)  = return . Value $ toEgison s
evalExpr _ (BoolExpr b)    = return . Value $ Bool b
evalExpr _ (IntegerExpr x) = return . Value $ toEgison x
evalExpr _ (FloatExpr x)   = return . Value $ Float x

evalExpr env (QuoteExpr expr) = do
  whnf <- evalExpr env expr
  case whnf of
    Value (ScalarData s) -> return . Value $ ScalarData $ SingleTerm 1 [(Quote s, 1)]
    _ -> throwError =<< TypeMismatch "scalar in quote" whnf <$> getFuncNameStack

evalExpr env (QuoteSymbolExpr expr) = do
  whnf <- evalExpr env expr
  case whnf of
    Value fn@(Func (Just _) _ _ _) -> return . Value $ symbolScalarData "" (prettyS fn)
    Value (ScalarData _) -> return whnf
    _ -> throwError =<< TypeMismatch "value in quote-function" whnf <$> getFuncNameStack

evalExpr env (VarExpr var@(Var [name@(c:_)] [])) | isUpper c = refVar' env var >>= evalRef
 where
  refVar' :: Env -> Var -> EgisonM ObjectRef
  refVar' env var = maybe (newEvaluatedObjectRef (Value (InductiveData name []))) return
                          (refVar env var)

evalExpr env (VarExpr name) = refVar' env name >>= evalRef
 where
  refVar' :: Env -> Var -> EgisonM ObjectRef
  refVar' env var = maybe (newEvaluatedObjectRef (Value (symbolScalarData "" $ prettyS var))) return
                          (refVar env var)

evalExpr env (PartialVarExpr n) = evalExpr env (stringToVarExpr ("::" ++ show n))

evalExpr _ (InductiveDataExpr name []) = return . Value $ InductiveData name []
evalExpr env (InductiveDataExpr name exprs) =
  Intermediate . IInductiveData name <$> mapM (newObjectRef env) exprs

evalExpr _ (TupleExpr []) = return . Value $ Tuple []
evalExpr env (TupleExpr [expr]) = evalExpr env expr
evalExpr env (TupleExpr exprs) = Intermediate . ITuple <$> mapM (newObjectRef env) exprs

evalExpr _ (CollectionExpr []) = return . Value $ Collection Sq.empty

evalExpr env (CollectionExpr inners) = do
  inners' <- mapM fromInnerExpr inners
  innersSeq <- liftIO $ newIORef $ Sq.fromList inners'
  return $ Intermediate $ ICollection innersSeq
 where
  fromInnerExpr :: InnerExpr -> EgisonM Inner
  fromInnerExpr (ElementExpr expr) = IElement <$> newObjectRef env expr
  fromInnerExpr (SubCollectionExpr expr) = ISubCollection <$> newObjectRef env expr

evalExpr env (ArrayExpr exprs) = do
  refs' <- mapM (newObjectRef env) exprs
  return . Intermediate . IArray $ Array.listArray (1, toInteger (length exprs)) refs'

evalExpr env@(Env frame maybe_vwi) (VectorExpr exprs) = do
  whnfs <- mapM (\(expr, i) ->
    let env' = maybe env (\(VarWithIndices nameString indexList) -> Env frame $ Just $ VarWithIndices nameString $ changeIndexList indexList [toEgison $ toInteger i]) maybe_vwi
     in evalExpr env' expr) $ zip exprs [1..(length exprs + 1)]
  case whnfs of
    Intermediate (ITensor Tensor{}):_ ->
      mapM toTensor (zipWith (curry f) whnfs [1..(length exprs + 1)]) >>= tConcat' >>= fromTensor
    _ -> fromTensor (Tensor [fromIntegral $ length whnfs] (V.fromList whnfs) [])
 where
  f (Intermediate (ITensor (Tensor ns xs indices)), i) =
    Intermediate $ ITensor $ Tensor ns (V.fromList $ zipWith (curry g) (V.toList xs) $ map (\ms -> map toEgison $ toInteger i:ms) $ enumTensorIndices ns) indices
  f (x, _) = x
  g (Value (ScalarData (Div (Plus [Term 1 [(FunctionData fn argnames args js, 1)]]) p)), ms) =
    let fn' = maybe fn (\(VarWithIndices nameString indexList) -> symbolScalarData' "" $ prettyS $ VarWithIndices nameString $ changeIndexList indexList ms) maybe_vwi
     in Value $ ScalarData $ Div (Plus [Term 1 [(FunctionData fn' argnames args js, 1)]]) p
  g (x, _) = x

evalExpr env (TensorExpr nsExpr xsExpr) = do
  nsWhnf <- evalExpr env nsExpr
  ns <- (fromCollection nsWhnf >>= fromMList >>= mapM evalRef >>= mapM fromWHNF) :: EgisonM [Integer]
  xsWhnf <- evalExpr env xsExpr
  xs <- fromCollection xsWhnf >>= fromMList >>= mapM evalRef
  if product ns == toInteger (length xs)
    then fromTensor (initTensor ns xs)
    else throwError =<< InconsistentTensorShape <$> getFuncNameStack

evalExpr env (HashExpr assocs) = do
  let (keyExprs, exprs) = unzip assocs
  keyWhnfs <- mapM (evalExpr env) keyExprs
  keys <- mapM makeHashKey keyWhnfs
  refs <- mapM (newObjectRef env) exprs
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
  makeHashKey :: WHNFData -> EgisonM EgisonHashKey
  makeHashKey (Value val) =
    case val of
      ScalarData _ -> IntKey <$> fromEgison val
      Char c       -> return (CharKey c)
      String str   -> return (StrKey str)
      _ -> throwError =<< TypeMismatch "integer or string" (Value val) <$> getFuncNameStack
  makeHashKey whnf = throwError =<< TypeMismatch "integer or string" whnf <$> getFuncNameStack

evalExpr env (IndexedExpr override expr indices) = do
  tensor <- case expr of
              VarExpr (Var xs is) -> do
                let mObjRef = refVar env (Var xs $ is ++ map (const () <$>) indices)
                case mObjRef of
                  Just objRef -> evalRef objRef
                  Nothing     -> evalExpr env expr
              _ -> evalExpr env expr
  js <- mapM evalIndex indices
  ret <- case tensor of
      Value (ScalarData (SingleTerm 1 [(Symbol id name [], 1)])) -> do
        js2 <- mapM evalIndexToScalar indices
        return $ Value (ScalarData (SingleTerm 1 [(Symbol id name js2, 1)]))
      Value (ScalarData (SingleTerm 1 [(Symbol id name js', 1)])) -> do
        js2 <- mapM evalIndexToScalar indices
        return $ Value (ScalarData (SingleTerm 1 [(Symbol id name (js' ++ js2), 1)]))
      Value (TensorData t@Tensor{})     -> Value <$> refTenworWithOverride override js t
      Intermediate (ITensor t@Tensor{}) -> refTenworWithOverride override js t
      _ -> do
        js2 <- mapM evalIndexToScalar indices
        refArray tensor (map (ScalarData . extractIndex) js2)
  return ret -- TODO: refactor
 where
  evalIndex :: Index EgisonExpr -> EgisonM (Index EgisonValue)
  evalIndex index = traverse (evalExprDeep env) index

  evalIndexToScalar :: Index EgisonExpr -> EgisonM (Index ScalarData)
  evalIndexToScalar index = traverse ((extractScalar =<<) . evalExprDeep env) index

evalExpr env (SubrefsExpr override expr jsExpr) = do
  js <- map Subscript <$> (evalExpr env jsExpr >>= collectionToList)
  tensor <- case expr of
              VarExpr (Var xs is) -> do
                let mObjRef = refVar env (Var xs $ is ++ replicate (length js) (Subscript ()))
                case mObjRef of
                  Just objRef -> evalRef objRef
                  Nothing     -> evalExpr env expr
              _ -> evalExpr env expr
  case tensor of
    Value (ScalarData _)              -> return tensor
    Value (TensorData t@Tensor{})     -> Value <$> refTenworWithOverride override js t
    Intermediate (ITensor t@Tensor{}) -> refTenworWithOverride override js t
    _ -> throwError =<< NotImplemented "subrefs" <$> getFuncNameStack

evalExpr env (SuprefsExpr override expr jsExpr) = do
  js <- map Superscript <$> (evalExpr env jsExpr >>= collectionToList)
  tensor <- case expr of
              VarExpr (Var xs is) -> do
                let mObjRef = refVar env (Var xs $ is ++ replicate (length js) (Superscript ()))
                case mObjRef of
                  Just objRef -> evalRef objRef
                  Nothing     -> evalExpr env expr
              _ -> evalExpr env expr
  case tensor of
    Value (ScalarData _)              -> return tensor
    Value (TensorData t@Tensor{})     -> Value <$> refTenworWithOverride override js t
    Intermediate (ITensor t@Tensor{}) -> refTenworWithOverride override js t
    _ -> throwError =<< NotImplemented "suprefs" <$> getFuncNameStack

evalExpr env (UserrefsExpr _ expr jsExpr) = do
  val <- evalExprDeep env expr
  js <- map Userscript <$> (evalExpr env jsExpr >>= collectionToList >>= mapM extractScalar)
  case val of
    ScalarData (SingleTerm 1 [(Symbol id name is, 1)]) ->
      return $ Value (ScalarData (SingleTerm 1 [(Symbol id name (is ++ js), 1)]))
    ScalarData (SingleTerm 1 [(FunctionData name argnames args is, 1)]) ->
      return $ Value (ScalarData (SingleTerm 1 [(FunctionData name argnames args (is ++ js), 1)]))
    _ -> throwError =<< NotImplemented "user-refs" <$> getFuncNameStack

evalExpr env (LambdaExpr names expr) = do
  names' <- mapM (\case
                     TensorArg name' -> return name'
                     ScalarArg _ -> throwError =<< EgisonBug "scalar-arg remained" <$> getFuncNameStack) names
  return . Value $ Func Nothing env names' expr

evalExpr env (PartialExpr n expr) = return . Value $ PartialFunc env n expr

evalExpr env (CambdaExpr name expr) = return . Value $ CFunc Nothing env name expr

evalExpr env (ProcedureExpr names expr) = return . Value $ Proc Nothing env names expr

evalExpr env (PatternFunctionExpr names pattern) = return . Value $ PatternFunc env names pattern

evalExpr (Env _ Nothing) (FunctionExpr _) = throwError $ Default "function symbol is not bound to a variable"

evalExpr env@(Env _ (Just name)) (FunctionExpr args) = do
  args' <- mapM (evalExprDeep env) args >>= mapM extractScalar
  return . Value $ ScalarData (SingleTerm 1 [(FunctionData (symbolScalarData' "" (prettyS name)) (map (symbolScalarData' "" . prettyS) args) args' [], 1)])

evalExpr env (IfExpr test expr expr') = do
  test <- evalExpr env test >>= fromWHNF
  evalExpr env $ if test then expr else expr'

evalExpr env (LetExpr bindings expr) =
  mapM extractBindings bindings >>= flip evalExpr expr . extendEnv env . concat
 where
  extractBindings :: BindingExpr -> EgisonM [Binding]
  extractBindings ([name], expr) =
    case expr of
      FunctionExpr _ ->
        let Env frame _ = env
         in makeBindings [name] . (:[]) <$> newObjectRef (Env frame (Just $ varToVarWithIndices name)) expr
      _ -> makeBindings [name] . (:[]) <$> newObjectRef env expr
  extractBindings (names, expr) =
    makeBindings names <$> (evalExpr env expr >>= fromTuple)

evalExpr env (LetRecExpr bindings expr) =
  let bindings' = evalState (concat <$> mapM extractBindings bindings) 0
  in recursiveBind env bindings' >>= flip evalExpr expr
 where
  extractBindings :: BindingExpr -> State Int [(Var, EgisonExpr)]
  extractBindings ([name], expr) = return [(name, expr)]
  extractBindings (names, expr) = do
    var <- genVar
    let k = length names
        target = VarExpr var
        matcher = TupleExpr $ replicate k SomethingExpr
        nth n =
          let pattern = TuplePat $ flip map [1..k] $ \i ->
                if i == n then PatVar (stringToVar "#_") else WildCard
          in MatchExpr BFSMode target matcher [(pattern, stringToVarExpr "#_")]
    return ((var, expr) : map (second nth) (zip names [1..]))

  genVar :: State Int Var
  genVar = modify (1+) >> gets (stringToVar . ('#':) . show)

evalExpr env (TransposeExpr vars expr) = do
  syms <- evalExpr env vars >>= collectionToList
  whnf <- evalExpr env expr
  case whnf of
    Intermediate (ITensor t) -> Intermediate . ITensor <$> tTranspose' syms t
    Value (TensorData t)     -> Value . TensorData <$> tTranspose' syms t
    _                        -> return whnf

evalExpr env (FlipIndicesExpr expr) = do
  whnf <- evalExpr env expr
  case whnf of
    Intermediate (ITensor t) -> Intermediate . ITensor <$> tFlipIndices t
    Value (TensorData t)     -> Value . TensorData <$> tFlipIndices t
    _                        -> return whnf

evalExpr env (WithSymbolsExpr vars expr) = do
  symId <- fresh
  syms <- mapM (newEvaluatedObjectRef . Value . symbolScalarData symId) vars
  let bindings = zip (map stringToVar vars) syms
  whnf <- evalExpr (extendEnv env bindings) expr
  case whnf of
    Value (TensorData t@Tensor{}) ->
      Value . TensorData <$> removeTmpScripts symId t
    Intermediate (ITensor t@Tensor{}) ->
      Intermediate . ITensor <$> removeTmpScripts symId t
    _ -> return whnf
 where
  isTmpSymbol :: String -> Index EgisonValue -> Bool
  isTmpSymbol symId (Subscript    (ScalarData (SingleTerm 1 [(Symbol id _ _, _)]))) = symId == id
  isTmpSymbol symId (Superscript  (ScalarData (SingleTerm 1 [(Symbol id _ _, _)]))) = symId == id
  isTmpSymbol symId (SupSubscript (ScalarData (SingleTerm 1 [(Symbol id _ _, _)]))) = symId == id
  isTmpSymbol symId (Userscript   (ScalarData (SingleTerm 1 [(Symbol id _ _, _)]))) = symId == id
  removeTmpScripts :: HasTensor a => String -> Tensor a -> EgisonM (Tensor a)
  removeTmpScripts symId (Tensor s xs is) = do
    let (ds, js) = partition (isTmpSymbol symId) is
    Tensor s ys _ <- tTranspose (js ++ ds) (Tensor s xs is)
    return (Tensor s ys js)


evalExpr env (DoExpr bindings expr) = return $ Value $ IOFunc $ do
  let body = foldr genLet (ApplyExpr expr $ TupleExpr [stringToVarExpr "#1"]) bindings
  applyFunc env (Value $ Func Nothing env ["#1"] body) $ Value World
 where
  genLet (names, expr) expr' =
    LetExpr [(map stringToVar ["#1", "#2"], ApplyExpr expr $ TupleExpr [stringToVarExpr "#1"])] $
    LetExpr [(names, stringToVarExpr "#2")] expr'

evalExpr env (IoExpr expr) = do
  io <- evalExpr env expr
  case io of
    Value (IOFunc m) -> do
      val <- m >>= evalWHNF
      case val of
        Tuple [_, val'] -> return $ Value val'
    _ -> throwError =<< TypeMismatch "io" io <$> getFuncNameStack

evalExpr env (MatchAllExpr pmmode target matcher clauses) = do
  target <- evalExpr env target
  matcher <- evalExpr env matcher >>= evalMatcherWHNF
  f matcher target >>= fromMList
 where
  fromMList :: MList EgisonM WHNFData -> EgisonM WHNFData
  fromMList MNil = return . Value $ Collection Sq.empty
  fromMList (MCons val m) = do
    head <- IElement <$> newEvaluatedObjectRef val
    tail <- ISubCollection <$> (liftIO . newIORef . Thunk $ m >>= fromMList)
    seqRef <- liftIO . newIORef $ Sq.fromList [head, tail]
    return . Intermediate $ ICollection seqRef
  f matcher target = do
      let tryMatchClause (pattern, expr) results = do
            result <- patternMatch pmmode env pattern target matcher
            mmap (flip evalExpr expr . extendEnv env) result >>= (`mappend` results)
      mfoldr tryMatchClause (return MNil) (fromList clauses)

evalExpr env (MatchExpr pmmode target matcher clauses) = do
  target <- evalExpr env target
  matcher <- evalExpr env matcher >>= evalMatcherWHNF
  f matcher target
 where
  f matcher target = do
      let tryMatchClause (pattern, expr) cont = do
            result <- patternMatch pmmode env pattern target matcher
            case result of
              MCons bindings _ -> evalExpr (extendEnv env bindings) expr
              MNil             -> cont
      currentFuncName <- topFuncName
      callstack <- getFuncNameStack
      foldr tryMatchClause (throwError $ MatchFailure currentFuncName callstack) clauses

evalExpr env (SeqExpr expr1 expr2) = do
  _ <- evalExprDeep env expr1
  evalExpr env expr2

evalExpr env (CApplyExpr func arg) = do
  func <- evalExpr env func
  args <- evalExpr env arg >>= collectionToList
  case func of
    Value (MemoizedFunc name ref hashRef env names body) -> do
      indices' <- mapM fromEgison args
      hash <- liftIO $ readIORef hashRef
      case HL.lookup indices' hash of
        Just objRef ->
          evalRef objRef
        Nothing -> do
          whnf <- applyFunc env (Value (Func Nothing env names body)) (Value (makeTuple args))
          retRef <- newEvaluatedObjectRef whnf
          hash <- liftIO $ readIORef hashRef
          liftIO $ writeIORef hashRef (HL.insert indices' retRef hash)
          writeObjectRef ref (Value (MemoizedFunc name ref hashRef env names body))
          return whnf
    _ -> applyFunc env func (Value (makeTuple args))

evalExpr env (ApplyExpr func arg) = do
  func <- evalExpr env func >>= appendDFscripts 0
  case func of
--    Value (ScalarData (SingleTerm 1 [(Symbol "" name@(c:_) [], 1)])) | isUpper c ->
    Value (InductiveData name []) ->
      case arg of
        TupleExpr exprs ->
          Intermediate . IInductiveData name <$> mapM (newObjectRef env) exprs
        _ -> throwError $ Default "argument is not a tuple"
    Value (TensorData t@Tensor{}) -> do
      arg <- evalExpr env arg
      Value <$> (tMap (\f -> applyFunc env (Value f) arg >>= evalWHNF) t >>= fromTensor) >>= removeDFscripts
    Intermediate (ITensor t@Tensor{}) -> do
      arg <- evalExpr env arg
      tMap (\f -> applyFunc env f arg) t >>= fromTensor
    Value (MemoizedFunc name ref hashRef env' names body) -> do
      arg <- evalExpr env arg
      indices <- evalWHNF arg
      indices' <- mapM fromEgison $ fromTupleValue indices
      hash <- liftIO $ readIORef hashRef
      case HL.lookup indices' hash of
        Just objRef ->
          evalRef objRef
        Nothing -> do
          whnf <- applyFunc env' (Value (Func Nothing env' names body)) arg
          retRef <- newEvaluatedObjectRef whnf
          hash <- liftIO $ readIORef hashRef
          liftIO $ writeIORef hashRef (HL.insert indices' retRef hash)
          writeObjectRef ref (Value (MemoizedFunc name ref hashRef env' names body))
          return whnf
    _ -> do
      arg <- evalExpr env arg
      applyFunc env func arg >>= removeDFscripts

evalExpr env (WedgeApplyExpr func arg) = do
  func <- evalExpr env func >>= appendDFscripts 0
  arg <- evalExpr env arg >>= fromTupleWHNF
  let k = fromIntegral (length arg)
  arg <- zipWithM appendDFscripts [1..k] arg >>= makeITuple
  case func of
    Value (TensorData t@Tensor{}) ->
      Value <$> (tMap (\f -> applyFunc env (Value f) arg >>= evalWHNF) t >>= fromTensor)
    Intermediate (ITensor t@Tensor{}) ->
      tMap (\f -> applyFunc env f arg) t >>= fromTensor
    Value (MemoizedFunc name ref hashRef env names body) -> do
      indices <- evalWHNF arg
      indices' <- mapM fromEgison $ fromTupleValue indices
      hash <- liftIO $ readIORef hashRef
      case HL.lookup indices' hash of
        Just objRef ->
          evalRef objRef
        Nothing -> do
          whnf <- applyFunc env (Value (Func Nothing env names body)) arg
          retRef <- newEvaluatedObjectRef whnf
          hash <- liftIO $ readIORef hashRef
          liftIO $ writeIORef hashRef (HL.insert indices' retRef hash)
          writeObjectRef ref (Value (MemoizedFunc name ref hashRef env names body))
          return whnf
    _ -> applyFunc env func arg >>= removeDFscripts

evalExpr env (MemoizeExpr memoizeFrame expr) = do
  mapM_ (\(x, y, z) -> do x' <- evalExprDeep env x
                          case x' of
                            MemoizedFunc name ref hashRef env' names body -> do
                              indices <- evalExprDeep env y
                              indices' <- mapM fromEgison $ fromTupleValue indices
                              hash <- liftIO $ readIORef hashRef
                              ret <- evalExprDeep env z
                              retRef <- newEvaluatedObjectRef (Value ret)
                              liftIO $ writeIORef hashRef (HL.insert indices' retRef hash)
                              writeObjectRef ref (Value (MemoizedFunc name ref hashRef env' names body))
                            _ -> throwError =<< TypeMismatch "memoized-function" (Value x') <$> getFuncNameStack)
       memoizeFrame
  evalExpr env expr

evalExpr env (MatcherExpr info) = return $ Value $ UserMatcher env info

evalExpr env (GenerateArrayExpr fnExpr (fstExpr, lstExpr)) = do
  fN <- (evalExpr env fstExpr >>= fromWHNF) :: EgisonM Integer
  eN <- (evalExpr env lstExpr >>= fromWHNF) :: EgisonM Integer
  xs <- mapM (newObjectRef env . ApplyExpr fnExpr . IntegerExpr) [fN..eN]
  return $ Intermediate $ IArray $ Array.listArray (fN, eN) xs

evalExpr env (ArrayBoundsExpr expr) =
  evalExpr env expr >>= arrayBounds

evalExpr env (GenerateTensorExpr fnExpr shapeExpr) = do
  shape <- evalExpr env shapeExpr >>= collectionToList
  ns    <- mapM fromEgison shape :: EgisonM Shape
  xs    <- mapM (indexToWHNF env . map toEgison) (enumTensorIndices ns)
  fromTensor (Tensor ns (V.fromList xs) [])
 where
  indexToWHNF :: Env -> [EgisonValue] {- index -} -> EgisonM WHNFData
  indexToWHNF (Env frame maybe_vwi) ms = do
    let env' = maybe env (\(VarWithIndices nameString indexList) -> Env frame $ Just $ VarWithIndices nameString $ changeIndexList indexList ms) maybe_vwi
    fn <- evalExpr env' fnExpr
    applyFunc env fn $ Value $ makeTuple ms

evalExpr env (TensorContractExpr fnExpr tExpr) = do
  fn <- evalExpr env fnExpr
  whnf <- evalExpr env tExpr
  case whnf of
    Intermediate (ITensor t@Tensor{}) -> do
      ts <- tContract t
      tMapN (\xs -> do xs' <- mapM newEvaluatedObjectRef xs
                       applyFunc env fn (Intermediate (ITuple xs'))) ts >>= fromTensor
    Value (TensorData t@Tensor{}) -> do
      ts <- tContract t
      Value <$> (tMapN (applyFunc' env fn . Tuple) ts >>= fromTensor)
    _ -> return whnf
 where
  applyFunc' :: Env -> WHNFData -> EgisonValue -> EgisonM EgisonValue
  applyFunc' env fn x = applyFunc env fn (Value x) >>= evalWHNF

evalExpr env (TensorMapExpr fnExpr tExpr) = do
  fn <- evalExpr env fnExpr
  whnf <- evalExpr env tExpr
  case whnf of
    Intermediate (ITensor t) ->
      tMap (applyFunc env fn) t >>= fromTensor
    Value (TensorData t) ->
      Value <$> (tMap (applyFunc' env fn) t >>= fromTensor)
    _ -> applyFunc env fn whnf
 where
  applyFunc' :: Env -> WHNFData -> EgisonValue -> EgisonM EgisonValue
  applyFunc' env fn x = applyFunc env fn (Value x) >>= evalWHNF

evalExpr env (TensorMap2Expr fnExpr t1Expr t2Expr) = do
  fn <- evalExpr env fnExpr
  whnf1 <- evalExpr env t1Expr
  whnf2 <- evalExpr env t2Expr
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
  applyFunc' :: Env -> WHNFData -> EgisonValue -> EgisonM EgisonValue
  applyFunc' env fn x = applyFunc env fn (Value x) >>= evalWHNF
  applyFunc'' :: Env -> WHNFData -> WHNFData -> WHNFData -> EgisonM WHNFData
  applyFunc'' env fn x y = do
    xRef <- newEvaluatedObjectRef x
    yRef <- newEvaluatedObjectRef y
    applyFunc env fn (Intermediate (ITuple [xRef, yRef]))


evalExpr _ SomethingExpr = return $ Value Something
evalExpr _ UndefinedExpr = return $ Value Undefined
evalExpr _ expr = throwError =<< NotImplemented ("evalExpr for " ++ show expr) <$> getFuncNameStack

evalExprDeep :: Env -> EgisonExpr -> EgisonM EgisonValue
evalExprDeep env expr = evalExpr env expr >>= evalWHNF

evalRef :: ObjectRef -> EgisonM WHNFData
evalRef ref = do
  obj <- liftIO $ readIORef ref
  case obj of
    WHNF val -> return val
    Thunk thunk -> do
      val <- thunk
      writeObjectRef ref val
      return val

evalRefDeep :: ObjectRef -> EgisonM EgisonValue
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

evalWHNF :: WHNFData -> EgisonM EgisonValue
evalWHNF (Value val) = return val
evalWHNF (Intermediate (IInductiveData name refs)) =
  InductiveData name <$> mapM evalRefDeep refs
evalWHNF (Intermediate (IArray refs)) = do
  refs' <- mapM evalRefDeep $ Array.elems refs
  return $ Array $ Array.listArray (Array.bounds refs) refs'
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
evalWHNF coll = Collection <$> (fromCollection coll >>= fromMList >>= mapM evalRefDeep . Sq.fromList)

addscript :: (Index EgisonValue, Tensor a) -> Tensor a
addscript (subj, Tensor s t i) = Tensor s t (i ++ [subj])

valuetoTensor2 :: WHNFData -> Tensor WHNFData
valuetoTensor2 (Intermediate (ITensor t)) = t

applyFunc :: Env -> WHNFData -> WHNFData -> EgisonM WHNFData
applyFunc env (Value (TensorData (Tensor s1 t1 i1))) tds = do
  tds <- fromTupleWHNF tds
  if length s1 > length i1 && all (\(Intermediate (ITensor (Tensor s _ i))) -> length s - length i == 1) tds
    then do
      symId <- fresh
      let argnum = length tds
          subjs = map (Subscript . symbolScalarData symId . show) [1 .. argnum]
          supjs = map (Superscript . symbolScalarData symId . show) [1 .. argnum]
      dot <- evalExpr env (stringToVarExpr ".")
      makeITuple (Value (TensorData (Tensor s1 t1 (i1 ++ supjs))):map (Intermediate .ITensor . addscript) (zip subjs $ map valuetoTensor2 tds)) >>= applyFunc env dot
    else throwError $ Default "applyfunc"

applyFunc env (Intermediate (ITensor (Tensor s1 t1 i1))) tds = do
  tds <- fromTupleWHNF tds
  if length s1 > length i1 && all (\(Intermediate (ITensor (Tensor s _ i))) -> length s - length i == 1) tds
    then do
      symId <- fresh
      let argnum = length tds
          subjs = map (Subscript . symbolScalarData symId . show) [1 .. argnum]
          supjs = map (Superscript . symbolScalarData symId . show) [1 .. argnum]
      dot <- evalExpr env (stringToVarExpr ".")
      makeITuple (map Intermediate (ITensor (Tensor s1 t1 (i1 ++ supjs)):map (ITensor . addscript) (zip subjs $ map valuetoTensor2 tds))) >>= applyFunc env dot
    else throwError $ Default "applyfunc"

applyFunc _ (Value (PartialFunc env n body)) arg = do
  refs <- fromTuple arg
  if n == fromIntegral (length refs)
    then evalExpr (extendEnv env $ makeBindings (map (\n -> stringToVar $ "::" ++ show n) [1..n]) refs) body
    else throwError =<< ArgumentsNumWithNames ["partial"] (fromIntegral n) (length refs) <$> getFuncNameStack
applyFunc _ (Value (Func (Just (Var [funcname] _)) env [name] body)) arg = do
  pushFuncName funcname
  ref <- newEvaluatedObjectRef arg
  result <- evalExpr (extendEnv env $ makeBindings' [name] [ref]) body
  popFuncName
  return result
applyFunc _ (Value (Func _ env [name] body)) arg = do
  ref <- newEvaluatedObjectRef arg
  evalExpr (extendEnv env $ makeBindings' [name] [ref]) body
applyFunc _ (Value (Func (Just (Var [funcname] _)) env names body)) arg = do
  pushFuncName funcname
  refs <- fromTuple arg
  result <- if length names == length refs
              then evalExpr (extendEnv env $ makeBindings' names refs) body
              else throwError =<< ArgumentsNumWithNames names (length names) (length refs) <$> getFuncNameStack
  popFuncName
  return result
applyFunc _ (Value (Func _ env names body)) arg = do
  refs <- fromTuple arg
  if length names == length refs
    then evalExpr (extendEnv env $ makeBindings' names refs) body
    else throwError =<< ArgumentsNumWithNames names (length names) (length refs) <$> getFuncNameStack
applyFunc _ (Value (Proc _ env [name] body)) arg = do
  ref <- newEvaluatedObjectRef arg
  evalExpr (extendEnv env $ makeBindings' [name] [ref]) body
applyFunc _ (Value (Proc _ env names body)) arg = do
  refs <- fromTuple arg
  if length names == length refs
    then evalExpr (extendEnv env $ makeBindings' names refs) body
    else throwError =<< ArgumentsNumWithNames names (length names) (length refs) <$> getFuncNameStack
applyFunc _ (Value (CFunc _ env name body)) arg = do
  refs <- fromTuple arg
  seqRef <- liftIO . newIORef $ Sq.fromList (map IElement refs)
  col <- liftIO . newIORef $ WHNF $ Intermediate $ ICollection seqRef
  if not (null refs)
    then evalExpr (extendEnv env $ makeBindings' [name] [col]) body
    else throwError =<< ArgumentsNumWithNames [name] 1 0 <$> getFuncNameStack
applyFunc _ (Value (PrimitiveFunc _ func)) arg = func arg
applyFunc _ (Value (IOFunc m)) arg =
  case arg of
     Value World -> m
     _           -> throwError =<< TypeMismatch "world" arg <$> getFuncNameStack
applyFunc _ (Value (ScalarData fn@(SingleTerm 1 [(Symbol{}, 1)]))) arg = do
  args <- tupleToList arg
  mExprs <- mapM (\arg -> case arg of
                            ScalarData _ -> extractScalar arg
                            _ -> throwError =<< EgisonBug "to use undefined functions, you have to use ScalarData args" <$> getFuncNameStack) args
  return (Value (ScalarData (SingleTerm 1 [(Apply fn mExprs, 1)])))
applyFunc _ whnf _ = throwError =<< TypeMismatch "function" whnf <$> getFuncNameStack

refArray :: WHNFData -> [EgisonValue] -> EgisonM WHNFData
refArray val [] = return val
refArray (Value (Array array)) (index:indices) =
  if isInteger index
    then do i <- (fmap fromInteger . fromEgison) index
            if (\(a,b) -> a <= i && i <= b) $ Array.bounds array
              then refArray (Value (array Array.! i)) indices
              else return  $ Value Undefined
    else case index of
           ScalarData (SingleTerm 1 [(Symbol _ _ [], 1)]) -> do
             let (_,size) = Array.bounds array
             elms <- mapM (\arr -> refArray (Value arr) indices) (Array.elems array)
             elmRefs <- mapM newEvaluatedObjectRef elms
             return $ Intermediate $ IArray $ Array.listArray (1, size) elmRefs
           _  -> throwError =<< TypeMismatch "integer or symbol" (Value index) <$> getFuncNameStack
refArray (Intermediate (IArray array)) (index:indices) =
  if isInteger index
    then do i <- (fmap fromInteger . fromEgison) index
            if (\(a,b) -> a <= i && i <= b) $ Array.bounds array
              then let ref = array Array.! i in
                   evalRef ref >>= flip refArray indices
              else return  $ Value Undefined
    else case index of
           ScalarData (SingleTerm 1 [(Symbol _ _ [], 1)]) -> do
             let (_,size) = Array.bounds array
             let refs = Array.elems array
             arrs <- mapM evalRef refs
             elms <- mapM (`refArray` indices) arrs
             elmRefs <- mapM newEvaluatedObjectRef elms
             return $ Intermediate $ IArray $ Array.listArray (1, size) elmRefs
           _  -> throwError =<< TypeMismatch "integer or symbol" (Value index) <$> getFuncNameStack
refArray (Value (IntHash hash)) (index:indices) = do
  key <- fromEgison index
  case HL.lookup key hash of
    Just val -> refArray (Value val) indices
    Nothing  -> return $ Value Undefined
refArray (Intermediate (IIntHash hash)) (index:indices) = do
  key <- fromEgison index
  case HL.lookup key hash of
    Just ref -> evalRef ref >>= flip refArray indices
    Nothing  -> return $ Value Undefined
refArray (Value (CharHash hash)) (index:indices) = do
  key <- fromEgison index
  case HL.lookup key hash of
    Just val -> refArray (Value val) indices
    Nothing  -> return $ Value Undefined
refArray (Intermediate (ICharHash hash)) (index:indices) = do
  key <- fromEgison index
  case HL.lookup key hash of
    Just ref -> evalRef ref >>= flip refArray indices
    Nothing  -> return $ Value Undefined
refArray (Value (StrHash hash)) (index:indices) = do
  key <- fromEgison index
  case HL.lookup key hash of
    Just val -> refArray (Value val) indices
    Nothing  -> return $ Value Undefined
refArray (Intermediate (IStrHash hash)) (index:indices) = do
  key <- fromEgison index
  case HL.lookup key hash of
    Just ref -> evalRef ref >>= flip refArray indices
    Nothing  -> return $ Value Undefined
refArray val _ = throwError =<< TypeMismatch "array or hash" val <$> getFuncNameStack

arrayBounds :: WHNFData -> EgisonM WHNFData
arrayBounds val = Value <$> arrayBounds' val

arrayBounds' :: WHNFData -> EgisonM EgisonValue
arrayBounds' (Intermediate (IArray arr)) = return $ Tuple [toEgison (fst (Array.bounds arr)), toEgison (snd (Array.bounds arr))]
arrayBounds' (Value (Array arr))         = return $ Tuple [toEgison (fst (Array.bounds arr)), toEgison (snd (Array.bounds arr))]
arrayBounds' val                         = throwError =<< TypeMismatch "array" val <$> getFuncNameStack

newThunk :: Env -> EgisonExpr -> Object
newThunk env expr = Thunk $ evalExpr env expr

newObjectRef :: Env -> EgisonExpr -> EgisonM ObjectRef
newObjectRef env expr = liftIO $ newIORef $ newThunk env expr

writeObjectRef :: ObjectRef -> WHNFData -> EgisonM ()
writeObjectRef ref val = liftIO . writeIORef ref $ WHNF val

newEvaluatedObjectRef :: WHNFData -> EgisonM ObjectRef
newEvaluatedObjectRef = liftIO . newIORef . WHNF

makeBindings :: [Var] -> [ObjectRef] -> [Binding]
makeBindings = zip

makeBindings' :: [String] -> [ObjectRef] -> [Binding]
makeBindings' xs = zip (map stringToVar xs)

recursiveBind :: Env -> [(Var, EgisonExpr)] -> EgisonM Env
recursiveBind env bindings = do
  let (names, _) = unzip bindings
  refs <- replicateM (length bindings) $ newObjectRef nullEnv UndefinedExpr
  let env' = extendEnv env $ makeBindings names refs
  let Env frame _ = env'
  zipWithM_ (\ref (name,expr) ->
               case expr of
                 MemoizedLambdaExpr names body -> do
                   hashRef <- liftIO $ newIORef HL.empty
                   liftIO . writeIORef ref . WHNF . Value $ MemoizedFunc (Just name) ref hashRef env' names body
                 LambdaExpr _ _ -> do
                   whnf <- evalExpr env' expr
                   case whnf of
                     Value (Func _ env args body) -> liftIO . writeIORef ref . WHNF $ Value (Func (Just name) env args body)
                 CambdaExpr _ _ -> do
                   whnf <- evalExpr env' expr
                   case whnf of
                     Value (CFunc _ env arg body) -> liftIO . writeIORef ref . WHNF $ Value (CFunc (Just name) env arg body)
                 FunctionExpr args -> liftIO . writeIORef ref . Thunk $ evalExpr (Env frame (Just $ varToVarWithIndices name)) $ FunctionExpr args
                 _ | isVarWithIndices name -> liftIO . writeIORef ref . Thunk $ evalExpr (Env frame (Just $ varToVarWithIndices name)) expr
                   | otherwise -> liftIO . writeIORef ref . Thunk $ evalExpr env' expr)
            refs bindings
  return env'
 where
  isVarWithIndices :: Var -> Bool
  isVarWithIndices (Var _ xs) = not $ null xs

recursiveRebind :: Env -> (Var, EgisonExpr) -> EgisonM Env
recursiveRebind env (name, expr) = do
  case refVar env name of
    Nothing -> throwError =<< UnboundVariable (prettyS name) <$> getFuncNameStack
    Just ref -> case expr of
                  MemoizedLambdaExpr names body -> do
                    hashRef <- liftIO $ newIORef HL.empty
                    liftIO . writeIORef ref . WHNF . Value $ MemoizedFunc (Just name) ref hashRef env names body
                  LambdaExpr _ _ -> do
                    whnf <- evalExpr env expr
                    case whnf of
                      Value (Func _ env args body) -> liftIO . writeIORef ref . WHNF $ Value (Func (Just name) env args body)
                  CambdaExpr _ _ -> do
                    whnf <- evalExpr env expr
                    case whnf of
                      Value (CFunc _ env arg body) -> liftIO . writeIORef ref . WHNF $ Value (CFunc (Just name) env arg body)
                  _ -> liftIO . writeIORef ref . Thunk $ evalExpr env expr
  return env

--
-- Pattern Match
--

patternMatch :: PMMode -> Env -> EgisonPattern -> WHNFData -> Matcher -> EgisonM (MList EgisonM Match)
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

processMStatesAllDFS :: MList EgisonM MatchingState -> EgisonM (MList EgisonM Match)
processMStatesAllDFS MNil = return MNil
processMStatesAllDFS (MCons (MState _ _ [] bindings []) ms) = MCons bindings . processMStatesAllDFS <$> ms
processMStatesAllDFS (MCons mstate ms) = processMState mstate >>= (`mappend` ms) >>= processMStatesAllDFS

processMStatesAllDFSForall :: MList EgisonM MatchingState -> EgisonM (MList EgisonM MatchingState)
processMStatesAllDFSForall MNil = return MNil
processMStatesAllDFSForall (MCons mstate@(MState _ _ (ForallPatContext _ _ : _) _ []) ms) = MCons mstate . processMStatesAllDFSForall <$> ms
processMStatesAllDFSForall (MCons mstate ms) = processMState mstate >>= (`mappend` ms) >>= processMStatesAllDFSForall

processMStatesAll :: [MList EgisonM MatchingState] -> EgisonM (MList EgisonM Match)
processMStatesAll [] = return MNil
processMStatesAll streams = do
  (matches, streams') <- mapM processMStates streams >>= extractMatches . concat
  mappend (fromList matches) $ processMStatesAll streams'

processMStates :: MList EgisonM MatchingState -> EgisonM [MList EgisonM MatchingState]
processMStates MNil = return []
processMStates (MCons state stream) = (\x y -> [x, y]) <$> processMState state <*> stream

extractMatches :: [MList EgisonM MatchingState] -> EgisonM ([Match], [MList EgisonM MatchingState])
extractMatches = extractMatches' ([], [])
 where
  extractMatches' :: ([Match], [MList EgisonM MatchingState]) -> [MList EgisonM MatchingState] -> EgisonM ([Match], [MList EgisonM MatchingState])
  extractMatches' (xs, ys) [] = return (xs, ys)
  extractMatches' (xs, ys) (MCons (gatherBindings -> Just bindings) states : rest) = do
    states' <- states
    extractMatches' (xs ++ [bindings], ys ++ [states']) rest
  extractMatches' (xs, ys) (stream:rest) = extractMatches' (xs, ys ++ [stream]) rest

gatherBindings :: MatchingState -> Maybe [Binding]
gatherBindings mstate@MState{ seqPatCtx = [], mTrees = [] } = return (mStateBindings mstate)
gatherBindings _ = Nothing

processMState :: MatchingState -> EgisonM (MList EgisonM MatchingState)
processMState state =
  if nullMState state
    then processMState' state
    else case splitMState state of
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
             b <- mAny (\s -> case s of
                                MNil -> return True
                                _ -> return False) statess'
             if b
               then return MNil
--               else return MNil
               else do nstatess <- mmap (\states' -> mmap (\(MState e' l' (ForallPatContext [] []:s') b' []) -> return $ MState e' l' s' b' trees) states') statess'
                       mconcat nstatess
           _ -> processMState' state
 where
  nullMState :: MatchingState -> Bool
  nullMState MState{ mTrees = [] } = True
  nullMState MState{ mTrees = MNode _ state : _ } = nullMState state
  nullMState _ = False
  splitMState :: MatchingState -> (Integer, MatchingState, MatchingState)
  splitMState mstate@MState{ mTrees = MAtom (NotPat pattern) target matcher : trees } =
    (1, mstate { mTrees = [MAtom pattern target matcher] }, mstate { mTrees = trees })
  splitMState mstate@MState{ mTrees = MAtom pattern target matcher : trees } =
    (0, mstate { mTrees = [MAtom pattern target matcher] }, mstate { mTrees = trees })
  splitMState mstate@MState{ mTrees = MNode penv state' : trees } =
    (f, mstate { mTrees = [MNode penv state1] }, mstate { mTrees = MNode penv state2 : trees })
      where (f, state1, state2) = splitMState state'

processMState' :: MatchingState -> EgisonM (MList EgisonM MatchingState)
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
      indices' <- mapM (evalExpr env'' >=> fmap fromInteger . fromWHNF) indices
      let pattern' = IndexedPat pattern $ map IntegerExpr indices'
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
        Nothing -> processMState' (MState env loops seqs bindings (MAtom (InductivePat name args) target matcher:trees))
        Just _ -> processMState' (MState env loops seqs bindings (MAtom (PApplyPat (VarExpr (stringToVar name)) args) target matcher:trees))

    NotPat _ -> throwError =<< EgisonBug "should not reach here (not-pattern)" <$> getFuncNameStack
    VarPat _ -> throwError $ Default $ "cannot use variable except in pattern function:" ++ prettyS pattern

    LetPat bindings' pattern' -> do
      b <- fmap concat (mapM extractBindings bindings')
      return . msingleton $ mstate { mStateBindings = b ++ bindings, mTrees = MAtom pattern' target matcher:trees }
        where
          extractBindings ([name], expr) = makeBindings [name] . (:[]) <$> newObjectRef env' expr
          extractBindings (names, expr)  = makeBindings names <$> (evalExpr env' expr >>= fromTuple)

    PredPat predicate -> do
      func <- evalExpr env' predicate
      let arg = target
      result <- applyFunc env func arg >>= fromWHNF
      if result then return . msingleton $ mstate { mTrees = trees }
                else return MNil

    PApplyPat func args -> do
      func' <- evalExpr env' func
      case func' of
        Value (PatternFunc env'' names expr) ->
          return . msingleton $ mstate { mTrees = MNode penv (MState env'' [] [] [] [MAtom expr target matcher]) : trees }
            where penv = zip names args
        _ -> throwError =<< TypeMismatch "pattern constructor" func' <$> getFuncNameStack

    DApplyPat func args ->
      return . msingleton $ mstate { mTrees = MAtom (InductivePat "apply" [func, toListPat args]) target matcher:trees }

    LoopPat name (LoopRange start ends endPat) pat pat' -> do
      startNum    <- evalExpr env' start >>= fromWHNF :: (EgisonM Integer)
      startNumRef <- newEvaluatedObjectRef $ Value $ toEgison (startNum - 1)
      ends'       <- evalExpr env' ends
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
          startNum <- fromWHNF startNumWhnf :: (EgisonM Integer)
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
        (SeqPatContext stack pat mats tgts:seqs) -> return . msingleton $ MState env loops (SeqPatContext stack pat (mats ++ [matcher]) (tgts ++ [target]):seqs) bindings trees
        (ForallPatContext mats tgts:seqs) -> return . msingleton $ MState env loops (ForallPatContext (mats ++ [matcher]) (tgts ++ [target]):seqs) bindings trees
    AndPat patterns ->
      let trees' = map (\pat -> MAtom pat target matcher) patterns ++ trees
       in return . msingleton $ mstate { mTrees = trees' }
    OrPat patterns ->
      return $ fromList $ flip map patterns $ \pat ->
        mstate { mTrees = MAtom pat target matcher : trees }

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
                targets <- evalRef ref >>= fromTupleWHNF
                let trees' = zipWith3 MAtom patterns targets matchers ++ trees
                return $ mstate { mTrees = trees' }

        Tuple matchers ->
          case pattern of
            ValuePat _ -> return . msingleton $ mstate { mTrees = MAtom pattern target Something:trees }
            WildCard   -> return . msingleton $ mstate { mTrees = MAtom pattern target Something:trees }
            PatVar _   -> return . msingleton $ mstate { mTrees = MAtom pattern target Something:trees }
            IndexedPat _ _ -> return . msingleton $ mstate { mTrees = MAtom pattern target Something:trees }
            TuplePat patterns -> do
              targets <- fromTupleWHNF target
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
              indices <- mapM (evalExpr env' >=> fmap fromInteger . fromWHNF) indices
              case lookup name bindings of
                Just ref -> do
                  obj <- evalRef ref >>= updateHash indices >>= newEvaluatedObjectRef
                  return . msingleton $ mstate { mStateBindings = subst name obj bindings, mTrees = trees }
                Nothing  -> do
                  obj <- updateHash indices (Intermediate . IIntHash $ HL.empty) >>= newEvaluatedObjectRef
                  return . msingleton $ mstate { mStateBindings = (name,obj):bindings, mTrees = trees }
               where
                updateHash :: [Integer] -> WHNFData -> EgisonM WHNFData
                updateHash [index] (Intermediate (IIntHash hash)) = do
                  targetRef <- newEvaluatedObjectRef target
                  return . Intermediate . IIntHash $ HL.insert index targetRef hash
                updateHash (index:indices) (Intermediate (IIntHash hash)) = do
                  val <- maybe (return $ Intermediate $ IIntHash HL.empty) evalRef $ HL.lookup index hash
                  ref <- updateHash indices val >>= newEvaluatedObjectRef
                  return . Intermediate . IIntHash $ HL.insert index ref hash
                updateHash indices (Value (IntHash hash)) = do
                  let keys = HL.keys hash
                  vals <- mapM (newEvaluatedObjectRef . Value) $ HL.elems hash
                  updateHash indices (Intermediate $ IIntHash $ HL.fromList $ zip keys vals)
                updateHash _ v = throwError $ Default $ "expected hash value: " ++ show v
                subst :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
                subst k nv ((k', v'):xs) | k == k'   = (k', nv):subst k nv xs
                                         | otherwise = (k', v'):subst k nv xs
                subst _ _ [] = []
            IndexedPat pattern _ -> throwError $ Default ("invalid indexed-pattern: " ++ prettyS pattern)
            TuplePat patterns -> do
              targets <- fromTupleWHNF target
              when (length patterns /= length targets) $ throwError =<< TupleLength (length patterns) (length targets) <$> getFuncNameStack
              let trees' = zipWith3 MAtom patterns targets (replicate (length patterns) Something) ++ trees
              return . msingleton $ mstate { mTrees = trees' }
            _ -> throwError $ Default $ "something can only match with a pattern variable. not: " ++ prettyS pattern
        _ ->  throwError =<< EgisonBug ("should not reach here. matcher: " ++ show matcher ++ ", pattern:  " ++ show pattern) <$> getFuncNameStack

inductiveMatch :: Env -> EgisonPattern -> WHNFData -> Matcher ->
                  EgisonM ([EgisonPattern], MList EgisonM ObjectRef, [Matcher])
inductiveMatch env pattern target (UserMatcher matcherEnv clauses) =
  foldr tryPPMatchClause failPPPatternMatch clauses
 where
  tryPPMatchClause (pat, matchers, clauses) cont = do
    result <- runMaybeT $ primitivePatPatternMatch env pat pattern
    case result of
      Just ([pattern], bindings) -> do
        targetss <- foldr (tryPDMatchClause bindings) failPDPatternMatch clauses
        matcher <- evalExpr matcherEnv matchers >>= evalMatcherWHNF
        return ([pattern], targetss, [matcher])
      Just (patterns, bindings) -> do
        targetss <- foldr (tryPDMatchClause bindings) failPDPatternMatch clauses
        matchers <- fromTupleValue <$> (evalExpr matcherEnv matchers >>= evalMatcherWHNF)
        return (patterns, targetss, matchers)
      _ -> cont
  tryPDMatchClause bindings (pat, expr) cont = do
    result <- runMaybeT $ primitiveDataPatternMatch pat target
    case result of
      Just bindings' -> do
        let env = extendEnv matcherEnv $ bindings ++ bindings'
        evalExpr env expr >>= fromCollection
      _ -> cont
  failPPPatternMatch = throwError $ Default "failed primitive pattern pattern match"
  failPDPatternMatch = throwError $ Default "failed primitive data pattern match"

primitivePatPatternMatch :: Env -> PrimitivePatPattern -> EgisonPattern ->
                            MatchM ([EgisonPattern], [Binding])
primitivePatPatternMatch _ PPWildCard WildCard = return ([], [])
primitivePatPatternMatch _ PPPatVar pattern = return ([pattern], [])
primitivePatPatternMatch env (PPValuePat name) (ValuePat expr) = do
  ref <- lift $ newObjectRef env expr
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

primitiveDataPatternMatch :: PrimitiveDataPattern -> WHNFData -> MatchM [Binding]
primitiveDataPatternMatch PDWildCard _ = return []
primitiveDataPatternMatch (PDPatVar name) whnf = do
  ref <- lift $ newEvaluatedObjectRef whnf
  return [(stringToVar name, ref)]
primitiveDataPatternMatch (PDInductivePat name patterns) whnf =
  case whnf of
    Intermediate (IInductiveData name' refs) | name == name' -> do
      whnfs <- lift $ mapM evalRef refs
      concat <$> zipWithM primitiveDataPatternMatch patterns whnfs
    Value (InductiveData name' vals) | name == name' -> do
      let whnfs = map Value vals
      concat <$> zipWithM primitiveDataPatternMatch patterns whnfs
    _ -> matchFail
primitiveDataPatternMatch (PDTuplePat patterns) whnf =
  case whnf of
    Intermediate (ITuple refs) -> do
      whnfs <- lift $ mapM evalRef refs
      concat <$> zipWithM primitiveDataPatternMatch patterns whnfs
    Value (Tuple vals) -> do
      let whnfs = map Value vals
      concat <$> zipWithM primitiveDataPatternMatch patterns whnfs
    _ -> matchFail
primitiveDataPatternMatch PDEmptyPat whnf = do
  isEmpty <- lift $ isEmptyCollection whnf
  if isEmpty then return [] else matchFail
primitiveDataPatternMatch (PDConsPat pattern pattern') whnf = do
  (head, tail) <- unconsCollection whnf
  head' <- lift $ evalRef head
  tail' <- lift $ evalRef tail
  (++) <$> primitiveDataPatternMatch pattern head'
       <*> primitiveDataPatternMatch pattern' tail'
primitiveDataPatternMatch (PDSnocPat pattern pattern') whnf = do
  (init, last) <- unsnocCollection whnf
  init' <- lift $ evalRef init
  last' <- lift $ evalRef last
  (++) <$> primitiveDataPatternMatch pattern init'
       <*> primitiveDataPatternMatch pattern' last'
primitiveDataPatternMatch (PDConstantPat expr) whnf = do
  target <- either (const matchFail) return $ extractPrimitiveValue whnf
  isEqual <- lift $ (==) <$> evalExprDeep nullEnv expr <*> pure target
  if isEqual then return [] else matchFail
 where
  extractPrimitiveValue :: WHNFData -> Either ([String] -> EgisonError) EgisonValue
  extractPrimitiveValue (Value val@(Char _)) = return val
  extractPrimitiveValue (Value val@(Bool _)) = return val
  extractPrimitiveValue (Value val@(ScalarData _)) = return val
  extractPrimitiveValue (Value val@(Float _)) = return val
  extractPrimitiveValue whnf =
    -- we don't need to extract call stack since detailed error information is not used
    throwError $ TypeMismatch "primitive value" whnf

expandCollection :: WHNFData -> EgisonM (Seq Inner)
expandCollection (Value (Collection vals)) =
  mapM (fmap IElement . newEvaluatedObjectRef . Value) vals
expandCollection (Intermediate (ICollection innersRef)) = liftIO $ readIORef innersRef
expandCollection val = throwError =<< TypeMismatch "collection" val <$> getFuncNameStack

isEmptyCollection :: WHNFData -> EgisonM Bool
isEmptyCollection (Value (Collection col)) = return $ Sq.null col
isEmptyCollection coll@(Intermediate (ICollection innersRef)) = do
  inners <- liftIO $ readIORef innersRef
  case Sq.viewl inners of
    EmptyL -> return True
    ISubCollection ref' :< tInners -> do
      hInners <- evalRef ref' >>= expandCollection
      liftIO $ writeIORef innersRef (hInners >< tInners)
      isEmptyCollection coll
    _ -> return False
isEmptyCollection _ = return False

unconsCollection :: WHNFData -> MatchM (ObjectRef, ObjectRef)
unconsCollection (Value (Collection col)) =
  case Sq.viewl col of
    EmptyL -> matchFail
    val :< vals ->
      lift $ (,) <$> newEvaluatedObjectRef (Value val)
                 <*> newEvaluatedObjectRef (Value $ Collection vals)
unconsCollection coll@(Intermediate (ICollection innersRef)) = do
  inners <- liftIO $ readIORef innersRef
  case Sq.viewl inners of
    EmptyL -> matchFail
    IElement ref' :< tInners -> do
      tInnersRef <- liftIO $ newIORef tInners
      lift $ (ref', ) <$> newEvaluatedObjectRef (Intermediate $ ICollection tInnersRef)
    ISubCollection ref' :< tInners -> do
      hInners <- lift $ evalRef ref' >>= expandCollection
      liftIO $ writeIORef innersRef (hInners >< tInners)
      unconsCollection coll
unconsCollection _ = matchFail

unsnocCollection :: WHNFData -> MatchM (ObjectRef, ObjectRef)
unsnocCollection (Value (Collection col)) =
  case Sq.viewr col of
    EmptyR -> matchFail
    vals :> val ->
      lift $ (,) <$> newEvaluatedObjectRef (Value $ Collection vals)
                 <*> newEvaluatedObjectRef (Value val)
unsnocCollection coll@(Intermediate (ICollection innersRef)) = do
  inners <- liftIO $ readIORef innersRef
  case Sq.viewr inners of
    EmptyR -> matchFail
    hInners :> IElement ref' -> do
      hInnersRef <- liftIO $ newIORef hInners
      lift $ (, ref') <$> newEvaluatedObjectRef (Intermediate $ ICollection hInnersRef)
    hInners :> ISubCollection ref' -> do
      tInners <- lift $ evalRef ref' >>= expandCollection
      liftIO $ writeIORef innersRef (hInners >< tInners)
      unsnocCollection coll
unsnocCollection _ = matchFail

extendEnvForNonLinearPatterns :: Env -> [Binding] -> [LoopPatContext] -> Env
extendEnvForNonLinearPatterns env bindings loops =  extendEnv env $ bindings ++ map (\(LoopPatContext binding _ _ _ _) -> binding) loops

evalMatcherWHNF :: WHNFData -> EgisonM Matcher
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
toListPat :: [EgisonPattern] -> EgisonPattern
toListPat []         = InductivePat "nil" []
toListPat (pat:pats) = InductivePat "cons" [pat, toListPat pats]

fromTuple :: WHNFData -> EgisonM [ObjectRef]
fromTuple (Intermediate (ITuple refs)) = return refs
fromTuple (Value (Tuple vals)) = mapM (newEvaluatedObjectRef . Value) vals
fromTuple whnf = return <$> newEvaluatedObjectRef whnf

fromTupleWHNF :: WHNFData -> EgisonM [WHNFData]
fromTupleWHNF (Intermediate (ITuple refs)) = mapM evalRef refs
fromTupleWHNF (Value (Tuple vals))         = return $ map Value vals
fromTupleWHNF whnf                         = return [whnf]

fromTupleValue :: EgisonValue -> [EgisonValue]
fromTupleValue (Tuple vals) = vals
fromTupleValue val          = [val]

fromCollection :: WHNFData -> EgisonM (MList EgisonM ObjectRef)
fromCollection (Value (Collection vals)) =
  if Sq.null vals then return MNil
                  else fromSeq <$> mapM (newEvaluatedObjectRef . Value) vals
fromCollection whnf@(Intermediate (ICollection _)) = do
  isEmpty <- isEmptyCollection whnf
  if isEmpty
    then return MNil
    else do
      (head, tail) <- fromJust <$> runMaybeT (unconsCollection whnf)
      tail' <- evalRef tail
      return $ MCons head (fromCollection tail')
fromCollection whnf = throwError =<< TypeMismatch "collection" whnf <$> getFuncNameStack

tupleToList :: WHNFData -> EgisonM [EgisonValue]
tupleToList whnf = do
  val <- evalWHNF whnf
  return $ tupleToList' val
 where
  tupleToList' (Tuple vals) = vals
  tupleToList' val          = [val]

collectionToList :: WHNFData -> EgisonM [EgisonValue]
collectionToList whnf = do
  val <- evalWHNF whnf
  collectionToList' val
 where
  collectionToList' :: EgisonValue -> EgisonM [EgisonValue]
  collectionToList' (Collection sq) = return $ toList sq
  collectionToList' val = throwError =<< TypeMismatch "collection" (Value val) <$> getFuncNameStack

makeTuple :: [EgisonValue] -> EgisonValue
makeTuple []  = Tuple []
makeTuple [x] = x
makeTuple xs  = Tuple xs

makeITuple :: [WHNFData] -> EgisonM WHNFData
makeITuple []  = return $ Intermediate (ITuple [])
makeITuple [x] = return x
makeITuple xs  = Intermediate . ITuple <$> mapM newEvaluatedObjectRef xs

-- Refer the specified tensor index with potential overriding of the index.
refTenworWithOverride :: HasTensor a => Bool -> [Index EgisonValue] -> Tensor a -> EgisonM a
refTenworWithOverride override js (Tensor ns xs is) =
  tref js' (Tensor ns xs js') >>= toTensor >>= tContract' >>= fromTensor
    where
      js' = if override then js else is ++ js
