{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE MultiWayIf      #-}

{- |
Module      : Language.Egison.Core
Copyright   : Satoshi Egi
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
    -- * Utiltiy functions
    , packStringValue
    ) where

import           Prelude                     hiding (mapM, mappend, mconcat)

import           Control.Arrow
import           Control.Monad               (when)
import           Control.Monad.Except        hiding (mapM)
import           Control.Monad.State         hiding (mapM)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State   (evalStateT, withStateT)

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

import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Language.Egison.AST
import           Language.Egison.CmdOptions
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
collectDefs _ [] bindings rest = return (bindings, reverse rest)

evalTopExpr' :: EgisonOpts -> StateT [(Var, EgisonExpr)] EgisonM Env -> EgisonTopExpr -> EgisonM (Maybe String, StateT [(Var, EgisonExpr)] EgisonM Env)
evalTopExpr' _ st (Define name expr) = return (Nothing, withStateT (\defines -> (name, expr):defines) st)
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

evalExpr :: Env -> EgisonExpr -> EgisonM WHNFData
evalExpr _ (CharExpr c) = return . Value $ Char c
evalExpr _ (StringExpr s) = return $ Value $ toEgison s
evalExpr _ (BoolExpr b) = return . Value $ Bool b
evalExpr _ (IntegerExpr x) = return . Value $ toEgison x
evalExpr _ (FloatExpr x) = return . Value $ Float x

evalExpr env (QuoteExpr expr) = do
  whnf <- evalExpr env expr
  case whnf of
    Value (ScalarData s) -> return . Value $ ScalarData $ Div (Plus [Term 1 [(Quote s, 1)]]) (Plus [Term 1 []])
    _ -> throwError =<< TypeMismatch "scalar in quote" whnf <$> getFuncNameStack

evalExpr env (QuoteSymbolExpr expr) = do
  whnf <- evalExpr env expr
  case whnf of
    Value val -> return . Value $ QuotedFunc val
    _         -> throwError =<< TypeMismatch "value in quote-function" whnf <$> getFuncNameStack

evalExpr env (VarExpr name) = do
  x <- refVar' env name >>= evalRef
  return (case x of
            Value (ScalarData (Div (Plus [Term 1 [(FunctionData fn argnames args js, 1)]]) p)) ->
              case fn of
                Nothing -> Value $ ScalarData (Div (Plus [Term 1 [(FunctionData (Just $ symbolScalarData "" $ prettyS name) argnames args js, 1)]]) p)
                Just s -> Value $ ScalarData (Div (Plus [Term 1 [(FunctionData fn argnames args js, 1)]]) p)
            _ -> x)
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
    (Intermediate (ITensor Tensor{}):_) ->
      mapM toTensor (zipWith (curry f) whnfs [1..(length exprs + 1)]) >>= tConcat' >>= fromTensor
    _ -> fromTensor (Tensor [fromIntegral $ length whnfs] (V.fromList whnfs) [])
 where
  f (Intermediate (ITensor (Tensor ns xs indices)), i) =
    Intermediate $ ITensor $ Tensor ns (V.fromList $ zipWith (curry g) (V.toList xs) $ map (\ms -> map toEgison $ toInteger i:ms) $ enumTensorIndices ns) indices
  f (x, _) = x
  g (Value (ScalarData (Div (Plus [Term 1 [(FunctionData fn argnames args js, 1)]]) p)), ms) =
    let fn' = maybe fn (\(VarWithIndices nameString indexList) -> Just $ symbolScalarData "" $ prettyS $ VarWithIndices nameString $ changeIndexList indexList ms) maybe_vwi
     in Value $ ScalarData $ Div (Plus [Term 1 [(FunctionData fn' argnames args js, 1)]]) p
  g (x, _) = x

evalExpr env (TensorExpr nsExpr xsExpr supExpr subExpr) = do
  nsWhnf <- evalExpr env nsExpr
  ns <- (fromCollection nsWhnf >>= fromMList >>= mapM evalRef >>= mapM fromWHNF) :: EgisonM [Integer]
  xsWhnf <- evalExpr env xsExpr
  xs <- fromCollection xsWhnf >>= fromMList >>= mapM evalRef
  supWhnf <- evalExpr env supExpr
  sup <- fromCollection supWhnf >>= fromMList >>= mapM evalRefDeep -- >>= mapM extractScalar'
  subWhnf <- evalExpr env subExpr
  sub <- fromCollection subWhnf >>= fromMList >>= mapM evalRefDeep -- >>= mapM extractScalar'
  if product ns == toInteger (length xs)
    then fromTensor (initTensor ns xs sup sub)
    else throwError =<< InconsistentTensorSize <$> getFuncNameStack

evalExpr env (HashExpr assocs) = do
  let (keyExprs, exprs) = unzip assocs
  keyWhnfs <- mapM (evalExpr env) keyExprs
  keys <- mapM makeHashKey keyWhnfs
  refs <- mapM (newObjectRef env) exprs
  case keys of
    [] -> do
      let keys' = map (\case IntKey i -> i) keys
      return . Intermediate . IIntHash $ HL.fromList $ zip keys' refs
    _ ->
     case head keys of
       IntKey _ -> do
         let keys' = map (\ case IntKey i -> i) keys
         return . Intermediate . IIntHash $ HL.fromList $ zip keys' refs
       CharKey _ -> do
         let keys' = map (\case CharKey c -> c) keys
         return . Intermediate . ICharHash $ HL.fromList $ zip keys' refs
       StrKey _ -> do
          let keys' = map (\case StrKey s -> s) keys
          return . Intermediate . IStrHash $ HL.fromList $ zip keys' refs
 where
  makeHashKey :: WHNFData -> EgisonM EgisonHashKey
  makeHashKey (Value val) =
    case val of
      ScalarData _ -> IntKey <$> fromEgison val
      Char c -> return (CharKey c)
      String str -> return (StrKey str)
      _ -> throwError =<< TypeMismatch "integer or string" (Value val) <$> getFuncNameStack
  makeHashKey whnf = throwError =<< TypeMismatch "integer or string" whnf <$> getFuncNameStack

evalExpr env (IndexedExpr bool expr indices) = do
  tensor <- case expr of
              VarExpr (Var xs is) -> do
                let mObjRef = refVar env (Var xs $ is ++ map f indices)
                case mObjRef of
                  (Just objRef) -> evalRef objRef
                  Nothing       -> evalExpr env expr
              _ -> evalExpr env expr
  js <- mapM evalIndex indices
  ret <- case tensor of
      (Value (ScalarData (Div (Plus [Term 1 [(Symbol id name [], 1)]]) (Plus [Term 1 []])))) -> do
        js2 <- mapM evalIndexToScalar indices
        return $ Value (ScalarData (Div (Plus [Term 1 [(Symbol id name js2, 1)]]) (Plus [Term 1 []])))
      (Value (ScalarData (Div (Plus [Term 1 [(Symbol id name js', 1)]]) (Plus [Term 1 []])))) -> do
        js2 <- mapM evalIndexToScalar indices
        return $ Value (ScalarData (Div (Plus [Term 1 [(Symbol id name (js' ++ js2), 1)]]) (Plus [Term 1 []])))
      (Value (TensorData (Tensor ns xs is))) ->
        if bool then Value <$> (tref js (Tensor ns xs js) >>= toTensor >>= tContract' >>= fromTensor)
                else Value <$> (tref (is ++ js) (Tensor ns xs (is ++ js)) >>= toTensor >>= tContract' >>= fromTensor)
      (Intermediate (ITensor (Tensor ns xs is))) ->
        if bool then tref js (Tensor ns xs js) >>= toTensor >>= tContract' >>= fromTensor
                else tref (is ++ js) (Tensor ns xs (is ++ js)) >>= toTensor >>= tContract' >>= fromTensor
      _ -> do
        js2 <- mapM evalIndexToScalar indices
        refArray tensor (map (\case
                                 Superscript k  -> ScalarData k
                                 Subscript k    -> ScalarData k
                                 SupSubscript k -> ScalarData k
                                 Userscript k   -> ScalarData k
                              ) js2)
  let ret2 = case expr of
               (VarExpr var) ->
                 case ret of
                   Value (ScalarData (Div (Plus [Term 1 [(FunctionData fn argnames args js, 1)]]) p)) ->
                     case fn of
                       Nothing -> Value $ ScalarData (Div (Plus [Term 1 [(FunctionData (Just $ symbolScalarData "" $ prettyS var ++ concatMap show indices) argnames args js, 1)]]) p)
                       Just s -> Value $ ScalarData (Div (Plus [Term 1 [(FunctionData fn argnames args js, 1)]]) p)
                   _ -> ret
               _ -> ret
  return ret2
 where
  evalIndex :: Index EgisonExpr -> EgisonM (Index EgisonValue)
  evalIndex = \case
    Superscript n  -> Superscript  <$> evalExprDeep env n
    Subscript n    -> Subscript    <$> evalExprDeep env n
    SupSubscript n -> SupSubscript <$> evalExprDeep env n
    Userscript n   -> Userscript   <$> evalExprDeep env n
  evalIndexToScalar :: Index EgisonExpr -> EgisonM (Index ScalarData)
  evalIndexToScalar = \case
    Superscript n  -> Superscript  <$> (evalExprDeep env n >>= extractScalar)
    Subscript n    -> Subscript    <$> (evalExprDeep env n >>= extractScalar)
    SupSubscript n -> SupSubscript <$> (evalExprDeep env n >>= extractScalar)
    Userscript n   -> Userscript   <$> (evalExprDeep env n >>= extractScalar)

  f :: Index a -> Index ()
  f (Superscript _)  = Superscript ()
  f (Subscript _)    = Subscript ()
  f (SupSubscript _) = SupSubscript ()
  f (Userscript _)   = Userscript ()

evalExpr env (SubrefsExpr bool expr jsExpr) = do
  js <- map Subscript <$> (evalExpr env jsExpr >>= collectionToList)
  tensor <- case expr of
              VarExpr (Var xs is) -> do
                let mObjRef = refVar env (Var xs $ is ++ replicate (length js) (Subscript ()))
                case mObjRef of
                  (Just objRef) -> evalRef objRef
                  Nothing       -> evalExpr env expr
              _ -> evalExpr env expr
  case tensor of
    (Value (ScalarData _)) ->
      return tensor
    (Value (TensorData (Tensor ns xs is))) ->
      if bool then Value <$> (tref js (Tensor ns xs js) >>= toTensor >>= tContract' >>= fromTensor)
              else Value <$> (tref (is ++ js) (Tensor ns xs (is ++ js)) >>= toTensor >>= tContract' >>= fromTensor)
    (Intermediate (ITensor (Tensor ns xs is))) ->
      if bool then tref js (Tensor ns xs js) >>= toTensor >>= tContract' >>= fromTensor
              else tref (is ++ js) (Tensor ns xs (is ++ js)) >>= toTensor >>= tContract' >>= fromTensor
    _ -> throwError =<< NotImplemented "subrefs" <$> getFuncNameStack

evalExpr env (SuprefsExpr bool expr jsExpr) = do
  js <- map Superscript <$> (evalExpr env jsExpr >>= collectionToList)
  tensor <- case expr of
              VarExpr (Var xs is) -> do
                let mObjRef = refVar env (Var xs $ is ++ replicate (length js) (Superscript ()))
                case mObjRef of
                  (Just objRef) -> evalRef objRef
                  Nothing       -> evalExpr env expr
              _ -> evalExpr env expr
  case tensor of
    (Value (ScalarData _)) ->
      return tensor
    (Value (TensorData (Tensor ns xs is))) ->
      if bool then Value <$> (tref js (Tensor ns xs js) >>= toTensor >>= tContract' >>= fromTensor)
              else Value <$> (tref (is ++ js) (Tensor ns xs (is ++ js)) >>= toTensor >>= tContract' >>= fromTensor)
    (Intermediate (ITensor (Tensor ns xs is))) ->
      if bool then tref js (Tensor ns xs js) >>= toTensor >>= tContract' >>= fromTensor
              else tref (is ++ js) (Tensor ns xs (is ++ js)) >>= toTensor >>= tContract' >>= fromTensor
    _ -> throwError =<< NotImplemented "suprefs" <$> getFuncNameStack

evalExpr env (UserrefsExpr bool expr jsExpr) = do
  val <- evalExprDeep env expr
  js <- map Userscript <$> (evalExpr env jsExpr >>= collectionToList >>= mapM extractScalar)
  case val of
    (ScalarData (Div (Plus [Term 1 [(Symbol id name is, 1)]]) (Plus [Term 1 []]))) -> return $ Value (ScalarData (Div (Plus [Term 1 [(Symbol id name (is ++ js), 1)]]) (Plus [Term 1 []])))
    (ScalarData (Div (Plus [Term 1 [(FunctionData (Just name) argnames args is, 1)]]) (Plus [Term 1 []]))) -> return $ Value (ScalarData (Div (Plus [Term 1 [(FunctionData (Just name) argnames args (is ++ js), 1)]]) (Plus [Term 1 []])))
    _ -> throwError =<< NotImplemented "user-refs" <$> getFuncNameStack

evalExpr env (LambdaExpr names expr) = do
  names' <- mapM (\case
                     (TensorArg name') -> return name'
                     (ScalarArg _) -> throwError =<< EgisonBug "scalar-arg remained" <$> getFuncNameStack) names
  return . Value $ Func Nothing env names' expr

evalExpr env (PartialExpr n expr) = return . Value $ PartialFunc env n expr

evalExpr env (CambdaExpr name expr) = return . Value $ CFunc Nothing env name expr

evalExpr env (ProcedureExpr names expr) = return . Value $ Proc Nothing env names expr

evalExpr env (MacroExpr names expr) = return . Value $ Macro names expr

evalExpr env (PatternFunctionExpr names pattern) = return . Value $ PatternFunc env names pattern

evalExpr (Env frame Nothing) (FunctionExpr args) = throwError $ Default "function symbol is not bound to a variable"

evalExpr env@(Env frame (Just name)) (FunctionExpr args) = do
  args' <- mapM (evalExprDeep env) args
  return . Value $ ScalarData (Div (Plus [Term 1 [(FunctionData (Just $ symbolScalarData "" $ prettyS name) (map (symbolScalarData "" . prettyS) args) args' [], 1)]]) (Plus [Term 1 []]))

evalExpr env (IfExpr test expr expr') = do
  test <- evalExpr env test >>= fromWHNF
  evalExpr env $ if test then expr else expr'

evalExpr env (LetExpr bindings expr) =
  mapM extractBindings bindings >>= flip evalExpr expr . extendEnv env . concat
 where
  extractBindings :: BindingExpr -> EgisonM [Binding]
  extractBindings ([name], expr) =
    case expr of
      FunctionExpr args -> let Env frame _ = env in makeBindings [name] . (:[]) <$> newObjectRef (Env frame (Just $ varToVarWithIndices name)) expr
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
    (Intermediate (ITensor t)) -> do
      t' <- tTranspose' syms t
      return (Intermediate (ITensor t'))
    (Value (TensorData t)) -> do
      t' <- tTranspose' syms t
      return (Value (TensorData t'))
    _ -> return whnf

evalExpr env (FlipIndicesExpr expr) = do
  whnf <- evalExpr env expr
  case whnf of
    (Intermediate (ITensor t)) -> do
      t' <- tFlipIndices t
      return (Intermediate (ITensor t'))
    (Value (TensorData t)) -> do
      t' <- tFlipIndices t
      return (Value (TensorData t'))
    _ -> return whnf

evalExpr env (WithSymbolsExpr vars expr) = do
  symId <- fresh
  syms <- mapM (newEvaluatedObjectRef . Value . symbolScalarData symId) vars
  let bindings = zip (map stringToVar vars) syms
  whnf <- evalExpr (extendEnv env bindings) expr
  case whnf of
    (Value (TensorData (Tensor ns xs js))) ->
      removeTmpscripts symId (Value (TensorData (Tensor ns xs js)))
    (Intermediate (ITensor (Tensor ns xs js))) ->
      removeTmpscripts symId (Intermediate (ITensor (Tensor ns xs js)))
    _ -> return whnf
 where
  isTmpSymbol :: String -> Index EgisonValue -> Bool
  isTmpSymbol symId (Subscript (ScalarData (Div (Plus [Term 1 [(Symbol id name is,n)]]) (Plus [Term 1 []])))) = symId == id
  isTmpSymbol symId (Superscript (ScalarData (Div (Plus [Term 1 [(Symbol id name is,n)]]) (Plus [Term 1 []])))) = symId == id
  isTmpSymbol symId (SupSubscript (ScalarData (Div (Plus [Term 1 [(Symbol id name is,n)]]) (Plus [Term 1 []])))) = symId == id
  isTmpSymbol symId (Userscript (ScalarData (Div (Plus [Term 1 [(Symbol id name is,n)]]) (Plus [Term 1 []])))) = symId == id
  removeTmpscripts :: String -> WHNFData -> EgisonM WHNFData
  removeTmpscripts symId (Intermediate (ITensor (Tensor s xs is))) = do
    let (ds, js) = partition (isTmpSymbol symId) is
    (Tensor s ys _) <- tTranspose (js ++ ds) (Tensor s xs is)
    return (Intermediate (ITensor (Tensor s ys js)))
  removeTmpscripts symId (Value (TensorData (Tensor s xs is))) = do
    let (ds, js) = partition (isTmpSymbol symId) is
    (Tensor s ys _) <- tTranspose (js ++ ds) (Tensor s xs is)
    return (Value (TensorData (Tensor s ys js)))
  removeDFscripts _ = return


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
            mmap (flip evalExpr expr . extendEnv env) result >>= flip mappend results
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
  evalExprDeep env expr1
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
  arg <- evalExpr env arg
  case func of
    Value (TensorData t@(Tensor ns fs js)) ->
      Value <$> (tMap (\f -> applyFunc env (Value f) arg >>= evalWHNF) t >>= fromTensor) >>= removeDFscripts
    Intermediate (ITensor t@(Tensor ns fs js)) ->
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

evalExpr env (WedgeApplyExpr func arg) = do
  func <- evalExpr env func >>= appendDFscripts 0
  arg <- evalExpr env arg >>= fromTupleWHNF
  let k = fromIntegral (length arg)
  arg <- zipWithM appendDFscripts [1..k] arg >>= makeITuple
  case func of
    Value (TensorData t@(Tensor ns fs js)) ->
      Value <$> (tMap (\f -> applyFunc env (Value f) arg >>= evalWHNF) t >>= fromTensor)
    Intermediate (ITensor t@(Tensor ns fs js)) ->
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
                            (MemoizedFunc name ref hashRef env' names body) -> do
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

evalExpr env (GenerateTensorExpr fnExpr sizeExpr) = do
  size' <- evalExpr env sizeExpr
  size'' <- collectionToList size'
  ns <- mapM fromEgison size'' :: EgisonM [Integer]
  let Env frame maybe_vwi = env
  xs <- mapM ((\ms -> do
    let env' = maybe env (\(VarWithIndices nameString indexList) -> Env frame $ Just $ VarWithIndices nameString $ changeIndexList indexList ms) maybe_vwi
    fn <- evalExpr env' fnExpr
    applyFunc env fn $ Value $ makeTuple ms)
                . map toEgison) (enumTensorIndices ns)
  fromTensor (Tensor ns (V.fromList xs) [])

evalExpr env (TensorContractExpr fnExpr tExpr) = do
  fn <- evalExpr env fnExpr
  whnf <- evalExpr env tExpr
  case whnf of
    (Intermediate (ITensor t@Tensor{})) -> do
      ts <- tContract t
      tMapN (\xs -> do xs' <- mapM newEvaluatedObjectRef xs
                       applyFunc env fn (Intermediate (ITuple xs'))) ts >>= fromTensor
    (Value (TensorData t@Tensor{})) -> do
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
  if length s1 > length i1 && all (\(Intermediate (ITensor (Tensor s u i))) -> length s - length i == 1) tds
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
  if length s1 > length i1 && all (\(Intermediate (ITensor (Tensor s u i))) -> length s - length i == 1) tds
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
applyFunc env (Value (Macro [name] body)) arg = do
  ref <- newEvaluatedObjectRef arg
  evalExpr (extendEnv env $ makeBindings' [name] [ref]) body
applyFunc env (Value (Macro names body)) arg = do
  refs <- fromTuple arg
  if length names == length refs
    then evalExpr (extendEnv env $ makeBindings' names refs) body
    else throwError =<< ArgumentsNumWithNames names (length names) (length refs) <$> getFuncNameStack
applyFunc _ (Value (PrimitiveFunc _ func)) arg = func arg
applyFunc _ (Value (IOFunc m)) arg =
  case arg of
     Value World -> m
     _           -> throwError =<< TypeMismatch "world" arg <$> getFuncNameStack
applyFunc _ (Value (QuotedFunc fn)) arg = do
  args <- tupleToList arg
  mExprs <- mapM extractScalar args
  return (Value (ScalarData (Div (Plus [Term 1 [(Apply fn mExprs, 1)]]) (Plus [Term 1 []]))))
applyFunc _ (Value fn@(ScalarData (Div (Plus [Term 1 [(Symbol{}, 1)]]) (Plus [Term 1 []])))) arg = do
  args <- tupleToList arg
  mExprs <- mapM (\arg -> case arg of
                            ScalarData _ -> extractScalar arg
                            _ -> throwError =<< EgisonBug "to use undefined functions, you have to use ScalarData args" <$> getFuncNameStack) args
  return (Value (ScalarData (Div (Plus [Term 1 [(Apply fn mExprs, 1)]]) (Plus [Term 1 []]))))
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
           (ScalarData (Div (Plus [Term 1 [(Symbol _ _ [], 1)]]) (Plus [Term 1 []]))) -> do
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
           (ScalarData (Div (Plus [Term 1 [(Symbol _ _ [], 1)]]) (Plus [Term 1 []]))) -> do
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
  let (names, exprs) = unzip bindings
  refs <- replicateM (length bindings) $ newObjectRef nullEnv UndefinedExpr
  let env' = extendEnv env $ makeBindings names refs
  let Env frame _ = env'
  zipWithM_ (\ref (name,expr) ->
               case expr of
                 MemoizedLambdaExpr names body -> do
                   hashRef <- liftIO $ newIORef HL.empty
                   liftIO . writeIORef ref . WHNF . Value $ MemoizedFunc (Just name) ref hashRef env' names body
                 LambdaExpr args body -> do
                   whnf <- evalExpr env' expr
                   case whnf of
                     (Value (Func _ env args body)) -> liftIO . writeIORef ref . WHNF $ Value (Func (Just name) env args body)
                 CambdaExpr arg body -> do
                   whnf <- evalExpr env' expr
                   case whnf of
                     (Value (CFunc _ env arg body)) -> liftIO . writeIORef ref . WHNF $ Value (CFunc (Just name) env arg body)
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
                  LambdaExpr args body -> do
                    whnf <- evalExpr env expr
                    case whnf of
                      (Value (Func _ env args body)) -> liftIO . writeIORef ref . WHNF $ Value (Func (Just name) env args body)
                  CambdaExpr arg body -> do
                    whnf <- evalExpr env expr
                    case whnf of
                      (Value (CFunc _ env arg body)) -> liftIO . writeIORef ref . WHNF $ Value (CFunc (Just name) env arg body)
                  _ -> liftIO . writeIORef ref . Thunk $ evalExpr env expr
  return env

--
-- Pattern Match
--

patternMatch :: PMMode -> Env -> EgisonPattern -> WHNFData -> Matcher -> EgisonM (MList EgisonM Match)
patternMatch DFSMode env pattern target matcher = processMStatesAllDFS (msingleton $ MState env [] [] [] [MAtom pattern target matcher])
patternMatch BFSMode env pattern target matcher = processMStatesAll [msingleton $ MState env [] [] [] [MAtom pattern target matcher]]

processMStatesAllDFS :: (MList EgisonM MatchingState) -> EgisonM (MList EgisonM Match)
processMStatesAllDFS MNil = return MNil
processMStatesAllDFS (MCons (MState _ _ [] bindings []) ms) = MCons bindings <$> (processMStatesAllDFS <$> ms)
processMStatesAllDFS (MCons mstate ms) = processMState mstate >>= (flip mappend) ms >>= processMStatesAllDFS

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
  extractMatches' (xs, ys) ((MCons (gatherBindings -> Just bindings) states):rest) = do
    states' <- states
    extractMatches' (xs ++ [bindings], ys ++ [states']) rest
  extractMatches' (xs, ys) (stream:rest) = extractMatches' (xs, ys ++ [stream]) rest

gatherBindings :: MatchingState -> Maybe [Binding]
gatherBindings (MState _ _ [] bindings []) = return bindings
gatherBindings _ = Nothing

topMAtom :: MatchingState -> Maybe MatchingTree
topMAtom (MState _ _ _ _ (mAtom@MAtom{}:_))  = Just mAtom
topMAtom (MState _ _ _ _ (MNode _ mstate:_)) = topMAtom mstate
topMAtom _ = Nothing

processMState :: MatchingState -> EgisonM (MList EgisonM MatchingState)
processMState state =
  case topMAtom state of
    Just (MAtom (NotPat _) _ _) -> do
      let (state1, state2) = splitMState state
      result <- processMStatesAll [msingleton state1]
      case result of
        MNil -> return $ msingleton state2
        _    -> return MNil
    _ -> processMState' state
 where
  splitMState :: MatchingState -> (MatchingState, MatchingState)
  splitMState (MState env loops seqs bindings (MAtom (NotPat pattern) target matcher : trees)) =
    (MState env loops seqs bindings [MAtom pattern target matcher], MState env loops seqs bindings trees)
  splitMState (MState env loops seqs bindings (MNode penv state' : trees)) =
    let (state1, state2) = splitMState state'
     in (MState env loops seqs bindings [MNode penv state1], MState env loops seqs bindings (MNode penv state2 : trees))

processMState' :: MatchingState -> EgisonM (MList EgisonM MatchingState)
processMState' (MState _ _ [] _ []) = throwError =<< EgisonBug "should not reach here (empty matching-state)" <$> getFuncNameStack

-- Sequential patterns
processMState' (MState env loops (SeqPatContext stack SeqNilPat [] []:seqs) bindings []) = return $ msingleton $ MState env loops seqs bindings stack
processMState' (MState env loops (SeqPatContext stack seqPat mats tgts:seqs) bindings []) = do
  let mat' = makeTuple mats
  tgt' <- makeITuple tgts
  return $ msingleton $ MState env loops seqs bindings (MAtom seqPat tgt' mat' : stack)

-- Matching Nodes
processMState' (MState _ _ _ _ (MNode _ (MState _ _ _ [] []):_)) = throwError =<< EgisonBug "should not reach here (empty matching-node)" <$> getFuncNameStack

processMState' (MState env loops seqs bindings (MNode penv (MState env' loops' seqs' bindings' ((MAtom (VarPat name) target matcher):trees')):trees)) =
  case lookup name penv of
    Just pattern ->
      case trees' of
        [] -> return $ msingleton $ MState env loops seqs bindings ((MAtom pattern target matcher):trees)
        _ -> return $ msingleton $ MState env loops seqs bindings (MAtom pattern target matcher:MNode penv (MState env' loops' seqs' bindings' trees'):trees)
    Nothing -> throwError =<< UnboundVariable name <$> getFuncNameStack

processMState' (MState env loops seqs bindings (MNode penv (MState env' loops' seqs' bindings' (MAtom (IndexedPat (VarPat name) indices) target matcher:trees')):trees)) =
  case lookup name penv of
    Just pattern -> do
      let env'' = extendEnvForNonLinearPatterns env' bindings loops'
      indices' <- mapM (evalExpr env'' >=> fmap fromInteger . fromWHNF) indices
      let pattern' = IndexedPat pattern $ map IntegerExpr indices'
      case trees' of
        [] -> return $ msingleton $ MState env loops seqs bindings (MAtom pattern' target matcher:trees)
        _ -> return $ msingleton $ MState env loops seqs bindings (MAtom pattern' target matcher:MNode penv (MState env' loops' seqs' bindings' trees'):trees)
    Nothing -> throwError =<< UnboundVariable name <$> getFuncNameStack

processMState' (MState env loops seqs bindings (MNode penv state:trees)) =
  processMState' state >>= mmap (\state' -> case state' of
                                              MState _ _ _ _ [] -> return $ MState env loops seqs bindings trees
                                              _ -> return $ MState env loops seqs bindings (MNode penv state':trees))

-- Matching Atoms
processMState' (MState env loops seqs bindings (MAtom pattern target matcher:trees)) =
  let env' = extendEnvForNonLinearPatterns env bindings loops in
  case pattern of
    NotPat _ -> throwError =<< EgisonBug "should not reach here (not pattern)" <$> getFuncNameStack
    VarPat _ -> throwError $ Default $ "cannot use variable except in pattern function:" ++ prettyS pattern

    LetPat bindings' pattern' ->
      let extractBindings ([name], expr) =
            makeBindings [name] . (:[]) <$> newObjectRef env' expr
          extractBindings (names, expr) =
            makeBindings names <$> (evalExpr env' expr >>= fromTuple)
      in
       fmap concat (mapM extractBindings bindings')
         >>= (\b -> return $ msingleton $ MState env loops seqs (b ++ bindings) (MAtom pattern' target matcher:trees))
    PredPat predicate -> do
      func <- evalExpr env' predicate
      let arg = target
      result <- applyFunc env func arg >>= fromWHNF
      if result then return $ msingleton $ MState env loops seqs bindings trees
                else return MNil

    PApplyPat func args -> do
      func' <- evalExpr env' func
      case func' of
        Value (PatternFunc env'' names expr) ->
          let penv = zip names args
          in return $ msingleton $ MState env loops seqs bindings (MNode penv (MState env'' [] [] [] [MAtom expr target matcher]) : trees)
        _ -> throwError =<< TypeMismatch "pattern constructor" func' <$> getFuncNameStack

    DApplyPat func args ->
      return $ msingleton $ MState env loops seqs bindings (MAtom (InductivePat "apply" [func, toListPat args]) target matcher:trees)

    LoopPat name (LoopRange start ends endPat) pat pat' -> do
      startNum <- evalExpr env' start >>= fromWHNF :: (EgisonM Integer)
      startNumRef <- newEvaluatedObjectRef $ Value $ toEgison (startNum - 1)
      ends' <- evalExpr env' ends
      case ends' of
        Value (ScalarData _) -> do -- the case when the end numbers are an integer
          endsRef <- newEvaluatedObjectRef ends'
          inners <- liftIO $ newIORef $ Sq.fromList [IElement endsRef]
          endsRef' <- liftIO $ newIORef (WHNF (Intermediate (ICollection inners)))
          return $ msingleton $ MState env (LoopPatContext (name, startNumRef) endsRef' endPat pat pat':loops) seqs bindings (MAtom ContPat target matcher:trees)
        _ -> do -- the case when the end numbers are a collection
          endsRef <- newEvaluatedObjectRef ends'
          return $ msingleton $ MState env (LoopPatContext (name, startNumRef) endsRef endPat pat pat':loops) seqs bindings (MAtom ContPat target matcher:trees)
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
                  fromList [MState env loops' seqs bindings (MAtom endPat startNumWhnf Something:MAtom pat' target matcher:trees)]
                | startNum == carEndsNum ->
                  fromList [MState env loops' seqs bindings (MAtom endPat startNumWhnf Something:MAtom pat' target matcher:trees),
                            MState env (LoopPatContext (name, nextNumRef) cdrEndsRef endPat pat pat':loops') seqs bindings (MAtom pat target matcher:trees)]
                | otherwise ->
                  fromList [MState env (LoopPatContext (name, nextNumRef) endsRef endPat pat pat':loops') seqs bindings (MAtom pat target matcher:trees)]
    SeqNilPat -> throwError =<< EgisonBug "should not reach here (seq nil pattern)" <$> getFuncNameStack
    SeqConsPat pattern pattern' -> return $ msingleton $ MState env loops (SeqPatContext trees pattern' [] []:seqs) bindings [MAtom pattern target matcher]
    LaterPatVar ->
      case seqs of
        [] -> throwError $ Default "cannot use # out of seq patterns"
        (SeqPatContext stack pat mats tgts:seqs) -> return $ msingleton $ MState env loops (SeqPatContext stack pat (mats ++ [matcher]) (tgts ++ [target]):seqs) bindings trees
    AndPat patterns ->
      let trees' = map (\pat -> MAtom pat target matcher) patterns ++ trees
      in return $ msingleton $ MState env loops seqs bindings trees'
    OrPat patterns ->
      return $ fromList $ flip map patterns $ \pat ->
        MState env loops seqs bindings (MAtom pat target matcher : trees)

    _ ->
      case matcher of
        UserMatcher{} -> do
          (patterns, targetss, matchers) <- inductiveMatch env' pattern target matcher
          case length patterns of
            1 ->
              mfor targetss $ \ref -> do
                targets <- evalRef ref >>= (\x -> return [x])
                let trees' = zipWith3 MAtom patterns targets matchers ++ trees
                return $ MState env loops seqs bindings trees'
            _ ->
              mfor targetss $ \ref -> do
                targets <- evalRef ref >>= fromTupleWHNF
                let trees' = zipWith3 MAtom patterns targets matchers ++ trees
                return $ MState env loops seqs bindings trees'

        Tuple matchers ->
          case pattern of
            ValuePat _ -> return $ msingleton $ MState env loops seqs bindings (MAtom pattern target Something:trees)
            WildCard -> return $ msingleton $ MState env loops seqs bindings (MAtom pattern target Something:trees)
            PatVar _ -> return $ msingleton $ MState env loops seqs bindings (MAtom pattern target Something:trees)
            IndexedPat _ _ -> return $ msingleton $ MState env loops seqs bindings (MAtom pattern target Something:trees)
            TuplePat patterns -> do
              targets <- fromTupleWHNF target
              when (length patterns /= length targets) $ throwError =<< TupleLength (length patterns) (length targets) <$> getFuncNameStack
              when (length patterns /= length matchers) $ throwError =<< TupleLength (length patterns) (length matchers) <$> getFuncNameStack
              let trees' = zipWith3 MAtom patterns targets matchers ++ trees
              return $ msingleton $ MState env loops seqs bindings trees'
            _ ->  throwError $ Default $ "should not reach here. matcher: " ++ show matcher ++ ", pattern:  " ++ show pattern

        Something ->
          case pattern of
            ValuePat valExpr -> do
              val <- evalExprDeep env' valExpr
              tgtVal <- evalWHNF target
              if val == tgtVal
                then return $ msingleton $ MState env loops seqs bindings trees
                else return MNil
            WildCard -> return $ msingleton $ MState env loops seqs bindings trees
            PatVar name -> do
              targetRef <- newEvaluatedObjectRef target
              return $ msingleton $ MState env loops seqs ((name, targetRef):bindings) trees
            IndexedPat (PatVar name) indices -> do
              indices <- mapM (evalExpr env' >=> fmap fromInteger . fromWHNF) indices
              case lookup name bindings of
                Just ref -> do
                  obj <- evalRef ref >>= updateHash indices >>= newEvaluatedObjectRef
                  return $ msingleton $ MState env loops seqs (subst name obj bindings) trees
                Nothing  -> do
                  obj <- updateHash indices (Intermediate . IIntHash $ HL.empty) >>= newEvaluatedObjectRef
                  return $ msingleton $ MState env loops seqs ((name,obj):bindings) trees
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
            IndexedPat pattern indices -> throwError $ Default ("invalid indexed-pattern: " ++ prettyS pattern)
            TuplePat patterns -> do
              targets <- fromTupleWHNF target
              when (length patterns /= length targets) $ throwError =<< TupleLength (length patterns) (length targets) <$> getFuncNameStack
              let trees' = zipWith3 MAtom patterns targets (replicate (length patterns) Something) ++ trees
              return $ msingleton $ MState env loops seqs bindings trees'
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
  target <- (either (const matchFail) return) $ extractPrimitiveValue whnf
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

--
-- String
--
packStringValue :: EgisonValue -> EgisonM Text
packStringValue (Collection seq) = do
  let ls = toList seq
  str <- mapM (\val -> case val of
                         Char c -> return c
                         _ -> throwError =<< TypeMismatch "char" (Value val) <$> getFuncNameStack)
              ls
  return $ T.pack str
packStringValue (Tuple [val]) = packStringValue val
packStringValue val = throwError =<< TypeMismatch "string" (Value val) <$> getFuncNameStack
