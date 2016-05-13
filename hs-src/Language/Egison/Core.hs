{-# Language TupleSections, ViewPatterns #-}

{- |
Module      : Language.Egison.Core
Copyright   : Satoshi Egi
Licence     : MIT

This module provides functions to evaluate various objects.
-}

module Language.Egison.Core
    (
    -- * Egison code evaluation
      evalTopExprs
    , evalTopExprsTestOnly
    , evalTopExprsNoIO
    , evalTopExpr
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

import Prelude hiding (mapM, mappend)

import Control.Arrow
import Control.Applicative
import Control.Monad.Error hiding (mapM)
import Control.Monad.State hiding (mapM, state)
import Control.Monad.Trans.Maybe

import Data.Sequence (Seq, ViewL(..), ViewR(..), (><))
import qualified Data.Sequence as Sq
import Data.Ratio
import Data.Foldable (toList)
import Data.Traversable (mapM)
import Data.IORef
import Data.Maybe

import qualified Data.HashMap.Lazy as HL
import Data.Array ((!))
import qualified Data.Array as Array
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.Text (Text)
import qualified Data.Text as T

import Language.Egison.Types
import Language.Egison.Parser

--
-- Evaluator
--

evalTopExprs :: Env -> [EgisonTopExpr] -> EgisonM Env
evalTopExprs env exprs = do
  (bindings, rest) <- collectDefs exprs [] []
  env <- recursiveBind env bindings
  forM_ rest $ evalTopExpr env
  return env
 where
  collectDefs :: [EgisonTopExpr] -> [(String, EgisonExpr)] -> [EgisonTopExpr] -> EgisonM ([(String, EgisonExpr)], [EgisonTopExpr])
  collectDefs (expr:exprs) bindings rest =
    case expr of
      Define name expr -> collectDefs exprs ((name, expr) : bindings) rest
      Load file -> do
        exprs' <- loadLibraryFile file
        collectDefs (exprs' ++ exprs) bindings rest
      LoadFile file -> do
        exprs' <- loadFile file
        collectDefs (exprs' ++ exprs) bindings rest
      Execute _ -> collectDefs exprs bindings (expr : rest)
      _ -> collectDefs exprs bindings rest
  collectDefs [] bindings rest = return (bindings, reverse rest)

evalTopExprsTestOnly :: Env -> [EgisonTopExpr] -> EgisonM Env
evalTopExprsTestOnly env exprs = do
  (bindings, rest) <- collectDefs exprs [] []
  env <- recursiveBind env bindings
  forM_ rest $ evalTopExpr env
  return env
 where
  collectDefs :: [EgisonTopExpr] -> [(String, EgisonExpr)] -> [EgisonTopExpr] -> EgisonM ([(String, EgisonExpr)], [EgisonTopExpr])
  collectDefs (expr:exprs) bindings rest =
    case expr of
      Define name expr -> collectDefs exprs ((name, expr) : bindings) rest
      Load file -> do
        exprs' <- loadLibraryFile file
        collectDefs (exprs' ++ exprs) bindings rest
      LoadFile file -> do
        exprs' <- loadFile file
        collectDefs (exprs' ++ exprs) bindings rest
      Test _ -> collectDefs exprs bindings (expr : rest)
      _ -> collectDefs exprs bindings rest
  collectDefs [] bindings rest = return (bindings, reverse rest)

evalTopExprsNoIO :: Env -> [EgisonTopExpr] -> EgisonM Env
evalTopExprsNoIO env exprs = do
  (bindings, rest) <- collectDefs exprs [] []
  env <- recursiveBind env bindings
  forM_ rest $ evalTopExpr env
  return env
 where
  collectDefs :: [EgisonTopExpr] -> [(String, EgisonExpr)] -> [EgisonTopExpr] -> EgisonM ([(String, EgisonExpr)], [EgisonTopExpr])
  collectDefs (expr:exprs) bindings rest =
    case expr of
      Define name expr -> collectDefs exprs ((name, expr) : bindings) rest
      Load _ -> throwError $ strMsg "No IO support"
      LoadFile _ -> throwError $ strMsg "No IO support"
      _ -> collectDefs exprs bindings (expr : rest)
  collectDefs [] bindings rest = return (bindings, reverse rest)

evalTopExpr :: Env -> EgisonTopExpr -> EgisonM Env
evalTopExpr env topExpr = do
  ret <- evalTopExpr' env topExpr
  case fst ret of
    Nothing -> return ()
    Just output -> liftIO $ putStrLn output
  return $ snd ret

evalTopExpr' :: Env -> EgisonTopExpr -> EgisonM (Maybe String, Env)
evalTopExpr' env (Define name expr) = recursiveBind env [(name, expr)] >>= return . ((,) Nothing)
evalTopExpr' env (Test expr) = do
  val <- evalExprDeep env expr
  return (Just (show val), env)
evalTopExpr' env (Execute expr) = do
  io <- evalExpr env expr
  case io of
    Value (IOFunc m) -> m >> return (Nothing, env)
    _ -> throwError $ TypeMismatch "io" io
evalTopExpr' env (Load file) = loadLibraryFile file >>= evalTopExprs env >>= return . ((,) Nothing)
evalTopExpr' env (LoadFile file) = loadFile file >>= evalTopExprs env >>= return . ((,) Nothing)

evalExpr :: Env -> EgisonExpr -> EgisonM WHNFData
evalExpr _ (CharExpr c) = return . Value $ Char c
evalExpr _ (StringExpr s) = return $ Value $ toEgison s
evalExpr _ (BoolExpr b) = return . Value $ Bool b
evalExpr _ (IntegerExpr x) = return . Value $ toEgison x
evalExpr _ (FloatExpr x y) = return . Value $ Float x y

evalExpr env (QuoteExpr expr) = do
  whnf <- evalExpr env expr
  case whnf of
    Value val -> return . Value $ Quote val
    _ -> throwError $ TypeMismatch "value in quote" $ whnf

evalExpr env (VarExpr name) = refVar' env name >>= evalRef
 where
  refVar' :: Env -> Var -> EgisonM ObjectRef
  refVar' env var = maybe (newEvalutedObjectRef (Value (symbolScalarData var))) return
                          (refVar env var)

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

evalExpr env (TensorExpr nsExpr xsExpr) = do
  nsWhnf <- evalExpr env nsExpr
  ns <- ((fromCollection nsWhnf >>= fromMList >>= mapM evalRef >>= mapM fromWHNF) :: EgisonM [Integer])
  xsWhnf <- evalExpr env xsExpr
  xs <- fromCollection xsWhnf >>= fromMList >>= mapM evalRef >>= mapM toScalarData
  if product ns == toInteger (length xs)
    then return $ Value $ TensorData (makeTensor ns xs Nothing)
    else throwError $ InconsistentTensorSize
 where
  toScalarData :: WHNFData -> EgisonM ScalarData
  toScalarData (Value (ScalarData x)) = return x
  toScalarData val = throwError $ TypeMismatch "integer or string" $ val

evalExpr env (InitTensorExpr nsExpr xsExpr supExpr subExpr) = do
  nsWhnf <- evalExpr env nsExpr
  ns <- ((fromCollection nsWhnf >>= fromMList >>= mapM evalRef >>= mapM fromWHNF) :: EgisonM [Integer])
  xsWhnf <- evalExpr env xsExpr
  xs <- fromCollection xsWhnf >>= fromMList >>= mapM evalRef >>= mapM toScalarData
  supWhnf <- evalExpr env supExpr
  sup <- fromCollection supWhnf >>= fromMList >>= mapM evalRef >>= mapM toScalarData
  subWhnf <- evalExpr env subExpr
  sub <- fromCollection subWhnf >>= fromMList >>= mapM evalRef >>= mapM toScalarData
  if product ns == toInteger (length xs)
    then return $ Value $ TensorData (initTensor ns xs sup sub)
    else throwError $ InconsistentTensorSize
 where
  toScalarData :: WHNFData -> EgisonM ScalarData
  toScalarData (Value (ScalarData x)) = return x
  toScalarData val = throwError $ TypeMismatch "integer or string" $ val

evalExpr env (HashExpr assocs) = do
  let (keyExprs, exprs) = unzip assocs
  keyWhnfs <- mapM (evalExpr env) keyExprs
  keys <- mapM makeHashKey keyWhnfs
  refs <- mapM (newObjectRef env) exprs
  case keys of
    [] -> do
      let keys' = map (\key -> case key of IntKey i -> i) keys
      return . Intermediate . IIntHash $ HL.fromList $ zip keys' refs
    _ ->
     case head keys of
       IntKey _ -> do
         let keys' = map (\key -> case key of IntKey i -> i) keys
         return . Intermediate . IIntHash $ HL.fromList $ zip keys' refs
       CharKey _ -> do
         let keys' = map (\key -> case key of CharKey c -> c) keys
         return . Intermediate . ICharHash $ HL.fromList $ zip keys' refs
       StrKey _ -> do
          let keys' = map (\key -> case key of StrKey s -> s) keys
          return . Intermediate . IStrHash $ HL.fromList $ zip keys' refs
 where
  makeHashKey :: WHNFData -> EgisonM EgisonHashKey
  makeHashKey (Value val) =
    case val of
      ScalarData _ -> fromEgison val >>= (return . IntKey)
      Char c -> return (CharKey c)
      String str -> return (StrKey str)
      _ -> throwError $ TypeMismatch "integer or string" $ Value val
  makeHashKey whnf = throwError $ TypeMismatch "integer or string" $ whnf

evalExpr env (IndexedExpr expr indices) = do
  tensor <- evalExpr env expr
  js <- mapM (\i -> case i of
                      Superscript n -> evalExprDeep env n >>= extract >>= return . Superscript
                      Subscript n -> evalExprDeep env n >>= extract >>= return . Subscript
              ) indices
  let indices'' = map (\j -> case j of
                              Superscript k -> k
                              Subscript k -> k) js
  let indices' = map (\j -> ScalarData j) indices''
  case tensor of
    (Value (ScalarData (Div (Plus [(Term 1 [(Symbol name [], 1)])]) (Plus [(Term 1 [])])))) -> do
      return $ Value (ScalarData (Div (Plus [(Term 1 [(Symbol name js, 1)])]) (Plus [(Term 1 [])])))
    (Value (TensorData (TData (Tensor ns xs) _))) -> do
      tCheckIndex indices'' ns
      if all (\x -> isInteger x) indices'
        then do indices''' <- ((mapM fromEgison indices') :: EgisonM [Integer])
                return $ Value $ ScalarData (tref' indices''' (Tensor ns xs))
        else do ret <- tContract (TData (tref indices'' (Tensor ns xs)) (Just (filter p js)))
                return $ Value ret
    _ -> refArray tensor indices'
 where
  extract :: EgisonValue -> EgisonM ScalarData
  extract (ScalarData s) = return s
  extract val = throwError $ TypeMismatch "scalar expression" (Value val)
  p :: Index ScalarData -> Bool
  p (Superscript k) = isSymbol (ScalarData k)
  p (Subscript k) = isSymbol (ScalarData k)

evalExpr env (LambdaExpr names expr) = return . Value $ Func Nothing env names expr

evalExpr env (CambdaExpr name expr) = return . Value $ CFunc Nothing env name expr

evalExpr env (MacroExpr names expr) = return . Value $ Macro names expr

evalExpr env (PatternFunctionExpr names pattern) = return . Value $ PatternFunc env names pattern

evalExpr env (IfExpr test expr expr') = do
  test <- evalExpr env test >>= fromWHNF
  evalExpr env $ if test then expr else expr'

evalExpr env (LetExpr bindings expr) =
  mapM extractBindings bindings >>= flip evalExpr expr . extendEnv env . concat
 where
  extractBindings :: BindingExpr -> EgisonM [Binding]
  extractBindings ([name], expr) =
    makeBindings [name] . (:[]) <$> newObjectRef env expr
  extractBindings (names, expr) =
    makeBindings names <$> (evalExpr env expr >>= fromTuple)

evalExpr env (LetRecExpr bindings expr) =
  let bindings' = evalState (concat <$> mapM extractBindings bindings) 0
  in recursiveBind env bindings' >>= flip evalExpr expr 
 where
  extractBindings :: BindingExpr -> State Int [(String, EgisonExpr)]
  extractBindings ([name], expr) = return [(name, expr)]
  extractBindings (names, expr) = do
    var <- genVar
    let k = length names
        target = VarExpr var
        matcher = TupleExpr $ replicate k SomethingExpr
        nth n =
          let pattern = TuplePat $ flip map [1..k] $ \i ->
                if i == n then PatVar "#_" else WildCard
          in MatchExpr target matcher [(pattern, VarExpr "#_")]
    return ((var, expr) : map (second nth) (zip names [1..]))

  genVar :: State Int String
  genVar = modify (1+) >> gets (('#':) . show)

evalExpr env (DoExpr bindings expr) = return $ Value $ IOFunc $ do
  let body = foldr genLet (ApplyExpr expr $ TupleExpr [VarExpr "#1"]) bindings
  applyFunc env (Value $ Func Nothing env ["#1"] body) $ Value World
 where
  genLet (names, expr) expr' =
    LetExpr [(["#1", "#2"], ApplyExpr expr $ TupleExpr [VarExpr "#1"])] $
    LetExpr [(names, VarExpr "#2")] expr'

evalExpr env (IoExpr expr) = do
  io <- evalExpr env expr
  case io of
    Value (IOFunc m) -> do
      val <- m >>= evalWHNF
      case val of
        Tuple [_, val'] -> return $ Value val'
    _ -> throwError $ TypeMismatch "io" io

evalExpr env (MatchAllExpr target matcher (pattern, expr)) = do
  target <- newObjectRef env target
  matcher <- evalExpr env matcher >>= evalMatcherWHNF
  result <- patternMatch env pattern target matcher
  mmap (flip evalExpr expr . extendEnv env) result >>= fromMList
 where
  fromMList :: MList EgisonM WHNFData -> EgisonM WHNFData
  fromMList MNil = return . Value $ Collection Sq.empty
  fromMList (MCons val m) = do
    head <- IElement <$> newEvalutedObjectRef val
    tail <- ISubCollection <$> (liftIO . newIORef . Thunk $ m >>= fromMList)
    seqRef <- liftIO . newIORef $ Sq.fromList [head, tail]
    return . Intermediate $ ICollection $ seqRef

evalExpr env (MatchExpr target matcher clauses) = do
  target <- newObjectRef env target
  matcher <- evalExpr env matcher >>= evalMatcherWHNF
  let tryMatchClause (pattern, expr) cont = do
        result <- patternMatch env pattern target matcher
        case result of
          MCons bindings _ -> evalExpr (extendEnv env bindings) expr
          MNil -> cont
  foldr tryMatchClause (throwError $ strMsg "failed pattern match") clauses

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
        Just objRef -> do
          evalRef objRef
        Nothing -> do
          whnf <- applyFunc env (Value (Func Nothing env names body)) (Value (makeTuple args))
          retRef <- newEvalutedObjectRef whnf
          hash <- liftIO $ readIORef hashRef
          liftIO $ writeIORef hashRef (HL.insert indices' retRef hash)
          writeObjectRef ref (Value (MemoizedFunc name ref hashRef env names body))
          return whnf
    _ -> applyFunc env func (Value (makeTuple args))

evalExpr env (ApplyExpr func arg) = do
  func <- evalExpr env func
  arg <- evalExpr env arg
  case func of
    Value (MemoizedFunc name ref hashRef env names body) -> do
      indices <- evalWHNF arg
      indices' <- mapM fromEgison $ fromTupleValue indices
      hash <- liftIO $ readIORef hashRef
      case HL.lookup indices' hash of
        Just objRef -> do
          evalRef objRef
        Nothing -> do
          whnf <- applyFunc env (Value (Func Nothing env names body)) arg
          retRef <- newEvalutedObjectRef whnf
          hash <- liftIO $ readIORef hashRef
          liftIO $ writeIORef hashRef (HL.insert indices' retRef hash)
          writeObjectRef ref (Value (MemoizedFunc name ref hashRef env names body))
          return whnf
    _ -> applyFunc env func arg

evalExpr env (MemoizeExpr memoizeFrame expr) = do
  mapM (\(x, y, z) -> do x' <- evalExprDeep env x
                         case x' of
                           (MemoizedFunc name ref hashRef env' names body) -> do
                             indices <- evalExprDeep env y
                             indices' <- mapM fromEgison $ fromTupleValue indices
                             hash <- liftIO $ readIORef hashRef
                             ret <- evalExprDeep env z
                             retRef <- newEvalutedObjectRef (Value ret)
                             liftIO $ writeIORef hashRef (HL.insert indices' retRef hash)
                             writeObjectRef ref (Value (MemoizedFunc name ref hashRef env' names body))
                           _ -> throwError $ TypeMismatch "memoized-function" (Value x'))
       memoizeFrame
  evalExpr env expr

evalExpr env (MatcherBFSExpr info) = return $ Value $ UserMatcher env BFSMode info
evalExpr env (MatcherDFSExpr info) = return $ Value $ UserMatcher env DFSMode info

evalExpr env (GenerateArrayExpr fnExpr (fstExpr, lstExpr)) = do
  fN <- (evalExpr env fstExpr >>= fromWHNF) :: EgisonM Integer
  eN <- (evalExpr env lstExpr >>= fromWHNF) :: EgisonM Integer
  xs <- mapM (\n -> (newObjectRef env (ApplyExpr fnExpr (IntegerExpr n)))) [fN..eN]
  return $ Intermediate $ IArray $ Array.listArray (fN, eN) xs

evalExpr env (ArrayBoundsExpr expr) = 
  evalExpr env expr >>= arrayBounds

evalExpr env (GenerateTensorExpr fnExpr sizeExpr) = do
  size' <- evalExpr env sizeExpr
  size'' <- collectionToList size'
  ns <- (mapM fromEgison size'') :: EgisonM [Integer]
  fn <- evalExpr env fnExpr
  xs <-  mapM (\ms -> applyFunc env fn (Value (makeTuple ms)) >>= evalWHNF >>= extractScalar) (map (\ms -> map toEgison ms) (tensorIndices ns))
  return $ Value (TensorData (makeTensor ns xs Nothing))
 where
  extractScalar :: EgisonValue -> EgisonM ScalarData
  extractScalar (ScalarData x) = return x
  extractScalar x = throwError $ TypeMismatch "scalar expression" (Value x)

evalExpr env (TensorMapExpr fnExpr tExpr) = do
  fn <- evalExpr env fnExpr
  tVal <- evalExpr env tExpr
  case tVal of
    Value (TensorData t) -> do
      tMap (applyScalarFunc env fn) t >>= (return . Value . TensorData)
    _ -> throwError $ TypeMismatch "tensor" tVal
 where
  applyScalarFunc :: Env -> WHNFData -> ScalarData -> EgisonM ScalarData
  applyScalarFunc env fn s = applyFunc env fn (Value (ScalarData s)) >>= extractScalar
  extractScalar :: WHNFData -> EgisonM ScalarData
  extractScalar (Value (ScalarData x)) = return x
  extractScalar x = throwError $ TypeMismatch "scalar expression" x

evalExpr env (TensorMap2Expr fnExpr t1Expr t2Expr) = do
  fn <- evalExpr env fnExpr
  t1Val <- evalExpr env t1Expr
  t2Val <- evalExpr env t2Expr
  case (t1Val, t2Val) of
    (Value (TensorData t1), Value (TensorData t2)) -> do
      tMap2 (applyScalarFunc env fn) t1 t2 >>= (return . Value . TensorData)
    (Value (TensorData _), _) -> throwError $ TypeMismatch "tensor" t1Val
    _ -> throwError $ TypeMismatch "tensor" t2Val
 where
  applyScalarFunc :: Env -> WHNFData -> ScalarData -> ScalarData -> EgisonM ScalarData
  applyScalarFunc env fn s1 s2 = applyFunc env fn (Value (Tuple [(ScalarData s1), (ScalarData s2)])) >>= extractScalar
  extractScalar :: WHNFData -> EgisonM ScalarData
  extractScalar (Value (ScalarData x)) = return x
  extractScalar x = throwError $ TypeMismatch "scalar expression" x

evalExpr _ SomethingExpr = return $ Value Something
evalExpr _ UndefinedExpr = return $ Value Undefined
evalExpr _ expr = throwError $ NotImplemented ("evalExpr for " ++ show expr)

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
evalWHNF coll = Collection <$> (fromCollection coll >>= fromMList >>= mapM evalRefDeep . Sq.fromList)

applyFunc :: Env -> WHNFData -> WHNFData -> EgisonM WHNFData
applyFunc _ (Value (Func _ env [name] body)) arg = do
  ref <- newEvalutedObjectRef arg
  evalExpr (extendEnv env $ makeBindings [name] [ref]) body
applyFunc _ (Value (Func _ env names body)) arg = do
  refs <- fromTuple arg
  if length names == length refs
    then evalExpr (extendEnv env $ makeBindings names refs) body
    else throwError $ ArgumentsNumWithNames names (length names) (length refs)
applyFunc _ (Value (CFunc _ env name body)) arg = do
  refs <- fromTuple arg
  seqRef <- liftIO . newIORef $ Sq.fromList (map IElement refs)
  col <- liftIO . newIORef $ WHNF $ Intermediate $ ICollection $ seqRef
  if length refs > 0
    then evalExpr (extendEnv env $ makeBindings [name] [col]) body
    else throwError $ ArgumentsNumWithNames [name] 1 0
applyFunc env (Value (Macro [name] body)) arg = do
  ref <- newEvalutedObjectRef arg
  evalExpr (extendEnv env $ makeBindings [name] [ref]) body
applyFunc env (Value (Macro names body)) arg = do
  refs <- fromTuple arg
  if length names == length refs
    then evalExpr (extendEnv env $ makeBindings names refs) body
    else throwError $ ArgumentsNumWithNames names (length names) (length refs)
applyFunc _ (Value (PrimitiveFunc _ func)) arg = func arg
applyFunc _ (Value (IOFunc m)) arg = do
  case arg of
     Value World -> m
     _ -> throwError $ TypeMismatch "world" arg
applyFunc _ (Value (Quote fn)) arg = do
  args <- tupleToList arg
  mExprs <- mapM extractScalar args
  return (Value (ScalarData (Div (Plus [(Term 1 [(Apply fn mExprs, 1)])]) (Plus [(Term 1 [])]))))
applyFunc _ (Value fn@(ScalarData (Div (Plus [(Term 1 [(Symbol _ _, 1)])]) (Plus [(Term 1 [])])))) arg = do
  args <- tupleToList arg
  mExprs <- mapM extractScalar args
  return (Value (ScalarData (Div (Plus [(Term 1 [(Apply fn mExprs, 1)])]) (Plus [(Term 1 [])]))))
applyFunc _ whnf _ = throwError $ TypeMismatch "function" whnf


extractScalar :: EgisonValue -> EgisonM ScalarData
extractScalar (ScalarData mExpr) = return mExpr
extractScalar val = throwError $ TypeMismatch "math expression" (Value val)

refArray :: WHNFData -> [EgisonValue] -> EgisonM WHNFData
refArray val [] = return val 
refArray (Value (Array array)) (index:indices) = do
  if isInteger index
    then do i <- (liftM fromInteger . fromEgison) index
            if (\(a,b) -> if a <= i && i <= b then True else False) $ Array.bounds array
              then refArray (Value (array ! i)) indices
              else return  $ Value Undefined
    else case index of
           (ScalarData (Div (Plus [(Term 1 [(Symbol var [], 1)])]) (Plus [(Term 1 [])]))) -> do
             let (_,size) = Array.bounds array
             elms <- mapM (\arr -> refArray (Value arr) indices) (Array.elems array)
             elmRefs <- mapM newEvalutedObjectRef elms
             return $ Intermediate $ IArray $ Array.listArray (1, size) elmRefs
           _  -> throwError $ TypeMismatch "integer or symbol" (Value index)
refArray (Intermediate (IArray array)) (index:indices) = do
  if isInteger index
    then do i <- (liftM fromInteger . fromEgison) index
            if (\(a,b) -> if a <= i && i <= b then True else False) $ Array.bounds array
              then let ref = array ! i in
                   evalRef ref >>= flip refArray indices
              else return  $ Value Undefined
    else case index of
           (ScalarData (Div (Plus [(Term 1 [(Symbol var [], 1)])]) (Plus [(Term 1 [])]))) -> do
             let (_,size) = Array.bounds array
             let refs = Array.elems array
             arrs <- mapM evalRef refs
             elms <- mapM (\arr -> refArray arr indices) arrs
             elmRefs <- mapM newEvalutedObjectRef elms
             return $ Intermediate $ IArray $ Array.listArray (1, size) elmRefs
           _  -> throwError $ TypeMismatch "integer or symbol" (Value index)
refArray (Value (IntHash hash)) (index:indices) = do
  key <- fromEgison index
  case HL.lookup key hash of
    Just val -> refArray (Value val) indices
    Nothing -> return $ Value Undefined
refArray (Intermediate (IIntHash hash)) (index:indices) = do
  key <- fromEgison index
  case HL.lookup key hash of
    Just ref -> evalRef ref >>= flip refArray indices
    Nothing -> return $ Value Undefined
refArray (Value (CharHash hash)) (index:indices) = do
  key <- fromEgison index
  case HL.lookup key hash of
    Just val -> refArray (Value val) indices
    Nothing -> return $ Value Undefined
refArray (Intermediate (ICharHash hash)) (index:indices) = do
  key <- fromEgison index
  case HL.lookup key hash of
    Just ref -> evalRef ref >>= flip refArray indices
    Nothing -> return $ Value Undefined
refArray (Value (StrHash hash)) (index:indices) = do
  key <- fromEgison index
  case HL.lookup key hash of
    Just val -> refArray (Value val) indices
    Nothing -> return $ Value Undefined
refArray (Intermediate (IStrHash hash)) (index:indices) = do
  key <- fromEgison index
  case HL.lookup key hash of
    Just ref -> evalRef ref >>= flip refArray indices
    Nothing -> return $ Value Undefined
refArray val _ = throwError $ TypeMismatch "array or hash" val

arrayBounds :: WHNFData -> EgisonM WHNFData
arrayBounds val = arrayBounds' val >>= return . Value

arrayBounds' :: WHNFData -> EgisonM EgisonValue
arrayBounds' (Intermediate (IArray arr)) = return $ Tuple [(toEgison (fst (Array.bounds arr))), (toEgison (snd (Array.bounds arr)))]
arrayBounds' (Value (Array arr))         = return $ Tuple [(toEgison (fst (Array.bounds arr))), (toEgison (snd (Array.bounds arr)))]
arrayBounds' val                         = throwError $ TypeMismatch "array" val

newThunk :: Env -> EgisonExpr -> Object
newThunk env expr = Thunk $ evalExpr env expr

newObjectRef :: Env -> EgisonExpr -> EgisonM ObjectRef
newObjectRef env expr = liftIO $ newIORef $ newThunk env expr

writeObjectRef :: ObjectRef -> WHNFData -> EgisonM ()
writeObjectRef ref val = liftIO . writeIORef ref $ WHNF val

newEvalutedObjectRef :: WHNFData -> EgisonM ObjectRef
newEvalutedObjectRef = liftIO . newIORef . WHNF

makeBindings :: [String] -> [ObjectRef] -> [Binding]
makeBindings = zip

recursiveBind :: Env -> [(String, EgisonExpr)] -> EgisonM Env
recursiveBind env bindings = do
  let (names, exprs) = unzip bindings
  refs <- replicateM (length bindings) $ newObjectRef nullEnv UndefinedExpr
  let env' = extendEnv env $ makeBindings names refs
  zipWithM_ (\ref (name,expr) ->
               case expr of
                 MemoizedLambdaExpr names body -> do
                   hashRef <- liftIO $ newIORef HL.empty
                   liftIO . writeIORef ref . WHNF . Value $ MemoizedFunc (Just name) ref hashRef env' names body
                 LambdaExpr args body -> do
                   whnf <- evalExpr env' expr
                   case whnf of
                     (Value (Func _ env args body)) -> liftIO . writeIORef ref . WHNF $ (Value (Func (Just name) env args body))
                 CambdaExpr arg body -> do
                   whnf <- evalExpr env' expr
                   case whnf of
                     (Value (CFunc _ env arg body)) -> liftIO . writeIORef ref . WHNF $ (Value (CFunc (Just name) env arg body))
                 _ -> liftIO . writeIORef ref . Thunk $ evalExpr env' expr)
            refs bindings
  return env'

--
-- Pattern Match
--

patternMatch :: Env -> EgisonPattern -> ObjectRef -> Matcher -> EgisonM (MList EgisonM Match) 
patternMatch env pattern target matcher = processMStates [msingleton $ MState env [] [] [MAtom pattern target matcher]]

processMStates :: [MList EgisonM MatchingState] -> EgisonM (MList EgisonM Match)
processMStates [] = return MNil
processMStates streams = do
  (matches, streams') <- mapM processMStates' streams >>= extractMatches . concat
  mappend (fromList matches) $ processMStates streams'

processMStates' :: MList EgisonM MatchingState -> EgisonM [MList EgisonM MatchingState]
processMStates' MNil = return []
processMStates' stream@(MCons state _) =
  case pmMode (getMatcher (topMAtom state)) of
    DFSMode -> processMStatesDFS stream
    BFSMode -> processMStatesBFS stream

gatherBindings :: MatchingState -> Maybe [Binding]
gatherBindings (MState _ _ bindings []) = return bindings
gatherBindings (MState _ _ bindings trees) = isResolved trees >> return bindings
  where isResolved :: [MatchingTree] -> Maybe ()
        isResolved [] = return ()
        isResolved (MAtom _ _ _ : _) = Nothing
        isResolved (MNode _ state : rest) = gatherBindings state >> isResolved rest

extractMatches :: [MList EgisonM MatchingState] -> EgisonM ([Match], [MList EgisonM MatchingState])
extractMatches = extractMatches' ([], [])
 where
  extractMatches' :: ([Match], [MList EgisonM MatchingState]) -> [MList EgisonM MatchingState] -> EgisonM ([Match], [MList EgisonM MatchingState])
  extractMatches' (xs, ys) [] = return (xs, ys)
  extractMatches' (xs, ys) ((MCons (gatherBindings -> Just bindings) states):rest) = do
    states' <- states
    extractMatches' (xs ++ [bindings], ys ++ [states']) rest
  extractMatches' (xs, ys) (stream:rest) = extractMatches' (xs, ys ++ [stream]) rest
          
processMStatesDFS :: MList EgisonM MatchingState -> EgisonM [(MList EgisonM MatchingState)]
processMStatesDFS (MCons state stream) = do
  stream' <- processMState state
  newStream <- mappend stream' stream
  return [newStream]
  
processMStatesBFS :: MList EgisonM MatchingState -> EgisonM [(MList EgisonM MatchingState)]
processMStatesBFS (MCons state stream) = do
  newStream <- processMState state
  newStream' <- stream
  return [newStream, newStream']

topMAtom :: MatchingState -> MatchingTree
topMAtom (MState _ _ _ (mAtom@(MAtom _ _ _):_)) = mAtom
topMAtom (MState _ _ _ ((MNode _ mstate):_)) = topMAtom mstate

getMatcher :: MatchingTree -> Matcher
getMatcher (MAtom _ _ matcher) = matcher

processMState :: MatchingState -> EgisonM (MList EgisonM MatchingState)
processMState state = do
  if isNotPat state
    then do
      let (state1, state2) = splitMState state
      result <- processMStates [msingleton state1]
      case result of
        MNil -> return $ msingleton state2
        _ -> return MNil
    else processMState' state
 where
  isNotPat :: MatchingState -> Bool
  isNotPat state = case topMAtom state of
                     MAtom (NotPat _) _ _ -> True
                     _ -> False
  splitMState :: MatchingState -> (MatchingState, MatchingState)
  splitMState (MState env loops bindings ((MAtom (NotPat pattern) target matcher) : trees)) =
    (MState env loops bindings [MAtom pattern target matcher], MState env loops bindings trees)
  splitMState (MState env loops bindings ((MNode penv state') : trees)) =
    let (state1, state2) = splitMState state'
    in (MState env loops bindings [MNode penv state1], MState env loops bindings (MNode penv state2 : trees))

processMState' :: MatchingState -> EgisonM (MList EgisonM MatchingState)
processMState' (MState _ _ _ []) = throwError $ EgisonBug "should not reach here (empty matching-state)"

processMState' (MState _ _ _ ((MNode _ (MState _ _ _ [])):_)) = throwError $ EgisonBug "should not reach here (empty matching-node)"

processMState' (MState env loops bindings (MNode penv (MState env' loops' bindings' ((MAtom (VarPat name) target matcher):trees')):trees)) = do
  case lookup name penv of
    Just pattern ->
      case trees' of
        [] -> return $ msingleton $ MState env loops bindings ((MAtom pattern target matcher):trees)
        _ -> return $ msingleton $ MState env loops bindings ((MAtom pattern target matcher):(MNode penv (MState env' loops' bindings' trees')):trees)
    Nothing -> throwError $ UnboundVariable name

processMState' (MState env loops bindings (MNode penv (MState env' loops' bindings' ((MAtom (IndexedPat (VarPat name) indices) target matcher):trees')):trees)) = do
  case lookup name penv of
    Just pattern -> do
      let env'' = extendEnvForNonLinearPatterns env' bindings loops'
      indices' <- mapM (evalExpr env'' >=> liftM fromInteger . fromWHNF) indices
      let pattern' = IndexedPat pattern $ map (\i -> IntegerExpr i) indices'
      case trees' of
        [] -> return $ msingleton $ MState env loops bindings ((MAtom pattern' target matcher):trees)
        _ -> return $ msingleton $ MState env loops bindings ((MAtom pattern' target matcher):(MNode penv (MState env' loops' bindings' trees')):trees)
    Nothing -> throwError $ UnboundVariable name

processMState' (MState env loops bindings ((MNode penv state):trees)) = do
  processMState' state >>= mmap (\state' -> case state' of
                                              MState _ _ _ [] -> return $ MState env loops bindings trees
                                              _ -> (return . MState env loops bindings . (: trees) . MNode penv) state')

processMState' (MState env loops bindings ((MAtom pattern target matcher):trees)) = do
  let env' = extendEnvForNonLinearPatterns env bindings loops
  case pattern of
    NotPat _ -> throwError $ EgisonBug "should not reach here (not pattern)"
    VarPat _ -> throwError $ strMsg $ "cannot use variable except in pattern function:" ++ show pattern

    LetPat bindings' pattern' ->
      let extractBindings ([name], expr) =
            makeBindings [name] . (:[]) <$> newObjectRef env' expr
          extractBindings (names, expr) =
            makeBindings names <$> (evalExpr env' expr >>= fromTuple)
      in
       liftM concat (mapM extractBindings bindings')
         >>= (\b -> return $ msingleton $ MState env loops (b ++ bindings) ((MAtom pattern' target matcher):trees))
    PredPat predicate -> do
      func <- evalExpr env' predicate
      arg <- evalRef target
      result <- applyFunc env func arg >>= fromWHNF
      if result then return $ msingleton $ (MState env loops bindings trees)
                else return MNil

    PApplyPat func args -> do
      func' <- evalExpr env' func
      case func' of
        Value (PatternFunc env'' names expr) ->
          let penv = zip names args
          in return $ msingleton $ MState env loops bindings (MNode penv (MState env'' [] [] [MAtom expr target matcher]) : trees)
        _ -> throwError $ TypeMismatch "pattern constructor" func'

    DApplyPat func args -> do
      return $ msingleton $ (MState env loops bindings ((MAtom (InductivePat "apply" [func, (toListPat args)]) target matcher):trees))
    
    LoopPat name (LoopRange start ends endPat) pat pat' -> do
      startNum <- evalExpr env' start >>= fromWHNF :: (EgisonM Integer)
      startNumRef <- newEvalutedObjectRef $ Value $ toEgison (startNum - 1)
      ends' <- evalExpr env' ends
      if isPrimitiveValue ends'
        then do 
          endsRef <- newEvalutedObjectRef ends'
          inners <- liftIO $ newIORef $ Sq.fromList [IElement endsRef]
          endsRef' <- liftIO $ newIORef (WHNF (Intermediate (ICollection inners)))
          return $ msingleton $ MState env ((LoopPatContext (name, startNumRef) endsRef' endPat pat pat'):loops) bindings ((MAtom ContPat target matcher):trees)
        else do
          endsRef <- newEvalutedObjectRef ends'
          return $ msingleton $ MState env ((LoopPatContext (name, startNumRef) endsRef endPat pat pat'):loops) bindings ((MAtom ContPat target matcher):trees)
    ContPat ->
      case loops of
        [] -> throwError $ strMsg "cannot use cont pattern except in loop pattern"
        LoopPatContext (name, startNumRef) endsRef endPat pat pat' : loops' -> do
          startNum <- evalRef startNumRef >>= fromWHNF :: (EgisonM Integer)
          nextNumRef <- newEvalutedObjectRef $ Value $ toEgison (startNum + 1)
          ends <- evalRef endsRef
          b <- isEmptyCollection ends
          if b
            then return MNil
            else do
              (carEndsRef, cdrEndsRef) <- fromJust <$> runMaybeT (unconsCollection ends)
              carEndsNum <- evalRef carEndsRef >>= fromWHNF
              if startNum > carEndsNum
                then return MNil
                else if startNum == carEndsNum
                       then return $ fromList [MState env loops' bindings ((MAtom endPat startNumRef Something):(MAtom pat' target matcher):trees),
                                               MState env ((LoopPatContext (name, nextNumRef) cdrEndsRef endPat pat pat'):loops') bindings ((MAtom pat target matcher):trees)]
                       else return $ fromList [MState env ((LoopPatContext (name, nextNumRef) endsRef endPat pat pat'):loops') bindings ((MAtom pat target matcher):trees)]
    AndPat patterns ->
      let trees' = map (\pat -> MAtom pat target matcher) patterns ++ trees
      in return $ msingleton $ MState env loops bindings trees'
    OrPat patterns ->
      return $ fromList $ flip map patterns $ \pat ->
        MState env loops bindings (MAtom pat target matcher : trees)

    _ ->
      case matcher of
        UserMatcher _ _ _ -> do
          (patterns, targetss, matchers) <- inductiveMatch env' pattern target matcher
          mfor targetss $ \ref -> do
            targets <- evalRef ref >>= fromTuple
            let trees' = zipWith3 MAtom patterns targets matchers ++ trees
            return $ MState env loops bindings trees'
            
        Tuple matchers -> do
          case pattern of
            ValuePat _ -> return $ msingleton $ MState env loops bindings ((MAtom pattern target Something):trees)
            WildCard -> return $ msingleton $ MState env loops bindings ((MAtom pattern target Something):trees)
            PatVar _ -> return $ msingleton $ MState env loops bindings ((MAtom pattern target Something):trees)
            IndexedPat _ _ -> return $ msingleton $ MState env loops bindings ((MAtom pattern target Something):trees)
            TuplePat patterns -> do
              targets <- evalRef target >>= fromTuple
              if not (length patterns == length targets) then throwError $ ArgumentsNum (length patterns) (length targets) else return ()
              if not (length patterns == length matchers) then throwError $ ArgumentsNum (length patterns) (length matchers) else return ()
              let trees' = zipWith3 MAtom patterns targets matchers ++ trees
              return $ msingleton $ MState env loops bindings trees'
            _ ->  throwError $ strMsg $ "should not reach here. matcher: " ++ show matcher ++ ", pattern:  " ++ show pattern

        Something ->
          case pattern of
            ValuePat valExpr -> do
              val <- evalExprDeep env' valExpr
              tgtVal <- evalRefDeep target
              if val == tgtVal
                then return $ msingleton $ MState env loops bindings trees
                else return MNil
            WildCard -> return $ msingleton $ MState env loops bindings trees
            PatVar name -> return $ msingleton $ MState env loops ((name, target):bindings) trees
            IndexedPat (PatVar name) indices -> do
              indices <- mapM (evalExpr env' >=> liftM fromInteger . fromWHNF) indices
              case lookup name bindings of
                Just ref -> do
                  obj <- evalRef ref >>= updateHash indices >>= newEvalutedObjectRef
                  return $ msingleton $ MState env loops (subst name obj bindings) trees
                Nothing  -> do
                  obj <- updateHash indices (Intermediate . IIntHash $ HL.empty) >>= newEvalutedObjectRef
                  return $ msingleton $ MState env loops ((name,obj):bindings) trees
               where
                updateHash :: [Integer] -> WHNFData -> EgisonM WHNFData
                updateHash [index] (Intermediate (IIntHash hash)) = do
                  return . Intermediate . IIntHash $ HL.insert index target hash
                updateHash (index:indices) (Intermediate (IIntHash hash)) = do
                  val <- maybe (return $ Intermediate $ IIntHash HL.empty) evalRef $ HL.lookup index hash
                  ref <- updateHash indices val >>= newEvalutedObjectRef
                  return . Intermediate . IIntHash $ HL.insert index ref hash
                updateHash indices (Value (IntHash hash)) = do
                  keys <- return $ HL.keys hash
                  vals <- mapM (newEvalutedObjectRef . Value) $ HL.elems hash
                  updateHash indices (Intermediate $ IIntHash $ HL.fromList $ zip keys vals)
                updateHash _ v = throwError $ strMsg $ "expected hash value: " ++ show v
                subst :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
                subst k nv ((k', v'):xs) | k == k'   = (k', nv):(subst k nv xs)
                                         | otherwise = (k', v'):(subst k nv xs)
                subst _ _ [] = []
            IndexedPat pattern indices -> throwError $ strMsg ("invalid indexed-pattern: " ++ show pattern) 
            TuplePat patterns -> do
              targets <- evalRef target >>= fromTuple
              if not (length patterns == length targets) then throwError $ ArgumentsNum (length patterns) (length targets) else return ()
              let trees' = zipWith3 MAtom patterns targets (take (length patterns) (repeat Something)) ++ trees
              return $ msingleton $ MState env loops bindings trees'
            _ -> throwError $ strMsg "something can only match with a pattern variable"
        _ ->  throwError $ EgisonBug $ "should not reach here. matcher: " ++ show matcher ++ ", pattern:  " ++ show pattern

inductiveMatch :: Env -> EgisonPattern -> ObjectRef -> Matcher ->
                  EgisonM ([EgisonPattern], MList EgisonM ObjectRef, [Matcher])
inductiveMatch env pattern target (UserMatcher matcherEnv _ clauses) = do
  foldr tryPPMatchClause failPPPatternMatch clauses
 where
  tryPPMatchClause (pat, matchers, clauses) cont = do
    result <- runMaybeT $ primitivePatPatternMatch env pat pattern
    case result of
      Just (patterns, bindings) -> do
        targetss <- foldr tryPDMatchClause failPDPatternMatch clauses
        matchers <- evalExpr matcherEnv matchers >>= evalMatcherWHNF >>= (return . fromTupleValue)
        return (patterns, targetss, matchers)
       where
        tryPDMatchClause (pat, expr) cont = do
          result <- runMaybeT $ primitiveDataPatternMatch pat target
          case result of
            Just bindings' -> do
              let env = extendEnv matcherEnv $ bindings ++ bindings'
              evalExpr env expr >>= fromCollection
            _ -> cont
      _ -> cont
  failPPPatternMatch = throwError $ strMsg "failed primitive pattern pattern match"
  failPDPatternMatch = throwError $ strMsg "failed primitive data pattern match"

primitivePatPatternMatch :: Env -> PrimitivePatPattern -> EgisonPattern ->
                            MatchM ([EgisonPattern], [Binding])
primitivePatPatternMatch _ PPWildCard _ = return ([], [])
primitivePatPatternMatch _ PPPatVar pattern = return ([pattern], [])
primitivePatPatternMatch env (PPValuePat name) (ValuePat expr) = do
  ref <- lift $ newObjectRef env expr
  return ([], [(name, ref)])
primitivePatPatternMatch env (PPInductivePat name patterns) (InductivePat name' exprs)
  | name == name' && length patterns == length exprs =
    (concat *** concat) . unzip <$> zipWithM (primitivePatPatternMatch env) patterns exprs
  | otherwise = matchFail
primitivePatPatternMatch _ _ _ = matchFail

primitiveDataPatternMatch :: PrimitiveDataPattern -> ObjectRef -> MatchM [Binding]
primitiveDataPatternMatch PDWildCard _ = return []
primitiveDataPatternMatch (PDPatVar name) ref = return [(name, ref)]
primitiveDataPatternMatch (PDInductivePat name patterns) ref = do
  whnf <- lift $ evalRef ref
  case whnf of
    Intermediate (IInductiveData name' refs) | name == name' ->
      concat <$> zipWithM primitiveDataPatternMatch patterns refs
    Value (InductiveData name' vals) | name == name' -> do
      refs <- lift $ mapM (newEvalutedObjectRef . Value) vals
      concat <$> zipWithM primitiveDataPatternMatch patterns refs
    _ -> matchFail
primitiveDataPatternMatch (PDTuplePat patterns) ref = do
  whnf <- lift $ evalRef ref
  case whnf of
    Intermediate (ITuple refs) ->
      concat <$> zipWithM primitiveDataPatternMatch patterns refs
    Value (Tuple vals) -> do
      refs <- lift $ mapM (newEvalutedObjectRef . Value) vals
      concat <$> zipWithM primitiveDataPatternMatch patterns refs
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
  target <- lift (evalRef ref) >>= either (const matchFail) return . extractPrimitiveValue
  isEqual <- lift $ (==) <$> evalExprDeep nullEnv expr <*> pure target
  if isEqual then return [] else matchFail

expandCollection :: WHNFData -> EgisonM (Seq Inner)
expandCollection (Value (Collection vals)) =
  mapM (liftM IElement . newEvalutedObjectRef . Value) vals
expandCollection (Intermediate (ICollection innersRef)) = liftIO $ readIORef innersRef
expandCollection val = throwError $ TypeMismatch "collection" val

isEmptyCollection :: WHNFData -> EgisonM Bool
isEmptyCollection (Value (Collection col)) = return $ Sq.null col
isEmptyCollection coll@(Intermediate (ICollection innersRef)) = do
  inners <- liftIO $ readIORef innersRef
  case Sq.viewl inners of
    EmptyL -> return True
    (ISubCollection ref') :< tInners -> do
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
      lift $ (,) <$> newEvalutedObjectRef (Value val)
                 <*> newEvalutedObjectRef (Value $ Collection vals)
unconsCollection coll@(Intermediate (ICollection innersRef)) = do
  inners <- liftIO $ readIORef innersRef
  case Sq.viewl inners of
    EmptyL -> matchFail
    (IElement ref') :< tInners -> do
      tInnersRef <- liftIO $ newIORef tInners
      lift $ (ref', ) <$> newEvalutedObjectRef (Intermediate $ ICollection tInnersRef)
    (ISubCollection ref') :< tInners -> do
      hInners <- lift $ evalRef ref' >>= expandCollection
      liftIO $ writeIORef innersRef (hInners >< tInners)
      unconsCollection coll
unconsCollection _ = matchFail

unsnocCollection :: WHNFData -> MatchM (ObjectRef, ObjectRef)
unsnocCollection (Value (Collection col)) =
  case Sq.viewr col of
    EmptyR -> matchFail
    vals :> val ->
      lift $ (,) <$> newEvalutedObjectRef (Value $ Collection vals)
                 <*> newEvalutedObjectRef (Value val)
unsnocCollection coll@(Intermediate (ICollection innersRef)) = do
  inners <- liftIO $ readIORef innersRef
  case Sq.viewr inners of
    EmptyR -> matchFail
    hInners :> (IElement ref') -> do
      hInnersRef <- liftIO $ newIORef hInners
      lift $ (, ref') <$> newEvalutedObjectRef (Intermediate $ ICollection hInnersRef)
    hInners :> (ISubCollection ref') -> do
      tInners <- lift $ evalRef ref' >>= expandCollection
      liftIO $ writeIORef innersRef (hInners >< tInners)
      unsnocCollection coll
unsnocCollection _ = matchFail

extendEnvForNonLinearPatterns :: Env -> [Binding] -> [LoopPatContext] -> Env
extendEnvForNonLinearPatterns env bindings loops =  extendEnv env $ bindings ++ map (\(LoopPatContext binding _ _ _ _) -> binding) loops

evalMatcherWHNF :: WHNFData -> EgisonM Matcher
evalMatcherWHNF (Value matcher@Something) = return matcher
evalMatcherWHNF (Value matcher@(UserMatcher _ _ _)) = return matcher
evalMatcherWHNF (Value (Tuple ms)) = Tuple <$> mapM (evalMatcherWHNF . Value) ms
evalMatcherWHNF (Intermediate (ITuple refs)) = do
  whnfs <- mapM evalRef refs
  ms <- mapM evalMatcherWHNF whnfs
  return $ Tuple ms
evalMatcherWHNF whnf = throwError $ TypeMismatch "matcher" whnf

--
-- Util
--
toListPat :: [EgisonPattern] -> EgisonPattern
toListPat [] = InductivePat "nil" []
toListPat (pat:pats) = InductivePat "cons" [pat, (toListPat pats)]

fromTuple :: WHNFData -> EgisonM [ObjectRef]
fromTuple (Intermediate (ITuple refs)) = return refs
fromTuple (Value (Tuple vals)) = mapM (newEvalutedObjectRef . Value) vals
fromTuple whnf = return <$> newEvalutedObjectRef whnf

fromTupleValue :: EgisonValue -> [EgisonValue]
fromTupleValue (Tuple vals) = vals
fromTupleValue val = [val]

fromCollection :: WHNFData -> EgisonM (MList EgisonM ObjectRef)
fromCollection (Value (Collection vals)) =
  if Sq.null vals then return MNil
                  else fromSeq <$> mapM (newEvalutedObjectRef . Value) vals
fromCollection whnf@(Intermediate (ICollection _)) = do
  isEmpty <- isEmptyCollection whnf
  if isEmpty
    then return MNil
    else do
      (head, tail) <- fromJust <$> runMaybeT (unconsCollection whnf)
      tail' <- evalRef tail
      return $ MCons head (fromCollection tail')
fromCollection whnf = throwError $ TypeMismatch "collection" whnf

tupleToList :: WHNFData -> EgisonM [EgisonValue]
tupleToList whnf = do
  val <- evalWHNF whnf
  return $ tupleToList' val
 where
  tupleToList' (Tuple vals) = vals
  tupleToList' val = [val]

collectionToList :: WHNFData -> EgisonM [EgisonValue]
collectionToList whnf = do
  val <- evalWHNF whnf
  return $ collectionToList' val
 where
  collectionToList' (Collection sq) = toList sq

makeTuple :: [EgisonValue] -> EgisonValue
makeTuple [] = Tuple []
makeTuple [x] = x
makeTuple xs = Tuple xs

--
-- String
--
packStringValue :: EgisonValue -> EgisonM Text
packStringValue (Collection seq) = do
  let ls = toList seq
  str <- mapM (\val -> case val of
                         Char c -> return c
                         _ -> throwError $ TypeMismatch "char" (Value val))
              ls
  return $ T.pack str
packStringValue (Tuple [val]) = packStringValue val
packStringValue val = throwError $ TypeMismatch "string" (Value val)

--
-- Util
--
data EgisonHashKey =
    IntKey Integer
  | CharKey Char
  | StrKey Text

extractPrimitiveValue :: WHNFData -> Either EgisonError EgisonValue
extractPrimitiveValue (Value val@(Char _)) = return val
extractPrimitiveValue (Value val@(Bool _)) = return val
extractPrimitiveValue (Value val@(ScalarData _)) = return val
extractPrimitiveValue (Value val@(Float _ _)) = return val
extractPrimitiveValue whnf = throwError $ TypeMismatch "primitive value" whnf

isPrimitiveValue :: WHNFData -> Bool
isPrimitiveValue (Value (Char _)) = True
isPrimitiveValue (Value (Bool _)) = True
isPrimitiveValue (Value (ScalarData _)) = True
isPrimitiveValue (Value (Float _ _)) = True
isPrimitiveValue _ = False
