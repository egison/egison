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

import Prelude hiding (mapM, mappend)

import Control.Arrow
import Control.Applicative
import Control.Monad.Except hiding (mapM)
import Control.Monad.State hiding (mapM, state)
import Control.Monad.Trans.Maybe

import Data.List (partition)
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
import qualified Data.Vector as V
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
  collectDefs :: [EgisonTopExpr] -> [(Var, EgisonExpr)] -> [EgisonTopExpr] -> EgisonM ([(Var, EgisonExpr)], [EgisonTopExpr])
  collectDefs (expr:exprs) bindings rest =
    case expr of
      Define name expr -> collectDefs exprs (((stringToVar $ show name), expr) : bindings) rest
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
  collectDefs :: [EgisonTopExpr] -> [(Var, EgisonExpr)] -> [EgisonTopExpr] -> EgisonM ([(Var, EgisonExpr)], [EgisonTopExpr])
  collectDefs (expr:exprs) bindings rest =
    case expr of
      Define name expr -> collectDefs exprs (((stringToVar $ show name), expr) : bindings) rest
      Load file -> do
        exprs' <- loadLibraryFile file
        collectDefs (exprs' ++ exprs) bindings rest
      LoadFile file -> do
        exprs' <- loadFile file
        collectDefs (exprs' ++ exprs) bindings rest
      Test _ -> collectDefs exprs bindings (expr : rest)
      Redefine _ _ -> collectDefs exprs bindings (expr : rest)
      _ -> collectDefs exprs bindings rest
  collectDefs [] bindings rest = return (bindings, reverse rest)

evalTopExprsNoIO :: Env -> [EgisonTopExpr] -> EgisonM Env
evalTopExprsNoIO env exprs = do
  (bindings, rest) <- collectDefs exprs [] []
  env <- recursiveBind env bindings
  forM_ rest $ evalTopExpr env
  return env
 where
  collectDefs :: [EgisonTopExpr] -> [(Var, EgisonExpr)] -> [EgisonTopExpr] -> EgisonM ([(Var, EgisonExpr)], [EgisonTopExpr])
  collectDefs (expr:exprs) bindings rest =
    case expr of
      Define name expr -> collectDefs exprs (((stringToVar $ show name), expr) : bindings) rest
      Load _ -> throwError $ Default "No IO support"
      LoadFile _ -> throwError $ Default "No IO support"
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
evalTopExpr' env (Define name expr) = recursiveBind env [((stringToVar $ show name), expr)] >>= return . ((,) Nothing)
evalTopExpr' env (Redefine name expr) = recursiveRebind env ((stringToVar $ show name), expr) >>= return . ((,) Nothing)
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
    Value (ScalarData s) -> return . Value $ ScalarData $ Div (Plus [Term 1 [(Quote s, 1)]]) (Plus [Term 1 []])
    _ -> throwError $ TypeMismatch "scalar in quote" $ whnf

evalExpr env (QuoteFunctionExpr expr) = do
  whnf <- evalExpr env expr
  case whnf of
    Value val -> return . Value $ QuotedFunc val
    _ -> throwError $ TypeMismatch "value in quote-function" $ whnf

evalExpr env (VarExpr name) = do
  x <- refVar' env name >>= evalRef
  return (case x of
            Value (ScalarData (Div (Plus [Term 1 [(FunctionData ms args js, 1)]]) p)) -> case ms of
                                                                                        Nothing -> Value $ ScalarData (Div (Plus [Term 1 [(FunctionData (Just $ show name) args js, 1)]]) p)
                                                                                        Just s -> Value $ ScalarData (Div (Plus [Term 1 [(FunctionData ms args js, 1)]]) p)
            _ -> x)
 where
  refVar' :: Env -> Var -> EgisonM ObjectRef
  refVar' env var = maybe (newEvaluatedObjectRef (Value (symbolScalarData "" $ show var))) return
                          (refVar env var)

evalExpr env (PartialVarExpr n) = evalExpr env (VarExpr $ stringToVar ("::" ++ show n))

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

evalExpr env (VectorExpr exprs) = do
  whnfs <- mapM (evalExpr env) exprs
  case whnfs of
    ((Intermediate (ITensor (Tensor _ _ _))):_) -> do
      ret <- mapM toTensor whnfs >>= tConcat' >>= fromTensor
      return ret
    _ -> do
      fromTensor (Tensor [fromIntegral (length whnfs)] (V.fromList whnfs) [])

evalExpr env (TensorExpr nsExpr xsExpr supExpr subExpr) = do
  nsWhnf <- evalExpr env nsExpr
  ns <- ((fromCollection nsWhnf >>= fromMList >>= mapM evalRef >>= mapM fromWHNF) :: EgisonM [Integer])
  xsWhnf <- evalExpr env xsExpr
  xs <- fromCollection xsWhnf >>= fromMList >>= mapM evalRef
  supWhnf <- evalExpr env supExpr
  sup <- fromCollection supWhnf >>= fromMList >>= mapM evalRefDeep -- >>= mapM extractScalar'
  subWhnf <- evalExpr env subExpr
  sub <- fromCollection subWhnf >>= fromMList >>= mapM evalRefDeep -- >>= mapM extractScalar'
  if product ns == toInteger (length xs)
    then fromTensor (initTensor ns xs sup sub)
    else throwError $ InconsistentTensorSize

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

evalExpr env (IndexedExpr bool expr indices) = do
  tensor <- case expr of
              (VarExpr var) -> do
                let mObjRef = refVar env ((stringToVar . show) (VarWithIndexType (show var) (map f indices)))
                case mObjRef of
                  (Just objRef) -> evalRef objRef
                  Nothing -> evalExpr env expr
              _ -> evalExpr env expr
  js <- mapM (\i -> case i of
                      Superscript n -> evalExprDeep env n >>= return . Superscript
                      Subscript n -> evalExprDeep env n >>= return . Subscript
                      SupSubscript n -> evalExprDeep env n >>= return . SupSubscript
              ) indices
  
  ret <- case tensor of
      (Value (ScalarData (Div (Plus [(Term 1 [(Symbol id name [], 1)])]) (Plus [(Term 1 [])])))) -> do
        js2 <- mapM (\i -> case i of
                             Superscript n -> evalExprDeep env n >>= extractScalar >>= return . Superscript
                             Subscript n -> evalExprDeep env n >>= extractScalar >>= return . Subscript
                             SupSubscript n -> evalExprDeep env n >>= extractScalar >>= return . SupSubscript
                    ) indices
        return $ Value (ScalarData (Div (Plus [(Term 1 [(Symbol id name js2, 1)])]) (Plus [(Term 1 [])])))
      (Value (ScalarData _)) -> do
        return $ tensor
      (Value (TensorData (Tensor ns xs is))) -> do
        if bool then tref js (Tensor ns xs js) >>= toTensor >>= tContract' >>= fromTensor >>= return . Value
                else tref (is ++ js) (Tensor ns xs (is ++ js)) >>= toTensor >>= tContract' >>= fromTensor >>= return . Value
      (Intermediate (ITensor (Tensor ns xs is))) -> do
        if bool then tref js (Tensor ns xs js) >>= toTensor >>= tContract' >>= fromTensor
                else tref (is ++ js) (Tensor ns xs (is ++ js)) >>= toTensor >>= tContract' >>= fromTensor
      _ -> do
        js2 <- mapM (\i -> case i of
                             Superscript n -> evalExprDeep env n >>= extractScalar >>= return . Superscript
                             Subscript n -> evalExprDeep env n >>= extractScalar >>= return . Subscript
                             SupSubscript n -> evalExprDeep env n >>= extractScalar >>= return . SupSubscript
                    ) indices
        refArray tensor (map (\j -> case j of
                                      Superscript k -> ScalarData k
                                      Subscript k -> ScalarData k
                                      SupSubscript k -> ScalarData k
                              ) js2)
  return ret
 where
  f :: Index a -> Index ()
  f (Superscript _) = Superscript ()
  f (Subscript _) = Subscript ()
  f (SupSubscript _) = SupSubscript ()

evalExpr env (SubrefsExpr bool expr jsExpr) = do
  js <- evalExpr env jsExpr >>= collectionToList >>= return . (map Subscript)
  tensor <- case expr of
              (VarExpr var) -> do
                let mObjRef = refVar env ((stringToVar . show) (VarWithIndexType (show var) (take (length js) (repeat (Subscript ())))))
                case mObjRef of
                  (Just objRef) -> evalRef objRef
                  Nothing -> evalExpr env expr
              _ -> evalExpr env expr
  ret <- case tensor of
      (Value (ScalarData _)) -> do
        return $ tensor
      (Value (TensorData (Tensor ns xs is))) -> do
        if bool then tref js (Tensor ns xs js) >>= toTensor >>= tContract' >>= fromTensor >>= return . Value
                else tref (is ++ js) (Tensor ns xs (is ++ js)) >>= toTensor >>= tContract' >>= fromTensor >>= return . Value
      (Intermediate (ITensor (Tensor ns xs is))) -> do
        if bool then tref js (Tensor ns xs js) >>= toTensor >>= tContract' >>= fromTensor
                else tref (is ++ js) (Tensor ns xs (is ++ js)) >>= toTensor >>= tContract' >>= fromTensor
      _ -> throwError $ NotImplemented "subrefs"
  return ret
 where
  f :: Index a -> Index ()
  f (Superscript _) = Superscript ()
  f (Subscript _) = Subscript ()
  f (SupSubscript _) = SupSubscript ()

evalExpr env (SuprefsExpr bool expr jsExpr) = do
  js <- evalExpr env jsExpr >>= collectionToList >>= return . (map Superscript)
  tensor <- case expr of
              (VarExpr var) -> do
                let mObjRef = refVar env ((stringToVar . show) (VarWithIndexType (show var) (take (length js) (repeat (Superscript ())))))
                case mObjRef of
                  (Just objRef) -> evalRef objRef
                  Nothing -> evalExpr env expr
              _ -> evalExpr env expr
  ret <- case tensor of
      (Value (ScalarData _)) -> do
        return $ tensor
      (Value (TensorData (Tensor ns xs is))) -> do
        if bool then tref js (Tensor ns xs js) >>= toTensor >>= tContract' >>= fromTensor >>= return . Value
                else tref (is ++ js) (Tensor ns xs (is ++ js)) >>= toTensor >>= tContract' >>= fromTensor >>= return . Value
      (Intermediate (ITensor (Tensor ns xs is))) -> do
        if bool then tref js (Tensor ns xs js) >>= toTensor >>= tContract' >>= fromTensor
                else tref (is ++ js) (Tensor ns xs (is ++ js)) >>= toTensor >>= tContract' >>= fromTensor
      _ -> throwError $ NotImplemented "suprefs"
  return ret
 where
  f :: Index a -> Index ()
  f (Superscript _) = Superscript ()
  f (Subscript _) = Subscript ()
  f (SupSubscript _) = SupSubscript ()

evalExpr env (UserrefsExpr bool expr jsExpr) = do
  val <- evalExprDeep env expr
  js <- evalExpr env jsExpr >>= collectionToList >>= mapM extractScalar >>= return . (map Userscript)
  ret <- case val of
      (ScalarData (Div (Plus [Term 1 [(Symbol id name [], 1)]]) (Plus [Term 1 []]))) -> return $ Value (ScalarData (Div (Plus [Term 1 [(Symbol id name js, 1)]]) (Plus [Term 1 []])))
      (ScalarData (Div (Plus [Term 1 [(FunctionData (Just name) args is, 1)]]) (Plus [Term 1 []]))) -> return $ Value (ScalarData (Div (Plus [Term 1 [(FunctionData (Just name) args (is ++ js), 1)]]) (Plus [Term 1 []])))
      _ -> throwError $ NotImplemented "user-refs"
  return ret

evalExpr env (LambdaExpr names expr) = do
  names' <- mapM (\name -> case name of
                             (TensorArg name') -> return name'
                             (ScalarArg _) -> throwError $ EgisonBug "scalar-arg remained") names
  return . Value $ Func Nothing env names' expr

evalExpr env (PartialExpr n expr) = return . Value $ PartialFunc env n expr

evalExpr env (CambdaExpr name expr) = return . Value $ CFunc Nothing env name expr

evalExpr env (ProcedureExpr names expr) = return . Value $ Proc Nothing env names expr

evalExpr env (MacroExpr names expr) = return . Value $ Macro names expr

evalExpr env (PatternFunctionExpr names pattern) = return . Value $ PatternFunc env names pattern

evalExpr env (FunctionExpr args) = do
  args' <- mapM (\arg -> evalExprDeep env arg) args
  return . Value $ ScalarData (Div (Plus [Term 1 [(FunctionData Nothing args' [], 1)]]) (Plus [Term 1 []]))

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
          in MatchExpr target matcher [(pattern, VarExpr $ stringToVar "#_")]
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
  syms <- mapM (\var -> (newEvaluatedObjectRef (Value (symbolScalarData symId var)))) vars
  let bindings = zip (map stringToVar vars) syms
  whnf <- evalExpr (extendEnv env bindings) expr
  case whnf of
    (Value (TensorData (Tensor ns xs js))) -> do
      removeTmpscripts symId (Value (TensorData (Tensor ns xs js)))
    (Intermediate (ITensor (Tensor ns xs js))) -> do
      removeTmpscripts symId (Intermediate (ITensor (Tensor ns xs js)))
    _ -> return whnf
 where
  isTmpSymbol :: String -> (Index EgisonValue) -> Bool
  isTmpSymbol symId (Subscript (ScalarData (Div (Plus [Term 1 [(Symbol id name is,n)]]) (Plus [Term 1 []]))))
    | symId == id = True
    | otherwise = False
  isTmpSymbol symId (Superscript (ScalarData (Div (Plus [Term 1 [(Symbol id name is,n)]]) (Plus [Term 1 []]))))
    | symId == id = True
    | otherwise = False
  isTmpSymbol symId (SupSubscript (ScalarData (Div (Plus [Term 1 [(Symbol id name is,n)]]) (Plus [Term 1 []]))))
    | symId == id = True
    | otherwise = False
  removeTmpscripts :: String -> WHNFData -> EgisonM WHNFData
  removeTmpscripts symId (Intermediate (ITensor (Tensor s xs is))) = do
    let (ds, js) = partition (isTmpSymbol symId) is
    (Tensor s ys _) <- tTranspose (js ++ ds) (Tensor s xs is)
    return (Intermediate (ITensor (Tensor s ys js)))
  removeTmpscripts symId (Value (TensorData (Tensor s xs is))) = do
    let (ds, js) = partition (isTmpSymbol symId) is
    (Tensor s ys _) <- tTranspose (js ++ ds) (Tensor s xs is)
    return (Value (TensorData (Tensor s ys js)))
  removeDFscripts _ whnf = return whnf
    

evalExpr env (DoExpr bindings expr) = return $ Value $ IOFunc $ do
  let body = foldr genLet (ApplyExpr expr $ TupleExpr [VarExpr $ stringToVar "#1"]) bindings
  applyFunc env (Value $ Func Nothing env ["#1"] body) $ Value World
 where
  genLet (names, expr) expr' =
    LetExpr [(map stringToVar ["#1", "#2"], ApplyExpr expr $ TupleExpr [VarExpr $ stringToVar "#1"])] $
    LetExpr [(names, VarExpr $ stringToVar "#2")] expr'

evalExpr env (IoExpr expr) = do
  io <- evalExpr env expr
  case io of
    Value (IOFunc m) -> do
      val <- m >>= evalWHNF
      case val of
        Tuple [_, val'] -> return $ Value val'
    _ -> throwError $ TypeMismatch "io" io

evalExpr env (MatchAllExpr target matcher (pattern, expr)) = do
  target <- evalExpr env target
  matcher <- evalExpr env matcher >>= evalMatcherWHNF
  result <- patternMatch env pattern target matcher
  mmap (flip evalExpr expr . extendEnv env) result >>= fromMList
 where
  fromMList :: MList EgisonM WHNFData -> EgisonM WHNFData
  fromMList MNil = return . Value $ Collection Sq.empty
  fromMList (MCons val m) = do
    head <- IElement <$> newEvaluatedObjectRef val
    tail <- ISubCollection <$> (liftIO . newIORef . Thunk $ m >>= fromMList)
    seqRef <- liftIO . newIORef $ Sq.fromList [head, tail]
    return . Intermediate $ ICollection $ seqRef

evalExpr env (MatchExpr target matcher clauses) = do
  target <- evalExpr env target
  matcher <- evalExpr env matcher >>= evalMatcherWHNF
  f matcher target
 where
  f matcher target = do
      let tryMatchClause (pattern, expr) cont = do
            result <- patternMatch env pattern target matcher
            case result of
              MCons bindings _ -> evalExpr (extendEnv env bindings) expr
              MNil -> cont
      foldr tryMatchClause (throwError $ Default "failed pattern match") clauses

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
          retRef <- newEvaluatedObjectRef whnf
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
          retRef <- newEvaluatedObjectRef whnf
          hash <- liftIO $ readIORef hashRef
          liftIO $ writeIORef hashRef (HL.insert indices' retRef hash)
          writeObjectRef ref (Value (MemoizedFunc name ref hashRef env names body))
          return whnf
    _ -> applyFunc env func arg >>= removeDFscripts
-- evalExpr env (ApplyExpr func arg) = do
--   func <- evalExpr env func >>= appendDFscripts 0
--   arg <- evalExpr env arg
-- --  arg <- evalExpr env arg >>= fromTupleWHNF
-- --  let k = fromIntegral (length arg)
-- --  arg <-  mapM (\(_,j) -> appendDFscripts 0 j) (zip [1..k] arg) >>= makeITuple
--   case func of
--     Value (TensorData t@(Tensor ns fs js)) -> do
--       tMap (\f -> applyFunc env (Value f) arg >>= evalWHNF) t >>= fromTensor >>= return . Value >>= removeDFscripts
--     Intermediate (ITensor t@(Tensor ns fs js)) -> do
--       tMap (\f -> applyFunc env f arg) t >>= fromTensor
--     Value (MemoizedFunc name ref hashRef env names body) -> do
--       indices <- evalWHNF arg
--       indices' <- mapM fromEgison $ fromTupleValue indices
--       hash <- liftIO $ readIORef hashRef
--       case HL.lookup indices' hash of
--         Just objRef -> do
--           evalRef objRef
--         Nothing -> do
--           whnf <- applyFunc env (Value (Func Nothing env names body)) arg
--           retRef <- newEvaluatedObjectRef whnf
--           hash <- liftIO $ readIORef hashRef
--           liftIO $ writeIORef hashRef (HL.insert indices' retRef hash)
--           writeObjectRef ref (Value (MemoizedFunc name ref hashRef env names body))
--           return whnf
--     _ -> applyFunc env func arg >>= removeDFscripts

evalExpr env (WedgeApplyExpr func arg) = do
  func <- evalExpr env func >>= appendDFscripts 0
  arg <- evalExpr env arg >>= fromTupleWHNF
  let k = fromIntegral (length arg)
  arg <-  mapM (\(i,j) -> appendDFscripts i j) (zip [1..k] arg) >>= makeITuple
  case func of
    Value (TensorData t@(Tensor ns fs js)) -> do
      tMap (\f -> applyFunc env (Value f) arg >>= evalWHNF) t >>= fromTensor >>= return . Value
    Intermediate (ITensor t@(Tensor ns fs js)) -> do
      tMap (\f -> applyFunc env f arg) t >>= fromTensor
    Value (MemoizedFunc name ref hashRef env names body) -> do
      indices <- evalWHNF arg
      indices' <- mapM fromEgison $ fromTupleValue indices
      hash <- liftIO $ readIORef hashRef
      case HL.lookup indices' hash of
        Just objRef -> do
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
  mapM (\(x, y, z) -> do x' <- evalExprDeep env x
                         case x' of
                           (MemoizedFunc name ref hashRef env' names body) -> do
                             indices <- evalExprDeep env y
                             indices' <- mapM fromEgison $ fromTupleValue indices
                             hash <- liftIO $ readIORef hashRef
                             ret <- evalExprDeep env z
                             retRef <- newEvaluatedObjectRef (Value ret)
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
  xs <-  mapM (\ms -> applyFunc env fn (Value (makeTuple ms))) (map (\ms -> map toEgison ms) (enumTensorIndices ns))
  case (ns, xs) of
    _ -> fromTensor (Tensor ns (V.fromList xs) [])

evalExpr env (TensorContractExpr fnExpr tExpr) = do
  fn <- evalExpr env fnExpr
  whnf <- evalExpr env tExpr
  case whnf of
    (Intermediate (ITensor t@(Tensor _ _ _))) -> do
      ts <- tContract t
      tMapN (\xs -> do xs' <- mapM newEvaluatedObjectRef xs
                       applyFunc env fn (Intermediate (ITuple xs'))) ts >>= fromTensor
    (Value (TensorData t@(Tensor _ _ _))) -> do
      ts <- tContract t
      tMapN (\xs -> applyFunc' env fn (Tuple xs)) ts >>= fromTensor >>= return . Value
    _ -> return whnf
 where
  applyFunc' :: Env -> WHNFData -> EgisonValue -> EgisonM EgisonValue
  applyFunc' env fn x = applyFunc env fn (Value x) >>= evalWHNF

evalExpr env (TensorMapExpr fnExpr tExpr) = do
  fn <- evalExpr env fnExpr
  whnf <- evalExpr env tExpr
  case whnf of
    Intermediate (ITensor t) -> do
      tMap (applyFunc env fn) t >>= fromTensor
    Value (TensorData t) -> do
      tMap (applyFunc' env fn) t >>= fromTensor >>= return . Value
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
    (Intermediate (ITensor t1), Intermediate (ITensor t2)) -> do
      tMap2 (applyFunc'' env fn) t1 t2 >>= fromTensor
    (Intermediate (ITensor t), Value (TensorData (Tensor ns xs js))) -> do
      let xs' = V.map Value xs
      tMap2 (applyFunc'' env fn) t (Tensor ns xs' js) >>= fromTensor
    (Value (TensorData (Tensor ns xs js)), Intermediate (ITensor t)) -> do
      let xs' = V.map Value xs
      tMap2 (applyFunc'' env fn) (Tensor ns xs' js) t >>= fromTensor
    (Value (TensorData t1), Value (TensorData t2)) -> do
      tMap2 (\x y -> applyFunc' env fn (Tuple [x, y])) t1 t2 >>= fromTensor >>= return . Value
    -- an argument is scalar
    (Intermediate (ITensor (Tensor ns xs js)), whnf) -> do
      ys <- V.mapM (\x -> (applyFunc'' env fn x whnf)) xs
      return $ Intermediate (ITensor (Tensor ns ys js))
    (whnf, Intermediate (ITensor (Tensor ns xs js))) -> do
      ys <- V.mapM (\x -> (applyFunc'' env fn whnf x)) xs
      return $ Intermediate (ITensor (Tensor ns ys js))
    (Value (TensorData (Tensor ns xs js)), whnf) -> do
      ys <- V.mapM (\x -> (applyFunc'' env fn (Value x) whnf)) xs
      return $ Intermediate (ITensor (Tensor ns ys js))
    (whnf, Value (TensorData (Tensor ns xs js))) -> do
      ys <- V.mapM (\x -> (applyFunc'' env fn whnf (Value x))) xs
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

evalExpr env (ParExpr expr1 expr2) = undefined
evalExpr env (PseqExpr expr1 expr2) = undefined

evalExpr env (PmapExpr fnExpr cExpr) = do
  fn <- evalExpr env fnExpr
  xs <- evalExpr env cExpr >>= collectionToList
  ys <- parallelMapM (applyFunc' env fn) xs
  return $ Value $ Collection (Sq.fromList ys)
 where
  applyFunc' :: Env -> WHNFData -> EgisonValue -> EgisonM EgisonValue
  applyFunc' env fn x = applyFunc env fn (Value x) >>= evalWHNF
  

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
evalWHNF (Intermediate (ITensor (Tensor ns whnfs js))) = do
  vals <- mapM evalWHNF (V.toList whnfs)
  return $ TensorData $ Tensor ns (V.fromList vals) js
evalWHNF coll = Collection <$> (fromCollection coll >>= fromMList >>= mapM evalRefDeep . Sq.fromList)

addscript :: (Index EgisonValue, Tensor a) -> Tensor a
addscript (subj, (Tensor s t i)) = (Tensor s t (i ++ [subj]))

valuetoTensor2 :: WHNFData -> Tensor WHNFData
valuetoTensor2 (Intermediate (ITensor t)) = t

applyFunc :: Env -> WHNFData -> WHNFData -> EgisonM WHNFData
applyFunc env (Value (TensorData (Tensor s1 t1 i1))) tds = do
    tds <- fromTupleWHNF tds
    if (length s1) > (length i1) && (all (\(Intermediate (ITensor (Tensor s u i))) -> ((length s) - (length i) == 1)) tds)
       then do
            symId <- fresh
            let argnum = length tds
                subjs = map (\symName -> Subscript $ symbolScalarData symId (show symName)) [1 .. argnum]
                supjs = map (\symName -> Superscript $ symbolScalarData symId (show symName)) [1 .. argnum]
            dot <- evalExpr env (VarExpr $ stringToVar ".")
            makeITuple ((Value (TensorData (Tensor s1 t1 (i1 ++ supjs)))):(map Intermediate (map (ITensor . addscript) (zip subjs $ map valuetoTensor2 tds)))) >>= applyFunc env dot
       else throwError $ Default "applyfunc"

applyFunc env (Intermediate (ITensor (Tensor s1 t1 i1))) tds = do
    tds <- fromTupleWHNF tds
    if (length s1) > (length i1) && (all (\(Intermediate (ITensor (Tensor s u i))) -> ((length s) - (length i) == 1)) tds)
       then do
            symId <- fresh
            let argnum = length tds
                subjs = map (\symName -> Subscript $ symbolScalarData symId (show symName)) [1 .. argnum]
                supjs = map (\symName -> Superscript $ symbolScalarData symId (show symName)) [1 .. argnum]
            dot <- evalExpr env (VarExpr $ stringToVar ".")
            makeITuple (map Intermediate (ITensor (Tensor s1 t1 (i1 ++ supjs)):(map (ITensor . addscript) (zip subjs $ map valuetoTensor2 tds)))) >>= applyFunc env dot
       else throwError $ Default "applyfunc"

applyFunc _ (Value (PartialFunc env n body)) arg = do
  refs <- fromTuple arg
  if n == fromIntegral (length refs)
    then evalExpr (extendEnv env $ makeBindings (map (\n -> (stringToVar $ "::" ++ show n)) [1..n]) refs) body
    else throwError $ ArgumentsNumWithNames ["partial"] (fromIntegral n) (length refs)
applyFunc _ (Value (Func _ env [name] body)) arg = do
  ref <- newEvaluatedObjectRef arg
  evalExpr (extendEnv env $ makeBindings' [name] [ref]) body
applyFunc _ (Value (Func _ env names body)) arg = do
  refs <- fromTuple arg
  if length names == length refs
    then evalExpr (extendEnv env $ makeBindings' names refs) body
    else throwError $ ArgumentsNumWithNames names (length names) (length refs)
applyFunc _ (Value (Proc _ env [name] body)) arg = do
  ref <- newEvaluatedObjectRef arg
  evalExpr (extendEnv env $ makeBindings' [name] [ref]) body
applyFunc _ (Value (Proc _ env names body)) arg = do
  refs <- fromTuple arg
  if length names == length refs
    then evalExpr (extendEnv env $ makeBindings' names refs) body
    else throwError $ ArgumentsNumWithNames names (length names) (length refs)
applyFunc _ (Value (CFunc _ env name body)) arg = do
  refs <- fromTuple arg
  seqRef <- liftIO . newIORef $ Sq.fromList (map IElement refs)
  col <- liftIO . newIORef $ WHNF $ Intermediate $ ICollection $ seqRef
  if length refs > 0
    then evalExpr (extendEnv env $ makeBindings' [name] [col]) body
    else throwError $ ArgumentsNumWithNames [name] 1 0
applyFunc env (Value (Macro [name] body)) arg = do
  ref <- newEvaluatedObjectRef arg
  evalExpr (extendEnv env $ makeBindings' [name] [ref]) body
applyFunc env (Value (Macro names body)) arg = do
  refs <- fromTuple arg
  if length names == length refs
    then evalExpr (extendEnv env $ makeBindings' names refs) body
    else throwError $ ArgumentsNumWithNames names (length names) (length refs)
applyFunc _ (Value (PrimitiveFunc _ func)) arg = func arg
applyFunc _ (Value (IOFunc m)) arg = do
  case arg of
     Value World -> m
     _ -> throwError $ TypeMismatch "world" arg
applyFunc _ (Value (QuotedFunc fn)) arg = do
  args <- tupleToList arg
  mExprs <- mapM extractScalar args
  return (Value (ScalarData (Div (Plus [(Term 1 [(Apply fn mExprs, 1)])]) (Plus [(Term 1 [])]))))
applyFunc _ (Value fn@(ScalarData (Div (Plus [(Term 1 [(Symbol _ _ _, 1)])]) (Plus [(Term 1 [])])))) arg = do
  args <- tupleToList arg
  mExprs <- mapM extractScalar args
  return (Value (ScalarData (Div (Plus [(Term 1 [(Apply fn mExprs, 1)])]) (Plus [(Term 1 [])]))))
applyFunc _ whnf _ = throwError $ TypeMismatch "function" whnf

refArray :: WHNFData -> [EgisonValue] -> EgisonM WHNFData
refArray val [] = return val 
refArray (Value (Array array)) (index:indices) = do
  if isInteger index
    then do i <- (liftM fromInteger . fromEgison) index
            if (\(a,b) -> if a <= i && i <= b then True else False) $ Array.bounds array
              then refArray (Value (array ! i)) indices
              else return  $ Value Undefined
    else case index of
           (ScalarData (Div (Plus [(Term 1 [(Symbol _ _ [], 1)])]) (Plus [(Term 1 [])]))) -> do
             let (_,size) = Array.bounds array
             elms <- mapM (\arr -> refArray (Value arr) indices) (Array.elems array)
             elmRefs <- mapM newEvaluatedObjectRef elms
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
           (ScalarData (Div (Plus [(Term 1 [(Symbol _ _ [], 1)])]) (Plus [(Term 1 [])]))) -> do
             let (_,size) = Array.bounds array
             let refs = Array.elems array
             arrs <- mapM evalRef refs
             elms <- mapM (\arr -> refArray arr indices) arrs
             elmRefs <- mapM newEvaluatedObjectRef elms
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

recursiveRebind :: Env -> (Var, EgisonExpr) -> EgisonM Env
recursiveRebind env (name, expr) = do
  case refVar env name of
    Nothing -> throwError $ UnboundVariable $ show name
    Just ref -> case expr of
                  MemoizedLambdaExpr names body -> do
                    hashRef <- liftIO $ newIORef HL.empty
                    liftIO . writeIORef ref . WHNF . Value $ MemoizedFunc (Just name) ref hashRef env names body
                  LambdaExpr args body -> do
                    whnf <- evalExpr env expr
                    case whnf of
                      (Value (Func _ env args body)) -> liftIO . writeIORef ref . WHNF $ (Value (Func (Just name) env args body))
                  CambdaExpr arg body -> do
                    whnf <- evalExpr env expr
                    case whnf of
                      (Value (CFunc _ env arg body)) -> liftIO . writeIORef ref . WHNF $ (Value (CFunc (Just name) env arg body))
                  _ -> liftIO . writeIORef ref . Thunk $ evalExpr env expr
  return env

--
-- Pattern Match
--

patternMatch :: Env -> EgisonPattern -> WHNFData -> Matcher -> EgisonM (MList EgisonM Match) 
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
    VarPat _ -> throwError $ Default $ "cannot use variable except in pattern function:" ++ show pattern

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
      let arg = target
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
      startNumRef <- newEvaluatedObjectRef $ Value $ toEgison (startNum - 1)
      ends' <- evalExpr env' ends
      if isPrimitiveValue ends'
        then do 
          endsRef <- newEvaluatedObjectRef ends'
          inners <- liftIO $ newIORef $ Sq.fromList [IElement endsRef]
          endsRef' <- liftIO $ newIORef (WHNF (Intermediate (ICollection inners)))
          return $ msingleton $ MState env ((LoopPatContext (name, startNumRef) endsRef' endPat pat pat'):loops) bindings ((MAtom ContPat target matcher):trees)
        else do
          endsRef <- newEvaluatedObjectRef ends'
          return $ msingleton $ MState env ((LoopPatContext (name, startNumRef) endsRef endPat pat pat'):loops) bindings ((MAtom ContPat target matcher):trees)
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
              carEndsNum <- evalRef carEndsRef >>= fromWHNF
              if startNum > carEndsNum
                then return MNil
                else if startNum == carEndsNum
                       then return $ fromList [MState env loops' bindings ((MAtom endPat startNumWhnf Something):(MAtom pat' target matcher):trees),
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
            targets <- evalRef ref >>= fromTupleWHNF
            let trees' = zipWith3 MAtom patterns targets matchers ++ trees
            return $ MState env loops bindings trees'
            
        Tuple matchers -> do
          case pattern of
            ValuePat _ -> return $ msingleton $ MState env loops bindings ((MAtom pattern target Something):trees)
            WildCard -> return $ msingleton $ MState env loops bindings ((MAtom pattern target Something):trees)
            PatVar _ -> return $ msingleton $ MState env loops bindings ((MAtom pattern target Something):trees)
            IndexedPat _ _ -> return $ msingleton $ MState env loops bindings ((MAtom pattern target Something):trees)
            TuplePat patterns -> do
              targets <- fromTupleWHNF target
              if not (length patterns == length targets) then throwError $ ArgumentsNum (length patterns) (length targets) else return ()
              if not (length patterns == length matchers) then throwError $ ArgumentsNum (length patterns) (length matchers) else return ()
              let trees' = zipWith3 MAtom patterns targets matchers ++ trees
              return $ msingleton $ MState env loops bindings trees'
            _ ->  throwError $ Default $ "should not reach here. matcher: " ++ show matcher ++ ", pattern:  " ++ show pattern

        Something ->
          case pattern of
            ValuePat valExpr -> do
              val <- evalExprDeep env' valExpr
              tgtVal <- evalWHNF target
              if val == tgtVal
                then return $ msingleton $ MState env loops bindings trees
                else return MNil
            WildCard -> return $ msingleton $ MState env loops bindings trees
            PatVar name -> do
              targetRef <- newEvaluatedObjectRef target
              return $ msingleton $ MState env loops ((name, targetRef):bindings) trees
            IndexedPat (PatVar name) indices -> do
              indices <- mapM (evalExpr env' >=> liftM fromInteger . fromWHNF) indices
              case lookup name bindings of
                Just ref -> do
                  obj <- evalRef ref >>= updateHash indices >>= newEvaluatedObjectRef
                  return $ msingleton $ MState env loops (subst name obj bindings) trees
                Nothing  -> do
                  obj <- updateHash indices (Intermediate . IIntHash $ HL.empty) >>= newEvaluatedObjectRef
                  return $ msingleton $ MState env loops ((name,obj):bindings) trees
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
                  keys <- return $ HL.keys hash
                  vals <- mapM (newEvaluatedObjectRef . Value) $ HL.elems hash
                  updateHash indices (Intermediate $ IIntHash $ HL.fromList $ zip keys vals)
                updateHash _ v = throwError $ Default $ "expected hash value: " ++ show v
                subst :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
                subst k nv ((k', v'):xs) | k == k'   = (k', nv):(subst k nv xs)
                                         | otherwise = (k', v'):(subst k nv xs)
                subst _ _ [] = []
            IndexedPat pattern indices -> throwError $ Default ("invalid indexed-pattern: " ++ show pattern) 
            TuplePat patterns -> do
              targets <- fromTupleWHNF target
              if not (length patterns == length targets) then throwError $ ArgumentsNum (length patterns) (length targets) else return ()
              let trees' = zipWith3 MAtom patterns targets (take (length patterns) (repeat Something)) ++ trees
              return $ msingleton $ MState env loops bindings trees'
            _ -> throwError $ Default "something can only match with a pattern variable"
        _ ->  throwError $ EgisonBug $ "should not reach here. matcher: " ++ show matcher ++ ", pattern:  " ++ show pattern

inductiveMatch :: Env -> EgisonPattern -> WHNFData -> Matcher ->
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
  failPPPatternMatch = throwError $ Default "failed primitive pattern pattern match"
  failPDPatternMatch = throwError $ Default "failed primitive data pattern match"

primitivePatPatternMatch :: Env -> PrimitivePatPattern -> EgisonPattern ->
                            MatchM ([EgisonPattern], [Binding])
primitivePatPatternMatch _ PPWildCard _ = return ([], [])
primitivePatPatternMatch _ PPPatVar pattern = return ([pattern], [])
primitivePatPatternMatch env (PPValuePat name) (ValuePat expr) = do
  ref <- lift $ newObjectRef env expr
  return ([], [(stringToVar name, ref)])
primitivePatPatternMatch env (PPInductivePat name patterns) (InductivePat name' exprs)
  | name == name' && length patterns == length exprs =
    (concat *** concat) . unzip <$> zipWithM (primitivePatPatternMatch env) patterns exprs
  | otherwise = matchFail
primitivePatPatternMatch _ _ _ = matchFail

primitiveDataPatternMatch :: PrimitiveDataPattern -> WHNFData -> MatchM [Binding]
primitiveDataPatternMatch PDWildCard _ = return []
primitiveDataPatternMatch (PDPatVar name) whnf = do
  ref <- lift $ newEvaluatedObjectRef whnf
  return [(stringToVar name, ref)]
primitiveDataPatternMatch (PDInductivePat name patterns) whnf = do
  case whnf of
    Intermediate (IInductiveData name' refs) | name == name' -> do
      whnfs <- lift $ mapM evalRef refs
      concat <$> zipWithM primitiveDataPatternMatch patterns whnfs
    Value (InductiveData name' vals) | name == name' -> do
      let whnfs = map Value vals
      concat <$> zipWithM primitiveDataPatternMatch patterns whnfs
    _ -> matchFail
primitiveDataPatternMatch (PDTuplePat patterns) whnf = do
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
  target <- (either (const matchFail) return . extractPrimitiveValue) whnf
  isEqual <- lift $ (==) <$> evalExprDeep nullEnv expr <*> pure target
  if isEqual then return [] else matchFail

expandCollection :: WHNFData -> EgisonM (Seq Inner)
expandCollection (Value (Collection vals)) =
  mapM (liftM IElement . newEvaluatedObjectRef . Value) vals
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
      lift $ (,) <$> newEvaluatedObjectRef (Value val)
                 <*> newEvaluatedObjectRef (Value $ Collection vals)
unconsCollection coll@(Intermediate (ICollection innersRef)) = do
  inners <- liftIO $ readIORef innersRef
  case Sq.viewl inners of
    EmptyL -> matchFail
    (IElement ref') :< tInners -> do
      tInnersRef <- liftIO $ newIORef tInners
      lift $ (ref', ) <$> newEvaluatedObjectRef (Intermediate $ ICollection tInnersRef)
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
      lift $ (,) <$> newEvaluatedObjectRef (Value $ Collection vals)
                 <*> newEvaluatedObjectRef (Value val)
unsnocCollection coll@(Intermediate (ICollection innersRef)) = do
  inners <- liftIO $ readIORef innersRef
  case Sq.viewr inners of
    EmptyR -> matchFail
    hInners :> (IElement ref') -> do
      hInnersRef <- liftIO $ newIORef hInners
      lift $ (, ref') <$> newEvaluatedObjectRef (Intermediate $ ICollection hInnersRef)
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
fromTuple (Value (Tuple vals)) = mapM (newEvaluatedObjectRef . Value) vals
fromTuple whnf = return <$> newEvaluatedObjectRef whnf

fromTupleWHNF :: WHNFData -> EgisonM [WHNFData]
fromTupleWHNF (Intermediate (ITuple refs)) = mapM evalRef refs
fromTupleWHNF (Value (Tuple vals)) = return $ map Value vals
fromTupleWHNF whnf = return [whnf]

fromTupleValue :: EgisonValue -> [EgisonValue]
fromTupleValue (Tuple vals) = vals
fromTupleValue val = [val]

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
  collectionToList' val
 where
  collectionToList' :: EgisonValue -> EgisonM [EgisonValue]
  collectionToList' (Collection sq) = return $ toList sq
  collectionToList' val = throwError $ TypeMismatch "collection" (Value val)

makeTuple :: [EgisonValue] -> EgisonValue
makeTuple [] = Tuple []
makeTuple [x] = x
makeTuple xs = Tuple xs

makeITuple :: [WHNFData] -> EgisonM WHNFData
makeITuple [] = return $ Intermediate (ITuple [])
makeITuple [x] = return $ x
makeITuple xs = mapM newEvaluatedObjectRef xs >>= (return . Intermediate . ITuple)

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
