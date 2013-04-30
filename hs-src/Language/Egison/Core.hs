module Language.Egison.Core where

import Control.Arrow
import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Trans.Maybe

import Data.IORef
import Data.List
import Data.Maybe

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Text.Parsec.ByteString.Lazy (parseFromFile)
import System.Directory (doesFileExist)

import Language.Egison.Types
import Language.Egison.Parser
import Language.Egison.Desugar
import Paths_egison (getDataFileName)

--
-- Evaluator
--

evalTopExprs :: Env -> [EgisonTopExpr] -> EgisonM Env
evalTopExprs env exprs = do
  let (bindings, rest) = foldr collectDefs ([], []) exprs
  env <- recursiveBind env bindings
  forM_ rest $ evalTopExpr env
  return env
 where
  collectDefs (Define name expr) (bindings, rest) = ((name, expr) : bindings, rest)
  collectDefs expr (bindings, rest) = (bindings, expr : rest)  

evalTopExpr :: Env -> EgisonTopExpr -> EgisonM Env
evalTopExpr env (Define name expr) = recursiveBind env [(name, expr)]
evalTopExpr env (Test expr) = do
  val <- evalExpr' env expr
  liftIO $ print val
  return env
evalTopExpr env (Execute argv) = do
  main <- refVar env "main" >>= evalRef
  io <- applyFunc main $ Value $ Collection $ map String argv
  case io of
    Value (IOFunc m) -> m >> return env
    _ -> throwError $ TypeMismatch "io" main
evalTopExpr env (Load file) = loadLibraryFile file >>= evalTopExprs env
evalTopExpr env (LoadFile file) = loadFile file >>= evalTopExprs env

loadFile :: FilePath -> EgisonM [EgisonTopExpr]
loadFile file = do
  doesExist <- liftIO $ doesFileExist file
  unless doesExist $ throwError $ strMsg ("file does not exist: " ++ file)
  input <- liftIO $ readFile file
  exprs <-  liftEgisonM $ readTopExprs input
  concat <$> mapM recursiveLoad exprs
 where
  recursiveLoad (Load file) = loadFile file
  recursiveLoad (LoadFile file) = loadLibraryFile file
  recursiveLoad expr = return [expr]

loadLibraryFile :: FilePath -> EgisonM [EgisonTopExpr]
loadLibraryFile file = liftIO (getDataFileName file) >>= loadFile

evalExpr :: Env -> EgisonExpr -> EgisonM WHNFData
evalExpr _ (CharExpr c) = return . Value $ Char c
evalExpr _ (StringExpr s) = return . Value $ String s 
evalExpr _ (BoolExpr b) = return . Value $ Bool b
evalExpr _ (IntegerExpr i) = return . Value $ Integer i
evalExpr _ (FloatExpr d) = return . Value $ Float d

evalExpr env (VarExpr name) = refVar env name >>= evalRef

evalExpr _ (InductiveDataExpr name []) = return . Value $ InductiveData name []
evalExpr env (InductiveDataExpr name exprs) =
  Intermediate . IInductiveData name <$> mapM (newThunk env) exprs 

evalExpr _ (TupleExpr []) = return . Value $ Tuple []
evalExpr env (TupleExpr [expr]) = evalExpr env expr
evalExpr env (TupleExpr exprs) =
  Intermediate . ITuple <$> mapM (newThunk env) exprs 

evalExpr _ (CollectionExpr []) = return . Value $ Collection []
evalExpr env (CollectionExpr inners) =
  Intermediate . ICollection <$> mapM fromInnerExpr inners
 where
  fromInnerExpr (ElementExpr expr) = IElement <$> newThunk env expr
  fromInnerExpr (SubCollectionExpr expr) = ISubCollection <$> newThunk env expr

evalExpr env (ArrayExpr exprs) = do
  ref' <- mapM (newThunk env) exprs
  return . Intermediate . IArray $ IntMap.fromList $ zip (enumFromTo 1 (length exprs)) ref'

evalExpr env (IndexedExpr expr indices) = do
  array <- evalExpr env expr
  indices <- mapM (evalExpr env >=> liftError . liftM fromInteger . fromIntegerValue) indices
  refArray array indices
 where
  refArray :: WHNFData -> [Int] -> EgisonM WHNFData
  refArray val [] = return val 
  refArray (Value (Array array)) (index:indices) =
    case IntMap.lookup index array of
      Just val -> refArray (Value val) indices
      Nothing -> return $ Value Undefined
  refArray (Intermediate (IArray array)) (index:indices) =
    case IntMap.lookup index array of
      Just ref -> evalRef ref >>= flip refArray indices
      Nothing -> return $ Value Undefined
  refArray val _ = throwError $ TypeMismatch "array" val

evalExpr env (LambdaExpr names expr) = return . Value $ Func env names expr 

evalExpr env (IfExpr test expr expr') = do
  test <- evalExpr env test >>= liftError . fromBoolValue
  evalExpr env $ if test then expr else expr'

evalExpr env (LetExpr bindings expr) =
  mapM extractBindings bindings >>= flip evalExpr expr . extendEnv env . concat
 where
  extractBindings :: BindingExpr -> EgisonM [Binding]
  extractBindings ([name], expr) =
    makeBindings [name] . (:[]) <$> newThunk env expr
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
          let pattern = TupleExpr $ flip map [1..k] $ \i ->
                if i == n
                  then PatternExpr $ PatVar "#_"
                  else PatternExpr WildCard
          in MatchExpr target matcher [(pattern, VarExpr "#_")]
    return ((var, expr) : map (second nth) (zip names [1..]))

  genVar :: State Int String
  genVar = modify (1+) >> gets (('#':) . show)

evalExpr env (DoExpr bindings expr) = return $ Value $ IOFunc $ do
  let body = foldr genLet (TupleExpr [VarExpr "#1", expr]) bindings
  applyFunc (Value $ Func env ["#1"] body) $ Value World
 where
  genLet (names, expr) expr' =
    LetExpr [(["#1", "#2"], ApplyExpr expr $ TupleExpr [VarExpr "#1"])] $
    LetExpr [(names, VarExpr "#2")] expr'

evalExpr env (MatchAllExpr target matcher (pattern, expr)) = do
  target <- newThunk env target
  
  matcher <- evalExpr env matcher
  result <- patternMatch env pattern target matcher
  mmap (flip evalExpr expr . extendEnv env) result >>= fromMList
 where
  fromMList :: MList EgisonM WHNFData -> EgisonM WHNFData
  fromMList MNil = return . Value $ Collection []
  fromMList (MCons val m) = do
    head <- IElement <$> newEvaluatedThunk val
    tail <- ISubCollection <$> (liftIO . newIORef . Thunk $ m >>= fromMList)
    return . Intermediate $ ICollection [head, tail]

evalExpr env (MatchExpr target matcher clauses) = do
  target <- newThunk env target
  matcher <- evalExpr env matcher
  let tryMatchClause (pattern, expr) cont = do
        result <- patternMatch env pattern target matcher
        case result of
          MCons bindings _ -> evalExpr (extendEnv env bindings) expr
          MNil -> cont
  foldr tryMatchClause (throwError $ strMsg "failed pattern match") clauses

evalExpr env (FunctionExpr matcher clauses) =
  return . Value $ Func env ["#_"] (MatchExpr (VarExpr "#_") matcher clauses)

evalExpr env (ApplyExpr func arg) = do
  func <- evalExpr env func
  arg <- evalExpr env arg
  applyFunc func arg

evalExpr env (MatcherExpr info) = return $ Value $ Matcher (env, info)

evalExpr env (GenerateArrayExpr (name:[]) (TupleExpr (size:[])) expr) =
  generateArray env name size expr
evalExpr env (GenerateArrayExpr (name:xs) (TupleExpr (size:ys)) expr) = 
  generateArray env name size (GenerateArrayExpr xs (TupleExpr ys) expr)
evalExpr _ (GenerateArrayExpr _ _ _) = throwError $ strMsg "invalid generate-array expression"

evalExpr env (ArraySizeExpr expr) = 
  evalExpr env expr >>= arraySize
  where
    arraySize :: WHNFData -> EgisonM WHNFData
    arraySize (Intermediate (IArray vals)) = return . Value . Integer . toInteger $ IntMap.size vals
    arraySize (Value (Array vals))         = return . Value . Integer . toInteger $ IntMap.size vals
    arraySize val                          = throwError $ TypeMismatch "array" val

evalExpr _ (PatternExpr pattern) = return $ Value $ Pattern pattern

evalExpr _ SomethingExpr = return $ Value Something
evalExpr _ UndefinedExpr = return $ Value Undefined
evalExpr _ expr = throwError $ NotImplemented ("evalExpr for " ++ show expr)

evalExpr' :: Env -> EgisonExpr -> EgisonM EgisonValue
evalExpr' env expr = evalExpr env expr >>= evalDeep

evalRef :: ObjectRef -> EgisonM WHNFData
evalRef ref = do
  obj <- liftIO $ readIORef ref
  case obj of
    WHNF val -> return val
    Thunk thunk -> do
      val <- thunk
      writeThunk ref val
      return val

evalRef' :: ObjectRef -> EgisonM EgisonValue
evalRef' ref = do
  obj <- liftIO $ readIORef ref
  case obj of
    WHNF (Value val) -> return val
    WHNF val -> do
      val <- evalDeep val
      writeThunk ref $ Value val
      return val
    Thunk thunk -> do
      val <- thunk >>= evalDeep
      writeThunk ref $ Value val
      return val

evalDeep :: WHNFData -> EgisonM EgisonValue
evalDeep (Value val) = return val
evalDeep (Intermediate (IInductiveData name refs)) =
  InductiveData name <$> mapM evalRef' refs
evalDeep (Intermediate (IArray refs)) = do
  refs' <- mapM evalRef' $ IntMap.elems refs
  return $ Array $ IntMap.fromList $ zip (enumFromTo 1 (IntMap.size refs)) refs'
evalDeep (Intermediate (ITuple refs)) = Tuple <$> mapM evalRef' refs
evalDeep coll = Collection <$> (fromCollection coll >>= fromMList >>= mapM evalRef')

applyFunc :: WHNFData -> WHNFData -> EgisonM WHNFData
applyFunc (Value (Func env [name] body)) arg = do
  ref <- newEvaluatedThunk arg
  evalExpr (extendEnv env $ makeBindings [name] [ref]) body
applyFunc (Value (Func env names body)) arg = do
  refs <- fromTuple arg
  if length names == length refs
    then evalExpr (extendEnv env $ makeBindings names refs) body
    else throwError $ ArgumentsNum (length names) (length refs)
applyFunc (Value (PrimitiveFunc func)) arg =
  fromTuple arg >>= mapM evalRef >>= liftM Value . func
applyFunc (Value (IOFunc m)) arg = do
  case arg of
     Value World -> m
     _ -> throwError $ TypeMismatch "world" arg
applyFunc val _ = throwError $ TypeMismatch "function" val

generateArray :: Env -> String -> EgisonExpr -> EgisonExpr -> EgisonM WHNFData
generateArray env name size expr = do  
  size' <- evalExpr env size >>= either throwError (return . fromInteger) . fromIntegerValue
  elems <- mapM genElem (enumFromTo 1 size')
  return $ Intermediate $ IArray $ IntMap.fromList elems
  where
    genElem :: Int -> EgisonM (Int, ObjectRef)
    genElem i = do env <- bindEnv env name $ toInteger i
                   val <- evalExpr env expr >>= newEvaluatedThunk                   
                   return (i, val)
    
    bindEnv :: Env -> String -> Integer -> EgisonM Env
    bindEnv env name i = do
      ref <- liftIO $ newIORef (WHNF . Value . Integer $ i)
      return $ extendEnv env [(name, ref)]

newThunk :: Env -> EgisonExpr -> EgisonM ObjectRef
newThunk env expr = liftIO . newIORef . Thunk $ evalExpr env expr

writeThunk :: ObjectRef -> WHNFData -> EgisonM ()
writeThunk ref val = liftIO . writeIORef ref $ WHNF val

newEvaluatedThunk :: WHNFData -> EgisonM ObjectRef
newEvaluatedThunk = liftIO . newIORef . WHNF

makeBindings :: [String] -> [ObjectRef] -> [Binding]
makeBindings = zip

recursiveBind :: Env -> [(String, EgisonExpr)] -> EgisonM Env
recursiveBind env bindings = do
  let (names, exprs) = unzip bindings
  refs <- replicateM (length bindings) $ newThunk nullEnv UndefinedExpr
  let env' = extendEnv env $ makeBindings names refs
  zipWithM_ (\ref expr -> liftIO . writeIORef ref . Thunk $ evalExpr env' expr) refs exprs
  return env'

fromArray :: WHNFData -> EgisonM [ObjectRef]
fromArray (Intermediate (IArray refs)) = return $ IntMap.elems refs
fromArray (Value (Array vals)) = mapM (newEvaluatedThunk . Value) $ IntMap.elems vals
fromArray val = throwError $ TypeMismatch "array" val

fromTuple :: WHNFData -> EgisonM [ObjectRef]
fromTuple (Intermediate (ITuple refs)) = return refs
fromTuple (Value (Tuple vals)) = mapM (newEvaluatedThunk . Value) vals
fromTuple val = return <$> newEvaluatedThunk val

fromCollection :: WHNFData -> EgisonM (MList EgisonM ObjectRef)
fromCollection (Value (Collection [])) = return MNil
fromCollection (Value (Collection vals)) =
  fromList <$> mapM (newEvaluatedThunk . Value) vals
fromCollection coll@(Intermediate (ICollection _)) =
  newEvaluatedThunk coll >>= fromCollection'
 where
  fromCollection' ref = do
    isEmpty <- isEmptyCollection ref
    if isEmpty
      then return MNil
      else do
        (head, tail) <- fromJust <$> runMaybeT (unconsCollection ref)
        return $ MCons head (fromCollection' tail)
fromCollection val = throwError $ TypeMismatch "collection" val

--
-- Pattern Match
--

patternMatch :: Env -> EgisonExpr -> ObjectRef -> WHNFData ->
                EgisonM (MList EgisonM [Binding]) 
patternMatch env pattern target matcher =
  processMState (MState env [] [MAtom pattern target matcher]) >>= processMStates . (:[])

processMStates :: [MList EgisonM MatchingState] -> EgisonM (MList EgisonM [Binding])
processMStates [] = return MNil
processMStates streams = do
  let (bindings, streams') = (catMaybes *** concat) . unzip $ map processMStates' streams
  mappend (fromList bindings) (sequence streams' >>= processMStates)
 where
  processMStates' :: MList EgisonM MatchingState ->
                     (Maybe [Binding], [EgisonM (MList EgisonM MatchingState)])
  processMStates' MNil = (Nothing, [])
  processMStates' (MCons (MState _ bindings []) states) = (Just bindings, [states])
  processMStates' (MCons state states) = (Nothing, [processMState state, states])

processMState :: MatchingState -> EgisonM (MList EgisonM MatchingState)
processMState state@(MState env bindings []) = throwError $ strMsg "should not reach here"
processMState (MState env bindings ((MAtom pattern target matcher):trees)) = do
  let env' = extendEnv env bindings
  pattern <- evalPattern env' pattern
  case pattern of
    VarExpr _ -> throwError $ strMsg "cannot use variable in pattern"
    ApplyExpr func arg -> do
      func <- evalExpr env' func
      case func of
        Value (Func env [name] expr) -> do
          penv <- (:[]) . (,) name <$> evalPattern env' arg
          return $ msingleton $ MState env bindings (MNode penv (MState env [] [MAtom expr target matcher]) : trees)
        Value (Func env names expr) -> do
          let args = case arg of
                       TupleExpr exprs -> exprs
                       expr -> [expr]
          penv <- zip names <$> mapM (evalPattern env') args
          return $ msingleton $ MState env bindings (MNode penv (MState env [] [MAtom expr target matcher]) : trees)
        _ -> throwError $ TypeMismatch "pattern constructor" func
    IndexedExpr (PatternExpr (PatVar name)) indices -> do
      indices <- mapM (evalExpr env' >=> liftError . liftM fromInteger . fromIntegerValue) indices
      let (index:_) = indices 
      case lookup name bindings of
        Just ref -> do
          val <- evalRef ref
          case val of
            Intermediate (IArray val) -> do
              obj <- newEvaluatedThunk $ Intermediate . IArray $ IntMap.insert index target val
              return $ msingleton $ MState env (subst name obj bindings) trees
            Value (Array val) -> do
              keys' <- return $ IntMap.keys val
              vals' <- mapM (newEvaluatedThunk . Value) $ IntMap.elems val
              obj <- newEvaluatedThunk $ Intermediate . IArray $ IntMap.insert index target (IntMap.fromList $ zip keys' vals')
              return $ msingleton $ MState env (subst name obj bindings) trees
        Nothing  -> do
          obj <- newEvaluatedThunk $ Intermediate . IArray $ IntMap.singleton index target 
          return $ msingleton $ MState env ((name, obj):bindings) trees
       where
        subst :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
        subst k nv ((k', v'):xs) | k == k'   = (k', nv):(subst k nv xs)
                                 | otherwise = (k', v'):(subst k nv xs)
        subst _ _ [] = []
    IndexedExpr expr index -> throwError $ strMsg ("invalid indexed-pattern: " ++ show expr) 
    TupleExpr patterns -> do
      matchers <- fromTuple matcher >>= mapM evalRef
      targets <- evalRef target >>= fromTuple
      let trees' = zipWith3 MAtom patterns targets matchers ++ trees
      return $ msingleton $ MState env bindings trees'
    PatternExpr WildCard -> return $ msingleton $ MState env bindings trees 
    PatternExpr (AndPat patterns) ->
      let trees' = map (\pattern -> MAtom pattern target matcher) patterns ++ trees
      in return $ msingleton $ MState env bindings trees'
    PatternExpr (OrPat patterns) ->
      return $ fromList $ flip map patterns $ \pattern ->
        MState env bindings (MAtom pattern target matcher : trees)
    PatternExpr (NotPat pattern) -> do 
      results <- processMState (MState env bindings [MAtom pattern target matcher])
      case results of
        MNil -> return $ msingleton $ MState env bindings trees
        _    -> return $ MNil
    PatternExpr (CutPat pattern) -> -- TEMPORARY ignoring cut patterns
      return $ msingleton (MState env bindings ((MAtom pattern target matcher):trees))
    PatternExpr (PredPat pred) -> do
      func <- evalExpr env' pred
      arg <- evalRef target
      result <- applyFunc func arg >>= liftError . fromBoolValue
      if result then return $ msingleton $ (MState env bindings trees)
                else return MNil
    PatternExpr pattern' ->
      case matcher of
        Value Something -> 
          case pattern' of
            PatVar name -> do
              return $ msingleton $ MState env ((name, target):bindings) trees
            _ -> throwError $ strMsg "something can only match with a pattern variable"
        Value (Matcher matcher) -> do
          (patterns, targetss, matchers) <- inductiveMatch env' pattern target matcher
          mfor targetss $ \ref -> do
            targets <- evalRef ref >>= fromTuple
            let trees' = zipWith3 MAtom patterns targets matchers ++ trees
            return $ MState env bindings trees'
        _ -> throwError $ TypeMismatch "matcher" matcher
    _ -> throwError $ strMsg "should not reach here"
processMState (MState env bindings ((MNode penv (MState _ _ [])):trees)) =
  return $ msingleton $ MState env bindings trees
processMState (MState env bindings ((MNode penv state@(MState env' bindings' (tree:trees')):trees))) = do
  let env'' = extendEnv env' bindings
  case tree of
    MAtom pattern target matcher -> do
      pattern <- evalPattern env'' pattern
      case pattern of
        VarExpr name ->
          case lookup name penv of
            Just pattern ->
              return $ msingleton $ MState env bindings (MAtom pattern target matcher:MNode penv (MState env' bindings' trees'):trees)
            Nothing -> throwError $ UnboundVariable name
        IndexedExpr (VarExpr name) (index:_) ->
          case lookup name penv of
            Just pattern -> do
              index <- evalExpr env'' index >>= liftError . fromIntegerValue
              let pattern' = IndexedExpr pattern [IntegerExpr index]
              return $ msingleton $ MState env bindings (MAtom pattern' target matcher:MNode penv (MState env' bindings' trees'):trees)
            Nothing -> throwError $ UnboundVariable name
        _ -> processMState state >>= mmap (return . MState env bindings . (: trees) . MNode penv)
    _ -> processMState state >>= mmap (return . MState env bindings . (: trees) . MNode penv)

evalPattern :: Env -> EgisonExpr -> EgisonM EgisonExpr
evalPattern _ (TupleExpr [expr]) = return expr
evalPattern _ expr@(TupleExpr _) = return expr
evalPattern _ expr@(PatternExpr _) = return expr
evalPattern _ expr@(VarExpr _) = return expr
evalPattern _ expr@(IndexedExpr _ _) = return expr
evalPattern _ expr@(ApplyExpr _ _) = return expr
evalPattern env (IfExpr test expr expr') = do
  test <- evalExpr env test >>= liftError . fromBoolValue
  evalPattern env $ if test then expr else expr'  
evalPattern _ _ = throwError $ strMsg "pattern expression required"

inductiveMatch :: Env -> EgisonExpr -> ObjectRef -> Matcher ->
                  EgisonM ([EgisonExpr], MList EgisonM ObjectRef, [WHNFData])
inductiveMatch env pattern target (matcherEnv, clauses) = do
  foldr tryPPMatchClause failPPPatternMatch clauses
 where
  tryPPMatchClause (pat, matchers, clauses) cont = do
    result <- runMaybeT $ primitivePatPatternMatch env pat pattern
    case result of
      Just (patterns, bindings) -> do
        targetss <- foldr tryPDMatchClause failPDPatternMatch clauses
        matchers <- evalExpr matcherEnv matchers >>= fromTuple >>= mapM evalRef
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

primitivePatPatternMatch :: Env -> PrimitivePatPattern -> EgisonExpr ->
                            MatchM ([EgisonExpr], [Binding])
primitivePatPatternMatch _ PPWildCard _ = return ([], [])
primitivePatPatternMatch _ PPPatVar pattern = return ([pattern], [])
primitivePatPatternMatch env (PPValuePat name) pattern = do
  pattern <- lift $ evalPattern env pattern
  case pattern of
    PatternExpr (ValuePat expr) -> do
      ref <- lift $ newThunk env expr
      return ([], [(name, ref)])
    _ -> matchFail
primitivePatPatternMatch env (PPInductivePat name patterns) pattern = do
  pattern <- lift $ evalPattern env pattern
  case pattern of
    PatternExpr (InductivePattern name' exprs) | name == name' ->
      (concat *** concat) . unzip <$> zipWithM (primitivePatPatternMatch env) patterns exprs
    _ -> matchFail

primitiveDataPatternMatch :: PrimitiveDataPattern -> ObjectRef -> MatchM [Binding]
primitiveDataPatternMatch PDWildCard _ = return []
primitiveDataPatternMatch (PDPatVar name) ref = return [(name, ref)]
primitiveDataPatternMatch (PDInductivePat name patterns) ref = do
  val <- lift $ evalRef ref
  case val of
    Intermediate (IInductiveData name' refs) | name == name' ->
      concat <$> zipWithM primitiveDataPatternMatch patterns refs
    Value (InductiveData name' vals) | name == name' -> do
      refs <- lift $ mapM (newEvaluatedThunk . Value) vals
      concat <$> zipWithM primitiveDataPatternMatch patterns refs
    _ -> matchFail
primitiveDataPatternMatch PDEmptyPat ref = do
  isEmpty <- lift $ isEmptyCollection ref
  if isEmpty then return [] else matchFail
primitiveDataPatternMatch (PDConsPat pattern pattern') ref = do
  (head, tail) <- unconsCollection ref
  (++) <$> primitiveDataPatternMatch pattern head
       <*> primitiveDataPatternMatch pattern' tail
primitiveDataPatternMatch (PDSnocPat pattern pattern') ref = do
  (init, last) <- unsnocCollection ref
  (++) <$> primitiveDataPatternMatch pattern init
       <*> primitiveDataPatternMatch pattern' last
primitiveDataPatternMatch (PDConstantPat expr) ref = do
  target <- lift (evalRef ref) >>= either (const matchFail) return . fromPrimitiveValue
  isEqual <- lift $ (==) <$> evalExpr' nullEnv expr <*> pure target
  if isEqual then return [] else matchFail

expandCollection :: WHNFData -> EgisonM [Inner]
expandCollection (Value (Collection vals)) =
  mapM (liftM IElement . newEvaluatedThunk . Value) vals
expandCollection (Intermediate (ICollection inners)) = return inners
expandCollection val = throwError $ TypeMismatch "collection" val

isEmptyCollection :: ObjectRef -> EgisonM Bool
isEmptyCollection ref = evalRef ref >>= isEmptyCollection'
 where
  isEmptyCollection' :: WHNFData -> EgisonM Bool
  isEmptyCollection' (Value (Collection [])) = return True
  isEmptyCollection' (Intermediate (ICollection [])) = return True
  isEmptyCollection' (Intermediate (ICollection ((ISubCollection ref'):inners))) = do
    inners' <- evalRef ref' >>= expandCollection
    let coll = Intermediate (ICollection (inners' ++ inners))
    writeThunk ref coll
    isEmptyCollection' coll
  isEmptyCollection' _ = return False

unconsCollection :: ObjectRef -> MatchM (ObjectRef, ObjectRef)
unconsCollection ref = lift (evalRef ref) >>= unconsCollection'
 where
  unconsCollection' :: WHNFData -> MatchM (ObjectRef, ObjectRef)
  unconsCollection' (Value (Collection (val:vals))) =
    lift $ (,) <$> newEvaluatedThunk (Value val)
               <*> newEvaluatedThunk (Value $ Collection vals)
  unconsCollection' (Intermediate (ICollection ((IElement ref'):inners))) =
    lift $ (,) ref' <$> newEvaluatedThunk (Intermediate $ ICollection inners)
  unconsCollection' (Intermediate (ICollection ((ISubCollection ref'):inners))) = do
    inners' <- lift $ evalRef ref' >>= expandCollection
    let coll = Intermediate (ICollection (inners' ++ inners))
    lift $ writeThunk ref coll
    unconsCollection' coll
  unconsCollection' _ = matchFail

unsnocCollection :: ObjectRef -> MatchM (ObjectRef, ObjectRef)
unsnocCollection ref = lift (evalRef ref) >>= unsnocCollection'
  where
    unsnocCollection' :: WHNFData -> MatchM (ObjectRef, ObjectRef)
    unsnocCollection' (Value (Collection [])) = matchFail
    unsnocCollection' (Value (Collection vals)) =
      lift $ (,) <$> newEvaluatedThunk (Value . Collection $ init vals)
                 <*> newEvaluatedThunk (Value $ last vals)
    unsnocCollection' (Intermediate (ICollection [])) = matchFail
    unsnocCollection' (Intermediate (ICollection inners)) =
      case last inners of
        IElement ref' ->
          lift $ (,) <$> newEvaluatedThunk (Intermediate . ICollection $ init inners)
                     <*> pure ref'
        ISubCollection ref' -> do
          inners' <- lift $ evalRef ref' >>= expandCollection
          let coll = Intermediate (ICollection (init inners ++ inners'))
          lift $ writeThunk ref coll
          unsnocCollection' coll
