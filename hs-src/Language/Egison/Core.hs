{-# Language TupleSections #-}
module Language.Egison.Core where

import Prelude hiding (mapM)

import Control.Arrow
import Control.Applicative
import Control.Monad.Error hiding (mapM)
import Control.Monad.State hiding (mapM)
import Control.Monad.Trans.Maybe

import Data.Sequence (Seq, ViewL(..), ViewR(..), (><))
import qualified Data.Sequence as Sq
import Data.Traversable (mapM)
import Data.IORef
import Data.List
import Data.Maybe

import qualified Data.HashMap.Lazy as HL

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Lazy.Char8 as B

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

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
  (bindings, rest) <- collectDefs exprs [] []
  env <- recursiveBind env bindings
  forM_ rest $ evalTopExpr env
  return env
 where
  collectDefs (expr:exprs) bindings rest =
    case expr of
      Define name expr -> collectDefs exprs ((name, expr) : bindings) rest
      Load file -> do
        exprs' <- loadLibraryFile file
        collectDefs (exprs' ++ exprs) bindings rest
      LoadFile file -> do
        exprs' <- loadFile file
        collectDefs (exprs' ++ exprs) bindings rest
      _ -> collectDefs exprs bindings (expr : rest)
  collectDefs [] bindings rest = return (bindings, reverse rest)

evalTopExpr :: Env -> EgisonTopExpr -> EgisonM Env
evalTopExpr env (Define name expr) = recursiveBind env [(name, expr)]
evalTopExpr env (Test expr) = do
  val <- evalExpr' env expr
  liftIO $ print val
  return env
evalTopExpr env (Execute argv) = do
  main <- refVar env "main" >>= evalRef
  io <- applyFunc main $ Value $ Collection $ Sq.fromList $ map makeStringValue argv
  case io of
    Value (IOFunc m) -> m >> return env
    _ -> throwError $ TypeMismatch "io" io
evalTopExpr env (Load file) = loadLibraryFile file >>= evalTopExprs env
evalTopExpr env (LoadFile file) = loadFile file >>= evalTopExprs env

evalExpr :: Env -> EgisonExpr -> EgisonM WHNFData
evalExpr _ (CharExpr c) = return . Value $ Char c
evalExpr _ (StringExpr s) = return $ Value $ makeStringValue s
evalExpr _ (BoolExpr b) = return . Value $ Bool b
evalExpr _ (RationalExpr x) = return . Value $ Rational x
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

evalExpr env (CollectionExpr inners) =
  if Sq.null inners then
    return . Value $ Collection Sq.empty
  else
    Intermediate . ICollection <$> mapM fromInnerExpr inners
    where
      fromInnerExpr (ElementExpr expr) = IElement <$> newThunk env expr
      fromInnerExpr (SubCollectionExpr expr) = ISubCollection <$> newThunk env expr

evalExpr env (ArrayExpr exprs) = do
  ref' <- mapM (newThunk env) exprs
  return . Intermediate . IArray $ IntMap.fromList $ zip (enumFromTo 1 (length exprs)) ref'

evalExpr env (HashExpr assocs) = do
  let (keyExprs, exprs) = unzip assocs
  keyVals <- mapM (evalExpr' env) keyExprs
  keys <- liftError $ mapM makeKey keyVals
  refs <- mapM (newThunk env) exprs
  case head keys of
    IntKey _ -> do
      let keys' = map (\key -> case key of
                                 IntKey i -> i) keys
      return . Intermediate . IIntHash $ HL.fromList $ zip keys' refs
    StrKey _ -> do
      let keys' = map (\key -> case key of
                                 StrKey s -> s) keys
      return . Intermediate . IStrHash $ HL.fromList $ zip keys' refs

evalExpr env (IndexedExpr expr indices) = do
  array <- evalExpr env expr
  indices <- mapM (evalExpr' env) indices
  refArray array indices
 where
  refArray :: WHNFData -> [EgisonValue] -> EgisonM WHNFData
  refArray val [] = return val 
  refArray (Value (Array array)) (index:indices) = do
    i <- (liftError . liftM fromInteger . fromIntegerValue) (Value index)
    case IntMap.lookup i array of
      Just val -> refArray (Value val) indices
      Nothing -> return $ Value Undefined
  refArray (Intermediate (IArray array)) (index:indices) = do
    i <- (liftError . liftM fromInteger . fromIntegerValue) (Value index)
    case IntMap.lookup i array of
      Just ref -> evalRef ref >>= flip refArray indices
      Nothing -> return $ Value Undefined
  refArray (Value (IntHash hash)) (index:indices) = do
    key <- liftError $ fromIntegerValue $ Value index
    case HL.lookup key hash of
      Just val -> refArray (Value val) indices
      Nothing -> return $ Value Undefined
  refArray (Intermediate (IIntHash hash)) (index:indices) = do
    key <- liftError $ fromIntegerValue $ Value index
    case HL.lookup key hash of
      Just ref -> evalRef ref >>= flip refArray indices
      Nothing -> return $ Value Undefined
  refArray (Value (StrHash hash)) (index:indices) = do
    key <- liftError $ fromStringValue $ Value index
    case HL.lookup (B.pack key) hash of
      Just val -> refArray (Value val) indices
      Nothing -> return $ Value Undefined
  refArray (Intermediate (IStrHash hash)) (index:indices) = do
    key <- liftError $ fromStringValue $ Value index
    case HL.lookup (B.pack key) hash of
      Just ref -> evalRef ref >>= flip refArray indices
      Nothing -> return $ Value Undefined
  refArray val _ = throwError $ TypeMismatch "array" val

evalExpr env (LambdaExpr names expr) = return . Value $ Func env names expr 
evalExpr env (PatternFunctionExpr names pattern) = return . Value $ PatternFunc env names pattern

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
          let pattern = TuplePat $ flip map [1..k] $ \i ->
                if i == n then PatVar "#_" else WildCard
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
  fromMList MNil = return . Value $ Collection Sq.empty
  fromMList (MCons val m) = do
    head <- IElement <$> newEvaluatedThunk val
    tail <- ISubCollection <$> (liftIO . newIORef . Thunk $ m >>= fromMList)
    return . Intermediate $ ICollection $ Sq.fromList [head, tail]

evalExpr env (MatchExpr target matcher clauses) = do
  target <- newThunk env target
  matcher <- evalExpr env matcher
  let tryMatchClause (pattern, expr) cont = do
        result <- patternMatch env pattern target matcher
        case result of
          MCons bindings _ -> evalExpr (extendEnv env bindings) expr
          MNil -> cont
  foldr tryMatchClause (throwError $ strMsg "failed pattern match") clauses

evalExpr env (ApplyExpr func arg) = do
  func <- evalExpr env func
  arg <- evalExpr env arg
  applyFunc func arg

evalExpr env (MatcherExpr info) = return $ Value $ Matcher (env, info)

evalExpr env (GenerateArrayExpr (name:[]) (TupleExpr (size:[])) expr) =
  generateArray env name size expr
evalExpr env (GenerateArrayExpr (name:xs) (TupleExpr (size:ys)) expr) = 
  generateArray env name size (GenerateArrayExpr xs (TupleExpr ys) expr)
evalExpr env (GenerateArrayExpr names size expr) = 
  evalExpr env (GenerateArrayExpr names (TupleExpr [size]) expr)

evalExpr env (ArraySizeExpr expr) = 
  evalExpr env expr >>= arraySize
  where
    arraySize :: WHNFData -> EgisonM WHNFData
    arraySize (Intermediate (IArray vals)) = return . Value . Integer . toInteger $ IntMap.size vals
    arraySize (Value (Array vals))         = return . Value . Integer . toInteger $ IntMap.size vals
    arraySize val                          = throwError $ TypeMismatch "array" val

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
evalDeep (Intermediate (IIntHash refs)) = do
  refs' <- mapM evalRef' refs
  return $ IntHash refs'
evalDeep (Intermediate (IStrHash refs)) = do
  refs' <- mapM evalRef' refs
  return $ StrHash refs'
evalDeep (Intermediate (ITuple refs)) = Tuple <$> mapM evalRef' refs
evalDeep coll = Collection <$> (fromCollection coll >>= fromMList >>= mapM evalRef' . Sq.fromList)

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
fromCollection (Value (Collection vals)) =
  if Sq.null vals then return MNil
                  else fromSeq <$> mapM (newEvaluatedThunk . Value) vals
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

patternMatch :: Env -> EgisonPattern -> ObjectRef -> WHNFData ->
                EgisonM (MList EgisonM [Binding]) 
patternMatch env pattern target matcher =
  processMState (MState env [] [] [MAtom pattern target matcher]) >>= processMStates . (:[])

processMStates :: [MList EgisonM MatchingState] -> EgisonM (MList EgisonM [Binding])
processMStates [] = return MNil
processMStates streams = do
  let (bindings, streams') = (catMaybes *** concat) . unzip $ map processMStates' streams
  mappend (fromList bindings) (sequence streams' >>= processMStates)
 where
  processMStates' :: MList EgisonM MatchingState ->
                     (Maybe [Binding], [EgisonM (MList EgisonM MatchingState)])
  processMStates' MNil = (Nothing, [])
  processMStates' (MCons (MState _ _ bindings []) states) = (Just bindings, [states])
  processMStates' (MCons state states) = (Nothing, [processMState state, states])

processMState :: MatchingState -> EgisonM (MList EgisonM MatchingState)
processMState state =
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
  isNotPat (MState _ _ _ ((MAtom (NotPat _) _ _) : _)) = True
  isNotPat (MState _ _ _ ((MNode _ state) : _)) = isNotPat state
  isNotPat _ = False

  splitMState :: MatchingState -> (MatchingState, MatchingState)
  splitMState (MState env loops bindings ((MAtom (NotPat pattern) target matcher) : trees)) =
    (MState env loops bindings [MAtom pattern target matcher], MState env loops bindings trees)
  splitMState (MState env loops bindings ((MNode penv state) : trees)) =
    let (state1, state2) = splitMState state
    in (MState env loops bindings [MNode penv state1], MState env loops bindings (MNode penv state2 : trees)) 

processMState' :: MatchingState -> EgisonM (MList EgisonM MatchingState)
processMState' state@(MState _ _ _ []) = throwError $ strMsg "should not reach here"
processMState' (MState env loops bindings ((MAtom pattern target matcher):trees)) = do
  let env' = extendEnv env (bindings ++ map (\lc -> case lc of
                                                      (LoopContextConstant binding _ _ _) -> binding
                                                      (LoopContextVariable binding _ _ _) -> binding) loops)
  case pattern of
    VarPat _ -> throwError $ strMsg "cannot use variable except in pattern function"
    ApplyPat func args -> do
      func <- evalExpr env' func
      case func of
        Value (PatternFunc env'' names expr) ->
          let penv = zip names args
          in return $ msingleton $ MState env loops bindings (MNode penv (MState env'' [] [] [MAtom expr target matcher]) : trees)
        _ -> throwError $ TypeMismatch "pattern constructor" func
    
    LoopPat name (LoopRangeConstant start end) pat pat' -> do
      startNum' <- evalExpr' env' start
      startNum <- extractInteger startNum'
      endNum' <- evalExpr' env' end
      endNum <- extractInteger endNum'
      if startNum > endNum
        then do
          return $ msingleton $ MState env loops bindings (MAtom pat' target matcher : trees)
        else do
          startNumRef <- liftIO . newIORef . WHNF $ Value $ Integer startNum
          let loops' = LoopContextConstant (name, startNumRef) endNum pat pat' : loops
          return $ msingleton $ MState env loops' bindings (MAtom pat target matcher : trees)
    LoopPat name (LoopRangeVariable start lastNumPat) pat pat' -> do
      startNum' <- evalExpr' env' start
      startNum <- extractInteger startNum'
      startNumRef <- liftIO . newIORef . WHNF $ Value $ Integer startNum
      lastNumRef <- liftIO . newIORef . WHNF $ Value $ Integer (startNum - 1)
      return $ fromList [MState env loops bindings (MAtom lastNumPat lastNumRef (Value Something) : MAtom pat' target matcher : trees),
                         MState env (LoopContextVariable (name, startNumRef) lastNumPat pat pat' : loops) bindings (MAtom pat target matcher : trees)]
    ContPat ->
      case loops of
        [] -> throwError $ strMsg "cannot use cont pattern except in loop pattern"
        LoopContextConstant (name, startNumRef) endNum pat pat' : loops -> do
          startNum' <- evalRef' startNumRef
          startNum <- extractInteger startNum'
          let nextNum = startNum + 1
          if nextNum > endNum
            then return $ msingleton $ MState env loops bindings (MAtom pat' target matcher : trees)
            else do
              nextNumRef <- liftIO . newIORef . WHNF $ Value $ Integer nextNum
              let loops' = LoopContextConstant (name, nextNumRef) endNum pat pat' : loops 
              return $ msingleton $ MState env loops' bindings (MAtom pat target matcher : trees)
        LoopContextVariable (name, startNumRef) lastNumPat pat pat' : loops -> do
          startNum' <- evalRef' startNumRef
          startNum <- extractInteger startNum'
          let nextNum = startNum + 1
          nextNumRef <- liftIO . newIORef . WHNF $ Value $ Integer nextNum
          let loops' = LoopContextVariable (name, nextNumRef) lastNumPat pat pat' : loops 
          return $ fromList [MState env loops bindings (MAtom lastNumPat startNumRef (Value Something) : MAtom pat' target matcher : trees),
                             MState env loops' bindings (MAtom pat target matcher : trees)]
          
    TuplePat patterns -> do
      matchers <- fromTuple matcher >>= mapM evalRef
      targets <- evalRef target >>= fromTuple
      let trees' = zipWith3 MAtom patterns targets matchers ++ trees
      return $ msingleton $ MState env loops bindings trees'
    AndPat patterns ->
      let trees' = map (\pattern -> MAtom pattern target matcher) patterns ++ trees
      in return $ msingleton $ MState env loops bindings trees'
    OrPat patterns ->
      return $ fromList $ flip map patterns $ \pattern ->
        MState env loops bindings (MAtom pattern target matcher : trees)
    NotPat pattern -> throwError $ strMsg "should not reach here (cut pattern)"
    CutPat pattern -> -- TEMPORARY ignoring cut patterns
      return $ msingleton (MState env loops bindings ((MAtom pattern target matcher):trees))
    PredPat pred -> do
      func <- evalExpr env' pred
      arg <- evalRef target
      result <- applyFunc func arg >>= liftError . fromBoolValue
      if result then return $ msingleton $ (MState env loops bindings trees)
                else return MNil
    LetPat bindings' pattern ->
      let extractBindings ([name], expr) =
            makeBindings [name] . (:[]) <$> newThunk env' expr
          extractBindings (names, expr) =
            makeBindings names <$> (evalExpr env' expr >>= fromTuple)
      in
       liftM concat (mapM extractBindings bindings')
         >>= (\b -> return $ msingleton $ MState env loops (b ++ bindings) ((MAtom pattern target matcher):trees))
    _ ->
      case matcher of
        Value (Matcher matcher) -> do
          (patterns, targetss, matchers) <- inductiveMatch env' pattern target matcher
          mfor targetss $ \ref -> do
            targets <- evalRef ref >>= fromTuple
            let trees' = zipWith3 MAtom patterns targets matchers ++ trees
            return $ MState env loops bindings trees'
            
        _ -> -- Value Something -> -- for tupple patterns
          case pattern of
            WildCard -> return $ msingleton $ MState env loops bindings trees 
            PatVar name -> return $ msingleton $ MState env loops ((name, target):bindings) trees
            IndexedPat (PatVar name) indices -> do
              indices <- mapM (evalExpr env' >=> liftError . liftM fromInteger . fromIntegerValue) indices
              case lookup name bindings of
                Just ref -> do
                  obj <- evalRef ref >>= flip updateArray indices >>= newEvaluatedThunk
                  return $ msingleton $ MState env loops (subst name obj bindings) trees
                Nothing  -> do
                  obj <- updateArray (Value $ Array IntMap.empty) indices >>= newEvaluatedThunk
                  return $ msingleton $ MState env loops ((name, obj):bindings) trees
               where
                updateArray :: WHNFData -> [Int] -> EgisonM WHNFData
                updateArray (Intermediate (IArray ary)) [index] =
                  return . Intermediate . IArray $ IntMap.insert index target ary
                updateArray (Intermediate (IArray ary)) (index:indices) = do
                  val <- maybe (return $ Value $ Array IntMap.empty) evalRef $ IntMap.lookup index ary
                  ref <- updateArray val indices >>= newEvaluatedThunk
                  return . Intermediate . IArray $ IntMap.insert index ref ary
                updateArray (Value (Array ary)) [index] = do
                  keys <- return $ IntMap.keys ary
                  vals <- mapM (newEvaluatedThunk . Value) $ IntMap.elems ary
                  return . Intermediate . IArray $ IntMap.insert index target (IntMap.fromList $ zip keys vals)
                updateArray (Value (Array ary)) (index:indices) = do
                  let val = Value $ fromMaybe (Array IntMap.empty) $ IntMap.lookup index ary
                  ref <- updateArray val indices >>= newEvaluatedThunk
                  keys <- return $ IntMap.keys ary
                  vals <- mapM (newEvaluatedThunk . Value) $ IntMap.elems ary
                  return . Intermediate . IArray $ IntMap.insert index ref (IntMap.fromList $ zip keys vals)
                updateArray _ _ = do
                  throwError $ strMsg "expected array value"
                subst :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
                subst k nv ((k', v'):xs) | k == k'   = (k', nv):(subst k nv xs)
                                         | otherwise = (k', v'):(subst k nv xs)
                subst _ _ [] = []
            IndexedPat pattern indices -> throwError $ strMsg ("invalid indexed-pattern: " ++ show pattern) 
            _ -> throwError $ strMsg "something can only match with a pattern variable"

--        _ -> throwError $ TypeMismatch "matcher" matcher
processMState' (MState env loops bindings ((MNode penv (MState _ _ _ [])):trees)) =
  return $ msingleton $ MState env loops bindings trees
processMState' (MState env loops bindings ((MNode penv state@(MState env' loops' bindings' (tree:trees')):trees))) = do
  case tree of
    MAtom pattern target matcher -> do
      case pattern of
        VarPat name ->
          case lookup name penv of
            Just pattern ->
              return $ msingleton $ MState env loops bindings (MAtom pattern target matcher:MNode penv (MState env' loops' bindings' trees'):trees)
            Nothing -> throwError $ UnboundVariable name
        IndexedPat (VarPat name) indices ->
          case lookup name penv of
            Just pattern -> do
              let env'' = extendEnv env' (bindings' ++ map (\lc -> case lc of
                                                                     (LoopContextConstant binding _ _ _) -> binding
                                                                     (LoopContextVariable binding _ _ _) -> binding) loops')
              indices <- mapM (evalExpr env'' >=> liftError . liftM fromInteger . fromIntegerValue) indices
              let pattern' = IndexedPat pattern $ map IntegerExpr indices
              return $ msingleton $ MState env loops bindings (MAtom pattern' target matcher:MNode penv (MState env' loops' bindings' trees'):trees)
            Nothing -> throwError $ UnboundVariable name
        _ -> processMState' state >>= mmap (return . MState env loops bindings . (: trees) . MNode penv)
    _ -> processMState' state >>= mmap (return . MState env loops bindings . (: trees) . MNode penv)

inductiveMatch :: Env -> EgisonPattern -> ObjectRef -> Matcher ->
                  EgisonM ([EgisonPattern], MList EgisonM ObjectRef, [WHNFData])
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

primitivePatPatternMatch :: Env -> PrimitivePatPattern -> EgisonPattern ->
                            MatchM ([EgisonPattern], [Binding])
primitivePatPatternMatch _ PPWildCard _ = return ([], [])
primitivePatPatternMatch _ PPPatVar pattern = return ([pattern], [])
primitivePatPatternMatch env (PPValuePat name) (ValuePat expr) = do
  ref <- lift $ newThunk env expr
  return ([], [(name, ref)])
primitivePatPatternMatch env (PPInductivePat name patterns) (InductivePat name' exprs)
  | name == name' =
    (concat *** concat) . unzip <$> zipWithM (primitivePatPatternMatch env) patterns exprs
  | otherwise = matchFail
primitivePatPatternMatch _ _ _ = matchFail

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

expandCollection :: WHNFData -> EgisonM (Seq Inner)
expandCollection (Value (Collection vals)) =
  mapM (liftM IElement . newEvaluatedThunk . Value) vals
expandCollection (Intermediate (ICollection inners)) = return inners
expandCollection val = throwError $ TypeMismatch "collection" val

isEmptyCollection :: ObjectRef -> EgisonM Bool
isEmptyCollection ref = evalRef ref >>= isEmptyCollection'
 where
  isEmptyCollection' :: WHNFData -> EgisonM Bool
  isEmptyCollection' (Value (Collection col)) = return $ Sq.null col
  isEmptyCollection' (Intermediate (ICollection ic)) =
    case Sq.viewl ic of
      EmptyL -> return True
      (ISubCollection ref') :< inners -> do
        inners' <- evalRef ref' >>= expandCollection
        let coll = Intermediate (ICollection (inners' >< inners))
        writeThunk ref coll
        isEmptyCollection' coll
      _ -> return False
  isEmptyCollection' _ = return False

unconsCollection :: ObjectRef -> MatchM (ObjectRef, ObjectRef)
unconsCollection ref = lift (evalRef ref) >>= unconsCollection'
 where
  unconsCollection' :: WHNFData -> MatchM (ObjectRef, ObjectRef)
  unconsCollection' (Value (Collection col)) =
    case Sq.viewl col of
      EmptyL -> matchFail
      val :< vals ->
        lift $ (,) <$> newEvaluatedThunk (Value val)
                   <*> newEvaluatedThunk (Value $ Collection vals)
  unconsCollection' (Intermediate (ICollection ic)) =
    case Sq.viewl ic of
      EmptyL -> matchFail
      (IElement ref') :< inners ->
        lift $ (ref', ) <$> newEvaluatedThunk (Intermediate $ ICollection inners)
      (ISubCollection ref') :< inners -> do
        inners' <- lift $ evalRef ref' >>= expandCollection
        let coll = Intermediate (ICollection (inners' >< inners))
        lift $ writeThunk ref coll
        unconsCollection' coll
  unconsCollection' _ = matchFail

unsnocCollection :: ObjectRef -> MatchM (ObjectRef, ObjectRef)
unsnocCollection ref = lift (evalRef ref) >>= unsnocCollection'
 where
  unsnocCollection' :: WHNFData -> MatchM (ObjectRef, ObjectRef)
  unsnocCollection' (Value (Collection col)) =
   case Sq.viewr col of
     EmptyR -> matchFail
     vals :> val ->
       lift $ (,) <$> newEvaluatedThunk (Value $ Collection vals)
                  <*> newEvaluatedThunk (Value val)
  unsnocCollection' (Intermediate (ICollection ic)) =
   case Sq.viewr ic of
     EmptyR -> matchFail
     inners :> (IElement ref') ->
       lift $ (, ref') <$> newEvaluatedThunk (Intermediate $ ICollection inners)
     inners :> (ISubCollection ref') -> do
       inners' <- lift $ evalRef ref' >>= expandCollection
       let coll = Intermediate (ICollection (inners >< inners'))
       lift $ writeThunk ref coll
       unsnocCollection' coll
  unsnocCollection' _ = matchFail
