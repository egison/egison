{-# Language TupleSections #-}

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
    , evalTopExpr
    , evalTopExpr'
    , evalExpr
    , evalExprDeep
    , evalRef
    , evalRefDeep
    , evalWHNF
    , applyFunc
    -- * Environment
    , recursiveBind
    -- * Pattern matching
    , patternMatch
    -- * Collection
    , isEmptyCollection
    , unconsCollection
    , unsnocCollection
    -- * Utiltiy functions
    , evalStringWHNF
    , fromStringValue
    ) where

import Prelude hiding (mapM)

import Control.Arrow
import Control.Applicative
import Control.Monad.Error hiding (mapM)
import Control.Monad.State hiding (mapM)
import Control.Monad.Trans.Maybe

import Data.Sequence (Seq, ViewL(..), ViewR(..), (><))
import qualified Data.Sequence as Sq
import Data.Foldable (toList)
import Data.Traversable (mapM)
import Data.IORef
import Data.Maybe

import qualified Data.HashMap.Lazy as HL

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Lazy.Char8 as B

import qualified Data.IntMap as IntMap


import Language.Egison.Types
import Language.Egison.Parser

--
-- Evaluator
--

evalTopExprs :: Env -> [EgisonTopExpr] -> EgisonM Env
evalTopExprs env exprs = do
  (bindings, rest) <- collectDefs exprs [] []
  env <- recursiveBind env bindings
  forM_ rest $ evalTopExpr' env
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
evalTopExpr env topExpr = evalTopExpr'' env topExpr >>= return . snd

evalTopExpr' :: Env -> EgisonTopExpr -> EgisonM Env
evalTopExpr' env topExpr = do
  ret <- evalTopExpr'' env topExpr
  liftIO $ putStrLn $ fst ret
  return $ snd ret

evalTopExpr'' :: Env -> EgisonTopExpr -> EgisonM (String, Env)
evalTopExpr'' env (Define name expr) = recursiveBind env [(name, expr)] >>= return . ((,) "")
evalTopExpr'' env (Test expr) = do
  val <- evalExprDeep env expr
  return ((show val), env)
evalTopExpr'' env (Execute expr) = do
  io <- evalExpr env expr
  case io of
    Value (IOFunc m) -> m >> return ("", env)
    _ -> throwError $ TypeMismatch "io" io
evalTopExpr'' env (Load file) = loadLibraryFile file >>= evalTopExprs env >>= return . ((,) "")
evalTopExpr'' env (LoadFile file) = loadFile file >>= evalTopExprs env >>= return . ((,) "")

evalExpr :: Env -> EgisonExpr -> EgisonM WHNFData
evalExpr _ (CharExpr c) = return . Value $ Char c
evalExpr _ (StringExpr s) = return $ Value $ toEgison s
evalExpr _ (BoolExpr b) = return . Value $ Bool b
evalExpr _ (RationalExpr x) = return . Value $ Rational x
evalExpr _ (IntegerExpr i) = return . Value $ Integer i
evalExpr _ (FloatExpr d) = return . Value $ Float d

evalExpr env (VarExpr name) = refVar env name >>= evalRef

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
  ref' <- mapM (newObjectRef env) exprs
  return . Intermediate . IArray $ IntMap.fromList $ zip (enumFromTo 1 (length exprs)) ref'

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
       StrKey _ -> do
          let keys' = map (\key -> case key of StrKey s -> s) keys
          return . Intermediate . IStrHash $ HL.fromList $ zip keys' refs
 where
  makeHashKey :: WHNFData -> EgisonM EgisonHashKey
  makeHashKey (Value val) =
    case val of
      Integer i -> return (IntKey i)
      Collection _ -> do
        str <- evalStringWHNF $ Value val
        return $ StrKey $ B.pack str
      _ -> throwError $ TypeMismatch "integer or string" $ Value val
  makeHashKey whnf = do
    str <- evalStringWHNF whnf
    return $ StrKey $ B.pack str

evalExpr env (IndexedExpr expr indices) = do
  array <- evalExpr env expr
  indices <- mapM (evalExprDeep env) indices
  refArray array indices
 where
  refArray :: WHNFData -> [EgisonValue] -> EgisonM WHNFData
  refArray val [] = return val 
  refArray (Value (Array array)) (index:indices) = do
    i <- (liftM fromInteger . fromEgison) index
    case IntMap.lookup i array of
      Just val -> refArray (Value val) indices
      Nothing -> return $ Value Undefined
  refArray (Intermediate (IArray array)) (index:indices) = do
    i <- (liftM fromInteger . fromEgison) index
    case IntMap.lookup i array of
      Just ref -> evalRef ref >>= flip refArray indices
      Nothing -> return $ Value Undefined
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
  refArray (Value (StrHash hash)) (index:indices) = do
    key <- evalStringWHNF $ Value index
    case HL.lookup (B.pack key) hash of
      Just val -> refArray (Value val) indices
      Nothing -> return $ Value Undefined
  refArray (Intermediate (IStrHash hash)) (index:indices) = do
    key <- evalStringWHNF $ Value index
    case HL.lookup (B.pack key) hash of
      Just ref -> evalRef ref >>= flip refArray indices
      Nothing -> return $ Value Undefined
  refArray val _ = throwError $ TypeMismatch "array or hash" val

evalExpr env (LambdaExpr names expr) = return . Value $ Func env names expr 
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
  let body = foldr genLet (TupleExpr [VarExpr "#1", expr]) bindings
  applyFunc (Value $ Func env ["#1"] body) $ Value World
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

evalExpr env (ApplyExpr func arg) = do
  func <- evalExpr env func
  arg <- evalExpr env arg
  applyFunc func arg

evalExpr env (MatcherBFSExpr info) = return $ Value $ UserMatcher env BFSMode info
evalExpr env (MatcherDFSExpr info) = return $ Value $ UserMatcher env DFSMode info

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
  refs' <- mapM evalRefDeep $ IntMap.elems refs
  return $ Array $ IntMap.fromList $ zip (enumFromTo 1 (IntMap.size refs)) refs'
evalWHNF (Intermediate (IIntHash refs)) = do
  refs' <- mapM evalRefDeep refs
  return $ IntHash refs'
evalWHNF (Intermediate (IStrHash refs)) = do
  refs' <- mapM evalRefDeep refs
  return $ StrHash refs'
evalWHNF (Intermediate (ITuple [ref])) = evalRefDeep ref
evalWHNF (Intermediate (ITuple refs)) = Tuple <$> mapM evalRefDeep refs
evalWHNF coll = Collection <$> (fromCollection coll >>= fromMList >>= mapM evalRefDeep . Sq.fromList)

applyFunc :: WHNFData -> WHNFData -> EgisonM WHNFData
applyFunc (Value (Func env [name] body)) arg = do
  ref <- newEvalutedObjectRef arg
  evalExpr (extendEnv env $ makeBindings [name] [ref]) body
applyFunc (Value (Func env names body)) arg = do
  refs <- fromTuple arg
  if length names == length refs
    then evalExpr (extendEnv env $ makeBindings names refs) body
    else throwError $ ArgumentsNum (length names) (length refs)
applyFunc (Value (PrimitiveFunc func)) arg = func arg
applyFunc (Value (IOFunc m)) arg = do
  case arg of
     Value World -> m
     _ -> throwError $ TypeMismatch "world" arg
applyFunc val _ = throwError $ TypeMismatch "function" val

generateArray :: Env -> String -> EgisonExpr -> EgisonExpr -> EgisonM WHNFData
generateArray env name size expr = do  
  size' <- evalExpr env size >>= fromWHNF >>= return . fromInteger
  elems <- mapM genElem (enumFromTo 1 size')
  return $ Intermediate $ IArray $ IntMap.fromList elems
  where
    genElem :: Int -> EgisonM (Int, ObjectRef)
    genElem i = do env <- bindEnv env name $ toInteger i
                   val <- evalExpr env expr >>= newEvalutedObjectRef                   
                   return (i, val)
    
    bindEnv :: Env -> String -> Integer -> EgisonM Env
    bindEnv env name i = do
      ref <- newEvalutedObjectRef (Value . Integer $ i)
      return $ extendEnv env [(name, ref)]

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
  zipWithM_ (\ref expr -> liftIO . writeIORef ref . Thunk $ evalExpr env' expr) refs exprs
  return env'

--
-- Pattern Match
--

patternMatch :: Env -> EgisonPattern -> ObjectRef -> Matcher -> EgisonM (MList EgisonM Match) 
patternMatch env pattern target matcher = processMStates [msingleton $ MState env [] [] [MAtom pattern target matcher]]

processMStates :: [MList EgisonM MatchingState] -> EgisonM (MList EgisonM Match)
processMStates [] = return MNil
processMStates streams = do
  (matches, streams') <- extractMatches streams
  nextStreamss <- mapM processMStates' streams'
  let nextStreams = concat nextStreamss
  mappend (fromList matches) $ processMStates nextStreams
 where
  processMStates' :: MList EgisonM MatchingState -> EgisonM [MList EgisonM MatchingState]
  processMStates' MNil = return []
  processMStates' stream@(MCons state _) = do
    case topMAtom state of
      Nothing -> processMStatesBFS stream
      Just matom -> do
        case pmMode (getMatcher matom) of
          DFSMode -> processMStatesDFS stream
          BFSMode -> processMStatesBFS stream
          
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

extractMatches :: [MList EgisonM MatchingState] -> EgisonM ([Match], [MList EgisonM MatchingState])
extractMatches = extractMatches' ([], [])
 where
  extractMatches' :: ([Match], [MList EgisonM MatchingState]) -> [MList EgisonM MatchingState] -> EgisonM ([Match], [MList EgisonM MatchingState])
  extractMatches' (xs, ys) [] = return (xs, ys)
  extractMatches' (xs, ys) ((MCons (MState _ _ bindings []) states):rest) = do
    states' <- states
    extractMatches' (xs ++ [bindings], ys ++ [states']) rest
  extractMatches' (xs, ys) (stream:rest) = extractMatches' (xs, ys ++ [stream]) rest

topMAtom :: MatchingState -> Maybe MatchingTree
topMAtom (MState _ _ _ (mAtom@(MAtom _ _ _):_)) = return mAtom
topMAtom (MState _ _ _ ((MNode _ mstate):_)) = topMAtom mstate
topMAtom (MState _ _ _ _) = Nothing

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
      startNum <- evalExpr env' start >>= fromWHNF
      endNum <- evalExpr env' end >>= fromWHNF
      if startNum > endNum
        then do
          return $ msingleton $ MState env loops bindings (MAtom pat' target matcher : trees)
        else do
          startNumRef <- newEvalutedObjectRef $ Value $ Integer startNum
          let loops' = LoopContextConstant (name, startNumRef) endNum pat pat' : loops
          return $ msingleton $ MState env loops' bindings (MAtom pat target matcher : trees)
    LoopPat name (LoopRangeVariable start lastNumPat) pat pat' -> do
      startNum <- evalExpr env' start >>= fromWHNF
      startNumRef <- newEvalutedObjectRef $ Value $ Integer startNum
      lastNumRef <- newEvalutedObjectRef $ Value $ Integer (startNum - 1)
      return $ fromList [MState env loops bindings (MAtom lastNumPat lastNumRef Something : MAtom pat' target matcher : trees),
                         MState env (LoopContextVariable (name, startNumRef) lastNumPat pat pat' : loops) bindings (MAtom pat target matcher : trees)]
    ContPat ->
      case loops of
        [] -> throwError $ strMsg "cannot use cont pattern except in loop pattern"
        LoopContextConstant (name, startNumRef) endNum pat pat' : loops -> do
          startNum <- evalRef startNumRef >>= fromWHNF
          let nextNum = startNum + 1
          if nextNum > endNum
            then return $ msingleton $ MState env loops bindings (MAtom pat' target matcher : trees)
            else do
              nextNumRef <- newEvalutedObjectRef $ Value $ Integer nextNum
              let loops' = LoopContextConstant (name, nextNumRef) endNum pat pat' : loops 
              return $ msingleton $ MState env loops' bindings (MAtom pat target matcher : trees)
        LoopContextVariable (name, startNumRef) lastNumPat pat pat' : loops -> do
          startNum <- evalRef startNumRef >>= fromWHNF
          let nextNum = startNum + 1
          nextNumRef <- newEvalutedObjectRef $ Value $ Integer nextNum
          let loops' = LoopContextVariable (name, nextNumRef) lastNumPat pat pat' : loops 
          return $ fromList [MState env loops bindings (MAtom lastNumPat startNumRef Something : MAtom pat' target matcher : trees),
                             MState env loops' bindings (MAtom pat target matcher : trees)]
          
    TuplePat patterns -> do
      targets <- evalRef target >>= fromTuple
      let matchers = fromTupleValue matcher
      if not (length patterns == length targets) then throwError $ ArgumentsNum (length patterns) (length targets) else return ()
      if not (length patterns == length matchers) then throwError $ ArgumentsNum (length patterns) (length matchers) else return ()
      let trees' = zipWith3 MAtom patterns targets matchers ++ trees
      return $ msingleton $ MState env loops bindings trees'
    AndPat patterns ->
      let trees' = map (\pattern -> MAtom pattern target matcher) patterns ++ trees
      in return $ msingleton $ MState env loops bindings trees'
    OrPat patterns ->
      return $ fromList $ flip map patterns $ \pattern ->
        MState env loops bindings (MAtom pattern target matcher : trees)
    NotPat pattern -> throwError $ strMsg "should not reach here (cut pattern)"
    PredPat pred -> do
      func <- evalExpr env' pred
      arg <- evalRef target
      result <- applyFunc func arg >>= fromWHNF
      if result then return $ msingleton $ (MState env loops bindings trees)
                else return MNil
    LetPat bindings' pattern ->
      let extractBindings ([name], expr) =
            makeBindings [name] . (:[]) <$> newObjectRef env' expr
          extractBindings (names, expr) =
            makeBindings names <$> (evalExpr env' expr >>= fromTuple)
      in
       liftM concat (mapM extractBindings bindings')
         >>= (\b -> return $ msingleton $ MState env loops (b ++ bindings) ((MAtom pattern target matcher):trees))
    _ ->
      case matcher of
        matcher@(UserMatcher _ _ _) -> do
          (patterns, targetss, matchers) <- inductiveMatch env' pattern target matcher
          mfor targetss $ \ref -> do
            targets <- evalRef ref >>= fromTuple
            let trees' = zipWith3 MAtom patterns targets matchers ++ trees
            return $ MState env loops bindings trees'
            
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
                Nothing  -> do -- throwError $ strMsg "Cannot reach here!\nIndexed-patttern-variable does not initialized."
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
            _ -> throwError $ strMsg "something can only match with a pattern variable"

processMState' (MState env loops bindings ((MNode _ (MState _ _ _ [])):trees)) = return $ msingleton $ MState env loops bindings trees
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
              indices <- mapM (evalExpr env'' >=> liftM fromInteger . fromWHNF) indices
              let pattern' = IndexedPat pattern $ map IntegerExpr indices
              return $ msingleton $ MState env loops bindings (MAtom pattern' target matcher:MNode penv (MState env' loops' bindings' trees'):trees)
            Nothing -> throwError $ UnboundVariable name
        _ -> processMState' state >>= mmap (return . MState env loops bindings . (: trees) . MNode penv)
    _ -> processMState' state >>= mmap (return . MState env loops bindings . (: trees) . MNode penv)

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
  | name == name' =
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

--
-- Util
--
fromTuple :: WHNFData -> EgisonM [ObjectRef]
fromTuple (Intermediate (ITuple refs)) = return refs
fromTuple (Value (Tuple vals)) = mapM (newEvalutedObjectRef . Value) vals
fromTuple val = return <$> newEvalutedObjectRef val

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

--
-- String
--
evalStringWHNF :: WHNFData -> EgisonM String
evalStringWHNF (Value (Collection seq)) = do
  let ls = toList seq
  mapM (\val -> case val of
                  Char c -> return c
                  _ -> throwError $ TypeMismatch "char" (Value val))
       ls
evalStringWHNF (Value (Tuple [val])) = evalStringWHNF (Value val)
evalStringWHNF whnf@(Intermediate (ICollection _)) = evalWHNF whnf >>= evalStringWHNF . Value
evalStringWHNF whnf = throwError $ TypeMismatch "string" whnf

evalMatcherWHNF :: WHNFData -> EgisonM Matcher
evalMatcherWHNF (Value matcher@Something) = return matcher
evalMatcherWHNF (Value matcher@(UserMatcher _ _ _)) = return matcher
evalMatcherWHNF (Value (Tuple ms)) = Tuple <$> mapM (evalMatcherWHNF . Value) ms
evalMatcherWHNF (Intermediate (ITuple refs)) = do
  whnfs <- mapM evalRef refs
  ms <- mapM evalMatcherWHNF whnfs
  return $ Tuple ms
evalMatcherWHNF whnf = throwError $ TypeMismatch "matcher" whnf

fromStringValue :: EgisonValue -> EgisonM String
fromStringValue (Collection seq) = do
  let ls = toList seq
  mapM (\val -> case val of
                  Char c -> return c
                  _ -> throwError $ TypeMismatch "char" (Value val))
       ls
fromStringValue (Tuple [val]) = fromStringValue val
fromStringValue val = throwError $ TypeMismatch "string" (Value val)

--
-- Util
--
data EgisonHashKey =
    IntKey Integer
  | StrKey ByteString

extractPrimitiveValue :: WHNFData -> Either EgisonError EgisonValue
extractPrimitiveValue (Value val@(Char _)) = return val
extractPrimitiveValue (Value val@(Bool _)) = return val
extractPrimitiveValue (Value val@(Integer _)) = return val
extractPrimitiveValue (Value val@(Float _)) = return val
extractPrimitiveValue whnf = throwError $ TypeMismatch "primitive value" whnf
