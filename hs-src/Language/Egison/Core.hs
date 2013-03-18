module Language.Egison.Core where

import Control.Arrow
import Control.Applicative
import Control.Monad.Error
import Control.Monad.Trans.Maybe

import Data.IORef
import Data.List
import Data.Maybe

import Language.Egison.Types

--
-- Evaluator
--

evalTopExprs :: Env -> [EgisonTopExpr] -> EgisonM ()
evalTopExprs env exprs = do
  let (bindings, rest) = foldr collectDefs ([], []) exprs
  env <- recursiveBind env bindings
  forM_ rest $ evalTopExpr env
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
  main <- refVar env ("main", []) >>= evalRef
  argv <- newEvaluatedThunk . Value . Collection $ map String argv
  world <- newEvaluatedThunk $ Value World
  applyFunc main [argv] >>= flip applyFunc [world]
  return env

evalExpr :: Env -> EgisonExpr -> EgisonM WHNFData
evalExpr _ (CharExpr c) = return . Value $ Char c
evalExpr _ (StringExpr s) = return . Value $ String s 
evalExpr _ (BoolExpr b) = return . Value $ Bool b
evalExpr _ (IntegerExpr i) = return . Value $ Integer i
evalExpr _ (FloatExpr d) = return . Value $ Float d

evalExpr env (VarExpr name nums) = do
  var <- (,) name <$> mapM (evalExpr env >=> liftError . fromIntegerValue) nums
  refVar env var >>= evalRef

evalExpr env (InductiveDataExpr name []) = return . Value $ InductiveData name []
evalExpr env (InductiveDataExpr name exprs) =
  Intermediate . IInductiveData name <$> mapM (newThunk env) exprs 

evalExpr _ (TupleExpr []) = return . Value $ Tuple []
evalExpr env (TupleExpr [expr]) = evalExpr env expr
evalExpr env (TupleExpr exprs) =
  Intermediate . ITuple <$> mapM (newThunk env) exprs 

evalExpr env (CollectionExpr []) = return . Value $ Collection []
evalExpr env (CollectionExpr inners) =
  Intermediate . ICollection <$> mapM fromInnerExpr inners
 where
  fromInnerExpr (ElementExpr expr) = IElement <$> newThunk env expr
  fromInnerExpr (SubCollectionExpr expr) = ISubCollection <$> newThunk env expr

evalExpr env (LambdaExpr names expr) = return . Value $ Func env names expr 

evalExpr env (IfExpr test expr expr') = do
  test <- evalExpr env test >>= liftError . fromBoolValue
  evalExpr env $ if test then expr else expr'

evalExpr env (LetExpr bindings expr) =
  mapM extractBindings bindings >>= flip evalExpr expr . extendEnv env . concat
 where
  extractBindings :: BindingExpr -> EgisonM [Binding]
  extractBindings ([name], expr) =
    newThunk env expr >>= makeBindings [name] . return
  extractBindings (names, expr) =
    evalExpr env expr >>= fromTuple >>= makeBindings names

evalExpr env (LetRecExpr bindings expr) =
  recursiveBind env bindings >>= flip evalExpr expr

evalExpr env (MatchAllExpr target matcher (pattern, expr)) = do
  target <- newThunk env target
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
  let tryMatchClause (pattern, expr) cont = do
        result <- patternMatch env pattern target matcher
        case result of
          MCons bindings _ -> evalExpr (extendEnv env bindings) expr
          MNil -> cont
  foldr tryMatchClause (throwError $ strMsg "failed pattern match") clauses

evalExpr env (ApplyExpr func args) = do
  func <- evalExpr env func
  args <- evalExpr env args >>= fromTuple 
  applyFunc func args

evalExpr env (MatcherExpr info) = return $ Value $ Matcher (env, info)

evalExpr _ (PatternExpr pattern) = return $ Value $ Pattern pattern

evalExpr _ SomethingExpr = return $ Value Something
evalExpr _ UndefinedExpr = throwError $ strMsg "undefined"
evalExpr _ expr = throwError $ NotImplemented ("evalExpr for " ++ show expr)

evalExpr' :: Env -> EgisonExpr -> EgisonM EgisonValue
evalExpr' env expr = evalExpr env expr >>= evalWHNF

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
    WHNF (Intermediate i) -> do
      val <- evalIntermediate i
      writeThunk ref $ Value val
      return val
    Thunk thunk -> do
      val <- thunk >>= evalWHNF
      writeThunk ref $ Value val
      return val

evalWHNF :: WHNFData -> EgisonM EgisonValue
evalWHNF (Value val) = return val
evalWHNF (Intermediate i) = evalIntermediate i

evalIntermediate :: Intermediate -> EgisonM EgisonValue
evalIntermediate (IInductiveData name refs) = InductiveData name <$> mapM evalRef' refs
evalIntermediate (ITuple refs) = Tuple <$> mapM evalRef' refs
evalIntermediate coll =
  Collection <$> (fromCollection (Intermediate coll) >>= fromMList >>= mapM evalRef')

applyFunc :: WHNFData -> [ObjectRef] -> EgisonM WHNFData
applyFunc (Value (Func env names body)) args =
  makeBindings names args >>= flip evalExpr body . extendEnv env
applyFunc (Value (PrimitiveFunc func)) args =
  mapM evalRef args >>= liftM Value . liftError . func
applyFunc (Value (IOFunc func)) args =
  mapM evalRef args >>= liftM Value . func
applyFunc val _ = throwError $ TypeMismatch "function" val

newThunk :: Env -> EgisonExpr -> EgisonM ObjectRef
newThunk env expr = liftIO . newIORef . Thunk $ evalExpr env expr

writeThunk :: ObjectRef -> WHNFData -> EgisonM ()
writeThunk ref val = liftIO . writeIORef ref $ WHNF val

newEvaluatedThunk :: WHNFData -> EgisonM ObjectRef
newEvaluatedThunk = liftIO . newIORef . WHNF

recursiveBind :: Env -> [(String, EgisonExpr)] -> EgisonM Env
recursiveBind env bindings = do
  let (names, exprs) = unzip bindings
  refs <- replicateM (length bindings) $ newThunk nullEnv UndefinedExpr
  env <- extendEnv env <$> makeBindings names refs
  zipWithM_ (\ref expr -> liftIO . writeIORef ref . Thunk $ evalExpr env expr) refs exprs
  return env

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

patternMatch :: Env -> EgisonExpr -> ObjectRef -> EgisonExpr ->
                EgisonM (MList EgisonM [Binding]) 
patternMatch env pattern target matcher = do
  matcher <- evalExpr env matcher
  processMState (MState env [] [MAtom pattern target matcher]) >>= processMStates

processMStates :: MList EgisonM MatchingState -> EgisonM (MList EgisonM [Binding])
processMStates MNil = return MNil
processMStates (MCons (MState _ bindings []) states) =
  return $ MCons bindings (states >>= processMStates)
processMStates (MCons state states) =
  processMState state >>= flip mappend states >>= processMStates

processMState :: MatchingState -> EgisonM (MList EgisonM MatchingState)
processMState state@(MState env bindings []) = throwError $ strMsg "should not reach here"
processMState (MState env bindings ((MAtom pattern target matcher):trees)) = do
  let env' = extendEnv env bindings
  pattern <- evalPattern env pattern
  case pattern of
    VarExpr _ _ -> throwError $ strMsg "cannot use variable in pattern"
    ApplyExpr func (TupleExpr args) -> do
      func <- evalExpr env' func
      case func of
        Value (Func env names expr) -> do
          penv <- zip (map (flip (,) []) names) <$> mapM (evalPattern env') args
          return $ msingleton $ MState env bindings (MNode penv (MState env [] [MAtom expr target matcher]) : trees)
        _ -> throwError $ TypeMismatch "pattern constructor" func
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
    PatternExpr (CutPat pattern) -> undefined
    PatternExpr (PredPat pred) -> do
      result <- evalExpr env' pred >>= flip applyFunc [target] >>= liftError . fromBoolValue
      if result then return $ msingleton $ (MState env bindings trees)
                else return MNil
    PatternExpr pattern' ->
      case matcher of
        Value Something -> 
          case pattern' of
            PatVar name nums -> do
              var <- (,) name <$> mapM (evalExpr env' >=> liftError . fromIntegerValue) nums
              return $ msingleton $ MState env ((var, target):bindings) trees
            _ -> throwError $ strMsg "something can only match with a pattern variable"
        Value (Matcher matcher) -> do
          (patterns, targetss, matchers) <- inductiveMatch env' pattern target matcher
          mfor targetss $ \ref -> do
            targets <- evalRef ref >>= fromTuple
            let trees' = zipWith3 MAtom patterns targets matchers ++ trees
            return $ MState env bindings trees'
        _ -> throwError $ TypeMismatch "matcher" matcher
processMState (MState env bindings ((MNode penv (MState _ _ [])):trees)) =
  return $ msingleton $ MState env bindings trees
processMState (MState env bindings ((MNode penv state@(MState env' bindings' (tree:trees')):trees))) = do
  case tree of
    MAtom pattern target matcher -> do
      pattern <- evalPattern env' pattern
      case pattern of
        VarExpr name nums -> do
          var <- (,) name <$> mapM (evalExpr env' >=> liftError . fromIntegerValue) nums
          case lookup var penv of
            Just pattern -> do
              return $ msingleton $ MState env bindings (MAtom pattern target matcher:MNode penv (MState env' bindings' trees'):trees)
            Nothing -> throwError $ UnboundVariable var
        _ -> processMState state >>= mmap (return . MState env bindings . (: trees) . MNode penv)
    _ -> processMState state >>= mmap (return . MState env bindings . (: trees) . MNode penv)

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
  failPPPatternMatch = throwError $ strMsg "failed primitive data pattern match"
  failPDPatternMatch = throwError $ strMsg "failed primitive pattern pattern match"

primitivePatPatternMatch :: Env -> PrimitivePatPattern -> EgisonExpr ->
                            MatchM ([EgisonExpr], [Binding])
primitivePatPatternMatch _ PPWildCard _ = return ([], [])
primitivePatPatternMatch _ PPPatVar pattern = return ([pattern], [])
primitivePatPatternMatch env (PPValuePat name) pattern = do
  pattern <- lift $ evalExpr env pattern
  case pattern of
    Value (Pattern (ValuePat expr)) -> do
      ref <- lift $ newThunk env expr
      return ([], [((name, []), ref)])
    _ -> matchFail
primitivePatPatternMatch env (PPInductivePat name patterns) pattern = do
  pattern <- lift $ evalExpr env pattern
  case pattern of
    Value (Pattern (InductivePattern name' exprs)) | name == name' ->
      (concat *** concat) . unzip <$> zipWithM (primitivePatPatternMatch env) patterns exprs
    _ -> matchFail

primitiveDataPatternMatch :: PrimitiveDataPattern -> ObjectRef -> MatchM [Binding]
primitiveDataPatternMatch PDWildCard _ = return []
primitiveDataPatternMatch (PDPatVar name) ref = return [((name, []), ref)]
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
primitiveDataPatternMatch (PDConstantPat expr) ref = undefined
--  isEqual <- (==) <$> evalExpr' nullEnv expr <*> evalRef' ref
--  if isEqual then return [] else matchFail

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
  unconsCollection' (Intermediate (ICollection ((IElement ref):inners))) =
    lift $ (,) ref <$> newEvaluatedThunk (Intermediate $ ICollection inners)
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
    unsnocCollection' (Intermediate (ICollection vals)) =
      case last vals of
        IElement ref -> lift $ (,) <$> newEvaluatedThunk (Intermediate . ICollection $ init vals) <*> pure ref
        ISubCollection ref -> do
          inners' <- lift $ evalRef ref >>= expandCollection
          let coll = Intermediate (ICollection (init vals ++ inners'))
          lift $ writeThunk ref coll
          unsnocCollection' coll

evalPattern :: Env -> EgisonExpr -> EgisonM EgisonExpr
evalPattern _ expr@(PatternExpr _) = return expr
evalPattern _ expr@(VarExpr _ _) = return expr
evalPattern _ expr@(ApplyExpr _ _) = return expr
evalPattern env (IfExpr test expr expr') = do
  test <- evalExpr env test >>= liftError . fromBoolValue
  evalPattern env $ if test then expr else expr'  
evalPattern env (LetExpr _ _) = undefined
evalPattern env (LetRecExpr _ _) = undefined
evalPattern _ _ = throwError $ strMsg "pattern expression required"
