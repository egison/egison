module Language.Egison.Core where

import Control.Arrow
import Control.Applicative
import Control.Monad.Error

import Data.IORef
import Data.List

import Language.Egison.Types
import Language.Egison.Primitives

fromTuple :: WHNFData -> EgisonM [ObjectRef]
fromTuple (Intermediate (ITuple refs)) = return refs
fromTuple (Value (Tuple vals)) = liftIO $ mapM (newIORef . WHNF . Value) vals
fromTuple val = liftIO $ return <$> newIORef (WHNF val)

recursiveBind :: Env -> [(String, EgisonExpr)] -> EgisonM Env
recursiveBind env bindings = do
  let (names, exprs) = unzip bindings
  refs <- replicateM (length bindings) $ makeThunk nullEnv UndefinedExpr
  env <- extendEnv env <$> makeBindings names refs
  zipWithM_ (\ref expr -> liftIO . writeIORef ref $ Thunk env expr) refs exprs
  return env

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

-- for REPL
evalTopExpr :: Env -> EgisonTopExpr -> EgisonM Env
evalTopExpr env (Define name expr) = recursiveBind env [(name, expr)]
evalTopExpr env (Test expr) = do
  val <- evalExpr' env expr
  liftIO $ print val
  return env
evalTopExpr env (Execute argv) = do
  main <- refVar env ("main", []) >>= evalRef
  argv <- liftIO . newIORef . WHNF . Value . Collection $ map String argv
  world <- liftIO . newIORef . WHNF $ Value World
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
evalExpr env (InductiveDataExpr name exprs) =
  Intermediate . IInductiveData name <$> mapM (makeThunk env) exprs 
evalExpr _ (TupleExpr []) = return . Value $ Tuple []
evalExpr env (TupleExpr [expr]) = evalExpr env expr
evalExpr env (TupleExpr exprs) =
  Intermediate . ITuple <$> mapM (makeThunk env) exprs 
evalExpr env (CollectionExpr []) = return . Value $ Collection []
evalExpr env (CollectionExpr inners) =
  Intermediate . ICollection <$> mapM fromInnerExpr inners
 where
  fromInnerExpr (ElementExpr expr) = IElement <$> makeThunk env expr
  fromInnerExpr (SubCollectionExpr expr) = ISubCollection <$> makeThunk env expr
evalExpr env (LambdaExpr names expr) = return . Value $ Func env names expr 
evalExpr env (IfExpr test expr expr') = do
  test <- evalExpr env test >>= liftError . fromBoolValue
  evalExpr env $ if test then expr else expr'
evalExpr env (LetExpr bindings expr) = do
  mapM extractBindings bindings >>= flip evalExpr expr . extendEnv env . concat
 where
  extractBindings ([name], expr) =
    makeThunk env expr >>= makeBindings [name] . return
  extractBindings (names, expr) =
    evalExpr env expr >>= fromTuple >>= makeBindings names
evalExpr env (LetRecExpr bindings expr) =
  recursiveBind env bindings >>= flip evalExpr expr
evalExpr env (MatchAllExpr target matcher (pattern, expr)) = do
  pattern <- makeThunk env pattern
  target <- makeThunk env target
  matcher <- makeThunk env matcher
  Intermidiate . ICollection . ISubCollection . MatchThunk <$> (patternMatch [(MState env [] [(MAtom pattern target matcher)])]) expr
evalExpr env (ApplyExpr func args) = do
  func <- evalExpr env func
  args <- evalExpr env args >>= fromTuple 
  applyFunc func args
evalExpr env UndefinedExpr = throwError $ strMsg "undefined"
evalExpr env expr = throwError $ NotImplemented ("evalExpr for " ++ show expr)

evalExpr' :: Env -> EgisonExpr -> EgisonM EgisonValue
evalExpr' env expr = do
  val <- evalExpr env expr
  case val of
    Value val -> return val
    Intermediate i -> evalIntermediate i

evalRef :: ObjectRef -> EgisonM WHNFData
evalRef ref = do
  obj <- liftIO $ readIORef ref
  case obj of
    WHNF val -> return val
    Thunk env expr -> do
      val <- evalExpr env expr
      liftIO . writeIORef ref $ WHNF val
      return val
    MatchThunk mstates expr -> undefined

evalRef' :: ObjectRef -> EgisonM EgisonValue
evalRef' ref = do
  obj <- liftIO $ readIORef ref
  case obj of
    WHNF (Value val) -> return val
    WHNF (Intermediate i) -> do
      val <- evalIntermediate i
      liftIO . writeIORef ref . WHNF $ Value val
      return val
    Thunk env expr -> do
      val <- evalExpr' env expr
      liftIO . writeIORef ref . WHNF $ Value val
      return val
    MatchThunk mstates expr -> undefined

evalIntermediate :: Intermediate -> EgisonM EgisonValue
evalIntermediate (IInductiveData name refs) = InductiveData name <$> mapM evalRef' refs
evalIntermediate (ITuple refs) = Tuple <$> mapM evalRef' refs
evalIntermediate coll = Collection <$> fromCollection (Intermediate coll)
 where
  fromCollection :: WHNFData -> EgisonM [EgisonValue]
  fromCollection (Intermediate (ICollection inners)) = concat <$> mapM fromInner inners
  fromCollection (Value (Collection vals)) = return vals
  fromCollection val = throwError $ TypeMismatch "collection" val

  fromInner :: Inner -> EgisonM [EgisonValue]
  fromInner (IElement ref) = return <$> evalRef' ref
  fromInner (ISubCollection ref) = evalRef ref >>= fromCollection

applyFunc :: WHNFData -> [ObjectRef] -> EgisonM WHNFData
applyFunc (Value (Func env names body)) args =
  makeBindings names args >>= flip evalExpr body . extendEnv env
applyFunc (Value (PrimitiveFunc func)) args =
  mapM evalRef args >>= liftM Value . liftError . func
applyFunc (Value (IOFunc func)) args =
  mapM evalRef args >>= liftM Value . func
applyFunc val _ = throwError $ TypeMismatch "function" val

patternMatch :: [MatchingState] -> EgisonM MatchingResult
patternMatch ((MState env ret []):mstates) = Find (extendEnv ret env) mstates
patternMatch [] = End
patternMatch ((MState env ret ((MAtom (PatVarExpr name nums) target matcher):mtrees)):mstates) =
  matcher <- evalExpr' matcher
  case matcher of
    Something ->
      patternMatch ((MState env (undefined:ret) mtrees):mstates)
    Matcher matcherInfo ->
      (patterns, targetss, matchers) <- inductiveMatch matcherInfo pattern target
      patternMatch ((objectMap (\targets ->
                                  (MState env ret ((map3 (\(pattern, target, matcher) ->
                                                            (MAtom pattern target matcher)
                                                         (patterns, (fromTuple targets), (fromTuple matchers)))) ++
                                                   mtrees)))
                               targetss) ++ mstates)
patternMatch ((MState env ret ((MNode _ _ []):mtrees)):mstates) =
  patternMatch ((MState env ret mtrees):mstates)
patternMatch ((MState env ret ((MNode env1 ret1 penv1 ((MAtom (VarExpr name nums) target matcher):mtrees1)):mtrees)):mstates) =
  patternMatch ((MState env ret ((MNode (refVar (VarExpr name nums) penv1) target matcher):(MNode env1 ret1 penv1 mtrees1):mtrees)):mstates)
patternMatch ((MState env ret mtrees):mstates) =
  patternMatch ((MState env ret (patternMatch' mtrees)):mstates)

patternMatch' :: [MatchingTree] -> EgisonM [MatchingTree]
patternMatch' ((MNode env ret penv ((MNode env1 ret1 penv1 ((MAtom (VarExpr name nums) target matcher):mtrees1)):mtrees)):mstates) =
  patternMatch' ((MNode env ret penv ((MAtom (refVar (VarExpr name nums) penv1) target matcher):(MNode env1 ret1 penv1 mtrees1):mtrees)):mstates)
patternMatch' = undefined

inductiveMatch :: MatcherInfo -> Object -> Object -> EgisonM [([EgisonExpr], Object, Object)]
inductiveMatch = undefined

