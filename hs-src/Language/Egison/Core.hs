module Language.Egison.Core where

import Control.Arrow
import Control.Applicative
import Control.Monad.Error

import Data.IORef
import Data.List

import Language.Egison.Types

newThunk :: Env -> EgisonExpr -> EgisonM ObjectRef
newThunk = ((liftIO . newIORef . Thunk) .) . evalExpr

newWHNF :: WHNFData -> EgisonM ObjectRef
newWHNF = liftIO . newIORef . WHNF

fromTuple :: WHNFData -> EgisonM [ObjectRef]
fromTuple (Intermediate (ITuple refs)) = return refs
fromTuple (Value (Tuple vals)) = liftIO $ mapM (newWHNF . Value) vals
fromTuple val = return <$> newWHNF val

recursiveBind :: Env -> [(String, EgisonExpr)] -> EgisonM Env
recursiveBind env bindings = do
  let (names, exprs) = unzip bindings
  refs <- replicateM (length bindings) $ newThunk nullEnv UndefinedExpr
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

evalTopExpr :: Env -> EgisonTopExpr -> EgisonM Env
evalTopExpr env (Define name expr) = recursiveBind env [(name, expr)]
evalTopExpr env (Test expr) = do
  val <- evalExpr' env expr
  liftIO $ print val
  return env
evalTopExpr env (Execute argv) = do
  main <- refVar env ("main", []) >>= evalRef
  argv <- newWHNF . Value . Collection $ map String argv
  world <- newWHNF $ Value World
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
  extractBindings ([name], expr) =
    newThunk env expr >>= makeBindings [name] . return
  extractBindings (names, expr) =
    evalExpr env expr >>= fromTuple >>= makeBindings names
evalExpr env (LetRecExpr bindings expr) =
  recursiveBind env bindings >>= flip evalExpr expr
evalExpr env (MatchAllExpr target matcher (pattern, expr)) = do
  target <- newThunk env target
  result <- patternMatch pattern target matcher
  mmap (flip evalExpr expr) result >>= fromMList
 where
  fromMList :: MList EgisonM WHNFData -> EgisonM WHNFData
  fromMList MNil = return . Value $ Collection []
  fromMList (MCons val vals) = do
    head <- IElement <$> newWHNF val
    tail <- ISubCollection <$> (liftIO . newIORef . Thunk $ fromMList vals)
    return . Intermidiate $ ICollection [head, tail]
evalExpr env (MatchExpr target matcher clauses) = do
  target <- newThunk env target
  result <- liftM msum .  forM clauses $ \(pattern, expr) -> do
    result <- patternMatch pattern target matcher
    case result of
      MCons env _ -> Just <$> evalExpr env expr
      MNil -> Nothing
  maybe (throwError $ strMsg "can't match") return result
evalExpr env (ApplyExpr func args) = do
  func <- evalExpr env func
  args <- evalExpr env args >>= fromTuple 
  applyFunc func args
evalExpr env UndefinedExpr = throwError $ strMsg "undefined"
evalExpr env expr = throwError $ NotImplemented ("evalExpr for " ++ show expr)

evalExpr' :: Env -> EgisonExpr -> EgisonM EgisonValue
evalExpr' env expr = evalExpr env expr >>= evalWHNF

evalRef :: ObjectRef -> EgisonM WHNFData
evalRef ref = do
  obj <- liftIO $ readIORef ref
  case obj of
    WHNF val -> return val
    Thunk thunk -> do
      val <- thunk
      liftIO . writeIORef ref $ WHNF val
      return val

evalRef' :: ObjectRef -> EgisonM EgisonValue
evalRef' ref = do
  obj <- liftIO $ readIORef ref
  case obj of
    WHNF (Value val) -> return val
    WHNF (Intermediate i) -> do
      val <- evalIntermediate i
      liftIO . writeIORef ref . WHNF $ Value val
      return val
    Thunk thunk -> do
      val <- thunk >>= evalWHNF
      liftIO . writeIORef ref . WHNF $ Value val
      return val

evalWHNF :: WHNFData -> EgisonM EgisonValue
evalWHNF (Value val) = return val
evalWHNF (Intermidiate i) = evalIntermediate i

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

--
-- Pattern Match
--

patternMatch :: Env -> EgisonExpr -> ObjectRef -> EgisonExpr -> EgisonM (MList EgisonM Env) 
patternMatch env pattern target matcher = undefined
--  processMState $ MState env [] [MAtom pattern target matcher]

processMState :: MatchingState -> EgisonM (Maybe Env, MList EgisonM MatchingState)
processMState = undefined

inductiveMatch :: EgisonExpr -> ObjectRef -> MatcherInfo ->
                  EgisonM ([EgisonExpr], MList ObjectRef, [EgisonExpr])
inductiveMatch pattern target matcherInfo = do
  forM matcherInfo $ \(pppat, matchers, clauses) -> do
  evalExpr env expr >>= fromCollection
 where
  fromCollection :: WHNFData -> EgisonM (MList EgisonM ObjectRef)
  fromCollection (Value (Collection [])) = MNil
  fromCollection (Value (Collection vals)) =
    mapM (newWHNF . Value) vals >>= fromList
  fromCollection coll@(Intermedeiate (ICollection _)) = do
    isEmpty <- isEmptyCollection coll
    if isEmpty
      then MNil
      else do
        (head, tail) <- consDestruct coll
        return $ MCons head (evalRef tail >>= fromCollection)
  fromCollection val = throwError $ TypeMismatch "collection" val

primitivePatPatternMatch :: Env -> PrimitivePatPattern -> EgisonExpr ->
                            EgisonM (Maybe ([EgisonExpr], Frame))
primitivePatPatternMatch PPWildCard _ = return $ Just ([], [])
primitivePatPatternMatch PPPatVar pattern = return $ Just ([pattern], [])
primitivePatPatternMatch (PPValuePat name) pattern = do
  pattern <- evalExpr env pattern
  case pattern of
    Value (Pattern (ValuePat expr)) ->
      Just . flip (,) [] . return . (,) (name, []) <$> newThunk env expr
    _ -> return Nothing
primitivePatPatternMatch (PPInductivePat name patterns) pattern = do
  pattern <- evalExpr env pattern
  case pattern of
    Value (Pattern (InductivePattern name' exprs)) | name == name' ->
      (concat *** concat) . unzip <$> zipWithM (primitivePatPatternMatch env) patterns exprs
    _ -> throwError MatchError

primitiveDataPatternMatch :: PrimitiveDataPattern -> ObjectRef -> EgisonM (Maybe Frame)
primitiveDataPatternMatch PDWildCard _ = return $ Just [] 
primitiveDataPatternMatch (PDPatVar name) ref = return $ Just [((name, []), ref)]
primitiveDataPatternMatch (PInductivePat name patterns) ref = undefined
primitiveDataPatternMatch PDEmptyPat ref = do
  val <- evalRef ref >>= isEmptyCollection
  val
primitiveDataPatternMatch (PDConsPat pattern1 pattern2) ref = do
  (head, tail) <- evalRef ref >>= consDestruct
  (++) <$> primitiveDataPatternMatch pattern1 head
       <*> primitiveDataPatternMatch pattern2 tail
primitiveDataPatternMatch (PDSnocPat pattern1 pattern2) ref = undefined
primitiveDataPatternMatch (PDConstantPat expr) val = undefined

isEmptyCollection :: WHNFData -> EgisonM Bool
isEmptyCollection (Value (Collection [])) = return True
isEmptyCollection (Intermediate (ICollection [])) = return True
isEmptyCollection (Intermediate (ICollection (ISubCollection ref):inners)) = do
  isEmpty <- evalRef ref >>= isEmptyCollection
  if isEmpty
    then isEmptyCollection $ Intermediate (ICollection inners)
    else return False
isEmptyCollection _ = return False

consDestruct :: WHNFData -> EgisonM (ObjectRef, ObjectRef)
consDestruct (Value (Collection (val:vals))) =
  (,) <$> newWHNF (Value val) <*>  newWHNF (Value $ Collection vals)
consDestruct (Intermediate (ICollection (IElement ref):inners)) =
  (,) ref <$> newWHNF (Intermediate $ ICollection inners)
consDestruct (Intermediate (ICollection (ISubCollection ref):inners)) = do
  (head, tail) <- evalRef ref >>= consDestruct
  (,) head <$> newWHNF (Intermediate $ ICollection ((ISubCollection tail):inners))
consDestruct _ = throwError $ MatchError

snocDestruct :: WHNFData -> EgisonM (ObjectRef, ObjectRef)
snocDestruct = undefined
