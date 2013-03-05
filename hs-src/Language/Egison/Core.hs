module Language.Egison.Core where

import Control.Applicative
import Control.Arrow
import Control.Applicative
import Control.Monad.Error

import Data.IORef
import Data.List

import Language.Egison.Types

fromIntegerValue :: WHNFData -> Either EgisonError Integer
fromIntegerValue (Value (Integer i)) = return i
fromIntegerValue val = throwError $ TypeMismatch "integer" val

fromBoolValue :: WHNFData -> Either EgisonError Bool
fromBoolValue (Value (Bool b)) = return b
fromBoolValue val = throwError $ TypeMismatch "bool" val

fromTuple :: WHNFData -> EgisonM [ObjectRef]
fromTuple (Intermediate (ITuple refs)) = return refs
fromTuple (Value (Tuple vals)) = liftIO $ mapM (newIORef . WHNF . Value) vals
fromTuple val = liftIO $ return <$> newIORef (WHNF val)

recursiveBind :: Env -> [(String, EgisonExpr)] -> EgisonM Env
recursiveBind env bindings = do
  refs <- replicateM (length bindings) $ makeThunk env UndefinedExpr
  env <- extendEnv env <$> makeBindings (map fst bindings) refs
  forM_ (zip refs (map snd bindings)) (\(ref, expr) -> do
    liftIO . writeIORef ref $ Thunk env expr)
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
  bindings <- concat <$> forM bindings (\(names, expr) ->
    evalExpr env expr >>= fromTuple >>= makeBindings names)
  evalExpr (extendEnv env bindings) expr
evalExpr env (LetRecExpr bindings expr) = do
  recursiveBind env bindings >>= flip evalExpr expr
evalExpr env (ApplyExpr func args) = do
  func <- evalExpr env func 
  args <- evalExpr env args
  case func of
    Value (Func env names body) -> do
      bindings <- fromTuple args >>= makeBindings names  
      evalExpr (extendEnv env bindings) body
    Value (PrimitiveFunc func) -> do
      args <- fromTuple args >>= mapM evalRef
      Value <$> liftError (func args)
    val -> throwError $ TypeMismatch "function" val
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

--
-- Primitives
--

primitives :: IO Env
primitives = do
  extendEnv nullEnv <$> forM primitiveOps (\(name, op) -> do
    ref <- newIORef . WHNF . Value $ PrimitiveFunc op
    return ((name, []), ref))

primitiveOps :: [(String, PrimitiveFunc)]
primitiveOps = [ ("+", add) 
               , ("-", sub)
               , ("*", mul)
               , ("eq?", eq) ]

add :: PrimitiveFunc
add [val, val'] = (Integer .) . (+) <$> fromIntegerValue val
                                    <*> fromIntegerValue val'
add vals = throwError $ ArgumentsNum 2 vals

sub :: PrimitiveFunc
sub [val, val'] = (Integer .) . (-) <$> fromIntegerValue val
                                    <*> fromIntegerValue val'
sub vals = throwError $ ArgumentsNum 2 vals

mul :: PrimitiveFunc
mul [val, val'] = (Integer .) . (*) <$> fromIntegerValue val
                                    <*> fromIntegerValue val'
mul vals = throwError $ ArgumentsNum 2 vals

eq :: PrimitiveFunc
eq [Value val, Value val'] = Bool <$> eq' val val'
 where
  eq' (Char c) (Char c') = return $ c == c'
  eq' (String s) (String s') = return $ s == s'
  eq' (Bool b) (Bool b') = return $ b == b'
  eq' (Integer i) (Integer i') = return $ i == i'
  eq' (Float f) (Float f') = return $ f == f'
  eq' _ _ = throwError $ TypeMismatch "immediate" $ Value val
eq [val, _] = throwError $ TypeMismatch "immediate" val
eq vals = throwError $ ArgumentsNum 2 vals
