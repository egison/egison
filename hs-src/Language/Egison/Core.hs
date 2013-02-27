module Language.Egison.Core where

import Control.Applicative
import Control.Arrow
import Control.Applicative
import Control.Monad.Error

import Data.IORef
import Data.List

import Language.Egison.Types

--
-- Evaluator
--

evalTopExprs :: [EgisonTopExpr] -> EgisonM ()
evalTopExprs = undefined 

evalTopExpr :: Env -> EgisonTopExpr -> EgisonM (Env,  EgisonValue)
evalTopExpr env (Define (name, expr)) = do
  obj <- evalExpr' env expr
  val <- eval obj
  binds <- tupleToRefs obj >>= makeBindings name
  liftIO $ return (extendEnv env binds, val)
evalTopExpr env (Test expr) = do
  val <- evalExpr env expr
  liftIO $ return (env, val)

evalExpr :: Env -> EgisonExpr -> EgisonM EgisonValue
evalExpr env expr = do
  obj <- evalExpr' env expr
  case obj of
    Value v -> return v
    Intermidiate i -> evalIntermidiate i
    _ -> throwError noMsg

-- Expr -> Value | Intermidiate
evalExpr' :: Env -> EgisonExpr -> EgisonM Object
evalExpr' _ (CharExpr c) = return . Value $ Char c
evalExpr' _ (StringExpr s) = return . Value $ String s 
evalExpr' _ (BoolExpr b) = return . Value $ Bool b
evalExpr' _ (IntegerExpr i) = return . Value $ Integer i
evalExpr' _ (FloatExpr d) = return . Value $ Float d
evalExpr' env (VarExpr name nums) =
  (,) name <$> mapM (evalNumExpr env) nums >>= refVar env >>= evalRef'
 where
  evalNumExpr env num = evalExpr env num >>= liftError . fromValue
evalExpr' env (InductiveDataExpr name exprs) =
  Intermidiate . IInductiveData name <$> mapM (makeThunk env) exprs 
evalExpr' _ (TupleExpr []) = return . Value $ Tuple []
evalExpr' env (TupleExpr exprs) =
  Intermidiate . ITuple <$> mapM (makeThunk env) exprs 
evalExpr' env (CollectionExpr []) = return . Value $ Collection []
evalExpr' env (CollectionExpr inners) =
  Intermidiate . ICollection <$> mapM fromInnerExpr inners
 where
  fromInnerExpr (ElementExpr expr) = IElement <$> makeThunk env expr
  fromInnerExpr (SubCollectionExpr expr) = ISubCollection <$> makeThunk env expr
evalExpr' env (LambdaExpr vars expr) = return . Value $ Func env vars expr 
evalExpr' env (IfExpr test expr expr') = do
  test <- evalExpr env test >>= liftError . fromValue
  evalExpr' env $ if test then expr else expr'
evalExpr' env (LetExpr bindings expr) =
  mapM extractBindings bindings >>= flip evalExpr' expr . extendEnv env . concat
 where
  extractBindings (names, expr) = evalExpr' env expr >>= tupleToRefs >>= makeBindings names
-- evalExpr' env (LetRecExpr bindings expr) =
evalExpr' env (ApplyExpr func args) = do
  func <- evalExpr' env func 
  args <- evalExpr' env args
  case func of
    Value (Func env names body) -> do
      bindings <- tupleToRefs args >>= makeBindings names  
      evalExpr' (extendEnv env bindings) body
    Value (PrimitiveFunc func) ->
      tupleToVals args >>= liftError . liftM Value . func
evalExpr' env UndefinedExpr = throwError $ strMsg "undefined"
evalExpr' env expr = throwError $ NotImplemented ("evalExpr' for " ++ show expr)

eval :: Object -> EgisonM EgisonValue
eval (Closure env expr) = evalExpr env expr
eval (Intermidiate i) = evalIntermidiate i
eval (Value val) = return val

eval' :: Object -> EgisonM Object
eval' (Closure env expr) = evalExpr' env expr
eval' obj = return obj

evalRef :: ObjectRef -> EgisonM EgisonValue
evalRef ref = do
  val <- liftIO (readIORef ref) >>= eval
  liftIO $ writeIORef ref $ Value val
  return val

evalRef' :: ObjectRef -> EgisonM Object
evalRef' ref = do
  obj <- liftIO (readIORef ref) >>= eval' 
  liftIO $ writeIORef ref obj
  return obj

evalIntermidiate :: Intermidiate -> EgisonM EgisonValue
evalIntermidiate (IInductiveData name refs) = InductiveData name <$> mapM evalRef refs
evalIntermidiate (ITuple refs) = Tuple <$> mapM evalRef refs
evalIntermidiate coll@(ICollection _) = Collection <$> collectionToVals (Intermidiate coll)
 where
  collectionToVals (Intermidiate (ICollection inners)) = concat <$> mapM innerToVals inners
  collectionToVals (Value (Collection vals)) = return vals
  collectionToVals _ = throwError $ TypeMismatch "collection" Something
  innerToVals (IElement ref) = return <$> evalRef ref
  innerToVals (ISubCollection ref) = evalRef' ref >>= collectionToVals

tupleToRefs :: Object -> EgisonM [ObjectRef]
tupleToRefs (Intermidiate (ITuple refs)) = return refs
tupleToRefs (Value (Tuple vals)) = liftIO $ mapM (newIORef . Value) vals
tupleToRefs obj = liftIO $ return <$> newIORef obj 

tupleToVals :: Object -> EgisonM [EgisonValue]
tupleToVals (Intermidiate (ITuple refs)) = mapM evalRef refs
tupleToVals (Value (Tuple vals)) = return vals
tupleToVals obj = return <$> eval obj

--
-- Primitives
--

primitives :: IO Env
primitives = extendEnv nullEnv <$> mapM (second' (newIORef . Value . PrimitiveFunc)) primitiveOps
 where
  second' = runKleisli . second . Kleisli

primitiveOps :: [(Var, PrimitiveFunc)]
primitiveOps = [(("+", []), add)]

add :: PrimitiveFunc
add vals = Integer . foldl' (+) 0 <$> mapM fromValue vals
