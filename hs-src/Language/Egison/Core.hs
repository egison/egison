{-# LANGUAGE TupleSections #-}
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

evalTopExpr :: EgisonTopExpr -> EgisonM EgisonValue
evalTopExpr (Test expr) = liftIO primitives >>= flip evalExpr expr

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
  (name,) <$> mapM (\num -> evalExpr env num >>= liftError . fromValue) nums >>= refVar env >>= evalRef'
evalExpr' env (InductiveDataExpr name exprs) =
  Intermidiate . IInductiveData name <$> mapM (liftIO . newIORef . Closure env) exprs 
evalExpr' _ (TupleExpr []) = return . Value $ Tuple []
evalExpr' env (TupleExpr exprs) =
  Intermidiate . ITuple <$> mapM (liftIO . newIORef . Closure env) exprs 
evalExpr' env (CollectionExpr []) = return . Value $ Collection []
evalExpr' env (CollectionExpr inners) =
  Intermidiate . ICollection <$> mapM fromInnerExpr inners
 where
  fromInnerExpr (ElementExpr expr) = liftIO $ IElement <$> newIORef (Closure env expr)
  fromInnerExpr (SubCollectionExpr expr) = liftIO $ ISubCollection <$> newIORef (Closure env expr)
evalExpr' env (LambdaExpr vars expr) = return . Value $ Func env vars expr 
evalExpr' env (IfExpr test expr expr') = do
  test <- evalExpr env test >>= liftError . fromValue
  evalExpr' env $ if test then expr else expr'
evalExpr' env (LetExpr bindings expr) =
  mapM extractBinding bindings >>= flip evalExpr' expr . extendEnv env . concat
 where
  extractBinding (names, expr) = evalExpr' env expr >>= fromTuple >>= makeBindings (map (, []) names)
-- evalExpr' env (LetRecExpr bindings expr) =
evalExpr' env (ApplyExpr func args) = do
  func <- evalExpr' env func 
  args <- evalExpr' env args >>= fromTuple
  apply' func args
evalExpr' env UndefinedExpr = throwError $ strMsg "undefined"
evalExpr' env _ = throwError $ NotImplemented "evalExpr'"

eval :: Object -> EgisonM EgisonValue
eval (Closure env expr) = evalExpr env expr
eval (Intermidiate i) = evalIntermidiate i
eval (Value val) = return val

eval' :: Object -> EgisonM Object
eval' (Closure env expr) = evalExpr' env expr
eval' obj = return obj

apply' :: Object -> [ObjectRef] -> EgisonM Object
apply' (Value (Func env names body)) args =
  makeBindings (map (, []) names) args >>= flip evalExpr' body . extendEnv env
apply' (Value (PrimitiveFunc func)) args =
  mapM evalRef args >>= liftError . liftM Value . func

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
evalIntermidiate coll@(ICollection _) = fromCollection (Intermidiate coll) >>= liftM Collection .  mapM evalRef

fromCollection :: Object -> EgisonM [ObjectRef]
fromCollection (Intermidiate (ICollection inners)) = concat <$> mapM fromInner inners
 where
  fromInner :: InnerObject -> EgisonM [ObjectRef]
  fromInner (IElement ref) = return [ref]
  fromInner (ISubCollection ref) = evalRef' ref >>= fromCollection 
fromCollection (Value (Collection vals)) = liftIO $ mapM (newIORef . Value) vals
fromCollection _ = throwError $ TypeMismatch "tuple" Something

fromTuple :: Object -> EgisonM [ObjectRef]
fromTuple (Intermidiate (ITuple refs)) = return refs
fromTuple (Value (Tuple vals)) = liftIO $ mapM (newIORef . Value) vals
fromTuple obj = liftIO $ return <$> newIORef obj 

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
