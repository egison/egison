module Language.Egison.Data.Utils
  ( evalRef
  , evalObj
  , writeObjectRef
  , newEvaluatedObjectRef
  , makeBindings
  , makeBindings'
  , tupleToRefs
  , tupleToListWHNF
  , tupleToList
  , makeTuple
  , makeITuple
  ) where

import           Control.Monad.State   (liftIO)

import           Data.IORef

import           Language.Egison.AST
import           Language.Egison.Data


evalRef :: ObjectRef -> EvalM WHNFData
evalRef ref = do
  obj <- liftIO $ readIORef ref
  case obj of
    WHNF val -> return val
    Thunk thunk -> do
      val <- thunk
      writeObjectRef ref val
      return val

evalObj :: Object -> EvalM WHNFData
evalObj (WHNF val) = return val
evalObj (Thunk thunk) = thunk

writeObjectRef :: ObjectRef -> WHNFData -> EvalM ()
writeObjectRef ref val = liftIO . writeIORef ref $ WHNF val

newEvaluatedObjectRef :: WHNFData -> EvalM ObjectRef
newEvaluatedObjectRef = liftIO . newIORef . WHNF

makeBindings :: [Var] -> [ObjectRef] -> [Binding]
makeBindings = zip

makeBindings' :: [String] -> [ObjectRef] -> [Binding]
makeBindings' xs = zip (map stringToVar xs)

tupleToRefs :: WHNFData -> EvalM [ObjectRef]
tupleToRefs (Intermediate (ITuple refs)) = return refs
tupleToRefs (Value (Tuple vals)) = mapM (newEvaluatedObjectRef . Value) vals
tupleToRefs whnf = return <$> newEvaluatedObjectRef whnf

tupleToListWHNF :: WHNFData -> EvalM [WHNFData]
tupleToListWHNF (Intermediate (ITuple refs)) = mapM evalRef refs
tupleToListWHNF (Value (Tuple vals))         = return $ map Value vals
tupleToListWHNF whnf                         = return [whnf]

tupleToList :: EgisonValue -> [EgisonValue]
tupleToList (Tuple vals) = vals
tupleToList val          = [val]

makeTuple :: [EgisonValue] -> EgisonValue
makeTuple []  = Tuple []
makeTuple [x] = x
makeTuple xs  = Tuple xs

makeITuple :: [WHNFData] -> EvalM WHNFData
makeITuple []  = return $ Intermediate (ITuple [])
makeITuple [x] = return x
makeITuple xs  = Intermediate . ITuple <$> mapM newEvaluatedObjectRef xs
