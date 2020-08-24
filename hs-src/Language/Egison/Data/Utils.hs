{-# LANGUAGE TupleSections   #-}

module Language.Egison.Data.Utils
  ( evalRef
  , writeObjectRef
  , newEvaluatedObjectRef
  , makeBindings
  , makeBindings'
  , fromTuple
  , fromTupleWHNF
  , fromTupleValue
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

writeObjectRef :: ObjectRef -> WHNFData -> EvalM ()
writeObjectRef ref val = liftIO . writeIORef ref $ WHNF val

newEvaluatedObjectRef :: WHNFData -> EvalM ObjectRef
newEvaluatedObjectRef = liftIO . newIORef . WHNF

makeBindings :: [Var] -> [ObjectRef] -> [Binding]
makeBindings = zip

makeBindings' :: [String] -> [ObjectRef] -> [Binding]
makeBindings' xs = zip (map stringToVar xs)

fromTuple :: WHNFData -> EvalM [ObjectRef]
fromTuple (Intermediate (ITuple refs)) = return refs
fromTuple (Value (Tuple vals)) = mapM (newEvaluatedObjectRef . Value) vals
fromTuple whnf = return <$> newEvaluatedObjectRef whnf

fromTupleWHNF :: WHNFData -> EvalM [WHNFData]
fromTupleWHNF (Intermediate (ITuple refs)) = mapM evalRef refs
fromTupleWHNF (Value (Tuple vals))         = return $ map Value vals
fromTupleWHNF whnf                         = return [whnf]

fromTupleValue :: EgisonValue -> [EgisonValue]
fromTupleValue (Tuple vals) = vals
fromTupleValue val          = [val]

makeTuple :: [EgisonValue] -> EgisonValue
makeTuple []  = Tuple []
makeTuple [x] = x
makeTuple xs  = Tuple xs

makeITuple :: [WHNFData] -> EvalM WHNFData
makeITuple []  = return $ Intermediate (ITuple [])
makeITuple [x] = return x
makeITuple xs  = Intermediate . ITuple <$> mapM newEvaluatedObjectRef xs
