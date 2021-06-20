{- |
Module      : Language.Egison.Data.Utils
Licence     : MIT

This module provides some helper functions for handling Egison data.
-}

module Language.Egison.Data.Utils
  ( evalRef
  , evalObj
  , writeObjectRef
  , newEvaluatedObjectRef
  , tupleToRefs
  , tupleToListWHNF
  , tupleToList
  , makeTuple
  , makeITuple
  , pmIndices
  ) where

import           Control.Monad.State   (liftIO)

import           Data.IORef

import           Language.Egison.Data
import           Language.Egison.IExpr


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
evalObj (WHNF val)    = return val
evalObj (Thunk thunk) = thunk

writeObjectRef :: ObjectRef -> WHNFData -> EvalM ()
writeObjectRef ref val = liftIO . writeIORef ref $ WHNF val

newEvaluatedObjectRef :: WHNFData -> EvalM ObjectRef
newEvaluatedObjectRef = liftIO . newIORef . WHNF

tupleToRefs :: WHNFData -> EvalM [ObjectRef]
tupleToRefs (ITuple refs)        = return refs
tupleToRefs (Value (Tuple vals)) = mapM (newEvaluatedObjectRef . Value) vals
tupleToRefs whnf                 = return <$> newEvaluatedObjectRef whnf

tupleToListWHNF :: WHNFData -> EvalM [WHNFData]
tupleToListWHNF (ITuple refs)        = mapM evalRef refs
tupleToListWHNF (Value (Tuple vals)) = return $ map Value vals
tupleToListWHNF whnf                 = return [whnf]

tupleToList :: EgisonValue -> [EgisonValue]
tupleToList (Tuple vals) = vals
tupleToList val          = [val]

makeTuple :: [EgisonValue] -> EgisonValue
makeTuple []  = Tuple []
makeTuple [x] = x
makeTuple xs  = Tuple xs

makeITuple :: [WHNFData] -> EvalM WHNFData
makeITuple []  = return (ITuple [])
makeITuple [x] = return x
makeITuple xs  = ITuple <$> mapM newEvaluatedObjectRef xs

pmIndices :: [Index (Maybe Var)] -> [Index EgisonValue] -> EvalM [Binding]
pmIndices [] [] = return []
pmIndices (x:xs) (v:vs) = do
  bs <- pmIndex x v
  bs' <- pmIndices xs vs
  return (bs ++ bs')
pmIndices _ _ = throwErrorWithTrace InconsistentTensorIndex

pmIndex :: Index (Maybe Var) -> Index EgisonValue -> EvalM [Binding]
pmIndex (Sub (Just var)) (Sub val) = do
  ref <- newEvaluatedObjectRef (Value val)
  return [(var, ref)]
pmIndex (Sup (Just var)) (Sup val) = do
  ref <- newEvaluatedObjectRef (Value val)
  return [(var, ref)]
pmIndex _ _ = throwErrorWithTrace InconsistentTensorIndex
