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
  , updateHash
  ) where

import           Control.Monad
import           Control.Monad.State   (liftIO)
import           Control.Monad.Except            (throwError)

import           Data.IORef
import qualified Data.HashMap.Lazy               as HL

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
pmIndices (MultiSub (Just a) s (Just e):xs) vs = do
  let (vs1, vs2) = span isSub vs
  let l = fromIntegral (length vs1)
  eRef <- newEvaluatedObjectRef (Value (toEgison l))
  let hash = (IIntHash HL.empty)
  liftIO $ putStrLn $ show l
  hash <- foldM (\hash (i, v) -> updateHash [i] v hash) hash (zip [s..(s + l - 1)] (map (\(Sub v) -> Value v) vs1)) 
  aRef <- newEvaluatedObjectRef hash
  bs <- pmIndices xs vs2
  return ((a, aRef) : (e, eRef) : bs)
 where
  isSub (Sub _) = True
  isSub _       = False
pmIndices (MultiSup (Just a) s (Just e):xs) vs = do
  let (vs1, vs2) = span isSup vs
  let l = fromIntegral (length vs1)
  eRef <- newEvaluatedObjectRef (Value (toEgison l))
  let hash = (IIntHash HL.empty)
  liftIO $ putStrLn $ show l
  hash <- foldM (\hash (i, v) -> updateHash [i] v hash) hash (zip [s..(s + l - 1)] (map (\(Sup v) -> Value v) vs1)) 
  aRef <- newEvaluatedObjectRef hash
  bs <- pmIndices xs vs2
  return ((a, aRef) : (e, eRef) : bs)
 where
  isSup (Sup _) = True
  isSup _       = False

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

updateHash :: [Integer] -> WHNFData -> WHNFData -> EvalM WHNFData
updateHash [index] tgt (IIntHash hash) = do
  targetRef <- newEvaluatedObjectRef tgt
  return . IIntHash $ HL.insert index targetRef hash
updateHash (index:indices) tgt (IIntHash hash) = do
  val <- maybe (return $ IIntHash HL.empty) evalRef $ HL.lookup index hash
  ref <- updateHash indices tgt val >>= newEvaluatedObjectRef
  return . IIntHash $ HL.insert index ref hash
updateHash indices tgt (Value (IntHash hash)) = do
  let keys = HL.keys hash
  vals <- mapM (newEvaluatedObjectRef . Value) $ HL.elems hash
  updateHash indices tgt (IIntHash $ HL.fromList $ zip keys vals)
updateHash _ _ v = throwError $ Default $ "expected hash value: " ++ show v
