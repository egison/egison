{-# LANGUAGE TupleSections #-}

{- |
Module      : Language.Egison.Data.Collection
Licence     : MIT

This module provides some helper functions that operates on / returns
collections.
-}

module Language.Egison.Data.Collection
  ( expandCollection
  , isEmptyCollection
  , unconsCollection
  , unsnocCollection
  , collectionToRefs
  , collectionToList
  , makeICollection
  ) where

import           Control.Monad.Except       (lift, liftIO)
import           Control.Monad.Trans.Maybe  (runMaybeT)

import           Data.Foldable              (toList)
import           Data.IORef
import           Data.Maybe                 (fromJust)
import           Data.Sequence              (Seq, ViewL (..), ViewR (..), (><))
import qualified Data.Sequence              as Sq

import           Language.Egison.Data
import           Language.Egison.Data.Utils
import           Language.Egison.MList
import           Language.Egison.Match

expandCollection :: WHNFData -> EvalM (Seq Inner)
expandCollection (Value (Collection vals)) =
  mapM (fmap IElement . newEvaluatedObjectRef . Value) vals
expandCollection (ICollection innersRef) = liftIO $ readIORef innersRef
expandCollection val = throwErrorWithTrace (TypeMismatch "collection" val)

isEmptyCollection :: WHNFData -> EvalM Bool
isEmptyCollection (Value (Collection col)) = return $ Sq.null col
isEmptyCollection coll@(ICollection innersRef) = do
  inners <- liftIO $ readIORef innersRef
  case Sq.viewl inners of
    EmptyL -> return True
    ISubCollection ref' :< tInners -> do
      hInners <- evalRef ref' >>= expandCollection
      liftIO $ writeIORef innersRef (hInners >< tInners)
      isEmptyCollection coll
    _ -> return False
isEmptyCollection _ = return False

unconsCollection :: WHNFData -> MatchM (ObjectRef, ObjectRef)
unconsCollection (Value (Collection col)) =
  case Sq.viewl col of
    EmptyL -> matchFail
    val :< vals ->
      lift $ (,) <$> newEvaluatedObjectRef (Value val)
                 <*> newEvaluatedObjectRef (Value $ Collection vals)
unconsCollection coll@(ICollection innersRef) = do
  inners <- liftIO $ readIORef innersRef
  case Sq.viewl inners of
    EmptyL -> matchFail
    IElement ref' :< tInners -> do
      tInnersRef <- liftIO $ newIORef tInners
      lift $ (ref', ) <$> newEvaluatedObjectRef (ICollection tInnersRef)
    ISubCollection ref' :< tInners -> do
      hInners <- lift $ evalRef ref' >>= expandCollection
      liftIO $ writeIORef innersRef (hInners >< tInners)
      unconsCollection coll
unconsCollection _ = matchFail

unsnocCollection :: WHNFData -> MatchM (ObjectRef, ObjectRef)
unsnocCollection (Value (Collection col)) =
  case Sq.viewr col of
    EmptyR -> matchFail
    vals :> val ->
      lift $ (,) <$> newEvaluatedObjectRef (Value $ Collection vals)
                 <*> newEvaluatedObjectRef (Value val)
unsnocCollection coll@(ICollection innersRef) = do
  inners <- liftIO $ readIORef innersRef
  case Sq.viewr inners of
    EmptyR -> matchFail
    hInners :> IElement ref' -> do
      hInnersRef <- liftIO $ newIORef hInners
      lift $ (, ref') <$> newEvaluatedObjectRef (ICollection hInnersRef)
    hInners :> ISubCollection ref' -> do
      tInners <- lift $ evalRef ref' >>= expandCollection
      liftIO $ writeIORef innersRef (hInners >< tInners)
      unsnocCollection coll
unsnocCollection _ = matchFail

collectionToRefs :: WHNFData -> EvalM (MList EvalM ObjectRef)
collectionToRefs (Value (Collection vals)) =
  if Sq.null vals then return MNil
                  else fromSeq <$> mapM (newEvaluatedObjectRef . Value) vals
collectionToRefs whnf@(ICollection _) = do
  isEmpty <- isEmptyCollection whnf
  if isEmpty
    then return MNil
    else do
      (head, tail) <- fromJust <$> runMaybeT (unconsCollection whnf)
      tail' <- evalRef tail
      return $ MCons head (collectionToRefs tail')
collectionToRefs whnf = throwErrorWithTrace (TypeMismatch "collection" whnf)

collectionToList :: EgisonValue -> EvalM [EgisonValue]
collectionToList (Collection sq) = return $ toList sq
collectionToList val             = throwErrorWithTrace (TypeMismatch "collection" (Value val))

makeICollection :: [WHNFData] -> EvalM WHNFData
makeICollection xs  = do
  is <- mapM (\x -> IElement <$> newEvaluatedObjectRef x) xs
  v <- liftIO $ newIORef $ Sq.fromList is
  return $ ICollection v
