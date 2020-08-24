{-# LANGUAGE TupleSections   #-}

module Language.Egison.Data.Collection
  ( expandCollection
  , isEmptyCollection
  , unconsCollection
  , unsnocCollection
  ) where

import           Control.Monad.Except        (throwError)
import           Control.Monad.State         hiding (join)

import           Data.IORef
import           Data.Sequence               (Seq, ViewL (..), ViewR (..), (><))
import qualified Data.Sequence               as Sq

import           Language.Egison.Data
import           Language.Egison.Data.Utils
import           Language.Egison.EvalState   (MonadEval(..))
import           Language.Egison.Match

expandCollection :: WHNFData -> EvalM (Seq Inner)
expandCollection (Value (Collection vals)) =
  mapM (fmap IElement . newEvaluatedObjectRef . Value) vals
expandCollection (Intermediate (ICollection innersRef)) = liftIO $ readIORef innersRef
expandCollection val = throwError =<< TypeMismatch "collection" val <$> getFuncNameStack

isEmptyCollection :: WHNFData -> EvalM Bool
isEmptyCollection (Value (Collection col)) = return $ Sq.null col
isEmptyCollection coll@(Intermediate (ICollection innersRef)) = do
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
unconsCollection coll@(Intermediate (ICollection innersRef)) = do
  inners <- liftIO $ readIORef innersRef
  case Sq.viewl inners of
    EmptyL -> matchFail
    IElement ref' :< tInners -> do
      tInnersRef <- liftIO $ newIORef tInners
      lift $ (ref', ) <$> newEvaluatedObjectRef (Intermediate $ ICollection tInnersRef)
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
unsnocCollection coll@(Intermediate (ICollection innersRef)) = do
  inners <- liftIO $ readIORef innersRef
  case Sq.viewr inners of
    EmptyR -> matchFail
    hInners :> IElement ref' -> do
      hInnersRef <- liftIO $ newIORef hInners
      lift $ (, ref') <$> newEvaluatedObjectRef (Intermediate $ ICollection hInnersRef)
    hInners :> ISubCollection ref' -> do
      tInners <- lift $ evalRef ref' >>= expandCollection
      liftIO $ writeIORef innersRef (hInners >< tInners)
      unsnocCollection coll
unsnocCollection _ = matchFail
