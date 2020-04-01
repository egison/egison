{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

{- |
Module      : Language.Egison.IState
Licence     : MIT

This module defines the internal state of Egison runtime.
-}

module Language.Egison.IState
  ( RuntimeState(..)
  , FreshT(..)
  , Fresh
  , MonadFresh(..)
  , runFreshT
  , runFresh
  , counter
  , readCounter
  , updateCounter
  , modifyCounter
  ) where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader      (ReaderT)
import           Control.Monad.State
import           Control.Monad.Writer      (WriterT)
import           Data.IORef

import           System.IO.Unsafe          (unsafePerformIO)

import           Language.Egison.AST       (Var(..))

data RuntimeState = RuntimeState
    -- index counter for generating fresh variable
      { indexCounter :: Int
    -- names of called functions for improved error message
      , funcNameStack :: [String]
      }

newtype FreshT m a = FreshT { unFreshT :: StateT RuntimeState m a }
  deriving (Functor, Applicative, Monad, MonadState RuntimeState, MonadTrans)

type Fresh = FreshT Identity

class (Applicative m, Monad m) => MonadFresh m where
  fresh :: m String
  freshV :: m Var
  pushFuncName :: String -> m ()
  topFuncName :: m String
  popFuncName :: m ()
  getFuncNameStack :: m [String]

instance (Applicative m, Monad m) => MonadFresh (FreshT m) where
  fresh = FreshT $ do
    st <- get; modify (\st -> st { indexCounter = indexCounter st + 1 })
    return $ "$_" ++ show (indexCounter st)
  freshV = FreshT $ do
    st <- get; modify (\st -> st {indexCounter = indexCounter st + 1 })
    return $ Var ["$_" ++ show (indexCounter st)] []
  pushFuncName name = FreshT $ do
    st <- get
    put $ st { funcNameStack = name : funcNameStack st }
    return ()
  topFuncName = FreshT $ head . funcNameStack <$> get
  popFuncName = FreshT $ do
    st <- get
    put $ st { funcNameStack = tail $ funcNameStack st }
    return ()
  getFuncNameStack = FreshT $ funcNameStack <$> get

instance (MonadError e m) => MonadError e (FreshT m) where
  throwError = lift . throwError
  catchError m h = FreshT $ catchError (unFreshT m) (unFreshT . h)

instance (MonadState s m) => MonadState s (FreshT m) where
  get = lift get
  put s = lift $ put s

instance (MonadFresh m) => MonadFresh (StateT s m) where
  fresh = lift fresh
  freshV = lift freshV
  pushFuncName name = lift $ pushFuncName name
  topFuncName = lift topFuncName
  popFuncName = lift popFuncName
  getFuncNameStack = lift getFuncNameStack

instance (MonadFresh m) => MonadFresh (ExceptT e m) where
  fresh = lift fresh
  freshV = lift freshV
  pushFuncName name = lift $ pushFuncName name
  topFuncName = lift topFuncName
  popFuncName = lift popFuncName
  getFuncNameStack = lift getFuncNameStack

instance (MonadFresh m, Monoid e) => MonadFresh (ReaderT e m) where
  fresh = lift fresh
  freshV = lift freshV
  pushFuncName name = lift $ pushFuncName name
  topFuncName = lift topFuncName
  popFuncName = lift popFuncName
  getFuncNameStack = lift getFuncNameStack

instance (MonadFresh m, Monoid e) => MonadFresh (WriterT e m) where
  fresh = lift fresh
  freshV = lift freshV
  pushFuncName name = lift $ pushFuncName name
  topFuncName = lift topFuncName
  popFuncName = lift popFuncName
  getFuncNameStack = lift getFuncNameStack

instance MonadIO (FreshT IO) where
  liftIO = lift

runFreshT :: Monad m => RuntimeState -> FreshT m a -> m (a, RuntimeState)
runFreshT = flip (runStateT . unFreshT)

runFresh :: RuntimeState -> Fresh a -> (a, RuntimeState)
runFresh seed m = runIdentity $ flip runStateT seed $ unFreshT m

{-# NOINLINE counter #-}
counter :: IORef Int
counter = unsafePerformIO $ newIORef 0

readCounter :: IO Int
readCounter = readIORef counter

updateCounter :: Int -> IO ()
updateCounter = writeIORef counter

modifyCounter :: FreshT IO a -> IO a
modifyCounter m = do
  x <- readCounter
  (result, st) <- runFreshT (RuntimeState { indexCounter = x, funcNameStack = [] }) m
  updateCounter $ indexCounter st
  return result
