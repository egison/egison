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
  ( IState(..)
  , MonadEval(..)
  , modifyCounter
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.IORef

import           System.IO.Unsafe          (unsafePerformIO)

import           Language.Egison.AST


data IState = IState
  -- Index counter for generating fresh variable
  { indexCounter  :: Int
  -- Names of called functions for improved error message
  , funcNameStack :: [String]
  }

class (Applicative m, Monad m) => MonadEval m where
  fresh :: m String
  freshV :: m Var
  pushFuncName :: String -> m ()
  topFuncName :: m String
  popFuncName :: m ()
  getFuncNameStack :: m [String]

instance (MonadEval m) => MonadEval (ExceptT e m) where
  fresh = lift fresh
  freshV = lift freshV
  pushFuncName name = lift $ pushFuncName name
  topFuncName = lift topFuncName
  popFuncName = lift popFuncName
  getFuncNameStack = lift getFuncNameStack

{-# NOINLINE counter #-}
counter :: IORef Int
counter = unsafePerformIO $ newIORef 0

readCounter :: IO Int
readCounter = readIORef counter

updateCounter :: Int -> IO ()
updateCounter = writeIORef counter

modifyCounter :: MonadIO m => StateT IState m a -> m a
modifyCounter m = do
  x <- liftIO $ readCounter
  (result, st) <- runStateT m (IState { indexCounter = x, funcNameStack = [] })
  liftIO $ updateCounter $ indexCounter st
  return result
