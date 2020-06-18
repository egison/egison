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
  , initialIState
  , MonadEval(..)
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.IORef

import           System.IO.Unsafe          (unsafePerformIO)

import           Language.Egison.AST


data IState = IState
  -- Names of called functions for improved error message
  { funcNameStack :: [String]
  }

initialIState :: IState
initialIState = IState { funcNameStack = [] }

class (Applicative m, Monad m) => MonadEval m where
  pushFuncName :: String -> m ()
  topFuncName :: m String
  popFuncName :: m ()
  getFuncNameStack :: m [String]

instance (MonadEval m) => MonadEval (ExceptT e m) where
  pushFuncName name = lift $ pushFuncName name
  topFuncName = lift topFuncName
  popFuncName = lift popFuncName
  getFuncNameStack = lift getFuncNameStack

-- {-# NOINLINE counter #-}
-- counter :: IORef Int
-- counter = unsafePerformIO $ newIORef 0
-- readCounter :: IO Int
-- readCounter = readIORef counter
-- updateCounter :: Int -> IO ()
-- updateCounter = writeIORef counter
