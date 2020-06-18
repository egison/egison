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
