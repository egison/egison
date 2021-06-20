{-# LANGUAGE FlexibleInstances #-}

{- |
Module      : Language.Egison.EvalState
Licence     : MIT

This module defines the state during the evaluation.
-}

module Language.Egison.EvalState
  ( EvalState(..)
  , initialEvalState
  , MonadEval(..)
  , mLabelFuncName
  ) where

import           Control.Monad.Except
import           Control.Monad.Trans.State.Strict

import           Language.Egison.IExpr


newtype EvalState = EvalState
  -- Names of called functions for improved error message
  { funcNameStack :: [Var]
  }

initialEvalState :: EvalState
initialEvalState = EvalState { funcNameStack = [] }

class (Applicative m, Monad m) => MonadEval m where
  pushFuncName :: Var -> m ()
  topFuncName :: m Var
  popFuncName :: m ()
  getFuncNameStack :: m [Var]

instance Monad m => MonadEval (StateT EvalState m) where
  pushFuncName name = do
    st <- get
    put $ st { funcNameStack = name : funcNameStack st }
    return ()
  topFuncName = head . funcNameStack <$> get
  popFuncName = do
    st <- get
    put $ st { funcNameStack = tail $ funcNameStack st }
    return ()
  getFuncNameStack = funcNameStack <$> get

instance (MonadEval m) => MonadEval (ExceptT e m) where
  pushFuncName name = lift $ pushFuncName name
  topFuncName = lift topFuncName
  popFuncName = lift popFuncName
  getFuncNameStack = lift getFuncNameStack

mLabelFuncName :: MonadEval m => Maybe Var -> m a -> m a
mLabelFuncName Nothing m = m
mLabelFuncName (Just name) m = do
  pushFuncName name
  v <- m
  popFuncName
  return v
