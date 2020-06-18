{- |
Module      : Language.Egison.EvalState
Licence     : MIT

This module defines the state during the evaluation.
-}

module Language.Egison.EvalState
  ( EvalState(..)
  , initialEvalState
  , MonadEval(..)
  ) where

import           Control.Monad.Except


data EvalState = EvalState
  -- Names of called functions for improved error message
  { funcNameStack :: [String]
  }

initialEvalState :: EvalState
initialEvalState = EvalState { funcNameStack = [] }

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
