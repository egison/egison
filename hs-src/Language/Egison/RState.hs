{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Language.Egison.RState
  ( RState(..)
  , RuntimeT
  , runRuntimeT
  , evalRuntimeT
  ) where

import           Control.Monad.Trans.State
import           Control.Monad.Trans.Reader

-- import           Language.Egison.AST
import           Language.Egison.CmdOptions
import           Language.Egison.Data


data RState = RState
  { indexCounter :: Int
  , environment :: Env
  }

initialRState :: Env -> RState
initialRState e = RState
  { indexCounter = 0
  , environment = e
  }

type RuntimeT m = ReaderT EgisonOpts (StateT RState m)

runRuntimeT :: Monad m => EgisonOpts -> Env -> RuntimeT m a -> m (a, RState)
runRuntimeT opts env = flip runStateT (initialRState env) . flip runReaderT opts

evalRuntimeT :: Monad m => EgisonOpts -> Env -> RuntimeT m a -> m a
evalRuntimeT opts env = flip evalStateT (initialRState env) . flip runReaderT opts
