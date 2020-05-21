{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Language.Egison.RState
  ( RState(..)
  , ReplT
  , runReplT
  , evalReplT
  ) where

import           Control.Monad.Trans.State
import           Control.Monad.Trans.Reader

-- import           Language.Egison.AST
import           Language.Egison.CmdOptions
import           Language.Egison.Data


data RState = RState
  { indexCounter :: Int
  -- , bindings :: [(Var, EgisonExpr)]
  , environment :: Env
  }

initialRState :: Env -> RState
initialRState e = RState
  { indexCounter = 0
  -- , bindings = []
  , environment = e
  }

type ReplT m = ReaderT EgisonOpts (StateT RState m)

runReplT :: Monad m => EgisonOpts -> Env -> ReplT m a -> m (a, RState)
runReplT opts env = flip runStateT (initialRState env) . flip runReaderT opts

evalReplT :: Monad m => EgisonOpts -> Env -> ReplT m a -> m a
evalReplT opts env = flip evalStateT (initialRState env) . flip runReaderT opts
