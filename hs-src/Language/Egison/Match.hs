{- |
Module      : Language.Egison.Match
Licence     : MIT

This module defines some data types Egison pattern matching.
-}

module Language.Egison.Match
    ( Match
    , MatchingTree (..)
    , MatchingState (..)
    , PatternBinding
    , LoopPatContext (..)
    , SeqPatContext (..)
    , nullMState
    , MatchM
    , matchFail
    ) where

import           Control.Monad.Trans.Maybe

import           Language.Egison.Data
import           Language.Egison.IExpr

--
-- Pattern Matching
--

type Match = [Binding]

data MatchingState
  = MState { mStateEnv      :: Env
           , loopPatCtx     :: [LoopPatContext]
           , seqPatCtx      :: [SeqPatContext]
           , mStateBindings :: [Binding]
           , mTrees         :: [MatchingTree]
           }

instance Show MatchingState where
  show ms = "(MState " ++ unwords ["_", "_", "_", show (mStateBindings ms), show (mTrees ms)] ++ ")" 

data MatchingTree
  = MAtom Pattern WHNFData Matcher
  | MNode [PatternBinding] MatchingState
  deriving Show

type PatternBinding = (String, Pattern)

data LoopPatContext = LoopPatContext Binding ObjectRef Pattern Pattern Pattern

data SeqPatContext
  = SeqPatContext [MatchingTree] Pattern [Matcher] [WHNFData]
  | ForallPatContext [Matcher] [WHNFData]

nullMState :: MatchingState -> Bool
nullMState MState{ mTrees = [] } = True
nullMState MState{ mTrees = MNode _ state : _ } = nullMState state
nullMState _ = False

--
-- Monads
--

type MatchM = MaybeT EvalM

matchFail :: MatchM a
matchFail = MaybeT $ return Nothing
