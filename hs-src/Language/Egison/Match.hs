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
    , MatchM
    , matchFail
    ) where

import           Control.Monad.Trans.Maybe

import           Language.Egison.AST
import           Language.Egison.Data

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

data MatchingTree =
    MAtom Pattern WHNFData Matcher
  | MNode [PatternBinding] MatchingState
 deriving (Show)

type PatternBinding = (String, Pattern)

data LoopPatContext = LoopPatContext Binding ObjectRef Pattern Pattern Pattern
 deriving (Show)

data SeqPatContext =
    SeqPatContext [MatchingTree] Pattern [Matcher] [WHNFData]
  | ForallPatContext [Matcher] [WHNFData]
 deriving (Show)

--
-- Monads
--

type MatchM = MaybeT EvalM

matchFail :: MatchM a
matchFail = MaybeT $ return Nothing
