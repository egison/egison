{- |
Module      : Language.Egison.VarEntry
Licence     : MIT

This module defines the VarEntry data structure used by both
evaluation environment (Data.hs) and type inference environment (Type/Env.hs).
-}

module Language.Egison.VarEntry
  ( VarEntry(..)
  ) where

import           Language.Egison.IExpr      (Var(..), Index(..))

-- | Variable entry in the environment
-- Contains indices and value for a single variable binding
-- Parametrized to support both ObjectRef (for evaluation) and TypeScheme (for type inference)
data VarEntry a = VarEntry 
  { veIndices :: [Index (Maybe Var)]
  , veValue :: a
  }
  deriving (Eq, Show)
