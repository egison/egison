{-# LANGUAGE FlexibleInstances  #-}

module Language.Egison.IExpr
  ( ITopExpr (..)
  ) where

import           Language.Egison.AST

data ITopExpr
  = IDefine Var Expr
  | ITest Expr
  | IExecute Expr
  | ILoadFile String
  | ILoad String
  deriving Show
