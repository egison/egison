{-# LANGUAGE FlexibleInstances #-}

{- |
Module      : Language.Egison.PrettyPrint
Copyright   : Satoshi Egi
Licence     : MIT

This module contains pretty printing for Egison syntax
-}

module Language.Egison.PrettyPrint
    ( prettyTopExprs
    ) where

import           Data.Text.Prettyprint.Doc

import           Language.Egison.Types

prettyTopExprs :: [EgisonTopExpr] -> Doc [EgisonTopExpr]
prettyTopExprs exprs = vsep (punctuate line (map pretty exprs))

instance Pretty EgisonTopExpr where
  pretty (Test expr) = pretty expr
  -- pretty (Define x (LambdaExpr args body)) = pretty x <+> pretty args

instance Pretty EgisonExpr where
  pretty (CharExpr x)    = squote <> pretty x <> squote
  pretty (StringExpr x)  = dquote <> pretty x <> dquote
  pretty (BoolExpr x)    = pretty x
  pretty (IntegerExpr x) = pretty x
  pretty (FloatExpr x)   = pretty x
