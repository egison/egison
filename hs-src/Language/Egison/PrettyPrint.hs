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
prettyTopExprs exprs = vsep $ punctuate line (map pretty exprs)

instance Pretty EgisonTopExpr where
  pretty (Define x (LambdaExpr args body)) =
    pretty x <+> hsep (map pretty args) <+> equals <> softline <> pretty body
  pretty (Define x expr) = pretty x <+> equals <> softline <> pretty expr -- TODO: use flatAlt
  pretty (Test expr) = pretty expr

instance Pretty EgisonExpr where
  pretty (CharExpr x)    = squote <> pretty x <> squote
  pretty (StringExpr x)  = dquote <> pretty x <> dquote
  pretty (BoolExpr x)    = pretty x
  pretty (IntegerExpr x) = pretty x
  pretty (FloatExpr x)   = pretty x
  pretty (VarExpr x)     = pretty x

  pretty (TupleExpr xs) = tupled (map pretty xs)
  pretty (CollectionExpr xs) = list (map pretty xs)
  pretty (ArrayExpr xs)  = listoid "(|" "|)" (map pretty xs)
  pretty (HashExpr xs)   = listoid "{|" "|}" (map (\(x, y) -> list [pretty x, pretty y]) xs)
  pretty (VectorExpr xs) = listoid "[|" "|]" (map pretty xs)

instance Pretty Arg where
  pretty (ScalarArg x)         = pretty x
  pretty (InvertedScalarArg x) = pretty "*$" <> pretty x
  pretty (TensorArg x)         = pretty '%' <> pretty x

instance Pretty Var where
  -- TODO: indices
  pretty (Var xs is) = concatWith (surround dot) (map pretty xs)

instance Pretty InnerExpr where
  pretty (ElementExpr x) = pretty x
  pretty (SubCollectionExpr _) = error "Not supported"

listoid :: String -> String -> [Doc ann] -> Doc ann
listoid lp rp elems = encloseSep (pretty lp) (pretty rp) (comma <> space) elems
