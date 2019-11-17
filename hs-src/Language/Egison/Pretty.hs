{-# LANGUAGE FlexibleInstances #-}

{- |
Module      : Language.Egison.PrettyPrint
Copyright   : Satoshi Egi
Licence     : MIT

This module contains pretty printing for Egison syntax
-}

module Language.Egison.Pretty
    ( prettyTopExprs
    ) where

import           Data.Text.Prettyprint.Doc

import           Language.Egison.Types

--
-- Pretty printing for Non-S syntax
--

prettyTopExprs :: [EgisonTopExpr] -> Doc [EgisonTopExpr]
prettyTopExprs exprs = vsep $ punctuate line (map pretty exprs)

instance Pretty EgisonTopExpr where
  pretty (Define x (LambdaExpr args body)) =
    pretty x <+> hsep (map pretty args) <+> equals <> softline <> pretty body
  pretty (Define x expr) = pretty x <+> equals <> nest 2 (softline <> pretty expr)
  pretty (Test expr) = pretty expr

instance Pretty EgisonExpr where
  pretty (CharExpr x)    = squote <> pretty x <> squote
  pretty (StringExpr x)  = dquote <> pretty x <> dquote
  pretty (BoolExpr x)    = pretty x
  pretty (IntegerExpr x) = pretty x
  pretty (FloatExpr x)   = pretty x
  pretty (VarExpr x)     = pretty x

  pretty (InductiveDataExpr c xs) = nest 2 (pretty c <+> fillSep (map pretty xs))

  pretty (TupleExpr xs) = tupled (map pretty xs)
  pretty (CollectionExpr xs) = list (map pretty xs)
  pretty (ArrayExpr xs)  = listoid "(|" "|)" (map pretty xs)
  pretty (HashExpr xs)   = listoid "{|" "|}" (map (\(x, y) -> list [pretty x, pretty y]) xs)
  pretty (VectorExpr xs) = listoid "[|" "|]" (map pretty xs)

  pretty (LambdaExpr xs y)          = pretty "\\" <> hsep (map pretty xs) <+> pretty "->" <> nest 2 (softline <> pretty y)
  pretty (PatternFunctionExpr xs y) = pretty "\\" <> hsep (map pretty xs) <+> pretty "=>" <> softline <> pretty y

  pretty (UnaryOpExpr op x) = pretty op <> pretty x
  -- (x1 op' x2) op y
  pretty (BinaryOpExpr op x@(BinaryOpExpr op' _ _) y) =
    if priority op > priority op' || priority op == priority op' && assoc op == RightAssoc
       then parens (pretty x) <+> pretty (repr op) <+> pretty' y
       else pretty x <+> pretty (repr op) <+> pretty' y
  -- x op (y1 op' y2)
  pretty (BinaryOpExpr op x y@(BinaryOpExpr op' _ _)) =
    if priority op > priority op' || priority op == priority op' && assoc op == LeftAssoc
       then pretty x <+> pretty (repr op) <+> parens (pretty y)
       else pretty x <+> pretty (repr op) <+> pretty' y
  pretty (BinaryOpExpr op x y) = pretty x <+> pretty (repr op) <+> pretty' y

  pretty (ApplyExpr x (TupleExpr ys)) = nest 2 (pretty x <+> fillSep (map pretty ys))

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

instance Pretty EgisonPattern where
  pretty x = undefined

pretty' :: EgisonExpr -> Doc ann
pretty' x@(UnaryOpExpr _ _) = parens $ pretty x
pretty' x                   = pretty x

listoid :: String -> String -> [Doc ann] -> Doc ann
listoid lp rp elems = encloseSep (pretty lp) (pretty rp) (comma <> space) elems
