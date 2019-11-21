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

import           Language.Egison.AST

--
-- Pretty printing for Non-S syntax
--

prettyTopExprs :: [EgisonTopExpr] -> Doc [EgisonTopExpr]
prettyTopExprs exprs = vsep $ punctuate line (map pretty exprs)

instance Pretty EgisonTopExpr where
  pretty (Define x (LambdaExpr args body)) =
    pretty x <+> hsep (map pretty args) <+> equals <> nest 2 (softline <> pretty body)
  pretty (Define x expr) = pretty x <+> equals <> nest 2 (softline <> pretty expr)
  pretty (Test expr) = pretty expr
  pretty (LoadFile file) = pretty "loadFile" <+> pretty (show file)
  pretty (Load lib) = pretty "load" <+> pretty (show lib)

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

  pretty (IfExpr x y z) =
    pretty "if" <+> pretty x
               <> nest 2 (softline <> pretty "then" <+> pretty y <>
                          softline <> pretty "else" <+> pretty z)
  pretty (LetRecExpr bindings body) =
    hang 1 (pretty "let" <+> align (vsep (map pretty bindings)) <> hardline <> pretty "in" <+> pretty body)
  pretty (LetExpr _ _) = error "unreachable"
  pretty (LetStarExpr _ _) = error "unreachable"

  pretty (MatchExpr BFSMode tgt matcher clauses) =
    pretty "match"       <+> pretty tgt <+> prettyMatch matcher clauses
  pretty (MatchExpr DFSMode tgt matcher clauses) =
    pretty "matchDFS"    <+> pretty tgt <+> prettyMatch matcher clauses
  pretty (MatchAllExpr BFSMode tgt matcher clauses) =
    pretty "matchAll"    <+> pretty tgt <+> prettyMatch matcher clauses
  pretty (MatchAllExpr DFSMode tgt matcher clauses) =
    pretty "matchAllDFS" <+> pretty tgt <+> prettyMatch matcher clauses
  pretty (MatchLambdaExpr matcher clauses) =
    pretty "\\match"     <+> prettyMatch matcher clauses
  pretty (MatchAllLambdaExpr matcher clauses) =
    pretty "\\matchAll"  <+> prettyMatch matcher clauses

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

  pretty SomethingExpr = pretty "something"
  pretty UndefinedExpr = pretty "undefined"

instance Pretty Arg where
  pretty (ScalarArg x)         = pretty x
  pretty (InvertedScalarArg x) = pretty "*" <> pretty x
  pretty (TensorArg x)         = pretty '%' <> pretty x

instance Pretty Var where
  -- TODO: indices
  pretty (Var xs is) = concatWith (surround dot) (map pretty xs)

instance Pretty InnerExpr where
  pretty (ElementExpr x) = pretty x
  pretty (SubCollectionExpr _) = error "Not supported"

instance {-# OVERLAPPING #-} Pretty BindingExpr where
  pretty ([var], expr) = pretty var <+> pretty "=" <+> pretty expr
  pretty (vars, expr) = tupled (map pretty vars) <+> pretty "=" <+> pretty expr

instance {-# OVERLAPPING #-} Pretty MatchClause where
  pretty (pat, expr) = pipe <+> pretty pat <+> pretty "->" <> softline <> pretty expr

instance Pretty EgisonPattern where
  pretty WildCard     = pretty "_"
  pretty (PatVar x)   = pretty "$" <> pretty x
  pretty (ValuePat v) = pretty "#" <> parens (pretty v) -- TODO: remove parens
  pretty (PredPat v)  = pretty "?" <> parens (pretty v)
  pretty _            = pretty "hoge"

pretty' :: EgisonExpr -> Doc ann
pretty' x@(UnaryOpExpr _ _) = parens $ pretty x
pretty' x                   = pretty x

prettyMatch :: EgisonExpr -> [MatchClause] -> Doc ann
prettyMatch matcher clauses =
  pretty "as" <+> pretty matcher <+> pretty "with" <> hardline <>
    align (vsep (map pretty clauses))

listoid :: String -> String -> [Doc ann] -> Doc ann
listoid lp rp elems = encloseSep (pretty lp) (pretty rp) (comma <> space) elems
