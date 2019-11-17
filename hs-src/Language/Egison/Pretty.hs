{-# LANGUAGE FlexibleInstances #-}

{- |
Module      : Language.Egison.PrettyPrint
Copyright   : Satoshi Egi
Licence     : MIT

This module contains pretty printing for Egison syntax
-}

module Language.Egison.Pretty
    ( prettyTopExprs
    , ASTElement(..)
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
  pretty (BinaryOpExpr op x@(BinaryOpExpr op' _ _) y)
    | priority op > priority op' = parens (pretty x) <+> pretty (repr op) <+> pretty' y
    | otherwise                  = pretty x <+> pretty (repr op) <+> pretty' y
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

--
-- Pretty printer for S-expression
--

class ASTElement a where
  prettyS :: a -> String

instance ASTElement EgisonPattern where
  prettyS WildCard = "_"
  prettyS (PatVar var) = "$" ++ show var
  prettyS (ValuePat expr) = "," ++ show expr
  prettyS (PredPat expr) = "?" ++ show expr
  prettyS (IndexedPat pat exprs) = prettyS pat ++ concatMap (("_" ++) . show) exprs
  prettyS (LetPat bexprs pat) = "(let {" ++ unwords (map (\(vars, expr) -> "[" ++ showVarsHelper vars ++ " " ++ show expr ++ "]") bexprs) ++
                             "} " ++ prettyS pat ++ ")"
    where showVarsHelper [] = ""
          showVarsHelper [v] = "$" ++ show v
          showVarsHelper vs = "[" ++ unwords (map (("$" ++) . show) vs) ++ "]"
  prettyS (LaterPat pat) = "(later " ++ prettyS pat ++ ")"
  prettyS (NotPat pat) = "!" ++ prettyS pat
  prettyS (AndPat pats) = "(&" ++ concatMap ((" " ++) . prettyS) pats ++ ")"
  prettyS (OrPat pats) = "(|" ++ concatMap ((" " ++) . prettyS) pats ++ ")"
  prettyS (TuplePat pats) = "[" ++ unwords (map prettyS pats) ++ "]"
  prettyS (InductivePat name pats) = "<" ++ name ++ concatMap ((" " ++) . prettyS) pats ++ ">"
  prettyS (LoopPat var range pat endPat) = "(loop $" ++ unwords [show var, prettyS range, prettyS pat, prettyS endPat] ++ ")"
  prettyS ContPat = "..."
  prettyS (PApplyPat expr pats) = "(" ++ unwords (show expr : map prettyS pats) ++ ")"
  prettyS (VarPat name) = name
  prettyS SeqNilPat = "{}"
  prettyS (SeqConsPat pat pat') = "{" ++ prettyS pat ++ showSeqPatHelper pat' ++ "}"
    where showSeqPatHelper SeqNilPat = ""
          showSeqPatHelper (SeqConsPat pat pat') = " " ++ prettyS pat ++ showSeqPatHelper pat'
          showSeqPatHelper pat = " " ++ prettyS pat
  prettyS LaterPatVar = "#"

  prettyS (DApplyPat pat pats) = "(" ++ unwords (prettyS pat : map prettyS pats) ++ ")"
  prettyS (DivPat pat pat') = "(/ " ++ prettyS pat ++ " " ++ prettyS pat' ++ ")"
  prettyS (PlusPat pats) = "(+" ++ concatMap ((" " ++) . prettyS) pats
  prettyS (MultPat pats) = "(*" ++ concatMap ((" " ++) . prettyS) pats
  prettyS (PowerPat pat pat') = "(" ++ prettyS pat ++ " ^ " ++ prettyS pat' ++ ")"

instance ASTElement LoopRange where
  prettyS (LoopRange start (ApplyExpr (VarExpr (Var ["from"] [])) (ApplyExpr _ (TupleExpr (x:_)))) endPat) =
    "[" ++ show start ++ " (from " ++ show x ++ ") " ++ prettyS endPat ++ "]"
  prettyS (LoopRange start ends endPat) = "[" ++ show start ++ " " ++ show ends ++ " " ++ prettyS endPat ++ "]"
