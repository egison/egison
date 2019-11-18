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
    , showTSV
    ) where

import qualified Data.Array                as Array
import           Data.Foldable             (toList)
import qualified Data.HashMap.Strict       as HashMap
import           Data.List                 (intercalate)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc
import qualified Data.Sequence             as Sq
import qualified Data.Vector               as V

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

instance ASTElement EgisonExpr where
  prettyS (CharExpr c) = "c#" ++ [c]
  prettyS (StringExpr str) = "\"" ++ T.unpack str ++ "\""
  prettyS (BoolExpr True) = "#t"
  prettyS (BoolExpr False) = "#f"
  prettyS (IntegerExpr n) = show n
  prettyS (FloatExpr x) = show x
  prettyS (VarExpr name) = prettyS name
  prettyS (PartialVarExpr n) = "%" ++ show n
  prettyS (FunctionExpr args) = "(function [" ++ unwords (map prettyS args) ++ "])"
  prettyS (IndexedExpr True expr idxs) = prettyS expr ++ concatMap prettyS idxs
  prettyS (IndexedExpr False expr idxs) = prettyS expr ++ "..." ++ concatMap prettyS idxs
  prettyS (TupleExpr exprs) = "[" ++ unwords (map prettyS exprs) ++ "]"
  prettyS (CollectionExpr ls) = "{" ++ unwords (map prettyS ls) ++ "}"

  prettyS (UnaryOpExpr op e) = op ++ " " ++ prettyS e
  prettyS (BinaryOpExpr op e1 e2) = "(" ++ prettyS e1 ++ " " ++ prettyS op ++ " " ++ prettyS e2 ++ ")"

  prettyS (QuoteExpr e) = "'" ++ prettyS e
  prettyS (QuoteSymbolExpr e) = "`" ++ prettyS e

  prettyS (ApplyExpr fn (TupleExpr [])) = "(" ++ prettyS fn ++ ")"
  prettyS (ApplyExpr fn (TupleExpr args)) = "(" ++ prettyS fn ++ " " ++ unwords (map prettyS args) ++ ")"
  prettyS (ApplyExpr fn arg) = "(" ++ prettyS fn ++ " " ++ prettyS arg ++ ")"
  prettyS (VectorExpr xs) = "[| " ++ unwords (map prettyS xs) ++ " |]"
  prettyS (WithSymbolsExpr xs e) = "(withSymbols {" ++ unwords xs ++ "} " ++ prettyS e ++ ")"
  prettyS _ = "(not supported)"

instance ASTElement EgisonValue where
  prettyS (Char c) = "c#" ++ [c]
  prettyS (String str) = "\"" ++ T.unpack str ++ "\""
  prettyS (Bool True) = "#t"
  prettyS (Bool False) = "#f"
  prettyS (ScalarData mExpr) = prettyS mExpr
  prettyS (TensorData (Tensor [_] xs js)) = "[| " ++ unwords (map prettyS (V.toList xs)) ++ " |]" ++ concatMap prettyS js
  prettyS (TensorData (Tensor [0, 0] _ js)) = "[| [|  |] |]" ++ concatMap prettyS js
  prettyS (TensorData (Tensor [i, j] xs js)) = "[| " ++ f (fromIntegral j) (V.toList xs) ++ "|]" ++ concatMap prettyS js
   where
    f j [] = ""
    f j xs = "[| " ++ unwords (map prettyS (take j xs)) ++ " |] " ++ f j (drop j xs)
  prettyS (TensorData (Tensor ns xs js)) = "(tensor {" ++ unwords (map show ns) ++ "} {" ++ unwords (map prettyS (V.toList xs)) ++ "} )" ++ concatMap prettyS js
  prettyS (Float x) = show x
  prettyS (InductiveData name []) = "<" ++ name ++ ">"
  prettyS (InductiveData name vals) = "<" ++ name ++ " " ++ unwords (map prettyS vals) ++ ">"
  prettyS (Tuple vals) = "[" ++ unwords (map prettyS vals) ++ "]"
  prettyS (Collection vals) = if Sq.null vals
                             then "{}"
                             else "{" ++ unwords (map prettyS (toList vals)) ++ "}"
  prettyS (Array vals) = "(|" ++ unwords (map prettyS $ Array.elems vals) ++ "|)"
  prettyS (IntHash hash) = "{|" ++ unwords (map (\(key, val) -> "[" ++ show key ++ " " ++ prettyS val ++ "]") $ HashMap.toList hash) ++ "|}"
  prettyS (CharHash hash) = "{|" ++ unwords (map (\(key, val) -> "[" ++ show key ++ " " ++ prettyS val ++ "]") $ HashMap.toList hash) ++ "|}"
  prettyS (StrHash hash) = "{|" ++ unwords (map (\(key, val) -> "[\"" ++ T.unpack key ++ "\" " ++ prettyS val ++ "]") $ HashMap.toList hash) ++ "|}"
  prettyS UserMatcher{} = "#<user-matcher>"
  prettyS (Func Nothing _ args _) = "(lambda [" ++ unwords (map ('$':) args) ++ "] ...)"
  prettyS (Func (Just name) _ _ _) = prettyS name
  prettyS (PartialFunc _ n expr) = show n ++ "#" ++ prettyS expr
  prettyS (CFunc Nothing _ name _) = "(cambda " ++ name ++ " ...)"
  prettyS (CFunc (Just name) _ _ _) = prettyS name
  prettyS (MemoizedFunc Nothing _ _ _ names _) = "(memoized-lambda [" ++ unwords names ++ "] ...)"
  prettyS (MemoizedFunc (Just name) _ _ _ names _) = prettyS name
  prettyS (Proc Nothing _ names _) = "(procedure [" ++ unwords names ++ "] ...)"
  prettyS (Proc (Just name) _ _ _) = name
  prettyS (Macro names _) = "(macro [" ++ unwords names ++ "] ...)"
  prettyS PatternFunc{} = "#<pattern-function>"
  prettyS (PrimitiveFunc name _) = "#<primitive-function " ++ name ++ ">"
  prettyS (IOFunc _) = "#<io-function>"
  prettyS (QuotedFunc _) = "#<quoted-function>"
  prettyS (Port _) = "#<port>"
  prettyS Something = "something"
  prettyS Undefined = "undefined"
  prettyS World = "#<world>"
  prettyS EOF = "#<eof>"

instance ASTElement Var where
  prettyS = show

instance ASTElement VarWithIndices where
  prettyS = show

instance ASTElement EgisonBinOp where
  prettyS = show

instance ASTElement InnerExpr where
  prettyS (ElementExpr e) = prettyS e
  prettyS (SubCollectionExpr e) = '@' : prettyS e

instance ASTElement Arg where
  prettyS (ScalarArg name)         = "$" ++ name
  prettyS (InvertedScalarArg name) = "*$" ++ name
  prettyS (TensorArg name)         = "%" ++ name

instance ASTElement ScalarData where
  prettyS (Div p1 (Plus [Term 1 []])) = prettyS p1
  prettyS (Div p1 p2)                 = "(/ " ++ prettyS p1 ++ " " ++ prettyS p2 ++ ")"

instance ASTElement PolyExpr where
  prettyS (Plus [])  = "0"
  prettyS (Plus [t]) = prettyS t
  prettyS (Plus ts)  = "(+ " ++ unwords (map prettyS ts)  ++ ")"

instance ASTElement TermExpr where
  prettyS (Term a []) = show a
  prettyS (Term 1 [x]) = showPoweredSymbol x
  prettyS (Term 1 xs) = "(* " ++ unwords (map showPoweredSymbol xs) ++ ")"
  prettyS (Term a xs) = "(* " ++ show a ++ " " ++ unwords (map showPoweredSymbol xs) ++ ")"

showPoweredSymbol :: (SymbolExpr, Integer) -> String
showPoweredSymbol (x, 1) = prettyS x
showPoweredSymbol (x, n) = prettyS x ++ "^" ++ show n

instance ASTElement SymbolExpr where
  prettyS (Symbol _ (':':':':':':_) []) = "#"
  prettyS (Symbol _ s []) = s
  prettyS (Symbol _ s js) = s ++ concatMap prettyS js
  prettyS (Apply fn mExprs) = "(" ++ prettyS fn ++ " " ++ unwords (map prettyS mExprs) ++ ")"
  prettyS (Quote mExprs) = "'" ++ prettyS mExprs
  prettyS (FunctionData Nothing argnames args js) = "(functionData [" ++ unwords (map prettyS argnames) ++ "])" ++ concatMap prettyS js
  prettyS (FunctionData (Just name) argnames args js) = prettyS name ++ concatMap prettyS js

showTSV :: EgisonValue -> String
showTSV (Tuple (val:vals)) = foldl (\r x -> r ++ "\t" ++ x) (prettyS val) (map prettyS vals)
showTSV (Collection vals) = intercalate "\t" (map prettyS (toList vals))
showTSV val = prettyS val

instance ASTElement (Index EgisonExpr) where
  prettyS (Superscript i)  = "~" ++ prettyS i
  prettyS (Subscript i)    = "_" ++ prettyS i
  prettyS (SupSubscript i) = "~_" ++ prettyS i
  prettyS (DFscript _ _)   = ""
  prettyS (Userscript i)   = "|" ++ prettyS i

instance ASTElement (Index ScalarData) where
  prettyS (Superscript i)  = "~" ++ prettyS i
  prettyS (Subscript i)    = "_" ++ prettyS i
  prettyS (SupSubscript i) = "~_" ++ prettyS i
  prettyS (DFscript _ _)   = ""
  prettyS (Userscript i)   = "|" ++ prettyS i

instance ASTElement (Index EgisonValue) where
  prettyS (Superscript i) = case i of
    ScalarData (Div (Plus [Term 1 [(Symbol id name (a:indices), 1)]]) (Plus [Term 1 []])) -> "~[" ++ prettyS i ++ "]"
    _ -> "~" ++ prettyS i
  prettyS (Subscript i) = case i of
    ScalarData (Div (Plus [Term 1 [(Symbol id name (a:indices), 1)]]) (Plus [Term 1 []])) -> "_[" ++ prettyS i ++ "]"
    _ -> "_" ++ prettyS i
  prettyS (SupSubscript i) = "~_" ++ prettyS i
  prettyS (DFscript i j) = "_d" ++ show i ++ show j
  prettyS (Userscript i) = case i of
    ScalarData (Div (Plus [Term 1 [(Symbol id name (a:indices), 1)]]) (Plus [Term 1 []])) -> "_[" ++ prettyS i ++ "]"
    _ -> "|" ++ prettyS i

instance ASTElement EgisonPattern where
  prettyS WildCard = "_"
  prettyS (PatVar var) = "$" ++ prettyS var
  prettyS (ValuePat expr) = "," ++ prettyS expr
  prettyS (PredPat expr) = "?" ++ prettyS expr
  prettyS (IndexedPat pat exprs) = prettyS pat ++ concatMap (("_" ++) . prettyS) exprs
  prettyS (LetPat bexprs pat) =
    "(let {" ++ unwords (map (\(vars, expr) -> "[" ++ varsHelper vars ++ " " ++ prettyS expr ++ "]") bexprs) ++
      "} " ++ prettyS pat ++ ")"
    where varsHelper [] = ""
          varsHelper [v] = "$" ++ prettyS v
          varsHelper vs = "[" ++ unwords (map (("$" ++) . prettyS) vs) ++ "]"
  prettyS (LaterPat pat) = "(later " ++ prettyS pat ++ ")"
  prettyS (NotPat pat) = "!" ++ prettyS pat
  prettyS (AndPat pats) = "(&" ++ concatMap ((" " ++) . prettyS) pats ++ ")"
  prettyS (OrPat pats) = "(|" ++ concatMap ((" " ++) . prettyS) pats ++ ")"
  prettyS (TuplePat pats) = "[" ++ unwords (map prettyS pats) ++ "]"
  prettyS (InductivePat name pats) = "<" ++ name ++ concatMap ((" " ++) . prettyS) pats ++ ">"
  prettyS (LoopPat var range pat endPat) = "(loop $" ++ unwords [prettyS var, prettyS range, prettyS pat, prettyS endPat] ++ ")"
  prettyS ContPat = "..."
  prettyS (PApplyPat expr pats) = "(" ++ unwords (prettyS expr : map prettyS pats) ++ ")"
  prettyS (VarPat name) = name
  prettyS SeqNilPat = "{}"
  prettyS (SeqConsPat pat pat') = "{" ++ prettyS pat ++ seqPatHelper pat' ++ "}"
    where seqPatHelper SeqNilPat = ""
          seqPatHelper (SeqConsPat pat pat') = " " ++ prettyS pat ++ seqPatHelper pat'
          seqPatHelper pat = " " ++ prettyS pat
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
