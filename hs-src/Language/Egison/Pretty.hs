{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

{- |
Module      : Language.Egison.PrettyPrint
Copyright   : Satoshi Egi
Licence     : MIT

This module contains pretty printing for Egison syntax
-}

module Language.Egison.Pretty
    ( prettyTopExprs
    , PrettyS(..)
    , showTSV
    ) where

import qualified Data.Array                as Array
import           Data.Foldable             (toList)
import qualified Data.HashMap.Strict       as HashMap
import           Data.List                 (intercalate)
import           Data.Text.Prettyprint.Doc
import qualified Data.Vector               as V

import           Language.Egison.AST
import           Language.Egison.MathExpr
import           Language.Egison.Types

--
-- Pretty printing for Non-S syntax
--

prettyTopExprs :: [EgisonTopExpr] -> Doc [EgisonTopExpr]
prettyTopExprs exprs = vsep $ punctuate line (map pretty exprs)

instance Pretty EgisonTopExpr where
  pretty (Define x (LambdaExpr args body)) =
    pretty x <+> hsep (map pretty args) <+> pretty ":=" <> nest 2 (hardline <> pretty body)
  pretty (Define x expr) = pretty x <+> pretty ":=" <> nest 2 (softline <> pretty expr)
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
  pretty (HashExpr xs)   = listoid "{|" "|}" (map (\(x, y) -> tupled [pretty x, pretty y]) xs)
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

  -- TODO: Fix display of binding expr for do
  pretty (DoExpr xs y) = pretty "do" <+> pretty xs <> hardline <> pretty y
  pretty (IoExpr x) = pretty "io" <+> pretty x

  pretty (ApplyExpr x (TupleExpr ys)) = nest 2 (pretty x <+> fillSep (map pretty' ys))

  pretty SomethingExpr = pretty "something"
  pretty UndefinedExpr = pretty "undefined"

instance Pretty Arg where
  pretty (ScalarArg x)         = pretty x
  pretty (InvertedScalarArg x) = pretty "*" <> pretty x
  pretty (TensorArg x)         = pretty '%' <> pretty x

instance Pretty Var where
  -- TODO: indices
  pretty (Var xs _) = concatWith (surround dot) (map pretty xs)

instance Pretty InnerExpr where
  pretty (ElementExpr x) = pretty x
  pretty (SubCollectionExpr _) = error "Not supported"

instance {-# OVERLAPPING #-} Pretty BindingExpr where
  pretty ([var], expr) = pretty var <+> pretty ":=" <+> pretty expr
  pretty (vars, expr) = tupled (map pretty vars) <+> pretty ":=" <+> pretty expr

instance {-# OVERLAPPING #-} Pretty MatchClause where
  pretty (pat, expr) = pipe <+> pretty pat <+> pretty "->" <> softline <> pretty expr

instance Pretty EgisonPattern where
  -- TODO: Add parenthesis according to priority
  pretty WildCard     = pretty "_"
  pretty (PatVar x)   = pretty "$" <> pretty x
  pretty (ValuePat v) = pretty "#" <> pretty v
  pretty (PredPat v)  = pretty "?" <> pretty v
  pretty (InductivePat "nil" []) = pretty "[]"
  pretty (InductivePat "cons" [x, y]) = pretty x <+> pretty "::" <+> pretty y
  pretty (InductivePat "join" [x, y]) = pretty x <+> pretty "++" <+> pretty y
  pretty (InductivePat ctor xs) = pretty ctor <+> hsep (map pretty xs)
  pretty (AndPat xs) = pintercalate (pretty "&") (map pretty xs)
  pretty (OrPat xs)  = pintercalate (pretty "|") (map pretty xs)
  pretty (TuplePat xs) = tupled $ map pretty xs
  pretty _            = pretty "hoge"

pretty' :: EgisonExpr -> Doc ann
pretty' x =
  case x of
    UnaryOpExpr _ _        -> parens $ pretty x
    ApplyExpr _ _          -> parens $ pretty x
    LambdaExpr _ _         -> parens $ pretty x
    IfExpr _ _ _           -> parens $ pretty x
    LetRecExpr _ _         -> parens $ pretty x
    MatchExpr _ _ _ _      -> parens $ pretty x
    MatchLambdaExpr _ _    -> parens $ pretty x
    MatchAllLambdaExpr _ _ -> parens $ pretty x
    _                      -> pretty x

prettyMatch :: EgisonExpr -> [MatchClause] -> Doc ann
prettyMatch matcher clauses =
  pretty "as" <+> group (pretty matcher) <+> pretty "with" <>
    (nest 2 (hardline <> align (vsep (map pretty clauses))))

listoid :: String -> String -> [Doc ann] -> Doc ann
listoid lp rp elems = encloseSep (pretty lp) (pretty rp) (comma <> space) elems

pintercalate :: Doc ann -> [Doc ann] -> Doc ann
pintercalate sep elems = encloseSep emptyDoc emptyDoc (space <> sep <> space) elems

--
-- Pretty printer for S-expression
--

class PrettyS a where
  prettyS :: a -> String

instance PrettyS EgisonExpr where
  prettyS (CharExpr c) = "c#" ++ [c]
  prettyS (StringExpr str) = show str
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

instance PrettyS EgisonValue where
  prettyS (Char c) = "c#" ++ [c]
  prettyS (String str) = show str
  prettyS (Bool True) = "#t"
  prettyS (Bool False) = "#f"
  prettyS (ScalarData mExpr) = prettyS mExpr
  prettyS (TensorData (Tensor [_] xs js)) = "[| " ++ unwords (map prettyS (V.toList xs)) ++ " |]" ++ concatMap prettyS js
  prettyS (TensorData (Tensor [0, 0] _ js)) = "[| [|  |] |]" ++ concatMap prettyS js
  prettyS (TensorData (Tensor [_, j] xs js)) = "[| " ++ f (fromIntegral j) (V.toList xs) ++ "|]" ++ concatMap prettyS js
   where
    f _ [] = ""
    f j xs = "[| " ++ unwords (map prettyS (take j xs)) ++ " |] " ++ f j (drop j xs)
  prettyS (TensorData (Tensor ns xs js)) = "(tensor {" ++ unwords (map show ns) ++ "} {" ++ unwords (map prettyS (V.toList xs)) ++ "} )" ++ concatMap prettyS js
  prettyS (Float x) = show x
  prettyS (InductiveData name vals) = "<" ++ name ++ concatMap ((' ':) . prettyS) vals ++ ">"
  prettyS (Tuple vals)      = "[" ++ unwords (map prettyS vals) ++ "]"
  prettyS (Collection vals) = "{" ++ unwords (map prettyS (toList vals)) ++ "}"
  prettyS (Array vals)      = "(|" ++ unwords (map prettyS $ Array.elems vals) ++ "|)"
  prettyS (IntHash hash)    = "{|" ++ unwords (map (\(key, val) -> "[" ++ show key ++ " " ++ prettyS val ++ "]") $ HashMap.toList hash) ++ "|}"
  prettyS (CharHash hash)   = "{|" ++ unwords (map (\(key, val) -> "[" ++ show key ++ " " ++ prettyS val ++ "]") $ HashMap.toList hash) ++ "|}"
  prettyS (StrHash hash)    = "{|" ++ unwords (map (\(key, val) -> "[" ++ show key ++ " " ++ prettyS val ++ "]") $ HashMap.toList hash) ++ "|}"
  prettyS UserMatcher{} = "#<user-matcher>"
  prettyS (Func Nothing _ args _) = "(lambda [" ++ unwords (map ('$':) args) ++ "] ...)"
  prettyS (Func (Just name) _ _ _) = prettyS name
  prettyS (PartialFunc _ n expr) = show n ++ "#" ++ prettyS expr
  prettyS (CFunc Nothing _ name _) = "(cambda " ++ name ++ " ...)"
  prettyS (CFunc (Just name) _ _ _) = prettyS name
  prettyS (MemoizedFunc Nothing _ _ _ names _) = "(memoized-lambda [" ++ unwords names ++ "] ...)"
  prettyS (MemoizedFunc (Just name) _ _ _ _ _) = prettyS name
  prettyS (Proc Nothing _ names _) = "(procedure [" ++ unwords names ++ "] ...)"
  prettyS (Proc (Just name) _ _ _) = name
  prettyS PatternFunc{} = "#<pattern-function>"
  prettyS (PrimitiveFunc name _) = "#<primitive-function " ++ name ++ ">"
  prettyS (IOFunc _) = "#<io-function>"
  prettyS (Port _) = "#<port>"
  prettyS Something = "something"
  prettyS Undefined = "undefined"
  prettyS World = "#<world>"
  prettyS EOF = "#<eof>"

instance PrettyS Var where
  prettyS = show

instance PrettyS VarWithIndices where
  prettyS = show

instance PrettyS EgisonBinOp where
  prettyS = repr

instance PrettyS InnerExpr where
  prettyS (ElementExpr e) = prettyS e
  prettyS (SubCollectionExpr e) = '@' : prettyS e

instance PrettyS Arg where
  prettyS (ScalarArg name)         = "$" ++ name
  prettyS (InvertedScalarArg name) = "*$" ++ name
  prettyS (TensorArg name)         = "%" ++ name

instance PrettyS ScalarData where
  prettyS (Div p1 (Plus [Term 1 []])) = prettyS p1
  prettyS (Div p1 p2)                 = "(/ " ++ prettyS p1 ++ " " ++ prettyS p2 ++ ")"

instance PrettyS PolyExpr where
  prettyS (Plus [])  = "0"
  prettyS (Plus [t]) = prettyS t
  prettyS (Plus ts)  = "(+ " ++ unwords (map prettyS ts)  ++ ")"

instance PrettyS TermExpr where
  prettyS (Term a []) = show a
  prettyS (Term 1 [x]) = showPoweredSymbol x
  prettyS (Term 1 xs) = "(* " ++ unwords (map showPoweredSymbol xs) ++ ")"
  prettyS (Term a xs) = "(* " ++ show a ++ " " ++ unwords (map showPoweredSymbol xs) ++ ")"

showPoweredSymbol :: (SymbolExpr, Integer) -> String
showPoweredSymbol (x, 1) = prettyS x
showPoweredSymbol (x, n) = prettyS x ++ "^" ++ show n

instance PrettyS SymbolExpr where
  prettyS (Symbol _ (':':':':':':_) []) = "#"
  prettyS (Symbol _ s []) = s
  prettyS (Symbol _ s js) = s ++ concatMap prettyS js
  prettyS (Apply fn mExprs) = "(" ++ prettyS fn ++ " " ++ unwords (map prettyS mExprs) ++ ")"
  prettyS (Quote mExprs) = "'" ++ prettyS mExprs
  prettyS (FunctionData name _ _ js) = show name ++ concatMap prettyS js

showTSV :: EgisonValue -> String
showTSV (Tuple (val:vals)) = foldl (\r x -> r ++ "\t" ++ x) (prettyS val) (map prettyS vals)
showTSV (Collection vals) = intercalate "\t" (map prettyS (toList vals))
showTSV val = prettyS val

instance PrettyS a => PrettyS (Index a) where
  prettyS (Subscript i)    = "_" ++ prettyS i
  prettyS (Superscript i)  = "~" ++ prettyS i
  prettyS (SupSubscript i) = "~_" ++ prettyS i
  prettyS (MultiSubscript x y) = "_" ++ prettyS x ++ "..._" ++ prettyS y
  prettyS (MultiSuperscript x y) = "~" ++ prettyS x ++ "...~" ++ prettyS y
  prettyS (DFscript _ _)   = ""
  prettyS (Userscript i)   = "|" ++ prettyS i

instance {-# OVERLAPPING #-} PrettyS (Index EgisonValue) where
  prettyS (Superscript i) = case i of
    ScalarData (Div (Plus [Term 1 [(Symbol _ _ (_:_), 1)]]) (Plus [Term 1 []])) -> "~[" ++ prettyS i ++ "]"
    _ -> "~" ++ prettyS i
  prettyS (Subscript i) = case i of
    ScalarData (Div (Plus [Term 1 [(Symbol _ _ (_:_), 1)]]) (Plus [Term 1 []])) -> "_[" ++ prettyS i ++ "]"
    _ -> "_" ++ prettyS i
  prettyS (SupSubscript i) = "~_" ++ prettyS i
  prettyS (DFscript i j) = "_d" ++ show i ++ show j
  prettyS (Userscript i) = case i of
    ScalarData (Div (Plus [Term 1 [(Symbol _ _ (_:_), 1)]]) (Plus [Term 1 []])) -> "_[" ++ prettyS i ++ "]"
    _ -> "|" ++ prettyS i

instance {-# OVERLAPPING #-} PrettyS EgisonPattern where
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

instance PrettyS LoopRange where
  prettyS (LoopRange start (ApplyExpr (VarExpr (Var ["from"] [])) (ApplyExpr _ (TupleExpr (x:_)))) endPat) =
    "[" ++ show start ++ " (from " ++ show x ++ ") " ++ prettyS endPat ++ "]"
  prettyS (LoopRange start ends endPat) = "[" ++ show start ++ " " ++ show ends ++ " " ++ prettyS endPat ++ "]"
