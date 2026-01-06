{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans        #-}

{- |
Module      : Language.Egison.PrettyPrint
Licence     : MIT

This module contains pretty printing for Egison syntax
-}

module Language.Egison.Pretty
    ( prettyTopExprs
    , prettyStr
    , showTSV
    ) where

import           Data.Foldable                  (toList)
import           Data.List                      (intercalate)
import           Prettyprinter
import           Prettyprinter.Render.String    (renderString)
import           Text.Show.Unicode              (ushow)

import           Language.Egison.AST
import           Language.Egison.Data
import           Language.Egison.IExpr
import qualified Language.Egison.Type.Types as Types

--
-- Pretty printing for Non-S syntax
--

prettyTopExprs :: [TopExpr] -> Doc [TopExpr]
prettyTopExprs exprs = vsep $ punctuate line (map pretty exprs)

instance Pretty TopExpr where
  pretty (Define x (LambdaExpr args body)) =
    hsep (pretty "def" : pretty x : map pretty' args) <+> indentBlock (pretty ":=") [pretty body]
  pretty (Define x expr) =
    pretty "def" <+> pretty x <+> indentBlock (pretty ":=") [pretty expr]
  pretty (Test expr) = pretty expr
  pretty (LoadFile file) = pretty "loadFile" <+> pretty (show file)
  pretty (Load lib) = pretty "load" <+> pretty (show lib)
  pretty (PatternInductiveDecl typeName typeParams constructors) =
    let typeParamsDoc = if null typeParams then emptyDoc else hsep (map pretty typeParams)
        constructorsDoc = vsep $ map (\(PatternConstructor name args) ->
          pretty "|" <+> pretty name <+> hsep (map pretty args)) constructors
    in pretty "inductive" <+> pretty "pattern" <+> pretty typeName <+> typeParamsDoc <+> 
       pretty ":=" <+> constructorsDoc
  pretty (PatternFunctionDecl name typeParams params retType body) =
    let typeParamsDoc = if null typeParams then emptyDoc 
                        else braces (hsep $ punctuate (pretty ",") (map pretty typeParams))
        paramsDoc = hsep $ map (\(pname, ptype) -> 
          parens (pretty pname <+> pretty ":" <+> pretty ptype)) params
    in pretty "def" <+> pretty "pattern" <+> pretty name <+> typeParamsDoc <+> 
       paramsDoc <+> pretty ":" <+> pretty retType <+> pretty ":=" <+> pretty body
  pretty _ = error "Unsupported topexpr"

instance Pretty ConstantExpr where
  pretty (CharExpr x)    = viaShow x
  pretty (StringExpr x)  = pretty (ushow x)
  pretty (BoolExpr x)    = pretty x
  pretty (IntegerExpr x) = pretty x
  pretty (FloatExpr x)   = pretty x
  pretty SomethingExpr   = pretty "something"
  pretty UndefinedExpr   = pretty "undefined"

instance Pretty Expr where
  pretty (ConstantExpr c) = pretty c
  -- Use |viaShow| to correctly handle escaped characters
  pretty (VarExpr x)     = pretty x
  pretty FreshVarExpr    = pretty "#"
  pretty (IndexedExpr True e indices) = pretty' e <> cat (map pretty indices)
  pretty (IndexedExpr False e indices) = pretty' e <> pretty "..." <> cat (map pretty indices)
  pretty (SubrefsExpr b e1 e2) =
    applyLike [pretty "subrefs" <> (if b then pretty "!" else emptyDoc),
               pretty' e1, pretty' e2]
  pretty (SuprefsExpr b e1 e2) =
    applyLike [pretty "suprefs" <> (if b then pretty "!" else emptyDoc),
               pretty' e1, pretty' e2]
  pretty (UserrefsExpr b e1 e2) =
    applyLike [pretty "userRefs" <> (if b then pretty "!" else emptyDoc),
               pretty' e1, pretty' e2]

  pretty (TupleExpr xs) = tupled (map pretty xs)
  pretty (CollectionExpr xs)
    | length xs < 20 = list (map pretty xs)
    | otherwise      =
      pretty "[" <> align (fillSepAtom (punctuate comma (map pretty xs))) <> pretty "]"
  pretty (HashExpr xs)   = listoid "{|" "|}" (map (\(x, y) -> tupled [pretty x, pretty y]) xs)
  pretty (VectorExpr xs) = listoid "[|" "|]" (map pretty xs)

  pretty (LambdaExpr xs e) =
    lambdaLike (pretty "\\") (map pretty xs) (pretty "->") (pretty e)
  pretty (MemoizedLambdaExpr xs e)  =
    lambdaLike (pretty "memoizedLambda ") (map pretty xs) (pretty "->") (pretty e)
  pretty (TypedMemoizedLambdaExpr params retType e) =
    lambdaLike (pretty "memoizedLambda ") (map pretty params) (pretty ":" <+> pretty retType <+> pretty "->") (pretty e)
  pretty (CambdaExpr x e) =
    indentBlock (pretty "cambda" <+> pretty x <+> pretty "->") [pretty e]
  pretty (PatternFunctionExpr xs p) =
    lambdaLike (pretty "\\") (map pretty xs) (pretty "=>") (pretty p)

  pretty (IfExpr x y z) =
    indentBlock (pretty "if" <+> pretty x)
      [pretty "then" <+> pretty y, pretty "else" <+> pretty z]
  pretty (LetRecExpr bindings body) =
    hang 1 (pretty "let" <+> align (vsep (map pretty bindings)) <> hardline <> pretty "in" <+> align (pretty body))
  pretty (WithSymbolsExpr xs e) =
    indentBlock (pretty "withSymbols" <+> list (map pretty xs)) [pretty e]

  pretty (MatchExpr BFSMode tgt matcher clauses) =
    nest 2 (pretty "match"       <+> pretty tgt <+> prettyMatch matcher clauses)
  pretty (MatchExpr DFSMode tgt matcher clauses) =
    nest 2 (pretty "matchDFS"    <+> pretty tgt <+> prettyMatch matcher clauses)
  pretty (MatchAllExpr BFSMode tgt matcher clauses) =
    nest 2 (pretty "matchAll"    <+> pretty tgt <+> prettyMatch matcher clauses)
  pretty (MatchAllExpr DFSMode tgt matcher clauses) =
    nest 2 (pretty "matchAllDFS" <+> pretty tgt <+> prettyMatch matcher clauses)
  pretty (MatchLambdaExpr matcher clauses) =
    nest 2 (pretty "\\match"     <+> prettyMatch matcher clauses)
  pretty (MatchAllLambdaExpr matcher clauses) =
    nest 2 (pretty "\\matchAll"  <+> prettyMatch matcher clauses)
  pretty (MatcherExpr patDefs) =
    nest 2 (pretty "matcher" <> hardline <> align (vsep (map prettyPatDef patDefs)))
      where
        prettyPatDef (PatternDef pppat expr body) =
          nest 2 (pipe <+> pretty pppat <+> pretty "as" <+>
            group (pretty expr) <+> pretty "with" <> hardline <>
              align (vsep (map prettyPatBody body)))
        prettyPatBody (pdpat, expr) =
          indentBlock (pipe <+> align (pretty pdpat) <+> pretty "->") [pretty expr]

  pretty (AlgebraicDataMatcherExpr patDefs) =
    nest 2 (pretty "algebraicDataMatcher" <> hardline <> align (vsep (map prettyPatDef patDefs)))
      where
        prettyPatDef (name, exprs) = pipe <+> hsep (pretty name : map pretty exprs)

  pretty (QuoteExpr e) = squote <> pretty' e
  pretty (QuoteSymbolExpr e) = pretty '`' <> pretty' e

  pretty (PrefixExpr op x@(ConstantExpr (IntegerExpr _))) = pretty op <> pretty x
  pretty (PrefixExpr op x)
    | isAtomOrApp x = pretty op <+> pretty x
    | otherwise     = pretty op <+> parens (pretty x)
  -- (x1 op' x2) op y
  pretty (InfixExpr op x@(InfixExpr op' _ _) y) =
    if priority op > priority op' || priority op == priority op' && assoc op == InfixR
       then parens (pretty x) <+> pretty op <> infixRight (pretty'' y)
       else pretty x          <+> pretty op <> infixRight (pretty'' y)
  -- x op (y1 op' y2)
  pretty (InfixExpr op x y@(InfixExpr op' _ _)) =
    if priority op > priority op' || priority op == priority op' && assoc op == InfixL
       then pretty'' x <+> pretty op <> infixRight (parens (pretty y))
       else pretty'' x <+> pretty op <> infixRight (pretty y)
  pretty (InfixExpr op x y) =
    pretty'' x <+> pretty op <> infixRight (pretty'' y)
  pretty (SectionExpr op Nothing Nothing) = parens (pretty op)
  pretty (SectionExpr op (Just x) Nothing) = parens (pretty x <+> pretty op)
  pretty (SectionExpr op Nothing (Just x)) = parens (pretty op <+> pretty x)

  pretty (DoExpr [] y) = pretty "do" <+> pretty y
  pretty (DoExpr xs (ApplyExpr (VarExpr "return") [])) =
    pretty "do" <+> align (hsepHard (map prettyDoBinds xs))
  pretty (DoExpr xs y) = pretty "do" <+> align (hsepHard (map prettyDoBinds xs ++ [pretty y]))

  pretty (SeqExpr e1 e2) = applyLike [pretty "seq", pretty' e1, pretty' e2]
  pretty (ApplyExpr x ys) = applyLike (map pretty' (x : ys))
  pretty (CApplyExpr e1 e2) = applyLike [pretty "capply", pretty' e1, pretty' e2]
  pretty (AnonParamFuncExpr n e) = pretty n <> pretty '#' <> pretty' e
  pretty (AnonParamExpr n) = pretty '%' <> pretty n

  pretty (GenerateTensorExpr gen shape) =
    applyLike [pretty "generateTensor", pretty' gen, pretty' shape]
  pretty (TensorExpr e1 e2) =
    applyLike [pretty "tensor", pretty' e1, pretty' e2]
  pretty (TensorContractExpr e1) =
    applyLike [pretty "contract", pretty' e1]
  pretty (TensorMapExpr e1 e2) =
    applyLike [pretty "tensorMap", pretty' e1, pretty' e2]
  pretty (TensorMap2Expr e1 e2 e3) =
    applyLike [pretty "tensorMap2", pretty' e1, pretty' e2, pretty' e3]
  pretty (TransposeExpr e1 e2) =
    applyLike [pretty "transpose", pretty' e1, pretty' e2]
  pretty (FlipIndicesExpr _) = error "unreachable"

  pretty (FunctionExpr xs) = pretty "function" <+> tupled (map pretty xs)

  pretty p = pretty (show p)

instance (Pretty a, Complex a) => Pretty (Arg a) where
  pretty (Arg x)         = pretty x
  pretty (InvertedArg x) = pretty "!" <> pretty' x

instance Pretty ArgPattern where
  pretty APWildCard              = pretty "_"
  pretty (APPatVar x)            = pretty x
  pretty (APInductivePat x args) = applyLike (pretty x : map pretty' args)
  pretty (APTuplePat args)       = tupled (map pretty args)
  pretty APEmptyPat              = pretty "[]"
  pretty (APConsPat arg1 arg2)   = pretty'' arg1 <+> pretty "::" <+> pretty'' arg2
  pretty (APSnocPat arg1 arg2)   = pretty'' arg1 <+> pretty "*:" <+> pretty' arg2

instance Pretty VarWithIndices where
  pretty (VarWithIndices xs is) = pretty xs <> hcat (map pretty is)

instance Pretty VarIndex where
  pretty (VSubscript x)        = pretty ('_' : x)
  pretty (VSuperscript x)      = pretty ('~' : x)
  pretty (VSymmScripts xs)     = pretty '{' <> hcat (map pretty xs) <> pretty '}'
  pretty (VAntiSymmScripts xs) = pretty '[' <> hcat (map pretty xs) <> pretty ']'

instance Pretty BindingExpr where
  pretty (Bind (PDPatVar f) (LambdaExpr args body)) =
    hsep (pretty f : map pretty' args) <+> indentBlock (pretty ":=") [pretty body]
  pretty (Bind pat expr) = pretty pat <+> pretty ":=" <+> align (pretty expr)
  pretty (BindWithIndices var expr) = pretty var <+> pretty ":=" <+> align (pretty expr)
  pretty (BindWithType typedVar expr) =
    let constraints = typedVarConstraints typedVar
        constraintsDoc = if null constraints
                         then mempty
                         else pretty "{" <> hsep (punctuate (pretty ",") (map pretty constraints)) <> pretty "}" <> space
    in hsep (pretty (typedVarName typedVar) : [constraintsDoc | not (null constraints)] ++ map pretty (typedVarParams typedVar)) <+>
       pretty ":" <+> pretty (typedVarRetType typedVar) <+> pretty ":=" <+> align (pretty expr)

instance Pretty TypedParam where
  pretty (TPVar name ty) = parens (pretty name <+> pretty ":" <+> pretty ty)
  pretty (TPInvertedVar name ty) = parens (pretty "!" <+> pretty name <+> pretty ":" <+> pretty ty)
  pretty (TPTuple elems) = parens (hsep (punctuate comma (map pretty elems)))
  pretty (TPWildcard ty) = parens (pretty "_" <+> pretty ":" <+> pretty ty)
  pretty (TPUntypedVar name) = pretty name
  pretty TPUntypedWildcard = pretty "_"

instance Pretty TypeExpr where
  pretty TEInt = pretty "Integer"
  pretty TEMathExpr = pretty "MathExpr"
  pretty TEFloat = pretty "Float"
  pretty TEBool = pretty "Bool"
  pretty TEChar = pretty "Char"
  pretty TEString = pretty "String"
  pretty (TEVar v) = pretty v
  pretty (TEList t) = brackets (pretty t)
  pretty (TETuple []) = pretty "()"
  pretty (TETuple ts) = parens (hsep (punctuate comma (map pretty ts)))
  pretty (TEFun t1 t2) = pretty t1 <+> pretty "->" <+> pretty t2
  pretty (TEMatcher t) = pretty "Matcher" <+> pretty t
  pretty (TEPattern t) = pretty "Pattern" <+> pretty t
  pretty (TEIO t) = pretty "IO" <+> pretty t
  pretty (TETensor t _ _) = pretty "Tensor" <+> pretty t
  pretty (TEApp t args) = hsep (pretty t : map pretty args)

instance Pretty ConstraintExpr where
  pretty (ConstraintExpr cls types) = hsep (pretty cls : map pretty types)

instance {-# OVERLAPPING #-} Pretty MatchClause where
  pretty (pat, expr) =
    pipe <+> align (pretty pat) <+> indentBlock (pretty "->") [pretty expr]

instance {-# OVERLAPPING #-} Pretty (IndexExpr String) where -- for 'VarWithIndices'
  pretty (Superscript s)  = pretty ("~" ++ s)
  pretty (Subscript s)    = pretty ("_" ++ s)
  pretty (SupSubscript s) = pretty ("~_" ++ s)
  pretty (Userscript i)   = pretty ("|" ++ show i)
  pretty _                = undefined

instance (Pretty a, Complex a) => Pretty (IndexExpr a) where
  pretty (Subscript i)          = pretty '_' <> pretty' i
  pretty (Superscript i)        = pretty '~' <> pretty' i
  pretty (SupSubscript i)       = pretty "~_" <> pretty' i
  pretty (MultiSubscript i j)   = pretty '_' <> pretty' i <> pretty "..._" <> pretty' j
  pretty (MultiSuperscript i j) = pretty '~' <> pretty' i <> pretty "...~" <> pretty' j
  pretty (Userscript i)         = pretty '|' <> pretty' i

instance Pretty Pattern where
  pretty WildCard     = pretty "_"
  pretty (PatVar x)   = pretty "$" <> pretty x
  pretty (ValuePat v) = pretty "#" <> pretty' v
  pretty (PredPat v)  = pretty "?" <> pretty' v
  pretty (IndexedPat p indices) =
    pretty p <> hcat (map (\i -> pretty '_' <> pretty' i) indices)
  pretty (LetPat binds pat) =
    pretty "let" <+> align (vsep (map pretty binds)) <+> pretty "in" <+> pretty pat
  -- (p11 op' p12) op p2
  pretty (InfixPat op p1@(InfixPat op' _ _) p2) =
    if priority op > priority op' || priority op == priority op' && assoc op == InfixR
       then parens (pretty p1) <+> pretty (repr op) <+> pretty'' p2
       else pretty p1          <+> pretty (repr op) <+> pretty'' p2
  -- p1 op (p21 op' p22)
  pretty (InfixPat op p1 p2@(InfixPat op' _ _)) =
    if priority op > priority op' || priority op == priority op' && assoc op == InfixL
       then pretty'' p1 <+> pretty (repr op) <+> parens (pretty p2)
       else pretty'' p1 <+> pretty (repr op) <+> pretty p2
  pretty (InfixPat op p1 p2) = pretty'' p1 <+> pretty (repr op) <+> pretty'' p2
  pretty (NotPat pat) = pretty "!" <> pretty' pat
  pretty (TuplePat pats) = tupled $ map pretty pats
  pretty (InductivePat "nil" []) = pretty "[]"
  pretty (InductivePat "::" [p, InductivePat "nil" []]) = pretty "[" <> pretty p <> pretty "]"
  pretty (InductivePat ctor xs) = hsep (pretty ctor : map pretty' xs)
  pretty (LoopPat i range p1 p2) =
    hang 2 (pretty "loop" <+> pretty '$' <> pretty i <+> pretty range <>
      flatAlt (hardline <> group (pretty' p1) <> hardline <> group (pretty' p2))
              (space <> pretty' p1 <+> pretty' p2))
  pretty ContPat = pretty "..."
  pretty (PApplyPat fn ps) = applyLike (pretty' fn : map pretty' ps)
  pretty (VarPat x) = pretty ('~' : x)
  pretty SeqNilPat = pretty "{}"
  pretty (SeqConsPat p1 p2) = listoid "{" "}" (f p1 p2)
    where
      f p1 SeqNilPat          = [pretty p1]
      f p1 (SeqConsPat p2 p3) = pretty p1 : f p2 p3
      f p1 p2                 = [pretty p1, pretty p2]
  pretty LaterPatVar = pretty "@"
  pretty (DApplyPat p ps) = applyLike (map pretty' (p : ps))
  pretty e            = pretty (show e)

instance {-# OVERLAPPING #-} Pretty LoopRange where
  pretty (LoopRange from (ApplyExpr (VarExpr "from")
                                    [InfixExpr Op{ repr = "-'" } _ (ConstantExpr (IntegerExpr 1))]) pat) =
    tupled [pretty from, pretty pat]
  pretty (LoopRange from to pat) = tupled [pretty from, pretty to, pretty pat]

instance Pretty PrimitivePatPattern where
  pretty PPWildCard                = pretty "_"
  pretty PPPatVar                  = pretty "$"
  pretty (PPValuePat x)          = pretty ('#' : '$' : x)
  pretty (PPInductivePat x pppats) = hsep (pretty x : map pretty pppats)
  pretty (PPTuplePat pppats)       = tupled (map pretty pppats)

instance Pretty PrimitiveDataPattern where
  pretty PDWildCard                = pretty "_"
  pretty (PDPatVar x)              = pretty x
  pretty (PDInductivePat x pdpats) = applyLike (pretty x : map pretty' pdpats)
  pretty (PDTuplePat pdpats)       = tupled (map pretty pdpats)
  pretty PDEmptyPat                = pretty "[]"
  pretty (PDConsPat pdp1 pdp2)     = pretty'' pdp1 <+> pretty "::" <+> pretty'' pdp2
  pretty (PDSnocPat pdp1 pdp2)     = pretty'' pdp1 <+> pretty "*:" <+> pretty' pdp2
  pretty (PDConstantPat expr)      = pretty expr

instance Pretty Op where
  pretty op | isWedge op = pretty ("!" ++ repr op)
            | otherwise  = pretty (repr op)

instance Pretty IExpr where
  pretty (IConstantExpr c) = pretty c
  pretty (IVarExpr name) = pretty name
  
  pretty (IIndexedExpr override expr indices) =
    pretty' expr <> (if override then pretty "..." else emptyDoc) <> hcat (map prettyIndex indices)
    where
      prettyIndex (Sub e) = pretty "_" <> prettyIndexExpr e
      prettyIndex (Sup e) = pretty "~" <> prettyIndexExpr e
      prettyIndex (SupSub e) = pretty "~_" <> prettyIndexExpr e
      prettyIndex (User e) = pretty "|" <> prettyIndexExpr e
      prettyIndex (DF _ _) = emptyDoc
      prettyIndex (MultiSub _ _ _) = pretty "_..."
      prettyIndex (MultiSup _ _ _) = pretty "~..."
      prettyIndexExpr e = if isAtom e then pretty e else parens (pretty e)
  
  pretty (ISubrefsExpr override expr subExpr) =
    pretty' expr <> (if override then pretty "..." else emptyDoc) <> pretty "._" <> prettyRefExpr subExpr
  pretty (ISuprefsExpr override expr supExpr) =
    pretty' expr <> (if override then pretty "..." else emptyDoc) <> pretty ".~" <> prettyRefExpr supExpr
  pretty (IUserrefsExpr override expr userExpr) =
    pretty' expr <> (if override then pretty "..." else emptyDoc) <> pretty ".|" <> prettyRefExpr userExpr
  
  pretty (IInductiveDataExpr name args)
    | null args = pretty name
    | otherwise = applyLike (pretty name : map pretty' args)
  
  pretty (ITupleExpr []) = pretty "[" <> pretty "]"
  pretty (ITupleExpr xs) = tupled (map pretty xs)
  
  pretty (ICollectionExpr xs) = list (map pretty xs)
  
  pretty (IConsExpr x xs) = pretty'' x <+> pretty "::" <+> pretty'' xs
  pretty (IJoinExpr x xs) = pretty'' x <+> pretty "++" <+> pretty'' xs
  
  pretty (IHashExpr pairs) = 
    pretty "{|" <+> hsep (punctuate comma (map prettyPair pairs)) <+> pretty "|}"
    where prettyPair (k, v) = parens (pretty k <> comma <+> pretty v)
  
  pretty (IVectorExpr xs) = 
    pretty "[|" <+> hsep (punctuate comma (map pretty xs)) <+> pretty "|]"
  
  pretty (ILambdaExpr _mVar params body) =
    lambdaLike (pretty "\\") (map prettyVar params) (pretty "->") (pretty body)
    where prettyVar (Var name []) = pretty name
          prettyVar v = pretty (show v)  -- fallback for complex vars
  
  pretty (IMemoizedLambdaExpr xs e) =
    lambdaLike (pretty "memoizedLambda") (map pretty xs) (pretty "->") (pretty e)
  
  pretty (ICambdaExpr x e) =
    indentBlock (pretty "cambda" <+> pretty x <+> pretty "->") [pretty e]
  
  pretty (IPatternFunctionExpr xs p) =
    lambdaLike (pretty "\\") (map pretty xs) (pretty "=>") (pretty p)
  
  pretty (IIfExpr cond thenE elseE) =
    indentBlock (pretty "if" <+> pretty cond)
      [pretty "then" <+> pretty thenE, pretty "else" <+> pretty elseE]
  
  pretty (ILetRecExpr bindings body) =
    hang 1 (pretty "let" <+> align (vsep (map prettyIBinding bindings)) <> hardline <> 
            pretty "in" <+> align (pretty body))
  
  pretty (ILetExpr bindings body) =
    hang 1 (pretty "let" <+> align (vsep (map prettyIBinding bindings)) <> hardline <> 
            pretty "in" <+> align (pretty body))
  
  pretty (IWithSymbolsExpr xs e) =
    indentBlock (pretty "withSymbols" <+> list (map pretty xs)) [pretty e]
  
  pretty (IMatchExpr BFSMode tgt matcher clauses) =
    nest 2 (pretty "match" <+> pretty tgt <+> prettyIMatch matcher clauses)
  pretty (IMatchExpr DFSMode tgt matcher clauses) =
    nest 2 (pretty "matchDFS" <+> pretty tgt <+> prettyIMatch matcher clauses)
  
  pretty (IMatchAllExpr BFSMode tgt matcher clauses) =
    nest 2 (pretty "matchAll" <+> pretty tgt <+> prettyIMatch matcher clauses)
  pretty (IMatchAllExpr DFSMode tgt matcher clauses) =
    nest 2 (pretty "matchAllDFS" <+> pretty tgt <+> prettyIMatch matcher clauses)
  
  pretty (IMatcherExpr patDefs) =
    nest 2 (pretty "matcher" <> hardline <> align (vsep (map prettyIPatDef patDefs)))
    where
      prettyIPatDef (pppat, expr, body) =
        nest 2 (pipe <+> pretty pppat <+> pretty "as" <+>
          group (pretty expr) <+> pretty "with" <> hardline <>
            align (vsep (map prettyIPatBody body)))
      prettyIPatBody (pdpat, expr) =
        indentBlock (pipe <+> align (pretty pdpat) <+> pretty "->") [pretty expr]
  
  pretty (IQuoteExpr e) = squote <> pretty' e
  pretty (IQuoteSymbolExpr e) = pretty '`' <> pretty' e
  
  pretty (IWedgeApplyExpr op args) = applyLike (pretty' op : map pretty' args)
  
  pretty (IDoExpr [] y) = pretty "do" <+> pretty y
  pretty (IDoExpr xs y) = pretty "do" <+> align (hsepHard (map prettyIDoBinds xs ++ [pretty y]))
  
  pretty (ISeqExpr e1 e2) = applyLike [pretty "seq", pretty' e1, pretty' e2]
  
  pretty (IApplyExpr fn args) = applyLike (map pretty' (fn : args))
  
  pretty (ICApplyExpr e1 e2) = applyLike [pretty "capply", pretty' e1, pretty' e2]
  
  pretty (IGenerateTensorExpr gen shape) =
    applyLike [pretty "generateTensor", pretty' gen, pretty' shape]
  
  pretty (ITensorExpr e1 e2) =
    applyLike [pretty "tensor", pretty' e1, pretty' e2]
  
  pretty (ITensorContractExpr e1) =
    applyLike [pretty "contract", pretty' e1]
  
  pretty (ITensorMapExpr e1 e2) =
    applyLike [pretty "tensorMap", pretty' e1, pretty' e2]
  
  pretty (ITensorMap2Expr e1 e2 e3) =
    applyLike [pretty "tensorMap2", pretty' e1, pretty' e2, pretty' e3]
  
  pretty (ITransposeExpr e1 e2) =
    applyLike [pretty "transpose", pretty' e1, pretty' e2]
  
  pretty (IFlipIndicesExpr e) =
    applyLike [pretty "flipIndices", pretty' e]
  
  pretty (IFunctionExpr xs) = pretty "function" <+> tupled (map pretty xs)

prettyRefExpr :: IExpr -> Doc ann
prettyRefExpr e = if isAtom e then pretty e else parens (pretty e)

prettyIBinding :: IBindingExpr -> Doc ann
prettyIBinding (pdpat, expr) = pretty pdpat <+> pretty ":=" <+> align (pretty expr)

prettyIDoBinds :: IBindingExpr -> Doc ann
prettyIDoBinds (pdpat, expr) = pretty pdpat <+> pretty "<-" <+> pretty expr

prettyIMatch :: IExpr -> [IMatchClause] -> Doc ann
prettyIMatch matcher clauses =
  pretty "as" <+> pretty matcher <+> pretty "with" <> hardline <>
    indent 2 (vsep (map prettyIClause clauses))
  where
    prettyIClause (pat, body) =
      indentBlock (pipe <+> pretty pat <+> pretty "->") [pretty body]

instance Complex IExpr where
  isAtom (IConstantExpr (IntegerExpr i)) | i < 0 = False
  isAtom (IConstantExpr _) = True
  isAtom (IVarExpr _) = True
  isAtom ITupleExpr{} = True
  isAtom ICollectionExpr{} = True
  isAtom IHashExpr{} = True
  isAtom IVectorExpr{} = True
  isAtom IMatcherExpr{} = True
  isAtom (IIndexedExpr _ e _) = isAtom e
  isAtom (IInductiveDataExpr _ []) = True
  isAtom _ = False
  
  isAtomOrApp (IApplyExpr _ _) = True
  isAtomOrApp (IInductiveDataExpr _ (_:_)) = True
  isAtomOrApp e = isAtom e
  
  isInfix _ = False  -- IExpr doesn't have infix expressions (they're desugared)

instance Pretty IPrimitiveDataPattern where
  pretty (PDPatVar (Var name [])) = pretty name
  pretty (PDPatVar var) = pretty (show var)
  pretty PDWildCard = pretty "_"
  pretty (PDInductivePat name []) = pretty name
  pretty (PDInductivePat name pats) = applyLike (pretty name : map pretty pats)
  pretty (PDTuplePat pats) = tupled (map pretty pats)
  pretty PDEmptyPat = pretty "[]"
  pretty (PDConsPat pat1 pat2) = pretty pat1 <+> pretty "::" <+> pretty pat2
  pretty (PDSnocPat pat1 pat2) = pretty pat1 <+> pretty "*:" <+> pretty pat2

instance Pretty IPattern where
  pretty IWildCard = pretty "_"
  pretty (IPatVar name) = pretty name
  pretty (IValuePat expr) = pretty "#" <> pretty' expr
  pretty (IPredPat expr) = pretty "?" <> pretty' expr
  pretty (IIndexedPat pat indices) =
    pretty' pat <> hcat (map prettyIndex indices)
    where
      prettyIndex e = if isAtom e then pretty e else parens (pretty e)
  pretty (ILetPat bindings pat) =
    pretty "let" <+> align (vsep (map prettyIBinding bindings)) <+> pretty "in" <+> pretty pat
  pretty (INotPat pat) = pretty "!" <> pretty' pat
  pretty (IAndPat pat1 pat2) = pretty' pat1 <+> pretty "&" <+> pretty' pat2
  pretty (IOrPat pat1 pat2) = pretty' pat1 <+> pretty "|" <+> pretty' pat2
  pretty (IForallPat var pat) = pretty "forall" <+> pretty var <+> pretty pat
  pretty (ITuplePat pats) = tupled (map pretty pats)
  pretty (IInductivePat name []) = pretty name
  pretty (IInductivePat name pats) = applyLike (pretty name : map pretty' pats)
  pretty (ILoopPat var (ILoopRange start end pat) bodyPat restPat) =
    pretty "loop" <+> pretty "$" <> pretty var <+>
    brackets (pretty start <> comma <+> pretty end <> comma <+> pretty pat) <+>
    pretty' bodyPat <+> pretty' restPat
  pretty IContPat = pretty "..."
  pretty (IPApplyPat expr pats) = applyLike (pretty' expr : map pretty' pats)
  pretty (IVarPat name) = pretty "$" <> pretty name
  pretty (IInductiveOrPApplyPat name pats)
    | null pats = pretty name
    | otherwise = applyLike (pretty name : map pretty' pats)
  pretty ISeqNilPat = pretty "[]"
  pretty (ISeqConsPat pat1 pat2) = pretty' pat1 <+> pretty "::" <+> pretty' pat2
  pretty ILaterPatVar = pretty "$"
  pretty (IDApplyPat pat pats) = applyLike (pretty' pat : map pretty' pats)

instance Complex IPattern where
  isAtom IWildCard = True
  isAtom (IPatVar _) = True
  isAtom (ITuplePat _) = True
  isAtom (IInductivePat _ []) = True
  isAtom ISeqNilPat = True
  isAtom _ = False
  
  isAtomOrApp (IPApplyPat _ _) = True
  isAtomOrApp (IInductiveOrPApplyPat _ (_:_)) = True
  isAtomOrApp (IInductivePat _ (_:_)) = True
  isAtomOrApp pat = isAtom pat
  
  isInfix _ = False

-- Pretty print for ITopExpr
instance Pretty ITopExpr where
  pretty (IDefine var iexpr) =
    pretty "def" <+> prettyVar var <+> indentBlock (pretty ":=") [pretty iexpr]
  pretty (IDefineMany bindings) =
    vsep (map prettyDefineMany bindings)
    where
      prettyDefineMany (var, iexpr) =
        pretty "def" <+> prettyVar var <+> pretty ":=" <+> pretty iexpr
  pretty (ITest iexpr) = 
    pretty iexpr
  pretty (IExecute iexpr) =
    pretty "execute" <+> pretty iexpr
  pretty (ILoadFile path) =
    pretty "loadFile" <+> pretty (show path)
  pretty (ILoad lib) =
    pretty "load" <+> pretty (show lib)

-- Pretty print for TIExpr and TITopExpr
instance Pretty TIExpr where
  pretty (TIExpr ty iexpr) = 
    parens (pretty iexpr <+> pretty ":" <+> prettyTypeDoc ty)

instance Pretty TITopExpr where
  pretty (TIDefine ty var tiexpr) =
    pretty "def" <+> prettyVar var <+> pretty ":" <+> prettyTypeDoc ty <+> 
    indentBlock (pretty ":=") [pretty tiexpr]
  pretty (TITest tiexpr) = 
    pretty tiexpr
  pretty (TIExecute tiexpr) =
    pretty "execute" <+> pretty tiexpr
  pretty (TILoadFile path) =
    pretty "loadFile" <+> pretty (show path)
  pretty (TILoad lib) =
    pretty "load" <+> pretty (show lib)
  pretty (TIDefineMany bindings) =
    vsep (map prettyBinding bindings)
    where
      prettyBinding (var, tiexpr) =
        prettyVar var <+> pretty ":=" <+> pretty tiexpr

-- Helper function to pretty print Var
prettyVar :: Var -> Doc ann
prettyVar (Var name []) = pretty name
prettyVar (Var name indices) = pretty name <> hcat (map prettyVarIndex indices)
  where
    prettyVarIndex (Sub Nothing) = pretty "_"
    prettyVarIndex (Sub (Just v)) = pretty "_" <> prettyVar v
    prettyVarIndex (Sup Nothing) = pretty "~"
    prettyVarIndex (Sup (Just v)) = pretty "~" <> prettyVar v
    prettyVarIndex (SupSub Nothing) = pretty "~_"
    prettyVarIndex (SupSub (Just v)) = pretty "~_" <> prettyVar v
    prettyVarIndex (User Nothing) = pretty "|"
    prettyVarIndex (User (Just v)) = pretty "|" <> prettyVar v
    prettyVarIndex (MultiSub _ _ _) = pretty "_..."
    prettyVarIndex (MultiSup _ _ _) = pretty "~..."
    prettyVarIndex (DF _ _) = emptyDoc

-- Helper function to pretty print Type as Doc
prettyTypeDoc :: Types.Type -> Doc ann
prettyTypeDoc Types.TInt = pretty "Integer"
prettyTypeDoc Types.TFloat = pretty "Float"
prettyTypeDoc Types.TBool = pretty "Bool"
prettyTypeDoc Types.TChar = pretty "Char"
prettyTypeDoc Types.TString = pretty "String"
prettyTypeDoc (Types.TVar (Types.TyVar v)) = pretty v
prettyTypeDoc (Types.TFun t1 t2) = prettyTypeDoc t1 <+> pretty "->" <+> prettyTypeDoc t2
prettyTypeDoc (Types.TTuple ts) = tupled (map prettyTypeDoc ts)
prettyTypeDoc (Types.TCollection t) = brackets (prettyTypeDoc t)
prettyTypeDoc (Types.THash k v) = 
  pretty "Hash" <+> prettyTypeDoc k <+> prettyTypeDoc v
prettyTypeDoc (Types.TMatcher t) = pretty "Matcher" <+> prettyTypeDoc t
prettyTypeDoc (Types.TIO t) = pretty "IO" <+> prettyTypeDoc t
prettyTypeDoc (Types.TIORef t) = pretty "IORef" <+> prettyTypeDoc t
prettyTypeDoc (Types.TTensor t) = pretty "Tensor" <+> prettyTypeDoc t
prettyTypeDoc (Types.TInductive name []) = pretty name
prettyTypeDoc (Types.TInductive name ts) = hsep (pretty name : map prettyTypeDoc ts)
prettyTypeDoc Types.TAny = pretty "_"

class Complex a where
  isAtom :: a -> Bool
  isAtomOrApp :: a -> Bool
  isInfix :: a -> Bool

instance Complex Expr where
  isAtom (ConstantExpr (IntegerExpr i)) | i < 0  = False
  isAtom PrefixExpr{}             = False
  isAtom InfixExpr{}              = False
  isAtom (ApplyExpr _ [])         = True
  isAtom ApplyExpr{}              = False
  isAtom CApplyExpr{}             = False
  isAtom LambdaExpr{}             = False
  isAtom MemoizedLambdaExpr{}     = False
  isAtom TypedMemoizedLambdaExpr{} = False
  isAtom CambdaExpr{}             = False
  isAtom PatternFunctionExpr{}    = False
  isAtom IfExpr{}                 = False
  isAtom LetRecExpr{}             = False
  isAtom SubrefsExpr{}            = False
  isAtom SuprefsExpr{}            = False
  isAtom UserrefsExpr{}           = False
  isAtom WithSymbolsExpr{}        = False
  isAtom MatchExpr{}              = False
  isAtom MatchAllExpr{}           = False
  isAtom MatchLambdaExpr{}        = False
  isAtom MatchAllLambdaExpr{}     = False
  isAtom MatcherExpr{}            = False
  isAtom AlgebraicDataMatcherExpr{} = False
  isAtom GenerateTensorExpr{}     = False
  isAtom TensorExpr{}             = False
  isAtom FunctionExpr{}           = False
  isAtom TensorContractExpr{}     = False
  isAtom TensorMapExpr{}          = False
  isAtom TensorMap2Expr{}         = False
  isAtom TransposeExpr{}          = False
  isAtom _                        = True

  isAtomOrApp ApplyExpr{} = True
  isAtomOrApp e           = isAtom e

  isInfix InfixExpr{} = True
  isInfix _           = False

instance Complex a => Complex (Arg a) where
  isAtom (Arg x) = isAtom x
  isAtom _       = True

  isAtomOrApp = isAtom

  isInfix _ = False

instance Complex ArgPattern where
  isAtom (APInductivePat _ []) = True
  isAtom APInductivePat{}      = False
  isAtom APConsPat{}           = False
  isAtom APSnocPat{}           = False
  isAtom _                     = True

  isAtomOrApp = isAtom
  isInfix _ = False

instance Complex Pattern where
  isAtom LetPat{}            = False
  isAtom (InductivePat _ []) = True
  isAtom (InductivePat _ _)  = False
  isAtom InfixPat{}          = False
  isAtom LoopPat{}           = False
  isAtom (PApplyPat _ [])    = True
  isAtom (PApplyPat _ _)     = False
  isAtom _                   = True

  isAtomOrApp PApplyPat{}    = True
  isAtomOrApp InductivePat{} = True
  isAtomOrApp e              = isAtom e

  isInfix InfixPat{} = True
  isInfix _          = False

instance Complex PrimitiveDataPattern where
  isAtom (PDInductivePat _ []) = True
  isAtom (PDInductivePat _ _)  = False
  isAtom PDConsPat{}           = False
  isAtom PDSnocPat{}           = False
  isAtom _                     = True

  isAtomOrApp PDInductivePat{} = True
  isAtomOrApp PDSnocPat{}      = True
  isAtomOrApp e                = isAtom e

  isInfix PDConsPat{} = True
  isInfix _           = False

pretty' :: (Pretty a, Complex a) => a -> Doc ann
pretty' x | isAtom x  = pretty x
          | otherwise = parens $ pretty x

pretty'' :: (Pretty a, Complex a) => a -> Doc ann
pretty'' x | isAtomOrApp x || isInfix x = pretty x
           | otherwise                  = parens $ pretty x

-- Display "hoge" instead of "() := hoge"
prettyDoBinds :: BindingExpr -> Doc ann
prettyDoBinds (Bind (PDTuplePat []) expr) = pretty expr
prettyDoBinds bind                        = pretty "let" <+> pretty bind

prettyMatch :: Expr -> [MatchClause] -> Doc ann
prettyMatch matcher clauses =
  pretty "as" <> group (flatAlt (hardline <> pretty matcher) (space <> pretty matcher) <+> pretty "with") <> hardline <>
    align (vsep (map pretty clauses))

listoid :: String -> String -> [Doc ann] -> Doc ann
listoid lp rp elems =
  encloseSep (pretty lp) (pretty rp) (comma <> space) elems

-- Just like |fillSep|, but does not break the atomicity of grouped Docs
fillSepAtom :: [Doc ann] -> Doc ann
fillSepAtom [] = emptyDoc
fillSepAtom [x] = x
fillSepAtom (x:xs) = x <> fillSepAtom' xs
  where
    fillSepAtom' [] = emptyDoc
    fillSepAtom' (x:xs) =
      group (flatAlt (hardline <> x) (space <> x)) <> fillSepAtom' xs

indentBlock :: Doc ann -> [Doc ann] -> Doc ann
indentBlock header bodies =
  group (nest 2 (header <> flatAlt (hardline <> hsepHard bodies) (space <> hsep bodies)))

hsepHard :: [Doc ann] -> Doc ann
hsepHard = concatWith (\x y -> x <> hardline <> y)

lambdaLike :: Doc ann -> [Doc ann] -> Doc ann -> Doc ann -> Doc ann
lambdaLike start [] arrow body =
  indentBlock (start <> pretty "()" <+> arrow) [body]
lambdaLike start args arrow body =
  indentBlock (start <> hsep args <+> arrow) [body]

applyLike :: [Doc ann] -> Doc ann
applyLike = hang 2 . sep . map group

-- Tests if the argument can be printed in a single line, and if not,
-- inserts a line break before printing it.
-- This is useful for nicely printing infix expressions.
infixRight :: Doc ann -> Doc ann
infixRight p = group (flatAlt (hardline <> p) (space <> p))

showTSV :: EgisonValue -> String
showTSV (Tuple (val:vals)) = foldl (\r x -> r ++ "\t" ++ x) (show val) (map show vals)
showTSV (Collection vals)  = intercalate "\t" (map show (toList vals))
showTSV val                = show val

--
-- Pretty printer for error messages
--

prettyStr :: Pretty a => a -> String
prettyStr = renderString . layoutPretty (LayoutOptions Unbounded) . pretty
