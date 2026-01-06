{- |
Module      : Language.Egison.PreDesugar
Licence     : MIT

This module implements syntactic desugaring BEFORE type inference.
These transformations are purely syntactic and do not require type information.

Pre-Type-Inference Desugaring:
  - Infix operator expansion (InfixExpr → ApplyExpr)
  - Other syntactic sugar that can be expanded before type inference

Design Note (design/implementation.md):
This phase is part of the "Desugar (構文糖衣展開)" step that occurs
BEFORE "型推論フェーズ". It transforms Expr to Expr, preserving the
AST structure while eliminating syntactic sugar.
-}

module Language.Egison.PreDesugar
  ( preDesugarExpr
  , preDesugarTopExpr
  ) where

import           Language.Egison.AST

-- | Pre-desugar an expression before type inference
-- Transforms: Expr → Expr
preDesugarExpr :: Expr -> Expr
preDesugarExpr expr = case expr of
  -- Infix expressions: transform to function application
  -- Example: x + y  →  (+) x y  →  ApplyExpr (VarExpr "+") [x, y]
  InfixExpr op e1 e2 ->
    let e1' = preDesugarExpr e1
        e2' = preDesugarExpr e2
    in ApplyExpr (VarExpr (repr op)) [e1', e2']
  
  -- Recursively process sub-expressions
  -- Constants and variables need no desugaring
  ConstantExpr c -> ConstantExpr c
  VarExpr name -> VarExpr name
  
  -- Indexed expressions
  IndexedExpr override base indices ->
    IndexedExpr override (preDesugarExpr base) (map preDesugarIndex indices)
  
  -- Tuples
  TupleExpr exprs ->
    TupleExpr (map preDesugarExpr exprs)
  
  -- Collections
  CollectionExpr exprs ->
    CollectionExpr (map preDesugarExpr exprs)
  
  -- Hash
  HashExpr pairs ->
    HashExpr [(preDesugarExpr k, preDesugarExpr v) | (k, v) <- pairs]
  
  -- Cons/Join
  ConsExpr h t ->
    ConsExpr (preDesugarExpr h) (preDesugarExpr t)
  JoinExpr l r ->
    JoinExpr (preDesugarExpr l) (preDesugarExpr r)
  
  -- Lambda expressions
  LambdaExpr args body ->
    LambdaExpr args (preDesugarExpr body)
  
  TypedLambdaExpr params retType body ->
    TypedLambdaExpr params retType (preDesugarExpr body)
  
  MemoizedLambdaExpr args body ->
    MemoizedLambdaExpr args (preDesugarExpr body)
  
  CambdaExpr name body ->
    CambdaExpr name (preDesugarExpr body)
  
  -- ProcedureExpr and MacroExpr don't exist in current AST
  -- Keeping for future compatibility
  
  PatternFunctionExpr params body ->
    PatternFunctionExpr params (preDesugarPattern body)
  
  -- Function application
  ApplyExpr func args ->
    ApplyExpr (preDesugarExpr func) (map preDesugarExpr args)
  
  CApplyExpr func arg ->
    CApplyExpr (preDesugarExpr func) (preDesugarExpr arg)
  
  -- Control flow
  IfExpr cond thenE elseE ->
    IfExpr (preDesugarExpr cond) (preDesugarExpr thenE) (preDesugarExpr elseE)
  
  -- Let expressions
  LetExpr bindings body ->
    LetExpr (map preDesugarBinding bindings) (preDesugarExpr body)
  
  LetRecExpr bindings body ->
    LetRecExpr (map preDesugarBinding bindings) (preDesugarExpr body)
  
  WithSymbolsExpr syms body ->
    WithSymbolsExpr syms (preDesugarExpr body)
  
  -- Do expression
  DoExpr bindings body ->
    DoExpr (map preDesugarBinding bindings) (preDesugarExpr body)
  
  -- Match expressions
  MatchExpr mode target matcher clauses ->
    MatchExpr mode (preDesugarExpr target) (preDesugarExpr matcher)
              (map preDesugarMatchClause clauses)
  
  MatchAllExpr mode target matcher clauses ->
    MatchAllExpr mode (preDesugarExpr target) (preDesugarExpr matcher)
                 (map preDesugarMatchClause clauses)
  
  MatchLambdaExpr matcher clauses ->
    MatchLambdaExpr (preDesugarExpr matcher) (map preDesugarMatchClause clauses)
  
  MatchAllLambdaExpr matcher clauses ->
    MatchAllLambdaExpr (preDesugarExpr matcher) (map preDesugarMatchClause clauses)
  
  -- Matcher expressions
  MatcherExpr patternDefs ->
    MatcherExpr (map preDesugarPatternDef patternDefs)
  
  AlgebraicDataMatcherExpr patterns ->
    AlgebraicDataMatcherExpr (map preDesugarAlgebraicPattern patterns)
  
  -- Tensor operations
  GenerateTensorExpr fn size ->
    GenerateTensorExpr (preDesugarExpr fn) (preDesugarExpr size)
  
  TensorExpr size body ->
    TensorExpr (preDesugarExpr size) (preDesugarExpr body)
  
  TensorContractExpr tensor ->
    TensorContractExpr (preDesugarExpr tensor)
  
  TensorMapExpr fn tensor ->
    TensorMapExpr (preDesugarExpr fn) (preDesugarExpr tensor)
  
  TensorMap2Expr fn t1 t2 ->
    TensorMap2Expr (preDesugarExpr fn) (preDesugarExpr t1) (preDesugarExpr t2)
  
  TransposeExpr indices tensor ->
    TransposeExpr (preDesugarExpr indices) (preDesugarExpr tensor)
  
  FlipIndicesExpr tensor ->
    FlipIndicesExpr (preDesugarExpr tensor)
  
  -- Math/Symbolic
  QuoteExpr e -> QuoteExpr (preDesugarExpr e)
  QuoteSymbolExpr e -> QuoteSymbolExpr (preDesugarExpr e)
  FunctionExpr args -> FunctionExpr args
  
  -- IO (SeqExpr exists, IOExpr doesn't in current AST)
  SeqExpr e1 e2 -> SeqExpr (preDesugarExpr e1) (preDesugarExpr e2)
  
  -- Section expressions (operators as values)
  SectionExpr op mLeft mRight ->
    SectionExpr op (fmap preDesugarExpr mLeft) (fmap preDesugarExpr mRight)
  
  -- Anonymous parameters
  AnonParamFuncExpr arity body ->
    AnonParamFuncExpr arity (preDesugarExpr body)
  
  AnonParamExpr n -> AnonParamExpr n
  
  -- Prefix operator
  PrefixExpr op e ->
    PrefixExpr op (preDesugarExpr e)
  
  -- Fresh variable
  FreshVarExpr -> FreshVarExpr

-- | Pre-desugar a top-level expression
preDesugarTopExpr :: TopExpr -> TopExpr
preDesugarTopExpr topExpr = case topExpr of
  Define var expr ->
    Define var (preDesugarExpr expr)
  
  DefineWithType var expr ->
    DefineWithType var (preDesugarExpr expr)
  
  Test expr ->
    Test (preDesugarExpr expr)
  
  Execute expr ->
    Execute (preDesugarExpr expr)
  
  LoadFile path -> LoadFile path
  Load lib -> Load lib
  InfixDecl isPat op -> InfixDecl isPat op
  
  InductiveDecl name params constructors ->
    InductiveDecl name params constructors
  
  ClassDeclExpr classDecl -> ClassDeclExpr classDecl
  InstanceDeclExpr instanceDecl -> InstanceDeclExpr instanceDecl
  
  PatternInductiveDecl name params constructors ->
    PatternInductiveDecl name params constructors
  
  PatternFunctionDecl name params args retType body ->
    PatternFunctionDecl name params args retType (preDesugarPattern body)

-- Helper functions

preDesugarIndex :: IndexExpr Expr -> IndexExpr Expr
preDesugarIndex idx = case idx of
  Subscript e -> Subscript (preDesugarExpr e)
  Superscript e -> Superscript (preDesugarExpr e)
  SupSubscript e -> SupSubscript (preDesugarExpr e)
  Userscript e -> Userscript (preDesugarExpr e)
  MultiSubscript e1 e2 -> MultiSubscript (preDesugarExpr e1) (preDesugarExpr e2)
  MultiSuperscript e1 e2 -> MultiSuperscript (preDesugarExpr e1) (preDesugarExpr e2)

preDesugarBinding :: BindingExpr -> BindingExpr
preDesugarBinding binding = case binding of
  Bind pat e -> Bind pat (preDesugarExpr e)  -- PrimitiveDataPattern doesn't need desugaring
  BindWithIndices var e -> BindWithIndices var (preDesugarExpr e)
  BindWithType typedVar e -> BindWithType typedVar (preDesugarExpr e)

preDesugarMatchClause :: MatchClause -> MatchClause
preDesugarMatchClause (pat, body) =
  (preDesugarPattern pat, preDesugarExpr body)

preDesugarPattern :: Pattern -> Pattern
preDesugarPattern pat = case pat of
  WildCard -> WildCard
  PatVar name -> PatVar name
  ValuePat expr -> ValuePat (preDesugarExpr expr)
  PredPat expr -> PredPat (preDesugarExpr expr)
  InductivePat name pats -> InductivePat name (map preDesugarPattern pats)
  TuplePat pats -> TuplePat (map preDesugarPattern pats)
  SeqNilPat -> SeqNilPat
  SeqConsPat p1 p2 -> SeqConsPat (preDesugarPattern p1) (preDesugarPattern p2)
  PApplyPat fn args -> PApplyPat (preDesugarExpr fn) (map preDesugarPattern args)
  LoopPat name range pat' pret ->
    LoopPat name (preDesugarLoopRange range) (preDesugarPattern pat') (preDesugarPattern pret)
  LetPat bindings body ->
    LetPat (map preDesugarBinding bindings) (preDesugarPattern body)
  NotPat pat' -> NotPat (preDesugarPattern pat')
  AndPat p1 p2 -> AndPat (preDesugarPattern p1) (preDesugarPattern p2)
  OrPat p1 p2 -> OrPat (preDesugarPattern p1) (preDesugarPattern p2)
  InfixPat op p1 p2 -> InfixPat op (preDesugarPattern p1) (preDesugarPattern p2)
  IndexedPat pat' indices -> IndexedPat (preDesugarPattern pat') indices
  VarPat name -> VarPat name
  InductiveOrPApplyPat name pats -> InductiveOrPApplyPat name (map preDesugarPattern pats)
  ForallPat p1 p2 -> ForallPat (preDesugarPattern p1) (preDesugarPattern p2)
  ContPat -> ContPat
  LaterPatVar -> LaterPatVar
  DApplyPat pat' pats -> DApplyPat (preDesugarPattern pat') (map preDesugarPattern pats)

preDesugarLoopRange :: LoopRange -> LoopRange
preDesugarLoopRange (LoopRange s e pat) =
  LoopRange (preDesugarExpr s) (preDesugarExpr e) (preDesugarPattern pat)

preDesugarPatternDef :: PatternDef -> PatternDef
preDesugarPatternDef (PatternDef patPat nextMatcher clauses) =
  PatternDef patPat (preDesugarExpr nextMatcher) (map preDesugarDataClause clauses)

preDesugarDataClause :: (PrimitiveDataPattern, Expr) -> (PrimitiveDataPattern, Expr)
preDesugarDataClause (dataPat, body) =
  (dataPat, preDesugarExpr body)

preDesugarAlgebraicPattern :: (String, [Expr]) -> (String, [Expr])
preDesugarAlgebraicPattern (name, pats) =
  (name, map preDesugarExpr pats)

