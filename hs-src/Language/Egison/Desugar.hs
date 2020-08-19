{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}

{- |
Module      : Language.Egison.Desugar
Licence     : MIT

This module provide desugar functions.
-}

module Language.Egison.Desugar
    (
      desugarTopExpr
    , desugarExpr
    ) where

import           Control.Monad.Except  (throwError)
import           Data.Char             (toUpper)
import           Data.Set              (Set)
import qualified Data.Set              as S

import           Language.Egison.AST
import           Language.Egison.Data
import           Language.Egison.RState


desugarTopExpr :: EgisonTopExpr -> EvalM EgisonTopExpr
desugarTopExpr (Define name expr)   = Define name <$> desugar expr
desugarTopExpr (DefineWithIndices (VarWithIndices name is) expr) = do
  body <- desugar expr
  let indexNames = map extractIndex is
  let indexNamesCollection = CollectionExpr (map stringToVarExpr indexNames)
  return $ Define (Var name (map (const () <$>) is))
    (WithSymbolsExpr indexNames (TransposeExpr indexNamesCollection body))
desugarTopExpr (Redefine name expr) = Redefine name <$> desugar expr
desugarTopExpr (Test expr)          = Test <$> desugar expr
desugarTopExpr (Execute expr)       = Execute <$> desugar expr
desugarTopExpr expr                 = return expr

desugarExpr :: EgisonExpr -> EvalM EgisonExpr
desugarExpr = desugar

desugar :: EgisonExpr -> EvalM EgisonExpr
desugar (AlgebraicDataMatcherExpr patterns) = do
  matcherName <- freshV
  let matcherRef = VarExpr matcherName
  matcher <- genMatcherClauses patterns matcherRef
  return $ LetRecExpr [([matcherName], matcher)] matcherRef
    where
      genMatcherClauses :: [(String, [EgisonExpr])] ->  EgisonExpr -> EvalM EgisonExpr
      genMatcherClauses patterns matcher = do
        main <- genMainClause patterns matcher
        body <- mapM genMatcherClause patterns
        footer <- genSomethingClause
        let clauses = [main] ++ body ++ [footer]
        return $ MatcherExpr clauses

      genMainClause :: [(String, [EgisonExpr])] -> EgisonExpr -> EvalM (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
      genMainClause patterns matcher = do
        clauses <- genClauses patterns
        return (PPValuePat "val", TupleExpr []
               ,[(PDPatVar "tgt", MatchExpr BFSMode
                                            (TupleExpr [stringToVarExpr "val", stringToVarExpr "tgt"])
                                            (TupleExpr [matcher, matcher])
                                             clauses)])
        where
          genClauses :: [(String, [EgisonExpr])] -> EvalM [MatchClause]
          genClauses patterns = (++) <$> mapM genClause patterns
                                     <*> pure [(TuplePat [WildCard, WildCard], matchingFailure)]

          genClause :: (String, [EgisonExpr]) -> EvalM MatchClause
          genClause pattern = do
            (pat0, pat1) <- genMatchingPattern pattern
            return (TuplePat [pat0, pat1], matchingSuccess)

          genMatchingPattern :: (String, [EgisonExpr]) -> EvalM (Pattern, Pattern)
          genMatchingPattern (name, patterns) = do
            names <- mapM (const freshV) patterns
            return (InductivePat name (map PatVar names),
                    InductivePat name (map (ValuePat . VarExpr) names))

      genMatcherClause :: (String, [EgisonExpr]) -> EvalM (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
      genMatcherClause pattern = do
        (ppat, matchers) <- genPrimitivePatPat pattern
        (dpat, body)     <- genPrimitiveDataPat pattern
        return (ppat, TupleExpr matchers, [(dpat, CollectionExpr [TupleExpr body]), (PDWildCard, matchingFailure)])

        where
          genPrimitivePatPat :: (String, [EgisonExpr]) -> EvalM (PrimitivePatPattern, [EgisonExpr])
          genPrimitivePatPat (name, matchers) = do
            patterns' <- mapM (const $ return PPPatVar) matchers
            return (PPInductivePat name patterns', matchers)

          genPrimitiveDataPat :: (String, [EgisonExpr]) -> EvalM (PrimitiveDataPattern, [EgisonExpr])
          genPrimitiveDataPat (name, patterns) = do
            patterns' <- mapM (const freshV) patterns
            return (PDInductivePat (capitalize name) $ map (PDPatVar . show) patterns', map VarExpr patterns')

          capitalize :: String -> String
          capitalize (x:xs) = toUpper x : xs


      genSomethingClause :: EvalM (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
      genSomethingClause =
        return (PPPatVar, TupleExpr [SomethingExpr], [(PDPatVar "tgt", CollectionExpr [stringToVarExpr "tgt"])])

      matchingSuccess :: EgisonExpr
      matchingSuccess = CollectionExpr [TupleExpr []]

      matchingFailure :: EgisonExpr
      matchingFailure = CollectionExpr []

desugar (MatchAllLambdaExpr matcher clauses) = do
  name <- fresh
  desugar $ LambdaExpr [TensorArg name] (MatchAllExpr BFSMode (stringToVarExpr name) matcher clauses)

desugar (MatchLambdaExpr matcher clauses) = do
  name <- fresh
  desugar $ LambdaExpr [TensorArg name] (MatchExpr BFSMode (stringToVarExpr name) matcher clauses)

-- TODO: Allow nested MultiSubscript and MultiSuperscript
desugar (IndexedExpr b expr indices) =
  case indices of
    [MultiSubscript x y] ->
      case (x, y) of
        (IndexedExpr b1 e1 [n1], IndexedExpr _ _ [n2]) ->
          desugarMultiScript SubrefsExpr b1 e1 n1 n2
        (TupleExpr [IndexedExpr b1 e1 [n1]], TupleExpr [IndexedExpr _ _ [n2]]) ->
          desugarMultiScript SubrefsExpr b1 e1 n1 n2
        _ -> throwError $ Default "Index should be IndexedExpr for multi subscript"
    [MultiSuperscript x y] ->
      case (x, y) of
        (IndexedExpr b1 e1 [n1], IndexedExpr _ _ [n2]) ->
          desugarMultiScript SuprefsExpr b1 e1 n1 n2
        (TupleExpr [IndexedExpr b1 e1 [n1]], TupleExpr [IndexedExpr _ _ [n2]]) ->
          desugarMultiScript SuprefsExpr b1 e1 n1 n2
        _ -> throwError $ Default "Index should be IndexedExpr for multi superscript"
    _ -> IndexedExpr b <$> desugar expr <*> mapM desugarIndex indices
  where
    desugarMultiScript refExpr b1 e1 n1 n2 = do
      k <- fresh
      return $ refExpr b expr (makeApply "map"
                                         [LambdaExpr [TensorArg k] (IndexedExpr b1 e1 [Subscript $ stringToVarExpr k]),
                                          makeApply "between" [extractIndex n1, extractIndex n2]])

desugar (SubrefsExpr bool expr1 expr2) =
  SubrefsExpr bool <$> desugar expr1 <*> desugar expr2

desugar (SuprefsExpr bool expr1 expr2) =
  SuprefsExpr bool <$> desugar expr1 <*> desugar expr2

desugar (UserrefsExpr bool expr1 expr2) =
  UserrefsExpr bool <$> desugar expr1 <*> desugar expr2

desugar (InductiveDataExpr name exprs) =
  InductiveDataExpr name <$> mapM desugar exprs

desugar (TupleExpr exprs) =
  TupleExpr <$> mapM desugar exprs

desugar expr@(CollectionExpr []) = return expr

desugar (CollectionExpr xs) = CollectionExpr <$> mapM desugar xs
desugar (ConsExpr x xs) = ConsExpr <$> desugar x <*> desugar xs
desugar (JoinExpr x xs) = JoinExpr <$> desugar x <*> desugar xs

desugar (HashExpr exprPairs) =
  HashExpr <$> mapM (\(expr1, expr2) -> (,) <$> desugar expr1 <*> desugar expr2) exprPairs

desugar (VectorExpr exprs) =
  VectorExpr <$> mapM desugar exprs

desugar (TensorExpr nsExpr xsExpr) =
  TensorExpr <$> desugar nsExpr <*> desugar xsExpr

desugar (LambdaExpr names expr) = do
  let (args', expr') = foldr desugarInvertedArgs ([], expr) names
  expr'' <- desugar expr'
  return $ LambdaExpr args' expr''
  where
    desugarInvertedArgs :: Arg -> ([Arg], EgisonExpr) -> ([Arg], EgisonExpr)
    desugarInvertedArgs (TensorArg x) (args, expr) = (TensorArg x : args, expr)
    desugarInvertedArgs (ScalarArg x) (args, expr) =
      (TensorArg x : args,
       TensorMapExpr (LambdaExpr [TensorArg x] expr) (stringToVarExpr x))
    desugarInvertedArgs (InvertedScalarArg x) (args, expr) =
      (TensorArg x : args,
       TensorMapExpr (LambdaExpr [TensorArg x] expr) (FlipIndicesExpr (stringToVarExpr x)))

desugar (MemoizedLambdaExpr names expr) =
  MemoizedLambdaExpr names <$> desugar expr

desugar (CambdaExpr name expr) =
  CambdaExpr name <$> desugar expr

desugar (PatternFunctionExpr names pattern) =
  PatternFunctionExpr names <$> desugarPattern pattern

desugar (IfExpr expr0 expr1 expr2) =
  IfExpr <$> desugar expr0 <*> desugar expr1 <*> desugar expr2

desugar (LetRecExpr binds expr) =
  LetRecExpr <$> desugarBindings binds <*> desugar expr

desugar (LetExpr binds expr) =
  LetExpr <$> desugarBindings binds <*> desugar expr

desugar (WithSymbolsExpr vars expr) =
  WithSymbolsExpr vars <$> desugar expr

desugar (MatchExpr pmmode expr0 expr1 clauses) =
  MatchExpr pmmode <$> desugar expr0 <*> desugar expr1 <*> desugarMatchClauses clauses

desugar (MatchAllExpr pmmode expr0 expr1 clauses) =
  MatchAllExpr pmmode <$> desugar expr0 <*> desugar expr1 <*> desugarMatchClauses clauses

desugar (DoExpr binds expr) =
  DoExpr <$> desugarBindings binds <*> desugar expr

desugar (IoExpr expr) =
  IoExpr <$> desugar expr

desugar (PrefixExpr "-" expr) = do
  expr' <- desugar expr
  return $ makeApply "*" [IntegerExpr (-1), expr']
desugar (PrefixExpr "!" (ApplyExpr expr1 expr2)) =
  WedgeApplyExpr <$> desugar expr1 <*> desugar expr2
desugar (PrefixExpr "'" expr) = QuoteExpr <$> desugar expr
desugar (PrefixExpr "`" expr) = QuoteSymbolExpr <$> desugar expr

desugar (InfixExpr op expr1 expr2) | isWedge op =
  (\x y -> WedgeApplyExpr (stringToVarExpr (repr op)) (TupleExpr [x, y]))
    <$> desugar expr1 <*> desugar expr2

desugar (InfixExpr op expr1 expr2) | repr op == "::" =
  ConsExpr <$> desugar expr1 <*> desugar expr2
desugar (InfixExpr op expr1 expr2) | repr op == "++" =
  JoinExpr <$> desugar expr1 <*> desugar expr2
desugar (InfixExpr op expr1 expr2) =
  (\x y -> makeApply (repr op) [x, y]) <$> desugar expr1 <*> desugar expr2

-- section
--
-- If `op` is not a cambda, simply desugar it into the function
desugar (SectionExpr op Nothing Nothing)
  | not (isWedge op || repr op `elem` ["::", "++"]) =
    desugar (stringToVarExpr (repr op))
desugar (SectionExpr op Nothing Nothing) = do
  x <- fresh
  y <- fresh
  desugar $ LambdaExpr [TensorArg x, TensorArg y]
                       (InfixExpr op (stringToVarExpr x) (stringToVarExpr y))

desugar (SectionExpr op Nothing (Just expr2)) = do
  x <- fresh
  desugar $ LambdaExpr [TensorArg x]
                       (InfixExpr op (stringToVarExpr x) expr2)

desugar (SectionExpr op (Just expr1) Nothing) = do
  y <- fresh
  desugar $ LambdaExpr [TensorArg y]
                       (InfixExpr op expr1 (stringToVarExpr y))

desugar SectionExpr{} = throwError $ Default "Cannot reach here: section with both arguments"

desugar (SeqExpr expr0 expr1) =
  SeqExpr <$> desugar expr0 <*> desugar expr1

desugar (GenerateTensorExpr fnExpr sizeExpr) =
  GenerateTensorExpr <$> desugar fnExpr <*> desugar sizeExpr

desugar (TensorContractExpr tExpr) =
  TensorContractExpr <$> desugar tExpr

desugar (TensorMapExpr (LambdaExpr [x] (TensorMapExpr (LambdaExpr [y] expr) b)) a) =
  desugar (TensorMap2Expr (LambdaExpr [x, y] expr) a b)

desugar (TensorMapExpr fnExpr tExpr) =
  TensorMapExpr <$> desugar fnExpr <*> desugar tExpr

desugar (TensorMap2Expr fnExpr t1Expr t2Expr) =
  TensorMap2Expr <$> desugar fnExpr <*> desugar t1Expr <*> desugar t2Expr

desugar (TransposeExpr vars expr) =
  TransposeExpr <$> desugar vars <*> desugar expr

desugar (FlipIndicesExpr expr) =
  FlipIndicesExpr <$> desugar expr

desugar (ApplyExpr expr0 expr1) =
  ApplyExpr <$> desugar expr0 <*> desugar expr1

desugar (CApplyExpr expr0 expr1) =
  CApplyExpr <$> desugar expr0 <*> desugar expr1

desugar FreshVarExpr = do
  id <- fresh
  return $ stringToVarExpr (":::" ++ id)

desugar (MatcherExpr patternDefs) =
  MatcherExpr <$> mapM desugarPatternDef patternDefs

desugar (AnonParamExpr n) = return $ stringToVarExpr ('%' : show n)

desugar (AnonParamFuncExpr n expr) = do
  expr' <- desugar expr
  let lambda = LambdaExpr (map (\n -> TensorArg ('%' : show n)) [1..n]) expr'
  return $ LetRecExpr [([stringToVar "%0"], lambda)] (stringToVarExpr "%0")

desugar (QuoteExpr expr) =
  QuoteExpr <$> desugar expr

desugar (QuoteSymbolExpr expr) =
  QuoteSymbolExpr <$> desugar expr

desugar (WedgeApplyExpr expr0 expr1) =
  WedgeApplyExpr <$> desugar expr0 <*> desugar expr1

desugar expr = return expr

desugarIndex :: Index EgisonExpr -> EvalM (Index EgisonExpr)
desugarIndex index = traverse desugar index

desugarPattern :: Pattern -> EvalM Pattern
desugarPattern pat = LetPat (map makeBinding $ S.elems $ collectName pat) <$> desugarPattern' pat
 where
   collectNames :: [Pattern] -> Set String
   collectNames pats = S.unions $ map collectName pats

   collectName :: Pattern -> Set String
   collectName (ForallPat pat1 pat2) = collectName pat1 `S.union` collectName pat2
   collectName (InfixPat _ pat1 pat2) = collectName pat1 `S.union` collectName pat2
   collectName (NotPat pat)  = collectName pat
   collectName (AndPat pat1 pat2) = collectName pat1 `S.union` collectName pat2
   collectName (OrPat pat1 pat2)  = collectName pat1 `S.union` collectName pat2
   collectName (TuplePat pats) = collectNames pats
   collectName (InductiveOrPApplyPat _ pats) = collectNames pats
   collectName (InductivePat _ pats) = collectNames pats
   collectName (PApplyPat _ pats) = collectNames pats
   collectName (DApplyPat _ pats) = collectNames pats
   collectName (LoopPat _ (LoopRange _ _ endNumPat) pat1 pat2) = collectName endNumPat `S.union` collectName pat1 `S.union` collectName pat2
   collectName (LetPat _ pat) = collectName pat
   collectName (IndexedPat (PatVar name) _) = S.singleton $ show name
   collectName _ = S.empty

   makeBinding :: String -> BindingExpr
   makeBinding name = ([stringToVar name], HashExpr [])

desugarPattern' :: Pattern -> EvalM Pattern
desugarPattern' (ValuePat expr) = ValuePat <$> desugar expr
desugarPattern' (PredPat expr) = PredPat <$> desugar expr
desugarPattern' (NotPat pat) = NotPat <$> desugarPattern' pat
desugarPattern' (ForallPat pat1 pat2) = ForallPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (AndPat pat1 pat2) = AndPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (OrPat pat1 pat2) = OrPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (InfixPat Op{ repr = "&" } pat1 pat2) =
  AndPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (InfixPat Op{ repr = "|" } pat1 pat2) =
  OrPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (InfixPat Op{ repr = f } pat1 pat2) =
  (\x y -> InductivePat f [x, y]) <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (TuplePat pats)  = TuplePat <$> mapM desugarPattern' pats
desugarPattern' (InductiveOrPApplyPat name pats) = InductiveOrPApplyPat name <$> mapM desugarPattern' pats
desugarPattern' (InductivePat name pats) = InductivePat name <$> mapM desugarPattern' pats
desugarPattern' (IndexedPat pat exprs) = IndexedPat <$> desugarPattern' pat <*> mapM desugar exprs
desugarPattern' (PApplyPat expr pats) = PApplyPat <$> desugar expr <*> mapM desugarPattern' pats
desugarPattern' (DApplyPat pat pats) = DApplyPat <$> desugarPattern' pat <*> mapM desugarPattern' pats
desugarPattern' (LoopPat name range pat1 pat2) =  LoopPat name <$> desugarLoopRange range <*> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (LetPat binds pat) = LetPat <$> desugarBindings binds <*> desugarPattern' pat
desugarPattern' (SeqConsPat pat1 pat2)  = SeqConsPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' pat = return pat

desugarLoopRange :: LoopRange -> EvalM LoopRange
desugarLoopRange (LoopRange sExpr eExpr pat) =
  LoopRange <$> desugar sExpr <*> desugar eExpr <*> desugarPattern' pat

desugarBindings :: [BindingExpr] -> EvalM [BindingExpr]
desugarBindings = mapM (\(name, expr) -> (name,) <$> desugar expr)

desugarMatchClauses :: [MatchClause] -> EvalM [MatchClause]
desugarMatchClauses = mapM (\(pat, expr) -> (,) <$> desugarPattern pat <*> desugar expr)

desugarPatternDef :: PatternDef -> EvalM PatternDef
desugarPatternDef (pp, matcher, pds) =
  (pp,,) <$> desugar matcher <*> desugarPrimitiveDataMatchClauses pds

desugarPrimitiveDataMatchClauses :: [(PrimitiveDataPattern, EgisonExpr)] -> EvalM [(PrimitiveDataPattern, EgisonExpr)]
desugarPrimitiveDataMatchClauses = mapM (\(pd, expr) -> (pd,) <$> desugar expr)
