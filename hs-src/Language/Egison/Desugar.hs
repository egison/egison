{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}

{- |
Module      : Language.Egison.Desugar
Licence     : MIT

This module provide desugar functions.
-}

module Language.Egison.Desugar
    ( desugarTopExpr
    , desugarTopExprs
    , desugarExpr
    ) where

import           Control.Monad.Except   (throwError)
import           Data.Char              (toUpper)
import           Data.List              (union)

import           Language.Egison.AST
import           Language.Egison.IExpr
import           Language.Egison.Data
import           Language.Egison.RState


desugarTopExpr :: TopExpr -> EvalM (Maybe ITopExpr)
desugarTopExpr (Define var@(VarWithIndices name []) expr) = do
  expr' <- desugar expr
  case expr' of
    ILambdaExpr Nothing args body -> return . Just $ IDefine (Var name []) (ILambdaExpr (Just (show var)) args body)
    _                             -> return . Just $ IDefine (Var name []) expr'
desugarTopExpr (Define (VarWithIndices name is) expr) = do
  body <- desugar expr
  let indexNames = map extractIndex is
  let indexNamesCollection = ICollectionExpr (map stringToIVarExpr indexNames)
  return . Just $ IDefine (Var name (map (const () <$>) is))
    (IWithSymbolsExpr indexNames (ITransposeExpr indexNamesCollection body))
desugarTopExpr (Test expr)     = Just . ITest <$> desugar expr
desugarTopExpr (Execute expr)  = Just . IExecute <$> desugar expr
desugarTopExpr (LoadFile file) = return . Just $ ILoadFile file
desugarTopExpr (Load file)     = return . Just $ ILoad file
desugarTopExpr InfixDecl{}     = return Nothing

desugarTopExprs :: [TopExpr] -> EvalM [ITopExpr]
desugarTopExprs []             = return []
desugarTopExprs (expr : exprs) = do
  expr' <- desugarTopExpr expr
  case expr' of
    Nothing -> desugarTopExprs exprs
    Just expr' -> (expr' :) <$> desugarTopExprs exprs

desugarExpr :: Expr -> EvalM IExpr
desugarExpr = desugar

desugar :: Expr -> EvalM IExpr
desugar (ConstantExpr c) = return $ IConstantExpr c
desugar (VarExpr v)      = return $ IVarExpr v

desugar (AlgebraicDataMatcherExpr patterns) = do
  matcherName <- freshV
  let matcherRef = IVarExpr matcherName
  matcher <- genMatcherClauses patterns matcherRef
  return $ ILetRecExpr [(PDPatVar matcherName, matcher)] matcherRef
    where
      genMatcherClauses :: [(String, [Expr])] -> IExpr -> EvalM IExpr
      genMatcherClauses patterns matcher = do
        main <- genMainClause patterns matcher
        body <- mapM genMatcherClause patterns
        footer <- genSomethingClause
        let clauses = [main] ++ body ++ [footer]
        return $ IMatcherExpr clauses

      genMainClause :: [(String, [Expr])] -> IExpr -> EvalM (PrimitivePatPattern, IExpr, [(PrimitiveDataPattern, IExpr)])
      genMainClause patterns matcher = do
        clauses <- genClauses patterns
        return (PPValuePat "val", ITupleExpr []
               ,[(PDPatVar (stringToVar "tgt"), IMatchExpr BFSMode
                                            (ITupleExpr [stringToIVarExpr "val", stringToIVarExpr "tgt"])
                                            (ITupleExpr [matcher, matcher])
                                             clauses)])
        where
          genClauses :: [(String, [Expr])] -> EvalM [IMatchClause]
          genClauses patterns = (++) <$> mapM genClause patterns
                                     <*> pure [(ITuplePat [IWildCard, IWildCard], matchingFailure)]

          genClause :: (String, [Expr]) -> EvalM IMatchClause
          genClause pattern = do
            (pat0, pat1) <- genMatchingPattern pattern
            return (ITuplePat [pat0, pat1], matchingSuccess)

          genMatchingPattern :: (String, [Expr]) -> EvalM (IPattern, IPattern)
          genMatchingPattern (name, patterns) = do
            names <- mapM (const freshV) patterns
            return (IInductivePat name (map IPatVar names),
                    IInductivePat name (map (IValuePat . IVarExpr) names))

      genMatcherClause :: (String, [Expr]) -> EvalM (PrimitivePatPattern, IExpr, [(PrimitiveDataPattern, IExpr)])
      genMatcherClause pattern = do
        (ppat, matchers) <- genPrimitivePatPat pattern
        (dpat, body)     <- genPrimitiveDataPat pattern
        return (ppat, ITupleExpr matchers, [(dpat, ICollectionExpr [ITupleExpr body]), (PDWildCard, matchingFailure)])

        where
          genPrimitivePatPat :: (String, [Expr]) -> EvalM (PrimitivePatPattern, [IExpr])
          genPrimitivePatPat (name, matchers) = do
            matchers' <- mapM desugar matchers
            patterns' <- mapM (const $ return PPPatVar) matchers
            return (PPInductivePat name patterns', matchers')

          genPrimitiveDataPat :: (String, [Expr]) -> EvalM (PrimitiveDataPattern, [IExpr])
          genPrimitiveDataPat (name, patterns) = do
            patterns' <- mapM (const freshV) patterns
            return (PDInductivePat (capitalize name) $ map (PDPatVar . stringToVar . show) patterns', map IVarExpr patterns')

          capitalize :: String -> String
          capitalize (x:xs) = toUpper x : xs

      genSomethingClause :: EvalM (PrimitivePatPattern, IExpr, [(PrimitiveDataPattern, IExpr)])
      genSomethingClause =
        return (PPPatVar, ITupleExpr [IConstantExpr SomethingExpr], [(PDPatVar (stringToVar "tgt"), ICollectionExpr [stringToIVarExpr "tgt"])])

      matchingSuccess :: IExpr
      matchingSuccess = ICollectionExpr [ITupleExpr []]

      matchingFailure :: IExpr
      matchingFailure = ICollectionExpr []

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
          desugarMultiScript ISubrefsExpr b1 e1 n1 n2
        (TupleExpr [IndexedExpr b1 e1 [n1]], TupleExpr [IndexedExpr _ _ [n2]]) ->
          desugarMultiScript ISubrefsExpr b1 e1 n1 n2
        _ -> throwError $ Default "Index should be IndexedExpr for multi subscript"
    [MultiSuperscript x y] ->
      case (x, y) of
        (IndexedExpr b1 e1 [n1], IndexedExpr _ _ [n2]) ->
          desugarMultiScript ISuprefsExpr b1 e1 n1 n2
        (TupleExpr [IndexedExpr b1 e1 [n1]], TupleExpr [IndexedExpr _ _ [n2]]) ->
          desugarMultiScript ISuprefsExpr b1 e1 n1 n2
        _ -> throwError $ Default "Index should be IndexedExpr for multi superscript"
    _ -> IIndexedExpr b <$> desugar expr <*> mapM desugarIndex indices
  where
    desugarMultiScript refExpr b1 e1 n1 n2 = do
      k     <- fresh
      n1'   <- desugar (extractIndex n1)
      n2'   <- desugar (extractIndex n2)
      e1'   <- desugar e1
      expr' <- desugar expr
      return $ refExpr b expr' (makeIApply "map"
                                         [ILambdaExpr Nothing [k] (IIndexedExpr b1 e1' [Subscript $ stringToIVarExpr k]),
                                          makeIApply "between" [n1', n2']])

desugar (SubrefsExpr bool expr1 expr2) =
  ISubrefsExpr bool <$> desugar expr1 <*> desugar expr2

desugar (SuprefsExpr bool expr1 expr2) =
  ISuprefsExpr bool <$> desugar expr1 <*> desugar expr2

desugar (UserrefsExpr bool expr1 expr2) =
  IUserrefsExpr bool <$> desugar expr1 <*> desugar expr2

desugar (TupleExpr exprs) = ITupleExpr <$> mapM desugar exprs
desugar (CollectionExpr xs) = ICollectionExpr <$> mapM desugar xs
desugar (ConsExpr x xs) = IConsExpr <$> desugar x <*> desugar xs
desugar (JoinExpr x xs) = IJoinExpr <$> desugar x <*> desugar xs

desugar (HashExpr exprPairs) =
  IHashExpr <$> mapM (\(expr1, expr2) -> (,) <$> desugar expr1 <*> desugar expr2) exprPairs

desugar (VectorExpr exprs) =
  IVectorExpr <$> mapM desugar exprs

desugar (TensorExpr nsExpr xsExpr) =
  ITensorExpr <$> desugar nsExpr <*> desugar xsExpr

desugar (LambdaExpr names expr) = do
  expr <- desugar expr
  let (args', expr') = foldr desugarInvertedArgs ([], expr) names
  return $ ILambdaExpr Nothing args' expr'
  where
    desugarInvertedArgs :: Arg -> ([String], IExpr) -> ([String], IExpr)
    desugarInvertedArgs (TensorArg x) (args, expr) = (x : args, expr)
    desugarInvertedArgs (ScalarArg x) (args, expr) = 
      (x : args, ITensorMapExpr (ILambdaExpr Nothing [x] expr) (stringToIVarExpr x))
    desugarInvertedArgs (InvertedScalarArg x) (args, expr) =
      (x : args, ITensorMapExpr (ILambdaExpr Nothing [x] expr) (IFlipIndicesExpr (stringToIVarExpr x)))

desugar (MemoizedLambdaExpr names expr) =
  IMemoizedLambdaExpr names <$> desugar expr

desugar (CambdaExpr name expr) =
  ICambdaExpr name <$> desugar expr

desugar (PatternFunctionExpr names pattern) =
  IPatternFunctionExpr names <$> desugarPattern pattern

desugar (IfExpr expr0 expr1 expr2) =
  IIfExpr <$> desugar expr0 <*> desugar expr1 <*> desugar expr2

desugar (LetRecExpr binds expr) =
  ILetRecExpr <$> desugarBindings binds <*> desugar expr

desugar (WithSymbolsExpr vars expr) =
  IWithSymbolsExpr vars <$> desugar expr

desugar (MatchExpr pmmode expr0 expr1 clauses) =
  IMatchExpr pmmode <$> desugar expr0 <*> desugar expr1 <*> desugarMatchClauses clauses

desugar (MatchAllExpr pmmode expr0 expr1 clauses) =
  IMatchAllExpr pmmode <$> desugar expr0 <*> desugar expr1 <*> desugarMatchClauses clauses

desugar (DoExpr binds expr) =
  IDoExpr <$> desugarBindings binds <*> desugar expr

desugar (IoExpr expr) =
  IIoExpr <$> desugar expr

desugar (PrefixExpr "-" expr) = do
  expr' <- desugar expr
  return $ makeIApply "*" [IConstantExpr (IntegerExpr (-1)), expr']
desugar (PrefixExpr "!" (ApplyExpr expr1 expr2)) =
  IWedgeApplyExpr <$> desugar expr1 <*> desugar expr2
desugar (PrefixExpr "'" expr) = IQuoteExpr <$> desugar expr
desugar (PrefixExpr "`" expr) = IQuoteSymbolExpr <$> desugar expr
desugar PrefixExpr{} = fail "Unknown prefix"

desugar (InfixExpr op expr1 expr2) | isWedge op =
  (\x y -> IWedgeApplyExpr (stringToIVarExpr (repr op)) (ITupleExpr [x, y]))
    <$> desugar expr1 <*> desugar expr2

desugar (InfixExpr op expr1 expr2) | repr op == "::" =
  IConsExpr <$> desugar expr1 <*> desugar expr2
desugar (InfixExpr op expr1 expr2) | repr op == "++" =
  IJoinExpr <$> desugar expr1 <*> desugar expr2
desugar (InfixExpr op expr1 expr2) =
  (\x y -> makeIApply (repr op) [x, y]) <$> desugar expr1 <*> desugar expr2

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
  ISeqExpr <$> desugar expr0 <*> desugar expr1

desugar (GenerateTensorExpr fnExpr sizeExpr) =
  IGenerateTensorExpr <$> desugar fnExpr <*> desugar sizeExpr

desugar (TensorContractExpr tExpr) =
  ITensorContractExpr <$> desugar tExpr

desugar (TensorMapExpr (LambdaExpr [x] (TensorMapExpr (LambdaExpr [y] expr) b)) a) =
  desugar (TensorMap2Expr (LambdaExpr [x, y] expr) a b)

desugar (TensorMapExpr fnExpr tExpr) =
  ITensorMapExpr <$> desugar fnExpr <*> desugar tExpr

desugar (TensorMap2Expr fnExpr t1Expr t2Expr) =
  ITensorMap2Expr <$> desugar fnExpr <*> desugar t1Expr <*> desugar t2Expr

desugar (TransposeExpr vars expr) =
  ITransposeExpr <$> desugar vars <*> desugar expr

desugar (ApplyExpr expr0 expr1) =
  IApplyExpr <$> desugar expr0 <*> desugar expr1

desugar (CApplyExpr expr0 expr1) =
  ICApplyExpr <$> desugar expr0 <*> desugar expr1

desugar FreshVarExpr = do
  id <- fresh
  return $ stringToIVarExpr (":::" ++ id)

desugar (MatcherExpr patternDefs) =
  IMatcherExpr <$> mapM desugarPatternDef patternDefs

desugar (AnonParamExpr n) = return $ stringToIVarExpr ('%' : show n)

desugar (AnonParamFuncExpr n expr) = do
  expr' <- desugar expr
  let lambda = ILambdaExpr Nothing (map (\n -> '%' : show n) [1..n]) expr'
  return $ ILetRecExpr [(PDPatVar (stringToVar "%0"), lambda)] (stringToIVarExpr "%0")

desugar (QuoteExpr expr) =
  IQuoteExpr <$> desugar expr

desugar (QuoteSymbolExpr expr) =
  IQuoteSymbolExpr <$> desugar expr

desugar (WedgeApplyExpr expr0 expr1) =
  IWedgeApplyExpr <$> desugar expr0 <*> desugar expr1

desugar (FunctionExpr vars) = return $ IFunctionExpr vars

desugarIndex :: Index Expr -> EvalM (Index IExpr)
desugarIndex index = traverse desugar index

desugarPattern :: Pattern -> EvalM IPattern
desugarPattern pat = ILetPat (map makeBinding (collectName pat)) <$> desugarPattern' pat
 where
   collectNames :: [Pattern] -> [Var]
   collectNames pats = foldl union [] (map collectName pats)

   collectName :: Pattern -> [Var]
   collectName (ForallPat pat1 pat2) = collectName pat1 `union` collectName pat2
   collectName (InfixPat _ pat1 pat2) = collectName pat1 `union` collectName pat2
   collectName (NotPat pat)  = collectName pat
   collectName (AndPat pat1 pat2) = collectName pat1 `union` collectName pat2
   collectName (OrPat pat1 pat2)  = collectName pat1 `union` collectName pat2
   collectName (TuplePat pats) = collectNames pats
   collectName (InductiveOrPApplyPat _ pats) = collectNames pats
   collectName (InductivePat _ pats) = collectNames pats
   collectName (PApplyPat _ pats) = collectNames pats
   collectName (DApplyPat _ pats) = collectNames pats
   collectName (LoopPat _ (LoopRange _ _ endNumPat) pat1 pat2) = collectName endNumPat `union` collectName pat1 `union` collectName pat2
   collectName (LetPat _ pat) = collectName pat
   collectName (IndexedPat (PatVar var) _) = [var]
   collectName _ = []

   makeBinding :: Var -> IBindingExpr
   makeBinding var = (PDPatVar var, IHashExpr [])

desugarPattern' :: Pattern -> EvalM IPattern
desugarPattern' WildCard        = return IWildCard
desugarPattern' ContPat         = return IContPat
desugarPattern' SeqNilPat       = return ISeqNilPat
desugarPattern' LaterPatVar     = return ILaterPatVar
desugarPattern' (PatVar var)    = return (IPatVar var)
desugarPattern' (VarPat var)    = return (IVarPat var)
desugarPattern' (ValuePat expr) = IValuePat <$> desugar expr
desugarPattern' (PredPat expr)  = IPredPat <$> desugar expr
desugarPattern' (NotPat pat)    = INotPat <$> desugarPattern' pat
desugarPattern' (ForallPat pat1 pat2) = IForallPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (AndPat pat1 pat2)    = IAndPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (OrPat pat1 pat2)     = IOrPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (InfixPat Op{ repr = "&" } pat1 pat2) =
  IAndPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (InfixPat Op{ repr = "|" } pat1 pat2) =
  IOrPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (InfixPat Op{ repr = f } pat1 pat2) =
  (\x y -> IInductivePat f [x, y]) <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (TuplePat pats) = ITuplePat <$> mapM desugarPattern' pats
desugarPattern' (InductiveOrPApplyPat name pats) = IInductiveOrPApplyPat name <$> mapM desugarPattern' pats
desugarPattern' (InductivePat name pats) = IInductivePat name <$> mapM desugarPattern' pats
desugarPattern' (IndexedPat pat exprs)   = IIndexedPat <$> desugarPattern' pat <*> mapM desugar exprs
desugarPattern' (PApplyPat expr pats)    = IPApplyPat <$> desugar expr <*> mapM desugarPattern' pats
desugarPattern' (DApplyPat pat pats)     = IDApplyPat <$> desugarPattern' pat <*> mapM desugarPattern' pats
desugarPattern' (LoopPat name range pat1 pat2) = ILoopPat name <$> desugarLoopRange range <*> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (LetPat binds pat)       = ILetPat <$> desugarBindings binds <*> desugarPattern' pat
desugarPattern' (SeqConsPat pat1 pat2)   = ISeqConsPat <$> desugarPattern' pat1 <*> desugarPattern' pat2

desugarLoopRange :: LoopRange -> EvalM ILoopRange
desugarLoopRange (LoopRange sExpr eExpr pat) =
  ILoopRange <$> desugar sExpr <*> desugar eExpr <*> desugarPattern' pat

desugarBindings :: [BindingExpr] -> EvalM [IBindingExpr]
desugarBindings = mapM f
  where
    f (name, expr) = do
      expr' <- desugar expr
      case (name, expr') of
        (PDPatVar var, ILambdaExpr Nothing args body) ->
          return (name, ILambdaExpr (Just (show var)) args body)
        _ -> return (name, expr')

desugarMatchClauses :: [MatchClause] -> EvalM [IMatchClause]
desugarMatchClauses = mapM (\(pat, expr) -> (,) <$> desugarPattern pat <*> desugar expr)

desugarPatternDef :: PatternDef -> EvalM IPatternDef
desugarPatternDef (pp, matcher, pds) =
  (pp,,) <$> desugar matcher <*> desugarPrimitiveDataMatchClauses pds

desugarPrimitiveDataMatchClauses :: [(PrimitiveDataPattern, Expr)] -> EvalM [(PrimitiveDataPattern, IExpr)]
desugarPrimitiveDataMatchClauses = mapM (\(pd, expr) -> (pd,) <$> desugar expr)
