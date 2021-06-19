{-# LANGUAGE TupleSections #-}

{- |
Module      : Language.Egison.Desugar
Licence     : MIT

This module provides desugar functions.
-}

module Language.Egison.Desugar
    ( desugarTopExpr
    , desugarTopExprs
    , desugarExpr
    ) where

import           Control.Monad.Except   (throwError)
import           Data.Char              (toUpper)
import           Data.Foldable          (foldrM)
import           Data.List              (union)

import           Language.Egison.AST
import           Language.Egison.Data
import           Language.Egison.IExpr
import           Language.Egison.RState


desugarTopExpr :: TopExpr -> EvalM (Maybe ITopExpr)
desugarTopExpr (Define vwi expr) = do
  (var, iexpr) <- desugarDefineWithIndices vwi expr
  return . Just $ IDefine var iexpr
desugarTopExpr (Test expr)     = Just . ITest <$> desugar expr
desugarTopExpr (Execute expr)  = Just . IExecute <$> desugar expr
desugarTopExpr (Load file)     = return . Just $ ILoad file
desugarTopExpr (LoadFile file) = return . Just $ ILoadFile file
desugarTopExpr _               = return Nothing

desugarTopExprs :: [TopExpr] -> EvalM [ITopExpr]
desugarTopExprs [] = return []
desugarTopExprs (expr : exprs) = do
  expr' <- desugarTopExpr expr
  case expr' of
    Nothing    -> desugarTopExprs exprs
    Just expr' -> (expr' :) <$> desugarTopExprs exprs

desugarExpr :: Expr -> EvalM IExpr
desugarExpr = desugar

desugar :: Expr -> EvalM IExpr
desugar (ConstantExpr c) = return $ IConstantExpr c
desugar (VarExpr var)    = return $ IVarExpr var

desugar (AlgebraicDataMatcherExpr patterns) = do
  matcherName <- fresh
  let matcherRef = IVarExpr matcherName
  matcher <- genMatcherClauses patterns matcherRef
  return $ ILetRecExpr [(PDPatVar (stringToVar matcherName), matcher)] matcherRef
    where
      genMatcherClauses :: [(String, [Expr])] ->  IExpr -> EvalM IExpr
      genMatcherClauses patterns matcher = do
        main <- genMainClause patterns matcher
        body <- mapM genMatcherClause patterns
        footer <- genSomethingClause
        let clauses = [main] ++ body ++ [footer]
        return $ IMatcherExpr clauses

      genMainClause :: [(String, [Expr])] -> IExpr -> EvalM (PrimitivePatPattern, IExpr, [(IPrimitiveDataPattern, IExpr)])
      genMainClause patterns matcher = do
        clauses <- genClauses patterns
        return (PPValuePat "val", ITupleExpr [],
                [(PDPatVar (stringToVar "tgt"),
                    IMatchExpr BFSMode
                               (ITupleExpr [IVarExpr "val", IVarExpr "tgt"])
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
            names <- mapM (const fresh) patterns
            return (IInductivePat name (map IPatVar names),
                    IInductivePat name (map (IValuePat . IVarExpr) names))

      genMatcherClause :: (String, [Expr]) -> EvalM (PrimitivePatPattern, IExpr, [(IPrimitiveDataPattern, IExpr)])
      genMatcherClause pattern = do
        (ppat, matchers) <- genPrimitivePatPat pattern
        (dpat, body)     <- genPrimitiveDataPat pattern
        return (ppat, ITupleExpr matchers, [(dpat, ICollectionExpr [ITupleExpr body]), (PDWildCard, matchingFailure)])

        where
          genPrimitivePatPat :: (String, [Expr]) -> EvalM (PrimitivePatPattern, [IExpr])
          genPrimitivePatPat (name, matchers) = do
            patterns' <- mapM (const $ return PPPatVar) matchers
            matchers' <- mapM desugar matchers
            return (PPInductivePat name patterns', matchers')

          genPrimitiveDataPat :: (String, [Expr]) -> EvalM (IPrimitiveDataPattern, [IExpr])
          genPrimitiveDataPat (name, patterns) = do
            patterns' <- mapM (const fresh) patterns
            return (PDInductivePat (capitalize name) $ map (PDPatVar . stringToVar) patterns', map IVarExpr patterns')

          capitalize :: String -> String
          capitalize (x:xs) = toUpper x : xs


      genSomethingClause :: EvalM (PrimitivePatPattern, IExpr, [(IPrimitiveDataPattern, IExpr)])
      genSomethingClause =
        return (PPPatVar, ITupleExpr [IConstantExpr SomethingExpr], [(PDPatVar (stringToVar "tgt"), ICollectionExpr [IVarExpr "tgt"])])

      matchingSuccess :: IExpr
      matchingSuccess = ICollectionExpr [ITupleExpr []]

      matchingFailure :: IExpr
      matchingFailure = ICollectionExpr []

desugar (MatchAllLambdaExpr matcher clauses) = do
  name <- fresh
  ILambdaExpr Nothing [name] <$>
    desugar (MatchAllExpr BFSMode (VarExpr name) matcher clauses)

desugar (MatchLambdaExpr matcher clauses) = do
  name <- fresh
  ILambdaExpr Nothing [name] <$>
    desugar (MatchExpr BFSMode (VarExpr name) matcher clauses)

-- TODO: Allow nested MultiSubscript and MultiSuperscript
desugar (IndexedExpr b expr indices) =
  case indices of
    [MultiSubscript x y] ->
      case (x, y) of
        (IndexedExpr b1 e1 [n1], IndexedExpr _ _ [n2]) ->
          desugarMultiScript ISubrefsExpr b1 e1 n1 n2
        _ -> throwError $ Default "Index should be IndexedExpr for multi subscript"
    [MultiSuperscript x y] ->
      case (x, y) of
        (IndexedExpr b1 e1 [n1], IndexedExpr _ _ [n2]) ->
          desugarMultiScript ISuprefsExpr b1 e1 n1 n2
        _ -> throwError $ Default "Index should be IndexedExpr for multi superscript"
    _ -> IIndexedExpr b <$> desugar expr <*> mapM desugarIndex indices
  where
    desugarMultiScript refExpr b1 e1 n1 n2 = do
      k     <- fresh
      n1'   <- desugar (extractIndexExpr n1)
      n2'   <- desugar (extractIndexExpr n2)
      e1'   <- desugar e1
      expr' <- desugar expr
      return $ refExpr b expr' (makeIApply "map"
                                           [ILambdaExpr Nothing [k] (IIndexedExpr b1 e1' [Sub (IVarExpr k)]),
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

-- Desugar of LambdaExpr takes place in 2 stages.
-- * LambdaExpr -> LambdaExpr'  : Desugar pattern matches at the arg positions
-- * LambdaExpr' -> ILambdaExpr : Desugar ScalarArg and InvertedScalarArg
desugar (LambdaExpr args expr) = do
  (args', expr') <- foldrM desugarArg ([], expr) args
  desugar $ LambdaExpr' args' expr'
  where
    desugarArg :: Arg ArgPattern -> ([Arg String], Expr) -> EvalM ([Arg String], Expr)
    desugarArg (TensorArg x) (args, expr) = do
      (var, expr') <- desugarArgPat x expr
      return (TensorArg var : args, expr')
    desugarArg (ScalarArg x) (args, expr) = do
      (var, expr') <- desugarArgPat x expr
      return (ScalarArg var : args, expr')
    desugarArg (InvertedScalarArg x) (args, expr) = do
      (var, expr') <- desugarArgPat x expr
      return (InvertedScalarArg var : args, expr')

    -- Desugar argument patterns. Examples:
    -- \$(%x, %y) -> expr   ==> \$tmp -> let (tmp1, tmp2) := tmp in (\%x %y -> expr) tmp1 tmp2
    -- \(x, (y, z)) -> expr ==> \tmp  -> let (tmp1, tmp2) := tmp in (\x (y, z) -> expr) tmp1 tmp2
    -- \%($x :: xs) -> expr ==> \%tmp -> let (tmp1 :: xs) := tmp in (\$x %xs -> expr) tmp1 tmp2
    desugarArgPat :: ArgPattern -> Expr -> EvalM (String, Expr)
    desugarArgPat APWildCard expr = do
      tmp <- fresh
      return (tmp, LetExpr [Bind PDWildCard (VarExpr tmp)] expr)
    desugarArgPat (APPatVar var) expr = return (var, expr)
    desugarArgPat (APTuplePat args) expr = do
      tmp  <- fresh
      tmps <- mapM (const fresh) args
      return (tmp, LetExpr [Bind (PDTuplePat (map PDPatVar tmps)) (VarExpr tmp)]
                     (ApplyExpr (LambdaExpr args expr) (map VarExpr tmps)))
    desugarArgPat (APInductivePat ctor args) expr = do
      tmp  <- fresh
      tmps <- mapM (const fresh) args
      return (tmp, LetExpr [Bind (PDInductivePat ctor (map PDPatVar tmps)) (VarExpr tmp)]
                     (ApplyExpr (LambdaExpr args expr) (map VarExpr tmps)))
    desugarArgPat APEmptyPat expr = do
      tmp <- fresh
      return (tmp, LetExpr [Bind PDEmptyPat (VarExpr tmp)] expr)
    desugarArgPat (APConsPat arg1 arg2) expr = do
      tmp  <- fresh
      tmp1 <- fresh
      tmp2 <- fresh
      return (tmp, LetExpr [Bind (PDConsPat (PDPatVar tmp1) (PDPatVar tmp2)) (VarExpr tmp)]
                     (ApplyExpr (LambdaExpr [arg1, TensorArg arg2] expr) [VarExpr tmp1, VarExpr tmp2]))
    desugarArgPat (APSnocPat arg1 arg2) expr = do
      tmp  <- fresh
      tmp1 <- fresh
      tmp2 <- fresh
      return (tmp, LetExpr [Bind (PDSnocPat (PDPatVar tmp1) (PDPatVar tmp2)) (VarExpr tmp)]
                     (ApplyExpr (LambdaExpr [TensorArg arg1, arg2] expr) [VarExpr tmp1, VarExpr tmp2]))

desugar (LambdaExpr' names expr) = do
  let (args', expr') = foldr desugarInvertedArgs ([], expr) names
  expr' <- desugar expr'
  return $ ILambdaExpr Nothing args' expr'
  where
    desugarInvertedArgs :: Arg String -> ([String], Expr) -> ([String], Expr)
    desugarInvertedArgs (TensorArg x) (args, expr) = (x : args, expr)
    desugarInvertedArgs (ScalarArg x) (args, expr) =
      (x : args,
       TensorMapExpr (LambdaExpr' [TensorArg x] expr) (VarExpr x))
    desugarInvertedArgs (InvertedScalarArg x) (args, expr) =
      (x : args,
       TensorMapExpr (LambdaExpr' [TensorArg x] expr) (FlipIndicesExpr (VarExpr x)))

desugar (MemoizedLambdaExpr names expr) =
  IMemoizedLambdaExpr names <$> desugar expr

desugar (CambdaExpr name expr) =
  ICambdaExpr name <$> desugar expr

desugar (PatternFunctionExpr names pattern) =
  IPatternFunctionExpr names <$> desugarPattern pattern

desugar (IfExpr expr0 expr1 expr2) =
  IIfExpr <$> desugar expr0 <*> desugar expr1 <*> desugar expr2

desugar (LetExpr binds expr) =
  ILetExpr <$> desugarBindings binds <*> desugar expr

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

desugar (PrefixExpr "-" expr) = do
  expr' <- desugar expr
  return $ makeIApply "*" [IConstantExpr (IntegerExpr (-1)), expr']
desugar (PrefixExpr "!" (ApplyExpr expr args)) =
  IWedgeApplyExpr <$> desugar expr <*> mapM desugar args
desugar (PrefixExpr "'" expr) = IQuoteExpr <$> desugar expr
desugar (PrefixExpr "`" expr) = IQuoteSymbolExpr <$> desugar expr
desugar (PrefixExpr op _) = fail ("Unknown prefix " ++ op)

desugar (InfixExpr op expr1 expr2) | isWedge op =
  (\x y -> IWedgeApplyExpr (IVarExpr (repr op)) [x, y])
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
    desugar (VarExpr (repr op))
desugar (SectionExpr op Nothing Nothing) = do
  x <- fresh
  y <- fresh
  ILambdaExpr Nothing [x, y] <$> desugar (InfixExpr op (VarExpr x) (VarExpr y))

desugar (SectionExpr op Nothing (Just expr2)) = do
  x <- fresh
  ILambdaExpr Nothing [x] <$> desugar (InfixExpr op (VarExpr x) expr2)

desugar (SectionExpr op (Just expr1) Nothing) = do
  y <- fresh
  ILambdaExpr Nothing [y] <$> desugar (InfixExpr op expr1 (VarExpr y))

desugar SectionExpr{} = throwError $ Default "Cannot reach here: section with both arguments"

desugar (SeqExpr expr0 expr1) =
  ISeqExpr <$> desugar expr0 <*> desugar expr1

desugar (GenerateTensorExpr fnExpr sizeExpr) =
  IGenerateTensorExpr <$> desugar fnExpr <*> desugar sizeExpr

desugar (TensorContractExpr tExpr) =
  ITensorContractExpr <$> desugar tExpr

desugar (TensorMapExpr (LambdaExpr' [x] (TensorMapExpr (LambdaExpr' [y] expr) b)) a) =
  desugar (TensorMap2Expr (LambdaExpr' [x, y] expr) a b)
desugar (TensorMapExpr (LambdaExpr [x] (TensorMapExpr (LambdaExpr [y] expr) b)) a) =
  desugar (TensorMap2Expr (LambdaExpr [x, y] expr) a b)

desugar (TensorMapExpr fnExpr tExpr) =
  ITensorMapExpr <$> desugar fnExpr <*> desugar tExpr

desugar (TensorMap2Expr fnExpr t1Expr t2Expr) =
  ITensorMap2Expr <$> desugar fnExpr <*> desugar t1Expr <*> desugar t2Expr

desugar (TransposeExpr vars expr) =
  ITransposeExpr <$> desugar vars <*> desugar expr

desugar (FlipIndicesExpr expr) =
  IFlipIndicesExpr <$> desugar expr

desugar (ApplyExpr expr args) =
  IApplyExpr <$> desugar expr <*> mapM desugar args

desugar (CApplyExpr expr0 expr1) =
  ICApplyExpr <$> desugar expr0 <*> desugar expr1

desugar FreshVarExpr = do
  id <- fresh
  return $ IVarExpr (":::" ++ id)

desugar (MatcherExpr patternDefs) =
  IMatcherExpr <$> mapM desugarPatternDef patternDefs

desugar (AnonParamExpr n) = return $ IVarExpr ('%' : show n)

desugar (AnonParamFuncExpr n expr) = do
  let args = map (\n -> '%' : show n) [1..n]
  lambda <- desugar $ LambdaExpr' (map TensorArg args) expr
  return $ ILetRecExpr [(PDPatVar (stringToVar "%0"), lambda)] (IVarExpr "%0")

desugar (AnonTupleParamFuncExpr 1 expr) = do
  lambda <- desugar $ LambdaExpr' [TensorArg "%1"] expr
  return $ ILetRecExpr [(PDPatVar (stringToVar "%0"), lambda)] (IVarExpr "%0")
desugar (AnonTupleParamFuncExpr n expr) = do
  let args = map (\n -> '%' : show n) [1..n]
  lambda <- desugar $
    LambdaExpr [TensorArg (APTuplePat $ map (TensorArg . APPatVar) args)] expr
  return $ ILetRecExpr [(PDPatVar (stringToVar "%0"), lambda)] (IVarExpr "%0")

desugar (AnonListParamFuncExpr n expr) = do
  let args' = map (\n -> TensorArg (APPatVar ('%' : show n))) [1..n]
  let args = foldr APConsPat APEmptyPat args'
  lambda <- desugar $ LambdaExpr [TensorArg args] expr
  return $ ILetRecExpr [(PDPatVar (stringToVar "%0"), lambda)] (IVarExpr "%0")

desugar (QuoteExpr expr) =
  IQuoteExpr <$> desugar expr

desugar (QuoteSymbolExpr expr) =
  IQuoteSymbolExpr <$> desugar expr

desugar (WedgeApplyExpr expr args) =
  IWedgeApplyExpr <$> desugar expr <*> mapM desugar args

desugar (FunctionExpr args) = return $ IFunctionExpr args

desugarIndex :: IndexExpr Expr -> EvalM (Index IExpr)
desugarIndex (Subscript e)    = Sub <$> desugar e
desugarIndex (Superscript e)  = Sup <$> desugar e
desugarIndex (SupSubscript e) = SupSub <$> desugar e
desugarIndex (Userscript e)   = User <$> desugar e
desugarIndex _                = undefined

desugarPattern :: Pattern -> EvalM IPattern
desugarPattern pat =
  case collectName pat of
    []    -> desugarPattern' pat
    names -> ILetPat (map makeBinding names) <$> desugarPattern' pat
 where
   collectNames :: [Pattern] -> [String]
   collectNames pats = foldl union [] (map collectName pats)

   collectName :: Pattern -> [String]
   collectName (ForallPat pat1 pat2)                           = collectName pat1 `union` collectName pat2
   collectName (InfixPat _ pat1 pat2)                          = collectName pat1 `union` collectName pat2
   collectName (NotPat pat)                                    = collectName pat
   collectName (AndPat pat1 pat2)                              = collectName pat1 `union` collectName pat2
   collectName (OrPat pat1 pat2)                               = collectName pat1 `union` collectName pat2
   collectName (TuplePat pats)                                 = collectNames pats
   collectName (InductiveOrPApplyPat _ pats)                   = collectNames pats
   collectName (InductivePat _ pats)                           = collectNames pats
   collectName (PApplyPat _ pats)                              = collectNames pats
   collectName (DApplyPat _ pats)                              = collectNames pats
   collectName (LoopPat _ (LoopRange _ _ endNumPat) pat1 pat2) = collectName endNumPat `union` collectName pat1 `union` collectName pat2
   collectName (LetPat _ pat)                                  = collectName pat
   collectName (IndexedPat (PatVar var) _)                     = [var]
   collectName _                                               = []

   makeBinding :: String -> IBindingExpr
   makeBinding var = (PDPatVar (stringToVar var), IHashExpr [])

desugarPattern' :: Pattern -> EvalM IPattern
desugarPattern' WildCard        = return IWildCard
desugarPattern' ContPat         = return IContPat
desugarPattern' SeqNilPat       = return ISeqNilPat
desugarPattern' LaterPatVar     = return ILaterPatVar
desugarPattern' (VarPat v)      = return (IVarPat v)
desugarPattern' (PatVar var)    = return (IPatVar var)
desugarPattern' (ValuePat expr) = IValuePat <$> desugar expr
desugarPattern' (PredPat expr)  = IPredPat <$> desugar expr
desugarPattern' (NotPat pat)       = INotPat <$> desugarPattern' pat
desugarPattern' (AndPat pat1 pat2) = IAndPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (OrPat pat1 pat2)  = IOrPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (ForallPat pat1 pat2) = IForallPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (InfixPat Op{ repr = "&" } pat1 pat2) =
  IAndPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (InfixPat Op{ repr = "|" } pat1 pat2) =
  IOrPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (InfixPat Op{ repr = f } pat1 pat2) =
  (\x y -> IInductivePat f [x, y]) <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (TuplePat pats) = ITuplePat <$> mapM desugarPattern' pats
desugarPattern' (InductiveOrPApplyPat name pats) = IInductiveOrPApplyPat name <$> mapM desugarPattern' pats
desugarPattern' (InductivePat name pats) = IInductivePat name <$> mapM desugarPattern' pats
desugarPattern' (IndexedPat pat exprs) = IIndexedPat <$> desugarPattern' pat <*> mapM desugar exprs
desugarPattern' (PApplyPat expr pats) = IPApplyPat <$> desugar expr <*> mapM desugarPattern' pats
desugarPattern' (DApplyPat pat pats) = IDApplyPat <$> desugarPattern' pat <*> mapM desugarPattern' pats
desugarPattern' (LoopPat name range pat1 pat2) = ILoopPat name <$> desugarLoopRange range <*> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (LetPat binds pat) = ILetPat <$> desugarBindings binds <*> desugarPattern' pat
desugarPattern' (SeqConsPat pat1 pat2) = ISeqConsPat <$> desugarPattern' pat1 <*> desugarPattern' pat2

desugarLoopRange :: LoopRange -> EvalM ILoopRange
desugarLoopRange (LoopRange sExpr eExpr pat) =
  ILoopRange <$> desugar sExpr <*> desugar eExpr <*> desugarPattern' pat

desugarBindings :: [BindingExpr] -> EvalM [IBindingExpr]
desugarBindings = mapM desugarBinding
  where
    desugarBinding (Bind name expr) = do
      let name' = fmap stringToVar name
      expr' <- desugar expr
      case (name, expr') of
        (PDPatVar var, ILambdaExpr Nothing args body) ->
          return (name', ILambdaExpr (Just var) args body)
        _ -> return (name', expr')
    desugarBinding (BindWithIndices vwi expr) = do
      (var, iexpr) <- desugarDefineWithIndices vwi expr
      return (PDPatVar var, iexpr)

desugarMatchClauses :: [MatchClause] -> EvalM [IMatchClause]
desugarMatchClauses = mapM (\(pat, expr) -> (,) <$> desugarPattern pat <*> desugar expr)

desugarPatternDef :: PatternDef -> EvalM IPatternDef
desugarPatternDef (pp, matcher, pds) =
  (pp,,) <$> desugar matcher <*> desugarPrimitiveDataMatchClauses pds

desugarPrimitiveDataMatchClauses :: [(PrimitiveDataPattern, Expr)] -> EvalM [(IPrimitiveDataPattern, IExpr)]
desugarPrimitiveDataMatchClauses = mapM (\(pd, expr) -> (fmap stringToVar pd,) <$> desugar expr)

desugarDefineWithIndices :: VarWithIndices -> Expr -> EvalM (Var, IExpr)
desugarDefineWithIndices var@(VarWithIndices name _) expr@(LambdaExpr _ _) = do
  expr' <- desugar expr
  case expr' of
    ILambdaExpr Nothing args body -> return (varWithIndicesToVar var, ILambdaExpr (Just name) args body)
    _                             -> return (varWithIndicesToVar var, expr')
desugarDefineWithIndices (VarWithIndices name is) expr = do
  let (isSubs, indexNames) = unzip $ concatMap extractSubSupIndex is
  expr <- if any isExtendedIndice is
             then desugarExtendedIndices is isSubs indexNames expr
             else return expr
  body <- desugar expr
  let indexNamesCollection = ICollectionExpr (map IVarExpr indexNames)
  let is' = map (\b -> if b then Sub Nothing else Sup Nothing) isSubs
  return (Var name is', IWithSymbolsExpr indexNames (ITransposeExpr indexNamesCollection body))

varWithIndicesToVar :: VarWithIndices -> Var
varWithIndicesToVar (VarWithIndices name is) = Var name (concatMap transVarIndex is)

transVarIndex :: VarIndex -> [Index (Maybe Var)]
transVarIndex (VSubscript x)        = [Sub (Just (stringToVar x))]
transVarIndex (VSuperscript x)      = [Sup (Just (stringToVar x))]
transVarIndex (VGroupScripts xs)    = concatMap transVarIndex xs
transVarIndex (VSymmScripts xs)     = concatMap transVarIndex xs
transVarIndex (VAntiSymmScripts xs) = concatMap transVarIndex xs

extractSubSupIndex :: VarIndex -> [(Bool, String)]
extractSubSupIndex (VSubscript x)        = [(True, x)]
extractSubSupIndex (VSuperscript x)      = [(False, x)]
extractSubSupIndex (VGroupScripts xs)    = concatMap extractSubSupIndex xs
extractSubSupIndex (VSymmScripts xs)     = concatMap extractSubSupIndex xs
extractSubSupIndex (VAntiSymmScripts xs) = concatMap extractSubSupIndex xs

desugarExtendedIndices :: [VarIndex] -> [Bool] -> [String] -> Expr -> EvalM Expr
desugarExtendedIndices indices isSubs indexNames tensorBody = do
  tensorName <- fresh
  tensorGenExpr <- f indices (VarExpr tensorName) [] []
  let indexFunctionExpr = LambdaExpr [TensorArg $ foldr APConsPat APEmptyPat (map (TensorArg . APPatVar) indexNames)] tensorGenExpr
  let genTensorExpr = GenerateTensorExpr indexFunctionExpr (makeApply "tensorShape" [VarExpr tensorName])
  let tensorIndices = zipWith (\isSub name -> if isSub then Subscript (VarExpr name) else Superscript (VarExpr name)) isSubs indexNames
  return $ LetExpr [Bind (PDPatVar tensorName) tensorBody] (IndexedExpr True genTensorExpr tensorIndices)
 where
  f :: [VarIndex] -> Expr -> [String] -> [BindingExpr] -> EvalM Expr
  f [] expr [] []       = return expr
  f [] expr [] bindings = return $ LetRecExpr bindings expr
  f [] expr signs bindings =
    return $ LetRecExpr bindings (makeApply "product" [CollectionExpr (map VarExpr signs ++ [expr])])
  f (index:indices) expr signs bindings = do
    (indices', signs', bindings') <- genBindings index
    let isSubs = subOrSupScripts index
    symbols <- mapM (const fresh) isSubs
    let is = zipWith (\x isSub -> (if isSub then Subscript else Superscript) (VarExpr x)) symbols isSubs
    f indices (IndexedExpr True expr is)
      (signs ++ signs') (bindings ++ bindings' ++ [Bind (foldr (PDConsPat . PDPatVar) PDEmptyPat symbols) indices'])

  subOrSupScripts :: VarIndex -> [Bool]
  subOrSupScripts VSubscript{}          = [True]
  subOrSupScripts VSuperscript{}        = [False]
  subOrSupScripts (VGroupScripts xs)    = concatMap subOrSupScripts xs
  subOrSupScripts (VSymmScripts xs)     = concatMap subOrSupScripts xs
  subOrSupScripts (VAntiSymmScripts xs) = concatMap subOrSupScripts xs

  genBindings :: VarIndex -> EvalM (Expr, [String], [BindingExpr])
  genBindings (VSubscript x)   = return (CollectionExpr [VarExpr x], [], [])
  genBindings (VSuperscript x) = return (CollectionExpr [VarExpr x], [], [])
  genBindings (VGroupScripts xs) = do
    (indices, signss, bindingss) <- unzip3 <$> mapM genBindings xs
    let newIndices =
          -- If indices are all CollectionExpr, we can calculate the concatenated result of them
          case allCollections indices of
            Just xs -> CollectionExpr xs
            Nothing -> makeApply "concat" [CollectionExpr indices]
    return (newIndices, concat signss, concat bindingss)
    where
      allCollections []                          = Just []
      allCollections (CollectionExpr xs : exprs) = (xs ++) <$> allCollections exprs
      allCollections _                           = Nothing
  genBindings (VSymmScripts xs) = do
    (indices, signss, bindingss) <- unzip3 <$> mapM genBindings xs
    let signs = concat signss
    let bindings = concat bindingss
    sortedCollectionName <- fresh
    let newBindings = bindings ++ [Bind (PDTuplePat [PDWildCard, PDPatVar sortedCollectionName]) (makeApply "sortWithSign" [CollectionExpr indices])]
    return (VarExpr sortedCollectionName, signs, newBindings)
  genBindings (VAntiSymmScripts xs) = do
    (indices, signss, bindingss) <- unzip3 <$> mapM genBindings xs
    let signs = concat signss
    let bindings = concat bindingss
    sortedCollectionName <- fresh
    signName <- fresh
    let newBindings = bindings ++ [Bind (PDTuplePat [PDPatVar signName, PDPatVar sortedCollectionName]) (makeApply "sortWithSign" [CollectionExpr indices])]
    return (VarExpr sortedCollectionName, signName : signs, newBindings)

--
-- Utils
--

extractIndexExpr :: IndexExpr a -> a
extractIndexExpr (Subscript x)    = x
extractIndexExpr (Superscript x)  = x
extractIndexExpr (SupSubscript x) = x
extractIndexExpr (Userscript x)   = x
extractIndexExpr _                = error "extractIndexExpr: Not supported"

isExtendedIndice :: VarIndex -> Bool
isExtendedIndice VSubscript{}       = False
isExtendedIndice VSuperscript{}     = False
isExtendedIndice (VGroupScripts xs) = isExtendedIndice (head xs)
isExtendedIndice _                  = True
