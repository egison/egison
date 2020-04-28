{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}

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
import           Language.Egison.IState (fresh, freshV)

desugarTopExpr :: EgisonTopExpr -> EgisonM EgisonTopExpr
desugarTopExpr (Define name expr)   = Define name <$> desugar expr
desugarTopExpr (DefineWithIndices (VarWithIndices name is) expr) = do
  body <- desugar expr
  let indexNames = map extractIndex is
  let indexNamesCollection = CollectionExpr (map (ElementExpr . stringToVarExpr) indexNames)
  return $ Define (Var name (map (const () <$>) is))
    (WithSymbolsExpr indexNames (TransposeExpr indexNamesCollection body))
desugarTopExpr (Redefine name expr) = Redefine name <$> desugar expr
desugarTopExpr (Test expr)          = Test <$> desugar expr
desugarTopExpr (Execute expr)       = Execute <$> desugar expr
desugarTopExpr expr                 = return expr

desugarExpr :: EgisonExpr -> EgisonM EgisonExpr
desugarExpr = desugar

desugar :: EgisonExpr -> EgisonM EgisonExpr
desugar (AlgebraicDataMatcherExpr patterns) = do
  matcherName <- freshV
  let matcherRef = VarExpr matcherName
  matcher <- genMatcherClauses patterns matcherRef
  return $ LetRecExpr [([matcherName], matcher)] matcherRef
    where
      genMatcherClauses :: [(String, [EgisonExpr])] ->  EgisonExpr -> EgisonM EgisonExpr
      genMatcherClauses patterns matcher = do
        main <- genMainClause patterns matcher
        body <- mapM genMatcherClause patterns
        footer <- genSomethingClause
        let clauses = [main] ++ body ++ [footer]
        return $ MatcherExpr clauses

      genMainClause :: [(String, [EgisonExpr])] -> EgisonExpr -> EgisonM (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
      genMainClause patterns matcher = do
        clauses <- genClauses patterns
        return (PPValuePat "val", TupleExpr []
               ,[(PDPatVar "tgt", MatchExpr BFSMode
                                            (TupleExpr [stringToVarExpr "val", stringToVarExpr "tgt"])
                                            (TupleExpr [matcher, matcher])
                                             clauses)])
        where
          genClauses :: [(String, [EgisonExpr])] -> EgisonM [MatchClause]
          genClauses patterns = (++) <$> mapM genClause patterns
                                     <*> pure [(TuplePat [WildCard, WildCard], matchingFailure)]

          genClause :: (String, [EgisonExpr]) -> EgisonM MatchClause
          genClause pattern = do
            (pat0, pat1) <- genMatchingPattern pattern
            return (TuplePat [pat0, pat1], matchingSuccess)

          genMatchingPattern :: (String, [EgisonExpr]) -> EgisonM (EgisonPattern, EgisonPattern)
          genMatchingPattern (name, patterns) = do
            names <- mapM (const freshV) patterns
            return (InductivePat name (map PatVar names),
                    InductivePat name (map (ValuePat . VarExpr) names))

      genMatcherClause :: (String, [EgisonExpr]) -> EgisonM (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
      genMatcherClause pattern = do
        (ppat, matchers) <- genPrimitivePatPat pattern
        (dpat, body)     <- genPrimitiveDataPat pattern
        return (ppat, TupleExpr matchers, [(dpat, CollectionExpr [ElementExpr . TupleExpr $ body]), (PDWildCard, matchingFailure)])

        where
          genPrimitivePatPat :: (String, [EgisonExpr]) -> EgisonM (PrimitivePatPattern, [EgisonExpr])
          genPrimitivePatPat (name, matchers) = do
            patterns' <- mapM (const $ return PPPatVar) matchers
            return (PPInductivePat name patterns', matchers)

          genPrimitiveDataPat :: (String, [EgisonExpr]) -> EgisonM (PrimitiveDataPattern, [EgisonExpr])
          genPrimitiveDataPat (name, patterns) = do
            patterns' <- mapM (const freshV) patterns
            return (PDInductivePat (capitalize name) $ map (PDPatVar . show) patterns', map VarExpr patterns')

          capitalize :: String -> String
          capitalize (x:xs) = toUpper x : xs


      genSomethingClause :: EgisonM (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
      genSomethingClause =
        return (PPPatVar, TupleExpr [SomethingExpr], [(PDPatVar "tgt", CollectionExpr [ElementExpr (stringToVarExpr "tgt")])])

      matchingSuccess :: EgisonExpr
      matchingSuccess = CollectionExpr [ElementExpr $ TupleExpr []]

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

desugar (PowerExpr expr1 expr2) =
  (\x y -> makeApply "**" [x, y]) <$> desugar expr1 <*> desugar expr2

desugar (InductiveDataExpr name exprs) =
  InductiveDataExpr name <$> mapM desugar exprs

desugar (TupleExpr exprs) =
  TupleExpr <$> mapM desugar exprs

desugar expr@(CollectionExpr []) = return expr

desugar (CollectionExpr (ElementExpr elm:inners)) = do
  elm' <- desugar elm
  CollectionExpr inners' <- desugar (CollectionExpr inners)
  return $ CollectionExpr (ElementExpr elm':inners')

desugar (CollectionExpr (SubCollectionExpr sub:inners)) = do
  sub' <- desugar sub
  CollectionExpr inners' <- desugar (CollectionExpr inners)
  return $ CollectionExpr (SubCollectionExpr sub':inners')

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

desugar (LetStarExpr binds expr) = do
  binds' <- desugarBindings binds
  expr' <- desugar expr
  return $ foldr (\bind ret -> LetExpr [bind] ret) expr' binds'

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

desugar (UnaryOpExpr "-" expr) =
  desugar (BinaryOpExpr mult (IntegerExpr (-1)) expr)
    where mult = findOpFrom "*" reservedExprInfix
desugar (UnaryOpExpr "!" (ApplyExpr expr1 expr2)) =
  WedgeApplyExpr <$> desugar expr1 <*> desugar expr2
desugar (UnaryOpExpr "'" expr) = QuoteExpr <$> desugar expr
desugar (UnaryOpExpr "`" expr) = QuoteSymbolExpr <$> desugar expr

desugar (BinaryOpExpr op expr1 expr2) | isWedge op =
  (\x y -> WedgeApplyExpr (stringToVarExpr (func op)) (TupleExpr [x, y]))
    <$> desugar expr1 <*> desugar expr2

desugar (BinaryOpExpr op expr1 expr2) | repr op == "::" =
  (\x y -> CollectionExpr [ElementExpr x, SubCollectionExpr y]) <$> desugar expr1 <*> desugar expr2
desugar (BinaryOpExpr op expr1 expr2) | repr op == "++" =
  (\x y -> CollectionExpr [SubCollectionExpr x, SubCollectionExpr y]) <$> desugar expr1 <*> desugar expr2
desugar (BinaryOpExpr op expr1 expr2) =
  (\x y -> makeApply (func op) [x, y]) <$> desugar expr1 <*> desugar expr2

-- section
--
-- If `op` is not a cambda, simply desugar it into the function
desugar (SectionExpr op Nothing Nothing) | not (isWedge op) =
  desugar (stringToVarExpr (func op))
desugar (SectionExpr op Nothing Nothing) = do
  x <- fresh
  y <- fresh
  desugar $ LambdaExpr [TensorArg x, TensorArg y]
                       (BinaryOpExpr op (stringToVarExpr x) (stringToVarExpr y))

desugar (SectionExpr op Nothing (Just expr2)) = do
  x <- fresh
  desugar $ LambdaExpr [TensorArg x]
                       (BinaryOpExpr op (stringToVarExpr x) expr2)

desugar (SectionExpr op (Just expr1) Nothing) = do
  y <- fresh
  desugar $ LambdaExpr [TensorArg y]
                       (BinaryOpExpr op expr1 (stringToVarExpr y))

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

desugar (AnonParamExpr n) = return $ AnonParamExpr n

desugar (AnonParamFuncExpr n expr) = do
  expr' <- desugar expr
  return $ LetRecExpr [([stringToVar "::0"], AnonParamFuncExpr n expr')] (stringToVarExpr "::0")

desugar (QuoteExpr expr) =
  QuoteExpr <$> desugar expr

desugar (QuoteSymbolExpr expr) =
  QuoteSymbolExpr <$> desugar expr

desugar (WedgeApplyExpr expr0 expr1) =
  WedgeApplyExpr <$> desugar expr0 <*> desugar expr1

desugar expr = return expr

desugarIndex :: Index EgisonExpr -> EgisonM (Index EgisonExpr)
desugarIndex index = traverse desugar index

desugarPattern :: EgisonPattern -> EgisonM EgisonPattern
desugarPattern pattern = LetPat (map makeBinding $ S.elems $ collectName pattern) <$> desugarPattern' (desugarPatternInfix pattern)
 where
   collectNames :: [EgisonPattern] -> Set String
   collectNames patterns = S.unions $ map collectName patterns

   collectName :: EgisonPattern -> Set String
   collectName (ForallPat pattern1 pattern2) = collectName pattern1 `S.union` collectName pattern2
   collectName (InfixPat _ pattern1 pattern2) = collectName pattern1 `S.union` collectName pattern2
   collectName (NotPat pattern)  = collectName pattern
   collectName (AndPat patterns) = collectNames patterns
   collectName (OrPat patterns)  = collectNames patterns
   collectName (TuplePat patterns) = collectNames patterns
   collectName (InductiveOrPApplyPat _ patterns) = collectNames patterns
   collectName (InductivePat _ patterns) = collectNames patterns
   collectName (PApplyPat _ patterns) = collectNames patterns
   collectName (DApplyPat _ patterns) = collectNames patterns
   collectName (LoopPat _ (LoopRange _ _ endNumPat) pattern1 pattern2) = collectName endNumPat `S.union` collectName pattern1 `S.union` collectName pattern2
   collectName (LetPat _ pattern) = collectName pattern
   collectName (IndexedPat (PatVar name) _) = S.singleton $ show name
   collectName (DivPat pattern1 pattern2) = collectName pattern1 `S.union` collectName pattern2
   collectName (PlusPat patterns) = collectNames patterns
   collectName (MultPat patterns) = collectNames patterns
   collectName (PowerPat pattern1 pattern2) = collectName pattern1 `S.union` collectName pattern2
   collectName _ = S.empty

   makeBinding :: String -> BindingExpr
   makeBinding name = ([stringToVar name], HashExpr [])

desugarPatternInfix :: EgisonPattern -> EgisonPattern
desugarPatternInfix (IndexedPat pat es) = IndexedPat (desugarPatternInfix pat) es
desugarPatternInfix (LetPat bindings pat) = LetPat bindings (desugarPatternInfix pat)
desugarPatternInfix (InfixPat Infix{ repr = "&" } pat1 pat2) =
  AndPat [desugarPatternInfix pat1, desugarPatternInfix pat2]
desugarPatternInfix (InfixPat Infix{ repr = "|" } pat1 pat2) =
  OrPat [desugarPatternInfix pat1, desugarPatternInfix pat2]
desugarPatternInfix (InfixPat Infix{ repr = "^" } pat1 pat2) =
  PowerPat (desugarPatternInfix pat1) (desugarPatternInfix pat2)
desugarPatternInfix (InfixPat Infix{ repr = "*" } pat1 pat2) =
  MultPat [desugarPatternInfix pat1, desugarPatternInfix pat2]
desugarPatternInfix (InfixPat Infix{ repr = "+" } pat1 pat2) =
  PlusPat [desugarPatternInfix pat1, desugarPatternInfix pat2]
desugarPatternInfix (InfixPat Infix{ func = f } pat1 pat2) =
  InductivePat f [desugarPatternInfix pat1, desugarPatternInfix pat2]
desugarPatternInfix (NotPat pat) = NotPat (desugarPatternInfix pat)
desugarPatternInfix (ForallPat pat1 pat2) =
  ForallPat (desugarPatternInfix pat1) (desugarPatternInfix pat2)
desugarPatternInfix (TuplePat pats) = TuplePat (map desugarPatternInfix pats)
desugarPatternInfix (InductivePat ctor pats) =
  InductivePat ctor (map desugarPatternInfix pats)
desugarPatternInfix (LoopPat name range pat1 pat2) =
  LoopPat name range (desugarPatternInfix pat1) (desugarPatternInfix pat2)
desugarPatternInfix (PApplyPat expr pats) =
  PApplyPat expr (map desugarPatternInfix pats)
desugarPatternInfix (InductiveOrPApplyPat name pats) =
  InductiveOrPApplyPat name (map desugarPatternInfix pats)
desugarPatternInfix (SeqConsPat pat1 pat2) =
  SeqConsPat (desugarPatternInfix pat1) (desugarPatternInfix pat2)
desugarPatternInfix (DApplyPat pat pats) =
  DApplyPat (desugarPatternInfix pat) (map desugarPatternInfix pats)
desugarPatternInfix (DivPat pat1 pat2) =
  DivPat (desugarPatternInfix pat1) (desugarPatternInfix pat2)
desugarPatternInfix (PlusPat pats) = PlusPat (map desugarPatternInfix pats)
desugarPatternInfix (MultPat pats) = MultPat (map desugarPatternInfix pats)
desugarPatternInfix (PowerPat pat1 pat2) =
  PowerPat (desugarPatternInfix pat1) (desugarPatternInfix pat2)
desugarPatternInfix pat = pat

desugarPattern' :: EgisonPattern -> EgisonM EgisonPattern
desugarPattern' (ValuePat expr) = ValuePat <$> desugar expr
desugarPattern' (PredPat expr) = PredPat <$> desugar expr
desugarPattern' (NotPat pattern) = NotPat <$> desugarPattern' pattern
desugarPattern' (ForallPat pattern1 pattern2) = ForallPat <$> desugarPattern' pattern1 <*> desugarPattern' pattern2
desugarPattern' (AndPat patterns) = AndPat <$> mapM desugarPattern' patterns
desugarPattern' (OrPat patterns)  =  OrPat <$> mapM desugarPattern' patterns
desugarPattern' (TuplePat patterns)  = TuplePat <$> mapM desugarPattern' patterns
desugarPattern' (InductiveOrPApplyPat name patterns) = InductiveOrPApplyPat name <$> mapM desugarPattern' patterns
desugarPattern' (InductivePat name patterns) = InductivePat name <$> mapM desugarPattern' patterns
desugarPattern' (IndexedPat pattern exprs) = IndexedPat <$> desugarPattern' pattern <*> mapM desugar exprs
desugarPattern' (PApplyPat expr patterns) = PApplyPat <$> desugar expr <*> mapM desugarPattern' patterns
desugarPattern' (DApplyPat pattern patterns) = DApplyPat <$> desugarPattern' pattern <*> mapM desugarPattern' patterns
desugarPattern' (LoopPat name range pattern1 pattern2) =  LoopPat name <$> desugarLoopRange range <*> desugarPattern' pattern1 <*> desugarPattern' pattern2
desugarPattern' (LetPat binds pattern) = LetPat <$> desugarBindings binds <*> desugarPattern' pattern
desugarPattern' (SeqConsPat pattern1 pattern2)  = SeqConsPat <$> desugarPattern' pattern1 <*> desugarPattern' pattern2
desugarPattern' (DivPat pattern1 pattern2) = do
  pat1' <- desugarPattern' pattern1
  pat2' <- desugarPattern' pattern2
  return $ InductivePat "div" [pat1', pat2']
desugarPattern' (PlusPat patterns) = do
  pats' <- mapM desugarPattern' (concatMap flatten patterns)
  case reverse pats' of
    [] -> return $ InductivePat "plus" [ValuePat (IntegerExpr 0)]
    lp:hps ->
      return $ InductivePat "plus" [foldr (\p acc -> InductivePat "cons" [p, acc]) lp (reverse hps)]
 where
   flatten (PlusPat xs) = concatMap flatten xs
   flatten pat          = [pat]
desugarPattern' (MultPat patterns) = do
  intPat:pats' <- mapM desugarPattern' (concatMap flatten patterns)
  case reverse pats' of
    [] -> return $ InductivePat "mult" [intPat, ValuePat (IntegerExpr 1)]
    lp:hps -> do
      let mono = foldr (\p acc -> case p of
                                    PowerPat p1 p2 -> InductivePat "ncons" [p1, p2, acc]
                                    _ -> InductivePat "cons" [p, acc])
                       (case lp of
                          PowerPat p1 p2 -> InductivePat "ncons" [p1, p2, ValuePat (IntegerExpr 1)]
                          _ -> lp)
                       (reverse hps)
      return $ InductivePat "mult" [intPat, mono]
 where
   flatten (MultPat xs) = concatMap flatten xs
   flatten pat          = [pat]
desugarPattern' (PowerPat pattern1 pattern2) = PowerPat <$> desugarPattern' pattern1 <*> desugarPattern' pattern2
desugarPattern' pattern = return pattern

desugarLoopRange :: LoopRange -> EgisonM LoopRange
desugarLoopRange (LoopRange sExpr eExpr pattern) =
  LoopRange <$> desugar sExpr <*> desugar eExpr <*> desugarPattern' pattern

desugarBindings :: [BindingExpr] -> EgisonM [BindingExpr]
desugarBindings = mapM (\(name, expr) -> (name,) <$> desugar expr)

desugarMatchClauses :: [MatchClause] -> EgisonM [MatchClause]
desugarMatchClauses = mapM (\(pattern, expr) -> (,) <$> desugarPattern pattern <*> desugar expr)

desugarPatternDef :: PatternDef -> EgisonM PatternDef
desugarPatternDef (pp, matcher, pds) =
  (pp,,) <$> desugar matcher <*> desugarPrimitiveDataMatchClauses pds

desugarPrimitiveDataMatchClauses :: [(PrimitiveDataPattern, EgisonExpr)] -> EgisonM [(PrimitiveDataPattern, EgisonExpr)]
desugarPrimitiveDataMatchClauses = mapM (\(pd, expr) -> (pd,) <$> desugar expr)

makeApply :: String -> [EgisonExpr] -> EgisonExpr
makeApply func args = ApplyExpr (stringToVarExpr func) (TupleExpr args)
