{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}

{- |
Module      : Language.Egison.Desugar
Copyright   : Satoshi Egi
Licence     : MIT

This module provide desugar functions.
-}

module Language.Egison.Desugar
    (
      desugarTopExpr
    , desugarExpr
    ) where

import           Data.Char             (toUpper)
import           Data.List             (span)
import           Data.Set              (Set)
import qualified Data.Set              as S

import           Language.Egison.AST
import           Language.Egison.Types

desugarTopExpr :: EgisonTopExpr -> EgisonM EgisonTopExpr
desugarTopExpr (Define name expr)   = Define name <$> desugar expr
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

desugar (ArrayRefExpr expr nums) =
  case nums of
    (TupleExpr nums') -> desugar $ IndexedExpr True expr (map Subscript nums')
    _ -> desugar $ IndexedExpr True expr [Subscript nums]

desugar (IndexedExpr b expr indices)
  | endWithThreeDots expr =
    case expr of
      VarExpr name ->
        let x = show name
         in desugar $ IndexedExpr False (stringToVarExpr $ take (length x - 3) x) indices
  | otherwise =
    case indices of
      [Subscript x, DotSubscript y] ->
        case (x, y) of
          (IntegerExpr _, IntegerExpr _) -> return $ SubrefsExpr b expr (makeApply "between" [x, y])
          (TupleExpr [IndexedExpr b1 e1 [n1]], TupleExpr [IndexedExpr b2 e2 [n2]]) -> do
            k <- fresh
            return $ SubrefsExpr b expr (makeApply "map"
                                                   [LambdaExpr [TensorArg k] (IndexedExpr b1 e1 [Subscript $ stringToVarExpr k]),
                                                    makeApply "between" [fromIndexToExpr n1, fromIndexToExpr n2]])
      [Superscript x, DotSupscript y] ->
        case (x, y) of
          (IntegerExpr _, IntegerExpr _) -> return $ SubrefsExpr b expr (makeApply "between" [x, y])
          (TupleExpr [IndexedExpr b1 e1 [n1]], TupleExpr [IndexedExpr b2 e2 [n2]]) -> do
            k <- fresh
            return $ SuprefsExpr b expr (makeApply "map"
                                                   [LambdaExpr [TensorArg k] (IndexedExpr b1 e1 [Subscript $ stringToVarExpr k]),
                                                    makeApply "between" [fromIndexToExpr n1, fromIndexToExpr n2]])
      _ -> IndexedExpr b <$> desugar expr <*> mapM desugarIndex indices
 where
  endWithThreeDots :: EgisonExpr -> Bool
  endWithThreeDots (VarExpr name) = take 3 (reverse (show name)) == "..."
  endWithThreeDots _              = False
  fromIndexToExpr :: Index EgisonExpr -> EgisonExpr
  fromIndexToExpr (Subscript a)    = a
  fromIndexToExpr (Superscript a)  = a
  fromIndexToExpr (SupSubscript a) = a

desugar (SubrefsExpr bool expr1 expr2) =
  SubrefsExpr bool <$> desugar expr1 <*> desugar expr2

desugar (SuprefsExpr bool expr1 expr2) =
  SuprefsExpr bool <$> desugar expr1 <*> desugar expr2

desugar (UserrefsExpr bool expr1 expr2) =
  UserrefsExpr bool <$> desugar expr1 <*> desugar expr2

desugar (PowerExpr expr1 expr2) =
  (\x y -> makeApply "**" [x, y]) <$> desugar expr1 <*> desugar expr2

desugar (ArrayBoundsExpr expr) =
  ArrayBoundsExpr <$> desugar expr

desugar (InductiveDataExpr name exprs) =
  InductiveDataExpr name <$> mapM desugar exprs

desugar (TupleExpr exprs) =
  TupleExpr <$> mapM desugar exprs

desugar expr@(CollectionExpr []) = return expr

desugar (CollectionExpr (ElementExpr elm:inners)) = do
  elm' <- desugar elm
  (CollectionExpr inners') <- desugar (CollectionExpr inners)
  return $ CollectionExpr (ElementExpr elm':inners')

desugar (CollectionExpr (SubCollectionExpr sub:inners)) = do
  sub' <- desugar sub
  (CollectionExpr inners') <- desugar (CollectionExpr inners)
  return $ CollectionExpr (SubCollectionExpr sub':inners')

desugar (VectorExpr exprs) =
  VectorExpr <$> mapM desugar exprs

desugar (TensorExpr nsExpr xsExpr supExpr subExpr) = do
  nsExpr' <- desugar nsExpr
  xsExpr' <- desugar xsExpr
  return $ TensorExpr nsExpr' xsExpr' supExpr subExpr

desugar (LambdaExpr names expr) = do
  let (rtnames, rhnames) = span (\case
                                    TensorArg _ -> True
                                    _           -> False) (reverse names)
  case rhnames of
    [] -> LambdaExpr names <$> desugar expr
    (InvertedScalarArg rhname:rhnames') -> do
      let (rtnames2, rhnames2) = span (const False) rhnames'
      case rhnames2 of
        [] -> desugar $ LambdaExpr (reverse rhnames' ++ [TensorArg rhname] ++ reverse rtnames)
                          (TensorMapExpr (LambdaExpr [TensorArg rhname] expr) (FlipIndicesExpr (stringToVarExpr rhname)))
        (ScalarArg rhname2:rhnames2') ->
          desugar $ LambdaExpr (reverse rhnames2' ++ [TensorArg rhname2] ++ rtnames2 ++ [TensorArg rhname] ++ reverse rtnames)
                      (TensorMap2Expr (LambdaExpr [TensorArg rhname2, TensorArg rhname] expr)
                                      (stringToVarExpr rhname2)
                                      (FlipIndicesExpr (stringToVarExpr rhname)))
        (InvertedScalarArg rhname2:rhnames2') ->
          desugar $ LambdaExpr (reverse rhnames2' ++ [TensorArg rhname2] ++ rtnames2 ++ [TensorArg rhname] ++ reverse rtnames)
                      (TensorMap2Expr (LambdaExpr [TensorArg rhname2, TensorArg rhname] expr)
                                      (FlipIndicesExpr (stringToVarExpr rhname2))
                                      (FlipIndicesExpr (stringToVarExpr rhname)))

    (ScalarArg rhname:rhnames') -> do
      let (rtnames2, rhnames2) = span (\case
                                          TensorArg _ -> True
                                          _           -> False) rhnames'
      case rhnames2 of
        [] -> desugar $ LambdaExpr (reverse rhnames' ++ [TensorArg rhname] ++ reverse rtnames)
                          (TensorMapExpr (LambdaExpr [TensorArg rhname] expr) (stringToVarExpr rhname))
        (ScalarArg rhname2:rhnames2') ->
          desugar $ LambdaExpr (reverse rhnames2' ++ [TensorArg rhname2] ++ rtnames2 ++ [TensorArg rhname] ++ reverse rtnames)
                      (TensorMap2Expr (LambdaExpr [TensorArg rhname2, TensorArg rhname] expr) (stringToVarExpr rhname2) (stringToVarExpr rhname))
        (InvertedScalarArg rhname2:rhnames2') ->
          desugar $ LambdaExpr (reverse rhnames2' ++ [TensorArg rhname2] ++ rtnames2 ++ [TensorArg rhname] ++ reverse rtnames)
                      (TensorMap2Expr (LambdaExpr [TensorArg rhname2, TensorArg rhname] expr) (FlipIndicesExpr (stringToVarExpr rhname2)) (stringToVarExpr rhname))

desugar (MemoizedLambdaExpr names expr) =
  MemoizedLambdaExpr names <$> desugar expr

desugar (MemoizeExpr memoizeBindings expr) = do
  memoizeBindings' <- mapM (\(x,y,z) -> do x' <- desugar x
                                           y' <- desugar y
                                           z' <- desugar z
                                           return (x',y',z'))
                           memoizeBindings
  expr' <- desugar expr
  return $ MemoizeExpr memoizeBindings' expr'

desugar (CambdaExpr name expr) =
  CambdaExpr name <$> desugar expr

desugar (ProcedureExpr names expr) =
  ProcedureExpr names <$> desugar expr

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
  (\x -> makeApply "neg" [x]) <$> desugar expr
desugar (UnaryOpExpr "!" (ApplyExpr expr1 expr2)) =
  WedgeApplyExpr <$> desugar expr1 <*> desugar expr2
desugar (UnaryOpExpr "'" expr) = QuoteExpr <$> desugar expr
desugar (UnaryOpExpr "`" expr) = QuoteSymbolExpr <$> desugar expr

desugar (BinaryOpExpr op expr1 expr2) | isWedge op = do
  (\x y -> WedgeApplyExpr (stringToVarExpr (func op)) (TupleExpr [x, y]))
    <$> desugar expr1 <*> desugar expr2

desugar (BinaryOpExpr op expr1 expr2) =
  (\x y -> makeApply (func op) [x, y]) <$> desugar expr1 <*> desugar expr2

desugar (SeqExpr expr0 expr1) =
  SeqExpr <$> desugar expr0 <*> desugar expr1

desugar (GenerateArrayExpr fnExpr (fstExpr, lstExpr)) = do
  fnExpr' <- desugar fnExpr
  fstExpr' <- desugar fstExpr
  lstExpr' <- desugar lstExpr
  return $ GenerateArrayExpr fnExpr' (fstExpr', lstExpr')

desugar (GenerateTensorExpr fnExpr sizeExpr) =
  GenerateTensorExpr <$> desugar fnExpr <*> desugar sizeExpr

desugar (TensorContractExpr fnExpr tExpr) =
  TensorContractExpr <$> desugar fnExpr <*> desugar tExpr

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

desugar (PartialVarExpr n) = return $ PartialVarExpr n

desugar (PartialExpr n expr) = do
  expr' <- desugar expr
  return $ LetRecExpr [([stringToVar "::0"], PartialExpr n expr')] (stringToVarExpr "::0")

desugar (QuoteExpr expr) =
  QuoteExpr <$> desugar expr

desugar (QuoteSymbolExpr expr) =
  QuoteSymbolExpr <$> desugar expr

desugar (WedgeApplyExpr expr0 expr1) =
  WedgeApplyExpr <$> desugar expr0 <*> desugar expr1

desugar expr = return expr

desugarIndex :: Index EgisonExpr -> EgisonM (Index EgisonExpr)
desugarIndex (Superscript expr)  = Superscript  <$> desugar expr
desugarIndex (Subscript expr)    = Subscript    <$> desugar expr
desugarIndex (SupSubscript expr) = SupSubscript <$> desugar expr
desugarIndex (Userscript expr)   = Userscript   <$> desugar expr

desugarPattern :: EgisonPattern -> EgisonM EgisonPattern
desugarPattern pattern = LetPat (map makeBinding $ S.elems $ collectName pattern) <$> desugarPattern' pattern
 where
   collectNames :: [EgisonPattern] -> Set String
   collectNames patterns = S.unions $ map collectName patterns

   collectName :: EgisonPattern -> Set String
   collectName (NotPat pattern) = collectName pattern
   collectName (LaterPat pattern) = collectName pattern
   collectName (AndPat patterns) = collectNames patterns
   collectName (TuplePat patterns) = collectNames patterns
   collectName (InductivePat _ patterns) = collectNames patterns
   collectName (PApplyPat _ patterns) = collectNames patterns
   collectName (DApplyPat _ patterns) = collectNames patterns
   collectName (LoopPat _ (LoopRange _ _ endNumPat) pattern1 pattern2) = collectName endNumPat `S.union` collectName pattern1 `S.union` collectName pattern2
   collectName (LetPat _ pattern) = collectName pattern
   collectName (IndexedPat (PatVar name) _) = S.singleton $ show name
   collectName (OrPat patterns) = collectNames patterns
   collectName (DivPat pattern1 pattern2) = collectName pattern1 `S.union` collectName pattern2
   collectName (PlusPat patterns) = collectNames patterns
   collectName (MultPat patterns) = collectNames patterns
   collectName (PowerPat pattern1 pattern2) = collectName pattern1 `S.union` collectName pattern2
   collectName _ = S.empty

   makeBinding :: String -> BindingExpr
   makeBinding name = ([stringToVar name], HashExpr [])

desugarPattern' :: EgisonPattern -> EgisonM EgisonPattern
desugarPattern' (ValuePat expr) = ValuePat <$> desugar expr
desugarPattern' (PredPat expr) = PredPat <$> desugar expr
desugarPattern' (NotPat pattern) = NotPat <$> desugarPattern' pattern
desugarPattern' (LaterPat pattern) = LaterPat <$> desugarPattern' pattern
desugarPattern' (AndPat patterns) = AndPat <$> mapM desugarPattern' patterns
desugarPattern' (OrPat patterns)  =  OrPat <$> mapM desugarPattern' patterns
desugarPattern' (TuplePat patterns)  = TuplePat <$> mapM desugarPattern' patterns
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
  pats' <- mapM desugarPattern' (concatMap f patterns)
  case reverse pats' of
    [] -> return $ InductivePat "plus" [ValuePat (IntegerExpr 0)]
    lp:hps ->
      return $ InductivePat "plus" [foldr (\p r -> InductivePat "cons" [p, r]) lp (reverse hps)]
 where
   f (PlusPat xs) = concatMap f xs
   f pat          = [pat]
desugarPattern' (MultPat (intPat:patterns)) = do
  intPat' <- desugarPattern' intPat
  pats' <- mapM desugarPattern' (concatMap f patterns)
  case reverse pats' of
    [] -> return $ InductivePat "mult" [intPat', ValuePat (IntegerExpr 1)]
    lp:hps ->
      return $ InductivePat "mult" [intPat',
                                    foldr (\p r -> case p of
                                                     PowerPat p1 p2 -> InductivePat "ncons" [p1, p2, r]
                                                     _ -> InductivePat "cons" [p, r])
                                          (case lp of
                                             PowerPat p1 p2 -> InductivePat "ncons" [p1, p2, ValuePat (IntegerExpr 1)]
                                             _ -> lp)
                                          (reverse hps)]
 where
   f (MultPat xs) = concatMap f xs
   f pat          = [pat]
desugarPattern' (PowerPat pattern1 pattern2) = PowerPat <$> desugarPattern' pattern1 <*> desugarPattern' pattern2
desugarPattern' pattern = return pattern

desugarLoopRange :: LoopRange -> EgisonM LoopRange
desugarLoopRange (LoopRange sExpr eExpr pattern) =
  LoopRange <$> desugar sExpr <*> desugar eExpr <*> desugarPattern' pattern

desugarBindings :: [BindingExpr] -> EgisonM [BindingExpr]
desugarBindings = mapM f
  where f (name, expr) = (name,) <$> desugar expr

desugarMatchClauses :: [MatchClause] -> EgisonM [MatchClause]
desugarMatchClauses = mapM f
  where f (pattern, expr) = (,) <$> desugarPattern pattern <*> desugar expr

desugarPatternDef :: PatternDef -> EgisonM PatternDef
desugarPatternDef (pp, matcher, pds) =
  (pp,,) <$> desugar matcher <*> desugarPrimitiveDataMatchClauses pds

desugarPrimitiveDataMatchClauses :: [(PrimitiveDataPattern, EgisonExpr)] -> EgisonM [(PrimitiveDataPattern, EgisonExpr)]
desugarPrimitiveDataMatchClauses = mapM f
  where f (pd, expr) = (pd,) <$> desugar expr

makeApply :: String -> [EgisonExpr] -> EgisonExpr
makeApply func args = ApplyExpr (stringToVarExpr func) (TupleExpr args)
