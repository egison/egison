{-# Language FlexibleInstances, GeneralizedNewtypeDeriving #-}

{- |
Module      : Language.Egison.Desugar
Copyright   : Satoshi Egi
Licence     : MIT

This module provide desugar functions.
-}

module Language.Egison.Desugar
    (
      DesugarM
    , runDesugarM
    , desugarTopExpr
    , desugarExpr
    , desugar
    ) where

import Control.Applicative (Applicative)
import Control.Applicative ((<$>), (<*>), (<*), (*>), pure)
import qualified Data.Sequence as Sq
import Data.Sequence (ViewL(..), (<|))
import qualified Data.Set as S
import Data.List (span)
import Data.Set (Set)
import Data.Char (toUpper)
import Control.Monad.Except
import Control.Monad.Reader
import Language.Egison.Types

type Subst = [(String, EgisonExpr)]

newtype DesugarM a = DesugarM { unDesugarM :: ReaderT Subst (ExceptT EgisonError Fresh) a }
  deriving (Functor, Applicative, Monad, MonadError EgisonError, MonadFresh, MonadReader Subst)

runDesugarM :: DesugarM a -> Fresh (Either EgisonError a)
runDesugarM = runExceptT . flip runReaderT [] . unDesugarM

desugarTopExpr :: EgisonTopExpr -> EgisonM EgisonTopExpr
desugarTopExpr (Define name expr) = do
  expr' <- liftEgisonM $ runDesugarM $ desugar expr
  return (Define name expr')
desugarTopExpr (Redefine name expr) = do
  expr' <- liftEgisonM $ runDesugarM $ desugar expr
  return (Redefine name expr')
desugarTopExpr (Test expr) = do
  expr' <- liftEgisonM $ runDesugarM $ desugar expr
  return (Test expr')
desugarTopExpr (Execute expr) = do
  expr' <- liftEgisonM $ runDesugarM $ desugar expr
  return (Execute expr')
desugarTopExpr expr = return expr

desugarExpr :: EgisonExpr -> EgisonM EgisonExpr
desugarExpr expr = liftEgisonM $ runDesugarM $ desugar expr

desugar :: EgisonExpr -> DesugarM EgisonExpr
desugar (AlgebraicDataMatcherExpr patterns) = do
  matcherName <- freshV
  matcherRef <- return $ VarExpr matcherName
  matcher <- genMatcherClauses patterns matcherRef
  return $ LetRecExpr [([matcherName], matcher)] matcherRef
    where
      genMatcherClauses :: [(String, [EgisonExpr])] ->  EgisonExpr -> DesugarM EgisonExpr
      genMatcherClauses patterns matcher = do
        main <- genMainClause patterns matcher
        body <- mapM genMatcherClause patterns
        footer <- genSomethingClause
        clauses <- return $ [main] ++ body ++ [footer]
        return $ MatcherDFSExpr clauses
        
      genMainClause :: [(String, [EgisonExpr])] -> EgisonExpr -> DesugarM (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
      genMainClause patterns matcher = do
        clauses <- genClauses patterns
        return (PPValuePat "val", TupleExpr []
               ,[(PDPatVar "tgt", (MatchExpr (TupleExpr [(VarExpr $ stringToVar "val"), (VarExpr $ stringToVar "tgt")]) 
                                             (TupleExpr [matcher, matcher]) 
                                             clauses))])
        where
          genClauses :: [(String, [EgisonExpr])] -> DesugarM [MatchClause]
          genClauses patterns = (++) <$> mapM genClause patterns
                                     <*> pure [((TuplePat [WildCard, WildCard]), matchingFailure)]
          
          genClause :: (String, [EgisonExpr]) -> DesugarM MatchClause
          genClause pattern = do
            (pat0, pat1) <- genMatchingPattern pattern
            return (TuplePat [pat0, pat1], matchingSuccess)
        
          genMatchingPattern :: (String, [EgisonExpr]) -> DesugarM (EgisonPattern, EgisonPattern)
          genMatchingPattern (name, patterns) = do
            names <- mapM (const freshV) patterns
            return $ ((InductivePat name (map PatVar names))  
                     ,(InductivePat name (map (ValuePat . VarExpr) names)))
          
      genMatcherClause :: (String, [EgisonExpr]) -> DesugarM (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
      genMatcherClause pattern = do
        (ppat, matchers) <- genPrimitivePatPat pattern
        (dpat, body)     <- genPrimitiveDataPat pattern
        return (ppat, TupleExpr matchers, [(dpat, CollectionExpr [ElementExpr . TupleExpr $ body]), (PDWildCard, matchingFailure)])
        
        where
          genPrimitivePatPat :: (String, [EgisonExpr]) -> DesugarM (PrimitivePatPattern, [EgisonExpr])
          genPrimitivePatPat (name, matchers) = do
            patterns' <- mapM (const $ return PPPatVar) matchers
            return (PPInductivePat name patterns', matchers)
          
          genPrimitiveDataPat :: (String, [EgisonExpr]) -> DesugarM (PrimitiveDataPattern, [EgisonExpr])
          genPrimitiveDataPat (name, patterns) = do
            patterns' <- mapM (const freshV) patterns 
            return (PDInductivePat (capitalize name) $ map PDPatVar $ map show patterns', map VarExpr patterns')

          capitalize :: String -> String
          capitalize (x:xs) = toUpper x : xs
                                            
      
      genSomethingClause :: DesugarM (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
      genSomethingClause = 
        return (PPPatVar, (TupleExpr [SomethingExpr]), [(PDPatVar "tgt", CollectionExpr [ElementExpr (VarExpr $ stringToVar "tgt")])])
    
      matchingSuccess :: EgisonExpr
      matchingSuccess = CollectionExpr [ElementExpr $ TupleExpr []]

      matchingFailure :: EgisonExpr
      matchingFailure = CollectionExpr []

desugar (MatchAllLambdaExpr matcher clause) = do
  name <- fresh
  desugar $ LambdaExpr [TensorArg name] (MatchAllExpr (VarExpr $ stringToVar name) matcher clause)

desugar (MatchLambdaExpr matcher clauses) = do
  name <- fresh
  desugar $ LambdaExpr [TensorArg name] (MatchExpr (VarExpr $ stringToVar name) matcher clauses)

desugar (ArrayRefExpr expr nums) =
  case nums of
    (TupleExpr nums') -> desugar $ IndexedExpr True expr (map Subscript nums')
    _ -> desugar $ IndexedExpr True expr [Subscript nums]

desugar (IndexedExpr b expr indices)
  | endWithThreeDots expr = case expr of
                              (VarExpr name) -> let x = show name in desugar $ IndexedExpr False (VarExpr $ stringToVar $ take ((length x)-3) x) indices
  | otherwise = case indices of
                 [MultiSubscript x y] -> case (x, y) of
                                           (IntegerExpr _, IntegerExpr _) -> return $ SubrefsExpr b expr (ApplyExpr (VarExpr $ stringToVar "between") (TupleExpr [x, y]))
                                           (TupleExpr [IndexedExpr b1 e1 [n1]], TupleExpr [IndexedExpr b2 e2 [n2]]) -> do
                                             k <- fresh
                                             return $ SubrefsExpr b expr (ApplyExpr (VarExpr $ stringToVar "map")
                                                                                    (TupleExpr [(LambdaExpr [TensorArg k] (IndexedExpr b1 e1 [(Subscript $ VarExpr $ stringToVar k)])),
                                                                                                (ApplyExpr (VarExpr $ stringToVar "between") (TupleExpr [(fromIndexToExpr n1), (fromIndexToExpr n2)]))]))
                 [MultiSuperscript x y] -> case (x, y) of
                                             (IntegerExpr _, IntegerExpr _) -> return $ SubrefsExpr b expr (ApplyExpr (VarExpr $ stringToVar "between") (TupleExpr [x, y]))
                                             (TupleExpr [IndexedExpr b1 e1 [n1]], TupleExpr [IndexedExpr b2 e2 [n2]]) -> do    
                                               k <- fresh
                                               return $ SuprefsExpr b expr (ApplyExpr (VarExpr $ stringToVar "map")
                                                                                      (TupleExpr [(LambdaExpr [TensorArg k] (IndexedExpr b1 e1 [(Subscript $ VarExpr $ stringToVar k)])),
                                                                                                  (ApplyExpr (VarExpr $ stringToVar "between") (TupleExpr [(fromIndexToExpr n1), (fromIndexToExpr n2)]))]))
                 _ -> IndexedExpr b <$> desugar expr <*> (mapM desugarIndex indices)
 where
  endWithThreeDots :: EgisonExpr -> Bool
  endWithThreeDots (VarExpr name) = (take 3 $ reverse (show name)) == "..."
  endWithThreeDots _ = False
  fromIndexToExpr :: Index EgisonExpr -> EgisonExpr
  fromIndexToExpr (Subscript a) = a
  fromIndexToExpr (Superscript a) = a
  fromIndexToExpr (SupSubscript a) = a

desugar (SubrefsExpr bool expr1 expr2) = 
  SubrefsExpr bool <$> desugar expr1 <*> desugar expr2

desugar (SuprefsExpr bool expr1 expr2) = 
  SuprefsExpr bool <$> desugar expr1 <*> desugar expr2

desugar (PowerExpr expr1 expr2) = do
  expr1' <- desugar expr1
  expr2' <- desugar expr2
  return $ ApplyExpr (VarExpr $ stringToVar "**") (TupleExpr [expr1', expr2'])

desugar (ArrayBoundsExpr expr) = do
  expr' <- desugar expr
  return $ ArrayBoundsExpr expr'

desugar (InductiveDataExpr name exprs) = do 
  exprs' <- mapM desugar exprs
  return $ InductiveDataExpr name exprs'

desugar (TupleExpr exprs) = do
  exprs' <- mapM desugar exprs
  return $ TupleExpr exprs'

desugar expr@(CollectionExpr []) = return expr

desugar (CollectionExpr ((ElementExpr elm):inners)) = do
      elm' <- desugar elm
      (CollectionExpr inners') <- desugar (CollectionExpr inners)
      return $ CollectionExpr (ElementExpr elm':inners')

desugar (CollectionExpr ((SubCollectionExpr sub):inners)) = do
      sub' <- desugar sub
      (CollectionExpr inners') <- desugar (CollectionExpr inners)
      return $ CollectionExpr (SubCollectionExpr sub':inners')

desugar (VectorExpr exprs) = do
  exprs' <- mapM desugar exprs
  return $ VectorExpr exprs'

desugar (TensorExpr nsExpr xsExpr supExpr subExpr) = do
  nsExpr' <- desugar nsExpr
  xsExpr' <- desugar xsExpr
  return $ TensorExpr nsExpr' xsExpr' supExpr subExpr

desugar (LambdaExpr names expr) = do
  let (rtnames, rhnames) = span (\name -> case name of
                                            TensorArg _ -> True
                                            _ -> False) (reverse names)
  case rhnames of
    [] -> do expr' <- desugar expr
             return $ LambdaExpr names expr'

    (InvertedScalarArg rhname:rhnames') -> do
      let (rtnames2, rhnames2) = span (\name -> case name of
                                                _ -> False) rhnames'
      case rhnames2 of
        [] -> desugar $ LambdaExpr (reverse rhnames' ++ [TensorArg rhname] ++ reverse rtnames)
                          (TensorMapExpr (LambdaExpr [TensorArg rhname] expr) (FlipIndicesExpr (VarExpr $ stringToVar rhname)))
        (ScalarArg rhname2:rhnames2') ->
          desugar $ LambdaExpr (reverse rhnames2' ++ [TensorArg rhname2] ++ rtnames2 ++ [TensorArg rhname] ++ reverse rtnames)
                      (TensorMap2Expr (LambdaExpr [TensorArg rhname2, TensorArg rhname] expr) (VarExpr $ stringToVar rhname2) (FlipIndicesExpr (VarExpr $ stringToVar rhname)))
        (InvertedScalarArg rhname2:rhnames2') ->
          desugar $ LambdaExpr (reverse rhnames2' ++ [TensorArg rhname2] ++ rtnames2 ++ [TensorArg rhname] ++ reverse rtnames)
                      (TensorMap2Expr (LambdaExpr [TensorArg rhname2, TensorArg rhname] expr) (FlipIndicesExpr (VarExpr $ stringToVar rhname2)) (FlipIndicesExpr (VarExpr $ stringToVar rhname)))

    (ScalarArg rhname:rhnames') -> do
      let (rtnames2, rhnames2) = span (\name -> case name of
                                                TensorArg _ -> True
                                                _ -> False) rhnames'
      case rhnames2 of
        [] -> desugar $ LambdaExpr (reverse rhnames' ++ [TensorArg rhname] ++ reverse rtnames)
                          (TensorMapExpr (LambdaExpr [TensorArg rhname] expr) (VarExpr $ stringToVar rhname))
        (ScalarArg rhname2:rhnames2') ->
          desugar $ LambdaExpr (reverse rhnames2' ++ [TensorArg rhname2] ++ rtnames2 ++ [TensorArg rhname] ++ reverse rtnames)
                      (TensorMap2Expr (LambdaExpr [TensorArg rhname2, TensorArg rhname] expr) (VarExpr $ stringToVar rhname2) (VarExpr $ stringToVar rhname))
        (InvertedScalarArg rhname2:rhnames2') ->
          desugar $ LambdaExpr (reverse rhnames2' ++ [TensorArg rhname2] ++ rtnames2 ++ [TensorArg rhname] ++ reverse rtnames)
                      (TensorMap2Expr (LambdaExpr [TensorArg rhname2, TensorArg rhname] expr) (FlipIndicesExpr (VarExpr $ stringToVar rhname2)) (VarExpr $ stringToVar rhname))

desugar (MemoizedLambdaExpr names expr) = do
  expr' <- desugar expr
  return $ MemoizedLambdaExpr names expr'

desugar (MemoizeExpr memoizeBindings expr) = do
  memoizeBindings' <- mapM (\(x,y,z) -> do x' <- desugar x
                                           y' <- desugar y
                                           z' <- desugar z
                                           return (x',y',z'))
                           memoizeBindings
  expr' <- desugar expr
  return $ MemoizeExpr memoizeBindings' expr'

desugar (CambdaExpr name expr) = do
  expr' <- desugar expr
  return $ CambdaExpr name expr'

desugar (ProcedureExpr names expr) = do
  expr' <- desugar expr
  return $ ProcedureExpr names expr'

--desugar (MacroExpr names expr) = do
--  expr' <- desugar expr
--  return $ MacroExpr names expr'

desugar (PatternFunctionExpr names pattern) = do
  pattern' <- desugarPattern pattern
  return $ PatternFunctionExpr names pattern'

desugar (IfExpr expr0 expr1 expr2) = do
  expr0' <- desugar expr0
  expr1' <- desugar expr1
  expr2' <- desugar expr2
  return $ IfExpr expr0' expr1' expr2'
  
desugar (LetRecExpr binds expr) = do
  binds' <- desugarBindings binds
  expr' <- desugar expr
  return $ LetRecExpr binds' expr'
  
desugar (LetExpr binds expr) = do
  binds' <- desugarBindings binds
  expr' <- desugar expr
  return $ LetExpr binds' expr'

desugar (LetStarExpr binds expr) = do
  binds' <- desugarBindings binds
  expr' <- desugar expr
  return $ foldr (\bind ret -> LetExpr [bind] ret) expr' binds'

desugar (WithSymbolsExpr vars expr) = do
  expr' <- desugar expr
  return $ WithSymbolsExpr vars expr'

desugar (MatchExpr expr0 expr1 clauses) = do  
  expr0' <- desugar expr0
  expr1' <- desugar expr1
  clauses' <- desugarMatchClauses clauses
  return (MatchExpr expr0' expr1' clauses')

desugar (MatchAllExpr expr0 expr1 clause) = do
  expr0' <- desugar expr0
  expr1' <- desugar expr1
  clause' <- desugarMatchClause clause
  return $ MatchAllExpr expr0' expr1' clause'
  
desugar (DoExpr binds expr) = do
  binds' <- desugarBindings binds
  expr' <- desugar expr
  return $ DoExpr binds' expr'
  
desugar (IoExpr expr) = do
  expr' <- desugar expr
  return $ IoExpr expr'
  
desugar (SeqExpr expr0 expr1) = do
  expr0' <- desugar expr0
  expr1' <- desugar expr1
  return $ SeqExpr expr0' expr1'

desugar (GenerateArrayExpr fnExpr (fstExpr, lstExpr)) = do
  fnExpr' <- desugar fnExpr
  fstExpr' <- desugar fstExpr
  lstExpr' <- desugar lstExpr
  return $ GenerateArrayExpr fnExpr' (fstExpr', lstExpr')

desugar (GenerateTensorExpr fnExpr sizeExpr) = do
  fnExpr' <- desugar fnExpr
  sizeExpr' <- desugar sizeExpr
  return $ GenerateTensorExpr fnExpr' sizeExpr'

desugar (TensorContractExpr fnExpr tExpr) = do
  fnExpr' <- desugar fnExpr
  tExpr' <- desugar tExpr
  return $ TensorContractExpr fnExpr' tExpr'

desugar (TensorMapExpr fnExpr tExpr) = do
  fnExpr' <- desugar fnExpr
  tExpr' <- desugar tExpr
  return $ TensorMapExpr fnExpr' tExpr'

desugar (TensorMap2Expr fnExpr t1Expr t2Expr) = do
  fnExpr' <- desugar fnExpr
  t1Expr' <- desugar t1Expr
  t2Expr' <- desugar t2Expr
  return $ TensorMap2Expr fnExpr' t1Expr' t2Expr'

desugar (TransposeExpr vars expr) = do
  vars' <- desugar vars
  expr' <- desugar expr
  return $ TransposeExpr vars' expr'

desugar (FlipIndicesExpr expr) = do
  expr' <- desugar expr
  return $ FlipIndicesExpr expr'

desugar (ParExpr expr1 expr2) = do
  expr1' <- desugar expr1
  expr2' <- desugar expr2
  return $ ParExpr expr1' expr2'

desugar (PseqExpr expr1 expr2) = do
  expr1' <- desugar expr1
  expr2' <- desugar expr2
  return $ PseqExpr expr1' expr2'

desugar (PmapExpr expr1 expr2) = do
  expr1' <- desugar expr1
  expr2' <- desugar expr2
  return $ PmapExpr expr1' expr2'

desugar (ApplyExpr expr0 expr1) = do
  expr0' <- desugar expr0
  expr1' <- desugar expr1
  return $ ApplyExpr expr0' expr1'

desugar (CApplyExpr expr0 expr1) = do
  expr0' <- desugar expr0
  expr1' <- desugar expr1
  return $ CApplyExpr expr0' expr1'

desugar (VarExpr name) = do
  asks $ maybe (VarExpr name) id . lookup (show name)

desugar FreshVarExpr = do
  id <- fresh
  return (VarExpr $ stringToVar (":::" ++ id))

desugar (MatcherBFSExpr matcherInfo) = do
  matcherInfo' <- desugarMatcherInfo matcherInfo
  return $ MatcherBFSExpr matcherInfo'
  
desugar (MatcherDFSExpr matcherInfo) = do
  matcherInfo' <- desugarMatcherInfo matcherInfo
  return $ MatcherDFSExpr matcherInfo'
  
desugar (PartialVarExpr n) = return $ PartialVarExpr n

desugar (PartialExpr n expr) = do
  expr' <- desugar expr
  return $ LetRecExpr [([stringToVar "::0"], PartialExpr n expr')] (VarExpr $ stringToVar "::0")

desugar (QuoteExpr expr) = do
  expr' <- desugar expr
  return $ QuoteExpr expr'

desugar (QuoteFunctionExpr expr) = do
  expr' <- desugar expr
  return $ QuoteFunctionExpr expr'

desugar (WedgeExpr (ApplyExpr expr0 expr1)) = do
  expr0' <- desugar expr0
  expr1' <- desugar expr1
  return $ WedgeApplyExpr expr0' expr1'

desugar expr = return expr

desugarIndex :: Index EgisonExpr -> DesugarM (Index EgisonExpr)
desugarIndex (Superscript expr) = desugar expr >>= return . Superscript
desugarIndex (Subscript expr) = desugar expr >>= return . Subscript
desugarIndex (SupSubscript expr) = desugar expr >>= return . SupSubscript

desugarPattern :: EgisonPattern -> DesugarM EgisonPattern
desugarPattern pattern = LetPat (map makeBinding $ S.elems $ collectName pattern) <$> desugarPattern' pattern 
 where
   collectNames :: [EgisonPattern] -> Set String
   collectNames patterns = S.unions $ map collectName patterns

   collectName :: EgisonPattern -> Set String
   collectName (NotPat pattern) = collectName pattern
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

desugarPattern' :: EgisonPattern -> DesugarM EgisonPattern
desugarPattern' (ValuePat expr) = ValuePat <$> desugar expr
desugarPattern' (PredPat expr) = PredPat <$> desugar expr
desugarPattern' (NotPat pattern) = NotPat <$> desugarPattern' pattern
desugarPattern' (AndPat patterns) = AndPat <$> mapM desugarPattern' patterns
desugarPattern' (OrPat patterns)  =  OrPat <$> mapM desugarPattern' patterns
desugarPattern' (OrderedOrPat [])  = return (NotPat WildCard)
desugarPattern' (OrderedOrPat (pattern:patterns)) = do
  pattern' <- desugarPattern' pattern
  pattern'' <- desugarPattern' (OrderedOrPat patterns)
  return $ OrPat [pattern', AndPat [(NotPat pattern'), pattern'']]
desugarPattern' (TuplePat patterns)  = TuplePat <$> mapM desugarPattern' patterns
desugarPattern' (InductivePat name patterns) = InductivePat name <$> mapM desugarPattern' patterns
desugarPattern' (IndexedPat pattern exprs) = IndexedPat <$> desugarPattern' pattern <*> mapM desugar exprs
desugarPattern' (PApplyPat expr patterns) = PApplyPat <$> desugar expr <*> mapM desugarPattern' patterns 
desugarPattern' (DApplyPat pattern patterns) = DApplyPat <$> desugarPattern' pattern <*> mapM desugarPattern' patterns 
desugarPattern' (LoopPat name range pattern1 pattern2) =  LoopPat name <$> desugarLoopRange range <*> desugarPattern' pattern1 <*> desugarPattern' pattern2
desugarPattern' (LetPat binds pattern) = LetPat <$> desugarBindings binds <*> desugarPattern' pattern
desugarPattern' (DivPat pattern1 pattern2) = do
  pat1' <- desugarPattern' pattern1
  pat2' <- desugarPattern' pattern2
  return $ InductivePat "div" [pat1', pat2']
desugarPattern' (PlusPat patterns) = do
  pats' <- mapM desugarPattern' patterns
  case (reverse pats') of
    [] -> return $ InductivePat "plus" [ValuePat (IntegerExpr 0)]
    lp:hps ->
      return $ InductivePat "plus" [foldr (\p r -> InductivePat "cons" [p, r]) lp (reverse hps)]
desugarPattern' (MultPat (intPat:patterns)) = do
  intPat' <- desugarPattern' intPat
  pats' <- mapM desugarPattern' patterns
  case (reverse pats') of
    [] -> return $ InductivePat "mult" [intPat', ValuePat (IntegerExpr 1)]
    lp:hps ->
      return $ InductivePat "mult" [intPat',
                                    foldr (\p r -> case p of
                                                     (PowerPat p1 p2) -> InductivePat "ncons" [p1, p2, r]
                                                     _ -> InductivePat "cons" [p, r])
                                          (case lp of
                                             (PowerPat p1 p2) -> InductivePat "ncons" [p1, p2, ValuePat (IntegerExpr 1)]
                                             _ -> lp)
                                          (reverse hps)]
desugarPattern' (PowerPat pattern1 pattern2) = PowerPat <$> desugarPattern' pattern1 <*> desugarPattern' pattern2
desugarPattern' pattern = return pattern

desugarLoopRange :: LoopRange -> DesugarM LoopRange
desugarLoopRange (LoopRange sExpr eExpr pattern) = do
  sExpr' <- desugar sExpr
  eExpr' <- desugar eExpr
  pattern' <- desugarPattern' pattern
  return $ LoopRange sExpr' eExpr' pattern'

desugarBinding :: BindingExpr -> DesugarM BindingExpr
desugarBinding (name, expr) = do
  expr' <- desugar expr
  return $ (name, expr')

desugarBindings :: [BindingExpr] -> DesugarM [BindingExpr]
desugarBindings (bind:rest) = do
  bind' <- desugarBinding bind
  rest' <- desugarBindings rest
  return $ bind' : rest'
  
desugarBindings [] = return []

desugarMatchClause :: MatchClause -> DesugarM MatchClause
desugarMatchClause (pattern, expr) = do
  pattern' <- desugarPattern pattern
  expr'    <- desugar expr
  return $ (pattern', expr')

desugarMatchClauses :: [MatchClause] -> DesugarM [MatchClause]
desugarMatchClauses (clause:rest) = do
  clause' <- desugarMatchClause clause
  rest'   <- desugarMatchClauses rest
  return $ clause' : rest'
desugarMatchClauses [] = return []

desugarMatcherInfo :: MatcherInfo -> DesugarM MatcherInfo
desugarMatcherInfo [] = return []
desugarMatcherInfo ((pp, matcher, pds):matcherInfo) = do
  matcher' <- desugar matcher
  pds' <- desugarPrimitiveDataMatchClauses pds
  matcherInfo' <- desugarMatcherInfo matcherInfo
  return $ (pp, matcher', pds'):matcherInfo'

desugarPrimitiveDataMatchClauses :: [(PrimitiveDataPattern, EgisonExpr)] -> DesugarM [(PrimitiveDataPattern, EgisonExpr)]
desugarPrimitiveDataMatchClauses [] = return []
desugarPrimitiveDataMatchClauses ((pd, expr):pds) = do
  expr' <- desugar expr
  pds' <- desugarPrimitiveDataMatchClauses pds
  return $ (pd, expr'):pds'
