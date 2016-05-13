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
import Data.Set (Set)
import Data.Char (toUpper)
import Control.Monad.Error
import Control.Monad.Reader
import Language.Egison.Types

type Subst = [(String, EgisonExpr)]

newtype DesugarM a = DesugarM { unDesugarM :: ReaderT Subst (ErrorT EgisonError Fresh) a }
  deriving (Functor, Applicative, Monad, MonadError EgisonError, MonadFresh, MonadReader Subst)

runDesugarM :: DesugarM a -> Fresh (Either EgisonError a)
runDesugarM = runErrorT . flip runReaderT [] . unDesugarM

desugarTopExpr :: EgisonTopExpr -> EgisonM EgisonTopExpr
desugarTopExpr (Define name expr) = do
  expr' <- liftEgisonM $ runDesugarM $ desugar expr
  return (Define name expr')
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
  matcherName <- fresh
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
               ,[(PDPatVar "tgt", (MatchExpr (TupleExpr [(VarExpr "val"), (VarExpr "tgt")]) 
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
            names <- mapM (const fresh) patterns
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
            patterns' <- mapM (const fresh) patterns 
            return (PDInductivePat (capitalize name) $ map PDPatVar patterns', map VarExpr patterns')

          capitalize :: String -> String
          capitalize (x:xs) = toUpper x : xs
                                            
      
      genSomethingClause :: DesugarM (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
      genSomethingClause = 
        return (PPPatVar, (TupleExpr [SomethingExpr]), [(PDPatVar "tgt", CollectionExpr [ElementExpr (VarExpr "tgt")])])
    
      matchingSuccess :: EgisonExpr
      matchingSuccess = CollectionExpr [ElementExpr $ TupleExpr []]

      matchingFailure :: EgisonExpr
      matchingFailure = CollectionExpr []

desugar (MatchAllLambdaExpr matcher clause) = do
  name <- fresh
  matcher' <- desugar matcher
  clause' <- desugarMatchClause clause
  return $ LambdaExpr [name] (MatchAllExpr (VarExpr name) matcher' clause')

desugar (MatchLambdaExpr matcher clauses) = do
  name <- fresh
  matcher' <- desugar matcher
  clauses' <- desugarMatchClauses clauses
  return $ LambdaExpr [name] (MatchExpr (VarExpr name) matcher' clauses')

desugar (ArrayRefExpr expr nums) =
  case nums of
    (TupleExpr nums') -> desugar $ IndexedExpr expr (map Subscript nums')
    _ -> desugar $ IndexedExpr expr [Subscript nums]
  
desugar (IndexedExpr expr indices) = 
  IndexedExpr <$> desugar expr <*> (mapM desugarIndex indices)

desugar (PowerExpr expr1 expr2) = do
  expr1' <- desugar expr1
  expr2' <- desugar expr2
  return $ ApplyExpr (VarExpr "**'") (TupleExpr [expr1', expr2'])

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

desugar (LambdaExpr names expr) = do
  expr' <- desugar expr
  return $ LambdaExpr names expr'

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

desugar (TensorMapExpr fnExpr tExpr) = do
  fnExpr' <- desugar fnExpr
  tExpr' <- desugar tExpr
  return $ TensorMapExpr fnExpr' tExpr'

desugar (TensorMap2Expr fnExpr t1Expr t2Expr) = do
  fnExpr' <- desugar fnExpr
  t1Expr' <- desugar t1Expr
  t2Expr' <- desugar t2Expr
  return $ TensorMap2Expr fnExpr' t1Expr' t2Expr'

desugar (ApplyExpr expr0 expr1) = do
  expr0' <- desugar expr0
  expr1' <- desugar expr1
  return $ ApplyExpr expr0' expr1'

desugar (CApplyExpr expr0 expr1) = do
  expr0' <- desugar expr0
  expr1' <- desugar expr1
  return $ CApplyExpr expr0' expr1'

desugar (VarExpr name) = do
  asks $ maybe (VarExpr name) id . lookup name

desugar (MatcherBFSExpr matcherInfo) = do
  matcherInfo' <- desugarMatcherInfo matcherInfo
  return $ MatcherBFSExpr matcherInfo'
  
desugar (MatcherDFSExpr matcherInfo) = do
  matcherInfo' <- desugarMatcherInfo matcherInfo
  return $ MatcherDFSExpr matcherInfo'
  
desugar (PartialVarExpr n) = return $ VarExpr $ "::" ++ show n

desugar RecVarExpr = return $ VarExpr "::"

desugar (PartialExpr n expr) = do
  expr' <- desugar expr
  if n == 0
    then return $ LetRecExpr [(["::"], LambdaExpr [] expr')] (LambdaExpr [] expr')
    else return $ LetRecExpr [(["::"], LambdaExpr (annonVars (fromIntegral n)) expr')] (LambdaExpr (annonVars (fromIntegral n)) expr')
 where
  annonVars n = take n $ map (((++) "::") . show) [1..]

desugar (QuoteExpr expr) = do
  expr' <- desugar expr
  return $ QuoteExpr expr'

desugar expr = return expr

desugarIndex :: Index EgisonExpr -> DesugarM (Index EgisonExpr)
desugarIndex (Superscript expr) = desugar expr >>= return . Superscript
desugarIndex (Subscript expr) = desugar expr >>= return . Subscript

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
--   collectName (LoopPat _ (LoopRange _ _ endNumPat) pattern1 pattern2) = collectName pattern1 `S.union` collectName pattern2
   collectName (LetPat _ pattern) = collectName pattern
   collectName (IndexedPat (PatVar name) _) = S.singleton name
   collectName (OrPat patterns) = collectNames patterns
   collectName _ = S.empty
   
   makeBinding :: String -> BindingExpr
   makeBinding name = ([name], HashExpr [])

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
