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
        return $ MatcherExpr clauses
        
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
    (TupleExpr nums') -> desugar $ IndexedExpr expr nums'
    _ -> desugar $ IndexedExpr expr [nums]
  
desugar (IndexedExpr expr indices) = 
  IndexedExpr <$> desugar expr <*> (mapM desugar indices)

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

desugar (PatternFunctionExpr names pattern) = do
  pattern' <- desugarPattern pattern
  return $ PatternFunctionExpr names pattern'

desugar (IfExpr expr0 expr1 expr2) = do
  expr0' <- desugar expr0
  expr1' <- desugar expr1
  expr2' <- desugar expr2
  return $ IfExpr expr0' expr1' expr2'
  
desugar (LetExpr binds expr) = do
  binds' <- desugarBindings binds
  expr' <- desugar expr
  return $ LetExpr binds' expr'

desugar (LetRecExpr binds expr) = do
  binds' <- desugarBindings binds
  expr' <- desugar expr
  return $ LetRecExpr binds' expr'
  
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
  
desugar (ApplyExpr (VarExpr "+") expr) = do
  expr' <- desugar expr
  case expr' of
    args@(TupleExpr (_:_:[])) -> return $ ApplyExpr (VarExpr "+") args
    (TupleExpr args) -> return $ ApplyExpr (VarExpr "foldl") (TupleExpr [(VarExpr "+"), (IntegerExpr 0), (CollectionExpr (map ElementExpr args))])

desugar (ApplyExpr (VarExpr "-") expr) = do
  expr' <- desugar expr
  case expr' of
    args@(TupleExpr (_:_:[])) -> return $ ApplyExpr (VarExpr "-") args
    (TupleExpr (x:args)) -> return $ ApplyExpr (VarExpr "foldl") (TupleExpr [(VarExpr "-"), x, (CollectionExpr (map ElementExpr args))])

desugar (ApplyExpr (VarExpr "*") expr) = do
  expr' <- desugar expr
  case expr' of
    args@(TupleExpr (_:_:[])) -> return $ ApplyExpr (VarExpr "*") args
    (TupleExpr args) -> return $ ApplyExpr (VarExpr "foldl") (TupleExpr [(VarExpr "*"), (IntegerExpr 1), (CollectionExpr (map ElementExpr args))])

desugar (ApplyExpr expr0 expr1) = do
  expr0' <- desugar expr0
  expr1' <- desugar expr1
  return $ ApplyExpr expr0' expr1'

desugar (VarExpr name) = do
  asks $ maybe (VarExpr name) id . lookup name
  
desugar expr = return expr

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
   collectName (ApplyPat _ patterns) = collectNames patterns
   collectName (LoopPat _ _ pattern1 pattern2) = collectName pattern1 `S.union` collectName pattern2
   collectName (LetPat _ pattern) = collectName pattern
   collectName (IndexedPat (PatVar name) _) = S.singleton name
   collectName (OrPat patterns) = collectNames patterns
   collectName _ = S.empty
   
   makeBinding :: String -> BindingExpr
   makeBinding name = ([name], ArrayExpr [])

desugarPattern' :: EgisonPattern -> DesugarM EgisonPattern
desugarPattern' (ValuePat expr) = ValuePat <$> desugar expr
desugarPattern' (PredPat expr) = PredPat <$> desugar expr
desugarPattern' (NotPat pattern) = NotPat <$> desugarPattern' pattern
desugarPattern' (AndPat patterns) = AndPat <$> mapM desugarPattern' patterns
desugarPattern' (TuplePat patterns)  = TuplePat <$> mapM desugarPattern' patterns
desugarPattern' (InductivePat name patterns) =
  InductivePat name <$> mapM desugarPattern' patterns
desugarPattern' (IndexedPat pattern exprs) =
  IndexedPat <$> desugarPattern' pattern <*> mapM desugar exprs
desugarPattern' (ApplyPat expr patterns) =
  ApplyPat <$> desugar expr <*> mapM desugarPattern' patterns 
desugarPattern' (LoopPat name range pattern1 pattern2) =
  LoopPat name <$> desugarLoopRange range <*> desugarPattern' pattern1 <*> desugarPattern' pattern2
desugarPattern' (LetPat binds pattern) = do
  LetPat <$> desugarBindings binds <*> desugarPattern' pattern
desugarPattern' (OrPat patterns)  = 
  OrPat <$> mapM desugarPattern' patterns
desugarPattern' pattern = return pattern

desugarLoopRange :: LoopRange -> DesugarM LoopRange
desugarLoopRange (LoopRangeConstant expr1 expr2) = do
  expr1' <- desugar expr1
  expr2' <- desugar expr2
  return $ LoopRangeConstant expr1' expr2'
desugarLoopRange (LoopRangeVariable expr1 name) = do
  expr1' <- desugar expr1
  return $ LoopRangeVariable expr1' name

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
