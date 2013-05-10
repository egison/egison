{-# Language FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Language.Egison.Desugar where
import Control.Applicative (Applicative)
import Control.Applicative ((<$>), (<*>), (<*), (*>), pure)
import qualified Data.Sequence as Sq
import Data.Sequence (ViewL(..), (<|))
import Data.Char (toUpper)
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Language.Egison.Types

type Subst = [(String, EgisonExpr)]

newtype DesugarM a = DesugarM { unDesugarM :: ReaderT Subst (ErrorT EgisonError Fresh) a }
  deriving (Functor, Applicative, Monad, MonadError EgisonError, MonadFresh, MonadReader Subst)

instance MonadFresh (ReaderT Subst (ErrorT EgisonError Fresh)) where
  fresh = lift $ lift $ fresh

runDesugarM :: DesugarM a -> Fresh (Either EgisonError a)
runDesugarM = runErrorT . flip runReaderT [] . unDesugarM

desugarTopExpr :: EgisonTopExpr -> DesugarM EgisonTopExpr
desugarTopExpr (Define name expr) = do
  expr' <- desugar expr
  return (Define name expr')
desugarTopExpr (Test expr) = do
  expr' <- desugar expr
  return (Test expr')
desugarTopExpr expr = return expr


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
                                     <*> pure [(WildCard, matchingFailure)]
          
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
        return (ppat, TupleExpr matchers, [(dpat, CollectionExpr (Sq.singleton . ElementExpr . TupleExpr $ body)), (PDWildCard, matchingFailure)])
        
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
        return (PPPatVar, (TupleExpr [SomethingExpr]), [(PDPatVar "tgt", CollectionExpr . Sq.singleton $ ElementExpr (VarExpr "tgt"))])
    
      matchingSuccess :: EgisonExpr
      matchingSuccess = CollectionExpr . Sq.singleton. ElementExpr $ TupleExpr []

      matchingFailure :: EgisonExpr
      matchingFailure = CollectionExpr Sq.empty

desugar (MatchLambdaExpr matcher clauses) = do
  name <- fresh
  matcher' <- desugar matcher
  clauses' <- desugarMatchClauses clauses
  desugar (LambdaExpr [name] (MatchExpr (VarExpr name) matcher' clauses'))

desugar (ArrayRefExpr (VarExpr name) (TupleExpr nums)) =
  desugar $ IndexedExpr (VarExpr name) nums
  
desugar (ArrayRefExpr expr nums) =
  desugar $ ArrayRefExpr expr (TupleExpr [nums])

desugar (IndexedExpr expr indices) = 
  IndexedExpr <$> desugar expr <*> (mapM desugar indices)

desugar (InductiveDataExpr name exprs) = do 
  exprs' <- mapM desugar exprs
  return $ InductiveDataExpr name exprs'

desugar (TupleExpr exprs) = do
  exprs' <- mapM desugar exprs
  return $ TupleExpr exprs'

desugar expr@(CollectionExpr seq) =
  case Sq.viewl seq of
    EmptyL -> return expr
    ElementExpr seqHead :< seqTail -> do
      seqHead' <- desugar seqHead
      (CollectionExpr seqTail') <- desugar (CollectionExpr seqTail)
      return $ CollectionExpr (ElementExpr seqHead' <| seqTail')
    SubCollectionExpr seqHead :< seqTail -> do
      seqHead' <- desugar seqHead
      (CollectionExpr seqTail') <- desugar (CollectionExpr seqTail)
      return $ CollectionExpr (SubCollectionExpr seqHead' <| seqTail')

{-
desugar (CollectionExpr ((ElementExpr expr):rest)) = do
  expr' <- desugar expr
  (CollectionExpr rest') <- desugar (CollectionExpr rest)
  return $ CollectionExpr ((ElementExpr expr'):rest')

desugar (CollectionExpr ((SubCollectionExpr expr):rest)) = do
  expr' <- desugar expr
  (CollectionExpr rest') <- desugar (CollectionExpr rest)
  return $ CollectionExpr ((SubCollectionExpr expr'):rest')

desugar expr@(CollectionExpr []) = return expr
-}

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
  
{- temporarily commented
desugar (IndexLoopExpr n0 n1 n2 expr0 expr1 expr2 expr3) = do
  name <- fresh
  helper <- genHelper name
  return $ ApplyExpr (LetRecExpr [([name], helper)] (ApplyExpr (VarExpr name) expr1)) expr0
 where
  genHelper :: String -> DesugarM EgisonExpr
  genHelper name = do
    indicesName <- fresh
    patName <- fresh
    let subst = [(n0, ApplyExpr (ApplyExpr (VarExpr name) (VarExpr indicesName)) (VarExpr patName)), (n1, VarExpr patName)]
    bodyExpr <- local (subst ++) $ desugar expr2
    initExpr <- desugar expr3
    let matchClauses = [ (PatternExpr $ InductivePattern "nil" [], LambdaExpr [patName] $ initExpr)
                       , (PatternExpr $ InductivePattern "cons" [PatternExpr (PatVar n2), PatternExpr (PatVar indicesName)], LambdaExpr [patName] $ bodyExpr)]
    return $ LambdaExpr [indicesName] $ MatchExpr (VarExpr indicesName) matcher matchClauses

  matcher :: EgisonExpr
  matcher = ApplyExpr (VarExpr "list") SomethingExpr
-}
  
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
  
desugar (ApplyExpr expr0 expr1) = do
  expr0' <- desugar expr0
  expr1' <- desugar expr1
  return $ ApplyExpr expr0' expr1'

desugar (VarExpr name) = do
  asks $ maybe (VarExpr name) id . lookup name
  
desugar expr = return expr

desugarPattern :: EgisonPattern -> DesugarM EgisonPattern
desugarPattern (ValuePat expr) = ValuePat <$> desugar expr
desugarPattern (PredPat expr) = PredPat <$> desugar expr
desugarPattern (CutPat pattern) = CutPat <$> desugarPattern pattern
desugarPattern (NotPat pattern) = NotPat <$> desugarPattern pattern
desugarPattern (AndPat patterns) = AndPat <$> mapM desugarPattern patterns
desugarPattern (OrPat patterns)  = OrPat <$> mapM desugarPattern patterns
desugarPattern (TuplePat patterns)  = TuplePat <$> mapM desugarPattern patterns
desugarPattern (InductivePat name patterns) =
  InductivePat name <$> mapM desugarPattern patterns
desugarPattern (IndexedPat pattern exprs) =
  IndexedPat <$> desugarPattern pattern <*> mapM desugar exprs
desugarPattern (ApplyPat expr patterns) =
  ApplyPat <$> desugar expr <*> mapM desugarPattern patterns 
desugarPattern pattern = return pattern

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
  return $ clause : rest'
desugarMatchClauses [] = return []
