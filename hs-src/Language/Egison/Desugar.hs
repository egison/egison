{-# Language FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Language.Egison.Desugar where
import Control.Applicative (Applicative)
import Control.Applicative ((<$>), (<*>), (<*), (*>), pure)
import Data.Char (toUpper)
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Language.Egison.Types

type Subst = [(String, EgisonExpr)]

newtype DesugarM a = DesugarM { unDesugarM :: ReaderT Subst (StateT Int (ErrorT EgisonError Identity)) a }
  deriving (Functor, Applicative, Monad, MonadReader Subst, MonadState Int, MonadError EgisonError)
           
class (Applicative m, Monad m) => MonadFresh m where
  fresh :: m String
           
instance MonadFresh DesugarM where
  fresh = do counter <- get; modify (+ 1) 
             return $ genFreshName counter
    where
      genFreshName :: Int -> String
      genFreshName n = "$_" ++ show n

runDesugarM :: DesugarM a -> Either EgisonError a
runDesugarM d =runIdentity $ runErrorT $ flip evalStateT 0 $ flip runReaderT [] $ unDesugarM d

desugar :: EgisonExpr -> DesugarM EgisonExpr
desugar (AlgebraicDataMatcherExpr patterns) = do
  matcherName <- fresh
  matcherRef <- return $ VarExpr matcherName
  matcher <- genMatcherClauses patterns matcherRef
  return $ LetRecExpr [([matcherName], matcher)] matcherRef
    where
      genMatcherClauses :: [EgisonExpr] ->  EgisonExpr -> DesugarM EgisonExpr
      genMatcherClauses patterns matcher = do
        main <- genMainClause patterns matcher
        body <- mapM genMatcherClause patterns
        footer <- genSomethingClause
        clauses <- return $ [main] ++ body ++ [footer]
        return $ MatcherExpr clauses
        
      genMainClause :: [EgisonExpr] -> EgisonExpr -> DesugarM (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
      genMainClause patterns matcher = do
        clauses <- genClauses patterns
        return (PPValuePat "val", TupleExpr [],
                 [(PDPatVar "tgt", (MatchExpr (TupleExpr [(VarExpr "val"), (VarExpr "tgt")]) 
                                              (TupleExpr [matcher, matcher]) 
                                              clauses))])
        where
          genClauses :: [EgisonExpr] -> DesugarM [MatchClause]
          genClauses patterns = (++) <$> mapM genClause patterns
                                     <*> pure [(PatternExpr WildCard, matchingFailure)]
          
          genClause :: EgisonExpr -> DesugarM MatchClause
          genClause pattern = do
            (pat0, pat1) <- genMatchingPattern pattern
            return (TupleExpr [pat0, pat1], matchingSuccess)
        
          genMatchingPattern :: EgisonExpr -> DesugarM (EgisonExpr, EgisonExpr)
          genMatchingPattern (PatternExpr (InductivePattern name patterns)) = do
            names <- mapM (const fresh) patterns
            return $ ((PatternExpr $ InductivePattern name (map (PatternExpr . PatVar) names))  
                     ,(PatternExpr $ InductivePattern name (map (PatternExpr . ValuePat . VarExpr) names)))
          
      genMatcherClause :: EgisonExpr -> DesugarM (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
      genMatcherClause pattern = do
        (ppat, matchers) <- genPrimitivePatPat pattern
        (dpat, body)     <- genPrimitiveDataPat pattern
        return (ppat, TupleExpr matchers, [(dpat, CollectionExpr [ElementExpr . TupleExpr $ body]), (PDWildCard, matchingFailure)])
        
        where
          genPrimitivePatPat :: EgisonExpr -> DesugarM (PrimitivePatPattern, [EgisonExpr])
          genPrimitivePatPat (PatternExpr (InductivePattern name matchers)) = do
            patterns' <- mapM (const $ return PPPatVar) matchers
            return (PPInductivePat name patterns', matchers)
          genPrimitivePatPat _ = throwError $ Desugar "invalid algebraic-data-matcher"
          
          genPrimitiveDataPat :: EgisonExpr -> DesugarM (PrimitiveDataPattern, [EgisonExpr])
          genPrimitiveDataPat (PatternExpr (InductivePattern name patterns)) = do
            patterns' <- mapM (const fresh) patterns 
            return (PDInductivePat (capitalize name) $ map PDPatVar patterns', map VarExpr patterns')
          genPrimitiveDataPatr _ = throwError $ Desugar "invalid algebraic-data-matcher"

          capitalize :: String -> String
          capitalize (x:xs) = toUpper x : xs
                                            
      
      genSomethingClause :: DesugarM (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
      genSomethingClause = 
        return (PPPatVar, (TupleExpr [SomethingExpr]), [(PDPatVar "tgt", CollectionExpr $ [ElementExpr (VarExpr "tgt")])])
    
      matchingSuccess :: EgisonExpr
      matchingSuccess = CollectionExpr $ [ElementExpr $ TupleExpr []]

      matchingFailure :: EgisonExpr
      matchingFailure = CollectionExpr []

desugar (FunctionExpr matcher clauses) = do
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

desugar (CollectionExpr ((ElementExpr expr):rest)) = do
  expr' <- desugar expr
  (CollectionExpr rest') <- desugar (CollectionExpr rest)
  return $ CollectionExpr ((ElementExpr expr'):rest')

desugar (CollectionExpr ((SubCollectionExpr expr):rest)) = do
  expr' <- desugar expr
  (CollectionExpr rest') <- desugar (CollectionExpr rest)
  return $ CollectionExpr ((SubCollectionExpr expr'):rest')

desugar expr@(CollectionExpr []) = return expr

desugar (LambdaExpr names expr) = do
  expr' <- desugar expr
  return $ LambdaExpr names expr'

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
  
desugar (PatternExpr pattern) =
  PatternExpr <$> desugarPattern pattern
  
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
desugarPattern (CutPat expr) = CutPat <$> desugar expr
desugarPattern (NotPat expr) = NotPat <$> desugar expr
desugarPattern (AndPat exprs) = AndPat <$> mapM desugar exprs
desugarPattern (OrPat exprs)  = OrPat <$> mapM desugar exprs
desugarPattern (IndexedPattern pat expr) = 
  IndexedPattern <$> desugarPattern pat <*> desugar expr
desugarPattern (InductivePattern name exprs) =
  InductivePattern name <$> mapM desugar exprs
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
desugarMatchClause (expr0, expr1) = do
  expr0' <- desugar expr0
  expr1' <- desugar expr1
  return $ (expr0', expr1')

desugarMatchClauses :: [MatchClause] -> DesugarM [MatchClause]
desugarMatchClauses (clause:rest) = do
  clause' <- desugarMatchClause clause
  rest'   <- desugarMatchClauses rest
  return $ clause : rest'
desugarMatchClauses [] = return []
