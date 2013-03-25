{-# Language GeneralizedNewtypeDeriving #-}
module Language.Egison.Desugar where
import Control.Applicative (Applicative)
import Control.Applicative ((<$>), (<*>), (<*), (*>), pure)
import Control.Monad.Error
import Control.Monad.Identity
import Language.Egison.Types

newtype DesugarM a = DesugarM { unDesugarM :: ErrorT DesugarError Identity a }
  deriving (Functor, Applicative, Monad, MonadError DesugarError)
           
data DesugarError = DesugarError String
  deriving (Show)

instance Error DesugarError where
  strMsg = DesugarError
  noMsg = DesugarError "desugar failed"


runDesugarM :: DesugarM a -> Either DesugarError a
runDesugarM = runIdentity . runErrorT . unDesugarM

desugar :: EgisonExpr -> DesugarM EgisonExpr
desugar (AlgebraicDataMatcher patterns) = do
  ppats <- mapM checkPattern patterns 
  dpats <- mapM genPrimitiveDataPat ppats
  clauses <- mapM genMatcherClause $ zip ppats dpats 
  return $ MatcherExpr clauses
  where
    checkPattern :: PrimitivePatPattern -> DesugarM PrimitivePatPattern
    checkPattern pat@(PPInductivePat _ _) = return pat
    checkPattern _                        = throwError $ DesugarError "unexpected primitive pattern pattern"
    
    genPrimitiveDataPat :: PrimitivePatPattern -> DesugarM PrimitiveDataPattern
    genPrimitiveDataPat PPWildCard = return PDWildCard
    genPrimitiveDataPat (PPInductivePat name body) =
      PDInductivePat <$> pure name <*> mapM genPrimitiveDataPat body
    genPrimitiveDataPat _ = throwError $ DesugarError "invalid primitive pattern pattern"
    
    genMatcherClause :: (PrimitivePatPattern, PrimitiveDataPattern) -> 
                        DesugarM (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
    genMatcherClause (ppat, dpat) = return (ppat, (TupleExpr []), [(dpat, matchingSuccess)
                                                               ,(PDWildCard, matchingFailure)])
    
    genSomethingClause :: DesugarM (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
    genSomethingClause = undefined
    
    matchingSuccess :: EgisonExpr
    matchingSuccess = CollectionExpr $ [ElementExpr $ TupleExpr []]

    matchingFailure :: EgisonExpr
    matchingFailure = CollectionExpr []

