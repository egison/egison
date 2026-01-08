{- |
Module      : Language.Egison.Type.TypeTensorExpand
Licence     : MIT

This module implements tensor-related transformations for Phase 8.
It performs type-driven transformations on TIExpr (Typed Internal Expressions)
specifically for tensor operations like tensorMap insertion.

Tensor Transformations:
  - tensorMap automatic insertion
    - Detect mismatches between Tensor MathExpr and MathExpr
    - Insert tensorMap at appropriate positions
-}

module Language.Egison.Type.TypeTensorExpand
  ( expandTensorApplications
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Language.Egison.Data       (EvalM)
import           Language.Egison.EvalState   (MonadEval(..))
import           Language.Egison.IExpr      (TIExpr(..), IExpr(..), IPattern(..), 
                                             IBindingExpr, IMatchClause, ILoopRange(..), Index(..))
import           Language.Egison.Type.Env   (generalize)
import           Language.Egison.Type.IInfer (runInferI, defaultInferConfig)
import           Language.Egison.Type.Types (TypeScheme(..), Type(..))

-- | Expand tensor applications in a typed expression (TIExpr)
-- This function recursively processes all sub-expressions in the IExpr tree.
-- Each sub-expression is type-inferred and processed as TIExpr to preserve type information
-- for tensorMap insertion.
expandTensorApplications :: TIExpr -> EvalM TIExpr
expandTensorApplications (TIExpr scheme expr) = do
  -- Recursively process the expression tree with type information
  expr' <- expandTIExpr expr
  -- For now, preserve the original type scheme
  -- TODO: Update type scheme based on transformations (e.g., after tensorMap insertion)
  return $ TIExpr scheme expr'
  where
    -- Helper function to infer type for an IExpr and process it as TIExpr
    inferAndExpand :: IExpr -> EvalM TIExpr
    inferAndExpand e = do
      typeEnv <- getTypeEnv
      -- Run type inference for the sub-expression
      result <- liftIO $ runInferI defaultInferConfig typeEnv e
      case result of
        Left _err -> do
          -- If type inference fails, create a TIExpr with TAny type
          -- This should not happen in normal flow, but provides a fallback
          let fallbackScheme = Forall [] [] TAny
          e' <- expandTIExpr e
          return $ TIExpr fallbackScheme e'
        Right (ty, _subst, _warnings) -> do
          -- Generalize the type to create a type scheme
          let typeScheme = generalize typeEnv ty
          e' <- expandTIExpr e
          return $ TIExpr typeScheme e'
    
    -- Helper function to recursively process IExpr and return IExpr
    -- This processes the expression tree and calls inferAndExpand for sub-expressions
    -- that need type information (like function applications)
    expandTIExpr :: IExpr -> EvalM IExpr
    expandTIExpr e = case e of
      -- Constants and variables (no sub-expressions)
      IConstantExpr c -> return $ IConstantExpr c
      IVarExpr name -> return $ IVarExpr name
      
      -- Indexed expressions
      IIndexedExpr b expr1 indices -> do
        expr1' <- expandTIExpr expr1
        indices' <- mapM expandIndex indices
        return $ IIndexedExpr b expr1' indices'
        where
          expandIndex :: Index IExpr -> EvalM (Index IExpr)
          expandIndex (Sub e) = do
            e' <- expandTIExpr e
            return $ Sub e'
          expandIndex (Sup e) = do
            e' <- expandTIExpr e
            return $ Sup e'
          expandIndex (MultiSub e1 n e2) = do
            e1' <- expandTIExpr e1
            e2' <- expandTIExpr e2
            return $ MultiSub e1' n e2'
          expandIndex (MultiSup e1 n e2) = do
            e1' <- expandTIExpr e1
            e2' <- expandTIExpr e2
            return $ MultiSup e1' n e2'
          expandIndex (SupSub e) = do
            e' <- expandTIExpr e
            return $ SupSub e'
          expandIndex (User e) = do
            e' <- expandTIExpr e
            return $ User e'
          expandIndex (DF i1 i2) = return $ DF i1 i2
      
      ISubrefsExpr b expr1 expr2 -> do
        expr1' <- expandTIExpr expr1
        expr2' <- expandTIExpr expr2
        return $ ISubrefsExpr b expr1' expr2'
      
      ISuprefsExpr b expr1 expr2 -> do
        expr1' <- expandTIExpr expr1
        expr2' <- expandTIExpr expr2
        return $ ISuprefsExpr b expr1' expr2'
      
      IUserrefsExpr b expr1 expr2 -> do
        expr1' <- expandTIExpr expr1
        expr2' <- expandTIExpr expr2
        return $ IUserrefsExpr b expr1' expr2'
      
      -- Data constructors
      IInductiveDataExpr name exprs -> do
        exprs' <- mapM expandTIExpr exprs
        return $ IInductiveDataExpr name exprs'
      
      -- Collections
      ITupleExpr exprs -> do
        exprs' <- mapM expandTIExpr exprs
        return $ ITupleExpr exprs'
      
      ICollectionExpr exprs -> do
        exprs' <- mapM expandTIExpr exprs
        return $ ICollectionExpr exprs'
      
      IConsExpr expr1 expr2 -> do
        expr1' <- expandTIExpr expr1
        expr2' <- expandTIExpr expr2
        return $ IConsExpr expr1' expr2'
      
      IJoinExpr expr1 expr2 -> do
        expr1' <- expandTIExpr expr1
        expr2' <- expandTIExpr expr2
        return $ IJoinExpr expr1' expr2'
      
      IHashExpr pairs -> do
        pairs' <- mapM (\(k, v) -> do
          k' <- expandTIExpr k
          v' <- expandTIExpr v
          return (k', v')) pairs
        return $ IHashExpr pairs'
      
      IVectorExpr exprs -> do
        exprs' <- mapM expandTIExpr exprs
        return $ IVectorExpr exprs'
      
      -- Lambda expressions
      ILambdaExpr mVar params body -> do
        body' <- expandTIExpr body
        return $ ILambdaExpr mVar params body'
      
      IMemoizedLambdaExpr args body -> do
        body' <- expandTIExpr body
        return $ IMemoizedLambdaExpr args body'
      
      ICambdaExpr var body -> do
        body' <- expandTIExpr body
        return $ ICambdaExpr var body'
      
      -- Control flow
      IIfExpr cond thenExpr elseExpr -> do
        cond' <- expandTIExpr cond
        thenExpr' <- expandTIExpr thenExpr
        elseExpr' <- expandTIExpr elseExpr
        return $ IIfExpr cond' thenExpr' elseExpr'
      
      -- Let expressions
      ILetRecExpr bindings body -> do
        bindings' <- mapM expandBinding bindings
        body' <- expandTIExpr body
        return $ ILetRecExpr bindings' body'
      
      ILetExpr bindings body -> do
        bindings' <- mapM expandBinding bindings
        body' <- expandTIExpr body
        return $ ILetExpr bindings' body'
      
      IWithSymbolsExpr syms body -> do
        body' <- expandTIExpr body
        return $ IWithSymbolsExpr syms body'
      
      -- Pattern matching
      IMatchExpr mode target matcher clauses -> do
        target' <- expandTIExpr target
        matcher' <- expandTIExpr matcher
        clauses' <- mapM expandMatchClause clauses
        return $ IMatchExpr mode target' matcher' clauses'
      
      IMatchAllExpr mode target matcher clauses -> do
        target' <- expandTIExpr target
        matcher' <- expandTIExpr matcher
        clauses' <- mapM expandMatchClause clauses
        return $ IMatchAllExpr mode target' matcher' clauses'
      
      IMatcherExpr patDefs -> do
        -- Pattern definitions contain IExpr, need to process them
        patDefs' <- mapM (\(pat, expr, bindings) -> do
          expr' <- expandTIExpr expr
          bindings' <- mapM expandBinding bindings
          return (pat, expr', bindings')) patDefs
        return $ IMatcherExpr patDefs'
      
      -- Quote expressions
      IQuoteExpr expr1 -> do
        expr1' <- expandTIExpr expr1
        return $ IQuoteExpr expr1'
      
      IQuoteSymbolExpr expr1 -> do
        expr1' <- expandTIExpr expr1
        return $ IQuoteSymbolExpr expr1'
      
      -- Application - This is where we need type information for tensorMap insertion
      IWedgeApplyExpr func args -> do
        -- Process with type information to enable tensorMap insertion
        funcTI <- inferAndExpand func
        argsTI <- mapM inferAndExpand args
        -- TODO: Check types and insert tensorMap if needed
        -- For now, just process recursively
        func' <- expandTIExpr (tiExpr funcTI)
        args' <- mapM (expandTIExpr . tiExpr) argsTI
        return $ IWedgeApplyExpr func' args'
      
      IDoExpr bindings body -> do
        bindings' <- mapM expandBinding bindings
        body' <- expandTIExpr body
        return $ IDoExpr bindings' body'
      
      ISeqExpr expr1 expr2 -> do
        expr1' <- expandTIExpr expr1
        expr2' <- expandTIExpr expr2
        return $ ISeqExpr expr1' expr2'
      
      -- Function application - This is where we need type information for tensorMap insertion
      IApplyExpr func args -> do
        -- Process with type information to enable tensorMap insertion
        funcTI <- inferAndExpand func
        argsTI <- mapM inferAndExpand args
        -- TODO: Check types and insert tensorMap if needed
        -- For now, just process recursively
        func' <- expandTIExpr (tiExpr funcTI)
        args' <- mapM (expandTIExpr . tiExpr) argsTI
        return $ IApplyExpr func' args'
      
      -- Tensor operations
      IGenerateTensorExpr expr1 expr2 -> do
        expr1' <- expandTIExpr expr1
        expr2' <- expandTIExpr expr2
        return $ IGenerateTensorExpr expr1' expr2'
      
      ITensorExpr expr1 expr2 -> do
        expr1' <- expandTIExpr expr1
        expr2' <- expandTIExpr expr2
        return $ ITensorExpr expr1' expr2'
      
      ITensorContractExpr expr1 -> do
        expr1' <- expandTIExpr expr1
        return $ ITensorContractExpr expr1'
      
      ITensorMapExpr expr1 expr2 -> do
        expr1' <- expandTIExpr expr1
        expr2' <- expandTIExpr expr2
        return $ ITensorMapExpr expr1' expr2'
      
      ITensorMap2Expr expr1 expr2 expr3 -> do
        expr1' <- expandTIExpr expr1
        expr2' <- expandTIExpr expr2
        expr3' <- expandTIExpr expr3
        return $ ITensorMap2Expr expr1' expr2' expr3'
      
      ITransposeExpr expr1 expr2 -> do
        expr1' <- expandTIExpr expr1
        expr2' <- expandTIExpr expr2
        return $ ITransposeExpr expr1' expr2'
      
      IFlipIndicesExpr expr1 -> do
        expr1' <- expandTIExpr expr1
        return $ IFlipIndicesExpr expr1'
      
      -- Function reference
      IFunctionExpr names -> return $ IFunctionExpr names
    
    -- Helper function to process bindings
    expandBinding :: IBindingExpr -> EvalM IBindingExpr
    expandBinding (pat, expr) = do
      expr' <- expandTIExpr expr
      return (pat, expr')
    
    -- Helper function to process match clauses
    expandMatchClause :: IMatchClause -> EvalM IMatchClause
    expandMatchClause (pat, expr) = do
      -- Patterns don't contain IExpr directly, but IValuePat and IPredPat do
      pat' <- expandPattern pat
      expr' <- expandTIExpr expr
      return (pat', expr')
    
    -- Helper function to process patterns (for IValuePat and IPredPat)
    expandPattern :: IPattern -> EvalM IPattern
    expandPattern pat = case pat of
      IValuePat expr -> do
        expr' <- expandTIExpr expr
        return $ IValuePat expr'
      IPredPat expr -> do
        expr' <- expandTIExpr expr
        return $ IPredPat expr'
      IIndexedPat pat1 exprs -> do
        pat1' <- expandPattern pat1
        exprs' <- mapM expandTIExpr exprs
        return $ IIndexedPat pat1' exprs'
      ILetPat bindings pat1 -> do
        bindings' <- mapM expandBinding bindings
        pat1' <- expandPattern pat1
        return $ ILetPat bindings' pat1'
      INotPat pat1 -> do
        pat1' <- expandPattern pat1
        return $ INotPat pat1'
      IAndPat pat1 pat2 -> do
        pat1' <- expandPattern pat1
        pat2' <- expandPattern pat2
        return $ IAndPat pat1' pat2'
      IOrPat pat1 pat2 -> do
        pat1' <- expandPattern pat1
        pat2' <- expandPattern pat2
        return $ IOrPat pat1' pat2'
      IForallPat pat1 pat2 -> do
        pat1' <- expandPattern pat1
        pat2' <- expandPattern pat2
        return $ IForallPat pat1' pat2'
      ITuplePat pats -> do
        pats' <- mapM expandPattern pats
        return $ ITuplePat pats'
      IInductivePat name pats -> do
        pats' <- mapM expandPattern pats
        return $ IInductivePat name pats'
      ILoopPat name range pat1 pat2 -> do
        -- ILoopRange contains IExpr, need to process it
        range' <- expandLoopRange range
        pat1' <- expandPattern pat1
        pat2' <- expandPattern pat2
        return $ ILoopPat name range' pat1' pat2'
      IPApplyPat expr pats -> do
        expr' <- expandTIExpr expr
        pats' <- mapM expandPattern pats
        return $ IPApplyPat expr' pats'
      IDApplyPat pat1 pats -> do
        pat1' <- expandPattern pat1
        pats' <- mapM expandPattern pats
        return $ IDApplyPat pat1' pats'
      -- Patterns without IExpr
      IWildCard -> return IWildCard
      IPatVar name -> return $ IPatVar name
      IContPat -> return IContPat
      IVarPat name -> return $ IVarPat name
      IInductiveOrPApplyPat name pats -> do
        pats' <- mapM expandPattern pats
        return $ IInductiveOrPApplyPat name pats'
      ISeqNilPat -> return ISeqNilPat
      ISeqConsPat pat1 pat2 -> do
        pat1' <- expandPattern pat1
        pat2' <- expandPattern pat2
        return $ ISeqConsPat pat1' pat2'
      ILaterPatVar -> return ILaterPatVar
    
    -- Helper function to process loop ranges
    expandLoopRange :: ILoopRange -> EvalM ILoopRange
    expandLoopRange (ILoopRange expr1 expr2 pat) = do
      expr1' <- expandTIExpr expr1
      expr2' <- expandTIExpr expr2
      pat' <- expandPattern pat
      return $ ILoopRange expr1' expr2' pat'

