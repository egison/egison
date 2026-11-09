{-# LANGUAGE LambdaCase #-}
{- |
Module      : Language.Egison.Type.TensorDemand
Licence     : MIT

Classification of unannotated lambda parameters whose values flow through
arithmetic into a position whose type is fixed to @Tensor@
(design/tensor-demand-propagation.md).

A position is fixed to @Tensor@ when it is the tensor operand of a tensor
primitive (@contract@, @tensorMap@, @tensorMap2@, index references,
@transpose@, @flipIndices@) or an argument position whose parameter type is
@Tensor ...@ in the type scheme that the environment assigns to the applied
function.  An arithmetic chain is a tree of binary arithmetic operator
applications; its leaves are the operands that are not such applications.

A parameter is demanded when some fixed tensor position in the body receives
an arithmetic chain with that parameter among its leaves.  Such a parameter
is typed @Tensor a@ instead of a bare type variable, so the arithmetic is
lifted by the ordinary insertion of @tensorMap@ / @tensorMap2@ and the
tensor primitive receives a whole tensor.  A parameter passed bare, a
literal, or a function value at a tensor position is not affected: those
positions keep rank-0 admission or ordinary unification.
-}

module Language.Egison.Type.TensorDemand
  ( demandedParameters
  , arithmeticOperators
  ) where

import           Data.Foldable             (toList)
import qualified Data.Set                  as Set

import           Language.Egison.AST       (PDPatternBase (..))
import           Language.Egison.IExpr
import           Language.Egison.Type.Env  (TypeEnv, lookupEnv)
import           Language.Egison.Type.Types (Type (..), TypeScheme (..))

-- | For each parameter name of a lambda, whether the body demands a tensor
-- for it.  The environment supplies the type schemes of the functions
-- applied in the body; names bound inside the body are never looked up.
demandedParameters :: TypeEnv -> [String] -> IExpr -> [Bool]
demandedParameters env names body = map (`Set.member` demanded) names
  where
    parameters = Set.fromList names
    demanded = collect env parameters parameters body

-- | Binary operators whose application forms an arithmetic chain.
arithmeticOperators :: Set.Set String
arithmeticOperators = Set.fromList ["+", "-", "*", "/", "+'", "-'", "*'", "/'", "^"]

isArithmetic :: IExpr -> Bool
isArithmetic (IApplyExpr (IVarExpr op) [_, _]) = op `Set.member` arithmeticOperators
isArithmetic _ = False

-- | The visible parameters at the leaves of the arithmetic chain rooted at
-- an expression.  A bare variable or any other non-arithmetic expression is
-- not a chain and yields nothing.
chainLeaves :: Set.Set String -> IExpr -> Set.Set String
chainLeaves visible expr
  | isArithmetic expr = leaves expr
  | otherwise = Set.empty
  where
    leaves (IApplyExpr (IVarExpr op) [a, b])
      | op `Set.member` arithmeticOperators = leaves a `Set.union` leaves b
    leaves (IVarExpr x)
      | x `Set.member` visible = Set.singleton x
    leaves _ = Set.empty

-- | The arguments of an expression that stand at positions fixed to Tensor.
tensorPositions :: TypeEnv -> Set.Set String -> IExpr -> [IExpr]
tensorPositions env bound = \case
  ITensorContractExpr t      -> [t]
  ITensorMapExpr _ t         -> [t]
  ITensorMap2Expr _ t u      -> [t, u]
  ITensorMap2WedgeExpr _ t u -> [t, u]
  IIndexedExpr _ base _      -> [base]
  ISubrefsExpr _ base _      -> [base]
  ISuprefsExpr _ base _      -> [base]
  IUserrefsExpr _ base _     -> [base]
  ITransposeExpr _ t         -> [t]
  IFlipIndicesExpr t         -> [t]
  IApplyExpr (IVarExpr f) args
    | not (f `Set.member` bound) -> fromScheme f args
  IWedgeApplyExpr (IVarExpr f) args
    | not (f `Set.member` bound) -> fromScheme f args
  _                          -> []
  where
    fromScheme f args = case lookupEnv (Var f []) env of
      Just (Forall _ _ _ ty) -> [arg | (arg, TTensor _) <- zip args (domains ty)]
      Nothing                -> []
    domains (TFun domain rest) = domain : domains rest
    domains _                  = []

-- | Collect the demanded parameters of a body.  @visible@ holds the lambda
-- parameters that are not shadowed at the current position; @bound@ holds
-- every name bound locally, including the parameters.
collect :: TypeEnv -> Set.Set String -> Set.Set String -> IExpr -> Set.Set String
collect env = go
  where
    go visible bound expr =
      Set.unions (map (chainLeaves visible) (tensorPositions env bound expr))
        `Set.union` children visible bound expr

    hide visible bound names = (visible `Set.difference` set, bound `Set.union` set)
      where set = Set.fromList names

    under visible bound names body = let (v, b) = hide visible bound names in go v b body

    many visible bound = Set.unions . map (go visible bound)

    children visible bound = \case
      ILambdaExpr _ ps body        -> under visible bound (map extractNameFromVar ps) body
      IMemoizedLambdaExpr ns body  -> under visible bound ns body
      ICambdaExpr n body           -> under visible bound [n] body
      ILetExpr bs body             ->
        let names = concatMap (dataPatternVars . fst) bs
        in many visible bound (map snd bs) `Set.union` under visible bound names body
      ILetRecExpr bs body          ->
        let names = concatMap (dataPatternVars . fst) bs
            (v, b) = hide visible bound names
        in many v b (map snd bs) `Set.union` go v b body
      IDoExpr bs body              ->
        let names = concatMap (dataPatternVars . fst) bs
            (v, b) = hide visible bound names
        in many v b (map snd bs) `Set.union` go v b body
      IWithSymbolsExpr ns body     -> under visible bound ns body
      IMatchExpr _ target matcher clauses fallback ->
        go visible bound target `Set.union` go visible bound matcher
          `Set.union` Set.unions (map (clause visible bound) clauses)
          `Set.union` maybe Set.empty (go visible bound) fallback
      IMatchAllExpr _ target matcher clauses ->
        go visible bound target `Set.union` go visible bound matcher
          `Set.union` Set.unions (map (clause visible bound) clauses)
      IMatcherExpr defs            -> Set.unions
        [ go visible bound matcher `Set.union`
            Set.unions [ under visible bound (dataPatternVars pat) body | (pat, body) <- bodies ]
        | (_, matcher, bodies) <- defs ]
      IIndexedExpr _ base indices  ->
        go visible bound base `Set.union` many visible bound (concatMap toList indices)
      ISubrefsExpr _ a b           -> many visible bound [a, b]
      ISuprefsExpr _ a b           -> many visible bound [a, b]
      IUserrefsExpr _ a b          -> many visible bound [a, b]
      IInductiveDataExpr _ es      -> many visible bound es
      ITupleExpr es                -> many visible bound es
      ICollectionExpr es           -> many visible bound es
      IVectorExpr es               -> many visible bound es
      IHashExpr pairs              -> many visible bound (concat [[k, v] | (k, v) <- pairs])
      IConsExpr a b                -> many visible bound [a, b]
      IJoinExpr a b                -> many visible bound [a, b]
      ISeqExpr a b                 -> many visible bound [a, b]
      IIfExpr a b c                -> many visible bound [a, b, c]
      IQuoteExpr e                 -> go visible bound e
      IQuoteSymbolExpr e           -> go visible bound e
      IApplyExpr f args            -> many visible bound (f : args)
      IWedgeApplyExpr f args       -> many visible bound (f : args)
      IGenerateTensorExpr a b      -> many visible bound [a, b]
      ITensorExpr a b              -> many visible bound [a, b]
      ITensorContractExpr e        -> go visible bound e
      ITensorMapExpr a b           -> many visible bound [a, b]
      ITensorMap2Expr a b c        -> many visible bound [a, b, c]
      ITensorMap2WedgeExpr a b c   -> many visible bound [a, b, c]
      ITransposeExpr a b           -> many visible bound [a, b]
      IFlipIndicesExpr e           -> go visible bound e
      IReshape _ e                 -> go visible bound e
      IRuntimeDispatch _ _ _ args  -> many visible bound args
      _                            -> Set.empty

    clause visible bound (pat, body) =
      many visible bound (patternExprs pat) `Set.union` under visible bound (patternVars pat) body

-- | Names bound by a match pattern (over-approximated: every pattern variable
-- in the pattern counts, which can only reduce the demanded set).
patternVars :: IPattern -> [String]
patternVars = \case
  IPatVar name                 -> [name]
  IVarPat name                 -> [name]
  IIndexedPat p _              -> patternVars p
  ILetPat bs p                 -> concatMap (dataPatternVars . fst) bs ++ patternVars p
  INotPat p                    -> patternVars p
  IAndPat p q                  -> patternVars p ++ patternVars q
  IOrPat p q                   -> patternVars p ++ patternVars q
  IForallPat p q               -> patternVars p ++ patternVars q
  ITuplePat ps                 -> concatMap patternVars ps
  IInductivePat _ ps           -> concatMap patternVars ps
  ILoopPat name _ p q          -> name : patternVars p ++ patternVars q
  IPApplyPat _ ps              -> concatMap patternVars ps
  IInductiveOrPApplyPat _ ps   -> concatMap patternVars ps
  ISeqConsPat p q              -> patternVars p ++ patternVars q
  IDApplyPat p ps              -> patternVars p ++ concatMap patternVars ps
  _                            -> []

-- | Expressions embedded in a match pattern.
patternExprs :: IPattern -> [IExpr]
patternExprs = \case
  IValuePat e                  -> [e]
  IPredPat e                   -> [e]
  IIndexedPat p es             -> patternExprs p ++ es
  ILetPat bs p                 -> map snd bs ++ patternExprs p
  INotPat p                    -> patternExprs p
  IAndPat p q                  -> patternExprs p ++ patternExprs q
  IOrPat p q                   -> patternExprs p ++ patternExprs q
  IForallPat p q               -> patternExprs p ++ patternExprs q
  ITuplePat ps                 -> concatMap patternExprs ps
  IInductivePat _ ps           -> concatMap patternExprs ps
  ILoopPat _ (ILoopRange a b p) q r -> a : b : patternExprs p ++ patternExprs q ++ patternExprs r
  IPApplyPat e ps              -> e : concatMap patternExprs ps
  IInductiveOrPApplyPat _ ps   -> concatMap patternExprs ps
  ISeqConsPat p q              -> patternExprs p ++ patternExprs q
  IDApplyPat p ps              -> patternExprs p ++ concatMap patternExprs ps
  _                            -> []

-- | Names bound by a primitive data pattern of a binding.
dataPatternVars :: PDPatternBase Var -> [String]
dataPatternVars = \case
  PDPatVar v           -> [extractNameFromVar v]
  PDInductivePat _ ps  -> concatMap dataPatternVars ps
  PDTuplePat ps        -> concatMap dataPatternVars ps
  PDConsPat p q        -> dataPatternVars p ++ dataPatternVars q
  PDSnocPat p q        -> dataPatternVars p ++ dataPatternVars q
  PDFracPat p q        -> dataPatternVars p ++ dataPatternVars q
  PDPlusPat p          -> dataPatternVars p
  PDTermPat p q        -> dataPatternVars p ++ dataPatternVars q
  PDSymbolPat p q      -> dataPatternVars p ++ dataPatternVars q
  _                    -> []
