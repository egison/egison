{-# LANGUAGE LambdaCase #-}
{- |
Module      : Language.Egison.Type.TensorDemand
Licence     : MIT

Potential tensor inputs of a retained function body. Fixed tensor parameter
positions supply the requirements. Result requirements pass through ordinary
applications and immutable bindings; function names do not classify products
or arithmetic. The inference procedure checks each proposed body at its use.
-}
module Language.Egison.Type.TensorDemand (demandedParameters, demandedInferredParameters) where

import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Language.Egison.IExpr
import           Language.Egison.Type.Env (TypeEnv, lookupEnv)
import           Language.Egison.Type.Types (Type (..), TypeScheme (..))

-- A binding carries its lexical environment, so shadowing cannot redirect
-- the dependency of an earlier definition to a later binding.
data Binding = Binding IExpr Bindings | Hidden
type Bindings = Map.Map String Binding

demandedParameters :: TypeEnv -> [String] -> IExpr -> [Bool]
demandedParameters env names body =
  let required = collect False env Map.empty (Set.fromList names) body
  in map (`Set.member` required) names

-- Check the proposed dependency against the inferred application types.
-- The quoted nodes below are analysis boundaries, never executable output:
-- requirements do not flow from an unlifted result to unrelated arguments.
demandedInferredParameters :: TypeEnv -> [String] -> TIExpr -> [Bool]
demandedInferredParameters env names body = demandedParameters env names (stripType (flowTree body))
  where
    flowTree expression =
      let mapped = expression { tiExprNode = mapTIExprChildren flowTree (tiExprNode expression) }
          boundary function arguments =
            if or (zipWith lifted (domains (tiExprType function)) arguments)
              then mapped else mapped { tiExprNode = TIQuoteExpr mapped }
      in case tiExprNode mapped of
        TIApplyExpr function arguments -> boundary function arguments
        TIWedgeApplyExpr function arguments -> boundary function arguments
        _ -> mapped
    domains (TFun argument result) = argument : domains result
    domains _ = []
    lifted TTensor{} _ = False
    lifted _ argument = case tiExprType argument of TTensor{} -> True; _ -> False

bind :: Bindings -> [(PDPatternBase Var, IExpr)] -> Bindings
bind = foldl $ \scope (pattern, value) -> case pattern of
  PDPatVar variable -> Map.insert (extractNameFromVar variable) (Binding value scope) scope
  _ -> hide scope (dataPatternVars pattern)

hide :: Bindings -> [String] -> Bindings
hide = foldl (\scope name -> Map.insert name Hidden scope)

-- Follow data dependencies only. A function value and a container are not
-- implicitly traversed. At least one application is needed to propose a new
-- tensor input; a bare parameter is handled by ordinary type inference.
dependencies :: Bool -> Bindings -> Set.Set String -> IExpr -> Set.Set String
dependencies applied scope visible = \case
  IVarExpr name -> case Map.lookup name scope of
    Just (Binding value captured) -> dependencies applied captured visible value
    Just Hidden -> Set.empty
    Nothing -> if applied && Set.member name visible then Set.singleton name else Set.empty
  IApplyExpr _ arguments -> Set.unions (map (dependencies True scope visible) arguments)
  IWedgeApplyExpr _ arguments -> Set.unions (map (dependencies True scope visible) arguments)
  ILetExpr bindings body -> dependencies applied (bind scope bindings) visible body
  ILetRecExpr bindings body -> dependencies applied (bind scope bindings) visible body
  _ -> Set.empty

-- Both built-in operations and user functions contribute requirements through
-- their parameter types. Aliases retain the original binding's scope.
tensorArguments :: TypeEnv -> Bindings -> IExpr -> [IExpr]
tensorArguments env scope = \case
  ITensorContractExpr t -> [t]
  ITensorMapExpr _ t -> [t]
  ITensorMap2Expr _ t u -> [t,u]
  ITensorMap2WedgeExpr _ t u -> [t,u]
  IIndexedExpr _ t _ -> [t]
  ISubrefsExpr _ t _ -> [t]
  ISuprefsExpr _ t _ -> [t]
  IUserrefsExpr _ t _ -> [t]
  ITransposeExpr _ t -> [t]
  IFlipIndicesExpr t -> [t]
  IApplyExpr function arguments -> select function arguments
  IWedgeApplyExpr function arguments -> select function arguments
  _ -> []
  where
    select function arguments = [argument | (argument,True) <- zip arguments (parameters scope function)]
    parameters bindings (IVarExpr name) = case Map.lookup name bindings of
      Just (Binding value captured) -> parameters captured value
      Just Hidden -> []
      Nothing -> case lookupEnv (Var name []) env of
        Just (Forall _ _ _ ty) -> domains ty
        Nothing -> []
    parameters bindings (ILambdaExpr _ names body) =
      let names' = map extractNameFromVar names
          required = collect True env (foldr Map.delete bindings names') (Set.fromList names') body
      in map (`Set.member` required) names'
    parameters bindings (ILetExpr definitions body) = parameters (bind bindings definitions) body
    parameters _ _ = []
    domains (TFun domain result) = (case domain of TTensor{} -> True; _ -> False) : domains result
    domains _ = []

collect :: Bool -> TypeEnv -> Bindings -> Set.Set String -> IExpr -> Set.Set String
collect bare env = go
  where
    go scope visible expr =
      -- An index reference itself consumes the indexed value. A tensor
      -- argument must reach that reference whole, including when its base
      -- is a bare parameter rather than an arithmetic application.
      let directIndex = case expr of IIndexedExpr{} -> True; _ -> False
      in Set.unions (map (dependencies (bare || directIndex) scope visible) (tensorArguments env scope expr))
        `Set.union` children scope visible expr
    under scope visible names = go (hide scope names) (visible `Set.difference` Set.fromList names)
    many scope visible = Set.unions . map (go scope visible)
    children scope visible = \case
      ILambdaExpr _ ps body -> under scope visible (map extractNameFromVar ps) body
      IMemoizedLambdaExpr ns body -> under scope visible ns body
      ICambdaExpr n body -> under scope visible [n] body
      ILetExpr bs body -> many scope visible (map snd bs) `Set.union` go (bind scope bs) visible body
      ILetRecExpr bs body ->
        let names = concatMap (dataPatternVars . fst) bs
            scoped = hide scope names
        in many scoped visible (map snd bs) `Set.union` go (bind scoped bs) visible body
      IDoExpr bs body -> many scope visible (map snd bs) `Set.union`
        under scope visible (concatMap (dataPatternVars . fst) bs) body
      IWithSymbolsExpr ns body -> under scope visible ns body
      IMatchExpr _ target matcher clauses fallback ->
        many scope visible (target : matcher : maybe [] (:[]) fallback) `Set.union`
          Set.unions (map (clause scope visible) clauses)
      IMatchAllExpr _ target matcher clauses ->
        many scope visible [target,matcher] `Set.union` Set.unions (map (clause scope visible) clauses)
      IMatcherExpr defs -> Set.unions
        [ go scope visible matcher `Set.union`
            Set.unions [ under scope visible (dataPatternVars pat) body | (pat,body) <- bodies ]
        | (_,matcher,bodies) <- defs ]
      IIndexedExpr _ base indices -> many scope visible (base : concatMap toList indices)
      ISubrefsExpr _ a b -> many scope visible [a,b]
      ISuprefsExpr _ a b -> many scope visible [a,b]
      IUserrefsExpr _ a b -> many scope visible [a,b]
      IInductiveDataExpr _ es -> many scope visible es
      ITupleExpr es -> many scope visible es
      ICollectionExpr es -> many scope visible es
      IVectorExpr es -> many scope visible es
      IHashExpr pairs -> many scope visible (concat [[k,v] | (k,v) <- pairs])
      IConsExpr a b -> many scope visible [a,b]
      IJoinExpr a b -> many scope visible [a,b]
      ISeqExpr a b -> many scope visible [a,b]
      IIfExpr a b c -> many scope visible [a,b,c]
      IQuoteExpr e -> go scope visible e
      IQuoteSymbolExpr e -> go scope visible e
      IApplyExpr f args -> many scope visible (f:args)
      IWedgeApplyExpr f args -> many scope visible (f:args)
      IGenerateTensorExpr a b -> many scope visible [a,b]
      ITensorExpr a b -> many scope visible [a,b]
      ITensorContractExpr e -> go scope visible e
      ITensorMapExpr a b -> many scope visible [a,b]
      ITensorMap2Expr a b c -> many scope visible [a,b,c]
      ITensorMap2WedgeExpr a b c -> many scope visible [a,b,c]
      ITransposeExpr a b -> many scope visible [a,b]
      IFlipIndicesExpr e -> go scope visible e
      IReshape _ e -> go scope visible e
      IRuntimeDispatch _ _ _ args -> many scope visible args
      _ -> Set.empty
    clause scope visible (pat,body) =
      many scope visible (patternExprs pat) `Set.union` under scope visible (patternVars pat) body

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
