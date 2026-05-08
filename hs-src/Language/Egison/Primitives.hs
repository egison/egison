{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Language.Egison.Primitives
Licence     : MIT

This module provides primitive functions in Egison.
-}

module Language.Egison.Primitives
  ( primitiveEnv
  , primitiveEnvNoIO
  ) where

import           Control.Monad                     (foldM, forM)
import           Control.Monad.Except              (throwError)
import           Control.Monad.IO.Class            (liftIO)

import           Data.IORef
import           Data.Foldable                     (toList)

import qualified Data.Sequence                     as Sq
import qualified Data.Text                         as T
import qualified Data.Vector                       as V

 {--  -- for 'egison-sqlite'
import qualified Database.SQLite3 as SQLite
 --}  -- for 'egison-sqlite'

import           Language.Egison.Core              (applyRef)
import           Language.Egison.Data
import           Language.Egison.Data.Collection   (makeICollection)
import           Language.Egison.Data.Utils        (newEvaluatedObjectRef)
import           Language.Egison.EvalState         (getReductionRulesCount, getDerivativeRulesCount,
                                                    getReductionRuleNames, getDerivativeRuleNames)
import           Language.Egison.IExpr             (Index (..), stringToVar)
import qualified Language.Egison.Math.CAS as CAS
import           Language.Egison.Primitives.Arith
import           Language.Egison.Primitives.IO
import           Language.Egison.Primitives.String
import           Language.Egison.Primitives.Types
import           Language.Egison.Primitives.Utils

primitiveEnv :: IO Env
primitiveEnv = do
  bindings <- forM (constants ++ primitives ++ ioPrimitives) $ \(name, op) -> do
    ref <- newIORef . WHNF $ Value op
    return (stringToVar name, ref)
  return $ extendEnv nullEnv bindings

primitiveEnvNoIO :: IO Env
primitiveEnvNoIO = do
  bindings <- forM (constants ++ primitives) $ \(name, op) -> do
    ref <- newIORef . WHNF $ Value op
    return (stringToVar name, ref)
  return $ extendEnv nullEnv bindings

--
-- Constants
--

constants :: [(String, EgisonValue)]
constants = [ ("f.pi", Float 3.141592653589793)
            , ("f.e" , Float 2.718281828459045)
            ]

--
-- Primitives
--

primitives :: [(String, EgisonValue)]
primitives =
  map (\(name, fn) -> (name, PrimitiveFunc (fn name))) strictPrimitives
  ++ map (\(name, fn) -> (name, LazyPrimitiveFunc (fn name))) lazyPrimitives
  ++ primitiveArithFunctions
  ++ primitiveStringFunctions
  ++ primitiveTypeFunctions
    where
      strictPrimitives =
        [ ("addSubscript", addSubscript)
        , ("addSuperscript", addSuperscript)

        , ("assert",      assert)
        , ("assertEqual", assertEqual)

        , ("sortWithSign", sortWithSign)
        , ("updateFunctionArgs", updateFunctionArgs)
        , ("casTerms", casTermsPrim)
        , ("casFromTerms", casFromTermsPrim)
        , ("termCoeff", termCoeffPrim)
        , ("termMonomial", termMonomialPrim)
        , ("typeOf", typeOfPrim)
        , ("inspect", inspectPrim)
        , ("differentialClosed", differentialClosedPrim)
        , ("isInPolyAtoms", isInPolyAtomsPrim)
        , ("isPureInteger", isPureIntegerPrim)
        , ("isPureFraction", isPureFractionPrim)
        , ("numReductionRules", numReductionRulesPrim)
        , ("numDerivativeRules", numDerivativeRulesPrim)
        , ("ruleNames", ruleNamesPrim)
        , ("derivativeNames", derivativeNamesPrim)
        , ("hasReductionRule", hasReductionRulePrim)
        , ("hasDerivativeRule", hasDerivativeRulePrim)
        , ("containsAnySymbol", containsAnySymbolPrim)
        , ("iterateRulesCAS", iterateRulesCASPrim)
        , ("applyTermRule", applyTermRulePrim)
        , ("mapPolyAll", mapPolyPrim)
        , ("mapTermAll", mapTermPrim)
        , ("mapFracAll", mapFracPrim)
        ]
      lazyPrimitives =
        [ ("tensorShape", tensorShape')
        , ("tensorToList", tensorToList')
        , ("dfOrder", dfOrder')
        ]

--
-- Miscellaneous primitive functions
--

tensorShape' :: String -> LazyPrimitiveFunc
tensorShape' = lazyOneArg tensorShape''
 where
  tensorShape'' (Value (TensorData (Tensor ns _ _))) =
    return . Value . Collection . Sq.fromList $ map toEgison ns
  tensorShape'' (ITensor (Tensor ns _ _)) =
    return . Value . Collection . Sq.fromList $ map toEgison ns
  tensorShape'' _ = return . Value . Collection $ Sq.fromList []

tensorToList' :: String -> LazyPrimitiveFunc
tensorToList' = lazyOneArg tensorToList''
 where
  tensorToList'' (Value (TensorData (Tensor _ xs _))) =
    return . Value . Collection . Sq.fromList $ V.toList xs
  tensorToList'' (ITensor (Tensor _ xs _)) = do
    inners <- liftIO . newIORef $ Sq.fromList (map IElement (V.toList xs))
    return (ICollection inners)
  tensorToList'' x = makeICollection [x]

dfOrder' :: String -> LazyPrimitiveFunc
dfOrder' = lazyOneArg dfOrder''
 where
  dfOrder'' (Value (TensorData (Tensor ns _ is))) =
    return $ Value (toEgison (fromIntegral (length ns - length is) :: Integer))
  dfOrder'' (ITensor (Tensor ns _ is)) =
    return $ Value (toEgison (fromIntegral (length ns - length is) :: Integer))
  dfOrder'' _ = return $ Value (toEgison (0 :: Integer))

addSubscript :: String -> PrimitiveFunc
addSubscript = twoArgs $ \fn sub ->
  case (fn, sub) of
    (CASData (CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name is, 1)]]),
     CASData s@(CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol _ _ [], 1)]])) ->
      return $ CASData $ CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name (is ++ [Sub s]), 1)]]
    (CASData (CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name is, 1)]]),
     CASData s@(CASPoly [CASTerm (CASInteger _) []])) ->
      return $ CASData $ CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name (is ++ [Sub s]), 1)]]
    (CASData (CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name is, 1)]]),
     CASData s@(CASInteger _)) ->
      let s' = CASPoly [CASTerm s []]
      in return $ CASData $ CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name (is ++ [Sub s']), 1)]]
    _ -> throwErrorWithTrace (TypeMismatch "symbol or integer" (Value fn))

addSuperscript :: String -> PrimitiveFunc
addSuperscript = twoArgs $ \fn sub ->
  case (fn, sub) of
    (CASData (CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name is, 1)]]),
     CASData s@(CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol _ _ [], 1)]])) ->
      return $ CASData $ CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name (is ++ [Sup s]), 1)]]
    (CASData (CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name is, 1)]]),
     CASData s@(CASPoly [CASTerm (CASInteger _) []])) ->
      return $ CASData $ CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name (is ++ [Sup s]), 1)]]
    (CASData (CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name is, 1)]]),
     CASData s@(CASInteger _)) ->
      let s' = CASPoly [CASTerm s []]
      in return $ CASData $ CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name (is ++ [Sup s']), 1)]]
    _ -> throwErrorWithTrace (TypeMismatch "symbol" (Value fn))

updateFunctionArgs :: String -> PrimitiveFunc
updateFunctionArgs = twoArgs' $ \funcVal newArgsColl ->
  case (funcVal, newArgsColl) of
    (CASData (CASPoly [CASTerm (CASInteger 1) [(CAS.FunctionData name _, 1)]]), Collection argsSeq) -> do
      args' <- mapM extractCAS (toList argsSeq)
      return $ CASData $ CASPoly [CASTerm (CASInteger 1) [(CAS.FunctionData name args', 1)]]
    _ -> throwErrorWithTrace (TypeMismatch "function value and collection of scalars" (Value funcVal))
 where
  extractCAS (CASData cv) = return cv
  extractCAS val = throwErrorWithTrace (TypeMismatch "scalar" (Value val))

-- | Convert a CASValue into a list of single-term polynomials.
-- A CASPoly is decomposed into its terms; an integer/factor becomes a singleton list;
-- zero becomes the empty list. Used by the parametric `poly` matcher to expose
-- term-level decomposition to Egison code.
casTermsPrim :: String -> PrimitiveFunc
casTermsPrim = oneArg' $ \v -> case v of
  CASData cv -> return . Collection . Sq.fromList . map CASData $ casValueToTerms cv
  _ -> throwErrorWithTrace (TypeMismatch "CAS value" (Value v))

casValueToTerms :: CAS.CASValue -> [CAS.CASValue]
casValueToTerms (CAS.CASInteger 0) = []
casValueToTerms v@(CAS.CASInteger _) = [CAS.CASPoly [CASTerm v []]]
casValueToTerms (CAS.CASFactor sym) = [CAS.CASPoly [CASTerm (CAS.CASInteger 1) [(sym, 1)]]]
casValueToTerms (CAS.CASPoly ts) = map (\t -> CAS.CASPoly [t]) ts
casValueToTerms v@(CAS.CASFrac _ _) = [v]

-- | Build a CASValue from a collection of single-term polynomials.
-- Each element should be a single-term polynomial (as produced by casTerms);
-- the result is normalized by re-running through casPlus.
casFromTermsPrim :: String -> PrimitiveFunc
casFromTermsPrim = oneArg' $ \v -> case v of
  Collection seq_ -> do
    cvs <- mapM extractCAS (toList seq_)
    return . CASData $ foldr CAS.casPlus (CAS.CASInteger 0) cvs
  _ -> throwErrorWithTrace (TypeMismatch "collection of CAS values" (Value v))
 where
  extractCAS (CASData cv) = return cv
  extractCAS val = throwErrorWithTrace (TypeMismatch "CAS value" (Value val))

-- | Extract the coefficient of a single-term CASValue.
-- For a single-term polynomial CASPoly [CASTerm c _], returns c.
-- For a bare integer, returns it as-is. For a bare factor, returns 1.
termCoeffPrim :: String -> PrimitiveFunc
termCoeffPrim = oneArg' $ \v -> case v of
  CASData (CAS.CASPoly [CASTerm c _]) -> return $ CASData c
  CASData (CAS.CASPoly []) -> return $ CASData (CAS.CASInteger 0)
  CASData v0@(CAS.CASInteger _) -> return $ CASData v0
  CASData (CAS.CASFactor _) -> return $ CASData (CAS.CASInteger 1)
  CASData v0@(CAS.CASFrac _ _) -> return $ CASData v0
  _ -> throwErrorWithTrace (TypeMismatch "single-term CAS value" (Value v))

-- | Phase B: term-level rule application primitive.
--
-- Given a `term`-level rule with LHS `<lhsValue>` (typically a single-monomial
-- value like `i^2`, with implicit coefficient 1) and RHS `<rhsValue>`, apply
-- the rule recursively to each term inside `<input>`. For each term:
--   - If the term's monomial equals LHS's monomial, replace the term with
--     `coeff × rhsValue` (where coeff is the term's coefficient).
--   - Otherwise, keep the term as-is.
-- The result is the sum of all (possibly-transformed) terms.
--
-- This makes user `declare rule auto term LHS = RHS` declarations recurse
-- into polynomial terms, so e.g. `(1+i)*(1-i) = 1 - i^2` simplifies to `2`
-- when the rule `i^2 = -1` is registered, even without a built-in
-- `casRewriteI` rule.
applyTermRulePrim :: String -> PrimitiveFunc
applyTermRulePrim = threeArgs' $ \lhsV rhsV inputV ->
  case (lhsV, rhsV, inputV) of
    (CASData lhs, CASData rhs, CASData input) ->
      return $ CASData $ applyTermRuleCAS lhs rhs input
    _ -> throwErrorWithTrace (TypeMismatch "CAS values" (Value lhsV))

-- | The actual term-level rule application logic at the CASValue level.
--
-- Containment matching: the LHS monomial is treated as a *factor* of the
-- target term's monomial. If the target contains LHS as a sub-factor, we
-- replace one occurrence of LHS with RHS. With `iterateRules` calling this
-- repeatedly to fixpoint, higher powers like `u^4` reduce as `u^4 → -u^2 →
-- 1` for the rule `u^2 = -1`.
applyTermRuleCAS :: CAS.CASValue -> CAS.CASValue -> CAS.CASValue -> CAS.CASValue
applyTermRuleCAS lhsValue rhsValue input =
  case extractLhsMonomial lhsValue of
    Nothing -> input  -- Can't extract a monomial from LHS → can't apply
    Just lhsMono -> transformValue lhsMono rhsValue input
  where
    -- Extract the monomial part of a single-coefficient-1 term value.
    extractLhsMonomial :: CAS.CASValue -> Maybe CAS.Monomial
    extractLhsMonomial (CAS.CASPoly [CAS.CASTerm (CAS.CASInteger 1) mono]) = Just mono
    extractLhsMonomial (CAS.CASFactor sym) = Just [(sym, 1)]
    extractLhsMonomial _ = Nothing

    -- Apply to a value, recursing into Frac numerator/denominator.
    transformValue :: CAS.Monomial -> CAS.CASValue -> CAS.CASValue -> CAS.CASValue
    transformValue lhsMono rhs (CAS.CASPoly terms) =
      foldr CAS.casPlus (CAS.CASInteger 0)
            (map (transformTerm lhsMono rhs) terms)
    transformValue lhsMono rhs (CAS.CASFrac n d) =
      CAS.casFrac (transformValue lhsMono rhs n) d
    transformValue _ _ v = v

    -- Apply to a single term: if the term's monomial contains LHS as a
    -- factor, replace that factor with RHS. Otherwise keep the term.
    transformTerm :: CAS.Monomial -> CAS.CASValue -> CAS.CASTerm -> CAS.CASValue
    transformTerm [] _rhs (CAS.CASTerm coeff mono) =
      -- LHS is the empty monomial (constant 1). Don't transform terms here
      -- (avoid replacing every term with rhs); the user should use a
      -- poly-level rule for constant replacement.
      CAS.CASPoly [CAS.CASTerm coeff mono]
    transformTerm lhsMono rhs (CAS.CASTerm coeff mono) =
      case monomialContains lhsMono mono of
        Just remaining ->
          -- coeff × rhs × (remaining monomial)
          let remTerm = CAS.CASPoly [CAS.CASTerm coeff remaining]
          in CAS.casMult remTerm rhs
        Nothing ->
          CAS.CASPoly [CAS.CASTerm coeff mono]

    -- Check if `lhsMono` is contained in `target` as a sub-factor. If so,
    -- return the remaining monomial (target minus lhsMono). Otherwise Nothing.
    -- Each (sym, exp) in lhsMono must be matched by (sym, exp') in target
    -- with exp' >= exp.
    monomialContains :: CAS.Monomial -> CAS.Monomial -> Maybe CAS.Monomial
    monomialContains [] target = Just target
    monomialContains ((sym, expL) : rest) target =
      case lookup sym target of
        Just expT | expT >= expL ->
          -- Subtract: keep all entries except (sym, _), then add (sym, expT-expL)
          -- if the remaining exponent is positive
          let target' = [(s, e) | (s, e) <- target, s /= sym]
                     ++ [(sym, expT - expL) | expT > expL]
          in monomialContains rest target'
        _ -> Nothing

-- | Phase A.5: structural traversal primitives `mapPoly`, `mapTerm`, `mapFrac`.
-- Each takes a user closure `f : MathValue -> MathValue` and a CAS value, and
-- recursively applies `f` at the corresponding granularity, descending into
-- sub-MathValues inside Apply1-4 / Quote / Function / Symbol indices.
--
-- - mapFrac f v : applies f at every Frac node (every MathValue is a Frac)
-- - mapPoly f v : applies f at every (sub-)polynomial; same nodes as mapFrac
--                 since a MathValue with denom=1 is also a poly. Useful when
--                 the user's pattern targets a poly shape (e.g. `$a + $b`).
-- - mapTerm f v : applies f to each *term* of every poly, lifting each term
--                 to a single-term MathValue before passing to f.
--
-- All variants iterate `f` at each visited node until a fixpoint (no change).
-- Traversal is bottom-up: deeper sub-expressions are reduced first.
mapPolyPrim :: String -> PrimitiveFunc
mapPolyPrim _ args = case args of
  [funcVal, CASData v] -> CASData <$> mapPolyCAS funcVal v
  (a:_) -> throwErrorWithTrace (TypeMismatch "function and CAS value" (Value a))
  []    -> throwError $ Default "mapPolyAll: no arguments"

mapTermPrim :: String -> PrimitiveFunc
mapTermPrim _ args = case args of
  [funcVal, CASData v] -> CASData <$> mapTermCAS funcVal v
  (a:_) -> throwErrorWithTrace (TypeMismatch "function and CAS value" (Value a))
  []    -> throwError $ Default "mapTermAll: no arguments"

mapFracPrim :: String -> PrimitiveFunc
mapFracPrim _ args = case args of
  [funcVal, CASData v] -> CASData <$> mapFracCAS funcVal v
  (a:_) -> throwErrorWithTrace (TypeMismatch "function and CAS value" (Value a))
  []    -> throwError $ Default "mapFracAll: no arguments"

-- | Apply a user closure to a CAS value, returning the resulting CAS value.
applyRuleClosure :: EgisonValue -> CAS.CASValue -> EvalM CAS.CASValue
applyRuleClosure f cv = do
  ref <- newEvaluatedObjectRef (Value (CASData cv))
  result <- applyRef nullEnv (Value f) [ref]
  case result of
    Value (CASData cv') -> return cv'
    other -> throwError $ Default $ "rule must return a CAS value, but got: " ++ show other

-- | Apply rule until fixpoint (CAS values become equal across iterations).
applyRuleFix :: EgisonValue -> CAS.CASValue -> EvalM CAS.CASValue
applyRuleFix f cv = do
  cv' <- applyRuleClosure f cv
  if cv' == cv then return cv else applyRuleFix f cv'

-- | CAS-specialised replacement for the lib's `iterateRules`. Runs the
-- entire rule-application + fixpoint loop in Haskell, eliminating the
-- per-iteration Egison-level fold/recursion/== overhead. Emitted by
-- desugar for `mathNormalize` (see Desugar.hs `buildMathNormalizeRedef`).
iterateRulesCASPrim :: String -> PrimitiveFunc
iterateRulesCASPrim name args = case args of
  [Collection ruleSeq, CASData v0] ->
    -- Collection holds EgisonValues (functions). Pass them directly to
    -- applyRuleClosure which wraps each with `Value` and calls applyRef.
    CASData <$> iterateRulesLoop (toList ruleSeq) v0
  [_, v] ->
    -- Non-CAS input: nothing to iterate (the lib invariant is that
    -- mathNormalize is only invoked on MathValue/CASData).
    return v
  _ -> throwErrorWithTrace (ArgumentsNumPrimitive name 2 (length args))

iterateRulesLoop :: [EgisonValue] -> CAS.CASValue -> EvalM CAS.CASValue
iterateRulesLoop rules v = do
  v' <- foldM (\acc r -> applyRuleClosure r acc) v rules
  if v' == v then return v else iterateRulesLoop rules v'

-- | mapPoly / mapFrac: traverse, apply at each (sub-)MathValue node.
-- Sub-recursion is gated on the outer "structure" of the value: we recurse
-- into Apply1-4/Quote/FunctionData arguments (which are user-visible
-- MathValue sub-expressions) and into Frac numerator/denominator. We do NOT
-- recurse into a term's *coefficient* slot, because that's an internal
-- representation detail (e.g. `2*x` has coef 2 sitting next to the monomial
-- `x`; the user's rule would otherwise be applied to bare integer 2).
mapFracCAS :: EgisonValue -> CAS.CASValue -> EvalM CAS.CASValue
mapFracCAS = mapPolyCAS

mapPolyCAS :: EgisonValue -> CAS.CASValue -> EvalM CAS.CASValue
mapPolyCAS f v = do
  v' <- descendCASNoCoef (mapPolyCAS f) v
  applyRuleFix f v'

-- | mapTerm: traverse, apply at each Term (lifted to single-term MathValue).
mapTermCAS :: EgisonValue -> CAS.CASValue -> EvalM CAS.CASValue
mapTermCAS f v = do
  v' <- descendCASNoCoef (mapTermCAS f) v
  case v' of
    CAS.CASPoly terms -> do
      newTerms <- mapM (\t -> applyRuleFix f (CAS.CASPoly [t])) terms
      return $ foldr CAS.casPlus (CAS.CASInteger 0) newTerms
    _ -> applyRuleFix f v'

-- | Variant of descendCAS that does NOT recurse into term coefficients.
-- Used by mapPoly/mapTerm/mapFrac: the coefficient is a raw integer scalar
-- representing multiplicity, not a user-rewriteable sub-expression.
descendCASNoCoef :: (CAS.CASValue -> EvalM CAS.CASValue) -> CAS.CASValue -> EvalM CAS.CASValue
descendCASNoCoef _ v@(CAS.CASInteger _) = return v
descendCASNoCoef recur (CAS.CASFactor sym) = CAS.casNormalize . CAS.CASFactor <$> descendSymbol recur sym
descendCASNoCoef recur (CAS.CASPoly terms) = (CAS.casNormalize . CAS.CASPoly) <$> mapM (descendTermNoCoef recur) terms
descendCASNoCoef recur (CAS.CASFrac n d) = CAS.casFrac <$> recur n <*> recur d

descendTermNoCoef :: (CAS.CASValue -> EvalM CAS.CASValue) -> CAS.CASTerm -> EvalM CAS.CASTerm
descendTermNoCoef recur (CAS.CASTerm coef factors) = do
  factors' <- mapM (\(sym, e) -> (\s -> (s, e)) <$> descendSymbol recur sym) factors
  return $ CAS.CASTerm coef factors'

-- | Recurse into sub-CASValues inside the structure of a CASValue.
-- Calls `recur` on each immediate sub-MathValue (in Apply args, Quote, Function,
-- and the coefficient slot of each term). Does NOT call f on the value itself.
-- The reconstructed value is re-normalized via casNormalize so that downstream
-- pattern matching sees canonical form (the matcher relies on Frac (Plus ...)
-- shape, which raw `CASPoly` constructors may not satisfy after edits).
descendSymbol :: (CAS.CASValue -> EvalM CAS.CASValue) -> CAS.SymbolExpr -> EvalM CAS.SymbolExpr
descendSymbol recur (CAS.Symbol sid name idxs) = do
  idxs' <- mapM (traverse recur) idxs
  return $ CAS.Symbol sid name idxs'
-- Apply1-4: recurse only on the *arguments*; the function slot holds a
-- symbolic reference that is not itself a rewrite target.
descendSymbol recur (CAS.Apply1 fn a) = CAS.Apply1 fn <$> recur a
descendSymbol recur (CAS.Apply2 fn a b) = CAS.Apply2 fn <$> recur a <*> recur b
descendSymbol recur (CAS.Apply3 fn a b c) = CAS.Apply3 fn <$> recur a <*> recur b <*> recur c
descendSymbol recur (CAS.Apply4 fn a b c d) = CAS.Apply4 fn <$> recur a <*> recur b <*> recur c <*> recur d
descendSymbol recur (CAS.Quote v) = CAS.Quote <$> recur v
-- FunctionData: name is the symbolic function reference, args are MathValue args.
descendSymbol recur (CAS.FunctionData name args) = CAS.FunctionData name <$> mapM recur args
descendSymbol _ s@(CAS.QuoteFunction _) = return s

-- | Phase 7.4/7.5: report the number of `declare rule` declarations seen by
-- the env-builder. The rule data itself is held in EnvBuildResult; here we
-- only expose the count so users can confirm registration worked.
numReductionRulesPrim :: String -> PrimitiveFunc
numReductionRulesPrim _ args = case args of
  [] -> do
    n <- getReductionRulesCount
    return $ toEgison (fromIntegral n :: Integer)
  [Tuple []] -> do
    n <- getReductionRulesCount
    return $ toEgison (fromIntegral n :: Integer)
  (a:_)  -> throwErrorWithTrace (TypeMismatch "no arguments" (Value a))

-- | Phase 6.3: report the number of `declare derivative` declarations seen.
numDerivativeRulesPrim :: String -> PrimitiveFunc
numDerivativeRulesPrim _ args = case args of
  [] -> do
    n <- getDerivativeRulesCount
    return $ toEgison (fromIntegral n :: Integer)
  [Tuple []] -> do
    n <- getDerivativeRulesCount
    return $ toEgison (fromIntegral n :: Integer)
  (a:_)  -> throwErrorWithTrace (TypeMismatch "no arguments" (Value a))

-- | List the names of all named reduction rules (auto rules excluded).
ruleNamesPrim :: String -> PrimitiveFunc
ruleNamesPrim _ args = case args of
  [] -> do
    ns <- getReductionRuleNames
    return $ Collection $ Sq.fromList $ map (String . T.pack) ns
  [Tuple []] -> do
    ns <- getReductionRuleNames
    return $ Collection $ Sq.fromList $ map (String . T.pack) ns
  (a:_)  -> throwErrorWithTrace (TypeMismatch "no arguments" (Value a))

-- | List the function names that have a registered derivative.
derivativeNamesPrim :: String -> PrimitiveFunc
derivativeNamesPrim _ args = case args of
  [] -> do
    ns <- getDerivativeRuleNames
    return $ Collection $ Sq.fromList $ map (String . T.pack) ns
  [Tuple []] -> do
    ns <- getDerivativeRuleNames
    return $ Collection $ Sq.fromList $ map (String . T.pack) ns
  (a:_)  -> throwErrorWithTrace (TypeMismatch "no arguments" (Value a))

-- | Check whether a named reduction rule is registered.
hasReductionRulePrim :: String -> PrimitiveFunc
hasReductionRulePrim = oneArg' $ \v -> case v of
  String s -> do
    ns <- getReductionRuleNames
    return $ Bool (T.unpack s `elem` ns)
  _ -> throwErrorWithTrace (TypeMismatch "string rule name" (Value v))

-- | Check whether a function name has a registered derivative.
hasDerivativeRulePrim :: String -> PrimitiveFunc
hasDerivativeRulePrim = oneArg' $ \v -> case v of
  String s -> do
    ns <- getDerivativeRuleNames
    return $ Bool (T.unpack s `elem` ns)
  _ -> throwErrorWithTrace (TypeMismatch "string function name" (Value v))

-- | True if a CASValue references any of the named symbols or functions
-- anywhere in its tree. Used by `declare rule auto` desugaring as a fast
-- pre-filter so rules whose trigger symbols are absent from the value can
-- skip the per-arithmetic match attempt.
containsAnySymbolPrim :: String -> PrimitiveFunc
containsAnySymbolPrim = twoArgs' $ \namesV valV ->
  case (namesV, valV) of
    (Collection nameSeq, CASData cv) -> do
      names <- mapM extractName (toList nameSeq)
      return $ Bool (casContainsAnyName names cv)
    _ -> throwErrorWithTrace (TypeMismatch "string list and CAS value" (Value valV))
 where
  extractName (String s) = return (T.unpack s)
  extractName v          = throwErrorWithTrace (TypeMismatch "string symbol name" (Value v))

casContainsAnyName :: [String] -> CAS.CASValue -> Bool
casContainsAnyName names = goV
 where
  goV (CAS.CASInteger _)  = False
  goV (CAS.CASFactor sym) = goSym sym
  goV (CAS.CASPoly terms) = any goTerm terms
  goV (CAS.CASFrac n d)   = goV n || goV d
  goTerm (CAS.CASTerm coeff mono) = goV coeff || any goFactor mono
  goFactor (sym, _)               = goSym sym
  goSym (CAS.Symbol _ name _)      = name `elem` names
  goSym (CAS.Apply1 f a1)          = goV f || goV a1
  goSym (CAS.Apply2 f a1 a2)       = goV f || goV a1 || goV a2
  goSym (CAS.Apply3 f a1 a2 a3)    = goV f || goV a1 || goV a2 || goV a3
  goSym (CAS.Apply4 f a1 a2 a3 a4) = goV f || goV a1 || goV a2 || goV a3 || goV a4
  goSym (CAS.Quote v)              = goV v
  goSym (CAS.QuoteFunction whnf)   = case prettyFunctionName whnf of
                                       Just n  -> n `elem` names
                                       Nothing -> False
  goSym (CAS.FunctionData fn args) = goV fn || any goV args

-- | Phase 5.5: runtime check that all atoms in a CAS value belong to the
-- given allowed-atom-name list. Used by user-level coerce-style helpers.
-- Atom names are pretty-printed (e.g. "x", "sqrt 2"), matched as strings.
isInPolyAtomsPrim :: String -> PrimitiveFunc
isInPolyAtomsPrim = twoArgs $ \v allowedC ->
  case (v, allowedC) of
    (CASData cv, Collection allowedSeq) -> do
      allowedNames <- mapM extractName (toList allowedSeq)
      let valueAtoms = CAS.casAtomSet cv
      return $ Bool (all (`elem` allowedNames) valueAtoms)
    _ -> throwErrorWithTrace (TypeMismatch "CAS value and string list" (Value v))
 where
  extractName (String s) = return (T.unpack s)
  extractName _          = throwErrorWithTrace (TypeMismatch "string atom name" (Value v))
  v = CASData (CAS.CASInteger 0)  -- unused placeholder for the error path

-- | Phase 5.5: check if a CASValue is a pure integer (no atoms / fractions).
isPureIntegerPrim :: String -> PrimitiveFunc
isPureIntegerPrim = oneArg' $ \v -> case v of
  CASData (CAS.CASInteger _)  -> return $ Bool True
  CASData (CAS.CASPoly [])    -> return $ Bool True   -- canonical zero
  CASData (CAS.CASPoly [CAS.CASTerm (CAS.CASInteger _) []]) -> return $ Bool True
  CASData _                   -> return $ Bool False
  _                           -> return $ Bool False

-- | Phase 5.5: check if a CASValue is a pure rational (Frac of integers).
isPureFractionPrim :: String -> PrimitiveFunc
isPureFractionPrim = oneArg' $ \v -> case v of
  CASData (CAS.CASInteger _) -> return $ Bool True
  CASData (CAS.CASFrac (CAS.CASInteger _) (CAS.CASInteger _)) -> return $ Bool True
  CASData (CAS.CASPoly []) -> return $ Bool True
  CASData (CAS.CASPoly [CAS.CASTerm (CAS.CASInteger _) []]) -> return $ Bool True
  _ -> return $ Bool False

-- | Phase 8: differential closure check.
-- Returns True iff the output value's atom set is a subset of the input's,
-- i.e. ∂/∂ did not introduce any new atoms. Used to label results that
-- stayed in the same Poly Integer [atoms] sub-ring.
differentialClosedPrim :: String -> PrimitiveFunc
differentialClosedPrim = twoArgs $ \input output ->
  case (input, output) of
    (CASData iv, CASData ov) ->
      return $ Bool (CAS.casDifferentialClosed iv ov)
    _ -> throwErrorWithTrace (TypeMismatch "two CAS values" (Value input))

-- | Phase 8: print the value alongside its observed type for REPL inspection.
-- Returns a string of the form "value : observed-type".
inspectPrim :: String -> PrimitiveFunc
inspectPrim = oneArg' $ \v -> case v of
  CASData cv ->
    return $ String (T.pack (CAS.prettyCAS cv ++ " : " ++ CAS.prettyTypeOf cv))
  Bool b ->
    return $ String (T.pack (show b ++ " : Bool"))
  Char c ->
    return $ String (T.pack (show c ++ " : Char"))
  String s ->
    return $ String (T.pack (show s ++ " : String"))
  Float f ->
    return $ String (T.pack (show f ++ " : Float"))
  Tuple [] ->
    return $ String (T.pack "() : ()")
  Tuple xs -> do
    descs <- mapM describeOne xs
    let valStr = "(" ++ intercalateComma (map fst descs) ++ ")"
        tyStr  = "(" ++ intercalateComma (map snd descs) ++ ")"
    return $ String (T.pack (valStr ++ " : " ++ tyStr))
  _ -> return $ String (T.pack "<value> : Any")
 where
  describeOne (CASData cv) = return (CAS.prettyCAS cv, CAS.prettyTypeOf cv)
  describeOne (Bool b)     = return (show b, "Bool")
  describeOne (Char c)     = return (show c, "Char")
  describeOne (String s)   = return (show s, "String")
  describeOne (Float f)    = return (show f, "Float")
  describeOne _            = return ("<value>", "Any")
  intercalateComma []     = ""
  intercalateComma [s]    = s
  intercalateComma (s:ss) = s ++ ", " ++ intercalateComma ss

-- | Phase 8 observed type: report the most specific runtime type of a value
-- as a string.
typeOfPrim :: String -> PrimitiveFunc
typeOfPrim = oneArg' $ \v -> case v of
  CASData cv -> return $ String (T.pack (CAS.prettyTypeOf cv))
  Tuple [] -> return $ String (T.pack "()")
  Tuple xs -> return $ String (T.pack ("(" ++ intercalateComma (map describeValue xs) ++ ")"))
  Collection _ -> return $ String (T.pack "Collection")
  TensorData _ -> return $ String (T.pack "Tensor")
  Bool _ -> return $ String (T.pack "Bool")
  Char _ -> return $ String (T.pack "Char")
  String _ -> return $ String (T.pack "String")
  Float _ -> return $ String (T.pack "Float")
  _ -> return $ String (T.pack "Any")
 where
  describeValue (CASData cv) = CAS.prettyTypeOf cv
  describeValue (Bool _)     = "Bool"
  describeValue (Char _)     = "Char"
  describeValue (String _)   = "String"
  describeValue (Float _)    = "Float"
  describeValue _            = "Any"
  intercalateComma []     = ""
  intercalateComma [s]    = s
  intercalateComma (s:ss) = s ++ ", " ++ intercalateComma ss

-- (`runtimeType` is intentionally NOT exposed as a user primitive: the
-- shallow runtime type is computed inside Eval.hs for type-class dispatch
-- only, so MathValue subtype types do not become first-class Egison values.)

-- | Extract the monomial of a single-term CASValue as a flat list of (factor, exponent) pairs.
-- For CASPoly [CASTerm _ mono], returns mono as a Collection of Tuple [factor, integer].
termMonomialPrim :: String -> PrimitiveFunc
termMonomialPrim = oneArg' $ \v -> case v of
  CASData (CAS.CASPoly [CASTerm _ mono]) -> return $ monoToCollection mono
  CASData (CAS.CASPoly []) -> return $ Collection Sq.empty
  CASData (CAS.CASInteger _) -> return $ Collection Sq.empty
  CASData (CAS.CASFactor sym) -> return $ monoToCollection [(sym, 1)]
  CASData (CAS.CASFrac _ _) -> return $ Collection Sq.empty
  _ -> throwErrorWithTrace (TypeMismatch "single-term CAS value" (Value v))
 where
  monoToCollection :: CAS.Monomial -> EgisonValue
  monoToCollection mono =
    Collection . Sq.fromList $
      map (\(sym, e) ->
            Tuple [ CASData (CAS.CASFactor sym)
                  , CASData (CAS.CASInteger e)
                  ]) mono

assert ::  String -> PrimitiveFunc
assert = twoArgs' $ \label test -> do
  test <- fromEgison test
  if test
    then return $ Bool True
    else throwErrorWithTrace (Assertion (show label))

assertEqual :: String -> PrimitiveFunc
assertEqual = threeArgs' $ \label actual expected ->
  if actual == expected
     then return actual
     else throwErrorWithTrace (Assertion
            (show label ++ "\n expected: " ++ show expected ++ "\n but found: " ++ show actual))

-- | Sort a list of lists of integers and return the sign of the permutation
-- Each sublist is treated as a unit and sorted lexicographically
-- Used for antisymmetric tensor indices
sortWithSign :: String -> PrimitiveFunc
sortWithSign = oneArg' $ \val -> do
  case val of
    Collection xss -> do
      -- Extract list of lists
      let xss' = toList xss
      xs <- mapM extractIntList xss'
      -- Sort lists lexicographically and calculate permutation sign
      let (sign, sortedLists) = sortWithPermSign xs
      let flatList = concat sortedLists
      return $ Tuple [toEgison sign, Collection (Sq.fromList (map toEgison flatList))]
    _ -> throwErrorWithTrace (TypeMismatch "collection of collections" (Value val))
 where
  -- Extract integers from a collection
  extractIntList :: EgisonValue -> EvalM [Integer]
  extractIntList (Collection xs) = mapM extractInt (toList xs)
  extractIntList x = (:[]) <$> extractInt x
  
  extractInt :: EgisonValue -> EvalM Integer
  extractInt val = case val of
    CASData _ -> fromEgison val
    _         -> throwErrorWithTrace (TypeMismatch "integer" (Value val))
  
  -- Sort lists lexicographically and calculate permutation sign using bubble sort
  sortWithPermSign :: [[Integer]] -> (Integer, [[Integer]])
  sortWithPermSign [] = (1, [])
  sortWithPermSign [x] = (1, [x])
  sortWithPermSign [x, y] =
    if x > y then (-1, [y, x]) else (1, [x, y])
  sortWithPermSign xs =
    let sorted = bubbleSort xs
        swaps = countInversions xs sorted
        sign = if even swaps then 1 else -1
    in (sign, sorted)
  
  -- Bubble sort for lists (lexicographic comparison)
  bubbleSort :: [[Integer]] -> [[Integer]]
  bubbleSort [] = []
  bubbleSort xs =
    let (xs', changed) = bubblePass xs
    in if changed then bubbleSort xs' else xs'
  
  bubblePass :: [[Integer]] -> ([[Integer]], Bool)
  bubblePass [] = ([], False)
  bubblePass [x] = ([x], False)
  bubblePass (x:y:rest) =
    if x > y
      then let (rest', _) = bubblePass (x:rest)
           in (y:rest', True)
      else let (rest', changed) = bubblePass (y:rest)
           in (x:rest', changed)
  
  -- Count inversions between original and sorted list
  countInversions :: (Eq a) => [a] -> [a] -> Int
  countInversions orig sorted =
    let indices = map (\x -> findIndex x sorted) orig
        findIndex x xs = case lookup x (zip xs [0..]) of
          Just i -> i
          Nothing -> 0
    in countInv indices
  
  countInv :: [Int] -> Int
  countInv [] = 0
  countInv (x:xs) = length (filter (< x) xs) + countInv xs

 {-- -- for 'egison-sqlite'
sqlite :: PrimitiveFunc
sqlite  = twoArgs' $ \val val' -> do
  dbName <- fromEgison val
  qStr <- fromEgison val'
  ret <- liftIO $ query' (T.pack dbName) $ T.pack qStr
  return $ makeIO $ return $ Collection $ Sq.fromList $ map (\r -> Tuple (map toEgison r)) ret
 where
  query' :: T.Text -> T.Text -> IO [[String]]
  query' dbName q = do
    db <- SQLite.open dbName
    rowsRef <- newIORef []
    SQLite.execWithCallback db q (\_ _ mcs -> do
                                    row <- forM mcs (\mcol -> case mcol of
                                                              Just col ->  return $ T.unpack col
                                                              Nothing -> return "null")
                                    rows <- readIORef rowsRef
                                    writeIORef rowsRef (row:rows))
    SQLite.close db
    ret <- readIORef rowsRef
    return $ reverse ret
 --} -- for 'egison-sqlite'
