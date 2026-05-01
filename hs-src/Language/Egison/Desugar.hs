{-# LANGUAGE TupleSections #-}

{- |
Module      : Language.Egison.Desugar
Licence     : MIT

This module implements Phase 3-4: Syntactic Desugaring (for untyped path).
For the typed path, desugaring is done inside type inference.

Syntactic Desugaring (Phase 3-4):
  - Operator desugaring (infix to function application)
  - Anonymous function expansion (cambda: 1#($1 + $2) etc.)
  - Match-lambda expansion (convert to match expressions)
  - Other syntactic sugar expansions
  
Design Note (design/implementation.md):
Pattern matching itself is NOT desugared here. Match expressions (IMatchExpr, 
IMatchAllExpr) are kept as-is and processed during evaluation (Phase 9).
This allows Egison's sophisticated pattern matching to be implemented in the evaluator.
-}

module Language.Egison.Desugar
    ( desugarTopExpr
    , desugarTopExprs
    , desugarExpr
    , transVarIndex
    ) where

import           Control.Monad.Except   (throwError)
import           Data.Char              (toUpper)
import           Data.Foldable          (foldrM)
import           Data.List              (union)
import           Data.Text              (pack)

import           Language.Egison.AST
import           Language.Egison.Data
import           Language.Egison.IExpr
import           Language.Egison.RState
import           Language.Egison.EvalState  (MonadEval(..))
import           Language.Egison.Type.Env   (lookupClass, ClassInfo(..))
import           Language.Egison.Type.Types (sanitizeMethodName, typeConstructorName,
                                             typeExprToType, capitalizeFirst, lowerFirst, TyVar(..))


desugarTopExpr :: TopExpr -> EvalM (Maybe ITopExpr)
desugarTopExpr (Define vwi expr) = do
  (var, iexpr) <- desugarDefineWithIndices vwi expr
  return . Just $ IDefine var iexpr
desugarTopExpr (DefineWithType typedVwi expr) = do
  -- Convert typed definition to regular definition
  -- Type information is used for type checking, but the runtime representation is the same
  -- Note: Constraints are preserved in the type scheme (by EnvBuilder),
  -- and dictionary passing is handled in TypeClassExpand phase
  let name = typedVarName typedVwi
      indices = typedVarIndices typedVwi
      params = typedVarParams typedVwi
      vwi = VarWithIndices name indices
  -- If there are typed parameters, wrap the body in a lambda
  case params of
    [] -> do
      (var, iexpr) <- desugarDefineWithIndices vwi expr
      return . Just $ IDefine var iexpr
    _  -> do
      -- Create lambda arguments from typed parameters
      let argPatterns = map typedParamToArgPattern params
          lambdaExpr = LambdaExpr argPatterns expr
      (var, iexpr) <- desugarDefineWithIndices vwi lambdaExpr
      return . Just $ IDefine var iexpr
desugarTopExpr (Test expr)     = Just . ITest <$> desugar expr
desugarTopExpr (Execute expr)  = Just . IExecute <$> desugar expr
desugarTopExpr (Load file)     = return . Just $ ILoad file
desugarTopExpr (LoadFile file) = return . Just $ ILoadFile file

-- Type class declarations: generate dictionary-passing wrapper functions
-- and register the class methods for dispatch
-- For a class like:
--   class Eq a where
--     (==) (x: a) (y: a) : Bool
-- We generate:
--   1. Dictionary wrapper: def classEqEq dict x y := (dict_"eq") x y
--   2. Instance registry variable: def registryEq := {| |}
--   3. Auto-dispatch function: def autoEqEq x y := (resolveEq x)_"eq" x y
desugarTopExpr (ClassDeclExpr (ClassDecl classNm _typeParams _supers methods)) = do
  -- Generate dictionary-passing wrapper functions for each method
  methodWrappers <- mapM (desugarClassMethod classNm) methods
  -- Generate empty instance registry
  let registryDef = makeRegistryDef classNm
  case methodWrappers of
    [] -> return Nothing
    _  -> return $ Just $ IDefineMany (registryDef : methodWrappers)
  where
    desugarClassMethod :: String -> ClassMethod -> EvalM (Var, IExpr)
    desugarClassMethod clsNm (ClassMethod methName methParams _retType _defaultImpl) = do
      -- Generate function name: e.g., "classEqEq" for (==) in Eq
      let wrapperName = "class" ++ clsNm ++ capitalizeFirst (sanitizeMethodName methName)
          var = stringToVar wrapperName
          dictVar = "dict"
          -- Parameter names: dict, x, y, ...
          paramNames = map extractParamName methParams
          allParams = dictVar : paramNames
      -- Build the body: (dict_"methodName") x y ...
      -- dict_"eq" is hash access, then apply to remaining params
      let dictAccessExpr = IIndexedExpr False (IVarExpr dictVar) 
                             [Sub (IConstantExpr (StringExpr (pack (sanitizeMethodName methName))))]
          bodyExpr = if null paramNames
                     then dictAccessExpr
                     else IApplyExpr dictAccessExpr (map IVarExpr paramNames)
          lambdaExpr = ILambdaExpr Nothing (map stringToVar allParams) bodyExpr
      return (var, lambdaExpr)
    
    -- Create empty instance registry: registryEq := {| |}
    makeRegistryDef :: String -> (Var, IExpr)
    makeRegistryDef clsNm = 
      let registryName = "registry" ++ clsNm
          var = stringToVar registryName
      in (var, IHashExpr [])
    
    extractParamName :: TypedParam -> String
    extractParamName (TPVar name _) = name
    extractParamName (TPInvertedVar name _) = name
    extractParamName (TPUntypedVar name) = name
    extractParamName _ = "x"  -- fallback

-- Instance declarations: generate a dictionary and individual method definitions
-- For an instance like:
--   instance Eq Integer where
--     (==) x y := x = y
--     (/=) x y := not (x = y)
-- We generate:
--   1. Individual method functions:
--      def eqIntegerEq x y := x = y
--      def eqIntegerNeq x y := not (x = y)
--   2. A dictionary for the instance:
--      def eqInteger := {| ("eq", eqIntegerEq), ("neq", eqIntegerNeq) |}
desugarTopExpr (InstanceDeclExpr (InstanceDecl constraints classNm instTypes methods)) = do
  -- Check if instTypes is not empty
  if null instTypes
    then return Nothing
    else do
      -- Multi-param-friendly: concatenate type-constructor names of ALL instance
      -- types so two instances of the same class with different type-tuples get
      -- distinct dictionary names. e.g.:
      --   Coerce MathValue Integer       → "coerceMathValueInteger"
      --   Coerce MathValue (Frac Integer) → "coerceMathValueFrac"
      -- For single-param classes the result equals the existing single-name form.
      let instTypeNames = map (typeConstructorName . typeExprToType) instTypes
          instTypeName  = concat instTypeNames
      -- Generate individual method definitions with constraint parameters
      methodDefs <- mapM (desugarInstanceMethod constraints classNm instTypeName) methods
      -- Generate dictionary definition with superclass references
      dictDef <- makeDictDef classNm instTypeName methods
      -- Always generate the dictionary (even for marker classes with no methods)
      return $ Just $ IDefineMany (dictDef : methodDefs)
  where
    desugarInstanceMethod :: [ConstraintExpr] -> String -> String -> InstanceMethod -> EvalM (Var, IExpr)
    desugarInstanceMethod _constrs clsNm typNm (InstanceMethod methName params body) = do
      -- Generate function name using type constructor name only
      -- e.g., "eqCollectionEq" not "eqCollectionaEq" for instance {Eq a} Eq [a]
      let funcName = lowerFirst clsNm ++ typNm ++ capitalizeFirst (sanitizeMethodName methName)
          var = stringToVar funcName
      
      -- Do NOT add dictionary parameters here!
      -- Dictionary parameters will be added automatically by addDictionaryParametersT
      -- after type inference, based on the inferred constraints.
      -- This allows the method body to be properly type-checked with constraints.
      
      -- Create lambda expression with only the method parameters
      let lambdaArgs = map (\p -> Arg (APPatVar (VarWithIndices p []))) params
          lambdaExpr = if null params then body else LambdaExpr lambdaArgs body
      iexpr <- desugar lambdaExpr
      return (var, iexpr)
    
    makeDictDef :: String -> String -> [InstanceMethod] -> EvalM (Var, IExpr)
    makeDictDef clsNm typNm meths = do
      let dictName = lowerFirst clsNm ++ typNm  -- e.g., "eqCollection"
          dictVar = stringToVar dictName
          methodEntries = map (makeHashEntry clsNm typNm) meths
      -- Add superclass dictionary references (Haskell-style nested dicts)
      superEntries <- makeSuperclassEntries clsNm typNm
      let hashExpr = IHashExpr (methodEntries ++ superEntries)
      return (dictVar, hashExpr)
    
    makeHashEntry :: String -> String -> InstanceMethod -> (IExpr, IExpr)
    makeHashEntry clsNm typNm (InstanceMethod methName _ _) =
      let keyExpr = IConstantExpr (StringExpr (pack (sanitizeMethodName methName)))
          funcName = lowerFirst clsNm ++ typNm ++ capitalizeFirst (sanitizeMethodName methName)
          valueExpr = IVarExpr funcName
      in (keyExpr, valueExpr)

    makeSuperclassEntries :: String -> String -> EvalM [(IExpr, IExpr)]
    makeSuperclassEntries clsNm typNm = do
      classEnv <- getClassEnv
      case lookupClass clsNm classEnv of
        Just info -> return $ map (makeSuperEntry typNm) (classSupers info)
        Nothing   -> return []

    makeSuperEntry :: String -> String -> (IExpr, IExpr)
    makeSuperEntry typNm superName =
      let keyExpr = IConstantExpr (StringExpr (pack ("__super_" ++ superName)))
          superDictName = lowerFirst superName ++ typNm
          valueExpr = IVarExpr superDictName
      in (keyExpr, valueExpr)
    

-- Inductive declarations don't produce runtime code
-- Constructor registration is handled by the type system
desugarTopExpr (InductiveDecl _ _ _) = return Nothing

-- Infix declarations don't produce runtime code
desugarTopExpr (InfixDecl _ _) = return Nothing
desugarTopExpr (PatternInductiveDecl _ _ _) = return Nothing  -- Handled in environment building phase

-- Pattern function declarations need type checking, so convert to IPatternFunctionDecl
desugarTopExpr (PatternFunctionDecl name typeParams params retType body) = do
  let paramTypes = map (\(pname, pty) -> (pname, typeExprToType pty)) params
      retType' = typeExprToType retType
      tyVars = map TyVar typeParams
  body' <- desugarPattern body
  return . Just $ IPatternFunctionDecl name tyVars paramTypes retType' body'

-- Symbol declarations
desugarTopExpr (DeclareSymbol names mTypeExpr) = do
  -- Convert type expression to type (defaults to Integer if not specified)
  let ty = case mTypeExpr of
             Just texpr -> typeExprToType texpr
             Nothing    -> typeExprToType TEInt
  return . Just $ IDeclareSymbol names (Just ty)
desugarTopExpr (DeclareRule mname level lhsPat rhs) = do
  -- Phase 7.5 (literal LHS) + Phase A (pattern variables `$x`, `#x`).
  --
  --   declare rule auto term i^2 = -1
  --   ⇒  def autoRule.<fresh> := \v -> applyTermRule i^2 (-1) v
  --
  -- With pattern variables (Phase A):
  --   declare rule trigPyth poly (sin $x)^2 + (cos #x)^2 = 1
  --   ⇒  def rule.trigPyth := \v -> match v as mathExpr with
  --                                  | (apply1 #sin $x)^2 + (apply1 #cos #x)^2 -> 1
  --                                  | _ -> v
  --
  -- Strategy:
  --   - If the LHS pattern contains no PatVar, take the literal-LHS path:
  --     reconstruct an Expr from the pattern and use `applyTermRule` so
  --     monomial-containment matching keeps working for term-level rules.
  --   - Otherwise, emit a `match v as <matcher> with | <pat> -> <rhs> | _ -> v`
  --     lambda. The user's surface syntax `f $x` (PApplyPat) is translated
  --     to `apply1 #f $x` so it matches mathExpr's matcher constructors.
  fr <- fresh
  let paramName = "__rule_input." ++ filter (\c -> c /= '$' && c /= '_') fr
  rhsI <- unNormalizeOps <$> desugar rhs
  body <- if patternHasPatVar lhsPat
            then buildPatternRuleBody paramName level lhsPat rhsI
            else case patternToLiteralExpr lhsPat of
                   Just lhsExpr -> do
                     lhsI <- unNormalizeOps <$> desugar lhsExpr
                     buildLiteralRuleBody paramName level lhsI rhsI
                   Nothing ->
                     throwError $ Default
                       "declare rule LHS must be either a literal expression \
                       \or a pattern containing pattern variables ($x)."
  case mname of
    Just n -> do
      -- Named rule: emit `def rule.<n> := <body>` only.
      let varName = "rule." ++ n
      return . Just $ IDefine (stringToVar varName) body
    Nothing -> do
      -- Auto rule: emit two definitions:
      --   1. `def autoRule.<idx> := <body>` (the rule lambda)
      --   2. `def mathNormalize := \v -> iterateRules [autoRule.0, ...]
      --                                              (mathNormalizeBuiltin v)`
      prevAutoNames <- getAutoRuleVarNames
      let idx        = length prevAutoNames
          autoVar    = "autoRule." ++ show idx
          allAutoVars = prevAutoNames ++ [autoVar]
      appendAutoRuleVarName autoVar
      mathNormBody <- buildMathNormalizeRedef allAutoVars
      return . Just $ IDefineMany
        [ (stringToVar autoVar, body)
        , (stringToVar "mathNormalize", mathNormBody)
        ]
  where
    -- Build the body of the redefined `mathNormalize`:
    --   \v -> iterateRules [autoRule.0, ..., autoRule.N] (mathNormalizeBuiltin v)
    buildMathNormalizeRedef :: [String] -> EvalM IExpr
    buildMathNormalizeRedef autoVars = do
      let rulesList = ICollectionExpr $ map IVarExpr autoVars
          mathBuiltinCall = IApplyExpr (IVarExpr "mathNormalizeBuiltin") [IVarExpr "v"]
          iterCall = IApplyExpr (IVarExpr "iterateRules") [rulesList, mathBuiltinCall]
      return $ ILambdaExpr Nothing [stringToVar "v"] iterCall

    -- Literal LHS path: \v -> applyTermRule lhs rhs v  (term-level)
    --                or \v -> if v = lhs then rhs else v  (poly/frac)
    buildLiteralRuleBody :: String -> RuleLevel -> IExpr -> IExpr -> EvalM IExpr
    buildLiteralRuleBody paramName lvl lhsI rhsI = case lvl of
      TermRuleLevel ->
        return $ ILambdaExpr Nothing [stringToVar paramName]
                   (IApplyExpr (IVarExpr "applyTermRule")
                               [lhsI, rhsI, IVarExpr paramName])
      _ ->
        return $ ILambdaExpr Nothing [stringToVar paramName]
                   (IIfExpr
                      (IApplyExpr (IVarExpr "=")
                                 [IVarExpr paramName, lhsI])
                      rhsI
                      (IVarExpr paramName))

    -- Pattern-variable LHS path: emit a sub-expression-aware rule.
    -- The rule's "one step" lambda matches the user's pattern at a single
    -- value node (returning RHS on match, input otherwise). This single-step
    -- rule is then wrapped with the structural traversal primitive
    -- corresponding to the rule level:
    --   TermRuleLevel → mapTerm  (apply to each term/monomial)
    --   PolyRuleLevel → mapPoly  (apply at each (sub-)polynomial)
    --   FracRuleLevel → mapFrac  (apply at each (sub-)fraction)
    -- These primitives recurse into Apply1-4 / Quote / Function arguments
    -- automatically, so the rule fires at any sub-expression and iterates to
    -- a fixpoint at each visited node.
    buildPatternRuleBody :: String -> RuleLevel -> Pattern -> IExpr -> EvalM IExpr
    buildPatternRuleBody paramName lvl pat rhsI = do
      -- Use a separate fresh inner-arg name so the inner match is independent
      -- from the outer lambda parameter.
      frInner <- fresh
      let innerArg = "__rule_step." ++ filter (\c -> c /= '$' && c /= '_') frInner
          translated = translateToMatcherPattern pat
          mapPrim = case lvl of
            TermRuleLevel -> "mapTermAll"
            PolyRuleLevel -> "mapPolyAll"
            FracRuleLevel -> "mapFracAll"
          -- Inner one-step lambda built as Egison surface for desugar to handle
          -- the match-clause shape; we patch the RHS afterwards with rhsI.
          matchExpr = MatchExpr BFSMode
                        (VarExpr innerArg)
                        (VarExpr "mathExpr")
                        [(translated, ConstantExpr UndefinedExpr),
                         (WildCard,   VarExpr innerArg)]
      matchI <- desugar matchExpr
      let patchedMatchI = patchFirstMatchRhs matchI rhsI
          oneStepLambda = ILambdaExpr Nothing [stringToVar innerArg] patchedMatchI
          mapCall       = IApplyExpr (IVarExpr mapPrim)
                                     [oneStepLambda, IVarExpr paramName]
      return $ ILambdaExpr Nothing [stringToVar paramName] mapCall

    -- Replace the first match clause's body in an IMatchExpr with the given
    -- IExpr. (We placeholder-desugar the match with `undefined`, then patch.)
    patchFirstMatchRhs :: IExpr -> IExpr -> IExpr
    patchFirstMatchRhs (IMatchExpr m tgt mtcher ((p, _) : rest)) newBody =
      IMatchExpr m tgt mtcher ((p, newBody) : rest)
    patchFirstMatchRhs e _ = e
desugarTopExpr (DeclareDerivative name rhs) = do
  -- Phase 6.3 part 4-6: emit
  --   def deriv.<name> := <rhs>
  --   def chainPartialDiff := \v dx ->
  --       match v as mathValue with
  --         | apply1 #<n_1> $a -> deriv.<n_1> a *' chainPartialDiff a dx
  --         ...
  --         | apply1 #<n_k> $a -> deriv.<n_k> a *' chainPartialDiff a dx
  --         | _ -> chainPartialDiffBuiltin v dx
  --   where n_1..n_k are *all* the derivative names seen so far (including
  --   <name>). Each declare derivative redefines `chainPartialDiff` with the
  --   broader pattern set; Egison's name shadowing lets the latest
  --   definition win. The fallback uses `chainPartialDiffBuiltin` (defined in
  --   lib/math/analysis/derivative.egi and never redefined) so the
  --   recursion through nested mathfuncs terminates.
  rhsI <- desugar rhs
  -- Use only derivatives desugared *up to and including* this one, so the
  -- emitted chainPartialDiff body doesn't forward-reference `deriv.<later>`
  -- bindings. EnvBuilder pre-populates `derivativeRuleNames` with all names,
  -- but for code generation we want each declaration to reference only the
  -- names that have already been emitted.
  prevDesugared <- getDerivativesDesugared
  let allNames = prevDesugared ++ [name | name `notElem` prevDesugared]
  appendDerivativeDesugared name
  -- Build the chainPartialDiff body: a lambda over (v, dx) with a match.
  let derivBinding = (stringToVar ("deriv." ++ name), rhsI)
  chainBindingI <- buildChainPartialDiff allNames
  let chainBinding = (stringToVar "chainPartialDiff", chainBindingI)
  return . Just $ IDefineMany [derivBinding, chainBinding]
  where
    -- Build:
    --   \v dx -> match v as mathValue with
    --              | apply1 #<n1> $a -> deriv.<n1> a *' chainPartialDiff a dx
    --              ...
    --              | _ -> chainPartialDiffBuiltin v dx
    --
    -- We desugar a synthetic Egison expression rather than hand-building
    -- the IExpr tree, since match patterns and `apply1 #` are easier at
    -- the surface level.
    buildChainPartialDiff :: [String] -> EvalM IExpr
    buildChainPartialDiff names = do
      -- Recursive arm: deriv.<n> a *' chainPartialDiff a dx.
      -- Using chainPartialDiff (not partialDiff) recursively lets nested
      -- mathfunc applications like f(g(x)) dispatch correctly: the inner
      -- call hits the apply1 #g pattern.
      let mkClause n =
            ( InductivePat "apply1"
                 [ ValuePat (VarExpr n)
                 , PatVar "a"
                 ]
            , InfixExpr (Op "*'" 7 InfixL False)
                 (ApplyExpr (VarExpr ("deriv." ++ n)) [VarExpr "a"])
                 (ApplyExpr (VarExpr "chainPartialDiff")
                            [VarExpr "a", VarExpr "dx"])
            )
          fallbackClause =
            ( WildCard
            , ApplyExpr (VarExpr "chainPartialDiffBuiltin") [VarExpr "v", VarExpr "dx"]
            )
          matchExpr = MatchExpr BFSMode
                        (VarExpr "v")
                        (VarExpr "mathValue")
                        (map mkClause names ++ [fallbackClause])
          lambda = LambdaExpr
                     [ Arg (APPatVar (VarWithIndices "v" []))
                     , Arg (APPatVar (VarWithIndices "dx" []))
                     ]
                     matchExpr
      desugar lambda
desugarTopExpr (DeclareMathFunc name _mType) = do
  -- Phase 6.3 part 5: emit a wrapper function that quotes the symbol on
  -- application:  def <name> (x : MathValue) : MathValue := '<name> x
  -- The parser builds `'name x` as `ApplyExpr (QuoteSymbolExpr (VarExpr name)) [VarExpr x]`,
  -- so we mirror that structure here.
  let body = LambdaExpr [Arg (APPatVar (VarWithIndices "x" []))]
                        (ApplyExpr (QuoteSymbolExpr (VarExpr name))
                                   [VarExpr "x"])
  bodyI <- desugar body
  return . Just $ IDefine (stringToVar name) bodyI

-- | Detect whether a Pattern has any PatVar at any depth.
patternHasPatVar :: Pattern -> Bool
patternHasPatVar (PatVar _)        = True
patternHasPatVar (ValuePat _)      = False
patternHasPatVar WildCard          = False
patternHasPatVar (PredPat _)       = False
patternHasPatVar ContPat           = False
patternHasPatVar LaterPatVar       = False
patternHasPatVar (NotPat p)        = patternHasPatVar p
patternHasPatVar (AndPat a b)      = patternHasPatVar a || patternHasPatVar b
patternHasPatVar (OrPat a b)       = patternHasPatVar a || patternHasPatVar b
patternHasPatVar (ForallPat a b)   = patternHasPatVar a || patternHasPatVar b
patternHasPatVar (TuplePat ps)     = any patternHasPatVar ps
patternHasPatVar (InductivePat _ ps)            = any patternHasPatVar ps
patternHasPatVar (InductiveOrPApplyPat _ ps)    = any patternHasPatVar ps
patternHasPatVar (InfixPat _ a b)  = patternHasPatVar a || patternHasPatVar b
patternHasPatVar (IndexedPat p _)  = patternHasPatVar p
patternHasPatVar (LetPat _ p)      = patternHasPatVar p
patternHasPatVar (LoopPat _ _ a b) = patternHasPatVar a || patternHasPatVar b
patternHasPatVar (PApplyPat _ ps)  = any patternHasPatVar ps
patternHasPatVar (DApplyPat p ps)  = patternHasPatVar p || any patternHasPatVar ps
patternHasPatVar (VarPat _)        = False
patternHasPatVar (SeqConsPat a b)  = patternHasPatVar a || patternHasPatVar b
patternHasPatVar SeqNilPat         = False

-- | Convert a literal pattern (no PatVar) back to an Expr so we can desugar
-- it via the existing applyTermRule path. Returns Nothing if the pattern
-- contains constructs that don't have an Expr equivalent.
patternToLiteralExpr :: Pattern -> Maybe Expr
patternToLiteralExpr (ValuePat e)         = Just e
patternToLiteralExpr (InfixPat op a b)    = do
  ae <- patternToLiteralExpr a
  be <- patternToLiteralExpr b
  return $ InfixExpr op ae be
patternToLiteralExpr (PApplyPat f args)   = do
  argExprs <- mapM patternToLiteralExpr args
  return $ ApplyExpr f argExprs
patternToLiteralExpr (TuplePat ps)        = do
  es <- mapM patternToLiteralExpr ps
  return $ TupleExpr es
patternToLiteralExpr (InductivePat _ _)   = Nothing
patternToLiteralExpr WildCard             = Nothing
patternToLiteralExpr (PatVar _)           = Nothing
patternToLiteralExpr _                    = Nothing

-- | Translate a user-written rule LHS pattern into a pattern that uses
-- mathExpr/multExpr matcher constructors. Specifically, surface syntax
-- `f $x` (PApplyPat with a VarExpr/QuoteSymbolExpr function) is rewritten
-- to `apply1 #f $x` (InductivePat using mathExpr's apply1 clause).
translateToMatcherPattern :: Pattern -> Pattern
translateToMatcherPattern p = case p of
  PApplyPat funcExpr args ->
    let funcName = case funcExpr of
                     VarExpr n                   -> Just n
                     QuoteSymbolExpr (VarExpr n) -> Just n
                     _                            -> Nothing
        translatedArgs = map translateToMatcherPattern args
    in case (funcName, length translatedArgs) of
         (Just _, 1) ->
           InductivePat "apply1" (ValuePat funcExpr : translatedArgs)
         (Just _, 2) ->
           InductivePat "apply2" (ValuePat funcExpr : translatedArgs)
         (Just _, 3) ->
           InductivePat "apply3" (ValuePat funcExpr : translatedArgs)
         (Just _, 4) ->
           InductivePat "apply4" (ValuePat funcExpr : translatedArgs)
         _ -> PApplyPat funcExpr translatedArgs
  InfixPat op a b ->
    InfixPat op (translateToMatcherPattern a) (translateToMatcherPattern b)
  InductivePat n args ->
    InductivePat n (map translateToMatcherPattern args)
  TuplePat ps -> TuplePat (map translateToMatcherPattern ps)
  AndPat a b  -> AndPat (translateToMatcherPattern a) (translateToMatcherPattern b)
  OrPat a b   -> OrPat (translateToMatcherPattern a) (translateToMatcherPattern b)
  NotPat q    -> NotPat (translateToMatcherPattern q)
  IndexedPat q es -> IndexedPat (translateToMatcherPattern q) es
  _ -> p

-- | Replace normalizing arithmetic operators (`+`, `-`, `*`, `/`, `^`) with
-- their **primitive** Haskell-level counterparts. Used by `declare rule auto`
-- to avoid infinite recursion:
--
-- The lib's un-normalized operators (`+'`, `-'`, etc.) are direct aliases of
-- `i.+`/`i.-`/etc. But the lib's `power'` (used by `^'`) iterates via
-- `take`/`foldl`, which use `-` (subtraction) on Integer. Subtraction at the
-- MathValue level dispatches to `minusForMathValue`, which calls
-- `mathNormalize`, creating a cycle:
--   `mathNormalize` → rule body → `power' p 2` → `take 2 ...` → `n - 1`
--   → `mathNormalize` → ...
--
-- Bypassing this requires using the primitives **directly** in the rule body.
-- For `^` (power), the primitive `i.power` only works on integer arguments,
-- so we instead expand `x ^ n` (where `n` is a positive integer literal) into
-- repeated `i.* x x ... x` calls. This works for symbolic CAS values.
unNormalizeOps :: IExpr -> IExpr
unNormalizeOps e = case e of
  -- Special case: `x ^ n` where n is a literal positive integer.
  -- Expand to nested i.* (works for symbolic CAS values).
  IApplyExpr (IVarExpr "^")
             [base, IConstantExpr (IntegerExpr n)]
    | n >= 1 ->
        let base' = unNormalizeOps base
            mulPrim = IVarExpr "i.*"
            go 1 = base'
            go k = IApplyExpr mulPrim [base', go (k - 1)]
        in go n
  IApplyExpr (IVarExpr nm) args
    | Just nm' <- lookup nm opTable ->
        IApplyExpr (IVarExpr nm') (map unNormalizeOps args)
  IApplyExpr f args ->
    IApplyExpr (unNormalizeOps f) (map unNormalizeOps args)
  ILambdaExpr mn vs body ->
    ILambdaExpr mn vs (unNormalizeOps body)
  IIfExpr c t f ->
    IIfExpr (unNormalizeOps c) (unNormalizeOps t) (unNormalizeOps f)
  ILetExpr bindings body ->
    ILetExpr [(p, unNormalizeOps b) | (p, b) <- bindings] (unNormalizeOps body)
  ITupleExpr es ->
    ITupleExpr (map unNormalizeOps es)
  ICollectionExpr es ->
    ICollectionExpr (map unNormalizeOps es)
  IConsExpr a b ->
    IConsExpr (unNormalizeOps a) (unNormalizeOps b)
  IJoinExpr a b ->
    IJoinExpr (unNormalizeOps a) (unNormalizeOps b)
  IInductiveDataExpr nm es ->
    IInductiveDataExpr nm (map unNormalizeOps es)
  IQuoteSymbolExpr e' ->
    IQuoteSymbolExpr (unNormalizeOps e')
  -- Other constructors: leave as-is (including IVarExpr, IConstantExpr,
  -- patterns, matcher refs, etc.). They don't contain operators.
  _ -> e
  where
    opTable :: [(String, String)]
    opTable =
      [ ("+", "i.+")
      , ("-", "i.-")
      , ("*", "i.*")
      , ("/", "i./")
      ]

-- | Convert TypedParam to Arg ArgPattern for lambda expressions
typedParamToArgPattern :: TypedParam -> Arg ArgPattern
typedParamToArgPattern (TPVar pname _) =
  Arg (APPatVar (VarWithIndices pname []))
typedParamToArgPattern (TPInvertedVar pname _) =
  InvertedArg (APPatVar (VarWithIndices pname []))
typedParamToArgPattern (TPTuple elems) =
  Arg (APTuplePat (map typedParamToArgPattern elems))
typedParamToArgPattern (TPWildcard _) =
  Arg APWildCard
typedParamToArgPattern (TPUntypedVar pname) =
  Arg (APPatVar (VarWithIndices pname []))
typedParamToArgPattern TPUntypedWildcard =
  Arg APWildCard

desugarTopExprs :: [TopExpr] -> EvalM [ITopExpr]
desugarTopExprs [] = return []
desugarTopExprs (expr : exprs) = do
  expr' <- desugarTopExpr expr
  case expr' of
    Nothing    -> desugarTopExprs exprs
    Just expr' -> (expr' :) <$> desugarTopExprs exprs

desugarExpr :: Expr -> EvalM IExpr
desugarExpr = desugar

desugar :: Expr -> EvalM IExpr
desugar (ConstantExpr c) = return $ IConstantExpr c
desugar (VarExpr var)    = return $ IVarExpr var

desugar (AlgebraicDataMatcherExpr patterns) = do
  matcherName <- fresh
  let matcherRef = IVarExpr matcherName
  matcher <- genMatcherClauses patterns matcherRef
  return $ ILetRecExpr [(PDPatVar (stringToVar matcherName), matcher)] matcherRef
    where
      genMatcherClauses :: [(String, [Expr])] ->  IExpr -> EvalM IExpr
      genMatcherClauses patterns matcher = do
        main <- genMainClause patterns matcher
        body <- mapM genMatcherClause patterns
        footer <- genSomethingClause
        let clauses = [main] ++ body ++ [footer]
        return $ IMatcherExpr clauses

      genMainClause :: [(String, [Expr])] -> IExpr -> EvalM (PrimitivePatPattern, IExpr, [(IPrimitiveDataPattern, IExpr)])
      genMainClause patterns matcher = do
        clauses <- genClauses patterns
        return (PPValuePat "val", ITupleExpr [],
                [(PDPatVar (stringToVar "tgt"),
                    IMatchExpr BFSMode
                               (ITupleExpr [IVarExpr "val", IVarExpr "tgt"])
                               (ITupleExpr [matcher, matcher])
                               clauses)])
        where
          genClauses :: [(String, [Expr])] -> EvalM [IMatchClause]
          genClauses patterns = (++) <$> mapM genClause patterns
                                     <*> pure [(ITuplePat [IWildCard, IWildCard], matchingFailure)]

          genClause :: (String, [Expr]) -> EvalM IMatchClause
          genClause pattern = do
            (pat0, pat1) <- genMatchingPattern pattern
            return (ITuplePat [pat0, pat1], matchingSuccess)

          genMatchingPattern :: (String, [Expr]) -> EvalM (IPattern, IPattern)
          genMatchingPattern (name, patterns) = do
            names <- mapM (const fresh) patterns
            return (IInductivePat name (map IPatVar names),
                    IInductivePat name (map (IValuePat . IVarExpr) names))

      genMatcherClause :: (String, [Expr]) -> EvalM (PrimitivePatPattern, IExpr, [(IPrimitiveDataPattern, IExpr)])
      genMatcherClause pattern = do
        (ppat, matchers) <- genPrimitivePatPat pattern
        (dpat, body)     <- genPrimitiveDataPat pattern
        return (ppat, ITupleExpr matchers, [(dpat, ICollectionExpr [ITupleExpr body]), (PDWildCard, matchingFailure)])

        where
          genPrimitivePatPat :: (String, [Expr]) -> EvalM (PrimitivePatPattern, [IExpr])
          genPrimitivePatPat (name, matchers) = do
            patterns' <- mapM (const $ return PPPatVar) matchers
            matchers' <- mapM desugar matchers
            return (PPInductivePat name patterns', matchers')

          genPrimitiveDataPat :: (String, [Expr]) -> EvalM (IPrimitiveDataPattern, [IExpr])
          genPrimitiveDataPat (name, patterns) = do
            patterns' <- mapM (const fresh) patterns
            return (PDInductivePat (capitalize name) $ map (PDPatVar . stringToVar) patterns', map IVarExpr patterns')

          capitalize :: String -> String
          capitalize (x:xs) = toUpper x : xs


      genSomethingClause :: EvalM (PrimitivePatPattern, IExpr, [(IPrimitiveDataPattern, IExpr)])
      genSomethingClause =
        return (PPPatVar, ITupleExpr [IConstantExpr SomethingExpr], [(PDPatVar (stringToVar "tgt"), ICollectionExpr [IVarExpr "tgt"])])

      matchingSuccess :: IExpr
      matchingSuccess = ICollectionExpr [ITupleExpr []]

      matchingFailure :: IExpr
      matchingFailure = ICollectionExpr []

desugar (MatchAllLambdaExpr matcher clauses) = do
  name <- fresh
  ILambdaExpr Nothing [stringToVar name] <$>
    desugar (MatchAllExpr BFSMode (VarExpr name) matcher clauses)

desugar (MatchLambdaExpr matcher clauses) = do
  name <- fresh
  ILambdaExpr Nothing [stringToVar name] <$>
    desugar (MatchExpr BFSMode (VarExpr name) matcher clauses)

desugar (IndexedExpr override expr indices) = do
  expr' <- desugar expr
  desugarIndexedExpr override expr' indices
  where
    desugarIndexedExpr :: Bool -> IExpr -> [IndexExpr Expr] -> EvalM IExpr
    desugarIndexedExpr override expr' indices =
      case indices of
        [] -> return expr'
        (MultiSubscript x y:indices') ->
          case (x, y) of
            (IndexedExpr override1 e1 [n1], IndexedExpr _ _ [n2]) -> do
              expr'' <- desugarMultiScript override expr' ISubrefsExpr override1 e1 n1 n2
              desugarIndexedExpr False expr'' indices'
            _ -> throwError $ Default "Index should be IndexedExpr for multi subscript"
        (MultiSuperscript x y:indices') ->
          case (x, y) of
            (IndexedExpr override1 e1 [n1], IndexedExpr _ _ [n2]) -> do
              expr'' <- desugarMultiScript override expr' ISuprefsExpr override1 e1 n1 n2
              desugarIndexedExpr False expr'' indices'
            _ -> throwError $ Default "Index should be IndexedExpr for multi superscript"
        _ -> do
          let (is, indices') = break isMulti indices
          expr'' <- IIndexedExpr override expr' <$> mapM desugarIndex is
          desugarIndexedExpr False expr'' indices'
    desugarMultiScript override expr' refExpr override1 e1 n1 n2 = do
      k     <- fresh
      n1'   <- desugar (extractIndexExpr n1)
      n2'   <- desugar (extractIndexExpr n2)
      e1'   <- desugar e1
      return $ refExpr override expr' (makeIApply "map"
                                           [ILambdaExpr Nothing [stringToVar k] (IIndexedExpr override1 e1' [Sub (IVarExpr k)]),
                                            makeIApply "between" [n1', n2']])
    isMulti (MultiSubscript _ _)   = True
    isMulti (MultiSuperscript _ _) = True
    isMulti _                      = False


desugar (SubrefsExpr bool expr1 expr2) =
  ISubrefsExpr bool <$> desugar expr1 <*> desugar expr2

desugar (SuprefsExpr bool expr1 expr2) =
  ISuprefsExpr bool <$> desugar expr1 <*> desugar expr2

desugar (UserrefsExpr bool expr1 expr2) =
  IUserrefsExpr bool <$> desugar expr1 <*> desugar expr2

desugar (TupleExpr exprs) = ITupleExpr <$> mapM desugar exprs
desugar (CollectionExpr xs) = ICollectionExpr <$> mapM desugar xs
desugar (ConsExpr x xs) = IConsExpr <$> desugar x <*> desugar xs
desugar (JoinExpr x xs) = IJoinExpr <$> desugar x <*> desugar xs

desugar (HashExpr exprPairs) =
  IHashExpr <$> mapM (\(expr1, expr2) -> (,) <$> desugar expr1 <*> desugar expr2) exprPairs

desugar (VectorExpr exprs) =
  IVectorExpr <$> mapM desugar exprs

desugar (TensorExpr nsExpr xsExpr) =
  ITensorExpr <$> desugar nsExpr <*> desugar xsExpr

-- Desugar of LambdaExpr takes place in 2 stages.
-- * LambdaExpr -> LambdaExpr'  : Desugar pattern matches at the arg positions
-- * LambdaExpr' -> ILambdaExpr : Desugar Arg and InvertedArg
desugar (LambdaExpr args expr) = do
  (args', expr') <- foldrM desugarArg ([], expr) args
  desugar $ LambdaExpr' args' expr'
  where
    desugarArg :: Arg ArgPattern -> ([Arg VarWithIndices], Expr) -> EvalM ([Arg VarWithIndices], Expr)
    desugarArg (Arg x) (args, expr) = do
      (var, expr') <- desugarArgPat x expr
      return (Arg var : args, expr')
    desugarArg (InvertedArg x) (args, expr) = do
      (var, expr') <- desugarArgPat x expr
      return (InvertedArg var : args, expr')

    -- Desugar argument patterns. Examples:
    -- \$(%x, %y) -> expr   ==> \$tmp -> let (tmp1, tmp2) := tmp in (\%x %y -> expr) tmp1 tmp2
    -- \(x, (y, z)) -> expr ==> \tmp  -> let (tmp1, tmp2) := tmp in (\x (y, z) -> expr) tmp1 tmp2
    -- \%($x :: xs) -> expr ==> \%tmp -> let (tmp1 :: xs) := tmp in (\$x %xs -> expr) tmp1 tmp2
    desugarArgPat :: ArgPattern -> Expr -> EvalM (VarWithIndices, Expr)
    desugarArgPat APWildCard expr = do
      tmp <- fresh
      let tmp' = stringToVarWithIndices tmp
      return (tmp', LetExpr [Bind PDWildCard (VarExpr tmp)] expr)
    desugarArgPat (APPatVar var) expr = return (var, expr)
    desugarArgPat (APTuplePat args) expr = do
      tmp  <- fresh
      let tmp' = stringToVarWithIndices tmp
      tmps <- mapM (const fresh) args
      return (tmp', LetExpr [Bind (PDTuplePat (map PDPatVar tmps)) (VarExpr tmp)]
                      (ApplyExpr (LambdaExpr args expr) (map VarExpr tmps)))
    desugarArgPat (APInductivePat ctor args) expr = do
      tmp  <- fresh
      let tmp' = stringToVarWithIndices tmp
      tmps <- mapM (const fresh) args
      return (tmp', LetExpr [Bind (PDInductivePat ctor (map PDPatVar tmps)) (VarExpr tmp)]
                      (ApplyExpr (LambdaExpr args expr) (map VarExpr tmps)))
    desugarArgPat APEmptyPat expr = do
      tmp <- fresh
      let tmp' = stringToVarWithIndices tmp
      return (tmp', LetExpr [Bind PDEmptyPat (VarExpr tmp)] expr)
    desugarArgPat (APConsPat arg1 arg2) expr = do
      tmp  <- fresh
      let tmp' = stringToVarWithIndices tmp
      tmp1 <- fresh
      tmp2 <- fresh
      return (tmp', LetExpr [Bind (PDConsPat (PDPatVar tmp1) (PDPatVar tmp2)) (VarExpr tmp)]
                     (ApplyExpr (LambdaExpr [arg1, Arg arg2] expr) [VarExpr tmp1, VarExpr tmp2]))
    desugarArgPat (APSnocPat arg1 arg2) expr = do
      tmp  <- fresh
      let tmp' = stringToVarWithIndices tmp
      tmp1 <- fresh
      tmp2 <- fresh
      return (tmp', LetExpr [Bind (PDSnocPat (PDPatVar tmp1) (PDPatVar tmp2)) (VarExpr tmp)]
                     (ApplyExpr (LambdaExpr [Arg arg1, arg2] expr) [VarExpr tmp1, VarExpr tmp2]))

desugar (LambdaExpr' vwis expr) = do
  let (vwis', expr') = foldr desugarInvertedArgs ([], expr) vwis
  let args' = map varWithIndicesToVar vwis'
  expr' <- desugar expr'
  return $ ILambdaExpr Nothing args' expr'
  where
    desugarInvertedArgs :: Arg VarWithIndices -> ([VarWithIndices], Expr) -> ([VarWithIndices], Expr)
    desugarInvertedArgs (Arg x) (args, expr) = (x : args, expr)
    desugarInvertedArgs (InvertedArg x) (args, expr) =
      let varName = extractNameFromVarWithIndices x
          flippedExpr = FlipIndicesExpr (VarExpr varName)
          bindPat = PDPatVar varName
      in (x : args, LetExpr [Bind bindPat flippedExpr] expr)

desugar (MemoizedLambdaExpr names expr) =
  IMemoizedLambdaExpr names <$> desugar expr

-- Typed memoized lambda is desugared the same way (type info used only for type checking)
desugar (TypedMemoizedLambdaExpr params _ body) =
  IMemoizedLambdaExpr (extractParamNames params) <$> desugar body
  where
    extractParamNames = concatMap extractName
    extractName (TPVar name _) = [name]
    extractName (TPInvertedVar name _) = [name]
    extractName (TPTuple elems) = concatMap extractName elems
    extractName (TPWildcard _) = []
    extractName (TPUntypedVar name) = [name]
    extractName TPUntypedWildcard = []

desugar (CambdaExpr name expr) =
  ICambdaExpr name <$> desugar expr

desugar (PatternFunctionExpr _names _pattern) =
  -- Pattern functions are only defined at TopExpr level
  -- They should not appear in expression context
  throwError $ Default "Pattern functions cannot be used as expressions"

desugar (IfExpr expr0 expr1 expr2) =
  IIfExpr <$> desugar expr0 <*> desugar expr1 <*> desugar expr2

desugar (LetExpr binds expr) =
  ILetExpr <$> desugarBindings binds <*> desugar expr

desugar (LetRecExpr binds expr) =
  ILetRecExpr <$> desugarBindings binds <*> desugar expr

desugar (WithSymbolsExpr vars expr) =
  IWithSymbolsExpr vars <$> desugar expr

desugar (MatchExpr pmmode expr0 expr1 clauses) =
  IMatchExpr pmmode <$> desugar expr0 <*> desugar expr1 <*> desugarMatchClauses clauses

desugar (MatchAllExpr pmmode expr0 expr1 clauses) =
  IMatchAllExpr pmmode <$> desugar expr0 <*> desugar expr1 <*> desugarMatchClauses clauses

desugar (DoExpr binds expr) =
  IDoExpr <$> desugarBindings binds <*> desugar expr

desugar (PrefixExpr "-" expr) = do
  expr' <- desugar expr
  return $ makeIApply "*" [IConstantExpr (IntegerExpr (-1)), expr']
desugar (PrefixExpr "!" (ApplyExpr expr args)) =
  IWedgeApplyExpr <$> desugar expr <*> mapM desugar args
desugar (PrefixExpr "'" expr) = IQuoteExpr <$> desugar expr
desugar (PrefixExpr "`" expr) = IQuoteSymbolExpr <$> desugar expr
desugar (PrefixExpr op _) = fail ("Unknown prefix " ++ op)

desugar (InfixExpr op expr1 expr2) | isWedge op =
  (\x y -> IWedgeApplyExpr (IVarExpr (repr op)) [x, y])
    <$> desugar expr1 <*> desugar expr2

desugar (InfixExpr op expr1 expr2) | repr op == "::" =
  IConsExpr <$> desugar expr1 <*> desugar expr2
desugar (InfixExpr op expr1 expr2) | repr op == "++" =
  IJoinExpr <$> desugar expr1 <*> desugar expr2
desugar (InfixExpr op expr1 expr2) =
  (\x y -> makeIApply (repr op) [x, y]) <$> desugar expr1 <*> desugar expr2

-- section
--
-- If `op` is not a cambda, simply desugar it into the function
desugar (SectionExpr op Nothing Nothing)
  | not (isWedge op || repr op `elem` ["::", "++"]) =
    desugar (VarExpr (repr op))
desugar (SectionExpr op Nothing Nothing) = do
  x <- fresh
  y <- fresh
  ILambdaExpr Nothing [stringToVar x, stringToVar y] <$> desugar (InfixExpr op (VarExpr x) (VarExpr y))

desugar (SectionExpr op Nothing (Just expr2)) = do
  x <- fresh
  ILambdaExpr Nothing [stringToVar x] <$> desugar (InfixExpr op (VarExpr x) expr2)

desugar (SectionExpr op (Just expr1) Nothing) = do
  y <- fresh
  ILambdaExpr Nothing [stringToVar y] <$> desugar (InfixExpr op expr1 (VarExpr y))

desugar SectionExpr{} = throwError $ Default "Cannot reach here: section with both arguments"

desugar (SeqExpr expr0 expr1) =
  ISeqExpr <$> desugar expr0 <*> desugar expr1

desugar (GenerateTensorExpr fnExpr sizeExpr) =
  IGenerateTensorExpr <$> desugar fnExpr <*> desugar sizeExpr

desugar (TensorContractExpr tExpr) =
  ITensorContractExpr <$> desugar tExpr

desugar (TensorMapExpr (LambdaExpr' [x] (TensorMapExpr (LambdaExpr' [y] expr) b)) a) =
  desugar (TensorMap2Expr (LambdaExpr' [x, y] expr) a b)
desugar (TensorMapExpr (LambdaExpr [x] (TensorMapExpr (LambdaExpr [y] expr) b)) a) =
  desugar (TensorMap2Expr (LambdaExpr [x, y] expr) a b)

desugar (TensorMapExpr fnExpr tExpr) =
  ITensorMapExpr <$> desugar fnExpr <*> desugar tExpr

desugar (TensorMap2Expr fnExpr t1Expr t2Expr) =
  ITensorMap2Expr <$> desugar fnExpr <*> desugar t1Expr <*> desugar t2Expr

desugar (TransposeExpr vars expr) =
  -- ITransposeExpr takes (permutation, tensor) as arguments to match tTranspose
  ITransposeExpr <$> desugar vars <*> desugar expr

desugar (FlipIndicesExpr expr) =
  IFlipIndicesExpr <$> desugar expr

desugar (ApplyExpr expr args) =
  IApplyExpr <$> desugar expr <*> mapM desugar args

desugar FreshVarExpr = do
  id <- fresh
  return $ IVarExpr (":::" ++ id)

desugar (MatcherExpr patternDefs) =
  IMatcherExpr <$> mapM desugarPatternDef patternDefs

desugar (AnonParamExpr n) = return $ IVarExpr ('%' : show n)

desugar (AnonParamFuncExpr n expr) = do
  let args = map (\n -> stringToVarWithIndices ('%' : show n)) [1..n]
  lambda <- desugar $ LambdaExpr' (map Arg args) expr
  return $ ILetRecExpr [(PDPatVar (stringToVar "%0"), lambda)] (IVarExpr "%0")

desugar (AnonTupleParamFuncExpr 1 expr) = do
  lambda <- desugar $ LambdaExpr' [Arg (stringToVarWithIndices "%1")] expr
  return $ ILetRecExpr [(PDPatVar (stringToVar "%0"), lambda)] (IVarExpr "%0")
desugar (AnonTupleParamFuncExpr n expr) = do
  let args = map (\n -> stringToVarWithIndices ('%' : show n)) [1..n]
  lambda <- desugar $
    LambdaExpr [Arg (APTuplePat $ map (Arg . APPatVar) args)] expr
  return $ ILetRecExpr [(PDPatVar (stringToVar "%0"), lambda)] (IVarExpr "%0")

desugar (AnonListParamFuncExpr n expr) = do
  let args' = map (\n -> Arg (APPatVar (stringToVarWithIndices ('%' : show n)))) [1..n]
  let args = foldr APConsPat APEmptyPat args'
  lambda <- desugar $ LambdaExpr [Arg args] expr
  return $ ILetRecExpr [(PDPatVar (stringToVar "%0"), lambda)] (IVarExpr "%0")

desugar (QuoteExpr expr) =
  IQuoteExpr <$> desugar expr

desugar (QuoteSymbolExpr expr) =
  IQuoteSymbolExpr <$> desugar expr

desugar (WedgeApplyExpr expr args) =
  IWedgeApplyExpr <$> desugar expr <*> mapM desugar args

desugar (FunctionExpr args) = return $ IFunctionExpr args

-- Type annotation is erased at runtime
desugar (TypeAnnotation expr _typeExpr) = desugar expr

-- `simplify <expr> using <ruleName>` (Phase 7.6).
-- Desugars to a direct call of the registered rule lambda:
--   simplify e using r  ⇒  rule.r e
-- The rule lambda was emitted by `desugarTopExpr (DeclareRule (Just r) ...)`.
desugar (SimplifyUsingExpr body ruleName) = do
  bodyI <- desugar body
  return $ IApplyExpr (IVarExpr ("rule." ++ ruleName)) [bodyI]

-- Typed lambda is desugared to regular lambda
desugar (TypedLambdaExpr params _retType body) = do
  let args = map (\(name, _) -> Arg (APPatVar (VarWithIndices name []))) params
  desugar $ LambdaExpr args body

desugarIndex :: IndexExpr Expr -> EvalM (Index IExpr)
desugarIndex (Subscript e)    = Sub <$> desugar e
desugarIndex (Superscript e)  = Sup <$> desugar e
desugarIndex (SupSubscript e) = SupSub <$> desugar e
desugarIndex (Userscript e)   = User <$> desugar e
desugarIndex _                = undefined

desugarPattern :: Pattern -> EvalM IPattern
desugarPattern pat =
  case collectName pat of
    []    -> desugarPattern' pat
    names -> ILetPat (map makeBinding names) <$> desugarPattern' pat
 where
   collectNames :: [Pattern] -> [String]
   collectNames pats = foldl union [] (map collectName pats)

   collectName :: Pattern -> [String]
   collectName (ForallPat pat1 pat2)                           = collectName pat1 `union` collectName pat2
   collectName (InfixPat _ pat1 pat2)                          = collectName pat1 `union` collectName pat2
   collectName (NotPat pat)                                    = collectName pat
   collectName (AndPat pat1 pat2)                              = collectName pat1 `union` collectName pat2
   collectName (OrPat pat1 pat2)                               = collectName pat1 `union` collectName pat2
   collectName (TuplePat pats)                                 = collectNames pats
   collectName (InductiveOrPApplyPat _ pats)                   = collectNames pats
   collectName (InductivePat _ pats)                           = collectNames pats
   collectName (PApplyPat _ pats)                              = collectNames pats
   collectName (DApplyPat _ pats)                              = collectNames pats
   collectName (LoopPat _ (LoopRange _ _ endNumPat) pat1 pat2) = collectName endNumPat `union` collectName pat1 `union` collectName pat2
   collectName (LetPat _ pat)                                  = collectName pat
   collectName (IndexedPat (PatVar var) _)                     = [var]
   collectName _                                               = []

   makeBinding :: String -> IBindingExpr
   makeBinding var = (PDPatVar (stringToVar var), IHashExpr [])

desugarPattern' :: Pattern -> EvalM IPattern
desugarPattern' WildCard        = return IWildCard
desugarPattern' ContPat         = return IContPat
desugarPattern' SeqNilPat       = return ISeqNilPat
desugarPattern' LaterPatVar     = return ILaterPatVar
desugarPattern' (VarPat v)      = return (IVarPat v)
desugarPattern' (PatVar var)    = return (IPatVar var)
desugarPattern' (ValuePat expr) = IValuePat <$> desugar expr
desugarPattern' (PredPat expr)  = IPredPat <$> desugar expr
desugarPattern' (NotPat pat)       = INotPat <$> desugarPattern' pat
desugarPattern' (AndPat pat1 pat2) = IAndPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (OrPat pat1 pat2)  = IOrPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (ForallPat pat1 pat2) = IForallPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (InfixPat Op{ repr = "&" } pat1 pat2) =
  IAndPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (InfixPat Op{ repr = "|" } pat1 pat2) =
  IOrPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (InfixPat Op{ repr = f } pat1 pat2) =
  (\x y -> IInductivePat f [x, y]) <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (TuplePat pats) = ITuplePat <$> mapM desugarPattern' pats
desugarPattern' (InductiveOrPApplyPat name pats) = IInductiveOrPApplyPat name <$> mapM desugarPattern' pats
-- Convert all InductivePat to IInductiveOrPApplyPat since we cannot distinguish between
-- pattern constructors and pattern functions at parse time
desugarPattern' (InductivePat name pats) = IInductiveOrPApplyPat name <$> mapM desugarPattern' pats
desugarPattern' (IndexedPat pat exprs) = IIndexedPat <$> desugarPattern' pat <*> mapM desugar exprs
desugarPattern' (PApplyPat expr pats) = IPApplyPat <$> desugar expr <*> mapM desugarPattern' pats
desugarPattern' (DApplyPat pat pats) = IDApplyPat <$> desugarPattern' pat <*> mapM desugarPattern' pats
desugarPattern' (LoopPat name range pat1 pat2) = ILoopPat name <$> desugarLoopRange range <*> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (LetPat binds pat) = ILetPat <$> desugarBindings binds <*> desugarPattern' pat
desugarPattern' (SeqConsPat pat1 pat2) = ISeqConsPat <$> desugarPattern' pat1 <*> desugarPattern' pat2

desugarLoopRange :: LoopRange -> EvalM ILoopRange
desugarLoopRange (LoopRange sExpr eExpr pat) =
  ILoopRange <$> desugar sExpr <*> desugar eExpr <*> desugarPattern' pat

desugarBindings :: [BindingExpr] -> EvalM [IBindingExpr]
desugarBindings = mapM desugarBinding
  where
    desugarBinding (Bind name expr) = do
      let name' = fmap stringToVar name
      expr' <- desugar expr
      case (name, expr') of
        (PDPatVar var, ILambdaExpr Nothing args body) ->
          return (name', ILambdaExpr (Just (Var var [])) args body)
        _ -> return (name', expr')
    desugarBinding (BindWithIndices vwi expr) = do
      (var, iexpr) <- desugarDefineWithIndices vwi expr
      return (PDPatVar var, iexpr)
    -- BindWithType: desugar like DefineWithType
    desugarBinding (BindWithType typedVarWI body) = do
      let name = typedVarName typedVarWI
          params = typedVarParams typedVarWI
          argPatterns = map typedParamToArgPattern params
          lambdaExpr = if null argPatterns
                         then body
                         else LambdaExpr argPatterns body
      body' <- desugar lambdaExpr
      let body'' = case body' of
            ILambdaExpr Nothing args b -> ILambdaExpr (Just (Var name [])) args b
            other -> other
      return (PDPatVar (Var name []), body'')

desugarMatchClauses :: [MatchClause] -> EvalM [IMatchClause]
desugarMatchClauses = mapM (\(pat, expr) -> (,) <$> desugarPattern pat <*> desugar expr)

desugarPatternDef :: PatternDef -> EvalM IPatternDef
desugarPatternDef (PatternDef pp matcher pds) =
  (pp,,) <$> desugar matcher <*> desugarPrimitiveDataMatchClauses pds

desugarPrimitiveDataMatchClauses :: [(PrimitiveDataPattern, Expr)] -> EvalM [(IPrimitiveDataPattern, IExpr)]
desugarPrimitiveDataMatchClauses = mapM (\(pd, expr) -> (fmap stringToVar pd,) <$> desugar expr)

desugarDefineWithIndices :: VarWithIndices -> Expr -> EvalM (Var, IExpr)
-- Case 1: No indices - simple desugaring without withSymbols/transpose
desugarDefineWithIndices (VarWithIndices name []) expr = do
  expr' <- desugar expr
  return (Var name [], expr')

-- Case 2: Non-empty indices - wrap with withSymbols and transpose
desugarDefineWithIndices (VarWithIndices name is) expr = do
  let (isSubs, indexNames) = unzip $ concatMap extractSubSupIndex is
  expr <- if any isExtendedIndice is
             then desugarExtendedIndices is isSubs indexNames expr
             else return expr
  body <- desugar expr
  let indexNamesCollection = ICollectionExpr (map IVarExpr indexNames)
  let is' = map (\b -> if b then Sub Nothing else Sup Nothing) isSubs
  -- ITransposeExpr takes (permutation, tensor) as arguments to match tTranspose
  return (Var name is', IWithSymbolsExpr indexNames (ITransposeExpr indexNamesCollection body))

varWithIndicesToVar :: VarWithIndices -> Var
varWithIndicesToVar (VarWithIndices name is) = Var name (concatMap transVarIndex is)

transVarIndex :: VarIndex -> [Index (Maybe Var)]
transVarIndex (VSubscript x)            = [Sub (Just (stringToVar x))]
transVarIndex (VSuperscript x)          = [Sup (Just (stringToVar x))]
transVarIndex (VMultiSubscript x s e)   = [MultiSub (Just (stringToVar x)) s (Just (stringToVar e))]
transVarIndex (VMultiSuperscript x s e) = [MultiSup (Just (stringToVar x)) s (Just (stringToVar e))]
transVarIndex (VGroupScripts xs)        = concatMap transVarIndex xs
transVarIndex (VSymmScripts xs)         = concatMap transVarIndex xs
transVarIndex (VAntiSymmScripts xs)     = concatMap transVarIndex xs

extractSubSupIndex :: VarIndex -> [(Bool, String)]
extractSubSupIndex (VSubscript x)        = [(True, x)]
extractSubSupIndex (VSuperscript x)      = [(False, x)]
extractSubSupIndex (VGroupScripts xs)    = concatMap extractSubSupIndex xs
extractSubSupIndex (VSymmScripts xs)     = concatMap extractSubSupIndex xs
extractSubSupIndex (VAntiSymmScripts xs) = concatMap extractSubSupIndex xs

desugarExtendedIndices :: [VarIndex] -> [Bool] -> [String] -> Expr -> EvalM Expr
desugarExtendedIndices indices isSubs indexNames tensorBody = do
  tensorName <- fresh
  tensorGenExpr <- f indices (VarExpr tensorName) [] []
  let indexFunctionExpr = LambdaExpr [Arg $ foldr APConsPat APEmptyPat (map (Arg . APPatVar) (map stringToVarWithIndices indexNames))] tensorGenExpr
  let genTensorExpr = GenerateTensorExpr indexFunctionExpr (makeApply "tensorShape" [VarExpr tensorName])
  let tensorIndices = zipWith (\isSub name -> if isSub then Subscript (VarExpr name) else Superscript (VarExpr name)) isSubs indexNames
  return $ LetExpr [Bind (PDPatVar tensorName) tensorBody] (IndexedExpr True genTensorExpr tensorIndices)
 where
  f :: [VarIndex] -> Expr -> [String] -> [BindingExpr] -> EvalM Expr
  f [] expr [] []       = return expr
  f [] expr [] bindings = return $ LetRecExpr bindings expr
  f [] expr signs bindings =
    return $ LetRecExpr bindings (makeApply "product" [CollectionExpr (map VarExpr signs ++ [expr])])
  f (index:indices) expr signs bindings = do
    (indices', signs', bindings') <- genBindings index
    let isSubs = subOrSupScripts index
    symbols <- mapM (const fresh) isSubs
    let is = zipWith (\x isSub -> (if isSub then Subscript else Superscript) (VarExpr x)) symbols isSubs
    f indices (IndexedExpr True expr is)
      (signs ++ signs') (bindings ++ bindings' ++ [Bind (foldr (PDConsPat . PDPatVar) PDEmptyPat symbols) indices'])

  subOrSupScripts :: VarIndex -> [Bool]
  subOrSupScripts VSubscript{}          = [True]
  subOrSupScripts VSuperscript{}        = [False]
  subOrSupScripts (VGroupScripts xs)    = concatMap subOrSupScripts xs
  subOrSupScripts (VSymmScripts xs)     = concatMap subOrSupScripts xs
  subOrSupScripts (VAntiSymmScripts xs) = concatMap subOrSupScripts xs

  genBindings :: VarIndex -> EvalM (Expr, [String], [BindingExpr])
  genBindings (VSubscript x)   = return (CollectionExpr [VarExpr x], [], [])
  genBindings (VSuperscript x) = return (CollectionExpr [VarExpr x], [], [])
  genBindings (VGroupScripts xs) = do
    (indices, signss, bindingss) <- unzip3 <$> mapM genBindings xs
    let newIndices =
          -- If indices are all CollectionExpr, we can calculate the concatenated result of them
          case allCollections indices of
            Just xs -> CollectionExpr xs
            Nothing -> makeApply "concat" [CollectionExpr indices]
    return (newIndices, concat signss, concat bindingss)
    where
      allCollections []                          = Just []
      allCollections (CollectionExpr xs : exprs) = (xs ++) <$> allCollections exprs
      allCollections _                           = Nothing
  genBindings (VSymmScripts xs) = do
    (indices, signss, bindingss) <- unzip3 <$> mapM genBindings xs
    let signs = concat signss
    let bindings = concat bindingss
    sortedCollectionName <- fresh
    let newBindings = bindings ++ [Bind (PDTuplePat [PDWildCard, PDPatVar sortedCollectionName]) (makeApply "sortWithSign" [CollectionExpr indices])]
    return (VarExpr sortedCollectionName, signs, newBindings)
  genBindings (VAntiSymmScripts xs) = do
    (indices, signss, bindingss) <- unzip3 <$> mapM genBindings xs
    let signs = concat signss
    let bindings = concat bindingss
    sortedCollectionName <- fresh
    signName <- fresh
    let newBindings = bindings ++ [Bind (PDTuplePat [PDPatVar signName, PDPatVar sortedCollectionName]) (makeApply "sortWithSign" [CollectionExpr indices])]
    return (VarExpr sortedCollectionName, signName : signs, newBindings)

--
-- Utils
--

extractIndexExpr :: IndexExpr a -> a
extractIndexExpr (Subscript x)    = x
extractIndexExpr (Superscript x)  = x
extractIndexExpr (SupSubscript x) = x
extractIndexExpr (Userscript x)   = x
extractIndexExpr _                = error "extractIndexExpr: Not supported"

isExtendedIndice :: VarIndex -> Bool
isExtendedIndice VSubscript{}            = False
isExtendedIndice VSuperscript{}          = False
isExtendedIndice (VGroupScripts (x:_))   = isExtendedIndice x
isExtendedIndice (VGroupScripts [])      = True
isExtendedIndice _                       = True
