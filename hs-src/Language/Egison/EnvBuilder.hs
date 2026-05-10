{- |
Module      : Language.Egison.EnvBuilder
Licence     : MIT

This module implements Phase 2: Environment Building Phase.
It collects all declarations from TopExpr list before type inference and evaluation.

Environment Building Phase (Phase 2):
  1. Data constructor definitions collection (from InductiveDecl)
  2. Type class definitions collection (from ClassDeclExpr)
  3. Instance definitions collection (from InstanceDeclExpr)
  4. Type signature collection (from Define, DefineWithType)

This phase must be completed BEFORE type inference (Phase 5) begins,
ensuring all necessary information is available for type checking.
-}

module Language.Egison.EnvBuilder
  ( buildEnvironments
  , EnvBuildResult(..)
  ) where

import           Control.Monad              (foldM)
import           Control.Monad.Except       (throwError)
import qualified Data.HashMap.Strict        as HashMap

import           Language.Egison.AST
import           Language.Egison.Data       (EvalM, EgisonError(..))
import           Language.Egison.EvalState  (ConstructorInfo(..), ConstructorEnv, PatternConstructorEnv)
import           Language.Egison.IExpr      (Var(..), stringToVar)
import           Language.Egison.Desugar    (transVarIndex)
import           Language.Egison.Type.Env   (TypeEnv, ClassEnv, PatternTypeEnv, emptyEnv, emptyClassEnv, emptyPatternEnv,
                                             extendEnv, extendPatternEnv, addClass, addInstance, lookupClass)
import qualified Language.Egison.Type.Types as Types
import           Language.Egison.Type.Types (Type(..), TyVar(..), Constraint(..), TypeScheme(..),
                                             freeTyVars, sanitizeMethodName, typeExprToType,
                                             capitalizeFirst, lowerFirst)
import qualified Data.Set as Set

-- | Result of environment building phase
data EnvBuildResult = EnvBuildResult
  { ebrTypeEnv        :: TypeEnv         -- ^ Type signatures for definitions
  , ebrClassEnv       :: ClassEnv        -- ^ Type class and instance information
  , ebrConstructorEnv :: ConstructorEnv  -- ^ Data constructor information
  , ebrPatternConstructorEnv :: PatternConstructorEnv  -- ^ Pattern constructor information
  , ebrPatternTypeEnv :: PatternTypeEnv  -- ^ Pattern function information
  -- Phase 7.4/7.5: collected `declare rule` declarations. Stored as the raw
  -- (name, level, lhs, rhs) tuple. Rule application is not yet wired into
  -- normalization; this field exists so the data round-trips through env
  -- building and is available for inspection / future Phase 7.5 code.
  , ebrReductionRules :: [(Maybe String, RuleLevel, Pattern, Expr)]
  -- Phase 6.3: collected `declare derivative` declarations. Maps function
  -- name -> derivative expression. Wiring into `Differentiable Factor` is
  -- still pending; for now, just the registration list.
  , ebrDerivativeRules :: [(String, Expr)]
  -- Names of functions declared via `declare mathfunc`. Used by the
  -- DeclareApply handler to enforce that `declare apply foo ...` only
  -- appears after a corresponding `declare mathfunc foo`.
  , ebrMathFuncNames :: Set.Set String
  } deriving (Show)

--------------------------------------------------------------------------------
-- Phase 2: Environment Building Phase
--------------------------------------------------------------------------------

-- | Build all environments from a list of top-level expressions.
-- This function implements Phase 2 of the processing flow.
-- It must be called AFTER expandLoads (Phase 1) and BEFORE type inference (Phase 5).
buildEnvironments :: [TopExpr] -> EvalM EnvBuildResult
buildEnvironments exprs = do
  -- Start with empty environments
  let initialResult = EnvBuildResult
        { ebrTypeEnv = emptyEnv
        , ebrClassEnv = emptyClassEnv
        , ebrConstructorEnv = HashMap.empty
        , ebrPatternConstructorEnv = emptyPatternEnv
        , ebrPatternTypeEnv = emptyPatternEnv
        , ebrReductionRules = []
        , ebrDerivativeRules = []
        , ebrMathFuncNames = Set.empty
        }
  
  -- Process each top-level expression to collect declarations
  foldM processTopExpr initialResult exprs

-- | Process a single top-level expression to collect environment information
processTopExpr :: EnvBuildResult -> TopExpr -> EvalM EnvBuildResult
processTopExpr result topExpr = case topExpr of
  
  -- 1. Data Constructor Definitions (from InductiveDecl)
  InductiveDecl typeName typeParams constructors -> do
    let typeParamVars = map (TVar . TyVar) typeParams
        adtType = TInductive typeName typeParamVars
        typeEnv = ebrTypeEnv result
        ctorEnv = ebrConstructorEnv result
    
    -- Register each constructor
    (typeEnv', ctorEnv') <- foldM (registerConstructor typeName typeParams adtType) 
                                   (typeEnv, ctorEnv) 
                                   constructors
    
    return result { ebrTypeEnv = typeEnv', ebrConstructorEnv = ctorEnv' }
  
  -- 2. Type Class Definitions (from ClassDeclExpr).
  -- Supports any number of type parameters (single-param `class Eq a` and
  -- multi-param `class Embed a b` go through the same path). Methods are still
  -- registered against the *first* parameter for backward compatibility with
  -- existing single-param infrastructure (Phase 5.5 multi-param-aware
  -- elaboration is a separate task).
  ClassDeclExpr (ClassDecl className typeParams superClasses methods) | not (null typeParams) -> do
    let classEnv = ebrClassEnv result
        typeEnv = ebrTypeEnv result
        tyVars = map TyVar typeParams

        -- Extract superclass names from ConstraintExprs
        superNames = map extractConstraintName superClasses

        -- Build method list with types
        methodsWithTypes = map extractMethodWithType methods

        -- Create ClassInfo
        -- Note: Use qualified name to avoid ambiguity with ClassDecl.classMethods
        classInfo = Types.ClassInfo
          { Types.classSupers = superNames
          , Types.classParams = tyVars
          , Types.classMethods = methodsWithTypes
          }

        -- Register class
        classEnv' = addClass className classInfo classEnv

        -- Register each class method to type environment
        typeEnv' = foldl (registerClassMethod tyVars className) typeEnv methods

    return result { ebrClassEnv = classEnv', ebrTypeEnv = typeEnv' }

  ClassDeclExpr _ ->
    -- Class with no type parameters is rejected.
    return result
  
  -- 3. Instance Definitions (from InstanceDeclExpr).
  -- Multi-param-friendly: instance declarations may carry one or more
  -- types (`instance Embed Integer (Frac Integer) where ...`). All of them
  -- are stored in `instTypes`; the legacy `instType` accessor reads the head.
  InstanceDeclExpr (InstanceDecl context className instTypes methods) -> do
    let classEnv = ebrClassEnv result
        typeEnv = ebrTypeEnv result

        -- Convert all instance types
        instanceTypeList = map typeExprToType instTypes

        -- Get the primary instance type (head) for backward compatibility
        mainInstType = case instanceTypeList of
          []    -> TAny
          (t:_) -> t

        -- Create InstanceInfo
        instInfo = Types.InstanceInfo
          { Types.instContext = map constraintToInternal context
          , Types.instClass = className
          , Types.instTypes = instanceTypeList
          , Types.instMethods = []  -- Methods are handled during desugaring/evaluation
          }
        
        -- Register instance
        classEnv' = addInstance className instInfo classEnv
        
        -- Register method type signatures for generated methods
        -- This prevents "Unbound variable" warnings during type inference.
        -- Pass the full instance type list so the registered names match the
        -- ones Desugar emits (e.g. `embedMathValueMathValueEmbed`,
        -- `embedMathValueMathValue` for `instance Embed MathValue MathValue`).
        typeEnv' = registerInstanceMethods className mainInstType instanceTypeList (map constraintToInternal context) methods classEnv' typeEnv

    return result { ebrClassEnv = classEnv', ebrTypeEnv = typeEnv' }
  
  -- 4. Type Signature Collection (from Define, DefineWithType)
  -- Note: We only collect explicit type signatures here.
  -- Inferred types will be added during type inference.
  DefineWithType typedVar _expr -> do
    let name = typedVarName typedVar
        varIndices = typedVarIndices typedVar
        -- Convert VarIndex to Index (Maybe Var) - like transVarIndex but with Nothing content
        indexTypes = concatMap transVarIndex varIndices
        -- Create Var with index structure (content is Just Var, so map to Nothing)
        var = Var name (map (fmap (const Nothing)) indexTypes)
        params = typedVarParams typedVar
        retType = typeExprToType (typedVarRetType typedVar)
        paramTypes = map typedParamToType params
        
        -- Build function type
        funType = foldr TFun retType paramTypes
        
        -- Convert constraints from AST to internal representation
        constraints = map constraintToInternal (typedVarConstraints typedVar)
        
        -- Generalize free type variables in the type signature
        -- This handles type parameters like {a, b, c} in def compose {a, b, c} ...
        freeVars = Set.toList (freeTyVars funType)
        typeScheme = Types.Forall freeVars constraints funType
        
        typeEnv = ebrTypeEnv result
        typeEnv' = extendEnv var typeScheme typeEnv
    
    return result { ebrTypeEnv = typeEnv' }
  
  -- 5. Pattern Inductive Declarations (from PatternInductiveDecl)
  PatternInductiveDecl typeName typeParams constructors -> do
    let typeParamVars = map (TVar . TyVar) typeParams
        -- Special cases: [a] as TCollection and String as TString
        patternType = case (typeName, typeParams) of
                        ("[]", [param]) -> TCollection (TVar (TyVar param))
                        ("String", [])  -> TString
                        _               -> TInductive typeName typeParamVars
        patternCtorEnv = ebrPatternConstructorEnv result
    
    -- Register each pattern constructor to pattern constructor environment
    patternCtorEnv' <- foldM (registerPatternConstructor typeName typeParams patternType) 
                              patternCtorEnv 
                              constructors
    
    return result { ebrPatternConstructorEnv = patternCtorEnv' }
  
  -- 6. Pattern Function Declarations (from PatternFunctionDecl)
  PatternFunctionDecl name typeParams params retType _body -> do
    let paramTypes = map (typeExprToType . snd) params
        retType' = typeExprToType retType
        -- Pattern function type: arg1 -> arg2 -> ... -> retType (without Pattern wrapper)
        patternFuncType = foldr TFun retType' paramTypes
        
        -- Quantify over type parameters
        tyVars = map TyVar typeParams
        typeScheme = Types.Forall tyVars [] patternFuncType
        
        patternEnv = ebrPatternTypeEnv result
        patternEnv' = extendPatternEnv name typeScheme patternEnv
    
    return result { ebrPatternTypeEnv = patternEnv' }
  
  -- Other expressions don't contribute to environment building
  Define {} -> return result
  Test {} -> return result
  Execute {} -> return result
  LoadFile {} -> return result  -- Should not appear after expandLoads
  InfixDecl {} -> return result
  
  -- 7. Symbol Declarations (from DeclareSymbol)
  DeclareSymbol names mTypeExpr -> do
    let ty = case mTypeExpr of
               Just texpr -> typeExprToType texpr
               Nothing    -> TInt  -- Default to Integer (MathValue)
        scheme = Forall [] [] ty
        typeEnv = ebrTypeEnv result
        -- Add each symbol to the type environment
        typeEnv' = foldr (\name env -> extendEnv (stringToVar name) scheme env) typeEnv names
    return result { ebrTypeEnv = typeEnv' }

  -- 8. Reduction Rule Declarations (from DeclareRule, Phase 7.4)
  -- The parser accepts the rule and we now stash the (name, level, lhs, rhs)
  -- tuple in `ebrReductionRules` for later inspection / Phase 7.5 use.
  -- Application during `casNormalize` is still pending.
  DeclareRule mname level lhs rhs ->
    return result { ebrReductionRules = ebrReductionRules result ++
                                          [(mname, level, lhs, rhs)] }

  -- 9. Derivative Declarations (from DeclareDerivative, Phase 6.3)
  -- Stash the (name, expr) pair in `ebrDerivativeRules`. Wiring into
  -- `Differentiable Factor`'s connection rule is still pending.
  DeclareDerivative name rhs ->
    return result { ebrDerivativeRules = ebrDerivativeRules result ++
                                            [(name, rhs)] }

  -- 10. Math function declarations (Phase 6.3 part 5).
  -- Register the function's type signature so the inference engine knows
  -- `f : MathValue -> MathValue` (the wrapper body that quotes the symbol).
  -- Without this, binary operations like `f 3 + f 4` infer `Any + Any` and
  -- the type-class `+` dispatch fails (returning the method-name string
  -- "plus" where a CASData was expected).
  -- The default `MathValue -> MathValue` may be widened by a subsequent
  -- `declare apply` based on its argument count (see DeclareApply below).
  DeclareMathFunc name mTypeExpr -> do
    let ty = case mTypeExpr of
               Just texpr -> typeExprToType texpr
               Nothing    -> TFun TMathValue TMathValue
        scheme = Forall [] [] ty
        typeEnv = ebrTypeEnv result
        typeEnv' = extendEnv (stringToVar name) scheme typeEnv
        names'   = Set.insert name (ebrMathFuncNames result)
    return result { ebrTypeEnv = typeEnv', ebrMathFuncNames = names' }

  -- 11. Math function application rules (Phase A of declare apply impl).
  -- Requires a prior `declare mathfunc <name>` so the function's type and
  -- intent are known. The actual implementation is emitted by Desugar as a
  -- plain `def` that overrides the mathfunc wrapper.
  -- Updates the registered type to `MathValue -> MathValue -> ... -> MathValue`
  -- (one MathValue per arg + result), unless `declare mathfunc` had an
  -- explicit type annotation that already matches arity.
  DeclareApply name args _body -> do
    if Set.member name (ebrMathFuncNames result)
      then do
        let arity = length args
            -- Build MathValue -> MathValue -> ... -> MathValue (n+1 MathValues)
            mathFunTy 0 = TMathValue
            mathFunTy n = TFun TMathValue (mathFunTy (n - 1))
            ty' = mathFunTy arity
            scheme' = Forall [] [] ty'
            typeEnv' = extendEnv (stringToVar name) scheme' (ebrTypeEnv result)
        return result { ebrTypeEnv = typeEnv' }
      else throwError $ Default $
        "declare apply " ++ name ++ ": no prior `declare mathfunc " ++ name ++ "`. " ++
        "Add `declare mathfunc " ++ name ++ "` before this `declare apply` declaration."

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Register a single data constructor
registerConstructor :: String -> [String] -> Type 
                    -> (TypeEnv, ConstructorEnv) -> InductiveConstructor 
                    -> EvalM (TypeEnv, ConstructorEnv)
registerConstructor typeName typeParams resultType (typeEnv, ctorEnv) 
                    (InductiveConstructor ctorName argTypeExprs) = do
  let argTypes = map typeExprToType argTypeExprs
      
      -- Constructor type: argTypes -> resultType
      constructorType = foldr TFun resultType argTypes
      
      -- Quantify over type parameters
      tyVars = map TyVar typeParams
      typeScheme = Types.Forall tyVars [] constructorType
      
      -- Add to type environment
      typeEnv' = extendEnv (stringToVar ctorName) typeScheme typeEnv
      
      -- Add to constructor environment (for pattern matching and evaluation)
      ctorInfo = ConstructorInfo
        { ctorTypeName = typeName
        , ctorArgTypes = argTypes
        , ctorTypeParams = typeParams
        }
      ctorEnv' = HashMap.insert ctorName ctorInfo ctorEnv
  
  return (typeEnv', ctorEnv')

-- | Register a class method to the type environment.
-- The constraint carries ALL class type parameters (multi-param-friendly).
-- For `class Coerce a b where coerce (x: a) : b`, the method is registered
-- with `forall a b. Coerce a b => a -> b`.
registerClassMethod :: [TyVar] -> String -> TypeEnv -> ClassMethod -> TypeEnv
registerClassMethod tyVars className typeEnv (ClassMethod methName params retType _defaultImpl) =
  let paramTypes = map typedParamToType params
      methodType = foldr TFun (typeExprToType retType) paramTypes
      -- Constraint with all class type params (single-param classes still
      -- produce a singleton list).
      constraint = Types.Constraint className (map TVar tyVars)
      typeScheme = Types.Forall tyVars [constraint] methodType
  in
    extendEnv (stringToVar methName) typeScheme typeEnv

-- | Register type signatures for instance methods (generated during desugaring)
-- This prevents "Unbound variable" warnings during type inference.
--
-- Names must match Desugar's `desugarInstanceMethod` / `makeDictDef`, which
-- concatenate the type-constructor name of EVERY instance type (multi-param
-- friendly): e.g. `instance Embed MathValue MathValue` →
-- `embedMathValueMathValueEmbed` (method) and `embedMathValueMathValue`
-- (dictionary).
registerInstanceMethods :: String -> Type -> [Type] -> [Constraint] -> [InstanceMethod] -> ClassEnv -> TypeEnv -> TypeEnv
registerInstanceMethods className instType instTypeList instConstraints methods classEnv typeEnv =
  case lookupClass className classEnv of
    Nothing -> typeEnv  -- Class not found, skip
    Just classInfo ->
      -- Register each instance method
      let typeEnv' = foldr (registerInstanceMethod className instType instTypeList instConstraints classInfo) typeEnv methods

          -- Also register the dictionary itself
          -- e.g., embedMathValueMathValue : Hash String (MathValue -> MathValue)
          instTypeName = concatMap Types.typeToName instTypeList
          dictName = lowerFirst className ++ instTypeName

          -- Build dictionary type: Hash String (method type)
          -- All methods should have the same general shape, so we use the first one
          dictValueType = case methods of
            [] -> TAny
            (m:_) -> case lookup (instanceMethodName m) (Types.classMethods classInfo) of
              Nothing -> TAny
              Just methodType ->
                let tyVar = Types.classParam classInfo
                    substitutedType = substituteTypeVar tyVar instType methodType
                in substitutedType

          dictType = THash TString dictValueType
          freeVars = Set.toList (freeTyVars dictType)
          dictScheme = Types.Forall freeVars instConstraints dictType
      in
        extendEnv (stringToVar dictName) dictScheme typeEnv'
  where
    instanceMethodName :: InstanceMethod -> String
    instanceMethodName (InstanceMethod name _ _) = name

    registerInstanceMethod :: String -> Type -> [Type] -> [Constraint] -> Types.ClassInfo -> InstanceMethod -> TypeEnv -> TypeEnv
    registerInstanceMethod clsName instTy instTyList constraints classInfo (InstanceMethod methName _params _body) env =
      -- Find the method in the class definition
      case lookup methName (Types.classMethods classInfo) of
        Nothing -> env  -- Method not in class definition, skip
        Just methodType ->
          -- Substitute type variable with instance type (use first type for the method body type)
          let tyVar = Types.classParam classInfo
              substitutedType = substituteTypeVar tyVar instTy methodType

              -- Generate method name from ALL instance types (matching Desugar)
              -- e.g., "embedMathValueMathValueEmbed" for instance Embed MathValue MathValue
              instTypeName = concatMap Types.typeToName instTyList
              sanitizedName = sanitizeMethodName methName
              generatedMethodName = lowerFirst clsName ++ instTypeName ++ capitalizeFirst sanitizedName

              -- Extract free type variables from the substituted type
              freeVars = Set.toList (freeTyVars substitutedType)

              -- Create type scheme with constraints from the instance context
              -- e.g., {Eq a} [a] -> [a] -> Bool for instance {Eq a} Eq [a]
              typeScheme = Types.Forall freeVars constraints substitutedType
          in
            extendEnv (stringToVar generatedMethodName) typeScheme env
    
    -- Substitute type variable with concrete type in a type expression
    substituteTypeVar :: TyVar -> Type -> Type -> Type
    substituteTypeVar oldVar newType = go
      where
        go TInt = TInt
        go TMathValue = TMathValue
        go TPolyExpr = TPolyExpr
        go TTermExpr = TTermExpr
        go TSymbolExpr = TSymbolExpr
        go TIndexExpr = TIndexExpr
        go TFloat = TFloat
        go TBool = TBool
        go TChar = TChar
        go TString = TString
        go (TVar v) | v == oldVar = newType
                    | otherwise = TVar v
        go (TTuple ts) = TTuple (map go ts)
        go (TCollection t) = TCollection (go t)
        go (TInductive name ts) = TInductive name (map go ts)
        go (TTensor t) = TTensor (go t)
        go (THash k v) = THash (go k) (go v)
        go (TMatcher t) = TMatcher (go t)
        go (TFun t1 t2) = TFun (go t1) (go t2)
        go (TIO t) = TIO (go t)
        go (TIORef t) = TIORef (go t)
        go TPort = TPort
        go TAny = TAny
        go TFactor = TFactor
        go (TTerm t ss) = TTerm (go t) ss
        go (TFrac t) = TFrac (go t)
        go (TPoly t ss) = TPoly (go t) ss

-- | Extract method name and type from ClassMethod
extractMethodWithType :: ClassMethod -> (String, Type)
extractMethodWithType (ClassMethod name params retType _) =
  let paramTypes = map typedParamToType params
      methodType = foldr TFun (typeExprToType retType) paramTypes
  in (name, methodType)

-- | Extract class name from ConstraintExpr
extractConstraintName :: ConstraintExpr -> String
extractConstraintName (ConstraintExpr clsName _) = clsName

-- | Convert ConstraintExpr to internal Constraint.
-- Multi-param classes (e.g. `Coerce a b`) carry all class type parameters.
constraintToInternal :: ConstraintExpr -> Types.Constraint
constraintToInternal (ConstraintExpr clsName tyExprs) =
  Types.Constraint clsName (case tyExprs of
    [] -> [TAny]
    _  -> map typeExprToType tyExprs)

-- | Register a single pattern constructor
registerPatternConstructor :: String -> [String] -> Type 
                           -> PatternConstructorEnv -> PatternConstructor 
                           -> EvalM PatternConstructorEnv
registerPatternConstructor _typeName typeParams resultType patternCtorEnv 
                          (PatternConstructor ctorName argTypeExprs) = do
  let argTypes = map typeExprToType argTypeExprs
      
      -- Pattern constructor type: arg1 -> arg2 -> ... -> resultType (without Pattern wrapper)
      patternCtorType = foldr TFun resultType argTypes
      
      -- Quantify over type parameters
      tyVars = map TyVar typeParams
      typeScheme = Types.Forall tyVars [] patternCtorType
      
      -- Add to pattern constructor environment (same format as PatternTypeEnv)
      patternCtorEnv' = extendPatternEnv ctorName typeScheme patternCtorEnv
  
  return patternCtorEnv'

-- | Convert TypedParam to Type
typedParamToType :: TypedParam -> Type
typedParamToType (TPVar _ ty) = typeExprToType ty
typedParamToType (TPInvertedVar _ ty) = typeExprToType ty
typedParamToType (TPTuple elems) = TTuple (map typedParamToType elems)
typedParamToType (TPWildcard ty) = typeExprToType ty
typedParamToType (TPUntypedVar _) = TVar (TyVar "a")  -- Will be inferred
typedParamToType TPUntypedWildcard = TVar (TyVar "a")  -- Will be inferred

