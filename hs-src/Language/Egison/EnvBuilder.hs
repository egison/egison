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
import           Control.Monad.State
import           Data.Char                  (toUpper, toLower)
import qualified Data.HashMap.Strict        as HashMap

import           Language.Egison.AST
import           Language.Egison.Data       (EvalM)
import           Language.Egison.EvalState  (ConstructorInfo(..), ConstructorEnv, PatternConstructorEnv)
import           Language.Egison.Type.Env   (TypeEnv, ClassEnv, PatternTypeEnv, emptyEnv, emptyClassEnv, emptyPatternEnv,
                                             extendEnv, extendPatternEnv, addClass, addInstance, lookupClass)
import qualified Language.Egison.Type.Types as Types
import           Language.Egison.Type.Types (Type(..), TyVar(..), Constraint(..), TypeScheme(..), TensorShape(..),
                                             ClassInfo, InstanceInfo, freeTyVars, typeToName, sanitizeMethodName, typeExprToType,
                                             capitalizeFirst, lowerFirst)
import qualified Data.Set as Set

-- | Result of environment building phase
data EnvBuildResult = EnvBuildResult
  { ebrTypeEnv        :: TypeEnv         -- ^ Type signatures for definitions
  , ebrClassEnv       :: ClassEnv        -- ^ Type class and instance information
  , ebrConstructorEnv :: ConstructorEnv  -- ^ Data constructor information
  , ebrPatternConstructorEnv :: PatternConstructorEnv  -- ^ Pattern constructor information
  , ebrPatternTypeEnv :: PatternTypeEnv  -- ^ Pattern function information
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
  
  -- 2. Type Class Definitions (from ClassDeclExpr)
  ClassDeclExpr (ClassDecl className [typeParam] superClasses methods) -> do
    let classEnv = ebrClassEnv result
        typeEnv = ebrTypeEnv result
        tyVar = TyVar typeParam
        
        -- Extract superclass names from ConstraintExprs
        superNames = map extractConstraintName superClasses
        
        -- Build method list with types
        methodsWithTypes = map extractMethodWithType methods
        
        -- Create ClassInfo
        -- Note: Use qualified name to avoid ambiguity with ClassDecl.classMethods
        classInfo = Types.ClassInfo
          { Types.classSupers = superNames
          , Types.classParam = tyVar
          , Types.classMethods = methodsWithTypes
          }
        
        -- Register class
        classEnv' = addClass className classInfo classEnv
        
        -- Register each class method to type environment
        typeEnv' = foldl (registerClassMethod tyVar className) typeEnv methods
    
    return result { ebrClassEnv = classEnv', ebrTypeEnv = typeEnv' }
  
  ClassDeclExpr _ -> 
    -- Unsupported class declaration format (multiple type parameters, etc.)
    return result
  
  -- 3. Instance Definitions (from InstanceDeclExpr)
  InstanceDeclExpr (InstanceDecl context className instTypes methods) -> do
    let classEnv = ebrClassEnv result
        typeEnv = ebrTypeEnv result
        
        -- Get the main instance type
        mainInstType = case instTypes of
          []    -> TAny
          (t:_) -> typeExprToType t
        
        -- Create InstanceInfo
        instInfo = Types.InstanceInfo
          { Types.instContext = map constraintToInternal context
          , Types.instClass = className
          , Types.instType = mainInstType
          , Types.instMethods = []  -- Methods are handled during desugaring/evaluation
          }
        
        -- Register instance
        classEnv' = addInstance className instInfo classEnv
        
        -- Register method type signatures for generated methods
        -- This prevents "Unbound variable" warnings during type inference
        typeEnv' = registerInstanceMethods className mainInstType methods classEnv' typeEnv
    
    return result { ebrClassEnv = classEnv', ebrTypeEnv = typeEnv' }
  
  -- 4. Type Signature Collection (from Define, DefineWithType)
  -- Note: We only collect explicit type signatures here.
  -- Inferred types will be added during type inference.
  DefineWithType typedVar _expr -> do
    let name = typedVarName typedVar
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
        typeEnv' = extendEnv name typeScheme typeEnv
    
    return result { ebrTypeEnv = typeEnv' }
  
  -- 5. Pattern Inductive Declarations (from PatternInductiveDecl)
  PatternInductiveDecl typeName typeParams constructors -> do
    let typeParamVars = map (TVar . TyVar) typeParams
        -- Special case: [a] should be treated as TCollection, not TInductive "[]"
        patternType = case (typeName == "[]", typeParams) of
                        (True, [param]) -> TCollection (TVar (TyVar param))
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
  Load {} -> return result      -- Should not appear after expandLoads
  InfixDecl {} -> return result

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
      typeEnv' = extendEnv ctorName typeScheme typeEnv
      
      -- Add to constructor environment (for pattern matching and evaluation)
      ctorInfo = ConstructorInfo
        { ctorTypeName = typeName
        , ctorArgTypes = argTypes
        , ctorTypeParams = typeParams
        }
      ctorEnv' = HashMap.insert ctorName ctorInfo ctorEnv
  
  return (typeEnv', ctorEnv')

-- | Register a class method to the type environment
registerClassMethod :: TyVar -> String -> TypeEnv -> ClassMethod -> TypeEnv
registerClassMethod tyVar className typeEnv (ClassMethod methName params retType _defaultImpl) =
  let paramTypes = map typedParamToType params
      methodType = foldr TFun (typeExprToType retType) paramTypes
      
      -- Method has constrained type: ClassName a => methodType
      constraint = Types.Constraint className (TVar tyVar)
      typeScheme = Types.Forall [tyVar] [constraint] methodType
  in
    extendEnv methName typeScheme typeEnv

-- | Register type signatures for instance methods (generated during desugaring)
-- This prevents "Unbound variable" warnings during type inference
registerInstanceMethods :: String -> Type -> [InstanceMethod] -> ClassEnv -> TypeEnv -> TypeEnv
registerInstanceMethods className instType methods classEnv typeEnv =
  case lookupClass className classEnv of
    Nothing -> typeEnv  -- Class not found, skip
    Just classInfo -> 
      -- Register each instance method
      foldr (registerInstanceMethod className instType classInfo) typeEnv methods
  where
    registerInstanceMethod :: String -> Type -> Types.ClassInfo -> InstanceMethod -> TypeEnv -> TypeEnv
    registerInstanceMethod clsName instTy classInfo (InstanceMethod methName _params _body) env =
      -- Find the method in the class definition
      case lookup methName (Types.classMethods classInfo) of
        Nothing -> env  -- Method not in class definition, skip
        Just methodType -> 
          -- Substitute type variable with instance type
          let tyVar = Types.classParam classInfo
              substitutedType = substituteTypeVar tyVar instTy methodType
              
              -- Generate method name: e.g., "eqIntegerEq" for (==) in Eq Integer
              typeName' = typeToName instTy
              sanitizedName = sanitizeMethodName methName
              generatedMethodName = lowerFirst clsName ++ typeName' ++ capitalizeFirst sanitizedName
              
              -- No type variables or constraints in the concrete instance method
              typeScheme = Types.Forall [] [] substitutedType
          in
            extendEnv generatedMethodName typeScheme env
    
    -- Substitute type variable with concrete type in a type expression
    substituteTypeVar :: TyVar -> Type -> Type -> Type
    substituteTypeVar oldVar newType = go
      where
        go TInt = TInt
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
        go TAny = TAny

-- | Extract method name from ClassMethod
extractMethodName :: ClassMethod -> String
extractMethodName (ClassMethod name _ _ _) = name

-- | Extract method name and type from ClassMethod
extractMethodWithType :: ClassMethod -> (String, Type)
extractMethodWithType (ClassMethod name params retType _) =
  let paramTypes = map typedParamToType params
      methodType = foldr TFun (typeExprToType retType) paramTypes
  in (name, methodType)

-- | Extract class name from ConstraintExpr
extractConstraintName :: ConstraintExpr -> String
extractConstraintName (ConstraintExpr clsName _) = clsName

-- | Convert ConstraintExpr to internal Constraint
constraintToInternal :: ConstraintExpr -> Types.Constraint
constraintToInternal (ConstraintExpr clsName tyExprs) =
  Types.Constraint clsName (case tyExprs of 
    [] -> TAny
    (t:_) -> typeExprToType t)

-- | Register a single pattern constructor
registerPatternConstructor :: String -> [String] -> Type 
                           -> PatternConstructorEnv -> PatternConstructor 
                           -> EvalM PatternConstructorEnv
registerPatternConstructor typeName typeParams resultType patternCtorEnv 
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

