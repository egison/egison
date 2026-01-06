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
import qualified Data.HashMap.Strict        as HashMap

import           Language.Egison.AST
import           Language.Egison.Data       (EvalM)
import           Language.Egison.EvalState  (ConstructorInfo(..), ConstructorEnv, PatternConstructorEnv)
import           Language.Egison.Type.Env   (TypeEnv, ClassEnv, PatternTypeEnv, emptyEnv, emptyClassEnv, emptyPatternEnv,
                                             extendEnv, extendPatternEnv, addClass, addInstance)
import qualified Language.Egison.Type.Types as Types
import           Language.Egison.Type.Types (Type(..), TyVar(..), Constraint(..), TypeScheme(..), TensorShape(..),
                                             ClassInfo, InstanceInfo, freeTyVars)
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
  InstanceDeclExpr (InstanceDecl context className instTypes _methods) -> do
    let classEnv = ebrClassEnv result
        
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
    
    return result { ebrClassEnv = classEnv' }
  
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
        
        -- Generalize free type variables in the type signature
        -- This handles type parameters like {a, b, c} in def compose {a, b, c} ...
        freeVars = Set.toList (freeTyVars funType)
        typeScheme = Types.Forall freeVars [] funType
        
        typeEnv = ebrTypeEnv result
        typeEnv' = extendEnv name typeScheme typeEnv
    
    return result { ebrTypeEnv = typeEnv' }
  
  -- 5. Pattern Inductive Declarations (from PatternInductiveDecl)
  PatternInductiveDecl typeName typeParams constructors -> do
    let typeParamVars = map (TVar . TyVar) typeParams
        patternType = TInductive typeName typeParamVars
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

-- | Convert TypeExpr to Type
typeExprToType :: TypeExpr -> Type
typeExprToType TEInt = TInt
typeExprToType TEMathExpr = TInt  -- MathExpr = Integer in Egison
typeExprToType TEFloat = TFloat
typeExprToType TEBool = TBool
typeExprToType TEChar = TChar
typeExprToType TEString = TString
typeExprToType (TEVar name) = TVar (TyVar name)
typeExprToType (TETuple ts) = TTuple (map typeExprToType ts)
typeExprToType (TEList t) = TCollection (typeExprToType t)
typeExprToType (TEApp t1 ts) = 
  case typeExprToType t1 of
    TVar (TyVar name) -> TInductive name (map typeExprToType ts)  -- Type application: MyList a
    TInductive name existingTs -> TInductive name (existingTs ++ map typeExprToType ts)
    baseType -> baseType  -- Can't apply to non-inductive types
typeExprToType (TETensor elemT _ _) = TTensor (typeExprToType elemT)
typeExprToType (TEMatcher t) = TMatcher (typeExprToType t)
typeExprToType (TEFun t1 t2) = TFun (typeExprToType t1) (typeExprToType t2)
typeExprToType (TEIO t) = TIO (typeExprToType t)
typeExprToType (TEConstrained _ t) = typeExprToType t  -- Ignore constraints

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

