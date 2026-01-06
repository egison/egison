{- |
Module      : Language.Egison.Type.TypedDesugar
Licence     : MIT

This module implements Phase 8 of the processing flow: TypedDesugar.
It performs type-driven transformations from typed AST (TypedExpr) to 
typed internal expressions (TIExpr).

Type-Driven Transformations (Phase 8):
  1. Type class dictionary passing
     - Instance selection based on types
     - Method call concretization
  2. tensorMap automatic insertion
     - Detect mismatches between Tensor MathExpr and MathExpr
     - Insert tensorMap at appropriate positions
  3. Type information optimization and embedding
     - Preserve type info for better error messages during evaluation
     - Each node in TIExpr contains its type

Type information is preserved throughout desugaring, enabling:
  - Better runtime error messages with type information
  - Type-based dispatch during evaluation
  - Debugging support with type annotations
-}

module Language.Egison.Type.TypedDesugar
  ( desugarTypedExpr
  , desugarTypedTopExpr
  -- Typed versions that preserve type information
  , desugarTypedExprT
  , desugarTypedTopExprT
  ) where

import           Control.Monad              (forM)
import           Data.Char                  (toUpper, toLower)
import           Data.Text                  (Text, pack)

import           Language.Egison.AST        (Arg (..), ConstantExpr (..), IndexExpr (..),
                                             Op (..), PMMode (..),
                                             PrimitiveDataPattern (..),
                                             PDPatternBase (..), PrimitivePatPattern (..))
import           Language.Egison.Data       (EvalM)
import           Language.Egison.IExpr
import           Language.Egison.Type.TypedIAST
import           Language.Egison.Type.Types (Type(..), TyVar(..))

-- | Desugar a typed expression to internal expression
desugarTypedExpr :: TypedExpr -> EvalM IExpr
desugarTypedExpr (TypedExpr _ty node) = case node of
  
  -- Constants
  TConstantExpr c -> return $ IConstantExpr c
  
  -- Variables
  TVarExpr name -> return $ IVarExpr name
  
  -- Indexed expressions
  TIndexedExpr override base indices -> do
    baseExpr <- desugarTypedExpr base
    indicesExpr <- mapM desugarIndex indices
    return $ IIndexedExpr override baseExpr indicesExpr
  
  -- Tuples
  TTupleExpr es -> do
    desugared <- mapM desugarTypedExpr es
    return $ ITupleExpr desugared
  
  -- Collections
  TCollectionExpr es -> do
    desugared <- mapM desugarTypedExpr es
    return $ ICollectionExpr desugared
  
  -- Hash
  THashExpr pairs -> do
    desugaredPairs <- forM pairs $ \(k, v) -> do
      dk <- desugarTypedExpr k
      dv <- desugarTypedExpr v
      return (dk, dv)
    return $ IHashExpr desugaredPairs
  
  -- Cons/Join
  TConsExpr h t -> do
    hExpr <- desugarTypedExpr h
    tExpr <- desugarTypedExpr t
    return $ IConsExpr hExpr tExpr
  
  TJoinExpr l r -> do
    lExpr <- desugarTypedExpr l
    rExpr <- desugarTypedExpr r
    return $ IJoinExpr lExpr rExpr
  
  -- Lambda
  -- Handle Arg/InvertedArg properly:
  -- InvertedArg => flip indices
  -- Arg => use directly (type determines tensor/scalar)
  -- This matches the behavior in Desugar.hs for LambdaExpr'
  TLambdaExpr argParams body -> do
    bodyExpr <- desugarTypedExpr body
    -- Apply scalar argument transformation in reverse order (foldr)
    -- For InvertedArg: flip indices (type determines tensor/scalar behavior)
    let (vars, bodyExpr') = foldr desugarArg ([], bodyExpr) argParams
    return $ ILambdaExpr Nothing vars bodyExpr'
    where
      desugarArg :: Arg String -> ([Var], IExpr) -> ([Var], IExpr)
      desugarArg (Arg x) (vars, expr) = 
        (stringToVar x : vars, expr)
      desugarArg (InvertedArg x) (vars, expr) = 
        -- For inverted arg: flip indices
        (stringToVar x : vars, IFlipIndicesExpr (IVarExpr x))
  
  -- Typed Lambda (type info erased at runtime)
  TTypedLambdaExpr params _retType body -> do
    bodyExpr <- desugarTypedExpr body
    let paramNames = map fst params
    return $ ILambdaExpr Nothing (map stringToVar paramNames) bodyExpr
  
  -- Memoized Lambda
  TMemoizedLambdaExpr params body -> do
    bodyExpr <- desugarTypedExpr body
    return $ IMemoizedLambdaExpr params bodyExpr
  
  -- Function application
  TApplyExpr func args -> do
    funcExpr <- desugarTypedExpr func
    argsExpr <- mapM desugarTypedExpr args
    return $ IApplyExpr funcExpr argsExpr
  
  -- Curried apply
  TCApplyExpr f a -> do
    fExpr <- desugarTypedExpr f
    aExpr <- desugarTypedExpr a
    return $ ICApplyExpr fExpr aExpr
  
  -- If expression
  TIfExpr cond thenE elseE -> do
    condExpr <- desugarTypedExpr cond
    thenExpr <- desugarTypedExpr thenE
    elseExpr <- desugarTypedExpr elseE
    return $ IIfExpr condExpr thenExpr elseExpr
  
  -- Let expression
  TLetExpr bindings body -> do
    bindingsExpr <- mapM desugarBinding bindings
    bodyExpr <- desugarTypedExpr body
    return $ ILetExpr bindingsExpr bodyExpr
  
  -- Let rec expression
  TLetRecExpr bindings body -> do
    bindingsExpr <- mapM desugarBinding bindings
    bodyExpr <- desugarTypedExpr body
    return $ ILetRecExpr bindingsExpr bodyExpr
  
  -- Match expressions
  TMatchExpr mode target matcher clauses -> do
    targetExpr <- desugarTypedExpr target
    matcherExpr <- desugarTypedExpr matcher
    clausesExpr <- mapM desugarTypedClause clauses
    return $ IMatchExpr mode targetExpr matcherExpr clausesExpr
  
  TMatchAllExpr mode target matcher clauses -> do
    targetExpr <- desugarTypedExpr target
    matcherExpr <- desugarTypedExpr matcher
    clausesExpr <- mapM desugarTypedClause clauses
    return $ IMatchAllExpr mode targetExpr matcherExpr clausesExpr
  
  -- Match lambda
  TMatchLambdaExpr matcher clauses -> do
    matcherExpr <- desugarTypedExpr matcher
    clausesExpr <- mapM desugarTypedClause clauses
    let paramVar = stringToVar "x"
    return $ ILambdaExpr Nothing [paramVar]
           $ IMatchExpr BFSMode (IVarExpr "x") matcherExpr clausesExpr
  
  TMatchAllLambdaExpr matcher clauses -> do
    matcherExpr <- desugarTypedExpr matcher
    clausesExpr <- mapM desugarTypedClause clauses
    let paramVar = stringToVar "x"
    return $ ILambdaExpr Nothing [paramVar]
           $ IMatchAllExpr BFSMode (IVarExpr "x") matcherExpr clausesExpr
  
  -- Type class method call - THIS IS THE KEY FOR INSTANCE RESOLUTION
  -- The dictionary has already been resolved during type inference
  TClassMethodExpr _className methodName dict args -> do
    dictExpr <- desugarTypedExpr dict
    argsExpr <- mapM desugarTypedExpr args
    -- Access method from dictionary: dict_"methodName"
    let dictAccess = IIndexedExpr False dictExpr [Sub (IConstantExpr (StringExpr (pack methodName)))]
    return $ IApplyExpr dictAccess argsExpr
  
  -- Matcher expressions
  TMatcherExpr patDefs -> do
    desugaredDefs <- forM patDefs $ \(patPat, nextMatcher, clauses) -> do
      nextMatcherExpr <- desugarTypedExpr nextMatcher
      clausesExpr <- forM clauses $ \(dataPat, clauseBody) -> do
        bodyExpr <- desugarTypedExpr clauseBody
        return (convertPDPattern dataPat, bodyExpr)
      return (patPat, nextMatcherExpr, clausesExpr)
    return $ IMatcherExpr desugaredDefs
  
  TAlgebraicDataMatcherExpr constructors -> do
    desugaredConstrs <- forM constructors $ \(name, argExprs) -> do
      argsExpr <- mapM desugarTypedExpr argExprs
      return (name, argsExpr)
    return $ algebraicDataMatcherToIExpr desugaredConstrs
  
  -- Tensor operations
  TGenerateTensorExpr gen shape -> do
    genExpr <- desugarTypedExpr gen
    shapeExpr <- desugarTypedExpr shape
    return $ IGenerateTensorExpr genExpr shapeExpr
  
  TTensorExpr dataE shapeE -> do
    dataExpr <- desugarTypedExpr dataE
    shapeExpr <- desugarTypedExpr shapeE
    return $ ITensorExpr dataExpr shapeExpr
  
  TTensorContractExpr e -> do
    expr <- desugarTypedExpr e
    return $ ITensorContractExpr expr
  
  TTensorMapExpr f t -> do
    fExpr <- desugarTypedExpr f
    tExpr <- desugarTypedExpr t
    return $ ITensorMapExpr fExpr tExpr
  
  TTensorMap2Expr f t1 t2 -> do
    fExpr <- desugarTypedExpr f
    t1Expr <- desugarTypedExpr t1
    t2Expr <- desugarTypedExpr t2
    return $ ITensorMap2Expr fExpr t1Expr t2Expr
  
  TTransposeExpr indices tensor -> do
    indicesExpr <- desugarTypedExpr indices
    tensorExpr <- desugarTypedExpr tensor
    return $ ITransposeExpr indicesExpr tensorExpr
  
  -- Quote
  TQuoteExpr e -> do
    expr <- desugarTypedExpr e
    return $ IQuoteExpr expr
  
  TQuoteSymbolExpr e -> do
    expr <- desugarTypedExpr e
    return $ IQuoteSymbolExpr expr
  
  TWithSymbolsExpr syms body -> do
    bodyExpr <- desugarTypedExpr body
    return $ IWithSymbolsExpr syms bodyExpr
  
  TFunctionExpr args -> do
    return $ IFunctionExpr args
  
  -- IO
  TIOExpr e -> do
    desugarTypedExpr e  -- IO expressions are evaluated directly
  
  TSeqExpr e1 e2 -> do
    e1Expr <- desugarTypedExpr e1
    e2Expr <- desugarTypedExpr e2
    return $ ISeqExpr e1Expr e2Expr
  
  -- Do expression
  TDoExpr bindings body -> do
    bindingsExpr <- mapM desugarBinding bindings
    bodyExpr <- desugarTypedExpr body
    return $ IDoExpr bindingsExpr bodyExpr
  
  -- Pattern function
  TPatternFunctionExpr params body -> do
    bodyPat <- desugarTypedPattern body
    return $ IPatternFunctionExpr params bodyPat
  
  -- Section
  TSectionExpr op mLeft mRight -> do
    mLeftExpr <- traverse desugarTypedExpr mLeft
    mRightExpr <- traverse desugarTypedExpr mRight
    case (mLeftExpr, mRightExpr) of
      (Nothing, Nothing) -> 
        -- Both sides missing: create a binary function
        return $ ILambdaExpr Nothing [stringToVar "x", stringToVar "y"]
               $ IApplyExpr (IVarExpr (repr op)) [IVarExpr "x", IVarExpr "y"]
      (Just l, Nothing) ->
        -- Left section: (x op) y = op x y
        return $ ILambdaExpr Nothing [stringToVar "y"]
               $ IApplyExpr (IVarExpr (repr op)) [l, IVarExpr "y"]
      (Nothing, Just r) ->
        -- Right section: (op y) x = op x y
        return $ ILambdaExpr Nothing [stringToVar "x"]
               $ IApplyExpr (IVarExpr (repr op)) [IVarExpr "x", r]
      (Just l, Just r) ->
        -- Both sides: just apply
        return $ IApplyExpr (IVarExpr (repr op)) [l, r]
  
  -- Anonymous parameters
  TAnonParamFuncExpr arity body -> do
    bodyExpr <- desugarTypedExpr body
    let params = [stringToVar ("%" ++ show i) | i <- [1..arity]]
    return $ ILambdaExpr Nothing params bodyExpr
  
  TAnonParamExpr n -> do
    return $ IVarExpr ("%" ++ show n)
  
  -- Fresh variable - generate a unique variable
  TFreshVarExpr -> return $ IVarExpr "#fresh"
  
  -- Prefix operator
  TPrefixExpr op e -> do
    expr <- desugarTypedExpr e
    return $ IApplyExpr (IVarExpr op) [expr]

-- | Helper to convert algebraic data matcher constructors to IExpr
-- This creates the appropriate matcher expression
algebraicDataMatcherToIExpr :: [(String, [IExpr])] -> IExpr
algebraicDataMatcherToIExpr constructors =
  -- Create a matcher expression for algebraic data types
  -- Each constructor becomes an inductive pattern definition
  let patDefs = map mkPatternDef constructors
  in IMatcherExpr patDefs
  where
    mkPatternDef :: (String, [IExpr]) -> IPatternDef
    mkPatternDef (name, argMatchers) =
      let numArgs = length argMatchers
          -- Pattern: constructor name with argument patterns
          patPat = PPInductivePat name (replicate numArgs PPWildCard)
          -- Next matchers for each argument position
          nextMatcher = if null argMatchers 
                        then IConstantExpr (BoolExpr True)  -- something matcher
                        else ITupleExpr argMatchers
          -- Data pattern clause: matches the constructor
          -- Need to use Var type (IPrimitiveDataPattern = PDPatternBase Var)
          varNames = map (\i -> stringToVar ("$" ++ show i)) [1..numArgs]
          clause :: (IPrimitiveDataPattern, IExpr)
          clause = (PDInductivePat name (map PDPatVar varNames),
                    if numArgs == 0
                    then ICollectionExpr [ITupleExpr []]
                    else ICollectionExpr [ITupleExpr (map (\i -> IVarExpr ("$" ++ show i)) [1..numArgs])])
      in (patPat, nextMatcher, [clause])

-- | Desugar index expression
desugarIndex :: IndexExpr TypedExpr -> EvalM (Index IExpr)
desugarIndex idx = case idx of
  Subscript e -> Sub <$> desugarTypedExpr e
  Superscript e -> Sup <$> desugarTypedExpr e
  SupSubscript e -> SupSub <$> desugarTypedExpr e
  Userscript e -> User <$> desugarTypedExpr e
  -- MultiSubscript and MultiSuperscript require more complex handling
  -- For now, just use the first expression as a subscript
  MultiSubscript e1 _e2 -> Sub <$> desugarTypedExpr e1
  MultiSuperscript e1 _e2 -> Sup <$> desugarTypedExpr e1

-- | Desugar typed binding (returns IBindingExpr which is (IPrimitiveDataPattern, IExpr))
desugarBinding :: TypedBinding -> EvalM IBindingExpr
desugarBinding (TBind pat e) = do
  expr <- desugarTypedExpr e
  let ipdpat = convertPDPattern pat
  return (ipdpat, expr)
desugarBinding (TBindWithType name _ty e) = do
  expr <- desugarTypedExpr e
  return (PDPatVar (stringToVar name), expr)

-- | Convert PrimitiveDataPattern to IPrimitiveDataPattern
convertPDPattern :: PrimitiveDataPattern -> IPrimitiveDataPattern
convertPDPattern (PDPatVar name) = PDPatVar (stringToVar name)
convertPDPattern PDWildCard = PDWildCard
convertPDPattern (PDInductivePat name ps) = PDInductivePat name (map convertPDPattern ps)
convertPDPattern (PDTuplePat ps) = PDTuplePat (map convertPDPattern ps)
convertPDPattern PDEmptyPat = PDEmptyPat
convertPDPattern (PDConsPat p1 p2) = PDConsPat (convertPDPattern p1) (convertPDPattern p2)
convertPDPattern (PDSnocPat p1 p2) = PDSnocPat (convertPDPattern p1) (convertPDPattern p2)
convertPDPattern (PDConstantPat c) = PDConstantPat c

-- | Convert pattern to var (simplified)
patternToVar :: PrimitiveDataPattern -> Var
patternToVar (PDPatVar name) = stringToVar name
patternToVar _ = stringToVar "_"

-- | Desugar typed match clause
desugarTypedClause :: TypedMatchClause -> EvalM (IPattern, IExpr)
desugarTypedClause (pat, body) = do
  patExpr <- desugarTypedPattern pat
  bodyExpr <- desugarTypedExpr body
  return (patExpr, bodyExpr)

-- | Desugar typed pattern
desugarTypedPattern :: TypedPattern -> EvalM IPattern
desugarTypedPattern (TypedPattern _ty node) = case node of
  TPWildCard -> return IWildCard
  TPPatVar name -> return $ IPatVar name
  TPValuePat e -> IValuePat <$> desugarTypedExpr e
  TPPredPat e -> IPredPat <$> desugarTypedExpr e
  TPIndexedPat p idxs -> do
    pExpr <- desugarTypedPattern p
    idxsExpr <- mapM desugarTypedExpr idxs
    return $ IIndexedPat pExpr idxsExpr
  TPNotPat p -> INotPat <$> desugarTypedPattern p
  TPAndPat p1 p2 -> do
    p1Expr <- desugarTypedPattern p1
    p2Expr <- desugarTypedPattern p2
    return $ IAndPat p1Expr p2Expr
  TPOrPat p1 p2 -> do
    p1Expr <- desugarTypedPattern p1
    p2Expr <- desugarTypedPattern p2
    return $ IOrPat p1Expr p2Expr
  TPForallPat p1 p2 -> do
    p1Expr <- desugarTypedPattern p1
    p2Expr <- desugarTypedPattern p2
    return $ IForallPat p1Expr p2Expr
  TPTuplePat ps -> do
    psExpr <- mapM desugarTypedPattern ps
    return $ ITuplePat psExpr
  TPInductivePat name ps -> do
    psExpr <- mapM desugarTypedPattern ps
    return $ IInductivePat name psExpr
  TPInfixPat op p1 p2 -> do
    p1Expr <- desugarTypedPattern p1
    p2Expr <- desugarTypedPattern p2
    return $ IInductivePat (repr op) [p1Expr, p2Expr]
  TPLoopPat _ _ _ _ -> return IWildCard  -- Simplified
  TPContPat -> return IContPat
  TPPApplyPat e ps -> do
    eExpr <- desugarTypedExpr e
    psExpr <- mapM desugarTypedPattern ps
    return $ IPApplyPat eExpr psExpr
  TPVarPat name -> return $ IVarPat name
  TPSeqNilPat -> return ISeqNilPat
  TPSeqConsPat p1 p2 -> do
    p1Expr <- desugarTypedPattern p1
    p2Expr <- desugarTypedPattern p2
    return $ ISeqConsPat p1Expr p2Expr
  TPLaterPatVar -> return ILaterPatVar
  TPDApplyPat p ps -> do
    pExpr <- desugarTypedPattern p
    psExpr <- mapM desugarTypedPattern ps
    return $ IDApplyPat pExpr psExpr

-- | Desugar typed top-level expression
desugarTypedTopExpr :: TypedTopExpr -> EvalM (Maybe ITopExpr)
desugarTypedTopExpr texpr = case texpr of
  TDefine name body -> do
    bodyExpr <- desugarTypedExpr body
    return $ Just $ IDefine (stringToVar name) bodyExpr
  
  TDefineWithType name _constraints params _retType body -> do
    bodyExpr <- desugarTypedExpr body
    let paramNames = map fst params
        lambdaExpr = if null paramNames
                     then bodyExpr
                     else ILambdaExpr Nothing (map stringToVar paramNames) bodyExpr
    return $ Just $ IDefine (stringToVar name) lambdaExpr
  
  TTest body -> do
    bodyExpr <- desugarTypedExpr body
    return $ Just $ ITest bodyExpr
  
  TExecute body -> do
    bodyExpr <- desugarTypedExpr body
    return $ Just $ IExecute bodyExpr
  
  TLoadFile path -> return $ Just $ ILoadFile path
  TLoad lib -> return $ Just $ ILoad lib
  
  TInductiveDecl _typeName _typeParams _constructors -> 
    -- Inductive declarations don't produce runtime code
    return Nothing
  
  -- Type class declarations: generate dictionary-passing wrapper functions
  -- For a class like:
  --   class Eq a where
  --     (==) (x: a) (y: a) : Bool
  -- We generate:
  --   1. Dictionary wrapper: def classEqEq dict x y := (dict_"eq") x y
  --   2. Instance registry variable: def registryEq := {| |}
  TClassDecl className _typeParams _supers methods -> do
    -- Generate dictionary-passing wrapper functions for each method
    let methodWrappers = map (desugarClassMethod className) methods
        registryDef = makeRegistryDef className
    case methodWrappers of
      [] -> return Nothing
      _  -> return $ Just $ IDefineMany (registryDef : methodWrappers)
    where
      desugarClassMethod :: String -> (String, [(String, Type)], Type) -> (Var, IExpr)
      desugarClassMethod clsNm (methName, methParams, _retType) =
        let wrapperName = "class" ++ clsNm ++ capitalizeFirst (sanitizeMethodName methName)
            var = stringToVar wrapperName
            dictVar = "dict"
            paramNames = map fst methParams
            allParams = dictVar : paramNames
            -- Build the body: (dict_"methodName") x y ...
            dictAccessExpr = IIndexedExpr False (IVarExpr dictVar) 
                               [Sub (IConstantExpr (StringExpr (pack (sanitizeMethodName methName))))]
            bodyExpr = if null paramNames
                       then dictAccessExpr
                       else IApplyExpr dictAccessExpr (map IVarExpr paramNames)
            lambdaExpr = ILambdaExpr Nothing (map stringToVar allParams) bodyExpr
        in (var, lambdaExpr)
      
      makeRegistryDef :: String -> (Var, IExpr)
      makeRegistryDef clsNm = 
        let registryName = "registry" ++ clsNm
            var = stringToVar registryName
        in (var, IHashExpr [])
  
  -- Instance declarations: generate a dictionary and individual method definitions
  -- For an instance like:
  --   instance Eq Integer where
  --     (==) x y := x = y
  --     (/=) x y := not (x = y)
  -- We generate:
  --   1. Individual method functions: def eqIntegerEq x y := x = y
  --   2. A dictionary for the instance: def eqInteger := {| ("eq", eqIntegerEq), ... |}
  TInstanceDecl _context className instTypes methods -> do
    if null instTypes
      then return Nothing
      else do
        let instTypeName = typeToName (head instTypes)
        -- Generate individual method definitions
        methodDefs <- forM methods $ \(methName, params, body) -> do
          bodyExpr <- desugarTypedExpr body
          let funcName = lowerFirst className ++ instTypeName ++ capitalizeFirst (sanitizeMethodName methName)
              lambdaExpr = if null params
                           then bodyExpr
                           else ILambdaExpr Nothing (map stringToVar params) bodyExpr
          return (stringToVar funcName, lambdaExpr)
        -- Generate dictionary definition
        let dictDef = makeDictDef className instTypeName methods
        case methodDefs of
          [] -> return Nothing
          _  -> return $ Just $ IDefineMany (dictDef : methodDefs)
    where
      makeDictDef :: String -> String -> [(String, [String], TypedExpr)] -> (Var, IExpr)
      makeDictDef clsNm typNm meths =
        let dictName = lowerFirst clsNm ++ typNm
            dictVar = stringToVar dictName
            hashEntries = map (makeHashEntry clsNm typNm) meths
            hashExpr = IHashExpr hashEntries
        in (dictVar, hashExpr)
      
      makeHashEntry :: String -> String -> (String, [String], TypedExpr) -> (IExpr, IExpr)
      makeHashEntry clsNm typNm (methName, _, _) =
        let keyExpr = IConstantExpr (StringExpr (pack (sanitizeMethodName methName)))
            funcName = lowerFirst clsNm ++ typNm ++ capitalizeFirst (sanitizeMethodName methName)
            valueExpr = IVarExpr funcName
        in (keyExpr, valueExpr)

-- | Convert Type to name for instance generation
typeToName :: Type -> String
typeToName TInt = "Integer"
typeToName TFloat = "Float"
typeToName TBool = "Bool"
typeToName TChar = "Char"
typeToName TString = "String"
typeToName (TCollection _) = "Collection"
typeToName (TVar (TyVar n)) = n
typeToName _ = "Unknown"

-- | Sanitize method name for use as identifier
sanitizeMethodName :: String -> String
sanitizeMethodName "==" = "eq"
sanitizeMethodName "/=" = "neq"
sanitizeMethodName "<"  = "lt"
sanitizeMethodName "<=" = "le"
sanitizeMethodName ">"  = "gt"
sanitizeMethodName ">=" = "ge"
sanitizeMethodName "+"  = "plus"
sanitizeMethodName "-"  = "minus"
sanitizeMethodName "*"  = "times"
sanitizeMethodName "/"  = "div"
sanitizeMethodName name = name

-- | Capitalize first character
capitalizeFirst :: String -> String
capitalizeFirst []     = []
capitalizeFirst (c:cs) = toUpper c : cs

-- | Lowercase first character  
lowerFirst :: String -> String
lowerFirst []     = []
lowerFirst (c:cs) = toLower c : cs

--
-- Typed Desugaring (preserves type information)
--

-- | Desugar a typed expression to typed internal expression (TIExpr)
-- The type is preserved and can be used during evaluation
desugarTypedExprT :: TypedExpr -> EvalM TIExpr
desugarTypedExprT te@(TypedExpr ty _) = do
  iexpr <- desugarTypedExpr te
  return $ TIExpr ty iexpr

-- | Desugar a typed top expression to typed internal top expression (TITopExpr)
desugarTypedTopExprT :: TypedTopExpr -> EvalM (Maybe TITopExpr)
desugarTypedTopExprT (TDefine var texpr) = do
  let ty = texprType texpr  -- Get the type from the TypedExpr
  tiexpr <- desugarTypedExprT texpr
  return $ Just $ TIDefine ty (stringToVar var) tiexpr

desugarTypedTopExprT (TDefineWithType var _constraints params retType texpr) = do
  tiexpr <- desugarTypedExprT texpr
  -- If there are parameters, wrap the body in a lambda expression
  let paramNames = map fst params
      paramTypes = map snd params
      funType = if null paramNames
                then retType
                else foldr TFun retType paramTypes
      lambdaTIExpr = if null paramNames
                     then tiexpr
                     else TIExpr funType
                          (ILambdaExpr Nothing (map stringToVar paramNames) (tiExpr tiexpr))
  return $ Just $ TIDefine funType (stringToVar var) lambdaTIExpr

desugarTypedTopExprT (TTest texpr) = do
  tiexpr <- desugarTypedExprT texpr
  return $ Just $ TITest tiexpr

desugarTypedTopExprT (TExecute texpr) = do
  tiexpr <- desugarTypedExprT texpr
  return $ Just $ TIExecute tiexpr

desugarTypedTopExprT (TLoadFile file) = return $ Just $ TILoadFile file
desugarTypedTopExprT (TLoad file) = return $ Just $ TILoad file

-- For expressions that don't produce runtime code
desugarTypedTopExprT (TInductiveDecl {}) = return Nothing
desugarTypedTopExprT (TClassDecl {}) = return Nothing
desugarTypedTopExprT (TInstanceDecl {}) = return Nothing

