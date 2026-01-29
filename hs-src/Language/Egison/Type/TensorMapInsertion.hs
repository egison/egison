{- |
Module      : Language.Egison.Type.TensorMapInsertion
Licence     : MIT

This module implements automatic tensorMap insertion for Phase 8 of the Egison compiler.
This is the first step of TypedDesugar, before type class expansion.
When a function expects a scalar type (e.g., Integer) but receives a Tensor type,
this module automatically inserts tensorMap to apply the function element-wise.

Two insertion modes:
1. Direct application: When argument is Tensor and parameter expects scalar,
   wrap the application with tensorMap.
2. Higher-order functions (simplified approach): When a binary function with
   constrained/scalar parameter types is passed as an argument, always wrap
   it with tensorMap2. This handles cases like `foldl1 (+) xs` where elements
   of xs might be Tensors at runtime.

According to tensor-map-insertion-simple.md:
- For type constructors containing scalar types, always insert tensorMap/tensorMap2
- tensorMap/tensorMap2 act as identity for scalar values
- This simplifies implementation by avoiding is_tensor flag tracking

Example:
  def f (x : Integer) : Integer := x
  def t1 := [| 1, 2 |]
  f t1  --=>  tensorMap (\t1e -> f t1e) t1

  def sum {Num a} (xs: [a]) : a := foldl1 (+) xs
  --=>  def sum {Num a} (xs: [a]) : a := foldl1 (tensorMap2 (+)) xs
-}

module Language.Egison.Type.TensorMapInsertion
  ( insertTensorMaps
  ) where

import           Data.List                  (nub)
import           Language.Egison.Data       (EvalM)
import           Language.Egison.EvalState  (MonadEval(..))
import           Language.Egison.IExpr      (TIExpr(..), TIExprNode(..),
                                             Var(..), tiExprType, tiScheme, tiExprNode, stringToVar)
import           Language.Egison.Type.Env   (ClassEnv, lookupInstances, InstanceInfo(..), lookupEnv)
import           Language.Egison.Type.Tensor ()
import           Language.Egison.Type.Types (Type(..), TypeScheme(..), Constraint(..), TyVar(..))
import           Language.Egison.Type.Unify as Unify (unifyStrict, unifyStrictWithConstraints)

--------------------------------------------------------------------------------
-- * TensorMap Insertion Decision Logic
--------------------------------------------------------------------------------

-- | Check if tensorMap should be inserted for an argument
-- This implements the type-tensor-simple.md specification
--
-- Arguments:
--   ClassEnv     : The current type class environment (holds available type class instances).
--   [Constraint] : The set of type class constraints in scope (e.g., Num a, Eq a).
--   Type         : The type of the argument being applied to the function.
--   Type         : The type of the parameter as expected by the function (i.e., declared type).
shouldInsertTensorMap :: ClassEnv -> [Constraint] -> Type -> Type -> Bool
shouldInsertTensorMap classEnv constraints argType paramType = case argType of
  TTensor elemType -> case paramType of
    -- Tensor matched with Tensor → no tensorMap insertion needed
    -- Functions like dotProduct : Tensor a -> Tensor a -> Tensor a are designed for Tensor
    -- NOTE: This means (+) applied to Tensor won't get tensorMap at this point.
    -- For (+), the paramType should be the element type (Integer) after type class expansion,
    -- which will be handled by the concrete type case below.
    TTensor _ -> False

    -- Tensor matched with type variable → check type class instances
    TVar tyVar ->
      -- Find constraints on this type variable
      -- NOTE: The type variable in constraints may differ from tyVar due to instantiation
      -- (e.g., TypeEnv has {Num a} a -> a -> a, but context has {Num t0})
      -- So we look for constraints on EITHER the exact tyVar OR any numeric constraint
      let relevantConstraints = filter (\(Constraint _ t) -> t == TVar tyVar) constraints
          -- Also check for any numeric constraints that might apply
          isNumericConstraint (Constraint className _) =
            className `elem` ["Num", "Fractional", "Floating", "Real", "Integral"]
          numericConstraints = filter isNumericConstraint constraints
      in case relevantConstraints of
           -- Found constraints for this exact type variable
           cs@(_:_) -> not (all (hasInstanceForTensor classEnv elemType) cs)
           -- No exact match, but if there are numeric constraints in scope,
           -- and Tensor doesn't have instances for them, insert tensorMap
           [] -> case numericConstraints of
                   [] -> False  -- No constraints → 0-rank tensor interpretation
                   cs -> not (all (hasInstanceForTensor classEnv elemType) cs)

    -- Tensor matched with concrete non-tensor type → insertion needed
    _ -> True

  -- Non-tensor argument → no insertion needed
  _ -> False

-- | Check if a type class has an instance for Tensor elemType
-- Uses strict unify to check if an instance type matches the query type
-- IMPORTANT: We use unifyStrict here to ensure Tensor a does NOT unify with a
-- This prevents incorrectly detecting scalar instances as tensor instances
hasInstanceForTensor :: ClassEnv -> Type -> Constraint -> Bool
hasInstanceForTensor classEnv elemType (Constraint className _tyVar) =
  let tensorType = TTensor elemType
      instances = lookupInstances className classEnv
  in any (\inst -> case Unify.unifyStrict (instType inst) tensorType of
                     Right _ -> True   -- Instance type unifies with Tensor type
                     Left _  -> False  -- No match
         ) instances

-- | Unlift a function type that was lifted for Tensor arguments
-- Tensor a -> Tensor b -> Tensor c  becomes  a -> b -> c
unliftFunctionType :: Type -> Type
unliftFunctionType (TFun (TTensor paramType) restType) =
  TFun paramType (unliftFunctionType restType)
unliftFunctionType (TFun paramType restType) =
  TFun paramType (unliftFunctionType restType)
unliftFunctionType (TTensor returnType) = returnType
unliftFunctionType ty = ty

-- | Get the parameter type at the specified index from a function type
-- Example: (a -> b -> c) at index 0 → Just a, at index 1 → Just b
getParamType :: Type -> Int -> Maybe Type
getParamType (TFun param _) 0 = Just param
getParamType (TFun _ rest) n 
  | n > 0 = getParamType rest (n - 1)
getParamType _ _ = Nothing

-- | Apply one argument to a function type
-- Example: (a -> b -> c) → (b -> c)
applyOneArgType :: Type -> Type
applyOneArgType (TFun _ rest) = rest
applyOneArgType t = t  -- No more arguments

--------------------------------------------------------------------------------
-- * Simplified Approach: Always wrap binary functions with tensorMap2
--------------------------------------------------------------------------------

-- | Check if a type is a scalar type (not a Tensor type)
-- A scalar type is one that does NOT unify with Tensor a (using strict unification with constraints).
--
-- This uses unifyStrictWithConstraints to determine if a type can unify with Tensor a:
-- - If unification succeeds → the type IS compatible with Tensor → NOT a scalar type (False)
-- - If unification fails → the type is NOT compatible with Tensor → IS a scalar type (True)
--
-- Examples:
-- - {Num t0} t0: Tensor a doesn't have Num instance → cannot unify → scalar type (True)
-- - Tensor t0: Can unify with Tensor a → not a scalar type (False)
-- - Integer: Can unify with Tensor a (unconstrained) → not a scalar type (False)
--   BUT we want Integer to be treated as scalar, so we add a special case
-- - Unconstrained type variable: Can unify with Tensor a → not a scalar type (False)
isPotentialScalarType :: ClassEnv -> [Constraint] -> Type -> Bool
isPotentialScalarType classEnv constraints ty = case ty of
  -- Concrete scalar types are always scalar (special case)
  TInt -> True
  TFloat -> True
  TMathExpr -> True
  TChar -> True
  TString -> True
  TBool -> True

  -- Tensor types are never scalar
  TTensor _ -> False

  -- For type variables and other types, check if they can unify with Tensor a
  _ ->
    -- Create a fresh type variable 'a' and try to unify ty with Tensor a
    let freshVar = TyVar "a_scalar_check"
        tensorType = TTensor (TVar freshVar)
    in case Unify.unifyStrictWithConstraints classEnv constraints ty tensorType of
         Right _ -> False  -- Can unify with Tensor a → not scalar
         Left _  -> True   -- Cannot unify with Tensor a → is scalar

-- | Check if a binary function should be wrapped with tensorMap2
-- A function should be wrapped if:
-- 1. It's a binary function (a -> b -> c)
-- 2. Both parameter types are scalar types (not Tensor types)
--
-- For example:
-- - (+) : {Num a} a -> a -> a  -- Both params are scalar → wrap with tensorMap2
-- - (.) : {Num a} Tensor a -> Tensor a -> Tensor a  -- Both params are Tensor → do NOT wrap
shouldWrapWithTensorMap2 :: ClassEnv -> [Constraint] -> Type -> Bool
shouldWrapWithTensorMap2 classEnv constraints ty = case ty of
  TFun param1 (TFun param2 _result) ->
      isPotentialScalarType classEnv constraints param1 &&
      isPotentialScalarType classEnv constraints param2
  _ -> False

-- | Wrap a binary function expression with tensorMap2
-- f : a -> b -> c  becomes  \x y -> tensorMap2 f x y
-- The lambda receives TENSOR arguments and returns a TENSOR result
wrapWithTensorMap2 :: [Constraint] -> TIExpr -> TIExpr
wrapWithTensorMap2 _constraints funcExpr =
  let funcType = tiExprType funcExpr
  in case funcType of
    TFun param1 (TFun param2 result) ->
      let -- Create fresh variable names
          var1Name = "tmap2_arg1"
          var2Name = "tmap2_arg2"
          var1 = Var var1Name []
          var2 = Var var2Name []

          -- Variables have TENSOR types (they receive tensor arguments)
          var1Scheme = Forall [] [] (TTensor param1)
          var2Scheme = Forall [] [] (TTensor param2)
          var1TI = TIExpr var1Scheme (TIVarExpr var1Name)
          var2TI = TIExpr var2Scheme (TIVarExpr var2Name)

          -- Result is also a TENSOR
          resultScheme = Forall [] [] (TTensor result)

          -- Build: tensorMap2 funcExpr var1 var2
          innerNode = TITensorMap2Expr funcExpr var1TI var2TI
          innerExpr = TIExpr resultScheme innerNode

          -- Build lambda: \var1 var2 -> tensorMap2 funcExpr var1 var2
          -- Lambda type: Tensor a -> Tensor b -> Tensor c
          -- No constraints needed - this is just a wrapper
          lambdaType = TFun (TTensor param1) (TFun (TTensor param2) (TTensor result))
          lambdaScheme = Forall [] [] lambdaType
          lambdaNode = TILambdaExpr Nothing [var1, var2] innerExpr

      in TIExpr lambdaScheme lambdaNode
    _ -> funcExpr  -- Not a binary function, return unchanged

-- | Check if an expression is already wrapped with tensorMap2
isAlreadyWrappedWithTensorMap2 :: TIExprNode -> Bool
isAlreadyWrappedWithTensorMap2 (TILambdaExpr _ [_, _] body) =
  case tiExprNode body of
    TITensorMap2Expr _ _ _ -> True
    _ -> False
isAlreadyWrappedWithTensorMap2 _ = False

--------------------------------------------------------------------------------
-- * TensorMap Insertion Implementation
--------------------------------------------------------------------------------

-- | Insert tensorMap expressions where needed in a TIExpr
-- This is the main entry point for tensorMap insertion
insertTensorMaps :: TIExpr -> EvalM TIExpr
insertTensorMaps tiExpr = do
  classEnv <- getClassEnv
  let scheme = tiScheme tiExpr
  insertTensorMapsInExpr classEnv scheme tiExpr

-- | Wrap a binary function with tensorMap2 if it should be wrapped
-- This implements the simplified approach from tensor-map-insertion-simple.md
wrapBinaryFunctionIfNeeded :: ClassEnv -> [Constraint] -> TIExpr -> TIExpr
wrapBinaryFunctionIfNeeded classEnv constraints tiExpr =
  let exprType = tiExprType tiExpr
      node = tiExprNode tiExpr
  in -- Don't wrap if already wrapped with tensorMap2
     if isAlreadyWrappedWithTensorMap2 node
       then tiExpr
       else case node of
         -- For binary lambda expressions like \x y -> f x y, wrap the body with tensorMap2
         -- This handles eta-expanded type class methods like \etaVar1 etaVar2 -> dict_("plus") etaVar1 etaVar2
         TILambdaExpr mVar [var1, var2] body
           | shouldWrapWithTensorMap2 classEnv constraints exprType ->
               wrapLambdaBodyWithTensorMap2 constraints mVar var1 var2 body tiExpr
         -- Don't wrap other lambda expressions
         TILambdaExpr {} -> tiExpr
         -- Don't wrap function applications (they're already being applied)
         TIApplyExpr {} -> tiExpr
         -- Wrap variable references and other expressions that represent functions
         _ | shouldWrapWithTensorMap2 classEnv constraints exprType ->
               wrapWithTensorMap2 constraints tiExpr
           | otherwise -> tiExpr

-- | Wrap the body of a binary lambda with tensorMap2
-- Transform: \x y -> f x y  to  \x y -> tensorMap2 f x y
wrapLambdaBodyWithTensorMap2 :: [Constraint] -> Maybe Var -> Var -> Var -> TIExpr -> TIExpr -> TIExpr
wrapLambdaBodyWithTensorMap2 constraints mVar var1 var2 body originalExpr =
  case tiExprNode body of
    -- Body is a function application: \x y -> f x y
    TIApplyExpr func args
      | length args == 2 ->
          let arg1 = args !! 0
              arg2 = args !! 1
              -- Create tensorMap2 f arg1 arg2
              resultType = tiExprType body
              resultScheme = Forall [] [] resultType
              newBody = TIExpr resultScheme (TITensorMap2Expr func arg1 arg2)
              -- Rebuild the lambda with the new body
              (Forall tvs cs lambdaType) = tiScheme originalExpr
              newLambdaScheme = Forall tvs (constraints ++ cs) lambdaType
          in TIExpr newLambdaScheme (TILambdaExpr mVar [var1, var2] newBody)
    -- Body is already tensorMap2
    TITensorMap2Expr {} -> originalExpr
    -- Other cases: just wrap the whole thing
    _ -> wrapWithTensorMap2 constraints originalExpr

-- | Insert tensorMap in a TIExpr with type scheme information
insertTensorMapsInExpr :: ClassEnv -> TypeScheme -> TIExpr -> EvalM TIExpr
insertTensorMapsInExpr classEnv scheme tiExpr = do
  let (Forall _vars constraints _ty) = scheme
  expandedNode <- insertInNode classEnv constraints (tiExprNode tiExpr)
  -- Note: We don't wrap at this level. Wrapping only happens for function arguments
  -- in TIApplyExpr to avoid wrapping definitions like `def (*') := i.*`
  return $ TIExpr scheme expandedNode
  where
    -- Process a TIExprNode
    insertInNode :: ClassEnv -> [Constraint] -> TIExprNode -> EvalM TIExprNode
    insertInNode env cs node = case node of
      -- Constants and variables: no change needed
      TIConstantExpr c -> return $ TIConstantExpr c
      TIVarExpr name -> return $ TIVarExpr name
      
      -- Lambda expressions: process body
      TILambdaExpr mVar params body -> do
        let (Forall _ bodyConstraints _) = tiScheme body
            allConstraints = cs ++ bodyConstraints
        body' <- insertTensorMapsWithConstraints env allConstraints body
        return $ TILambdaExpr mVar params body'
      
      -- Function application: check if tensorMap is needed
      TIApplyExpr func args -> do
        -- First, recursively process function and arguments
        func' <- insertTensorMapsWithConstraints env cs func
        args' <- mapM (insertTensorMapsWithConstraints env cs) args

        -- Apply simplified approach: wrap binary function arguments with tensorMap2
        -- This handles cases like `foldl (+) 0 xs` where (+) needs to be wrapped because (+) is a binary function that takes two scalar arguments
        -- But `foldl1 (.) [t1, t2]` should not be wrapped with tensorMap2 because (.) is a binary function that takes two tensor arguments
        -- IMPORTANT: Include each argument's own constraints when deciding if it needs wrapping
        let (Forall _ funcConstraints _) = tiScheme func'
            baseConstraints = cs ++ funcConstraints
            -- For each argument, merge base constraints with the argument's own constraints
            wrapArg arg =
              let (Forall _ argConstraints _) = tiScheme arg
                  argAllConstraints = nub (baseConstraints ++ argConstraints)
              in wrapBinaryFunctionIfNeeded env argAllConstraints arg
            args'' = map wrapArg args'

        -- Get the ORIGINAL function type from TypeEnv if available
        -- This is important because type inference may have substituted type variables
        -- with Tensor types, but we need the original type to decide tensorMap insertion
        -- e.g., (*) has type {Num a} a -> a -> a, not Tensor t0 -> Tensor t0 -> Tensor t0
        typeEnv <- getTypeEnv
        let funcType = case tiExprNode func' of
              TIVarExpr varName ->
                case lookupEnv (stringToVar varName) typeEnv of
                  Just (Forall _ _ originalType) -> originalType
                  Nothing -> tiExprType func'  -- Fallback to inferred type
              _ -> tiExprType func'  -- Non-variable: use inferred type
            argTypes = map tiExprType args''

        -- Normal processing: check if tensorMap is needed based on parameter types
        result <- wrapWithTensorMapIfNeeded env baseConstraints func' funcType args'' argTypes
        case result of
          Just wrappedNode -> return wrappedNode
          Nothing -> return $ TIApplyExpr func' args''
      
      -- Collections
      TITupleExpr exprs -> do
        exprs' <- mapM (insertTensorMapsWithConstraints env cs) exprs
        return $ TITupleExpr exprs'
      
      TICollectionExpr exprs -> do
        exprs' <- mapM (insertTensorMapsWithConstraints env cs) exprs
        return $ TICollectionExpr exprs'
      
      TIConsExpr h t -> do
        h' <- insertTensorMapsWithConstraints env cs h
        t' <- insertTensorMapsWithConstraints env cs t
        return $ TIConsExpr h' t'
      
      TIJoinExpr l r -> do
        l' <- insertTensorMapsWithConstraints env cs l
        r' <- insertTensorMapsWithConstraints env cs r
        return $ TIJoinExpr l' r'
      
      TIHashExpr pairs -> do
        pairs' <- mapM (\(k, v) -> do
          k' <- insertTensorMapsWithConstraints env cs k
          v' <- insertTensorMapsWithConstraints env cs v
          return (k', v')) pairs
        return $ TIHashExpr pairs'
      
      TIVectorExpr exprs -> do
        exprs' <- mapM (insertTensorMapsWithConstraints env cs) exprs
        return $ TIVectorExpr exprs'
      
      -- Control flow
      TIIfExpr cond thenExpr elseExpr -> do
        cond' <- insertTensorMapsWithConstraints env cs cond
        thenExpr' <- insertTensorMapsWithConstraints env cs thenExpr
        elseExpr' <- insertTensorMapsWithConstraints env cs elseExpr
        return $ TIIfExpr cond' thenExpr' elseExpr'
      
      -- Let bindings
      TILetExpr bindings body -> do
        bindings' <- mapM (\(v, e) -> do
          e' <- insertTensorMapsWithConstraints env cs e
          return (v, e')) bindings
        body' <- insertTensorMapsWithConstraints env cs body
        return $ TILetExpr bindings' body'
      
      TILetRecExpr bindings body -> do
        bindings' <- mapM (\(v, e) -> do
          e' <- insertTensorMapsWithConstraints env cs e
          return (v, e')) bindings
        body' <- insertTensorMapsWithConstraints env cs body
        return $ TILetRecExpr bindings' body'
      
      TISeqExpr e1 e2 -> do
        e1' <- insertTensorMapsWithConstraints env cs e1
        e2' <- insertTensorMapsWithConstraints env cs e2
        return $ TISeqExpr e1' e2'
      
      -- Pattern matching
      TIMatchExpr mode target matcher clauses -> do
        target' <- insertTensorMapsWithConstraints env cs target
        matcher' <- insertTensorMapsWithConstraints env cs matcher
        clauses' <- mapM (\(pat, body) -> do
          body' <- insertTensorMapsWithConstraints env cs body
          return (pat, body')) clauses
        return $ TIMatchExpr mode target' matcher' clauses'
      
      TIMatchAllExpr mode target matcher clauses -> do
        target' <- insertTensorMapsWithConstraints env cs target
        matcher' <- insertTensorMapsWithConstraints env cs matcher
        clauses' <- mapM (\(pat, body) -> do
          body' <- insertTensorMapsWithConstraints env cs body
          return (pat, body')) clauses
        return $ TIMatchAllExpr mode target' matcher' clauses'
      
      -- More lambda-like constructs
      TIMemoizedLambdaExpr vars body -> do
        body' <- insertTensorMapsWithConstraints env cs body
        return $ TIMemoizedLambdaExpr vars body'
      
      TICambdaExpr var body -> do
        body' <- insertTensorMapsWithConstraints env cs body
        return $ TICambdaExpr var body'
      
      TIWithSymbolsExpr syms body -> do
        body' <- insertTensorMapsWithConstraints env cs body
        return $ TIWithSymbolsExpr syms body'
      
      TIDoExpr bindings body -> do
        bindings' <- mapM (\(v, e) -> do
          e' <- insertTensorMapsWithConstraints env cs e
          return (v, e')) bindings
        body' <- insertTensorMapsWithConstraints env cs body
        return $ TIDoExpr bindings' body'
      
      -- Tensor operations
      TITensorMapExpr func tensor -> do
        func' <- insertTensorMapsWithConstraints env cs func
        tensor' <- insertTensorMapsWithConstraints env cs tensor
        return $ TITensorMapExpr func' tensor'
      
      TITensorMap2Expr func t1 t2 -> do
        func' <- insertTensorMapsWithConstraints env cs func
        t1' <- insertTensorMapsWithConstraints env cs t1
        t2' <- insertTensorMapsWithConstraints env cs t2
        return $ TITensorMap2Expr func' t1' t2'
      
      TIGenerateTensorExpr func shape -> do
        func' <- insertTensorMapsWithConstraints env cs func
        shape' <- insertTensorMapsWithConstraints env cs shape
        return $ TIGenerateTensorExpr func' shape'
      
      TITensorExpr shape elems -> do
        shape' <- insertTensorMapsWithConstraints env cs shape
        elems' <- insertTensorMapsWithConstraints env cs elems
        return $ TITensorExpr shape' elems'
      
      TITensorContractExpr tensor -> do
        tensor' <- insertTensorMapsWithConstraints env cs tensor
        return $ TITensorContractExpr tensor'
      
      TITransposeExpr perm tensor -> do
        perm' <- insertTensorMapsWithConstraints env cs perm
        tensor' <- insertTensorMapsWithConstraints env cs tensor
        return $ TITransposeExpr perm' tensor'
      
      TIFlipIndicesExpr tensor -> do
        tensor' <- insertTensorMapsWithConstraints env cs tensor
        return $ TIFlipIndicesExpr tensor'
      
      -- Quote expressions
      TIQuoteExpr e -> do
        e' <- insertTensorMapsWithConstraints env cs e
        return $ TIQuoteExpr e'
      
      TIQuoteSymbolExpr e -> do
        e' <- insertTensorMapsWithConstraints env cs e
        return $ TIQuoteSymbolExpr e'
      
      -- Indexed expressions
      TISubrefsExpr b base ref -> do
        base' <- insertTensorMapsWithConstraints env cs base
        ref' <- insertTensorMapsWithConstraints env cs ref
        return $ TISubrefsExpr b base' ref'
      
      TISuprefsExpr b base ref -> do
        base' <- insertTensorMapsWithConstraints env cs base
        ref' <- insertTensorMapsWithConstraints env cs ref
        return $ TISuprefsExpr b base' ref'
      
      TIUserrefsExpr b base ref -> do
        base' <- insertTensorMapsWithConstraints env cs base
        ref' <- insertTensorMapsWithConstraints env cs ref
        return $ TIUserrefsExpr b base' ref'
      
      -- Other cases
      TIInductiveDataExpr name exprs -> do
        exprs' <- mapM (insertTensorMapsWithConstraints env cs) exprs
        return $ TIInductiveDataExpr name exprs'
      
      TIMatcherExpr patDefs -> return $ TIMatcherExpr patDefs
      
      TIIndexedExpr override base indices -> do
        base' <- insertTensorMapsWithConstraints env cs base
        indices' <- mapM (traverse (\tiexpr -> insertTensorMapsWithConstraints env cs tiexpr)) indices
        return $ TIIndexedExpr override base' indices'
      
      TIWedgeApplyExpr func args -> do
        func' <- insertTensorMapsWithConstraints env cs func
        args' <- mapM (insertTensorMapsWithConstraints env cs) args
        return $ TIWedgeApplyExpr func' args'
      
      TIFunctionExpr names -> return $ TIFunctionExpr names

-- | Helper to insert tensorMaps in a TIExpr with constraints
-- IMPORTANT: Merges context constraints with expression's own constraints
-- This is critical for polymorphic functions where the constraint (e.g., {Num t0})
-- comes from the enclosing scope, not the expression itself.
insertTensorMapsWithConstraints :: ClassEnv -> [Constraint] -> TIExpr -> EvalM TIExpr
insertTensorMapsWithConstraints env contextConstraints expr = do
  let (Forall tvs exprConstraints ty) = tiScheme expr
      -- Merge context constraints with expression's own constraints, deduplicating
      mergedConstraints = nub (contextConstraints ++ exprConstraints)
      mergedScheme = Forall tvs mergedConstraints ty
  insertTensorMapsInExpr env mergedScheme expr

-- | Wrap function application with tensorMap if needed
-- Returns Just wrappedNode if tensorMap was inserted, Nothing otherwise
wrapWithTensorMapIfNeeded :: ClassEnv -> [Constraint] -> TIExpr -> Type -> [TIExpr] -> [Type] -> EvalM (Maybe TIExprNode)
wrapWithTensorMapIfNeeded classEnv constraints func funcType args argTypes = do
  -- Check if any argument needs tensorMap
  let checks = zipWith (\argType idx -> 
                 case getParamType funcType idx of
                   Just paramType -> shouldInsertTensorMap classEnv constraints argType paramType
                   Nothing -> False
               ) argTypes [0..]
  
  if or checks
    then do
      -- Need to insert tensorMap - use recursive wrapping
      wrapped <- wrapWithTensorMapRecursive classEnv constraints func funcType args argTypes
      return $ Just wrapped
    else
      -- No tensorMap needed
      return Nothing

-- | Recursively wrap function application with tensorMap where needed
-- Process arguments from left to right, building tensorMap2 for consecutive tensor arguments
wrapWithTensorMapRecursive :: 
    ClassEnv
    -> [Constraint]
    -> TIExpr          -- Current function expression (possibly partially applied)
    -> Type            -- Current function type
    -> [TIExpr]        -- Remaining argument expressions  
    -> [Type]          -- Remaining argument types
    -> EvalM TIExprNode
wrapWithTensorMapRecursive _classEnv _constraints currentFunc _currentType [] [] = do
  -- All arguments processed - return the application
  return $ tiExprNode currentFunc

wrapWithTensorMapRecursive classEnv constraints currentFunc currentType (arg1:restArgs) (argType1:restArgTypes) = do
  -- Get the expected parameter type for first argument
  case getParamType currentType 0 of
    Nothing -> return $ TIApplyExpr currentFunc (arg1 : restArgs)
    Just paramType1 -> do
      let needsTensorMap1 = shouldInsertTensorMap classEnv constraints argType1 paramType1
      
      if needsTensorMap1
        then do
          -- Check if we have a second argument that also needs tensorMap
          -- If so, use tensorMap2 instead of nested tensorMap
          case (restArgs, restArgTypes) of
            (arg2:restArgs', argType2:restArgTypes') -> do
              let innerType = applyOneArgType currentType
              case getParamType innerType 0 of
                Just paramType2 | shouldInsertTensorMap classEnv constraints argType2 paramType2 -> do
                  -- Both first and second arguments need tensorMap → use tensorMap2
                  let varName1 = "tmapVar" ++ show (length restArgs)
                      varName2 = "tmapVar" ++ show (length restArgs')
                      var1 = Var varName1 []
                      var2 = Var varName2 []

                      -- Extract element types from tensors
                      elemType1 = case argType1 of
                                    TTensor t -> t
                                    _ -> argType1
                      elemType2 = case argType2 of
                                    TTensor t -> t
                                    _ -> argType2

                      varScheme1 = Forall [] [] elemType1
                      varScheme2 = Forall [] [] elemType2
                      varTIExpr1 = TIExpr varScheme1 (TIVarExpr varName1)
                      varTIExpr2 = TIExpr varScheme2 (TIVarExpr varName2)

                      -- Unlift the function type for use inside tensorMap
                      -- IMPORTANT: Use the instantiated type from currentFunc, not the polymorphic currentType
                      -- This ensures we use the unified type variable (e.g., t0) instead of fresh variables (e.g., a)
                      instantiatedFuncType = tiExprType currentFunc
                      unliftedFuncType = unliftFunctionType instantiatedFuncType
                      funcScheme = tiScheme currentFunc
                      (Forall tvs funcConstraints _) = funcScheme
                      unliftedFuncScheme = Forall tvs funcConstraints unliftedFuncType
                      unliftedFunc = TIExpr unliftedFuncScheme (tiExprNode currentFunc)

                      -- Build inner expression with both variables applied
                      innerType2 = applyOneArgType (applyOneArgType unliftedFuncType)
                      -- After applying both arguments, this is a fully-applied result - no constraints needed
                      innerFuncScheme = Forall [] [] innerType2
                      innerFuncTI = TIExpr innerFuncScheme (TIApplyExpr unliftedFunc [varTIExpr1, varTIExpr2])

                  -- Process remaining arguments after consuming two
                  innerNode <- wrapWithTensorMapRecursive classEnv constraints innerFuncTI innerType2 restArgs' restArgTypes'
                  let innerTIExpr = TIExpr innerFuncScheme innerNode
                      finalType = tiExprType innerTIExpr

                  -- Build lambda: \varName1 varName2 -> innerTIExpr
                  -- Lambda has no constraints - it's just a wrapper that receives scalars
                  let lambdaType = TFun elemType1 (TFun elemType2 finalType)
                      lambdaScheme = Forall [] [] lambdaType
                      lambdaTI = TIExpr lambdaScheme (TILambdaExpr Nothing [var1, var2] innerTIExpr)

                  return $ TITensorMap2Expr lambdaTI arg1 arg2
                
                _ -> do
                  -- Only first argument needs tensorMap → use regular tensorMap
                  insertSingleTensorMap classEnv constraints currentFunc currentType arg1 argType1 restArgs restArgTypes
            
            _ -> do
              -- No more arguments or types → use regular tensorMap for first argument
              insertSingleTensorMap classEnv constraints currentFunc currentType arg1 argType1 restArgs restArgTypes
        
        else do
          -- First argument doesn't need tensorMap, apply normally and continue
          let appliedType = applyOneArgType currentType
              appliedScheme = Forall [] constraints appliedType
              appliedTI = TIExpr appliedScheme (TIApplyExpr currentFunc [arg1])
          
          -- Process remaining arguments (recursive call)
          wrapWithTensorMapRecursive classEnv constraints appliedTI appliedType restArgs restArgTypes

wrapWithTensorMapRecursive _classEnv _constraints currentFunc _currentType _args _argTypes = 
  return $ TIApplyExpr currentFunc []

-- | Helper function to insert a single tensorMap (when tensorMap2 is not applicable)
insertSingleTensorMap ::
    ClassEnv
    -> [Constraint]
    -> TIExpr          -- Current function expression
    -> Type            -- Current function type
    -> TIExpr          -- Tensor argument
    -> Type            -- Tensor argument type
    -> [TIExpr]        -- Remaining arguments
    -> [Type]          -- Remaining argument types
    -> EvalM TIExprNode
insertSingleTensorMap classEnv constraints currentFunc _currentType arg argType restArgs restArgTypes = do
  let varName = "tmapVar" ++ show (length restArgs)
      var = Var varName []

      -- Extract element type from tensor
      elemType = case argType of
                   TTensor t -> t
                   _ -> argType

      varScheme = Forall [] [] elemType
      varTIExpr = TIExpr varScheme (TIVarExpr varName)

      -- Unlift the function type for use inside tensorMap
      -- IMPORTANT: Use the instantiated type from currentFunc, not the polymorphic currentType
      -- This ensures we use the unified type variable (e.g., t0) instead of fresh variables (e.g., a)
      instantiatedFuncType = tiExprType currentFunc
      unliftedFuncType = unliftFunctionType instantiatedFuncType
      funcScheme = tiScheme currentFunc
      (Forall tvs funcConstraints _) = funcScheme
      unliftedFuncScheme = Forall tvs funcConstraints unliftedFuncType
      unliftedFunc = TIExpr unliftedFuncScheme (tiExprNode currentFunc)

      -- Build inner expression (recursive call)
      innerType = applyOneArgType unliftedFuncType
      -- Only keep constraints if this is a partial application (function type)
      -- If it's a fully-applied value, no constraints needed
      innerConstraints = case innerType of
                           TFun _ _ -> funcConstraints  -- Partial application
                           _ -> []  -- Fully applied: no constraints
      innerFuncScheme = Forall [] innerConstraints innerType
      innerFuncTI = TIExpr innerFuncScheme (TIApplyExpr unliftedFunc [varTIExpr])

  -- Process remaining arguments
  innerNode <- wrapWithTensorMapRecursive classEnv constraints innerFuncTI innerType restArgs restArgTypes
  let innerTIExpr = TIExpr innerFuncScheme innerNode
      finalType = tiExprType innerTIExpr

  -- Build lambda: \varName -> innerTIExpr
  -- Lambda has no constraints - it's just a wrapper that receives a scalar
  let lambdaType = TFun elemType finalType
      lambdaScheme = Forall [] [] lambdaType
      lambdaTI = TIExpr lambdaScheme (TILambdaExpr Nothing [var] innerTIExpr)

  return $ TITensorMapExpr lambdaTI arg
