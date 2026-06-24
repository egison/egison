{- |
Module      : Language.Egison.Type.TensorMapInsertion
Licence     : MIT

This module implements automatic tensorMap insertion for Phase 7 of the Egison compiler.
This is the first step of TypedDesugar, before type class expansion.
When a function expects a scalar type (e.g., Integer) but receives a Tensor type,
this module automatically inserts tensorMap to apply the function element-wise.

Four implementation forms:
1. Direct application: When argument is Tensor and parameter expects scalar,
   wrap the application with tensorMap.
2. Type-directed higher-order lifting: when a scalar function is passed to a
   callback position that expects tensor arguments, eta-expand it and map those
   tensor arguments back to scalar arguments.
3. Feedback-aware higher-order lifting: when a lifted callback result feeds
   back into a callback parameter, e.g. the accumulator in foldl, lift that
   parameter too.
4. Derived binary-map compatibility: when a binary scalar function is passed as
   a value, e.g. `foldl1 (+) xs`, use tensorMap2 so reduction accumulators can
   become tensors without rebuilding the nested maps by hand.

According to tensor-map-insertion-simple.md:
- tensorMap2 is semantically equivalent to nested tensorMap
- tensorMap/tensorMap2 act as identity for scalar values, so wrapping is safe regardless of whether the actual argument is a tensor or scalar

Example:
  def f (x : Integer) : Integer := x
  def t1 := [| 1, 2 |]
  f t1  --=>  tensorMap (\t1e -> f t1e) t1

  def sum {Num a} (xs: [a]) : a := foldl1 (+) xs
  --=>  def sum {Num a} (xs: [a]) : a := foldl1 (tensorMap2 (+)) xs

  map inc [t1]
  --=>  map (\x -> tensorMap inc x) [t1]

  map2 (*) [t1] [10]
  --=>  map2 (\x y -> tensorMap (\xe -> (*) xe y) x) [t1] [10]

  foldl (+) 0 [t1]
  --=>  foldl (\acc x -> tensorMap2 (+) acc x) 0 [t1]
-}

module Language.Egison.Type.TensorMapInsertion
  ( insertTensorMaps
  ) where

import           Data.List                  (nub, zipWith4)
import           Language.Egison.Data       (EvalM)
import           Language.Egison.EvalState  (MonadEval(..))
import           Language.Egison.IExpr      (TIExpr(..), TIExprNode(..),
                                             Var(..), tiExprType, tiScheme, tiExprNode)
import           Language.Egison.Type.Env   (ClassEnv)
import           Language.Egison.Type.Tensor (normalizeTensorType)
import           Language.Egison.Type.Types (Type(..), TypeScheme(..), Constraint(..), TyVar(..))
import           Language.Egison.Type.Unify as Unify (unifyStrictWithConstraints)

--------------------------------------------------------------------------------
-- * TensorMap Insertion Decision Logic
--------------------------------------------------------------------------------

-- | Check if tensorMap should be inserted for an argument
-- This implements the type-tensor-simple.md specification
--
-- TensorMap should be inserted when:
-- 1. paramType does NOT unify with Tensor a (i.e., paramType is a scalar type)
-- 2. AND argType does unify with Tensor a (i.e., argType is a tensor type)
--
-- Arguments:
--   ClassEnv     : The current type class environment (holds available type class instances).
--   [Constraint] : The set of type class constraints in scope (e.g., Num a, Eq a).
--   Type         : The type of the argument being applied to the function.
--   Type         : The type of the parameter as expected by the function (i.e., declared type).
shouldInsertTensorMap :: ClassEnv -> [Constraint] -> Type -> Type -> Bool
shouldInsertTensorMap classEnv constraints argType paramType =
  -- Check if paramType does NOT unify with Tensor a (is scalar)
  let isParamScalar = isPotentialScalarType classEnv constraints paramType
      -- Check if argType does unify with Tensor a (is tensor)
      freshVar = TyVar "a_arg_check"
      tensorType = TTensor (TVar freshVar)
      isArgTensor = case Unify.unifyStrictWithConstraints classEnv constraints argType tensorType of
                      Right _ -> True   -- Can unify with Tensor a → is tensor
                      Left _  -> False  -- Cannot unify → not tensor
  in isParamScalar && isArgTensor


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
-- * Higher-order tensor lifting
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
-- - Tensor t0: Tensor t0 unifies with Tensor a → not a scalar type (False)
-- - Integer: Integer doesn't unify with Tensor a (concrete type mismatch) → scalar type (True)
-- - Unconstrained type variable a: can unify with Tensor b → not a scalar type (False)
isPotentialScalarType :: ClassEnv -> [Constraint] -> Type -> Bool
isPotentialScalarType classEnv constraints ty =
  -- Create a fresh type variable 'a' and try to unify ty with Tensor a
  let freshVar = TyVar "a_scalar_check"
      tensorType = TTensor (TVar freshVar)
  in case Unify.unifyStrictWithConstraints classEnv constraints ty tensorType of
       Right _ -> False  -- Can unify with Tensor a → not scalar
       Left _  -> True   -- Cannot unify with Tensor a → is scalar

-- | Exclude effectful/resource-like types from compatibility lifting. These
-- types are not tensors, but wrapping callbacks such as `io : IO a -> a` would
-- change control-flow behavior in ordinary higher-order applications.
containsNonLiftableType :: Type -> Bool
containsNonLiftableType ty = case ty of
  TIO _ -> True
  TIORef _ -> True
  TPort -> True
  TFun _ _ -> True
  TTuple ts -> any containsNonLiftableType ts
  TCollection t -> containsNonLiftableType t
  TInductive _ ts -> any containsNonLiftableType ts
  TTensor t -> containsNonLiftableType t
  THash k v -> containsNonLiftableType k || containsNonLiftableType v
  TMatcher t -> containsNonLiftableType t
  TMatcherSlot p t -> containsNonLiftableType p || containsNonLiftableType t
  TTerm t _ -> containsNonLiftableType t
  TFrac t -> containsNonLiftableType t
  TPoly t _ -> containsNonLiftableType t
  _ -> False

isTensorLiftableScalarType :: ClassEnv -> [Constraint] -> Type -> Bool
isTensorLiftableScalarType classEnv constraints ty =
  isPotentialScalarType classEnv constraints ty &&
  not (containsNonLiftableType ty)

-- | Split a curried function type into its argument types and result type.
collectFunctionType :: Type -> ([Type], Type)
collectFunctionType (TFun param result) =
  let (params, finalResult) = collectFunctionType result
  in (param : params, finalResult)
collectFunctionType ty = ([], ty)

-- | Build a curried function type from argument types and a result type.
buildFunctionType :: [Type] -> Type -> Type
buildFunctionType params result = foldr TFun result params

-- | Apply N arguments at the type level.
applyNArgType :: Type -> Int -> Type
applyNArgType ty 0 = ty
applyNArgType (TFun _ result) n
  | n > 0 = applyNArgType result (n - 1)
applyNArgType ty _ = ty

-- | A parameter in a generated higher-order callback wrapper.
data CallbackParamPlan = CallbackParamPlan
  { callbackParamIndex      :: Int
  , callbackParamActualType :: Type
  , callbackParamOuterType  :: Type
  , callbackParamOuterVar   :: Var
  , callbackParamOuterExpr  :: TIExpr
  , callbackParamNeedsLift  :: Bool
  }

mkVarTIExpr :: String -> Type -> TIExpr
mkVarTIExpr name ty = TIExpr (Forall [] [] ty) (TIVarExpr name)

sameNormalizedType :: Type -> Type -> Bool
sameNormalizedType ty1 ty2 =
  normalizeTensorType ty1 == normalizeTensorType ty2

-- | A callback parameter is a direct lift seed when the higher-order function
-- expects a tensor argument there but the supplied function consumes a scalar.
isDirectLiftSeed :: ClassEnv -> [Constraint] -> Type -> Type -> Bool
isDirectLiftSeed classEnv constraints expectedParam actualParam =
  case expectedParam of
    TTensor _ -> isTensorLiftableScalarType classEnv constraints actualParam
    _ -> False

-- | Detect whether a callback's result is fed back by the surrounding
-- higher-order function as a naked value. This distinguishes reductions like
-- foldl/foldr/scanl from maps, whose callback result only appears under a list.
callbackResultFeedsBack :: Type -> Int -> Type -> Bool
callbackResultFeedsBack outerFuncType callbackArgIndex expectedCallbackType =
  let (outerParams, outerResult) = collectFunctionType outerFuncType
      (_, callbackResult) = collectFunctionType expectedCallbackType
      laterOuterParams = drop (callbackArgIndex + 1) outerParams
  in any (sameNormalizedType callbackResult) (outerResult : laterOuterParams)

-- | Compute the callback parameters that should be tensor-lifted.
--
-- First seed the positions that the expected callback type already marks as
-- Tensor. If such a seed makes the scalar callback result tensor-valued, and
-- the surrounding higher-order function feeds that result back, propagate the
-- lift to callback parameters whose expected type is the callback result type.
callbackLiftMask ::
    ClassEnv
    -> [Constraint]
    -> Bool
    -> [Type]
    -> Type
    -> [Type]
    -> Type
    -> [Bool]
callbackLiftMask classEnv constraints resultFeedsBack expectedParams expectedResult actualParams actualResult =
  let initialMask = zipWith (isDirectLiftSeed classEnv constraints) expectedParams actualParams
      resultCanBecomeTensor mask =
        any id mask && isTensorLiftableScalarType classEnv constraints actualResult
      step mask =
        zipWith3
          (\already expectedParam actualParam ->
             already ||
             ( resultFeedsBack
             && resultCanBecomeTensor mask
             && sameNormalizedType expectedParam expectedResult
             && isTensorLiftableScalarType classEnv constraints actualParam))
          mask
          expectedParams
          actualParams
      go mask =
        let mask' = step mask
        in if mask' == mask then mask else go mask'
  in go initialMask

-- | Check if a binary scalar function can use the compatibility tensorMap2
-- wrapper. This keeps polymorphic scalar callbacks such as `foldl (*)` working
-- even when the callback type itself does not mention Tensor yet.
shouldUseTensorMap2Fallback :: ClassEnv -> [Constraint] -> Type -> Bool
shouldUseTensorMap2Fallback classEnv constraints ty =
  case collectFunctionType ty of
    ([param1, param2], _result) ->
      isTensorLiftableScalarType classEnv constraints param1 &&
      isTensorLiftableScalarType classEnv constraints param2
    _ -> False

-- | Compatibility wrapper for binary scalar callbacks.
wrapWithTensorMap2Fallback :: TIExpr -> TIExpr
wrapWithTensorMap2Fallback funcExpr =
  case tiExprType funcExpr of
    TFun param1 (TFun param2 result) ->
      let varName1 = "tmap2_arg1"
          varName2 = "tmap2_arg2"
          var1 = Var varName1 []
          var2 = Var varName2 []
          var1TI = mkVarTIExpr varName1 (TTensor param1)
          var2TI = mkVarTIExpr varName2 (TTensor param2)
          innerType = normalizeTensorType (TTensor result)
          innerExpr = TIExpr (Forall [] [] innerType) (TITensorMap2Expr funcExpr var1TI var2TI)
          lambdaType = TFun (TTensor param1) (TFun (TTensor param2) innerType)
          lambdaScheme = Forall [] [] lambdaType
      in TIExpr lambdaScheme (TILambdaExpr Nothing [var1, var2] innerExpr)
    _ -> funcExpr

-- | Check if a lambda already has a tensorMap/tensorMap2 body.
isAlreadyWrappedWithTensorMap :: TIExprNode -> Bool
isAlreadyWrappedWithTensorMap (TILambdaExpr _ _ body) =
  case tiExprNode body of
    TITensorMapExpr _ _ -> True
    TITensorMap2Expr _ _ _ -> True
    _ -> False
isAlreadyWrappedWithTensorMap _ = False

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

-- | Wrap a higher-order scalar function according to the callback type expected
-- by the call site. If the expected callback receives a tensor where the
-- supplied function receives a scalar, generate an eta-expanded wrapper that
-- maps the scalar function over that tensor argument.
wrapWithTypeDirectedTensorLift :: ClassEnv -> [Constraint] -> Type -> Int -> Type -> TIExpr -> Maybe TIExpr
wrapWithTypeDirectedTensorLift classEnv constraints outerFuncType callbackArgIndex expectedCallbackType funcExpr =
  let actualFuncType = tiExprType funcExpr
      (expectedParams, expectedResult) = collectFunctionType expectedCallbackType
      (actualParams, actualResult) = collectFunctionType actualFuncType
      arity = length expectedParams
  in if arity == 0 || length actualParams < arity
       then Nothing
       else
         let actualParamsForArity = take arity actualParams
             resultFeedsBack =
               callbackResultFeedsBack outerFuncType callbackArgIndex expectedCallbackType
             liftMask =
               callbackLiftMask
                 classEnv
                 constraints
                 resultFeedsBack
                 expectedParams
                 expectedResult
                 actualParamsForArity
                 actualResult
             callbackParams =
               zipWith4
                 (\index _expectedParam actualParam needsLift ->
                    let
                        outerType = if needsLift then TTensor actualParam else actualParam
                        outerName = "tmap_arg" ++ show (index + 1)
                        outerVar = Var outerName []
                        outerExpr = mkVarTIExpr outerName outerType
                    in CallbackParamPlan index actualParam outerType outerVar outerExpr needsLift)
                 [0..]
                 expectedParams
                 actualParamsForArity
                 liftMask
             liftedParams = filter callbackParamNeedsLift callbackParams
         in if null liftedParams
              then Nothing
              else
                let resultType = applyNArgType actualFuncType arity
                    body = buildTypeDirectedTensorLiftBody funcExpr resultType callbackParams liftedParams []
                    lambdaType = buildFunctionType (map callbackParamOuterType callbackParams) (tiExprType body)
                    lambdaScheme = Forall [] [] lambdaType
                    lambdaNode = TILambdaExpr Nothing (map callbackParamOuterVar callbackParams) body
                in Just $ TIExpr lambdaScheme lambdaNode

-- | Build the body of a generated callback wrapper.
buildTypeDirectedTensorLiftBody ::
    TIExpr
    -> Type
    -> [CallbackParamPlan]
    -> [CallbackParamPlan]
    -> [(Int, TIExpr)]
    -> TIExpr
buildTypeDirectedTensorLiftBody funcExpr resultType callbackParams [] scalarArgs =
  let argFor param =
        case lookup (callbackParamIndex param) scalarArgs of
          Just scalarArg -> scalarArg
          Nothing -> callbackParamOuterExpr param
      args = map argFor callbackParams
  in TIExpr (Forall [] [] resultType) (TIApplyExpr funcExpr args)
buildTypeDirectedTensorLiftBody funcExpr resultType callbackParams [param] scalarArgs =
  let index = callbackParamIndex param
      scalarName = "tmap_elem" ++ show (index + 1)
      scalarVar = Var scalarName []
      scalarExpr = mkVarTIExpr scalarName (callbackParamActualType param)
      inner = buildTypeDirectedTensorLiftBody
                funcExpr
                resultType
                callbackParams
                []
                ((index, scalarExpr) : scalarArgs)
      lambdaType = TFun (callbackParamActualType param) (tiExprType inner)
      lambdaExpr = TIExpr (Forall [] [] lambdaType) (TILambdaExpr Nothing [scalarVar] inner)
      mappedType = normalizeTensorType (TTensor (tiExprType inner))
  in TIExpr (Forall [] [] mappedType) (TITensorMapExpr lambdaExpr (callbackParamOuterExpr param))
buildTypeDirectedTensorLiftBody funcExpr resultType callbackParams (param1:param2:restParams) scalarArgs =
  let index1 = callbackParamIndex param1
      index2 = callbackParamIndex param2
      scalarName1 = "tmap_elem" ++ show (index1 + 1)
      scalarName2 = "tmap_elem" ++ show (index2 + 1)
      scalarVar1 = Var scalarName1 []
      scalarVar2 = Var scalarName2 []
      scalarExpr1 = mkVarTIExpr scalarName1 (callbackParamActualType param1)
      scalarExpr2 = mkVarTIExpr scalarName2 (callbackParamActualType param2)
      inner = buildTypeDirectedTensorLiftBody
                funcExpr
                resultType
                callbackParams
                restParams
                ((index2, scalarExpr2) : (index1, scalarExpr1) : scalarArgs)
      lambdaType =
        TFun (callbackParamActualType param1)
             (TFun (callbackParamActualType param2) (tiExprType inner))
      lambdaExpr =
        TIExpr (Forall [] [] lambdaType) (TILambdaExpr Nothing [scalarVar1, scalarVar2] inner)
      mappedType = normalizeTensorType (TTensor (tiExprType inner))
  in TIExpr
       (Forall [] [] mappedType)
       (TITensorMap2Expr lambdaExpr (callbackParamOuterExpr param1) (callbackParamOuterExpr param2))

-- | Wrap a higher-order function argument with tensorMap/tensorMap2 if needed.
-- This implements the simplified approach from tensor-map-insertion-simple.md
wrapFunctionArgumentIfNeeded :: ClassEnv -> [Constraint] -> Type -> Int -> Maybe Type -> TIExpr -> TIExpr
wrapFunctionArgumentIfNeeded classEnv constraints outerFuncType argIndex expectedArgType tiExpr =
  let node = tiExprNode tiExpr
      mBinaryFallback =
        case node of
          TIApplyExpr {} -> Nothing
          _ | shouldUseTensorMap2Fallback classEnv constraints (tiExprType tiExpr) ->
                Just (wrapWithTensorMap2Fallback tiExpr)
            | otherwise -> Nothing
  in if isAlreadyWrappedWithTensorMap node
       then tiExpr
       else
         case expectedArgType of
           Just expectedType ->
             case wrapWithTypeDirectedTensorLift classEnv constraints outerFuncType argIndex expectedType tiExpr of
               Just wrappedExpr -> wrappedExpr
               Nothing ->
                 case mBinaryFallback of
                   Just binaryFallback -> binaryFallback
                   Nothing -> tiExpr
           Nothing ->
             case mBinaryFallback of
               Just binaryFallback -> binaryFallback
               Nothing -> tiExpr

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
        -- and `map f xs` where f is a unary scalar function that may receive tensor elements.
        -- But `foldl1 (.) [t1, t2]` should not be wrapped with tensorMap2 because (.) is a binary function that takes two tensor arguments
        -- IMPORTANT: Include each argument's own constraints when deciding if it needs wrapping
        let (Forall _ funcConstraints _) = tiScheme func'
            baseConstraints = cs ++ funcConstraints
            -- For each argument, merge base constraints with the argument's own constraints
            funcType = tiExprType func'
            wrapArg (index, arg) =
              let (Forall _ argConstraints _) = tiScheme arg
                  argAllConstraints = nub (baseConstraints ++ argConstraints)
                  expectedArgType = getParamType funcType index
              in wrapFunctionArgumentIfNeeded env argAllConstraints funcType index expectedArgType arg
            args'' = map wrapArg (zip [0..] args')

        -- Use the INFERRED function type (after type inference)
        -- This ensures we use concrete types like Integer instead of type variables like a
        -- For example, (+) has inferred type {Num Integer} Integer -> Integer -> Integer
        -- instead of the polymorphic type {Num a} a -> a -> a
        let argTypes = map tiExprType args''

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

      TITensorMap2WedgeExpr func t1 t2 -> do
        func' <- insertTensorMapsWithConstraints env cs func
        t1' <- insertTensorMapsWithConstraints env cs t1
        t2' <- insertTensorMapsWithConstraints env cs t2
        return $ TITensorMap2WedgeExpr func' t1' t2'

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

        -- Check if the function's parameter types are NOT Tensor types
        -- If so, insert tensorMap2Wedge; otherwise, keep WedgeApply
        let funcType = tiExprType func'
            -- Check if this is a binary function with non-Tensor parameters
            -- A type is non-Tensor if it's not TTensor _ (could be TVar, TBase, etc.)
            isNonTensorType ty = case ty of
              TTensor _ -> False
              _ -> True
            isScalarFunction = case funcType of
              TFun param1 (TFun param2 _result) ->
                isNonTensorType param1 && isNonTensorType param2
              _ -> False

        case (isScalarFunction, args') of
          (True, [arg1, arg2]) -> do
            -- Insert tensorMap2Wedge for binary scalar functions
            let -- Preserve the function's original scheme with its constraints
                (Forall tvs funcConstraints _) = tiScheme func'
                -- Unlift the function type to get the scalar version
                unliftedFuncType = unliftFunctionType funcType
                unliftedFunc = TIExpr (Forall tvs funcConstraints unliftedFuncType) (tiExprNode func')
            return $ TITensorMap2WedgeExpr unliftedFunc arg1 arg2
          _ ->
            -- Keep WedgeApply for tensor functions or non-binary functions
            return $ TIWedgeApplyExpr func' args'
      
      TIFunctionExpr names -> return $ TIFunctionExpr names

      -- Runtime dispatch: traverse arguments, leave class/method/candidates intact.
      TIRuntimeDispatch className methodName candidates args -> do
        args' <- mapM (insertTensorMapsWithConstraints env cs) args
        return $ TIRuntimeDispatch className methodName candidates args'

      -- Reshape: traverse the inner expression; type annotation is metadata.
      TIReshape ty inner -> do
        inner' <- insertTensorMapsWithConstraints env cs inner
        return $ TIReshape ty inner'

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
