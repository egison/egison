-- Regressions for the contract between inference and tensorMap insertion.
-- Concrete constant callbacks expose missing maps even when arithmetic
-- primitives happen to accept tensors without an inserted wrapper.
module TensorInference (tensorInferenceTests) where

import qualified Data.Set as Set
import Test.HUnit

import Language.Egison (ConstantExpr(..), defaultOption, fromEvalM)
import Language.Egison.AST (PDPatternBase(..))
import Language.Egison.IExpr
  (IExpr(..), TIExpr(..), TIExprNode(..), Var(..), tiExprType)
import qualified Language.Egison.Type.Env as Env
import Language.Egison.Type.Infer
  (InferState(..), inferIExpr, initialInferState, runInferWithWarnings)
import Language.Egison.Type.Tensor (normalizeTensorType)
import Language.Egison.Type.TensorMapInsertion (insertTensorMaps)
import Language.Egison.Type.Types
  (Type(..), TypeScheme(..), TyVar(..), freeTyVars)

tensorInferenceTests :: Test
tensorInferenceTests = TestLabel "Tensor inference and insertion" $ TestList $
  map checkCase cases ++
  map rejectCase
    [ ("incompatible explicit scalar operand", ITensorMapExpr (var "toBool") (IConstantExpr (BoolExpr True)))
    , ("incompatible explicit mixed operand", ITensorMap2Expr (var "toBool2") (int 1)
        (IVectorExpr [IConstantExpr (BoolExpr True)]))
    , ("list Tensor is not erased into a scalar list", call "scalarList" [tensors])
    , ("tensor predicate cannot replace a scalar predicate", call "filter" [var "toBool", tensors])
    ] ++
  [ TestLabel "incompatible tensor element is rejected" . TestCase $ do
      (result, _) <- runInferWithWarnings
        (inferIExpr (call "map" [var "one", ICollectionExpr
          [IVectorExpr [IConstantExpr (BoolExpr True)]]])) inferenceState
      case result of
        Left _ -> return ()
        Right typed -> assertFailure ("accepted Integer callback on Bool: " ++ show typed)
  ]
  where
    cases =
      [ ("unary map", call "map" [var "one", tensors], listTensor,
          Just (0, arrow [tensor] tensor), True)
      , ("data-first map", call "mapDataFirst" [tensors, var "one"], listTensor,
          Just (1, arrow [tensor] tensor), True)
      , ("tuple elements", call "pairMap" [var "one", ITupleExpr [vector, vector]],
          TTuple [tensor, tensor], Just (0, arrow [tensor] tensor), True)
      , ("binary fold", call "foldl1" [var "two", tensors], tensor,
          Just (0, arrow [tensor, tensor] tensor), True)
      , ("scalar fold initializer", call "foldl" [var "two", int 0, tensors], tensor,
          Just (0, arrow [tensor, tensor] tensor), True)
      , ("scalar scan initializer", call "scanl" [var "two", int 0, tensors], listTensor,
          Just (0, arrow [tensor, tensor] tensor), True)
      , ("mixed binary map", call "map2" [var "two", tensors, scalars], listTensor,
          Just (0, arrow [tensor, TInt] tensor), True)
      , ("ternary map", call "map3" [var "three", tensors, scalars, tensors], listTensor,
          Just (0, arrow [tensor, TInt, tensor] tensor), True)
      , ("partial scalar callback", call "map" [call "two" [int 7], tensors], listTensor,
          Just (0, arrow [tensor] tensor), True)
      , ("a scalar map body does not suppress the required outer wrapper",
          call "map" [lambda ["x"] (ITensorMapExpr (lambda ["y"] (call "one" [var "x"])) (int 0)), tensors],
          listTensor, Just (0, arrow [tensor] tensor), True)
      , ("ordinary scalar map", call "map" [var "one", scalars], TCollection TInt,
          Just (0, arrow [TInt] TInt), False)
      , ("polymorphic identity consumes tensors", call "map" [var "id", tensors], listTensor,
          Just (0, arrow [tensor] tensor), False)
      , ("explicit tensor callback consumes whole tensors", call "map" [var "whole", tensors],
          TCollection TInt, Just (0, arrow [tensor] TInt), False)
      , ("tensor callback determines rank-zero initializer", call "iterate" [var "tensorIdentity", int 0],
          listTensor, Just (0, arrow [tensor] tensor), False)
      , ("rank-zero admission keeps list result", call "rankZeroList" [int 0], listTensor,
          Nothing, False)
      , ("direct tensor mapping keeps tuple inside Tensor", call "scalarPair" [vector],
          TTensor (TTuple [TInt, TInt]), Nothing, True)
      , ("explicit scalar map uses function result", ITensorMapExpr (var "toBool") (int 1),
          TBool, Nothing, True)
      , ("explicit tensor map uses function result", ITensorMapExpr (var "toBool") vector,
          TTensor TBool, Nothing, True)
      , ("explicit binary scalar map uses function result", ITensorMap2Expr (var "toBool2") (int 1) (int 2),
          TBool, Nothing, True)
      , ("explicit binary scalar/tensor map", ITensorMap2Expr (var "toBool2") (int 1) vector,
          TTensor TBool, Nothing, True)
      , ("explicit binary tensor/scalar map", ITensorMap2Expr (var "toBool2") vector (int 1),
          TTensor TBool, Nothing, True)
      , ("explicit binary tensor/tensor map", ITensorMap2Expr (var "toBool2") vector vector,
          TTensor TBool, Nothing, True)
      , ("explicit map normalizes tensor result", ITensorMapExpr (var "tensorResult") vector,
          tensor, Nothing, True)
      , ("unresolved explicit map has a checked Tensor domain",
          lambda ["x"] (ITensorMapExpr (var "toBool") (var "x")),
          arrow [tensor] (TTensor TBool), Nothing, True)
      , ("recursive let propagates a scalar function before its use",
          recursiveOnes False, listTensor, Nothing, True)
      , ("recursive let follows forward function dependencies",
          recursiveOnes True, listTensor, Nothing, True)
      , ("let-bound completion called on tensors",
          letOnes (call "ones" [tensors]), listTensor, Nothing, True)
      , ("let-bound completion called on scalars",
          letOnes (call "ones" [scalars]), listTensor, Nothing, True)
      , ("definition-time unary completion", lambda ["xs"] (call "map" [var "one", var "xs"]),
          arrow [listTensor] listTensor, Nothing, True)
      , ("definition-time binary completion", lambda ["xs"] (call "foldl1" [var "two", var "xs"]),
          arrow [listTensor] tensor, Nothing, True)
      , ("definition-time scalar initializer", lambda ["xs"] (call "foldl" [var "two", int 0, var "xs"]),
          arrow [listTensor] tensor, Nothing, True)
      , ("definition-time ternary completion",
          lambda ["xs", "ys", "zs"] (call "map3" [var "three", var "xs", var "ys", var "zs"]),
          arrow [listTensor, listTensor, listTensor] listTensor, Nothing, True)
      , ("definition-time partially applied callback",
          lambda ["xs"] (call "map" [call "two" [int 7], var "xs"]),
          arrow [listTensor] listTensor, Nothing, True)
      , ("scalar predicate does not trigger definition-time completion",
          lambda ["xs"] (call "filter" [var "toBool", var "xs"]),
          arrow [TCollection TInt] (TCollection TInt), Nothing, False)
      , ("definition-time completion keeps lists inside Tensor results",
          lambda ["xs"] (call "map" [var "scalarListResult", var "xs"]),
          arrow [listTensor] (TCollection (TTensor (TCollection TInt))), Nothing, True)
      ]

    checkCase (label, source, expectedResult, callback, needsMap) =
      TestLabel label . TestCase $ do
        (inferred, warnings) <- runInferWithWarnings (inferIExpr source) inferenceState
        assertEqual "inference emits no warnings" [] warnings
        case inferred of
          Left err -> assertFailure (show err)
          Right (before, _) -> do
            assertEqual "inferred result type" expectedResult (tiExprType before)
            inserted <- fromEvalM defaultOption (insertTensorMaps before)
            case inserted of
              Left err -> assertFailure (show err)
              Right after -> do
                assertEqual "insertion preserves result type" expectedResult (tiExprType after)
                assertEqual "tensorMap/tensorMap2 insertion" needsMap (hasTensorMap after)
                case callback of
                  Nothing -> return ()
                  Just (index, expectedCallback) ->
                    case (tiExprNode before, tiExprNode after) of
                      (TIApplyExpr fn _, TIApplyExpr fn' args') -> do
                        assertEqual "inference already records the lifted callback expectation"
                          (Just expectedCallback) (parameterAt index (tiExprType fn))
                        assertEqual "consumer and inserted callback agree"
                          (parameterAt index (tiExprType fn'))
                          (Just (tiExprType (args' !! index)))
                        assertEqual "consumer result agrees with application"
                          expectedResult (resultAfter (length args') (tiExprType fn'))
                      other -> assertFailure ("unexpected higher-order application: " ++ show other)

    rejectCase (label, source) = TestLabel label . TestCase $ do
      (inferred, _) <- runInferWithWarnings (inferIExpr source) inferenceState
      case inferred of
        Left _ -> return ()
        Right typed -> assertFailure ("accepted incompatible tensor application: " ++ show typed)

    recursiveOnes forward =
      let bindings =
            [(PDPatVar (Var "keptOne" []), var "one"),
             (PDPatVar (Var "ones" []), lambda ["xs"] (call "map" [var "keptOne", var "xs"]))]
      in ILetRecExpr (if forward then reverse bindings else bindings) (call "ones" [tensors])
    letOnes body = ILetExpr
      [(PDPatVar (Var "ones" []), lambda ["xs"] (call "map" [var "one", var "xs"]))] body
    tensor = TTensor TInt
    listTensor = TCollection tensor
    vector = IVectorExpr [int 1, int 2]
    tensors = ICollectionExpr [vector, vector]
    scalars = ICollectionExpr [int 3, int 4]

inferenceState :: InferState
inferenceState = initialInferState { inferEnv = foldr add Env.emptyEnv bindings }
  where
    add (name, ty) = Env.extendEnv (Var name [])
      (Forall [] (Set.toList (freeTyVars ty)) [] ty)
    a = TVar (TyVar "a")
    b = TVar (TyVar "b")
    c = TVar (TyVar "c")
    d = TVar (TyVar "d")
    tensor = TTensor TInt
    bindings =
      [ ("map", arrow [arrow [a] b, TCollection a] (TCollection b))
      , ("iterate", arrow [arrow [a] a, a] (TCollection a))
      , ("tensorIdentity", arrow [tensor] tensor)
      , ("filter", arrow [arrow [a] TBool, TCollection a] (TCollection a))
      , ("mapDataFirst", arrow [TCollection a, arrow [a] b] (TCollection b))
      , ("pairMap", arrow [arrow [a] b, TTuple [a, a]] (TTuple [b, b]))
      , ("map2", arrow [arrow [a, b] c, TCollection a, TCollection b] (TCollection c))
      , ("map3", arrow [arrow [a, b, c] d, TCollection a, TCollection b, TCollection c] (TCollection d))
      , ("foldl1", arrow [arrow [a, a] a, TCollection a] a)
      , ("foldl", arrow [arrow [b, a] b, b, TCollection a] b)
      , ("scanl", arrow [arrow [b, a] b, b, TCollection a] (TCollection b))
      , ("one", arrow [TInt] TInt)
      , ("two", arrow [TInt, TInt] TInt)
      , ("three", arrow [TInt, TInt, TInt] TInt)
      , ("id", arrow [a] a)
      , ("whole", arrow [tensor] TInt)
      , ("rankZeroList", arrow [tensor] (TCollection tensor))
      , ("scalarPair", arrow [TInt] (TTuple [TInt, TInt]))
      , ("toBool", arrow [TInt] TBool)
      , ("toBool2", arrow [TInt, TInt] TBool)
      , ("tensorResult", arrow [TInt] tensor)
      , ("scalarList", arrow [TCollection TInt] (TCollection TInt))
      , ("scalarListResult", arrow [TInt] (TCollection TInt))
      ]

arrow :: [Type] -> Type -> Type
arrow = flip (foldr TFun)

int :: Integer -> IExpr
int = IConstantExpr . IntegerExpr

var :: String -> IExpr
var = IVarExpr

call :: String -> [IExpr] -> IExpr
call = IApplyExpr . var

lambda :: [String] -> IExpr -> IExpr
lambda names = ILambdaExpr Nothing [Var name [] | name <- names]

parameterAt :: Int -> Type -> Maybe Type
parameterAt 0 (TFun param _) = Just param
parameterAt n (TFun _ result) = parameterAt (n - 1) result
parameterAt _ _ = Nothing

resultAfter :: Int -> Type -> Type
resultAfter 0 ty = normalizeTensorType ty
resultAfter n (TFun _ result) = resultAfter (n - 1) result
resultAfter _ ty = ty

hasTensorMap :: TIExpr -> Bool
hasTensorMap expression = case tiExprNode expression of
  TITensorMapExpr{} -> True
  TITensorMap2Expr{} -> True
  TIApplyExpr fn args -> any hasTensorMap (fn : args)
  TILambdaExpr _ _ body -> hasTensorMap body
  TILetExpr bindings body -> any (hasTensorMap . snd) bindings || hasTensorMap body
  TILetRecExpr bindings body -> any (hasTensorMap . snd) bindings || hasTensorMap body
  TITupleExpr elements -> any hasTensorMap elements
  TICollectionExpr elements -> any hasTensorMap elements
  TIVectorExpr elements -> any hasTensorMap elements
  _ -> False
