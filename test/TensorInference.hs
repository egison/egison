-- Regressions for the contract between inference and tensorMap insertion.
-- Concrete constant callbacks expose missing maps even when arithmetic
-- primitives happen to accept tensors without an inserted wrapper.
module TensorInference (tensorInferenceTests) where

import qualified Data.Set as Set
import Test.HUnit

import Language.Egison (ConstantExpr(..), defaultOption, fromEvalM)
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
      , ("ordinary scalar map", call "map" [var "one", scalars], TCollection TInt,
          Just (0, arrow [TInt] TInt), False)
      , ("polymorphic identity consumes tensors", call "map" [var "id", tensors], listTensor,
          Just (0, arrow [tensor] tensor), False)
      , ("explicit tensor callback consumes whole tensors", call "map" [var "whole", tensors],
          TCollection TInt, Just (0, arrow [tensor] TInt), False)
      , ("rank-zero admission keeps list result", call "rankZeroList" [int 0], listTensor,
          Nothing, False)
      , ("direct tensor mapping keeps tuple inside Tensor", call "scalarPair" [vector],
          TTensor (TTuple [TInt, TInt]), Nothing, True)
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
      ]

arrow :: [Type] -> Type -> Type
arrow = flip (foldr TFun)

int :: Integer -> IExpr
int = IConstantExpr . IntegerExpr

var :: String -> IExpr
var = IVarExpr

call :: String -> [IExpr] -> IExpr
call = IApplyExpr . var

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
  TITupleExpr elements -> any hasTensorMap elements
  TICollectionExpr elements -> any hasTensorMap elements
  TIVectorExpr elements -> any hasTensorMap elements
  _ -> False
