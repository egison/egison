-- Regressions for the contract between inference and tensorMap insertion.
-- Concrete constant callbacks expose missing maps even when arithmetic
-- primitives happen to accept tensors without an inserted wrapper.
module TensorInference (tensorInferenceTests) where

import qualified Data.Set as Set
import Test.HUnit

import Language.Egison (ConstantExpr(..), defaultOption, fromEvalM)
import Language.Egison.AST (PDPatternBase(..))
import Language.Egison.IExpr
  (IExpr(..), ITopExpr(..), TITopExpr(..), TIExpr(..), TIExprNode(..), Var(..), Index(..), tiExprType)
import qualified Language.Egison.Type.Env as Env
import Language.Egison.Type.Infer
  (InferState(..), inferIExpr, inferITopExpr, freshVar, initialInferState, runInferWithWarnings)
import Language.Egison.Type.Tensor (normalizeTensorType)
import Language.Egison.Type.TensorMapInsertion (insertTensorMaps)
import Language.Egison.Type.Types
  (Type(..), TypeScheme(..), Constraint(..), TyVar(..), freeTyVars)

tensorInferenceTests :: Test
tensorInferenceTests = TestLabel "Tensor inference and insertion" $ TestList $
  [TestLabel "numbered fresh-variable prefixes cannot collide" . TestCase $ do
     (generated,_) <- runInferWithWarnings (do
       first <- freshVar "param1"
       _ <- mapM (const (freshVar "padding")) [1 :: Int ..99]
       second <- freshVar "param"
       return (first,second)) initialInferState { inferCounter = 23 }
     case generated of
       Left err -> assertFailure (show err)
       Right (first,second) -> assertBool "distinct fresh identities" (first /= second)] ++
  [TestLabel "top-level tests resolve deferred callback reports before insertion" . TestCase $ do
     let source = ILetExpr [(PDPatVar (Var "partialMap" []),lambda ["xs"]
           (call "map" [call "two" [int 7],var "xs"]))]
           (call "partialMap" [tensors])
         expected = TCollection tensor
     (inferred,_) <- runInferWithWarnings (inferITopExpr (ITest source)) inferenceState
     case inferred of
       Right (Just (TITest before),_) -> do
         assertEqual "resolved top-level result" expected (tiExprType before)
         inserted <- fromEvalM defaultOption (insertTensorMaps before)
         case inserted of
           Left err -> assertFailure (show err)
           Right after -> assertBool "retained callback has its map" (hasTensorMap after)
       other -> assertFailure (show other)] ++
  map checkCase cases ++
  map rejectCase
    [ ("incompatible explicit scalar operand", ITensorMapExpr (var "toBool") (IConstantExpr (BoolExpr True)))
    , ("incompatible explicit mixed operand", ITensorMap2Expr (var "toBool2") (int 1)
        (IVectorExpr [IConstantExpr (BoolExpr True)]))
    , ("list Tensor is not erased into a scalar list", call "scalarList" [tensors])
    , ("nested map does not erase tensor elements",
        call "scalarList" [call "map" [var "one", tensors]])
    , ("nested MathValue map does not erase tensor elements",
        call "mathList" [call "map" [var "mathConstant", var "mathTensors"]])
    , ("nested MathValue function result does not erase Tensor",
        call "mathFunctionConsumer" [call "id" [var "mathTensorResult"]])
    , ("tensor predicate cannot replace a scalar predicate", call "filter" [var "toBool", tensors])
    , ("a scalar predicate body is not reinterpreted as tensor input",
        call "filter" [lambda ["x"] (ISeqExpr (call "one" [var "x"]) (IConstantExpr (BoolExpr True))), tensors])
    , ("a scalar component cannot join tensor components", IVectorExpr [int 1, vector])
    , ("a tensor component cannot join scalar components", IVectorExpr [vector, int 1])
    , ("joined tensor elements cannot enter a scalar list consumer",
        call "scalarList" [IJoinExpr scalars tensors])
    , ("constructed tensor elements cannot enter a scalar list consumer",
        call "scalarList" [ICollectionExpr [int 1, vector]])
    , ("tensor of lists is not a concatenation operand",
        IJoinExpr (IVectorExpr [scalars, scalars]) scalars)
    , ("common list elements do not admit an infinite list type",
        lambda ["x"] (ICollectionExpr [var "x", ICollectionExpr [var "x"]]))
    , ("Tensor normalization does not hide an infinite list type",
        lambda ["x"] (ICollectionExpr [var "x", IVectorExpr [ICollectionExpr [var "x"]]]))
    ] ++
  [ TestLabel "standalone open consumer has a polymorphic answer" . TestCase $ do
      (inferred, warnings) <- runInferWithWarnings
        (inferIExpr (lambda ["g"] (IApplyExpr (var "g") [var "one"]))) inferenceState
      assertEqual "inference emits no warnings" [] warnings
      case inferred of
        Left err -> assertFailure (show err)
        Right (before, _) -> do
          let completed ty = case ty of
                TFun (TFun (TFun domain result) answer@(TVar _)) answer' ->
                  domain == TInt && result == TInt && answer == answer'
                _ -> False
          assertBool "((Integer -> Integer) -> a) -> a" (completed (tiExprType before))
          inserted <- fromEvalM defaultOption (insertTensorMaps before)
          case inserted of
            Left err -> assertFailure (show err)
            Right after -> do
              assertBool "insertion preserves the polymorphic answer" (completed (tiExprType after))
              assertBool "the ordinary callback needs no map" (not (hasTensorMap after))
  , TestLabel "standalone unknown function receives the whole tensor" . TestCase $ do
      (inferred, warnings) <- runInferWithWarnings
        (inferIExpr (lambda ["k"] (IApplyExpr (var "k") [vector]))) inferenceState
      assertEqual "inference emits no warnings" [] warnings
      case inferred of
        Left err -> assertFailure (show err)
        Right (before, _) -> case tiExprType before of
          TFun (TFun domain answer@(TVar _)) answer' -> do
            assertEqual "whole tensor domain" tensor domain
            assertEqual "unwrapped answer" answer answer'
            assertBool "no speculative map" (not (hasTensorMap before))
          ty -> assertFailure ("unexpected whole-tensor function type: " ++ show ty)
  , TestLabel "incompatible tensor element is rejected" . TestCase $ do
      (result, _) <- runInferWithWarnings
        (inferIExpr (call "map" [var "one", ICollectionExpr
          [IVectorExpr [IConstantExpr (BoolExpr True)]]])) inferenceState
      case result of
        Left _ -> return ()
        Right typed -> assertFailure ("accepted Integer callback on Bool: " ++ show typed)
  , TestLabel "ordinary list construction remains polymorphic" . TestCase $ do
      (inferred, _) <- runInferWithWarnings
        (inferIExpr (lambda ["x"] (ICollectionExpr [var "x"]))) inferenceState
      case inferred of
        Right (typed, _) -> case tiExprType typed of
          TFun input@TVar{} (TCollection element) ->
            assertEqual "a -> [a]" input element
          ty -> assertFailure ("unexpected polymorphic list type: " ++ show ty)
        Left err -> assertFailure (show err)
  , TestLabel "empty-list concatenation remains polymorphic" . TestCase $ do
      (inferred, _) <- runInferWithWarnings
        (inferIExpr (lambda ["xs"] (IJoinExpr (ICollectionExpr []) (var "xs")))) inferenceState
      case inferred of
        Right (typed, _) -> case tiExprType typed of
          TFun (TCollection input@TVar{}) (TCollection element) ->
            assertEqual "[a] -> [a]" input element
          ty -> assertFailure ("unexpected polymorphic concatenation type: " ++ show ty)
        Left err -> assertFailure (show err)
  , TestLabel "ordinary conditional remains polymorphic" . TestCase $ do
      (inferred, _) <- runInferWithWarnings
        (inferIExpr (lambda ["x"] (conditional (var "x") (var "x")))) inferenceState
      case inferred of
        Right (typed, _) -> case tiExprType typed of
          TFun input@TVar{} output -> assertEqual "a -> a" input output
          ty -> assertFailure ("unexpected polymorphic conditional type: " ++ show ty)
        Left err -> assertFailure (show err)
  , TestLabel "rank-zero admission does not invent a recursive type" . TestCase $ do
      (inferred, _) <- runInferWithWarnings
        (inferIExpr (lambda ["x"] (ICollectionExpr [var "x", IVectorExpr [var "x"]]))) inferenceState
      case inferred of
        Right (typed, _) -> case tiExprType typed of
          TFun input@TVar{} (TCollection (TTensor element)) ->
            assertEqual "a -> [Tensor a]" input element
          ty -> assertFailure ("unexpected shared element type: " ++ show ty)
        Left err -> assertFailure (show err)
  , TestLabel "recursive list results respect Tensor normalization" . TestCase $ do
      let empty = ICollectionExpr []
          prefix = call "prefix" [empty]
          base = ICollectionExpr [ITupleExpr [prefix, empty]]
          recursive = IConsExpr (ITupleExpr [prefix, var "xs"])
            (call "walk" [lambda ["ys"]
              (call "prefix" [IConsExpr (int 1) (var "ys")]), var "xs"])
          definition = lambda ["prefix", "xs"] (conditional base recursive)
          source = ILetRecExpr [(PDPatVar (Var "walk" []), definition)] (var "walk")
      (inferred, warnings) <- runInferWithWarnings (inferIExpr source) inferenceState
      assertEqual "recursive inference emits no warnings" [] warnings
      case inferred of
        Right _ -> return ()
        Left err -> assertFailure (show err)
  ]
  where
    cases =
      [ ("concatenation preserves a right tensor element",
          IJoinExpr scalars tensors,
          listTensor,
          Nothing,
          False)
      , ("concatenation preserves a left tensor element",
          IJoinExpr tensors scalars,
          listTensor,
          Nothing,
          False)
      , ("list construction preserves a later tensor element",
          ICollectionExpr [int 1, vector],
          listTensor,
          Nothing,
          False)
      , ("list construction preserves an earlier tensor element",
          ICollectionExpr [vector, int 1],
          listTensor,
          Nothing,
          False)
      , ("tensor head promotes a scalar list tail",
          IConsExpr vector scalars,
          listTensor,
          Nothing,
          False)
      , ("scalar head retains a tensor list tail",
          IConsExpr (int 1) tensors,
          listTensor,
          Nothing,
          False)
      , ("conditional preserves a tensor else branch",
          conditional scalars tensors,
          listTensor,
          Nothing,
          False)
      , ("conditional preserves a tensor then branch",
          conditional tensors scalars,
          listTensor,
          Nothing,
          False)
      , ("nested lists preserve tensor elements",
          ICollectionExpr [scalars, tensors],
          TCollection listTensor,
          Nothing,
          False)
      , ("tuple elements preserve their tensor positions",
          ICollectionExpr [ITupleExpr [int 1, int 2], ITupleExpr [vector, int 3]],
          TCollection (TTuple [tensor, TInt]),
          Nothing,
          False)
      , ("tensor alternatives preserve outer Tensor",
          conditional (int 0) vector,
          tensor,
          Nothing,
          False)
      , ("ordinary list concatenation retains scalar types",
          IJoinExpr scalars scalars,
          TCollection TInt,
          Nothing,
          False)
      , ("ordinary scalar alternatives retain scalar types",
          conditional (int 0) (int 1),
          TInt,
          Nothing,
          False)
      , ("a shared unknown element receives the common Tensor type",
          lambda ["x"] (ICollectionExpr [var "x", int 1, vector]),
          arrow [tensor] listTensor,
          Nothing,
          False)
      , ("a shared unknown element before Tensor retains the same type",
          lambda ["x"] (ICollectionExpr [vector, int 1, var "x"]),
          arrow [tensor] listTensor,
          Nothing,
          False)
      , ("scalar element arithmetic retains its scalar domain",
          lambda ["x"] (ICollectionExpr [call "one" [var "x"], vector]),
          arrow [TInt] listTensor,
          Nothing,
          False)
      , ("a scalar consumer maps a joined tensor element",
          call "map" [var "one", IJoinExpr scalars tensors],
          listTensor,
          Just (0, arrow [tensor] tensor),
          True)
      , ("a scalar consumer maps a tensor-valued conditional",
          call "one" [conditional (int 0) vector],
          tensor,
          Nothing,
          True)
      , ("unary map",
          call "map" [var "one", tensors],
          listTensor,
          Just (0, arrow [tensor] tensor),
          True)
      , ("data-first map",
          call "mapDataFirst" [tensors, var "one"],
          listTensor,
          Just (1, arrow [tensor] tensor),
          True)
      , ("tuple elements",
          call "pairMap" [var "one", ITupleExpr [vector, vector]],
          TTuple [tensor, tensor],
          Just (0, arrow [tensor] tensor),
          True)
      , ("binary fold",
          call "foldl1" [var "two", tensors],
          tensor,
          Just (0, arrow [tensor, tensor] tensor),
          True)
      , ("scalar fold initializer",
          call "foldl" [var "two", int 0, tensors],
          tensor,
          Just (0, arrow [tensor, tensor] tensor),
          True)
      , ("scalar scan initializer",
          call "scanl" [var "two", int 0, tensors],
          listTensor,
          Just (0, arrow [tensor, tensor] tensor),
          True)
      , ("mixed binary map",
          call "map2" [var "two", tensors, scalars],
          listTensor,
          Just (0, arrow [tensor, TInt] tensor),
          True)
      , ("ternary map",
          call "map3" [var "three", tensors, scalars, tensors],
          listTensor,
          Just (0, arrow [tensor, TInt, tensor] tensor),
          True)
      , ("partial scalar callback",
          call "map" [call "two" [int 7], tensors],
          listTensor,
          Just (0, arrow [tensor] tensor),
          True)
      , ("a scalar map body does not suppress the required outer wrapper",
          call "map" [lambda ["x"] (ITensorMapExpr (lambda ["y"] (call "one" [var "x"])) (int 0)), tensors],
          listTensor,
          Just (0, arrow [tensor] tensor),
          True)
      , ("known collection result guides nested callback inference",
          lambda ["xss"] (call "scalarNestedList" [call "map" [var "scalarList", var "xss"]]),
          arrow [TCollection (TCollection TInt)] (TCollection (TCollection TInt)),
          Nothing,
          False)
      , ("known scalar list result avoids speculative tensor completion",
          lambda ["xs"] (call "scalarList" [call "map" [var "one", var "xs"]]),
          arrow [TCollection TInt] (TCollection TInt),
          Nothing,
          False)
      , ("known MathValue list result guides nested callback inference",
          lambda ["xs"] (call "mathList" [call "map" [var "mathConstant", var "xs"]]),
          arrow [TCollection TMathValue] (TCollection TMathValue),
          Nothing,
          False)
      , ("known nested MathValue lists specialize a polymorphic callback",
          lambda ["xss"] (call "mathNestedList" [call "map" [var "listIdentity", var "xss"]]),
          arrow [TCollection (TCollection TMathValue)] (TCollection (TCollection TMathValue)),
          Nothing,
          False)
      , ("known MathValue tuple result guides nested callback inference",
          lambda ["xs"] (call "mathPair" [call "pairMap" [var "mathConstant", var "xs"]]),
          arrow [TTuple [TMathValue, TMathValue]] (TTuple [TMathValue, TMathValue]),
          Nothing,
          False)
      , ("known MathValue scalar result guides nested callback inference",
          lambda ["x"] (call "mathConstant" [call "apply" [var "mathConstant", var "x"]]),
          arrow [TMathValue] TMathValue,
          Nothing,
          False)
      , ("MathValue callback still maps over explicit tensors",
          call "map" [var "mathConstant", var "mathTensors"],
          TCollection (TTensor TMathValue),
          Just (0, arrow [TTensor TMathValue] (TTensor TMathValue)),
          True)
      , ("scalar expectation retries ordinary inference for required tensor results",
          call "mathConstant" [call "foldl" [var "mathAdd", int 0,
            call "map" [var "mathTensorResult", scalars]]],
          TTensor TMathValue,
          Nothing,
          True)
      , ("a callback fixes an open data parameter to a scalar, which lifts the tensor argument",
          call "dataCallback" [vector, var "one"],
          tensor,
          Nothing,
          True)
      , ("a scalar argument with the same callback needs no lift",
          call "dataCallback" [int 1, var "one"],
          TInt,
          Nothing,
          False)
      , ("an open parameter without scalar evidence keeps a tensor argument whole",
          call "id" [vector],
          tensor,
          Nothing,
          False)
      , ("a literal of tensor components is regular",
          IVectorExpr [vector, vector],
          tensor,
          Nothing,
          False)
      , ("ordinary scalar map",
          call "map" [var "one", scalars],
          TCollection TInt,
          Just (0, arrow [TInt] TInt),
          False)
      , ("polymorphic identity consumes tensors",
          call "map" [var "id", tensors],
          listTensor,
          Just (0, arrow [tensor] tensor),
          False)
      , ("explicit tensor callback consumes whole tensors",
          call "map" [var "whole", tensors],
          TCollection TInt,
          Just (0, arrow [tensor] TInt),
          False)
      , ("tensor callback determines rank-zero initializer",
          call "iterate" [var "tensorIdentity", int 0],
          listTensor,
          Just (0, arrow [tensor] tensor),
          False)
      , ("rank-zero admission keeps list result",
          call "rankZeroList" [int 0],
          listTensor,
          Nothing,
          False)
      , ("direct tensor mapping keeps tuple inside Tensor",
          call "scalarPair" [vector],
          TTensor (TTuple [TInt, TInt]),
          Nothing,
          True)
      , ("explicit scalar map uses function result",
          ITensorMapExpr (var "toBool") (int 1),
          TBool,
          Nothing,
          True)
      , ("explicit tensor map uses function result",
          ITensorMapExpr (var "toBool") vector,
          TTensor TBool,
          Nothing,
          True)
      , ("explicit binary scalar map uses function result",
          ITensorMap2Expr (var "toBool2") (int 1) (int 2),
          TBool,
          Nothing,
          True)
      , ("explicit binary scalar/tensor map",
          ITensorMap2Expr (var "toBool2") (int 1) vector,
          TTensor TBool,
          Nothing,
          True)
      , ("explicit binary tensor/scalar map",
          ITensorMap2Expr (var "toBool2") vector (int 1),
          TTensor TBool,
          Nothing,
          True)
      , ("explicit binary tensor/tensor map",
          ITensorMap2Expr (var "toBool2") vector vector,
          TTensor TBool,
          Nothing,
          True)
      , ("explicit map normalizes tensor result",
          ITensorMapExpr (var "tensorResult") vector,
          tensor,
          Nothing,
          True)
      , ("unresolved explicit map has a checked Tensor domain",
          lambda ["x"] (ITensorMapExpr (var "toBool") (var "x")),
          arrow [tensor] (TTensor TBool),
          Nothing,
          True)
      , ("recursive let propagates a scalar function before its use",
          recursiveOnes False,
          listTensor,
          Nothing,
          True)
      , ("recursive let follows forward function dependencies",
          recursiveOnes True,
          listTensor,
          Nothing,
          True)
      , ("let-bound completion called on tensors",
          letOnes (call "ones" [tensors]),
          listTensor,
          Nothing,
          True)
      , ("ordinary candidate: let-bound completion called on scalars",
          letOnes (call "ones" [scalars]),
          TCollection TInt,
          Nothing,
          False)
      , ("ordinary candidate: unary completion",
          lambda ["xs"] (call "map" [var "one", var "xs"]),
          arrow [TCollection TInt] (TCollection TInt),
          Nothing,
          False)
      , ("generic consumer preserves a scalar function value",
          call "consumeAny" [var "one"],
          TInt,
          Just (0, arrow [TInt] TInt),
          False)
      , ("generic consumer preserves a higher-order function value",
          call "consumeAny" [var "map"],
          TInt,
          Nothing,
          False)
      , ("ordinary candidate: open consumer completes its scalar callback",
          lambda ["g"] (call "one" [IApplyExpr (var "g") [var "one"]]),
          arrow [arrow [arrow [TInt] TInt] TInt] TInt,
          Nothing,
          False)
      , ("open consumer and whole tensor application compose",
          IApplyExpr (lambda ["g"] (IApplyExpr (var "g") [var "one"]))
            [lambda ["k"] (IApplyExpr (var "k") [vector])],
          tensor,
          Nothing,
          True)
      , ("ordinary candidate: binary completion",
          lambda ["xs"] (call "foldl1" [var "two", var "xs"]),
          arrow [TCollection TInt] TInt,
          Nothing,
          False)
      , ("ordinary candidate: scalar initializer",
          lambda ["xs"] (call "foldl" [var "two", int 0, var "xs"]),
          arrow [TCollection TInt] TInt,
          Nothing,
          False)
      , ("ordinary candidate: lambda class constraints are retained in fold candidates",
          lambda ["xs"] (call "foldl" [constrainedAddition, int 0, var "xs"]),
          arrow [TCollection TInt] TInt,
          Nothing,
          False)
      , ("class-constrained scalar initializer waits for tensor fold data",
          call "foldl" [var "constrainedAdd", var "constrainedZero", tensors],
          tensor,
          Just (0, arrow [tensor, tensor] tensor),
          True)
      , ("ordinary candidate: local lambda class constraints are retained in fold candidates",
          lambda ["xs"] (ILetExpr [(PDPatVar (Var "add" []), constrainedAddition)]
            (call "foldl" [var "add", int 0, var "xs"])),
          arrow [TCollection TInt] TInt,
          Nothing,
          False)
      , ("ordinary candidate: recursive local lambda constraints are retained in fold candidates",
          lambda ["xs"] (ILetRecExpr [(PDPatVar (Var "add" []), constrainedAddition)]
            (call "foldl" [var "add", int 0, var "xs"])),
          arrow [TCollection TInt] TInt,
          Nothing,
          False)
      , ("known scalar fold data keeps a constrained lambda scalar",
          call "foldl" [constrainedAddition, int 0, scalars],
          TInt,
          Just (0, arrow [TInt, TInt] TInt),
          False)
      , ("ordinary constrained scalar application needs no map",
          lambda ["x"] (call "constrainedAdd" [var "x", int 1]),
          arrow [TInt] TInt,
          Nothing,
          False)
      , ("ordinary candidate: ternary completion",
          lambda ["xs", "ys", "zs"] (call "map3" [var "three", var "xs", var "ys", var "zs"]),
          arrow [TCollection TInt, TCollection TInt, TCollection TInt] (TCollection TInt),
          Nothing,
          False)
      , ("ordinary candidate: partially applied callback",
          lambda ["xs"] (call "map" [call "two" [int 7], var "xs"]),
          arrow [TCollection TInt] (TCollection TInt),
          Nothing,
          False)
      , ("scalar predicate keeps its ordinary type",
          lambda ["xs"] (call "filter" [var "toBool", var "xs"]),
          arrow [TCollection TInt] (TCollection TInt),
          Nothing,
          False)
      , ("ordinary candidate: completion keeps lists inside Tensor results",
          lambda ["xs"] (call "map" [var "scalarListResult", var "xs"]),
          arrow [TCollection TInt] (TCollection (TCollection TInt)),
          Nothing,
          False)
      , ("a polymorphic local higher-order function keeps independent uses",
          ILetRecExpr [(PDPatVar (Var "applyMap" []), lambda ["f", "xs"] (call "map" [var "f", var "xs"]))]
            (ITupleExpr [call "applyMap" [var "one", scalars], call "applyMap" [var "one", tensors]]),
          TTuple [TCollection TInt, TCollection tensor], Nothing, True)
      , ("one definition serves scalar and tensor uses", localFamily False, TTuple [TInt, tensor], Nothing, True)
      , ("functions are scalar components of a tensor callback input",
          call "map" [lambda ["f"] (call "f" [int 1]),
            ICollectionExpr [IVectorExpr [var "one", var "one"]]],
          TCollection tensor, Nothing, True)
      , ("an unconstrained constant callback receives the whole tensor",
          call "genericTensorCallbackConsumer" [lambda ["x"] (int 1)],
          tensor, Nothing, False)
      , ("a scalar callback applies arguments across a tensor-valued function result",
          call "tensorCallbackConsumer2" [lambda ["x"]
            (ITensorExpr (ICollectionExpr [])
              (ICollectionExpr [lambda ["y"] (call "two" [var "x",var "y"])]))],
          tensor, Nothing, True)
      , ("a callback can return a tensor of functions",
          call "functionTensorLists" [call "map" [var "two", tensors]],
          TInt, Nothing, True)
      , ("explicit mapping can return a tensor of functions",
          call "functionTensorLists" [call "map"
            [lambda ["t"] (ITensorMapExpr (var "two") (var "t")), tensors]],
          TInt, Nothing, True)
      , ("later scalar use resolves an unknown callback supplied to a captured consumer",
          IApplyExpr (lambda ["g", "h"]
            (ITupleExpr [call "g" [var "h"],
              call "scalarList" [ICollectionExpr [call "h" [int 1]]]]))
            [lambda ["k"] (call "k" [vector]), var "one"],
          TTuple [tensor, TCollection TInt], Nothing, True)
      , ("explicit callback conversion preserves a later scalar use",
          IApplyExpr (lambda ["g", "h"]
            (ITupleExpr [call "g" [lambda ["t"] (ITensorMapExpr (var "h") (var "t"))],
              call "scalarList" [ICollectionExpr [call "h" [int 1]]]]))
            [lambda ["k"] (call "k" [vector]), var "one"],
          TTuple [tensor, TCollection TInt], Nothing, True)
      , ("a retained definition adapts an unknown callback to an opaque consumer",
          ILetExpr [(PDPatVar (Var "invoke" []), lambda ["g", "h"]
            (ITupleExpr [call "g" [var "h"],
              call "scalarList" [ICollectionExpr [call "h" [int 1]]]]))]
            (call "invoke" [var "tensorCallbackConsumer", var "one"]),
          TTuple [tensor, TCollection TInt], Nothing, True)
      , ("explicit conversion works with an opaque consumer and a later scalar use",
          ILetExpr [(PDPatVar (Var "invoke" []), lambda ["g", "h"]
            (ITupleExpr [call "g" [lambda ["t"] (ITensorMapExpr (var "h") (var "t"))],
              call "scalarList" [ICollectionExpr [call "h" [int 1]]]]))]
            (call "invoke" [var "tensorCallbackConsumer", var "one"]),
          TTuple [tensor, TCollection TInt], Nothing, True)
      , ("a polymorphic callback result acquires function arguments at its use",
          let body = ITupleExpr [call "g" [var "h"], call "h" [int 1]] in
          ILetExpr [(PDPatVar (Var "invoke" []), lambda ["g", "h"] body)]
            (call "typedFunctionPair" [call "invoke" [var "tensorCallbackConsumer2", var "two"]]),
          TInt, Nothing, True)
      , ("explicit binary conversion admits a polymorphic function result",
          let converted = lambda ["x", "y"] (ITensorMap2Expr (var "h") (var "x") (var "y"))
              body = ITupleExpr [call "g" [converted], call "h" [int 1]] in
          ILetExpr [(PDPatVar (Var "invoke" []), lambda ["g", "h"] body)]
            (call "typedFunctionPair" [call "invoke" [var "tensorCallbackConsumer2", var "two"]]),
          TInt, Nothing, True)
      , ("function-result specialization also works with reversed consumer arguments",
          let body = ITupleExpr [call "g" [var "h"], call "h" [int 1]] in
          ILetExpr [(PDPatVar (Var "invoke" []), lambda ["h", "g"] body)]
            (call "typedFunctionPair" [call "invoke" [var "two", var "tensorCallbackConsumer2"]]),
          TInt, Nothing, True)
      , ("a function result known before generalization selects binary conversion",
          let body = call "typedFunctionPair" [ITupleExpr [call "g" [var "h"], call "h" [int 1]]] in
          ILetExpr [(PDPatVar (Var "invoke" []), lambda ["g", "h"] body)]
            (call "invoke" [var "tensorCallbackConsumer2", var "two"]),
          TInt, Nothing, True)
      , ("a deferred conversion preserves local let polymorphism",
          let body = ITupleExpr [call "g" [var "h"], call "h" [int 1]]
              first = call "typedFunctionPair" [call "invoke" [var "tensorCallbackConsumer2", var "two"]]
              second = call "invoke" [var "tensorCallbackConsumer", var "one"]
          in ILetExpr [(PDPatVar (Var "invoke" []), lambda ["g", "h"] body)]
            (ITupleExpr [first,second]),
          TTuple [TInt,TTuple [tensor,TInt]], Nothing, True)
      , ("a recursive family specializes a function-valued result",
          let body = ITupleExpr [call "g" [var "h"], call "h" [int 1]]
          in ILetRecExpr [(PDPatVar (Var "invoke" []), lambda ["g", "h"] body)]
            (call "typedFunctionPair" [call "invoke" [var "tensorCallbackConsumer2", var "two"]]),
          TInt, Nothing, True)
      , ("a local alias keeps a deferred function-valued result",
          let body = ITupleExpr [call "g" [var "h"], call "h" [int 1]]
          in ILetExpr [(PDPatVar (Var "invoke" []), lambda ["g", "h"] body),
               (PDPatVar (Var "alias" []),var "invoke")]
            (call "typedFunctionPair" [call "alias" [var "tensorCallbackConsumer2", var "two"]]),
          TInt, Nothing, True)
      , ("an opaque polymorphic callback acquires a function-valued result",
          call "tensorCallbackConsumer2" [var "polymorphicFunctionResult"], tensor, Nothing, True)
      , ("an alias retains every candidate", localFamily True, TTuple [TInt, tensor], Nothing, True)
      , ("recursive group serves independent scalar and tensor uses",
          case localFamily False of ILetExpr bindings body -> ILetRecExpr bindings body; expr -> expr,
          TTuple [TInt, tensor], Nothing, True)
      , ("later higher-order data selects the retained function candidate",
          let def = lambda ["xs"] (call "foldl" [var "two", int 0, var "xs"]) in
          ILetExpr [(PDPatVar (Var "f" []), def)]
            (IApplyExpr (lambda ["h", "xs"] (call "h" [var "xs"])) [var "f", tensors]),
          tensor, Nothing, True)
      , ("a later higher-order argument resolves a single tensor use",
          IApplyExpr (lambda ["h"] (ILetRecExpr
            [(PDPatVar (Var "f" []), lambda ["xs"]
              (call "foldl" [lambda ["a", "b"] (call "h" [var "a", var "b"]), int 0, var "xs"]))]
            (call "f" [tensors]))) [var "two"], tensor, Nothing, True)
      , ("later scalar use resolves captured callback types before tensor selection",
          IApplyExpr
            (lambda ["h"] (ILetExpr
              [(PDPatVar (Var "f" []), lambda ["xs"]
                (call "foldl" [lambda ["a", "b"] (call "h" [var "a", var "b"]), int 0, var "xs"]))]
              (ITupleExpr [call "f" [scalars], call "f" [tensors]]))) [var "two"],
          TTuple [TInt, tensor], Nothing, True)
      , ("independent recursive-group definitions generalize before their uses",
          ILetRecExpr [(PDPatVar (Var "localId" []),lambda ["x"] (var "x")),
            (PDPatVar (Var "uses" []), ITupleExpr [call "localId" [int 1],
              call "localId" [IConstantExpr (BoolExpr True)]])]
            (var "uses"), TTuple [TInt,TBool], Nothing, False)
      , ("a function result supports higher-order and direct tensor applications",
          let body = ITupleExpr [call "g" [var "h"], call "h" [int 1], call "h" [vector]]
          in ILetExpr [(PDPatVar (Var "invoke" []),lambda ["g","h"] body)]
            (call "typedFunctionTriple" [call "invoke" [var "tensorCallbackConsumer2",var "two"]]),
          TInt, Nothing, True)
      , ("integer index variables resolve after direct application",
          IApplyExpr (lambda ["i"] (IIndexedExpr False vector [Sub (var "i")])) [int 1],
          TInt, Nothing, False)
      , ("higher-order data fixes integer coordinate variables",
          call "scalarList" [call "map" [lambda ["i"]
            (IIndexedExpr False vector [Sub (var "i")]),scalars]],
          TCollection TInt, Nothing, False)
      , ("mathematical index symbols retain tensor axes",
          IIndexedExpr False vector [Sub (var "mathIndex")], tensor, Nothing, False)
      , ("quoted index symbols retain tensor axes",
          IIndexedExpr False vector [Sub (IQuoteExpr (var "mathIndex"))], tensor, Nothing, False)
      , ("an indexed function serves scalar and symbolic uses",
          ILetExpr [(PDPatVar (Var "indexed" []),lambda ["i"]
            (IIndexedExpr False vector [Sub (var "i")]))]
            (ITupleExpr [call "scalarList" [ICollectionExpr [call "indexed" [int 1]]],
              call "indexed" [var "mathIndex"]]),
          TTuple [TCollection TInt,tensor], Nothing, False)
      , ("an arithmetic result supplies tensor evidence to its enclosing application",
          call "two" [call "one" [call "tensorResult" [int 1]],int 2],
          tensor, Nothing, True)
      , ("a retained fold accepts a mapped tensor-valued callback over functions",
          ILetExpr [(PDPatVar (Var "reduction" []),lambda ["xs"]
            (call "foldl" [var "constrainedAdd",var "constrainedZero",var "xs"]))]
            (call "reduction" [call "map" [lambda ["operation"] vector,
              ICollectionExpr [var "one"]]]), tensor, Nothing, True)
      , ("a retained fold admits a polymorphic initializer at rank zero",
          ILetExpr [(PDPatVar (Var "reduction" []),lambda ["xs"]
            (call "foldl" [var "constrainedAdd",var "constrainedZero",var "xs"]))]
            (call "reduction" [tensors]), tensor, Nothing, True)
      , ("a delayed fold separates scalar and tensor accumulator types",
          ILetExpr [(PDPatVar (Var "reduction" []),lambda ["operation","initial","xs"]
            (call "foldl" [var "operation",var "initial",var "xs"]))]
            (call "reduction" [var "two",int 0,tensors]), tensor, Nothing, True)
      , ("a function stored in a tuple retains its conversion choices",
          let reduction = lambda ["xs"] (call "foldl" [var "two", int 0, var "xs"])
          in ILetExpr [(PDPatVar (Var "boxed" []), ITupleExpr [reduction,int 0]),
               (PDTuplePat [PDPatVar (Var "f" []),PDWildCard],var "boxed")]
            (call "f" [tensors]), tensor, Nothing, True)
      , ("an ordinary local use emits no unused tensor wrapper", ILetExpr [(PDPatVar (Var "f" []), lambda ["xs"] (call "foldl" [var "two", int 0, var "xs"]))] (call "f" [scalars]), TInt, Nothing, False)
      ] ++
      [("a use determines " ++ show arity ++ " scalar function parameters",
        let body = ITupleExpr [call "g" [var "h"], call "h" [int 1]]
        in ILetExpr [(PDPatVar (Var "invoke" []),lambda ["g", "h"] body)]
          (call ("functionPair" ++ show arity)
            [call "invoke" [var ("functionConsumer" ++ show arity),var ("function" ++ show arity)]]),
        TInt, Nothing, True) | arity <- [1 :: Int ..5]]

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

    localFamily alias =
      let definition = lambda ["xs"] (call "foldl" [var "two", int 0, var "xs"])
          uses name = ITupleExpr [call name [scalars], call name [tensors]]
          body = if alias then ILetExpr [(PDPatVar (Var "g" []), var "f")] (uses "g") else uses "f"
      in ILetExpr [(PDPatVar (Var "f" []), definition)] body
    recursiveOnes forward =
      let bindings =
            [(PDPatVar (Var "keptOne" []), var "one"),
             (PDPatVar (Var "ones" []), lambda ["xs"] (call "map" [var "keptOne", var "xs"]))]
      in ILetRecExpr (if forward then reverse bindings else bindings) (call "ones" [tensors])
    letOnes body = ILetExpr
      [(PDPatVar (Var "ones" []), lambda ["xs"] (call "map" [var "one", var "xs"]))] body
    constrainedAddition = lambda ["x", "y"] (call "constrainedAdd" [var "x", var "y"])
    conditional = IIfExpr (IConstantExpr (BoolExpr False))
    tensor = TTensor TInt
    listTensor = TCollection tensor
    vector = IVectorExpr [int 1, int 2]
    tensors = ICollectionExpr [vector, vector]
    scalars = ICollectionExpr [int 3, int 4]

inferenceState :: InferState
inferenceState = initialInferState { inferEnv =
  Env.extendEnv (Var "constrainedZero" [])
    (Forall [] [TyVar "a"] [Constraint "AddMonoid" [a]] a) $
  Env.extendEnv (Var "constrainedAdd" [])
    (Forall [] [TyVar "a"] [Constraint "AddSemigroup" [a]] (arrow [a, a] a))
    (foldr add Env.emptyEnv (bindings ++ concat
      [[("function" ++ show arity,arrow (replicate arity TInt) TInt),
        ("functionConsumer" ++ show arity,arrow [arrow (replicate arity tensor) tensor] tensor),
        ("functionPair" ++ show arity,arrow [TTuple [tensor,arrow (replicate (arity-1) TInt) TInt]] TInt)]
       | arity <- [1 :: Int ..5]])) }
  where
    add (name, ty) = Env.extendEnv (Var name [])
      (Forall [] (Set.toList (freeTyVars ty)) [] ty)
    a = TVar (TyVar "a")
    b = TVar (TyVar "b")
    c = TVar (TyVar "c")
    d = TVar (TyVar "d")
    tensor = TTensor TInt
    bindings =
      [ ("consumeAny", arrow [a] TInt)
      , ("map", arrow [arrow [a] b, TCollection a] (TCollection b))
      , ("apply", arrow [arrow [a] b, a] b)
      , ("dataCallback", arrow [a, arrow [a] TInt] TInt)
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
      , ("scalarNestedList", arrow [TCollection (TCollection TInt)] (TCollection (TCollection TInt)))
      , ("scalarListResult", arrow [TInt] (TCollection TInt))
      , ("functionTensorLists", arrow [TCollection (TTensor (arrow [TInt] TInt))] TInt)
      , ("genericTensorCallbackConsumer", arrow [arrow [TTensor a] tensor] tensor)
      , ("tensorCallbackConsumer", arrow [arrow [tensor] tensor] tensor)
      , ("tensorCallbackConsumer2", arrow [arrow [tensor, tensor] tensor] tensor)
      , ("polymorphicFunctionResult", arrow [TInt] a)
      , ("mathIndex", TMathValue)
      , ("typedFunctionTriple", arrow [TTuple [tensor, arrow [TInt] TInt, arrow [tensor] tensor]] TInt)
      , ("typedFunctionPair", arrow [TTuple [tensor, arrow [TInt] TInt]] TInt)
      , ("listIdentity", arrow [TCollection a] (TCollection a))
      , ("mathConstant", arrow [TMathValue] TMathValue)
      , ("mathAdd", arrow [TMathValue, TMathValue] TMathValue)
      , ("mathList", arrow [TCollection TMathValue] (TCollection TMathValue))
      , ("mathNestedList", arrow [TCollection (TCollection TMathValue)] (TCollection (TCollection TMathValue)))
      , ("mathPair", arrow [TTuple [TMathValue, TMathValue]] (TTuple [TMathValue, TMathValue]))
      , ("mathTensors", TCollection (TTensor TMathValue))
      , ("mathTensorResult", arrow [TMathValue] (TTensor TMathValue))
      , ("mathFunctionConsumer", arrow [arrow [TMathValue] TMathValue] TBool)
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
