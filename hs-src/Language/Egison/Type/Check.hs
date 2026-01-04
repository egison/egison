{- |
Module      : Language.Egison.Type.Check
Licence     : MIT

This module provides the main entry point for type checking Egison programs.
-}

module Language.Egison.Type.Check
  ( -- * Type checking
    typeCheck
  , typeCheckExpr
  , typeCheckTopExprs
  , typeCheckWithWarnings
  , typeCheckWithLoader
    -- * Type checking results
  , TypeCheckResult(..)
  , TypeCheckError(..)
  , TypeCheckEnv(..)
    -- * Configuration
  , TypeCheckConfig(..)
  , defaultConfig
  , strictConfig
  , permissiveConfig
    -- * Built-in environment
  , builtinEnv
    -- * File loading
  , FileLoader
  ) where

import           Control.Monad.State.Strict (get)

import           Language.Egison.AST
import           Language.Egison.Type.Env
import           Language.Egison.Type.Error
import           Language.Egison.Type.Infer
import           Language.Egison.Type.Types

-- | Result of type checking, containing both type environment and class environment
data TypeCheckEnv = TypeCheckEnv
  { tceTypeEnv  :: TypeEnv   -- ^ Type environment (variable → type scheme)
  , tceClassEnv :: ClassEnv  -- ^ Class environment (class definitions and instances)
  } deriving (Show, Eq)

-- | Type check configuration
data TypeCheckConfig = TypeCheckConfig
  { tcStrict       :: Bool   -- ^ Require all types to be fully inferred
  , tcWarnAny      :: Bool   -- ^ Warn when TAny is used
  , tcCheckTensors :: Bool   -- ^ Enable tensor index checking
  , tcPermissive   :: Bool   -- ^ Allow unbound variables (emit warnings instead of errors)
  , tcCollectWarnings :: Bool -- ^ Collect and report warnings
  } deriving (Show, Eq)

-- | Default configuration (permissive for gradual adoption)
defaultConfig :: TypeCheckConfig
defaultConfig = TypeCheckConfig
  { tcStrict       = False
  , tcWarnAny      = False
  , tcCheckTensors = True
  , tcPermissive   = True    -- Permissive by default for gradual adoption
  , tcCollectWarnings = True
  }

-- | Strict configuration (all types must be known)
strictConfig :: TypeCheckConfig
strictConfig = TypeCheckConfig
  { tcStrict       = True
  , tcWarnAny      = True
  , tcCheckTensors = True
  , tcPermissive   = False
  , tcCollectWarnings = True
  }

-- | Permissive configuration (maximum compatibility)
permissiveConfig :: TypeCheckConfig
permissiveConfig = TypeCheckConfig
  { tcStrict       = False
  , tcWarnAny      = False
  , tcCheckTensors = False
  , tcPermissive   = True
  , tcCollectWarnings = True
  }

-- | Type check error with location
data TypeCheckError = TypeCheckError
  { tceError    :: TypeError
  , tceLocation :: Maybe String
  } deriving (Show, Eq)

-- | Type check result
data TypeCheckResult = TypeCheckResult
  { tcrType     :: Type           -- ^ The inferred type
  , tcrWarnings :: [TypeWarning]  -- ^ Any warnings
  , tcrEnv      :: TypeEnv        -- ^ Updated type environment
  } deriving (Show)

-- | Convert TypeCheckConfig to InferConfig
toInferConfig :: TypeCheckConfig -> InferConfig
toInferConfig cfg = InferConfig
  { cfgPermissive = tcPermissive cfg
  , cfgCollectWarnings = tcCollectWarnings cfg
  , cfgFileLoader = Nothing
  }

-- | Type check a single expression (IO version)
typeCheckExpr :: TypeCheckConfig -> TypeEnv -> Expr -> IO (Either TypeCheckError TypeCheckResult)
typeCheckExpr config env expr = do
  let inferCfg = toInferConfig config
      initialState = InferState 0 env [] inferCfg emptyClassEnv
  (result, warnings) <- runInferWithWarnings (inferExpr expr) initialState
  return $ case result of
    Left err -> Left $ TypeCheckError err Nothing
    Right (ty, _) -> Right $ TypeCheckResult
      { tcrType = ty
      , tcrWarnings = warnings ++ collectWarnings config ty
      , tcrEnv = env
      }

-- | Type check multiple top-level expressions (IO version)
typeCheckTopExprs :: TypeCheckConfig -> [TopExpr] -> IO (Either TypeCheckError TypeEnv)
typeCheckTopExprs config exprs = do
  let inferCfg = toInferConfig config
      initialState = InferState 0 builtinEnv [] inferCfg emptyClassEnv
      checkAndGetEnv = do
        mapM_ inferTopExpr exprs
        inferEnv <$> get
  result <- runInfer checkAndGetEnv initialState
  return $ case result of
    Left err -> Left $ TypeCheckError err Nothing
    Right env -> Right env

-- | Type check and return both result and warnings (IO version)
typeCheckWithWarnings :: TypeCheckConfig -> [TopExpr] -> IO (Either [TypeCheckError] TypeEnv, [TypeWarning])
typeCheckWithWarnings config exprs = do
  let inferCfg = toInferConfig config
      initialState = InferState 0 builtinEnv [] inferCfg emptyClassEnv
      checkAndGetEnv = do
        mapM_ inferTopExpr exprs
        inferEnv <$> get
  (result, warnings) <- runInferWithWarnings checkAndGetEnv initialState
  return $ case result of
    Left err -> (Left [TypeCheckError err Nothing], warnings)
    Right env -> (Right env, warnings)

-- | Main entry point for type checking (IO version)
typeCheck :: TypeCheckConfig -> [TopExpr] -> IO (Either [TypeCheckError] TypeEnv)
typeCheck config exprs = do
  result <- typeCheckTopExprs config exprs
  return $ case result of
    Left err -> Left [err]
    Right env -> Right env

-- | Type check with a custom file loader (for loading library types)
typeCheckWithLoader :: TypeCheckConfig -> FileLoader -> [TopExpr] -> IO (Either [TypeCheckError] TypeCheckEnv, [TypeWarning])
typeCheckWithLoader config loader exprs = do
  let inferCfg = setFileLoader loader (toInferConfig config)
      initialState = InferState 0 builtinEnv [] inferCfg emptyClassEnv
      loadLibsAndCheck = do
        -- First, load all core libraries to build the type environment
        -- We ignore warnings during library loading (they are internal to the libraries)
        mapM_ loadCoreLibrary coreLibraries
        -- Clear warnings accumulated during library loading
        clearWarnings
        -- Then, type check the user's expressions
        -- Note: We use inferTopExprs which will NOT trigger recursive library loading
        -- because the file loader is already set, but user code should not have Load statements
        mapM_ inferTopExprNoLoad exprs
        st <- get
        return $ TypeCheckEnv (inferEnv st) (inferClassEnv st)
  (result, warnings) <- runInferWithWarnings loadLibsAndCheck initialState
  return $ case result of
    Left err -> (Left [TypeCheckError err Nothing], warnings)
    Right env -> (Right env, warnings)

-- | Infer types for a top expression without loading files
-- This is used for user code after libraries are already loaded
inferTopExprNoLoad :: TopExpr -> Infer ()
inferTopExprNoLoad (Load _) = return ()  -- Don't load again
inferTopExprNoLoad (LoadFile _) = return ()  -- Don't load again
inferTopExprNoLoad expr = inferTopExpr expr

-- | Load a core library for type checking
loadCoreLibrary :: FilePath -> Infer ()
loadCoreLibrary path = loadAndInferFile path

-- | List of core libraries in load order
-- This must match the order in Language.Egison.coreLibraries
coreLibraries :: [String]
coreLibraries =
  -- Libs that defines user-defined infixes comes first
  [ "lib/core/base.egi"              -- Defines (&&) (||)
  , "lib/math/common/arithmetic.egi" -- Defines (+) (-) (*) (/) (+') (-') (*') (/')
  , "lib/math/algebra/tensor.egi"    -- Defines (.) (.')
  , "lib/core/collection.egi"        -- Defines (++) for patterns
  , "lib/math/expression.egi"        -- Defines (+) (*) (/) (^) for patterns

  , "lib/core/assoc.egi"
  , "lib/core/io.egi"
  , "lib/core/maybe.egi"
  , "lib/core/number.egi"
  , "lib/core/order.egi"
  , "lib/core/random.egi"
  , "lib/core/string.egi"
  , "lib/core/sort.egi"
  , "lib/math/common/constants.egi"
  , "lib/math/common/functions.egi"
  , "lib/math/algebra/root.egi"
  , "lib/math/algebra/equations.egi"
  , "lib/math/algebra/inverse.egi"
  , "lib/math/analysis/derivative.egi"
  , "lib/math/analysis/integral.egi"
  , "lib/math/algebra/vector.egi"
  , "lib/math/algebra/matrix.egi"
  , "lib/math/geometry/differential-form.egi"
  -- Normalize library (loaded after core libraries)
  , "lib/math/normalize.egi"
  ]

-- | Collect warnings from a type
collectWarnings :: TypeCheckConfig -> Type -> [TypeWarning]
collectWarnings config ty
  | tcWarnAny config && containsAny ty =
      [AnyTypeWarning "Expression contains 'Any' type" emptyContext]
  | otherwise = []
  where
    containsAny TAny = True
    containsAny (TList t) = containsAny t
    containsAny (TTuple ts) = any containsAny ts
    containsAny (TFun t1 t2) = containsAny t1 || containsAny t2
    containsAny (TMatcher t) = containsAny t
    containsAny (TTensor t _ _) = containsAny t
    containsAny (TIO t) = containsAny t
    containsAny _ = False

-- | Built-in type environment with primitive functions
builtinEnv :: TypeEnv
builtinEnv = extendEnvMany builtinTypes emptyEnv

-- | Types for built-in functions
builtinTypes :: [(String, TypeScheme)]
builtinTypes = concat
  [ arithTypes
  , comparisonTypes
  , booleanTypes
  , listTypes
  , tupleTypes
  , ioTypes
  , conversionTypes
  , tensorTypes
  , matcherTypes
  , stringTypes
  , mathTypes
  , utilityTypes
  ]
  where
    a = TyVar "a"
    b = TyVar "b"
    c = TyVar "c"

    -- | Make a binary operator type (returns Type, not TypeScheme)
    binOpT :: Type -> Type -> Type -> Type
    binOpT t1 t2 t3 = TFun t1 (TFun t2 t3)

    -- | Make a ternary operator type
    ternOpT :: Type -> Type -> Type -> Type -> Type
    ternOpT t1 t2 t3 t4 = TFun t1 (TFun t2 (TFun t3 t4))

    -- | Make a binary operator type scheme (no type variables)
    binOp :: Type -> Type -> Type -> TypeScheme
    binOp t1 t2 t3 = Forall [] [] $ binOpT t1 t2 t3

    -- | Unary operation
    unaryOp :: Type -> Type -> TypeScheme
    unaryOp t1 t2 = Forall [] [] $ TFun t1 t2

    forallA :: Type -> TypeScheme
    forallA = Forall [a] []

    forallAB :: Type -> TypeScheme
    forallAB = Forall [a, b] []

    forallABC :: Type -> TypeScheme
    forallABC = Forall [a, b, c] []

    -- | forallA with binary op
    forallABinOp :: Type -> Type -> Type -> TypeScheme
    forallABinOp t1 t2 t3 = Forall [a] [] $ binOpT t1 t2 t3

    -- | forallAB with binary op
    forallABBinOp :: Type -> Type -> Type -> TypeScheme
    forallABBinOp t1 t2 t3 = Forall [a, b] [] $ binOpT t1 t2 t3

    -- Arithmetic operators
    arithTypes =
      [ ("+",  binOp TInt TInt TInt)
      , ("-",  binOp TInt TInt TInt)
      , ("*",  binOp TInt TInt TInt)
      , ("/",  binOp TInt TInt TInt)
      , ("mod", binOp TInt TInt TInt)
      , ("modulo", binOp TInt TInt TInt)
      , ("quotient", binOp TInt TInt TInt)
      , ("%", binOp TInt TInt TInt)
      , ("^",  binOp TInt TInt TInt)
      , ("abs", unaryOp TInt TInt)
      , ("neg", unaryOp TInt TInt)
      -- Internal base operators (b.+ etc.)
      , ("b.+", binOp TInt TInt TInt)
      , ("b.-", binOp TInt TInt TInt)
      , ("b.*", binOp TInt TInt TInt)
      , ("b./", binOp TInt TInt TInt)
      , ("b.abs", unaryOp TInt TInt)
      , ("b.neg", unaryOp TInt TInt)
      -- Floating point arithmetic
      , ("+.",  binOp TFloat TFloat TFloat)
      , ("-.",  binOp TFloat TFloat TFloat)
      , ("*.",  binOp TFloat TFloat TFloat)
      , ("/.",  binOp TFloat TFloat TFloat)
      , ("f.+",  binOp TFloat TFloat TFloat)
      , ("f.-",  binOp TFloat TFloat TFloat)
      , ("f.*",  binOp TFloat TFloat TFloat)
      , ("f./",  binOp TFloat TFloat TFloat)
      -- Floating point constants
      , ("f.pi", Forall [] [] TFloat)
      , ("f.e", Forall [] [] TFloat)
      -- Math functions
      , ("sqrt", unaryOp TFloat TFloat)
      , ("exp", unaryOp TFloat TFloat)
      , ("log", unaryOp TFloat TFloat)
      , ("sin", unaryOp TFloat TFloat)
      , ("cos", unaryOp TFloat TFloat)
      , ("tan", unaryOp TFloat TFloat)
      , ("asin", unaryOp TFloat TFloat)
      , ("acos", unaryOp TFloat TFloat)
      , ("atan", unaryOp TFloat TFloat)
      , ("sinh", unaryOp TFloat TFloat)
      , ("cosh", unaryOp TFloat TFloat)
      , ("tanh", unaryOp TFloat TFloat)
      , ("asinh", unaryOp TFloat TFloat)
      , ("acosh", unaryOp TFloat TFloat)
      , ("atanh", unaryOp TFloat TFloat)
      -- Internal base math (b.sqrt etc.)
      , ("b.sqrt", unaryOp TFloat TFloat)
      , ("b.sqrt'", unaryOp TFloat TFloat)
      , ("b.exp", unaryOp TFloat TFloat)
      , ("b.log", unaryOp TFloat TFloat)
      , ("b.sin", unaryOp TFloat TFloat)
      , ("b.cos", unaryOp TFloat TFloat)
      , ("b.tan", unaryOp TFloat TFloat)
      , ("b.asin", unaryOp TFloat TFloat)
      , ("b.acos", unaryOp TFloat TFloat)
      , ("b.atan", unaryOp TFloat TFloat)
      , ("b.sinh", unaryOp TFloat TFloat)
      , ("b.cosh", unaryOp TFloat TFloat)
      , ("b.tanh", unaryOp TFloat TFloat)
      , ("b.asinh", unaryOp TFloat TFloat)
      , ("b.acosh", unaryOp TFloat TFloat)
      , ("b.atanh", unaryOp TFloat TFloat)
      -- Rounding functions
      , ("round", unaryOp TFloat TInt)
      , ("floor", unaryOp TFloat TInt)
      , ("ceiling", unaryOp TFloat TInt)
      , ("truncate", unaryOp TFloat TInt)
      -- Fraction operations
      , ("numerator", unaryOp TInt TInt)
      , ("denominator", unaryOp TInt TInt)
      ]

    -- Comparison operators
    comparisonTypes =
      [ ("=",  forallABinOp (TVar a) (TVar a) TBool)
      , ("/=", forallABinOp (TVar a) (TVar a) TBool)
      , ("<",  forallABinOp (TVar a) (TVar a) TBool)
      , (">",  forallABinOp (TVar a) (TVar a) TBool)
      , ("<=", forallABinOp (TVar a) (TVar a) TBool)
      , (">=", forallABinOp (TVar a) (TVar a) TBool)
      , ("compare", forallABinOp (TVar a) (TVar a) TInt)
      , ("min", forallABinOp (TVar a) (TVar a) (TVar a))
      , ("max", forallABinOp (TVar a) (TVar a) (TVar a))
      ]

    -- Boolean operators
    booleanTypes =
      [ ("&&", binOp TBool TBool TBool)
      , ("||", binOp TBool TBool TBool)
      , ("not", unaryOp TBool TBool)
      ]

    -- List functions
    listTypes =
      [ ("head", forallA $ TFun (TList (TVar a)) (TVar a))
      , ("tail", forallA $ TFun (TList (TVar a)) (TList (TVar a)))
      , ("last", forallA $ TFun (TList (TVar a)) (TVar a))
      , ("init", forallA $ TFun (TList (TVar a)) (TList (TVar a)))
      , ("::",  forallABinOp (TVar a) (TList (TVar a)) (TList (TVar a)))
      , ("++",  forallABinOp (TList (TVar a)) (TList (TVar a)) (TList (TVar a)))
      , ("length", forallA $ TFun (TList (TVar a)) TInt)
      , ("take", forallABinOp TInt (TList (TVar a)) (TList (TVar a)))
      , ("drop", forallABinOp TInt (TList (TVar a)) (TList (TVar a)))
      , ("takeWhile", forallABinOp (TFun (TVar a) TBool) (TList (TVar a)) (TList (TVar a)))
      , ("dropWhile", forallABinOp (TFun (TVar a) TBool) (TList (TVar a)) (TList (TVar a)))
      , ("nth", forallABinOp TInt (TList (TVar a)) (TVar a))
      , ("map", forallABBinOp (TFun (TVar a) (TVar b)) (TList (TVar a)) (TList (TVar b)))
      , ("map2", forallABC $ ternOpT (TFun (TVar a) (TFun (TVar b) (TVar c)))
                                     (TList (TVar a))
                                     (TList (TVar b))
                                     (TList (TVar c)))
      , ("filter", forallABinOp (TFun (TVar a) TBool) (TList (TVar a)) (TList (TVar a)))
      , ("partition", forallA $ binOpT (TFun (TVar a) TBool)
                                       (TList (TVar a))
                                       (TTuple [TList (TVar a), TList (TVar a)]))
      , ("foldl", forallAB $ ternOpT (TFun (TVar b) (TFun (TVar a) (TVar b)))
                                     (TVar b)
                                     (TList (TVar a))
                                     (TVar b))
      , ("foldl1", forallA $ binOpT (TFun (TVar a) (TFun (TVar a) (TVar a)))
                                    (TList (TVar a))
                                    (TVar a))
      , ("foldr", forallAB $ ternOpT (TFun (TVar a) (TFun (TVar b) (TVar b)))
                                     (TVar b)
                                     (TList (TVar a))
                                     (TVar b))
      , ("reduce", forallA $ binOpT (TFun (TVar a) (TFun (TVar a) (TVar a)))
                                    (TList (TVar a))
                                    (TVar a))
      , ("scanl", forallAB $ ternOpT (TFun (TVar b) (TFun (TVar a) (TVar b)))
                                     (TVar b)
                                     (TList (TVar a))
                                     (TList (TVar b)))
      , ("zip", forallAB $ binOpT (TList (TVar a))
                                  (TList (TVar b))
                                  (TList (TTuple [TVar a, TVar b])))
      , ("zip3", forallABC $ ternOpT (TList (TVar a))
                                     (TList (TVar b))
                                     (TList (TVar c))
                                     (TList (TTuple [TVar a, TVar b, TVar c])))
      , ("concat", forallA $ TFun (TList (TList (TVar a))) (TList (TVar a)))
      , ("reverse", forallA $ TFun (TList (TVar a)) (TList (TVar a)))
      , ("intersperse", forallABinOp (TVar a) (TList (TVar a)) (TList (TVar a)))
      , ("intercalate", forallABinOp (TList (TVar a)) (TList (TList (TVar a))) (TList (TVar a)))
      , ("split", forallABinOp (TVar a) (TList (TVar a)) (TList (TList (TVar a))))
      , ("splitAt", forallA $ binOpT TInt (TList (TVar a)) (TTuple [TList (TVar a), TList (TVar a)]))
      , ("takeAndDrop", forallA $ binOpT TInt (TList (TVar a)) (TTuple [TList (TVar a), TList (TVar a)]))
      , ("repeat", forallA $ TFun (TList (TVar a)) (TList (TVar a)))
      , ("repeat1", forallA $ TFun (TVar a) (TList (TVar a)))
      , ("iterate", forallABinOp (TFun (TVar a) (TVar a)) (TVar a) (TList (TVar a)))
      , ("all", forallABinOp (TFun (TVar a) TBool) (TList (TVar a)) TBool)
      , ("any", forallABinOp (TFun (TVar a) TBool) (TList (TVar a)) TBool)
      , ("member", forallABinOp (TVar a) (TList (TVar a)) TBool)
      , ("count", forallABinOp (TVar a) (TList (TVar a)) TInt)
      , ("unique", forallA $ TFun (TList (TVar a)) (TList (TVar a)))
      , ("isEmpty", forallA $ TFun (TList (TVar a)) TBool)
      , ("from", unaryOp TInt (TList TInt))
      , ("between", binOp TInt TInt (TList TInt))
      , ("uncons", forallA $ TFun (TList (TVar a)) (TTuple [TVar a, TList (TVar a)]))
      , ("unsnoc", forallA $ TFun (TList (TVar a)) (TTuple [TList (TVar a), TVar a]))
      , ("deleteFirst", forallABinOp (TVar a) (TList (TVar a)) (TList (TVar a)))
      , ("delete", forallABinOp (TVar a) (TList (TVar a)) (TList (TVar a)))
      , ("difference", forallABinOp (TList (TVar a)) (TList (TVar a)) (TList (TVar a)))
      , ("include", forallABinOp (TList (TVar a)) (TList (TVar a)) TBool)
      , ("union", forallABinOp (TList (TVar a)) (TList (TVar a)) (TList (TVar a)))
      , ("intersect", forallABinOp (TList (TVar a)) (TList (TVar a)) (TList (TVar a)))
      , ("add", forallABinOp (TVar a) (TList (TVar a)) (TList (TVar a)))
      , ("frequency", forallA $ TFun (TList (TVar a)) (TList (TTuple [TVar a, TInt])))
      , ("elemIndices", forallABinOp (TVar a) (TList (TVar a)) (TList TInt))
      , ("lookup", forallAB $ binOpT (TVar a) (TList (TTuple [TVar a, TVar b])) (TVar b))
      ]

    -- Tuple functions
    tupleTypes =
      [ ("fst", forallAB $ TFun (TTuple [TVar a, TVar b]) (TVar a))
      , ("snd", forallAB $ TFun (TTuple [TVar a, TVar b]) (TVar b))
      , ("curry", forallABC $ binOpT (TFun (TTuple [TVar a, TVar b]) (TVar c))
                                     (TVar a)
                                     (TFun (TVar b) (TVar c)))
      , ("uncurry", forallABC $ binOpT (TFun (TVar a) (TFun (TVar b) (TVar c)))
                                       (TTuple [TVar a, TVar b])
                                       (TVar c))
      ]

    -- IO functions
    -- Note: In Egison's do-notation, IO functions return IO types
    ioTypes =
      [ ("print", forallA $ TFun (TVar a) (TIO TUnit))
      , ("read", Forall [] [] (TIO TString))
      , ("return", forallA $ TFun (TVar a) (TIO (TVar a)))
      , ("io", forallA $ TFun (TIO (TVar a)) (TVar a))
      , ("openInputFile", unaryOp TString (TIO TUnit))
      , ("openOutputFile", unaryOp TString (TIO TUnit))
      , ("closeInputPort", unaryOp TUnit (TIO TUnit))
      , ("closeOutputPort", unaryOp TUnit (TIO TUnit))
      , ("readChar", Forall [] [] (TIO TChar))
      , ("readLine", Forall [] [] (TIO TString))
      , ("writeChar", unaryOp TChar (TIO TUnit))
      , ("write", forallA $ TFun (TVar a) (TIO TUnit))
      , ("readFile", unaryOp TString (TIO TString))
      , ("isEof", Forall [] [] (TIO TBool))
      , ("flush", unaryOp TUnit (TIO TUnit))
      , ("rand", binOp TInt TInt (TIO TInt))
      , ("f.rand", binOp TFloat TFloat (TIO TFloat))
      , ("readProcess", Forall [a] [] $ ternOpT TString (TList TString) TString (TIO TString))
      , ("show", forallA $ TFun (TVar a) TString)
      ]

    -- Type conversion functions
    conversionTypes =
      [ ("itof", unaryOp TInt TFloat)
      , ("rtof", unaryOp TInt TFloat)
      , ("integerToFloat", unaryOp TInt TFloat)
      , ("rationalToFloat", unaryOp TInt TFloat)
      , ("floatToInteger", unaryOp TFloat TInt)
      , ("ctoi", unaryOp TChar TInt)
      , ("itoc", unaryOp TInt TChar)
      , ("charToInteger", unaryOp TChar TInt)
      , ("integerToChar", unaryOp TInt TChar)
      , ("show", forallA $ TFun (TVar a) TString)
      , ("fromMathExpr", unaryOp TInt TInt)  -- MathExpr operations
      , ("toMathExpr'", unaryOp TInt TInt)
      , ("symbolNormalize", unaryOp TInt TInt)
      ]

    -- Tensor operations
    tensorTypes =
      [ ("generateTensor", forallABinOp (TFun (TList TInt) (TVar a))
                                        (TList TInt)
                                        (TTensor (TVar a) ShapeUnknown []))
      , ("tensorShape", forallA $ TFun (TTensor (TVar a) ShapeUnknown []) (TList TInt))
      , ("tensorToList", forallA $ TFun (TTensor (TVar a) ShapeUnknown []) (TList (TVar a)))
      , ("transpose", forallABinOp (TList TInt)
                                   (TTensor (TVar a) ShapeUnknown [])
                                   (TTensor (TVar a) ShapeUnknown []))
      , ("contract", forallA $ TFun (TTensor (TVar a) ShapeUnknown [])
                                    (TTensor (TVar a) ShapeUnknown []))
      , ("dfOrder", forallA $ TFun (TTensor (TVar a) ShapeUnknown []) TInt)
      , ("addSubscript", binOp TInt TInt TInt)
      , ("addSuperscript", binOp TInt TInt TInt)
      ]

    -- Matchers
    matcherTypes =
      [ ("something", forallA $ TMatcher (TVar a))
      , ("integer", Forall [] [] $ TMatcher TInt)
      , ("bool", Forall [] [] $ TMatcher TBool)
      , ("char", Forall [] [] $ TMatcher TChar)
      , ("string", Forall [] [] $ TMatcher TString)
      , ("float", Forall [] [] $ TMatcher TFloat)
      , ("eq", forallA $ TMatcher (TVar a))
      , ("list", forallA $ TFun (TMatcher (TVar a)) (TMatcher (TList (TVar a))))
      , ("multiset", forallA $ TFun (TMatcher (TVar a)) (TMatcher (TList (TVar a))))
      , ("set", forallA $ TFun (TMatcher (TVar a)) (TMatcher (TList (TVar a))))
      , ("sortedList", forallA $ TFun (TMatcher (TVar a)) (TMatcher (TList (TVar a))))
      , ("unorderedPair", forallA $ TFun (TMatcher (TVar a))
                                         (TMatcher (TTuple [TVar a, TVar a])))
      ]

    -- String functions
    stringTypes =
      [ ("pack", Forall [] [] $ TFun (TList TChar) TString)
      , ("unpack", Forall [] [] $ TFun TString (TList TChar))
      , ("unconsString", Forall [] [] $ TFun TString (TTuple [TChar, TString]))
      , ("lengthString", unaryOp TString TInt)
      , ("appendString", binOp TString TString TString)
      , ("splitString", binOp TString TString (TList TString))
      , ("regex", binOp TString TString (TList (TTuple [TString, TString, TString])))
      , ("regexCg", binOp TString TString (TList (TTuple [TString, TList TString, TString])))
      , ("readTsv", unaryOp TString (TVar a))
      , ("showTsv", forallA $ TFun (TVar a) TString)
      ]

    -- Math expressions
    mathTypes =
      [ ("primes", Forall [] [] $ TList TInt)
      , ("nats", Forall [] [] $ TList TInt)
      ]

    -- Utility functions
    utilityTypes =
      [ ("id", forallA $ TFun (TVar a) (TVar a))
      , ("$", forallAB $ binOpT (TFun (TVar a) (TVar b)) (TVar a) (TVar b))
      , ("compose", forallABC $ binOpT (TFun (TVar a) (TVar b))
                                       (TFun (TVar b) (TVar c))
                                       (TFun (TVar a) (TVar c)))
      , ("flip", forallABC $ TFun (TFun (TVar a) (TFun (TVar b) (TVar c)))
                                  (TFun (TVar b) (TFun (TVar a) (TVar c))))
      , ("eqAs", forallA $ ternOpT (TMatcher (TVar a)) (TVar a) (TVar a) TBool)
      , ("seq", forallAB $ binOpT (TVar a) (TVar b) (TVar b))
      , ("assert", binOp TString TBool TBool)
      , ("assertEqual", forallA $ ternOpT TString (TVar a) (TVar a) TBool)
      -- Type checking predicates
      , ("isBool", forallA $ TFun (TVar a) TBool)
      , ("isInteger", forallA $ TFun (TVar a) TBool)
      , ("isRational", forallA $ TFun (TVar a) TBool)
      , ("isScalar", forallA $ TFun (TVar a) TBool)
      , ("isFloat", forallA $ TFun (TVar a) TBool)
      , ("isChar", forallA $ TFun (TVar a) TBool)
      , ("isString", forallA $ TFun (TVar a) TBool)
      , ("isCollection", forallA $ TFun (TVar a) TBool)
      , ("isHash", forallA $ TFun (TVar a) TBool)
      , ("isTensor", forallA $ TFun (TVar a) TBool)
      -- Boolean constructors
      , ("True", Forall [] [] TBool)
      , ("False", Forall [] [] TBool)
      -- Forward-declared functions (used before their definition in library loading order)
      , ("mathNormalize", forallA $ TFun (TVar a) (TVar a))
      , ("termExpr", Forall [] [] $ TMatcher TInt)  -- mathExpr alias
      , ("coefficients", forallA $ TFun (TVar a) (TList TInt))
      -- Note: Ordering constructors (Less, Equal, Greater), Maybe constructors (Nothing, Just),
      -- and other algebraicDataMatcher constructors are now automatically registered
      -- when the matcher is defined via registerAlgebraicConstructors
      ]

