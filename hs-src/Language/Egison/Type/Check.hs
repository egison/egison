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
    containsAny (TTensor t) = containsAny t
    containsAny (TIO t) = containsAny t
    containsAny _ = False

-- | Built-in type environment with primitive functions
builtinEnv :: TypeEnv
builtinEnv = extendEnvMany builtinTypes emptyEnv

-- | Types for built-in functions
-- Only functions defined in Primitives.hs are included here.
-- Functions defined in lib/ are NOT included (they are loaded from files).
builtinTypes :: [(String, TypeScheme)]
builtinTypes = concat
  [ constantsTypes
  , primitivesTypes
  , arithTypes
  , stringTypes
  , typeFunctionsTypes
  , ioTypes
  , matcherTypes
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

    -- Constants (from Primitives.hs)
    constantsTypes =
      [ ("f.pi", Forall [] [] TFloat)
      , ("f.e", Forall [] [] TFloat)
      ]

    -- Primitives from Primitives.hs (strictPrimitives and lazyPrimitives)
    primitivesTypes =
      [ ("addSubscript", binOp TInt TInt TInt)  -- MathExpr operations
      , ("addSuperscript", binOp TInt TInt TInt)  -- MathExpr operations
      , ("assert", binOp TString TBool TBool)
      , ("assertEqual", forallA $ ternOpT TString (TVar a) (TVar a) TBool)
      , ("tensorShape", forallA $ TFun (TTensor (TVar a)) (TList TInt))
      , ("tensorToList", forallA $ TFun (TTensor (TVar a)) (TList (TVar a)))
      , ("dfOrder", forallA $ TFun (TTensor (TVar a)) TInt)
      ]

    -- Arithmetic operators (from Primitives.Arith.hs)
    -- Note: +, -, *, /, mod, ^, abs, neg, +., -., *., /., sqrt, exp, log, sin, cos, tan, etc.
    -- are defined in lib/ and are NOT included here
    arithTypes =
      [ -- Internal base operators
        ("b.+", binOp TInt TInt TInt)
      , ("b.-", binOp TInt TInt TInt)
      , ("b.*", binOp TInt TInt TInt)
      , ("b./", binOp TInt TInt TInt)
      -- Floating point arithmetic
      , ("f.+", binOp TFloat TFloat TFloat)
      , ("f.-", binOp TFloat TFloat TFloat)
      , ("f.*", binOp TFloat TFloat TFloat)
      , ("f./", binOp TFloat TFloat TFloat)
      -- Fraction operations
      , ("numerator", unaryOp TInt TInt)
      , ("denominator", unaryOp TInt TInt)
      -- MathExpr operations
      , ("fromMathExpr", unaryOp TInt TInt)
      , ("toMathExpr'", unaryOp TInt TInt)
      , ("symbolNormalize", unaryOp TInt TInt)
      -- Integer operations
      , ("modulo", binOp TInt TInt TInt)
      , ("quotient", binOp TInt TInt TInt)
      , ("%", binOp TInt TInt TInt)
      , ("b.abs", unaryOp TInt TInt)
      , ("b.neg", unaryOp TInt TInt)
      -- Comparison operators
      , ("=", forallABinOp (TVar a) (TVar a) TBool)
      , ("<", forallABinOp (TVar a) (TVar a) TBool)
      , ("<=", forallABinOp (TVar a) (TVar a) TBool)
      , (">", forallABinOp (TVar a) (TVar a) TBool)
      , (">=", forallABinOp (TVar a) (TVar a) TBool)
      -- Rounding functions
      , ("round", unaryOp TFloat TInt)
      , ("floor", unaryOp TFloat TInt)
      , ("ceiling", unaryOp TFloat TInt)
      , ("truncate", unaryOp TFloat TInt)
      -- Math functions
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
      ]


    -- IO functions (from Primitives.IO.hs)
    ioTypes =
      [ ("return", forallA $ TFun (TVar a) (TIO (TVar a)))
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
      ]

    -- Type conversion functions (from Primitives.Types.hs)
    typeFunctionsTypes =
      [ ("itof", unaryOp TInt TFloat)
      , ("rtof", unaryOp TInt TFloat)
      , ("ctoi", unaryOp TChar TInt)
      , ("itoc", unaryOp TInt TChar)
      , ("isInteger", forallA $ TFun (TVar a) TBool)
      , ("isRational", forallA $ TFun (TVar a) TBool)
      ]

    -- Matchers (only primitive matchers defined in Haskell)
    -- Note: integer, bool, char, string, float, list, multiset, set, sortedList, unorderedPair, eq are defined in lib/
    matcherTypes =
      [ ("something", forallA $ TMatcher (TVar a))
      ]

    -- String functions (from Primitives.String.hs)
    stringTypes =
      [ ("pack", Forall [] [] $ TFun (TList TChar) TString)
      , ("unpack", Forall [] [] $ TFun TString (TList TChar))
      , ("unconsString", Forall [] [] $ TFun TString (TTuple [TChar, TString]))
      , ("lengthString", unaryOp TString TInt)
      , ("appendString", binOp TString TString TString)
      , ("splitString", binOp TString TString (TList TString))
      , ("regex", binOp TString TString (TList (TTuple [TString, TString, TString])))
      , ("regexCg", binOp TString TString (TList (TTuple [TString, TList TString, TString])))
      , ("read", Forall [] [] (TIO TString))
      , ("readTsv", unaryOp TString (TVar a))
      , ("show", forallA $ TFun (TVar a) TString)
      , ("showTsv", forallA $ TFun (TVar a) TString)
      ]

    -- Utility functions (from Primitives.hs)
    -- Note: assert and assertEqual are already in primitivesTypes
    -- Note: isInteger and isRational are already in typeFunctionsTypes
    utilityTypes =
      [ -- Boolean constructors
        ("True", Forall [] [] TBool)
      , ("False", Forall [] [] TBool)
      -- Note: Ordering constructors (Less, Equal, Greater), Maybe constructors (Nothing, Just),
      -- and other algebraicDataMatcher constructors are now automatically registered
      -- when the matcher is defined via registerAlgebraicConstructors
      ]

