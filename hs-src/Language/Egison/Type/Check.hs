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
    -- * Type checking results
  , TypeCheckResult(..)
  , TypeCheckError(..)
    -- * Configuration
  , TypeCheckConfig(..)
  , defaultConfig
  , strictConfig
    -- * Built-in environment
  , builtinEnv
  ) where

import           Control.Monad              (foldM, forM, when)
import           Control.Monad.Except       (ExceptT, runExceptT, throwError)
import           Control.Monad.State.Strict (State, evalState, get, modify)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map

import           Language.Egison.AST
import           Language.Egison.Type.Env
import           Language.Egison.Type.Error
import           Language.Egison.Type.Infer
import           Language.Egison.Type.Types

-- | Type check configuration
data TypeCheckConfig = TypeCheckConfig
  { tcStrict       :: Bool   -- ^ Require all types to be fully inferred
  , tcWarnAny      :: Bool   -- ^ Warn when TAny is used
  , tcCheckTensors :: Bool   -- ^ Enable tensor index checking
  } deriving (Show, Eq)

-- | Default configuration (permissive)
defaultConfig :: TypeCheckConfig
defaultConfig = TypeCheckConfig
  { tcStrict       = False
  , tcWarnAny      = False
  , tcCheckTensors = True
  }

-- | Strict configuration (all types must be known)
strictConfig :: TypeCheckConfig
strictConfig = TypeCheckConfig
  { tcStrict       = True
  , tcWarnAny      = True
  , tcCheckTensors = True
  }

-- | Type check error with location
data TypeCheckError = TypeCheckError
  { tceError    :: TypeError
  , tceLocation :: Maybe String
  } deriving (Show, Eq)

-- | Type check result
data TypeCheckResult = TypeCheckResult
  { tcrType     :: Type           -- ^ The inferred type
  , tcrWarnings :: [String]       -- ^ Any warnings
  , tcrEnv      :: TypeEnv        -- ^ Updated type environment
  } deriving (Show)

-- | Type check a single expression
typeCheckExpr :: TypeCheckConfig -> TypeEnv -> Expr -> Either TypeCheckError TypeCheckResult
typeCheckExpr config env expr =
  let initialState = InferState 0 env
  in case runInfer (inferExpr expr) initialState of
    Left err -> Left $ TypeCheckError err Nothing
    Right (ty, _) -> Right $ TypeCheckResult
      { tcrType = ty
      , tcrWarnings = collectWarnings config ty
      , tcrEnv = env
      }

-- | Type check multiple top-level expressions
typeCheckTopExprs :: TypeCheckConfig -> [TopExpr] -> Either TypeCheckError TypeEnv
typeCheckTopExprs _config exprs =
  let initialState = InferState 0 builtinEnv
      checkAndGetEnv = do
        mapM_ inferTopExpr exprs
        inferEnv <$> get
  in case runInfer checkAndGetEnv initialState of
    Left err -> Left $ TypeCheckError err Nothing
    Right env -> Right env

-- | Main entry point for type checking
typeCheck :: TypeCheckConfig -> [TopExpr] -> Either [TypeCheckError] TypeEnv
typeCheck config exprs =
  case typeCheckTopExprs config exprs of
    Left err -> Left [err]
    Right env -> Right env

-- | Collect warnings from a type
collectWarnings :: TypeCheckConfig -> Type -> [String]
collectWarnings config ty
  | tcWarnAny config && containsAny ty = ["Type contains 'any' type"]
  | otherwise = []
  where
    containsAny TAny = True
    containsAny (TList t) = containsAny t
    containsAny (TTuple ts) = any containsAny ts
    containsAny (TFun t1 t2) = containsAny t1 || containsAny t2
    containsAny (TMatcher t) = containsAny t
    containsAny (TTensor t _ _) = containsAny t
    containsAny _ = False

-- | Built-in type environment with primitive functions
builtinEnv :: TypeEnv
builtinEnv = extendEnvMany builtinTypes emptyEnv

-- | Types for built-in functions
builtinTypes :: [(String, TypeScheme)]
builtinTypes =
  -- Arithmetic operators
  [ ("+",  binOp TInt TInt TInt)
  , ("-",  binOp TInt TInt TInt)
  , ("*",  binOp TInt TInt TInt)
  , ("/",  binOp TInt TInt TInt)
  , ("mod", binOp TInt TInt TInt)
  , ("^",  binOp TInt TInt TInt)

  -- Floating point arithmetic
  , ("+.",  binOp TFloat TFloat TFloat)
  , ("-.",  binOp TFloat TFloat TFloat)
  , ("*.",  binOp TFloat TFloat TFloat)
  , ("/.",  binOp TFloat TFloat TFloat)

  -- Comparison operators
  , ("=",  forallABinOp (TVar a) (TVar a) TBool)
  , ("/=", forallABinOp (TVar a) (TVar a) TBool)
  , ("<",  forallABinOp (TVar a) (TVar a) TBool)
  , (">",  forallABinOp (TVar a) (TVar a) TBool)
  , ("<=", forallABinOp (TVar a) (TVar a) TBool)
  , (">=", forallABinOp (TVar a) (TVar a) TBool)

  -- Boolean operators
  , ("&&", binOp TBool TBool TBool)
  , ("||", binOp TBool TBool TBool)
  , ("not", Forall [] $ TFun TBool TBool)

  -- List functions
  , ("head", forallA $ TFun (TList (TVar a)) (TVar a))
  , ("tail", forallA $ TFun (TList (TVar a)) (TList (TVar a)))
  , ("::",  forallABinOp (TVar a) (TList (TVar a)) (TList (TVar a)))
  , ("++",  forallABinOp (TList (TVar a)) (TList (TVar a)) (TList (TVar a)))
  , ("length", forallA $ TFun (TList (TVar a)) TInt)
  , ("take", forallABinOp TInt (TList (TVar a)) (TList (TVar a)))
  , ("drop", forallABinOp TInt (TList (TVar a)) (TList (TVar a)))
  , ("map", forallABBinOp (TFun (TVar a) (TVar b)) (TList (TVar a)) (TList (TVar b)))
  , ("filter", forallABinOp (TFun (TVar a) TBool) (TList (TVar a)) (TList (TVar a)))
  , ("foldl", forallAB $ TFun (TFun (TVar b) (TFun (TVar a) (TVar b)))
                              (TFun (TVar b) (TFun (TList (TVar a)) (TVar b))))
  , ("foldr", forallAB $ TFun (TFun (TVar a) (TFun (TVar b) (TVar b)))
                              (TFun (TVar b) (TFun (TList (TVar a)) (TVar b))))

  -- Tuple functions
  , ("fst", forallAB $ TFun (TTuple [TVar a, TVar b]) (TVar a))
  , ("snd", forallAB $ TFun (TTuple [TVar a, TVar b]) (TVar b))

  -- IO functions
  , ("print", forallA $ TFun (TVar a) TUnit)
  , ("read", Forall [] TString)

  -- Type conversion
  , ("integerToFloat", Forall [] $ TFun TInt TFloat)
  , ("floatToInteger", Forall [] $ TFun TFloat TInt)
  , ("show", forallA $ TFun (TVar a) TString)

  -- Tensor operations
  , ("generateTensor", forallABinOp (TFun (TList TInt) (TVar a))
                                    (TList TInt)
                                    (TTensor (TVar a) ShapeUnknown []))
  , ("tensorShape", forallA $ TFun (TTensor (TVar a) ShapeUnknown []) (TList TInt))
  , ("tensorToList", forallA $ TFun (TTensor (TVar a) ShapeUnknown []) (TList (TVar a)))
  , ("transpose", forallABinOp (TList TInt)
                               (TTensor (TVar a) ShapeUnknown [])
                               (TTensor (TVar a) ShapeUnknown []))
  , ("contract", forallA $ TFun (TTensor (TVar a) ShapeUnknown [])
                                (TTensor (TVar a) ShapeUnknown []))

  -- Matchers
  , ("something", forallA $ TMatcher (TVar a))
  , ("integer", Forall [] $ TMatcher TInt)
  , ("bool", Forall [] $ TMatcher TBool)
  , ("string", Forall [] $ TMatcher TString)
  , ("list", forallA $ TFun (TMatcher (TVar a)) (TMatcher (TList (TVar a))))
  , ("multiset", forallA $ TFun (TMatcher (TVar a)) (TMatcher (TList (TVar a))))
  , ("set", forallA $ TFun (TMatcher (TVar a)) (TMatcher (TList (TVar a))))

  -- Common library functions
  , ("primes", Forall [] $ TList TInt)
  , ("nats", Forall [] $ TList TInt)
  ]
  where
    a = TyVar "a"
    b = TyVar "b"

    -- | Make a binary operator type (returns Type, not TypeScheme)
    binOpT :: Type -> Type -> Type -> Type
    binOpT t1 t2 t3 = TFun t1 (TFun t2 t3)

    -- | Make a binary operator type scheme (no type variables)
    binOp :: Type -> Type -> Type -> TypeScheme
    binOp t1 t2 t3 = Forall [] $ binOpT t1 t2 t3

    forallA :: Type -> TypeScheme
    forallA = Forall [a]

    forallAB :: Type -> TypeScheme
    forallAB = Forall [a, b]

    -- | forallA with binary op
    forallABinOp :: Type -> Type -> Type -> TypeScheme
    forallABinOp t1 t2 t3 = Forall [a] $ binOpT t1 t2 t3

    -- | forallAB with binary op
    forallABBinOp :: Type -> Type -> Type -> TypeScheme
    forallABBinOp t1 t2 t3 = Forall [a, b] $ binOpT t1 t2 t3

