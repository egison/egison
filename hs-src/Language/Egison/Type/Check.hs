{- |
Module      : Language.Egison.Type.Check
Licence     : MIT

This module provides the built-in type environment for Egison programs.
Note: Type checking is now handled by IInfer.hs. This module only provides
the built-in type environment.
-}

module Language.Egison.Type.Check
  ( -- * Built-in environment
    builtinEnv
  ) where

import           Language.Egison.IExpr      (stringToVar)
import           Language.Egison.Type.Env
import           Language.Egison.Type.Types

-- | Built-in type environment with primitive functions
builtinEnv :: TypeEnv
builtinEnv = extendEnvMany (map (\(name, scheme) -> (stringToVar name, scheme)) builtinTypes) emptyEnv

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

    -- | forallA with binary op
    forallABinOp :: Type -> Type -> Type -> TypeScheme
    forallABinOp t1 t2 t3 = Forall [a] [] $ binOpT t1 t2 t3

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
      , ("tensorShape", forallA $ TFun (TTensor (TVar a)) (TCollection TInt))
      , ("tensorToList", forallA $ TFun (TTensor (TVar a)) (TCollection (TVar a)))
      , ("dfOrder", forallA $ TFun (TTensor (TVar a)) TInt)
      ]

    -- Arithmetic operators (from Primitives.Arith.hs)
    -- Note: +, -, *, /, mod, ^, abs, neg, +., -., *., /., sqrt, exp, log, sin, cos, tan, etc.
    -- are defined in lib/ and are NOT included here
    arithTypes =
      [ -- Internal base operators
        ("i.+", binOp TInt TInt TInt)
      , ("i.-", binOp TInt TInt TInt)
      , ("i.*", binOp TInt TInt TInt)
      , ("i./", binOp TInt TInt TInt)
      -- Floating point arithmetic
      , ("f.+", binOp TFloat TFloat TFloat)
      , ("f.-", binOp TFloat TFloat TFloat)
      , ("f.*", binOp TFloat TFloat TFloat)
      , ("f./", binOp TFloat TFloat TFloat)
      -- Fraction operations
      , ("numerator", unaryOp TInt TInt)
      , ("denominator", unaryOp TInt TInt)
      -- MathExpr operations
      , ("fromMathExpr", unaryOp TInt (TInductive "MathExpr'" []))
      , ("toMathExpr'", unaryOp (TInductive "MathExpr'" []) TInt)
      , ("symbolNormalize", unaryOp TInt TInt)
      -- Integer operations
      , ("i.modulo", binOp TInt TInt TInt)
      , ("i.quotient", binOp TInt TInt TInt)
      , ("i.%", binOp TInt TInt TInt)
      , ("i.power", binOp TInt TInt TInt)
      , ("i.abs", unaryOp TInt TInt)
      , ("i.neg", unaryOp TInt TInt)
      , ("f.abs", unaryOp TFloat TFloat)
      , ("f.neg", unaryOp TFloat TFloat)
      -- Comparison operators
      , ("=", forallABinOp (TVar a) (TVar a) TBool)
      , ("<", forallABinOp (TVar a) (TVar a) TBool)
      , ("<=", forallABinOp (TVar a) (TVar a) TBool)
      , (">", forallABinOp (TVar a) (TVar a) TBool)
      , (">=", forallABinOp (TVar a) (TVar a) TBool)
      -- Primitive comparison aliases (to avoid type class method conflicts)
      , ("i.<", binOp TInt TInt TBool)
      , ("i.<=", binOp TInt TInt TBool)
      , ("i.>", binOp TInt TInt TBool)
      , ("i.>=", binOp TInt TInt TBool)
      , ("f.<", binOp TFloat TFloat TBool)
      , ("f.<=", binOp TFloat TFloat TBool)
      , ("f.>", binOp TFloat TFloat TBool)
      , ("f.>=", binOp TFloat TFloat TBool)
      -- Rounding functions
      , ("round", unaryOp TFloat TInt)
      , ("floor", unaryOp TFloat TInt)
      , ("ceiling", unaryOp TFloat TInt)
      , ("truncate", unaryOp TFloat TInt)
      -- Math functions
      , ("f.sqrt", unaryOp TFloat TFloat)
      , ("f.sqrt'", unaryOp TFloat TFloat)
      , ("f.exp", unaryOp TFloat TFloat)
      , ("f.log", unaryOp TFloat TFloat)
      , ("f.sin", unaryOp TFloat TFloat)
      , ("f.cos", unaryOp TFloat TFloat)
      , ("f.tan", unaryOp TFloat TFloat)
      , ("f.asin", unaryOp TFloat TFloat)
      , ("f.acos", unaryOp TFloat TFloat)
      , ("f.atan", unaryOp TFloat TFloat)
      , ("f.sinh", unaryOp TFloat TFloat)
      , ("f.cosh", unaryOp TFloat TFloat)
      , ("f.tanh", unaryOp TFloat TFloat)
      , ("f.asinh", unaryOp TFloat TFloat)
      , ("f.acosh", unaryOp TFloat TFloat)
      , ("f.atanh", unaryOp TFloat TFloat)
      ]


    -- IO functions (from Primitives.IO.hs)
    ioTypes =
      [ ("return", forallA $ TFun (TVar a) (TIO (TVar a)))
      , ("io", forallA $ TFun (TIO (TVar a)) (TVar a))
      -- File operations (Port type)
      , ("openInputFile", unaryOp TString (TIO TPort))
      , ("openOutputFile", unaryOp TString (TIO TPort))
      , ("closeInputPort", unaryOp TPort (TIO (TTuple [])))
      , ("closeOutputPort", unaryOp TPort (TIO (TTuple [])))
      -- Standard input/output
      , ("readChar", unaryOp (TTuple []) (TIO TChar))
      , ("readLine", unaryOp (TTuple []) (TIO TString))
      , ("writeChar", unaryOp TChar (TIO (TTuple [])))
      , ("write", forallA $ TFun (TVar a) (TIO (TTuple [])))
      -- Port-based input/output
      , ("readCharFromPort", unaryOp TPort (TIO TChar))
      , ("readLineFromPort", unaryOp TPort (TIO TString))
      , ("writeCharToPort", binOp TPort TChar (TIO (TTuple [])))
      , ("writeToPort", forallA $ binOpT TPort (TVar a) (TIO (TTuple [])))
      -- File operations
      , ("readFile", unaryOp TString (TIO TString))
      -- EOF checking
      , ("isEof", unaryOp (TTuple []) (TIO TBool))
      , ("isEofPort", unaryOp TPort (TIO TBool))
      -- Flushing
      , ("flush", unaryOp (TTuple []) (TIO (TTuple [])))
      , ("flushPort", unaryOp TPort (TIO (TTuple [])))
      -- Random numbers
      , ("rand", binOp TInt TInt (TIO TInt))
      , ("f.rand", binOp TFloat TFloat (TIO TFloat))
      -- IORef operations
      , ("newIORef", forallA $ TFun (TVar a) (TIO (TIORef (TVar a))))
      , ("writeIORef", forallA $ binOpT (TIORef (TVar a)) (TVar a) (TIO (TTuple [])))
      , ("readIORef", forallA $ TFun (TIORef (TVar a)) (TIO (TVar a)))
      -- Process operations
      , ("readProcess", Forall [a] [] $ ternOpT TString (TCollection TString) TString (TIO TString))
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
      [ ("pack", Forall [] [] $ TFun (TCollection TChar) TString)
      , ("unpack", Forall [] [] $ TFun TString (TCollection TChar))
      , ("unconsString", Forall [] [] $ TFun TString (TTuple [TChar, TString]))
      , ("lengthString", unaryOp TString TInt)
      , ("appendString", binOp TString TString TString)
      , ("splitString", binOp TString TString (TCollection TString))
      , ("regex", binOp TString TString (TCollection (TTuple [TString, TString, TString])))
      , ("regexCg", binOp TString TString (TCollection (TTuple [TString, TCollection TString, TString])))
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

