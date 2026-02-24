{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{- |
Module      : Language.Egison.Data
Licence     : MIT

This module contains definitions for Egison internal data.
-}

module Language.Egison.Data
    (
    -- * Egison values
      EgisonValue (..)
    , Matcher
    , PrimitiveFunc
    , LazyPrimitiveFunc
    , EgisonHashKey (..)
    , EgisonData (..)
    , Tensor (..)
    , Shape
    -- * Scalar
    , symbolScalarData
    , symbolScalarData'
    , getSymId
    , getSymName
    , mathExprToEgison
    , egisonToScalarData
    , extractScalar
    -- * Internal data
    , Object (..)
    , ObjectRef
    , WHNFData (..)
    , Inner (..)
    , prettyFunctionName
    -- * Environment
    , Env (..)
    , EnvLayer
    , Binding
    , nullEnv
    , extendEnv
    , refVar
    , envToBindingList
    -- * Errors
    , EgisonError (..)
    , throwErrorWithTrace
    -- * Monads
    , EvalM
    , fromEvalM
    , fromEvalT
    , fromEvalTWithState
    ) where

import           Control.Exception

import           Control.Monad                    (liftM2)
import           Control.Monad.Except
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict

import           Data.Foldable                    (msum, toList)
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HashMap
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.IORef

import           Language.Egison.VarEntry         (VarEntry(..))
import           Data.Sequence                    (Seq)
import qualified Data.Sequence                    as Sq
import qualified Data.Vector                      as V

import           Data.List                        (intercalate, sortOn)
import           Data.Text                        (Text, pack, unpack)
import           Text.Show.Unicode                (ushow)

import           Data.Ratio
import           System.IO

import           Language.Egison.CmdOptions
import           Language.Egison.EvalState
import           Language.Egison.IExpr
import           Language.Egison.Math
import           Language.Egison.RState

--
-- Values
--

data EgisonValue
  = World
  | Char Char
  | String Text
  | Bool Bool
  | ScalarData ScalarData
  | TensorData (Tensor EgisonValue)
  | Float Double
  | InductiveData String [EgisonValue]
  | Tuple [EgisonValue]
  | Collection (Seq EgisonValue)
  | IntHash (HashMap Integer EgisonValue)
  | CharHash (HashMap Char EgisonValue)
  | StrHash (HashMap Text EgisonValue)
  | UserMatcher Env [IPatternDef]
  | Func (Maybe Var) Env [Var] IExpr
  | CFunc Env String IExpr
  | MemoizedFunc (IORef (HashMap [Integer] WHNFData)) Env [String] IExpr
  | PatternFunc Env [String] IPattern
  | PrimitiveFunc PrimitiveFunc
  | LazyPrimitiveFunc LazyPrimitiveFunc
  | IOFunc (EvalM WHNFData)
  | Port Handle
  | RefBox (IORef EgisonValue)
  | Something
  | Undefined
  -- | Type class method reference: dispatches based on argument type at runtime
  -- ClassMethodRef className methodName
  -- Looks up implementation from the instance environment in EvalState
  | ClassMethodRef String String
  -- MathExpr internal types for direct pattern matching
  | PolyExprData PolyExpr
  | TermExprData TermExpr
  | SymbolExprData SymbolExpr
  | IndexExprData (Index ScalarData)

type Matcher = EgisonValue

type PrimitiveFunc = [EgisonValue] -> EvalM EgisonValue
type LazyPrimitiveFunc = [WHNFData] -> EvalM WHNFData

data EgisonHashKey
  = IntKey Integer
  | CharKey Char
  | StrKey Text

--
-- Scalar and Tensor Types
--

data Tensor a
  = Tensor Shape (V.Vector a) [Index EgisonValue]
  | Scalar a
 deriving Show

type Shape = [Integer]

--
-- Scalars
--

symbolScalarData :: String -> String -> EgisonValue
symbolScalarData id name = ScalarData (SingleTerm 1 [(Symbol id name [], 1)])

symbolScalarData' :: String -> ScalarData
symbolScalarData' name = SingleTerm 1 [(Symbol "" name [], 1)]

getSymId :: EgisonValue -> String
getSymId (ScalarData (SingleTerm 1 [(Symbol id _ _, _)])) = id

getSymName :: EgisonValue -> String
getSymName (ScalarData (SingleTerm 1 [(Symbol _ name [], 1)])) = name

mathExprToEgison :: ScalarData -> EgisonValue
mathExprToEgison (Div p1 p2) = InductiveData "Div" [polyExprToEgison p1, polyExprToEgison p2]

polyExprToEgison :: PolyExpr -> EgisonValue
polyExprToEgison (Plus ts) = InductiveData "Plus" [Collection (Sq.fromList (map termExprToEgison ts))]

termExprToEgison :: TermExpr -> EgisonValue
termExprToEgison (Term a xs) = InductiveData "Term" [toEgison a, Collection (Sq.fromList (map symbolExprToEgison xs))]

symbolExprToEgison :: (SymbolExpr, Integer) -> EgisonValue
symbolExprToEgison (Symbol id x js, n) = Tuple [InductiveData "Symbol" [symbolScalarData id x, f js], toEgison n]
 where
  f js = Collection (Sq.fromList (map scalarIndexToEgison js))
symbolExprToEgison (Apply1 fn a1, n) = Tuple [InductiveData "Apply1" [ScalarData fn, ScalarData a1], toEgison n]
symbolExprToEgison (Apply2 fn a1 a2, n) = Tuple [InductiveData "Apply2" [ScalarData fn, ScalarData a1, ScalarData a2], toEgison n]
symbolExprToEgison (Apply3 fn a1 a2 a3, n) = Tuple [InductiveData "Apply3" [ScalarData fn, ScalarData a1, ScalarData a2, ScalarData a3], toEgison n]
symbolExprToEgison (Apply4 fn a1 a2 a3 a4, n) = Tuple [InductiveData "Apply4" [ScalarData fn, ScalarData a1, ScalarData a2, ScalarData a3, ScalarData a4], toEgison n]
symbolExprToEgison (Quote mExpr, n) = Tuple [InductiveData "Quote" [mathExprToEgison mExpr], toEgison n]
symbolExprToEgison (QuoteFunction (Value funcVal), n) = Tuple [InductiveData "QuoteFunction" [funcVal], toEgison n]
symbolExprToEgison (QuoteFunction whnf, n) = error $ "symbolExprToEgison: QuoteFunction with non-Value WHNF: " ++ show whnf
symbolExprToEgison (FunctionData name args, n) =
  Tuple [InductiveData "Function" [ScalarData name, Collection (Sq.fromList (map ScalarData args))], toEgison n]

scalarIndexToEgison :: Index ScalarData -> EgisonValue
scalarIndexToEgison (Sup k)  = InductiveData "Sup"  [ScalarData k]
scalarIndexToEgison (Sub k)  = InductiveData "Sub"  [ScalarData k]
scalarIndexToEgison (User k) = InductiveData "User" [ScalarData k]

-- Direct index conversion for primitive pattern matching
indexToEgison :: Index ScalarData -> EgisonValue
indexToEgison = IndexExprData

-- Implementation of 'toMathExpr' (Primitive function)
egisonToScalarData :: EgisonValue -> EvalM ScalarData
egisonToScalarData (InductiveData "Div" [p1, p2]) = Div <$> egisonToPolyExpr p1 <*> egisonToPolyExpr p2
egisonToScalarData p1@(InductiveData "Plus" _) = Div <$> egisonToPolyExpr p1 <*> return (Plus [Term 1 []])
egisonToScalarData t1@(InductiveData "Term" _) = do
  t1' <- egisonToTermExpr t1
  return $ Div (Plus [t1']) (Plus [Term 1 []])
egisonToScalarData s1@(InductiveData "Symbol" _) = do
  s1' <- egisonToSymbolExpr (Tuple [s1, toEgison (1 ::Integer)])
  return $ SingleTerm 1 [s1']
egisonToScalarData s1@(InductiveData "Apply1" _) = do
  s1' <- egisonToSymbolExpr (Tuple [s1, toEgison (1 :: Integer)])
  return $ SingleTerm 1 [s1']
egisonToScalarData s1@(InductiveData "Apply2" _) = do
  s1' <- egisonToSymbolExpr (Tuple [s1, toEgison (1 :: Integer)])
  return $ SingleTerm 1 [s1']
egisonToScalarData s1@(InductiveData "Apply3" _) = do
  s1' <- egisonToSymbolExpr (Tuple [s1, toEgison (1 :: Integer)])
  return $ SingleTerm 1 [s1']
egisonToScalarData s1@(InductiveData "Apply4" _) = do
  s1' <- egisonToSymbolExpr (Tuple [s1, toEgison (1 :: Integer)])
  return $ SingleTerm 1 [s1']
egisonToScalarData s1@(InductiveData "Quote" _) = do
  s1' <- egisonToSymbolExpr (Tuple [s1, toEgison (1 :: Integer)])
  return $ SingleTerm 1 [s1']
egisonToScalarData s1@(InductiveData "QuoteFunction" _) = do
  s1' <- egisonToSymbolExpr (Tuple [s1, toEgison (1 :: Integer)])
  return $ SingleTerm 1 [s1']
egisonToScalarData s1@(InductiveData "Function" _) = do
  s1' <- egisonToSymbolExpr (Tuple [s1, toEgison (1 :: Integer)])
  return $ SingleTerm 1 [s1']
egisonToScalarData (ScalarData s) = return s
egisonToScalarData val = throwErrorWithTrace (TypeMismatch "math expression" (Value val))

egisonToPolyExpr :: EgisonValue -> EvalM PolyExpr
egisonToPolyExpr (InductiveData "Plus" [Collection ts]) = Plus <$> mapM egisonToTermExpr (toList ts)
egisonToPolyExpr val                                    = throwErrorWithTrace (TypeMismatch "math poly expression" (Value val))

egisonToTermExpr :: EgisonValue -> EvalM TermExpr
egisonToTermExpr (InductiveData "Term" [n, Collection ts]) = Term <$> fromEgison n <*> mapM egisonToSymbolExpr (toList ts)
egisonToTermExpr val                                       = throwErrorWithTrace (TypeMismatch "math term expression" (Value val))

egisonToSymbolExpr :: EgisonValue -> EvalM (SymbolExpr, Integer)
egisonToSymbolExpr (Tuple [InductiveData "Symbol" [x, Collection seq], n]) = do
  let js = toList seq
  js' <- mapM egisonToScalarIndex js
  n' <- fromEgison n
  case x of
    (ScalarData (Div (Plus [Term 1 [(Symbol id name [], 1)]]) (Plus [Term 1 []]))) ->
      return (Symbol id name js', n')
egisonToSymbolExpr (Tuple [InductiveData "Apply1" [fn, a1], n]) = do
  fn' <- extractScalar fn
  a1' <- egisonToScalarData a1
  n' <- fromEgison n
  return (Apply1 fn' a1', n')
egisonToSymbolExpr (Tuple [InductiveData "Apply2" [fn, a1, a2], n]) = do
  fn' <- extractScalar fn
  a1' <- egisonToScalarData a1
  a2' <- egisonToScalarData a2
  n' <- fromEgison n
  return (Apply2 fn' a1' a2', n')
egisonToSymbolExpr (Tuple [InductiveData "Apply3" [fn, a1, a2, a3], n]) = do
  fn' <- extractScalar fn
  a1' <- egisonToScalarData a1
  a2' <- egisonToScalarData a2
  a3' <- egisonToScalarData a3
  n' <- fromEgison n
  return (Apply3 fn' a1' a2' a3', n')
egisonToSymbolExpr (Tuple [InductiveData "Apply4" [fn, a1, a2, a3, a4], n]) = do
  fn' <- extractScalar fn
  a1' <- egisonToScalarData a1
  a2' <- egisonToScalarData a2
  a3' <- egisonToScalarData a3
  a4' <- egisonToScalarData a4
  n' <- fromEgison n
  return (Apply4 fn' a1' a2' a3' a4', n')
egisonToSymbolExpr (Tuple [InductiveData "Quote" [mExpr], n]) = do
  mExpr' <- egisonToScalarData mExpr
  n' <- fromEgison n
  return (Quote mExpr', n')
egisonToSymbolExpr (Tuple [InductiveData "QuoteFunction" [funcVal], n]) = do
  n' <- fromEgison n
  return (QuoteFunction (Value funcVal), n')
egisonToSymbolExpr (Tuple [InductiveData "Function" [name, Collection args], n]) = do
  name' <- extractScalar name
  args' <- mapM extractScalar (toList args)
  n' <- fromEgison n
  return (FunctionData name' args', n')
egisonToSymbolExpr val = throwErrorWithTrace (TypeMismatch "math symbol expression" (Value val))

egisonToScalarIndex :: EgisonValue -> EvalM (Index ScalarData)
egisonToScalarIndex j = case j of
  InductiveData "Sup"  [ScalarData k] -> return (Sup k)
  InductiveData "Sub"  [ScalarData k] -> return (Sub k)
  InductiveData "User" [ScalarData k] -> return (User k)
  _                                   -> throwErrorWithTrace (TypeMismatch "math symbol expression" (Value j))

--
-- ExtractScalar
--

extractScalar :: EgisonValue -> EvalM ScalarData
extractScalar (ScalarData mExpr) = return mExpr
extractScalar val                = throwErrorWithTrace (TypeMismatch "math expression" (Value val))

extractString :: EgisonValue -> EvalM String
extractString (String t) = return (unpack t)
extractString val        = throwErrorWithTrace (TypeMismatch "string" (Value val))

-- New-syntax version of EgisonValue pretty printer.
-- TODO(momohatt): Don't make it a show instance of EgisonValue.
instance Show EgisonValue where
  show (Char c) = '\'' : c : "'"
  show (String str) = ushow str
  show (Bool True) = "True"
  show (Bool False) = "False"
  show (ScalarData mExpr) = show mExpr
  show (TensorData (Tensor [_] xs js)) = "[| " ++ intercalate ", " (map show (V.toList xs)) ++ " |]" ++ concatMap show js
  show (TensorData (Tensor [0, 0] _ js)) = "[| [|  |] |]" ++ concatMap show js
  show (TensorData (Tensor [_, j] xs js)) = "[| " ++ intercalate ", " (f (fromIntegral j) (V.toList xs)) ++ " |]" ++ concatMap show js
    where
      f _ [] = []
      f j xs = ("[| " ++ intercalate ", " (map show (take j xs)) ++ " |]") : f j (drop j xs)
  show (TensorData (Tensor ns xs js)) = "(tensor [" ++ intercalate ", " (map show ns) ++ "] [" ++ intercalate ", " (map show (V.toList xs)) ++ "] )" ++ concatMap show js
  show (Float x) = show x
  show (InductiveData name vals) = name ++ concatMap ((' ':) . show') vals
    where
      show' x | isAtomic x = show x
              | otherwise  = "(" ++ show x ++ ")"
  show (Tuple vals)      = "(" ++ intercalate ", " (map show vals) ++ ")"
  show (Collection vals) = "[" ++ intercalate ", " (map show (toList vals)) ++ "]"
  show (IntHash hash)  = "{|" ++ intercalate ", " (map (\(key, val) -> "[" ++ show key ++ ", " ++ show val ++ "]") $ HashMap.toList hash) ++ "|}"
  show (CharHash hash) = "{|" ++ intercalate ", " (map (\(key, val) -> "[" ++ show key ++ ", " ++ show val ++ "]") $ HashMap.toList hash) ++ "|}"
  show (StrHash hash)  = "{|" ++ intercalate ", " (map (\(key, val) -> "[" ++ show key ++ ", " ++ show val ++ "]") $ HashMap.toList hash) ++ "|}"
  show UserMatcher{} = "#<user-matcher>"
  show (Func maybeName _ args _) = case maybeName of
    Just name -> "#<lambda " ++ show name ++ " [" ++ intercalate ", " (map show args) ++ "] ...>"
    Nothing -> "#<lambda [" ++ intercalate ", " (map show args) ++ "] ...>"
  show (CFunc _ name _) = "#<cambda " ++ name ++ " ...>"
  show (MemoizedFunc _ _ names _) = "#<memoized-lambda [" ++ intercalate ", " names ++ "] ...>"
  show PatternFunc{} = "#<pattern-function>"
  show PrimitiveFunc{} = "#<primitive-function>"
  show LazyPrimitiveFunc{} = "#<primitive-function>"
  show IOFunc{} = "#<io-function>"
  show Port{}   = "#<port>"
  show RefBox{} = "#<refbox>"
  show Something = "something"
  show Undefined = "undefined"
  show World = "#<world>"
  show (ClassMethodRef clsName methName) = "#<class-method " ++ clsName ++ "." ++ methName ++ ">"
  -- MathExpr internal types
  show (PolyExprData polyExpr) = show polyExpr
  show (TermExprData termExpr) = show termExpr
  show (SymbolExprData symbolExpr) = show symbolExpr
  show (IndexExprData indexExpr) = show indexExpr

-- False if we have to put parenthesis around it to make it an atomic expression.
isAtomic :: EgisonValue -> Bool
isAtomic (InductiveData _ []) = True
isAtomic (InductiveData _ _)  = False
isAtomic (ScalarData m)       = isAtom m
isAtomic (PolyExprData _)     = False
isAtomic (TermExprData _)     = False
isAtomic (SymbolExprData _)   = False
isAtomic (IndexExprData _)    = False
isAtomic _                    = True

instance Eq EgisonValue where
  (Char c) == (Char c')                                            = c == c'
  (String str) == (String str')                                    = str == str'
  (Bool b) == (Bool b')                                            = b == b'
  (ScalarData x) == (ScalarData y)                                 = x == y
  (TensorData (Tensor js xs _)) == (TensorData (Tensor js' xs' _)) = js == js' && xs == xs'
  (Float x) == (Float x')                                          = x == x'
  (InductiveData name vals) == (InductiveData name' vals')         = name == name' && vals == vals'
  (Tuple vals) == (Tuple vals')                                    = vals == vals'
  (Collection vals) == (Collection vals')                          = vals == vals'
  (IntHash vals) == (IntHash vals')                                = vals == vals'
  (CharHash vals) == (CharHash vals')                              = vals == vals'
  (StrHash vals) == (StrHash vals')                                = vals == vals'
  -- MathExpr internal types
  (PolyExprData p) == (PolyExprData p')                            = p == p'
  (TermExprData t) == (TermExprData t')                            = t == t'
  (SymbolExprData s) == (SymbolExprData s')                        = s == s'
  (IndexExprData i) == (IndexExprData i')                          = i == i'
  -- Temporary: searching a better solution
  (Func (Just name1) _ _ _) == (Func (Just name2) _ _ _)           = name1 == name2
  _ == _                                                           = False

--
-- Egison data and Haskell data
--
class EgisonData a where
  toEgison :: a -> EgisonValue
  fromEgison :: EgisonValue -> EvalM a

instance EgisonData Char where
  toEgison = Char
  fromEgison (Char c) = return c
  fromEgison val      = throwErrorWithTrace (TypeMismatch "char" (Value val))

instance EgisonData Text where
  toEgison = String
  fromEgison (String str) = return str
  fromEgison val          = throwErrorWithTrace (TypeMismatch "string" (Value val))

instance EgisonData Bool where
  toEgison = Bool
  fromEgison (Bool b) = return b
  fromEgison val      = throwErrorWithTrace (TypeMismatch "bool" (Value val))

instance EgisonData Integer where
  toEgison 0 = ScalarData (Div (Plus []) (Plus [Term 1 []]))
  toEgison i = ScalarData (SingleTerm i [])
  fromEgison (ScalarData (Div (Plus []) (Plus [Term 1 []]))) = return 0
  fromEgison (ScalarData (SingleTerm x []))                  = return x
  fromEgison val                                             = throwErrorWithTrace (TypeMismatch "integer" (Value val))

instance EgisonData Rational where
  toEgison r = ScalarData $ mathNormalize' (Div (Plus [Term (numerator r) []]) (Plus [Term (denominator r) []]))
  fromEgison (ScalarData (Div (Plus []) _))                           = return 0
  fromEgison (ScalarData (Div (Plus [Term x []]) (Plus [Term y []]))) = return (x % y)
  fromEgison val                                                      = throwErrorWithTrace (TypeMismatch "rational" (Value val))

instance EgisonData Double where
  toEgison f = Float f
  fromEgison (Float f) = return f
  fromEgison val       = throwErrorWithTrace (TypeMismatch "float" (Value val))

instance EgisonData Handle where
  toEgison = Port
  fromEgison (Port h) = return h
  fromEgison val      = throwErrorWithTrace (TypeMismatch "port" (Value val))

instance EgisonData a => EgisonData [a] where
  toEgison xs = Collection $ Sq.fromList (map toEgison xs)
  fromEgison (Collection seq) = mapM fromEgison (toList seq)
  fromEgison val              = throwErrorWithTrace (TypeMismatch "collection" (Value val))

instance EgisonData () where
  toEgison () = Tuple []
  fromEgison (Tuple []) = return ()
  fromEgison val        = throwErrorWithTrace (TypeMismatch "zero element tuple" (Value val))

instance (EgisonData a, EgisonData b) => EgisonData (a, b) where
  toEgison (x, y) = Tuple [toEgison x, toEgison y]
  fromEgison (Tuple [x, y]) = liftM2 (,) (fromEgison x) (fromEgison y)
  fromEgison val            = throwErrorWithTrace (TypeMismatch "two elements tuple" (Value val))

instance (EgisonData a, EgisonData b, EgisonData c) => EgisonData (a, b, c) where
  toEgison (x, y, z) = Tuple [toEgison x, toEgison y, toEgison z]
  fromEgison (Tuple [x, y, z]) = do
    x' <- fromEgison x
    y' <- fromEgison y
    z' <- fromEgison z
    return (x', y', z')
  fromEgison val = throwErrorWithTrace (TypeMismatch "two elements tuple" (Value val))

instance (EgisonData a, EgisonData b, EgisonData c, EgisonData d) => EgisonData (a, b, c, d) where
  toEgison (x, y, z, w) = Tuple [toEgison x, toEgison y, toEgison z, toEgison w]
  fromEgison (Tuple [x, y, z, w]) = do
    x' <- fromEgison x
    y' <- fromEgison y
    z' <- fromEgison z
    w' <- fromEgison w
    return (x', y', z', w')
  fromEgison val = throwErrorWithTrace (TypeMismatch "two elements tuple" (Value val))

instance EgisonData (IORef EgisonValue) where
  toEgison = RefBox
  fromEgison (RefBox ref) = return ref
  fromEgison val          = throwErrorWithTrace (TypeMismatch "ioRef" (Value val))

--
-- Internal Data
--

-- |For memoization
type ObjectRef = IORef Object

data Object
  = Thunk (EvalM WHNFData)
  | WHNF WHNFData

data WHNFData
  = Value EgisonValue
  | IInductiveData String [ObjectRef]
  | ITuple [ObjectRef]
  | ICollection (IORef (Seq Inner))
  | IIntHash (HashMap Integer ObjectRef)
  | ICharHash (HashMap Char ObjectRef)
  | IStrHash (HashMap Text ObjectRef)
  | ITensor (Tensor ObjectRef)

data Inner
  = IElement ObjectRef
  | ISubCollection ObjectRef

-- Helper to extract function name from WHNFData for pretty printing
-- Returns Nothing for anonymous functions
prettyFunctionName :: WHNFData -> Maybe String
prettyFunctionName (Value (Func (Just (Var name _)) _ _ _)) = Just name
prettyFunctionName _ = Nothing

instance Show WHNFData where
  show (Value val)                = show val
  show (IInductiveData name _)    = "<" ++ name ++ " ...>"
  show (ITuple _)                 = "(...)"
  show (ICollection _)            = "[...]"
  show (IIntHash _)               = "{|...|}"
  show (ICharHash _)              = "{|...|}"
  show (IStrHash _)               = "{|...|}"
  show (ITensor (Tensor ns xs _)) = "[|" ++ show (length ns) ++ show (V.length xs) ++ "|]"
  show (ITensor (Scalar _))       = "scalar"

instance Show Object where
  show (Thunk _)   = "#<thunk>"
  show (WHNF whnf) = show whnf

instance Show ObjectRef where
  show _ = "#<ref>"

--
-- Environment
--

-- | Environment layer: maps base variable names to all bindings with that name
-- VarEntry list is sorted by index length (shortest first) for efficient prefix matching
type EnvLayer = Map String [VarEntry ObjectRef]

-- | Environment: list of layers (for scoping) plus optional index context
data Env = Env [EnvLayer] (Maybe (String, [Index (Maybe ScalarData)]))

type Binding = (Var, ObjectRef)

instance {-# OVERLAPPING #-} Show (Index EgisonValue) where
  show (Sup i) = case i of
    ScalarData (SingleTerm 1 [(Symbol _ _ (_:_), 1)]) -> "~[" ++ show i ++ "]"
    _                                                 -> "~" ++ show i
  show (Sub i) = case i of
    ScalarData (SingleTerm 1 [(Symbol _ _ (_:_), 1)]) -> "_[" ++ show i ++ "]"
    _                                                 -> "_" ++ show i
  show (SupSub i) = "~_" ++ show i
  show (User i) = case i of
    ScalarData (SingleTerm 1 [(Symbol _ _ (_:_), 1)]) -> "_[" ++ show i ++ "]"
    _                                                 -> "|" ++ show i
  show (DF i j) = "_df-" ++ show i ++ "-" ++ show j

nullEnv :: Env
nullEnv = Env [] Nothing

-- | Extend environment with new bindings
-- Groups bindings by base name and sorts by index length (shortest first)
extendEnv :: Env -> [Binding] -> Env
extendEnv (Env layers idx) bindings = Env (newLayer : layers) idx
  where
    -- Group bindings by base variable name
    grouped :: Map String [VarEntry ObjectRef]
    grouped = foldr insertBinding Map.empty bindings
    
    insertBinding :: Binding -> Map String [VarEntry ObjectRef] -> Map String [VarEntry ObjectRef]
    insertBinding (Var name indices, ref) acc =
      let entry = VarEntry indices ref
      in Map.insertWith combineEntries name [entry] acc
    
    -- Combine and sort entries by index length (shortest first)
    combineEntries :: [VarEntry ObjectRef] -> [VarEntry ObjectRef] -> [VarEntry ObjectRef]
    combineEntries new old = 
      sortByIndexLength (new ++ old)
    
    -- Sort VarEntry list by index length (ascending)
    sortByIndexLength :: [VarEntry ObjectRef] -> [VarEntry ObjectRef]
    sortByIndexLength = Data.List.sortOn (length . veIndices)
    
    newLayer = grouped

-- | Look up a variable in the environment
-- Search algorithm:
--   1. Try exact match
--   2. Try prefix match (find longer indices and auto-complete with #)
--   3. Try suffix removal (find shorter indices, pick longest match)
-- No recursion is used; all matching is done in a single pass to avoid infinite loops.
refVar :: Env -> Var -> Maybe ObjectRef
refVar (Env layers _) (Var name targetIndices) =
  -- Search through all layers
  msum $ map searchInLayer layers
  where
    searchInLayer :: EnvLayer -> Maybe ObjectRef
    searchInLayer layer =
      case Map.lookup name layer of
        Nothing -> Nothing
        Just entries ->
          -- 1. Try exact match first
          case findExactMatch targetIndices entries of
            Just ref -> Just ref
            Nothing ->
              -- 2. Try prefix matching (e_a matches e_i_j with wildcards)
              case findPrefixMatch targetIndices entries of
                Just ref -> Just ref
                Nothing ->
                  -- 3. Try suffix removal (e_i_j_k matches e_i_j, pick longest)
                  findSuffixMatch targetIndices entries
    
    -- Exact match: same length and same indices
    findExactMatch :: [Index (Maybe Var)] -> [VarEntry ObjectRef] -> Maybe ObjectRef
    findExactMatch indices entries =
      case [veValue e | e <- entries, veIndices e == indices] of
        (ref:_) -> Just ref
        [] -> Nothing
    
    -- Prefix matching: find shortest entry where target indices are a prefix
    -- Example: target [a] matches [i, j] in e_i_j (shortest match)
    findPrefixMatch :: [Index (Maybe Var)] -> [VarEntry ObjectRef] -> Maybe ObjectRef
    findPrefixMatch indices entries =
      -- entries are sorted by index length (ascending), so first match is shortest
      case [veValue e | e <- entries, isPrefixOfIndices indices (veIndices e)] of
        (ref:_) -> Just ref
        [] -> Nothing
    
    -- Suffix removal: find longest entry where stored indices are a prefix of target
    -- Example: target [i,j,k] matches e_i_j (stored [i,j]); prefer e_i_j over e_i
    -- Single pass, no recursion - safe from infinite loops
    findSuffixMatch :: [Index (Maybe Var)] -> [VarEntry ObjectRef] -> Maybe ObjectRef
    findSuffixMatch targetIndices entries =
      let suffixMatches = [e | e <- entries, storedIsPrefixOfTarget (veIndices e) targetIndices]
      in case sortByIndexLengthDesc suffixMatches of
        (e:_) -> Just (veValue e)
        [] -> Nothing
    
    -- stored is prefix of target: stored has fewer indices, first part of target matches
    storedIsPrefixOfTarget :: [Index (Maybe Var)] -> [Index (Maybe Var)] -> Bool
    storedIsPrefixOfTarget stored target =
      not (null target) &&
      length stored < length target &&
      stored == take (length stored) target
    
    sortByIndexLengthDesc :: [VarEntry ObjectRef] -> [VarEntry ObjectRef]
    sortByIndexLengthDesc = reverse . Data.List.sortOn (length . veIndices)
    
    -- Check if target is a prefix of candidate (for prefix matching)
    -- Example: [a] is prefix of [i, j]
    -- IMPORTANT: target must be non-empty to avoid matching everything
    isPrefixOfIndices :: [Index (Maybe Var)] -> [Index (Maybe Var)] -> Bool
    isPrefixOfIndices target candidate =
      not (null target) &&
      length target < length candidate &&
      target == take (length target) candidate

-- | Convert environment to list of bindings
-- Used for completion and debugging
envToBindingList :: Env -> [Binding]
envToBindingList (Env layers _) =
  [ (Var name (veIndices entry), veValue entry)
  | layer <- layers
  , (name, entries) <- Map.toList layer
  , entry <- entries
  ]

--
-- Errors
--

type CallStack = [Var]

data EgisonError
  = UnboundVariable String CallStack
  | TypeMismatch String WHNFData CallStack
  | ArgumentsNumPrimitive String Int Int CallStack
  | TupleLength Int Int CallStack
  | InconsistentTensorShape CallStack
  | InconsistentTensorIndex [String] [String] CallStack
  | TensorIndexOutOfBounds Integer Integer CallStack
  | NotImplemented String CallStack
  | Assertion String CallStack
  | Parser String
  | EgisonBug String CallStack
  | MatchFailure CallStack
  | PrimitiveMatchFailure CallStack
  | Default String

instance Show EgisonError where
  show (UnboundVariable var stack) =
    "Unbound variable: " ++ show var ++ showTrace stack
  show (TypeMismatch expected found stack) =
    "Expected " ++  expected ++ ", but found: " ++ show found ++ showTrace stack
  show (ArgumentsNumPrimitive name expected got stack) =
    "Wrong number of arguments for a primitive function '" ++ name ++ "': expected " ++ show expected ++ ", but got " ++  show got ++ showTrace stack
  show (TupleLength expected got stack) =
    "Inconsistent tuple lengths: expected " ++ show expected ++ ", but got " ++  show got ++ showTrace stack
  show (InconsistentTensorShape stack) = "Inconsistent tensor shape" ++ showTrace stack
  show (InconsistentTensorIndex expected actual stack) =
    "Inconsistent tensor index:\n" ++
    "  Expected pattern: [" ++ intercalate ", " expected ++ "]\n" ++
    "  Actual indices:   [" ++ intercalate ", " actual ++ "]" ++
    showTrace stack
  show (TensorIndexOutOfBounds m n stack) = "Tensor index out of bounds: " ++ show m ++ ", " ++ show n ++ showTrace stack
  show (NotImplemented message stack) = "Not implemented: " ++ message ++ showTrace stack
  show (Assertion message stack) = "Assertion failed: " ++ message ++ showTrace stack
  show (Parser err) = "Parse error at: " ++ err
  show (EgisonBug message stack) = "Egison Error: " ++ message ++ showTrace stack
  show (MatchFailure stack) = "Pattern match failed" ++ showTrace stack
  show (PrimitiveMatchFailure stack) = "Primitive data pattern match failed" ++ showTrace stack
  show (Default message) = "Error: " ++ message

showTrace :: CallStack -> String
showTrace stack = "\n  stack trace: " ++ intercalate ", " (map show stack)

instance Exception EgisonError

--
-- Monads
--

type EvalT m = StateT EvalState (ExceptT EgisonError m)

type EvalM = EvalT RuntimeM

throwErrorWithTrace :: (CallStack -> EgisonError) -> EvalM a
throwErrorWithTrace e = throwError . e =<< getFuncNameStack

instance MonadRuntime EvalM where
  fresh = lift $ lift fresh

fromEvalT :: EvalM a -> RuntimeM (Either EgisonError a)
fromEvalT m = runExceptT (evalStateT m initialEvalState)

-- | Run EvalM with a given EvalState (for REPL to preserve state between evaluations)
fromEvalTWithState :: EvalState -> EvalM a -> RuntimeM (Either EgisonError (a, EvalState))
fromEvalTWithState state m = do
  result <- runExceptT (runStateT m state)
  case result of
    Left err -> return $ Left err
    Right (val, state') -> return $ Right (val, state')

fromEvalM :: EgisonOpts -> EvalM a -> IO (Either EgisonError a)
fromEvalM opts = evalRuntimeT opts . fromEvalT
