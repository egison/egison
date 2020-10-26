{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuasiQuotes           #-}

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
    -- * Environment
    , Env (..)
    , Binding
    , nullEnv
    , extendEnv
    , refVar
    -- * Errors
    , EgisonError (..)
    , throwErrorWithTrace
    -- * Monads
    , EvalM
    , fromEvalM
    , fromEvalT
    ) where

import           Control.Exception

import           Control.Monad.Except      hiding (join)
import           Control.Monad.Trans.State.Strict

import           Data.Foldable             (toList)
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap
import           Data.IORef
import           Data.Sequence             (Seq)
import qualified Data.Sequence             as Sq
import qualified Data.Vector               as V

import           Data.List                 (intercalate)
import           Data.Text                 (Text)
import           Text.Show.Unicode         (ushow)

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
  | Func (Maybe String) Env [String] IExpr
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
symbolExprToEgison (Apply fn mExprs, n) = Tuple [InductiveData "Apply" [ScalarData fn, Collection (Sq.fromList (map mathExprToEgison mExprs))], toEgison n]
symbolExprToEgison (Quote mExpr, n) = Tuple [InductiveData "Quote" [mathExprToEgison mExpr], toEgison n]
symbolExprToEgison (FunctionData name argnames args js, n) =
  Tuple [InductiveData "Function" [ScalarData name, Collection (Sq.fromList (map ScalarData argnames)), Collection (Sq.fromList (map ScalarData args)), f js], toEgison n]
 where
  f js = Collection (Sq.fromList (map scalarIndexToEgison js))

scalarIndexToEgison :: Index ScalarData -> EgisonValue
scalarIndexToEgison (Sup k)   = InductiveData "Sup"  [ScalarData k]
scalarIndexToEgison (Sub k)   = InductiveData "Sub"  [ScalarData k]
scalarIndexToEgison (User k)  = InductiveData "User" [ScalarData k]

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
egisonToScalarData s1@(InductiveData "Apply" _) = do
  s1' <- egisonToSymbolExpr (Tuple [s1, toEgison (1 :: Integer)])
  return $ SingleTerm 1 [s1']
egisonToScalarData s1@(InductiveData "Quote" _) = do
  s1' <- egisonToSymbolExpr (Tuple [s1, toEgison (1 :: Integer)])
  return $ SingleTerm 1 [s1']
egisonToScalarData s1@(InductiveData "Function" _) = do
  s1' <- egisonToSymbolExpr (Tuple [s1, toEgison (1 :: Integer)])
  return $ SingleTerm 1 [s1']
egisonToScalarData val = throwErrorWithTrace (TypeMismatch "math expression" (Value val))

egisonToPolyExpr :: EgisonValue -> EvalM PolyExpr
egisonToPolyExpr (InductiveData "Plus" [Collection ts]) = Plus <$> mapM egisonToTermExpr (toList ts)
egisonToPolyExpr val = throwErrorWithTrace (TypeMismatch "math poly expression" (Value val))

egisonToTermExpr :: EgisonValue -> EvalM TermExpr
egisonToTermExpr (InductiveData "Term" [n, Collection ts]) = Term <$> fromEgison n <*> mapM egisonToSymbolExpr (toList ts)
egisonToTermExpr val = throwErrorWithTrace (TypeMismatch "math term expression" (Value val))

egisonToSymbolExpr :: EgisonValue -> EvalM (SymbolExpr, Integer)
egisonToSymbolExpr (Tuple [InductiveData "Symbol" [x, Collection seq], n]) = do
  let js = toList seq
  js' <- mapM egisonToScalarIndex js
  n' <- fromEgison n
  case x of
    (ScalarData (Div (Plus [Term 1 [(Symbol id name [], 1)]]) (Plus [Term 1 []]))) ->
      return (Symbol id name js', n')
egisonToSymbolExpr (Tuple [InductiveData "Apply" [fn, Collection mExprs], n]) = do
  fn' <- extractScalar fn
  mExprs' <- mapM egisonToScalarData (toList mExprs)
  n' <- fromEgison n
  return (Apply fn' mExprs', n')
egisonToSymbolExpr (Tuple [InductiveData "Quote" [mExpr], n]) = do
  mExpr' <- egisonToScalarData mExpr
  n' <- fromEgison n
  return (Quote mExpr', n')
egisonToSymbolExpr (Tuple [InductiveData "Function" [name, Collection argnames, Collection args, Collection seq], n]) = do
  name' <- extractScalar name
  argnames' <- mapM extractScalar (toList argnames)
  args' <- mapM extractScalar (toList args)
  let js = toList seq
  js' <- mapM egisonToScalarIndex js
  n' <- fromEgison n
  return (FunctionData name' argnames' args' js', n')
egisonToSymbolExpr val = throwErrorWithTrace (TypeMismatch "math symbol expression" (Value val))

egisonToScalarIndex :: EgisonValue -> EvalM (Index ScalarData)
egisonToScalarIndex j = case j of
  InductiveData "Sup"  [ScalarData k] -> return (Sup k)
  InductiveData "Sub"  [ScalarData k] -> return (Sub k)
  InductiveData "User" [ScalarData k] -> return (User k)
  _ -> throwErrorWithTrace (TypeMismatch "math symbol expression" (Value j))

--
-- ExtractScalar
--

extractScalar :: EgisonValue -> EvalM ScalarData
extractScalar (ScalarData mExpr) = return mExpr
extractScalar val = throwErrorWithTrace (TypeMismatch "math expression" (Value val))

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
  show (Func _ _ args _) = "#<lambda [" ++ intercalate ", " (map show args) ++ "] ...>"
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

-- False if we have to put parenthesis around it to make it an atomic expression.
isAtomic :: EgisonValue -> Bool
isAtomic (InductiveData _ []) = True
isAtomic (InductiveData _ _)  = False
isAtomic (ScalarData m) = isAtom m
isAtomic _ = True

instance Eq EgisonValue where
  (Char c) == (Char c') = c == c'
  (String str) == (String str') = str == str'
  (Bool b) == (Bool b') = b == b'
  (ScalarData x) == (ScalarData y) = x == y
  (TensorData (Tensor js xs _)) == (TensorData (Tensor js' xs' _)) = js == js' && xs == xs'
  (Float x) == (Float x') = x == x'
  (InductiveData name vals) == (InductiveData name' vals') = name == name' && vals == vals'
  (Tuple vals) == (Tuple vals') = vals == vals'
  (Collection vals) == (Collection vals') = vals == vals'
  (IntHash vals) == (IntHash vals') = vals == vals'
  (CharHash vals) == (CharHash vals') = vals == vals'
  (StrHash vals) == (StrHash vals') = vals == vals'
  -- Temporary: searching a better solution
  (Func (Just name1) _ _ _) == (Func (Just name2) _ _ _) = name1 == name2
  _ == _ = False

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
  fromEgison (ScalarData (SingleTerm x [])) = return x
  fromEgison val = throwErrorWithTrace (TypeMismatch "integer" (Value val))

instance EgisonData Rational where
  toEgison r = ScalarData $ mathNormalize' (Div (Plus [Term (numerator r) []]) (Plus [Term (denominator r) []]))
  fromEgison (ScalarData (Div (Plus []) _)) = return 0
  fromEgison (ScalarData (Div (Plus [Term x []]) (Plus [Term y []]))) = return (x % y)
  fromEgison val = throwErrorWithTrace (TypeMismatch "rational" (Value val))

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
  fromEgison val = throwErrorWithTrace (TypeMismatch "collection" (Value val))

instance EgisonData () where
  toEgison () = Tuple []
  fromEgison (Tuple []) = return ()
  fromEgison val = throwErrorWithTrace (TypeMismatch "zero element tuple" (Value val))

instance (EgisonData a, EgisonData b) => EgisonData (a, b) where
  toEgison (x, y) = Tuple [toEgison x, toEgison y]
  fromEgison (Tuple [x, y]) = liftM2 (,) (fromEgison x) (fromEgison y)
  fromEgison val = throwErrorWithTrace (TypeMismatch "two elements tuple" (Value val))

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
  fromEgison val      = throwErrorWithTrace (TypeMismatch "ioRef" (Value val))

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

instance Show WHNFData where
  show (Value val) = show val
  show (IInductiveData name _) = "<" ++ name ++ " ...>"
  show (ITuple _) = "(...)"
  show (ICollection _) = "[...]"
  show (IIntHash _) = "{|...|}"
  show (ICharHash _) = "{|...|}"
  show (IStrHash _) = "{|...|}"
  show (ITensor (Tensor ns xs _)) = "[|" ++ show (length ns) ++ show (V.length xs) ++ "|]"
  show (ITensor (Scalar _)) = "scalar"

instance Show Object where
  show (Thunk _)   = "#<thunk>"
  show (WHNF whnf) = show whnf

instance Show ObjectRef where
  show _ = "#<ref>"

--
-- Environment
--

data Env = Env [HashMap Var ObjectRef] (Maybe (String, [Index String]))

type Binding = (Var, ObjectRef)

instance {-# OVERLAPPING #-} Show (Index EgisonValue) where
  show (Sup i) = case i of
    ScalarData (SingleTerm 1 [(Symbol _ _ (_:_), 1)]) -> "~[" ++ show i ++ "]"
    _ -> "~" ++ show i
  show (Sub i) = case i of
    ScalarData (SingleTerm 1 [(Symbol _ _ (_:_), 1)]) -> "_[" ++ show i ++ "]"
    _ -> "_" ++ show i
  show (SupSub i) = "~_" ++ show i
  show (User i) = case i of
    ScalarData (SingleTerm 1 [(Symbol _ _ (_:_), 1)]) -> "_[" ++ show i ++ "]"
    _ -> "|" ++ show i
  show (DF i j) = "_d" ++ show i ++ show j

nullEnv :: Env
nullEnv = Env [] Nothing

extendEnv :: Env -> [Binding] -> Env
extendEnv (Env env idx) bdg = Env (HashMap.fromList bdg : env) idx

refVar :: Env -> Var -> Maybe ObjectRef
refVar (Env env _) var@(Var _ []) = msum $ map (HashMap.lookup var) env
refVar e@(Env env _) var@(Var name is) =
  case msum $ map (HashMap.lookup var) env of
    Nothing -> refVar e (Var name (init is))
    Just x  -> Just x

--
-- Errors
--

type CallStack = [String]

data EgisonError
  = UnboundVariable String CallStack
  | TypeMismatch String WHNFData CallStack
  | ArgumentsNumPrimitive String Int Int CallStack
  | TupleLength Int Int CallStack
  | InconsistentTensorShape CallStack
  | InconsistentTensorIndex CallStack
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
  show (InconsistentTensorIndex stack) = "Inconsistent tensor index" ++ showTrace stack
  show (TensorIndexOutOfBounds m n stack) = "Tensor index out of bounds: " ++ show m ++ ", " ++ show n ++ showTrace stack
  show (NotImplemented message stack) = "Not implemented: " ++ message ++ showTrace stack
  show (Assertion message stack) = "Assertion failed: " ++ message ++ showTrace stack
  show (Parser err) = "Parse error at: " ++ err
  show (EgisonBug message stack) = "Egison Error: " ++ message ++ showTrace stack
  show (MatchFailure stack) = "Pattern match failed" ++ showTrace stack
  show (PrimitiveMatchFailure stack) = "Primitive data pattern match failed" ++ showTrace stack
  show (Default message) = "Error: " ++ message

showTrace :: CallStack -> String
showTrace stack = "\n  stack trace: " ++ intercalate ", " stack

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

fromEvalM :: EgisonOpts -> EvalM a -> IO (Either EgisonError a)
fromEvalM opts = evalRuntimeT opts . fromEvalT
