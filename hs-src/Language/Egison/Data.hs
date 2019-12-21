{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

{- |
Module      : Language.Egison.Data
Copyright   : Satoshi Egi
Licence     : MIT

This module contains definitions for Egison internal data.
-}

module Language.Egison.Data
    (
    -- * Egison values
      EgisonValue (..)
    , Matcher
    , PrimitiveFunc
    , EgisonHashKey (..)
    , EgisonData (..)
    , Tensor (..)
    , HasTensor (..)
    -- * Scalar
    , symbolScalarData
    , symbolScalarData'
    , getSymId
    , getSymName
    , mathExprToEgison
    , egisonToScalarData
    , extractScalar
    , extractScalar'
    -- * Internal data
    , Object (..)
    , ObjectRef
    , WHNFData (..)
    , Intermediate (..)
    , Inner (..)
    , EgisonWHNF (..)
    -- * Environment
    , Env (..)
    , Binding
    , nullEnv
    , extendEnv
    , refVar
    -- * Pattern matching
    , Match
    , MatchingTree (..)
    , MatchingState (..)
    , PatternBinding
    , LoopPatContext (..)
    , SeqPatContext (..)
    -- * Errors
    , EgisonError (..)
    , liftError
    -- * Monads
    , EgisonM (..)
    , runEgisonM
    , liftEgisonM
    , fromEgisonM
    , FreshT (..)
    , Fresh
    , MonadFresh (..)
    , runFreshT
    , MatchM
    , matchFail
    , MList (..)
    , fromList
    , fromSeq
    , fromMList
    , msingleton
    , mfoldr
    , mappend
    , mconcat
    , mmap
    , mfor
    ) where

import           Prelude                   hiding (foldr, mappend, mconcat)

import           Control.Exception
import           Data.Typeable

import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.Identity
import           Control.Monad.Reader      (ReaderT)
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer      (WriterT)

import qualified Data.Array                as Array
import           Data.Foldable             (foldr, toList)
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap
import           Data.IORef
import           Data.Monoid               (Monoid)
import           Data.Sequence             (Seq)
import qualified Data.Sequence             as Sq
import qualified Data.Vector               as V

import           Data.List                 (intercalate)
import           Data.Text                 (Text)

import           Data.Ratio
import           System.IO

import           System.IO.Unsafe          (unsafePerformIO)

import           Language.Egison.AST
import           Language.Egison.MathExpr

--
-- Values
--

data EgisonValue =
    World
  | Char Char
  | String Text
  | Bool Bool
  | ScalarData ScalarData
  | TensorData (Tensor EgisonValue)
  | Float Double
  | InductiveData String [EgisonValue]
  | Tuple [EgisonValue]
  | Collection (Seq EgisonValue)
  | Array (Array.Array Integer EgisonValue)
  | IntHash (HashMap Integer EgisonValue)
  | CharHash (HashMap Char EgisonValue)
  | StrHash (HashMap Text EgisonValue)
  | UserMatcher Env [PatternDef]
  | Func (Maybe Var) Env [String] EgisonExpr
  | PartialFunc Env Integer EgisonExpr
  | CFunc (Maybe Var) Env String EgisonExpr
  | MemoizedFunc (Maybe Var) ObjectRef (IORef (HashMap [Integer] ObjectRef)) Env [String] EgisonExpr
  | Proc (Maybe String) Env [String] EgisonExpr
  | PatternFunc Env [String] EgisonPattern
  | PrimitiveFunc String PrimitiveFunc
  | IOFunc (EgisonM WHNFData)
  | Port Handle
  | Something
  | Undefined

type Matcher = EgisonValue

type PrimitiveFunc = WHNFData -> EgisonM WHNFData

data EgisonHashKey =
    IntKey Integer
  | CharKey Char
  | StrKey Text

--
-- Scalar and Tensor Types
--

data Tensor a =
    Tensor [Integer] (V.Vector a) [Index EgisonValue]
  | Scalar a
 deriving (Show)

class HasTensor a where
  tensorElems :: a -> V.Vector a
  tensorSize :: a -> [Integer]
  tensorIndices :: a -> [Index EgisonValue]
  fromTensor :: Tensor a -> EgisonM a
  toTensor :: a -> EgisonM (Tensor a)
  undef :: a

instance HasTensor EgisonValue where
  tensorElems (TensorData (Tensor _ xs _)) = xs
  tensorSize (TensorData (Tensor ns _ _)) = ns
  tensorIndices (TensorData (Tensor _ _ js)) = js
  fromTensor t@Tensor{} = return $ TensorData t
  fromTensor (Scalar x) = return x
  toTensor (TensorData t) = return t
  toTensor x              = return $ Scalar x
  undef = Undefined

instance HasTensor WHNFData where
  tensorElems (Intermediate (ITensor (Tensor _ xs _))) = xs
  tensorSize (Intermediate (ITensor (Tensor ns _ _))) = ns
  tensorIndices (Intermediate (ITensor (Tensor _ _ js))) = js
  fromTensor t@Tensor{} = return $ Intermediate $ ITensor t
  fromTensor (Scalar x) = return x
  toTensor (Intermediate (ITensor t)) = return t
  toTensor x                          = return $ Scalar x
  undef = Value Undefined

--
-- Scalars
--

symbolScalarData :: String -> String -> EgisonValue
symbolScalarData id name = ScalarData (SingleTerm 1 [(Symbol id name [], 1)])

symbolScalarData' :: String -> String -> ScalarData
symbolScalarData' id name = SingleTerm 1 [(Symbol id name [], 1)]

getSymId :: EgisonValue -> String
getSymId (ScalarData (SingleTerm 1 [(Symbol id _ [], 1)])) = id

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
scalarIndexToEgison (Superscript k) = InductiveData "Sup"  [ScalarData k]
scalarIndexToEgison (Subscript k)   = InductiveData "Sub"  [ScalarData k]
scalarIndexToEgison (Userscript k)  = InductiveData "User" [ScalarData k]

egisonToScalarData :: EgisonValue -> EgisonM ScalarData
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
egisonToScalarData val = throwError =<< TypeMismatch "math expression" (Value val) <$> getFuncNameStack

egisonToPolyExpr :: EgisonValue -> EgisonM PolyExpr
egisonToPolyExpr (InductiveData "Plus" [Collection ts]) = Plus <$> mapM egisonToTermExpr (toList ts)
egisonToPolyExpr val = throwError =<< TypeMismatch "math poly expression" (Value val) <$> getFuncNameStack

egisonToTermExpr :: EgisonValue -> EgisonM TermExpr
egisonToTermExpr (InductiveData "Term" [n, Collection ts]) = Term <$> fromEgison n <*> mapM egisonToSymbolExpr (toList ts)
egisonToTermExpr val = throwError =<< TypeMismatch "math term expression" (Value val) <$> getFuncNameStack

egisonToSymbolExpr :: EgisonValue -> EgisonM (SymbolExpr, Integer)
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
egisonToSymbolExpr val = throwError =<< TypeMismatch "math symbol expression" (Value val) <$> getFuncNameStack

egisonToScalarIndex :: EgisonValue -> EgisonM (Index ScalarData)
egisonToScalarIndex j = case j of
  InductiveData "Sup"  [ScalarData k] -> return (Superscript k)
  InductiveData "Sub"  [ScalarData k] -> return (Subscript k)
  InductiveData "User" [ScalarData k] -> return (Userscript k)
  _ -> throwError =<< TypeMismatch "math symbol expression" (Value j) <$> getFuncNameStack

--
-- ExtractScalar
--

extractScalar :: EgisonValue -> EgisonM ScalarData
extractScalar (ScalarData mExpr) = return mExpr
extractScalar val = throwError =<< TypeMismatch "math expression" (Value val) <$> getFuncNameStack

extractScalar' :: WHNFData -> EgisonM ScalarData
extractScalar' (Value (ScalarData x)) = return x
extractScalar' val = throwError =<< TypeMismatch "integer or string" val <$> getFuncNameStack

--
--
--

-- New-syntax version of EgisonValue pretty printer.
-- TODO(momohatt): Don't make it a show instance of EgisonValue.
instance Show EgisonValue where
  show (Char c) = '\'' : c : "'"
  show (String str) = show str
  show (Bool True) = "True"
  show (Bool False) = "False"
  show (ScalarData mExpr) = show mExpr
  show (TensorData (Tensor [_] xs js)) = "[| " ++ intercalate ", " (map show (V.toList xs)) ++ " |]" ++ concatMap show js
  show (TensorData (Tensor [0, 0] _ js)) = "[| [|  |] |]" ++ concatMap show js
  show (TensorData (Tensor [_, j] xs js)) = "[| " ++ intercalate ", " (f (fromIntegral j) (V.toList xs)) ++ " |]" ++ concatMap show js
    where
      f _ [] = []
      f j xs = ["[| " ++ intercalate ", " (map show (take j xs)) ++ " |]"] ++ f j (drop j xs)
  show (TensorData (Tensor ns xs js)) = "(tensor [" ++ intercalate ", " (map show ns) ++ "] [" ++ intercalate ", " (map show (V.toList xs)) ++ "] )" ++ concatMap show js
  show (Float x) = show x
  show (InductiveData name vals) = name ++ concatMap ((' ':) . show') vals
    where
      show' x | isAtomic x = show x
              | otherwise  = "(" ++ show x ++ ")"
  show (Tuple vals)      = "(" ++ intercalate ", " (map show vals) ++ ")"
  show (Collection vals) = "[" ++ intercalate ", " (map show (toList vals)) ++ "]"
  show (Array vals)      = "(| " ++ intercalate ", " (map show $ Array.elems vals) ++ " |)"
  show (IntHash hash)  = "{|" ++ intercalate ", " (map (\(key, val) -> "[" ++ show key ++ ", " ++ show val ++ "]") $ HashMap.toList hash) ++ "|}"
  show (CharHash hash) = "{|" ++ intercalate ", " (map (\(key, val) -> "[" ++ show key ++ ", " ++ show val ++ "]") $ HashMap.toList hash) ++ "|}"
  show (StrHash hash)  = "{|" ++ intercalate ", " (map (\(key, val) -> "[" ++ show key ++ ", " ++ show val ++ "]") $ HashMap.toList hash) ++ "|}"
  show UserMatcher{} = "#<user-matcher>"
  show (Func Nothing _ args _) = "(lambda [" ++ intercalate ", " (map show args) ++ "] ...)"
  show (Func (Just name) _ _ _) = show name
  show (PartialFunc _ n expr) = show n ++ "#" ++ show expr
  show (CFunc Nothing _ name _) = "(cambda " ++ name ++ " ...)"
  show (CFunc (Just name) _ _ _) = show name
  show (MemoizedFunc Nothing _ _ _ names _) = "(memoized-lambda [" ++ intercalate ", " names ++ "] ...)"
  show (MemoizedFunc (Just name) _ _ _ _ _) = show name
  show (Proc Nothing _ names _) = "(procedure [" ++ intercalate ", " names ++ "] ...)"
  show (Proc (Just name) _ _ _) = name
  show PatternFunc{} = "#<pattern-function>"
  show (PrimitiveFunc name _) = "#<primitive-function " ++ name ++ ">"
  show (IOFunc _) = "#<io-function>"
  show (Port _) = "#<port>"
  show Something = "something"
  show Undefined = "undefined"
  show World = "#<world>"

-- False if we have to put parenthesis around it to make it an atomic expression.
isAtomic :: EgisonValue -> Bool
isAtomic (InductiveData _ []) = True
isAtomic (InductiveData _ _)  = False
isAtomic (ScalarData (Div (Plus [Term _ []]) (Plus [Term 1 []]))) = True
isAtomic (ScalarData _) = False
isAtomic _ = True

instance Eq EgisonValue where
 (Char c) == (Char c') = c == c'
 (String str) == (String str') = str == str'
 (Bool b) == (Bool b') = b == b'
 (ScalarData x) == (ScalarData y) = x == y
 (TensorData (Tensor js xs _)) == (TensorData (Tensor js' xs' _)) = (js == js') && (xs == xs')
 (Float x) == (Float x') = x == x'
 (InductiveData name vals) == (InductiveData name' vals') = (name == name') && (vals == vals')
 (Tuple vals) == (Tuple vals') = vals == vals'
 (Collection vals) == (Collection vals') = vals == vals'
 (Array vals) == (Array vals') = vals == vals'
 (IntHash vals) == (IntHash vals') = vals == vals'
 (CharHash vals) == (CharHash vals') = vals == vals'
 (StrHash vals) == (StrHash vals') = vals == vals'
 (PrimitiveFunc name1 _) == (PrimitiveFunc name2 _) = name1 == name2
 -- Temporary: searching a better solution
 (Func Nothing _ xs1 expr1) == (Func Nothing _ xs2 expr2) = (xs1 == xs2) && (expr1 == expr2)
 (Func (Just name1) _ _ _) == (Func (Just name2) _ _ _) = name1 == name2
 (CFunc Nothing _ x1 expr1) == (CFunc Nothing _ x2 expr2) = (x1 == x2) && (expr1 == expr2)
 (CFunc (Just name1) _ _ _) == (CFunc (Just name2) _ _ _) = name1 == name2
 _ == _ = False

--
-- Egison data and Haskell data
--
class EgisonData a where
  toEgison :: a -> EgisonValue
  fromEgison :: EgisonValue -> EgisonM a

instance EgisonData Char where
  toEgison = Char
  fromEgison (Char c) = return c
  fromEgison val      = throwError =<< TypeMismatch "char" (Value val) <$> getFuncNameStack

instance EgisonData Text where
  toEgison = String
  fromEgison (String str) = return str
  fromEgison val          = throwError =<< TypeMismatch "string" (Value val) <$> getFuncNameStack

instance EgisonData Bool where
  toEgison = Bool
  fromEgison (Bool b) = return b
  fromEgison val      = throwError =<< TypeMismatch "bool" (Value val) <$> getFuncNameStack

instance EgisonData Integer where
  toEgison 0 = ScalarData $ mathNormalize' (Div (Plus []) (Plus [Term 1 []]))
  toEgison i = ScalarData $ mathNormalize' (SingleTerm i [])
  fromEgison (ScalarData (Div (Plus []) (Plus [Term 1 []]))) = return 0
  fromEgison (ScalarData (SingleTerm x [])) = return x
  fromEgison val = throwError =<< TypeMismatch "integer" (Value val) <$> getFuncNameStack

instance EgisonData Rational where
  toEgison r = ScalarData $ mathNormalize' (Div (Plus [Term (numerator r) []]) (Plus [Term (denominator r) []]))
  fromEgison (ScalarData (Div (Plus []) _)) = return 0
  fromEgison (ScalarData (Div (Plus [Term x []]) (Plus [Term y []]))) = return (x % y)
  fromEgison val = throwError =<< TypeMismatch "rational" (Value val) <$> getFuncNameStack

instance EgisonData Double where
  toEgison f = Float f
  fromEgison (Float f) = return f
  fromEgison val       = throwError =<< TypeMismatch "float" (Value val) <$> getFuncNameStack

instance EgisonData Handle where
  toEgison = Port
  fromEgison (Port h) = return h
  fromEgison val      = throwError =<< TypeMismatch "port" (Value val) <$> getFuncNameStack

instance EgisonData a => EgisonData [a] where
  toEgison xs = Collection $ Sq.fromList (map toEgison xs)
  fromEgison (Collection seq) = mapM fromEgison (toList seq)
  fromEgison val = throwError =<< TypeMismatch "collection" (Value val) <$> getFuncNameStack

instance EgisonData () where
  toEgison () = Tuple []
  fromEgison (Tuple []) = return ()
  fromEgison val = throwError =<< TypeMismatch "zero element tuple" (Value val) <$> getFuncNameStack

instance (EgisonData a, EgisonData b) => EgisonData (a, b) where
  toEgison (x, y) = Tuple [toEgison x, toEgison y]
  fromEgison (Tuple [x, y]) = liftM2 (,) (fromEgison x) (fromEgison y)
  fromEgison val = throwError =<< TypeMismatch "two elements tuple" (Value val) <$> getFuncNameStack

instance (EgisonData a, EgisonData b, EgisonData c) => EgisonData (a, b, c) where
  toEgison (x, y, z) = Tuple [toEgison x, toEgison y, toEgison z]
  fromEgison (Tuple [x, y, z]) = do
    x' <- fromEgison x
    y' <- fromEgison y
    z' <- fromEgison z
    return (x', y', z')
  fromEgison val = throwError =<< TypeMismatch "two elements tuple" (Value val) <$> getFuncNameStack

instance (EgisonData a, EgisonData b, EgisonData c, EgisonData d) => EgisonData (a, b, c, d) where
  toEgison (x, y, z, w) = Tuple [toEgison x, toEgison y, toEgison z, toEgison w]
  fromEgison (Tuple [x, y, z, w]) = do
    x' <- fromEgison x
    y' <- fromEgison y
    z' <- fromEgison z
    w' <- fromEgison w
    return (x', y', z', w')
  fromEgison val = throwError =<< TypeMismatch "two elements tuple" (Value val) <$> getFuncNameStack

--
-- Internal Data
--

-- |For memoization
type ObjectRef = IORef Object

data Object =
    Thunk (EgisonM WHNFData)
  | WHNF WHNFData

data WHNFData =
    Intermediate Intermediate
  | Value EgisonValue

data Intermediate =
    IInductiveData String [ObjectRef]
  | ITuple [ObjectRef]
  | ICollection (IORef (Seq Inner))
  | IArray (Array.Array Integer ObjectRef)
  | IIntHash (HashMap Integer ObjectRef)
  | ICharHash (HashMap Char ObjectRef)
  | IStrHash (HashMap Text ObjectRef)
  | ITensor (Tensor WHNFData)

data Inner =
    IElement ObjectRef
  | ISubCollection ObjectRef

instance Show WHNFData where
  show (Value val) = show val
  show (Intermediate (IInductiveData name _)) = "<" ++ name ++ " ...>"
  show (Intermediate (ITuple _)) = "[...]"
  show (Intermediate (ICollection _)) = "{...}"
  show (Intermediate (IArray _)) = "(|...|)"
  show (Intermediate (IIntHash _)) = "{|...|}"
  show (Intermediate (ICharHash _)) = "{|...|}"
  show (Intermediate (IStrHash _)) = "{|...|}"
--  show (Intermediate (ITensor _)) = "[|...|]"
  show (Intermediate (ITensor (Tensor ns xs _))) = "[|" ++ show (length ns) ++ show (V.length xs) ++ "|]"

instance Show Object where
  show (Thunk _)   = "#<thunk>"
  show (WHNF whnf) = show whnf

instance Show ObjectRef where
  show _ = "#<ref>"

--
-- Extract data from WHNF
--
class EgisonData a => EgisonWHNF a where
  toWHNF :: a -> WHNFData
  fromWHNF :: WHNFData -> EgisonM a
  toWHNF = Value . toEgison

instance EgisonWHNF Char where
  fromWHNF (Value (Char c)) = return c
  fromWHNF whnf             = throwError =<< TypeMismatch "char" whnf <$> getFuncNameStack

instance EgisonWHNF Text where
  fromWHNF (Value (String str)) = return str
  fromWHNF whnf                 = throwError =<< TypeMismatch "string" whnf <$> getFuncNameStack

instance EgisonWHNF Bool where
  fromWHNF (Value (Bool b)) = return b
  fromWHNF whnf             = throwError =<< TypeMismatch "bool" whnf <$> getFuncNameStack

instance EgisonWHNF Integer where
  fromWHNF (Value (ScalarData (Div (Plus []) (Plus [Term 1 []])))) = return 0
  fromWHNF (Value (ScalarData (SingleTerm x []))) = return x
  fromWHNF whnf = throwError =<< TypeMismatch "integer" whnf <$> getFuncNameStack

instance EgisonWHNF Double where
  fromWHNF (Value (Float f)) = return f
  fromWHNF whnf              = throwError =<< TypeMismatch "float" whnf <$> getFuncNameStack

instance EgisonWHNF Handle where
  fromWHNF (Value (Port h)) = return h
  fromWHNF whnf             = throwError =<< TypeMismatch "port" whnf <$> getFuncNameStack

--
-- Environment
--

data Env = Env [HashMap Var ObjectRef] (Maybe VarWithIndices)
 deriving (Show)

type Binding = (Var, ObjectRef)

instance Show (Index EgisonValue) where
  show (Superscript i) = case i of
    ScalarData (SingleTerm 1 [(Symbol _ _ (_:_), 1)]) -> "~[" ++ show i ++ "]"
    _ -> "~" ++ show i
  show (Subscript i) = case i of
    ScalarData (SingleTerm 1 [(Symbol _ _ (_:_), 1)]) -> "_[" ++ show i ++ "]"
    _ -> "_" ++ show i
  show (SupSubscript i) = "~_" ++ show i
  show (DFscript i j) = "_d" ++ show i ++ show j
  show (Userscript i) = case i of
    ScalarData (SingleTerm 1 [(Symbol _ _ (_:_), 1)]) -> "_[" ++ show i ++ "]"
    _ -> "|" ++ show i

nullEnv :: Env
nullEnv = Env [] Nothing

extendEnv :: Env -> [Binding] -> Env
extendEnv (Env env idx) bdg = Env ((: env) $ HashMap.fromList bdg) idx

refVar :: Env -> Var -> Maybe ObjectRef
refVar (Env env _) var = msum $ map (HashMap.lookup var) env

--
-- Pattern Match
--

type Match = [Binding]

data MatchingState
  = MState { mStateEnv      :: Env
           , loopPatCtx     :: [LoopPatContext]
           , seqPatCtx      :: [SeqPatContext]
           , mStateBindings :: [Binding]
           , mTrees         :: [MatchingTree]
           }

instance Show MatchingState where
  show ms = "(MState " ++ unwords ["_", "_", "_", show (mStateBindings ms), show (mTrees ms)] ++ ")"

data MatchingTree =
    MAtom EgisonPattern WHNFData Matcher
  | MNode [PatternBinding] MatchingState
 deriving (Show)

type PatternBinding = (String, EgisonPattern)

data LoopPatContext = LoopPatContext Binding ObjectRef EgisonPattern EgisonPattern EgisonPattern
 deriving (Show)

data SeqPatContext = SeqPatContext [MatchingTree] EgisonPattern [Matcher] [WHNFData]
 deriving (Show)

--
-- Errors
--

type CallStack = [String]

data EgisonError =
    UnboundVariable String CallStack
  | TypeMismatch String WHNFData CallStack
  | ArgumentsNumWithNames [String] Int Int CallStack
  | ArgumentsNumPrimitive Int Int CallStack
  | TupleLength Int Int CallStack
  | InconsistentTensorSize CallStack
  | InconsistentTensorIndex CallStack
  | TensorIndexOutOfBounds Integer Integer CallStack
  | NotImplemented String CallStack
  | Assertion String CallStack
  | Parser String
  | EgisonBug String CallStack
  | MatchFailure String CallStack
  | Default String
  deriving Typeable

instance Show EgisonError where
  show (UnboundVariable var stack) =
    "Unbound variable: " ++ show var ++ showTrace stack
  show (TypeMismatch expected found stack) =
    "Expected " ++  expected ++ ", but found: " ++ show found ++ showTrace stack
  show (ArgumentsNumWithNames names expected got stack) =
    "Wrong number of arguments: " ++ show names ++ ": expected " ++ show expected ++ ", but got " ++  show got ++ showTrace stack
  show (ArgumentsNumPrimitive expected got stack) =
    "Wrong number of arguments for a primitive function: expected " ++ show expected ++ ", but got " ++  show got ++ showTrace stack
  show (TupleLength expected got stack) =
    "Inconsistent tuple lengths: expected " ++ show expected ++ ", but got " ++  show got ++ showTrace stack
  show (InconsistentTensorSize stack) = "Inconsistent tensor size" ++ showTrace stack
  show (InconsistentTensorIndex stack) = "Inconsistent tensor index" ++ showTrace stack
  show (TensorIndexOutOfBounds m n stack) = "Tensor index out of bounds: " ++ show m ++ ", " ++ show n ++ showTrace stack
  show (NotImplemented message stack) = "Not implemented: " ++ message ++ showTrace stack
  show (Assertion message stack) = "Assertion failed: " ++ message ++ showTrace stack
  show (Parser err) = "Parse error at: " ++ err
  show (EgisonBug message stack) = "Egison Error: " ++ message ++ showTrace stack
  show (MatchFailure currentFunc stack) = "Failed pattern match in: " ++ currentFunc ++ showTrace stack
  show (Default message) = "Error: " ++ message

showTrace :: CallStack -> String
showTrace stack = "\n  stack trace: " ++ intercalate ", " stack

instance Exception EgisonError

liftError :: (MonadError e m) => Either e a -> m a
liftError = either throwError return

--
-- Monads
--

newtype EgisonM a = EgisonM {
    unEgisonM :: ExceptT EgisonError (FreshT IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadError EgisonError, MonadFresh)

instance MonadFail EgisonM where
    fail msg = throwError =<< EgisonBug msg <$> getFuncNameStack

runEgisonM :: EgisonM a -> FreshT IO (Either EgisonError a)
runEgisonM = runExceptT . unEgisonM

liftEgisonM :: Fresh (Either EgisonError a) -> EgisonM a
liftEgisonM m = EgisonM $ ExceptT $ FreshT $ do
  s <- get
  (a, s') <- return $ runFresh s m
  put s'
  return $ either throwError return a

fromEgisonM :: EgisonM a -> IO (Either EgisonError a)
fromEgisonM = modifyCounter . runEgisonM

{-# NOINLINE counter #-}
counter :: IORef Int
counter = unsafePerformIO $ newIORef 0

readCounter :: IO Int
readCounter = readIORef counter

updateCounter :: Int -> IO ()
updateCounter = writeIORef counter

modifyCounter :: FreshT IO a -> IO a
modifyCounter m = do
  x <- readCounter
  (result, st) <- runFreshT (RuntimeState { indexCounter = x, funcNameStack = [] }) m
  updateCounter $ indexCounter st
  return result

data RuntimeState = RuntimeState
    -- index counter for generating fresh variable
      { indexCounter :: Int
    -- names of called functions for improved error message
      , funcNameStack :: [String]
      }

newtype FreshT m a = FreshT { unFreshT :: StateT RuntimeState m a }
  deriving (Functor, Applicative, Monad, MonadState RuntimeState, MonadTrans)

type Fresh = FreshT Identity

class (Applicative m, Monad m) => MonadFresh m where
  fresh :: m String
  freshV :: m Var
  pushFuncName :: String -> m ()
  topFuncName :: m String
  popFuncName :: m ()
  getFuncNameStack :: m [String]

instance (Applicative m, Monad m) => MonadFresh (FreshT m) where
  fresh = FreshT $ do
    st <- get; modify (\st -> st { indexCounter = indexCounter st + 1 })
    return $ "$_" ++ show (indexCounter st)
  freshV = FreshT $ do
    st <- get; modify (\st -> st {indexCounter = indexCounter st + 1 })
    return $ Var ["$_" ++ show (indexCounter st)] []
  pushFuncName name = FreshT $ do
    st <- get
    put $ st { funcNameStack = name : funcNameStack st }
    return ()
  topFuncName = FreshT $ head . funcNameStack <$> get
  popFuncName = FreshT $ do
    st <- get
    put $ st { funcNameStack = tail $ funcNameStack st }
    return ()
  getFuncNameStack = FreshT $ funcNameStack <$> get

instance (MonadError e m) => MonadError e (FreshT m) where
  throwError = lift . throwError
  catchError m h = FreshT $ catchError (unFreshT m) (unFreshT . h)

instance (MonadState s m) => MonadState s (FreshT m) where
  get = lift get
  put s = lift $ put s

instance (MonadFresh m) => MonadFresh (StateT s m) where
  fresh = lift fresh
  freshV = lift freshV
  pushFuncName name = lift $ pushFuncName name
  topFuncName = lift topFuncName
  popFuncName = lift popFuncName
  getFuncNameStack = lift getFuncNameStack

instance (MonadFresh m) => MonadFresh (ExceptT e m) where
  fresh = lift fresh
  freshV = lift freshV
  pushFuncName name = lift $ pushFuncName name
  topFuncName = lift topFuncName
  popFuncName = lift popFuncName
  getFuncNameStack = lift getFuncNameStack

instance (MonadFresh m, Monoid e) => MonadFresh (ReaderT e m) where
  fresh = lift fresh
  freshV = lift freshV
  pushFuncName name = lift $ pushFuncName name
  topFuncName = lift topFuncName
  popFuncName = lift popFuncName
  getFuncNameStack = lift getFuncNameStack

instance (MonadFresh m, Monoid e) => MonadFresh (WriterT e m) where
  fresh = lift fresh
  freshV = lift freshV
  pushFuncName name = lift $ pushFuncName name
  topFuncName = lift topFuncName
  popFuncName = lift popFuncName
  getFuncNameStack = lift getFuncNameStack

instance MonadIO (FreshT IO) where
  liftIO = lift

runFreshT :: Monad m => RuntimeState -> FreshT m a -> m (a, RuntimeState)
runFreshT = flip (runStateT . unFreshT)

runFresh :: RuntimeState -> Fresh a -> (a, RuntimeState)
runFresh seed m = runIdentity $ flip runStateT seed $ unFreshT m

--
-- MList
--

type MatchM = MaybeT EgisonM

matchFail :: MatchM a
matchFail = MaybeT $ return Nothing

data MList m a = MNil | MCons a (m (MList m a))

instance Show a => Show (MList m a) where
  show MNil        = "MNil"
  show (MCons x _) = "(MCons " ++ show x ++ " ...)"

fromList :: Monad m => [a] -> MList m a
fromList = foldr f MNil
 where f x xs = MCons x $ return xs

fromSeq :: Monad m => Seq a -> MList m a
fromSeq = foldr f MNil
 where f x xs = MCons x $ return xs

fromMList :: Monad m => MList m a -> m [a]
fromMList = mfoldr f $ return []
  where f x xs = (x:) <$> xs

msingleton :: Monad m => a -> MList m a
msingleton = flip MCons $ return MNil

mfoldr :: Monad m => (a -> m b -> m b) -> m b -> MList m a -> m b
mfoldr _ init MNil         = init
mfoldr f init (MCons x xs) = f x (xs >>= mfoldr f init)

mappend :: Monad m => MList m a -> m (MList m a) -> m (MList m a)
mappend xs ys = mfoldr ((return .) . MCons) ys xs

mconcat :: Monad m => MList m (MList m a) -> m (MList m a)
mconcat = mfoldr mappend $ return MNil

mmap :: Monad m => (a -> m b) -> MList m a -> m (MList m b)
mmap f = mfoldr g $ return MNil
  where g x xs = flip MCons xs <$> f x

mfor :: Monad m => MList m a -> (a -> m b) -> m (MList m b)
mfor = flip mmap
