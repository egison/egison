{-# Language TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, UndecidableInstances, DeriveDataTypeable,
             TypeFamilies, TupleSections #-}
{- |
Module      : Language.Egison.Types
Copyright   : Satoshi Egi
Licence     : MIT

This module contains type definitions of Egison Data.
-}

module Language.Egison.Types
    (
    -- * Egison expressions
      EgisonTopExpr (..)
    , EgisonExpr (..)
    , EgisonPattern (..)
    , InnerExpr (..)
    , BindingExpr (..)
    , MatchClause (..)
    , MatcherInfo (..)
    , LoopRange (..)
    , PrimitivePatPattern (..)
    , PrimitiveDataPattern (..)
    -- * Egison values
    , EgisonValue (..)
    , MathExpr (..)
    , PolyExpr (..)
    , TermExpr (..)
    , SymbolExpr (..)
    , symbolMathExpr
    , mathExprToEgison
    , egisonToMathExpr
    , mathNormalize'
    , mathFold
    , mathRemoveZero
    , mathReduceFraction
    , mathReduceSymbolFraction
    , Matcher (..)
    , PrimitiveFunc (..)
    , EgisonData (..)
    , showTSV
    -- * Internal data
    , Object (..)
    , ObjectRef (..)
    , WHNFData (..)
    , Intermediate (..)
    , Inner (..)
    , EgisonWHNF (..)
    -- * Environment
    , Env (..)
    , Var (..)
    , Binding (..)
    , nullEnv
    , extendEnv
    , refVar
    -- * Pattern matching
    , Match
    , PMMode (..)
    , pmMode
    , MatchingState (..)
    , MatchingTree (..)
    , PatternBinding (..)
    , LoopPatContext (..)
    -- * Errors
    , EgisonError (..)
    , liftError
    -- * Monads
    , EgisonM (..)
    , runEgisonM
    , liftEgisonM
    , fromEgisonM
    , FreshT (..)
    , Fresh (..)
    , MonadFresh (..)
    , runFreshT
    , MatchM (..)
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

import Prelude hiding (foldr, mappend, mconcat)

import Control.Exception
import Data.Typeable

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader (ReaderT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Identity
import Control.Monad.Trans.Maybe

import Data.Monoid (Monoid)
import qualified Data.HashMap.Lazy as HL
import qualified Data.Array as Array
import qualified Data.Sequence as Sq
import Data.Sequence (Seq)
import Data.Foldable (foldr, toList)
import Data.IORef
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.List (intercalate, sort, sortBy)
import Data.Text (Text)
import qualified Data.Text as T

import System.IO
import Data.Ratio
import Numeric

import System.IO.Unsafe (unsafePerformIO)

--
-- Expressions
--

data EgisonTopExpr =
    Define String EgisonExpr
  | Test EgisonExpr
  | Execute EgisonExpr
    -- temporary : we will replace load to import and export
  | LoadFile String
  | Load String
 deriving (Show)

data EgisonExpr =
    CharExpr Char
  | StringExpr Text
  | BoolExpr Bool
  | IntegerExpr Integer
  | FloatExpr Double Double
  | VarExpr String
  | IndexedExpr EgisonExpr [EgisonExpr]
  | PowerExpr EgisonExpr EgisonExpr
  | InductiveDataExpr String [EgisonExpr]
  | TupleExpr [EgisonExpr]
  | CollectionExpr [InnerExpr]
  | ArrayExpr [EgisonExpr]
  | HashExpr [(EgisonExpr, EgisonExpr)]

  | LambdaExpr [String] EgisonExpr
  | MemoizedLambdaExpr [String] EgisonExpr
  | MemoizeExpr [(EgisonExpr, EgisonExpr, EgisonExpr)] EgisonExpr
  | PatternFunctionExpr [String] EgisonPattern
  
  | IfExpr EgisonExpr EgisonExpr EgisonExpr
  | LetRecExpr [BindingExpr] EgisonExpr
  | LetExpr [BindingExpr] EgisonExpr
  | LetStarExpr [BindingExpr] EgisonExpr

  | MatchExpr EgisonExpr EgisonExpr [MatchClause]
  | MatchAllExpr EgisonExpr EgisonExpr MatchClause
  | MatchLambdaExpr EgisonExpr [MatchClause]
  | MatchAllLambdaExpr EgisonExpr MatchClause

  | NextMatchExpr EgisonExpr EgisonExpr [MatchClause]
  | NextMatchAllExpr EgisonExpr EgisonExpr MatchClause
  | NextMatchLambdaExpr EgisonExpr [MatchClause]
  | NextMatchAllLambdaExpr EgisonExpr MatchClause

  | MatcherBFSExpr MatcherInfo
  | MatcherDFSExpr MatcherInfo
  
  | DoExpr [BindingExpr] EgisonExpr
  | IoExpr EgisonExpr
    
  | SeqExpr EgisonExpr EgisonExpr
  | ContExpr
  | ApplyExpr EgisonExpr EgisonExpr
  | PartialExpr Integer EgisonExpr
  | PartialVarExpr Integer
  | RecVarExpr

  | AlgebraicDataMatcherExpr [(String, [EgisonExpr])]
  | GenerateArrayExpr [String] EgisonExpr EgisonExpr
  | ArraySizeExpr EgisonExpr
  | ArrayRefExpr EgisonExpr EgisonExpr

  | SomethingExpr
  | UndefinedExpr
 deriving (Show)

data InnerExpr =
    ElementExpr EgisonExpr
  | SubCollectionExpr EgisonExpr
 deriving (Show)

type BindingExpr = ([String], EgisonExpr)
type MatchClause = (EgisonPattern, EgisonExpr)
type MatcherInfo = [(PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])]

data EgisonPattern =
    WildCard
  | PatVar String
  | ValuePat EgisonExpr
  | RegexPat EgisonExpr
  | PredPat EgisonExpr
  | IndexedPat EgisonPattern [EgisonExpr]
  | LetPat [BindingExpr] EgisonPattern
  | NotPat EgisonPattern
  | AndPat [EgisonPattern]
  | OrPat [EgisonPattern]
  | OrderedOrPat [EgisonPattern]
  | TuplePat [EgisonPattern]
  | InductivePat String [EgisonPattern]
  | LoopPat String LoopRange EgisonPattern EgisonPattern
  | ContPat
  | ApplyPat EgisonExpr [EgisonPattern]
  | VarPat String
 deriving (Show)

data LoopRange = LoopRange EgisonExpr EgisonExpr EgisonPattern
 deriving (Show)

data PrimitivePatPattern =
    PPWildCard
  | PPPatVar
  | PPValuePat String
  | PPInductivePat String [PrimitivePatPattern]
 deriving (Show)

data PrimitiveDataPattern =
    PDWildCard
  | PDPatVar String
  | PDInductivePat String [PrimitiveDataPattern]
  | PDEmptyPat
  | PDConsPat PrimitiveDataPattern PrimitiveDataPattern
  | PDSnocPat PrimitiveDataPattern PrimitiveDataPattern
  | PDConstantPat EgisonExpr
 deriving (Show)

--
-- Values
--

data EgisonValue =
    World
  | Char Char
  | String Text
  | Bool Bool
  | MathExpr Bool MathExpr
  | Float Double Double
  | InductiveData String [EgisonValue]
  | Tuple [EgisonValue]
  | Collection (Seq EgisonValue)
  | Array (Array.Array Integer EgisonValue)
  | IntHash (HashMap Integer EgisonValue)
  | CharHash (HashMap Char EgisonValue)
  | StrHash (HashMap Text EgisonValue)
  | UserMatcher Env PMMode MatcherInfo
  | Func Env [String] EgisonExpr
  | MemoizedFunc ObjectRef (IORef (HashMap [Integer] ObjectRef)) Env [String] EgisonExpr
  | PatternFunc Env [String] EgisonPattern
  | PrimitiveFunc PrimitiveFunc
  | IOFunc (EgisonM WHNFData)
  | Port Handle
  | Something
  | Undefined
  | EOF

data MathExpr =
    Div PolyExpr PolyExpr
 deriving (Eq)

data PolyExpr =
    Plus [TermExpr]
 deriving (Eq)

data TermExpr =
    Term Integer [SymbolExpr]
 deriving (Eq)

data SymbolExpr =
    Symbol String Integer
  | AppFun String [MathExpr] Integer
 deriving (Eq)


symbolMathExpr :: String -> EgisonValue
symbolMathExpr name = (MathExpr False (Div (Plus [(Term 1 [(Symbol name 1)])]) (Plus [(Term 1 [])])))

mathExprToEgison :: MathExpr -> EgisonValue
mathExprToEgison (Div p1 p2) = InductiveData "Div" [(polyExprToEgison p1), (polyExprToEgison p2)]

polyExprToEgison :: PolyExpr -> EgisonValue
polyExprToEgison (Plus ts) = InductiveData "Plus" [Collection (Sq.fromList (map termExprToEgison ts))]

termExprToEgison :: TermExpr -> EgisonValue
termExprToEgison (Term a xs) = InductiveData "Term" [toEgison a, Collection (Sq.fromList (map symbolExprToEgison xs))]

symbolExprToEgison :: SymbolExpr -> EgisonValue
symbolExprToEgison (Symbol x n) = InductiveData "Symbol" [toEgison (T.pack x), toEgison n]
symbolExprToEgison (AppFun name mExprs n) = InductiveData "Func" [toEgison (T.pack name), Collection (Sq.fromList (map mathExprToEgison mExprs)), toEgison n]

egisonToMathExpr :: EgisonValue -> EgisonM MathExpr
egisonToMathExpr (InductiveData "Div" [p1, p2]) = Div <$> egisonToPolyExpr p1 <*> egisonToPolyExpr p2
egisonToMathExpr p1@(InductiveData "Plus" _) = Div <$> egisonToPolyExpr p1 <*> (return (Plus [(Term 1 [])]))
egisonToMathExpr t1@(InductiveData "Term" _) = do
  t1' <- egisonToTermExpr t1
  return $ Div (Plus [t1']) (Plus [(Term 1 [])])
egisonToMathExpr s1@(InductiveData "Symbol" _) = do
  s1' <- egisonToSymbolExpr s1
  return $ Div (Plus [(Term 1 [s1'])]) (Plus [(Term 1 [])])
egisonToMathExpr s1@(InductiveData "Func" _) = do
  s1' <- egisonToSymbolExpr s1
  return $ Div (Plus [(Term 1 [s1'])]) (Plus [(Term 1 [])])
egisonToMathExpr val = liftError $ throwError $ TypeMismatch "math expression" (Value val)

egisonToPolyExpr :: EgisonValue -> EgisonM PolyExpr
egisonToPolyExpr (InductiveData "Plus" [Collection ts]) = Plus <$> mapM egisonToTermExpr (toList ts)
egisonToPolyExpr val = liftError $ throwError $ TypeMismatch "math poly expression" (Value val)

egisonToTermExpr :: EgisonValue -> EgisonM TermExpr
egisonToTermExpr (InductiveData "Term" [n, Collection ts]) = Term <$> fromEgison n <*> mapM egisonToSymbolExpr (toList ts)
egisonToTermExpr val = liftError $ throwError $ TypeMismatch "math term expression" (Value val)

egisonToSymbolExpr :: EgisonValue -> EgisonM SymbolExpr
egisonToSymbolExpr (InductiveData "Symbol" [x, n]) = do
  x' <- fromEgison x
  Symbol (T.unpack x') <$> (fromEgison n)
egisonToSymbolExpr (InductiveData "Func" [name, (Collection mExprs), n]) = do
  name' <- fromEgison name
  mExprs' <- mapM egisonToMathExpr (toList mExprs)
  n' <- fromEgison n
  return $ AppFun (T.unpack name') mExprs' n'
egisonToSymbolExpr val = liftError $ throwError $ TypeMismatch "math symbol expression" (Value val)

mathNormalize' :: MathExpr -> MathExpr
mathNormalize' mExpr = mathReduceSymbolFraction (mathReduceFraction (mathRemoveZero (mathFold (mathRemoveZeroSymbol mExpr))))

mathRemoveZeroSymbol :: MathExpr -> MathExpr
mathRemoveZeroSymbol (Div (Plus ts1) (Plus ts2)) =
  let p x = case x of
              (Symbol _ 0) -> False
              (AppFun _ _ 0) -> False
              _ -> True in
  let ts1' = map (\(Term a xs) -> Term a (filter p xs)) ts1 in
  let ts2' = map (\(Term a xs) -> Term a (filter p xs)) ts2 in
    Div (Plus ts1') (Plus ts2')

mathRemoveZero :: MathExpr -> MathExpr
mathRemoveZero (Div (Plus ts1) (Plus ts2)) =
  let ts1' = filter (\(Term a _) -> a /= 0) ts1 in
  let ts2' = filter (\(Term a _) -> a /= 0) ts2 in
    case ts1' of
      [] -> Div (Plus []) (Plus [Term 1 []])
      _ -> Div (Plus ts1') (Plus ts2')

mathReduceFraction :: MathExpr -> MathExpr
mathReduceFraction (Div (Plus []) (Plus ts2)) = Div (Plus []) (Plus ts2)
mathReduceFraction (Div (Plus [Term a xs]) (Plus [])) = Div (Plus [Term 1 xs]) (Plus [])
mathReduceFraction (Div (Plus ts1) (Plus ts2)) =
  let as1 = map (\(Term a _) -> a) ts1 in
  let as2 = map (\(Term a _) -> a) ts2 in
  let d = foldl gcd (head as1) ((tail as1) ++ as2) in
  let us1 = map (\(Term a xs) -> Term (a `quot` d) xs) ts1 in
  let us2 = map (\(Term a xs) -> Term (a `quot` d) xs) ts2 in
    Div (Plus us1) (Plus us2)

mathReduceSymbolFraction :: MathExpr -> MathExpr
mathReduceSymbolFraction (Div (Plus ts) (Plus ((Term a xs):[]))) = f xs [] ts
 where
  f :: [SymbolExpr] -> [SymbolExpr] -> [TermExpr] -> MathExpr
  f [] ret ts = Div (Plus ts) (Plus [Term a ret])
  f ((Symbol x n):xs) ret ts =
    let k = g x ts in
      if n > k
        then f xs (ret ++ [Symbol x (n - k)]) (h x k ts)
        else f xs ret (h x n ts)
  f ((AppFun x mExprs n):xs) ret ts =
    f xs (ret ++ [(AppFun x mExprs n)]) ts
  g :: String -> [TermExpr] -> Integer
  g x ts = minimum (map (\(Term _ xs) -> g' x xs) ts)
  g' :: String -> [SymbolExpr] -> Integer
  g' x [] = 0
  g' x ((Symbol y n):xs) = if x == y
                             then n
                             else g' x xs
  h :: String -> Integer -> [TermExpr] -> [TermExpr]
  h x k ts = map (\(Term a xs) -> Term a (filter (\(Symbol y n) -> n /= 0)
                                                 (map (\(Symbol y n) -> if x == y
                                                                          then Symbol y (n - k)
                                                                          else Symbol y n)
                                                      xs)))
                 ts
mathReduceSymbolFraction mExpr = mExpr

mathFold :: MathExpr -> MathExpr
mathFold (Div (Plus ts1) (Plus ts2)) = Div (Plus (mFold ts1)) (Plus (mFold ts2))
 where
  mFold :: [TermExpr] -> [TermExpr]
  mFold ts = mFold' [] ts
  mFold' :: [TermExpr] -> [TermExpr] -> [TermExpr]
  mFold' ret [] = ret
  mFold' ret ((Term a xs):ts) =
    let xs' = foldSymbolExpr xs in
    if all (\(Term _ ys) -> not (p xs' ys)) ret
      then mFold' (ret ++ [(Term a xs')]) ts
      else mFold' (map (\(Term b ys) -> if p xs' ys
                                          then (Term (a + b) ys)
                                          else (Term b ys))
                       ret)
                  ts
  foldSymbolExpr :: [SymbolExpr] -> [SymbolExpr]
  foldSymbolExpr xs = foldSymbolExpr' [] xs
  foldSymbolExpr' ret [] = ret
  foldSymbolExpr' ret (xn:ts) =
    if (any (sameSymbol xn) ret)
      then foldSymbolExpr' (map (addPower xn)
                                ret) ts
      else foldSymbolExpr' (ret ++ [xn]) ts

  sameSymbol :: SymbolExpr -> SymbolExpr -> Bool
  sameSymbol (Symbol x _) (Symbol y _) = x == y
  sameSymbol (AppFun x mExpr _) (AppFun y mExpr' _) = (x == y && mExpr == mExpr')
  sameSymbol _ _ = False

  addPower :: SymbolExpr -> SymbolExpr -> SymbolExpr
  addPower (Symbol x n) (Symbol y n') = if x == y 
                                          then (Symbol y (n + n'))
                                          else (Symbol y n')
  addPower (Symbol x n) _ = (Symbol x n)
  addPower (AppFun x mExpr n) (AppFun y mExpr' n') = if (x == y && mExpr == mExpr')
                                                       then (AppFun y mExpr' (n + n'))
                                                       else (AppFun y mExpr' n')
  addPower (AppFun x mExpr n) _ = (AppFun x mExpr n)

  p :: [SymbolExpr] -> [SymbolExpr] -> Bool
  p xs ys = (sortSymbolExpr xs) == (sortSymbolExpr ys)
  sortSymbolExpr :: [SymbolExpr] -> [SymbolExpr]
  sortSymbolExpr xs = sortBy h xs
  h :: SymbolExpr -> SymbolExpr -> Ordering
  h (Symbol x _) (Symbol y _) = compare x y
  h (Symbol x _) (AppFun y _ _) = compare x y
  h (AppFun x _ _) (Symbol y _) = compare x y
  h (AppFun x _ _) (AppFun y _ _) = compare x y

type Matcher = EgisonValue

type PrimitiveFunc = WHNFData -> EgisonM WHNFData

instance Show EgisonValue where
  show (Char c) = "'" ++ [c] ++ "'"
  show (String str) = "\"" ++ T.unpack str ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (MathExpr _ mExpr) = show mExpr
  show (Float x y) = showComplexFloat x y
  show (InductiveData name []) = "<" ++ name ++ ">"
  show (InductiveData name vals) = "<" ++ name ++ " " ++ unwords (map show vals) ++ ">"
  show (Tuple vals) = "[" ++ unwords (map show vals) ++ "]"
  show (Collection vals) = if Sq.null vals
                             then "{}"
                             else "{" ++ unwords (map show (toList vals)) ++ "}"
  show (Array vals) = "[|" ++ unwords (map show $ Array.elems vals) ++ "|]"
  show (IntHash hash) = "{|" ++ unwords (map (\(key, val) -> "[" ++ show key ++ " " ++ show val ++ "]") $ HashMap.toList hash) ++ "|}"
  show (CharHash hash) = "{|" ++ unwords (map (\(key, val) -> "[" ++ show key ++ " " ++ show val ++ "]") $ HashMap.toList hash) ++ "|}"
  show (StrHash hash) = "{|" ++ unwords (map (\(key, val) -> "[\"" ++ T.unpack key ++ "\" " ++ show val ++ "]") $ HashMap.toList hash) ++ "|}"
  show (UserMatcher _ BFSMode _) = "#<matcher-bfs>"
  show (UserMatcher _ DFSMode _) = "#<matcher-dfs>"
  show (Func _ names _) = "(lambda [" ++ unwords names ++ "] ...)"
  show (MemoizedFunc _ _ _ names _) = "(memoized-lambda [" ++ unwords names ++ "] ...)"
  show (PatternFunc _ _ _) = "#<pattern-function>"
  show (PrimitiveFunc _) = "#<primitive-function>"
  show (IOFunc _) = "#<io-function>"
  show (Port _) = "#<port>"
  show Something = "something"
  show Undefined = "undefined"
  show World = "#<world>"
  show EOF = "#<eof>"

instance Show MathExpr where
  show (Div p1 (Plus [(Term 1 [])])) = show p1
  show (Div p1 p2) = "(/ " ++ show p1 ++ " " ++ show p2 ++ ")"

instance Show PolyExpr where
  show (Plus []) = "0"
  show (Plus [t]) = show t
  show (Plus ts) = "(+ " ++ unwords (map show ts)  ++ ")"

instance Show TermExpr where
  show (Term a []) = show a
  show (Term 1 [x]) = show x
  show (Term 1 xs) = "(* " ++ unwords (map show xs) ++ ")"
  show (Term a xs) = "(* " ++ show a ++ " " ++ unwords (map show xs) ++ ")"

instance Show SymbolExpr where
  show (Symbol s 1) = s
  show (Symbol s n) = s ++ "^" ++ show n
  show (AppFun s mExprs 1) = "(" ++ s ++ " " ++ unwords (map show mExprs) ++ ")"
  show (AppFun s mExprs n) = "(" ++ s ++ " " ++ unwords (map show mExprs) ++ ")" ++ "^" ++ show n

showComplex :: (Num a, Eq a, Ord a, Show a) => a -> a -> String
showComplex x 0 = show x
showComplex 0 y = show y ++ "i"
showComplex x y = show x ++ (if y > 0 then "+" else "") ++ show y ++ "i"

showComplexFloat :: Double -> Double -> String
showComplexFloat x 0.0 = showFFloat Nothing x ""
showComplexFloat 0.0 y = showFFloat Nothing y "i"
showComplexFloat x y = (showFFloat Nothing x "") ++ (if y > 0 then "+" else "") ++ (showFFloat Nothing y "i")

showTSV :: EgisonValue -> String
showTSV (Tuple (val:vals)) = foldl (\r x -> r ++ "\t" ++ x) (show val) (map showTSV vals)
showTSV (Collection vals) = intercalate "\t" (map showTSV (toList vals))
showTSV val = show val

instance Eq EgisonValue where
 (Char c) == (Char c') = c == c'
 (String str) == (String str') = str == str'
 (Bool b) == (Bool b') = b == b'
 (MathExpr _ x) == (MathExpr _ y) = (x == y)
 (Float x y) == (Float x' y') = (x == x') && (y == y')
 (InductiveData name vals) == (InductiveData name' vals') = (name == name') && (vals == vals')
 (Tuple vals) == (Tuple vals') = vals == vals'
 (Collection vals) == (Collection vals') = vals == vals'
 (Array vals) == (Array vals') = vals == vals'
 (IntHash vals) == (IntHash vals') = vals == vals'
 (CharHash vals) == (CharHash vals') = vals == vals'
 (StrHash vals) == (StrHash vals') = vals == vals'
 _ == _ = False


--
-- Egison data and Haskell data
--
class EgisonData a where
  toEgison :: a -> EgisonValue
  fromEgison :: EgisonValue -> EgisonM a

instance EgisonData Char where
  toEgison c = Char c
  fromEgison = liftError . fromCharValue

instance EgisonData Text where
  toEgison str = String str
  fromEgison = liftError . fromStringValue

instance EgisonData Bool where
  toEgison b = Bool b
  fromEgison = liftError . fromBoolValue

instance EgisonData Integer where
  toEgison 0 = MathExpr False $ mathNormalize' (Div (Plus []) (Plus [(Term 1 [])]))
  toEgison i = MathExpr False $ mathNormalize' (Div (Plus [(Term i [])]) (Plus [(Term 1 [])]))
  fromEgison = liftError . fromIntegerValue

instance EgisonData Rational where
  toEgison r = MathExpr False $ mathNormalize' (Div (Plus [(Term (numerator r) [])]) (Plus [(Term (denominator r) [])]))
  fromEgison = liftError . fromRationalValue

instance EgisonData Double where
  toEgison f = Float f 0
  fromEgison = liftError . fromFloatValue

instance EgisonData Handle where
  toEgison h = Port h
  fromEgison = liftError . fromPortValue

instance (EgisonData a) => EgisonData [a] where
  toEgison xs = Collection $ Sq.fromList (map toEgison xs)
  fromEgison (Collection seq) = mapM fromEgison (toList seq)
  fromEgison val = liftError $ throwError $ TypeMismatch "collection" (Value val)

instance EgisonData () where
  toEgison () = Tuple []
  fromEgison (Tuple []) = return ()
  fromEgison val = liftError $ throwError $ TypeMismatch "zero element tuple" (Value val)

instance (EgisonData a, EgisonData b) => EgisonData (a, b) where
  toEgison (x, y) = Tuple [toEgison x, toEgison y]
  fromEgison (Tuple (x:y:[])) = (liftM2 (,)) (fromEgison x) (fromEgison y)
  fromEgison val = liftError $ throwError $ TypeMismatch "two elements tuple" (Value val)

instance (EgisonData a, EgisonData b, EgisonData c) => EgisonData (a, b, c) where
  toEgison (x, y, z) = Tuple [toEgison x, toEgison y, toEgison z]
  fromEgison (Tuple (x:y:z:[])) = do
    x' <- fromEgison x
    y' <- fromEgison y
    z' <- fromEgison z
    return (x', y', z')
  fromEgison val = liftError $ throwError $ TypeMismatch "two elements tuple" (Value val)

instance (EgisonData a, EgisonData b, EgisonData c, EgisonData d) => EgisonData (a, b, c, d) where
  toEgison (x, y, z, w) = Tuple [toEgison x, toEgison y, toEgison z, toEgison w]
  fromEgison (Tuple (x:y:z:w:[])) = do
    x' <- fromEgison x
    y' <- fromEgison y
    z' <- fromEgison z
    w' <- fromEgison w
    return (x', y', z', w')
  fromEgison val = liftError $ throwError $ TypeMismatch "two elements tuple" (Value val)

fromCharValue :: EgisonValue -> Either EgisonError Char
fromCharValue (Char c) = return c
fromCharValue val = throwError $ TypeMismatch "char" (Value val)

fromStringValue :: EgisonValue -> Either EgisonError Text
fromStringValue (String str) = return str
fromStringValue val = throwError $ TypeMismatch "string" (Value val)

fromBoolValue :: EgisonValue -> Either EgisonError Bool
fromBoolValue (Bool b) = return b
fromBoolValue val = throwError $ TypeMismatch "bool" (Value val)

fromIntegerValue :: EgisonValue -> Either EgisonError Integer
fromIntegerValue (MathExpr _ (Div (Plus []) (Plus [(Term 1 [])]))) = return 0
fromIntegerValue (MathExpr _ (Div (Plus [(Term x [])]) (Plus [(Term 1 [])]))) = return x
fromIntegerValue val = throwError $ TypeMismatch "integer" (Value val)

fromRationalValue :: EgisonValue -> Either EgisonError Rational
fromRationalValue (MathExpr _ (Div (Plus []) _)) = return 0
fromRationalValue (MathExpr _ (Div (Plus [(Term x [])]) (Plus [(Term y [])]))) = return (x % y)
fromRationalValue val = throwError $ TypeMismatch "rational" (Value val)

fromFloatValue :: EgisonValue -> Either EgisonError Double
fromFloatValue (Float f 0) = return f
fromFloatValue val = throwError $ TypeMismatch "float" (Value val)

fromPortValue :: EgisonValue -> Either EgisonError Handle
fromPortValue (Port h) = return h
fromPortValue val = throwError $ TypeMismatch "port" (Value val)

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

data Inner =
    IElement ObjectRef
  | ISubCollection ObjectRef
    
instance Show WHNFData where
  show (Value val) = show val 
  show (Intermediate (IInductiveData name _)) = "<" ++ name ++ " ...>"
  show (Intermediate (ITuple _)) = "[...]"
  show (Intermediate (ICollection _)) = "{...}"
  show (Intermediate (IArray _)) = "[|...|]" 
  show (Intermediate (IIntHash _)) = "{|...|}" 
  show (Intermediate (ICharHash _)) = "{|...|}" 
  show (Intermediate (IStrHash _)) = "{|...|}" 

instance Show Object where
  show (Thunk _) = "#<thunk>"
  show (WHNF whnf) = show whnf

instance Show ObjectRef where
  show _ = "#<ref>"

--
-- Extract data from WHNF
--
class (EgisonData a) => EgisonWHNF a where
  toWHNF :: a -> WHNFData
  fromWHNF :: WHNFData -> EgisonM a
  toWHNF = Value . toEgison
  
instance EgisonWHNF Char where
  fromWHNF = liftError . fromCharWHNF
  
instance EgisonWHNF Text where
  fromWHNF = liftError . fromStringWHNF
  
instance EgisonWHNF Bool where
  fromWHNF = liftError . fromBoolWHNF
  
instance EgisonWHNF Integer where
  fromWHNF = liftError . fromIntegerWHNF
  
instance EgisonWHNF Double where
  fromWHNF = liftError . fromFloatWHNF
  
instance EgisonWHNF Handle where
  fromWHNF = liftError . fromPortWHNF
  
fromCharWHNF :: WHNFData -> Either EgisonError Char
fromCharWHNF (Value (Char c)) = return c
fromCharWHNF whnf = throwError $ TypeMismatch "char" whnf

fromStringWHNF :: WHNFData -> Either EgisonError Text
fromStringWHNF (Value (String str)) = return str
fromStringWHNF whnf = throwError $ TypeMismatch "string" whnf

fromBoolWHNF :: WHNFData -> Either EgisonError Bool
fromBoolWHNF (Value (Bool b)) = return b
fromBoolWHNF whnf = throwError $ TypeMismatch "bool" whnf

fromIntegerWHNF :: WHNFData -> Either EgisonError Integer
fromIntegerWHNF (Value (MathExpr _ (Div (Plus []) (Plus [(Term 1 [])])))) = return 0
fromIntegerWHNF (Value (MathExpr _ (Div (Plus [(Term x [])]) (Plus [(Term 1 [])])))) = return x
fromIntegerWHNF whnf = throwError $ TypeMismatch "integer" whnf

fromFloatWHNF :: WHNFData -> Either EgisonError Double
fromFloatWHNF (Value (Float f 0)) = return f
fromFloatWHNF whnf = throwError $ TypeMismatch "float" whnf

fromPortWHNF :: WHNFData -> Either EgisonError Handle
fromPortWHNF (Value (Port h)) = return h
fromPortWHNF whnf = throwError $ TypeMismatch "port" whnf

class (EgisonWHNF a) => EgisonObject a where
  toObject :: a -> Object
  toObject = WHNF . toWHNF
  
--
-- Environment
--

type Env = [HashMap Var ObjectRef]
type Var = String
type Binding = (Var, ObjectRef)

nullEnv :: Env
nullEnv = []

extendEnv :: Env -> [Binding] -> Env
extendEnv env = (: env) . HashMap.fromList

refVar :: Env -> Var -> EgisonM ObjectRef
refVar env var = maybe (throwError $ UnboundVariable var) return
                       (msum $ map (HashMap.lookup var) env)

--
-- Pattern Match
--

type Match = [Binding]

data PMMode = BFSMode | DFSMode
 deriving (Show)

pmMode :: Matcher -> PMMode
pmMode (UserMatcher _ mode _) = mode
pmMode (Tuple _) = DFSMode
pmMode Something = DFSMode

data MatchingState = MState Env [LoopPatContext] [Binding] [MatchingTree]
 deriving (Show)

data MatchingTree =
    MAtom EgisonPattern ObjectRef Matcher
  | MNode [PatternBinding] MatchingState
 deriving (Show)

type PatternBinding = (Var, EgisonPattern)

data LoopPatContext = LoopPatContext Binding ObjectRef EgisonPattern EgisonPattern EgisonPattern
 deriving (Show)

--
-- Errors
--

data EgisonError =
    UnboundVariable Var
  | TypeMismatch String WHNFData
  | ArgumentsNumWithNames [String] Int Int
  | ArgumentsNumPrimitive Int Int
  | ArgumentsNum Int Int
  | NotImplemented String
  | Assertion String
  | Match String
  | Parser String
  | Desugar String
  | EgisonBug String
  | Default String
  deriving Typeable
    
instance Show EgisonError where
  show (Parser err) = "Parse error at: " ++ err
  show (UnboundVariable var) = "Unbound variable: " ++ var
  show (TypeMismatch expected found) = "Expected " ++  expected ++
                                        ", but found: " ++ show found
  show (ArgumentsNumWithNames names expected got) = "Wrong number of arguments: " ++ show names ++ ": expected " ++
                                                    show expected ++ ", but got " ++  show got
  show (ArgumentsNumPrimitive expected got) = "Wrong number of arguments for a primitive function: expected " ++
                                              show expected ++ ", but got " ++  show got
  show (ArgumentsNum expected got) = "Wrong number of arguments: expected " ++
                                      show expected ++ ", but got " ++  show got
  show (NotImplemented message) = "Not implemented: " ++ message
  show (Assertion message) = "Assertion failed: " ++ message
  show (Desugar message) = "Error: " ++ message
  show (EgisonBug message) = "Egison Error: " ++ message
  show (Default message) = "Error: " ++ message

instance Exception EgisonError

instance Error EgisonError where
  noMsg = Default "An error has occurred"
  strMsg = Default

liftError :: (MonadError e m) => Either e a -> m a
liftError = either throwError return

--
-- Monads
--

newtype EgisonM a = EgisonM {
    unEgisonM :: ErrorT EgisonError (FreshT IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadError EgisonError, MonadFresh)

runEgisonM :: EgisonM a -> FreshT IO (Either EgisonError a)
runEgisonM = runErrorT . unEgisonM

liftEgisonM :: Fresh (Either EgisonError a) -> EgisonM a
liftEgisonM m = EgisonM $ ErrorT $ FreshT $ do
  s <- get
  (a, s') <- return $ runFresh s m
  put s'
  return $ either throwError return $ a   
  
fromEgisonM :: EgisonM a -> IO (Either EgisonError a)
fromEgisonM = modifyCounter . runEgisonM

counter :: IORef Int
counter = unsafePerformIO (newIORef 0)

readCounter :: IO Int
readCounter = readIORef counter

updateCounter :: Int -> IO ()
updateCounter = writeIORef counter

modifyCounter :: FreshT IO a -> IO a
modifyCounter m = do
  seed <- readCounter
  (result, seed) <- runFreshT seed m 
  updateCounter seed
  return result  

newtype FreshT m a = FreshT { unFreshT :: StateT Int m a }
  deriving (Functor, Applicative, Monad, MonadState Int, MonadTrans)

type Fresh = FreshT Identity

class (Applicative m, Monad m) => MonadFresh m where
  fresh :: m String

instance (Applicative m, Monad m) => MonadFresh (FreshT m) where
  fresh = FreshT $ do counter <- get; modify (+ 1)
                      return $ "$_" ++ show counter

instance (MonadError e m) => MonadError e (FreshT m) where
  throwError = lift . throwError
  catchError m h = FreshT $ catchError (unFreshT m) (unFreshT . h)

instance (MonadState s m) => MonadState s (FreshT m) where
  get = lift $ get
  put s = lift $ put s

instance (MonadFresh m) => MonadFresh (StateT s m) where
  fresh = lift $ fresh

instance (MonadFresh m, Error e) => MonadFresh (ErrorT e m) where
  fresh = lift $ fresh

instance (MonadFresh m, Monoid e) => MonadFresh (ReaderT e m) where
  fresh = lift $ fresh

instance (MonadFresh m, Monoid e) => MonadFresh (WriterT e m) where
  fresh = lift $ fresh

instance MonadIO (FreshT IO) where
  liftIO = lift

runFreshT :: Monad m => Int -> FreshT m a -> m (a, Int)
runFreshT seed = flip (runStateT . unFreshT) seed

runFresh :: Int -> Fresh a -> (a, Int)
runFresh seed m = runIdentity $ flip runStateT seed $ unFreshT m


type MatchM = MaybeT EgisonM

matchFail :: MatchM a
matchFail = MaybeT $ return Nothing

data MList m a = MNil | MCons a (m (MList m a))

instance Show (MList m a) where
  show MNil = "MNil"
  show (MCons _ _) = "(MCons ... ...)"

fromList :: Monad m => [a] -> MList m a
fromList = foldr f MNil
 where f x xs = MCons x $ return xs

fromSeq :: Monad m => Seq a -> MList m a
fromSeq = foldr f MNil
 where f x xs = MCons x $ return xs

fromMList :: Monad m => MList m a -> m [a]
fromMList = mfoldr f $ return []
 where f x xs = xs >>= return . (x:)

msingleton :: Monad m => a -> MList m a
msingleton = flip MCons $ return MNil

mfoldr :: Monad m => (a -> m b -> m b) -> m b -> MList m a -> m b
mfoldr f init MNil = init
mfoldr f init (MCons x xs) = f x (xs >>= mfoldr f init)

mappend :: Monad m => MList m a -> m (MList m a) -> m (MList m a)
mappend xs ys = mfoldr ((return .) . MCons) ys xs

mconcat :: Monad m => MList m (MList m a) -> m (MList m a)
mconcat = mfoldr mappend $ return MNil

mmap :: Monad m => (a -> m b) -> MList m a -> m (MList m b)
mmap f = mfoldr g $ return MNil
 where g x xs = f x >>= return . flip MCons xs

mfor :: Monad m => MList m a -> (a -> m b) -> m (MList m b)
mfor = flip mmap
