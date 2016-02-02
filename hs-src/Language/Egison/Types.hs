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
    , mathExprToEgisonValue
    , mathPlus
    , mathMult
    , mathNegate
    , mathNumerator
    , mathDenominator
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

import Data.List (intercalate)
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
  | MathExpr MathExpr
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
 deriving (Eq)

symbolMathExpr :: String -> EgisonValue
symbolMathExpr name = (MathExpr (Div (Plus [(Term 1 [(Symbol name 1)])]) (Plus [(Term 1 [])])))

mathExprToEgisonValue :: MathExpr -> EgisonValue
mathExprToEgisonValue (Div p1 p2) = InductiveData "Div" [(polyExprToEgisonValue p1), (polyExprToEgisonValue p2)]

polyExprToEgisonValue :: PolyExpr -> EgisonValue
polyExprToEgisonValue (Plus ts) = InductiveData "Poly" [Collection (Sq.fromList (map termExprtoEgisonValue ts))]

termExprtoEgisonValue :: TermExpr -> EgisonValue
termExprtoEgisonValue (Term a xs) = InductiveData "Term" [toEgison a, Collection (Sq.fromList (map symbolExprtoEgisonValue xs))]

symbolExprtoEgisonValue :: SymbolExpr -> EgisonValue
symbolExprtoEgisonValue (Symbol x n) = InductiveData "Symbol" [symbolMathExpr x, toEgison n]

mathPlus :: MathExpr -> MathExpr -> MathExpr
mathPlus (Div m1 n1) (Div m2 n2) = Div (mathPlus' (mathMult' m1 n2) (mathMult' m2 n1)) (mathMult' n1 n2)

mathPlus' :: PolyExpr -> PolyExpr -> PolyExpr
mathPlus' (Plus ts1) (Plus ts2) = Plus (mathFold [] (ts1 ++ ts2))
 where
  mathFold :: [TermExpr] -> [TermExpr] -> [TermExpr]
  mathFold ret [] = filter (\t -> case t of
                                    (Term 0 (_:_)) -> False
                                    _ -> True)
                           ret
  mathFold ret ((Term a xs):ts) =
    if all (\(Term _ ys) -> xs /= ys) ret
      then mathFold (ret ++ [(Term a xs)]) ts
      else mathFold (map (\(Term b ys) -> if xs == ys
                                            then (Term (a + b) xs)
                                            else (Term b ys))
                         ret)
                    ts

mathMult :: MathExpr -> MathExpr -> MathExpr
mathMult (Div m1 n1) (Div m2 n2) = Div (mathMult' m1 m2) (mathMult' n1 n2)

mathMult' :: PolyExpr -> PolyExpr -> PolyExpr
mathMult' (Plus []) (Plus _) = Plus []
mathMult' (Plus _) (Plus []) = Plus []
mathMult' (Plus ts1) (Plus ts2) = foldl mathPlus' (Plus []) (map (\(Term a xs) -> (Plus (map (\(Term b ys) -> (Term (a * b) (xs ++ ys))) ts2))) ts1)

mathNegate :: MathExpr -> MathExpr
mathNegate (Div m n) = Div (mathNegate' m) n

mathNegate' :: PolyExpr -> PolyExpr
mathNegate' (Plus ts) = Plus (map (\(Term a xs) -> (Term (negate a) xs)) ts)

mathNumerator :: MathExpr -> MathExpr
mathNumerator (Div m _) = Div m (Plus [(Term 1 [])])

mathDenominator :: MathExpr -> MathExpr
mathDenominator (Div _ n) = Div n (Plus [(Term 1 [])])

type Matcher = EgisonValue

type PrimitiveFunc = WHNFData -> EgisonM WHNFData

instance Show EgisonValue where
  show (Char c) = "'" ++ [c] ++ "'"
  show (String str) = "\"" ++ T.unpack str ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (MathExpr mExpr) = show mExpr
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
 (MathExpr x) == (MathExpr y) = (x == y)
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
  toEgison i = MathExpr (Div (Plus [(Term i [])]) (Plus [(Term 1 [])]))
  fromEgison = liftError . fromIntegerValue

instance EgisonData Rational where
  toEgison r = MathExpr (Div (Plus [(Term (numerator r) [])]) (Plus [(Term (denominator r) [])]))
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
fromIntegerValue (MathExpr (Div (Plus [(Term x [])]) (Plus [(Term 1 [])]))) = return x
fromIntegerValue val = throwError $ TypeMismatch "integer" (Value val)

fromRationalValue :: EgisonValue -> Either EgisonError Rational
fromRationalValue (MathExpr (Div (Plus [(Term x [])]) (Plus [(Term y [])]))) = return (x % y)
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
fromIntegerWHNF (Value (MathExpr (Div (Plus [(Term x [])]) (Plus [(Term 1 [])])))) = return x
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
