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
    , Index (..)
    , InnerExpr (..)
    , BindingExpr (..)
    , MatchClause (..)
    , MatcherInfo (..)
    , LoopRange (..)
    , PrimitivePatPattern (..)
    , PrimitiveDataPattern (..)
    -- * Egison values
    , EgisonValue (..)
    , ScalarData (..)
    , PolyExpr (..)
    , TermExpr (..)
    , SymbolExpr (..)
    , TensorData (..)
    , Tensor (..)
    , scalarToUnitTensor
    , scalarToTensor
    , tMap
    , tMap2
    , tCheckIndex
    , tContract
    , tref
    , tref'
    , tSize
    , tToList
    , tIndex
    , initTensor
    , makeTensor
    , tensorIndices
    , symbolScalarData
    , mathExprToEgison
    , egisonToScalarData
    , mathNormalize'
    , mathFold
    , mathSymbolFold
    , mathTermFold
    , mathRemoveZero
    , mathDivide
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
    -- * Typing
    , isBool
    , isInteger
    , isRational
    , isSymbol
    , isScalar
    , isTensor
    , isTensorWithIndex
    , isBool'
    , isInteger'
    , isRational'
    , isScalar'
    , isFloat'
    , isComplex'
    , isTensor'
    , isTensorWithIndex'
    , isChar'
    , isString'
    , isCollection'
    , isArray'
    , isHash'
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
 deriving (Show, Eq)

data EgisonExpr =
    CharExpr Char
  | StringExpr Text
  | BoolExpr Bool
  | IntegerExpr Integer
  | FloatExpr Double Double
  | VarExpr String
  | IndexedExpr EgisonExpr [Index EgisonExpr]
  | PowerExpr EgisonExpr EgisonExpr
  | InductiveDataExpr String [EgisonExpr]
  | TupleExpr [EgisonExpr]
  | CollectionExpr [InnerExpr]
  | ArrayExpr [EgisonExpr]
  | HashExpr [(EgisonExpr, EgisonExpr)]
  | TensorExpr EgisonExpr EgisonExpr

  | LambdaExpr [String] EgisonExpr
  | MemoizedLambdaExpr [String] EgisonExpr
  | MemoizeExpr [(EgisonExpr, EgisonExpr, EgisonExpr)] EgisonExpr
  | CambdaExpr String EgisonExpr
  | MacroExpr [String] EgisonExpr
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
  | AlgebraicDataMatcherExpr [(String, [EgisonExpr])]

  | QuoteExpr EgisonExpr
  
  | DoExpr [BindingExpr] EgisonExpr
  | IoExpr EgisonExpr
    
  | SeqExpr EgisonExpr EgisonExpr
  | ApplyExpr EgisonExpr EgisonExpr
  | CApplyExpr EgisonExpr EgisonExpr
  | PartialExpr Integer EgisonExpr
  | PartialVarExpr Integer
  | RecVarExpr

  | GenerateArrayExpr EgisonExpr (EgisonExpr, EgisonExpr)
  | ArrayBoundsExpr EgisonExpr
  | ArrayRefExpr EgisonExpr EgisonExpr

  | GenerateTensorExpr EgisonExpr EgisonExpr
  | InitTensorExpr EgisonExpr EgisonExpr EgisonExpr EgisonExpr
  | TensorMapExpr EgisonExpr EgisonExpr
  | TensorMap2Expr EgisonExpr EgisonExpr EgisonExpr

  | SomethingExpr
  | UndefinedExpr
 deriving (Show, Eq)

data Index a =
    Subscript a
  | Superscript a
 deriving (Eq)

data InnerExpr =
    ElementExpr EgisonExpr
  | SubCollectionExpr EgisonExpr
 deriving (Show, Eq)

type BindingExpr = ([String], EgisonExpr)
type MatchClause = (EgisonPattern, EgisonExpr)
type MatcherInfo = [(PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])]

data EgisonPattern =
    WildCard
  | PatVar String
  | ValuePat EgisonExpr
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
  | PApplyPat EgisonExpr [EgisonPattern]
  | DApplyPat EgisonPattern [EgisonPattern]
  | VarPat String
 deriving (Show, Eq)

data LoopRange = LoopRange EgisonExpr EgisonExpr EgisonPattern
 deriving (Show, Eq)

data PrimitivePatPattern =
    PPWildCard
  | PPPatVar
  | PPValuePat String
  | PPInductivePat String [PrimitivePatPattern]
 deriving (Show, Eq)

data PrimitiveDataPattern =
    PDWildCard
  | PDPatVar String
  | PDInductivePat String [PrimitiveDataPattern]
  | PDTuplePat [PrimitiveDataPattern]
  | PDEmptyPat
  | PDConsPat PrimitiveDataPattern PrimitiveDataPattern
  | PDSnocPat PrimitiveDataPattern PrimitiveDataPattern
  | PDConstantPat EgisonExpr
 deriving (Show, Eq)

--
-- Values
--

data EgisonValue =
    World
  | Char Char
  | String Text
  | Bool Bool
  | ScalarData ScalarData
  | TensorData TensorData
  | Float Double Double
  | InductiveData String [EgisonValue]
  | Tuple [EgisonValue]
  | Collection (Seq EgisonValue)
  | Array (Array.Array Integer EgisonValue)
  | IntHash (HashMap Integer EgisonValue)
  | CharHash (HashMap Char EgisonValue)
  | StrHash (HashMap Text EgisonValue)
  | UserMatcher Env PMMode MatcherInfo
  | Func (Maybe String) Env [String] EgisonExpr
  | CFunc (Maybe String) Env String EgisonExpr
  | MemoizedFunc (Maybe String) ObjectRef (IORef (HashMap [Integer] ObjectRef)) Env [String] EgisonExpr
  | Macro [String] EgisonExpr
  | PatternFunc Env [String] EgisonPattern
  | PrimitiveFunc String PrimitiveFunc
  | IOFunc (EgisonM WHNFData)
  | Quote EgisonValue
  | Port Handle
  | Something
  | Undefined
  | EOF

--
-- Scalars
--

data ScalarData =
    Div PolyExpr PolyExpr
 deriving (Eq)

data PolyExpr =
    Plus [TermExpr]
 deriving (Eq)

data TermExpr =
    Term Integer [(SymbolExpr, Integer)]
 deriving (Eq)

data SymbolExpr =
    Symbol String [Index ScalarData]
  | Apply EgisonValue [ScalarData]
 deriving (Eq)

symbolScalarData :: String -> EgisonValue
symbolScalarData name = ScalarData (Div (Plus [(Term 1 [(Symbol name [], 1)])]) (Plus [(Term 1 [])]))

mathExprToEgison :: ScalarData -> EgisonValue
mathExprToEgison (Div p1 p2) = InductiveData "Div" [(polyExprToEgison p1), (polyExprToEgison p2)]

polyExprToEgison :: PolyExpr -> EgisonValue
polyExprToEgison (Plus ts) = InductiveData "Plus" [Collection (Sq.fromList (map termExprToEgison ts))]

termExprToEgison :: TermExpr -> EgisonValue
termExprToEgison (Term a xs) = InductiveData "Term" [toEgison a, Collection (Sq.fromList (map symbolExprToEgison xs))]

symbolExprToEgison :: (SymbolExpr, Integer) -> EgisonValue
symbolExprToEgison (Symbol x js, n) = Tuple [InductiveData "Symbol" [toEgison (T.pack x), Collection (Sq.fromList (map (\j -> case j of
                                                                                                                                Superscript k -> InductiveData "Sup" [ScalarData k]
                                                                                                                                Subscript k -> InductiveData "Sub" [ScalarData k]
                                                                                                                     ) js))], toEgison n]
symbolExprToEgison (Apply fn mExprs, n) = Tuple [InductiveData "Apply" [fn, Collection (Sq.fromList (map mathExprToEgison mExprs))], toEgison n]

egisonToScalarData :: EgisonValue -> EgisonM ScalarData
egisonToScalarData (InductiveData "Div" [p1, p2]) = Div <$> egisonToPolyExpr p1 <*> egisonToPolyExpr p2
egisonToScalarData p1@(InductiveData "Plus" _) = Div <$> egisonToPolyExpr p1 <*> (return (Plus [(Term 1 [])]))
egisonToScalarData t1@(InductiveData "Term" _) = do
  t1' <- egisonToTermExpr t1
  return $ Div (Plus [t1']) (Plus [(Term 1 [])])
egisonToScalarData s1@(InductiveData "Symbol" _) = do
  s1' <- egisonToSymbolExpr (Tuple [s1, toEgison (1 ::Integer)])
  return $ Div (Plus [(Term 1 [s1'])]) (Plus [(Term 1 [])])
egisonToScalarData s1@(InductiveData "Apply" _) = do
  s1' <- egisonToSymbolExpr (Tuple [s1, toEgison (1 :: Integer)])
  return $ Div (Plus [(Term 1 [s1'])]) (Plus [(Term 1 [])])
egisonToScalarData val = liftError $ throwError $ TypeMismatch "math expression" (Value val)

egisonToPolyExpr :: EgisonValue -> EgisonM PolyExpr
egisonToPolyExpr (InductiveData "Plus" [Collection ts]) = Plus <$> mapM egisonToTermExpr (toList ts)
egisonToPolyExpr val = liftError $ throwError $ TypeMismatch "math poly expression" (Value val)

egisonToTermExpr :: EgisonValue -> EgisonM TermExpr
egisonToTermExpr (InductiveData "Term" [n, Collection ts]) = Term <$> fromEgison n <*> mapM egisonToSymbolExpr (toList ts)
egisonToTermExpr val = liftError $ throwError $ TypeMismatch "math term expression" (Value val)

egisonToSymbolExpr :: EgisonValue -> EgisonM (SymbolExpr, Integer)
egisonToSymbolExpr (Tuple [InductiveData "Symbol" [x, (Collection seq)], n]) = do
  x' <- fromEgison x
  let js = toList seq
  js' <- mapM (\j -> case j of
                         InductiveData "Sup" [ScalarData k] -> return (Superscript k)
                         InductiveData "Sub" [ScalarData k] -> return (Subscript k)
                         _ -> liftError $ throwError $ TypeMismatch "math symbol expression" (Value j)
               ) js
  n' <- fromEgison n
  return (Symbol (T.unpack x') js', n')
egisonToSymbolExpr (Tuple [InductiveData "Apply" [fn, (Collection mExprs)], n]) = do
  mExprs' <- mapM egisonToScalarData (toList mExprs)
  n' <- fromEgison n
  return (Apply fn mExprs', n')
egisonToSymbolExpr val = liftError $ throwError $ TypeMismatch "math symbol expression" (Value val)

mathNormalize' :: ScalarData -> ScalarData
mathNormalize' mExpr = mathDivide (mathRemoveZero (mathFold (mathRemoveZeroSymbol mExpr)))

mathDivide :: ScalarData -> ScalarData
mathDivide (Div (Plus ts1) (Plus [])) = (Div (Plus ts1) (Plus []))
mathDivide (Div (Plus []) (Plus ts2)) = (Div (Plus []) (Plus ts2))
mathDivide (Div (Plus ts1) (Plus [(Term a xs)])) =
  case f (Term a xs) ts1 of
    Nothing -> (Div (Plus ts1) (Plus [(Term a xs)]))
    Just ts1' -> (Div (Plus ts1') (Plus [(Term 1 [])]))
 where
  f :: TermExpr -> [TermExpr] -> Maybe [TermExpr]
  f _ [] = Just []
  f t1 (t:ts) = do t' <- f' t1 t
                   ts' <- f t1 ts
                   return (t':ts')
  f' :: TermExpr -> TermExpr -> Maybe TermExpr
  f' (Term a xs) (Term b ys) =
    if b `mod` a == 0
      then do ys' <- g xs ys
              return (Term (b `quot` a) ys')
      else Nothing
  g :: [(SymbolExpr, Integer)] -> [(SymbolExpr, Integer)] -> Maybe [(SymbolExpr, Integer)]
  g [] ys = Just ys
  g ((x,n):xs) ys = do
    ys' <- g' (x,n) ys
    g xs ys'
  g' :: (SymbolExpr, Integer) -> [(SymbolExpr, Integer)] -> Maybe [(SymbolExpr, Integer)]
  g' (x, n) [] = Nothing
  g' (x, n) ((y, m):ys) = do
    if (x == y && n <= m)
      then Just ((y, (m - n)):ys)
      else do ys' <- g' (x,n) ys
              return ((y,m):ys')
mathDivide (Div (Plus ts1) (Plus ts2)) = (Div (Plus ts1) (Plus ts2))

mathRemoveZeroSymbol :: ScalarData -> ScalarData
mathRemoveZeroSymbol (Div (Plus ts1) (Plus ts2)) =
  let p x = case x of
              (_, 0) -> False
              _ -> True in
  let ts1' = map (\(Term a xs) -> Term a (filter p xs)) ts1 in
  let ts2' = map (\(Term a xs) -> Term a (filter p xs)) ts2 in
    Div (Plus ts1') (Plus ts2')

mathRemoveZero :: ScalarData -> ScalarData
mathRemoveZero (Div (Plus ts1) (Plus ts2)) =
  let ts1' = filter (\(Term a _) -> a /= 0) ts1 in
  let ts2' = filter (\(Term a _) -> a /= 0) ts2 in
    case ts1' of
      [] -> Div (Plus []) (Plus [Term 1 []])
      _ -> Div (Plus ts1') (Plus ts2')

mathFold :: ScalarData -> ScalarData
mathFold mExpr = (mathTermFold (mathSymbolFold (mathTermFold mExpr)))

mathSymbolFold :: ScalarData -> ScalarData
mathSymbolFold (Div (Plus ts1) (Plus ts2)) = Div (Plus (map f ts1)) (Plus (map f ts2))
 where
  f :: TermExpr -> TermExpr
  f (Term a xs) = Term a (g [] xs)
  g :: [(SymbolExpr, Integer)] -> [(SymbolExpr, Integer)] -> [(SymbolExpr, Integer)]
  g ret [] = ret
  g ret ((x, n):xs) =
    if (any (p (x, n)) ret)
      then g (map (h (x, n)) ret) xs
      else g (ret ++ [(x, n)]) xs
  p :: (SymbolExpr, Integer) -> (SymbolExpr, Integer) -> Bool
  p (x, _) (y, _) = x == y
  h :: (SymbolExpr, Integer) -> (SymbolExpr, Integer) -> (SymbolExpr, Integer)
  h (x, n) (y, m) = if x == y
                     then (y, m + n)
                     else (y, m)

mathTermFold :: ScalarData -> ScalarData
mathTermFold (Div (Plus ts1) (Plus ts2)) = Div (Plus (f ts1)) (Plus (f ts2))
 where
  f :: [TermExpr] -> [TermExpr]
  f ts = f' [] ts
  f' :: [TermExpr] -> [TermExpr] -> [TermExpr]
  f' ret [] = ret
  f' ret ((Term a xs):ts) =
    if any (\(Term _ ys) -> (p xs ys)) ret
      then f' (map (g (Term a xs)) ret) ts
      else f' (ret ++ [(Term a xs)]) ts
  g :: TermExpr -> TermExpr -> TermExpr
  g (Term a xs) (Term b ys) = if p xs ys
                                then (Term (a + b) ys)
                                else Term b ys
  p :: [(SymbolExpr, Integer)] -> [(SymbolExpr, Integer)] -> Bool
  p [] [] = True
  p [] _ = False
  p ((x, n):xs) ys =
    let (b, ys') = q (x, n) [] ys in
      if b 
        then p xs ys'
        else False
  q :: (SymbolExpr, Integer) -> [(SymbolExpr, Integer)] -> [(SymbolExpr, Integer)] -> (Bool, [(SymbolExpr, Integer)])
  q _ _ [] = (False, [])
  q (x, n) ret ((y, m):ys) = if (x == y) && (n == m)
                               then (True, (ret ++ ys))
                               else q (x, n) (ret ++ [(y, m)]) ys

--
--  Arithmetic operations
--

mathPlus :: ScalarData -> ScalarData -> ScalarData
mathPlus (Div m1 n1) (Div m2 n2) = mathNormalize' $ Div (mathPlusPoly (mathMultPoly m1 n2) (mathMultPoly m2 n1)) (mathMultPoly n1 n2)

mathPlusPoly :: PolyExpr -> PolyExpr -> PolyExpr
mathPlusPoly (Plus ts1) (Plus ts2) = Plus (ts1 ++ ts2)

mathMult :: ScalarData -> ScalarData -> ScalarData
mathMult (Div m1 n1) (Div m2 n2) = mathNormalize' $ Div (mathMultPoly m1 m2) (mathMultPoly n1 n2)

mathMultPoly :: PolyExpr -> PolyExpr -> PolyExpr
mathMultPoly (Plus []) (Plus _) = Plus []
mathMultPoly (Plus _) (Plus []) = Plus []
mathMultPoly (Plus ts1) (Plus ts2) = foldl mathPlusPoly (Plus []) (map (\(Term a xs) -> (Plus (map (\(Term b ys) -> (Term (a * b) (xs ++ ys))) ts2))) ts1)

mathNegate :: ScalarData -> ScalarData
mathNegate (Div m n) = Div (mathNegate' m) n

mathNegate' :: PolyExpr -> PolyExpr
mathNegate' (Plus ts) = Plus (map (\(Term a xs) -> (Term (negate a) xs)) ts)

mathNumerator :: ScalarData -> ScalarData
mathNumerator (Div m _) = Div m (Plus [(Term 1 [])])

mathDenominator :: ScalarData -> ScalarData
mathDenominator (Div _ n) = Div n (Plus [(Term 1 [])])

--
-- Tensors
--

data TensorData =
    TData (Tensor ScalarData) (Maybe [Index ScalarData])
 deriving (Eq)

data Tensor a = Tensor [Integer] [a]
 deriving (Eq)

scalarToUnitTensor :: [Integer] -> ScalarData -> (Maybe [Index ScalarData]) -> TensorData
scalarToUnitTensor ns x js = makeTensor ns (map (\ms -> if all (\m -> m == (head ms)) (tail ms)
                                                         then x
                                                         else (Div (Plus []) (Plus [(Term 1 [])]))) (tensorIndices ns))
                                                js

scalarToTensor :: [Integer] -> ScalarData -> (Maybe [Index ScalarData]) -> TensorData
scalarToTensor ns x js = makeTensor ns (map (\ms -> x) (tensorIndices ns)) js

initTensor :: [Integer] -> [ScalarData] -> [ScalarData] -> [ScalarData] -> TensorData
initTensor ns xs sup sub = TData (Tensor ns xs) (Just ((map Superscript sup) ++ (map Subscript sub)))

makeTensor :: [Integer] -> [ScalarData] -> (Maybe [Index ScalarData]) -> TensorData
makeTensor ns xs js = TData (Tensor ns xs) js

tensorIndices :: [Integer] -> [[Integer]]
tensorIndices [] = [[]]
tensorIndices (n:ns) = concat (map (\i -> (map (\is -> i:is) (tensorIndices ns))) [1..n])

tMap :: (ScalarData -> EgisonM ScalarData) -> TensorData -> EgisonM TensorData
tMap f (TData (Tensor ns xs) js) = do
  xs' <- mapM f xs
  return $ TData (Tensor ns xs') js

tMap2 :: (ScalarData -> ScalarData -> EgisonM ScalarData) -> TensorData -> TensorData -> EgisonM TensorData
tMap2 f (TData t1@(Tensor ns1 xs1) (Just js1)) (TData t2@(Tensor ns2 xs2) (Just js2)) = do
  ns2' <- transIndex js1 js2 ns2
  if ns1 == ns2'
    then do ys <- mapM (\is -> do is' <- transIndex js1 js2 is
                                  f (tref' is t1) (tref' is' t2))
                       (tensorIndices ns1)
            return $ makeTensor ns1 ys (Just js1)
    else throwError $ InconsistentTensorSize
tMap2 f (TData t1@(Tensor ns1 xs1) Nothing) (TData t2@(Tensor ns2 xs2) Nothing) = do
  if ns1 == ns2
    then do ys <- mapM (\is -> f (tref' is t1) (tref' is t2))
                       (tensorIndices ns1)
            return $ makeTensor ns1 ys Nothing
    else throwError $ InconsistentTensorSize
tMap2 _ t1 t2 = do
  throwError $ InconsistentTensorIndex -- TODO : new error type

tSum :: [Tensor ScalarData] -> (Tensor ScalarData)
tSum (t:ts) = tSum' t ts
 where
  tSum' :: (Tensor ScalarData) -> [Tensor ScalarData] -> (Tensor ScalarData)
  tSum' (Tensor ns xs) [] = Tensor ns xs
  tSum' (Tensor ns xs) ((Tensor _ xs1):ts) =
    tSum' (Tensor ns (map (\(x,y) -> mathNormalize' (mathPlus x y)) (zip xs xs1))) ts

transIndex :: [Index ScalarData] -> [Index ScalarData] -> [Integer] -> EgisonM [Integer]
transIndex [] [] [] = return []
transIndex (j1:js1) js2 is = do
  let (hjs2, tjs2) = break (\j2 -> j1 == j2) js2
  if tjs2 == []
    then throwError $ InconsistentTensorIndex
    else do let n = (length hjs2) + 1
            rs <- transIndex js1 (hjs2 ++ (tail tjs2)) ((take (n - 1) is) ++ (drop n is))
            return ((is !! (n - 1)):rs)
transIndex _ _ _ = throwError $ InconsistentTensorSize

tContract :: TensorData -> EgisonM EgisonValue
tContract (TData t@(Tensor ns xs) (Just js)) = do
  case (findPairs js) of
    [] -> return $ TensorData (TData (Tensor ns xs) (Just js))
    ((hs,ms,ts):_) -> do
      let hn = (length hs) + 1
      let mn = (length (hs ++ ms)) + 2
      if (ns !! (hn - 1)) == (ns !! (mn - 1))
        then do
          let n = ns !! (hn - 1)
          let ret = TData (tSum (map (\i -> (tref (map extract hs ++ [(Div (Plus [(Term i [])]) (Plus [(Term 1 [])]))] ++ map extract ms
                                                      ++ [(Div (Plus [(Term i [])]) (Plus [(Term 1 [])]))] ++ map extract ts) t))
                                     [1..n]))
                          (Just (hs ++ ms ++ ts))
          case ret of
            (TData (Tensor [] [x]) (Just [])) -> return $ ScalarData x
            _ -> return $ TensorData ret
        else throwError $ InconsistentTensorIndex
 where
  findPairs :: [Index ScalarData] -> [([Index ScalarData], [Index ScalarData], [Index ScalarData])]
  findPairs xs = findPairs' [] xs
  findPairs' :: [Index ScalarData] -> [Index ScalarData] -> [([Index ScalarData], [Index ScalarData], [Index ScalarData])]
  findPairs' _ [] = []
  findPairs' hs (x:xs) = (findPairs'' hs x xs) ++ (findPairs' (hs ++ [x]) xs)
  findPairs'' :: [Index ScalarData] -> Index ScalarData -> [Index ScalarData] -> [([Index ScalarData], [Index ScalarData], [Index ScalarData])]
  findPairs'' hs x xs =
    let (hxs, txs) = break (\e -> p e x) xs in
    if txs == []
      then []
      else [(hs, hxs, (tail txs))]
  p :: Index ScalarData -> Index ScalarData -> Bool
  p (Superscript i) (Subscript j) = i == j
  p (Subscript i) (Superscript j) = i == j
  p _ _ = False
  extract :: Index ScalarData -> ScalarData
  extract (Superscript k) = k
  extract (Subscript k) = k
tContract (TData _ Nothing) = throwError $ InconsistentTensorIndex -- TODO : new error type

tCheckIndex :: [ScalarData] -> [Integer] -> EgisonM ()
tCheckIndex [] [] = return ()
tCheckIndex ((Div (Plus [(Term m [])]) (Plus [(Term 1 [])])):ms) (n:ns) =
  if (0 < m) && (m <= n)
    then tCheckIndex ms ns
    else throwError $ TensorIndexOutOfBounds m n
tCheckIndex (Div (Plus [(Term 1 [(Symbol _ _, 1)])]) (Plus [(Term 1 [])]):ms) (n:ns) = tCheckIndex ms ns
tCheckIndex (m:_) _ = throwError $ TypeMismatch "symbol or natural number" (Value (ScalarData m))

tref' :: [Integer] -> (Tensor a) -> a
tref' ms (Tensor ns xs) = tref'' ms ns xs
 where
  tref'' :: [Integer] -> [Integer] -> [a] -> a
  tref'' [m] [n] xs = xs !! (fromIntegral (m - 1))
  tref'' (m:ms) (n:ns) xs =
    let w = fromIntegral (product ns) in
    let ys = take w (drop (w * (fromIntegral (m - 1))) xs) in
      tref'' ms ns ys

tref :: [ScalarData] -> (Tensor a) -> (Tensor a)
tref ms (Tensor ns xs) = let rns = map snd (filter (\(m,_) -> (isSymbol (ScalarData m))) (zip ms ns)) in
                         let rxs = tsub' ms ns xs in
                           Tensor rns rxs
 where
  tsub' :: [ScalarData] -> [Integer] -> [a] -> [a]
  tsub' [] [] rs = rs
  tsub' (m:ms) (n:ns) xs =
    if isSymbol (ScalarData m)
      then let w = fromIntegral (product ns) in
           let yss = split w xs in
             concat (map (\ys -> tsub' ms ns ys) yss)
      else let i = extractInteger m in
           let w = fromIntegral (product ns) in
           let ys = take w (drop (w * (fromIntegral (i - 1))) xs) in
             tsub' ms ns ys
  split :: Int -> [a] -> [[a]]
  split _ [] = [[]]
  split w xs = let (hs, ts) = splitAt w xs in
                 hs:(split w ts)
  extractInteger :: ScalarData -> Integer
  extractInteger (Div (Plus []) (Plus [(Term 1 [])])) = 0
  extractInteger (Div (Plus [(Term i [])]) (Plus [(Term 1 [])])) = i

tSize :: TensorData -> [Integer]
tSize (TData (Tensor ns _) _) = ns

tToList :: (Tensor a) -> [a]
tToList (Tensor _ xs) = xs

tIndex :: TensorData -> Maybe [Index ScalarData]
tIndex (TData (Tensor _ _) js) = js

type Matcher = EgisonValue

type PrimitiveFunc = WHNFData -> EgisonM WHNFData

instance Show EgisonValue where
  show (Char c) = "c#" ++ [c]
  show (String str) = "\"" ++ T.unpack str ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (ScalarData mExpr) = show mExpr
  show (TensorData tExpr) = show tExpr
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
  show (Func Nothing _ names _) = "(lambda [" ++ unwords names ++ "] ...)"
  show (Func (Just name) _ _ _) = name
  show (CFunc Nothing _ name _) = "(cambda " ++ name ++ " ...)"
  show (CFunc (Just name) _ _ _) = name
  show (MemoizedFunc Nothing _ _ _ names _) = "(memoized-lambda [" ++ unwords names ++ "] ...)"
  show (MemoizedFunc (Just name) _ _ _ names _) = name
  show (Macro names _) = "(macro [" ++ unwords names ++ "] ...)"
  show (PatternFunc _ _ _) = "#<pattern-function>"
  show (PrimitiveFunc name _) = "#<primitive-function " ++ name ++ ">"
  show (IOFunc _) = "#<io-function>"
  show (Port _) = "#<port>"
  show Something = "something"
  show Undefined = "undefined"
  show World = "#<world>"
  show EOF = "#<eof>"

instance Show ScalarData where
  show (Div p1 (Plus [(Term 1 [])])) = show p1
  show (Div p1 p2) = "(/ " ++ show p1 ++ " " ++ show p2 ++ ")"

instance Show PolyExpr where
  show (Plus []) = "0"
  show (Plus [t]) = show t
  show (Plus ts) = "(+ " ++ unwords (map show ts)  ++ ")"

instance Show TermExpr where
  show (Term a []) = show a
  show (Term 1 [x]) = showPoweredSymbol x
  show (Term 1 xs) = "(* " ++ unwords (map showPoweredSymbol xs) ++ ")"
  show (Term a xs) = "(* " ++ show a ++ " " ++ unwords (map showPoweredSymbol xs) ++ ")"

showPoweredSymbol :: (SymbolExpr, Integer) -> String
showPoweredSymbol (x, 1) = show x
showPoweredSymbol (x, n) = show x ++ "^" ++ show n

instance Show SymbolExpr where
  show (Symbol s []) = s
  show (Symbol s js) = s ++ concat (map show js)
  show (Apply fn mExprs) = "(" ++ show fn ++ " " ++ unwords (map show mExprs) ++ ")"

instance (Show a) => Show (Index a) where
  show (Superscript i) = "~" ++ show i
  show (Subscript i) = "_" ++ show i

showComplex :: (Num a, Eq a, Ord a, Show a) => a -> a -> String
showComplex x 0 = show x
showComplex 0 y = show y ++ "i"
showComplex x y = show x ++ (if y > 0 then "+" else "") ++ show y ++ "i"

showComplexFloat :: Double -> Double -> String
showComplexFloat x 0.0 = showFFloat Nothing x ""
showComplexFloat 0.0 y = showFFloat Nothing y "i"
showComplexFloat x y = showFFloat Nothing x "" ++ if y > 0 then "+" else "" ++ showFFloat Nothing y "i"

instance Show TensorData where
  show (TData xs Nothing) = show xs
  show (TData xs (Just indices)) = show xs ++ concat (map show indices)

instance Show (Tensor ScalarData) where
  show (Tensor ns xs) =  "(| {" ++ unwords (map show ns) ++ "} {" ++ unwords (map show xs) ++ "} |)"


showTSV :: EgisonValue -> String
showTSV (Tuple (val:vals)) = foldl (\r x -> r ++ "\t" ++ x) (show val) (map showTSV vals)
showTSV (Collection vals) = intercalate "\t" (map showTSV (toList vals))
showTSV val = show val

instance Eq EgisonValue where
 (Char c) == (Char c') = c == c'
 (String str) == (String str') = str == str'
 (Bool b) == (Bool b') = b == b'
 (ScalarData x) == (ScalarData y) = (x == y)
 (TensorData x) == (TensorData y) = (x == y)
 (Float x y) == (Float x' y') = (x == x') && (y == y')
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
 (Macro xs1 expr1) == (Macro xs2 expr2) = (xs1 == xs2) && (expr1 == expr2)
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
  toEgison 0 = ScalarData $ mathNormalize' (Div (Plus []) (Plus [(Term 1 [])]))
  toEgison i = ScalarData $ mathNormalize' (Div (Plus [(Term i [])]) (Plus [(Term 1 [])]))
  fromEgison = liftError . fromIntegerValue

instance EgisonData Rational where
  toEgison r = ScalarData $ mathNormalize' (Div (Plus [(Term (numerator r) [])]) (Plus [(Term (denominator r) [])]))
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
fromIntegerValue (ScalarData (Div (Plus []) (Plus [(Term 1 [])]))) = return 0
fromIntegerValue (ScalarData (Div (Plus [(Term x [])]) (Plus [(Term 1 [])]))) = return x
fromIntegerValue val = throwError $ TypeMismatch "integer" (Value val)

fromRationalValue :: EgisonValue -> Either EgisonError Rational
fromRationalValue (ScalarData (Div (Plus []) _)) = return 0
fromRationalValue (ScalarData (Div (Plus [(Term x [])]) (Plus [(Term y [])]))) = return (x % y)
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
fromIntegerWHNF (Value (ScalarData (Div (Plus []) (Plus [(Term 1 [])])))) = return 0
fromIntegerWHNF (Value (ScalarData (Div (Plus [(Term x [])]) (Plus [(Term 1 [])])))) = return x
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

data Env = Env [HashMap Var ObjectRef]
 deriving (Show)

type Var = String
type Binding = (Var, ObjectRef)

nullEnv :: Env
nullEnv = Env []

extendEnv :: Env -> [Binding] -> Env
extendEnv (Env env) = Env . (: env) . HashMap.fromList

refVar :: Env -> Var -> Maybe ObjectRef
refVar (Env env) var = msum $ map (HashMap.lookup var) env

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
  | InconsistentTensorSize
  | InconsistentTensorIndex
  | TensorIndexOutOfBounds Integer Integer
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
  show InconsistentTensorSize = "Inconsistent tensor size"
  show InconsistentTensorIndex = "Inconsistent tensor index"
  show (TensorIndexOutOfBounds m n) = "Tensor index out of bounds: " ++ show m ++ ", " ++ show n
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

-- Typing

isBool :: EgisonValue -> Bool
isBool (Bool _) = True
isBool _ = False

isBool' :: PrimitiveFunc
isBool' (Value val) = return $ Value $ Bool $ isBool val

isInteger :: EgisonValue -> Bool
isInteger (ScalarData (Div (Plus []) (Plus [(Term 1 [])]))) = True
isInteger (ScalarData (Div (Plus [(Term _ [])]) (Plus [(Term 1 [])]))) = True
isInteger _ = False

isInteger' :: PrimitiveFunc
isInteger' (Value val) = return $ Value $ Bool $ isInteger val

isRational :: EgisonValue -> Bool
isRational (ScalarData (Div (Plus []) (Plus [(Term _ [])]))) = True
isRational (ScalarData (Div (Plus [(Term _ [])]) (Plus [(Term _ [])]))) = True
isRational _ = False

isRational' :: PrimitiveFunc
isRational' (Value val) = return $ Value $ Bool $ isRational val

isSymbol :: EgisonValue -> Bool
isSymbol (ScalarData (Div (Plus [(Term 1 [(Symbol _ _, 1)])]) (Plus [(Term 1 [])]))) = True
isSymbol _ = False

isScalar :: EgisonValue -> Bool
isScalar (ScalarData _) = True
isScalar _ = False

isScalar' :: PrimitiveFunc
isScalar' (Value val) = return $ Value $ Bool $ isScalar val
isScalar' _ = return $ Value $ Bool False

isTensor :: EgisonValue -> Bool
isTensor (TensorData _) = True
isTensor _ = False

isTensor' :: PrimitiveFunc
isTensor' (Value val) = return $ Value $ Bool $ isTensor val
isTensor' _ = return $ Value $ Bool False

isTensorWithIndex :: EgisonValue -> Bool
isTensorWithIndex (TensorData (TData (Tensor _ _) (Just ms))) = True
isTensorWithIndex _ = False

isTensorWithIndex' :: PrimitiveFunc
isTensorWithIndex' (Value val) = return $ Value $ Bool $ isTensorWithIndex val
isTensorWithIndex' _ = return $ Value $ Bool False

isFloat' :: PrimitiveFunc
isFloat' (Value (Float _ 0)) = return $ Value $ Bool True
isFloat' _ = return $ Value $ Bool False

isComplex' :: PrimitiveFunc
isComplex' (Value (Float _ _)) = return $ Value $ Bool True
isComplex' _ = return $ Value $ Bool False

isChar' :: PrimitiveFunc
isChar' (Value (Char _)) = return $ Value $ Bool True
isChar' _ = return $ Value $ Bool False

isString' :: PrimitiveFunc
isString' (Value (String _)) = return $ Value $ Bool True
isString' _ = return $ Value $ Bool False

isCollection' :: PrimitiveFunc
isCollection' (Value (Collection _)) = return $ Value $ Bool True
isCollection' (Intermediate (ICollection _)) = return $ Value $ Bool True
isCollection' _ = return $ Value $ Bool False

isArray' :: PrimitiveFunc
isArray' (Value (Array _)) = return $ Value $ Bool True
isArray' (Intermediate (IArray _)) = return $ Value $ Bool True
isArray' _ = return $ Value $ Bool False

isHash' :: PrimitiveFunc
isHash' (Value (IntHash _)) = return $ Value $ Bool True
isHash' (Value (StrHash _)) = return $ Value $ Bool True
isHash' (Intermediate (IIntHash _)) = return $ Value $ Bool True
isHash' (Intermediate (IStrHash _)) = return $ Value $ Bool True
isHash' _ = return $ Value $ Bool False
