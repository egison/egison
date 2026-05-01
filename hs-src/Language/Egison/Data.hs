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
    -- * Symbol helpers
    , getSymId
    -- * CAS types and helpers
    , CASValue(..)
    , CASTerm(..)
    , extractCASValue
    , symbolCASData
    , quoteCASData
    , quoteFunctionCASData
    , functionCASData
    , applyCASData
    -- * Internal data
    , Object (..)
    , ObjectRef
    , WHNFData (..)
    , Inner (..)
    , prettyFunctionName
    -- * Environment
    , Env (..)
    , EnvLayer
    , PatFuncEnv
    , Binding
    , nullEnv
    , extendEnv
    , extendPatFuncEnv
    , refVar
    , refPatFunc
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
import           Data.Text                        (Text, unpack)
import           Text.Show.Unicode                (ushow)

import           Data.Ratio
import           System.IO

import           Language.Egison.CmdOptions
import           Language.Egison.EvalState
import           Language.Egison.IExpr
import           Language.Egison.Math
import qualified Language.Egison.Math.CAS as CAS
import           Language.Egison.RState

--
-- Values
--

data EgisonValue
  = World
  | Char Char
  | String Text
  | Bool Bool
  | CASData CASValue  -- Computer algebra system type (CAS)
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
  -- CAS internal type for index pattern matching (subscript, superscript, user)
  | CASIndexData (Index CASValue)

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
-- CAS Value Helpers
--

-- | Extract symbol ID from a CAS symbol value
getSymId :: EgisonValue -> String
getSymId val = case val of
  CASData (CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId _ _, _)]]) -> symId
  _ -> error "getSymId: not a symbol"

getSymName :: EgisonValue -> String
getSymName val = case val of
  CASData (CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol _ name [], 1)]]) -> name
  _ -> error "getSymName: not a symbol"

-- | Create a symbol CASValue
symbolCASData :: String -> String -> EgisonValue
symbolCASData symId name = CASData $ CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name [], 1)]]

-- | Create a Quote CASValue
quoteCASData :: CASValue -> EgisonValue
quoteCASData cv = CASData $ CASPoly [CASTerm (CASInteger 1) [(CAS.Quote cv, 1)]]

-- | Create a QuoteFunction CASValue
quoteFunctionCASData :: WHNFData -> EgisonValue
quoteFunctionCASData whnf = CASData $ CASPoly [CASTerm (CASInteger 1) [(CAS.QuoteFunction whnf, 1)]]

-- | Create a FunctionData CASValue
functionCASData :: CASValue -> [CASValue] -> EgisonValue
functionCASData sym args = CASData $ CASPoly [CASTerm (CASInteger 1) [(CAS.FunctionData sym args, 1)]]

-- | Create an Apply CASValue (Apply1-4 based on argument count)
applyCASData :: CASValue -> [CASValue] -> EgisonValue
applyCASData fn args = CASData $ CASPoly [CASTerm (CASInteger 1) [(CAS.makeApplyExpr fn args, 1)]]

-- | Extract CASValue from EgisonValue
extractCASValue :: EgisonValue -> EvalM CASValue
extractCASValue (CASData cv) = return cv
extractCASValue val          = throwErrorWithTrace (TypeMismatch "math expression" (Value val))

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
  show (CASData cv) = prettyCAS cv
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
  -- CAS internal type for index pattern matching
  show (CASIndexData idx) = show idx

-- False if we have to put parenthesis around it to make it an atomic expression.
isAtomic :: EgisonValue -> Bool
isAtomic (InductiveData _ []) = True
isAtomic (InductiveData _ _)  = False
isAtomic (CASData cv)         = casIsAtom cv
-- CAS internal type for index pattern matching
isAtomic (CASIndexData _)     = False
isAtomic _                    = True

instance Eq EgisonValue where
  (Char c) == (Char c')                                            = c == c'
  (String str) == (String str')                                    = str == str'
  (Bool b) == (Bool b')                                            = b == b'
  (CASData x) == (CASData y)                                       = CAS.casNormalize x == CAS.casNormalize y  -- Normalize before comparing
  (TensorData (Tensor js xs _)) == (TensorData (Tensor js' xs' _)) = js == js' && xs == xs'
  (Float x) == (Float x')                                          = x == x'
  (InductiveData name vals) == (InductiveData name' vals')         = name == name' && vals == vals'
  (Tuple vals) == (Tuple vals')                                    = vals == vals'
  (Collection vals) == (Collection vals')                          = vals == vals'
  (IntHash vals) == (IntHash vals')                                = vals == vals'
  (CharHash vals) == (CharHash vals')                              = vals == vals'
  (StrHash vals) == (StrHash vals')                                = vals == vals'
  -- CAS internal types
  (CASIndexData i) == (CASIndexData i')                            = i == i'
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
  toEgison 0 = CASData (CASInteger 0)
  toEgison i = CASData (CASInteger i)
  fromEgison val = case val of
    CASData cv -> case extractCASInteger cv of
      Just n  -> return n
      Nothing -> throwErrorWithTrace (TypeMismatch "integer" (Value val))
    _ -> throwErrorWithTrace (TypeMismatch "integer" (Value val))

-- | Extract an Integer from a CASValue, handling divisions that simplify to integers
extractCASInteger :: CASValue -> Maybe Integer
extractCASInteger cv = case cv of
  CASInteger n -> Just n
  CASPoly [] -> Just 0
  CASPoly [CASTerm coef []] -> extractCASInteger coef
  CASFrac num den -> do
    n <- extractCASInteger num
    d <- extractCASInteger den
    if d /= 0 && n `mod` d == 0 then Just (n `div` d) else Nothing
  _ -> Nothing

instance EgisonData Rational where
  toEgison r = CASData $ CAS.casNormalize (CAS.CASFrac (CAS.CASInteger (numerator r)) (CAS.CASInteger (denominator r)))
  fromEgison val = case val of
    CASData (CASInteger 0)                  -> return 0
    CASData (CASPoly [])                    -> return 0
    CASData (CASInteger x)                  -> return (x % 1)
    CASData (CASPoly [CASTerm (CASInteger x) []]) -> return (x % 1)
    CASData (CASFrac (CASPoly [CASTerm (CASInteger x) []]) (CASPoly [CASTerm (CASInteger y) []])) -> return (x % y)
    _                                       -> throwErrorWithTrace (TypeMismatch "rational" (Value val))

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

-- | Pattern function environment: maps pattern function names to their ObjectRefs.
-- Kept separate from the regular value environment so that pattern typing is
-- independent of which matcher is in scope (matcher polymorphism).
type PatFuncEnv = Map String ObjectRef

-- | Environment: list of layers (for scoping) plus optional index context,
-- plus a separate store for pattern functions.
data Env = Env [EnvLayer] (Maybe (String, [Index (Maybe CASValue)])) PatFuncEnv

type Binding = (Var, ObjectRef)

instance {-# OVERLAPPING #-} Show (Index EgisonValue) where
  show (Sup i) = case i of
    CASData (CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol _ _ (_:_), 1)]]) -> "~[" ++ show i ++ "]"
    _ -> "~" ++ show i
  show (Sub i) = case i of
    CASData (CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol _ _ (_:_), 1)]]) -> "_[" ++ show i ++ "]"
    _ -> "_" ++ show i
  show (SupSub i) = "~_" ++ show i
  show (User i) = case i of
    CASData (CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol _ _ (_:_), 1)]]) -> "_[" ++ show i ++ "]"
    _ -> "|" ++ show i
  show (DF i j) = "_df-" ++ show i ++ "-" ++ show j

nullEnv :: Env
nullEnv = Env [] Nothing Map.empty

-- | Extend environment with new bindings
-- Groups bindings by base name and sorts by index length (shortest first)
extendEnv :: Env -> [Binding] -> Env
extendEnv (Env layers idx pfEnv) bindings = Env (newLayer : layers) idx pfEnv
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

-- | Extend the pattern function environment with new name→ref bindings.
-- The regular value layers and index context are preserved unchanged.
extendPatFuncEnv :: Env -> [(String, ObjectRef)] -> Env
extendPatFuncEnv (Env layers idx pfEnv) newBindings =
  Env layers idx (foldr (\(name, ref) acc -> Map.insert name ref acc) pfEnv newBindings)

-- | Look up a variable in the environment
-- Search algorithm:
--   1. Try exact match
--   2. Try prefix match (find longer indices and auto-complete with #)
--   3. Try suffix removal (find shorter indices, pick longest match)
-- No recursion is used; all matching is done in a single pass to avoid infinite loops.
refVar :: Env -> Var -> Maybe ObjectRef
refVar (Env layers _ _) (Var name targetIndices) =
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

-- | Look up a pattern function by name in the pattern function environment.
refPatFunc :: Env -> String -> Maybe ObjectRef
refPatFunc (Env _ _ pfEnv) name = Map.lookup name pfEnv

-- | Convert environment to list of bindings
-- Used for completion and debugging
envToBindingList :: Env -> [Binding]
envToBindingList (Env layers _ _) =
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
