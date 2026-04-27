{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Language.Egison.Primitives
Licence     : MIT

This module provides primitive functions in Egison.
-}

module Language.Egison.Primitives
  ( primitiveEnv
  , primitiveEnvNoIO
  ) where

import           Control.Monad                     (forM)
import           Control.Monad.IO.Class            (liftIO)

import           Data.IORef
import           Data.List                         (lookup)
import           Data.Foldable                     (toList)

import qualified Data.Sequence                     as Sq
import qualified Data.Text                         as T
import qualified Data.Vector                       as V

 {--  -- for 'egison-sqlite'
import qualified Database.SQLite3 as SQLite
 --}  -- for 'egison-sqlite'

import           Language.Egison.Data
import           Language.Egison.Data.Collection   (makeICollection)
import           Language.Egison.EvalState         (getReductionRulesCount, getDerivativeRulesCount)
import           Language.Egison.IExpr             (Index (..), stringToVar)
import           Language.Egison.Math
import qualified Language.Egison.Math.CAS as CAS
import           Language.Egison.Primitives.Arith
import           Language.Egison.Primitives.IO
import           Language.Egison.Primitives.String
import           Language.Egison.Primitives.Types
import           Language.Egison.Primitives.Utils

primitiveEnv :: IO Env
primitiveEnv = do
  bindings <- forM (constants ++ primitives ++ ioPrimitives) $ \(name, op) -> do
    ref <- newIORef . WHNF $ Value op
    return (stringToVar name, ref)
  return $ extendEnv nullEnv bindings

primitiveEnvNoIO :: IO Env
primitiveEnvNoIO = do
  bindings <- forM (constants ++ primitives) $ \(name, op) -> do
    ref <- newIORef . WHNF $ Value op
    return (stringToVar name, ref)
  return $ extendEnv nullEnv bindings

--
-- Constants
--

constants :: [(String, EgisonValue)]
constants = [ ("f.pi", Float 3.141592653589793)
            , ("f.e" , Float 2.718281828459045)
            ]

--
-- Primitives
--

primitives :: [(String, EgisonValue)]
primitives =
  map (\(name, fn) -> (name, PrimitiveFunc (fn name))) strictPrimitives
  ++ map (\(name, fn) -> (name, LazyPrimitiveFunc (fn name))) lazyPrimitives
  ++ primitiveArithFunctions
  ++ primitiveStringFunctions
  ++ primitiveTypeFunctions
    where
      strictPrimitives =
        [ ("addSubscript", addSubscript)
        , ("addSuperscript", addSuperscript)

        , ("assert",      assert)
        , ("assertEqual", assertEqual)

        , ("sortWithSign", sortWithSign)
        , ("updateFunctionArgs", updateFunctionArgs)
        , ("casTerms", casTermsPrim)
        , ("casFromTerms", casFromTermsPrim)
        , ("termCoeff", termCoeffPrim)
        , ("termMonomial", termMonomialPrim)
        , ("typeOf", typeOfPrim)
        , ("inspect", inspectPrim)
        , ("differentialClosed", differentialClosedPrim)
        , ("isInPolyAtoms", isInPolyAtomsPrim)
        , ("isPureInteger", isPureIntegerPrim)
        , ("isPureFraction", isPureFractionPrim)
        , ("numReductionRules", numReductionRulesPrim)
        , ("numDerivativeRules", numDerivativeRulesPrim)
        ]
      lazyPrimitives =
        [ ("tensorShape", tensorShape')
        , ("tensorToList", tensorToList')
        , ("dfOrder", dfOrder')
        ]

--
-- Miscellaneous primitive functions
--

tensorShape' :: String -> LazyPrimitiveFunc
tensorShape' = lazyOneArg tensorShape''
 where
  tensorShape'' (Value (TensorData (Tensor ns _ _))) =
    return . Value . Collection . Sq.fromList $ map toEgison ns
  tensorShape'' (ITensor (Tensor ns _ _)) =
    return . Value . Collection . Sq.fromList $ map toEgison ns
  tensorShape'' _ = return . Value . Collection $ Sq.fromList []

tensorToList' :: String -> LazyPrimitiveFunc
tensorToList' = lazyOneArg tensorToList''
 where
  tensorToList'' (Value (TensorData (Tensor _ xs _))) =
    return . Value . Collection . Sq.fromList $ V.toList xs
  tensorToList'' (ITensor (Tensor _ xs _)) = do
    inners <- liftIO . newIORef $ Sq.fromList (map IElement (V.toList xs))
    return (ICollection inners)
  tensorToList'' x = makeICollection [x]

dfOrder' :: String -> LazyPrimitiveFunc
dfOrder' = lazyOneArg dfOrder''
 where
  dfOrder'' (Value (TensorData (Tensor ns _ is))) =
    return $ Value (toEgison (fromIntegral (length ns - length is) :: Integer))
  dfOrder'' (ITensor (Tensor ns _ is)) =
    return $ Value (toEgison (fromIntegral (length ns - length is) :: Integer))
  dfOrder'' _ = return $ Value (toEgison (0 :: Integer))

addSubscript :: String -> PrimitiveFunc
addSubscript = twoArgs $ \fn sub ->
  case (fn, sub) of
    (CASData (CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name is, 1)]]),
     CASData s@(CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol _ _ [], 1)]])) ->
      return $ CASData $ CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name (is ++ [Sub s]), 1)]]
    (CASData (CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name is, 1)]]),
     CASData s@(CASPoly [CASTerm (CASInteger _) []])) ->
      return $ CASData $ CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name (is ++ [Sub s]), 1)]]
    (CASData (CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name is, 1)]]),
     CASData s@(CASInteger _)) ->
      let s' = CASPoly [CASTerm s []]
      in return $ CASData $ CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name (is ++ [Sub s']), 1)]]
    _ -> throwErrorWithTrace (TypeMismatch "symbol or integer" (Value fn))

addSuperscript :: String -> PrimitiveFunc
addSuperscript = twoArgs $ \fn sub ->
  case (fn, sub) of
    (CASData (CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name is, 1)]]),
     CASData s@(CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol _ _ [], 1)]])) ->
      return $ CASData $ CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name (is ++ [Sup s]), 1)]]
    (CASData (CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name is, 1)]]),
     CASData s@(CASPoly [CASTerm (CASInteger _) []])) ->
      return $ CASData $ CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name (is ++ [Sup s]), 1)]]
    (CASData (CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name is, 1)]]),
     CASData s@(CASInteger _)) ->
      let s' = CASPoly [CASTerm s []]
      in return $ CASData $ CASPoly [CASTerm (CASInteger 1) [(CAS.Symbol symId name (is ++ [Sup s']), 1)]]
    _ -> throwErrorWithTrace (TypeMismatch "symbol" (Value fn))

updateFunctionArgs :: String -> PrimitiveFunc
updateFunctionArgs = twoArgs' $ \funcVal newArgsColl ->
  case (funcVal, newArgsColl) of
    (CASData (CASPoly [CASTerm (CASInteger 1) [(CAS.FunctionData name _, 1)]]), Collection argsSeq) -> do
      args' <- mapM extractCAS (toList argsSeq)
      return $ CASData $ CASPoly [CASTerm (CASInteger 1) [(CAS.FunctionData name args', 1)]]
    _ -> throwErrorWithTrace (TypeMismatch "function value and collection of scalars" (Value funcVal))
 where
  extractCAS (CASData cv) = return cv
  extractCAS val = throwErrorWithTrace (TypeMismatch "scalar" (Value val))

-- | Convert a CASValue into a list of single-term polynomials.
-- A CASPoly is decomposed into its terms; an integer/factor becomes a singleton list;
-- zero becomes the empty list. Used by the parametric `poly` matcher to expose
-- term-level decomposition to Egison code.
casTermsPrim :: String -> PrimitiveFunc
casTermsPrim = oneArg' $ \v -> case v of
  CASData cv -> return . Collection . Sq.fromList . map CASData $ casValueToTerms cv
  _ -> throwErrorWithTrace (TypeMismatch "CAS value" (Value v))

casValueToTerms :: CAS.CASValue -> [CAS.CASValue]
casValueToTerms (CAS.CASInteger 0) = []
casValueToTerms v@(CAS.CASInteger _) = [CAS.CASPoly [CASTerm v []]]
casValueToTerms (CAS.CASFactor sym) = [CAS.CASPoly [CASTerm (CAS.CASInteger 1) [(sym, 1)]]]
casValueToTerms (CAS.CASPoly ts) = map (\t -> CAS.CASPoly [t]) ts
casValueToTerms v@(CAS.CASFrac _ _) = [v]

-- | Build a CASValue from a collection of single-term polynomials.
-- Each element should be a single-term polynomial (as produced by casTerms);
-- the result is normalized by re-running through casPlus.
casFromTermsPrim :: String -> PrimitiveFunc
casFromTermsPrim = oneArg' $ \v -> case v of
  Collection seq_ -> do
    cvs <- mapM extractCAS (toList seq_)
    return . CASData $ foldr CAS.casPlus (CAS.CASInteger 0) cvs
  _ -> throwErrorWithTrace (TypeMismatch "collection of CAS values" (Value v))
 where
  extractCAS (CASData cv) = return cv
  extractCAS val = throwErrorWithTrace (TypeMismatch "CAS value" (Value val))

-- | Extract the coefficient of a single-term CASValue.
-- For a single-term polynomial CASPoly [CASTerm c _], returns c.
-- For a bare integer, returns it as-is. For a bare factor, returns 1.
termCoeffPrim :: String -> PrimitiveFunc
termCoeffPrim = oneArg' $ \v -> case v of
  CASData (CAS.CASPoly [CASTerm c _]) -> return $ CASData c
  CASData (CAS.CASPoly []) -> return $ CASData (CAS.CASInteger 0)
  CASData v0@(CAS.CASInteger _) -> return $ CASData v0
  CASData (CAS.CASFactor _) -> return $ CASData (CAS.CASInteger 1)
  CASData v0@(CAS.CASFrac _ _) -> return $ CASData v0
  _ -> throwErrorWithTrace (TypeMismatch "single-term CAS value" (Value v))

-- | Phase 7.4/7.5: report the number of `declare rule` declarations seen by
-- the env-builder. The rule data itself is held in EnvBuildResult; here we
-- only expose the count so users can confirm registration worked.
numReductionRulesPrim :: String -> PrimitiveFunc
numReductionRulesPrim _ args = case args of
  [] -> do
    n <- getReductionRulesCount
    return $ toEgison (fromIntegral n :: Integer)
  [Tuple []] -> do
    n <- getReductionRulesCount
    return $ toEgison (fromIntegral n :: Integer)
  _  -> throwErrorWithTrace (TypeMismatch "no arguments" (Value (head args)))

-- | Phase 6.3: report the number of `declare derivative` declarations seen.
numDerivativeRulesPrim :: String -> PrimitiveFunc
numDerivativeRulesPrim _ args = case args of
  [] -> do
    n <- getDerivativeRulesCount
    return $ toEgison (fromIntegral n :: Integer)
  [Tuple []] -> do
    n <- getDerivativeRulesCount
    return $ toEgison (fromIntegral n :: Integer)
  _  -> throwErrorWithTrace (TypeMismatch "no arguments" (Value (head args)))

-- | Phase 5.5: runtime check that all atoms in a CAS value belong to the
-- given allowed-atom-name list. Used by user-level coerce-style helpers.
-- Atom names are pretty-printed (e.g. "x", "sqrt 2"), matched as strings.
isInPolyAtomsPrim :: String -> PrimitiveFunc
isInPolyAtomsPrim = twoArgs $ \v allowedC ->
  case (v, allowedC) of
    (CASData cv, Collection allowedSeq) -> do
      allowedNames <- mapM extractName (toList allowedSeq)
      let valueAtoms = CAS.casAtomSet cv
      return $ Bool (all (`elem` allowedNames) valueAtoms)
    _ -> throwErrorWithTrace (TypeMismatch "CAS value and string list" (Value v))
 where
  extractName (String s) = return (T.unpack s)
  extractName _          = throwErrorWithTrace (TypeMismatch "string atom name" (Value v))
  v = CASData (CAS.CASInteger 0)  -- unused placeholder for the error path

-- | Phase 5.5: check if a CASValue is a pure integer (no atoms / fractions).
isPureIntegerPrim :: String -> PrimitiveFunc
isPureIntegerPrim = oneArg' $ \v -> case v of
  CASData (CAS.CASInteger _)  -> return $ Bool True
  CASData (CAS.CASPoly [])    -> return $ Bool True   -- canonical zero
  CASData (CAS.CASPoly [CAS.CASTerm (CAS.CASInteger _) []]) -> return $ Bool True
  CASData _                   -> return $ Bool False
  _                           -> return $ Bool False

-- | Phase 5.5: check if a CASValue is a pure rational (Frac of integers).
isPureFractionPrim :: String -> PrimitiveFunc
isPureFractionPrim = oneArg' $ \v -> case v of
  CASData (CAS.CASInteger _) -> return $ Bool True
  CASData (CAS.CASFrac (CAS.CASInteger _) (CAS.CASInteger _)) -> return $ Bool True
  CASData (CAS.CASPoly []) -> return $ Bool True
  CASData (CAS.CASPoly [CAS.CASTerm (CAS.CASInteger _) []]) -> return $ Bool True
  _ -> return $ Bool False

-- | Phase 8: differential closure check.
-- Returns True iff the output value's atom set is a subset of the input's,
-- i.e. ∂/∂ did not introduce any new atoms. Used to label results that
-- stayed in the same Poly Integer [atoms] sub-ring.
differentialClosedPrim :: String -> PrimitiveFunc
differentialClosedPrim = twoArgs $ \input output ->
  case (input, output) of
    (CASData iv, CASData ov) ->
      return $ Bool (CAS.casDifferentialClosed iv ov)
    _ -> throwErrorWithTrace (TypeMismatch "two CAS values" (Value input))

-- | Phase 8: print the value alongside its observed type for REPL inspection.
-- Returns a string of the form "value : observed-type".
inspectPrim :: String -> PrimitiveFunc
inspectPrim = oneArg' $ \v -> case v of
  CASData cv ->
    return $ String (T.pack (CAS.prettyCAS cv ++ " : " ++ CAS.prettyTypeOf cv))
  Bool b ->
    return $ String (T.pack (show b ++ " : Bool"))
  Char c ->
    return $ String (T.pack (show c ++ " : Char"))
  String s ->
    return $ String (T.pack (show s ++ " : String"))
  Float f ->
    return $ String (T.pack (show f ++ " : Float"))
  Tuple [] ->
    return $ String (T.pack "() : ()")
  Tuple xs -> do
    descs <- mapM describeOne xs
    let valStr = "(" ++ intercalateComma (map fst descs) ++ ")"
        tyStr  = "(" ++ intercalateComma (map snd descs) ++ ")"
    return $ String (T.pack (valStr ++ " : " ++ tyStr))
  _ -> return $ String (T.pack "<value> : Any")
 where
  describeOne (CASData cv) = return (CAS.prettyCAS cv, CAS.prettyTypeOf cv)
  describeOne (Bool b)     = return (show b, "Bool")
  describeOne (Char c)     = return (show c, "Char")
  describeOne (String s)   = return (show s, "String")
  describeOne (Float f)    = return (show f, "Float")
  describeOne _            = return ("<value>", "Any")
  intercalateComma []     = ""
  intercalateComma [s]    = s
  intercalateComma (s:ss) = s ++ ", " ++ intercalateComma ss

-- | Phase 8 observed type: report the most specific runtime type of a value
-- as a string.
typeOfPrim :: String -> PrimitiveFunc
typeOfPrim = oneArg' $ \v -> case v of
  CASData cv -> return $ String (T.pack (CAS.prettyTypeOf cv))
  Tuple [] -> return $ String (T.pack "()")
  Tuple xs -> return $ String (T.pack ("(" ++ intercalateComma (map describeValue xs) ++ ")"))
  Collection _ -> return $ String (T.pack "Collection")
  TensorData _ -> return $ String (T.pack "Tensor")
  Bool _ -> return $ String (T.pack "Bool")
  Char _ -> return $ String (T.pack "Char")
  String _ -> return $ String (T.pack "String")
  Float _ -> return $ String (T.pack "Float")
  _ -> return $ String (T.pack "Any")
 where
  describeValue (CASData cv) = CAS.prettyTypeOf cv
  describeValue (Bool _)     = "Bool"
  describeValue (Char _)     = "Char"
  describeValue (String _)   = "String"
  describeValue (Float _)    = "Float"
  describeValue _            = "Any"
  intercalateComma []     = ""
  intercalateComma [s]    = s
  intercalateComma (s:ss) = s ++ ", " ++ intercalateComma ss

-- | Extract the monomial of a single-term CASValue as a flat list of (factor, exponent) pairs.
-- For CASPoly [CASTerm _ mono], returns mono as a Collection of Tuple [factor, integer].
termMonomialPrim :: String -> PrimitiveFunc
termMonomialPrim = oneArg' $ \v -> case v of
  CASData (CAS.CASPoly [CASTerm _ mono]) -> return $ monoToCollection mono
  CASData (CAS.CASPoly []) -> return $ Collection Sq.empty
  CASData (CAS.CASInteger _) -> return $ Collection Sq.empty
  CASData (CAS.CASFactor sym) -> return $ monoToCollection [(sym, 1)]
  CASData (CAS.CASFrac _ _) -> return $ Collection Sq.empty
  _ -> throwErrorWithTrace (TypeMismatch "single-term CAS value" (Value v))
 where
  monoToCollection :: CAS.Monomial -> EgisonValue
  monoToCollection mono =
    Collection . Sq.fromList $
      map (\(sym, e) ->
            Tuple [ CASData (CAS.CASFactor sym)
                  , CASData (CAS.CASInteger e)
                  ]) mono

assert ::  String -> PrimitiveFunc
assert = twoArgs' $ \label test -> do
  test <- fromEgison test
  if test
    then return $ Bool True
    else throwErrorWithTrace (Assertion (show label))

assertEqual :: String -> PrimitiveFunc
assertEqual = threeArgs' $ \label actual expected ->
  if actual == expected
     then return actual
     else throwErrorWithTrace (Assertion
            (show label ++ "\n expected: " ++ show expected ++ "\n but found: " ++ show actual))

-- | Sort a list of lists of integers and return the sign of the permutation
-- Each sublist is treated as a unit and sorted lexicographically
-- Used for antisymmetric tensor indices
sortWithSign :: String -> PrimitiveFunc
sortWithSign = oneArg' $ \val -> do
  case val of
    Collection xss -> do
      -- Extract list of lists
      let xss' = toList xss
      xs <- mapM extractIntList xss'
      -- Sort lists lexicographically and calculate permutation sign
      let (sign, sortedLists) = sortWithPermSign xs
      let flatList = concat sortedLists
      return $ Tuple [toEgison sign, Collection (Sq.fromList (map toEgison flatList))]
    _ -> throwErrorWithTrace (TypeMismatch "collection of collections" (Value val))
 where
  -- Extract integers from a collection
  extractIntList :: EgisonValue -> EvalM [Integer]
  extractIntList (Collection xs) = mapM extractInt (toList xs)
  extractIntList x = (:[]) <$> extractInt x
  
  extractInt :: EgisonValue -> EvalM Integer
  extractInt val = case val of
    CASData _ -> fromEgison val
    _         -> throwErrorWithTrace (TypeMismatch "integer" (Value val))
  
  -- Sort lists lexicographically and calculate permutation sign using bubble sort
  sortWithPermSign :: [[Integer]] -> (Integer, [[Integer]])
  sortWithPermSign [] = (1, [])
  sortWithPermSign [x] = (1, [x])
  sortWithPermSign [x, y] =
    if x > y then (-1, [y, x]) else (1, [x, y])
  sortWithPermSign xs =
    let sorted = bubbleSort xs
        swaps = countInversions xs sorted
        sign = if even swaps then 1 else -1
    in (sign, sorted)
  
  -- Bubble sort for lists (lexicographic comparison)
  bubbleSort :: [[Integer]] -> [[Integer]]
  bubbleSort [] = []
  bubbleSort xs =
    let (xs', changed) = bubblePass xs
    in if changed then bubbleSort xs' else xs'
  
  bubblePass :: [[Integer]] -> ([[Integer]], Bool)
  bubblePass [] = ([], False)
  bubblePass [x] = ([x], False)
  bubblePass (x:y:rest) =
    if x > y
      then let (rest', _) = bubblePass (x:rest)
           in (y:rest', True)
      else let (rest', changed) = bubblePass (y:rest)
           in (x:rest', changed)
  
  -- Count inversions between original and sorted list
  countInversions :: (Eq a) => [a] -> [a] -> Int
  countInversions orig sorted =
    let indices = map (\x -> findIndex x sorted) orig
        findIndex x xs = case lookup x (zip xs [0..]) of
          Just i -> i
          Nothing -> 0
    in countInv indices
  
  countInv :: [Int] -> Int
  countInv [] = 0
  countInv (x:xs) = length (filter (< x) xs) + countInv xs

 {-- -- for 'egison-sqlite'
sqlite :: PrimitiveFunc
sqlite  = twoArgs' $ \val val' -> do
  dbName <- fromEgison val
  qStr <- fromEgison val'
  ret <- liftIO $ query' (T.pack dbName) $ T.pack qStr
  return $ makeIO $ return $ Collection $ Sq.fromList $ map (\r -> Tuple (map toEgison r)) ret
 where
  query' :: T.Text -> T.Text -> IO [[String]]
  query' dbName q = do
    db <- SQLite.open dbName
    rowsRef <- newIORef []
    SQLite.execWithCallback db q (\_ _ mcs -> do
                                    row <- forM mcs (\mcol -> case mcol of
                                                              Just col ->  return $ T.unpack col
                                                              Nothing -> return "null")
                                    rows <- readIORef rowsRef
                                    writeIORef rowsRef (row:rows))
    SQLite.close db
    ret <- readIORef rowsRef
    return $ reverse ret
 --} -- for 'egison-sqlite'
