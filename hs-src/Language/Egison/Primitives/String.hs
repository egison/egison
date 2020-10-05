module Language.Egison.Primitives.String
  ( primitiveStringFunctions
  ) where

import           Control.Monad.Except

import           Data.Foldable                    (toList)
import qualified Data.Sequence                    as Sq
import           Data.Text                        (Text)
import qualified Data.Text                        as T

import           Text.Regex.TDFA                  ((=~~))

import           Language.Egison.Data
import           Language.Egison.Eval
import           Language.Egison.EvalState        (MonadEval(..))
import           Language.Egison.Parser
import           Language.Egison.Pretty
import           Language.Egison.Primitives.Utils


primitiveStringFunctions :: [(String, EgisonValue)]
primitiveStringFunctions =
  map (\(name, fn) -> (name, PrimitiveFunc (fn name))) strictPrimitives

strictPrimitives :: [(String, String -> PrimitiveFunc)]
strictPrimitives =
  [ ("pack", pack)
  , ("unpack", unpack)
  , ("unconsString", unconsString)
  , ("lengthString", lengthString)
  , ("appendString", appendString)
  , ("splitString", splitString)
  , ("regex", regexString)
  , ("regexCg", regexStringCaptureGroup)

  , ("read", read')
  , ("readTsv", readTSV)
  , ("show", show')
  , ("showTsv", showTSV')
  ]

pack :: String -> PrimitiveFunc
pack = oneArg $ \val -> do
  str <- packStringValue val
  return $ String str
  where
    packStringValue :: EgisonValue -> EvalM Text
    packStringValue (Collection seq) = do
      let ls = toList seq
      str <- mapM fromEgison ls
      return $ T.pack str
    packStringValue (Tuple [val]) = packStringValue val
    packStringValue val = throwError =<< TypeMismatch "collection" (Value val) <$> getFuncNameStack

unpack :: String -> PrimitiveFunc
unpack = unaryOp T.unpack

unconsString :: String -> PrimitiveFunc
unconsString = oneArg $ \val -> do
  str <- fromEgison val
  case T.uncons str of
    Just (c, rest) -> return $ Tuple [Char c, String rest]
    Nothing -> throwError $ Default "Tried to unsnoc empty string"

lengthString :: String -> PrimitiveFunc
lengthString = unaryOp (toInteger . T.length)

appendString :: String -> PrimitiveFunc
appendString = binaryOp T.append

splitString :: String -> PrimitiveFunc
splitString = twoArgs $ \pat src -> do
  patStr <- fromEgison pat
  srcStr <- fromEgison src
  return . Collection . Sq.fromList $ map String $ T.splitOn patStr srcStr

regexString :: String -> PrimitiveFunc
regexString = twoArgs $ \pat src -> do
  patStr <- fromEgison pat
  srcStr <- fromEgison src
  case (T.unpack srcStr =~~ T.unpack patStr) :: (Maybe (String, String, String)) of
    Nothing -> return . Collection . Sq.fromList $ []
    Just (a,b,c) -> return . Collection . Sq.fromList $ [Tuple [String (T.pack a), String (T.pack b), String (T.pack c)]]

regexStringCaptureGroup :: String -> PrimitiveFunc
regexStringCaptureGroup = twoArgs $ \pat src -> do
  patStr <- fromEgison pat
  srcStr <- fromEgison src
  case (T.unpack srcStr =~~ T.unpack patStr) :: (Maybe [[String]]) of
    Nothing -> return . Collection . Sq.fromList $ []
    Just ((x:xs):_) -> do let (a, c) = T.breakOn (T.pack x) srcStr
                          return . Collection . Sq.fromList $ [Tuple [String a, Collection (Sq.fromList (map (String . T.pack) xs)), String (T.drop (length x) c)]]

--
-- Read / Show
--

read' :: String -> PrimitiveFunc
read'= oneArg' $ \val -> do
  str <- fromEgison val
  ast <- readExpr (T.unpack str)
  evalExpr nullEnv ast

readTSV :: String -> PrimitiveFunc
readTSV = oneArg' $ \val -> do
  str   <- fromEgison val
  exprs <- mapM (readExpr . T.unpack) (T.split (== '\t') str)
  rets  <- mapM (evalExpr nullEnv) exprs
  case rets of
    [ret] -> return ret
    _     -> return (Tuple rets)

show' :: String -> PrimitiveFunc
show'= oneArg' $ \val -> return $ toEgison $ T.pack $ show val

showTSV' :: String -> PrimitiveFunc
showTSV'= oneArg' $ \val -> return $ toEgison $ T.pack $ showTSV val
