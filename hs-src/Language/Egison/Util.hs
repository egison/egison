{- |
Module      : Language.Egison.Util
Copyright   : Satoshi Egi
Licence     : MIT

This module provides utility functions.
-}

module Language.Egison.Util
  ( getEgisonExpr
  , completeEgison
  ) where

import           Control.Monad.Except             (liftIO)
import           Data.List
import           System.Console.Haskeline         hiding (catch, handle, throwTo)
import           System.Console.Haskeline.History (addHistoryUnlessConsecutiveDupe)
import           Text.Regex.TDFA                  ((=~))

import           Language.Egison.Parser           as Parser
import           Language.Egison.ParserNonS       as ParserNonS
import           Language.Egison.ParserNonS2      as ParserNonS2
import           Language.Egison.Types

-- |Get Egison expression from the prompt. We can handle multiline input.
getEgisonExpr :: EgisonOpts -> InputT IO (Maybe (String, EgisonTopExpr))
getEgisonExpr opts = getEgisonExpr' opts ""

getEgisonExpr' :: EgisonOpts -> String -> InputT IO (Maybe (String, EgisonTopExpr))
getEgisonExpr' opts prev = do
  mLine <- case prev of
             "" -> getInputLine $ optPrompt opts
             _  -> getInputLine $ replicate (length $ optPrompt opts) ' '
  case mLine of
    Nothing -> return Nothing
    Just [] ->
      if null prev
        then getEgisonExpr opts
        else getEgisonExpr' opts prev
    Just line -> do
      history <- getHistory
      putHistory $ addHistoryUnlessConsecutiveDupe line history
      let input = prev ++ line
      let parsedExpr = (if optUseNonS2 opts then ParserNonS2.parseTopExpr else if optSExpr opts then Parser.parseTopExpr else ParserNonS.parseTopExpr) input
      case parsedExpr of
        Left err | show err =~ "unexpected end of input" ->
          getEgisonExpr' opts $ input ++ "\n"
        Left err -> do
          liftIO $ print err
          getEgisonExpr opts
        Right topExpr -> return $ Just (input, topExpr)

-- |Complete Egison keywords
completeEgison :: Monad m => CompletionFunc m
completeEgison arg@(')':_, _) = completeParen arg
completeEgison arg@('>':_, _) = completeParen arg
completeEgison arg@(']':_, _) = completeParen arg
completeEgison arg@('}':_, _) = completeParen arg
completeEgison arg@('(':_, _) = completeWord Nothing " \t<>[]{}$," completeAfterOpenParen arg
completeEgison arg@('<':_, _) = completeWord Nothing " \t()[]{}$," completeAfterOpenCons arg
completeEgison arg@(' ':_, _) = completeWord Nothing "" completeNothing arg
completeEgison arg@('[':_, _) = completeWord Nothing "" completeNothing arg
completeEgison arg@('{':_, _) = completeWord Nothing "" completeNothing arg
completeEgison arg@([], _) = completeWord Nothing "" completeNothing arg
completeEgison arg@(_, _) = completeWord Nothing " \t[]{}$," completeEgisonKeyword arg

completeAfterOpenParen :: Monad m => String -> m [Completion]
completeAfterOpenParen str = return $ map (\kwd -> Completion kwd kwd False) $ filter (isPrefixOf str) $ egisonPrimitivesAfterOpenParen ++ egisonKeywordsAfterOpenParen

completeAfterOpenCons :: Monad m => String -> m [Completion]
completeAfterOpenCons str = return $ map (\kwd -> Completion kwd kwd False) $ filter (isPrefixOf str) egisonKeywordsAfterOpenCons

completeNothing :: Monad m => String -> m [Completion]
completeNothing _ = return []

completeEgisonKeyword :: Monad m => String -> m [Completion]
completeEgisonKeyword str = return $ map (\kwd -> Completion kwd kwd False) $ filter (isPrefixOf str) egisonKeywords

egisonPrimitivesAfterOpenParen = map ((:) '(') ["+", "-", "*", "/", "numerator", "denominator", "modulo", "quotient", "remainder", "neg", "abs", "eq?", "lt?", "lte?", "gt?", "gte?", "round", "floor", "ceiling", "truncate", "sqrt", "exp", "log", "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh", "asinh", "acosh", "atanh", "itof", "rtof", "stoi", "read", "show", "empty?", "uncons", "unsnoc", "assert", "assert-equal"]
egisonKeywordsAfterOpenParen = map ((:) '(') ["define", "let", "letrec", "lambda", "match", "match-all", "match-lambda", "matcher", "algebraic-data-matcher", "pattern-function", "if", "loop", "io", "do"]
                            ++ ["id", "or", "and", "not", "char", "eq?/m", "compose", "compose3", "list", "map", "between", "repeat1", "repeat", "filter", "separate", "concat", "foldr", "foldl", "map2", "zip", "member?", "member?/m", "include?", "include?/m", "any", "all", "length", "count", "count/m", "car", "cdr", "rac", "rdc", "nth", "take", "drop", "while", "reverse", "multiset", "add", "add/m", "delete-first", "delete-first/m", "delete", "delete/m", "difference", "difference/m", "union", "union/m", "intersect", "intersect/m", "set", "unique", "unique/m", "print", "print-to-port", "each", "pure-rand", "fib", "fact", "divisor?", "gcd", "primes", "find-factor", "prime-factorization", "p-f", "min", "max", "min-and-max", "power", "mod", "sort", "intersperse", "intercalate", "split", "split/m"]
egisonKeywordsAfterOpenCons = map ((:) '<') ["nil", "cons", "join", "snoc", "nioj"]
egisonKeywordsInNeutral = "something" : ["bool", "string", "integer", "nats", "primes"]
egisonKeywords = egisonPrimitivesAfterOpenParen ++ egisonKeywordsAfterOpenParen ++ egisonKeywordsAfterOpenCons ++ egisonKeywordsInNeutral

completeParen :: Monad m => CompletionFunc m
completeParen (lstr, _) = case closeParen lstr of
  Nothing    -> return (lstr, [])
  Just paren -> return (lstr, [Completion paren paren False])

closeParen :: String -> Maybe String
closeParen str = closeParen' 0 $ removeCharAndStringLiteral str

removeCharAndStringLiteral :: String -> String
removeCharAndStringLiteral [] = []
removeCharAndStringLiteral ('"':'\\':str) = '"':'\\':removeCharAndStringLiteral str
removeCharAndStringLiteral ('"':str) = removeCharAndStringLiteral' str
removeCharAndStringLiteral ('\'':'\\':str) = '\'':'\\':removeCharAndStringLiteral str
removeCharAndStringLiteral ('\'':str) = removeCharAndStringLiteral' str
removeCharAndStringLiteral (c:str) = c:removeCharAndStringLiteral str

removeCharAndStringLiteral' :: String -> String
removeCharAndStringLiteral' []              = []
removeCharAndStringLiteral' ('"':'\\':str)  = removeCharAndStringLiteral' str
removeCharAndStringLiteral' ('"':str)       = removeCharAndStringLiteral str
removeCharAndStringLiteral' ('\'':'\\':str) = removeCharAndStringLiteral' str
removeCharAndStringLiteral' ('\'':str)      = removeCharAndStringLiteral str
removeCharAndStringLiteral' (_:str)         = removeCharAndStringLiteral' str

closeParen' :: Integer -> String -> Maybe String
closeParen' _ []        = Nothing
closeParen' 0 ('(':_)   = Just ")"
closeParen' 0 ('<':_)   = Just ">"
closeParen' 0 ('[':_)   = Just "]"
closeParen' 0 ('{':_)   = Just "}"
closeParen' n ('(':str) = closeParen' (n - 1) str
closeParen' n ('<':str) = closeParen' (n - 1) str
closeParen' n ('[':str) = closeParen' (n - 1) str
closeParen' n ('{':str) = closeParen' (n - 1) str
closeParen' n (')':str) = closeParen' (n + 1) str
closeParen' n ('>':str) = closeParen' (n + 1) str
closeParen' n (']':str) = closeParen' (n + 1) str
closeParen' n ('}':str) = closeParen' (n + 1) str
closeParen' n (_:str)   = closeParen' n str
