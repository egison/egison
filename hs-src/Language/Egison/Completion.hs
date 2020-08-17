{- |
Module      : Language.Egison.Completion
Licence     : MIT

This module provides command-line completion.
-}

module Language.Egison.Completion
  ( completeEgison
  ) where

import           Data.HashMap.Strict        (keys)
import           Data.List
import           System.Console.Haskeline    hiding (catch, handle, throwTo)

import           Language.Egison.Data        (Env (..))
import           Language.Egison.Parser.NonS (upperReservedWords, lowerReservedWords)

-- |Complete Egison keywords
completeEgison :: Monad m => Env -> CompletionFunc m
completeEgison _   arg@(')':_, _) = completeParen arg
completeEgison _   arg@(']':_, _) = completeParen arg
completeEgison _   arg@('(':_, _) = completeWord Nothing "" completeNothing arg
completeEgison _   arg@(' ':_, _) = completeWord Nothing "" completeNothing arg
completeEgison _   arg@('[':_, _) = completeWord Nothing "" completeNothing arg
completeEgison _   arg@([], _)    = completeWord Nothing "" completeNothing arg
completeEgison env arg            = completeWord Nothing " \t[]{}$," (completeEgisonKeyword env) arg

completeNothing :: Monad m => String -> m [Completion]
completeNothing _ = return []

completeEgisonKeyword :: Monad m => Env -> String -> m [Completion]
completeEgisonKeyword (Env env _) str = do
  let definedWords = filter f $ map show $ concatMap keys env
  return $ map (\kwd -> Completion kwd kwd False) $ filter (isPrefixOf str) (egisonKeywords ++ definedWords)
 where
   f [_]         = False
   f [_, '\'']   = False
   f ('b':'.':_) = False
   f _           = True

egisonKeywords :: [String]
egisonKeywords = upperReservedWords ++ lowerReservedWords

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
closeParen' 0 ('[':_)   = Just "]"
closeParen' n ('(':str) = closeParen' (n - 1) str
closeParen' n ('[':str) = closeParen' (n - 1) str
closeParen' n (')':str) = closeParen' (n + 1) str
closeParen' n (']':str) = closeParen' (n + 1) str
closeParen' n (_:str)   = closeParen' n str
