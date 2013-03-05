{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Error

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec
import Text.Parsec.ByteString.Lazy

import System.Environment
import System.Console.Haskeline

import Language.Egison.Types
import Language.Egison.Parser
import Language.Egison.Core

main :: IO ()
-- main = do args <- getArgs
--          if null args
--            then repl
--            else readFile (args !! 0) >>= putStrLn . runParser' parseTopExprs
main = repl

runParser' :: Parser a -> String -> Either EgisonError a
runParser' parser input = either (throwError . Parser) return $ parse parser "egison" (B.pack input)

runEgison :: Env -> String -> IO (Either EgisonError Env)
runEgison env input = runErrorT . runEgisonM $ do 
  expr <- liftError $ runParser' parseTopExpr input
  evalTopExpr env expr

repl :: IO ()
repl = primitives >>= repl'

repl' :: Env -> IO ()
repl' env = do
  mInput <- runInputT defaultSettings $ getInputLine "> "
  case mInput of
    Nothing -> return ()
    Just input -> 
      do result <- runEgison env input
         case result of
           Left err -> print err >> repl' env
           Right env -> repl' env
