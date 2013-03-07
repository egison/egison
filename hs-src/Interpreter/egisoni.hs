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
import Language.Egison.Primitives

main :: IO ()
main = do args <- getArgs
          if null args
            then repl
            else do
              input <- readFile (args !! 0)
              env <- primitiveEnv
              runEgisonTopExprs env input >>= either print return

runParser' :: Parser a -> String -> Either EgisonError a
runParser' parser input = either (throwError . Parser) return $ parse parser "egison" (B.pack input)

runEgisonTopExpr :: Env -> String -> IO (Either EgisonError Env)
runEgisonTopExpr env input = runErrorT . runEgisonM $ do 
  expr <- liftError $ runParser' parseTopExpr input
  evalTopExpr env expr

runEgisonTopExprs :: Env -> String -> IO (Either EgisonError ())
runEgisonTopExprs env input = runErrorT . runEgisonM $ do 
  expr <- liftError $ runParser' parseTopExprs input
  evalTopExprs env expr

repl :: IO ()
repl = primitiveEnv >>= repl'

repl' :: Env -> IO ()
repl' env = do
  mInput <- runInputT defaultSettings $ getInputLine "> "
  case mInput of
    Nothing -> return ()
    Just input -> 
      do result <- runEgisonTopExpr env input
         case result of
           Left err -> print err >> repl' env
           Right env -> repl' env
