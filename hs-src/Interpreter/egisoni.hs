{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import System.Environment
import System.Console.Haskeline

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec
import Text.Parsec.ByteString.Lazy

import Language.Egison.Types
import Language.Egison.Parser

main :: IO ()
main = do args <- getArgs
          if null args
            then repl
            else readFile (args !! 0) >>= putStrLn . runParser' parseTopExprs

runParser' :: Show a => Parser a -> String -> String
runParser' parser input = either show show $ parse parser "egison" (B.pack input)

repl :: IO ()
repl = do
  mInput <- runInputT defaultSettings $ getInputLine "> "
  case mInput of
    Nothing -> return ()
    Just input -> do
      putStrLn $ runParser' parseTopExpr input
      repl
