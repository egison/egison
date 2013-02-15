{-# LANGUAGE OverloadedStrings #-}

module Main where
import Prelude hiding (putStr, putStrLn)
import Text.Parsec
import Text.Parsec.Combinator

import Data.ByteString.Lazy(ByteString)
import Data.ByteString.Lazy.Char8
import qualified Data.ByteString.Lazy as B


import Language.Egison.Types
import Language.Egison.Parser

main :: IO ()
main = repl 

runParser' :: String -> String
runParser' input = case parse parseEgisonTopExpr "Test" (pack input)  of
                  Left  err -> show err
                  Right val -> show val 

repl :: IO ()
repl = do putStr ">" 
          line <- getLine
          putStrLn . pack $ runParser' line
          repl

