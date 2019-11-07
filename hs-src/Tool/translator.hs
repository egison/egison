module Main where

import           System.Environment (getArgs)

import           Language.Egison.Parser

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  let ast = parseTopExprs input
  case ast of
    Left _ -> return ()
    Right ast -> print ast
