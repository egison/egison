module Main where

import           Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import           System.Environment                    (getArgs)

import           Language.Egison.Parser
import           Language.Egison.PrettyPrint

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  let ast = parseTopExprs input
  case ast of
    Left _ -> return ()
    Right ast -> do
      putDoc $ prettyTopExprs ast
      putStrLn ""
