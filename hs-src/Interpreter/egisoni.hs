module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error

import Data.Version
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import Text.Regex.Posix

import System.Environment
import System.Console.Haskeline
import Language.Egison

main :: IO ()
main = do args <- getArgs
          env <- primitiveEnv >>= loadLibraries
          if null args
            then showBanner >> repl env "> "
            else do
              result <- runEgisonM $ evalTopExpr env $ LoadFile (args !! 0)
              either print (const $ return ()) result

showBanner :: IO ()
showBanner = do
  putStrLn $ "Egison Version " ++ showVersion version ++ " (C) 2011-2013 Satoshi Egi"
  putStrLn $ "http://egison.pira.jp"
  putStrLn $ "Welcome to Egison Interpreter!"

showByebyeMessage :: IO ()
showByebyeMessage = do
  putStrLn $ "Leaving Egison Interpreter."

settings :: Settings IO
settings = defaultSettings {historyFile = Just ".egison_history"}

repl :: Env -> String -> IO ()
repl env prompt = loop env prompt ""
  where
    loop :: Env -> String -> String -> IO ()
    loop env prompt' rest = do
      input <- runInputT settings $ getInputLine prompt'
      case input of
        Nothing -> return ()
        Just "quit" -> showByebyeMessage >> return ()
        Just "" ->  loop env prompt ""
        Just input' -> do
          let newInput = rest ++ input'
          result <- runEgisonTopExpr env newInput
          case result of
            Left err | show err =~ "unexpected end of input" -> do
              loop env (take (length prompt) (repeat ' ')) $ newInput ++ "\n"
            Left err -> do
              liftIO $ putStrLn $ show err
              loop env prompt ""
            Right env' ->
              loop env' prompt ""
        
     
    
