module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error

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
            then repl env "> "
            else do
              result <- runEgisonM $ evalTopExpr env $ LoadFile (args !! 0)
              either print (const $ return ()) result


repl :: Env -> String -> IO ()
repl env prompt = loop env prompt ""
  where
    loop :: Env -> String -> String -> IO ()
    loop env prompt' rest = do
      input <- runInputT defaultSettings $ getInputLine prompt'
      case input of
        Nothing -> return ()
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
        
     
    
