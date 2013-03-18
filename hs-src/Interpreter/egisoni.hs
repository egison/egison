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
import Language.Egison.Types
import Language.Egison.Parser
import Language.Egison.Core
import Language.Egison.Primitives

main :: IO ()
main = do args <- getArgs
          env <- primitiveEnv >>= loadLibraries
          if null args
            then repl env "> "
            else do
              result <- runErrorT . runEgisonM $ do
                exprs <- loadFile (args !! 0)
                evalTopExprs env exprs
              either print return result

loadLibraries :: Env -> IO Env
loadLibraries env = do
  result <- runErrorT $ runEgisonM $ foldM evalTopExpr env (map Load libraries)
  case result of
    Left err -> do
      print . show $ err
      return env
    Right env' -> 
      return env'
  where
    libraries :: [String]
    libraries = [ "lib/core/base.egi"
                , "lib/core/number.egi"
                , "lib/core/collection.egi"
                , "lib/core/pattern.egi" ]

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
        
     
    
