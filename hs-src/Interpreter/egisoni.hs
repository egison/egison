module Main where

import Prelude hiding (catch)
import Control.Exception ( SomeException(..),
                           AsyncException(..),
                           catch, handle, throw)
import System.Posix.Signals
import Control.Concurrent

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error

import Data.List
import Data.Sequence (Seq, ViewL(..), ViewR(..), (><))
import qualified Data.Sequence as Sq

import Data.Version
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 ()
import Text.Regex.Posix

import System.Environment
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.Console.Haskeline hiding (handle, catch, throwTo)
import System.Console.GetOpt
import System.Exit (ExitCode (..), exitWith, exitFailure)
import System.IO
import Language.Egison
import Language.Egison.Util

main :: IO ()
main = do args <- getArgs
          let (actions, nonOpts, _) = getOpt Permute options args
          let opts = foldl (flip id) defaultOptions actions
          case opts of
            Options {optShowHelp = True} -> printHelp
            Options {optShowVersion = True} -> printVersionNumber
            Options {optPrompt = prompt, optShowBanner = bannerFlag} -> do
              case nonOpts of
                [] -> do
                  env <- initialEnv
                  when bannerFlag showBanner >> repl env prompt >> when bannerFlag showByebyeMessage
                (file:args) -> do
                  case opts of
                    Options {optLoadOnly = True} -> do
                      env <- initialEnvNoIO
                      result <- evalEgisonTopExprs env [LoadFile file]
                      either print (const $ return ()) result
                    Options {optLoadOnly = False} -> do
                      env <- initialEnv
                      result <- evalEgisonTopExprs env [LoadFile file, Execute (ApplyExpr (VarExpr "main") (CollectionExpr (Sq.fromList (map (ElementExpr . StringExpr) args))))]
                      either print (const $ return ()) result

data Options = Options {
    optShowVersion :: Bool,
    optShowHelp :: Bool,
    optShowBanner :: Bool,
    optLoadOnly :: Bool,
    optPrompt :: String
    }

defaultOptions :: Options
defaultOptions = Options {
    optShowVersion = False,
    optShowHelp = False,
    optShowBanner = True,
    optLoadOnly = False,
    optPrompt = "> "
    }

options :: [OptDescr (Options -> Options)]
options = [
  Option ['v', 'V'] ["version"]
    (NoArg (\opts -> opts {optShowVersion = True}))
    "show version number",
  Option ['h', '?'] ["help"]
    (NoArg (\opts -> opts {optShowHelp = True}))
    "show usage information",
  Option [] ["no-banner"]
    (NoArg (\opts -> opts {optShowBanner = False}))
    "show usage information",
  Option ['l'] ["load"]
    (NoArg (\opts -> opts {optLoadOnly = True}))
    "show usage information",
  Option ['p'] ["prompt"]
    (ReqArg (\prompt opts -> opts {optPrompt = prompt})
            "String")
    "prompt string"
  ]

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: egison [options] file"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  --help                Display this information"
  putStrLn "  --version             Display egison version information"
  putStrLn "  --no-banner           Don't show banner"
  putStrLn "  --load                Don't execute main function"
  putStrLn "  --prompt string       Set prompt of the interpreter"
  putStrLn ""
  exitWith ExitSuccess

printVersionNumber :: IO ()
printVersionNumber = do
  putStrLn $ showVersion version 
  exitWith ExitSuccess

showBanner :: IO ()
showBanner = do
  putStrLn $ "Egison Version " ++ showVersion version ++ " (C) 2011-2014 Satoshi Egi"
  putStrLn $ "http://www.egison.org"
  putStrLn $ "Welcome to Egison Interpreter!"
  putStrLn $ "** Info **"
  putStrLn $ "We can use a \'Tab\' key to complete keywords on the interpreter."
  putStrLn $ "If we type a \'Tab\' key after after a closed parenthesis, the next closed parenthesis will be completed."

showByebyeMessage :: IO ()
showByebyeMessage = do
  putStrLn $ "Leaving Egison Interpreter."

onAbort :: EgisonError -> IO (Either EgisonError a)
onAbort e = do
  let x = show e
  return $ Left e

repl :: Env -> String -> IO ()
repl env prompt = do
  home <- getHomeDirectory
  liftIO $ runInputT (settings home) $ loop env
 where
  settings :: MonadIO m => FilePath -> Settings m
  settings home = setComplete completeEgison $ defaultSettings { historyFile = Just (home </> ".egison_history") }
    
  loop :: Env -> InputT IO ()
  loop env = do
    _ <- liftIO $ installHandler keyboardSignal (Catch (do {putStr "^C"; hFlush stdout})) Nothing
    input <- getEgisonExpr prompt
    tid <- liftIO $ myThreadId
    _ <- liftIO $ installHandler keyboardSignal (Catch (throwTo tid UserInterruption)) Nothing
    case input of
      Nothing -> return ()
      Just (Left topExpr) -> do
        result <- liftIO $ handle onAbort $ evalEgisonTopExpr env topExpr
        case result of
          Left err -> do
            liftIO $ putStrLn $ show err
            loop env
          Right env' -> loop env'
      Just (Right expr) -> do
        result <- liftIO $ handle onAbort $ evalEgisonExpr env expr
        case result of
          Left err -> do
            liftIO $ putStrLn $ show err
            loop env
          Right val -> do
            liftIO $ putStrLn $ show val
            loop env


getEgisonExpr :: String -> InputT IO (Maybe (Either EgisonTopExpr EgisonExpr))
getEgisonExpr prompt = getEgisonExpr' ""
 where
  getEgisonExpr' :: String -> InputT IO (Maybe (Either EgisonTopExpr EgisonExpr))
  getEgisonExpr' prev = do
    mLine <- case prev of
               "" -> getInputLine prompt
               _ -> getInputLine $ take (length prompt ) (repeat ' ')
    case mLine of
      Nothing -> return Nothing
      Just line -> do
        let input = prev ++ line
        case parseTopExpr input of
          Left err | show err =~ "unexpected end of input" -> do
            getEgisonExpr' input
          Left err | show err =~ "expecting (top-level|\"define\")" ->
            case parseExpr input of
              Left err | show err =~ "unexpected end of input" -> do
                getEgisonExpr' input
              Left err -> do
                liftIO $ putStrLn $ show err
                getEgisonExpr prompt
              Right expr -> return $ Just $ Right expr
          Left err -> do
            liftIO $ putStrLn $ show err
            getEgisonExpr prompt
          Right topExpr -> return $ Just $ Left topExpr
