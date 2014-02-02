module Main where

import Prelude hiding (catch)
import Control.Exception ( SomeException(..),
                           AsyncException(..),
                           catch, handle, throw)
import System.Posix.Signals
import Control.Concurrent

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error

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
                        env <- primitiveEnv >>= loadLibraries
                        when bannerFlag showBanner >> repl env prompt >> when bannerFlag showByebyeMessage
                    (file:args) -> do
                        case opts of
                          Options {optLoadOnly = True} -> do
                            env <- primitiveEnvNoIO >>= loadLibraries
                            result <- evalEgisonTopExprs env [LoadFile file]
                            either print (const $ return ()) result
                          Options {optLoadOnly = False} -> do
                            env <- primitiveEnv >>= loadLibraries
                            result <- evalEgisonTopExprs env [LoadFile file, Execute (VarExpr "main") (TupleExpr (map StringExpr args))]
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
  liftIO (runInputT (settings home) $ (loop env prompt ""))
  where
    settings :: MonadIO m => FilePath -> Settings m
    settings home = do
      setComplete noCompletion $ defaultSettings { historyFile = Just (home </> ".egison_history") }
    
    loop :: Env -> String -> String -> InputT IO ()
    loop env prompt' rest = do
      _ <- liftIO $ installHandler keyboardSignal (Catch (do {putStr "^C"; hFlush stdout})) Nothing
      input <- getInputLine prompt'
      tid <- liftIO $ myThreadId
      _ <- liftIO $ installHandler keyboardSignal (Catch (throwTo tid UserInterruption)) Nothing
      case input of
        Nothing -> return () 
        Just "quit" -> return () 
        Just "" ->
          case rest of
            "" -> loop env prompt rest
            _ -> loop env (take (length prompt) (repeat ' ')) rest
        Just input' -> do
          let newInput = rest ++ input'
          result <- liftIO $ handle onAbort $ runEgisonTopExpr env newInput
          case result of
            Left err | show err =~ "unexpected end of input" -> do
              loop env (take (length prompt) (repeat ' ')) $ newInput ++ "\n"
            Left err | show err =~ "expecting (top-level|\"define\")" -> do
              result <- liftIO $ handle onAbort $ fromEgisonM (readExpr newInput) >>= either (return . Left) (evalEgisonExpr env)
              case result of
                Left err | show err =~ "unexpected end of input" -> do
                  loop env (take (length prompt) (repeat ' ')) $ newInput ++ "\n"
                Left err -> do
                  liftIO $ putStrLn $ show err
                  loop env prompt ""
                Right val -> do
                  liftIO $ putStrLn $ show val
                  loop env prompt ""
            Left err -> do
              liftIO $ putStrLn $ show err
              loop env prompt ""
            Right env' ->
              loop env' prompt ""
