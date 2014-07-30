module Main where

import Prelude hiding ( catch )
import Control.Exception ( AsyncException(..), catch )
import Control.Monad.Error

import Data.ByteString.Lazy.Char8 ()

import Data.Version

import System.Environment
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.Console.Haskeline hiding (handle, catch, throwTo)
import System.Console.GetOpt
import System.Exit (ExitCode (..), exitWith, exitFailure)

import Language.Egison
import Language.Egison.Util

main :: IO ()
main = do args <- getArgs
          let (actions, nonOpts, _) = getOpt Permute options args
          let opts = foldl (flip id) defaultOptions actions
          case opts of
            Options {optShowHelp = True} -> printHelp
            Options {optShowVersion = True} -> printVersionNumber
            Options {optEvalString = mExpr, optExecuteString = mCmd, optLoadFiles = loadFiles, optPrompt = prompt, optShowBanner = bannerFlag, optNoIO = noIOFlag} -> do
              env <- if noIOFlag then initialEnvNoIO else initialEnv
              result <- evalEgisonTopExprs env (map LoadFile loadFiles)
              case result of
                Left err -> putStrLn $ show err
                Right env -> do
                  case mExpr of
                    Just expr -> do
                      ret <- runEgisonExpr env expr
                      case ret of
                        Left err -> putStrLn $ show err
                        Right val -> putStrLn $ show val
                    Nothing ->
                      case mCmd of
                        Just cmd -> runEgisonTopExpr env ("(execute " ++ cmd ++ ")") >> return ()
                        Nothing ->
                          case nonOpts of
                            [] -> do
                              when bannerFlag showBanner >> repl noIOFlag env prompt >> when bannerFlag showByebyeMessage
                            (file:args) -> do
                              case opts of
                                Options {optTestOnly = True} -> do
                                  result <- if noIOFlag
                                              then do input <- readFile file
                                                      runEgisonTopExprsNoIO env input
                                              else evalEgisonTopExprsTestOnly env [LoadFile file]
                                  either print (const $ return ()) result
                                Options {optTestOnly = False} -> do
                                  result <- evalEgisonTopExprs env [LoadFile file, Execute (ApplyExpr (VarExpr "main") (CollectionExpr (map (ElementExpr . StringExpr) args)))]
                                  either print (const $ return ()) result

data Options = Options {
    optShowVersion :: Bool,
    optShowHelp :: Bool,
    optEvalString :: Maybe String,
    optExecuteString :: Maybe String,
    optLoadFiles :: [String],
    optNoIO :: Bool,
    optShowBanner :: Bool,
    optTestOnly :: Bool,
    optPrompt :: String
    }

defaultOptions :: Options
defaultOptions = Options {
    optShowVersion = False,
    optShowHelp = False,
    optEvalString = Nothing,
    optExecuteString = Nothing,
    optLoadFiles = [],
    optNoIO = False,
    optShowBanner = True,
    optTestOnly = False,
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
  Option ['e'] ["eval"]
    (ReqArg (\expr opts -> opts {optEvalString = Just expr})
            "String")
    "eval the argument string",
  Option ['c'] ["command"]
    (ReqArg (\expr opts -> opts {optExecuteString = Just expr})
            "String")
    "execute the argument string",
  Option ['l'] ["load-file"]
    (ReqArg (\d opts -> opts {optLoadFiles = optLoadFiles opts ++ [d]})
            "[String]")
    "load files",
  Option [] ["no-io"]
    (NoArg (\opts -> opts {optNoIO = True}))
    "prohibit all io primitives",
  Option [] ["no-banner"]
    (NoArg (\opts -> opts {optShowBanner = False}))
    "do not display banner",
  Option ['t'] ["test"]
    (NoArg (\opts -> opts {optTestOnly = True}))
    "execute only test expressions",
  Option ['p'] ["prompt"]
    (ReqArg (\prompt opts -> opts {optPrompt = prompt})
            "String")
    "set prompt string"
  ]

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: egison [option]"
  putStrLn "       egison [options] file"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  --help, -h                 Display this information"
  putStrLn "  --version, -v              Display egison version information"
  putStrLn "  --eval, -e string          Evaluate the argument string"
  putStrLn "  --command, -c string       Execute the argument string"
  putStrLn "  --load-file, -l string     Load the argument file"
  putStrLn "  --test, -t                 Run only test expressions"
  putStrLn "  --prompt string            Set prompt of the interpreter"
  putStrLn "  --no-banner                Don't show banner"
  putStrLn "  --no-io                    Prohibit all IO primitives"
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
  putStrLn $ "** Information **"
  putStrLn $ "We can use the tab key to complete keywords on the interpreter."
  putStrLn $ "If we press the tab key after a closed parenthesis, the next closed parenthesis will be completed."
  putStrLn $ "*****************"

showByebyeMessage :: IO ()
showByebyeMessage = do
  putStrLn $ "Leaving Egison Interpreter."
  exitWith ExitSuccess

repl :: Bool -> Env -> String -> IO ()
repl noIOFlag env prompt = do
  loop env
 where
  settings :: MonadIO m => FilePath -> Settings m
  settings home = setComplete completeEgison $ defaultSettings { historyFile = Just (home </> ".egison_history") }
    
  loop :: Env -> IO ()
  loop env = (do 
    home <- getHomeDirectory
    input <- liftIO $ runInputT (settings home) $ getEgisonExpr prompt
    case (noIOFlag, input) of
      (_, Nothing) -> return ()
      (True, Just (_, (LoadFile _))) -> do
        putStrLn "error: No IO support"
        loop env
      (True, Just (_, (Load _))) -> do
        putStrLn "error: No IO support"
        loop env
      (_, Just (topExpr, _)) -> do
        result <- liftIO $ runEgisonTopExpr env topExpr
        case result of
          Left err -> do
            liftIO $ putStrLn $ show err
            loop env
          Right env' -> loop env')
    `catch`
    (\e -> case e of
             UserInterrupt -> putStrLn "" >> loop env
             StackOverflow -> putStrLn "Stack over flow!" >> loop env
             HeapOverflow -> putStrLn "Heap over flow!" >> loop env
             _ -> putStrLn "error!" >> loop env
     )
