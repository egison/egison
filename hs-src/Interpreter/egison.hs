module Main where

import Prelude hiding (catch)
import Control.Exception ( AsyncException(..), catch )
import Control.Monad.Except

import qualified Data.Text as T
import Data.Char
import Data.List (intercalate)

import Data.Version

import System.Environment
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.Console.Haskeline hiding (handle, catch, throwTo)
import System.Console.GetOpt
import System.Exit (ExitCode (..), exitWith, exitFailure)
import System.IO

import Language.Egison
import Language.Egison.Util
import Language.Egison.MathOutput

main :: IO ()
main = do args <- getArgs
          let (actions, nonOpts, _) = getOpt Permute options args
          let opts = foldl (flip id) defaultOptions actions
          case opts of
            Options {optShowHelp = True} -> printHelp
            Options {optShowVersion = True} -> printVersionNumber
            Options {optEvalString = mExpr, optExecuteString = mCmd, optSubstituteString = mSub, optFieldInfo = fieldInfo, optLoadLibs = loadLibs, optLoadFiles = loadFiles, optPrompt = prompt, optShowBanner = bannerFlag, optTsvOutput = tsvFlag, optNoIO = noIOFlag, optMathExpr = mathExprLang} -> do
              coreEnv <- if noIOFlag then initialEnvNoIO else initialEnv
              mEnv <- evalEgisonTopExprs coreEnv $ (map Load loadLibs) ++ (map LoadFile loadFiles)
              case mEnv of
                Left err -> putStrLn $ show err
                Right env -> do
                  case mExpr of
                    Just expr ->
                      if tsvFlag
                        then do ret <- runEgisonTopExprs env ("(execute (each (compose show-tsv print) " ++ expr ++ "))")
                                case ret of
                                  Left err -> hPutStrLn stderr $ show err
                                  Right _ -> return ()
                        else do ret <- runEgisonExpr env expr
                                case ret of
                                  Left err -> hPutStrLn stderr (show err) >> exitFailure
                                  Right val -> putStrLn (show val) >> exitWith ExitSuccess
                    Nothing ->
                      case mCmd of
                        Just cmd -> do cmdRet <- runEgisonTopExpr env ("(execute " ++ cmd ++ ")")
                                       case cmdRet of
                                         Left err -> putStrLn (show err) >> exitFailure
                                         _ -> exitWith ExitSuccess
                        Nothing ->
                          case mSub of
                            Just sub -> do cmdRet <- runEgisonTopExprs env ("(load \"lib/core/shell.egi\") (execute (each (compose " ++ (if tsvFlag then "show-tsv" else "show") ++ " print) (let {[$SH.input (SH.gen-input {" ++ intercalate " " (map fst fieldInfo) ++  "} {" ++ intercalate " " (map snd fieldInfo) ++  "})]} (" ++ sub ++ " SH.input))))")
                                           case cmdRet of
                                             Left err -> putStrLn (show err) >> exitFailure
                                             _ -> exitWith ExitSuccess
                            Nothing ->
                              case nonOpts of
                                [] -> do
                                  when bannerFlag showBanner >> repl noIOFlag mathExprLang env prompt >> when bannerFlag showByebyeMessage >> exitWith ExitSuccess
                                (file:args) -> do
                                  case opts of
                                    Options {optTestOnly = True} -> do
                                      result <- if noIOFlag
                                                  then do input <- readFile file
                                                          runEgisonTopExprsNoIO env input
                                                  else evalEgisonTopExprsTestOnly env [LoadFile file]
                                      either print (const $ return ()) result
                                    Options {optTestOnly = False} -> do
                                      result <- evalEgisonTopExprs env [LoadFile file, Execute (ApplyExpr (VarExpr $ stringToVar "main") (CollectionExpr (map (ElementExpr . StringExpr) (map T.pack args))))]
                                      either print (const $ return ()) result

data Options = Options {
    optShowVersion :: Bool,
    optShowHelp :: Bool,
    optEvalString :: Maybe String,
    optExecuteString :: Maybe String,
    optSubstituteString :: Maybe String,
    optFieldInfo :: [(String, String)],
    optLoadLibs :: [String],
    optLoadFiles :: [String],
    optTsvOutput :: Bool,
    optNoIO :: Bool,
    optShowBanner :: Bool,
    optTestOnly :: Bool,
    optPrompt :: String,
    optMathExpr :: Maybe String
    }

defaultOptions :: Options
defaultOptions = Options {
    optShowVersion = False,
    optShowHelp = False,
    optEvalString = Nothing,
    optExecuteString = Nothing,
    optSubstituteString = Nothing,
    optFieldInfo = [],
    optLoadLibs = [],
    optLoadFiles = [],
    optTsvOutput = False,
    optNoIO = False,
    optShowBanner = True,
    optTestOnly = False,
    optPrompt = "> ",
    optMathExpr = Nothing
    }

options :: [OptDescr (Options -> Options)]
options = [
  Option ['v', 'V'] ["version"]
    (NoArg (\opts -> opts {optShowVersion = True}))
    "show version number",
  Option ['h', '?'] ["help"]
    (NoArg (\opts -> opts {optShowHelp = True}))
    "show usage information",
  Option ['T'] ["tsv"]
    (NoArg (\opts -> opts {optTsvOutput = True}))
    "output in tsv format",
  Option ['e'] ["eval"]
    (ReqArg (\expr opts -> opts {optEvalString = Just expr})
            "String")
    "eval the argument string",
  Option ['c'] ["command"]
    (ReqArg (\expr opts -> opts {optExecuteString = Just expr})
            "String")
    "execute the argument string",
  Option ['L'] ["load-library"]
    (ReqArg (\d opts -> opts {optLoadLibs = optLoadLibs opts ++ [d]})
            "[String]")
    "load library",
  Option ['l'] ["load-file"]
    (ReqArg (\d opts -> opts {optLoadFiles = optLoadFiles opts ++ [d]})
            "[String]")
    "load file",
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
    "set prompt string",
  Option ['s'] ["substitute"]
    (ReqArg (\expr opts -> opts {optSubstituteString = Just expr})
            "String")
    "substitute strings",
  Option ['m'] ["map"]
    (ReqArg (\expr opts -> opts {optSubstituteString = Just ("(map " ++ expr ++ " $)")})
            "String")
    "filter strings",
  Option ['f'] ["filter"]
    (ReqArg (\expr opts -> opts {optSubstituteString = Just ("(filter " ++ expr ++ " $)")})
            "String")
    "filter strings",
  Option ['F'] ["--field"]
    (ReqArg (\d opts -> opts {optFieldInfo = optFieldInfo opts ++ [(readFieldOption d)]})
            "String")
    "field information",
  Option ['M'] ["math"]
    (ReqArg (\lang opts -> opts {optMathExpr = Just lang})
            "String")
    "output in LaTeX format"
  ]

readFieldOption :: String -> (String, String)
readFieldOption str =
   let (s, rs) = span isDigit str in
   case rs of
     ',':rs' -> let (e, opts) = span isDigit rs' in
                case opts of
                  ['s'] -> ("{" ++ s ++ " " ++ e ++ "}", "")
                  ['c'] -> ("{}", "{" ++ s ++ " " ++ e ++ "}")
                  ['s', 'c'] -> ("{" ++ s ++ " " ++ e ++ "}", "{" ++ s ++ " " ++ e ++ "}")
                  ['c', 's'] -> ("{" ++ s ++ " " ++ e ++ "}", "{" ++ s ++ " " ++ e ++ "}")
     ['s'] -> ("{" ++ s ++ "}", "")
     ['c'] -> ("", "{" ++ s ++ "}")
     ['s', 'c'] -> ("{" ++ s ++ "}", "{" ++ s ++ "}")
     ['c', 's'] -> ("{" ++ s ++ "}", "{" ++ s ++ "}")

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: egison [options]"
  putStrLn "       egison [options] file"
  putStrLn "       egison [options] expr"
  putStrLn ""
  putStrLn "Global Options:"
  putStrLn "  --help, -h                 Display this information"
  putStrLn "  --version, -v              Display egison version information"
  putStrLn ""
  putStrLn "  --load-library, -L file    Load the argument library"
  putStrLn "  --load-file, -l file       Load the argument file"
  putStrLn "  --no-io                    Prohibit all IO primitives"
  putStrLn ""
  putStrLn "Options as an interactive interpreter:"
  putStrLn "  --prompt string            Set prompt of the interpreter"
  putStrLn "  --no-banner                Don't show banner"
  putStrLn ""
  putStrLn "Options to handle Egison program file:"
  putStrLn "  --test, -t file            Run only test expressions"
  putStrLn ""
  putStrLn "Options as a shell command:"
  putStrLn "  --eval, -e expr            Evaluate the argument expression"
  putStrLn "  --command, -c expr         Execute the argument expression"
  putStrLn ""
  putStrLn "  --substitute, -s expr      Substitute input using the argument expression"
  putStrLn "  --map, -m expr             Substitute each line of input using the argument expression"
  putStrLn "  --filter, -f expr          Filter each line of input using the argument predicate"
  exitWith ExitSuccess

printVersionNumber :: IO ()
printVersionNumber = do
  putStrLn $ showVersion version 
  exitWith ExitSuccess

showBanner :: IO ()
showBanner = do
  putStrLn $ "Egison Version " ++ showVersion version ++ " (C) 2011-2017 Satoshi Egi"
  putStrLn $ "https://www.egison.org"
  putStrLn $ "Welcome to Egison Interpreter!"
--  putStrLn $ "** Information **"
--  putStrLn $ "We can use the tab key to complete keywords on the interpreter."
--  putStrLn $ "If we press the tab key after a closed parenthesis, the next closed parenthesis will be completed."
--  putStrLn $ "*****************"

showByebyeMessage :: IO ()
showByebyeMessage = putStrLn $ "Leaving Egison Interpreter."

repl :: Bool -> (Maybe String) -> Env -> String -> IO ()
repl noIOFlag mathExprLang env prompt = do
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
        result <- liftIO $ runEgisonTopExpr' env topExpr
        case result of
          Left err -> do
            liftIO $ putStrLn $ show err
            loop env
          Right (Nothing, env') -> loop env'
          Right (Just output, env') ->
            case mathExprLang of
              Nothing -> putStrLn output >> loop env'
              (Just "haskell") -> putStrLn (mathExprToHaskell output) >> loop env'
              (Just "asciimath") -> putStrLn (mathExprToAsciiMath output) >> loop env'
              (Just "latex") -> putStrLn (mathExprToLatex output) >> loop env'
             )
    `catch`
    (\e -> case e of
             UserInterrupt -> putStrLn "" >> loop env
             StackOverflow -> putStrLn "Stack over flow!" >> loop env
             HeapOverflow -> putStrLn "Heap over flow!" >> loop env
             _ -> putStrLn "error!" >> loop env
     )
