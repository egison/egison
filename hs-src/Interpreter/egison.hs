{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Exception          (AsyncException (..), catch)
import           Control.Monad.Except
import           Control.Monad.Trans.State
import           Prelude                    hiding (catch)

import           Data.Char
import           Data.List                  (intercalate)
import qualified Data.Text                  as T

import           Data.Version

import           System.Console.GetOpt
import           System.Console.Haskeline   hiding (catch, handle, throwTo)
import           System.Directory           (getHomeDirectory)
import           System.Environment
import           System.Exit                (ExitCode (..), exitFailure,
                                             exitSuccess)
import           System.FilePath            ((</>))
import           System.IO

import           Language.Egison
import           Language.Egison.Core       (recursiveBind)
import           Language.Egison.MathOutput
import           Language.Egison.Util

import           Options.Applicative

runWithOptions :: EgisonOpt -> IO ()
runWithOptions opts =
  if optShowHelp opts then printHelp else printVersionNumber

main :: IO ()
main = execParser opts >>= runWithOptions
 where
  parser = EgisonOpt <$> switch (short 'h' <> long "help" <> help "show usage information")
                     <*> switch (short 'v' <> long "version" <> help "show version number")
                     <*> strOption (short 'e' <> long "eval" <> help "eval the argument string")
                     <*> strOption (short 'c' <> long "command" <> help "execute the argument string")
                     <*> strOption (short 's' <> long "substitute" <> help "substitute strings")
                     <*> strOption (short 'F' <> long "field" <> help "field information")
                     <*> switch (short 'L' <> long "load-library" <> help "load library")
                     <*> switch (short 'l' <> long "load-file" <> help "load file")
                     <*> switch (short 'T' <> long "tsv" <> help "output in tsv format")
                     <*> switch (long "no-io" <> help "prohibit all io primitives")
                     <*> switch (long "no-banner" <> help "do not display banner")
                     <*> switch (short 't' <> long "test" <> help "execute only test expressions")
                     <*> switch (short 'p' <> long "prompt" <> help "set prompt string")
                     <*> switch (short 'M' <> long "math" <> help "output in AsciiMath, Latex, Mathematica, or Maxima format")
                     <*> switch (short 'N' <> long "new-syntax" <> help "parse by new syntax")
  opts = info parser mempty
-- main = do args <- getArgs
--           let (actions, nonOpts, _) = getOpt Permute options args
--           let opts = foldl (flip id) defaultOptions actions
 --          if | opts ^. optShowHelp -> printHelp
 --             | opts ^. optShowVersion -> printVersionNumber
 --             | otherwise ->
 --                case opts of
 --                  Options {_optEvalString = mExpr, _optSubstituteString = mSub, _optFieldInfo = fieldInfo, _optLoadLibs = loadLibs, _optLoadFiles = loadFiles, _optShowBanner = bannerFlag, _optTsvOutput = tsvFlag, _optSExpr = isSExpr} -> do
 --                    coreEnv <- if opts ^. optNoIO then initialEnvNoIO else initialEnv
 --                    mEnv <- evalEgisonTopExprs coreEnv $ map (Load isSExpr) loadLibs ++ map (LoadFile isSExpr) loadFiles
 --                    case mEnv of
 --                      Left err -> print err
 --                      Right env ->
 --                        case mExpr of
 --                          Just expr ->
 --                            if tsvFlag
 --                              then f isSExpr env $ "(execute (each (compose show-tsv print) " ++ expr ++ "))"
 --                              else do ret <- runEgisonExpr isSExpr env expr
 --                                      case ret of
 --                                        Left err  -> hPrint stderr err >> exitFailure
 --                                        Right val -> print val >> exitSuccess
 --                          Nothing ->
 --                            case (opts ^. optExecuteString) of
 --                              Just cmd -> f isSExpr env $ "(execute " ++ cmd ++ ")"
 --                              Nothing ->
 --                                case mSub of
 --                                  Just sub -> f isSExpr env $ "(load \"lib/core/shell.egi\") (execute (each (compose " ++ (if tsvFlag then "show-tsv" else "show") ++ " print) (let {[$SH.input (SH.gen-input {" ++ unwords (map fst fieldInfo) ++  "} {" ++ unwords (map snd fieldInfo) ++  "})]} (" ++ sub ++ " SH.input))))"
 --                                  Nothing ->
 --                                    case nonOpts of
 --                                      [] -> when bannerFlag showBanner >> repl (opts ^. optNoIO) isSExpr (opts ^. optMathExpr) env (opts ^. optPrompt) >> when bannerFlag showByebyeMessage >> exitSuccess
 --                                      (file:args) ->
 --                                        case opts of
 --                                          Options {_optTestOnly = True} -> do
 --                                            result <- if (opts ^. optNoIO)
 --                                                        then do input <- readFile file
 --                                                                nunEgisonTopExprsNoIO isSExpr env input
 --                                                        else evalEgisonTopExprsTestOnly env [LoadFile isSExpr file]
 --                                            either print (const $ return ()) result
 --                                          Options {_optTestOnly = False} -> do
 --                                            result <- evalEgisonTopExprs env [LoadFile isSExpr file, Execute (ApplyExpr (VarExpr $ stringToVar "main") (CollectionExpr (map ((ElementExpr . StringExpr) . T.pack) args)))]
 --                                            either print (const $ return ()) result
 -- where
 --  f isSExpr env expr = do
 --    cmdRet <- runEgisonTopExpr opts env expr
 --    case cmdRet of
 --      Left err -> hPrint stderr err >> exitFailure
 --      _        -> exitSuccess
--
-- options
--

data EgisonOpt = EgisonOpt {
    optShowHelp         :: Bool,
    optShowVersion      :: Bool,
    optEvalString       :: Maybe String,
    optExecuteString    :: Maybe String,
    optSubstituteString :: Maybe String,
    optFieldInfo        :: [(String, String)],
    optLoadLibs         :: [String],
    optLoadFiles        :: [String],
    optTsvOutput        :: Bool,
    optNoIO             :: Bool,
    optShowBanner       :: Bool,
    optTestOnly         :: Bool,
    optPrompt           :: String,
    optMathExpr         :: Maybe String,
    optSExpr            :: Bool
    }

defaultOptions :: EgisonOpt
defaultOptions = EgisonOpt {
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
    optMathExpr = Nothing,
    optSExpr = True
    }

options :: [OptDescr (EgisonOpt -> EgisonOpt)]
options = [
  Option ['h', '?'] ["help"]
    (NoArg (\opts -> opts {optShowHelp = True}))
    "show usage information",
  Option ['v', 'V'] ["version"]
    (NoArg (\opts -> opts {optShowVersion = True}))
    "show version number",
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
  Option ['p'] ["prompt"]
    (ReqArg (\prompt opts -> opts {optPrompt = prompt})
            "String")
    "set prompt string",
  Option [] ["no-banner"]
    (NoArg (\opts -> opts {optShowBanner = False}))
    "do not display banner",
  Option ['t'] ["test"]
    (NoArg (\opts -> opts {optTestOnly = True}))
    "execute only test expressions",
  Option ['e'] ["eval"]
    (ReqArg (\expr opts -> opts {optEvalString = Just expr})
            "String")
    "eval the argument string",
  Option ['c'] ["command"]
    (ReqArg (\expr opts -> opts {optExecuteString = Just expr})
            "String")
    "execute the argument string",
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
  Option ['T'] ["tsv"]
    (NoArg (\opts -> opts {optTsvOutput = True}))
    "output in tsv format",
  Option ['F'] ["--field"]
    (ReqArg (\d opts -> opts {optFieldInfo = optFieldInfo opts ++ [readFieldOption d]})
            "String")
    "field information",
  Option ['M'] ["math"]
    (ReqArg (\lang opts -> opts {optMathExpr = Just lang})
            "String")
    "output in AsciiMath, Latex, Mathematica, or Maxima format",
  Option ['N'] ["new-syntax"]
    (NoArg (\opts -> opts {optSExpr = False}))
    "parse by new syntax"
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
  putStrLn ""
  putStrLn "Options to change input or output format:"
  putStrLn "  --tsv, -T                  Input and output in tsv format"
  putStrLn "  --field, -F field          Specify a field type of input tsv"
  putStrLn "  --math, -M (asciimath|latex|mathematica|maxima)"
  putStrLn "                             Output in AsciiMath, LaTeX, or Mathematica format (only for interpreter)"
  exitSuccess

printVersionNumber :: IO ()
printVersionNumber = do
  putStrLn $ showVersion version
  exitSuccess

showBanner :: IO ()
showBanner = do
  putStrLn $ "Egison Version " ++ showVersion version ++ " (C) 2011-2018 Satoshi Egi"
  putStrLn $ "https://www.egison.org"
  putStrLn $ "Welcome to Egison Interpreter!"
--  putStrLn $ "** Information **"
--  putStrLn $ "We can use the tab key to complete keywords on the interpreter."
--  putStrLn $ "If we press the tab key after a closed parenthesis, the next closed parenthesis will be completed."
--  putStrLn $ "*****************"

showByebyeMessage :: IO ()
showByebyeMessage = putStrLn "Leaving Egison Interpreter."

repl :: Bool -> Bool -> Maybe String -> Env -> String -> IO ()
repl noIOFlag isSExpr mathExprLang env prompt =
  loop $ StateT (\defines -> (, defines) <$> recursiveBind env defines)
 where
  settings :: MonadIO m => FilePath -> Settings m
  settings home = setComplete completeEgison $ defaultSettings { historyFile = Just (home </> ".egison_history") }

  loop :: StateT [(Var, EgisonExpr)] EgisonM Env -> IO ()
  loop st = (do
    home <- getHomeDirectory
    input <- liftIO $ runInputT (settings home) $ getEgisonExpr isSExpr prompt
    case (noIOFlag, input) of
      (_, Nothing) -> return ()
      (True, Just (_, LoadFile _ _)) -> do
        putStrLn "error: No IO support"
        loop st
      (True, Just (_, Load _ _)) -> do
        putStrLn "error: No IO support"
        loop st
      (_, Just (topExpr, _)) -> do
        result <- liftIO $ runEgisonTopExpr' isSExpr st topExpr
        case result of
          Left err -> do
            liftIO $ print err
            loop st
          Right (Nothing, st') -> loop st'
          Right (Just output, st') ->
            case mathExprLang of
              Nothing -> putStrLn output >> loop st'
              (Just "haskell") -> putStrLn (mathExprToHaskell output) >> loop st'
              (Just "asciimath") -> putStrLn (mathExprToAsciiMath output) >> loop st'
              (Just "latex") -> putStrLn (mathExprToLatex output) >> loop st'
              (Just "mathematica") -> putStrLn (mathExprToMathematica output) >> loop st'
              (Just "maxima") -> putStrLn (mathExprToMaxima output) >> loop st'
              _ -> putStrLn "error: this output lang is not supported"
             )
    `catch`
    (\case
        UserInterrupt -> putStrLn "" >> loop st
        StackOverflow -> putStrLn "Stack over flow!" >> loop st
        HeapOverflow  -> putStrLn "Heap over flow!" >> loop st
        _             -> putStrLn "error!" >> loop st
     )

