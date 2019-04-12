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


runWithOptions :: EgisonOpts -> IO ()
runWithOptions opts
  | optShowHelp opts = printHelp
  | optShowVersion opts = printVersionNumber
  | otherwise = do
      coreEnv <- if optNoIO opts then initialEnvNoIO else initialEnv
      mEnv <- evalEgisonTopExprs coreEnv $ map (Load $ optSExpr opts) (optLoadLibs opts) ++ map (LoadFile $ optSExpr opts) (optLoadFiles opts)
      case mEnv of
        Left err -> print err
        Right env ->
          case optEvalString opts of
            Just expr ->
              if optTsvOutput opts
                then f opts env $ "(execute (each (compose show-tsv print) " ++ expr ++ "))"
                else do ret <- runEgisonExpr opts env expr
                        case ret of
                          Left err  -> hPrint stderr err >> exitFailure
                          Right val -> print val >> exitSuccess
            Nothing ->
              case optExecuteString opts of
                Just cmd -> f opts env $ "(execute " ++ cmd ++ ")"
                Nothing -> do
                  let fieldinfo = []
                  case optSubstituteString opts of
                    Just sub -> f opts env $ "(load \"lib/core/shell.egi\") (execute (each (compose " ++ (if optTsvOutput opts then "show-tsv" else "show") ++ " print) (let {[$SH.input (SH.gen-input {" ++ unwords (map fst fieldinfo) ++  "} {" ++ unwords (map snd fieldinfo) ++  "})]} (" ++ sub ++ " SH.input))))"
                    Nothing ->
                      case optExecFileandExpr opts of
                        Nothing -> when (optShowBanner opts) showBanner >> repl opts env >> when (optShowBanner opts) showByebyeMessage >> exitSuccess
                        -- (file:args) ->
                        Just file ->
                          case opts of
                            EgisonOpts {optTestOnly = True} -> do
                              result <- if optNoIO opts
                                          then do input <- readFile file
                                                  runEgisonTopExprsNoIO opts env input
                                          else evalEgisonTopExprsTestOnly env [LoadFile (optSExpr opts) file]
                              either print (const $ return ()) result
                            EgisonOpts {optTestOnly = False} -> do
                              let args = []
                              result <- evalEgisonTopExprs env [LoadFile (optSExpr opts) file, Execute (ApplyExpr (VarExpr $ stringToVar "main") (CollectionExpr (map ((ElementExpr . StringExpr) . T.pack) args)))]
                              either print (const $ return ()) result
 where
  f opts env expr = do
    cmdRet <- runEgisonTopExpr opts env expr
    case cmdRet of
      Left err -> hPrint stderr err >> exitFailure
      _        -> exitSuccess

main :: IO ()
main = execParser opts >>= runWithOptions
 where
  parser = EgisonOpts
            <$> optional (strArgument (metavar "FILE"))
            <*> switch (short 'h' <> long "help" <> help "show usage information")
            <*> switch (short 'v' <> long "version" <> help "show version number")
            <*> optional (strOption (short 'e' <> long "eval" <> help "eval the argument string"))
            <*> optional (strOption (short 'c' <> long "command" <> help "execute the argument string"))
            <*> many (readFieldOption <$> strOption (short 'F' <> long "field" <> help "field information"))
            <*> many (strOption (short 'L' <> long "load-library" <> help "load library"))
            <*> many (strOption (short 'l' <> long "load-file" <> help "load file"))
            <*> optional (strOption (short 's' <> long "substitute" <> help "operate input in tsv format as infinite stream"))
            <*> optional ((\s -> "(map " ++ s ++ " $)") <$> strOption (short 'm' <> long "map" <> help "operate input in tsv format line by line"))
            <*> optional ((\s -> "(filter " ++ s ++ " $)") <$> strOption (short 'f' <> long "filter" <> help "filter input in tsv format line by line"))
            <*> switch (short 'T' <> long "tsv" <> help "output in tsv format")
            <*> switch (long "no-io" <> help "prohibit all io primitives")
            <*> flag True False (long "no-banner" <> help "do not display banner")
            <*> switch (short 't' <> long "test" <> help "execute only test expressions")
            <*> strOption (short 'p' <> long "prompt" <> value "> " <> help "set prompt string")
            <*> optional (strOption (short 'M' <> long "math" <> help "output in AsciiMath, Latex, Mathematica, or Maxima format"))
            <*> flag True False (short 'N' <> long "new-syntax" <> help "parse by new syntax")
  opts = info parser mempty

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

repl :: EgisonOpts -> Env -> IO ()
repl opts env =
  loop $ StateT (\defines -> (, defines) <$> recursiveBind env defines)
 where
  settings :: MonadIO m => FilePath -> Settings m
  settings home = setComplete completeEgison $ defaultSettings { historyFile = Just (home </> ".egison_history") }

  loop :: StateT [(Var, EgisonExpr)] EgisonM Env -> IO ()
  loop st = (do
    home <- getHomeDirectory
    input <- liftIO $ runInputT (settings home) $ getEgisonExpr (optSExpr opts) (optPrompt opts)
    case (optNoIO opts, input) of
      (_, Nothing) -> return ()
      (True, Just (_, LoadFile _ _)) -> do
        putStrLn "error: No IO support"
        loop st
      (True, Just (_, Load _ _)) -> do
        putStrLn "error: No IO support"
        loop st
      (_, Just (topExpr, _)) -> do
        result <- liftIO $ runEgisonTopExpr' opts st topExpr
        case result of
          Left err -> do
            liftIO $ print err
            loop st
          Right (Nothing, st') -> loop st'
          Right (Just output, st') ->
            case optMathExpr opts of
              Nothing -> putStrLn output >> loop st'
              Just "haskell" -> putStrLn (mathExprToHaskell output) >> loop st'
              Just "asciimath" -> putStrLn (mathExprToAsciiMath output) >> loop st'
              Just "latex" -> putStrLn (mathExprToLatex output) >> loop st'
              Just "mathematica" -> putStrLn (mathExprToMathematica output) >> loop st'
              Just "maxima" -> putStrLn (mathExprToMaxima output) >> loop st'
              _ -> putStrLn "error: this output lang is not supported"
             )
    `catch`
    (\case
        UserInterrupt -> putStrLn "" >> loop st
        StackOverflow -> putStrLn "Stack over flow!" >> loop st
        HeapOverflow  -> putStrLn "Heap over flow!" >> loop st
        _             -> putStrLn "error!" >> loop st
     )

