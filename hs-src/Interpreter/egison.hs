{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Exception          (AsyncException (..), catch)
import           Control.Monad.Except
import           Control.Monad.Trans.State
import           Prelude                    hiding (catch)

import           Data.Char
import           Data.Semigroup             ((<>))
import qualified Data.Text                  as T

import           Data.Version

import           System.Console.Haskeline   hiding (catch, handle, throwTo)
import           System.Directory           (getHomeDirectory)
import           System.Exit                (exitFailure, exitSuccess)
import           System.FilePath            ((</>))
import           System.IO

import           Language.Egison
import           Language.Egison.Core       (recursiveBind)
import           Language.Egison.MathOutput
import           Language.Egison.Util

import           Options.Applicative


main :: IO ()
main = execParser parserInfo >>= runWithOptions

parserInfo :: ParserInfo EgisonOpts
parserInfo = info (helper <*> parser)
              $ fullDesc
              <> header "The Egison Programming Language"
 where
  parser = EgisonOpts
            <$> optional ((,) <$> strArgument (metavar "FILE") <*> many (strArgument (metavar "ARGS")))
            <*> switch
                  (short 'v'
                  <> long "version"
                  <> help "Show version number")
            <*> optional (strOption
                  (short 'e'
                  <> long "eval"
                  <> metavar "EXPR"
                  <> help "Evaluate the argument string"))
            <*> optional (strOption
                  (short 'c'
                  <> long "command"
                  <> metavar "EXPR"
                  <> help "Execute the argument string"))
            <*> many (readFieldOption <$> strOption
                  (short 'F'
                  <> long "field"
                  <> metavar "FIELD"
                  <> help "Field information"))
            <*> many (strOption
                  (short 'L'
                  <> long "load-library"
                  <> metavar "FILE"
                  <> help "Load library"))
            <*> many (strOption
                  (short 'l'
                  <> long "load-file"
                  <> metavar "FILE"
                  <> help "Load file"))
            <*> optional (strOption
                  (short 's'
                  <> long "substitute"
                  <> metavar "EXPR"
                  <> help "Operate input in tsv format as infinite stream"))
            <*> optional ((\s -> "(map " ++ s ++ " $)") <$> strOption
                  (short 'm'
                  <> long "map"
                  <> metavar "EXPR"
                  <> help "Operate input in tsv format line by line"))
            <*> optional ((\s -> "(filter " ++ s ++ " $)") <$> strOption
                  (short 'f'
                  <> long "filter"
                  <> metavar "EXPR"
                  <> help "Filter input in tsv format line by line"))
            <*> switch
                  (short 'T'
                  <> long "tsv"
                  <> help "Output in tsv format")
            <*> switch
                  (long "no-io"
                  <> help "Prohibit all io primitives")
            <*> flag True False
                  (long "no-banner"
                  <> help "Do not display banner")
            <*> switch
                  (short 't'
                  <> long "test"
                  <> help "Execute only test expressions")
            <*> strOption
                  (short 'p'
                  <> long "prompt"
                  <> metavar "STRING"
                  <> value "> "
                  <> help "Set prompt string")
            <*> optional (strOption
                  (short 'M'
                  <> long "math"
                  <> metavar "(asciimath|latex|mathematica|maxima)"
                  <> help "Output in AsciiMath, Latex, Mathematica, or Maxima format"))
            <*> flag True False
                  (short 'N'
                  <> long "new-syntax"
                  <> help "Use non-S-expressoin syntax")
            <*> flag False True
                  (long "N2"
                  <> help "[experimental] Use new lexer and parser")

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

runWithOptions :: EgisonOpts -> IO ()
runWithOptions opts
  | optShowVersion opts = putStrLn (showVersion version) >> exitSuccess
  | isInValidMathOption opts = hPrint stderr (Default "this output lang is not supported") >> exitFailure
  | otherwise = do
      coreEnv <- initialEnv opts
      mEnv <- evalEgisonTopExprs opts coreEnv $ map Load (optLoadLibs opts) ++ map LoadFile (optLoadFiles opts)
      case mEnv of
        Left err -> print err
        Right env ->
          case opts of
            EgisonOpts { optEvalString = Just expr }
              | optTsvOutput opts ->
                f opts env $ "(execute (each (compose show-tsv print) " ++ expr ++ "))"
              | otherwise -> do
                ret <- runEgisonExpr opts env expr
                case ret of
                  Left err  -> hPrint stderr err >> exitFailure
                  Right val -> print val >> exitSuccess
            EgisonOpts { optExecuteString = Just cmd } ->
              f opts env $ "(execute " ++ cmd ++ ")"
            EgisonOpts { optSubstituteString = Just sub } ->
              let expr = "(load \"lib/core/shell.egi\") "
                      ++ "(execute (each (compose " ++ (if optTsvOutput opts then "show-tsv" else "show") ++ " print) (let {[$SH.input (SH.gen-input {" ++ unwords (map fst $ optFieldInfo opts) ++  "} {" ++ unwords (map snd $ optFieldInfo opts) ++  "})]} (" ++ sub ++ " SH.input))))"
                in f opts env expr
            EgisonOpts { optExecFile = Nothing } ->
              when (optShowBanner opts) showBanner >> repl opts env >> when (optShowBanner opts) showByebyeMessage >> exitSuccess
            EgisonOpts { optExecFile = Just (file, args) }
              | optTestOnly opts -> do
                result <- if optNoIO opts
                            then do input <- readFile file
                                    runEgisonTopExprs opts env input
                            else evalEgisonTopExprs opts env [LoadFile file]
                either print (const $ return ()) result
              | otherwise -> do
                result <- evalEgisonTopExprs opts env [LoadFile file, Execute (ApplyExpr (VarExpr $ stringToVar "main") (CollectionExpr (map ((ElementExpr . StringExpr) . T.pack) args)))]
                either print (const $ return ()) result
 where
  isInValidMathOption EgisonOpts{ optMathExpr = Just lang } = notElem lang ["asciimath", "latex", "mathematica", "maxima"]
  isInValidMathOption EgisonOpts{ optMathExpr = Nothing } = False
  f opts env expr = do
    cmdRet <- runEgisonTopExpr opts env expr
    case cmdRet of
      Left err -> hPrint stderr err >> exitFailure
      _        -> exitSuccess

showBanner :: IO ()
showBanner = do
  putStrLn $ "Egison Version " ++ showVersion version ++ " (C) 2011-2019 Satoshi Egi"
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
  settings home = setComplete completeEgison $ defaultSettings { historyFile = Just (home </> ".egison_history"), autoAddHistory = False }

  loop :: StateT [(Var, EgisonExpr)] EgisonM Env -> IO ()
  loop st = (do
    home <- getHomeDirectory
    input <- liftIO $ runInputT (settings home) $ getEgisonExpr opts
    case (optNoIO opts, input) of
      (_, Nothing) -> return ()
      (True, Just (_, LoadFile _)) -> do
        putStrLn "error: No IO support"
        loop st
      (True, Just (_, Load _)) -> do
        putStrLn "error: No IO support"
        loop st
      (_, Just (topExpr, _)) -> do
        result <- liftIO $ runEgisonTopExpr' opts st topExpr
        case result of
          Left err -> liftIO (print err) >> loop st
          Right (Nothing, st') -> loop st'
          Right (Just output, st') ->
            case optMathExpr opts of
              Nothing   -> putStrLn output >> loop st'
              Just lang -> putStrLn (changeOutputInLang lang output) >> loop st'
             )
    `catch`
    (\case
        UserInterrupt -> putStrLn "" >> loop st
        StackOverflow -> putStrLn "Stack over flow!" >> loop st
        HeapOverflow  -> putStrLn "Heap over flow!" >> loop st
        _             -> putStrLn "error!" >> loop st
     )
