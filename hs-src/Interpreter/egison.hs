{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Exception          (AsyncException (..))
import           Control.Monad.Catch        (catch)
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.List                  (intercalate)
import qualified Data.Text                  as T

import           Data.Version

import           System.Console.Haskeline   hiding (catch, handle, throwTo)
import           System.Console.Haskeline.History (addHistoryUnlessConsecutiveDupe)
import           System.Directory           (getHomeDirectory)
import           System.Exit                (exitFailure, exitSuccess)
import           System.FilePath            ((</>))
import           System.IO
import           Text.Regex.TDFA            ((=~))

import           Language.Egison
import           Language.Egison.CmdOptions
import           Language.Egison.Completion
import           Language.Egison.Desugar    (desugarTopExpr)
import           Language.Egison.Parser     (parseTopExpr)
import           Language.Egison.RState

import           Options.Applicative

main :: IO ()
main = execParser cmdParser >>= runWithOptions

isInValidMathOption :: EgisonOpts -> Bool
isInValidMathOption EgisonOpts{ optMathExpr = Just lang } = notElem lang ["asciimath", "latex", "mathematica", "maxima"]
isInValidMathOption EgisonOpts{ optMathExpr = Nothing } = False

runWithOptions :: EgisonOpts -> IO ()
runWithOptions opts | isInValidMathOption opts =
  hPrint stderr (Default "this output lang is not supported") >> exitFailure
runWithOptions EgisonOpts{ optShowVersion = True } =
  putStrLn (showVersion version) >> exitSuccess
runWithOptions opts = evalRuntimeT opts run

run :: RuntimeM ()
run = do
  opts <- ask
  coreEnv <- initialEnv
  mEnv <- evalEgisonTopExprs coreEnv $ map Load (optLoadLibs opts) ++ map LoadFile (optLoadFiles opts)
  case mEnv of
    Left err -> liftIO $ print err
    Right env ->
      case opts of
        -- Evaluate the given string
        EgisonOpts { optEvalString = Just expr } ->
          runAndPrintEgisonExpr env expr
        -- Execute the given string
        EgisonOpts { optExecuteString = Just cmd } ->
          executeEgisonTopExpr env $ "execute (" ++ cmd ++ ")"
        -- Operate input in tsv format as infinite stream
        EgisonOpts { optSubstituteString = Just sub } ->
          let (sopts, copts) = unzip (optFieldInfo opts)
              sopts' = "[" ++ intercalate ", " sopts ++ "]"
              copts' = "[" ++ intercalate ", " copts ++ "]"
              expr = "load \"lib/core/shell.egi\"\n"
                  ++ "execute (let SH.input := SH.genInput " ++ sopts' ++ " " ++ copts' ++ "\n"
                  ++ "          in each (\\x -> print (" ++ if optTsvOutput opts then "showTsv" else "show" ++ " x)) (" ++ sub ++ " SH.input))"
            in executeEgisonTopExpr env expr
        -- Execute a script (test only)
        EgisonOpts { optTestOnly = True, optExecFile = Just (file, _) } -> do
          exprs <- liftIO $ readFile file
          result <- if optNoIO opts
                       then runEgisonTopExprs env exprs
                       else evalEgisonTopExprs env [LoadFile file]
          liftIO $ either print (const $ return ()) result
        -- Execute a script from the main function
        EgisonOpts { optExecFile = Just (file, args) } -> do
          result <- evalEgisonTopExprs env [LoadFile file, Execute (ApplyExpr (stringToVarExpr "main") (CollectionExpr (map (StringExpr . T.pack) args)))]
          liftIO $ either print (const $ return ()) result
        -- Start the read-eval-print-loop
        _ -> do
          when (optShowBanner opts) (liftIO showBanner)
          repl env
          when (optShowBanner opts) (liftIO showByebyeMessage)
          liftIO exitSuccess

runAndPrintEgisonExpr :: Env -> String -> RuntimeM ()
runAndPrintEgisonExpr env expr = do
  isTsvOutput <- asks optTsvOutput
  if isTsvOutput
     then executeEgisonTopExpr env $ "execute (each (\\x -> print (showTsv x)) (" ++ expr ++ "))"
     else executeEgisonTopExpr env $ "execute (print (show (" ++ expr ++ ")))"

executeEgisonTopExpr :: Env -> String -> RuntimeM ()
executeEgisonTopExpr env expr = do
  cmdRet <- runEgisonTopExprs env expr
  case cmdRet of
    Left err -> liftIO $ hPrint stderr err >> exitFailure
    _        -> liftIO exitSuccess

showBanner :: IO ()
showBanner = do
  putStrLn $ "Egison Version " ++ showVersion version
  putStrLn $ "https://www.egison.org"
  putStrLn $ "Welcome to Egison Interpreter!"
--  putStrLn $ "** Information **"
--  putStrLn $ "We can use the tab key to complete keywords on the interpreter."
--  putStrLn $ "If we press the tab key after a closed parenthesis, the next closed parenthesis will be completed."
--  putStrLn $ "*****************"

showByebyeMessage :: IO ()
showByebyeMessage = putStrLn "Leaving Egison Interpreter."

settings :: MonadIO m => FilePath -> Settings m
settings home = setComplete completeEgison $ defaultSettings { historyFile = Just (home </> ".egison_history"), autoAddHistory = False }

repl :: Env -> RuntimeM ()
repl env = (do
  home <- liftIO getHomeDirectory
  input <- runInputT (settings home) getEgisonExpr
  case input of
    Nothing -> return ()
    Just topExpr -> do
      result <- fromEvalT (desugarTopExpr topExpr >>= evalTopExpr env)
      case result of
        Left err -> do
          liftIO (print err)
          repl env
        Right env' ->
          repl env'
  )
  `catch`
  (\case
      UserInterrupt -> liftIO (putStrLn "") >> repl env
      StackOverflow -> liftIO (putStrLn "Stack over flow!") >> repl env
      HeapOverflow  -> liftIO (putStrLn "Heap over flow!") >> repl env
      _             -> liftIO (putStrLn "error!") >> repl env
   )

-- |Get Egison expression from the prompt. We can handle multiline input.
getEgisonExpr :: InputT RuntimeM (Maybe EgisonTopExpr)
getEgisonExpr = getEgisonExpr' ""
  where
    getEgisonExpr' prev = do
      opts <- lift ask
      mLine <- case prev of
                 "" -> getInputLine $ optPrompt opts
                 _  -> getInputLine $ replicate (length $ optPrompt opts) ' '
      case mLine of
        Nothing -> return Nothing
        Just [] | null prev -> getEgisonExpr
        Just [] -> getEgisonExpr' prev
        Just line -> do
          history <- getHistory
          putHistory $ addHistoryUnlessConsecutiveDupe line history
          let input = prev ++ line
          parsedExpr <- lift $ parseTopExpr (optSExpr opts) input
          case parsedExpr of
            Left err | show err =~ "unexpected end of input" ->
              getEgisonExpr' (input ++ "\n")
            Left err -> do
              liftIO $ print err
              getEgisonExpr
            Right topExpr -> do
              -- outputStr $ show topExpr
              return $ Just topExpr
