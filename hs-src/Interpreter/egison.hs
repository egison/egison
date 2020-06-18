{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Exception          (AsyncException (..))
import           Control.Monad.Catch        (catch)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

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
import           Language.Egison.Desugar
import qualified Language.Egison.Parser.SExpr as SExpr
import qualified Language.Egison.Parser.NonS  as NonS

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
runWithOptions opts = do
  coreEnv <- initialEnv opts
  mEnv <- evalEgisonTopExprs opts coreEnv $ map Load (optLoadLibs opts) ++ map LoadFile (optLoadFiles opts)
  case mEnv of
    Left err -> print err
    Right env ->
      case opts of
        -- Evaluate the given string
        EgisonOpts { optEvalString = Just expr }
          | optTsvOutput opts ->
            executeEgisonTopExpr opts env $ "execute (each (\\x -> print (showTsv x)) (" ++ expr ++ "))"
          | otherwise -> do
            executeEgisonTopExpr opts env $ "execute (print (show (" ++ expr ++ ")))"
        -- Execute the given string
        EgisonOpts { optExecuteString = Just cmd } ->
          executeEgisonTopExpr opts env $ "execute (" ++ cmd ++ ")"
        -- Operate input in tsv format as infinite stream
        EgisonOpts { optSubstituteString = Just sub } ->
          let (sopts, copts) = unzip (optFieldInfo opts)
              sopts' = "[" ++ intercalate ", " sopts ++ "]"
              copts' = "[" ++ intercalate ", " copts ++ "]"
              expr = "load \"lib/core/shell.egi\"\n"
                  ++ "execute (let SH.input := SH.genInput " ++ sopts' ++ " " ++ copts' ++ "\n"
                  ++ "          in each (\\x -> print (" ++ if optTsvOutput opts then "showTsv" else "show" ++ " x)) (" ++ sub ++ " SH.input))"
            in executeEgisonTopExpr opts env expr
        -- Execute a script (test only)
        EgisonOpts { optTestOnly = True, optExecFile = Just (file, _) } -> do
          result <- if optNoIO opts
                       then do input <- readFile file
                               runEgisonTopExprs opts env input
                       else evalEgisonTopExprs opts env [LoadFile file]
          either print (const $ return ()) result
        -- Execute a script from the main function
        EgisonOpts { optExecFile = Just (file, args) } -> do
          result <- evalEgisonTopExprs opts env [LoadFile file, Execute (ApplyExpr (stringToVarExpr "main") (CollectionExpr (map ((ElementExpr . StringExpr) . T.pack) args)))]
          either print (const $ return ()) result
        -- Start the read-eval-print-loop
        _ -> do
          when (optShowBanner opts) showBanner
          evalRuntimeT opts env repl
          when (optShowBanner opts) showByebyeMessage
          exitSuccess

executeEgisonTopExpr :: EgisonOpts -> Env -> String -> IO ()
executeEgisonTopExpr opts env expr = do
  cmdRet <- runEgisonTopExprs opts env expr
  case cmdRet of
    Left err -> hPrint stderr err >> exitFailure
    _        -> exitSuccess

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

repl :: RuntimeT IO ()
repl = (do
  opts <- ask
  env  <- gets environment
  home <- liftIO getHomeDirectory
  input <- liftIO $ runInputT (settings home) $ getEgisonExpr opts
  case input of
    Nothing -> return ()
    Just topExpr -> do
      result <- fromEvalT (desugarTopExpr topExpr >>= evalTopExpr opts env)
      case result of
        Left err -> do
          liftIO (print err)
          repl
        Right env' -> do
          modify (\s -> s { environment = env' })
          repl
  )
  `catch`
  (\case
      UserInterrupt -> liftIO (putStrLn "") >> repl
      StackOverflow -> liftIO (putStrLn "Stack over flow!") >> repl
      HeapOverflow  -> liftIO (putStrLn "Heap over flow!") >> repl
      _             -> liftIO (putStrLn "error!") >> repl
   )

-- |Get Egison expression from the prompt. We can handle multiline input.
getEgisonExpr :: EgisonOpts -> InputT IO (Maybe EgisonTopExpr)
getEgisonExpr opts = getEgisonExpr' opts ""
  where
    getEgisonExpr' opts prev = do
      mLine <- case prev of
                 "" -> getInputLine $ optPrompt opts
                 _  -> getInputLine $ replicate (length $ optPrompt opts) ' '
      case mLine of
        Nothing -> return Nothing
        Just [] ->
          if null prev
            then getEgisonExpr opts
            else getEgisonExpr' opts prev
        Just line -> do
          history <- getHistory
          putHistory $ addHistoryUnlessConsecutiveDupe line history
          let input = prev ++ line
          let parsedExpr = if optSExpr opts then SExpr.parseTopExpr input
                                            else NonS.parseTopExpr input
          case parsedExpr of
            Left err | show err =~ "unexpected end of input" ->
              getEgisonExpr' opts $ input ++ "\n"
            Left err -> do
              liftIO $ print err
              getEgisonExpr opts
            Right topExpr -> do
              -- outputStr $ show topExpr
              return $ Just topExpr
