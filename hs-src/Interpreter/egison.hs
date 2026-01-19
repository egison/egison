{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Exception                (AsyncException (..))
import           Control.Monad                    (when)
import           Control.Monad.Catch              (catch)
import           Control.Monad.Reader

import           Data.List                        (intercalate)
import qualified Data.Text                        as T

import           Data.Version

import           System.Console.Haskeline         (InputT, Settings (..), getHistory, getInputLine, putHistory, runInputT)
import           System.Console.Haskeline.History (addHistoryUnlessConsecutiveDupe)
import           System.Directory                 (getHomeDirectory)
import           System.Exit                      (exitFailure, exitSuccess)
import           System.FilePath                  ((</>))
import           System.IO
import           Text.Regex.TDFA                  ((=~))

import           Language.Egison
import           Language.Egison.Completion

import           Options.Applicative

main :: IO ()
main = execParser cmdParser >>= runWithOptions

isInValidMathOption :: EgisonOpts -> Bool
isInValidMathOption EgisonOpts{ optMathExpr = Just lang } = lang `notElem` ["asciimath", "latex", "mathematica", "maxima", "haskell"]
isInValidMathOption EgisonOpts{ optMathExpr = Nothing }   = False

runWithOptions :: EgisonOpts -> IO ()
runWithOptions opts | isInValidMathOption opts =
  hPrint stderr (Default "this output lang is not supported") >> exitFailure
runWithOptions EgisonOpts{ optShowVersion = True } =
  putStrLn (showVersion version) >> exitSuccess
runWithOptions opts = evalRuntimeT opts run

run :: RuntimeM ()
run = do
  opts <- ask
  -- Collect all files to load (core libs, user libs, load files, and test files)
  -- Load core libraries first unless --no-prelude is set
  isNoPrelude <- asks optNoPrelude
  mathNormalize <- asks optMathNormalize
  let coreLibExprs = if isNoPrelude then [] else map Load coreLibraries
      -- Add math normalization library based on option
      mathLibExpr = if isNoPrelude
                      then []
                      else [Load (if mathNormalize
                                    then "lib/math/normalize.egi"
                                    else "lib/math/no-normalize.egi")]
      libExprs = map Load (optLoadLibs opts)
      loadFileExprs = map LoadFile (optLoadFiles opts)
      -- Include test file in the initial load to preserve type class environment
      testFileExprs = case (optTestOnly opts, optExecFile opts) of
        (True, Just (file, _)) -> [LoadFile file]
        _                      -> []
      -- Load core libraries first, then math library, then user libraries and files
      allLoadExprs = coreLibExprs ++ mathLibExpr ++ libExprs ++ loadFileExprs ++ testFileExprs
  -- Load all libraries and user files in a single EvalM context to preserve EvalState
  mResult <- fromEvalTWithState initialEvalState $ do
    env <- initialEnv  -- Only primitive environment
    evalTopExprs' env allLoadExprs True True
  case mResult of
    Left err  -> liftIO $ print err
    Right (env, evalState) -> handleOptionWithState env evalState opts

handleOption :: Env -> EgisonOpts -> RuntimeM ()
handleOption env opts = handleOptionWithState env initialEvalState opts

handleOptionWithState :: Env -> EvalState -> EgisonOpts -> RuntimeM ()
handleOptionWithState env evalState opts =
  case opts of
    -- Evaluate the given string
    EgisonOpts { optEvalString = Just expr } ->
      runAndPrintExprWithState env evalState expr
    -- Execute the given string
    EgisonOpts { optExecuteString = Just cmd } ->
      executeTopExprWithState env evalState $ "execute (" ++ cmd ++ ")"
    -- Operate input in tsv format as infinite stream
    EgisonOpts { optSubstituteString = Just sub } ->
      let (sopts, copts) = unzip (optFieldInfo opts)
          sopts' = "[" ++ intercalate ", " sopts ++ "]"
          copts' = "[" ++ intercalate ", " copts ++ "]"
          expr = "load \"lib/core/shell.egi\"\n"
              ++ "execute (let SH.input := SH.genInput " ++ sopts' ++ " " ++ copts' ++ "\n"
              ++ if optTsvOutput opts then "          in each (\\x -> print (showTsv x)) ((" ++ sub ++ ") SH.input))"
                                      else "          in each (\\x -> print (show x)) ((" ++ sub ++ ") SH.input))"
        in executeTopExprWithState env evalState expr
    -- Execute a script (test only)
    -- Note: The test file has already been loaded in the run function
    -- to preserve the type class environment from library files.
    -- Here we just need to evaluate the test expressions.
    EgisonOpts { optTestOnly = True, optExecFile = Just (_file, _) } -> do
      -- Test file has already been loaded, nothing more to do
      return ()
    -- Execute a script from the main function
    EgisonOpts { optExecFile = Just (file, args) } -> do
      result <- fromEvalT $ evalTopExprs env [LoadFile file, Execute (makeApply "main" [CollectionExpr (map (ConstantExpr . StringExpr . T.pack) args)])]
      liftIO $ either print (const $ return ()) result
    EgisonOpts { optMapTsvInput = Just expr } ->
      handleOption env (opts { optSubstituteString = Just $ "\\x -> map (" ++ expr ++ ") x" })
    EgisonOpts { optFilterTsvInput = Just expr } ->
      handleOption env (opts { optSubstituteString = Just $ "\\x -> filter (" ++ expr ++ ") x" })
    -- Start the read-eval-print-loop
    _ -> do
      when (optShowBanner opts) (liftIO showBanner)
      replWithState env evalState
      when (optShowBanner opts) (liftIO showByebyeMessage)
      liftIO exitSuccess

runAndPrintExpr :: Env -> String -> RuntimeM ()
runAndPrintExpr env expr = do
  isTsvOutput <- asks optTsvOutput
  if isTsvOutput
     then executeTopExpr env $ "execute (each (\\x -> print (showTsv x)) (" ++ expr ++ "))"
     else executeTopExpr env $ "execute (print (show (" ++ expr ++ ")))"

runAndPrintExprWithState :: Env -> EvalState -> String -> RuntimeM ()
runAndPrintExprWithState env evalState expr = do
  isTsvOutput <- asks optTsvOutput
  if isTsvOutput
     then executeTopExprWithState env evalState $ "execute (each (\\x -> print (showTsv x)) (" ++ expr ++ "))"
     else executeTopExprWithState env evalState $ "execute (print (show (" ++ expr ++ ")))"

executeTopExpr :: Env -> String -> RuntimeM ()
executeTopExpr env expr = do
  cmdRet <- fromEvalT (runTopExprs env expr)
  case cmdRet of
    Left err -> liftIO $ hPrint stderr err >> exitFailure
    _        -> liftIO exitSuccess

executeTopExprWithState :: Env -> EvalState -> String -> RuntimeM ()
executeTopExprWithState env evalState expr = do
  cmdRet <- fromEvalTWithState evalState (runTopExprs env expr)
  case cmdRet of
    Left err -> liftIO $ hPrint stderr err >> exitFailure
    Right _ -> liftIO exitSuccess

showBanner :: IO ()
showBanner = do
  putStrLn $ "Egison Version " ++ showVersion version
  putStrLn   "https://www.egison.org"
  putStrLn   "Welcome to Egison Interpreter!"
--  putStrLn $ "** Information **"
--  putStrLn $ "We can use the tab key to complete keywords on the interpreter."
--  putStrLn $ "If we press the tab key after a closed parenthesis, the next closed parenthesis will be completed."
--  putStrLn $ "*****************"

showByebyeMessage :: IO ()
showByebyeMessage = putStrLn "Leaving Egison Interpreter."

settings :: MonadIO m => FilePath -> Env -> Settings m
settings home env =
  Settings { complete       = completeEgison env
           , historyFile    = Just (home </> ".egison_history")
           , autoAddHistory = False
           }

repl :: Env -> RuntimeM ()
repl env = replWithState env initialEvalState

replWithState :: Env -> EvalState -> RuntimeM ()
replWithState env evalState = (do
  home <- liftIO getHomeDirectory
  input <- runInputT (settings home env) (getReplInput env)
  case input of
    Nothing -> return ()
    Just (ReplExpr topExpr) -> do
      result <- fromEvalTWithState evalState (evalTopExprStr env topExpr)
      case result of
        Left err -> liftIO (print err) >> replWithState env evalState
        Right ((Just str, env'), evalState') -> liftIO (putStrLn str) >> replWithState env' evalState'
        Right ((Nothing, env'), evalState')  -> replWithState env' evalState'
    Just (ReplTypeStr exprStr) -> do
      -- Parse and type check the expression
      -- Note: This feature is temporarily disabled due to refactoring.
      -- TODO: Re-implement using IInfer pipeline.
      liftIO $ putStrLn $ "Type checking in REPL is temporarily disabled during refactoring."
      liftIO $ putStrLn $ "Expression: " ++ exprStr
      replWithState env evalState
    Just ReplHelp -> do
      liftIO showReplHelp
      replWithState env evalState
    Just ReplQuit -> return ()
  )
  `catch`
  (\case
      UserInterrupt -> liftIO (putStrLn "") >> replWithState env evalState
      StackOverflow -> liftIO (putStrLn "Stack over flow!") >> replWithState env evalState
      HeapOverflow  -> liftIO (putStrLn "Heap over flow!") >> replWithState env evalState
      _             -> liftIO (putStrLn "error!") >> replWithState env evalState
   )

-- | REPL input types
data ReplInput
  = ReplExpr TopExpr      -- ^ Regular expression to evaluate
  | ReplTypeStr String    -- ^ :type command with expression string
  | ReplHelp              -- ^ :help command
  | ReplQuit              -- ^ :quit command

-- | Show REPL help
showReplHelp :: IO ()
showReplHelp = do
  putStrLn "REPL Commands:"
  putStrLn "  :type <expr>  - Show the type of an expression"
  putStrLn "  :t <expr>     - Short for :type"
  putStrLn "  :help         - Show this help"
  putStrLn "  :h            - Short for :help"
  putStrLn "  :quit         - Exit the REPL"
  putStrLn "  :q            - Short for :quit"

-- |Get REPL input from the prompt. We can handle multiline input and special commands.
getReplInput :: Env -> InputT RuntimeM (Maybe ReplInput)
getReplInput env = getReplInput' ""
  where
    getReplInput' prev = do
      opts <- lift ask
      mLine <- case prev of
                 "" -> getInputLine $ optPrompt opts
                 _  -> getInputLine $ replicate (length $ optPrompt opts) ' '
      case mLine of
        Nothing -> return Nothing
        Just [] | null prev -> getReplInput env
        Just [] -> getReplInput' prev
        Just line -> do
          history <- getHistory
          putHistory $ addHistoryUnlessConsecutiveDupe line history
          let input = prev ++ line
          -- Check for special commands
          case parseReplCommand input of
            Just cmd -> return $ Just cmd
            Nothing -> do
              parsedExpr <- lift $ parseTopExpr (replaceNewLine input)
              case parsedExpr of
                Left err | err =~ "unexpected end of input" ->
                  getReplInput' (input ++ "\n")
                Left err -> do
                  liftIO $ putStrLn ("Parse error at: " ++ err)
                  getReplInput env
                Right topExpr ->
                  return $ Just (ReplExpr topExpr)

-- | Parse REPL special commands
parseReplCommand :: String -> Maybe ReplInput
parseReplCommand input = case words input of
  [":quit"]     -> Just ReplQuit
  [":q"]        -> Just ReplQuit
  [":help"]     -> Just ReplHelp
  [":h"]        -> Just ReplHelp
  (":type":rest) -> parseTypeCommand (unwords rest)
  (":t":rest)    -> parseTypeCommand (unwords rest)
  _             -> Nothing

-- | Parse :type command (returns expression string to be parsed later)
parseTypeCommand :: String -> Maybe ReplInput
parseTypeCommand exprStr
  | null exprStr = Nothing
  | otherwise    = Just $ ReplTypeStr exprStr

replaceNewLine :: String -> String
replaceNewLine input =
  let (before, _, after) = input =~ "#newline" :: (String, String, String) in
    case after of
      "" -> before
      _ -> before ++ "\n" ++ replaceNewLine after
