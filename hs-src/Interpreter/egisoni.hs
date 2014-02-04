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
      setComplete completeParen $ defaultSettings { historyFile = Just (home </> ".egison_history") }
    
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

completeParen :: Monad m => CompletionFunc m
completeParen arg@((')':_), _) = completeParen' arg
completeParen arg@(('>':_), _) = completeParen' arg
completeParen arg@((']':_), _) = completeParen' arg
completeParen arg@(('}':_), _) = completeParen' arg
completeParen arg@(('(':_), _) = (completeWord Nothing " \t<>[]{}$," completeAfterOpenParen) arg
completeParen arg@(('<':_), _) = (completeWord Nothing " \t()[]{}$," completeAfterOpenCons) arg
completeParen arg@((' ':_), _) = (completeWord Nothing "" completeNothing) arg
completeParen arg@([], _) = (completeWord Nothing "" completeNothing) arg
completeParen arg@(_, _) = (completeWord Nothing " \t[]{}$," completeEgisonKeyword) arg

completeAfterOpenParen :: Monad m => String -> m [Completion]
completeAfterOpenParen str = return $ map (\kwd -> Completion kwd kwd False) $ filter (isPrefixOf str) egisonKeywordsAfterOpenParen

completeAfterOpenCons :: Monad m => String -> m [Completion]
completeAfterOpenCons str = return $ map (\kwd -> Completion kwd kwd False) $ filter (isPrefixOf str) egisonKeywordsAfterOpenCons

completeNothing :: Monad m => String -> m [Completion]
completeNothing _ = return []

completeEgisonKeyword :: Monad m => String -> m [Completion]
completeEgisonKeyword str = return $ map (\kwd -> Completion kwd kwd False) $ filter (isPrefixOf str) egisonKeywords

egisonKeywordsAfterOpenParen = map ((:) '(') $ ["define", "let", "letrec", "do", "lambda", "match-lambda", "match", "match-all", "pattern-function", "matcher", "algebraic-data-matcher", "if", "loop", "io"]
                            ++ ["id", "or", "and", "not", "char", "eq?/m", "compose", "compose3", "list", "map", "between", "repeat1", "repeat", "filter", "separate", "concat", "foldr", "foldl", "map2", "zip", "empty?", "member?", "member?/m", "include?", "include?/m", "any", "all", "length", "count", "count/m", "car", "cdr", "rac", "rdc", "nth", "take", "drop", "while", "reverse", "multiset", "add", "add/m", "delete-first", "delete-first/m", "delete", "delete/m", "difference", "difference/m", "union", "union/m", "intersect", "intersect/m", "set", "unique", "unique/m", "simple-select", "print", "print-to-port", "each", "pure-rand", "fib", "fact", "divisor?", "gcd", "primes", "find-factor", "prime-factorization", "p-f", "pfs", "pfs-n", "min", "max", "min-and-max", "power", "mod", "float", "ordering", "qsort", "intersperse", "intercalate", "split", "split/m"]
egisonKeywordsAfterOpenCons = map ((:) '<') ["nil", "cons", "join", "snoc", "nioj"]
egisonKeywordsInNeutral = ["something"]
                       ++ ["bool", "string", "integer", "nat", "nats", "nats0"]
egisonKeywords = egisonKeywordsAfterOpenParen ++ egisonKeywordsAfterOpenCons ++ egisonKeywordsInNeutral

completeParen' :: Monad m => CompletionFunc m
completeParen' (lstr, _) = case (closeParen lstr) of
                             Nothing -> return (lstr, [])
                             Just paren -> return (lstr, [(Completion paren paren False)])

closeParen :: String -> Maybe String
closeParen str = closeParen' 0 $ removeCharAndStringLiteral str

removeCharAndStringLiteral :: String -> String
removeCharAndStringLiteral [] = []
removeCharAndStringLiteral ('"':'\\':str) = '"':'\\':(removeCharAndStringLiteral str)
removeCharAndStringLiteral ('"':str) = removeCharAndStringLiteral' str
removeCharAndStringLiteral ('\'':'\\':str) = '\'':'\\':(removeCharAndStringLiteral str)
removeCharAndStringLiteral ('\'':str) = removeCharAndStringLiteral' str
removeCharAndStringLiteral (c:str) = c:(removeCharAndStringLiteral str)

removeCharAndStringLiteral' :: String -> String
removeCharAndStringLiteral' [] = []
removeCharAndStringLiteral' ('"':'\\':str) = removeCharAndStringLiteral' str
removeCharAndStringLiteral' ('"':str) = removeCharAndStringLiteral str
removeCharAndStringLiteral' ('\'':'\\':str) = removeCharAndStringLiteral' str
removeCharAndStringLiteral' ('\'':str) = removeCharAndStringLiteral str
removeCharAndStringLiteral' (_:str) = removeCharAndStringLiteral' str

closeParen' :: Integer -> String -> Maybe String
closeParen' _ [] = Nothing
closeParen' 0 ('(':_) = Just ")"
closeParen' 0 ('<':_) = Just ">"
closeParen' 0 ('[':_) = Just "]"
closeParen' 0 ('{':_) = Just "}"
closeParen' n ('(':str) = closeParen' (n - 1) str
closeParen' n ('<':str) = closeParen' (n - 1) str
closeParen' n ('[':str) = closeParen' (n - 1) str
closeParen' n ('{':str) = closeParen' (n - 1) str
closeParen' n (')':str) = closeParen' (n + 1) str
closeParen' n ('>':str) = closeParen' (n + 1) str
closeParen' n (']':str) = closeParen' (n + 1) str
closeParen' n ('}':str) = closeParen' (n + 1) str
closeParen' n (_:str) = closeParen' n str
