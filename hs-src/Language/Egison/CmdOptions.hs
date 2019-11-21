{- |
Module      : Language.Egison.CmdOptions
Copyright   : Satoshi Egi
Licence     : MIT

This module provides command line options of Egison interpreter.
-}

module Language.Egison.CmdOptions
  ( EgisonOpts (..)
  , defaultOption
  , cmdParser
  ) where

import           Data.Char           (isDigit)
import           Options.Applicative

data EgisonOpts = EgisonOpts {
    optExecFile         :: Maybe (String, [String]),
    optShowVersion      :: Bool,
    optEvalString       :: Maybe String,
    optExecuteString    :: Maybe String,
    optFieldInfo        :: [(String, String)],
    optLoadLibs         :: [String],
    optLoadFiles        :: [String],
    optSubstituteString :: Maybe String,
    optMapTsvInput      :: Maybe String,
    optFilterTsvInput   :: Maybe String,
    optTsvOutput        :: Bool,
    optNoIO             :: Bool,
    optShowBanner       :: Bool,
    optTestOnly         :: Bool,
    optPrompt           :: String,
    optMathExpr         :: Maybe String,
    optSExpr            :: Bool
    }

defaultOption :: EgisonOpts
defaultOption = EgisonOpts Nothing False Nothing Nothing [] [] [] Nothing Nothing Nothing False False True False "> " Nothing True

cmdParser :: ParserInfo EgisonOpts
cmdParser = info (helper <*> cmdArgParser)
          $ fullDesc
          <> header "The Egison Programming Language"

cmdArgParser :: Parser EgisonOpts
cmdArgParser = EgisonOpts
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
                  <> help "[experimental] Use non-S expression syntax")

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
