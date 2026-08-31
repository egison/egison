{- |
Module      : Language.Egison.CmdOptions
Licence     : MIT

This module provides command line options of Egison interpreter.
-}

module Language.Egison.CmdOptions
  ( EgisonOpts (..)
  , defaultOption
  , cmdParser
  ) where

import           Data.Functor        (($>))
import           Data.List           (intercalate)
import           Data.Maybe          (maybeToList)
import           Options.Applicative
import qualified Text.Parsec         as P

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
    optNoPrelude        :: Bool,       -- ^ Do not load core libraries
    optShowBanner       :: Bool,
    optTestOnly         :: Bool,
    optPrompt           :: String,
    optMathValue         :: Maybe String,
    optMathNormalize    :: Bool,
    optTypeCheck        :: Bool,       -- ^ Enable type checking
    optTypeCheckStrict  :: Bool,       -- ^ Strict type checking mode
    optDumpEnv          :: Bool,       -- ^ Dump environment after Phase 2
    optDumpDesugared    :: Bool,       -- ^ Dump desugared AST after Phase 3
    optDumpTyped        :: Bool,       -- ^ Dump typed AST after Phase 6 (type inference & check)
    optDumpTi           :: Bool,       -- ^ Dump typed AST after TensorMap insertion (before type class expansion)
    optDumpTc           :: Bool,       -- ^ Dump typed AST after type class expansion (Phase 7 complete)
    optMatcherConsistencyWarnings :: Bool,       -- ^ Emit matcher Coverage warnings (paper Def 4.2(3))
    optOutsideEgisonCoreWarnings :: Bool,         -- ^ Warn when checking proceeds through an extension outside Egison core
    optPatternHoleBeforePrimitiveValuePatternWarnings :: Bool,
                                                   -- ^ Warn when a primitive-pattern hole precedes a primitive value pattern
    optNestedStructuredPrimitivePatternPatternWarnings :: Bool,
                                                   -- ^ Warn about nested structured primitive-pattern patterns
    optMatchWithoutElseWarnings :: Bool,            -- ^ Warn when match/matchDFS omits else
    optTypePMMetrics    :: Bool                      -- ^ Report TypePM inference counters
    }

defaultOption :: EgisonOpts
defaultOption = EgisonOpts
  { optExecFile = Nothing
  , optShowVersion = False
  , optEvalString = Nothing
  , optExecuteString = Nothing
  , optFieldInfo = []
  , optLoadLibs = []
  , optLoadFiles = []
  , optSubstituteString = Nothing
  , optMapTsvInput = Nothing
  , optFilterTsvInput = Nothing
  , optTsvOutput = False
  , optNoIO = False
  , optNoPrelude = False
  , optShowBanner = True
  , optTestOnly = False
  , optPrompt = "> "
  , optMathValue = Nothing
  , optMathNormalize = True
  , optTypeCheck = True
  , optTypeCheckStrict = False
  , optDumpEnv = False
  , optDumpDesugared = False
  , optDumpTyped = False
  , optDumpTi = False
  , optDumpTc = False
  , optMatcherConsistencyWarnings = False
  , optOutsideEgisonCoreWarnings = False
  , optPatternHoleBeforePrimitiveValuePatternWarnings = False
  , optNestedStructuredPrimitivePatternPatternWarnings = False
  , optMatchWithoutElseWarnings = False
  , optTypePMMetrics = False
  }

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
            <*> many (option readFieldOption
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
            <*> optional (strOption
                  (short 'm'
                  <> long "map"
                  <> metavar "EXPR"
                  <> help "Operate input in tsv format line by line"))
            <*> optional (strOption
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
            <*> switch
                  (long "no-prelude"
                  <> help "Do not load core libraries")
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
                  (long "no-normalize"
                  <> help "Turn off normalization of math expressions")
            <*> pure True  -- Type checking is always enabled
            <*> switch
                  (long "type-check-strict"
                  <> help "Strict type checking (all types must be known)")
            <*> switch
                  (long "dump-env"
                  <> help "Dump environment information after Phase 2 (environment building)")
            <*> switch
                  (long "dump-desugared"
                  <> help "Dump desugared AST after Phase 3 (desugaring)")
            <*> switch
                  (long "dump-typed"
                  <> help "Dump typed AST after Phase 6 (type inference & check)")
            <*> switch
                  (long "dump-ti"
                  <> help "Dump typed AST after TensorMap insertion (before type class expansion)")
            <*> switch
                  (long "dump-tc"
                  <> help "Dump typed AST after type class expansion (Phase 7 complete)")
            <*> switch
                  (long "matcher-consistency-warnings"
                  <> help "Emit matcher Coverage warnings (paper Def 4.2(3)): a matcher lacking a general clause for some pattern constructor of its matched type. PP-Con (4.2(1a)) and arm exhaustiveness (4.2(1c)) are ordinary type errors, not gated by this flag")
            <*> switch
                  (long "outside-egison-core-warnings"
                  <> help "Warn when type checking proceeds through an extension outside Egison core")
            <*> switch
                  (long "pattern-hole-before-primitive-value-pattern-warnings"
                  <> help "Warn when a matcher clause's primitive-pattern pattern has a pattern hole before a primitive value pattern in depth-first, left-to-right order")
            <*> switch
                  (long "nested-structured-primitive-pattern-pattern-warnings"
                  <> help "Warn when a matcher clause contains a nested structured primitive-pattern pattern")
            <*> switch
                  (long "match-without-else-warnings"
                  <> help "Warn when match or matchDFS omits its optional final else branch")
            <*> switch
                  (long "type-pm-metrics"
                  <> help "Report explicit eager slot boundaries and multi-demand capability combinations")

readFieldOption :: ReadM (String, String)
readFieldOption = eitherReader $ \str ->
  case P.parse parseFieldOption "(argument)" str of
    Left err -> Left $ show err
    Right ok -> Right ok

parseFieldOption :: P.Parsec String () (String, String)
parseFieldOption = do
  s <- P.many1 P.digit
  e <- P.optionMaybe (P.char ',' >> P.many1 P.digit)
  let se = s : maybeToList e
  (rs, rc)
    <-  P.try (P.string "sc") $> (se, se)
    <|> P.try (P.string "cs") $> (se, se)
    <|> P.try (P.string "s" ) $> (se, [])
    <|> P.try (P.string "c" ) $> ([], se)
  P.eof
  let f x = "[" ++ intercalate ", " x ++ "]"
  return (f rs, f rc)
