{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

{- |
Module      : Language.Egison.ParserNonS
Copyright   : Satoshi Egi
Licence     : MIT

This module provide Egison parser.
-}

module Language.Egison.ParserNonS2
       (
       -- * Parse a string
         readTopExprs
       , readTopExpr
       , readExprs
       , readExpr
       , parseTopExprs
       , parseTopExpr
       , parseExprs
       , parseExpr
       -- * Parse a file
       , loadLibraryFile
       , loadFile
       ) where

import           Control.Applicative     (pure, (*>), (<$>), (<*), (<*>))
import           Control.Monad.Except    hiding (mapM)
import           Control.Monad.Identity  hiding (mapM)
import           Control.Monad.State     hiding (mapM)
import           Prelude                 hiding (mapM)

import           System.Directory        (doesFileExist, getHomeDirectory)

import           Data.Char               (isLower, isUpper, toLower)
import           Data.Either
import           Data.Functor            (($>))
import           Data.List               (intercalate)
import           Data.List.Split         (split, splitOn, startsWithOneOf)
import           Data.Ratio
import qualified Data.Sequence           as Sq
import qualified Data.Set                as Set
import           Data.Traversable        (mapM)

import           Control.Monad.Combinators.Expr
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Text               as T
import           Text.Regex.TDFA

import           Language.Egison.Desugar
import           Language.Egison.Types
import           Paths_egison            (getDataFileName)

readTopExprs :: String -> EgisonM [EgisonTopExpr]
readTopExprs = either throwError (mapM desugarTopExpr) . parseTopExprs

readTopExpr :: String -> EgisonM EgisonTopExpr
readTopExpr = either throwError desugarTopExpr . parseTopExpr

readExprs :: String -> EgisonM [EgisonExpr]
readExprs = liftEgisonM . runDesugarM . either throwError (mapM desugar) . parseExprs

readExpr :: String -> EgisonM EgisonExpr
readExpr = liftEgisonM . runDesugarM . either throwError desugar . parseExpr

parseTopExprs :: String -> Either EgisonError [EgisonTopExpr]
parseTopExprs = undefined

parseTopExpr :: String -> Either EgisonError EgisonTopExpr
parseTopExpr = doParse topExpr

parseExprs :: String -> Either EgisonError [EgisonExpr]
parseExprs = undefined

parseExpr :: String -> Either EgisonError EgisonExpr
parseExpr = doParse expr

-- |Load a libary file
loadLibraryFile :: FilePath -> EgisonM [EgisonTopExpr]
loadLibraryFile file = do
  homeDir <- liftIO getHomeDirectory
  doesExist <- liftIO $ doesFileExist $ homeDir ++ "/.egison/" ++ file
  if doesExist
    then loadFile $ homeDir ++ "/.egison/" ++ file
    else liftIO (getDataFileName file) >>= loadFile

-- |Load a file
loadFile :: FilePath -> EgisonM [EgisonTopExpr]
loadFile file = do
  doesExist <- liftIO $ doesFileExist file
  unless doesExist $ throwError $ Default ("file does not exist: " ++ file)
  input <- liftIO $ readUTF8File file
  exprs <- readTopExprs $ shebang input
  concat <$> mapM  recursiveLoad exprs
 where
  recursiveLoad (Load file)     = loadLibraryFile file
  recursiveLoad (LoadFile file) = loadFile file
  recursiveLoad expr            = return [expr]
  shebang :: String -> String
  shebang ('#':'!':cs) = ';':'#':'!':cs
  shebang cs           = cs

--
-- Parser
--

type Parser = Parsec Void String

doParse :: Parser a -> String -> Either EgisonError a
doParse p input = either (throwError . fromParsecError) return $ parse p "egison" input
  where
    fromParsecError :: ParseErrorBundle String Void -> EgisonError
    fromParsecError = Parser . errorBundlePretty

doParse' :: Parser a -> String -> a
doParse' p input = case doParse p input of
                     Right x -> x

--
-- Expressions
--

topExpr :: Parser EgisonTopExpr
topExpr = Test <$> expr

expr :: Parser EgisonExpr
expr = IntegerExpr <$> integerLiteral

--
-- Tokens
--

-- space comsumer
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockCommentNested "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")
semicolon = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."

integerLiteral :: Parser Integer
integerLiteral = lexeme L.decimal
             <|> L.signed sc (lexeme L.decimal)

reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

keywordDefine               = reserved "define"
keywordSet                  = reserved "set!"
keywordTest                 = reserved "test"
keywordLoadFile             = reserved "loadFile"
keywordLoad                 = reserved "load"
keywordIf                   = reserved "if"
keywordThen                 = reserved "then"
keywordElse                 = reserved "else"
keywordAs                   = reserved "as"
keywordSeq                  = reserved "seq"
keywordApply                = reserved "apply"
keywordCApply               = reserved "capply"
keywordLambda               = reserved "lambda"
keywordMemoizedLambda       = reserved "memoizedLambda"
keywordCambda               = reserved "cambda"
keywordProcedure            = reserved "procedure"
keywordMacro                = reserved "macro"
keywordPatternFunction      = reserved "patternFunction"
keywordLetRec               = reserved "letrec"
keywordLet                  = reserved "let"
keywordLetStar              = reserved "let*"
keywordLetIn                = reserved "in"
keywordWithSymbols          = reserved "withSymbols"
keywordLoop                 = reserved "loop"
keywordCont                 = reserved "..."
keywordMatchAll             = reserved "matchAll"
keywordMatchAllDFS          = reserved "matchAllDFS"
keywordMatchAllLambda       = reserved "matchAllLambda"
keywordMatch                = reserved "match"
keywordMatchLambda          = reserved "matchLambda"
keywordMatcher              = reserved "matcher"
keywordDo                   = reserved "do"
keywordIo                   = reserved "io"
keywordSomething            = reserved "something"
keywordUndefined            = reserved "undefined"
keywordAlgebraicDataMatcher = reserved "algebraicDataMatcher"
keywordGenerateTensor       = reserved "generateTensor"
keywordTensor               = reserved "tensor"
keywordTensorContract       = reserved "contract"
keywordSubrefs              = reserved "subrefs"
keywordSubrefsNew           = reserved "subrefs!"
keywordSuprefs              = reserved "suprefs"
keywordSuprefsNew           = reserved "suprefs!"
keywordUserrefs             = reserved "userRefs"
keywordUserrefsNew          = reserved "userRefs!"
keywordFunction             = reserved "function"

reservedWords = ["define"
               , "set!"
               , "test"
               , "loadFile"
               , "load"
               , "if"
               , "then"
               , "else"
               , "as"
               , "seq"
               , "apply"
               , "capply"
               , "lambda"
               , "memoizedLambda"
               , "cambda"
               , "procedure"
               , "macro"
               , "patternFunction"
               , "letrec"
               , "let"
               , "let*"
               , "in"
               , "withSymbols"
               , "loop"
               , "..."
               , "matchAll"
               , "matchAllDFS"
               , "matchAllLambda"
               , "match"
               , "matchLambda"
               , "matcher"
               , "do"
               , "io"
               , "something"
               , "undefined"
               , "algebraicDataMatcher"
               , "generateTensor"
               , "tensor"
               , "contract"
               , "subrefs"
               , "subrefs!"
               , "suprefs"
               , "suprefs!"
               , "userRefs"
               , "userRefs!"
               , "function"]
