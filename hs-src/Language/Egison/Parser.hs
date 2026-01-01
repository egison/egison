{- |
Module      : Language.Egison.Parser
Licence     : MIT

This module provides the parser interface.
-}

module Language.Egison.Parser
       (
       -- * Parse
         readTopExprs
       , readTopExpr
       , readExprs
       , readExpr
       , parseTopExpr
       -- * Parse a file
       , loadLibraryFile
       , loadFile
       -- * Parser utils (for translator)
       , removeShebang
       , readUTF8File
       ) where

import           Control.Monad                 (unless)
import           Control.Monad.Except         (throwError)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)

import           System.Directory             (doesFileExist, getHomeDirectory)
import           System.IO

import           Language.Egison.AST
import           Language.Egison.Data
import qualified Language.Egison.Parser.NonS  as NonS
import           Language.Egison.RState
import           Paths_egison                 (getDataFileName)

readTopExprs :: String -> EvalM [TopExpr]
readTopExprs expr = do
  r <- lift . lift $ NonS.parseTopExprs expr
  either (throwError . Parser) return r

parseTopExpr :: String -> RuntimeM (Either String TopExpr)
parseTopExpr = NonS.parseTopExpr

readTopExpr :: String -> EvalM TopExpr
readTopExpr expr = do
  r <- lift . lift $ NonS.parseTopExpr expr
  either (throwError . Parser) return r

readExprs :: String -> EvalM [Expr]
readExprs expr = do
  r <- lift . lift $ NonS.parseExprs expr
  either (throwError . Parser) return r

readExpr :: String -> EvalM Expr
readExpr expr = do
  r <- lift . lift $ NonS.parseExpr expr
  either (throwError . Parser) return r

-- |Load a libary file
loadLibraryFile :: FilePath -> EvalM [TopExpr]
loadLibraryFile file = do
  homeDir <- liftIO getHomeDirectory
  doesExist <- liftIO $ doesFileExist $ homeDir ++ "/.egison/" ++ file
  if doesExist
    then loadFile $ homeDir ++ "/.egison/" ++ file
    else liftIO (getDataFileName file) >>= loadFile

-- |Load a file
loadFile :: FilePath -> EvalM [TopExpr]
loadFile file = do
  doesExist <- liftIO $ doesFileExist file
  unless doesExist $ throwError $ Default ("file does not exist: " ++ file)
  input <- liftIO $ readUTF8File file
  exprs <- readTopExprs (removeShebang input)
  concat <$> mapM recursiveLoad exprs
 where
  recursiveLoad (Load file')     = loadLibraryFile file'
  recursiveLoad (LoadFile file') = loadFile file'
  recursiveLoad expr             = return [expr]

removeShebang :: String -> String
removeShebang cs@('#':'!':_) = "--" ++ cs
removeShebang cs             = cs

readUTF8File :: FilePath -> IO String
readUTF8File name = do
  h <- openFile name ReadMode
  hSetEncoding h utf8
  hGetContents h
