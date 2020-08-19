{- |
Module      : Language.Egison.Parser
Licence     : MIT

This module provides the parser interface.
-}

module Language.Egison.Parser
       (
       -- * Parse and desugar
         readTopExprs
       , readTopExpr
       , readExprs
       , readExpr
       , parseTopExpr
       -- * Parse and desugar a file
       , loadLibraryFile
       , loadFile
       -- * Parser utils (for translator)
       , removeShebang
       , readUTF8File
       ) where
 
import           Control.Monad.Except           (lift, liftIO, throwError)
import           Control.Monad.State            (unless)

import           System.Directory               (doesFileExist, getHomeDirectory)
import           System.IO

import           Language.Egison.AST
import           Language.Egison.Desugar
import           Language.Egison.Data
import           Language.Egison.RState
import qualified Language.Egison.Parser.SExpr   as SExpr
import qualified Language.Egison.Parser.NonS    as NonS
import           Paths_egison                   (getDataFileName)

readTopExprs :: Bool -> String -> EvalM [TopExpr]
readTopExprs useSExpr expr | useSExpr =
  either throwError (mapM desugarTopExpr) (SExpr.parseTopExprs expr)
readTopExprs _ expr = do
  r <- lift . lift $ NonS.parseTopExprs expr
  either throwError (mapM desugarTopExpr) r

parseTopExpr :: Bool -> String -> RuntimeM (Either EgisonError TopExpr)
parseTopExpr useSExpr expr | useSExpr = return $ SExpr.parseTopExpr expr
parseTopExpr _ expr = NonS.parseTopExpr expr

readTopExpr :: Bool -> String -> EvalM TopExpr
readTopExpr useSExpr expr | useSExpr =
  either throwError desugarTopExpr (SExpr.parseTopExpr expr)
readTopExpr _ expr = do
  r <- lift . lift $ NonS.parseTopExpr expr
  either throwError desugarTopExpr r

readExprs :: Bool -> String -> EvalM [EgisonExpr]
readExprs useSExpr expr | useSExpr =
  either throwError (mapM desugarExpr) (SExpr.parseExprs expr)
readExprs _ expr = do
  r <- lift . lift $ NonS.parseExprs expr
  either throwError (mapM desugarExpr) r

readExpr :: Bool -> String -> EvalM EgisonExpr
readExpr useSExpr expr | useSExpr =
  either throwError desugarExpr (SExpr.parseExpr expr)
readExpr _ expr = do
  r <- lift . lift $ NonS.parseExpr expr
  either throwError desugarExpr r

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
  useSExpr <- checkIfUseSExpr file
  exprs <- readTopExprs useSExpr $ removeShebang useSExpr input
  concat <$> mapM recursiveLoad exprs
 where
  recursiveLoad (Load file)     = loadLibraryFile file
  recursiveLoad (LoadFile file) = loadFile file
  recursiveLoad expr            = return [expr]

removeShebang :: Bool -> String -> String
removeShebang useSExpr cs@('#':'!':_) = if useSExpr then ';' : cs else "--" ++ cs
removeShebang _        cs             = cs

readUTF8File :: FilePath -> IO String
readUTF8File name = do
  h <- openFile name ReadMode
  hSetEncoding h utf8
  hGetContents h

hasDotEgiExtension :: String -> Bool
hasDotEgiExtension file = drop (length file - 4) file == ".egi"

hasDotSEgiExtension :: String -> Bool
hasDotSEgiExtension file = drop (length file - 5) file == ".segi"

checkIfUseSExpr :: String -> EvalM Bool
checkIfUseSExpr file
  | hasDotEgiExtension file  = return False
  | hasDotSEgiExtension file = return True
  | otherwise                = throwError (UnknownFileExtension file)
