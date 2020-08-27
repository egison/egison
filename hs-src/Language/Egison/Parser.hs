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
import           Control.Monad.Reader           (asks, local)

import           System.Directory               (doesFileExist, getHomeDirectory)
import           System.IO

import           Language.Egison.AST
import           Language.Egison.CmdOptions
import           Language.Egison.Desugar
import           Language.Egison.Data
import           Language.Egison.IExpr
import           Language.Egison.RState
import qualified Language.Egison.Parser.SExpr   as SExpr
import qualified Language.Egison.Parser.NonS    as NonS
import           Paths_egison                   (getDataFileName)

readTopExprs :: String -> EvalM [ITopExpr]
readTopExprs expr = do
  isSExpr <- asks optSExpr
  if isSExpr
     then either throwError (mapM desugarTopExpr) (SExpr.parseTopExprs expr)
     else do r <- lift . lift $ NonS.parseTopExprs expr
             either throwError (mapM desugarTopExpr) r

parseTopExpr :: String -> RuntimeM (Either EgisonError TopExpr)
parseTopExpr expr = do
  isSExpr <- asks optSExpr
  if isSExpr
     then return (SExpr.parseTopExpr expr)
     else NonS.parseTopExpr expr

readTopExpr :: String -> EvalM ITopExpr
readTopExpr expr = do
  isSExpr <- asks optSExpr
  if isSExpr
     then either throwError desugarTopExpr (SExpr.parseTopExpr expr)
     else do r <- lift . lift $ NonS.parseTopExpr expr
             either throwError desugarTopExpr r

readExprs :: String -> EvalM [IExpr]
readExprs expr = do
  isSExpr <- asks optSExpr
  if isSExpr
     then either throwError (mapM desugarExpr) (SExpr.parseExprs expr)
     else do r <- lift . lift $ NonS.parseExprs expr
             either throwError (mapM desugarExpr) r

readExpr :: String -> EvalM IExpr
readExpr expr = do
  isSExpr <- asks optSExpr
  if isSExpr
     then either throwError desugarExpr (SExpr.parseExpr expr)
     else do r <- lift . lift $ NonS.parseExpr expr
             either throwError desugarExpr r

-- |Load a libary file
loadLibraryFile :: FilePath -> EvalM [ITopExpr]
loadLibraryFile file = do
  homeDir <- liftIO getHomeDirectory
  doesExist <- liftIO $ doesFileExist $ homeDir ++ "/.egison/" ++ file
  if doesExist
    then loadFile $ homeDir ++ "/.egison/" ++ file
    else liftIO (getDataFileName file) >>= loadFile

-- |Load a file
loadFile :: FilePath -> EvalM [ITopExpr]
loadFile file = do
  doesExist <- liftIO $ doesFileExist file
  unless doesExist $ throwError $ Default ("file does not exist: " ++ file)
  input <- liftIO $ readUTF8File file
  let useSExpr = checkIfUseSExpr file
  exprs <- local (\opt -> opt { optSExpr = useSExpr })
                 (readTopExprs (removeShebang useSExpr input))
  concat <$> mapM recursiveLoad exprs
 where
  recursiveLoad (ILoad file)     = loadLibraryFile file
  recursiveLoad (ILoadFile file) = loadFile file
  recursiveLoad expr             = return [expr]

removeShebang :: Bool -> String -> String
removeShebang useSExpr cs@('#':'!':_) = if useSExpr then ';' : cs else "--" ++ cs
removeShebang _        cs             = cs

readUTF8File :: FilePath -> IO String
readUTF8File name = do
  h <- openFile name ReadMode
  hSetEncoding h utf8
  hGetContents h

checkIfUseSExpr :: String -> Bool
checkIfUseSExpr file = drop (length file - 5) file == ".segi"
