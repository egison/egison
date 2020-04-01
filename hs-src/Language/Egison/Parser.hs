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
       -- * Parse and desugar a file
       , loadLibraryFile
       , loadFile
       ) where
 
import           Control.Monad.Except           (liftIO, throwError)
import           Control.Monad.State            (unless)

import           System.Directory               (doesFileExist, getHomeDirectory)
import           System.IO

import           Language.Egison.AST
import           Language.Egison.Desugar
import           Language.Egison.Data
import qualified Language.Egison.Parser.SExpr   as SExpr
import qualified Language.Egison.Parser.NonS    as NonS
import           Paths_egison                   (getDataFileName)

readTopExprs :: Bool -> String -> EgisonM [EgisonTopExpr]
readTopExprs useSExpr =
  either throwError (mapM desugarTopExpr) . parseTopExprs
    where parseTopExprs | useSExpr  = SExpr.parseTopExprs
                        | otherwise = NonS.parseTopExprs

-- TODO(momohatt): Parse from the last state
readTopExpr :: Bool -> String -> EgisonM EgisonTopExpr
readTopExpr useSExpr =
  either throwError desugarTopExpr . parseTopExpr
    where parseTopExpr | useSExpr  = SExpr.parseTopExpr
                       | otherwise = NonS.parseTopExpr

readExprs :: Bool -> String -> EgisonM [EgisonExpr]
readExprs useSExpr =
  either throwError (mapM desugarExpr) . parseExprs
    where parseExprs | useSExpr  = SExpr.parseExprs
                     | otherwise = NonS.parseExprs

readExpr :: Bool -> String -> EgisonM EgisonExpr
readExpr useSExpr =
  either throwError desugarExpr . parseExpr
    where parseExpr | useSExpr  = SExpr.parseExpr
                    | otherwise = NonS.parseExpr

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
  useSExpr <- checkIfUseSExpr file
  exprs <- readTopExprs useSExpr $ shebang input
  concat <$> mapM recursiveLoad exprs
 where
  recursiveLoad (Load file)     = loadLibraryFile file
  recursiveLoad (LoadFile file) = loadFile file
  recursiveLoad expr            = return [expr]
  shebang :: String -> String
  shebang ('#':'!':cs) = ';':'#':'!':cs
  shebang cs           = cs

readUTF8File :: FilePath -> IO String
readUTF8File name = do
  h <- openFile name ReadMode
  hSetEncoding h utf8
  hGetContents h

hasDotEgiExtension :: String -> Bool
hasDotEgiExtension file = drop (length file - 4) file == ".egi"

hasDotSEgiExtension :: String -> Bool
hasDotSEgiExtension file = drop (length file - 5) file == ".segi"

checkIfUseSExpr :: String -> EgisonM Bool
checkIfUseSExpr file
  | hasDotEgiExtension file  = return False
  | hasDotSEgiExtension file = return True
  | otherwise                = throwError (UnknownFileExtension file)
