{
{-# OPTIONS -w #-}

{- |
Module      : Language.Egison.ParserNonS2
Copyright   : Satoshi Egi
Licence     : MIT

This module provides new Egison parser.
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

import           Control.Monad.Except    hiding (mapM)

import           Data.Either
import qualified Data.Set                as Set

import           System.Directory        (doesFileExist, getHomeDirectory)

import           Language.Egison.Desugar
import           Language.Egison.Lexer
import           Language.Egison.Types
import           Paths_egison            (getDataFileName)

}

%name parseTopExprs_ TopExprs
%name parseTopExpr_ TopExpr
%name parseExprs_ Exprs
%name parseExpr_ Expr
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
-- Without this we get a type error
%error { happyError }

%left '<' '>' "==" "<=" ">="
%left '+' '-'
%left '*' '/'

%token
      int      { Token _ (TokenInt $$) }
      var      { Token _ (TokenVar $$) }
      "#t"     { Token _ TokenTrue }
      "#f"     { Token _ TokenFalse }

      test     { Token _ TokenTest }

      "=="     { Token _ TokenEq }
      '<'      { Token _ TokenLT }
      '>'      { Token _ TokenGT }
      "<="     { Token _ TokenLE }
      ">="     { Token _ TokenGE }
      '+'      { Token _ TokenPlus }
      '-'      { Token _ TokenMinus }
      '*'      { Token _ TokenAsterisk }
      '/'      { Token _ TokenDiv }

      '('      { Token _ TokenLParen }
      ')'      { Token _ TokenRParen }
      '['      { Token _ TokenLBracket }
      ']'      { Token _ TokenRBracket }

%%

TopExprs :
    TopExpr           { [$1] }
  | TopExpr TopExprs  { $1 : $2 }

TopExpr :
    Expr              { Test $1 }
  | test '(' Expr ')' { Test $3 }

Exprs :
    Expr              { [$1] }
  | Expr Exprs        { $1 : $2 }

Expr :
    int               { IntegerExpr $1 }
  | "#t"              { BoolExpr True }
  | "#f"              { BoolExpr False }
  | Expr "==" Expr    { makeApply (VarExpr $ stringToVar "eq?") [$1, $3] }
  | Expr "<=" Expr    { makeApply (VarExpr $ stringToVar "lte?") [$1, $3] }
  | Expr '<' Expr     { makeApply (VarExpr $ stringToVar "lt?") [$1, $3] }
  | Expr ">=" Expr    { makeApply (VarExpr $ stringToVar "gte?") [$1, $3] }
  | Expr '>' Expr     { makeApply (VarExpr $ stringToVar "gt?") [$1, $3] }
  | Expr '+' Expr     { makeApply (VarExpr $ stringToVar "+") [$1, $3] }
  | Expr '-' Expr     { makeApply (VarExpr $ stringToVar "-") [$1, $3] }
  | Expr '*' Expr     { makeApply (VarExpr $ stringToVar "*") [$1, $3] }
  | Expr '/' Expr     { makeApply (VarExpr $ stringToVar "/") [$1, $3] }
  | '(' Expr ')'      { $2 }

{
makeApply :: EgisonExpr -> [EgisonExpr] -> EgisonExpr
makeApply func xs = do
  let args = map (\x -> case x of
                          LambdaArgExpr s -> Left s
                          _               -> Right x) xs
  let vars = lefts args
  case vars of
    [] -> ApplyExpr func . TupleExpr $ rights args
    _ | all null vars ->
        let args' = rights args
            args'' = zipWith (curry f) args (annonVars 1 (length args))
            args''' = map (VarExpr . stringToVar . either id id) args''
        in ApplyExpr (LambdaExpr (map ScalarArg (rights args'')) (LambdaExpr (map ScalarArg (lefts args'')) $ ApplyExpr func $ TupleExpr args''')) $ TupleExpr args'
      | all (not . null) vars ->
        let n = Set.size $ Set.fromList vars
            args' = rights args
            args'' = zipWith (curry g) args (annonVars (n + 1) (length args))
            args''' = map (VarExpr . stringToVar . either id id) args''
        in ApplyExpr (LambdaExpr (map ScalarArg (rights args'')) (LambdaExpr (map ScalarArg (annonVars 1 n)) $ ApplyExpr func $ TupleExpr args''')) $ TupleExpr args'
 where
  annonVars m n = take n $ map ((':':) . show) [m..]
  f (Left _, var)  = Left var
  f (Right _, var) = Right var
  g (Left arg, _)  = Left (':':arg)
  g (Right _, var) = Right var

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ show t ++ "'")

readTopExprs :: String -> EgisonM [EgisonTopExpr]
readTopExprs = either throwError (mapM desugarTopExpr) . parseTopExprs

readTopExpr :: String -> EgisonM EgisonTopExpr
readTopExpr = either throwError desugarTopExpr . parseTopExpr

readExprs :: String -> EgisonM [EgisonExpr]
readExprs = liftEgisonM . runDesugarM . either throwError (mapM desugar) . parseExprs

readExpr :: String -> EgisonM EgisonExpr
readExpr = liftEgisonM . runDesugarM . either throwError desugar . parseExpr

parseTopExprs :: String -> Either EgisonError [EgisonTopExpr]
parseTopExprs = runAlex' parseTopExprs_

parseTopExpr :: String -> Either EgisonError EgisonTopExpr
parseTopExpr = runAlex' parseTopExpr_

parseExprs :: String -> Either EgisonError [EgisonExpr]
parseExprs = runAlex' parseExprs_

parseExpr :: String -> Either EgisonError EgisonExpr
parseExpr = runAlex' parseExpr_

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
}
