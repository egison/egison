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

-- %name parseTopExprs_ TopExprs
%name parseTopExpr_ TopExpr
%name parseExpr_ Expr
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
-- Without this we get a type error
%error { happyError }

%right '|'
%right "++"
%right "||"
%right "&&"
%right ':'
%left '<' '>' "==" "<=" ">="
%left '+' '-' '%'
%left '*' '/'
%left '^'

%token
      True        { Token _ TokenTrue        }
      False       { Token _ TokenFalse       }
      test        { Token _ TokenTest        }
      match       { Token _ TokenMatch       }
      matchDFS    { Token _ TokenMatchDFS    }
      matchAll    { Token _ TokenMatchAll    }
      matchAllDFS { Token _ TokenMatchAllDFS }
      as          { Token _ TokenAs          }
      with        { Token _ TokenWith        }

      int         { Token _ (TokenInt $$)    }
      var         { Token _ (TokenVar $$)    }

      "=="        { Token _ TokenEq          }
      '<'         { Token _ TokenLT          }
      '>'         { Token _ TokenGT          }
      "<="        { Token _ TokenLE          }
      ">="        { Token _ TokenGE          }
      '+'         { Token _ TokenPlus        }
      '-'         { Token _ TokenMinus       }
      '%'         { Token _ TokenPercent     }
      '*'         { Token _ TokenAsterisk    }
      '/'         { Token _ TokenDiv         }
      '^'         { Token _ TokenCaret       }
      "&&"        { Token _ TokenAndAnd      }
      "||"        { Token _ TokenBarBar      }
      ':'         { Token _ TokenColon       }
      ".."        { Token _ TokenDotDot      }
      "++"        { Token _ TokenPlusPlus    }

      '|'         { Token _ TokenBar         }
      "->"        { Token _ TokenArrow       }
      '$'         { Token _ TokenDollar      }
      '_'         { Token _ TokenUnderscore  }
      '#'         { Token _ TokenSharp       }
      ','         { Token _ TokenComma       }

      '('         { Token _ TokenLParen      }
      ')'         { Token _ TokenRParen      }
      '['         { Token _ TokenLBracket    }
      ']'         { Token _ TokenRBracket    }

%%

-- TopExprs :
--     TopExpr           { [$1] }
--   | TopExpr TopExprs  { $1 : $2 }

TopExpr :
    Expr              { Test $1 }
  | test '(' Expr ')' { Test $3 }

Expr :
    Expr1             { $1 }
  | MatchExpr         { $1 }

Expr1 :
    Atoms             { $1 }
  | '-' Atom          { makeApply (VarExpr $ stringToVar "*") [IntegerExpr(-1), $2] }
  | BinOpExpr         { $1 }

BinOpExpr :
    Expr1 "==" Expr1  { makeApply (VarExpr $ stringToVar "eq?")       [$1, $3] }
  | Expr1 "<=" Expr1  { makeApply (VarExpr $ stringToVar "lte?")      [$1, $3] }
  | Expr1 '<'  Expr1  { makeApply (VarExpr $ stringToVar "lt?")       [$1, $3] }
  | Expr1 ">=" Expr1  { makeApply (VarExpr $ stringToVar "gte?")      [$1, $3] }
  | Expr1 '>'  Expr1  { makeApply (VarExpr $ stringToVar "gt?")       [$1, $3] }
  | Expr1 '+'  Expr1  { makeApply (VarExpr $ stringToVar "+")         [$1, $3] }
  | Expr1 '-'  Expr1  { makeApply (VarExpr $ stringToVar "-")         [$1, $3] }
  | Expr1 '%'  Expr1  { makeApply (VarExpr $ stringToVar "remainder") [$1, $3] }
  | Expr1 '*'  Expr1  { makeApply (VarExpr $ stringToVar "*")         [$1, $3] }
  | Expr1 '/'  Expr1  { makeApply (VarExpr $ stringToVar "/")         [$1, $3] }
  | Expr1 '^'  Expr1  { makeApply (VarExpr $ stringToVar "**")        [$1, $3] }
  | Expr1 "&&" Expr1  { makeApply (VarExpr $ stringToVar "and")       [$1, $3] }
  | Expr1 "||" Expr1  { makeApply (VarExpr $ stringToVar "or")        [$1, $3] }
  | Expr1 ':'  Expr1  { makeApply (VarExpr $ stringToVar "cons")      [$1, $3] }
  | Expr1 "++" Expr1  { makeApply (VarExpr $ stringToVar "append")    [$1, $3] }

Atoms :
    Atom              { $1 }
  | Atoms Atom        { ApplyExpr $1 $2 }

MatchExpr :
    match       Expr as Atoms with '|' MatchClauses { MatchExpr $2 $4 $7 }
  | matchDFS    Expr as Atoms with '|' MatchClauses { MatchDFSExpr $2 $4 $7 }
  | matchAll    Expr as Atoms with '|' MatchClauses { MatchAllExpr $2 $4 $7 }
  | matchAllDFS Expr as Atoms with '|' MatchClauses { MatchAllDFSExpr $2 $4 $7 }

MatchClauses :
    Pattern "->" Expr                               { [($1, $3)] }
  | MatchClauses '|' Pattern "->" Expr              { $1 ++ [($3, $5)] }

Atom :
    int                    { IntegerExpr $1 }
  | var                    { VarExpr $ stringToVar $1 }
  | True                   { BoolExpr True }
  | False                  { BoolExpr False }
  | '(' TupleExpr1 ')'     { TupleExpr $2 }
  | '(' Expr ')'           { $2 }
  | '[' Expr ".." Expr ']' { makeApply (VarExpr $ stringToVar "between") [$2, $4] }

TupleExpr1 :
    Expr ',' Expr          { [$1, $3] }
  | Expr ',' TupleExpr1    { $1 : $3 }

--
-- Patterns
--

Pattern :
    '_'                    { WildCard }
  | '$' var                { PatVar (stringToVar $2) }


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
parseTopExprs = undefined -- runAlex' parseTopExprs_

parseTopExpr :: String -> Either EgisonError EgisonTopExpr
parseTopExpr = runAlex' parseTopExpr_

parseExprs :: String -> Either EgisonError [EgisonExpr]
parseExprs = undefined

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
