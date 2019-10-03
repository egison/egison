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

import           Data.Char               (readLitChar)
import           Data.Either
import qualified Data.Set                as Set
import           Data.List.Split         (endByOneOf)
import           Data.Text               (pack)

import           System.Directory        (doesFileExist, getHomeDirectory)

import           Language.Egison.Desugar
import           Language.Egison.Lexer
import           Language.Egison.Types
import           Paths_egison            (getDataFileName)

}

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
%nonassoc '!'
%left '<' '>' "==" "<=" ">="
%left '+' '-' '%'
%left '*' '/'
%left '^'

%token
      True           { Token _ TokenTrue           }
      False          { Token _ TokenFalse          }
      test           { Token _ TokenTest           }
      match          { Token _ TokenMatch          }
      matchDFS       { Token _ TokenMatchDFS       }
      matchAll       { Token _ TokenMatchAll       }
      matchAllDFS    { Token _ TokenMatchAllDFS    }
      matchLambda    { Token _ TokenMatchLambda    }
      matchAllLambda { Token _ TokenMatchAllLambda }
      as             { Token _ TokenAs             }
      with           { Token _ TokenWith           }
      something      { Token _ TokenSomething      }
      if             { Token _ TokenIf             }
      then           { Token _ TokenThen           }
      else           { Token _ TokenElse           }

      int            { Token _ (TokenInt $$)       }
      str            { Token _ (TokenString $$)    }
      char           { Token _ (TokenChar $$)      }
      Var            { Token _ (TokenUpperVar $$)  }
      var            { Token _ (TokenLowerVar $$)  }

      "=="           { Token _ TokenEqEq           }
      '<'            { Token _ TokenLT             }
      '>'            { Token _ TokenGT             }
      "<="           { Token _ TokenLE             }
      ">="           { Token _ TokenGE             }
      '+'            { Token _ TokenPlus           }
      '-'            { Token _ TokenMinus          }
      '%'            { Token _ TokenPercent        }
      '*'            { Token _ TokenAsterisk       }
      '/'            { Token _ TokenDiv            }
      '^'            { Token _ TokenCaret          }
      "&&"           { Token _ TokenAndAnd         }
      "||"           { Token _ TokenBarBar         }
      '!'            { Token _ TokenExclamationMark }
      ':'            { Token _ TokenColon          }
      ".."           { Token _ TokenDotDot         }
      "++"           { Token _ TokenPlusPlus       }

      '|'            { Token _ TokenBar            }
      "->"           { Token _ TokenArrow          }
      '$'            { Token _ TokenDollar         }
      '_'            { Token _ TokenUnderscore     }
      '#'            { Token _ TokenSharp          }
      ','            { Token _ TokenComma          }
      '\\'           { Token _ TokenBackSlash      }
      "*$"           { Token _ TokenAstDollar      }
      '='            { Token _ TokenEq             }

      '('            { Token _ TokenLParen         }
      ')'            { Token _ TokenRParen         }
      '['            { Token _ TokenLBracket       }
      ']'            { Token _ TokenRBracket       }
      eof            { Token _ TokenEOF            }

%%

TopExpr :: { EgisonTopExpr }
  : var list1(ArgNoConflict) '=' Expr opt(eof) { Define (stringToVar $1) (LambdaExpr $2 $4) }
  | Atoms '=' Expr                    opt(eof) { makeDefine $1 $3 }
  | Expr                              opt(eof) { Test $1 }
  | test '(' Expr ')'                 opt(eof) { Test $3 }

Expr :: { EgisonExpr }
  : Expr1                       { $1 }
  | MatchExpr                   { $1 }
  | '\\' list1(Arg) "->" Expr   { LambdaExpr $2 $4 }
  | if Expr then Expr else Expr { IfExpr $2 $4 $6 }

Expr1 :: { EgisonExpr }
  : Atoms             { $1 }
  | '-' Atom          { makeApply (VarExpr $ stringToVar "*") [IntegerExpr(-1), $2] }
  | BinOpExpr         { $1 }

BinOpExpr :: { EgisonExpr }
  : Expr1 "==" Expr1  { makeApply (VarExpr $ stringToVar "eq?")       [$1, $3] }
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

Atoms :: { EgisonExpr }
  : Atom              { $1 }
  | Atom list1(Atom)  { makeApply $1 $2 }

MatchExpr :: { EgisonExpr }
  : match       Expr as Expr with opt('|') MatchClauses { MatchExpr $2 $4 $7 }
  | matchDFS    Expr as Expr with opt('|') MatchClauses { MatchDFSExpr $2 $4 $7 }
  | matchAll    Expr as Expr with opt('|') MatchClauses { MatchAllExpr $2 $4 $7 }
  | matchAllDFS Expr as Expr with opt('|') MatchClauses { MatchAllDFSExpr $2 $4 $7 }
  | matchLambda      as Expr with opt('|') MatchClauses { MatchLambdaExpr $3 $6 }
  | matchAllLambda   as Expr with opt('|') MatchClauses { MatchAllLambdaExpr $3 $6 }

-- match ... as ... with
--   ... -> match ... as ... with
--            ... -> ...
--          | ... -> ...           <= this will be interpreted as a pattern of first match
-- | ... -> ...

MatchClauses :: { [MatchClause] }
  : Pattern "->" Expr1                               { [($1, $3)] }
  | Pattern "->" Expr1 '|' MatchClauses              { ($1, $3) : $5 }

Arg :: { Arg }
  : '$' var                  { ScalarArg $2 }
  | var                      { ScalarArg $1 }
  | "*$" var                 { InvertedScalarArg $2 }
  | '%' var                  { TensorArg $2 }

ArgNoConflict :: { Arg }
  : '$' var                  { ScalarArg $2 }
  | "*$" var                 { InvertedScalarArg $2 }

Atom :: { EgisonExpr }
  : int                      { IntegerExpr $1 }
  | str                      { (StringExpr . pack . init  . tail) $1 }
  | char                     { (CharExpr . fst . (!! 0) . readLitChar . tail) $1 }
  | var                      { VarExpr $ stringToVar $1 }
  | Var                      { InductiveDataExpr $1 [] }
  | True                     { BoolExpr True }
  | False                    { BoolExpr False }
  | something                { SomethingExpr }
  | '(' sep2(Expr, ',') ')'  { TupleExpr $2 }
  | '[' sep(Expr, ',') ']'   { CollectionExpr (map ElementExpr $2) }
  | '(' Expr ')'             { $2 }
  | '[' Expr ".." Expr ']'   { makeApply (VarExpr $ stringToVar "between") [$2, $4] }

--
-- Patterns
--

Pattern :: { EgisonPattern }
  : '_'                        { WildCard }
  | '$' var                    { PatVar (stringToVar $2) }
  | '#' Atom                   { ValuePat $2 }
  | '(' sep2(Pattern, ',') ')' { TuplePat $2 }
  | '(' var list(Pattern) ')'  { InductivePat $2 $3 }
  | '[' ']'                    { InductivePat "nil" [] }
  | Pattern ':' Pattern        { InductivePat "cons" [$1, $3] }
  | Pattern "++" Pattern       { InductivePat "join" [$1, $3] }
  | Pattern "&&" Pattern       { AndPat [$1, $3] }
  | Pattern "||" Pattern       { OrPat [$1, $3] }
  | '!' Pattern                { NotPat $2 }
  | '(' Pattern ')'            { $2 }

--
-- Helpers (Parameterized Products)
--

sep2(p, q)    : p q sep1(p, q)     { $1 : $3 }
sep1(p, q)    : p list(snd(q, p))  { $1 : $2 }
sep(p, q)     : sep1(p, q)         { $1 }
              |                    { [] }

opt(p)        : p                  { Just $1 }
              |                    { Nothing }

snd(p, q)     : p q                { $2 }

list1(p)      : rev_list1(p)       { reverse $1 }
list(p)       : list1(p)           { $1 }
              |                    { [] }

rev_list1(p)  : p                  { [$1] }
              | rev_list1(p) p     { $2 : $1 }

{
makeApply :: EgisonExpr -> [EgisonExpr] -> EgisonExpr
makeApply (InductiveDataExpr x []) xs = InductiveDataExpr x xs
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

makeDefine :: EgisonExpr -> EgisonExpr -> EgisonTopExpr
makeDefine (VarExpr x) bodyExpr =
  Define x bodyExpr
makeDefine (ApplyExpr (VarExpr (Var [f] _)) (TupleExpr xs)) bodyExpr =
  Define (stringToVar f) (LambdaExpr (map exprToArg xs) bodyExpr)

exprToArg :: EgisonExpr -> Arg
exprToArg (VarExpr (Var [x] _)) = ScalarArg x

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token _ TokenEOF) = alexError "unexpected eof"
happyError (Token p t) = alexError' p ("parse error at token '" ++ show t ++ "'")

readTopExprs :: String -> EgisonM [EgisonTopExpr]
readTopExprs = either throwError (mapM desugarTopExpr) . parseTopExprs

readTopExpr :: String -> EgisonM EgisonTopExpr
readTopExpr = either throwError desugarTopExpr . parseTopExpr

readExprs :: String -> EgisonM [EgisonExpr]
readExprs = liftEgisonM . runDesugarM . either throwError (mapM desugar) . parseExprs

readExpr :: String -> EgisonM EgisonExpr
readExpr = liftEgisonM . runDesugarM . either throwError desugar . parseExpr

parseLines :: (String -> Either EgisonError a) -> [a] -> String -> [String] -> Either EgisonError [a]
parseLines parser parsed [] [] = Right parsed
parseLines parser parsed deferred pending =
  case pending of
    [] -> Left $ Parser "shouldn't reach here"
    [last] -> case parser (deferred ++ last) of
                Left ParserUnexpectedEOF -> Right parsed
                Left err -> Left err
                Right expr -> Right (parsed ++ [expr])
    new:rest ->
      case parser (deferred ++ new) of
        Left _ -> parseLines parser parsed (deferred ++ new) rest
        Right expr -> parseLines parser (parsed ++ [expr]) [] rest

parseTopExprs :: String -> Either EgisonError [EgisonTopExpr]
parseTopExprs input =
  parseLines parseTopExpr [] "" $ map (++ "\n") $ endByOneOf "\r\n" input

parseTopExpr :: String -> Either EgisonError EgisonTopExpr
parseTopExpr = runAlex' parseTopExpr_ . (++ "\n")

parseExprs :: String -> Either EgisonError [EgisonExpr]
parseExprs input =
  parseLines parseExpr [] "" $ map (++ "\n") $ endByOneOf "\r\n" input

parseExpr :: String -> Either EgisonError EgisonExpr
parseExpr = runAlex' parseExpr_ . (++ "\n")

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
  shebang ('#':'!':cs) = "-- #!" ++ cs
  shebang cs           = cs
}
