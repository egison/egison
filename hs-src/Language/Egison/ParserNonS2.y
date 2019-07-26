{
{-# OPTIONS -w #-}
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
       , loadFile
       , loadLibraryFile
       ) where

import Language.Egison.Lexer
import Language.Egison.Types

}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
-- Without this we get a type error
%error { happyError }

%right let in
%left '<' '>' '=' "<=" ">="
%left '+' '-'
%left '*' '/'

%token
      int      { Token _ (TokenInt $$) }
      var      { Token _ (TokenVar $$) }
      '='      { Token _ TokenEq }
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

EgisonExpr :
    int      { IntegerExpr $1 }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ show t ++ "'")

readTopExprs :: String -> EgisonM [EgisonTopExpr]
readTopExprs = undefined

readTopExpr :: String -> EgisonM EgisonTopExpr
readTopExpr = undefined

readExprs :: String -> EgisonM [EgisonExpr]
readExprs = undefined

readExpr :: String -> EgisonM EgisonExpr
readExpr = undefined

parseTopExprs :: String -> Either EgisonError [EgisonTopExpr]
parseTopExprs = undefined

parseTopExpr :: String -> Either EgisonError EgisonTopExpr
parseTopExpr = undefined

parseExprs :: String -> Either EgisonError [EgisonExpr]
parseExprs = undefined

parseExpr :: String -> Either EgisonError EgisonExpr
parseExpr = undefined
-- parseExpr = runAlex' parse

loadFile :: FilePath -> EgisonM [EgisonTopExpr]
loadFile = undefined

loadLibraryFile :: FilePath -> EgisonM [EgisonTopExpr]
loadLibraryFile = undefined
}
