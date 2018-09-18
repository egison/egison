{
{-# OPTIONS -w #-}
module Language.Egison.Parser.Parser( parseExp ) where

import Language.Egison.Types
import Language.Egison.Lexer

}

-- The expression language used here comes straight from the happy
-- documentation with virtually no changes (open, so TokenOB/TokenCB were
-- changed to TokenLParen/TokenRParen

%name parse
%name topExpr texpr
%name doParse parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
-- Without this we get a type error
%error { happyError }

%expect 0
%token 
      if              { Token _ TokenIf }
      else            { Token _ TokenElse }
      int             { Token _ (TokenInt $$) }
      var             { Token _ (TokenVar $$) }
      '='             { Token _ TokenEq }
      '+'             { Token _ TokenPlus }
      '-'             { Token _ TokenMinus }
      '*'             { Token _ TokenTimes }
      '/'             { Token _ TokenDiv }
      ':'             { Token _ TokenCons }
      '('             { Token _ TokenLParen }
      ')'             { Token _ TokenRParen }

%%

-- 値を返さないやつ
Exp   : if Exp1 Exp1 else Exp1 end  { IfExpr $2 $4 $6 }
      | var '=' Exp1             { Define $1 $2 }
      | var '(' Args ')'        { ApplyExpr $1 $3 }
      | Exp1                    { $1 }

-- 値を返すやつ
Exp1  : Exp1 '+' Term           { Plus $1 $3 }
      | Exp1 '-' Term           { Minus $1 $3 }
      | Term                    { $1 }

Term  : Term '*' Term1         { Times $1 $3 }
      | Term '/' Temr1         { Div $1 $3 }
      | Term1                  { $1 }

Term1 : Term1 ':' Factor       { Cons $1 $3 }
      | Factor                 { $1 }

Factor        
      : int                     { Int $1 }
      | var                     { VarExpr $1 }
      | '(' Exp ')'             { $2 }

{
data Exp  
      = Let String Exp Exp
      | Exp1 Exp1
      deriving Show

data Exp1 
      = Plus Exp1 Term 
      | Minus Exp1 Term 
      | Term Term
      deriving Show

data Term 
      = Times Term Factor 
      | Div Term Factor 
      | Factor Factor
      deriving Show

data Factor 
      = Int Int 
      | Var String 
      | Brack Exp
      deriving Show

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseExp :: FilePath -> String -> Either String EgisonExpr
parseExp = runAlex' parse

texpr :: Parser EgisonTopExpr

}
