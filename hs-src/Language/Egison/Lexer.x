{
{-# OPTIONS -w #-}

{- |
Module      : Language.Egison.Lexer
Copyright   : Satoshi Egi
Licence     : MIT

This module provides new Egison lexer.
-}

module Language.Egison.Lexer
       ( Token(..)
       , AlexPosn(..)
       , TokenClass(..)
       , Alex(..)
       , runAlex'
       , alexMonadScan'
       , alexError'
       ) where

import           Prelude               hiding (lex)
import           Control.Monad         (liftM)

import           Language.Egison.Types

}

%wrapper "monadUserState"
$digit = 0-9
$alpha = [A-Za-z]
tokens :-
  $white+                        ;
  $digit+                        { lex (TokenInt . read) }
  $alpha [$alpha $digit \_ \']*  { lex  TokenVar         }
  \=                             { lex' TokenEq          }
  \<                             { lex' TokenLT          }
  \>                             { lex' TokenGT          }
  \<\=                           { lex' TokenLE          }
  \>\=                           { lex' TokenGE          }
  \+                             { lex' TokenPlus        }
  \-                             { lex' TokenMinus       }
  \*                             { lex' TokenAsterisk    }
  \/                             { lex' TokenDiv         }
  \(                             { lex' TokenLParen      }
  \)                             { lex' TokenRParen      }
  \[                             { lex' TokenLBracket    }
  \]                             { lex' TokenRBracket    }

{
-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

-- The token type, consisting of the source code position and a token class.
data Token = Token AlexPosn TokenClass
  deriving ( Show )

data TokenClass
  = TokenInt Integer
  | TokenVar String
  | TokenEq
  | TokenLT
  | TokenGT
  | TokenLE
  | TokenGE
  | TokenPlus
  | TokenMinus
  | TokenAsterisk
  | TokenDiv
  | TokenLParen
  | TokenRParen
  | TokenLBracket
  | TokenRBracket
  | TokenEOF

-- For nice parser error messages.
instance Show TokenClass where
  show (TokenInt i) = show i
  show (TokenVar s) = show s
  show TokenEq = "="
  show TokenLT = "<"
  show TokenGT = ">"
  show TokenLE = "<="
  show TokenGE = ">="
  show TokenPlus = "+"
  show TokenMinus = "-"
  show TokenAsterisk = "*"
  show TokenDiv = "/"
  show TokenLParen = "("
  show TokenRParen = ")"
  show TokenLBracket = "["
  show TokenRBracket = "]"
  show TokenEOF = "<EOF>"

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ Token p TokenEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> String -> Either EgisonError a
runAlex' a input =
  case runAlex input (setFilePath "<stdin>" >> a) of
    Left msg -> Left $ Parser msg
    Right r  -> Right r
}
