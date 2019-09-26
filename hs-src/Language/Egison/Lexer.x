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
       , alexError
       , alexError'
       ) where

import           Prelude               hiding (lex)
import           Control.Monad         (liftM)

import           Language.Egison.Types

}

%wrapper "monadUserState"
$digit = [0-9]
$octdig = [0-7]
$hexdig = [0-9A-Fa-f]
$special = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
$alpha = [A-Za-z]

$escaped = [nrtbfv0\'\"\\]

tokens :-
  \-\- .* [\r\n]                 ;
  $white+                        ;
  \{\-                           { nestedComment            }

  -- Keywords
  True                           { lex' TokenTrue           }
  False                          { lex' TokenFalse          }
  test                           { lex' TokenTest           }
  match                          { lex' TokenMatch          }
  matchDFS                       { lex' TokenMatchDFS       }
  matchAll                       { lex' TokenMatchAll       }
  matchAllDFS                    { lex' TokenMatchAllDFS    }
  matchLambda                    { lex' TokenMatchLambda    }
  matchAllLambda                 { lex' TokenMatchAllLambda }
  as                             { lex' TokenAs             }
  with                           { lex' TokenWith           }
  something                      { lex' TokenSomething      }
  if                             { lex' TokenIf             }
  then                           { lex' TokenThen           }
  else                           { lex' TokenElse           }

  -- Data
  $digit+                        { lex (TokenInt . read)    }
  \" [^\" \n]* \"                { lex  TokenString         }
  \' . \'                        { lex  TokenChar           }
  \' \\ $escaped \'              { lex  TokenChar           }
  $alpha [$alpha $digit \? \']*  { lex  TokenVar            }

  -- Operators
  \=\=                           { lex' TokenEqEq           }
  \<                             { lex' TokenLT             }
  \>                             { lex' TokenGT             }
  \<\=                           { lex' TokenLE             }
  \>\=                           { lex' TokenGE             }
  \+                             { lex' TokenPlus           }
  \-                             { lex' TokenMinus          }
  \%                             { lex' TokenPercent        }
  \*                             { lex' TokenAsterisk       }
  \/                             { lex' TokenDiv            }
  \^                             { lex' TokenCaret          }
  \&\&                           { lex' TokenAndAnd         }
  \|\|                           { lex' TokenBarBar         }
  \:                             { lex' TokenColon          }
  \.\.                           { lex' TokenDotDot         }
  \+\+                           { lex' TokenPlusPlus       }

  \|                             { lex' TokenBar            }
  \-\>                           { lex' TokenArrow          }
  \$                             { lex' TokenDollar         }
  \_                             { lex' TokenUnderscore     }
  \#                             { lex' TokenSharp          }
  \,                             { lex' TokenComma          }
  \\                             { lex' TokenBackSlash      }
  \*\$                           { lex' TokenAstDollar      }
  \=                             { lex' TokenEq             }

  \(                             { lex' TokenLParen         }
  \)                             { lex' TokenRParen         }
  \[                             { lex' TokenLBracket       }
  \]                             { lex' TokenRBracket       }

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
  -- Keywords
  = TokenTrue
  | TokenFalse
  | TokenTest
  | TokenMatch
  | TokenMatchDFS
  | TokenMatchAll
  | TokenMatchAllDFS
  | TokenMatchLambda
  | TokenMatchAllLambda
  | TokenAs
  | TokenWith
  | TokenSomething
  | TokenIf
  | TokenThen
  | TokenElse

  -- Data and Variables
  | TokenInt Integer
  | TokenString String  -- with double quotation at both sides
  | TokenChar String    -- with single quotation at both sides
  | TokenVar String

  | TokenEqEq
  | TokenLT
  | TokenGT
  | TokenLE
  | TokenGE
  | TokenPlus
  | TokenMinus
  | TokenPercent
  | TokenAsterisk
  | TokenDiv
  | TokenCaret
  | TokenAndAnd
  | TokenBarBar
  | TokenColon
  | TokenDotDot
  | TokenPlusPlus

  | TokenBar
  | TokenArrow
  | TokenDollar
  | TokenUnderscore
  | TokenSharp
  | TokenComma
  | TokenBackSlash
  | TokenAstDollar
  | TokenEq

  | TokenLParen
  | TokenRParen
  | TokenLBracket
  | TokenRBracket
  | TokenEOF

-- For nice parser error messages.
instance Show TokenClass where
  show TokenTrue = "True"
  show TokenFalse = "False"
  show TokenTest = "test"
  show TokenMatch = "match"
  show TokenMatchDFS = "matchDFS"
  show TokenMatchAll = "matchAll"
  show TokenMatchAllDFS = "matchAllDFS"
  show TokenMatchLambda = "matchLambda"
  show TokenMatchAllLambda = "matchAllLambda"
  show TokenAs = "as"
  show TokenWith = "with"
  show TokenSomething = "something"
  show TokenIf = "if"
  show TokenThen = "then"
  show TokenElse = "else"

  show (TokenInt i) = show i
  show (TokenString s) = s
  show (TokenChar c) = c
  show (TokenVar s) = show s

  show TokenEqEq = "=="
  show TokenLT = "<"
  show TokenGT = ">"
  show TokenLE = "<="
  show TokenGE = ">="
  show TokenPlus = "+"
  show TokenMinus = "-"
  show TokenPercent = "%"
  show TokenAsterisk = "*"
  show TokenDiv = "/"
  show TokenCaret = "^"
  show TokenAndAnd = "&&"
  show TokenBarBar = "||"
  show TokenColon = ":"
  show TokenDotDot = ".."
  show TokenPlusPlus = "++"

  show TokenBar = "|"
  show TokenArrow = "->"
  show TokenDollar = "$"
  show TokenUnderscore = "_"
  show TokenSharp = "#"
  show TokenComma = ","
  show TokenBackSlash = "\\"
  show TokenAstDollar = "*$"
  show TokenEq = "="

  show TokenLParen = "("
  show TokenRParen = ")"
  show TokenLBracket = "["
  show TokenRBracket = "]"
  show TokenEOF = "<EOF>"

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ Token p TokenEOF

-- Unfortunately, we have to extract the matching bit of string ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) len -> return $ Token p (f (take len s))

-- For constructing tokens that do not depend on the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const

-- cf. https://github.com/simonmar/alex/blob/master/examples/haskell.x
nestedComment :: AlexInput -> Int -> Alex Token
nestedComment _ _ = do
  input <- alexGetInput
  go 1 input
    where
      go 0 input = do
        alexSetInput input
        alexMonadScan'
      go n input = do
        case alexGetByte input of
          Nothing  -> err input
          Just (45, input) -> do                   -- '-'
            let temp = input
            case alexGetByte input of
              Nothing  -> err input
              Just (125, input) -> go (n-1) input  -- '}'
              Just (45,  input) -> go n temp       -- '-'
              Just (_,   input) -> go n input

          Just (123, input) ->                     -- '{'
            case alexGetByte input of
              Nothing -> err input
              Just (45, input) -> go (n+1) input   -- '-'
              Just (_,  input) -> go n input

          Just (_, input) -> go n input

      err input = do
        alexSetInput input
        -- TODO(momohatt): Use alexError'
        alexError "error in nested comment"

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
