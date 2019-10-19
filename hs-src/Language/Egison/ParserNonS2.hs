{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

{- |
Module      : Language.Egison.ParserNonS
Copyright   : Satoshi Egi
Licence     : MIT

This module provide Egison parser.
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

import           Control.Applicative     (pure, (*>), (<$>), (<$), (<*), (<*>))
import           Control.Monad.Except    hiding (mapM)
import           Control.Monad.Identity  hiding (mapM)
import           Control.Monad.State     hiding (mapM)
import           Prelude                 hiding (mapM)

import           System.Directory        (doesFileExist, getHomeDirectory)

import           Data.Char               (isLower, isUpper, toLower)
import           Data.Either
import           Data.Functor            (($>))
import           Data.List.Split         (split, splitOn, startsWithOneOf)
import           Data.Ratio
import           Data.Traversable        (mapM)

import           Control.Monad.Combinators.Expr
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Debug
import           Text.Megaparsec.Pos     (Pos)

import qualified Data.Text               as T
import           Text.Regex.TDFA

import           Language.Egison.Desugar
import           Language.Egison.Types
import           Paths_egison            (getDataFileName)

readTopExprs :: String -> EgisonM [EgisonTopExpr]
readTopExprs = either throwError (mapM desugarTopExpr) . parseTopExprs

readTopExpr :: String -> EgisonM EgisonTopExpr
readTopExpr = either throwError desugarTopExpr . parseTopExpr

readExprs :: String -> EgisonM [EgisonExpr]
readExprs = liftEgisonM . runDesugarM . either throwError (mapM desugar) . parseExprs

readExpr :: String -> EgisonM EgisonExpr
readExpr = liftEgisonM . runDesugarM . either throwError desugar . parseExpr

parseTopExprs :: String -> Either EgisonError [EgisonTopExpr]
parseTopExprs = doParse $ many $ L.nonIndented sc topExpr

parseTopExpr :: String -> Either EgisonError EgisonTopExpr
parseTopExpr = doParse (sc >> topExpr)

parseExprs :: String -> Either EgisonError [EgisonExpr]
parseExprs = doParse $ many $ L.nonIndented sc expr

parseExpr :: String -> Either EgisonError EgisonExpr
parseExpr = doParse (sc >> expr)

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

--
-- Parser
--

type Parser = Parsec Void String

doParse :: Parser a -> String -> Either EgisonError a
doParse p input = either (throwError . fromParsecError) return $ parse p "egison" input
  where
    fromParsecError :: ParseErrorBundle String Void -> EgisonError
    fromParsecError = Parser . errorBundlePretty

doParse' :: Parser a -> String -> a
doParse' p input = case doParse p input of
                     Right x -> x

--
-- Expressions
--

topExpr :: Parser EgisonTopExpr
topExpr = Test <$> (dbg "expr" expr)

expr :: Parser EgisonExpr
expr = dbg "ifExpr" ifExpr
   <|> dbg "patternMatchExpr" patternMatchExpr
   <|> try (dbg "applyExpr" applyExpr)
   <|> dbg "opExpr" opExpr
   <?> "expressions"

-- Also parses atomExpr
opExpr :: Parser EgisonExpr
opExpr = makeExprParser atomExpr table
  where
    table :: [[Operator Parser EgisonExpr]]
    table =
      [ [ Prefix (unary  "-"         "-" ) ]
      , [ InfixL (binary "**"        "^" ) ]
      , [ InfixL (binary "*"         "*" )
        , InfixL (binary "/"         "/" )
        , InfixL (binary "and"       "&&") ]
      , [ InfixL (binary "+"         "+" )
        , InfixL (binary "-"         "-" )
        , InfixL (binary "remainder" "%" )
        , InfixL (binary "or"        "||") ]
      , [ InfixL (binary "cons"      ":" )
        , InfixL (binary "append"    "++") ]
      , [ InfixL (binary "eq?"       "==")
        , InfixL (binary "lte?"      "<=")
        , InfixL (binary "lt?"       "<" )
        , InfixL (binary "gte?"      ">=")
        , InfixL (binary "gt?"       ">" ) ]
      ]
    unary  internalName sym = makeUnaryOpApply  internalName <$ symbol sym
    binary internalName sym = makeBinaryOpApply internalName <$ symbol sym


ifExpr :: Parser EgisonExpr
ifExpr = keywordIf >> IfExpr <$> expr <* keywordThen <*> expr <* keywordElse <*> expr

patternMatchExpr :: Parser EgisonExpr
patternMatchExpr = makeMatchExpr       keywordMatch          MatchExpr
               <|> makeMatchExpr       keywordMatchDFS       MatchDFSExpr
               <|> makeMatchExpr       keywordMatchAll       MatchAllExpr
               <|> makeMatchExpr       keywordMatchAllDFS    MatchAllDFSExpr
               <|> makeMatchLambdaExpr keywordMatchLambda    MatchLambdaExpr
               <|> makeMatchLambdaExpr keywordMatchAllLambda MatchAllLambdaExpr
  where
    makeMatchExpr keyword ctor = do
      pos     <- L.indentLevel
      tgt     <- keyword >> expr
      matcher <- keywordAs >> expr
      clauses <- keywordWith >> optional (symbol "|") >> matchClauses pos
      return $ ctor tgt matcher clauses

    makeMatchLambdaExpr keyword ctor = do
      pos     <- L.indentLevel
      matcher <- keyword >> keywordAs >> expr
      clauses <- keywordWith >> optional (symbol "|") >> matchClauses pos
      return $ ctor matcher clauses

matchClauses :: Pos -> Parser [MatchClause]
matchClauses pos = sepBy1 (matchClause pos) (symbol "|")

matchClause :: Pos -> Parser MatchClause
matchClause pos = (,) <$> (L.indentGuard sc GT pos *> patternExpr) <*> (symbol "->" >> expr)

patternExpr :: Parser EgisonPattern
patternExpr = WildCard <$ symbol "_"
          <|> PatVar   <$> (symbol "$" >> varLiteral)
          <|> ValuePat <$> (symbol "#" >> atomExpr)

applyExpr :: Parser EgisonExpr
applyExpr = do
  pos <- L.indentLevel
  func <- atomExpr
  args <- some (L.indentGuard sc GT pos *> atomExpr)
  return $ case args of
             [] -> func
             _  -> makeApply func args

boolExpr :: Parser EgisonExpr
boolExpr = (reserved "True"  $> BoolExpr True)
       <|> (reserved "False" $> BoolExpr False)

collectionExpr :: Parser EgisonExpr
collectionExpr = symbol "[" >> (try _betweenExpr <|> _elementsExpr)
  where
    _betweenExpr = makeBinaryOpApply "between" <$> expr <*> (symbol ".." >> expr <* symbol "]")
    _elementsExpr = CollectionExpr <$> (sepBy (ElementExpr <$> expr) comma <* symbol "]")

tupleExpr :: Parser EgisonExpr
-- TODO(momohatt): Do we really need an empty tuple?
tupleExpr = try (TupleExpr <$> (symbol "(" >> symbol ")" $> []))
        <|> (\x y -> TupleExpr (x:y)) <$> (symbol "(" >> expr) <*> some (comma >> expr) <* symbol ")"

atomExpr :: Parser EgisonExpr
atomExpr = IntegerExpr <$> positiveIntegerLiteral
       <|> boolExpr
       <|> VarExpr <$> varLiteral
       <|> collectionExpr
       <|> tupleExpr
       <|> parens expr
       <?> "atomic expressions"

--
-- Tokens
--

-- space comsumer
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockCommentNested "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
brackets  = between (symbol "[") (symbol "]")
semicolon = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."

positiveIntegerLiteral :: Parser Integer
positiveIntegerLiteral = lexeme L.decimal

varLiteral :: Parser Var
varLiteral = stringToVar <$> identifier

reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

keywordDefine               = reserved "define"
keywordSet                  = reserved "set!"
keywordTest                 = reserved "test"
keywordLoadFile             = reserved "loadFile"
keywordLoad                 = reserved "load"
keywordIf                   = reserved "if"
keywordThen                 = reserved "then"
keywordElse                 = reserved "else"
keywordSeq                  = reserved "seq"
keywordApply                = reserved "apply"
keywordCApply               = reserved "capply"
keywordLambda               = reserved "lambda"
keywordMemoizedLambda       = reserved "memoizedLambda"
keywordCambda               = reserved "cambda"
keywordProcedure            = reserved "procedure"
keywordMacro                = reserved "macro"
keywordPatternFunction      = reserved "patternFunction"
keywordLetRec               = reserved "letrec"
keywordLet                  = reserved "let"
keywordLetStar              = reserved "let*"
keywordLetIn                = reserved "in"
keywordWithSymbols          = reserved "withSymbols"
keywordLoop                 = reserved "loop"
keywordCont                 = reserved "..."
keywordMatch                = reserved "match"
keywordMatchDFS             = reserved "matchDFS"
keywordMatchAll             = reserved "matchAll"
keywordMatchAllDFS          = reserved "matchAllDFS"
keywordMatchLambda          = reserved "matchLambda"
keywordMatchAllLambda       = reserved "matchAllLambda"
keywordAs                   = reserved "as"
keywordWith                 = reserved "with"
keywordMatcher              = reserved "matcher"
keywordDo                   = reserved "do"
keywordIo                   = reserved "io"
keywordSomething            = reserved "something"
keywordUndefined            = reserved "undefined"
keywordAlgebraicDataMatcher = reserved "algebraicDataMatcher"
keywordGenerateTensor       = reserved "generateTensor"
keywordTensor               = reserved "tensor"
keywordTensorContract       = reserved "contract"
keywordSubrefs              = reserved "subrefs"
keywordSubrefsNew           = reserved "subrefs!"
keywordSuprefs              = reserved "suprefs"
keywordSuprefsNew           = reserved "suprefs!"
keywordUserrefs             = reserved "userRefs"
keywordUserrefsNew          = reserved "userRefs!"
keywordFunction             = reserved "function"

reservedWords = ["define"
               , "set!"
               , "test"
               , "loadFile"
               , "load"
               , "if"
               , "then"
               , "else"
               , "seq"
               , "apply"
               , "capply"
               , "lambda"
               , "memoizedLambda"
               , "cambda"
               , "procedure"
               , "macro"
               , "patternFunction"
               , "letrec"
               , "let"
               , "let*"
               , "in"
               , "withSymbols"
               , "loop"
               , "..."
               , "match"
               , "matchDFS"
               , "matchAll"
               , "matchAllDFS"
               , "matchLambda"
               , "matchAllLambda"
               , "as"
               , "with"
               , "matcher"
               , "do"
               , "io"
               , "something"
               , "undefined"
               , "algebraicDataMatcher"
               , "generateTensor"
               , "tensor"
               , "contract"
               , "subrefs"
               , "subrefs!"
               , "suprefs"
               , "suprefs!"
               , "userRefs"
               , "userRefs!"
               , "function"]

--
-- Utils
--

makeBinaryOpApply :: String -> EgisonExpr -> EgisonExpr -> EgisonExpr
makeBinaryOpApply func x y = makeApply (VarExpr $ stringToVar func) [x, y]

makeUnaryOpApply :: String -> EgisonExpr -> EgisonExpr
makeUnaryOpApply "-" x  = makeBinaryOpApply "*" (IntegerExpr (-1)) x
makeUnaryOpApply func x = makeApply (VarExpr $ stringToVar func) [x]

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
        let n = length vars
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