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

import           Data.Text               (pack)
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
parseTopExprs = doParse $ many (L.nonIndented sc topExpr) <* eof

parseTopExpr :: String -> Either EgisonError EgisonTopExpr
parseTopExpr = doParse $ sc >> topExpr

parseExprs :: String -> Either EgisonError [EgisonExpr]
parseExprs = doParse $ many (L.nonIndented sc expr) <* eof

parseExpr :: String -> Either EgisonError EgisonExpr
parseExpr = doParse $ sc >> expr

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

--
-- Expressions
--

topExpr :: Parser EgisonTopExpr
topExpr = Test <$> (dbg "expr" expr)

expr :: Parser EgisonExpr
expr = dbg "ifExpr" ifExpr
   <|> dbg "patternMatchExpr" patternMatchExpr
   <|> dbg "lambdaExpr" lambdaExpr
   <|> try (dbg "applyExpr" applyExpr)
   <|> dbg "opExpr" opExpr
   <?> "expressions"

-- Also parses atomExpr
opExpr :: Parser EgisonExpr
opExpr = do
  pos <- L.indentLevel
  makeExprParser atomExpr (makeTable pos)
  where
    -- TODO(momohatt): '++' doesn't work
    makeTable :: Pos -> [[Operator Parser EgisonExpr]]
    makeTable pos =
      let unary  internalName sym =
            makeUnaryOpApply  internalName <$ symbol sym
          binary internalName sym =
            makeBinaryOpApply internalName <$ (L.indentGuard sc GT pos >> symbol sym)
       in
          [ [ Prefix (unary  "-"         "-" ) ]
          , [ InfixL (binary "**"        "^" ) ]
          , [ InfixL (binary "*"         "*" )
            , InfixL (binary "/"         "/" )
            , InfixL (binary "and"       "&&") ]
          , [ InfixL (binary "+"         "+" )
            , InfixL (binary "-"         "-" )
            , InfixL (binary "remainder" "%" )
            , InfixL (binary "or"        "||") ]
          , [ InfixR (binary "cons"      ":" ) ]
          , [ InfixR (binary "append"    "++") ]
          , [ InfixL (binary "eq?"       "==")
            , InfixL (binary "lte?"      "<=")
            , InfixL (binary "lt?"       "<" )
            , InfixL (binary "gte?"      ">=")
            , InfixL (binary "gt?"       ">" ) ]
          ]


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
matchClause pos = (,) <$> (L.indentGuard sc GT pos *> pattern) <*> (symbol "->" >> expr)

pattern :: Parser EgisonPattern
pattern = opPattern

opPattern :: Parser EgisonPattern
opPattern = makeExprParser atomPattern table
  where
    table :: [[Operator Parser EgisonPattern]]
    table =
      let inductive2 name sym =
            (\x y -> InductivePat name [x, y]) <$ symbol sym
       in [ [ InfixR (inductive2 "cons"    ":" )
            , InfixR (inductive2 "join"    "++") ]
          ]


atomPattern :: Parser EgisonPattern
atomPattern = WildCard <$   symbol "_"
          <|> PatVar . stringToVar <$> (symbol "$" >> identifier)
          <|> ValuePat <$> (symbol "#" >> atomExpr)
          <|> InductivePat "nil" [] <$ (symbol "[" >> symbol "]")

lambdaExpr :: Parser EgisonExpr
lambdaExpr = LambdaExpr <$> (symbol "\\" >> some arg) <*> (symbol "->" >> expr)

arg :: Parser Arg
arg = ScalarArg         <$> (symbol "$"  >> identifier)
  <|> InvertedScalarArg <$> (symbol "*$" >> identifier)
  <|> TensorArg         <$> (symbol "%"  >> identifier)
  <|> ScalarArg         <$> identifier

applyExpr :: Parser EgisonExpr
applyExpr = do
  pos <- L.indentLevel
  func <- atomExpr
  args <- some (L.indentGuard sc GT pos *> atomExpr)
  return $ makeApply func args

collectionExpr :: Parser EgisonExpr
collectionExpr = symbol "[" >> (try _betweenExpr <|> _elementsExpr)
  where
    _betweenExpr = makeBinaryOpApply "between" <$> expr <*> (symbol ".." >> expr <* symbol "]")
    _elementsExpr = CollectionExpr <$> (sepBy (ElementExpr <$> expr) comma <* symbol "]")

tupleOrParenExpr :: Parser EgisonExpr
tupleOrParenExpr = do
  elems <- parens $ sepBy expr comma
  case elems of
    [x] -> return x
    _   -> return $ TupleExpr elems

atomExpr :: Parser EgisonExpr
atomExpr = IntegerExpr <$> positiveIntegerLiteral
       <|> BoolExpr <$> boolLiteral
       <|> CharExpr <$> charLiteral
       <|> StringExpr . pack <$> stringLiteral
       <|> VarExpr <$> varLiteral
       <|> SomethingExpr <$ keywordSomething
       <|> UndefinedExpr <$ keywordUndefined
       <|> (\x -> InductiveDataExpr x []) <$> upperId
       <|> collectionExpr
       <|> tupleOrParenExpr
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

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

boolLiteral :: Parser Bool
boolLiteral = reserved "True"  $> True
          <|> reserved "False" $> False

varLiteral :: Parser Var
varLiteral = stringToVar <$> lowerId

reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

symbol :: String -> Parser String
symbol sym = try (L.symbol sc sym)

lowerId :: Parser String
lowerId = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> lowerChar <*> many alphaNumChar
    check x = if x `elem` lowerReservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

-- TODO(momohatt): Deprecate BoolExpr and merge it with InductiveDataExpr
upperId :: Parser String
upperId = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> upperChar <*> many alphaNumChar
    check x = if x `elem` upperReservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

identifier :: Parser String
identifier = lowerId <|> upperId

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

upperReservedWords =
  [ "True"
  , "False"
  ]

lowerReservedWords =
  [ "define"
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
