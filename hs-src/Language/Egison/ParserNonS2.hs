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
topExpr = Load     <$> (keywordLoad >> stringLiteral)
      <|> LoadFile <$> (keywordLoadFile >> stringLiteral)
      <|> try (dbg "define" defineExpr)
      <|> Test     <$> dbg "expr" expr

defineExpr :: Parser EgisonTopExpr
defineExpr = do
  var <- varLiteral
  symbol "="
  body <- expr
  return $ Define var body

expr :: Parser EgisonExpr
expr = ifExpr
   <|> patternMatchExpr
   <|> lambdaExpr
   <|> letExpr
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
          -- 8
          , [ InfixL (binary "**"        "^" ) ]
          -- 7
          , [ InfixL (binary "*"         "*" )
            , InfixL (binary "/"         "/" )
            , InfixL (binary "remainder" "%" ) ]
          -- 6
          , [ InfixL (binary "+"         "+" )
            , InfixL (binary "-"         "-" ) ]
          -- 5
          , [ InfixR (binary "cons"      ":" )
            , InfixR (binary "append"    "++") ]
          -- 4
          , [ InfixL (binary "eq?"       "==")
            , InfixL (binary "lte?"      "<=")
            , InfixL (binary "lt?"       "<" )
            , InfixL (binary "gte?"      ">=")
            , InfixL (binary "gt?"       ">" ) ]
          -- 3
          , [ InfixR (binary "and"       "&&") ]
          -- 2
          , [ InfixR (binary "or"        "||") ]
          ]


ifExpr :: Parser EgisonExpr
ifExpr = keywordIf >> IfExpr <$> expr <* keywordThen <*> expr <* keywordElse <*> expr

patternMatchExpr :: Parser EgisonExpr
patternMatchExpr = makeMatchExpr keywordMatch       MatchExpr
               <|> makeMatchExpr keywordMatchDFS    MatchDFSExpr
               <|> makeMatchExpr keywordMatchAll    MatchAllExpr
               <|> makeMatchExpr keywordMatchAllDFS MatchAllDFSExpr
  where
    makeMatchExpr keyword ctor = do
      pos     <- L.indentLevel
      tgt     <- keyword >> expr
      matcher <- keywordAs >> expr
      clauses <- keywordWith >> matchClauses1 pos
      return $ ctor tgt matcher clauses

matchClauses1 :: Pos -> Parser [MatchClause]
matchClauses1 pos = (:) <$> (optional (symbol "|") >> matchClause pos) <*> matchClauses pos
  where
    matchClauses :: Pos -> Parser [MatchClause]
    matchClauses pos = try ((:) <$> (symbol "|" >> matchClause pos) <*> matchClauses pos)
                   <|> (return [])
    matchClause :: Pos -> Parser MatchClause
    matchClause pos = (,) <$> (L.indentGuard sc GT pos *> pattern) <*> (symbol "->" >> expr)

lambdaExpr :: Parser EgisonExpr
lambdaExpr = symbol "\\" >> (
      makeMatchLambdaExpr keywordMatch    MatchLambdaExpr
  <|> makeMatchLambdaExpr keywordMatchAll MatchAllLambdaExpr
  <|> LambdaExpr <$> some arg <*> (symbol "->" >> expr))
  where
    makeMatchLambdaExpr keyword ctor = do
      pos     <- L.indentLevel
      matcher <- keyword >> keywordAs >> expr
      clauses <- keywordWith >> matchClauses1 pos
      return $ ctor matcher clauses

arg :: Parser Arg
arg = ScalarArg         <$> (symbol "$"  >> identifier)
  <|> InvertedScalarArg <$> (symbol "*$" >> identifier)
  <|> TensorArg         <$> (symbol "%"  >> identifier)
  <|> ScalarArg         <$> identifier

letExpr :: Parser EgisonExpr
letExpr = do
  pos   <- keywordLet >> L.indentLevel
  binds <- some (L.indentGuard sc EQ pos *> binding)
  body  <- keywordIn >> expr
  return $ LetStarExpr binds body

binding :: Parser BindingExpr
binding = do
  vars <- ((:[]) <$> varLiteral) <|> (parens $ sepBy varLiteral comma)
  body <- symbol "=" >> expr
  return (vars, body)

applyExpr :: Parser EgisonExpr
applyExpr = do
  pos <- L.indentLevel
  func <- atomExpr
  args <- some (L.indentGuard sc GT pos *> atomExpr)
  return $ makeApply func args

collectionExpr :: Parser EgisonExpr
collectionExpr = symbol "[" >> (try betweenOrFromExpr <|> elementsExpr)
  where
    betweenOrFromExpr = do
      start <- expr <* symbol ".."
      end   <- optional expr <* symbol "]"
      case end of
        Just end' -> return $ makeBinaryOpApply "between" start end'
        Nothing   -> return $ makeUnaryOpApply "from" start

    elementsExpr = CollectionExpr <$> (sepBy (ElementExpr <$> expr) comma <* symbol "]")

tupleOrParenExpr :: Parser EgisonExpr
tupleOrParenExpr = do
  elems <- parens $ try pointFreeExpr <|> sepBy expr comma
  case elems of
    [x] -> return x
    _   -> return $ TupleExpr elems
  where
    makeLambda name =
      LambdaExpr [ScalarArg "x", ScalarArg "y"]
                 (ApplyExpr (stringToVarExpr name)
                            (TupleExpr [stringToVarExpr "x", stringToVarExpr "y"]))
    makeLambdaL name rarg =
      LambdaExpr [ScalarArg "x"]
                 (ApplyExpr (stringToVarExpr name)
                            (TupleExpr [stringToVarExpr "x", rarg]))
    makeLambdaR name larg =
      LambdaExpr [ScalarArg "y"]
                 (ApplyExpr (stringToVarExpr name)
                            (TupleExpr [larg, stringToVarExpr "y"]))

    pointFreeExpr :: Parser [EgisonExpr]
    pointFreeExpr = do
      op   <- parseOneOf $ map (\(sym, sem) -> symbol sym $> sem) reservedBinops
      rarg <- optional $ expr
      case rarg of
        Nothing -> return [makeLambda op]
        Just y  -> return [makeLambdaL op y]

hashExpr :: Parser EgisonExpr
hashExpr = HashExpr <$> hashBraces (sepEndBy hashElem comma)
  where
    hashBraces = between (symbol "{|") (symbol "|}")
    hashElem = brackets $ (,) <$> expr <*> (comma >> expr)

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
       <|> hashExpr
       <?> "atomic expressions"

--
-- Pattern
--

pattern :: Parser EgisonPattern
pattern = letPattern
      <|> try applyPattern
      <|> opPattern

letPattern :: Parser EgisonPattern
letPattern = do
  pos   <- keywordLet >> L.indentLevel
  binds <- some (L.indentGuard sc EQ pos *> binding)
  body  <- keywordIn >> pattern
  return $ LetPat binds body

applyPattern :: Parser EgisonPattern
applyPattern = do
  pos <- L.indentLevel
  func <- atomPattern
  args <- some (L.indentGuard sc GT pos *> atomPattern)
  case func of
    InductivePat x [] -> return $ InductivePat x args

opPattern :: Parser EgisonPattern
opPattern = makeExprParser atomPattern table
  where
    table :: [[Operator Parser EgisonPattern]]
    table =
      [ [ Prefix (NotPat <$ symbol "!") ]
      -- 5
      , [ InfixR (inductive2 "cons" ":" )
        , InfixR (inductive2 "join" "++") ]
      -- 3
      , [ InfixR (binary AndPat "&&") ]
      -- 2
      , [ InfixR (binary OrPat  "||") ]
      ]
    inductive2 name sym = (\x y -> InductivePat name [x, y]) <$ symbol sym
    binary name sym     = (\x y -> name [x, y]) <$ symbol sym

tupleOrParenPattern :: Parser EgisonPattern
tupleOrParenPattern = do
  elems <- parens $ sepBy pattern comma
  case elems of
    [p] -> return p
    _   -> return $ TuplePat elems

atomPattern :: Parser EgisonPattern
atomPattern = WildCard <$   symbol "_"
          <|> PatVar . stringToVar <$> (symbol "$" >> identifier)
          <|> ValuePat <$> (symbol "#" >> atomExpr)
          <|> InductivePat "nil" [] <$ (symbol "[" >> symbol "]")
          <|> InductivePat <$> identifier <*> pure []
          <|> VarPat   <$> (symbol "~" >> identifier)
          <|> PredPat  <$> (symbol "?" >> atomExpr)
          <|> tupleOrParenPattern

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
comma     = symbol ","
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

keywordLoadFile             = reserved "loadFile"
keywordLoad                 = reserved "load"
keywordIf                   = reserved "if"
keywordThen                 = reserved "then"
keywordElse                 = reserved "else"
keywordSeq                  = reserved "seq"
keywordApply                = reserved "apply"
keywordCApply               = reserved "capply"
keywordMemoizedLambda       = reserved "memoizedLambda"
keywordCambda               = reserved "cambda"
keywordProcedure            = reserved "procedure"
keywordMacro                = reserved "macro"
keywordLetRec               = reserved "letrec"
keywordLet                  = reserved "let"
keywordIn                   = reserved "in"
keywordWithSymbols          = reserved "withSymbols"
keywordLoop                 = reserved "loop"
keywordCont                 = reserved "..."
keywordMatch                = reserved "match"
keywordMatchDFS             = reserved "matchDFS"
keywordMatchAll             = reserved "matchAll"
keywordMatchAllDFS          = reserved "matchAllDFS"
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
  [ "loadFile"
  , "load"
  , "if"
  , "then"
  , "else"
  , "seq"
  , "apply"
  , "capply"
  , "memoizedLambda"
  , "cambda"
  , "procedure"
  , "macro"
  , "letrec"
  , "let"
  , "in"
  , "withSymbols"
  , "loop"
  , "..."
  , "match"
  , "matchDFS"
  , "matchAll"
  , "matchAllDFS"
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
  , "function"
  ]

reservedBinops :: [(String, String)]
reservedBinops = [ ("^",  "**"       )
                 , ("*",  "*"        )
                 , ("/",  "/"        )
                 , ("%",  "remainder")
                 , ("+",  "+"        )
                 , ("-",  "-"        )
                 , (":",  "cons"     )
                 , ("++", "append"   )
                 , ("==", "eq?"      )
                 , ("<=", "lte?"     )
                 , ("<",  "lt?"      )
                 , (">=", "gte?"     )
                 , (">",  "gt?"      )
                 , ("&&", "and"      )
                 , ("||", "or"       ) ]

--
-- Utils
--

parseOneOf :: [Parser a] -> Parser a
parseOneOf = foldl1 (\acc p -> acc <|> p)

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
