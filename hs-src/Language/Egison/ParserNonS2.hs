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

import           Control.Applicative            (pure, (*>), (<$>), (<$), (<*), (<*>))
import           Control.Monad.Except           (liftIO, throwError)
import           Control.Monad.State            (unless)
import           Prelude                        hiding (mapM)

import           System.Directory               (doesFileExist, getHomeDirectory)

import           Data.Functor                   (($>))
import           Data.List                      (find)
import           Data.Maybe                     (fromJust, fromMaybe)
import           Data.Traversable               (mapM)

import           Control.Monad.Combinators.Expr
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Debug          (dbg)
import           Text.Megaparsec.Pos            (Pos)

import           Data.Text                      (pack)

import           Language.Egison.Desugar
import           Language.Egison.Types
import           Paths_egison                   (getDataFileName)

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

type Parser = Parsec CustomError String

data CustomError
  = IllFormedPointFreeExpr String String
  deriving (Eq, Ord)

instance ShowErrorComponent CustomError where
  showErrorComponent (IllFormedPointFreeExpr op op') =
    "The operator " ++ info op ++ " must have lower precedence than " ++ info op'
    where
      info op =
        let priority = priorityOf op
            assoc = case assocOf op of
                      LeftAssoc -> "infixl"
                      RightAssoc -> "infixr"
                      NonAssoc -> "infix"
         in "'" ++ op ++ "' [" ++ assoc ++ " " ++ show priority ++ "]"


doParse :: Parser a -> String -> Either EgisonError a
doParse p input = either (throwError . fromParsecError) return $ parse p "egison" input
  where
    fromParsecError :: ParseErrorBundle String CustomError -> EgisonError
    fromParsecError = Parser . errorBundlePretty

--
-- Expressions
--

topExpr :: Parser EgisonTopExpr
topExpr = Load     <$> (keywordLoad >> stringLiteral)
      <|> LoadFile <$> (keywordLoadFile >> stringLiteral)
      <|> defineOrTestExpr
      <?> "toplevel expression"

defineOrTestExpr :: Parser EgisonTopExpr
defineOrTestExpr = do
  e <- expr
  (do symbol "="
      body <- expr
      return (convertToDefine e body))
      <|> return (Test e)
  where
    convertToDefine :: EgisonExpr -> EgisonExpr -> EgisonTopExpr
    convertToDefine (VarExpr var) body = Define var body
    convertToDefine (ApplyExpr (VarExpr var) (TupleExpr args)) body =
      Define var (LambdaExpr (map exprToArg args) body)

    -- TODO(momohatt): Handle other types of arg
    exprToArg :: EgisonExpr -> Arg
    exprToArg (VarExpr (Var [x] [])) = ScalarArg x

expr :: Parser EgisonExpr
expr = ifExpr
   <|> patternMatchExpr
   <|> lambdaExpr
   <|> letExpr
   <|> matcherExpr
   <|> algebraicDataMatcherExpr
   <|> dbg "opExpr" opExpr
   <?> "expression"

-- Also parses atomExpr
opExpr :: Parser EgisonExpr
opExpr = do
  pos <- L.indentLevel
  makeExprParser atomOrApplyExpr (makeTable pos)
  where
    makeTable :: Pos -> [[Operator Parser EgisonExpr]]
    makeTable pos =
      let unary  sym = UnaryOpExpr  <$> operator' sym
          binary sym = BinaryOpExpr <$> (L.indentGuard sc GT pos >> operator' sym)
       in
          [ [ Prefix (unary  "-" ) ]
          -- 8
          , [ InfixL (binary "^" ) ]
          -- 7
          , [ InfixL (binary "*" )
            , InfixL (binary "/" )
            , InfixL (binary "%" ) ]
          -- 6
          , [ InfixL (binary "+" )
            , InfixL (binary "-" ) ]
          -- 5
          , [ InfixR (binary ":" )
            , InfixR (binary "++") ]
          -- 4
          , [ InfixL (binary "==")
            , InfixL (binary "<=")
            , InfixL (binary "<" )
            , InfixL (binary ">=")
            , InfixL (binary ">" ) ]
          -- 3
          , [ InfixR (binary "&&") ]
          -- 2
          , [ InfixR (binary "||") ]
          ]


ifExpr :: Parser EgisonExpr
ifExpr = keywordIf >> IfExpr <$> expr <* keywordThen <*> expr <* keywordElse <*> expr

patternMatchExpr :: Parser EgisonExpr
patternMatchExpr = makeMatchExpr keywordMatch       MatchExpr
               <|> makeMatchExpr keywordMatchDFS    MatchDFSExpr
               <|> makeMatchExpr keywordMatchAll    MatchAllExpr
               <|> makeMatchExpr keywordMatchAllDFS MatchAllDFSExpr
               <?> "pattern match expression"
  where
    makeMatchExpr keyword ctor = do
      tgt     <- keyword >> expr
      matcher <- keywordAs >> expr <* keywordWith
      clauses <- matchClauses1
      return $ ctor tgt matcher clauses

-- Parse more than 1 match clauses.
matchClauses1 :: Parser [MatchClause]
matchClauses1 = do
  pos <- optional (symbol "|") >> L.indentLevel
  (:) <$> matchClause pos <*> matchClauses pos
  where
    matchClauses :: Pos -> Parser [MatchClause]
    matchClauses pos = try ((:) <$> (symbol "|" >> matchClause pos) <*> matchClauses pos)
                   <|> return []
    matchClause :: Pos -> Parser MatchClause
    matchClause pos = (,) <$> (L.indentGuard sc EQ pos *> pattern) <*> (symbol "->" >> expr)

lambdaExpr :: Parser EgisonExpr
lambdaExpr = symbol "\\" >> (
      makeMatchLambdaExpr keywordMatch    MatchLambdaExpr
  <|> makeMatchLambdaExpr keywordMatchAll MatchAllLambdaExpr
  <|> try (LambdaExpr <$> some arg <*> (symbol "->" >> expr))
  <|> PatternFunctionExpr <$> some lowerId <*> (symbol "=>" >> pattern))
  <?> "lambda or pattern function expression"
  where
    makeMatchLambdaExpr keyword ctor = do
      matcher <- keyword >> keywordAs >> expr
      clauses <- keywordWith >> matchClauses1
      return $ ctor matcher clauses

arg :: Parser Arg
arg = ScalarArg         <$> (symbol "$"  >> lowerId)
  <|> InvertedScalarArg <$> (symbol "*$" >> lowerId)
  <|> TensorArg         <$> (symbol "%"  >> lowerId)
  <|> ScalarArg         <$> lowerId
  <?> "argument"

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

matcherExpr :: Parser EgisonExpr
matcherExpr = do
  keywordMatcher
  pos <- L.indentLevel
  -- In matcher expression, the first '|' (bar) is indispensable
  info <- some (L.indentGuard sc EQ pos >> symbol "|" >> patternDef)
  return $ MatcherExpr info
  where
    patternDef :: Parser (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
    patternDef = do
      pp <- ppPattern
      returnMatcher <- keywordAs >> expr <* keywordWith
      pos <- L.indentLevel
      datapat <- some (L.indentGuard sc EQ pos >> symbol "|" >> dataCases)
      return (pp, returnMatcher, datapat)

    dataCases :: Parser (PrimitiveDataPattern, EgisonExpr)
    dataCases = (,) <$> pdPattern <*> (symbol "->" >> expr)

algebraicDataMatcherExpr :: Parser EgisonExpr
algebraicDataMatcherExpr = do
  keywordAlgebraicDataMatcher
  pos <- L.indentLevel
  defs <- some (L.indentGuard sc EQ pos >> symbol "|" >> patternDef)
  return $ AlgebraicDataMatcherExpr defs
  where
    patternDef :: Parser (String, [EgisonExpr])
    patternDef = do
      pos <- L.indentLevel
      patternCtor <- lowerId
      args <- many (L.indentGuard sc GT pos >> atomExpr)
      return (patternCtor, args)

collectionExpr :: Parser EgisonExpr
collectionExpr = symbol "[" >> (try betweenOrFromExpr <|> elementsExpr)
  where
    betweenOrFromExpr = do
      start <- expr <* symbol ".."
      end   <- optional expr <* symbol "]"
      case end of
        Just end' -> return $ makeApply' "between" [start, end']
        Nothing   -> return $ makeApply' "from" [start]

    elementsExpr = CollectionExpr <$> (sepBy (ElementExpr <$> expr) comma <* symbol "]")

tupleOrParenExpr :: Parser EgisonExpr
tupleOrParenExpr = do
  elems <- parens $ pointFreeExpr <|> sepBy expr comma
  case elems of
    [x] -> return x
    _   -> return $ TupleExpr elems
  where
    pointFreeExpr :: Parser [EgisonExpr]
    pointFreeExpr =
          (do op   <- try . choice $ map (operator . repr) reservedBinops
              rarg <- optional $ expr
              case rarg of
                Just (BinaryOpExpr op' _ _) | priorityOf op >= priorityOf op' ->
                  customFailure (IllFormedPointFreeExpr op op')
                _ -> return [makeLambda op Nothing rarg])
      <|> (do (larg, op) <- try $ (,) <$> opExpr <*> (choice $ map (operator . repr) reservedBinops)
              case larg of
                BinaryOpExpr op' _ _ | priorityOf op >= priorityOf op' ->
                  customFailure (IllFormedPointFreeExpr op op')
                _ -> return [makeLambda op (Just larg) Nothing])

    makeLambda :: String -> Maybe EgisonExpr -> Maybe EgisonExpr -> EgisonExpr
    makeLambda op Nothing Nothing =
      LambdaExpr [ScalarArg ":x", ScalarArg ":y"]
                 (BinaryOpExpr op (stringToVarExpr ":x") (stringToVarExpr ":y"))
    makeLambda op Nothing (Just rarg) =
      LambdaExpr [ScalarArg ":x"] (BinaryOpExpr op (stringToVarExpr ":x") rarg)
    makeLambda op (Just larg) Nothing =
      LambdaExpr [ScalarArg ":y"] (BinaryOpExpr op larg (stringToVarExpr ":y"))

hashExpr :: Parser EgisonExpr
hashExpr = HashExpr <$> hashBraces (sepEndBy hashElem comma)
  where
    hashBraces = between (symbol "{|") (symbol "|}")
    hashElem = brackets $ (,) <$> expr <*> (comma >> expr)

index :: Parser (Index EgisonExpr)
index = SupSubscript <$> (string "~_" >> atomExpr')
    <|> try (char '_' >> subscript)
    <|> try (char '~' >> superscript)
    <|> try (Userscript <$> (char '|' >> atomExpr'))
    <?> "index"
  where
    subscript = do
      e1 <- atomExpr'
      e2 <- optional (string "..._" >> atomExpr')
      case e2 of
        Nothing  -> return $ Subscript e1
        Just e2' -> return $ MultiSubscript e1 e2'
    superscript = do
      e1 <- atomExpr'
      e2 <- optional (string "...~" >> atomExpr')
      case e2 of
        Nothing  -> return $ Superscript e1
        Just e2' -> return $ MultiSuperscript e1 e2'

atomOrApplyExpr :: Parser EgisonExpr
atomOrApplyExpr = do
  pos <- L.indentLevel
  func <- atomExpr
  args <- many (L.indentGuard sc GT pos *> atomExpr)
  return $ case args of
             [] -> func
             _  -> makeApply func args

atomExpr :: Parser EgisonExpr
atomExpr = do
  e <- atomExpr'
  -- TODO(momohatt): "..." (override of index) collides with ContPat
  indices <- many index
  case indices of
    [] -> return e
    _  -> return $ IndexedExpr False e indices

-- atom expr without index
atomExpr' :: Parser EgisonExpr
atomExpr' = numericExpr
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
        <?> "atomic expression"

numericExpr :: Parser EgisonExpr
numericExpr = try (uncurry FloatExpr <$> floatLiteral)
          <|> IntegerExpr <$> positiveIntegerLiteral
          <?> "numeric expression"
--
-- Pattern
--

pattern :: Parser EgisonPattern
pattern = letPattern
      <|> loopPattern
      <|> try applyPattern
      <|> opPattern
      <?> "pattern"

letPattern :: Parser EgisonPattern
letPattern = do
  pos   <- keywordLet >> L.indentLevel
  binds <- some (L.indentGuard sc EQ pos *> binding)
  body  <- keywordIn >> pattern
  return $ LetPat binds body

loopPattern :: Parser EgisonPattern
loopPattern = do
  keywordLoop
  iter <- patVarLiteral
  range <- loopRange
  loopBody <- optional (symbol "|") >> pattern
  loopEnd <- symbol "|" >> pattern
  return $ LoopPat iter range loopBody loopEnd
  where
    loopRange :: Parser LoopRange
    loopRange =
      try (parens $
           do start <- expr
              ends  <- fromMaybe (defaultEnds start) <$> optional (try $ comma >> expr)
              as    <- fromMaybe WildCard <$> optional (comma >> pattern)
              return $ LoopRange start ends as)
      <|> (do start <- keywordFrom >> expr
              ends  <- fromMaybe (defaultEnds start) <$> optional (keywordTo >> expr)
              as    <- fromMaybe WildCard <$> optional (keywordAs >> pattern)
              keywordOf
              return $ LoopRange start ends as)

    defaultEnds s =
      ApplyExpr (stringToVarExpr "from")
                (makeApply (stringToVarExpr "-'") [s, IntegerExpr 1])

seqPattern :: Parser EgisonPattern
seqPattern = do
  pats <- braces $ sepBy pattern comma
  return $ foldr SeqConsPat SeqNilPat pats

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
    inductive2 name sym = (\x y -> InductivePat name [x, y]) <$ operator sym
    binary name sym     = (\x y -> name [x, y]) <$ operator sym

atomPattern :: Parser EgisonPattern
atomPattern = WildCard <$   symbol "_"
          <|> PatVar   <$> patVarLiteral
          <|> ValuePat <$> (char '#' >> atomExpr)
          <|> InductivePat "nil" [] <$ (symbol "[" >> symbol "]")
          <|> InductivePat <$> lowerId <*> pure []
          <|> VarPat   <$> (char '~' >> lowerId)
          <|> PredPat  <$> (symbol "?" >> atomExpr)
          <|> ContPat  <$ symbol "..."
          <|> makeTupleOrParen pattern TuplePat
          <|> seqPattern
          <|> LaterPatVar <$ symbol "@"
          <?> "atomic pattern"

patVarLiteral :: Parser Var
patVarLiteral = stringToVar <$> (char '$' >> lowerId)

ppPattern :: Parser PrimitivePatPattern
ppPattern = PPInductivePat <$> lowerId <*> many ppAtom
        <|> makeExprParser ppAtom table
        <?> "primitive pattern pattern"
  where
    table :: [[Operator Parser PrimitivePatPattern]]
    table =
      [ [ InfixR (inductive2 "cons" ":" )
        , InfixR (inductive2 "join" "++") ]
      ]
    inductive2 name sym = (\x y -> PPInductivePat name [x, y]) <$ operator sym

    ppAtom :: Parser PrimitivePatPattern
    ppAtom = PPWildCard <$ symbol "_"
         <|> PPPatVar   <$ symbol "$"
         <|> PPValuePat <$> (symbol "#$" >> lowerId)
         <|> PPInductivePat "nil" [] <$ brackets sc
         <|> makeTupleOrParen ppPattern PPTuplePat

-- TODO(momohatt): cons pat, snoc pat, empty pat, constant pat
pdPattern :: Parser PrimitiveDataPattern
pdPattern = PDInductivePat <$> upperId <*> many pdAtom
        <|> pdAtom
        <?> "primitive data pattern"
  where
    pdAtom :: Parser PrimitiveDataPattern
    pdAtom = PDWildCard <$ symbol "_"
         <|> PDPatVar   <$> (symbol "$" >> lowerId)
         <|> makeTupleOrParen pdPattern PDTuplePat

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

positiveIntegerLiteral :: Parser Integer
positiveIntegerLiteral = lexeme L.decimal
                     <?> "unsinged integer"

charLiteral :: Parser Char
charLiteral = between (char '\'') (symbol "\'") L.charLiteral
          <?> "character"

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (symbol "\"")
          <?> "string"

boolLiteral :: Parser Bool
boolLiteral = reserved "True"  $> True
          <|> reserved "False" $> False
          <?> "boolean"

floatLiteral :: Parser (Double, Double)
floatLiteral = try ((,0) <$> (lexeme L.float <* notFollowedBy (symbol "i")))
                <|> (0,) <$> (lexeme L.float <* symbol "i")
                <?> "float"

varLiteral :: Parser Var
varLiteral = stringToVar <$> lowerId

reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

symbol :: String -> Parser String
symbol sym = try $ L.symbol sc sym

-- Ensures that the next character isn't any of the characters that could consist other operators
-- ! # @ $ are omitted because they can appear at the beginning of atomPattern
operator :: String -> Parser String
operator sym = try $ string sym <* notFollowedBy (oneOf opChars) <* sc
  where
    opChars :: [Char]
    opChars = "%^&*-+\\|:<>.?/'"

-- Mostly same as `operator`, but doesn't allow closing symbols (')', ']', ...) to follow
operator' :: String -> Parser String
operator' sym = try $ string sym <* notFollowedBy (oneOf opChars) <* sc
  where
    opChars :: [Char]
    opChars = "%^&*-+\\|:<>.?/')]}"

parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
brackets  = between (symbol "[") (symbol "]")
comma     = symbol ","

lowerId :: Parser String
lowerId = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> lowerChar <*> many (alphaNumChar <|> oneOf ['?', '\''])
    check x = if x `elem` lowerReservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

-- TODO: Deprecate BoolExpr and merge it with InductiveDataExpr
upperId :: Parser String
upperId = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> upperChar <*> many alphaNumChar
    check x = if x `elem` upperReservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

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
keywordFrom                 = reserved "from"
keywordTo                   = reserved "to"
keywordOf                   = reserved "of"
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
  , "from"
  , "to"
  , "of"
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

--
-- Utils
--

makeTupleOrParen :: Parser a -> ([a] -> a) -> Parser a
makeTupleOrParen parser tupleCtor = do
  elems <- parens $ sepBy parser comma
  case elems of
    [elem] -> return elem
    _      -> return $ tupleCtor elems

makeApply :: EgisonExpr -> [EgisonExpr] -> EgisonExpr
makeApply (InductiveDataExpr x []) xs = InductiveDataExpr x xs
makeApply func xs = ApplyExpr func (TupleExpr xs)

makeApply' :: String -> [EgisonExpr] -> EgisonExpr
makeApply' func xs = ApplyExpr (stringToVarExpr func) (TupleExpr xs)

priorityOf :: String -> Int
priorityOf op = priority . fromJust $ find ((== op) . repr) reservedBinops

assocOf :: String -> BinOpAssoc
assocOf op = assoc . fromJust $ find ((== op) . repr) reservedBinops
