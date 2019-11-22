{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE MultiWayIf       #-}

{- |
Module      : Language.Egison.ParserNonS
Copyright   : Satoshi Egi
Licence     : MIT

This module provides the new parser of Egison.
-}

module Language.Egison.ParserNonS
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

import           Prelude                        hiding (mapM)

import           Control.Applicative            (pure, (*>), (<$>), (<$), (<*), (<*>))
import           Control.Monad.Except           (liftIO, throwError)
import           Control.Monad.State            (unless)

import           Data.Functor                   (($>))
import           Data.List                      (find)
import           Data.Maybe                     (fromJust, isJust)
import           Data.Text                      (pack)
import           Data.Traversable               (mapM)

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Debug          (dbg)
import           Text.Megaparsec.Pos            (Pos)
import           System.Directory               (doesFileExist, getHomeDirectory)
import           System.IO

import           Language.Egison.AST
import           Language.Egison.Desugar
import           Language.Egison.Types
import           Paths_egison                   (getDataFileName)

readTopExprs :: String -> EgisonM [EgisonTopExpr]
readTopExprs = either throwError (mapM desugarTopExpr) . parseTopExprs

readTopExpr :: String -> EgisonM EgisonTopExpr
readTopExpr = either throwError desugarTopExpr . parseTopExpr

readExprs :: String -> EgisonM [EgisonExpr]
readExprs = either throwError (mapM desugarExpr) . parseExprs

readExpr :: String -> EgisonM EgisonExpr
readExpr = either throwError desugarExpr . parseExpr

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

readUTF8File :: FilePath -> IO String
readUTF8File name = do
  h <- openFile name ReadMode
  hSetEncoding h utf8
  hGetContents h

--
-- Parser
--

type Parser = Parsec CustomError String

data CustomError
  = IllFormedPointFreeExpr EgisonBinOp EgisonBinOp
  | IllFormedDefine
  deriving (Eq, Ord)

instance ShowErrorComponent CustomError where
  showErrorComponent (IllFormedPointFreeExpr op op') =
    "The operator " ++ info op ++ " must have lower precedence than " ++ info op'
    where
      info op =
         "'" ++ repr op ++ "' [" ++ show (assoc op) ++ " " ++ show (priority op) ++ "]"
  showErrorComponent IllFormedDefine =
    "Ill-formed definition syntax."


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
  (do symbol ":="
      body <- expr
      return $ convertToDefine e body)
      <|> return (Test e)
  where
    -- TODO: Throw IllFormedDefine in pattern match failure.
    -- first 2 cases are the most common ones
    convertToDefine :: EgisonExpr -> EgisonExpr -> EgisonTopExpr
    convertToDefine (VarExpr var) body = Define var body
    convertToDefine (ApplyExpr (VarExpr var) (TupleExpr args)) body =
      Define var (LambdaExpr (map exprToArg args) body)
    convertToDefine e@(BinaryOpExpr op _ _) body
      | repr op == "*" || repr op == "%" =
        case exprToArgs e of
          ScalarArg var : args -> Define (Var [var] []) (LambdaExpr args body)

    exprToArg :: EgisonExpr -> Arg
    exprToArg (VarExpr (Var [x] [])) = ScalarArg x

    exprToArgs :: EgisonExpr -> [Arg]
    exprToArgs (VarExpr (Var [x] [])) = [ScalarArg x]
    exprToArgs (ApplyExpr func (TupleExpr args)) =
      exprToArgs func ++ map exprToArg args
    exprToArgs (BinaryOpExpr op lhs rhs) | repr op == "*" =
      case exprToArgs rhs of
        ScalarArg x : xs -> exprToArgs lhs ++ InvertedScalarArg x : xs
    exprToArgs (BinaryOpExpr op lhs rhs) | repr op == "%" =
      case exprToArgs rhs of
        ScalarArg x : xs -> exprToArgs lhs ++ TensorArg x : xs

expr :: Parser EgisonExpr
expr = ifExpr
   <|> patternMatchExpr
   <|> lambdaExpr
   <|> letExpr
   <|> withSymbolsExpr
   <|> doExpr
   <|> ioExpr
   <|> matcherExpr
   <|> algebraicDataMatcherExpr
   <|> memoizedLambdaExpr
   <|> procedureExpr
   <|> macroExpr
   <|> generateTensorExpr
   <|> tensorExpr
   <|> functionExpr
   <|> opExpr
   <?> "expression"

-- Also parses atomExpr
opExpr :: Parser EgisonExpr
opExpr = do
  pos <- L.indentLevel
  makeExprParser atomOrApplyExpr (makeTable pos)
  where
    makeTable :: Pos -> [[Operator Parser EgisonExpr]]
    makeTable pos =
      let unary  sym = UnaryOpExpr  <$> operator sym
          binary sym = do
            op <- try (L.indentGuard sc GT pos >> binOpLiteral sym <* notFollowedBy (symbol ")"))
            return $ BinaryOpExpr op
       in
          [ [ Prefix (unary  "-" )
            , Prefix (unary  "!" ) ]
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
          , [ InfixR (binary "::" )
            , InfixR (binary "++") ]
          -- 4
          , [ InfixL (binary "=")
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
patternMatchExpr = makeMatchExpr keywordMatch       (MatchExpr BFSMode)
               <|> makeMatchExpr keywordMatchDFS    (MatchExpr DFSMode)
               <|> makeMatchExpr keywordMatchAll    (MatchAllExpr BFSMode)
               <|> makeMatchExpr keywordMatchAllDFS (MatchAllExpr DFSMode)
               <?> "pattern match expression"
  where
    makeMatchExpr keyword ctor = ctor <$> (keyword >> expr)
                                      <*> (keywordAs >> expr)
                                      <*> (keywordWith >> matchClauses1)

-- Parse more than 1 match clauses.
matchClauses1 :: Parser [MatchClause]
matchClauses1 = do
  pos <- L.indentLevel
  -- If the first bar '|' is missing, then it is expected to have only one match clause.
  (lookAhead (symbol "|") >> some (matchClause pos)) <|> (:[]) <$> matchClauseWithoutBar
  where
    matchClauseWithoutBar :: Parser MatchClause
    matchClauseWithoutBar = (,) <$> pattern <*> (symbol "->" >> expr)

    matchClause :: Pos -> Parser MatchClause
    matchClause pos = (,) <$> (L.indentGuard sc EQ pos >> symbol "|" >> pattern) <*> (symbol "->" >> expr)

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
arg = InvertedScalarArg <$> (symbol "*" >> lowerId)
  <|> TensorArg         <$> (symbol "%" >> lowerId)
  <|> ScalarArg         <$> lowerId
  <?> "argument"

letExpr :: Parser EgisonExpr
letExpr = do
  pos   <- keywordLet >> L.indentLevel
  binds <- some (L.indentGuard sc EQ pos *> binding)
  body  <- keywordIn >> expr
  return $ LetRecExpr binds body

binding :: Parser BindingExpr
binding = do
  (vars, args) <- (,[]) <$> parens (sepBy varLiteral comma)
              <|> do var <- varLiteral
                     args <- many arg
                     return ([var], args)
  body <- symbol ":=" >> expr
  return $ case args of
             [] -> (vars, body)
             _  -> (vars, LambdaExpr args body)

withSymbolsExpr :: Parser EgisonExpr
withSymbolsExpr = WithSymbolsExpr <$> (keywordWithSymbols >> brackets (sepBy lowerId comma)) <*> expr

doExpr :: Parser EgisonExpr
doExpr = do
  pos   <- keywordDo >> L.indentLevel
  stmts <- some $ L.indentGuard sc EQ pos >> statement
  ret   <- option (makeApply' "return" []) $ L.indentGuard sc EQ pos >> expr
  return $ DoExpr stmts ret
  where
    statement :: Parser BindingExpr
    statement = (keywordLet >> binding) <|> ([],) <$> expr

ioExpr :: Parser EgisonExpr
ioExpr = IoExpr <$> (keywordIo >> expr)

matcherExpr :: Parser EgisonExpr
matcherExpr = do
  keywordMatcher
  pos  <- L.indentLevel
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
  pos  <- L.indentLevel
  defs <- some (L.indentGuard sc EQ pos >> symbol "|" >> patternDef)
  return $ AlgebraicDataMatcherExpr defs
  where
    patternDef :: Parser (String, [EgisonExpr])
    patternDef = do
      pos <- L.indentLevel
      patternCtor <- lowerId
      args <- many (L.indentGuard sc GT pos >> atomExpr)
      return (patternCtor, args)

memoizedLambdaExpr :: Parser EgisonExpr
memoizedLambdaExpr = MemoizedLambdaExpr <$> (keywordMemoizedLambda >> many lowerId) <*> (symbol "->" >> expr)

procedureExpr :: Parser EgisonExpr
procedureExpr = ProcedureExpr <$> (keywordProcedure >> many lowerId) <*> (symbol "->" >> expr)

macroExpr :: Parser EgisonExpr
macroExpr = MacroExpr <$> (keywordMacro >> many lowerId) <*> (symbol "->" >> expr)

generateTensorExpr :: Parser EgisonExpr
generateTensorExpr = GenerateTensorExpr <$> (keywordGenerateTensor >> atomExpr) <*> atomExpr

tensorExpr :: Parser EgisonExpr
tensorExpr = TensorExpr <$> (keywordTensor >> atomExpr) <*> atomExpr
                        <*> option (CollectionExpr []) atomExpr
                        <*> option (CollectionExpr []) atomExpr

functionExpr :: Parser EgisonExpr
functionExpr = FunctionExpr <$> (keywordFunction >> parens (sepBy expr comma))

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
  elems <- symbol "(" >> try (sepBy expr comma <* symbol ")") <|> (pointFreeExpr <* symbol ")")
  case elems of
    [x] -> return x
    _   -> return $ TupleExpr elems
  where
    pointFreeExpr :: Parser [EgisonExpr]
    pointFreeExpr =
          (do op   <- try . choice $ map (binOpLiteral . repr) reservedBinops
              rarg <- optional expr
              -- TODO(momohatt): Take associativity of operands into account
              case rarg of
                Just (BinaryOpExpr op' _ _) | priority op >= priority op' ->
                  customFailure (IllFormedPointFreeExpr op op')
                _ -> return [makeLambda op Nothing rarg])
      <|> (do larg <- opExpr
              op   <- choice $ map (binOpLiteral . repr) reservedBinops
              case larg of
                BinaryOpExpr op' _ _ | priority op >= priority op' ->
                  customFailure (IllFormedPointFreeExpr op op')
                _ -> return [makeLambda op (Just larg) Nothing])

    makeLambda :: EgisonBinOp -> Maybe EgisonExpr -> Maybe EgisonExpr -> EgisonExpr
    makeLambda op Nothing Nothing =
      LambdaExpr [ScalarArg ":x", ScalarArg ":y"]
                 (BinaryOpExpr op (stringToVarExpr ":x") (stringToVarExpr ":y"))
    makeLambda op Nothing (Just rarg) =
      LambdaExpr [ScalarArg ":x"] (BinaryOpExpr op (stringToVarExpr ":x") rarg)
    makeLambda op (Just larg) Nothing =
      LambdaExpr [ScalarArg ":y"] (BinaryOpExpr op larg (stringToVarExpr ":y"))

arrayExpr :: Parser EgisonExpr
arrayExpr = ArrayExpr <$> between (symbol "(|") (symbol "|)") (sepEndBy expr comma)

vectorExpr :: Parser EgisonExpr
vectorExpr = VectorExpr <$> between (symbol "[|") (symbol "|]") (sepEndBy expr comma)

hashExpr :: Parser EgisonExpr
hashExpr = HashExpr <$> hashBraces (sepEndBy hashElem comma)
  where
    hashBraces = between (symbol "{|") (symbol "|}")
    hashElem = parens $ (,) <$> expr <*> (comma >> expr)

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
  return $ case indices of
             [] -> e
             _  -> IndexedExpr False e indices

-- atom expr without index
atomExpr' :: Parser EgisonExpr
atomExpr' = constantExpr
        <|> VarExpr <$> varLiteral
        <|> (\x -> InductiveDataExpr x []) <$> upperId
        <|> vectorExpr     -- must come before collectionExpr
        <|> arrayExpr      -- must come before tupleOrParenExpr
        <|> collectionExpr
        <|> tupleOrParenExpr
        <|> hashExpr
        <|> QuoteExpr <$> (char '\'' >> atomExpr')
        <|> QuoteSymbolExpr <$> (char '`' >> atomExpr')
        <?> "atomic expression"

constantExpr :: Parser EgisonExpr
constantExpr = numericExpr
           <|> BoolExpr <$> boolLiteral
           <|> CharExpr <$> try charLiteral        -- try for quoteExpr
           <|> StringExpr . pack <$> stringLiteral
           <|> SomethingExpr <$ keywordSomething
           <|> UndefinedExpr <$ keywordUndefined

numericExpr :: Parser EgisonExpr
numericExpr = FloatExpr <$> try positiveFloatLiteral
          <|> IntegerExpr <$> positiveIntegerLiteral
          <?> "numeric expression"
--
-- Pattern
--

pattern :: Parser EgisonPattern
pattern = letPattern
      <|> loopPattern
      <|> opPattern
      <?> "pattern"

letPattern :: Parser EgisonPattern
letPattern = do
  pos   <- keywordLet >> L.indentLevel
  binds <- some (L.indentGuard sc EQ pos *> binding)
  body  <- keywordIn >> pattern
  return $ LetPat binds body

loopPattern :: Parser EgisonPattern
loopPattern =
  LoopPat <$> (keywordLoop >> patVarLiteral) <*> loopRange
          <*> atomPattern <*> atomPattern
  where
    loopRange :: Parser LoopRange
    loopRange =
      parens $ do start <- expr
                  ends  <- option (defaultEnds start) (try $ comma >> expr)
                  as    <- option WildCard (comma >> pattern)
                  return $ LoopRange start ends as

    defaultEnds s =
      ApplyExpr (stringToVarExpr "from")
                (makeApply (stringToVarExpr "-'") [s, IntegerExpr 1])

seqPattern :: Parser EgisonPattern
seqPattern = do
  pats <- braces $ sepBy pattern comma
  return $ foldr SeqConsPat SeqNilPat pats

opPattern :: Parser EgisonPattern
opPattern = makeExprParser applyOrAtomPattern table
  where
    table :: [[Operator Parser EgisonPattern]]
    table =
      [ [ Prefix (NotPat <$ symbol "!") ]
      -- 5
      , [ InfixR (inductive2 "cons" "::" )
        , InfixR (inductive2 "join" "++") ]
      -- 3
      , [ InfixR (binary AndPat "&") ]
      -- 2
      , [ InfixR (binary OrPat  "|") ]
      ]
    inductive2 name sym = (\x y -> InductivePat name [x, y]) <$ patOperator sym
    binary name sym     = (\x y -> name [x, y]) <$ patOperator sym

applyOrAtomPattern :: Parser EgisonPattern
applyOrAtomPattern = do
  pos <- L.indentLevel
  func <- atomPattern
  args <- many (L.indentGuard sc GT pos *> atomPattern)
  case (func, args) of
    (_,                 []) -> return func
    (InductivePat x [], _)  -> return $ InductivePat x args

atomPattern :: Parser EgisonPattern
atomPattern = do
  pat     <- atomPattern'
  indices <- many . try $ char '_' >> atomExpr'
  return $ case indices of
             [] -> pat
             _  -> IndexedPat pat indices

-- atomic pattern without index
atomPattern' :: Parser EgisonPattern
atomPattern' = WildCard <$   symbol "_"
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

ppPattern :: Parser PrimitivePatPattern
ppPattern = PPInductivePat <$> lowerId <*> many ppAtom
        <|> makeExprParser ppAtom table
        <?> "primitive pattern pattern"
  where
    table :: [[Operator Parser PrimitivePatPattern]]
    table =
      [ [ InfixR (inductive2 "cons" "::" )
        , InfixR (inductive2 "join" "++") ]
      ]
    inductive2 name sym = (\x y -> PPInductivePat name [x, y]) <$ operator sym

    ppAtom :: Parser PrimitivePatPattern
    ppAtom = PPWildCard <$ symbol "_"
         <|> PPPatVar   <$ symbol "$"
         <|> PPValuePat <$> (symbol "#$" >> lowerId)
         <|> PPInductivePat "nil" [] <$ brackets sc
         <|> makeTupleOrParen ppPattern PPTuplePat

pdPattern :: Parser PrimitiveDataPattern
pdPattern = PDInductivePat <$> upperId <*> many pdAtom
        <|> PDSnocPat <$> (symbol "snoc" >> pdAtom) <*> pdAtom
        <|> makeExprParser pdAtom table
        <?> "primitive data pattern"
  where
    table :: [[Operator Parser PrimitiveDataPattern]]
    table =
      [ [ InfixR (PDConsPat <$ symbol "::") ]
      ]
    pdAtom :: Parser PrimitiveDataPattern
    pdAtom = PDWildCard    <$ symbol "_"
         <|> PDPatVar      <$> (symbol "$" >> lowerId)
         <|> PDConstantPat <$> constantExpr
         <|> PDEmptyPat    <$ (symbol "[" >> symbol "]")
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

positiveFloatLiteral :: Parser Double
positiveFloatLiteral = lexeme L.float
           <?> "unsigned float"

varLiteral :: Parser Var
varLiteral = stringToVar <$> lowerId

patVarLiteral :: Parser Var
patVarLiteral = stringToVar <$> (char '$' >> lowerId)

binOpLiteral :: String -> Parser EgisonBinOp
binOpLiteral sym = do
  wedge <- optional (char '!')
  opSym <- operator sym
  let opInfo = fromJust $ find ((== opSym) . repr) reservedBinops
  return $ opInfo { isWedge = isJust wedge }

reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy identChar)

symbol :: String -> Parser String
symbol sym = try $ L.symbol sc sym

operator :: String -> Parser String
operator sym = try $ string sym <* notFollowedBy opChar <* sc

patOperator :: String -> Parser String
patOperator sym = try $ string sym <* notFollowedBy patOpChar <* sc

-- Characters that could consist expression operators.
opChar :: Parser Char
opChar = oneOf "%^&*-+\\|:<>.?/'!#@$"

-- Characters that could consist pattern operators.
-- ! # @ $ are omitted because they can appear at the beginning of atomPattern
patOpChar :: Parser Char
patOpChar = oneOf "%^&*-+\\|:<>.?/'"

-- Characters that consist identifiers
identChar :: Parser Char
identChar = alphaNumChar <|> oneOf ['.', '?', '\'', '/']

parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
brackets  = between (symbol "[") (symbol "]")
comma     = symbol ","

lowerId :: Parser String
lowerId = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> lowerChar <*> many identChar
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
keywordLet                  = reserved "let"
keywordIn                   = reserved "in"
keywordWithSymbols          = reserved "withSymbols"
keywordLoop                 = reserved "loop"
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
