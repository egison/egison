{-# LANGUAGE TupleSections    #-}

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

import           Control.Applicative            (pure, (*>), (<$>), (<$), (<*), (<*>))
import           Control.Monad.Except           (liftIO, throwError)
import           Control.Monad.State            (unless)

import           Data.Char                      (isAsciiUpper, isLetter)
import           Data.Functor                   (($>))
import           Data.List                      (find, groupBy)
import           Data.Maybe                     (fromJust, isJust)
import           Data.Text                      (pack)

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
-- import           Text.Megaparsec.Debug          (dbg)
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
parseTopExpr = doParse $ sc >> topExpr <* eof

parseExprs :: String -> Either EgisonError [EgisonExpr]
parseExprs = doParse $ many (L.nonIndented sc expr) <* eof

parseExpr :: String -> Either EgisonError EgisonExpr
parseExpr = doParse $ sc >> expr <* eof

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
  concat <$> mapM recursiveLoad exprs
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
  = IllFormedSection EgisonBinOp EgisonBinOp
  | IllFormedDefine
  deriving (Eq, Ord)

instance ShowErrorComponent CustomError where
  showErrorComponent (IllFormedSection op op') =
    "The operator " ++ info op ++ " must have lower precedence than " ++ info op'
    where
      info op =
         "'" ++ repr op ++ "' [" ++ show (assoc op) ++ " " ++ show (priority op) ++ "]"
  showErrorComponent IllFormedDefine =
    "Failed to parse the left hand side of definition expression."


doParse :: Parser a -> String -> Either EgisonError a
doParse p input = either (throwError . fromParsecError) return $ parse p "egison" input
  where
    fromParsecError :: ParseErrorBundle String CustomError -> EgisonError
    fromParsecError = Parser . errorBundlePretty

--
-- Expressions
--

topExpr :: Parser EgisonTopExpr
topExpr = Load     <$> (reserved "load" >> stringLiteral)
      <|> LoadFile <$> (reserved "loadFile" >> stringLiteral)
      <|> defineOrTestExpr
      <?> "toplevel expression"

-- Return type of |convertToDefine|.
data ConversionResult
  = Variable Var        -- Definition of a variable with no arguments on lhs.
  | Function Var [Arg]  -- Definition of a function with some arguments on lhs.
  | IndexedVar VarWithIndices

defineOrTestExpr :: Parser EgisonTopExpr
defineOrTestExpr = do
  e <- expr
  defineExpr e <|> return (Test e)
  where
    defineExpr :: EgisonExpr -> Parser EgisonTopExpr
    defineExpr e = do
      _    <- symbol ":="
      -- When ":=" is observed and the current expression turns out to be a
      -- definition, we do not start over from scratch but re-interpret
      -- what's parsed so far as the lhs of definition.
      case convertToDefine e of
        Nothing -> customFailure IllFormedDefine
        Just (Variable var)      -> Define var <$> expr
        Just (Function var args) -> Define var . LambdaExpr args <$> expr
        Just (IndexedVar var)    -> DefineWithIndices var <$> expr

    convertToDefine :: EgisonExpr -> Maybe ConversionResult
    convertToDefine (VarExpr var) = return $ Variable var
    convertToDefine (ApplyExpr (VarExpr var) (TupleExpr args)) = do
      args' <- mapM ((ScalarArg <$>) . exprToStr) args
      return $ Function var args'
    convertToDefine e@(BinaryOpExpr op _ _)
      | repr op == "*" || repr op == "%" = do
        args <- exprToArgs e
        case args of
          ScalarArg var : args -> return $ Function (Var [var] []) args
          _                    -> Nothing
    convertToDefine (IndexedExpr True (VarExpr (Var var [])) indices) = do
      -- [Index EgisonExpr] -> Maybe [Index String]
      indices' <- mapM (traverse exprToStr) indices
      return $ IndexedVar (VarWithIndices var indices')
    convertToDefine _ = Nothing

    exprToStr :: EgisonExpr -> Maybe String
    exprToStr (VarExpr (Var [x] [])) = Just x
    exprToStr _                      = Nothing

    exprToArgs :: EgisonExpr -> Maybe [Arg]
    exprToArgs (VarExpr (Var [x] [])) = return [ScalarArg x]
    exprToArgs (ApplyExpr func (TupleExpr args)) =
      (++) <$> exprToArgs func <*> mapM ((ScalarArg <$>) . exprToStr) args
    exprToArgs (BinaryOpExpr op lhs rhs) | repr op == "*" = do
      lhs' <- exprToArgs lhs
      rhs' <- exprToArgs rhs
      case rhs' of
        ScalarArg x : xs -> return (lhs' ++ InvertedScalarArg x : xs)
        _                -> Nothing
    exprToArgs (BinaryOpExpr op lhs rhs) | repr op == "%" = do
      lhs' <- exprToArgs lhs
      rhs' <- exprToArgs rhs
      case rhs' of
        ScalarArg x : xs -> return (lhs' ++ TensorArg x : xs)
        _                -> Nothing
    exprToArgs _ = Nothing

expr :: Parser EgisonExpr
expr = do
  body <- exprWithoutWhere
  bindings <- optional whereDefs
  return $ case bindings of
             Nothing -> body
             Just bindings -> LetRecExpr bindings body
  where
    whereDefs = do
      pos <- reserved "where" >> L.indentLevel
      some (L.indentGuard sc EQ pos >> binding)

exprWithoutWhere :: Parser EgisonExpr
exprWithoutWhere =
       ifExpr
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

makeTable :: Pos -> [[Operator Parser EgisonExpr]]
makeTable pos =
  -- prefixes have top priority
  let prefixes = [ [ Prefix (unary "-")
                   , Prefix (unary "!") ] ]
      -- Generate binary operator table from |reservedBinops|
      binops = map (map binOpToOperator)
        (groupBy (\x y -> priority x == priority y) reservedBinops)
   in prefixes ++ binops
  where
    -- notFollowedBy (in unary and binary) is necessary for section expression.
    unary :: String -> Parser (EgisonExpr -> EgisonExpr)
    unary sym = UnaryOpExpr <$> try (operator sym <* notFollowedBy (symbol ")"))

    binary :: String -> Parser (EgisonExpr -> EgisonExpr -> EgisonExpr)
    binary sym = do
      -- TODO: Is this indentation guard necessary?
      op <- try (L.indentGuard sc GT pos >> binOpLiteral sym <* notFollowedBy (symbol ")"))
      return $ BinaryOpExpr op

    binOpToOperator :: EgisonBinOp -> Operator Parser EgisonExpr
    binOpToOperator op = case assoc op of
                           LeftAssoc  -> InfixL (binary (repr op))
                           RightAssoc -> InfixR (binary (repr op))
                           NonAssoc   -> InfixN (binary (repr op))


ifExpr :: Parser EgisonExpr
ifExpr = reserved "if" >> IfExpr <$> expr <* reserved "then" <*> expr <* reserved "else" <*> expr

patternMatchExpr :: Parser EgisonExpr
patternMatchExpr = makeMatchExpr (reserved "match")       (MatchExpr BFSMode)
               <|> makeMatchExpr (reserved "matchDFS")    (MatchExpr DFSMode)
               <|> makeMatchExpr (reserved "matchAll")    (MatchAllExpr BFSMode)
               <|> makeMatchExpr (reserved "matchAllDFS") (MatchAllExpr DFSMode)
               <?> "pattern match expression"
  where
    makeMatchExpr keyword ctor = ctor <$> (keyword >> expr)
                                      <*> (reserved "as" >> expr)
                                      <*> (reserved "with" >> matchClauses1)

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
      makeMatchLambdaExpr (reserved "match")    MatchLambdaExpr
  <|> makeMatchLambdaExpr (reserved "matchAll") MatchAllLambdaExpr
  <|> try (LambdaExpr <$> some arg <*> (symbol "->" >> expr))
  <|> PatternFunctionExpr <$> some lowerId <*> (symbol "=>" >> pattern))
  <?> "lambda or pattern function expression"
  where
    makeMatchLambdaExpr keyword ctor = do
      matcher <- keyword >> reserved "as" >> expr
      clauses <- reserved "with" >> matchClauses1
      return $ ctor matcher clauses

arg :: Parser Arg
arg = InvertedScalarArg <$> (char '*' >> lowerId)
  <|> TensorArg         <$> (char '%' >> lowerId)
  <|> ScalarArg         <$> lowerId
  <?> "argument"

letExpr :: Parser EgisonExpr
letExpr = do
  pos   <- reserved "let" >> L.indentLevel
  binds <- oneLiner <|> some (L.indentGuard sc EQ pos *> binding)
  body  <- reserved "in" >> expr
  return $ LetRecExpr binds body
  where
    oneLiner :: Parser [BindingExpr]
    oneLiner = braces $ sepBy binding (symbol ";")

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
withSymbolsExpr = WithSymbolsExpr <$> (reserved "withSymbols" >> brackets (sepBy lowerId comma)) <*> expr

doExpr :: Parser EgisonExpr
doExpr = do
  pos   <- reserved "do" >> L.indentLevel
  stmts <- oneLiner <|> some (L.indentGuard sc EQ pos >> statement)
  return $ case last stmts of
             ([], retExpr@(ApplyExpr (VarExpr (Var ["return"] _)) _)) ->
               DoExpr (init stmts) retExpr
             _ -> DoExpr stmts (makeApply' "return" [])
  where
    statement :: Parser BindingExpr
    statement = (reserved "let" >> binding) <|> ([],) <$> expr

    oneLiner :: Parser [BindingExpr]
    oneLiner = braces $ sepBy statement (symbol ";")

ioExpr :: Parser EgisonExpr
ioExpr = IoExpr <$> (reserved "io" >> expr)

matcherExpr :: Parser EgisonExpr
matcherExpr = do
  reserved "matcher"
  pos  <- L.indentLevel
  -- Assuming it is unlikely that users want to write matchers with only 1
  -- pattern definition, the first '|' (bar) is made indispensable in matcher
  -- expression.
  info <- some (L.indentGuard sc EQ pos >> symbol "|" >> patternDef)
  return $ MatcherExpr info
  where
    patternDef :: Parser (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
    patternDef = do
      pp <- ppPattern
      returnMatcher <- reserved "as" >> expr <* reserved "with"
      pos <- L.indentLevel
      datapat <- some (L.indentGuard sc EQ pos >> symbol "|" >> dataCases)
      return (pp, returnMatcher, datapat)

    dataCases :: Parser (PrimitiveDataPattern, EgisonExpr)
    dataCases = (,) <$> pdPattern <*> (symbol "->" >> expr)

algebraicDataMatcherExpr :: Parser EgisonExpr
algebraicDataMatcherExpr = do
  reserved "algebraicDataMatcher"
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
memoizedLambdaExpr = MemoizedLambdaExpr <$> (reserved "memoizedLambda" >> many lowerId) <*> (symbol "->" >> expr)

procedureExpr :: Parser EgisonExpr
procedureExpr = ProcedureExpr <$> (reserved "procedure" >> many lowerId) <*> (symbol "->" >> expr)

generateTensorExpr :: Parser EgisonExpr
generateTensorExpr = GenerateTensorExpr <$> (reserved "generateTensor" >> atomExpr) <*> atomExpr

tensorExpr :: Parser EgisonExpr
tensorExpr = TensorExpr <$> (reserved "tensor" >> atomExpr) <*> atomExpr
                        <*> option (CollectionExpr []) atomExpr
                        <*> option (CollectionExpr []) atomExpr

functionExpr :: Parser EgisonExpr
functionExpr = FunctionExpr <$> (reserved "function" >> parens (sepBy expr comma))

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

-- Parse an atomic expression starting with '(', which can be:
--   * a tuple
--   * an arbitrary expression wrapped with parenthesis
--   * section
tupleOrParenExpr :: Parser EgisonExpr
tupleOrParenExpr = do
  elems <- symbol "(" >> try (sepBy expr comma <* symbol ")") <|> (section <* symbol ")")
  case elems of
    [x] -> return x                 -- expression wrapped in parenthesis
    _   -> return $ TupleExpr elems -- tuple
  where
    section :: Parser [EgisonExpr]
    -- Start from right, in order to parse expressions like (-1 +) correctly
    section = (:[]) <$> (rightSection <|> leftSection)

    -- Sections without the left operand: eg. (+), (+ 1)
    leftSection :: Parser EgisonExpr
    leftSection = do
      op   <- choice $ map (binOpLiteral . repr) reservedBinops
      rarg <- optional expr
      case rarg of
        Just (BinaryOpExpr op' _ _)
          | assoc op' /= RightAssoc && priority op >= priority op' ->
          customFailure (IllFormedSection op op')
        _ -> return (makeLambda op Nothing rarg)

    -- Sections with the left operand but lacks the right operand: eg. (1 +)
    rightSection :: Parser EgisonExpr
    rightSection = do
      larg <- opExpr
      op   <- choice $ map (binOpLiteral . repr) reservedBinops
      case larg of
        BinaryOpExpr op' _ _
          | assoc op' /= LeftAssoc && priority op >= priority op' ->
          customFailure (IllFormedSection op op')
        _ -> return (makeLambda op (Just larg) Nothing)

    -- TODO(momohatt): Generate fresh variable for argument
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

-- (Possibly indexed) atomic expressions
atomExpr :: Parser EgisonExpr
atomExpr = do
  e <- atomExpr'
  -- TODO(momohatt): "..." (override of index) collides with ContPat
  indices <- many index
  return $ case indices of
             [] -> e
             _  -> IndexedExpr True e indices

-- Atomic expressions without index
atomExpr' :: Parser EgisonExpr
atomExpr' = partialExpr    -- must come before |constantExpr|
        <|> constantExpr
        <|> FreshVarExpr <$ symbol "#"
        <|> VarExpr <$> varLiteral
        <|> inductiveDataOrModuleExpr
        <|> vectorExpr     -- must come before |collectionExpr|
        <|> arrayExpr      -- must come before |tupleOrParenExpr|
        <|> collectionExpr
        <|> tupleOrParenExpr
        <|> hashExpr
        <|> QuoteExpr <$> (char '\'' >> atomExpr') -- must come after |constantExpr|
        <|> QuoteSymbolExpr <$> (char '`' >> atomExpr')
        <|> PartialVarExpr  <$> try (char '%' >> positiveIntegerLiteral)
        <?> "atomic expression"

inductiveDataOrModuleExpr :: Parser EgisonExpr
inductiveDataOrModuleExpr = do
  (ident, rest) <- upperOrModuleId
  return $ case rest of
             [] -> InductiveDataExpr ident []
             _  -> VarExpr (Var (ident : rest) [])

partialExpr :: Parser EgisonExpr
partialExpr = do
  n    <- try (L.decimal <* char '#') -- No space after the index
  body <- atomExpr                    -- No space after '#'
  return $ PartialExpr n body

constantExpr :: Parser EgisonExpr
constantExpr = numericExpr
           <|> BoolExpr <$> boolLiteral
           <|> CharExpr <$> try charLiteral        -- try for quoteExpr
           <|> StringExpr . pack <$> stringLiteral
           <|> SomethingExpr <$ reserved "something"
           <|> UndefinedExpr <$ reserved "undefined"

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
  pos   <- reserved "let" >> L.indentLevel
  binds <- some (L.indentGuard sc EQ pos *> binding)
  body  <- reserved "in" >> pattern
  return $ LetPat binds body

loopPattern :: Parser EgisonPattern
loopPattern =
  LoopPat <$> (reserved "loop" >> patVarLiteral) <*> loopRange
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
    _                       -> error (show (func, args))

-- (Possibly indexed) atomic pattern
atomPattern :: Parser EgisonPattern
atomPattern = do
  pat     <- atomPattern'
  indices <- many . try $ char '_' >> atomExpr'
  return $ case indices of
             [] -> pat
             _  -> IndexedPat pat indices

-- Atomic pattern without index
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

-- Space Comsumer
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
binOpLiteral sym =
  try (do wedge <- optional (char '!')
          opSym <- operator' sym
          let opInfo = fromJust $ find ((== opSym) . repr) reservedBinops
          return $ opInfo { isWedge = isJust wedge })
   <?> "binary operator"
  where
    -- operator without try
    operator' :: String -> Parser String
    operator' sym = string sym <* notFollowedBy opChar <* sc

reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy identChar)

symbol :: String -> Parser String
symbol sym = try $ L.symbol sc sym

operator :: String -> Parser String
operator sym = try $ string sym <* notFollowedBy opChar <* sc

patOperator :: String -> Parser String
patOperator sym = try $ string sym <* notFollowedBy patOpChar <* sc

-- Characters that can consist expression operators.
opChar :: Parser Char
opChar = oneOf "%^&*-+\\|:<>.?/'!#@$"

-- Characters that can consist pattern operators.
-- ! # @ $ are omitted because they can appear at the beginning of atomPattern
patOpChar :: Parser Char
patOpChar = oneOf "%^&*-+\\|:<>.?/'"

-- Characters that consist identifiers.
-- Note that 'alphaNumChar' can also parse greek letters.
-- TODO(momohatt): Probably remove '.' ?
identChar :: Parser Char
identChar = alphaNumChar <|> oneOf (['.', '?', '\'', '/'] ++ mathSymbols)

-- Non-alphabetical symbols that are allowed for identifiers
mathSymbols :: String
mathSymbols = "∂∇"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets  = between (symbol "[") (symbol "]")

comma :: Parser String
comma = symbol ","

-- Notes on identifiers:
-- * Identifiers must be able to include greek letters and some symbols in
--   |mathSymbols|.
-- * Only identifiers starting with capital English letters ('A' - 'Z') can be
--   parsed as |upperId|. Identifiers starting with capital Greek letters must
--   be regarded as |lowerId|.

lowerId :: Parser String
lowerId = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> satisfy (\c -> c `elem` mathSymbols || isLetter c && not (isAsciiUpper c)) <*> many identChar
    check x = if x `elem` lowerReservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

upperId :: Parser String
upperId = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> satisfy isAsciiUpper <*> many alphaNumChar
    check x = if x `elem` upperReservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

-- Parses both InductiveDataExpr and Var with module
-- ex. "Greater"       -> ("Greater", [])
--     "S.intercalate" -> ("S", ["intercalate"])
upperOrModuleId :: Parser (String, [String])
upperOrModuleId = do
  ident <- (:) <$> upperChar <*> many alphaNumChar
  follows <- many (char '.' >> some alphaNumChar) <* sc
  return (ident, follows)

upperReservedWords :: [String]
upperReservedWords =
  [ "True"
  , "False"
  ]

lowerReservedWords :: [String]
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
  , "let"
  , "in"
  , "where"
  , "withSymbols"
  , "loop"
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
