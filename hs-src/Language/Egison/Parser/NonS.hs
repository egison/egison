{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE NamedFieldPuns   #-}

{- |
Module      : Language.Egison.Parser.NonS
Licence     : MIT

This module provides the new parser of Egison.
-}

module Language.Egison.Parser.NonS
       (
       -- * Parse a string
         parseTopExprs
       , parseTopExpr
       , parseExprs
       , parseExpr
       , upperReservedWords
       , lowerReservedWords
       ) where

import           Control.Monad.State            (get, gets, put)

import           Data.Char                      (isAsciiUpper, isLetter)
import           Data.Either                    (isRight)
import           Data.Functor                   (($>))
import           Data.List                      (groupBy, insertBy)
import           Data.Maybe                     (isJust, isNothing)
import           Data.Text                      (pack)

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Language.Egison.AST            hiding (Assoc(..))
import qualified Language.Egison.AST            as E
import           Language.Egison.Data
import           Language.Egison.RState


parseTopExprs :: String -> RuntimeM (Either EgisonError [EgisonTopExpr])
parseTopExprs = doParse $ many (L.nonIndented sc topExpr) <* eof

parseTopExpr :: String -> RuntimeM (Either EgisonError EgisonTopExpr)
parseTopExpr = doParse $ sc >> topExpr <* eof

parseExprs :: String -> RuntimeM (Either EgisonError [EgisonExpr])
parseExprs = doParse $ many (L.nonIndented sc expr) <* eof

parseExpr :: String -> RuntimeM (Either EgisonError EgisonExpr)
parseExpr = doParse $ sc >> expr <* eof

--
-- Parser
--

type Parser = ParsecT CustomError String RuntimeM

data CustomError
  = IllFormedSection Op Op
  | IllFormedDefine
  | LastStmtInDoBlock
  deriving (Eq, Ord)

instance ShowErrorComponent CustomError where
  showErrorComponent (IllFormedSection op op') =
    "The operator " ++ info op ++ " must have lower precedence than " ++ info op'
    where
      info op =
         "'" ++ repr op ++ "' [" ++ show (assoc op) ++ " " ++ show (priority op) ++ "]"
  showErrorComponent IllFormedDefine =
    "Failed to parse the left hand side of definition expression."
  showErrorComponent LastStmtInDoBlock =
    "The last statement in a 'do' block must be an expression."


doParse :: Parser a -> String -> RuntimeM (Either EgisonError a)
doParse p input = do
  result <- runParserT p "egison" input
  case result of
    Left e  -> return $ Left (Parser (errorBundlePretty e))
    Right r -> return $ Right r

--
-- Expressions
--

topExpr :: Parser EgisonTopExpr
topExpr = Load     <$> (reserved "load" >> stringLiteral)
      <|> LoadFile <$> (reserved "loadFile" >> stringLiteral)
      <|> Execute  <$> (reserved "execute" >> expr)
      <|> infixExpr
      <|> defineOrTestExpr
      <?> "toplevel expression"

-- Return type of |convertToDefine|.
data ConversionResult
  = Variable Var        -- Definition of a variable with no arguments on lhs.
  | Function Var [Arg]  -- Definition of a function with some arguments on lhs.
  | IndexedVar VarWithIndices

-- Sort binaryop table on the insertion
addNewOp :: Op -> Bool -> Parser ()
addNewOp newop isPattern | isPattern = do
  pstate <- get
  put $! pstate { patternOps = insertBy
                                     (\x y -> compare (priority y) (priority x))
                                     newop
                                     (patternOps pstate) }
addNewOp newop _ = do
  pstate <- get
  put $! pstate { exprOps = insertBy
                                  (\x y -> compare (priority y) (priority x))
                                  newop
                                  (exprOps pstate) }

infixExpr :: Parser EgisonTopExpr
infixExpr = do
  assoc     <- (reserved "infixl" $> E.InfixL)
           <|> (reserved "infixr" $> E.InfixR)
           <|> (reserved "infix"  $> E.InfixN)
  isPattern <- isRight <$> eitherP (reserved "expression") (reserved "pattern")
  priority  <- fromInteger <$> positiveIntegerLiteral
  sym       <- if isPattern then newPatOp >>= checkP else some opChar >>= check
  let newop = Op { repr = sym, priority, assoc, isWedge = False }
  addNewOp newop isPattern
  return (InfixDecl isPattern newop)
  where
    check :: String -> Parser String
    check ('!':_) = fail $ "cannot declare infix starting with '!'"
    check x | x `elem` reservedOp = fail $ show x ++ " cannot be a new infix"
            | otherwise           = return x

    -- Checks if given string is valid for pattern op.
    checkP :: String -> Parser String
    checkP x | x `elem` reservedPOp = fail $ show x ++ " cannot be a new pattern infix"
             | otherwise           = return x

    reservedOp = [":", ":=", "->"]
    reservedPOp = ["&", "|", ":=", "->"]

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
    convertToDefine (SectionExpr op Nothing Nothing) =
      return $ Variable (stringToVar (repr op))
    convertToDefine (ApplyExpr (VarExpr var) (TupleExpr [TupleExpr args])) = do
      args' <- mapM ((TensorArg <$>) . exprToStr) args
      return $ Function var args'
    convertToDefine (ApplyExpr (VarExpr var) (TupleExpr args)) = do
      args' <- mapM ((TensorArg <$>) . exprToStr) args
      return $ Function var args'
    convertToDefine (ApplyExpr (SectionExpr op Nothing Nothing) (TupleExpr [x, y])) = do
      args <- mapM ((TensorArg <$>) . exprToStr) [x, y]
      return $ Function (stringToVar (repr op)) args
    convertToDefine e@(InfixExpr op _ _)
      | repr op == "*$" || repr op == "%" || repr op == "$" = do
        args <- exprToArgs e
        case args of
          TensorArg var : args -> return $ Function (stringToVar var) args
          _                    -> Nothing
    convertToDefine (IndexedExpr True (VarExpr (Var var [])) indices) = do
      -- [Index EgisonExpr] -> Maybe [Index String]
      indices' <- mapM (traverse exprToStr) indices
      return $ IndexedVar (VarWithIndices var indices')
    convertToDefine _ = Nothing

    exprToStr :: EgisonExpr -> Maybe String
    exprToStr (VarExpr v) = Just (show v)
    exprToStr _           = Nothing

    exprToArgs :: EgisonExpr -> Maybe [Arg]
    exprToArgs (VarExpr v) = return [TensorArg (show v)]
    exprToArgs (ApplyExpr func (TupleExpr args)) =
      (++) <$> exprToArgs func <*> mapM ((TensorArg <$>) . exprToStr) args
    exprToArgs (SectionExpr op Nothing Nothing) = return [TensorArg (repr op)]
    exprToArgs (InfixExpr op lhs rhs) | repr op == "*$" = do
      lhs' <- exprToArgs lhs
      rhs' <- exprToArgs rhs
      case rhs' of
        TensorArg x : xs -> return (lhs' ++ InvertedScalarArg x : xs)
        _                -> Nothing
    exprToArgs (InfixExpr op lhs rhs) | repr op == "$" = do
      lhs' <- exprToArgs lhs
      rhs' <- exprToArgs rhs
      case rhs' of
        TensorArg x : xs -> return (lhs' ++ ScalarArg x : xs)
        _                -> Nothing
    exprToArgs (InfixExpr op lhs rhs) | repr op == "%" = do
      lhs' <- exprToArgs lhs
      rhs' <- exprToArgs rhs
      case rhs' of
        TensorArg _ : _ -> return (lhs' ++ rhs')
        _               -> Nothing
    exprToArgs _ = Nothing

expr :: Parser EgisonExpr
expr = do
  body <- exprWithoutWhere
  bindings <- optional (reserved "where" >> alignSome binding)
  return $ case bindings of
             Nothing -> body
             Just bindings -> LetRecExpr bindings body

exprWithoutWhere :: Parser EgisonExpr
exprWithoutWhere =
       ifExpr
   <|> patternMatchExpr
   <|> lambdaExpr
   <|> lambdaLikeExpr
   <|> letExpr
   <|> withSymbolsExpr
   <|> doExpr
   <|> ioExpr
   <|> seqExpr
   <|> capplyExpr
   <|> matcherExpr
   <|> algebraicDataMatcherExpr
   <|> tensorExpr
   <|> functionExpr
   <|> refsExpr
   <|> opExpr
   <?> "expression"

-- Also parses atomExpr
opExpr :: Parser EgisonExpr
opExpr = do
  ops <- gets exprOps
  makeExprParser atomOrApplyExpr (makeExprTable ops)

makeExprTable :: [Op] -> [[Operator Parser EgisonExpr]]
makeExprTable ops =
  -- Generate binary operator table from |ops|
  map (map toOperator) (groupBy (\x y -> priority x == priority y) ops)
  where
    -- notFollowedBy (in unary and binary) is necessary for section expression.
    unary :: String -> Parser (EgisonExpr -> EgisonExpr)
    unary sym = PrefixExpr <$> try (operator sym <* notFollowedBy (symbol ")"))

    binary :: Op -> Parser (EgisonExpr -> EgisonExpr -> EgisonExpr)
    binary op = do
      -- Operators should be indented than pos1 in order to avoid
      -- "1\n-2" (2 topExprs, 1 and -2) to be parsed as "1 - 2".
      op <- try (indented >> infixLiteral (repr op) <* notFollowedBy (symbol ")"))
      return $ InfixExpr op

    toOperator :: Op -> Operator Parser EgisonExpr
    toOperator op =
      case assoc op of
        E.InfixL -> InfixL (binary op)
        E.InfixR -> InfixR (binary op)
        E.InfixN -> InfixN (binary op)
        E.Prefix -> Prefix (unary (repr op))

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
matchClauses1 =
  -- If the first bar '|' is missing, then it is expected to have only one match clause.
  (lookAhead (symbol "|") >> alignSome matchClause) <|> (:[]) <$> matchClauseWithoutBar
  where
    matchClauseWithoutBar :: Parser MatchClause
    matchClauseWithoutBar = (,) <$> pattern <*> (symbol "->" >> expr)

    matchClause :: Parser MatchClause
    matchClause = (,) <$> (symbol "|" >> pattern) <*> (symbol "->" >> expr)

lambdaExpr :: Parser EgisonExpr
lambdaExpr = symbol "\\" >> (
      makeMatchLambdaExpr (reserved "match")    MatchLambdaExpr
  <|> makeMatchLambdaExpr (reserved "matchAll") MatchAllLambdaExpr
  <|> try (LambdaExpr <$> tupleOrSome arg <* symbol "->") <*> expr
  <|> PatternFunctionExpr <$> tupleOrSome lowerId <*> (symbol "=>" >> pattern))
  <?> "lambda or pattern function expression"
  where
    makeMatchLambdaExpr keyword ctor = do
      matcher <- keyword >> reserved "as" >> expr
      clauses <- reserved "with" >> matchClauses1
      return $ ctor matcher clauses

lambdaLikeExpr :: Parser EgisonExpr
lambdaLikeExpr =
        (reserved "memoizedLambda" >> MemoizedLambdaExpr <$> tupleOrSome lowerId <*> (symbol "->" >> expr))
    <|> (reserved "cambda"         >> CambdaExpr         <$> lowerId      <*> (symbol "->" >> expr))

arg :: Parser Arg
arg = InvertedScalarArg <$> (char '*' >> ident)
  <|> TensorArg         <$> (char '%' >> ident)
  <|> ScalarArg         <$> (char '$' >> ident)
  <|> TensorArg         <$> ident
  <?> "argument"

letExpr :: Parser EgisonExpr
letExpr = do
  binds <- reserved "let" >> oneLiner <|> alignSome binding
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
withSymbolsExpr = WithSymbolsExpr <$> (reserved "withSymbols" >> brackets (sepBy ident comma)) <*> expr

doExpr :: Parser EgisonExpr
doExpr = do
  stmts <- reserved "do" >> oneLiner <|> alignSome statement
  case reverse stmts of
    []           -> return $ DoExpr []           (makeApply "return" [])
    ([], expr):_ -> return $ DoExpr (init stmts) expr
    _:_          -> customFailure LastStmtInDoBlock
  where
    statement :: Parser BindingExpr
    statement = (reserved "let" >> binding) <|> ([],) <$> expr

    oneLiner :: Parser [BindingExpr]
    oneLiner = braces $ sepBy statement (symbol ";")

ioExpr :: Parser EgisonExpr
ioExpr = IoExpr <$> (reserved "io" >> expr)

seqExpr :: Parser EgisonExpr
seqExpr = SeqExpr <$> (reserved "seq" >> atomExpr) <*> atomExpr

capplyExpr :: Parser EgisonExpr
capplyExpr = CApplyExpr <$> (reserved "capply" >> atomExpr) <*> atomExpr

matcherExpr :: Parser EgisonExpr
matcherExpr = do
  reserved "matcher"
  -- Assuming it is unlikely that users want to write matchers with only 1
  -- pattern definition, the first '|' (bar) is made indispensable in matcher
  -- expression.
  MatcherExpr <$> alignSome (symbol "|" >> patternDef)
  where
    patternDef :: Parser (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
    patternDef = do
      pp <- ppPattern
      returnMatcher <- reserved "as" >> expr <* reserved "with"
      datapat <- alignSome (symbol "|" >> dataCases)
      return (pp, returnMatcher, datapat)

    dataCases :: Parser (PrimitiveDataPattern, EgisonExpr)
    dataCases = (,) <$> pdPattern <*> (symbol "->" >> expr)

algebraicDataMatcherExpr :: Parser EgisonExpr
algebraicDataMatcherExpr = do
  reserved "algebraicDataMatcher"
  AlgebraicDataMatcherExpr <$> alignSome (symbol "|" >> patternDef)
  where
    patternDef = indentBlock lowerId atomExpr

tensorExpr :: Parser EgisonExpr
tensorExpr =
      (reserved "tensor"         >> TensorExpr         <$> atomExpr <*> atomExpr)
  <|> (reserved "generateTensor" >> GenerateTensorExpr <$> atomExpr <*> atomExpr)
  <|> (reserved "contract"       >> TensorContractExpr <$> atomExpr)
  <|> (reserved "tensorMap"      >> TensorMapExpr      <$> atomExpr <*> atomExpr)
  <|> (reserved "tensorMap2"     >> TensorMap2Expr     <$> atomExpr <*> atomExpr <*> atomExpr)
  <|> (reserved "transpose"      >> TransposeExpr      <$> atomExpr <*> atomExpr)

functionExpr :: Parser EgisonExpr
functionExpr = FunctionExpr <$> (reserved "function" >> parens (sepBy expr comma))

refsExpr :: Parser EgisonExpr
refsExpr =
      (reserved "subrefs"   >> SubrefsExpr  False <$> atomExpr <*> atomExpr)
  <|> (reserved "subrefs!"  >> SubrefsExpr  True  <$> atomExpr <*> atomExpr)
  <|> (reserved "suprefs"   >> SuprefsExpr  False <$> atomExpr <*> atomExpr)
  <|> (reserved "suprefs!"  >> SuprefsExpr  True  <$> atomExpr <*> atomExpr)
  <|> (reserved "userRefs"  >> UserrefsExpr False <$> atomExpr <*> atomExpr)
  <|> (reserved "userRefs!" >> UserrefsExpr True  <$> atomExpr <*> atomExpr)

collectionExpr :: Parser EgisonExpr
collectionExpr = symbol "[" >> betweenOrFromExpr <|> elementsExpr
  where
    betweenOrFromExpr = do
      start <- try (expr <* symbol "..")
      end   <- optional expr <* symbol "]"
      case end of
        Just end' -> return $ makeApply "between" [start, end']
        Nothing   -> return $ makeApply "from" [start]

    elementsExpr = CollectionExpr <$> (sepBy expr comma <* symbol "]")

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
      ops  <- gets exprOps
      op   <- choice $ map (infixLiteral . repr) ops
      rarg <- optional expr
      case rarg of
        -- Disabling for now... (See issue 159)
        -- Just (InfixExpr op' _ _)
        --   | assoc op' /= InfixR && priority op >= priority op' ->
        --   customFailure (IllFormedSection op op')
        _ -> return (SectionExpr op Nothing rarg)

    -- Sections with the left operand but lacks the right operand: eg. (1 +)
    rightSection :: Parser EgisonExpr
    rightSection = do
      ops  <- gets exprOps
      larg <- opExpr
      op   <- choice $ map (infixLiteral . repr) ops
      case larg of
        -- InfixExpr op' _ _
        --   | assoc op' /= InfixL && priority op >= priority op' ->
        --   customFailure (IllFormedSection op op')
        _ -> return (SectionExpr op (Just larg) Nothing)

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
  (func, args) <- indentBlock atomExpr atomExpr
  return $ case args of
             [] -> func
             _  -> makeApply' func args

-- (Possibly indexed) atomic expressions
atomExpr :: Parser EgisonExpr
atomExpr = do
  e <- atomExpr'
  override <- isNothing <$> optional (try (string "..." <* lookAhead index))
  indices <- many index
  return $ case indices of
             [] -> e
             _  -> IndexedExpr override e indices

-- Atomic expressions without index
atomExpr' :: Parser EgisonExpr
atomExpr' = anonParamFuncExpr    -- must come before |constantExpr|
        <|> constantExpr
        <|> FreshVarExpr <$ symbol "#"
        <|> VarExpr <$> varLiteral
        <|> vectorExpr     -- must come before |collectionExpr|
        <|> collectionExpr
        <|> tupleOrParenExpr
        <|> hashExpr
        <|> QuoteExpr <$> (char '\'' >> atomExpr') -- must come after |constantExpr|
        <|> QuoteSymbolExpr <$> (char '`' >> atomExpr')
        <|> AnonParamExpr  <$> try (char '%' >> positiveIntegerLiteral)
        <?> "atomic expression"

anonParamFuncExpr :: Parser EgisonExpr
anonParamFuncExpr = do
  n    <- try (L.decimal <* char '#') -- No space after the index
  body <- atomExpr                    -- No space after '#'
  return $ AnonParamFuncExpr n body

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

pattern :: Parser Pattern
pattern = letPattern
      <|> forallPattern
      <|> loopPattern
      <|> opPattern
      <?> "pattern"

letPattern :: Parser Pattern
letPattern =
  reserved "let" >> LetPat <$> alignSome binding <*> (reserved "in" >> pattern)

forallPattern :: Parser Pattern
forallPattern =
  reserved "forall" >> ForallPat <$> atomPattern <*> atomPattern

loopPattern :: Parser Pattern
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
                (makeApply' (stringToVarExpr "-'") [s, IntegerExpr 1])

seqPattern :: Parser Pattern
seqPattern = do
  pats <- braces $ sepBy pattern comma
  return $ foldr SeqConsPat SeqNilPat pats

opPattern :: Parser Pattern
opPattern = do
  ops <- gets patternOps
  makeExprParser applyOrAtomPattern (makePatternTable ops)

makePatternTable :: [Op] -> [[Operator Parser Pattern]]
makePatternTable ops =
  let ops' = map toOperator ops
   in map (map snd) (groupBy (\x y -> fst x == fst y) ops')
  where
    toOperator :: Op -> (Int, Operator Parser Pattern)
    toOperator op = (priority op, infixToOperator binary op)

    binary :: Op -> Parser (Pattern -> Pattern -> Pattern)
    binary op = do
      op <- try (indented >> patInfixLiteral (repr op))
      return $ InfixPat op

applyOrAtomPattern :: Parser Pattern
applyOrAtomPattern = (do
    (func, args) <- indentBlock (try atomPattern) atomPattern
    case (func, args) of
      (_,                 []) -> return func
      (InductivePat x [], _)  -> return $ InductiveOrPApplyPat x args
      _                       -> return $ DApplyPat func args)
  <|> (do
    (func, args) <- indentBlock atomExpr atomPattern
    return $ PApplyPat func args)

collectionPattern :: Parser Pattern
collectionPattern = brackets $ do
  elems <- sepBy pattern comma
  return $ foldr (InfixPat consOp) nilPat elems
    where
      nilPat = InductivePat "nil" []
      consOp = findOpFrom "::" reservedPatternOp

-- (Possibly indexed) atomic pattern
atomPattern :: Parser Pattern
atomPattern = do
  pat     <- atomPattern'
  indices <- many . try $ char '_' >> atomExpr'
  return $ case indices of
             [] -> pat
             _  -> IndexedPat pat indices

-- Atomic pattern without index
atomPattern' :: Parser Pattern
atomPattern' = WildCard <$  symbol "_"
           <|> PatVar   <$> patVarLiteral
           <|> NotPat   <$> (symbol "!" >> atomPattern)
           <|> ValuePat <$> (char '#' >> atomExpr)
           <|> collectionPattern
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
        <|> do ops <- gets patternOps
               makeExprParser ppAtom (makeTable ops)
        <?> "primitive pattern pattern"
  where
    makeTable :: [Op] -> [[Operator Parser PrimitivePatPattern]]
    makeTable ops =
      map (map toOperator) (groupBy (\x y -> priority x == priority y) ops)

    toOperator :: Op -> Operator Parser PrimitivePatPattern
    toOperator = infixToOperator inductive2

    inductive2 op = (\x y -> PPInductivePat (repr op) [x, y]) <$ operator (repr op)

    ppAtom :: Parser PrimitivePatPattern
    ppAtom = PPWildCard <$ symbol "_"
         <|> PPPatVar   <$ symbol "$"
         <|> PPValuePat <$> (string "#$" >> lowerId)
         <|> PPInductivePat "nil" [] <$ (symbol "[" >> symbol "]")
         <|> makeTupleOrParen ppPattern PPTuplePat

pdPattern :: Parser PrimitiveDataPattern
pdPattern = makeExprParser pdApplyOrAtom table
        <?> "primitive data pattern"
  where
    table :: [[Operator Parser PrimitiveDataPattern]]
    table =
      [ [ InfixR (PDConsPat <$ symbol "::") ]
      ]

    pdApplyOrAtom :: Parser PrimitiveDataPattern
    pdApplyOrAtom = PDInductivePat <$> upperId <*> many pdAtom
                <|> PDSnocPat <$> (symbol "snoc" >> pdAtom) <*> pdAtom
                <|> pdAtom

    pdCollection :: Parser PrimitiveDataPattern
    pdCollection = do
      elts <- brackets (sepBy pdPattern comma)
      return (foldr PDConsPat PDEmptyPat elts)

    pdAtom :: Parser PrimitiveDataPattern
    pdAtom = PDWildCard    <$ symbol "_"
         <|> PDPatVar      <$> (char '$' >> ident)
         <|> PDConstantPat <$> constantExpr
         <|> pdCollection
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
varLiteral = stringToVar <$> ident

patVarLiteral :: Parser Var
patVarLiteral = stringToVar <$> (char '$' >> ident)

-- Parse infix (binary operator) literal.
-- If the operator is prefixed with '!', |isWedge| is turned to true.
infixLiteral :: String -> Parser Op
infixLiteral sym =
  try (do wedge <- optional (char '!')
          opSym <- operator' sym
          ops   <- gets exprOps
          let opInfo = findOpFrom opSym ops
          return $ opInfo { isWedge = isJust wedge })
   <?> "infix"
  where
    -- operator without try
    operator' :: String -> Parser String
    operator' sym = string sym <* notFollowedBy opChar <* sc

reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy identChar)

symbol :: String -> Parser ()
symbol sym = try (L.symbol sc sym) >> pure ()

operator :: String -> Parser String
operator sym = try $ string sym <* notFollowedBy opChar <* sc

-- |infixLiteral| for pattern infixes.
patInfixLiteral :: String -> Parser Op
patInfixLiteral sym =
  try (do opSym <- string sym <* notFollowedBy patOpChar <* sc
          ops   <- gets patternOps
          let opInfo = findOpFrom opSym ops
          return opInfo)

-- Characters that can consist expression operators.
opChar :: Parser Char
opChar = oneOf ("%^&*-+\\|:<>?!./'#@$" ++ "∧")

-- Characters that can consist pattern operators.
-- ! ? # @ $ are omitted because they can appear at the beginning of atomPattern
patOpChar :: Parser Char
patOpChar = oneOf "%^&*-+\\|:<>./'"

newPatOp :: Parser String
newPatOp = (:) <$> patOpChar <*> many (patOpChar <|> oneOf "!?#@$")

-- Characters that consist identifiers.
-- Note that 'alphaNumChar' can also parse greek letters.
identChar :: Parser Char
identChar = alphaNumChar
        <|> oneOf (['?', '\'', '/'] ++ mathSymbols)

identString :: Parser String
identString = do
  strs <- many substr
  return $ concat strs
  where
    substr = ((:) <$> try (char '.' <* notFollowedBy (char '.')) <*> many opChar)
         <|> (:[]) <$> identChar

-- Non-alphabetical symbols that are allowed for identifiers
mathSymbols :: String
mathSymbols = "∂∇"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets  = between (symbol "[") (symbol "]")

comma :: Parser ()
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
    p = (:) <$> satisfy checkHead <*> identString
    checkHead c = c `elem` mathSymbols || isLetter c && not (isAsciiUpper c)
    check x = if x `elem` lowerReservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

upperId :: Parser String
upperId = (lexeme . try) (p >>= check)
  where
    p = (:) <$> satisfy isAsciiUpper <*> many alphaNumChar
    check x = if x `elem` upperReservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

-- union of lowerId and upperId
ident :: Parser String
ident = (lexeme . try) (p >>= check)
  where
    p = (:) <$> satisfy checkHead <*> identString
    checkHead c = c `elem` mathSymbols || isLetter c
    check x = if x `elem` (lowerReservedWords ++ upperReservedWords)
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

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
  , "capply"
  , "memoizedLambda"
  , "cambda"
  , "let"
  , "in"
  , "where"
  , "withSymbols"
  , "loop"
  , "forall"
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
  , "tensorMap"
  , "tensorMap2"
  , "transpose"
  , "subrefs"
  , "subrefs!"
  , "suprefs"
  , "suprefs!"
  , "userRefs"
  , "userRefs!"
  , "function"
  , "infixl"
  , "infixr"
  , "infix"
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

makeApply' :: EgisonExpr -> [EgisonExpr] -> EgisonExpr
makeApply' (InductiveDataExpr x []) xs = InductiveDataExpr x xs
makeApply' func xs = ApplyExpr func (TupleExpr xs)

indentGuardEQ :: Pos -> Parser Pos
indentGuardEQ pos = L.indentGuard sc EQ pos

indentGuardGT :: Pos -> Parser Pos
indentGuardGT pos = L.indentGuard sc GT pos

-- Variant of 'some' that requires every element to be at the same indentation level
alignSome :: Parser a -> Parser [a]
alignSome p = do
  pos <- L.indentLevel
  some (indentGuardEQ pos >> p)

-- Useful for parsing syntax like function applications, where all 'arguments'
-- should be indented deeper than the 'function'.
indentBlock :: Parser a -> Parser b -> Parser (a, [b])
indentBlock phead parg = do
  pos  <- L.indentLevel
  head <- phead
  args <- many (indentGuardGT pos >> parg)
  return (head, args)

indented :: Parser Pos
indented = indentGuardGT pos1

infixToOperator :: (Op -> Parser (a -> a -> a)) -> Op -> Operator Parser a
infixToOperator opToParser op =
  case assoc op of
    E.InfixL -> InfixL (opToParser op)
    E.InfixR -> InfixR (opToParser op)
    E.InfixN -> InfixN (opToParser op)

tupleOrSome :: Parser a -> Parser [a]
tupleOrSome p = parens (sepBy p comma) <|> some p
