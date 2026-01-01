{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      : Language.Egison.Parser.NonS
Licence     : MIT

This module provides the parser for the new syntax.
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
import           Data.Function                  (on)
import           Data.Functor                   (($>))
import           Data.List                      (groupBy, insertBy, sortOn)
import           Data.Maybe                     (isJust, isNothing)
import           Data.Text                      (pack)

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Language.Egison.AST            hiding (Assoc (..))
import qualified Language.Egison.AST            as E
import           Language.Egison.RState


parseTopExprs :: String -> RuntimeM (Either String [TopExpr])
parseTopExprs = doParse $ many (L.nonIndented sc topExpr) <* eof

parseTopExpr :: String -> RuntimeM (Either String TopExpr)
parseTopExpr = doParse $ sc >> topExpr <* eof

parseExprs :: String -> RuntimeM (Either String [Expr])
parseExprs = doParse $ many (L.nonIndented sc expr) <* eof

parseExpr :: String -> RuntimeM (Either String Expr)
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


doParse :: Parser a -> String -> RuntimeM (Either String a)
doParse p input = do
  result <- runParserT p "egison" input
  case result of
    Left e  -> return $ Left (errorBundlePretty e)
    Right r -> return $ Right r

--
-- Expressions
--

topExpr :: Parser TopExpr
topExpr = Load     <$> (reserved "load" >> stringLiteral)
      <|> LoadFile <$> (reserved "loadFile" >> stringLiteral)
      <|> Execute  <$> (reserved "execute" >> expr)
      <|> (reserved "def" >> defineExpr)
      <|> inductiveExpr
      <|> classExpr
      <|> instanceExpr
      <|> infixExpr
      <|> Test     <$> expr
      <?> "toplevel expression"

-- | Parse inductive data type declaration
-- e.g., inductive Ordering := | Less | Equal | Greater
--       inductive Nat := | O | S Nat
--       inductive Ordering := Less | Equal | Greater  (also valid)
inductiveExpr :: Parser TopExpr
inductiveExpr = try $ do
  pos <- L.indentLevel
  reserved "inductive"
  typeName <- upperId
  -- Parse optional type parameters (lowercase identifiers)
  typeParams <- many typeVarIdent
  _ <- symbol ":="
  -- Parse constructors - they must be indented more than the 'inductive' keyword
  -- or on the same line separated by |
  constructors <- inductiveConstructors pos
  return $ InductiveDecl typeName typeParams constructors

-- | Parse constructors for inductive data type
-- Constructors must be indented more than the base position, or separated by |
inductiveConstructors :: Pos -> Parser [InductiveConstructor]
inductiveConstructors basePos = do
  -- Optional leading |
  _ <- optional (symbol "|")
  first <- inductiveConstructor
  rest <- many $ try $ do
    -- Either | separator or indented on new line
    (symbol "|" >> inductiveConstructor) <|> (indentGuardGT basePos >> inductiveConstructor)
  return (first : rest)

-- | Parse a single constructor
-- e.g., Less, S Nat, Node Tree Tree
inductiveConstructor :: Parser InductiveConstructor
inductiveConstructor = do
  name <- upperId
  -- Parse argument types using typeAtom (handles both uppercase and lowercase)
  args <- many (try inductiveArgType)
  return $ InductiveConstructor name args

-- | Parse an argument type for inductive constructor
-- Only parses simple type atoms that are clearly types
inductiveArgType :: Parser TypeExpr
inductiveArgType = try $ do
  -- Don't parse if next token is | (constructor separator)
  notFollowedBy (symbol "|")
  -- Parse type atom, but use a restricted version that only accepts:
  -- - Builtin types (Integer, Bool, etc.)
  -- - Type names (uppercase identifiers)
  -- - Type variables (lowercase, but short to avoid function names)
  -- - List types [a]
  -- - Tuple types (a, b)
  inductiveTypeAtom

-- | Restricted type atom parser for inductive constructors
inductiveTypeAtom :: Parser TypeExpr
inductiveTypeAtom =
      TEInt     <$ reserved "Integer"
  <|> TEMathExpr <$ reserved "MathExpr"
  <|> TEFloat   <$ reserved "Float"
  <|> TEBool    <$ reserved "Bool"
  <|> TEChar    <$ reserved "Char"
  <|> TEString  <$ reserved "String"
  <|> TEList    <$> brackets typeExpr
  <|> TEVar     <$> typeNameIdent     -- Uppercase type names (Nat, Tree, etc.)
  <|> TEVar     <$> inductiveTypeVar  -- Short lowercase type variables
  <|> inductiveParenType              -- Parenthesized types like (Tree a)
  <?> "type expression in inductive constructor"

-- | Parse parenthesized type in inductive context
-- Handles both simple parens (Tree a) and tuples (a, b)
inductiveParenType :: Parser TypeExpr
inductiveParenType = parens $ do
  first <- optional inductiveTypeExprInParen
  case first of
    Nothing -> return $ TETuple []  -- Unit type: ()
    Just t -> do
      rest <- optional (symbol "," >> inductiveTypeExprInParen `sepBy1` symbol ",")
      return $ case rest of
        Nothing  -> t              -- Just parenthesized: (Tree a)
        Just ts  -> TETuple (t:ts) -- Tuple: (a, b)

-- | Type expression inside parentheses in inductive context
-- Allows function types and type applications
inductiveTypeExprInParen :: Parser TypeExpr
inductiveTypeExprInParen = do
  atoms <- some inductiveTypeAtom
  -- For now, just return the first atom (type application not fully supported)
  -- In the future, we could add proper type application
  case atoms of
    [t] -> return t
    _   -> return $ TEVar "Complex"  -- Placeholder for type application

-- | Parse type variable in inductive context (must be short)
inductiveTypeVar :: Parser String
inductiveTypeVar = lexeme $ try $ do
  c <- lowerChar
  cs <- many alphaNumChar
  let name = c : cs
  -- Reject if it looks like a keyword or function name (> 2 chars usually)
  -- Common type vars: a, b, c, t, k, v, xs, elem
  if length name > 4 || name `elem` inductiveReserved
    then fail $ "Not a type variable: " ++ name
    else return name
  where
    inductiveReserved = ["def", "let", "if", "match", "load", "assert", "true", "false", "class", "instance", "where"]

-- | Parse type class declaration
-- e.g., class Eq a where
--         (==) (x: a) (y: a) : Bool
--         (/=) (x: a) (y: a) : Bool := not (x == y)
--       class Eq a => Ord a where
--         compare (x: a) (y: a) : Ordering
classExpr :: Parser TopExpr
classExpr = try $ do
  pos <- L.indentLevel
  reserved "class"
  -- Parse optional superclass constraints: Eq a =>
  (superclasses, classNm, typeParams) <- classHeader
  reserved "where"
  -- Parse methods - use alignSome for consistent indentation handling
  methods <- many $ try $ do
    _ <- indentGuardGT pos
    -- Check that this looks like a method definition
    notFollowedBy (reserved "def" <|> reserved "class" <|> reserved "instance" <|> reserved "inductive")
    classMethod
  return $ ClassDeclExpr $ ClassDecl classNm typeParams superclasses methods

-- | Parse class header: "Eq a" or "Eq a => Ord a"
-- Note: type parameters are parsed until "where" is encountered
classHeader :: Parser ([ConstraintExpr], String, [String])
classHeader = try withConstraints <|> withoutConstraints
  where
    withConstraints = do
      constraints <- constraintList
      _ <- symbol "=>"
      classNm <- upperId
      typeParams <- manyTill typeVarIdent (lookAhead (reserved "where"))
      return (constraints, classNm, typeParams)

    withoutConstraints = do
      classNm <- upperId
      typeParams <- manyTill typeVarIdent (lookAhead (reserved "where"))
      return ([], classNm, typeParams)

-- | Parse constraint list: "Eq a" or "(Eq a, Ord b)"
constraintList :: Parser [ConstraintExpr]
constraintList = try parenConstraints <|> ((:[]) <$> singleConstraint)
  where
    parenConstraints = parens $ singleConstraint `sepBy1` symbol ","
    singleConstraint = do
      classNm <- upperId
      typeArgs <- some typeAtomSimple
      return $ ConstraintExpr classNm typeArgs

-- | Parse class methods
classMethodsParser :: Pos -> Parser [ClassMethod]
classMethodsParser basePos = many $ try $ do
  -- Check that we're indented more than the class keyword
  _ <- indentGuardGT basePos
  -- And that this looks like a method (starts with operator or non-reserved identifier)
  lookAhead (try parenOperatorLookahead <|> try nonReservedLowerId)
  classMethod
  where
    parenOperatorLookahead = do
      _ <- char '('
      _ <- some (oneOf ("!#$%&*+./<=>?@\\^|-~:" :: String))
      return ()
    nonReservedLowerId = do
      c <- lowerChar
      cs <- many alphaNumChar
      let name = c : cs
      if name `elem` reservedWordsForClass
        then fail "reserved word"
        else return ()
    reservedWordsForClass = ["def", "class", "instance", "inductive", "load", "loadFile", "where", "let", "if", "match"]

-- | Parse a single class method
-- e.g., (==) (x: a) (y: a) : Bool
--       (/=) (x: a) (y: a) : Bool := not (x == y)
classMethod :: Parser ClassMethod
classMethod = do
  name <- methodName'
  params <- many (try typedParam)
  _ <- symbol ":"
  -- Use typeAtomSimple to avoid consuming too much
  retType <- typeAtomSimple
  -- Check if there's a default implementation on the same line (not crossing to new unindented line)
  defaultImpl <- optional $ try $ do
    _ <- symbol ":="
    expr
  return $ ClassMethod name params retType defaultImpl

-- | Parse method name (can be operator in parens or regular identifier)
methodName' :: Parser String
methodName' = try parenOperator <|> lowerId
  where
    parenOperator = do
      _ <- symbol "("
      op <- some (oneOf ("!#$%&*+./<=>?@\\^|-~:" :: String))
      _ <- symbol ")"
      return op

-- | Parse type class instance declaration
-- e.g., instance Eq Integer where
--         (==) x y := x = y
--       instance Eq a => Eq [a] where
--         (==) xs ys := ...
instanceExpr :: Parser TopExpr
instanceExpr = try $ do
  pos <- L.indentLevel
  reserved "instance"
  -- Parse optional instance constraints: Eq a =>
  (constraints, classNm, instTypes) <- instanceHeader
  reserved "where"
  -- Parse method implementations (indented)
  methods <- instanceMethodsParser pos
  return $ InstanceDeclExpr $ InstanceDecl constraints classNm instTypes methods

-- | Parse instance header: "Eq Integer" or "Eq a => Eq [a]"
-- Note: instance types are parsed until "where" is encountered
instanceHeader :: Parser ([ConstraintExpr], String, [TypeExpr])
instanceHeader = try withConstraints <|> withoutConstraints
  where
    withConstraints = do
      constraints <- constraintList
      _ <- symbol "=>"
      classNm <- upperId
      instTypes <- someTill typeAtomSimple (lookAhead (reserved "where"))
      return (constraints, classNm, instTypes)

    withoutConstraints = do
      classNm <- upperId
      instTypes <- someTill typeAtomSimple (lookAhead (reserved "where"))
      return ([], classNm, instTypes)

-- | Parse instance methods
instanceMethodsParser :: Pos -> Parser [InstanceMethod]
instanceMethodsParser basePos = many (try $ indentGuardGT basePos >> instanceMethod)

-- | Parse a single instance method
-- e.g., (==) x y := x = y
instanceMethod :: Parser InstanceMethod
instanceMethod = do
  name <- methodName'
  params <- many lowerId
  _ <- symbol ":="
  body <- expr
  return $ InstanceMethod name params body

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

infixExpr :: Parser TopExpr
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
    check ('!':_) = fail "cannot declare infix starting with '!'"
    check x | x `elem` reservedOp = fail $ show x ++ " cannot be a new infix"
            | otherwise           = return x

    -- Checks if given string is valid for pattern op.
    checkP :: String -> Parser String
    checkP x | x `elem` reservedPOp = fail $ show x ++ " cannot be a new pattern infix"
             | otherwise           = return x

    reservedOp = [":", ":=", "->"]
    reservedPOp = ["&", "|", ":=", "->"]

defineExpr :: Parser TopExpr
defineExpr = try defineWithType <|> defineWithoutType
  where
    defineWithoutType = do
      ops  <- gets exprOps
      f    <-   parens (stringToVarWithIndices . repr <$> choice (map (infixLiteral . repr) ops))
            <|> varWithIndicesLiteral
      args <- many arg
      _    <- symbol ":="
      body <- expr
      case args of
        [] -> return (Define f body)
        _  -> return (Define f (LambdaExpr args body))

    defineWithType = do
      ops <- gets exprOps
      varWithIdx <- parens (stringToVarWithIndices . repr <$> choice (map (infixLiteral . repr) ops))
                    <|> varWithIndicesLiteral
      let (name, indices) = extractVarWithIndices varWithIdx
      typedParams <- many typedParam
      _ <- symbol ":"
      retType <- typeExpr
      _ <- symbol ":="
      body <- expr
      let typedVar = TypedVarWithIndices name indices typedParams retType
      return (DefineWithType typedVar body)

-- | Extract name and indices from VarWithIndices
extractVarWithIndices :: VarWithIndices -> (String, [VarIndex])
extractVarWithIndices (VarWithIndices name indices) = (name, indices)

-- | Parse a typed parameter: supports both simple (x: a) and tuple ((x: a), (y: b)) patterns
typedParam :: Parser TypedParam
typedParam = parens typedParamInner

-- Parse the inner part of a typed parameter (inside parentheses)
typedParamInner :: Parser TypedParam
typedParamInner = try typedTupleParam <|> typedSimpleParam

-- Parse a tuple pattern with typed elements: (x: a), (y: b) or x: a, y: b
typedTupleParam :: Parser TypedParam
typedTupleParam = do
  first <- typedTupleElement
  _ <- symbol ","
  rest <- typedTupleElement `sepBy1` symbol ","
  return $ TPTuple (first : rest)

-- Parse an element in a typed tuple
typedTupleElement :: Parser TypedParam
typedTupleElement =
      try (parens typedParamInner)  -- Nested: ((x: a))
  <|> try typedWildcard             -- Wildcard with type: _: a
  <|> try typedVar                  -- Variable with type: x: a
  <|> untypedWildcard               -- Just wildcard: _
  <|> untypedVar                    -- Just variable: x

-- Simple typed parameter: x: a or _: a
typedSimpleParam :: Parser TypedParam
typedSimpleParam = try typedWildcard <|> typedVar

typedVar :: Parser TypedParam
typedVar = do
  paramName <- ident
  _ <- symbol ":"
  paramType <- typeExpr
  return $ TPVar paramName paramType

typedWildcard :: Parser TypedParam
typedWildcard = do
  _ <- symbol "_"
  _ <- symbol ":"
  paramType <- typeExpr
  return $ TPWildcard paramType

untypedVar :: Parser TypedParam
untypedVar = TPUntypedVar <$> ident

untypedWildcard :: Parser TypedParam
untypedWildcard = TPUntypedWildcard <$ symbol "_"

-- | Parse a type expression
typeExpr :: Parser TypeExpr
typeExpr = makeTypeExprParser

makeTypeExprParser :: Parser TypeExpr
makeTypeExprParser = do
  t <- typeAtomOrParenType
  rest <- optional (symbol "->" >> makeTypeExprParser)
  return $ case rest of
    Nothing -> t
    Just r  -> TEFun t r

typeAtomOrParenType :: Parser TypeExpr
typeAtomOrParenType =
      try parenTypeOrTuple  -- Allow (a -> b) or (a, b) as a type atom
  <|> typeAtom

-- Parse parenthesized type or tuple type (including unit type ())
parenTypeOrTuple :: Parser TypeExpr
parenTypeOrTuple = parens $ do
  first <- optional typeExprWithApp
  case first of
    Nothing -> return $ TETuple []  -- Unit type: ()
    Just t -> do
      rest <- optional (symbol "," >> typeExprWithApp `sepBy1` symbol ",")
      return $ case rest of
        Nothing  -> t              -- Just parenthesized: (a -> b) or (Maybe a)
        Just ts  -> TETuple (t:ts) -- Tuple: (a, b, c)

-- | Type expression with type application support
-- e.g., Maybe a, List Integer, Tree a b
typeExprWithApp :: Parser TypeExpr
typeExprWithApp = do
  atoms <- some typeAtomSimple
  rest <- optional (symbol "->" >> typeExprWithApp)
  let baseType = case atoms of
                   [t]    -> t
                   (t:ts) -> TEApp t ts
                   []     -> error "unreachable"
  return $ case rest of
    Nothing -> baseType
    Just r  -> TEFun baseType r

-- | Simple type atom (no function arrows)
typeAtomSimple :: Parser TypeExpr
typeAtomSimple =
      TEInt     <$ reserved "Integer"
  <|> TEMathExpr <$ reserved "MathExpr"
  <|> TEFloat   <$ reserved "Float"
  <|> TEBool    <$ reserved "Bool"
  <|> TEChar    <$ reserved "Char"
  <|> TEString  <$ reserved "String"
  <|> TEIO      <$> (reserved "IO" >> typeAtomOrParenType)
  <|> TEList    <$> brackets typeExpr
  <|> try tensorTypeExpr
  <|> TEMatcher <$> (reserved "Matcher" >> typeAtomOrParenType)
  <|> TEPattern <$> (reserved "Pattern" >> typeAtomOrParenType)
  <|> TEVar     <$> typeVarIdent      -- lowercase type variables (a, b, etc.)
  <|> TEVar     <$> typeNameIdent     -- uppercase type names (Nat, Tree, Ordering, etc.)
  <|> parenTypeOrTuple                -- Parenthesized or tuple types
  <?> "type expression"

typeAtom :: Parser TypeExpr
typeAtom =
      TEInt     <$ reserved "Integer"
  <|> TEMathExpr <$ reserved "MathExpr"
  <|> TEFloat   <$ reserved "Float"
  <|> TEBool    <$ reserved "Bool"
  <|> TEChar    <$ reserved "Char"
  <|> TEString  <$ reserved "String"
  <|> TEIO      <$> (reserved "IO" >> typeAtomOrParenType)
  <|> TEList    <$> brackets typeExpr
  <|> try tensorTypeExpr
  <|> TEMatcher <$> (reserved "Matcher" >> typeAtomOrParenType)
  <|> TEPattern <$> (reserved "Pattern" >> typeAtomOrParenType)
  <|> TEVar     <$> typeVarIdent      -- lowercase type variables (a, b, etc.)
  <|> TEVar     <$> typeNameIdent     -- uppercase type names (Nat, Tree, Ordering, etc.)
  <?> "type expression"

-- | Parse an uppercase type name (for user-defined inductive types)
typeNameIdent :: Parser String
typeNameIdent = lexeme $ do
  c <- upperChar
  cs <- many alphaNumChar
  let name = c : cs
  -- Don't consume reserved type keywords
  if name `elem` typeReservedKeywords
    then fail $ "Reserved type keyword: " ++ name
    else return name
  where
    typeReservedKeywords = ["Integer", "MathExpr", "Float", "Bool", "Char", "String", "Matcher", "Pattern", "Tensor", "IO"]

tensorTypeExpr :: Parser TypeExpr
tensorTypeExpr = do
  _ <- reserved "Tensor"
  elemType <- typeAtom
  shape <- tensorShapeExpr
  indices <- many tensorIndexExpr
  return $ TETensor elemType shape indices

tensorShapeExpr :: Parser TensorShapeExpr
tensorShapeExpr =
      try (brackets mixedShape)
  <|> TSVar <$> ident
  where
    mixedShape = do
      dims <- shapeDim `sepBy` symbol ","
      -- If all literals, use TSLit for backwards compatibility
      let allLits = all isLitDim dims
      return $ if allLits
                 then TSLit (map extractLit dims)
                 else TSMixed dims
    shapeDim = SDLit <$> positiveIntegerLiteral
           <|> SDVar <$> ident
    isLitDim (SDLit _) = True
    isLitDim _         = False
    extractLit (SDLit n) = n
    extractLit _         = 0  -- Should not happen

tensorIndexExpr :: Parser TensorIndexExpr
tensorIndexExpr =
      TIPlaceholderSub <$ try (symbol "_#")
  <|> TIPlaceholderSup <$ try (symbol "~#")
  <|> TISub <$> (char '_' *> ident)
  <|> TISup <$> (char '~' *> ident)

typeVarIdent :: Parser String
typeVarIdent = lexeme $ do
  c <- lowerChar
  cs <- many (alphaNumChar <|> char '_')
  let name = c : cs
  if name `elem` typeReservedWords
    then fail $ "Reserved word: " ++ name
    else return name
  where
    typeReservedWords = ["Integer", "MathExpr", "Float", "Bool", "Char", "String", "Matcher", "Pattern", "Tensor"]

expr :: Parser Expr
expr = do
  body <- exprWithoutWhere
  bindings <- optional (reserved "where" >> alignSome binding)
  return $ case bindings of
             Nothing       -> body
             Just bindings -> LetRecExpr bindings body

exprWithoutWhere :: Parser Expr
exprWithoutWhere = opExpr

-- Expressions that can be the arguments for the operators.
exprInOp :: Parser Expr
exprInOp =
       ifExpr
   <|> patternMatchExpr
   <|> lambdaExpr
   <|> lambdaLikeExpr
   <|> letExpr
   <|> withSymbolsExpr
   <|> doExpr
   <|> seqExpr
   <|> capplyExpr
   <|> matcherExpr
   <|> algebraicDataMatcherExpr
   <|> tensorExpr
   <|> functionExpr
   <|> refsExpr
   <|> atomOrApplyExpr
   <?> "expression"

-- Also parses exprInOp
opExpr :: Parser Expr
opExpr = do
  ops <- gets exprOps
  makeExprParser exprInOp (makeExprTable ops)

makeExprTable :: [Op] -> [[Operator Parser Expr]]
makeExprTable ops =
  -- Generate binary operator table from |ops|
  reverse $ map (map snd) $ groupBy ((==) `on` fst) $ sortOn fst $
    (infixFuncOpPriority, infixFuncOperator) : map (\op -> (priority op, toOperator op)) ops
  where
    -- notFollowedBy (in unary and binary) is necessary for section expression.
    unary :: String -> Parser (Expr -> Expr)
    unary sym = PrefixExpr <$> try (operator sym <* notFollowedBy (symbol ")"))

    binary :: Op -> Parser (Expr -> Expr -> Expr)
    binary op = do
      -- Operators should be indented than pos1 in order to avoid
      -- "1\n-2" (2 topExprs, 1 and -2) to be parsed as "1 - 2".
      op <- try (indented >> infixLiteral (repr op) <* notFollowedBy (symbol ")"))
      return $ InfixExpr op

    toOperator :: Op -> Operator Parser Expr
    toOperator op =
      case assoc op of
        E.InfixL -> InfixL (binary op)
        E.InfixR -> InfixR (binary op)
        E.InfixN -> InfixN (binary op)
        E.Prefix -> Prefix (unary (repr op))

    infixFuncOperator :: Operator Parser Expr
    infixFuncOperator = InfixL $ InfixExpr <$> infixFuncOp

infixFuncOp :: Parser Op
infixFuncOp = do
  func <- try (indented >> between (symbol "`") (symbol "`") ident)
  return $ Op { repr = func, priority = infixFuncOpPriority, assoc = E.InfixL, isWedge = False }

infixFuncOpPriority :: Int
infixFuncOpPriority = 7

ifExpr :: Parser Expr
ifExpr = reserved "if" >> IfExpr <$> expr <* reserved "then" <*> expr <* reserved "else" <*> expr

patternMatchExpr :: Parser Expr
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

lambdaExpr :: Parser Expr
lambdaExpr = symbol "\\" >> (
      makeMatchLambdaExpr (reserved "match")    MatchLambdaExpr
  <|> makeMatchLambdaExpr (reserved "matchAll") MatchAllLambdaExpr
  <|> try (LambdaExpr <$> some arg <* symbol "->") <*> expr
  <|> PatternFunctionExpr <$> tupleOrSome lowerId <*> (symbol "=>" >> pattern))
  <?> "lambda or pattern function expression"
  where
    makeMatchLambdaExpr keyword ctor = do
      matcher <- keyword >> reserved "as" >> expr
      clauses <- reserved "with" >> matchClauses1
      return $ ctor matcher clauses

lambdaLikeExpr :: Parser Expr
lambdaLikeExpr =
        try typedMemoizedLambda
    <|> (reserved "memoizedLambda" >> MemoizedLambdaExpr <$> tupleOrSome lowerId <*> (symbol "->" >> expr))
    <|> (reserved "cambda"         >> CambdaExpr         <$> lowerId      <*> (symbol "->" >> expr))
  where
    -- memoizedLambda (x: Integer) : Integer -> body
    -- Note: retType must be parsed with typeAtomOrParenType to avoid consuming the "->" arrow
    typedMemoizedLambda = do
      reserved "memoizedLambda"
      params <- some typedParam
      _ <- symbol ":"
      retType <- typeAtomOrParenType
      _ <- symbol "->"
      body <- expr
      return $ TypedMemoizedLambdaExpr params retType body

arg :: Parser (Arg ArgPattern)
arg = InvertedScalarArg <$> (string "*$" >> argPatternAtom)
  <|> TensorArg         <$> (char '%' >> argPatternAtom)
  <|> ScalarArg         <$> (char '$' >> argPatternAtom)
  <|> TensorArg         <$> argPattern
  <?> "argument"

argPattern :: Parser ArgPattern
argPattern =
  argPatternAtom

argPatternAtom :: Parser ArgPattern
argPatternAtom
  =   APWildCard <$  symbol "_"
  <|> APTuplePat <$> parens (sepBy arg comma)
  <|> collectionPattern
  <|> APPatVar   <$> varWithIndicesLiteral
    where
      collectionPattern = brackets $ do
        elems <- sepBy arg comma
        return $ foldr APConsPat APEmptyPat elems

letExpr :: Parser Expr
letExpr = do
  binds <- reserved "let" >> oneLiner <|> alignSome binding
  body  <- reserved "in" >> expr
  return $ LetRecExpr binds body
  where
    oneLiner :: Parser [BindingExpr]
    oneLiner = braces $ sepBy binding (symbol ";")

binding :: Parser BindingExpr
binding = try bindingWithType <|> bindingWithoutType
  where
    -- Binding with type annotation: f (x: Integer) : Integer := body
    bindingWithType = do
      varWithIdx <- varWithIndicesLiteral
      let (name, indices) = extractVarWithIndices varWithIdx
      typedParams <- many typedParam
      _ <- symbol ":"
      retType <- typeExpr
      _ <- symbol ":="
      body <- expr
      let typedVar = TypedVarWithIndices name indices typedParams retType
      return $ BindWithType typedVar body

    -- Original binding without type annotation
    bindingWithoutType = do
      id <- Left <$> try varWithIndicesLiteral' <|> Right <$> pdAtom
      args <- many arg
      body <- symbol ":=" >> expr
      case (id, args) of
        (Left var, [])  -> return $ BindWithIndices var body
        (Right pdp, []) -> return $ Bind pdp body
        (Right pdp, _)  -> return $ Bind pdp (LambdaExpr args body)
        _               -> error "unreachable"

withSymbolsExpr :: Parser Expr
withSymbolsExpr = WithSymbolsExpr <$> (reserved "withSymbols" >> brackets (sepBy ident comma)) <*> expr

doExpr :: Parser Expr
doExpr = do
  stmts <- reserved "do" >> oneLiner <|> alignSome statement
  case reverse stmts of
    []                          -> return $ DoExpr []           (makeApply "return" [])
    Bind (PDTuplePat []) expr:_ -> return $ DoExpr (init stmts) expr
    _:_                         -> customFailure LastStmtInDoBlock
  where
    statement :: Parser BindingExpr
    statement = (reserved "let" >> binding) <|> Bind (PDTuplePat []) <$> expr

    oneLiner :: Parser [BindingExpr]
    oneLiner = braces $ sepBy statement (symbol ";")

seqExpr :: Parser Expr
seqExpr = SeqExpr <$> (reserved "seq" >> atomExpr) <*> atomExpr

capplyExpr :: Parser Expr
capplyExpr = CApplyExpr <$> (reserved "capply" >> atomExpr) <*> atomExpr

matcherExpr :: Parser Expr
matcherExpr = do
  reserved "matcher"
  -- Assuming it is unlikely that users want to write matchers with only 1
  -- pattern definition, the first '|' (bar) is made indispensable in matcher
  -- expression.
  MatcherExpr <$> alignSome (symbol "|" >> patternDef)
  where
    patternDef :: Parser (PrimitivePatPattern, Expr, [(PrimitiveDataPattern, Expr)])
    patternDef = do
      pp <- ppPattern
      returnMatcher <- reserved "as" >> expr <* reserved "with"
      datapat <- alignSome (symbol "|" >> dataCases)
      return (pp, returnMatcher, datapat)

    dataCases :: Parser (PrimitiveDataPattern, Expr)
    dataCases = (,) <$> pdPattern <*> (symbol "->" >> expr)

algebraicDataMatcherExpr :: Parser Expr
algebraicDataMatcherExpr = do
  reserved "algebraicDataMatcher"
  AlgebraicDataMatcherExpr <$> alignSome (symbol "|" >> patternDef)
  where
    patternDef = indentBlock lowerId atomExpr

tensorExpr :: Parser Expr
tensorExpr =
      (reserved "tensor"         >> TensorExpr         <$> atomExpr <*> atomExpr)
  <|> (reserved "generateTensor" >> GenerateTensorExpr <$> atomExpr <*> atomExpr)
  <|> (reserved "contract"       >> TensorContractExpr <$> atomExpr)
  <|> (reserved "tensorMap"      >> TensorMapExpr      <$> atomExpr <*> atomExpr)
  <|> (reserved "tensorMap2"     >> TensorMap2Expr     <$> atomExpr <*> atomExpr <*> atomExpr)
  <|> (reserved "transpose"      >> TransposeExpr      <$> atomExpr <*> atomExpr)

functionExpr :: Parser Expr
functionExpr = FunctionExpr <$> (reserved "function" >> parens (sepBy ident comma))

refsExpr :: Parser Expr
refsExpr =
      (reserved "subrefs"   >> SubrefsExpr  False <$> atomExpr <*> atomExpr)
  <|> (reserved "subrefs!"  >> SubrefsExpr  True  <$> atomExpr <*> atomExpr)
  <|> (reserved "suprefs"   >> SuprefsExpr  False <$> atomExpr <*> atomExpr)
  <|> (reserved "suprefs!"  >> SuprefsExpr  True  <$> atomExpr <*> atomExpr)
  <|> (reserved "userRefs"  >> UserrefsExpr False <$> atomExpr <*> atomExpr)
  <|> (reserved "userRefs!" >> UserrefsExpr True  <$> atomExpr <*> atomExpr)

collectionExpr :: Parser Expr
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
tupleOrParenExpr :: Parser Expr
tupleOrParenExpr = do
  elems <- symbol "(" >> try (sepBy expr comma <* symbol ")") <|> (section <* symbol ")")
  case elems of
    [x] -> return x                 -- expression wrapped in parenthesis
    _   -> return $ TupleExpr elems -- tuple
  where
    section :: Parser [Expr]
    -- Start from right, in order to parse expressions like (-1 +) correctly
    section = (:[]) <$> (rightSection <|> leftSection)

    -- Sections without the left operand: eg. (+), (+ 1)
    leftSection :: Parser Expr
    leftSection = do
      ops  <- gets exprOps
      op   <- choice $ infixFuncOp : map (infixLiteral . repr) ops
      rarg <- optional expr
      case rarg of
        -- Disabling for now... (See issue 159)
        -- Just (InfixExpr op' _ _)
        --   | assoc op' /= InfixR && priority op >= priority op' ->
        --   customFailure (IllFormedSection op op')
        _ -> return (SectionExpr op Nothing rarg)

    -- Sections with the left operand but lacks the right operand: eg. (1 +)
    rightSection :: Parser Expr
    rightSection = do
      ops  <- gets exprOps
      larg <- opExpr
      op   <- choice $ infixFuncOp : map (infixLiteral . repr) ops
      case larg of
        -- InfixExpr op' _ _
        --   | assoc op' /= InfixL && priority op >= priority op' ->
        --   customFailure (IllFormedSection op op')
        _ -> return (SectionExpr op (Just larg) Nothing)

vectorExpr :: Parser Expr
vectorExpr = VectorExpr <$> between (symbol "[|") (symbol "|]") (sepEndBy expr comma)

hashExpr :: Parser Expr
hashExpr = HashExpr <$> hashBraces (sepEndBy hashElem comma)
  where
    hashBraces = between (symbol "{|") (symbol "|}")
    hashElem = parens $ (,) <$> expr <*> (comma >> expr)

index :: Parser a -> Parser (IndexExpr a)
index p = SupSubscript <$> (string "~_" >> p)
    <|> try (char '_' >> subscript)
    <|> try (char '~' >> superscript)
    <|> try (Userscript <$> (char '|' >> p))
    <?> "index"
  where
    subscript = do
      e1 <- p
      e2 <- optional (string "..._" >> p)
      case e2 of
        Nothing  -> return $ Subscript e1
        Just e2' -> return $ MultiSubscript e1 e2'
    superscript = do
      e1 <- p
      e2 <- optional (string "...~" >> p)
      case e2 of
        Nothing  -> return $ Superscript e1
        Just e2' -> return $ MultiSuperscript e1 e2'

atomOrApplyExpr :: Parser Expr
atomOrApplyExpr = do
  (func, args) <- indentBlock atomExpr atomExpr
  return $ case args of
             [] -> func
             _  -> ApplyExpr func args

-- (Possibly indexed) atomic expressions
atomExpr :: Parser Expr
atomExpr = do
  e <- atomExpr'
  override <- isNothing <$> optional (try (string "..." <* lookAhead (index atomExpr')))
  indices <- many (index atomExpr')
  return $ case indices of
             [] -> e
             _  -> IndexedExpr override e indices

-- Atomic expressions without index
atomExpr' :: Parser Expr
atomExpr' = anonParamFuncExpr      -- must come before |constantExpr|
        <|> anonTupleParamFuncExpr -- must come before |tupleOrParenExpr|
        <|> anonListParamFuncExpr  -- must come before |collectionExpr|
        <|> ConstantExpr <$> constantExpr
        <|> FreshVarExpr <$ symbol "#"
        <|> VarExpr <$> ident
        <|> vectorExpr     -- must come before |collectionExpr|
        <|> collectionExpr
        <|> tupleOrParenExpr
        <|> hashExpr
        <|> QuoteExpr <$> (try (symbol "`" <* notFollowedBy ident) >> atomExpr') -- must come after |constantExpr|
        <|> QuoteSymbolExpr <$> try (char '\'' >> atomExpr')
        <|> AnonParamExpr  <$> try (char '%' >> positiveIntegerLiteral)
        <?> "atomic expression"

anonParamFuncExpr :: Parser Expr
anonParamFuncExpr = do
  n    <- try (L.decimal <* char '#') -- No space after the index
  body <- atomExpr                    -- No space after '#'
  return $ AnonParamFuncExpr n body

anonTupleParamFuncExpr :: Parser Expr
anonTupleParamFuncExpr = do
  n <- try (char '(' *> L.decimal <* string ")#")
  AnonTupleParamFuncExpr n <$> atomExpr

anonListParamFuncExpr :: Parser Expr
anonListParamFuncExpr = do
  n <- try (char '[' *> L.decimal <* string "]#")
  AnonListParamFuncExpr n <$> atomExpr

constantExpr :: Parser ConstantExpr
constantExpr = numericExpr
           <|> BoolExpr <$> boolLiteral
           <|> CharExpr <$> try charLiteral        -- try for quoteExpr
           <|> StringExpr . pack <$> stringLiteral
           <|> SomethingExpr <$ reserved "something"
           <|> UndefinedExpr <$ reserved "undefined"

numericExpr :: Parser ConstantExpr
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
  LoopPat <$> (reserved "loop" >> char '$' >> ident) <*> loopRange
          <*> atomPattern <*> atomPattern
  where
    loopRange :: Parser LoopRange
    loopRange =
      parens $ do start <- expr
                  ends  <- option (defaultEnds start) (try $ comma >> expr)
                  as    <- option WildCard (comma >> pattern)
                  return $ LoopRange start ends as

    defaultEnds s =
      makeApply "from"
                [makeApply "-'" [s, ConstantExpr (IntegerExpr 1)]]

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

pdAtom :: Parser PrimitiveDataPattern
pdAtom = PDWildCard    <$ symbol "_"
     <|> PDPatVar      <$> patVarLiteral
     <|> PDPatVar      <$> ident
     <|> PDConstantPat <$> constantExpr
     <|> pdCollection
     <|> makeTupleOrParen pdPattern PDTuplePat
  where
    pdCollection :: Parser PrimitiveDataPattern
    pdCollection = do
      elts <- brackets (sepBy pdPattern comma)
      return (foldr PDConsPat PDEmptyPat elts)

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

varWithIndicesLiteral :: Parser VarWithIndices
varWithIndicesLiteral =
  lexeme (VarWithIndices <$> ident' <*> many varIndex)

varWithIndicesLiteral' :: Parser VarWithIndices
varWithIndicesLiteral' =
  lexeme (VarWithIndices <$> ident' <*> some varIndex)

varIndex :: Parser VarIndex
varIndex = (char '_' >> subscript)
       <|> (char '~' >> supscript)
       <|> parens (VGroupScripts <$> some varIndex)
       <|> braces (VSymmScripts <$> some varIndex)
       <|> brackets (VAntiSymmScripts <$> some varIndex)
  where
    subscript = VSubscript <$> ident'
            <|> (do
              (n, s) <- parens $ (,) <$> ident' <*> (char '_' >> positiveIntegerLiteral)
              _ <- string "..." >> char '_'
              e <- parens $ string n >> char '_' >> ident'
              return (VMultiSubscript n s e))
    supscript = VSuperscript <$> ident'
            <|> (do
              (n, s) <- parens $ (,) <$> ident' <*> (char '_' >> positiveIntegerLiteral)
              _ <- string "..." >> char '~'
              e <- parens $ string n >> char '_' >> ident'
              return (VMultiSuperscript n s e))

patVarLiteral :: Parser String
patVarLiteral = char '$' >> ident

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

-- |ident| not followed by a space
ident' :: Parser String
ident' = try (p >>= check)
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
  , "def"
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
