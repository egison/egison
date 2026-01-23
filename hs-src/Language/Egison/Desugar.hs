{-# LANGUAGE TupleSections #-}

{- |
Module      : Language.Egison.Desugar
Licence     : MIT

This module implements Phase 3-4: Syntactic Desugaring (for untyped path).
For the typed path, desugaring is done inside type inference.

Syntactic Desugaring (Phase 3-4):
  - Operator desugaring (infix to function application)
  - Anonymous function expansion (cambda: 1#($1 + $2) etc.)
  - Match-lambda expansion (convert to match expressions)
  - Other syntactic sugar expansions
  
Design Note (design/implementation.md):
Pattern matching itself is NOT desugared here. Match expressions (IMatchExpr, 
IMatchAllExpr) are kept as-is and processed during evaluation (Phase 10).
This allows Egison's sophisticated pattern matching to be implemented in the evaluator.
-}

module Language.Egison.Desugar
    ( desugarTopExpr
    , desugarTopExprs
    , desugarExpr
    , transVarIndex
    ) where

import           Control.Monad.Except   (throwError)
import           Data.Char              (toUpper)
import           Data.Foldable          (foldrM)
import           Data.List              (union)
import           Data.Text              (pack)

import           Language.Egison.AST
import           Language.Egison.Data
import           Language.Egison.IExpr
import           Language.Egison.RState
import           Language.Egison.Type.Types (sanitizeMethodName, typeToName, typeConstructorName, 
                                             typeExprToType, capitalizeFirst, lowerFirst)


desugarTopExpr :: TopExpr -> EvalM (Maybe ITopExpr)
desugarTopExpr (Define vwi expr) = do
  (var, iexpr) <- desugarDefineWithIndices vwi expr
  return . Just $ IDefine var iexpr
desugarTopExpr (DefineWithType typedVwi expr) = do
  -- Convert typed definition to regular definition
  -- Type information is used for type checking, but the runtime representation is the same
  -- Note: Constraints are preserved in the type scheme (by EnvBuilder),
  -- and dictionary passing is handled in TypeClassExpand phase
  let name = typedVarName typedVwi
      indices = typedVarIndices typedVwi
      params = typedVarParams typedVwi
      vwi = VarWithIndices name indices
  -- If there are typed parameters, wrap the body in a lambda
  case params of
    [] -> do
      (var, iexpr) <- desugarDefineWithIndices vwi expr
      return . Just $ IDefine var iexpr
    _  -> do
      -- Create lambda arguments from typed parameters
      let argPatterns = map typedParamToArgPattern params
          lambdaExpr = LambdaExpr argPatterns expr
      (var, iexpr) <- desugarDefineWithIndices vwi lambdaExpr
      return . Just $ IDefine var iexpr
desugarTopExpr (Test expr)     = Just . ITest <$> desugar expr
desugarTopExpr (Execute expr)  = Just . IExecute <$> desugar expr
desugarTopExpr (Load file)     = return . Just $ ILoad file
desugarTopExpr (LoadFile file) = return . Just $ ILoadFile file

-- Type class declarations: generate dictionary-passing wrapper functions
-- and register the class methods for dispatch
-- For a class like:
--   class Eq a where
--     (==) (x: a) (y: a) : Bool
-- We generate:
--   1. Dictionary wrapper: def classEqEq dict x y := (dict_"eq") x y
--   2. Instance registry variable: def registryEq := {| |}
--   3. Auto-dispatch function: def autoEqEq x y := (resolveEq x)_"eq" x y
desugarTopExpr (ClassDeclExpr (ClassDecl classNm _typeParams _supers methods)) = do
  -- Generate dictionary-passing wrapper functions for each method
  methodWrappers <- mapM (desugarClassMethod classNm) methods
  -- Generate empty instance registry
  let registryDef = makeRegistryDef classNm
  case methodWrappers of
    [] -> return Nothing
    _  -> return $ Just $ IDefineMany (registryDef : methodWrappers)
  where
    desugarClassMethod :: String -> ClassMethod -> EvalM (Var, IExpr)
    desugarClassMethod clsNm (ClassMethod methName methParams _retType _defaultImpl) = do
      -- Generate function name: e.g., "classEqEq" for (==) in Eq
      let wrapperName = "class" ++ clsNm ++ capitalizeFirst (sanitizeMethodName methName)
          var = stringToVar wrapperName
          dictVar = "dict"
          -- Parameter names: dict, x, y, ...
          paramNames = map extractParamName methParams
          allParams = dictVar : paramNames
      -- Build the body: (dict_"methodName") x y ...
      -- dict_"eq" is hash access, then apply to remaining params
      let dictAccessExpr = IIndexedExpr False (IVarExpr dictVar) 
                             [Sub (IConstantExpr (StringExpr (pack (sanitizeMethodName methName))))]
          bodyExpr = if null paramNames
                     then dictAccessExpr
                     else IApplyExpr dictAccessExpr (map IVarExpr paramNames)
          lambdaExpr = ILambdaExpr Nothing (map stringToVar allParams) bodyExpr
      return (var, lambdaExpr)
    
    -- Create empty instance registry: registryEq := {| |}
    makeRegistryDef :: String -> (Var, IExpr)
    makeRegistryDef clsNm = 
      let registryName = "registry" ++ clsNm
          var = stringToVar registryName
      in (var, IHashExpr [])
    
    extractParamName :: TypedParam -> String
    extractParamName (TPVar name _) = name
    extractParamName (TPInvertedVar name _) = name
    extractParamName (TPUntypedVar name) = name
    extractParamName _ = "x"  -- fallback

-- Instance declarations: generate a dictionary and individual method definitions
-- For an instance like:
--   instance Eq Integer where
--     (==) x y := x = y
--     (/=) x y := not (x = y)
-- We generate:
--   1. Individual method functions:
--      def eqIntegerEq x y := x = y
--      def eqIntegerNeq x y := not (x = y)
--   2. A dictionary for the instance:
--      def eqInteger := {| ("eq", eqIntegerEq), ("neq", eqIntegerNeq) |}
desugarTopExpr (InstanceDeclExpr (InstanceDecl constraints classNm instTypes methods)) = do
  -- Check if instTypes is not empty
  if null instTypes
    then return Nothing
    else do
      -- Use type constructor name only (without type parameters)
      -- e.g., "Collection" not "Collectiona" for [a]
      let instTypeName = typeConstructorName (typeExprToType (head instTypes))
      -- Generate individual method definitions with constraint parameters
      methodDefs <- mapM (desugarInstanceMethod constraints classNm instTypeName) methods
      -- Generate dictionary definition (with constraints if any)
      let dictDef = makeDictDef constraints classNm instTypeName methods
      -- Return all definitions
      case methodDefs of
        []  -> return Nothing
        _   -> return $ Just $ IDefineMany (dictDef : methodDefs)
  where
    desugarInstanceMethod :: [ConstraintExpr] -> String -> String -> InstanceMethod -> EvalM (Var, IExpr)
    desugarInstanceMethod _constrs clsNm typNm (InstanceMethod methName params body) = do
      -- Generate function name using type constructor name only
      -- e.g., "eqCollectionEq" not "eqCollectionaEq" for instance {Eq a} Eq [a]
      let funcName = lowerFirst clsNm ++ typNm ++ capitalizeFirst (sanitizeMethodName methName)
          var = stringToVar funcName
      
      -- Do NOT add dictionary parameters here!
      -- Dictionary parameters will be added automatically by addDictionaryParametersT
      -- after type inference, based on the inferred constraints.
      -- This allows the method body to be properly type-checked with constraints.
      
      -- Create lambda expression with only the method parameters
      let lambdaArgs = map (\p -> Arg (APPatVar (VarWithIndices p []))) params
          lambdaExpr = if null params then body else LambdaExpr lambdaArgs body
      iexpr <- desugar lambdaExpr
      return (var, iexpr)
    
    makeDictDef :: [ConstraintExpr] -> String -> String -> [InstanceMethod] -> (Var, IExpr)
    makeDictDef _constrs clsNm typNm meths =
      let dictName = lowerFirst clsNm ++ typNm  -- e.g., "eqCollection"
          dictVar = stringToVar dictName
          
          -- For nested instances (with constraints), the dictionary becomes a function
          -- that takes dictionary parameters and returns a hash.
          -- e.g., for instance {Eq a} Eq [a]:
          --   eqCollection = \dict_Eq -> {| ("eq", eqCollectionEq dict_Eq), ... |}
          --
          -- Dictionary parameters will be automatically added by addDictionaryParametersT
          -- after type inference, so we don't add them here manually.
          -- We just create the hash with references to the methods.
          
          hashEntries = map (makeHashEntry clsNm typNm) meths
          hashExpr = IHashExpr hashEntries
      in (dictVar, hashExpr)
    
    makeHashEntry :: String -> String -> InstanceMethod -> (IExpr, IExpr)
    makeHashEntry clsNm typNm (InstanceMethod methName _ _) =
      let keyExpr = IConstantExpr (StringExpr (pack (sanitizeMethodName methName)))
          -- Reference to the method function
          funcName = lowerFirst clsNm ++ typNm ++ capitalizeFirst (sanitizeMethodName methName)
          valueExpr = IVarExpr funcName
      in (keyExpr, valueExpr)
    

-- Inductive declarations don't produce runtime code
-- Constructor registration is handled by the type system
desugarTopExpr (InductiveDecl _ _ _) = return Nothing

-- Infix declarations don't produce runtime code
desugarTopExpr (InfixDecl _ _) = return Nothing
desugarTopExpr (PatternInductiveDecl _ _ _) = return Nothing  -- Handled in environment building phase
desugarTopExpr (PatternFunctionDecl _ _ _ _ _) = return Nothing  -- Handled in environment building phase

-- Symbol declarations
desugarTopExpr (DeclareSymbol names mTypeExpr) = do
  -- Convert type expression to type (defaults to Integer if not specified)
  let ty = case mTypeExpr of
             Just texpr -> typeExprToType texpr
             Nothing    -> typeExprToType TEInt
  return . Just $ IDeclareSymbol names (Just ty)

-- | Convert TypedParam to Arg ArgPattern for lambda expressions
typedParamToArgPattern :: TypedParam -> Arg ArgPattern
typedParamToArgPattern (TPVar pname _) =
  Arg (APPatVar (VarWithIndices pname []))
typedParamToArgPattern (TPInvertedVar pname _) =
  InvertedArg (APPatVar (VarWithIndices pname []))
typedParamToArgPattern (TPTuple elems) =
  Arg (APTuplePat (map typedParamToArgPattern elems))
typedParamToArgPattern (TPWildcard _) =
  Arg APWildCard
typedParamToArgPattern (TPUntypedVar pname) =
  Arg (APPatVar (VarWithIndices pname []))
typedParamToArgPattern TPUntypedWildcard =
  Arg APWildCard

desugarTopExprs :: [TopExpr] -> EvalM [ITopExpr]
desugarTopExprs [] = return []
desugarTopExprs (expr : exprs) = do
  expr' <- desugarTopExpr expr
  case expr' of
    Nothing    -> desugarTopExprs exprs
    Just expr' -> (expr' :) <$> desugarTopExprs exprs

desugarExpr :: Expr -> EvalM IExpr
desugarExpr = desugar

desugar :: Expr -> EvalM IExpr
desugar (ConstantExpr c) = return $ IConstantExpr c
desugar (VarExpr var)    = return $ IVarExpr var

desugar (AlgebraicDataMatcherExpr patterns) = do
  matcherName <- fresh
  let matcherRef = IVarExpr matcherName
  matcher <- genMatcherClauses patterns matcherRef
  return $ ILetRecExpr [(PDPatVar (stringToVar matcherName), matcher)] matcherRef
    where
      genMatcherClauses :: [(String, [Expr])] ->  IExpr -> EvalM IExpr
      genMatcherClauses patterns matcher = do
        main <- genMainClause patterns matcher
        body <- mapM genMatcherClause patterns
        footer <- genSomethingClause
        let clauses = [main] ++ body ++ [footer]
        return $ IMatcherExpr clauses

      genMainClause :: [(String, [Expr])] -> IExpr -> EvalM (PrimitivePatPattern, IExpr, [(IPrimitiveDataPattern, IExpr)])
      genMainClause patterns matcher = do
        clauses <- genClauses patterns
        return (PPValuePat "val", ITupleExpr [],
                [(PDPatVar (stringToVar "tgt"),
                    IMatchExpr BFSMode
                               (ITupleExpr [IVarExpr "val", IVarExpr "tgt"])
                               (ITupleExpr [matcher, matcher])
                               clauses)])
        where
          genClauses :: [(String, [Expr])] -> EvalM [IMatchClause]
          genClauses patterns = (++) <$> mapM genClause patterns
                                     <*> pure [(ITuplePat [IWildCard, IWildCard], matchingFailure)]

          genClause :: (String, [Expr]) -> EvalM IMatchClause
          genClause pattern = do
            (pat0, pat1) <- genMatchingPattern pattern
            return (ITuplePat [pat0, pat1], matchingSuccess)

          genMatchingPattern :: (String, [Expr]) -> EvalM (IPattern, IPattern)
          genMatchingPattern (name, patterns) = do
            names <- mapM (const fresh) patterns
            return (IInductivePat name (map IPatVar names),
                    IInductivePat name (map (IValuePat . IVarExpr) names))

      genMatcherClause :: (String, [Expr]) -> EvalM (PrimitivePatPattern, IExpr, [(IPrimitiveDataPattern, IExpr)])
      genMatcherClause pattern = do
        (ppat, matchers) <- genPrimitivePatPat pattern
        (dpat, body)     <- genPrimitiveDataPat pattern
        return (ppat, ITupleExpr matchers, [(dpat, ICollectionExpr [ITupleExpr body]), (PDWildCard, matchingFailure)])

        where
          genPrimitivePatPat :: (String, [Expr]) -> EvalM (PrimitivePatPattern, [IExpr])
          genPrimitivePatPat (name, matchers) = do
            patterns' <- mapM (const $ return PPPatVar) matchers
            matchers' <- mapM desugar matchers
            return (PPInductivePat name patterns', matchers')

          genPrimitiveDataPat :: (String, [Expr]) -> EvalM (IPrimitiveDataPattern, [IExpr])
          genPrimitiveDataPat (name, patterns) = do
            patterns' <- mapM (const fresh) patterns
            return (PDInductivePat (capitalize name) $ map (PDPatVar . stringToVar) patterns', map IVarExpr patterns')

          capitalize :: String -> String
          capitalize (x:xs) = toUpper x : xs


      genSomethingClause :: EvalM (PrimitivePatPattern, IExpr, [(IPrimitiveDataPattern, IExpr)])
      genSomethingClause =
        return (PPPatVar, ITupleExpr [IConstantExpr SomethingExpr], [(PDPatVar (stringToVar "tgt"), ICollectionExpr [IVarExpr "tgt"])])

      matchingSuccess :: IExpr
      matchingSuccess = ICollectionExpr [ITupleExpr []]

      matchingFailure :: IExpr
      matchingFailure = ICollectionExpr []

desugar (MatchAllLambdaExpr matcher clauses) = do
  name <- fresh
  ILambdaExpr Nothing [stringToVar name] <$>
    desugar (MatchAllExpr BFSMode (VarExpr name) matcher clauses)

desugar (MatchLambdaExpr matcher clauses) = do
  name <- fresh
  ILambdaExpr Nothing [stringToVar name] <$>
    desugar (MatchExpr BFSMode (VarExpr name) matcher clauses)

desugar (IndexedExpr override expr indices) = do
  expr' <- desugar expr
  desugarIndexedExpr override expr' indices
  where
    desugarIndexedExpr :: Bool -> IExpr -> [IndexExpr Expr] -> EvalM IExpr
    desugarIndexedExpr override expr' indices =
      case indices of
        [] -> return expr'
        (MultiSubscript x y:indices') ->
          case (x, y) of
            (IndexedExpr override1 e1 [n1], IndexedExpr _ _ [n2]) -> do
              expr'' <- desugarMultiScript override expr' ISubrefsExpr override1 e1 n1 n2
              desugarIndexedExpr False expr'' indices'
            _ -> throwError $ Default "Index should be IndexedExpr for multi subscript"
        (MultiSuperscript x y:indices') ->
          case (x, y) of
            (IndexedExpr override1 e1 [n1], IndexedExpr _ _ [n2]) -> do
              expr'' <- desugarMultiScript override expr' ISuprefsExpr override1 e1 n1 n2
              desugarIndexedExpr False expr'' indices'
            _ -> throwError $ Default "Index should be IndexedExpr for multi superscript"
        _ -> do
          let (is, indices') = break isMulti indices
          expr'' <- IIndexedExpr override expr' <$> mapM desugarIndex is
          desugarIndexedExpr False expr'' indices'
    desugarMultiScript override expr' refExpr override1 e1 n1 n2 = do
      k     <- fresh
      n1'   <- desugar (extractIndexExpr n1)
      n2'   <- desugar (extractIndexExpr n2)
      e1'   <- desugar e1
      return $ refExpr override expr' (makeIApply "map"
                                           [ILambdaExpr Nothing [stringToVar k] (IIndexedExpr override1 e1' [Sub (IVarExpr k)]),
                                            makeIApply "between" [n1', n2']])
    isMulti (MultiSubscript _ _)   = True
    isMulti (MultiSuperscript _ _) = True
    isMulti _                      = False


desugar (SubrefsExpr bool expr1 expr2) =
  ISubrefsExpr bool <$> desugar expr1 <*> desugar expr2

desugar (SuprefsExpr bool expr1 expr2) =
  ISuprefsExpr bool <$> desugar expr1 <*> desugar expr2

desugar (UserrefsExpr bool expr1 expr2) =
  IUserrefsExpr bool <$> desugar expr1 <*> desugar expr2

desugar (TupleExpr exprs) = ITupleExpr <$> mapM desugar exprs
desugar (CollectionExpr xs) = ICollectionExpr <$> mapM desugar xs
desugar (ConsExpr x xs) = IConsExpr <$> desugar x <*> desugar xs
desugar (JoinExpr x xs) = IJoinExpr <$> desugar x <*> desugar xs

desugar (HashExpr exprPairs) =
  IHashExpr <$> mapM (\(expr1, expr2) -> (,) <$> desugar expr1 <*> desugar expr2) exprPairs

desugar (VectorExpr exprs) =
  IVectorExpr <$> mapM desugar exprs

desugar (TensorExpr nsExpr xsExpr) =
  ITensorExpr <$> desugar nsExpr <*> desugar xsExpr

-- Desugar of LambdaExpr takes place in 2 stages.
-- * LambdaExpr -> LambdaExpr'  : Desugar pattern matches at the arg positions
-- * LambdaExpr' -> ILambdaExpr : Desugar Arg and InvertedArg
desugar (LambdaExpr args expr) = do
  (args', expr') <- foldrM desugarArg ([], expr) args
  desugar $ LambdaExpr' args' expr'
  where
    desugarArg :: Arg ArgPattern -> ([Arg VarWithIndices], Expr) -> EvalM ([Arg VarWithIndices], Expr)
    desugarArg (Arg x) (args, expr) = do
      (var, expr') <- desugarArgPat x expr
      return (Arg var : args, expr')
    desugarArg (InvertedArg x) (args, expr) = do
      (var, expr') <- desugarArgPat x expr
      return (InvertedArg var : args, expr')

    -- Desugar argument patterns. Examples:
    -- \$(%x, %y) -> expr   ==> \$tmp -> let (tmp1, tmp2) := tmp in (\%x %y -> expr) tmp1 tmp2
    -- \(x, (y, z)) -> expr ==> \tmp  -> let (tmp1, tmp2) := tmp in (\x (y, z) -> expr) tmp1 tmp2
    -- \%($x :: xs) -> expr ==> \%tmp -> let (tmp1 :: xs) := tmp in (\$x %xs -> expr) tmp1 tmp2
    desugarArgPat :: ArgPattern -> Expr -> EvalM (VarWithIndices, Expr)
    desugarArgPat APWildCard expr = do
      tmp <- fresh
      let tmp' = stringToVarWithIndices tmp
      return (tmp', LetExpr [Bind PDWildCard (VarExpr tmp)] expr)
    desugarArgPat (APPatVar var) expr = return (var, expr)
    desugarArgPat (APTuplePat args) expr = do
      tmp  <- fresh
      let tmp' = stringToVarWithIndices tmp
      tmps <- mapM (const fresh) args
      return (tmp', LetExpr [Bind (PDTuplePat (map PDPatVar tmps)) (VarExpr tmp)]
                      (ApplyExpr (LambdaExpr args expr) (map VarExpr tmps)))
    desugarArgPat (APInductivePat ctor args) expr = do
      tmp  <- fresh
      let tmp' = stringToVarWithIndices tmp
      tmps <- mapM (const fresh) args
      return (tmp', LetExpr [Bind (PDInductivePat ctor (map PDPatVar tmps)) (VarExpr tmp)]
                      (ApplyExpr (LambdaExpr args expr) (map VarExpr tmps)))
    desugarArgPat APEmptyPat expr = do
      tmp <- fresh
      let tmp' = stringToVarWithIndices tmp
      return (tmp', LetExpr [Bind PDEmptyPat (VarExpr tmp)] expr)
    desugarArgPat (APConsPat arg1 arg2) expr = do
      tmp  <- fresh
      let tmp' = stringToVarWithIndices tmp
      tmp1 <- fresh
      tmp2 <- fresh
      return (tmp', LetExpr [Bind (PDConsPat (PDPatVar tmp1) (PDPatVar tmp2)) (VarExpr tmp)]
                     (ApplyExpr (LambdaExpr [arg1, Arg arg2] expr) [VarExpr tmp1, VarExpr tmp2]))
    desugarArgPat (APSnocPat arg1 arg2) expr = do
      tmp  <- fresh
      let tmp' = stringToVarWithIndices tmp
      tmp1 <- fresh
      tmp2 <- fresh
      return (tmp', LetExpr [Bind (PDSnocPat (PDPatVar tmp1) (PDPatVar tmp2)) (VarExpr tmp)]
                     (ApplyExpr (LambdaExpr [Arg arg1, arg2] expr) [VarExpr tmp1, VarExpr tmp2]))

desugar (LambdaExpr' vwis expr) = do
  let (vwis', expr') = foldr desugarInvertedArgs ([], expr) vwis
  let args' = map varWithIndicesToVar vwis'
  expr' <- desugar expr'
  return $ ILambdaExpr Nothing args' expr'
  where
    desugarInvertedArgs :: Arg VarWithIndices -> ([VarWithIndices], Expr) -> ([VarWithIndices], Expr)
    desugarInvertedArgs (Arg x) (args, expr) = (x : args, expr)
    desugarInvertedArgs (InvertedArg x) (args, expr) =
      let varName = extractNameFromVarWithIndices x
          flippedExpr = FlipIndicesExpr (VarExpr varName)
          bindPat = PDPatVar varName
      in (x : args, LetExpr [Bind bindPat flippedExpr] expr)

desugar (MemoizedLambdaExpr names expr) =
  IMemoizedLambdaExpr names <$> desugar expr

-- Typed memoized lambda is desugared the same way (type info used only for type checking)
desugar (TypedMemoizedLambdaExpr params _ body) =
  IMemoizedLambdaExpr (extractParamNames params) <$> desugar body
  where
    extractParamNames = concatMap extractName
    extractName (TPVar name _) = [name]
    extractName (TPInvertedVar name _) = [name]
    extractName (TPTuple elems) = concatMap extractName elems
    extractName (TPWildcard _) = []
    extractName (TPUntypedVar name) = [name]
    extractName TPUntypedWildcard = []

desugar (CambdaExpr name expr) =
  ICambdaExpr name <$> desugar expr

desugar (PatternFunctionExpr _names _pattern) =
  -- Pattern functions are only defined at TopExpr level
  -- They should not appear in expression context
  throwError $ Default "Pattern functions cannot be used as expressions"

desugar (IfExpr expr0 expr1 expr2) =
  IIfExpr <$> desugar expr0 <*> desugar expr1 <*> desugar expr2

desugar (LetExpr binds expr) =
  ILetExpr <$> desugarBindings binds <*> desugar expr

desugar (LetRecExpr binds expr) =
  ILetRecExpr <$> desugarBindings binds <*> desugar expr

desugar (WithSymbolsExpr vars expr) =
  IWithSymbolsExpr vars <$> desugar expr

desugar (MatchExpr pmmode expr0 expr1 clauses) =
  IMatchExpr pmmode <$> desugar expr0 <*> desugar expr1 <*> desugarMatchClauses clauses

desugar (MatchAllExpr pmmode expr0 expr1 clauses) =
  IMatchAllExpr pmmode <$> desugar expr0 <*> desugar expr1 <*> desugarMatchClauses clauses

desugar (DoExpr binds expr) =
  IDoExpr <$> desugarBindings binds <*> desugar expr

desugar (PrefixExpr "-" expr) = do
  expr' <- desugar expr
  return $ makeIApply "*" [IConstantExpr (IntegerExpr (-1)), expr']
desugar (PrefixExpr "!" (ApplyExpr expr args)) =
  IWedgeApplyExpr <$> desugar expr <*> mapM desugar args
desugar (PrefixExpr "'" expr) = IQuoteExpr <$> desugar expr
desugar (PrefixExpr "`" expr) = IQuoteSymbolExpr <$> desugar expr
desugar (PrefixExpr op _) = fail ("Unknown prefix " ++ op)

desugar (InfixExpr op expr1 expr2) | isWedge op =
  (\x y -> IWedgeApplyExpr (IVarExpr (repr op)) [x, y])
    <$> desugar expr1 <*> desugar expr2

desugar (InfixExpr op expr1 expr2) | repr op == "::" =
  IConsExpr <$> desugar expr1 <*> desugar expr2
desugar (InfixExpr op expr1 expr2) | repr op == "++" =
  IJoinExpr <$> desugar expr1 <*> desugar expr2
desugar (InfixExpr op expr1 expr2) =
  (\x y -> makeIApply (repr op) [x, y]) <$> desugar expr1 <*> desugar expr2

-- section
--
-- If `op` is not a cambda, simply desugar it into the function
desugar (SectionExpr op Nothing Nothing)
  | not (isWedge op || repr op `elem` ["::", "++"]) =
    desugar (VarExpr (repr op))
desugar (SectionExpr op Nothing Nothing) = do
  x <- fresh
  y <- fresh
  ILambdaExpr Nothing [stringToVar x, stringToVar y] <$> desugar (InfixExpr op (VarExpr x) (VarExpr y))

desugar (SectionExpr op Nothing (Just expr2)) = do
  x <- fresh
  ILambdaExpr Nothing [stringToVar x] <$> desugar (InfixExpr op (VarExpr x) expr2)

desugar (SectionExpr op (Just expr1) Nothing) = do
  y <- fresh
  ILambdaExpr Nothing [stringToVar y] <$> desugar (InfixExpr op expr1 (VarExpr y))

desugar SectionExpr{} = throwError $ Default "Cannot reach here: section with both arguments"

desugar (SeqExpr expr0 expr1) =
  ISeqExpr <$> desugar expr0 <*> desugar expr1

desugar (GenerateTensorExpr fnExpr sizeExpr) =
  IGenerateTensorExpr <$> desugar fnExpr <*> desugar sizeExpr

desugar (TensorContractExpr tExpr) =
  ITensorContractExpr <$> desugar tExpr

desugar (TensorMapExpr (LambdaExpr' [x] (TensorMapExpr (LambdaExpr' [y] expr) b)) a) =
  desugar (TensorMap2Expr (LambdaExpr' [x, y] expr) a b)
desugar (TensorMapExpr (LambdaExpr [x] (TensorMapExpr (LambdaExpr [y] expr) b)) a) =
  desugar (TensorMap2Expr (LambdaExpr [x, y] expr) a b)

desugar (TensorMapExpr fnExpr tExpr) =
  ITensorMapExpr <$> desugar fnExpr <*> desugar tExpr

desugar (TensorMap2Expr fnExpr t1Expr t2Expr) =
  ITensorMap2Expr <$> desugar fnExpr <*> desugar t1Expr <*> desugar t2Expr

desugar (TransposeExpr vars expr) =
  -- ITransposeExpr takes (permutation, tensor) as arguments to match tTranspose
  ITransposeExpr <$> desugar vars <*> desugar expr

desugar (FlipIndicesExpr expr) =
  IFlipIndicesExpr <$> desugar expr

desugar (ApplyExpr expr args) =
  IApplyExpr <$> desugar expr <*> mapM desugar args

desugar FreshVarExpr = do
  id <- fresh
  return $ IVarExpr (":::" ++ id)

desugar (MatcherExpr patternDefs) =
  IMatcherExpr <$> mapM desugarPatternDef patternDefs

desugar (AnonParamExpr n) = return $ IVarExpr ('%' : show n)

desugar (AnonParamFuncExpr n expr) = do
  let args = map (\n -> stringToVarWithIndices ('%' : show n)) [1..n]
  lambda <- desugar $ LambdaExpr' (map Arg args) expr
  return $ ILetRecExpr [(PDPatVar (stringToVar "%0"), lambda)] (IVarExpr "%0")

desugar (AnonTupleParamFuncExpr 1 expr) = do
  lambda <- desugar $ LambdaExpr' [Arg (stringToVarWithIndices "%1")] expr
  return $ ILetRecExpr [(PDPatVar (stringToVar "%0"), lambda)] (IVarExpr "%0")
desugar (AnonTupleParamFuncExpr n expr) = do
  let args = map (\n -> stringToVarWithIndices ('%' : show n)) [1..n]
  lambda <- desugar $
    LambdaExpr [Arg (APTuplePat $ map (Arg . APPatVar) args)] expr
  return $ ILetRecExpr [(PDPatVar (stringToVar "%0"), lambda)] (IVarExpr "%0")

desugar (AnonListParamFuncExpr n expr) = do
  let args' = map (\n -> Arg (APPatVar (stringToVarWithIndices ('%' : show n)))) [1..n]
  let args = foldr APConsPat APEmptyPat args'
  lambda <- desugar $ LambdaExpr [Arg args] expr
  return $ ILetRecExpr [(PDPatVar (stringToVar "%0"), lambda)] (IVarExpr "%0")

desugar (QuoteExpr expr) =
  IQuoteExpr <$> desugar expr

desugar (QuoteSymbolExpr expr) =
  IQuoteSymbolExpr <$> desugar expr

desugar (WedgeApplyExpr expr args) =
  IWedgeApplyExpr <$> desugar expr <*> mapM desugar args

desugar (FunctionExpr args) = return $ IFunctionExpr args

-- Type annotation is erased at runtime
desugar (TypeAnnotation expr _typeExpr) = desugar expr

-- Typed lambda is desugared to regular lambda
desugar (TypedLambdaExpr params _retType body) = do
  let args = map (\(name, _) -> Arg (APPatVar (VarWithIndices name []))) params
  desugar $ LambdaExpr args body

desugarIndex :: IndexExpr Expr -> EvalM (Index IExpr)
desugarIndex (Subscript e)    = Sub <$> desugar e
desugarIndex (Superscript e)  = Sup <$> desugar e
desugarIndex (SupSubscript e) = SupSub <$> desugar e
desugarIndex (Userscript e)   = User <$> desugar e
desugarIndex _                = undefined

desugarPattern :: Pattern -> EvalM IPattern
desugarPattern pat =
  case collectName pat of
    []    -> desugarPattern' pat
    names -> ILetPat (map makeBinding names) <$> desugarPattern' pat
 where
   collectNames :: [Pattern] -> [String]
   collectNames pats = foldl union [] (map collectName pats)

   collectName :: Pattern -> [String]
   collectName (ForallPat pat1 pat2)                           = collectName pat1 `union` collectName pat2
   collectName (InfixPat _ pat1 pat2)                          = collectName pat1 `union` collectName pat2
   collectName (NotPat pat)                                    = collectName pat
   collectName (AndPat pat1 pat2)                              = collectName pat1 `union` collectName pat2
   collectName (OrPat pat1 pat2)                               = collectName pat1 `union` collectName pat2
   collectName (TuplePat pats)                                 = collectNames pats
   collectName (InductiveOrPApplyPat _ pats)                   = collectNames pats
   collectName (InductivePat _ pats)                           = collectNames pats
   collectName (PApplyPat _ pats)                              = collectNames pats
   collectName (DApplyPat _ pats)                              = collectNames pats
   collectName (LoopPat _ (LoopRange _ _ endNumPat) pat1 pat2) = collectName endNumPat `union` collectName pat1 `union` collectName pat2
   collectName (LetPat _ pat)                                  = collectName pat
   collectName (IndexedPat (PatVar var) _)                     = [var]
   collectName _                                               = []

   makeBinding :: String -> IBindingExpr
   makeBinding var = (PDPatVar (stringToVar var), IHashExpr [])

desugarPattern' :: Pattern -> EvalM IPattern
desugarPattern' WildCard        = return IWildCard
desugarPattern' ContPat         = return IContPat
desugarPattern' SeqNilPat       = return ISeqNilPat
desugarPattern' LaterPatVar     = return ILaterPatVar
desugarPattern' (VarPat v)      = return (IVarPat v)
desugarPattern' (PatVar var)    = return (IPatVar var)
desugarPattern' (ValuePat expr) = IValuePat <$> desugar expr
desugarPattern' (PredPat expr)  = IPredPat <$> desugar expr
desugarPattern' (NotPat pat)       = INotPat <$> desugarPattern' pat
desugarPattern' (AndPat pat1 pat2) = IAndPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (OrPat pat1 pat2)  = IOrPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (ForallPat pat1 pat2) = IForallPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (InfixPat Op{ repr = "&" } pat1 pat2) =
  IAndPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (InfixPat Op{ repr = "|" } pat1 pat2) =
  IOrPat <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (InfixPat Op{ repr = f } pat1 pat2) =
  (\x y -> IInductivePat f [x, y]) <$> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (TuplePat pats) = ITuplePat <$> mapM desugarPattern' pats
desugarPattern' (InductiveOrPApplyPat name pats) = IInductiveOrPApplyPat name <$> mapM desugarPattern' pats
desugarPattern' (InductivePat name pats) = IInductivePat name <$> mapM desugarPattern' pats
desugarPattern' (IndexedPat pat exprs) = IIndexedPat <$> desugarPattern' pat <*> mapM desugar exprs
desugarPattern' (PApplyPat expr pats) = IPApplyPat <$> desugar expr <*> mapM desugarPattern' pats
desugarPattern' (DApplyPat pat pats) = IDApplyPat <$> desugarPattern' pat <*> mapM desugarPattern' pats
desugarPattern' (LoopPat name range pat1 pat2) = ILoopPat name <$> desugarLoopRange range <*> desugarPattern' pat1 <*> desugarPattern' pat2
desugarPattern' (LetPat binds pat) = ILetPat <$> desugarBindings binds <*> desugarPattern' pat
desugarPattern' (SeqConsPat pat1 pat2) = ISeqConsPat <$> desugarPattern' pat1 <*> desugarPattern' pat2

desugarLoopRange :: LoopRange -> EvalM ILoopRange
desugarLoopRange (LoopRange sExpr eExpr pat) =
  ILoopRange <$> desugar sExpr <*> desugar eExpr <*> desugarPattern' pat

desugarBindings :: [BindingExpr] -> EvalM [IBindingExpr]
desugarBindings = mapM desugarBinding
  where
    desugarBinding (Bind name expr) = do
      let name' = fmap stringToVar name
      expr' <- desugar expr
      case (name, expr') of
        (PDPatVar var, ILambdaExpr Nothing args body) ->
          return (name', ILambdaExpr (Just (Var var [])) args body)
        _ -> return (name', expr')
    desugarBinding (BindWithIndices vwi expr) = do
      (var, iexpr) <- desugarDefineWithIndices vwi expr
      return (PDPatVar var, iexpr)
    -- BindWithType: desugar like DefineWithType
    desugarBinding (BindWithType typedVarWI body) = do
      let name = typedVarName typedVarWI
          params = typedVarParams typedVarWI
          argPatterns = map typedParamToArgPattern params
          lambdaExpr = if null argPatterns
                         then body
                         else LambdaExpr argPatterns body
      body' <- desugar lambdaExpr
      let body'' = case body' of
            ILambdaExpr Nothing args b -> ILambdaExpr (Just (Var name [])) args b
            other -> other
      return (PDPatVar (Var name []), body'')

desugarMatchClauses :: [MatchClause] -> EvalM [IMatchClause]
desugarMatchClauses = mapM (\(pat, expr) -> (,) <$> desugarPattern pat <*> desugar expr)

desugarPatternDef :: PatternDef -> EvalM IPatternDef
desugarPatternDef (PatternDef pp matcher pds) =
  (pp,,) <$> desugar matcher <*> desugarPrimitiveDataMatchClauses pds

desugarPrimitiveDataMatchClauses :: [(PrimitiveDataPattern, Expr)] -> EvalM [(IPrimitiveDataPattern, IExpr)]
desugarPrimitiveDataMatchClauses = mapM (\(pd, expr) -> (fmap stringToVar pd,) <$> desugar expr)

desugarDefineWithIndices :: VarWithIndices -> Expr -> EvalM (Var, IExpr)
-- Case 1: No indices - simple desugaring without withSymbols/transpose
desugarDefineWithIndices (VarWithIndices name []) expr = do
  expr' <- desugar expr
  return (Var name [], expr')

-- Case 2: Non-empty indices - wrap with withSymbols and transpose
desugarDefineWithIndices (VarWithIndices name is) expr = do
  let (isSubs, indexNames) = unzip $ concatMap extractSubSupIndex is
  expr <- if any isExtendedIndice is
             then desugarExtendedIndices is isSubs indexNames expr
             else return expr
  body <- desugar expr
  let indexNamesCollection = ICollectionExpr (map IVarExpr indexNames)
  let is' = map (\b -> if b then Sub Nothing else Sup Nothing) isSubs
  -- ITransposeExpr takes (permutation, tensor) as arguments to match tTranspose
  return (Var name is', IWithSymbolsExpr indexNames (ITransposeExpr indexNamesCollection body))

varWithIndicesToVar :: VarWithIndices -> Var
varWithIndicesToVar (VarWithIndices name is) = Var name (concatMap transVarIndex is)

transVarIndex :: VarIndex -> [Index (Maybe Var)]
transVarIndex (VSubscript x)            = [Sub (Just (stringToVar x))]
transVarIndex (VSuperscript x)          = [Sup (Just (stringToVar x))]
transVarIndex (VMultiSubscript x s e)   = [MultiSub (Just (stringToVar x)) s (Just (stringToVar e))]
transVarIndex (VMultiSuperscript x s e) = [MultiSup (Just (stringToVar x)) s (Just (stringToVar e))]
transVarIndex (VGroupScripts xs)        = concatMap transVarIndex xs
transVarIndex (VSymmScripts xs)         = concatMap transVarIndex xs
transVarIndex (VAntiSymmScripts xs)     = concatMap transVarIndex xs

extractSubSupIndex :: VarIndex -> [(Bool, String)]
extractSubSupIndex (VSubscript x)        = [(True, x)]
extractSubSupIndex (VSuperscript x)      = [(False, x)]
extractSubSupIndex (VGroupScripts xs)    = concatMap extractSubSupIndex xs
extractSubSupIndex (VSymmScripts xs)     = concatMap extractSubSupIndex xs
extractSubSupIndex (VAntiSymmScripts xs) = concatMap extractSubSupIndex xs

desugarExtendedIndices :: [VarIndex] -> [Bool] -> [String] -> Expr -> EvalM Expr
desugarExtendedIndices indices isSubs indexNames tensorBody = do
  tensorName <- fresh
  tensorGenExpr <- f indices (VarExpr tensorName) [] []
  let indexFunctionExpr = LambdaExpr [Arg $ foldr APConsPat APEmptyPat (map (Arg . APPatVar) (map stringToVarWithIndices indexNames))] tensorGenExpr
  let genTensorExpr = GenerateTensorExpr indexFunctionExpr (makeApply "tensorShape" [VarExpr tensorName])
  let tensorIndices = zipWith (\isSub name -> if isSub then Subscript (VarExpr name) else Superscript (VarExpr name)) isSubs indexNames
  return $ LetExpr [Bind (PDPatVar tensorName) tensorBody] (IndexedExpr True genTensorExpr tensorIndices)
 where
  f :: [VarIndex] -> Expr -> [String] -> [BindingExpr] -> EvalM Expr
  f [] expr [] []       = return expr
  f [] expr [] bindings = return $ LetRecExpr bindings expr
  f [] expr signs bindings =
    return $ LetRecExpr bindings (makeApply "product" [CollectionExpr (map VarExpr signs ++ [expr])])
  f (index:indices) expr signs bindings = do
    (indices', signs', bindings') <- genBindings index
    let isSubs = subOrSupScripts index
    symbols <- mapM (const fresh) isSubs
    let is = zipWith (\x isSub -> (if isSub then Subscript else Superscript) (VarExpr x)) symbols isSubs
    f indices (IndexedExpr True expr is)
      (signs ++ signs') (bindings ++ bindings' ++ [Bind (foldr (PDConsPat . PDPatVar) PDEmptyPat symbols) indices'])

  subOrSupScripts :: VarIndex -> [Bool]
  subOrSupScripts VSubscript{}          = [True]
  subOrSupScripts VSuperscript{}        = [False]
  subOrSupScripts (VGroupScripts xs)    = concatMap subOrSupScripts xs
  subOrSupScripts (VSymmScripts xs)     = concatMap subOrSupScripts xs
  subOrSupScripts (VAntiSymmScripts xs) = concatMap subOrSupScripts xs

  genBindings :: VarIndex -> EvalM (Expr, [String], [BindingExpr])
  genBindings (VSubscript x)   = return (CollectionExpr [VarExpr x], [], [])
  genBindings (VSuperscript x) = return (CollectionExpr [VarExpr x], [], [])
  genBindings (VGroupScripts xs) = do
    (indices, signss, bindingss) <- unzip3 <$> mapM genBindings xs
    let newIndices =
          -- If indices are all CollectionExpr, we can calculate the concatenated result of them
          case allCollections indices of
            Just xs -> CollectionExpr xs
            Nothing -> makeApply "concat" [CollectionExpr indices]
    return (newIndices, concat signss, concat bindingss)
    where
      allCollections []                          = Just []
      allCollections (CollectionExpr xs : exprs) = (xs ++) <$> allCollections exprs
      allCollections _                           = Nothing
  genBindings (VSymmScripts xs) = do
    (indices, signss, bindingss) <- unzip3 <$> mapM genBindings xs
    let signs = concat signss
    let bindings = concat bindingss
    sortedCollectionName <- fresh
    let newBindings = bindings ++ [Bind (PDTuplePat [PDWildCard, PDPatVar sortedCollectionName]) (makeApply "sortWithSign" [CollectionExpr indices])]
    return (VarExpr sortedCollectionName, signs, newBindings)
  genBindings (VAntiSymmScripts xs) = do
    (indices, signss, bindingss) <- unzip3 <$> mapM genBindings xs
    let signs = concat signss
    let bindings = concat bindingss
    sortedCollectionName <- fresh
    signName <- fresh
    let newBindings = bindings ++ [Bind (PDTuplePat [PDPatVar signName, PDPatVar sortedCollectionName]) (makeApply "sortWithSign" [CollectionExpr indices])]
    return (VarExpr sortedCollectionName, signName : signs, newBindings)

--
-- Utils
--

extractIndexExpr :: IndexExpr a -> a
extractIndexExpr (Subscript x)    = x
extractIndexExpr (Superscript x)  = x
extractIndexExpr (SupSubscript x) = x
extractIndexExpr (Userscript x)   = x
extractIndexExpr _                = error "extractIndexExpr: Not supported"

isExtendedIndice :: VarIndex -> Bool
isExtendedIndice VSubscript{}       = False
isExtendedIndice VSuperscript{}     = False
isExtendedIndice (VGroupScripts xs) = isExtendedIndice (head xs)
isExtendedIndice _                  = True
