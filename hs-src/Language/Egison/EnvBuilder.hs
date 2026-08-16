{- |
Module      : Language.Egison.EnvBuilder
Licence     : MIT

This module implements Phase 2: Environment Building Phase.
It collects all declarations from TopExpr list before type inference and evaluation.

Environment Building Phase (Phase 2):
  1. Data constructor definitions collection (from InductiveDecl)
  2. Type class definitions collection (from ClassDeclExpr)
  3. Instance definitions collection (from InstanceDeclExpr)
  4. Type signature collection (from Define, DefineWithType)

This phase must be completed BEFORE type inference (Phase 5) begins,
ensuring all necessary information is available for type checking.
-}

module Language.Egison.EnvBuilder
  ( buildEnvironments
  , EnvBuildResult(..)
  ) where

import           Control.Monad              (foldM, when)
import           Control.Monad.Except       (throwError)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Char                  (isUpper)
import qualified Data.HashMap.Strict        as HashMap
import           System.IO                  (hPutStrLn, stderr)

import           Language.Egison.AST
import           Language.Egison.Data       (EvalM, EgisonError(..))
import           Language.Egison.EvalState  (MonadEval(getConstructorEnv, getCasTypeAliasEnv, getCasSubtypeEdges), ConstructorInfo(..), ConstructorEnv, PatternConstructorEnv)
import           Language.Egison.Type.Pretty (prettyType)
import qualified Language.Egison.Type.Subtype as Subtype
import           Language.Egison.IExpr      (Var(..), stringToVar)
import           Language.Egison.Desugar    (transVarIndex)
import           Language.Egison.Type.Env   (TypeEnv, ClassEnv, PatternTypeEnv, emptyEnv, emptyClassEnv, emptyPatternEnv,
                                             extendEnv, extendPatternEnv, addClass, addInstance, lookupClass)
import qualified Language.Egison.Type.Types as Types
import           Language.Egison.Type.Types (Type(..), TyVar(..), Constraint(..), TypeScheme(..),
                                             freeCapVars, freeTyVars,
                                             sanitizeMethodName, typeExprToType,
                                             capitalizeFirst, lowerFirst)
import           Language.Egison.Type.Subst (emptySubst, singletonSubst, composeSubst, applySubst)
import qualified Data.Set as Set

-- | Result of environment building phase
data EnvBuildResult = EnvBuildResult
  { ebrTypeEnv        :: TypeEnv         -- ^ Type signatures for definitions
  , ebrClassEnv       :: ClassEnv        -- ^ Type class and instance information
  , ebrConstructorEnv :: ConstructorEnv  -- ^ Data constructor information
  , ebrPatternConstructorEnv :: PatternConstructorEnv  -- ^ Pattern constructor information
  , ebrPatternTypeEnv :: PatternTypeEnv  -- ^ Pattern function information
  -- Phase 7.4/7.5: collected `declare rule` declarations. Stored as the raw
  -- (name, level, lhs, rhs) tuple. Rule application is not yet wired into
  -- normalization; this field exists so the data round-trips through env
  -- building and is available for inspection / future Phase 7.5 code.
  , ebrReductionRules :: [(Maybe String, RuleLevel, Pattern, Expr)]
  -- Phase 6.3: collected `declare derivative` declarations. Maps function
  -- name -> derivative expression. Wiring into `Differentiable Factor` is
  -- still pending; for now, just the registration list.
  , ebrDerivativeRules :: [(String, Expr)]
  -- Names of functions declared via `declare mathfunc`. Used by the
  -- DeclareApply handler to enforce that `declare apply foo ...` only
  -- appears after a corresponding `declare mathfunc foo`.
  , ebrMathFuncNames :: Set.Set String
  -- Phase alpha (extensible CAS tower): `declare cas-type` aliases declared
  -- in THIS batch, name -> fully expanded Type. Merged into the persistent
  -- EvalState alias env by the caller (Eval.buildAndMergeEnvironments).
  , ebrCasTypeAliases :: HashMap.HashMap String Type
  -- Phase beta: `declare cas-subtype` edges declared in THIS batch
  -- (alias-expanded, D1-checked, redundant ones included). Appended to the
  -- persistent EvalState edge list by the caller.
  , ebrCasSubtypeEdges :: [(Type, Type)]
  } deriving (Show)

--------------------------------------------------------------------------------
-- Phase 2: Environment Building Phase
--------------------------------------------------------------------------------

-- | Build all environments from a list of top-level expressions.
-- This function implements Phase 2 of the processing flow.
-- It must be called AFTER expandLoads (Phase 1) and BEFORE type inference (Phase 5).
buildEnvironments :: [TopExpr] -> EvalM EnvBuildResult
buildEnvironments exprs = do
  -- Names of value-level inductive types: this batch's `inductive` declarations PLUS the
  -- types already registered by earlier load units (the accumulated constructor env).
  -- Cross-batch coverage matters because a matcher defined in file B over a type declared
  -- in file A must still keep that type concrete in its signature (see concretizeDeclaredTypes);
  -- B's TopExpr list alone would not mention `inductive A`.
  priorCtorEnv <- getConstructorEnv
  priorAliases <- getCasTypeAliasEnv
  let declaredTypes = Set.fromList ([ n | InductiveDecl n _ _ <- exprs ]
                                    ++ [ ctorTypeName ci | ci <- HashMap.elems priorCtorEnv ])
  capabilityFormerArities <-
    buildCapabilityFormerArities exprs priorCtorEnv

  -- Phase alpha (extensible CAS tower): collect `declare cas-type` aliases
  -- first (prepass, so declaration order does not matter for users of the
  -- alias), then resolve alias-in-alias references to a fixpoint. Bodies are
  -- stored fully expanded so a single substitution pass suffices at use sites.
  newAliasesRaw <- foldM (collectCasTypeAlias declaredTypes priorAliases)
                         HashMap.empty
                         [ (n, te) | DeclareCasType n te <- exprs ]
  newAliases <- resolveCasTypeAliases priorAliases newAliasesRaw
  -- Restriction on open atom sets, checked on the fully expanded alias
  -- bodies (alias-in-alias expansion can only be judged after resolution):
  -- a nested Poly tower may contain at most one [..]
  -- (Types.hasAmbiguousOpenTower; the runtime reshape's atom routing would
  -- otherwise be ambiguous).
  mapM_ (\(n, t) ->
          when (Types.hasAmbiguousOpenTower t) $ throwError $ Default $
            "declare cas-type " ++ n ++ ": at most one open atom set [..] " ++
            "may appear in a nested Poly tower: " ++ prettyType t)
        (HashMap.toList newAliases)
  let aliasEnv = HashMap.union newAliases priorAliases

  -- Elaborate the capability sort against the frozen type-former signature.
  -- This deliberately happens before ordinary declaration processing:
  -- malformed capability annotations must not be turned into well-formed
  -- TypeFormer values merely by counting their surface arguments.
  mapM_ (validateTopExprCapabilities capabilityFormerArities aliasEnv) exprs

  -- Phase beta: collect `declare cas-subtype` edges (alias-expanded) and run
  -- the D1 join-semilattice check per edge, in declaration order.
  priorEdges <- getCasSubtypeEdges
  newEdges <- foldM (collectCasSubtypeEdge aliasEnv priorEdges) []
                    [ (l, r) | DeclareCasSubtype l r <- exprs ]

  -- Start with empty environments
  let initialResult = EnvBuildResult
        { ebrTypeEnv = emptyEnv
        , ebrClassEnv = emptyClassEnv
        , ebrConstructorEnv = HashMap.empty
        , ebrPatternConstructorEnv = emptyPatternEnv
        , ebrPatternTypeEnv = emptyPatternEnv
        , ebrReductionRules = []
        , ebrDerivativeRules = []
        , ebrMathFuncNames = Set.empty
        , ebrCasTypeAliases = newAliases
        , ebrCasSubtypeEdges = newEdges
        }

  -- Process each top-level expression to collect declarations
  foldM (processTopExpr declaredTypes aliasEnv) initialResult exprs

--------------------------------------------------------------------------------
-- Capability name/kind elaboration
--------------------------------------------------------------------------------

-- | Arity environment for canonical capability formers.  It is separate from
-- the ordinary type environment because capability variables and ordinary
-- type variables are different sorts, and because transparent aliases are
-- intentionally absent from this signature.
type CapabilityFormerArities =
  HashMap.HashMap Types.TypeFormerId Int

-- | Construct the frozen former signature used by capability annotations.
--
-- Builtin entries mirror 'Types.typeFormerOf'.  User inductive declarations
-- are added from both the current batch and the accumulated constructor
-- environment.  The latter retains declarations with at least one
-- constructor; empty inductive declarations need a future dedicated
-- type-former environment if they are to survive across load-unit boundaries.
buildCapabilityFormerArities
  :: [TopExpr]
  -> ConstructorEnv
  -> EvalM CapabilityFormerArities
buildCapabilityFormerArities exprs priorCtorEnv = do
  mapM_ rejectBuiltinCollision userEntries
  foldM register HashMap.empty
    (builtinCapabilityFormerArities ++ userEntries)
  where
    userEntries =
      [ (name, length params)
      | InductiveDecl name params _ <- exprs
      ]
      ++ [ (ctorTypeName info, length (ctorTypeParams info))
         | info <- HashMap.elems priorCtorEnv
         ]

    builtinFormerIds =
      Set.fromList
        [ Types.typeFormerId (Types.mkTypeFormer name arity)
        | (name, arity) <- builtinCapabilityFormerArities
        ]

    rejectBuiltinCollision (surfaceName, arity) =
      let former = Types.mkTypeFormer surfaceName arity
          formerId = Types.typeFormerId former
      in when (Set.member formerId builtinFormerIds) $
           capabilityKindError "type-former signature" $
             "inductive type `" ++ surfaceName
             ++ "` collides with builtin canonical capability former "
             ++ describeCapabilityFormer formerId

    register arities (surfaceName, arity) =
      let former = Types.mkTypeFormer surfaceName arity
          formerId = Types.typeFormerId former
      in case HashMap.lookup formerId arities of
           Nothing ->
             return (HashMap.insert formerId arity arities)
           Just previousArity
             | previousArity == arity ->
                 return arities
             | otherwise ->
                 capabilityKindError "type-former signature" $
                   "canonical former " ++ describeCapabilityFormer formerId
                   ++ " is declared at both arity "
                   ++ show previousArity ++ " and arity " ++ show arity

-- | Closed builtin portion of the capability former signature.
builtinCapabilityFormerArities :: [(String, Int)]
builtinCapabilityFormerArities =
  [ ("Integer", 0)
  , ("MathValue", 0)
  , ("PolyExpr", 0)
  , ("TermExpr", 0)
  , ("SymbolExpr", 0)
  , ("IndexExpr", 0)
  , ("Float", 0)
  , ("Bool", 0)
  , ("Char", 0)
  , ("String", 0)
  , ("Collection", 1)
  , ("Tensor", 1)
  , ("Hash", 2)
  , ("Factor", 0)
  , ("Term", 1)
  , ("Frac", 1)
  , ("Poly", 1)
  ]

-- | Validate every declaration surface that can contain a TypeExpr.
validateTopExprCapabilities
  :: CapabilityFormerArities
  -> HashMap.HashMap String Type
  -> TopExpr
  -> EvalM ()
validateTopExprCapabilities arities aliases topExpr =
  case topExpr of
    Define (VarWithIndices name _) body ->
      validateExprCapabilities
        arities aliases ("body of definition `" ++ name ++ "`") body

    DefineWithType typedVar body -> do
      let name = typedVarName typedVar
      validateTypedVarCapabilities
        arities aliases ("type signature of `" ++ name ++ "`") typedVar
      validateExprCapabilities
        arities aliases ("body of definition `" ++ name ++ "`") body

    Test expression ->
      validateExprCapabilities arities aliases "test expression" expression

    Execute expression ->
      validateExprCapabilities arities aliases "execute expression" expression

    InductiveDecl typeName _ constructors ->
      mapM_ (validateInductiveConstructorCapabilities typeName) constructors

    ClassDeclExpr (ClassDecl className _ superclasses methods) -> do
      let context = "class `" ++ className ++ "`"
      mapM_ (validateConstraintCapabilities arities aliases context)
            superclasses
      mapM_ (validateClassMethodCapabilities className) methods

    InstanceDeclExpr
        (InstanceDecl constraints className instanceTypes methods) -> do
      let context = "instance `" ++ className ++ "`"
      mapM_ (validateConstraintCapabilities arities aliases context)
            constraints
      mapM_ (validateTypeExprCapabilities arities aliases context)
            instanceTypes
      mapM_ (validateInstanceMethodCapabilities className) methods

    PatternInductiveDecl typeName _ constructors ->
      mapM_ (validatePatternConstructorCapabilities typeName) constructors

    PatternFunctionDecl name _ params retType body -> do
      let context = "pattern function `" ++ name ++ "`"
      mapM_ (validateTypeExprCapabilities arities aliases context . snd) params
      validateTypeExprCapabilities arities aliases context retType
      validatePatternCapabilities arities aliases context body

    DeclareSymbol _ maybeType ->
      mapM_ (validateTypeExprCapabilities
               arities aliases "declare symbol annotation")
            maybeType

    DeclareMathFunc name maybeType ->
      mapM_ (validateTypeExprCapabilities
               arities aliases ("declare mathfunc `" ++ name ++ "`"))
            maybeType

    DeclareCasType name body ->
      validateTypeExprCapabilities
        arities aliases ("declare cas-type `" ++ name ++ "`") body

    DeclareCasSubtype left right -> do
      validateTypeExprCapabilities
        arities aliases "left side of declare cas-subtype" left
      validateTypeExprCapabilities
        arities aliases "right side of declare cas-subtype" right

    DeclareCasQuotient name base reducer -> do
      validateTypeExprCapabilities
        arities aliases ("declare cas-quotient `" ++ name ++ "`") base
      validateExprCapabilities
        arities aliases ("reducer of declare cas-quotient `" ++ name ++ "`")
        reducer

    DeclareIdeal generators ->
      mapM_ (validateExprCapabilities arities aliases "declare ideal")
            generators

    DeclareRule maybeName _ pattern rhs -> do
      let context =
            case maybeName of
              Nothing ->
                "unnamed declare rule"
              Just name ->
                "declare rule `" ++ name ++ "`"
      validatePatternCapabilities arities aliases context pattern
      validateExprCapabilities arities aliases context rhs

    DeclareDerivative name derivative ->
      validateExprCapabilities
        arities aliases ("declare derivative `" ++ name ++ "`") derivative

    DeclareApply name _ body ->
      validateExprCapabilities
        arities aliases ("declare apply `" ++ name ++ "`") body

    _ ->
      return ()
  where
    validateInductiveConstructorCapabilities typeName
        (InductiveConstructor ctorName fields) =
      mapM_ (validateTypeExprCapabilities arities aliases context) fields
      where
        context =
          "constructor `" ++ ctorName ++ "` of inductive `" ++ typeName ++ "`"

    validatePatternConstructorCapabilities typeName
        (PatternConstructor ctorName fields) =
      mapM_ (validateTypeExprCapabilities arities aliases context) fields
      where
        context =
          "pattern constructor `" ++ ctorName
          ++ "` of inductive pattern `" ++ typeName ++ "`"

    validateClassMethodCapabilities className
        (ClassMethod methodName params retType maybeDefault) = do
      let context =
            "method `" ++ methodName ++ "` of class `" ++ className ++ "`"
      mapM_ (validateTypedParamCapabilities arities aliases context) params
      validateTypeExprCapabilities arities aliases context retType
      mapM_ (validateExprCapabilities arities aliases context) maybeDefault

    validateInstanceMethodCapabilities className
        (InstanceMethod methodName _ body) =
      validateExprCapabilities arities aliases context body
      where
        context =
          "method `" ++ methodName ++ "` of instance `" ++ className ++ "`"

validateTypedVarCapabilities
  :: CapabilityFormerArities
  -> HashMap.HashMap String Type
  -> String
  -> TypedVarWithIndices
  -> EvalM ()
validateTypedVarCapabilities arities aliases context typedVar = do
  mapM_ (validateConstraintCapabilities arities aliases context)
        (typedVarConstraints typedVar)
  mapM_ (validateTypedParamCapabilities arities aliases context)
        (typedVarParams typedVar)
  validateTypeExprCapabilities
    arities aliases context (typedVarRetType typedVar)

validateConstraintCapabilities
  :: CapabilityFormerArities
  -> HashMap.HashMap String Type
  -> String
  -> ConstraintExpr
  -> EvalM ()
validateConstraintCapabilities arities aliases context
    (ConstraintExpr _ typeExprs) =
  mapM_ (validateTypeExprCapabilities arities aliases context) typeExprs

validateTypedParamCapabilities
  :: CapabilityFormerArities
  -> HashMap.HashMap String Type
  -> String
  -> TypedParam
  -> EvalM ()
validateTypedParamCapabilities arities aliases context typedParam =
  case typedParam of
    TPVar _ typeExpr ->
      validate typeExpr
    TPInvertedVar _ typeExpr ->
      validate typeExpr
    TPTuple params ->
      mapM_ (validateTypedParamCapabilities arities aliases context) params
    TPWildcard typeExpr ->
      validate typeExpr
    TPUntypedVar _ ->
      return ()
    TPUntypedWildcard ->
      return ()
  where
    validate = validateTypeExprCapabilities arities aliases context

-- | Traverse expressions so local signatures and annotations cannot bypass
-- capability name/kind elaboration.
validateExprCapabilities
  :: CapabilityFormerArities
  -> HashMap.HashMap String Type
  -> String
  -> Expr
  -> EvalM ()
validateExprCapabilities arities aliases context expression =
  case expression of
    ConstantExpr _ ->
      return ()
    VarExpr _ ->
      return ()
    FreshVarExpr ->
      return ()
    IndexedExpr _ base indices -> do
      validate base
      mapM_ (mapM_ validate) indices
    SubrefsExpr _ base index -> do
      validate base
      validate index
    SuprefsExpr _ base index -> do
      validate base
      validate index
    UserrefsExpr _ base index -> do
      validate base
      validate index
    TupleExpr elements ->
      mapM_ validate elements
    CollectionExpr elements ->
      mapM_ validate elements
    ConsExpr headExpr tailExpr -> do
      validate headExpr
      validate tailExpr
    JoinExpr left right -> do
      validate left
      validate right
    HashExpr entries ->
      mapM_ (\(key, value) -> validate key >> validate value) entries
    VectorExpr elements ->
      mapM_ validate elements
    LambdaExpr _ body ->
      validate body
    LambdaExpr' _ body ->
      validate body
    TypedLambdaExpr params retType body -> do
      mapM_ (validateTypeExprCapabilities arities aliases context . snd) params
      validateTypeExprCapabilities arities aliases context retType
      validate body
    MemoizedLambdaExpr _ body ->
      validate body
    TypedMemoizedLambdaExpr params retType body -> do
      mapM_ (validateTypedParamCapabilities arities aliases context) params
      validateTypeExprCapabilities arities aliases context retType
      validate body
    CambdaExpr _ body ->
      validate body
    PatternFunctionExpr _ body ->
      validatePatternCapabilities arities aliases context body
    IfExpr condition consequent alternative -> do
      validate condition
      validate consequent
      validate alternative
    LetExpr bindings body -> do
      mapM_ (validateBindingCapabilities arities aliases context) bindings
      validate body
    LetRecExpr bindings body -> do
      mapM_ (validateBindingCapabilities arities aliases context) bindings
      validate body
    WithSymbolsExpr _ body ->
      validate body
    MatchExpr _ target matcher clauses -> do
      validate target
      validate matcher
      mapM_ (validateMatchClauseCapabilities arities aliases context) clauses
    MatchAllExpr _ target matcher clauses -> do
      validate target
      validate matcher
      mapM_ (validateMatchClauseCapabilities arities aliases context) clauses
    MatchLambdaExpr matcher clauses -> do
      validate matcher
      mapM_ (validateMatchClauseCapabilities arities aliases context) clauses
    MatchAllLambdaExpr matcher clauses -> do
      validate matcher
      mapM_ (validateMatchClauseCapabilities arities aliases context) clauses
    MatcherExpr patternDefs ->
      mapM_ (validatePatternDefCapabilities arities aliases context) patternDefs
    AlgebraicDataMatcherExpr constructors ->
      mapM_ (mapM_ validate . snd) constructors
    QuoteExpr quoted ->
      validate quoted
    QuoteSymbolExpr quoted ->
      validate quoted
    WedgeApplyExpr function arguments -> do
      validate function
      mapM_ validate arguments
    DoExpr bindings body -> do
      mapM_ (validateBindingCapabilities arities aliases context) bindings
      validate body
    PrefixExpr _ operand ->
      validate operand
    InfixExpr _ left right -> do
      validate left
      validate right
    SectionExpr _ left right -> do
      mapM_ validate left
      mapM_ validate right
    SeqExpr left right -> do
      validate left
      validate right
    ApplyExpr function arguments -> do
      validate function
      mapM_ validate arguments
    AnonParamFuncExpr _ body ->
      validate body
    AnonTupleParamFuncExpr _ body ->
      validate body
    AnonListParamFuncExpr _ body ->
      validate body
    AnonParamExpr _ ->
      return ()
    GenerateTensorExpr size body -> do
      validate size
      validate body
    TensorExpr shape body -> do
      validate shape
      validate body
    TensorContractExpr tensor ->
      validate tensor
    TensorMapExpr function tensor -> do
      validate function
      validate tensor
    TensorMap2Expr function tensor1 tensor2 -> do
      validate function
      validate tensor1
      validate tensor2
    TransposeExpr permutation tensor -> do
      validate permutation
      validate tensor
    FlipIndicesExpr tensor ->
      validate tensor
    FunctionExpr _ ->
      return ()
    TypeAnnotation body typeExpr -> do
      validate body
      validateTypeExprCapabilities arities aliases context typeExpr
    SimplifyUsingExpr body _ ->
      validate body
  where
    validate = validateExprCapabilities arities aliases context

validateBindingCapabilities
  :: CapabilityFormerArities
  -> HashMap.HashMap String Type
  -> String
  -> BindingExpr
  -> EvalM ()
validateBindingCapabilities arities aliases context binding =
  case binding of
    Bind _ expression ->
      validate expression
    BindWithIndices _ expression ->
      validate expression
    BindWithType typedVar expression -> do
      validateTypedVarCapabilities arities aliases context typedVar
      validate expression
  where
    validate = validateExprCapabilities arities aliases context

validateMatchClauseCapabilities
  :: CapabilityFormerArities
  -> HashMap.HashMap String Type
  -> String
  -> MatchClause
  -> EvalM ()
validateMatchClauseCapabilities arities aliases context (pattern, body) = do
  validatePatternCapabilities arities aliases context pattern
  validateExprCapabilities arities aliases context body

validatePatternDefCapabilities
  :: CapabilityFormerArities
  -> HashMap.HashMap String Type
  -> String
  -> PatternDef
  -> EvalM ()
validatePatternDefCapabilities arities aliases context patternDef = do
  validateExprCapabilities arities aliases context (patDefMatcher patternDef)
  mapM_ (validateExprCapabilities arities aliases context . snd)
        (patDefClauses patternDef)

validatePatternCapabilities
  :: CapabilityFormerArities
  -> HashMap.HashMap String Type
  -> String
  -> Pattern
  -> EvalM ()
validatePatternCapabilities arities aliases context pattern =
  case pattern of
    WildCard ->
      return ()
    PatVar _ ->
      return ()
    ValuePat expression ->
      validateExprCapabilities arities aliases context expression
    PredPat expression ->
      validateExprCapabilities arities aliases context expression
    IndexedPat body indices -> do
      validate body
      mapM_ (validateExprCapabilities arities aliases context) indices
    LetPat bindings body -> do
      mapM_ (validateBindingCapabilities arities aliases context) bindings
      validate body
    InfixPat _ left right -> do
      validate left
      validate right
    NotPat body ->
      validate body
    AndPat left right -> do
      validate left
      validate right
    OrPat left right -> do
      validate left
      validate right
    ForallPat left right -> do
      validate left
      validate right
    TuplePat elements ->
      mapM_ validate elements
    InductivePat _ arguments ->
      mapM_ validate arguments
    LoopPat _ (LoopRange start end endPattern) repeatPattern body -> do
      validateExprCapabilities arities aliases context start
      validateExprCapabilities arities aliases context end
      validate endPattern
      validate repeatPattern
      validate body
    ContPat ->
      return ()
    PApplyPat function arguments -> do
      validateExprCapabilities arities aliases context function
      mapM_ validate arguments
    VarPat _ ->
      return ()
    InductiveOrPApplyPat _ arguments ->
      mapM_ validate arguments
    SeqNilPat ->
      return ()
    SeqConsPat headPattern tailPattern -> do
      validate headPattern
      validate tailPattern
    LaterPatVar ->
      return ()
    DApplyPat functionPattern arguments -> do
      validate functionPattern
      mapM_ validate arguments
  where
    validate = validatePatternCapabilities arities aliases context

-- | Walk an ordinary TypeExpr and elaborate only its embedded capability
-- annotations.  Ordinary type name resolution remains the responsibility of
-- the existing type elaboration path.
validateTypeExprCapabilities
  :: CapabilityFormerArities
  -> HashMap.HashMap String Type
  -> String
  -> TypeExpr
  -> EvalM ()
validateTypeExprCapabilities arities aliases context typeExpr =
  case typeExpr of
    TEList element ->
      validate element
    TETuple elements ->
      mapM_ validate elements
    TEFun argument result -> do
      validate argument
      validate result
    TEMatcher capability target -> do
      validateCapabilityExpr arities aliases context capability
      validate target
    TEMatcherSlot capability target -> do
      validateCapabilityExpr arities aliases context capability
      validate target
    TEPattern target ->
      validate target
    TEIO target ->
      validate target
    TETensor element ->
      validate element
    TEVector element ->
      validate element
    TEMatrix element ->
      validate element
    TEDiffForm element ->
      validate element
    TEApp headType arguments -> do
      validate headType
      mapM_ validate arguments
    TEConstrained constraints body -> do
      mapM_ (validateConstraintCapabilities arities aliases context)
            constraints
      validate body
    TETerm coefficient _ ->
      validate coefficient
    TEFrac coefficient ->
      validate coefficient
    TEPoly coefficient _ ->
      validate coefficient
    _ ->
      return ()
  where
    validate = validateTypeExprCapabilities arities aliases context

-- | Check a capability expression against the frozen former signature.
--
-- The transparent-alias check intentionally precedes canonical lookup so an
-- alias never gains capability meaning from the shape of its expansion.
validateCapabilityExpr
  :: CapabilityFormerArities
  -> HashMap.HashMap String Type
  -> String
  -> CapabilityExpr
  -> EvalM ()
validateCapabilityExpr arities aliases context capabilityExpr =
  case capabilityExpr of
    CEAny ->
      return ()
    CEVar _ ->
      return ()
    CEList element ->
      validateCapabilityExpr arities aliases context element
    CETuple elements ->
      mapM_ (validateCapabilityExpr arities aliases context) elements
    CECon surfaceName arguments -> do
      when (isTransparentTypeAlias aliases surfaceName) $
        capabilityKindError context $
          "capability head `" ++ surfaceName
          ++ "` is a transparent type alias; use its canonical declared "
          ++ "type former instead"
      let former = Types.mkTypeFormer surfaceName (length arguments)
          formerId = Types.typeFormerId former
      case HashMap.lookup formerId arities of
        Nothing ->
          capabilityKindError context $
            "unknown capability former `" ++ surfaceName ++ "`"
        Just expectedArity ->
          when (length arguments /= expectedArity) $
            capabilityKindError context $
              "capability former `" ++ surfaceName ++ "` expects "
              ++ show expectedArity ++ " argument"
              ++ plural expectedArity ++ ", but was given "
              ++ show (length arguments)
      mapM_ (validateCapabilityExpr arities aliases context) arguments

isTransparentTypeAlias :: HashMap.HashMap String Type -> String -> Bool
isTransparentTypeAlias aliases name =
  case HashMap.lookup name aliases of
    Nothing ->
      False
    Just (TInductive nominalName [])
      | nominalName == name ->
          False
    Just _ ->
      True

capabilityKindError :: String -> String -> EvalM a
capabilityKindError context detail =
  throwError $ Default $
    "Type error:\nCapability kind error in " ++ context ++ ": " ++ detail

describeCapabilityFormer :: Types.TypeFormerId -> String
describeCapabilityFormer (Types.TypeFormerId name) = "`" ++ name ++ "`"

plural :: Int -> String
plural 1 = ""
plural _ = "s"

-- | Validate and register a single `declare cas-type` alias.
-- Rules (design/type-cas-tower-implementation.md section 2):
--   * the alias name must be capitalized
--   * it must not clash with builtin type names, declared inductive types,
--     or an existing alias (no redeclaration)
--   * the body may reference previously declared aliases only (a leftover
--     alias name after expansion means a self/forward reference)
collectCasTypeAlias :: Set.Set String -> HashMap.HashMap String Type
                    -> HashMap.HashMap String Type -> (String, TypeExpr)
                    -> EvalM (HashMap.HashMap String Type)
collectCasTypeAlias declaredTypes priorAliases acc (name, te) = do
  when (not (startsUpper name)) $ throwError $ Default $
    "declare cas-type: alias name must be capitalized: " ++ name
  when (Set.member name Types.reservedCasTypeNames) $ throwError $ Default $
    "declare cas-type: alias name clashes with a builtin type: " ++ name
  when (Set.member name declaredTypes) $ throwError $ Default $
    "declare cas-type: alias name clashes with an inductive type: " ++ name
  -- A nominal entry `name -> TInductive name []` in the alias env is a
  -- cas-quotient type (registered by Eval.expandCasQuotientDecls).
  case HashMap.lookup name priorAliases of
    Just (TInductive n []) | n == name ->
      throwError $ Default $
        "declare cas-type: name is already a cas-quotient type: " ++ name
    _ -> return ()
  when (HashMap.member name priorAliases || HashMap.member name acc) $
    throwError $ Default $
      "declare cas-type: alias is already declared: " ++ name
  return (HashMap.insert name (typeExprToType te) acc)
  where
    startsUpper (c:_) = isUpper c
    startsUpper _     = False

-- | Validate and register a `declare cas-subtype A ⊂ B` edge (Phase beta;
-- D1 declare-time semilattice check, design/type-cas-tower.md §8 D1).
-- Redundant edges are stored anyway — their endpoints then participate in
-- the node set of later checks — with a warning.
collectCasSubtypeEdge :: HashMap.HashMap String Type -> [(Type, Type)]
                      -> [(Type, Type)] -> (TypeExpr, TypeExpr)
                      -> EvalM [(Type, Type)]
collectCasSubtypeEdge aliasEnv priorEdges acc (lhsTE, rhsTE) = do
  let lhs = Types.expandTypeAliases aliasEnv (typeExprToType lhsTE)
      rhs = Types.expandTypeAliases aliasEnv (typeExprToType rhsTE)
      edges = priorEdges ++ acc
      pp = prettyType
  when (not (Subtype.isCasType lhs) || not (Subtype.isCasType rhs)) $
    throwError $ Default $
      "declare cas-subtype: both sides must be CAS types: " ++
      pp lhs ++ " <: " ++ pp rhs
  mapM_ (\side ->
          when (Types.hasAmbiguousOpenTower side) $ throwError $ Default $
            "declare cas-subtype: at most one open atom set [..] may " ++
            "appear in a nested Poly tower: " ++ pp side)
        [lhs, rhs]
  case Subtype.checkEdgeAddition edges (lhs, rhs) of
    Subtype.EdgeCycle -> throwError $ Default $
      "declare cas-subtype " ++ pp lhs ++ " <: " ++ pp rhs ++
      ": the reverse relation already holds; adding this edge would " ++
      "collapse the two types into one order point (cycle)"
    Subtype.EdgeAmbiguous witnesses -> throwError $ Default $
      "declare cas-subtype " ++ pp lhs ++ " <: " ++ pp rhs ++
      ": join would become ambiguous (D1 semilattice check).\n" ++
      concatMap (\(x, y, j) ->
        "  pair (" ++ pp x ++ ", " ++ pp y ++ ") would get minimal upper bounds {" ++
        pp j ++ ", " ++ pp rhs ++ "}\n" ++
        "  hint: declare the completing edge first:\n" ++
        "    declare cas-subtype " ++ pp j ++ " <: " ++ pp rhs ++ "\n")
        witnesses
    Subtype.EdgeRedundant -> do
      liftIO $ hPutStrLn stderr $
        "Warning: declare cas-subtype " ++ pp lhs ++ " <: " ++ pp rhs ++
        " is already derivable (redundant edge)"
      return (acc ++ [(lhs, rhs)])
    Subtype.EdgeRefines witnesses -> do
      liftIO $ hPutStrLn stderr $
        "Warning: declare cas-subtype " ++ pp lhs ++ " <: " ++ pp rhs ++
        " refines existing joins (values unchanged, static types get more precise):" ++
        concatMap (\(x, y, j) ->
          "\n  join(" ++ pp x ++ ", " ++ pp y ++ "): " ++ pp j ++ " -> " ++ pp rhs)
          witnesses
      return (acc ++ [(lhs, rhs)])
    Subtype.EdgeOk -> return (acc ++ [(lhs, rhs)])

-- | Resolve alias-in-alias references to a fixpoint, so aliases may refer to
-- each other regardless of declaration order (prepass semantics, matching
-- the other `declare` families). Each round substitutes one nesting level;
-- a definition that keeps growing past |aliases| + 1 rounds is cyclic.
resolveCasTypeAliases :: HashMap.HashMap String Type -> HashMap.HashMap String Type
                      -> EvalM (HashMap.HashMap String Type)
resolveCasTypeAliases priorAliases = go (0 :: Int)
  where
    go rounds m
      | m' == m = return m
      | rounds > HashMap.size m + 1 =
          throwError $ Default $
            "declare cas-type: cyclic alias definition among: " ++
            unwords (HashMap.keys m)
      | otherwise = go (rounds + 1) m'
      where
        full = HashMap.union m priorAliases
        m'   = HashMap.map (Types.expandTypeAliases full) m

-- | Rewrite bare declared-inductive-type names that 'typeExprToType' produced
-- as type variables (e.g. the target index in @Matcher p Nat@) into the
-- concrete @TInductive@.  This applies both to explicit definitions and to
-- class method declarations.  Without it, an explicit signature
-- @nat : Matcher p Nat@ is generalized to @forall a. Matcher p a@, so a
-- recursive matcher's self-reference instantiates to a fresh target type and
-- fails the MatcherSlot capability/target check.
-- Only names declared via @inductive@ in this batch are rewritten; undeclared
-- capitalized names (e.g. a stale @MathExpr@) stay type variables.
concretizeDeclaredTypes :: Set.Set String -> Type -> Type
concretizeDeclaredTypes decls = applySubst subst
  where subst = foldr (\n s -> composeSubst (singletonSubst (TyVar n) (TInductive n [])) s)
                      emptySubst (Set.toList decls)

-- | Process a single top-level expression to collect environment information.
-- `aliasEnv` carries `declare cas-type` aliases (prior batches + this batch);
-- every TypeExpr conversion goes through `t2t` so alias names are expanded
-- before types are stored anywhere (Phase alpha of the extensible tower).
processTopExpr :: Set.Set String -> HashMap.HashMap String Type -> EnvBuildResult -> TopExpr -> EvalM EnvBuildResult
processTopExpr declaredTypes aliasEnv result topExpr = case topExpr of

  -- 1. Data Constructor Definitions (from InductiveDecl)
  InductiveDecl typeName typeParams constructors -> do
    let typeParamVars = map (TVar . TyVar) typeParams
        adtType = TInductive typeName typeParamVars
        typeEnv = ebrTypeEnv result
        ctorEnv = ebrConstructorEnv result

    -- Register each constructor
    (typeEnv', ctorEnv') <- foldM (registerConstructor aliasEnv typeName typeParams adtType)
                                   (typeEnv, ctorEnv)
                                   constructors
    
    return result { ebrTypeEnv = typeEnv', ebrConstructorEnv = ctorEnv' }
  
  -- 2. Type Class Definitions (from ClassDeclExpr).
  -- Supports any number of type parameters (single-param `class Eq a` and
  -- multi-param `class Embed a b` go through the same path). Methods are still
  -- registered against the *first* parameter for backward compatibility with
  -- existing single-param infrastructure (Phase 5.5 multi-param-aware
  -- elaboration is a separate task).
  ClassDeclExpr (ClassDecl className typeParams superClasses methods) | not (null typeParams) -> do
    let classEnv = ebrClassEnv result
        typeEnv = ebrTypeEnv result
        tyVars = map TyVar typeParams

        -- Extract superclass names from ConstraintExprs
        superNames = map extractConstraintName superClasses

        -- Build method list with types
        methodsWithTypes =
          map (extractMethodWithType declaredTypes aliasEnv) methods

        -- Create ClassInfo
        -- Note: Use qualified name to avoid ambiguity with ClassDecl.classMethods
        classInfo = Types.ClassInfo
          { Types.classSupers = superNames
          , Types.classParams = tyVars
          , Types.classMethods = methodsWithTypes
          }

        -- Register class
        classEnv' = addClass className classInfo classEnv

        -- Register each class method to type environment
        typeEnv' =
          foldl
            (registerClassMethod declaredTypes aliasEnv tyVars className)
            typeEnv
            methods

    return result { ebrClassEnv = classEnv', ebrTypeEnv = typeEnv' }

  ClassDeclExpr _ ->
    -- Class with no type parameters is rejected.
    return result
  
  -- 3. Instance Definitions (from InstanceDeclExpr).
  -- Multi-param-friendly: instance declarations may carry one or more
  -- types (`instance Embed Integer (Frac Integer) where ...`). All of them
  -- are stored in `instTypes`; the legacy `instType` accessor reads the head.
  InstanceDeclExpr (InstanceDecl context className instTypes methods) -> do
    let classEnv = ebrClassEnv result
        typeEnv = ebrTypeEnv result

        -- Convert all instance types
        instanceTypeList = map t2t instTypes

        -- Get the primary instance type (head) for backward compatibility
        mainInstType = case instanceTypeList of
          []    -> TAny
          (t:_) -> t

        -- Create InstanceInfo
        instInfo = Types.InstanceInfo
          { Types.instContext = map (constraintToInternal aliasEnv) context
          , Types.instClass = className
          , Types.instTypes = instanceTypeList
          , Types.instMethods = []  -- Methods are handled during desugaring/evaluation
          }
        
        -- Register instance
        classEnv' = addInstance className instInfo classEnv
        
        -- Register method type signatures for generated methods
        -- This prevents "Unbound variable" warnings during type inference.
        -- Pass the full instance type list so the registered names match the
        -- ones Desugar emits (e.g. `embedMathValueMathValueEmbed`,
        -- `embedMathValueMathValue` for `instance Embed MathValue MathValue`).
        typeEnv' = registerInstanceMethods className mainInstType instanceTypeList (map (constraintToInternal aliasEnv) context) methods classEnv' typeEnv

    return result { ebrClassEnv = classEnv', ebrTypeEnv = typeEnv' }
  
  -- 4. Type Signature Collection (from Define, DefineWithType)
  -- Note: We only collect explicit type signatures here.
  -- Inferred types will be added during type inference.
  DefineWithType typedVar _expr -> do
    let name = typedVarName typedVar
        varIndices = typedVarIndices typedVar
        -- Convert VarIndex to Index (Maybe Var) - like transVarIndex but with Nothing content
        indexTypes = concatMap transVarIndex varIndices
        -- Create Var with index structure (content is Just Var, so map to Nothing)
        var = Var name (map (fmap (const Nothing)) indexTypes)
        params = typedVarParams typedVar
        retType = t2t (typedVarRetType typedVar)
        paramTypes = map (typedParamToType aliasEnv) params
        
        -- Build the function type, then keep declared inductive names in the
        -- target index concrete (e.g. `nat : Matcher p Nat` does not become
        -- `forall a. Matcher p a`).
        funType = concretizeDeclaredTypes declaredTypes (foldr TFun retType paramTypes)

        -- Convert constraints from AST to internal representation
        constraints = map (constraintToInternal aliasEnv) (typedVarConstraints typedVar)

        -- Generalize free type variables in the type signature
        -- This handles type parameters like {a, b, c} in def compose {a, b, c} ...
        freeVars = Set.toList (freeTyVars funType)
        typeScheme = Types.Forall (Set.toList (freeCapVars funType))
                                  freeVars constraints funType
        
        typeEnv = ebrTypeEnv result
        typeEnv' = extendEnv var typeScheme typeEnv

    -- Restriction on open atom sets in the declared signature: a nested
    -- Poly tower may contain at most one [..] (the runtime reshape's atom
    -- routing would otherwise be ambiguous; Types.hasAmbiguousOpenTower).
    -- Definitions reach the reshape through TypedDesugar.maybeReshape, not
    -- the IReshape inference path, so the check lives at signature
    -- collection.
    when (Types.hasAmbiguousOpenTower funType) $ throwError $ Default $
      "def " ++ name ++ ": at most one open atom set [..] may appear " ++
      "in a nested Poly tower: " ++ prettyType funType

    return result { ebrTypeEnv = typeEnv' }

  -- 5. Pattern Inductive Declarations (from PatternInductiveDecl)
  PatternInductiveDecl typeName typeParams constructors -> do
    let typeParamVars = map (TVar . TyVar) typeParams
        -- Special cases: [a] as TCollection and String as TString
        patternType = case (typeName, typeParams) of
                        ("[]", [param]) -> TCollection (TVar (TyVar param))
                        ("String", [])  -> TString
                        _               -> TInductive typeName typeParamVars
        patternCtorEnv = ebrPatternConstructorEnv result
    
    -- Register each pattern constructor to pattern constructor environment
    patternCtorEnv' <- foldM (registerPatternConstructor aliasEnv typeName typeParams patternType)
                              patternCtorEnv
                              constructors
    
    return result { ebrPatternConstructorEnv = patternCtorEnv' }
  
  -- 6. Pattern Function Declarations (from PatternFunctionDecl)
  PatternFunctionDecl name typeParams params retType _body -> do
    let paramTypes = map (t2t . snd) params
        retType' = t2t retType
        -- Pattern function type: arg1 -> arg2 -> ... -> retType (without Pattern wrapper)
        patternFuncType = foldr TFun retType' paramTypes
        
        -- Quantify over type parameters
        tyVars = map TyVar typeParams
        typeScheme = Types.Forall (Set.toList (freeCapVars patternFuncType))
                                  tyVars [] patternFuncType
        
        patternEnv = ebrPatternTypeEnv result
        patternEnv' = extendPatternEnv name typeScheme patternEnv
    
    return result { ebrPatternTypeEnv = patternEnv' }
  
  -- Phase alpha/beta: cas-type aliases and cas-subtype edges were collected
  -- in the prepass (buildEnvironments), so nothing to do per-declaration here.
  DeclareCasType _ _ -> return result
  DeclareCasSubtype _ _ -> return result
  -- M4: cas-quotient declarations are macro-expanded away before environment
  -- building (Eval.expandCasQuotientDecls); nothing should reach here.
  DeclareCasQuotient {} -> return result

  -- Other expressions don't contribute to environment building
  Define {} -> return result
  Test {} -> return result
  Execute {} -> return result
  LoadFile {} -> return result  -- Should not appear after expandLoads
  InfixDecl {} -> return result
  
  -- 7. Symbol Declarations (from DeclareSymbol)
  DeclareSymbol names mTypeExpr -> do
    let ty = case mTypeExpr of
               Just texpr -> t2t texpr
               Nothing    -> TInt  -- Default to Integer (MathValue)
        scheme = Forall [] [] [] ty
        typeEnv = ebrTypeEnv result
        -- Add each symbol to the type environment
        typeEnv' = foldr (\name env -> extendEnv (stringToVar name) scheme env) typeEnv names
    return result { ebrTypeEnv = typeEnv' }

  -- 8. Reduction Rule Declarations (from DeclareRule, Phase 7.4)
  -- The parser accepts the rule and we now stash the (name, level, lhs, rhs)
  -- tuple in `ebrReductionRules` for later inspection / Phase 7.5 use.
  -- Application during `casNormalize` is still pending.
  DeclareRule mname level lhs rhs ->
    return result { ebrReductionRules = ebrReductionRules result ++
                                          [(mname, level, lhs, rhs)] }

  -- 8b. Ideal declarations (G3 of cas-simplification).  They desugar to
  -- ordinary definitions (idealRules.N / autoRule.N / mathNormalize),
  -- which are typed like any other define; nothing to register here.
  DeclareIdeal _ -> return result

  -- 9. Derivative Declarations (from DeclareDerivative, Phase 6.3)
  -- Stash the (name, expr) pair in `ebrDerivativeRules`. Wiring into
  -- `Differentiable Factor`'s connection rule is still pending.
  DeclareDerivative name rhs ->
    return result { ebrDerivativeRules = ebrDerivativeRules result ++
                                            [(name, rhs)] }

  -- 10. Math function declarations (Phase 6.3 part 5).
  -- Register the function's type signature so the inference engine knows
  -- `f : MathValue -> MathValue` (the wrapper body that quotes the symbol).
  -- Without this, binary operations like `f 3 + f 4` infer `Any + Any` and
  -- the type-class `+` dispatch fails (returning the method-name string
  -- "plus" where a CASData was expected).
  -- The default `MathValue -> MathValue` may be widened by a subsequent
  -- `declare apply` based on its argument count (see DeclareApply below).
  DeclareMathFunc name mTypeExpr -> do
    let ty = case mTypeExpr of
               Just texpr -> t2t texpr
               Nothing    -> TFun TMathValue TMathValue
        scheme = Forall [] [] [] ty
        typeEnv = ebrTypeEnv result
        typeEnv' = extendEnv (stringToVar name) scheme typeEnv
        names'   = Set.insert name (ebrMathFuncNames result)
    return result { ebrTypeEnv = typeEnv', ebrMathFuncNames = names' }

  -- 11. Math function application rules (Phase A of declare apply impl).
  -- Requires a prior `declare mathfunc <name>` so the function's type and
  -- intent are known. The actual implementation is emitted by Desugar as a
  -- plain `def` that overrides the mathfunc wrapper.
  -- Updates the registered type to `MathValue -> MathValue -> ... -> MathValue`
  -- (one MathValue per arg + result), unless `declare mathfunc` had an
  -- explicit type annotation that already matches arity.
  DeclareApply name args _body -> do
    if Set.member name (ebrMathFuncNames result)
      then do
        let arity = length args
            -- Build MathValue -> MathValue -> ... -> MathValue (n+1 MathValues)
            mathFunTy 0 = TMathValue
            mathFunTy n = TFun TMathValue (mathFunTy (n - 1))
            ty' = mathFunTy arity
            scheme' = Forall [] [] [] ty'
            typeEnv' = extendEnv (stringToVar name) scheme' (ebrTypeEnv result)
        return result { ebrTypeEnv = typeEnv' }
      else throwError $ Default $
        "declare apply " ++ name ++ ": no prior `declare mathfunc " ++ name ++ "`. " ++
        "Add `declare mathfunc " ++ name ++ "` before this `declare apply` declaration."

  where
    -- typeExprToType + cas-type alias expansion (Phase alpha).
    t2t :: TypeExpr -> Type
    t2t = Types.expandTypeAliases aliasEnv . typeExprToType

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Register a single data constructor
registerConstructor :: HashMap.HashMap String Type -> String -> [String] -> Type
                    -> (TypeEnv, ConstructorEnv) -> InductiveConstructor
                    -> EvalM (TypeEnv, ConstructorEnv)
registerConstructor aliasEnv typeName typeParams resultType (typeEnv, ctorEnv)
                    (InductiveConstructor ctorName argTypeExprs) = do
  let argTypes = map (Types.expandTypeAliases aliasEnv . typeExprToType) argTypeExprs
      
      -- Constructor type: argTypes -> resultType
      constructorType = foldr TFun resultType argTypes
      
      -- Quantify over type parameters
      tyVars = map TyVar typeParams
      typeScheme = Types.Forall (Set.toList (freeCapVars constructorType))
                                tyVars [] constructorType
      
      -- Add to type environment
      typeEnv' = extendEnv (stringToVar ctorName) typeScheme typeEnv
      
      -- Add to constructor environment (for pattern matching and evaluation)
      ctorInfo = ConstructorInfo
        { ctorTypeName = typeName
        , ctorArgTypes = argTypes
        , ctorTypeParams = typeParams
        }
      ctorEnv' = HashMap.insert ctorName ctorInfo ctorEnv
  
  return (typeEnv', ctorEnv')

-- | Register a class method to the type environment.
-- The constraint carries ALL class type parameters (multi-param-friendly).
-- For `class Coerce a b where coerce (x: a) : b`, the method is registered
-- with `forall a b. Coerce a b => a -> b`.
registerClassMethod
  :: Set.Set String
  -> HashMap.HashMap String Type
  -> [TyVar]
  -> String
  -> TypeEnv
  -> ClassMethod
  -> TypeEnv
registerClassMethod declaredTypes aliasEnv tyVars className typeEnv
                    (ClassMethod methName params retType _defaultImpl) =
  let paramTypes = map (typedParamToType aliasEnv) params
      methodType =
        concretizeDeclaredTypes declaredTypes $
          foldr
            TFun
            (Types.expandTypeAliases aliasEnv (typeExprToType retType))
            paramTypes
      -- Constraint with all class type params (single-param classes still
      -- produce a singleton list).
      constraint = Types.Constraint className (map TVar tyVars)
      typeScheme = Types.Forall (Set.toList (freeCapVars methodType))
                                tyVars [constraint] methodType
  in
    extendEnv (stringToVar methName) typeScheme typeEnv

-- | Register type signatures for instance methods (generated during desugaring)
-- This prevents "Unbound variable" warnings during type inference.
--
-- Names must match Desugar's `desugarInstanceMethod` / `makeDictDef`, which
-- concatenate the type-constructor name of EVERY instance type (multi-param
-- friendly): e.g. `instance Embed MathValue MathValue` →
-- `embedMathValueMathValueEmbed` (method) and `embedMathValueMathValue`
-- (dictionary).
registerInstanceMethods :: String -> Type -> [Type] -> [Constraint] -> [InstanceMethod] -> ClassEnv -> TypeEnv -> TypeEnv
registerInstanceMethods className instType instTypeList instConstraints methods classEnv typeEnv =
  case lookupClass className classEnv of
    Nothing -> typeEnv  -- Class not found, skip
    Just classInfo ->
      -- Register each instance method
      let typeEnv' = foldr (registerInstanceMethod className instType instTypeList instConstraints classInfo) typeEnv methods

          -- Also register the dictionary itself
          -- e.g., embedMathValueMathValue : Hash String (MathValue -> MathValue)
          instTypeName = concatMap Types.typeToName instTypeList
          dictName = lowerFirst className ++ instTypeName

          -- Build dictionary type: Hash String (method type)
          -- All methods should have the same general shape, so we use the first one
          dictValueType = case methods of
            [] -> TAny
            (m:_) -> case lookup (instanceMethodName m) (Types.classMethods classInfo) of
              Nothing -> TAny
              Just methodType ->
                substituteClassParams classInfo instTypeList methodType

          dictType = THash TString dictValueType
          freeVars = Set.toList (freeTyVars dictType)
          dictScheme = Types.Forall (Set.toList (freeCapVars dictType))
                                    freeVars instConstraints dictType
      in
        extendEnv (stringToVar dictName) dictScheme typeEnv'
  where
    instanceMethodName :: InstanceMethod -> String
    instanceMethodName (InstanceMethod name _ _) = name

    registerInstanceMethod :: String -> Type -> [Type] -> [Constraint] -> Types.ClassInfo -> InstanceMethod -> TypeEnv -> TypeEnv
    registerInstanceMethod clsName _instTy instTyList constraints classInfo (InstanceMethod methName _params _body) env =
      -- Find the method in the class definition
      case lookup methName (Types.classMethods classInfo) of
        Nothing -> env  -- Method not in class definition, skip
        Just methodType ->
          -- Substitute ALL class type parameters with the instance types
          let substitutedType = substituteClassParams classInfo instTyList methodType

              -- Generate method name from ALL instance types (matching Desugar)
              -- e.g., "embedMathValueMathValueEmbed" for instance Embed MathValue MathValue
              instTypeName = concatMap Types.typeToName instTyList
              sanitizedName = sanitizeMethodName methName
              generatedMethodName = lowerFirst clsName ++ instTypeName ++ capitalizeFirst sanitizedName

              -- Extract free type variables from the substituted type
              freeVars = Set.toList (freeTyVars substitutedType)

              -- Create type scheme with constraints from the instance context
              -- e.g., {Eq a} [a] -> [a] -> Bool for instance {Eq a} Eq [a]
              typeScheme = Types.Forall (Set.toList (freeCapVars substitutedType))
                                        freeVars constraints substitutedType
          in
            extendEnv (stringToVar generatedMethodName) typeScheme env
    
    -- Substitute every class type parameter simultaneously with the
    -- corresponding instance type.  Must be single-pass: an instance type
    -- may contain a type variable whose name collides with a later class
    -- parameter, which sequential substTyVar passes would capture.  A zip
    -- arity mismatch leaves the unmatched parameters quantified.
    substituteClassParams :: Types.ClassInfo -> [Type] -> Type -> Type
    substituteClassParams classInfo instTys =
      let pairs = zip (Types.classParams classInfo) instTys
          replace t@(TVar v) = maybe t id (lookup v pairs)
          replace t          = t
      in Types.mapType replace

-- | Extract method name and type from ClassMethod
extractMethodWithType
  :: Set.Set String
  -> HashMap.HashMap String Type
  -> ClassMethod
  -> (String, Type)
extractMethodWithType declaredTypes aliasEnv (ClassMethod name params retType _) =
  let paramTypes = map (typedParamToType aliasEnv) params
      methodType =
        concretizeDeclaredTypes declaredTypes $
          foldr
            TFun
            (Types.expandTypeAliases aliasEnv (typeExprToType retType))
            paramTypes
  in (name, methodType)

-- | Extract class name from ConstraintExpr
extractConstraintName :: ConstraintExpr -> String
extractConstraintName (ConstraintExpr clsName _) = clsName

-- | Convert ConstraintExpr to internal Constraint.
-- Multi-param classes (e.g. `Coerce a b`) carry all class type parameters.
constraintToInternal :: HashMap.HashMap String Type -> ConstraintExpr -> Types.Constraint
constraintToInternal aliasEnv (ConstraintExpr clsName tyExprs) =
  Types.Constraint clsName (case tyExprs of
    [] -> [TAny]
    _  -> map (Types.expandTypeAliases aliasEnv . typeExprToType) tyExprs)

-- | Register a single pattern constructor
registerPatternConstructor :: HashMap.HashMap String Type -> String -> [String] -> Type
                           -> PatternConstructorEnv -> PatternConstructor
                           -> EvalM PatternConstructorEnv
registerPatternConstructor aliasEnv _typeName typeParams resultType patternCtorEnv
                          (PatternConstructor ctorName argTypeExprs) = do
  let argTypes = map (Types.expandTypeAliases aliasEnv . typeExprToType) argTypeExprs
      
      -- Pattern constructor type: arg1 -> arg2 -> ... -> resultType (without Pattern wrapper)
      patternCtorType = foldr TFun resultType argTypes
      
      -- Quantify over type parameters
      tyVars = map TyVar typeParams
      typeScheme = Types.Forall (Set.toList (freeCapVars patternCtorType))
                                tyVars [] patternCtorType
      
      -- Add to pattern constructor environment (same format as PatternTypeEnv)
      patternCtorEnv' = extendPatternEnv ctorName typeScheme patternCtorEnv
  
  return patternCtorEnv'

-- | Convert TypedParam to Type (cas-type aliases expanded)
typedParamToType :: HashMap.HashMap String Type -> TypedParam -> Type
typedParamToType aliasEnv = go
  where
    go (TPVar _ ty) = t2t ty
    go (TPInvertedVar _ ty) = t2t ty
    go (TPTuple elems) = TTuple (map go elems)
    go (TPWildcard ty) = t2t ty
    go (TPUntypedVar _) = TVar (TyVar "a")  -- Will be inferred
    go TPUntypedWildcard = TVar (TyVar "a")  -- Will be inferred
    t2t = Types.expandTypeAliases aliasEnv . typeExprToType
