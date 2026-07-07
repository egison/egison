{- |
Module      : Language.Egison.Type.Error
Licence     : MIT

This module defines type errors for the Egison type system.
-}

{-# LANGUAGE DeriveGeneric #-}

module Language.Egison.Type.Error
  ( TypeError(..)
  , TypeErrorContext(..)
  , TypeWarning(..)
  , SourceLocation(..)
  , formatTypeError
  , formatTypeWarning
  , emptyContext
  , withLocation
  , withExpr
  , withContext
  ) where

import           Data.List                  (intercalate, nub)
import           GHC.Generics               (Generic)

import           Language.Egison.Type.Index (IndexSpec)
import           Language.Egison.Type.Types (TensorShape (..), TyVar (..), Type (..), SymbolSet(..), prettyTypeAtomValue,
                                             Constraint (..), constraintClass, constraintTypes)

-- | Source location information
data SourceLocation = SourceLocation
  { srcFile   :: Maybe FilePath     -- ^ Source file path
  , srcLine   :: Maybe Int          -- ^ Line number (1-based)
  , srcColumn :: Maybe Int          -- ^ Column number (1-based)
  } deriving (Eq, Show, Generic)

-- | Context information for where a type error occurred
data TypeErrorContext = TypeErrorContext
  { errorLocation :: Maybe SourceLocation  -- ^ Precise source location
  , errorExpr     :: Maybe String          -- ^ Expression that caused the error
  , errorContext  :: Maybe String          -- ^ Additional context (e.g., "in function application")
  } deriving (Eq, Show, Generic)

-- | Empty error context
emptyContext :: TypeErrorContext
emptyContext = TypeErrorContext Nothing Nothing Nothing

-- | Add location to a context
withLocation :: SourceLocation -> TypeErrorContext -> TypeErrorContext
withLocation loc ctx = ctx { errorLocation = Just loc }

-- | Add expression to a context
withExpr :: String -> TypeErrorContext -> TypeErrorContext
withExpr expr ctx = ctx { errorExpr = Just expr }

-- | Add context message
withContext :: String -> TypeErrorContext -> TypeErrorContext
withContext ctxMsg ctx = ctx { errorContext = Just ctxMsg }

-- | Type warnings (non-fatal issues)
data TypeWarning
  = UnboundVariableWarning String TypeErrorContext
    -- ^ Variable not in type environment (treated as Any in permissive mode)
  | AnyTypeWarning String TypeErrorContext
    -- ^ Expression has 'Any' type
  | PartiallyTypedWarning String Type TypeErrorContext
    -- ^ Expression is only partially typed
  | UnsupportedExpressionWarning String TypeErrorContext
    -- ^ Expression type cannot be inferred (treated as Any)
  | DeprecatedFeatureWarning String TypeErrorContext
    -- ^ Feature is deprecated
  | MatcherCoverageWarning Type [String] TypeErrorContext
    -- ^ A @matcher@ lacks a general clause for some pattern constructor(s) of its matched
    --   type (paper Coverage, Def 4.2(3)): the matched type, then the missing constructors.
  | MatcherNextMatcherWarning Type String TypeErrorContext
    -- ^ A bare-variable next matcher (rendered) at a constructor-/concrete-headed hole (the
    --   hole's type) is not structurally admissible (paper PP-Con, Def 4.2(1a)).
  | ClassMethodShadowWarning String String TypeErrorContext
    -- ^ A top-level definition reuses a class method name (method name, class name).
    --   The definition replaces the dispatching binding, so the method stops
    --   dispatching on its argument type everywhere after this point.
  deriving (Eq, Show, Generic)

-- | Type errors
data TypeError
  = UnificationError Type Type TypeErrorContext
    -- ^ Two types could not be unified
  | OccursCheckError TyVar Type TypeErrorContext
    -- ^ Infinite type detected (e.g., a = [a])
  | UnboundVariable String TypeErrorContext
    -- ^ Variable not found in type environment
  | TypeMismatch Type Type String TypeErrorContext
    -- ^ Types don't match with explanation
  | TensorShapeMismatch TensorShape TensorShape TypeErrorContext
    -- ^ Tensor shapes are incompatible
  | TensorIndexMismatch IndexSpec IndexSpec TypeErrorContext
    -- ^ Tensor indices are incompatible
  | ArityMismatch Int Int TypeErrorContext
    -- ^ Wrong number of arguments
  | NotAFunction Type TypeErrorContext
    -- ^ Tried to apply a non-function
  | NotATensor Type TypeErrorContext
    -- ^ Expected a tensor type
  | AmbiguousType TyVar TypeErrorContext
    -- ^ Could not infer a concrete type
  | TypeAnnotationMismatch Type Type TypeErrorContext
    -- ^ Inferred type doesn't match annotation
  | UnsupportedFeature String TypeErrorContext
    -- ^ Feature not yet implemented
  | MissingSignatureConstraint String [Constraint] TypeErrorContext
    -- ^ A definition's body requires type-class constraints on the
    -- signature's type variables that the signature does not declare
  | PatternFunctionLinearityError String [String] [String] TypeErrorContext
    -- ^ Pattern function parameter-linearity violation (paper PATFUN-DEF side
    --   condition): each parameter must be used exactly once in the body, in
    --   declaration order.  Fields: function name, declared parameters, actual
    --   parameter uses in body order.
  | PatternFunctionParamUnderBranchError String [String] TypeErrorContext
    -- ^ Pattern function parameters used under a branching or repeating pattern
    --   (or-, loop-, not-, forall-pattern): such an occurrence may be expanded
    --   zero or several times along a matching path, breaking the binding
    --   contract.  Fields: function name, offending parameters.
  | MatcherDataArmsNotExhaustive String Type TypeErrorContext
    -- ^ A @matcher@ clause (rendered pp pattern) of a matcher for the given matched type
    --   whose primitive-data-pattern arms are not exhaustive (paper Def 4.2(1c), arm
    --   exhaustiveness): a target that matches the clause's pattern but none of its arms
    --   raises "Primitive data pattern match failed" at runtime instead of backtracking.
    --   Checked as a conservative syntactic approximation (see 'pdArmsExhaustive' in the
    --   inference module); the standard-library convention is a final @| _ -> []@ (or
    --   @| $tgt -> ...@) arm.
  deriving (Eq, Show, Generic)


-- | Format a type error for display
formatTypeError :: TypeError -> String
formatTypeError err = case err of
  UnificationError t1 t2 ctx ->
    formatWithContext ctx $
      "Cannot unify types:\n" ++
      "  Expected: " ++ prettyType t1 ++ " (" ++ show t1 ++ ")\n" ++
      "  Actual:   " ++ prettyType t2 ++ " (" ++ show t2 ++ ")"

  OccursCheckError (TyVar v) t ctx ->
    formatWithContext ctx $
      "Infinite type detected:\n" ++
      "  Type variable '" ++ v ++ "' occurs in " ++ prettyType t

  UnboundVariable name ctx ->
    formatWithContext ctx $
      "Unbound variable: " ++ name

  TypeMismatch t1 t2 reason ctx ->
    formatWithContext ctx $
      "Type mismatch: " ++ reason ++ "\n" ++
      "  Expected: " ++ prettyType t1 ++ "\n" ++
      "  Actual:   " ++ prettyType t2

  TensorShapeMismatch sh1 sh2 ctx ->
    formatWithContext ctx $
      "Tensor shape mismatch:\n" ++
      "  Expected: " ++ prettyShape sh1 ++ "\n" ++
      "  Actual:   " ++ prettyShape sh2

  TensorIndexMismatch is1 is2 ctx ->
    formatWithContext ctx $
      "Tensor index mismatch:\n" ++
      "  Expected: " ++ show is1 ++ "\n" ++
      "  Actual:   " ++ show is2

  ArityMismatch expected actual ctx ->
    formatWithContext ctx $
      "Wrong number of arguments:\n" ++
      "  Expected: " ++ show expected ++ "\n" ++
      "  Actual:   " ++ show actual

  NotAFunction t ctx ->
    formatWithContext ctx $
      "Not a function type: " ++ prettyType t

  NotATensor t ctx ->
    formatWithContext ctx $
      "Expected a tensor type, but got: " ++ prettyType t

  AmbiguousType (TyVar v) ctx ->
    formatWithContext ctx $
      "Ambiguous type: cannot infer a concrete type for '" ++ v ++ "'"

  TypeAnnotationMismatch annotated inferred ctx ->
    formatWithContext ctx $
      "Type annotation mismatch:\n" ++
      "  Annotation: " ++ prettyType annotated ++ "\n" ++
      "  Inferred:   " ++ prettyType inferred

  UnsupportedFeature feature ctx ->
    formatWithContext ctx $
      "Unsupported feature: " ++ feature

  MissingSignatureConstraint name cs ctx ->
    formatWithContext ctx $
      "The body of '" ++ name ++ "' requires type class constraints that its signature does not declare:\n" ++
      "  Missing: " ++ intercalate ", " (map prettyConstraint' cs) ++ "\n" ++
      "  Declare them in the signature, e.g. def " ++ name ++ " {" ++
        intercalate ", " (map prettyConstraint' cs) ++ "} ..."
    where prettyConstraint' c =
            constraintClass c ++ concatMap ((' ' :) . prettyType) (constraintTypes c)

  PatternFunctionLinearityError name params uses ctx ->
    formatWithContext ctx $
      "Pattern function '" ++ name ++ "' must use each parameter exactly once, in declaration order:\n" ++
      "  Parameters:    " ++ intercalate ", " (map ("~" ++) params) ++ "\n" ++
      "  Uses in body:  " ++ (if null uses then "(none)" else intercalate ", " (map ("~" ++) uses))

  PatternFunctionParamUnderBranchError name offenders ctx ->
    formatWithContext ctx $
      "Pattern function '" ++ name ++ "' uses parameters under a branching or repeating pattern\n" ++
      "(or-, loop-, not-, or forall-pattern), where they may be expanded zero or several times:\n" ++
      "  Parameters:  " ++ intercalate ", " (map ("~" ++) offenders)

  MatcherDataArmsNotExhaustive ppStr ty ctx ->
    formatWithContext ctx $
      "Matcher clause `" ++ ppStr ++ "` (matcher for " ++ displayType ty ++ ")" ++
      " has non-exhaustive data-pattern arms: a target that matches the clause's pattern but" ++
      " none of its arms fails at runtime (\"Primitive data pattern match failed\");" ++
      " end the arms with `| _ -> []`" ++
      "\n  (arm exhaustiveness; paper Def 4.2(1c))"

-- | Format error with context
formatWithContext :: TypeErrorContext -> String -> String
formatWithContext ctx msg =
  let locStr = case errorLocation ctx of
        Just loc -> "At " ++ formatSourceLocation loc ++ ":\n"
        Nothing  -> ""
      exprStr = case errorExpr ctx of
        Just expr -> "In expression: " ++ expr ++ "\n"
        Nothing   -> ""
      ctxStr = case errorContext ctx of
        Just c -> "(" ++ c ++ ")\n"
        Nothing -> ""
  in locStr ++ exprStr ++ ctxStr ++ msg

-- | Format source location
formatSourceLocation :: SourceLocation -> String
formatSourceLocation loc =
  let file = maybe "<unknown>" id (srcFile loc)
      line = maybe "?" show (srcLine loc)
      col  = maybe "" ((":" ++) . show) (srcColumn loc)
  in file ++ ":" ++ line ++ col

-- | Format a type warning for display
formatTypeWarning :: TypeWarning -> String
formatTypeWarning warn = case warn of
  UnboundVariableWarning name ctx ->
    formatWithContext ctx $
      "Warning: Unbound variable '" ++ name ++ "' (assuming type 'Any')"

  AnyTypeWarning desc ctx ->
    formatWithContext ctx $
      "Warning: Expression has 'Any' type: " ++ desc

  PartiallyTypedWarning desc ty ctx ->
    formatWithContext ctx $
      "Warning: Partially typed expression: " ++ desc ++ "\n" ++
      "  Inferred type: " ++ prettyType ty

  UnsupportedExpressionWarning desc ctx ->
    formatWithContext ctx $
      "Warning: Cannot infer type for: " ++ desc ++ " (assuming 'Any')"

  DeprecatedFeatureWarning feature ctx ->
    formatWithContext ctx $
      "Warning: Deprecated feature: " ++ feature

  MatcherCoverageWarning ty missing ctx ->
    formatWithContext ctx $
      "Warning: matcher for " ++ displayType ty ++
      " has no general clause for pattern constructor(s): " ++ intercalate ", " missing ++
      "\n  (a pattern using such a constructor would get stuck at runtime; paper Coverage, Def 4.2(3))"

  MatcherNextMatcherWarning holeTy comp ctx ->
    formatWithContext ctx $
      "Warning: the next matcher `" ++ comp ++ "` is a bare-variable matcher, not structurally" ++
      " admissible at a constructor-headed hole of type " ++ displayType holeTy ++
      "\n  (a constructor pattern there would get stuck at runtime; paper PP-Con, Def 4.2(1a))"

  ClassMethodShadowWarning name cls ctx ->
    formatWithContext ctx $
      "Warning: '" ++ name ++ "' is a method of class '" ++ cls ++ "'," ++
      " and this top-level definition shadows it" ++
      "\n  ('" ++ name ++ "' no longer dispatches on its argument type anywhere after" ++
      " this point; rename the definition)"

-- | Pretty print a type after renaming its type variables, in order of first
-- appearance, to @a@, @b@, @c@, ...  Inference-internal names such as @t143@
-- carry no information for the user.  Used by the matcher diagnostics, which
-- each show a single type, so the renaming cannot break cross-references
-- between types (unification errors, which relate two types, are shown with
-- their original variable names).
displayType :: Type -> String
displayType = prettyType . renameVarsForDisplay

renameVarsForDisplay :: Type -> Type
renameVarsForDisplay ty = mapVars ty
  where
    mapping = zip (nub (collect ty))
                  ([TyVar [c] | c <- ['a'..'z']] ++ [TyVar ('a' : show i) | i <- [1 :: Int ..]])
    sub v = case lookup v mapping of
      Just v' -> v'
      Nothing -> v
    mapVars t = case t of
      TVar v             -> TVar (sub v)
      TTuple ts          -> TTuple (map mapVars ts)
      TCollection t1     -> TCollection (mapVars t1)
      TInductive n ts    -> TInductive n (map mapVars ts)
      TTensor t1         -> TTensor (mapVars t1)
      THash t1 t2        -> THash (mapVars t1) (mapVars t2)
      TMatcher t1        -> TMatcher (mapVars t1)
      TMatcherSlot t1 t2 -> TMatcherSlot (mapVars t1) (mapVars t2)
      TFun t1 t2         -> TFun (mapVars t1) (mapVars t2)
      TIO t1             -> TIO (mapVars t1)
      TIORef t1          -> TIORef (mapVars t1)
      TTerm t1 ss        -> TTerm (mapVars t1) ss
      TFrac t1           -> TFrac (mapVars t1)
      TPoly t1 ss        -> TPoly (mapVars t1) ss
      _                  -> t
    collect t = case t of
      TVar v             -> [v]
      TTuple ts          -> concatMap collect ts
      TCollection t1     -> collect t1
      TInductive _ ts    -> concatMap collect ts
      TTensor t1         -> collect t1
      THash t1 t2        -> collect t1 ++ collect t2
      TMatcher t1        -> collect t1
      TMatcherSlot t1 t2 -> collect t1 ++ collect t2
      TFun t1 t2         -> collect t1 ++ collect t2
      TIO t1             -> collect t1
      TIORef t1          -> collect t1
      TTerm t1 _         -> collect t1
      TFrac t1           -> collect t1
      TPoly t1 _         -> collect t1
      _                  -> []

-- | Pretty print a type
prettyType :: Type -> String
prettyType TInt = "Integer"
prettyType TMathValue = "MathValue"
prettyType TPolyExpr = "PolyExpr"
prettyType TTermExpr = "TermExpr"
prettyType TSymbolExpr = "SymbolExpr"
prettyType TIndexExpr = "IndexExpr"
prettyType TFloat = "Float"
prettyType TBool = "Bool"
prettyType TChar = "Char"
prettyType TString = "String"
prettyType (TVar (TyVar v)) = v
prettyType (TTuple ts) = "(" ++ intercalate ", " (map prettyType ts) ++ ")"
prettyType (TCollection t) = "[" ++ prettyType t ++ "]"
prettyType (TInductive name []) = name
prettyType (TInductive name args) = name ++ " " ++ unwords (map prettyType args)
prettyType (TTensor t) = "Tensor " ++ prettyType t
prettyType (THash k v) = "Hash " ++ prettyType k ++ " " ++ prettyType v
prettyType (TMatcher t) = "Matcher " ++ prettyType t
prettyType (TMatcherSlot s t) = "MatcherSlot " ++ prettyType s ++ " " ++ prettyType t
prettyType (TFun t1 t2) = prettyType t1 ++ " -> " ++ prettyType t2
prettyType (TIO t) = "IO " ++ prettyType t
prettyType (TIORef t) = "IORef " ++ prettyType t
prettyType TPort = "Port"
prettyType TAny = "_"
-- New CAS types
prettyType TFactor = "Factor"
prettyType (TTerm t ss) = "Term " ++ prettyType t ++ " " ++ prettySymbolSet ss
prettyType (TFrac t) = "Frac " ++ prettyType t
prettyType (TPoly t ss) = "Poly " ++ prettyType t ++ " " ++ prettySymbolSet ss

-- | Pretty print a SymbolSet (local helper for type errors)
prettySymbolSet :: SymbolSet -> String
prettySymbolSet (SymbolSetClosed syms) = "[" ++ intercalate ", " (map prettyTypeAtomValue syms) ++ "]"
prettySymbolSet SymbolSetOpen = "[..]"
prettySymbolSet (SymbolSetVar (TyVar v)) = v

-- | Pretty print a tensor shape
prettyShape :: TensorShape -> String
prettyShape (ShapeLit dims) = show dims
prettyShape (ShapeVar v) = v
prettyShape ShapeUnknown = "?"

