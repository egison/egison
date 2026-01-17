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

import           Data.List                  (intercalate)
import           GHC.Generics               (Generic)

import           Language.Egison.Type.Index (IndexSpec)
import           Language.Egison.Type.Types (TensorShape (..), TyVar (..), Type (..))

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

-- | Pretty print a type
prettyType :: Type -> String
prettyType TInt = "Integer"
prettyType TMathExpr = "MathExpr"
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
prettyType (TFun t1 t2) = prettyType t1 ++ " -> " ++ prettyType t2
prettyType (TIO t) = "IO " ++ prettyType t
prettyType (TIORef t) = "IORef " ++ prettyType t
prettyType TPort = "Port"
prettyType TAny = "_"

-- | Pretty print a tensor shape
prettyShape :: TensorShape -> String
prettyShape (ShapeLit dims) = show dims
prettyShape (ShapeVar v) = v
prettyShape ShapeUnknown = "?"

