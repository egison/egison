{- |
Module      : Language.Egison.Type.Error
Licence     : MIT

This module defines type errors for the Egison type system.
-}

{-# LANGUAGE DeriveGeneric #-}

module Language.Egison.Type.Error
  ( TypeError(..)
  , TypeErrorContext(..)
  , formatTypeError
  ) where

import           GHC.Generics               (Generic)

import           Language.Egison.Type.Index (IndexSpec, Index(..), IndexKind(..))
import           Language.Egison.Type.Types (TensorShape (..), TyVar (..), Type (..))

-- | Context information for where a type error occurred
data TypeErrorContext = TypeErrorContext
  { errorLocation :: Maybe String   -- ^ File location if available
  , errorExpr     :: Maybe String   -- ^ Expression that caused the error
  , errorContext  :: Maybe String   -- ^ Additional context (e.g., "in function application")
  } deriving (Eq, Show, Generic)

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

-- | Empty context (for future use)
_emptyContext :: TypeErrorContext
_emptyContext = TypeErrorContext Nothing Nothing Nothing

-- | Format a type error for display
formatTypeError :: TypeError -> String
formatTypeError err = case err of
  UnificationError t1 t2 ctx ->
    formatWithContext ctx $
      "Cannot unify types:\n" ++
      "  Expected: " ++ prettyType t1 ++ "\n" ++
      "  Actual:   " ++ prettyType t2

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
        Just loc -> "At " ++ loc ++ ":\n"
        Nothing  -> ""
      exprStr = case errorExpr ctx of
        Just expr -> "In expression: " ++ expr ++ "\n"
        Nothing   -> ""
      ctxStr = case errorContext ctx of
        Just c -> "(" ++ c ++ ")\n"
        Nothing -> ""
  in locStr ++ exprStr ++ ctxStr ++ msg

-- | Pretty print a type
prettyType :: Type -> String
prettyType TInt = "Integer"
prettyType TFloat = "Float"
prettyType TBool = "Bool"
prettyType TChar = "Char"
prettyType TString = "String"
prettyType TUnit = "()"
prettyType TAny = "_"
prettyType (TVar (TyVar v)) = v
prettyType (TList t) = "[" ++ prettyType t ++ "]"
prettyType (TTuple ts) = "(" ++ unwords (map prettyType ts) ++ ")"
prettyType (TFun t1 t2) = prettyType t1 ++ " -> " ++ prettyType t2
prettyType (TMatcher t) = "Matcher " ++ prettyType t
prettyType (TPattern t) = "Pattern " ++ prettyType t
prettyType (TTensor t sh is) =
  "Tensor " ++ prettyType t ++ " " ++ prettyShape sh ++ prettyIndices is
prettyType (TCollection t) = "Collection " ++ prettyType t
prettyType (THash k v) = "Hash " ++ prettyType k ++ " " ++ prettyType v
prettyType (TIORef t) = "IORef " ++ prettyType t

-- | Pretty print a tensor shape
prettyShape :: TensorShape -> String
prettyShape (ShapeLit dims) = show dims
prettyShape (ShapeVar v) = v
prettyShape ShapeUnknown = "?"

-- | Pretty print indices
prettyIndices :: IndexSpec -> String
prettyIndices [] = ""
prettyIndices is = concatMap prettyIndex is
  where
    prettyIndex (IndexSym Superscript s) = "~" ++ s
    prettyIndex (IndexSym Subscript s) = "_" ++ s
    prettyIndex (IndexPlaceholder Superscript) = "~#"
    prettyIndex (IndexPlaceholder Subscript) = "_#"
    prettyIndex (IndexVar s) = s

