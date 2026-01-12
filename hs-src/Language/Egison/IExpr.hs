{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
Module      : Language.Egison.IExpr
Licence     : MIT

This module defines internal representation of Egison language.
-}

module Language.Egison.IExpr
  ( ITopExpr (..)
  , IExpr (..)
  , IPattern (..)
  , ILoopRange (..)
  , IBindingExpr
  , IMatchClause
  , IPatternDef
  , IPrimitiveDataPattern
  -- Typed versions
  , TITopExpr (..)
  , TIExpr (..)
  , TIExprNode (..)
  , TIPattern (..)
  , TILoopRange (..)
  , TIBindingExpr
  , TIMatchClause
  , TIPatternDef
  , tiExprType
  , tiExprScheme
  , tiExprTypeVars
  , tiExprConstraints
  , tipType
  , stripType
  , stripTypeTopExpr
  , Var (..)
  , stringToVar
  , extractNameFromVar
  , Index (..)
  , extractSupOrSubIndex
  , extractIndex
  , makeIApply
  -- Re-export from AST
  , ConstantExpr (..)
  , PMMode (..)
  , PrimitivePatPattern (..)
  , PDPatternBase (..)
  ) where

import           Data.Hashable
import           GHC.Generics        (Generic)

import           Language.Egison.AST (ConstantExpr (..), PDPatternBase (..), PMMode (..), PrimitivePatPattern (..))
import           Language.Egison.Type.Types (Type(..), TypeScheme(..), Constraint(..), TyVar(..))

data ITopExpr
  = IDefine Var IExpr
  | IDefineMany [(Var, IExpr)]  -- Multiple definitions (for type class instances)
  | ITest IExpr
  | IExecute IExpr
  | ILoadFile String
  | ILoad String
  | IDeclareSymbol [String] (Maybe Type)  -- Symbol declaration
  deriving Show

data IExpr
  = IConstantExpr ConstantExpr
  | IVarExpr String
  | IIndexedExpr Bool IExpr [Index IExpr]
  | ISubrefsExpr Bool IExpr IExpr
  | ISuprefsExpr Bool IExpr IExpr
  | IUserrefsExpr Bool IExpr IExpr
  | IInductiveDataExpr String [IExpr]
  | ITupleExpr [IExpr]
  | ICollectionExpr [IExpr]
  | IConsExpr IExpr IExpr
  | IJoinExpr IExpr IExpr
  | IHashExpr [(IExpr, IExpr)]
  | IVectorExpr [IExpr]
  | ILambdaExpr (Maybe Var) [Var] IExpr
  | IMemoizedLambdaExpr [String] IExpr
  | ICambdaExpr String IExpr
  | IIfExpr IExpr IExpr IExpr
  | ILetRecExpr [IBindingExpr] IExpr
  | ILetExpr [IBindingExpr] IExpr
  | IWithSymbolsExpr [String] IExpr
  | IMatchExpr PMMode IExpr IExpr [IMatchClause]
  | IMatchAllExpr PMMode IExpr IExpr [IMatchClause]
  | IMatcherExpr [IPatternDef]
  | IQuoteExpr IExpr
  | IQuoteSymbolExpr IExpr
  | IWedgeApplyExpr IExpr [IExpr]
  | IDoExpr [IBindingExpr] IExpr
  | ISeqExpr IExpr IExpr
  | IApplyExpr IExpr [IExpr]
  | IGenerateTensorExpr IExpr IExpr
  | ITensorExpr IExpr IExpr
  | ITensorContractExpr IExpr
  | ITensorMapExpr IExpr IExpr
  | ITensorMap2Expr IExpr IExpr IExpr
  | ITransposeExpr IExpr IExpr
  | IFlipIndicesExpr IExpr
  | IFunctionExpr [String]
  deriving Show

type IBindingExpr = (IPrimitiveDataPattern, IExpr)
type IMatchClause = (IPattern, IExpr)
type IPatternDef  = (PrimitivePatPattern, IExpr, [(IPrimitiveDataPattern, IExpr)])
type IPrimitiveDataPattern = PDPatternBase Var

data IPattern
  = IWildCard
  | IPatVar String
  | IValuePat IExpr
  | IPredPat IExpr
  | IIndexedPat IPattern [IExpr]
  | ILetPat [IBindingExpr] IPattern
  | INotPat IPattern
  | IAndPat IPattern IPattern
  | IOrPat IPattern IPattern
  | IForallPat IPattern IPattern
  | ITuplePat [IPattern]
  | IInductivePat String [IPattern]
  | ILoopPat String ILoopRange IPattern IPattern
  | IContPat
  | IPApplyPat IExpr [IPattern]
  | IVarPat String
  | IInductiveOrPApplyPat String [IPattern]
  | ISeqNilPat
  | ISeqConsPat IPattern IPattern
  | ILaterPatVar
  -- For symbolic computing
  | IDApplyPat IPattern [IPattern]
  deriving Show

data ILoopRange = ILoopRange IExpr IExpr IPattern
  deriving Show

data Index a
  = Sub a
  | Sup a
  | MultiSub a Integer a
  | MultiSup a Integer a
  | SupSub a
  | User a
  | DF Integer Integer
  deriving (Show, Eq, Functor, Foldable, Generic, Traversable)

extractSupOrSubIndex :: Index a -> Maybe a
extractSupOrSubIndex (Sub x)    = Just x
extractSupOrSubIndex (Sup x)    = Just x
extractSupOrSubIndex (SupSub x) = Just x
extractSupOrSubIndex _          = Nothing

extractIndex :: Index a -> a
extractIndex (Sub x)    = x
extractIndex (Sup x)    = x
extractIndex (SupSub x) = x
extractIndex (User x)   = x
extractIndex DF{}       = undefined

data Var = Var String [Index (Maybe Var)]
  deriving (Generic, Show)

-- for eq and hashable
data Var' = Var' String [Index ()]
  deriving (Eq, Generic, Show)

instance Eq Var where
  Var name (MultiSup _ _ _:_) == Var name' is' = Var name [] == Var name' is'
  Var name (MultiSub _ _ _:_) == Var name' is' = Var name [] == Var name' is'
  Var name is == Var name' (MultiSup _ _ _:_)  = Var name is == Var name' []
  Var name is == Var name' (MultiSub _ _ _:_)  = Var name is == Var name' []
  Var name is == Var name' is'                 = Var' name (map (fmap (\_ -> ())) is) == Var' name' (map (fmap (\_ -> ())) is')

instance Hashable a => Hashable (Index a)
instance Hashable Var'
instance Hashable Var where
  hashWithSalt salt (Var name (MultiSup _ _ _:_)) = hashWithSalt salt (Var' name [])
  hashWithSalt salt (Var name (MultiSub _ _ _:_)) = hashWithSalt salt (Var' name [])
  hashWithSalt salt (Var name is) = hashWithSalt salt (Var' name (map (fmap (\_ -> ())) is))

stringToVar :: String -> Var
stringToVar name = Var name []

extractNameFromVar :: Var -> String
extractNameFromVar (Var name _) = name

makeIApply :: String -> [IExpr] -> IExpr
makeIApply fn args = IApplyExpr (IVarExpr fn) args

--
-- Typed Internal Expressions
--------------------------------------------------------------------------------
-- Phase 9: TIExpr - Evaluatable Typed IR with Type Info Preserved
--------------------------------------------------------------------------------
-- TIExpr is the result of Phase 8 (TypedDesugar) and input to Phase 10 (Evaluation).
-- It carries type information alongside the expression for:
--   - Better runtime error messages with type information
--   - Type-based dispatch during evaluation
--   - Debugging support with type annotations
--
-- Design Decision (design/implementation.md):
-- Type information is preserved after TypedDesugar for better error messages.
-- Type classes have already been resolved to dictionary passing, so no type class
-- constraints are needed here.

-- | Typed top-level expression (Phase 9: TITopExpr)
-- Result of TypedDesugar phase, ready for evaluation.
data TITopExpr
  = TIDefine TypeScheme Var TIExpr     -- ^ Typed definition with type scheme (includes type vars & constraints)
  | TIDefineMany [(Var, TIExpr)]       -- ^ Multiple definitions (letrec)
  | TITest TIExpr                      -- ^ Test expression (REPL)
  | TIExecute TIExpr                   -- ^ Execute IO expression
  | TILoadFile String                  -- ^ Load file (should not appear after expandLoads)
  | TILoad String                      -- ^ Load library (should not appear after expandLoads)
  | TIDeclareSymbol [String] Type      -- ^ Typed symbol declaration
  deriving Show

-- | Typed internal expression (Phase 9: TIExpr)
-- Each expression node carries its inferred/checked type scheme with type variables and constraints.
-- TypeScheme info is preserved for Phase 8 (TypedDesugar) to perform type-driven transformations
-- such as type class dictionary passing and tensorMap insertion.
--
-- NEW: TIExpr is now RECURSIVE - each sub-expression is also a TIExpr,
-- allowing type information to be preserved throughout the tree.
-- This eliminates the need to re-run type inference during TypeClassExpand.
data TIExpr = TIExpr
  { tiScheme :: TypeScheme    -- ^ Type scheme with type variables, constraints, and type
  , tiExprNode :: TIExprNode  -- ^ Typed expression node with typed sub-expressions
  } deriving Show

-- | Typed expression node - each constructor contains typed sub-expressions (TIExpr)
-- This mirrors IExpr but with TIExpr in place of IExpr for all sub-expressions
data TIExprNode
  -- Constants and variables
  = TIConstantExpr ConstantExpr
  | TIVarExpr String
  
  -- Collections
  | TITupleExpr [TIExpr]
  | TICollectionExpr [TIExpr]
  | TIConsExpr TIExpr TIExpr
  | TIJoinExpr TIExpr TIExpr
  | TIHashExpr [(TIExpr, TIExpr)]
  | TIVectorExpr [TIExpr]
  
  -- Lambda expressions
  | TILambdaExpr (Maybe Var) [Var] TIExpr
  | TIMemoizedLambdaExpr [String] TIExpr
  | TICambdaExpr String TIExpr
  
  -- Application
  | TIApplyExpr TIExpr [TIExpr]
  
  -- Control flow
  | TIIfExpr TIExpr TIExpr TIExpr
  
  -- Let expressions
  | TILetExpr [TIBindingExpr] TIExpr
  | TILetRecExpr [TIBindingExpr] TIExpr
  | TIWithSymbolsExpr [String] TIExpr
  
  -- Pattern matching
  | TIMatchExpr PMMode TIExpr TIExpr [TIMatchClause]
  | TIMatchAllExpr PMMode TIExpr TIExpr [TIMatchClause]
  | TIMatcherExpr [TIPatternDef]
  
  -- Inductive data
  | TIInductiveDataExpr String [TIExpr]
  
  -- Quote expressions
  | TIQuoteExpr TIExpr
  | TIQuoteSymbolExpr TIExpr
  
  -- Indexed expressions
  -- TODO: Change [Index IExpr] to [Index TIExpr] later
  | TIIndexedExpr Bool TIExpr [Index IExpr]
  | TISubrefsExpr Bool TIExpr TIExpr
  | TISuprefsExpr Bool TIExpr TIExpr
  | TIUserrefsExpr Bool TIExpr TIExpr
  
  -- Application variants
  | TIWedgeApplyExpr TIExpr [TIExpr]
  
  -- Do expressions
  | TIDoExpr [TIBindingExpr] TIExpr
  
  -- Sequence
  | TISeqExpr TIExpr TIExpr
  
  -- Tensor operations
  | TIGenerateTensorExpr TIExpr TIExpr
  | TITensorExpr TIExpr TIExpr
  | TITensorContractExpr TIExpr
  | TITensorMapExpr TIExpr TIExpr
  | TITensorMap2Expr TIExpr TIExpr TIExpr
  | TITransposeExpr TIExpr TIExpr
  | TIFlipIndicesExpr TIExpr
  
  -- Function reference
  | TIFunctionExpr [String]
  deriving Show

-- | Typed binding expression
type TIBindingExpr = (IPrimitiveDataPattern, TIExpr)

-- | Typed match clause
type TIMatchClause = (TIPattern, TIExpr)

-- | Typed pattern definition (for matcher expressions)
type TIPatternDef = (PrimitivePatPattern, TIExpr, [TIBindingExpr])

-- | Get the type of a typed expression (extracts Type from TypeScheme)
tiExprType :: TIExpr -> Type
tiExprType (TIExpr (Forall _ _ t) _) = t

-- | Get the type scheme of a typed expression
tiExprScheme :: TIExpr -> TypeScheme
tiExprScheme = tiScheme

-- | Get the type variables of a typed expression
tiExprTypeVars :: TIExpr -> [TyVar]
tiExprTypeVars (TIExpr (Forall tvs _ _) _) = tvs

-- | Get the constraints of a typed expression
tiExprConstraints :: TIExpr -> [Constraint]
tiExprConstraints (TIExpr (Forall _ cs _) _) = cs

-- | Strip type information, returning the untyped expression
-- This recursively converts TIExpr back to IExpr for evaluation
stripType :: TIExpr -> IExpr
stripType (TIExpr _ node) = case node of
  TIConstantExpr c -> IConstantExpr c
  TIVarExpr name -> IVarExpr name
  TITupleExpr exprs -> ITupleExpr (map stripType exprs)
  TICollectionExpr exprs -> ICollectionExpr (map stripType exprs)
  TIConsExpr e1 e2 -> IConsExpr (stripType e1) (stripType e2)
  TIJoinExpr e1 e2 -> IJoinExpr (stripType e1) (stripType e2)
  TIHashExpr pairs -> IHashExpr [(stripType k, stripType v) | (k, v) <- pairs]
  TIVectorExpr exprs -> IVectorExpr (map stripType exprs)
  TILambdaExpr mVar params body -> ILambdaExpr mVar params (stripType body)
  TIMemoizedLambdaExpr args body -> IMemoizedLambdaExpr args (stripType body)
  TICambdaExpr var body -> ICambdaExpr var (stripType body)
  TIApplyExpr func args -> IApplyExpr (stripType func) (map stripType args)
  TIIfExpr cond thenE elseE -> IIfExpr (stripType cond) (stripType thenE) (stripType elseE)
  TILetExpr bindings body -> ILetExpr (map stripTypeBinding bindings) (stripType body)
  TILetRecExpr bindings body -> ILetRecExpr (map stripTypeBinding bindings) (stripType body)
  TIWithSymbolsExpr syms body -> IWithSymbolsExpr syms (stripType body)
  TIMatchExpr mode target matcher clauses -> 
    IMatchExpr mode (stripType target) (stripType matcher) (map stripTypeClause clauses)
  TIMatchAllExpr mode target matcher clauses -> 
    IMatchAllExpr mode (stripType target) (stripType matcher) (map stripTypeClause clauses)
  TIMatcherExpr patDefs -> 
    IMatcherExpr [(pat, stripType expr, map stripTypeBinding bindings) | (pat, expr, bindings) <- patDefs]
  TIInductiveDataExpr name exprs -> IInductiveDataExpr name (map stripType exprs)
  TIQuoteExpr e -> IQuoteExpr (stripType e)
  TIQuoteSymbolExpr e -> IQuoteSymbolExpr (stripType e)
  TIIndexedExpr b expr indices -> IIndexedExpr b (stripType expr) indices
  TISubrefsExpr b e1 e2 -> ISubrefsExpr b (stripType e1) (stripType e2)
  TISuprefsExpr b e1 e2 -> ISuprefsExpr b (stripType e1) (stripType e2)
  TIUserrefsExpr b e1 e2 -> IUserrefsExpr b (stripType e1) (stripType e2)
  TIWedgeApplyExpr func args -> IWedgeApplyExpr (stripType func) (map stripType args)
  TIDoExpr bindings body -> IDoExpr (map stripTypeBinding bindings) (stripType body)
  TISeqExpr e1 e2 -> ISeqExpr (stripType e1) (stripType e2)
  TIGenerateTensorExpr shape func -> IGenerateTensorExpr (stripType shape) (stripType func)
  TITensorExpr shape elems -> ITensorExpr (stripType shape) (stripType elems)
  TITensorContractExpr e -> ITensorContractExpr (stripType e)
  TITensorMapExpr func tensor -> ITensorMapExpr (stripType func) (stripType tensor)
  TITensorMap2Expr func t1 t2 -> ITensorMap2Expr (stripType func) (stripType t1) (stripType t2)
  TITransposeExpr tensor perm -> ITransposeExpr (stripType tensor) (stripType perm)
  TIFlipIndicesExpr tensor -> IFlipIndicesExpr (stripType tensor)
  TIFunctionExpr names -> IFunctionExpr names
  where
    stripTypeBinding :: TIBindingExpr -> IBindingExpr
    stripTypeBinding (pat, expr) = (pat, stripType expr)
    
    stripTypeClause :: TIMatchClause -> IMatchClause
    stripTypeClause (TIPattern _ pat, expr) = (pat, stripType expr)
    
    _stripTypeIndex :: Index TIExpr -> Index IExpr
    _stripTypeIndex idx = case idx of
      DF i1 i2 -> DF i1 i2
      Sub e -> Sub (stripType e)
      Sup e -> Sup (stripType e)
      MultiSub e1 n e2 -> MultiSub (stripType e1) n (stripType e2)
      MultiSup e1 n e2 -> MultiSup (stripType e1) n (stripType e2)
      SupSub e -> SupSub (stripType e)
      User e -> User (stripType e)

-- | Strip type information from top-level expression
stripTypeTopExpr :: TITopExpr -> ITopExpr
stripTypeTopExpr (TIDefine _scheme var expr) = IDefine var (stripType expr)
stripTypeTopExpr (TIDefineMany bindings) = IDefineMany [(v, stripType e) | (v, e) <- bindings]
stripTypeTopExpr (TITest expr) = ITest (stripType expr)
stripTypeTopExpr (TIExecute expr) = IExecute (stripType expr)
stripTypeTopExpr (TILoadFile file) = ILoadFile file
stripTypeTopExpr (TILoad file) = ILoad file
stripTypeTopExpr (TIDeclareSymbol names ty) = IDeclareSymbol names (Just ty)

-- | Typed pattern (for future use)
data TIPattern = TIPattern
  { tipScheme :: TypeScheme  -- ^ Type scheme with type variables and constraints
  , tipPattern :: IPattern   -- ^ The underlying pattern
  } deriving Show

-- | Get the type of a typed pattern (extracts Type from TypeScheme)
tipType :: TIPattern -> Type
tipType (TIPattern (Forall _ _ t) _) = t

-- | Typed loop range
data TILoopRange = TILoopRange TIExpr TIExpr TIPattern
  deriving Show

-- NOTE: TIBindingExpr, TIMatchClause, and TIPatternDef are now defined
-- near TIExprNode (around line 302-308) to keep type definitions close together

instance {-# OVERLAPPING #-} Show (Index String) where
  show (Sup s)    = "~" ++ s
  show (Sub s)    = "_" ++ s
  show (SupSub s) = "~_" ++ s
  show (User s)   = "|" ++ s
  show (DF _ _)   = ""
