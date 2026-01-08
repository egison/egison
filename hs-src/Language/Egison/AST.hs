{-# LANGUAGE DeriveTraversable #-}

{- |
Module      : Language.Egison.AST
Licence     : MIT

This module defines the syntax of Egison.
-}

module Language.Egison.AST
  ( TopExpr (..)
  , ConstantExpr (..)
  , Expr (..)
  , Pattern (..)
  , VarWithIndices (..)
  , makeApply
  , Arg (..)
  , ArgPattern (..)
  , IndexExpr (..)
  , VarIndex (..)
  , PMMode (..)
  , BindingExpr (..)
  , MatchClause
  , PatternDef (..)
  , LoopRange (..)
  , PrimitivePatPattern (..)
  , PDPatternBase (..)
  , PrimitiveDataPattern
  , Op (..)
  , Assoc (..)
  , reservedExprOp
  , reservedPatternOp
  , findOpFrom
  , stringToVarWithIndices
  , extractNameFromVarWithIndices
  -- Type annotations
  , TypeExpr (..)
  , TensorShapeExpr (..)
  , ShapeDim (..)
  , TensorIndexExpr (..)
  , TypedParam (..)
  , TypedVarWithIndices (..)
  -- Inductive data types
  , InductiveConstructor (..)
  -- Pattern inductive types
  , PatternConstructor (..)
  -- Type classes
  , ClassDecl (..)
  , ClassMethod (..)
  , InstanceDecl (..)
  , InstanceMethod (..)
  , ConstraintExpr (..)
  ) where

import           Data.List  (find)
import           Data.Maybe (fromJust)
import           Data.Text  (Text)

data TopExpr
  = Define VarWithIndices Expr
  | DefineWithType TypedVarWithIndices Expr  -- ^ Definition with type annotation
  | Test Expr
  | Execute Expr
    -- temporary : we will replace load to import and export
  | LoadFile String
  | Load String
  | InfixDecl Bool Op -- True for pattern infix; False for expression infix
  | InductiveDecl String [String] [InductiveConstructor]
    -- ^ Inductive data type declaration with type parameters
    -- e.g., inductive Ordering := | Less | Equal | Greater
    --       inductive Maybe a := | Nothing | Just a
    -- String: type name, [String]: type parameters, [InductiveConstructor]: constructors
  | ClassDeclExpr ClassDecl
    -- ^ Type class declaration
    -- e.g., class Eq a where (==) (x: a) (y: a) : Bool
  | InstanceDeclExpr InstanceDecl
    -- ^ Type class instance declaration
    -- e.g., instance Eq Integer where (==) x y := x = y
  | PatternInductiveDecl String [String] [PatternConstructor]
    -- ^ Pattern inductive type declaration
    -- e.g., inductive pattern MyList a := | myNil | myCons a (MyList a)
    -- String: pattern type name, [String]: type parameters, [PatternConstructor]: constructors
  | PatternFunctionDecl String [String] [(String, TypeExpr)] TypeExpr Pattern
    -- ^ Pattern function declaration
    -- e.g., def pattern twin {a} (p1 : a) (p2 : MyList a) : MyList a := ...
    -- String: function name, [String]: type parameters, [(String, TypeExpr)]: parameters, TypeExpr: return type, Pattern: body
 deriving Show

-- | Type class declaration
-- e.g., class Eq a where ...
--       class Eq a => Ord a where ...
data ClassDecl = ClassDecl
  { className       :: String           -- ^ Class name (e.g., "Eq", "Ord")
  , classTypeParams :: [String]         -- ^ Type parameters (e.g., ["a"])
  , classSuperclasses :: [ConstraintExpr] -- ^ Superclass constraints (e.g., [Eq a] for Ord)
  , classMethods    :: [ClassMethod]    -- ^ Method declarations
  } deriving Show

-- | Type class method declaration
-- e.g., (==) (x: a) (y: a) : Bool
--       (/=) (x: a) (y: a) : Bool := not (x == y)
data ClassMethod = ClassMethod
  { methodName    :: String             -- ^ Method name (e.g., "==")
  , methodParams  :: [TypedParam]       -- ^ Method parameters with types
  , methodRetType :: TypeExpr           -- ^ Return type
  , methodDefault :: Maybe Expr         -- ^ Optional default implementation
  } deriving Show

-- | Type class instance declaration
-- e.g., instance Eq Integer where ...
--       instance Eq a => Eq [a] where ...
data InstanceDecl = InstanceDecl
  { instanceConstraints :: [ConstraintExpr] -- ^ Instance constraints (e.g., [Eq a] for Eq [a])
  , instanceClass       :: String           -- ^ Class name (e.g., "Eq")
  , instanceTypes       :: [TypeExpr]       -- ^ Instance types (e.g., [Integer] or [[a]])
  , instanceMethods     :: [InstanceMethod] -- ^ Method implementations
  } deriving Show

-- | Instance method implementation
-- e.g., (==) x y := x = y
data InstanceMethod = InstanceMethod
  { instMethodName   :: String          -- ^ Method name
  , instMethodParams :: [String]        -- ^ Parameter names
  , instMethodBody   :: Expr            -- ^ Method body
  } deriving Show

-- | Type constraint expression
-- e.g., Eq a, Ord a
data ConstraintExpr = ConstraintExpr
  { constraintClass :: String           -- ^ Class name
  , constraintTypes :: [TypeExpr]       -- ^ Type arguments
  } deriving (Show, Eq)

-- | Constructor for inductive data type
-- e.g., Less, S Nat, Node Tree Tree
data InductiveConstructor = InductiveConstructor
  { inductiveCtorName :: String      -- ^ Constructor name (e.g., "Less", "S", "Node")
  , inductiveCtorArgs :: [TypeExpr]  -- ^ Constructor argument types (e.g., [], [Nat], [Tree, Tree])
  } deriving (Show, Eq)

-- | Constructor for pattern inductive type
-- e.g., myNil, myCons a (MyList a)
data PatternConstructor = PatternConstructor
  { patternCtorName :: String      -- ^ Pattern constructor name (e.g., "myNil", "myCons")
  , patternCtorArgs :: [TypeExpr]  -- ^ Pattern constructor argument types (e.g., [], [a, MyList a])
  } deriving (Show, Eq)

data ConstantExpr
  = CharExpr Char
  | StringExpr Text
  | BoolExpr Bool
  | IntegerExpr Integer
  | FloatExpr Double
  | SomethingExpr
  | UndefinedExpr
  deriving Show

data Expr
  = ConstantExpr ConstantExpr
  | VarExpr String
  | FreshVarExpr
  | IndexedExpr Bool Expr [IndexExpr Expr]  -- True -> delete old index and append new one
  | SubrefsExpr Bool Expr Expr
  | SuprefsExpr Bool Expr Expr
  | UserrefsExpr Bool Expr Expr
  | TupleExpr [Expr]
  | CollectionExpr [Expr]
  | ConsExpr Expr Expr
  | JoinExpr Expr Expr
  | HashExpr [(Expr, Expr)]
  | VectorExpr [Expr]

  | LambdaExpr [Arg ArgPattern] Expr
  | LambdaExpr' [Arg VarWithIndices] Expr
  | TypedLambdaExpr [(String, TypeExpr)] TypeExpr Expr  -- ^ Lambda with typed parameters and return type
  | MemoizedLambdaExpr [String] Expr
  | TypedMemoizedLambdaExpr [TypedParam] TypeExpr Expr  -- ^ Memoized lambda with typed parameters
  | CambdaExpr String Expr
  | PatternFunctionExpr [String] Pattern

  | IfExpr Expr Expr Expr
  | LetExpr [BindingExpr] Expr
  | LetRecExpr [BindingExpr] Expr
  | WithSymbolsExpr [String] Expr

  | MatchExpr PMMode Expr Expr [MatchClause]
  | MatchAllExpr PMMode Expr Expr [MatchClause]
  | MatchLambdaExpr Expr [MatchClause]
  | MatchAllLambdaExpr Expr [MatchClause]

  | MatcherExpr [PatternDef]
  | AlgebraicDataMatcherExpr [(String, [Expr])]

  | QuoteExpr Expr
  | QuoteSymbolExpr Expr
  | WedgeApplyExpr Expr [Expr]

  | DoExpr [BindingExpr] Expr

  | PrefixExpr String Expr
  | InfixExpr Op Expr Expr
  | SectionExpr Op (Maybe Expr) (Maybe Expr) -- There cannot be 'SectionExpr op (Just _) (Just _)'

  | SeqExpr Expr Expr
  | ApplyExpr Expr [Expr]
  | CApplyExpr Expr Expr
  | AnonParamFuncExpr Integer Expr      -- e.g. 2#2, 3#$1, 2#($1 + $2)
  | AnonTupleParamFuncExpr Integer Expr -- e.g. (2)#2, (3)#$1, (2)#($1 + $2)
  | AnonListParamFuncExpr Integer Expr  -- e.g. [2]#2, [3]#$1, [2]#($1 + $2)
  | AnonParamExpr Integer               -- e.g. $1, $2

  | GenerateTensorExpr Expr Expr
  | TensorExpr Expr Expr
  | TensorContractExpr Expr
  | TensorMapExpr Expr Expr
  | TensorMap2Expr Expr Expr Expr
  | TransposeExpr Expr Expr
  | FlipIndicesExpr Expr                              -- Does not appear in user program

  | FunctionExpr [String]

  | TypeAnnotation Expr TypeExpr  -- ^ Expression with type annotation (expr : type)
  deriving Show

data VarWithIndices = VarWithIndices String [VarIndex]
  deriving (Show, Eq)

data Arg a
  = Arg a
  | InvertedArg a
  deriving Show

data ArgPattern
  = APWildCard
  | APPatVar VarWithIndices
  | APInductivePat String [Arg ArgPattern]
  | APTuplePat [Arg ArgPattern]
  | APEmptyPat
  | APConsPat (Arg ArgPattern) ArgPattern
  | APSnocPat ArgPattern (Arg ArgPattern)
  deriving Show

data VarIndex
  = VSubscript String
  | VSuperscript String
  | VMultiSubscript String Integer String   -- _(a_1)..._(a_n) -> VMultiSubscript "a" 1 "n"
  | VMultiSuperscript String Integer String -- ~(a_1)...~(a_n) -> VMultiSuperscript "a" 1 "n"
  | VGroupScripts [VarIndex]
  | VSymmScripts [VarIndex]
  | VAntiSymmScripts [VarIndex]
  deriving (Show, Eq)

data IndexExpr a
  = Subscript a
  | Superscript a
  | SupSubscript a
  | MultiSubscript a a
  | MultiSuperscript a a
  | Userscript a
  deriving (Show, Eq, Functor, Foldable, Traversable)

data PMMode = BFSMode | DFSMode
  deriving Show

data BindingExpr
  = Bind PrimitiveDataPattern Expr
  | BindWithIndices VarWithIndices Expr
  | BindWithType TypedVarWithIndices Expr  -- ^ Binding with type annotation (for where clauses)
  deriving Show

type MatchClause = (Pattern, Expr)

-- | Pattern definition in a matcher (with optional type class constraints)
data PatternDef = PatternDef
  { patDefPattern     :: PrimitivePatPattern
  , patDefMatcher     :: Expr
  , patDefClauses     :: [(PrimitiveDataPattern, Expr)]
  } deriving Show

data Pattern
  = WildCard
  | PatVar String
  | ValuePat Expr
  | PredPat Expr
  | IndexedPat Pattern [Expr]
  | LetPat [BindingExpr] Pattern
  | InfixPat Op Pattern Pattern -- Includes AndPat,OrPat,InductivePat(cons/join)
  | NotPat Pattern
  | AndPat Pattern Pattern
  | OrPat Pattern Pattern
  | ForallPat Pattern Pattern
  | TuplePat [Pattern]
  | InductivePat String [Pattern]
  | LoopPat String LoopRange Pattern Pattern
  | ContPat
  | PApplyPat Expr [Pattern]
  | VarPat String
  | InductiveOrPApplyPat String [Pattern]
  | SeqNilPat
  | SeqConsPat Pattern Pattern
  | LaterPatVar
  -- For symbolic computing
  | DApplyPat Pattern [Pattern]
  deriving Show

data LoopRange = LoopRange Expr Expr Pattern
  deriving Show

data PrimitivePatPattern
  = PPWildCard
  | PPPatVar
  | PPValuePat String  -- Variable name
  | PPInductivePat String [PrimitivePatPattern]
  | PPTuplePat [PrimitivePatPattern]
  deriving Show

data PDPatternBase var
  = PDWildCard
  | PDPatVar var
  | PDInductivePat String [PDPatternBase var]
  | PDTuplePat [PDPatternBase var]
  | PDEmptyPat
  | PDConsPat (PDPatternBase var) (PDPatternBase var)
  | PDSnocPat (PDPatternBase var) (PDPatternBase var)
  | PDConstantPat ConstantExpr
  deriving (Functor, Foldable, Show)

type PrimitiveDataPattern = PDPatternBase String

data Op
  = Op { repr     :: String  -- syntastic representation
       , priority :: Int
       , assoc    :: Assoc
       , isWedge  :: Bool    -- True if operator is prefixed with '!'. Only used for expression infix.
       }
  deriving (Eq, Ord, Show)

data Assoc
  = InfixL
  | InfixR
  | InfixN
  | Prefix
  deriving (Eq, Ord)

instance Show Assoc where
  show InfixL = "infixl"
  show InfixR = "infixr"
  show InfixN = "infix"
  show Prefix = "prefix"

reservedExprOp :: [Op]
reservedExprOp =
  [ Op "!"    8 Prefix False -- Wedge and InvertedArg prefix
  , Op "-"    7 Prefix False -- Negate
  , Op "%"    7 InfixL False -- primitive function
  , Op "++"   5 InfixR False
  , Op "::"   5 InfixR False
  , Op "=="   4 InfixL False -- equality (from type class)
  , Op "/="   4 InfixL False -- inequality (from type class)
  , Op "="    4 InfixL False -- primitive function
  , Op "<="   4 InfixL False -- primitive function
  , Op ">="   4 InfixL False -- primitive function
  , Op "<"    4 InfixL False -- primitive function
  , Op ">"    4 InfixL False -- primitive function
  , Op "&&"   3 InfixR False -- logical and (from base)
  , Op "||"   2 InfixR False -- logical or (from base)
  , Op "$"    0 InfixR False -- right-associative lowest-priority (application)
  , Op "+"    6 InfixL False
  , Op "-"    6 InfixL False
  , Op "*"    7 InfixL False
  , Op "/"    7 InfixL False
  , Op "^"    8 InfixL False
  , Op "+'"   6 InfixL False
  , Op "-'"   6 InfixL False
  , Op "*'"   7 InfixL False
  , Op "/'"   7 InfixL False
  , Op "^'"   8 InfixL False
  , Op "∧"    7 InfixL False
  , Op "."    7 InfixL False
  , Op ".'"   7 InfixL False
  ]

reservedPatternOp :: [Op]
reservedPatternOp =
  [ Op "++" 5 InfixR False
  , Op "*:" 5 InfixL False
  , Op "+" 7 InfixR False
  , Op "*" 8 InfixR False
  , Op "/" 8 InfixN False
  , Op "^" 9 InfixN False
  , Op "::" 6 InfixR False  -- required for desugaring collection pattern (priority 6 > ++ priority 5)
  , Op "&"  3 InfixR False
  , Op "|"  2 InfixR False
  ]

findOpFrom :: String -> [Op] -> Op
findOpFrom op table = fromJust $ find ((== op) . repr) table

makeApply :: String -> [Expr] -> Expr
makeApply func args = ApplyExpr (VarExpr func) args

stringToVarWithIndices :: String -> VarWithIndices
stringToVarWithIndices name = VarWithIndices name []

extractNameFromVarWithIndices :: VarWithIndices -> String
extractNameFromVarWithIndices (VarWithIndices name _) = name

--
-- Type expressions (for type annotations)
--

-- | Type expression in source code
data TypeExpr
  = TEInt                              -- ^ Integer (= MathExpr)
  | TEMathExpr                         -- ^ MathExpr (= Integer)
  | TEFloat                            -- ^ Float
  | TEBool                             -- ^ Bool
  | TEChar                             -- ^ Char
  | TEString                           -- ^ String
  | TEVar String                       -- ^ Type variable, e.g., a
  | TEList TypeExpr                    -- ^ List type, e.g., [a]
  | TETuple [TypeExpr]                 -- ^ Tuple type, e.g., (a, b)
  | TEFun TypeExpr TypeExpr            -- ^ Function type, e.g., a -> b
  | TEMatcher TypeExpr                 -- ^ Matcher type
  | TEPattern TypeExpr                 -- ^ Pattern type, e.g., Pattern a
  | TEIO TypeExpr                      -- ^ IO type, e.g., IO ()
  | TETensor TypeExpr TensorShapeExpr [TensorIndexExpr]
                                      -- ^ Tensor type with shape and indices
                                      --   e.g., Tensor Integer [2, 2]_#_#
  | TEApp TypeExpr [TypeExpr]          -- ^ Type application, e.g., List a
  | TEConstrained [ConstraintExpr] TypeExpr
                                      -- ^ Constrained type, e.g., Eq a => a
  deriving (Show, Eq)

-- | Tensor shape expression
data TensorShapeExpr
  = TSLit [Integer]                    -- ^ Concrete shape, e.g., [2, 2]
  | TSVar String                       -- ^ Shape variable
  | TSMixed [ShapeDim]                 -- ^ Mixed shape, e.g., [n, m, 2]
  deriving (Show, Eq)

-- | Shape dimension (can be concrete or variable)
data ShapeDim
  = SDLit Integer                      -- ^ Concrete dimension, e.g., 2
  | SDVar String                       -- ^ Dimension variable, e.g., n
  deriving (Show, Eq)

-- | Tensor index expression
data TensorIndexExpr
  = TISub String                       -- ^ Subscript, e.g., _i
  | TISup String                       -- ^ Superscript, e.g., ~i
  | TIPlaceholderSub                   -- ^ Subscript placeholder, _#
  | TIPlaceholderSup                   -- ^ Superscript placeholder, ~#
  deriving (Show, Eq)

-- | Typed parameter pattern
data TypedParam
  = TPVar String TypeExpr                    -- ^ Simple variable with type: (x: a)
  | TPInvertedVar String TypeExpr            -- ^ Inverted variable with type: (!x: a)
  | TPTuple [TypedParam]                     -- ^ Tuple pattern: ((x: a), (y: b)) or (x: a, y: b)
  | TPWildcard TypeExpr                      -- ^ Wildcard with type: (_: a)
  | TPUntypedVar String                      -- ^ Untyped variable in tuple: x (inferred)
  | TPUntypedWildcard                        -- ^ Untyped wildcard: _
  deriving (Show, Eq)

-- | Variable with type annotation
data TypedVarWithIndices = TypedVarWithIndices
  { typedVarName        :: String
  , typedVarIndices     :: [VarIndex]
  , typedVarConstraints :: [ConstraintExpr]  -- ^ Type class constraints
  , typedVarParams      :: [TypedParam]      -- ^ Typed parameters (can include tuples)
  , typedVarRetType     :: TypeExpr          -- ^ Return type
  } deriving (Show, Eq)

