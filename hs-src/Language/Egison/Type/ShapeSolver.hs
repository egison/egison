{- |
Module      : Language.Egison.Type.ShapeSolver
Licence     : MIT

Pure least-evidence solver for recursive matcher capability generation.

The ordinary type/capability unifier must not solve these equations.  A
'ShapeEquation'

@
g <- d
@

is directional: it records the evidence from which producer/path @g@
generates its capability.  References are initialized to 'ShapeUnseen', copy
cycles therefore have the least solution @unseen@, and external
'ShapeKnown' evidence supplies the only leaves that can seed such a cycle.

The solver is deliberately independent of inference state, annotations,
targets, coverage, and constructor-signature projection.  Callers first
normalize literal evidence and producer flow to the first-order language
below, solve a whole recursive group simultaneously, then run observability
finalization and deferred consumer checks.

There is no iteration limit.  The dependency graph is decomposed into SCCs.
An SCC containing a structurally growing dependency is rejected; the
remaining copy SCCs are collapsed semantically by treating their internal
references as bottom and exactly joining all incoming evidence.  The
component graph is acyclic and is evaluated with memoization.
-}

module Language.Egison.Type.ShapeSolver
  ( ShapeProducerId(..)
  , ShapePathStep(..)
  , ShapeNode(..)
  , ShapeEvidence(..)
  , ShapeEquation(..)
  , SolvedShape(..)
  , ShapeSolution
  , ShapeSolveError(..)
  , rootShapeNode
  , conParameterNode
  , tupleComponentNode
  , solveShapeEquations
  ) where

import           Control.Monad              (foldM)
import           Data.Graph                 (SCC (..), stronglyConnComp)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set

import           Language.Egison.Type.Types (Capability (..),
                                              TypeFormer (..))

-- | Stable identity of a matcher producer within one Shape-solving scope.
--
-- The identity is compiler-internal and must not escape into a type scheme or
-- typed runtime term.
newtype ShapeProducerId = ShapeProducerId Int
  deriving (Eq, Ord, Show)

-- | A canonical capability path below a producer root.
--
-- Indices are zero-based.  Carrying the former/product arity in the path makes
-- malformed paths rejectable before solving and prevents two structurally
-- different paths from sharing an identity accidentally.
data ShapePathStep
  = ShapeConParameter TypeFormer Int
  | ShapeTupleComponent Int Int
  deriving (Eq, Ord, Show)

-- | One generation variable: producer identity plus capability path.
--
-- A path is an identity, not an instruction evaluated by this module.  Before
-- calling 'solveShapeEquations', the producer-flow normalizer must project
-- each structured producer into equations for every path that can be
-- referenced.  In particular, a root equation containing @Known (K p)@ does
-- not implicitly define the node at @K@'s parameter path; that node needs its
-- own equation whose evidence is @Known p@.  Keeping projection outside the
-- solver prevents target types or consumer demands from being consulted while
-- generation evidence is solved.
data ShapeNode = ShapeNode
  { shapeNodeProducer :: ShapeProducerId
  , shapeNodePath     :: [ShapePathStep]
  } deriving (Eq, Ord, Show)

-- | First-order evidence accepted by the recursive Shape solver.
--
-- 'ShapeJoin' is an exact join, not weakening or unification.  It is retained
-- until references have been solved because, for example, @join(p, Ref g)@
-- may be consistent when @g@ itself has least solution @p@.
--
-- 'ShapeRef' names exactly the indicated producer/path node.  It never
-- projects a structured solution at another node.
data ShapeEvidence
  = ShapeUnseen
  | ShapeKnown Capability
  | ShapeRef ShapeNode
  | ShapeCon TypeFormer [ShapeEvidence]
  | ShapeTuple [ShapeEvidence]
  | ShapeJoin [ShapeEvidence]
  deriving (Eq, Show)

-- | A directional generation obligation @node <- evidence@.
--
-- Several equations may have the same left-hand node; all right-hand sides
-- are combined by the same exact join as clause/field evidence.
data ShapeEquation = ShapeEquation
  { shapeEquationNode     :: ShapeNode
  , shapeEquationEvidence :: ShapeEvidence
  } deriving (Eq, Show)

-- | Reference-free least evidence returned by the solver.
--
-- 'SolvedUnseen' is intentionally preserved.  The caller decides, using the
-- declaration-derived observability mask, whether it is an error or canonical
-- empty capability.
data SolvedShape
  = SolvedUnseen
  | SolvedKnown Capability
  | SolvedCon TypeFormer [SolvedShape]
  | SolvedTuple [SolvedShape]
  deriving (Eq, Show)

type ShapeSolution = Map ShapeNode SolvedShape

data ShapeSolveError
  = InvalidShapePath ShapeNode String
  | MalformedShapeFormer TypeFormer Int
  | MalformedKnownFormer TypeFormer Int
  | ExpansiveShapeCycle [ShapeNode]
  | ShapeExactMismatch [ShapeNode] [ShapePathStep]
                       SolvedShape SolvedShape
  | InternalShapeComponentCycle [ShapeNode]
  deriving (Eq, Show)

rootShapeNode :: ShapeProducerId -> ShapeNode
rootShapeNode producer = ShapeNode producer []

conParameterNode :: ShapeNode -> TypeFormer -> Int -> ShapeNode
conParameterNode node former index =
  node
    { shapeNodePath =
        shapeNodePath node ++ [ShapeConParameter former index]
    }

tupleComponentNode :: ShapeNode -> Int -> Int -> ShapeNode
tupleComponentNode node arity index =
  node
    { shapeNodePath =
        shapeNodePath node ++ [ShapeTupleComponent arity index]
    }

-- | Solve a finite simultaneous system of Shape generation equations.
--
-- Nodes mentioned only by 'ShapeRef' are included with the implicit equation
-- @g <- unseen@.  This is the intended least-evidence interpretation for an
-- unseeded recursive producer/path.
solveShapeEquations
  :: [ShapeEquation]
  -> Either ShapeSolveError ShapeSolution
solveShapeEquations equations = do
  let nodes =
        Set.toAscList $
          Set.unions
            [ Set.fromList (map shapeEquationNode equations)
            , Set.unions
                (map (evidenceReferences . shapeEquationEvidence) equations)
            ]
      equationsByNode =
        Map.fromListWith (++)
          [ (shapeEquationNode equation,
             [shapeEquationEvidence equation])
          | equation <- equations
          ]

  mapM_ validateShapeNode nodes
  mapM_ (validateShapeEvidence . shapeEquationEvidence) equations

  let edgesByNode =
        Map.fromList
          [ (node,
             concatMap (dependencyEdges node)
               (Map.findWithDefault [] node equationsByNode))
          | node <- nodes
          ]
      components =
        map flattenScc $
          stronglyConnComp
            [ (node, node, map dependencyTarget
                              (Map.findWithDefault [] node edgesByNode))
            | node <- nodes
            ]
      componentIds = zip [0 :: Int ..] components
      nodeComponents =
        Map.fromList
          [ (node, componentId)
          | (componentId, componentNodes) <- componentIds
          , node <- componentNodes
          ]

  mapM_
    (rejectExpansiveComponent edgesByNode)
    components

  (_, solvedComponents) <-
    foldM
      (\(visiting, solved) (componentId, _) -> do
        (_, solved') <-
          solveComponent
            equationsByNode
            components
            nodeComponents
            visiting
            solved
            componentId
        Right (visiting, solved'))
      (Set.empty, Map.empty)
      componentIds

  Right $
    Map.fromList
      [ (node,
         Map.findWithDefault SolvedUnseen componentId solvedComponents)
      | (node, componentId) <- Map.toAscList nodeComponents
      ]

-- Dependency graph -----------------------------------------------------------

data DependencyEdge = DependencyEdge
  { dependencyTarget    :: ShapeNode
  , dependencyExpansive :: Bool
  } deriving (Eq, Show)

dependencyEdges :: ShapeNode -> ShapeEvidence -> [DependencyEdge]
dependencyEdges source = go False
  where
    go _ ShapeUnseen = []
    go _ (ShapeKnown _) = []
    go underStructure (ShapeRef target) =
      [ DependencyEdge
          { dependencyTarget = target
          , dependencyExpansive =
              underStructure || pathMovesOutward source target
          }
      ]
    go _ (ShapeCon _ children) =
      concatMap (go True) children
    go _ (ShapeTuple components) =
      concatMap (go True) components
    go underStructure (ShapeJoin evidence) =
      concatMap (go underStructure) evidence

-- Assigning a shallower producer value to one of its deeper result paths is a
-- structural-growth edge even when the constructor wrapper has already been
-- normalized into node paths by the caller.  Comparing depths across producer
-- identities is conservative: D4 integration should connect corresponding
-- paths for a pure copy edge.
pathMovesOutward :: ShapeNode -> ShapeNode -> Bool
pathMovesOutward source target =
  length (shapeNodePath source) > length (shapeNodePath target)

evidenceReferences :: ShapeEvidence -> Set ShapeNode
evidenceReferences evidence =
  case evidence of
    ShapeUnseen -> Set.empty
    ShapeKnown _ -> Set.empty
    ShapeRef node -> Set.singleton node
    ShapeCon _ children ->
      Set.unions (map evidenceReferences children)
    ShapeTuple components ->
      Set.unions (map evidenceReferences components)
    ShapeJoin alternatives ->
      Set.unions (map evidenceReferences alternatives)

flattenScc :: SCC ShapeNode -> [ShapeNode]
flattenScc (AcyclicSCC node) = [node]
flattenScc (CyclicSCC nodes) = nodes

rejectExpansiveComponent
  :: Map ShapeNode [DependencyEdge]
  -> [ShapeNode]
  -> Either ShapeSolveError ()
rejectExpansiveComponent edgesByNode nodes =
  let members = Set.fromList nodes
      internalEdges =
        [ edge
        | node <- nodes
        , edge <- Map.findWithDefault [] node edgesByNode
        , dependencyTarget edge `Set.member` members
        ]
  in if any dependencyExpansive internalEdges
       then Left (ExpansiveShapeCycle nodes)
       else Right ()

-- Component evaluation -------------------------------------------------------

type ComponentId = Int
type SolvedComponents = Map ComponentId SolvedShape

solveComponent
  :: Map ShapeNode [ShapeEvidence]
  -> [[ShapeNode]]
  -> Map ShapeNode ComponentId
  -> Set ComponentId
  -> SolvedComponents
  -> ComponentId
  -> Either ShapeSolveError (SolvedShape, SolvedComponents)
solveComponent equationsByNode components nodeComponents visiting solved
               componentId =
  case Map.lookup componentId solved of
    Just value ->
      Right (value, solved)
    Nothing
      | componentId `Set.member` visiting ->
          Left (InternalShapeComponentCycle componentNodes)
      | otherwise -> do
          let visiting' = Set.insert componentId visiting
              evidence =
                concatMap
                  (\node -> Map.findWithDefault [] node equationsByNode)
                  componentNodes
          (values, solved') <-
            foldM
              (\(acc, solvedAcc) item -> do
                (value, solvedNext) <-
                  evaluateEvidence
                    equationsByNode
                    components
                    nodeComponents
                    componentId
                    visiting'
                    solvedAcc
                    item
                Right (value : acc, solvedNext))
              ([], solved)
              evidence
          value <-
            foldM
              (mergeSolved componentNodes [])
              SolvedUnseen
              (reverse values)
          let solved'' = Map.insert componentId value solved'
          Right (value, solved'')
  where
    componentNodes =
      componentAt components componentId

evaluateEvidence
  :: Map ShapeNode [ShapeEvidence]
  -> [[ShapeNode]]
  -> Map ShapeNode ComponentId
  -> ComponentId
  -> Set ComponentId
  -> SolvedComponents
  -> ShapeEvidence
  -> Either ShapeSolveError (SolvedShape, SolvedComponents)
evaluateEvidence equationsByNode components nodeComponents currentComponent
                 visiting solved evidence =
  case evidence of
    ShapeUnseen ->
      Right (SolvedUnseen, solved)
    ShapeKnown capability -> do
      value <- capabilityToSolved capability
      Right (value, solved)
    ShapeRef node ->
      case Map.lookup node nodeComponents of
        Nothing ->
          -- 'solveShapeEquations' includes all references in nodeComponents;
          -- retain bottom defensively if this helper is reused incorrectly.
          Right (SolvedUnseen, solved)
        Just referencedComponent
          | referencedComponent == currentComponent ->
              -- A copy SCC has least solution bottom until an incoming seed is
              -- joined.  Internal references add no evidence of their own.
              Right (SolvedUnseen, solved)
          | otherwise ->
              solveComponent
                equationsByNode
                components
                nodeComponents
                visiting
                solved
                referencedComponent
    ShapeCon former children -> do
      validateFormerArity MalformedShapeFormer former (length children)
      (children', solved') <-
        evaluateMany
          equationsByNode
          components
          nodeComponents
          currentComponent
          visiting
          solved
          children
      Right (SolvedCon former children', solved')
    ShapeTuple components' -> do
      (components'', solved') <-
        evaluateMany
          equationsByNode
          components
          nodeComponents
          currentComponent
          visiting
          solved
          components'
      Right (SolvedTuple components'', solved')
    ShapeJoin alternatives -> do
      (values, solved') <-
        evaluateMany
          equationsByNode
          components
          nodeComponents
          currentComponent
          visiting
          solved
          alternatives
      let componentNodes = componentAt components currentComponent
      value <-
        foldM
          (mergeSolved componentNodes [])
          SolvedUnseen
          values
      Right (value, solved')

evaluateMany
  :: Map ShapeNode [ShapeEvidence]
  -> [[ShapeNode]]
  -> Map ShapeNode ComponentId
  -> ComponentId
  -> Set ComponentId
  -> SolvedComponents
  -> [ShapeEvidence]
  -> Either ShapeSolveError ([SolvedShape], SolvedComponents)
evaluateMany equationsByNode components nodeComponents currentComponent
             visiting solved =
  foldM
    (\(values, solvedAcc) evidence -> do
      (value, solvedNext) <-
        evaluateEvidence
          equationsByNode
          components
          nodeComponents
          currentComponent
          visiting
          solvedAcc
          evidence
      Right (values ++ [value], solvedNext))
    ([], solved)

componentAt :: [[ShapeNode]] -> ComponentId -> [ShapeNode]
componentAt components componentId =
  case drop componentId components of
    nodes : _ -> nodes
    []        -> []

-- Exact evidence join --------------------------------------------------------

mergeSolved
  :: [ShapeNode]
  -> [ShapePathStep]
  -> SolvedShape
  -> SolvedShape
  -> Either ShapeSolveError SolvedShape
mergeSolved _ _ SolvedUnseen right = Right right
mergeSolved _ _ left SolvedUnseen = Right left
mergeSolved component path (SolvedKnown left) (SolvedKnown right)
  | left == right = Right (SolvedKnown left)
  | otherwise =
      Left (ShapeExactMismatch component path
              (SolvedKnown left) (SolvedKnown right))
mergeSolved component path
            left@(SolvedCon leftFormer leftChildren)
            right@(SolvedCon rightFormer rightChildren)
  | leftFormer /= rightFormer =
      Left (ShapeExactMismatch component path left right)
  | length leftChildren /= typeFormerArity leftFormer =
      Left (MalformedShapeFormer leftFormer (length leftChildren))
  | length rightChildren /= typeFormerArity rightFormer =
      Left (MalformedShapeFormer rightFormer (length rightChildren))
  | length leftChildren /= length rightChildren =
      Left (ShapeExactMismatch component path left right)
  | otherwise =
      SolvedCon leftFormer
        <$> sequence
              [ mergeSolved
                  component
                  (path ++ [ShapeConParameter leftFormer index])
                  leftChild
                  rightChild
              | (index, (leftChild, rightChild)) <-
                  zip [0 :: Int ..] (zip leftChildren rightChildren)
              ]
mergeSolved component path
            left@(SolvedTuple leftComponents)
            right@(SolvedTuple rightComponents)
  | length leftComponents /= length rightComponents =
      Left (ShapeExactMismatch component path left right)
  | otherwise =
      let arity = length leftComponents
      in SolvedTuple
           <$> sequence
                 [ mergeSolved
                     component
                     (path ++ [ShapeTupleComponent arity index])
                     leftComponent
                     rightComponent
                 | (index, (leftComponent, rightComponent)) <-
                     zip [0 :: Int ..]
                         (zip leftComponents rightComponents)
                 ]
mergeSolved component path left right =
  Left (ShapeExactMismatch component path left right)

capabilityToSolved
  :: Capability
  -> Either ShapeSolveError SolvedShape
capabilityToSolved capability =
  case capability of
    CapAny ->
      Right (SolvedKnown CapAny)
    cap@(CapVar _) ->
      Right (SolvedKnown cap)
    cap@(CapSkolem _) ->
      Right (SolvedKnown cap)
    CapCon former children -> do
      validateFormerArity MalformedKnownFormer former (length children)
      SolvedCon former <$> mapM capabilityToSolved children
    CapTuple components ->
      SolvedTuple <$> mapM capabilityToSolved components

-- Validation ----------------------------------------------------------------

validateShapeEvidence
  :: ShapeEvidence
  -> Either ShapeSolveError ()
validateShapeEvidence evidence =
  case evidence of
    ShapeUnseen ->
      Right ()
    ShapeKnown capability -> do
      _ <- capabilityToSolved capability
      Right ()
    ShapeRef node ->
      validateShapeNode node
    ShapeCon former children -> do
      validateFormerArity MalformedShapeFormer former (length children)
      mapM_ validateShapeEvidence children
    ShapeTuple components ->
      mapM_ validateShapeEvidence components
    ShapeJoin alternatives ->
      mapM_ validateShapeEvidence alternatives

validateShapeNode :: ShapeNode -> Either ShapeSolveError ()
validateShapeNode node =
  mapM_ validateStep (shapeNodePath node)
  where
    validateStep step =
      case step of
        ShapeConParameter former index
          | index < 0 ->
              Left (InvalidShapePath node
                      "constructor parameter index is negative")
          | index >= typeFormerArity former ->
              Left (InvalidShapePath node
                      "constructor parameter index exceeds former arity")
          | otherwise ->
              Right ()
        ShapeTupleComponent arity index
          | arity < 0 ->
              Left (InvalidShapePath node "tuple arity is negative")
          | index < 0 ->
              Left (InvalidShapePath node
                      "tuple component index is negative")
          | index >= arity ->
              Left (InvalidShapePath node
                      "tuple component index exceeds tuple arity")
          | otherwise ->
              Right ()

validateFormerArity
  :: (TypeFormer -> Int -> ShapeSolveError)
  -> TypeFormer
  -> Int
  -> Either ShapeSolveError ()
validateFormerArity makeError former actualArity
  | actualArity == typeFormerArity former =
      Right ()
  | otherwise =
      Left (makeError former actualArity)
