{- |
Module      : Language.Egison.Type.Capability
Licence     : MIT

Pure evidence operations used while inferring the structural capability of a
matcher literal.

This module deliberately does not use ordinary type unification.  A caller
must fresh-instantiate a constructor signature before projection and apply any
already-justified capability substitution before constructing the child
evidence.  Projection then uses the identity of the fresh ordinary type
variables only to route that evidence into the constructor result slots.
-}

module Language.Egison.Type.Capability
  ( CapEvidence(..)
  , ObservabilityLookup
  , ObservabilityMasks
  , computeObservabilityMasks
  , observabilityLookupFromMasks
  , observabilityLookup
  , evidenceFromCapability
  , mergeCapEvidence
  , mergeCapEvidences
  , projectionRelevantVariables
  , validateFieldEvidence
  , projectConstructorEvidence
  , finalizeCapEvidence
  , capTargetOK
  ) where

import           Control.Monad              (foldM, zipWithM)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set

import           Language.Egison.Type.Env   (PatternTypeEnv, patternEnvToList)
import           Language.Egison.Type.Types (Capability (..), TyVar,
                                              Type (..), TypeFormer (..),
                                              TypeFormerId (..),
                                              TypeScheme (..), freeTyVars,
                                              typeFormerOf)

-- | Partial structural-capability evidence.
--
-- 'CapKnown' is intended for capability leaves ('CapNone', 'CapVar', and
-- 'CapSkolem').  'evidenceFromCapability' and the public operations also
-- accept a structured capability in 'CapKnown' and expand it to the canonical
-- evidence-tree representation.
data CapEvidence
  = CapUnseen
  | CapKnown Capability
  | CapConEvidence TypeFormer [CapEvidence]
  | CapTupleEvidence [CapEvidence]
  deriving (Eq, Show)

-- | Lookup for a former's parameter-observability mask.
--
-- @Nothing@ means that the former is an opaque barrier.  @Just mask@ means
-- that the former is capability-visible, with one Boolean per canonical
-- parameter position.  Callers compute these masks from the frozen pattern
-- signature environment; neither target annotations nor the selected matcher
-- clauses belong in this lookup.
type ObservabilityLookup = TypeFormer -> Maybe [Bool]

-- | Frozen parameter-observability masks, keyed by canonical result former.
--
-- A former is absent precisely when it is opaque: no non-CAS pattern
-- constructor signature in the supplied environment has that canonical
-- result former.
type ObservabilityMasks = Map TypeFormer [Bool]

-- | Compute parameter observability from all pattern-constructor signatures.
--
-- The input must be the frozen pattern-constructor environment, not the
-- pattern-function environment.  Constructor names and selected matcher
-- clauses are irrelevant; only each signature's fields and result type are
-- inspected.
--
-- The iteration starts with every declared parameter unobservable.  A direct
-- result-variable occurrence in a field is a seed, products expose every
-- component, and an application exposes only positions enabled by the
-- current mask of its canonical former.  Consequently recursive occurrences
-- only propagate evidence already present in the current approximation.
-- Iteration to a fixed point therefore computes the least solution: a
-- recursive-only parameter remains false, while a seed in any member of a
-- mutually recursive group eventually reaches the others.
--
-- Function, effect, matcher, gradual, undeclared, and CAS-view types are
-- barriers.  CAS entries are deliberately omitted until D5-CAS supplies
-- target-indexed virtual signatures.
computeObservabilityMasks
  :: PatternTypeEnv
  -> Either String ObservabilityMasks
computeObservabilityMasks patternEnvironment = do
  signatures <-
    fmap foldMaybes
      (mapM prepareSignature (patternEnvToList patternEnvironment))
  validateFormerArities signatures
  let bottom = Map.fromList
        [ (constructorResultFormer signature,
           replicate
             (typeFormerArity (constructorResultFormer signature))
             False)
        | signature <- signatures
        ]
  return (leastFixpoint (observabilityStep signatures bottom) bottom)

-- | Turn a frozen mask table into the lookup consumed by projection and
-- finalization.  Missing formers remain opaque.
observabilityLookupFromMasks
  :: ObservabilityMasks
  -> ObservabilityLookup
observabilityLookupFromMasks masks former = Map.lookup former masks

-- | Convenience composition for callers that need only the lookup function.
observabilityLookup
  :: PatternTypeEnv
  -> Either String ObservabilityLookup
observabilityLookup =
  fmap observabilityLookupFromMasks . computeObservabilityMasks

-- | Embed a complete capability into the partial evidence domain.
evidenceFromCapability :: Capability -> CapEvidence
evidenceFromCapability CapNone         = CapKnown CapNone
evidenceFromCapability cap@(CapVar _)  = CapKnown cap
evidenceFromCapability cap@(CapSkolem _) = CapKnown cap
evidenceFromCapability (CapCon former capabilities) =
  CapConEvidence former (map evidenceFromCapability capabilities)
evidenceFromCapability (CapTuple capabilities) =
  CapTupleEvidence (map evidenceFromCapability capabilities)

-- | Exact evidence merge.
--
-- Unseen evidence is neutral.  Known leaves must be literally equal, so two
-- capability variables merge only when their 'CapVar' identities are equal.
-- Structured evidence merges componentwise only under the same canonical
-- former (including arity).  This operation never unifies variables and never
-- weakens structured evidence to 'CapNone'.
mergeCapEvidence :: CapEvidence -> CapEvidence -> Either String CapEvidence
mergeCapEvidence evidence1 evidence2 =
  merge (canonicalEvidence evidence1) (canonicalEvidence evidence2)
  where
    merge CapUnseen evidence = Right evidence
    merge evidence CapUnseen = Right evidence
    merge (CapKnown capability1) (CapKnown capability2)
      | capability1 == capability2 = Right (CapKnown capability1)
      | otherwise =
          Left ("capability evidence mismatch: "
                ++ show capability1 ++ " versus " ++ show capability2)
    merge (CapConEvidence former1 children1)
          (CapConEvidence former2 children2)
      | former1 /= former2 =
          Left ("capability former mismatch: "
                ++ describeFormer former1 ++ " versus "
                ++ describeFormer former2)
      | length children1 /= typeFormerArity former1 =
          Left ("malformed evidence for " ++ describeFormer former1
                ++ ": expected " ++ show (typeFormerArity former1)
                ++ " children but got " ++ show (length children1))
      | length children2 /= typeFormerArity former2 =
          Left ("malformed evidence for " ++ describeFormer former2
                ++ ": expected " ++ show (typeFormerArity former2)
                ++ " children but got " ++ show (length children2))
      | length children1 /= length children2 =
          Left ("malformed evidence for " ++ describeFormer former1
                ++ ": child counts " ++ show (length children1)
                ++ " and " ++ show (length children2) ++ " differ")
      | otherwise =
          CapConEvidence former1
            <$> zipWithIndex2M
                  (\index child1 child2 ->
                    prefixError ("parameter " ++ show index ++ ": ")
                      (merge child1 child2))
                  children1
                  children2
    merge (CapTupleEvidence components1) (CapTupleEvidence components2)
      | length components1 /= length components2 =
          Left ("tuple capability evidence arity mismatch: "
                ++ show (length components1) ++ " versus "
                ++ show (length components2))
      | otherwise =
          CapTupleEvidence
            <$> zipWithIndex2M
                  (\index component1 component2 ->
                    prefixError ("tuple component " ++ show index ++ ": ")
                      (merge component1 component2))
                  components1
                  components2
    merge left right =
      Left ("capability evidence shape mismatch: "
            ++ describeEvidence left ++ " versus " ++ describeEvidence right)

-- | Merge a collection of evidence trees.  The empty collection has no
-- evidence.
mergeCapEvidences :: [CapEvidence] -> Either String CapEvidence
mergeCapEvidences = foldM mergeCapEvidence CapUnseen

-- | Result-signature variables that are reachable through capability-visible
-- positions of a constructor field.  Inference uses this query before
-- projection to turn a flexible input capability into the required
-- constructor/tuple head by an ordinary capability unification.  Keeping the
-- reachability test here ensures that opaque and unobservable branches never
-- create such constraints.
projectionRelevantVariables
  :: ObservabilityLookup
  -> Set TyVar
  -> Type
  -> Either String (Set TyVar)
projectionRelevantVariables = reachableVariables

-- | Project child evidence through a fresh constructor signature.
--
-- The type list is the constructor's field types, the following 'Type' is its
-- result type, and the evidence list is in the same order as the fields.
-- Result type variables are identified by their fresh 'TyVar' identities.
--
-- Collection walks products and capability-visible former positions only.
-- A known head or arity mismatch on a relevant path is an error.  An
-- unresolved capability variable where a structured head is required is also
-- reported to the caller; it must be solved by the ordinary capability
-- constraint machinery before projection is retried.  Opaque, function,
-- effect, matcher, gradual, and other non-visible branches do not contribute.
--
-- The returned tree includes the result's structural root.  It can contain
-- 'CapUnseen' in result slots for which no child supplied evidence; those
-- positions are resolved or rejected by 'finalizeCapEvidence'.
projectConstructorEvidence
  :: ObservabilityLookup
  -> [Type]
  -> Type
  -> [CapEvidence]
  -> Either String CapEvidence
projectConstructorEvidence observability fieldTypes resultType childEvidence
  | length fieldTypes /= length childEvidence =
      Left ("constructor signature/evidence arity mismatch: "
            ++ show (length fieldTypes) ++ " fields but "
            ++ show (length childEvidence) ++ " evidence components")
  | otherwise = do
      resultVariables <-
        reachableVariables observability (freeTyVars resultType) resultType
      assignments <- foldM
        (collectField resultVariables)
        Map.empty
        (zip3 [1 :: Int ..] fieldTypes childEvidence)
      buildResultRoot observability resultVariables assignments resultType
  where
    collectField resultVariables assignments (index, fieldType, evidence) = do
      fieldAssignments <-
        prefixError ("constructor field " ++ show index ++ ": ")
          (collectAssignments observability resultVariables
                              fieldType evidence)
      prefixError ("constructor field " ++ show index ++ ": ")
        (mergeAssignments assignments fieldAssignments)

-- | Finalize an evidence tree to a capability.
--
-- Every unseen observable position is rejected.  Every unobservable former
-- parameter is canonicalized to 'CapNone', even if malformed input supplied
-- stronger evidence there.  Products expose all of their components.
-- 'CapUnseen' at the root is an error: a caller that observed no structured
-- root evidence must choose the catch-all capability 'CapNone' before calling
-- this function.
finalizeCapEvidence
  :: ObservabilityLookup
  -> CapEvidence
  -> Either String Capability
finalizeCapEvidence observability =
  finalizeAt "capability root" . canonicalEvidence
  where
    finalizeAt path CapUnseen =
      Left (path ++ " has no evidence in an observable position")
    finalizeAt _ (CapKnown capability) =
      Right capability
    finalizeAt path (CapTupleEvidence components) =
      CapTuple <$> zipWithIndexM
        (\index component ->
          finalizeAt (path ++ ", tuple component " ++ show index) component)
        components
    finalizeAt path (CapConEvidence former children) = do
      mask <- requireVisibleMask path observability former (length children)
      finalized <- zipWithIndexM
        (\index (isObservable, child) ->
          if isObservable
            then finalizeAt
                   (path ++ ", " ++ describeFormer former
                         ++ " parameter " ++ show index)
                   child
            else Right CapNone)
        (zip mask children)
      Right (CapCon former finalized)

-- | Check the value-level correspondence between a matcher capability and
-- its ordinary target.  This is deliberately not a type-formation rule:
-- open matcher combinators may use correspondence assumptions obtained from
-- matcher/slot values that they actually receive.
--
-- The comparison is purely syntactic after the dedicated canonical
-- 'typeFormerOf' boundary.  It never calls ordinary unification, CAS ground
-- equivalence, subtype joins, reshaping, or tensor normalization.
capTargetOK :: [(Capability, Type)] -> Capability -> Type -> Bool
capTargetOK assumptions = go Set.empty
  where
    go seen capability target
      | (capability, target) `elem` assumptions =
          True
      | (capability, target) `Set.member` seen =
          True
      | otherwise =
          let seen' = Set.insert (capability, target) seen
          in case capability of
            CapNone ->
              True
            CapVar _ ->
              False
            CapSkolem _ ->
              False
            CapTuple components ->
              case target of
                TTuple targets
                  | length components == length targets ->
                      and (zipWith (go seen') components targets)
                _ ->
                  False
            CapCon capabilityFormer capabilities ->
              case typeFormerOf target of
                Just (targetFormer, targets)
                  | capabilityFormer == targetFormer
                  , length capabilities == length targets ->
                      and (zipWith (go seen') capabilities targets)
                _ ->
                  False

-- Observability least fixpoint -----------------------------------------------

data ConstructorSignature = ConstructorSignature
  { constructorName          :: String
  , constructorFieldTypes    :: [Type]
  , constructorResultFormer  :: TypeFormer
  , constructorResultArgs    :: [Type]
  }

prepareSignature
  :: (String, TypeScheme)
  -> Either String (Maybe ConstructorSignature)
prepareSignature (name, Forall _ _ _ signatureType) =
  let (fieldTypes, resultType) = splitFunctionType signatureType
  in if isCasViewBarrier resultType
       then Right Nothing
       else case resultType of
         TTuple _ ->
           Left ("pattern constructor " ++ name
                 ++ " has a product result rather than a canonical former: "
                 ++ show resultType)
         TVar _ ->
           Left ("pattern constructor " ++ name
                 ++ " has a variable result without a canonical former: "
                 ++ show resultType)
         _ ->
           case typeFormerOf resultType of
             Nothing ->
               Left ("pattern constructor " ++ name
                     ++ " has an opaque result type: " ++ show resultType)
             Just (former, arguments)
               | length arguments /= typeFormerArity former ->
                   Left ("pattern constructor " ++ name ++ " returns "
                         ++ describeFormer former ++ " with "
                         ++ show (length arguments) ++ " arguments")
               | otherwise ->
                   Right (Just ConstructorSignature
                     { constructorName = name
                     , constructorFieldTypes = fieldTypes
                     , constructorResultFormer = former
                     , constructorResultArgs = arguments
                     })

splitFunctionType :: Type -> ([Type], Type)
splitFunctionType (TFun argument result) =
  let (arguments, finalResult) = splitFunctionType result
  in (argument : arguments, finalResult)
splitFunctionType result = ([], result)

validateFormerArities
  :: [ConstructorSignature]
  -> Either String ()
validateFormerArities =
  fmap (const ()) . foldM insertFormer Map.empty
  where
    insertFormer arities signature =
      let former = constructorResultFormer signature
          formerId = typeFormerId former
          arity = typeFormerArity former
      in case Map.lookup formerId arities of
           Nothing ->
             Right (Map.insert formerId arity arities)
           Just previousArity
             | previousArity == arity ->
                 Right arities
             | otherwise ->
                 Left ("pattern constructor " ++ constructorName signature
                       ++ " uses canonical former " ++ show formerId
                       ++ " at arity " ++ show arity
                       ++ ", but another constructor uses arity "
                       ++ show previousArity)

observabilityStep
  :: [ConstructorSignature]
  -> ObservabilityMasks
  -> ObservabilityMasks
  -> ObservabilityMasks
observabilityStep signatures bottom current =
  foldl addSignature bottom signatures
  where
    addSignature masks signature =
      let fieldVariables =
            Set.unions
              (map (visibleVariables current)
                   (constructorFieldTypes signature))
          contribution =
            map
              (not . Set.null
                   . Set.intersection fieldVariables
                   . visibleVariables current)
              (constructorResultArgs signature)
      in Map.insertWith
           (zipWith (||))
           (constructorResultFormer signature)
           contribution
           masks

visibleVariables
  :: ObservabilityMasks
  -> Type
  -> Set TyVar
visibleVariables masks = go
  where
    go (TVar variable) =
      Set.singleton variable
    go (TTuple types) =
      Set.unions (map go types)
    go type'
      | isObservabilityBarrier type' =
          Set.empty
      | otherwise =
          case typeFormerOf type' of
            Nothing ->
              Set.empty
            Just (former, arguments) ->
              case Map.lookup former masks of
                Nothing ->
                  Set.empty
                Just mask ->
                  Set.unions
                    [ go argument
                    | (True, argument) <- zip mask arguments
                    ]

-- D5-CAS requires target-indexed virtual signatures.  Until those exist,
-- neither a CAS result declaration nor a nested CAS view may make a parameter
-- observable.
isCasViewBarrier :: Type -> Bool
isCasViewBarrier TMathValue = True
isCasViewBarrier TPolyExpr = True
isCasViewBarrier TTermExpr = True
isCasViewBarrier TSymbolExpr = True
isCasViewBarrier TIndexExpr = True
isCasViewBarrier TFactor = True
isCasViewBarrier TTerm {} = True
isCasViewBarrier TFrac {} = True
isCasViewBarrier TPoly {} = True
isCasViewBarrier (TInductive name _) =
  name `elem`
    [ "MathValue", "PolyExpr", "TermExpr", "SymbolExpr", "IndexExpr"
    , "Factor", "Term", "Frac", "Poly"
    ]
isCasViewBarrier _ = False

isObservabilityBarrier :: Type -> Bool
isObservabilityBarrier type'
  | isCasViewBarrier type' = True
isObservabilityBarrier TFun {} = True
isObservabilityBarrier TIO {} = True
isObservabilityBarrier TIORef {} = True
isObservabilityBarrier TMatcher {} = True
isObservabilityBarrier TMatcherSlot {} = True
isObservabilityBarrier TAny = True
isObservabilityBarrier _ = False

leastFixpoint :: Eq a => (a -> a) -> a -> a
leastFixpoint step value =
  let next = step value
  in if next == value then value else leastFixpoint step next

foldMaybes :: [Maybe a] -> [a]
foldMaybes = foldr (\value values -> maybe values (: values) value) []

-- Projection internals -------------------------------------------------------

type EvidenceAssignments = Map TyVar CapEvidence

collectAssignments
  :: ObservabilityLookup
  -> Set TyVar
  -> Type
  -> CapEvidence
  -> Either String EvidenceAssignments
collectAssignments observability resultVariables fieldType rawEvidence =
  let evidence = canonicalEvidence rawEvidence
  in case evidence of
       CapUnseen -> Right Map.empty
       _ -> do
         relevant <-
           reachableVariables observability resultVariables fieldType
         if Set.null relevant
           then Right Map.empty
           else collectRelevant fieldType evidence
  where
    collectRelevant (TVar variable) evidence
      | variable `Set.member` resultVariables =
          Right (Map.singleton variable evidence)
      | otherwise =
          Right Map.empty
    collectRelevant (TTuple types) (CapTupleEvidence components)
      | length types /= length components =
          Left ("tuple field/evidence arity mismatch: "
                ++ show (length types) ++ " versus "
                ++ show (length components))
      | otherwise = do
          assignments <- zipWithIndexM
            (\index (componentType, componentEvidence) ->
              prefixError ("tuple component " ++ show index ++ ": ")
                (collectAssignments observability resultVariables
                                    componentType componentEvidence))
            (zip types components)
          foldM mergeAssignments Map.empty assignments
    collectRelevant tupleType@(TTuple _) evidence =
      Left ("expected tuple evidence for field type " ++ show tupleType
            ++ ", but found " ++ describeEvidence evidence)
    collectRelevant fieldType evidence =
      case typeFormerOf fieldType of
        Nothing ->
          Left ("internal projection error: a non-visible field type "
                ++ show fieldType ++ " was marked as relevant")
        Just (former, arguments) -> do
          mask <- requireVisibleMask
                    ("field type " ++ show fieldType)
                    observability
                    former
                    (length arguments)
          children <- case evidence of
            CapConEvidence evidenceFormer evidenceChildren
              | evidenceFormer /= former ->
                  Left ("expected evidence headed by "
                        ++ describeFormer former ++ ", but found "
                        ++ describeFormer evidenceFormer)
              | length evidenceChildren /= length arguments ->
                  Left ("evidence headed by " ++ describeFormer former
                        ++ " has " ++ show (length evidenceChildren)
                        ++ " children; expected "
                        ++ show (length arguments))
              | otherwise ->
                  Right evidenceChildren
            _ ->
              Left ("expected evidence headed by "
                    ++ describeFormer former ++ ", but found "
                    ++ describeEvidence evidence)
          assignments <- zipWithIndexM
            (\index (isObservable, argument, child) ->
              if isObservable
                then prefixError
                       (describeFormer former ++ " parameter "
                        ++ show index ++ ": ")
                       (collectAssignments observability resultVariables
                                           argument child)
                else Right Map.empty)
            (zip3 mask arguments children)
          foldM mergeAssignments Map.empty assignments

-- | Validate the capability-visible skeleton of one actual constructor-field
-- producer.
--
-- This is deliberately separate from result projection.  A closed field such
-- as @[Integer]@ cannot contribute evidence to a nullary result such as @Box@,
-- but a general @box $@ clause still requires its actual next matcher to have
-- a collection head.  At an actual clause leaf, 'CapUnseen' represents a
-- wildcard/value refinement; a nested constructor may also return unseen only
-- after validating its own actual holes.  Neither carries an outstanding
-- obligation here.  Opaque leaves likewise impose no structure.  This
-- operation never turns a field target into capability evidence or solves a
-- capability variable from it.
validateFieldEvidence
  :: ObservabilityLookup
  -> Type
  -> CapEvidence
  -> Either String ()
validateFieldEvidence observability fieldType =
  validate fieldType . canonicalEvidence
  where
    validate _ CapUnseen = Right ()
    validate fieldType evidence =
      case fieldType of
        TVar _ ->
          Right ()
        TTuple componentTypes ->
          case evidence of
            CapTupleEvidence componentEvidence
              | length componentTypes /= length componentEvidence ->
                  Left ("tuple field/evidence arity mismatch: "
                        ++ show (length componentTypes) ++ " versus "
                        ++ show (length componentEvidence))
              | otherwise ->
                  fmap (const ()) $
                    zipWithIndexM
                      (\index (componentType, component) ->
                        prefixError ("tuple component " ++ show index ++ ": ")
                          (validate componentType component))
                      (zip componentTypes componentEvidence)
            _ ->
              Left ("expected tuple evidence for field type "
                    ++ show fieldType ++ ", but found "
                    ++ describeEvidence evidence)
        _ ->
          case typeFormerOf fieldType of
            Nothing ->
              Right ()
            Just (former, arguments) -> do
              maybeMask <- validatedMask
                ("field type " ++ show fieldType)
                observability
                former
                (length arguments)
              case maybeMask of
                Nothing ->
                  Right ()
                Just mask -> do
                  children <- case evidence of
                    CapConEvidence evidenceFormer evidenceChildren
                      | evidenceFormer /= former ->
                          Left ("expected evidence headed by "
                                ++ describeFormer former ++ ", but found "
                                ++ describeFormer evidenceFormer)
                      | length evidenceChildren /= length arguments ->
                          Left ("evidence headed by "
                                ++ describeFormer former ++ " has "
                                ++ show (length evidenceChildren)
                                ++ " children; expected "
                                ++ show (length arguments))
                      | otherwise ->
                          Right evidenceChildren
                    _ ->
                      Left ("expected evidence headed by "
                            ++ describeFormer former ++ ", but found "
                            ++ describeEvidence evidence)
                  fmap (const ()) $
                    zipWithIndexM
                      (\index (isObservable, argument, child) ->
                        if isObservable
                          then prefixError
                                 (describeFormer former ++ " parameter "
                                  ++ show index ++ ": ")
                                 (validate argument child)
                          else Right ())
                      (zip3 mask arguments children)

mergeAssignments
  :: EvidenceAssignments
  -> EvidenceAssignments
  -> Either String EvidenceAssignments
mergeAssignments left right =
  foldM insertAssignment left (Map.toAscList right)
  where
    insertAssignment assignments (variable, evidence) =
      case Map.lookup variable assignments of
        Nothing ->
          Right (Map.insert variable evidence assignments)
        Just previous -> do
          merged <- prefixError
            ("conflicting evidence for fresh signature variable "
             ++ show variable ++ ": ")
            (mergeCapEvidence previous evidence)
          Right (Map.insert variable merged assignments)

buildResultRoot
  :: ObservabilityLookup
  -> Set TyVar
  -> EvidenceAssignments
  -> Type
  -> Either String CapEvidence
buildResultRoot observability resultVariables assignments resultType =
  case resultType of
    TTuple componentTypes ->
      CapTupleEvidence
        <$> mapM (buildResultSlot observability resultVariables assignments)
                 componentTypes
    TVar variable ->
      Left ("constructor result has no structured capability head: "
            ++ show variable)
    _ ->
      case typeFormerOf resultType of
        Nothing ->
          Left ("constructor result type is an opaque capability barrier: "
                ++ show resultType)
        Just (former, arguments) -> do
          mask <- requireVisibleMask
                    ("constructor result " ++ show resultType)
                    observability
                    former
                    (length arguments)
          children <- zipWithM
            (\isObservable argument ->
              if isObservable
                then buildResultSlot
                       observability resultVariables assignments argument
                else Right (CapKnown CapNone))
            mask
            arguments
          Right (CapConEvidence former children)

buildResultSlot
  :: ObservabilityLookup
  -> Set TyVar
  -> EvidenceAssignments
  -> Type
  -> Either String CapEvidence
buildResultSlot observability resultVariables assignments slotType = do
  variables <- reachableVariables observability resultVariables slotType
  if Set.null variables
    then Right (CapKnown CapNone)
    else if not (any (`Map.member` assignments) (Set.toList variables))
      then Right CapUnseen
      else buildResultTemplate
             observability resultVariables assignments slotType

buildResultTemplate
  :: ObservabilityLookup
  -> Set TyVar
  -> EvidenceAssignments
  -> Type
  -> Either String CapEvidence
buildResultTemplate observability resultVariables assignments resultType = do
  variables <- reachableVariables observability resultVariables resultType
  if Set.null variables
    then Right (CapKnown CapNone)
    else case resultType of
      TVar variable
        | variable `Set.member` resultVariables ->
            Right (Map.findWithDefault CapUnseen variable assignments)
        | otherwise ->
            Right (CapKnown CapNone)
      TTuple componentTypes ->
        CapTupleEvidence
          <$> mapM (buildResultTemplate
                      observability resultVariables assignments)
                   componentTypes
      _ ->
        case typeFormerOf resultType of
          Nothing ->
            Right (CapKnown CapNone)
          Just (former, arguments) -> do
            maybeMask <- validatedMask
              ("result slot " ++ show resultType)
              observability
              former
              (length arguments)
            case maybeMask of
              Nothing ->
                Right (CapKnown CapNone)
              Just mask ->
                CapConEvidence former
                  <$> zipWithM
                        (\isObservable argument ->
                          if isObservable
                            then buildResultTemplate
                                   observability resultVariables assignments
                                   argument
                            else Right (CapKnown CapNone))
                        mask
                        arguments

reachableVariables
  :: ObservabilityLookup
  -> Set TyVar
  -> Type
  -> Either String (Set TyVar)
reachableVariables observability candidates = go
  where
    go (TVar variable)
      | variable `Set.member` candidates =
          Right (Set.singleton variable)
      | otherwise =
          Right Set.empty
    go (TTuple types) =
      Set.unions <$> mapM go types
    go type' =
      case typeFormerOf type' of
        Nothing ->
          Right Set.empty
        Just (former, arguments) -> do
          maybeMask <- validatedMask
            ("type " ++ show type')
            observability
            former
            (length arguments)
          case maybeMask of
            Nothing ->
              Right Set.empty
            Just mask ->
              Set.unions
                <$> zipWithM
                      (\isObservable argument ->
                        if isObservable then go argument else Right Set.empty)
                      mask
                      arguments

-- Shared validation and formatting ------------------------------------------

validatedMask
  :: String
  -> ObservabilityLookup
  -> TypeFormer
  -> Int
  -> Either String (Maybe [Bool])
validatedMask context observability former actualArity
  | actualArity /= typeFormerArity former =
      Left (context ++ " uses " ++ describeFormer former
            ++ " with " ++ show actualArity ++ " arguments")
  | otherwise =
      case observability former of
        Nothing ->
          Right Nothing
        Just mask
          | length mask == typeFormerArity former ->
              Right (Just mask)
          | otherwise ->
              Left (context ++ " has an invalid observability mask for "
                    ++ describeFormer former ++ ": expected "
                    ++ show (typeFormerArity former) ++ " entries but got "
                    ++ show (length mask))

requireVisibleMask
  :: String
  -> ObservabilityLookup
  -> TypeFormer
  -> Int
  -> Either String [Bool]
requireVisibleMask context observability former actualArity = do
  maybeMask <- validatedMask context observability former actualArity
  case maybeMask of
    Nothing ->
      Left (context ++ " requires the opaque former "
            ++ describeFormer former ++ " to expose capability structure")
    Just mask ->
      Right mask

canonicalEvidence :: CapEvidence -> CapEvidence
canonicalEvidence CapUnseen = CapUnseen
canonicalEvidence (CapKnown capability) =
  evidenceFromCapability capability
canonicalEvidence (CapConEvidence former children) =
  CapConEvidence former (map canonicalEvidence children)
canonicalEvidence (CapTupleEvidence components) =
  CapTupleEvidence (map canonicalEvidence components)

describeFormer :: TypeFormer -> String
describeFormer (TypeFormer (TypeFormerId name) arity) =
  name ++ "/" ++ show arity

describeEvidence :: CapEvidence -> String
describeEvidence CapUnseen = "unseen"
describeEvidence (CapKnown capability) = show capability
describeEvidence (CapConEvidence former _) =
  "evidence headed by " ++ describeFormer former
describeEvidence (CapTupleEvidence components) =
  "tuple evidence of arity " ++ show (length components)

prefixError :: String -> Either String a -> Either String a
prefixError prefix = either (Left . (prefix ++)) Right

zipWithIndexM
  :: Monad m
  => (Int -> a -> m b)
  -> [a]
  -> m [b]
zipWithIndexM function =
  sequence . zipWith function [1 :: Int ..]

zipWithIndex2M
  :: Monad m
  => (Int -> a -> b -> m c)
  -> [a]
  -> [b]
  -> m [c]
zipWithIndex2M function values1 values2 =
  sequence (zipWith3 function [1 :: Int ..] values1 values2)
