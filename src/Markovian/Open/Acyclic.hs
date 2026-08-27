{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

{- | A validated boundary-functional acyclic fragment of finite open systems.

Every apex vertex has exactly one producer: one input-boundary occurrence or
one hyperedge output occurrence. Hyperedges are then required to form a DAG.
The constructor is opaque; raw 'OpenSystem' values must be validated before
interpretation.
-}
module Markovian.Open.Acyclic (
    AcyclicOpenSystem,
    AcyclicOpenSystemError (..),
    AcyclicOpenCompositionError (..),
    acyclicOpenSystem,
    forgetAcyclicOpenSystem,
    acyclicTopologicalOrder,
    identityAcyclicOpenSystem,
    composeAcyclicOpenSystem,
    tensorAcyclicOpenSystem,
) where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Void (Void)
import Markovian.Category.Finite.Set
import Markovian.Category.Finite.Set.Internal (FiniteSet (UnsafeFiniteSet))
import Markovian.Open.Hypergraph
import Markovian.Open.Interface
import Markovian.Open.Pushout (PushoutPoint)
import Markovian.Open.StructuredCospan

-- | Opaque evidence that an open system has unique production and no feedback.
type role AcyclicOpenSystem nominal nominal nominal nominal nominal nominal

data AcyclicOpenSystem sort input output vertex edge label where
    UnsafeAcyclicOpenSystem ::
        (Eq vertex, Eq edge) =>
        !(OpenSystem sort input output vertex edge label) ->
        !(FiniteSet edge) ->
        AcyclicOpenSystem sort input output vertex edge label

-- | Deterministic refinement failure. All indices use represented layout order.
data AcyclicOpenSystemError
    = AcyclicInputLegNotInjective !Int !(NonEmpty Int)
    | AcyclicUnproducedVertex !Int
    | AcyclicBoundaryAndEdgeProducer !Int !Int !(NonEmpty (Int, Int))
    | AcyclicMultipleEdgeProducers !Int !(NonEmpty (Int, Int))
    | AcyclicRepeatedEdgeOutput !Int !Int !(NonEmpty Int)
    | AcyclicDirectedCycle !(NonEmpty Int)
    | AcyclicValidationInternalInvariantFailure
    deriving (Eq, Show)

-- | Structured-cospan composition or post-composition refinement failure.
data AcyclicOpenCompositionError
    = AcyclicCompositionTopologyError !OpenSystemError
    | AcyclicCompositionInvariantError !AcyclicOpenSystemError
    deriving (Eq, Show)

-- | Validate unique production, then compute a stable topological schedule.
acyclicOpenSystem ::
    (Eq vertex, Eq edge) =>
    OpenSystem sort input output vertex edge label ->
    Either AcyclicOpenSystemError (AcyclicOpenSystem sort input output vertex edge label)
acyclicOpenSystem system = do
    validateProducers system
    edgeIndices <- stableTopologicalIndices system
    edgeOrder <- traverse (edgeAt system) edgeIndices
    order <-
        case finiteSet edgeOrder of
            Left _ -> Left AcyclicValidationInternalInvariantFailure
            Right represented -> Right represented
    Right (UnsafeAcyclicOpenSystem system order)

-- | Forget the acyclic refinement without changing the represented topology.
forgetAcyclicOpenSystem ::
    AcyclicOpenSystem sort input output vertex edge label ->
    OpenSystem sort input output vertex edge label
forgetAcyclicOpenSystem (UnsafeAcyclicOpenSystem system _) = system

-- | Stable Kahn order, represented as edge identities.
acyclicTopologicalOrder ::
    AcyclicOpenSystem sort input output vertex edge label ->
    FiniteSet edge
acyclicTopologicalOrder (UnsafeAcyclicOpenSystem _ order) = order

-- | The discrete identity is boundary-functional and deterministic.
identityAcyclicOpenSystem ::
    (Eq sort, Eq port, Eq label) =>
    Interface sort port ->
    AcyclicOpenSystem sort port port port Void label
identityAcyclicOpenSystem object =
    UnsafeAcyclicOpenSystem (identityOpenSystem object) (UnsafeFiniteSet [])

-- | Compose by pushout and re-establish the acyclic refinement.
composeAcyclicOpenSystem ::
    (Eq sort, Eq middle, Eq label) =>
    AcyclicOpenSystem sort input middle leftVertex leftEdge label ->
    AcyclicOpenSystem sort middle output rightVertex rightEdge label ->
    Either
        AcyclicOpenCompositionError
        ( AcyclicOpenSystem
            sort
            input
            output
            (PushoutPoint leftVertex rightVertex)
            (Either leftEdge rightEdge)
            label
        )
composeAcyclicOpenSystem
    (UnsafeAcyclicOpenSystem left _)
    (UnsafeAcyclicOpenSystem right _) = do
        composite <- first AcyclicCompositionTopologyError (composeOpenSystem left right)
        first AcyclicCompositionInvariantError (acyclicOpenSystem composite)

-- | Disjoint union preserves unique production and acyclicity.
tensorAcyclicOpenSystem ::
    AcyclicOpenSystem sort leftInput leftOutput leftVertex leftEdge label ->
    AcyclicOpenSystem sort rightInput rightOutput rightVertex rightEdge label ->
    AcyclicOpenSystem
        sort
        (Either leftInput rightInput)
        (Either leftOutput rightOutput)
        (Either leftVertex rightVertex)
        (Either leftEdge rightEdge)
        label
tensorAcyclicOpenSystem
    (UnsafeAcyclicOpenSystem left leftOrder)
    (UnsafeAcyclicOpenSystem right rightOrder) =
        UnsafeAcyclicOpenSystem
            (tensorOpenSystem left right)
            combinedOrder
      where
        combinedOrder =
            UnsafeFiniteSet
                (map Left (finiteSetValues leftOrder) ++ map Right (finiteSetValues rightOrder))

validateProducers ::
    (Eq vertex) =>
    OpenSystem sort input output vertex edge label ->
    Either AcyclicOpenSystemError ()
validateProducers system = go 0 (interfaceEntries apexVertices)
  where
    apexVertices = hypergraphVertices (openApex system)
    inputEntries = interfaceMapEntries (openInputLeg system)
    edges = hypergraphEdges (openApex system)

    go _ [] = Right ()
    go vertexIndex ((vertex, _) : remaining) = do
        validateVertex vertexIndex vertex
        go (vertexIndex + 1) remaining

    validateVertex vertexIndex vertex =
        case inputOccurrences vertex inputEntries of
            firstInput : secondInput : otherInputs ->
                Left
                    ( AcyclicInputLegNotInjective
                        vertexIndex
                        (firstInput :| (secondInput : otherInputs))
                    )
            [inputIndex] ->
                case outputOccurrences vertex edges of
                    [] -> Right ()
                    firstOutput : otherOutputs ->
                        Left
                            ( AcyclicBoundaryAndEdgeProducer
                                vertexIndex
                                inputIndex
                                (firstOutput :| otherOutputs)
                            )
            [] -> validateEdgeProduction vertexIndex vertex (outputOccurrences vertex edges)

    validateEdgeProduction vertexIndex _ occurrences =
        case firstRepeatedOutput occurrences of
            Just (edgeIndex, firstPosition, otherPositions) ->
                Left
                    ( AcyclicRepeatedEdgeOutput
                        edgeIndex
                        vertexIndex
                        (firstPosition :| otherPositions)
                    )
            Nothing ->
                case occurrences of
                    [] -> Left (AcyclicUnproducedVertex vertexIndex)
                    [_] -> Right ()
                    firstOutput : otherOutputs ->
                        Left
                            ( AcyclicMultipleEdgeProducers
                                vertexIndex
                                (firstOutput :| otherOutputs)
                            )

inputOccurrences :: (Eq vertex) => vertex -> [(input, vertex)] -> [Int]
inputOccurrences requested entries =
    [ index
    | (index, (_, vertex)) <- zip [0 ..] entries
    , vertex == requested
    ]

outputOccurrences ::
    (Eq vertex) =>
    vertex ->
    [(edge, TypedHyperedge sort label vertex)] ->
    [(Int, Int)]
outputOccurrences requested edges =
    [ (edgeIndex, outputIndex)
    | (edgeIndex, (_, edge)) <- zip [0 ..] edges
    , (outputIndex, (vertex, _)) <- zip [0 ..] (hyperedgeOutputs edge)
    , vertex == requested
    ]

firstRepeatedOutput :: [(Int, Int)] -> Maybe (Int, Int, [Int])
firstRepeatedOutput [] = Nothing
firstRepeatedOutput ((edgeIndex, position) : remaining) =
    case [otherPosition | (otherEdge, otherPosition) <- remaining, otherEdge == edgeIndex] of
        [] -> firstRepeatedOutput remaining
        otherPositions -> Just (edgeIndex, position, otherPositions)

stableTopologicalIndices ::
    (Eq vertex) =>
    OpenSystem sort input output vertex edge label ->
    Either AcyclicOpenSystemError [Int]
stableTopologicalIndices system = kahn [] [0 .. length edges - 1]
  where
    edges = hypergraphEdges (openApex system)

    kahn completed [] = Right completed
    kahn completed remaining =
        case firstReady completed remaining of
            Just ready -> kahn (completed ++ [ready]) (filter (/= ready) remaining)
            Nothing ->
                case actualCycle remaining of
                    Just cycleIndices -> Left (AcyclicDirectedCycle cycleIndices)
                    Nothing -> Left AcyclicValidationInternalInvariantFailure

    firstReady _ [] = Nothing
    firstReady completed (candidate : remaining)
        | all (`elem` completed) (predecessors candidate) = Just candidate
        | otherwise = firstReady completed remaining

    predecessors candidate =
        distinct
            [ producerIndex
            | inputVertex <- edgeInputVertices candidate
            , Just producerIndex <- [producerOf inputVertex]
            ]

    producerOf vertex =
        case [ edgeIndex
             | (edgeIndex, (_, edge)) <- zip [0 ..] edges
             , any ((== vertex) . fst) (hyperedgeOutputs edge)
             ] of
            [] -> Nothing
            producerIndex : _ -> Just producerIndex

    edgeInputVertices requested =
        case lookup requested (zip [0 ..] (map snd edges)) of
            Nothing -> []
            Just edge -> map fst (hyperedgeInputs edge)

    edgeOutputVertices requested =
        case lookup requested (zip [0 ..] (map snd edges)) of
            Nothing -> []
            Just edge -> map fst (hyperedgeOutputs edge)

    successors requested =
        distinct
            [ edgeIndex
            | (edgeIndex, (_, edge)) <- zip [0 ..] edges
            , (inputVertex, _) <- hyperedgeInputs edge
            , inputVertex `elem` edgeOutputVertices requested
            ]

    actualCycle remaining = searchStarts remaining
      where
        searchStarts [] = Nothing
        searchStarts (start : starts) =
            case searchPath [start] start of
                Just found -> nonEmptyRotated found
                Nothing -> searchStarts starts

        searchPath path current = searchSuccessors path (successors current)

        searchSuccessors _ [] = Nothing
        searchSuccessors path (successor : otherSuccessors)
            | successor `notElem` remaining = searchSuccessors path otherSuccessors
            | successor `elem` path = Just (dropUntil successor path)
            | otherwise =
                case searchPath (path ++ [successor]) successor of
                    Just found -> Just found
                    Nothing -> searchSuccessors path otherSuccessors

nonEmptyRotated :: [Int] -> Maybe (NonEmpty Int)
nonEmptyRotated [] = Nothing
nonEmptyRotated values@(firstValue : remaining) =
    case rotateAt (minimumFrom firstValue remaining) values of
        [] -> Nothing
        rotatedFirst : rotatedRest -> Just (rotatedFirst :| rotatedRest)

minimumFrom :: Int -> [Int] -> Int
minimumFrom = foldl min

rotateAt :: (Eq value) => value -> [value] -> [value]
rotateAt requested = seek []
  where
    seek _ [] = []
    seek prefix (value : remaining)
        | value == requested = value : remaining ++ reverse prefix
        | otherwise = seek (value : prefix) remaining

dropUntil :: (Eq value) => value -> [value] -> [value]
dropUntil _ [] = []
dropUntil requested values@(value : remaining)
    | requested == value = values
    | otherwise = dropUntil requested remaining

distinct :: (Eq value) => [value] -> [value]
distinct = go []
  where
    go _ [] = []
    go seen (value : remaining)
        | value `elem` seen = go seen remaining
        | otherwise = value : go (seen ++ [value]) remaining

edgeAt ::
    OpenSystem sort input output vertex edge label ->
    Int ->
    Either AcyclicOpenSystemError edge
edgeAt system requested =
    case lookup requested (zip [0 ..] (finiteSetValues (hypergraphEdgeIds (openApex system)))) of
        Nothing -> Left AcyclicValidationInternalInvariantFailure
        Just edge -> Right edge
