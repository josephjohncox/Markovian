{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}

module AcyclicOpenSystems (runAcyclicOpenSystemTests) where

import Data.Foldable (foldlM)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Void qualified
import Markovian.Algebra.NonNegativeRational
import Markovian.Algebra.Semiring
import Markovian.Category.Finite.Set
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Deterministic
import Markovian.Category.Matrix.Stochastic
import Markovian.Circuit
import Markovian.Circuit.Interpret.Exact
import Markovian.Open.Acyclic
import Markovian.Open.Acyclic.Circuit.Exact
import Markovian.Open.Hypergraph
import Markovian.Open.Interface
import Markovian.Open.StructuredCospan
import Markovian.Probability.Exact
import System.Timeout (timeout)

runAcyclicOpenSystemTests :: (String -> IO () -> IO ()) -> IO ()
runAcyclicOpenSystemTests run = do
    run "acyclic boundary-functional chain is accepted" testChainAccepted
    run "nullary source sink passthrough discard and disconnected components are accepted" testAcceptedShapes
    run "repeated inputs and noninjective output observation are accepted" testRepeatedReads
    run "noninjective input boundary is rejected" testInputRejected
    run "unproduced apex vertex is rejected" testUnproducedRejected
    run "input and edge cannot both produce one vertex" testBoundaryEdgeRejected
    run "two edges cannot produce one vertex" testTwoProducersRejected
    run "one edge cannot repeat an output vertex" testRepeatedOutputRejected
    run "producer rejection precedence is deterministic" testProducerRejectionPrecedence
    run "directed self-loop returns its edge witness" testSelfLoopRejected
    run "multi-edge cycle returns a stable actual cycle" testCycleRejected
    run "empty interface has one empty assignment" testEmptyAssignment
    run "assignment identity survives interface layout reordering" testAssignmentReordering
    run "duplicate sort domains and assignment errors are reported" testAssignmentErrors
    run "missing sort domain reports apex vertex and sort" testMissingDomain
    run "duplicate label signature key is rejected" testDuplicateLabel
    run "missing label and ordered signature retain edge context" testMissingLabel
    run "label source and target mismatches remain distinct" testLabelEndpointMismatch
    run "primitive exact failure retains edge context" testPrimitiveFailure
    run "run rejects outside inputs and returns exact represented rows" testRunAcyclic
    run "deterministic acyclic denotation retains one-hot evidence" testDeterministicDenotation
    run "zero arity and arbitrary finite arity supports are enumerated correctly" testArityEnumeration
    run "topological evaluation equals complete-valuation equation" testKnownCoinEquation
    run "every represented source row sums to exact one" testNormalization
    run "chain evaluation is matrix composition" testChainSemantics
    run "live-frontier evaluation handles a twelve-edge chain" testLongChainFrontier
    run "one stochastic producer copied twice has diagonal support" testSharing
    run "two stochastic edge occurrences have product mass" testIndependent
    run "sharing and repeated execution are unequal" testSharingDifference
    run "discarded stochastic values marginalize to unit mass" testDiscard
    run "partially discarded edge outputs are marginalized" testPartialOutputDiscard
    run "diamond evaluation has conditional-product form" testDiamond
    run "inconsistent duplicated output observation has zero mass" testInconsistentObservation
    run "ready-edge schedules are stochastically equivalent" testScheduleIndependence
    run "vertex and edge renaming preserves denotation" testRenaming
    run "boundary permutation preserves named-assignment denotation" testBoundaryPermutation
    run "empty boundaries denote the singleton unit channel" testEmptyBoundaryUnit
    run "vacuous empty-source layouts preserve matrix laws" testVacuous
    run "pushout composition preserves acyclic unique production" testCompositionTopology
    run "mismatched composition boundaries retain topology error" testCompositionBoundaryMismatch
    run "acyclic composition equals composeStochastic" testCompositionSemantics
    run "composition through duplicated output preserves sharing" testCompositionSharing
    run "identity acyclic system denotes identityStochastic" testIdentity
    run "disjoint-union tensor denotes tensorStochastic" testTensor
    run "associations are equivalent after pushout reindexing" testAssociations
    run "left and right units are observationally equivalent" testUnits

-- Test syntax uses one runtime carrier, as required by the S6 fragment.
data Sort = Bit | EmptySort
    deriving (Eq, Show)

data Label = Coin | BiasedCoin | PairCoin | Negate | Asymmetric | PairRead | Sink | Fail
    deriving (Eq, Show)

data Primitive purity source target where
    PrimitiveStochastic ::
        StochasticMatrix NonNegativeRational source target ->
        Primitive 'Stochastic source target
    PrimitiveFailure :: Primitive 'Stochastic source target

primitiveInterpreter :: ExactPrimitiveInterpreter Primitive String
primitiveInterpreter =
    ExactPrimitiveInterpreter
        { interpretDeterministicPrimitive = \_ _ primitive -> case primitive of {}
        , interpretStochasticPrimitive = \_ _ primitive ->
            case primitive of
                PrimitiveStochastic arrow -> Right arrow
                PrimitiveFailure -> Left "fixture primitive failure"
        }

assertA :: String -> Bool -> IO ()
assertA message condition = if condition then pure () else ioError (userError message)

assertDeterministicA :: SPurity purity -> IO ()
assertDeterministicA SDeterministic = pure ()
assertDeterministicA SStochastic = ioError (userError "network acquired stochastic purity")

rightA :: (Show error) => String -> Either error value -> IO value
rightA _ (Right value) = pure value
rightA label (Left problem) = ioError (userError (label ++ ": " ++ show problem))

setA :: (Eq value, Show value) => [value] -> IO (FiniteSet value)
setA = rightA "finite set" . finiteSet

interfaceA :: [(String, Sort)] -> IO (Interface Sort String)
interfaceA = rightA "interface" . interface

systemA ::
    [(String, Sort)] ->
    [(String, Sort)] ->
    [(String, Sort)] ->
    [(String, Label, [(String, Sort)], [(String, Sort)])] ->
    [(String, String)] ->
    [(String, String)] ->
    IO (OpenSystem Sort String String String String Label)
systemA inputEntries outputEntries vertexEntries edgeEntries inputMapEntries outputMapEntries = do
    input <- interfaceA inputEntries
    output <- interfaceA outputEntries
    vertices <- interfaceA vertexEntries
    graph <- rightA "hypergraph" (typedHypergraph vertices edgeEntries)
    inputLeg <- rightA "input leg" (interfaceMap input vertices inputMapEntries)
    outputLeg <- rightA "output leg" (interfaceMap output vertices outputMapEntries)
    rightA "open system" (openSystem input output graph inputLeg outputLeg)

refineA :: OpenSystem Sort String String String String Label -> IO (AcyclicOpenSystem Sort String String String String Label)
refineA = rightA "acyclic refinement" . acyclicOpenSystem

chainA :: String -> String -> String -> IO (OpenSystem Sort String String String String Label)
chainA prefix inputPort outputPort =
    systemA
        [(inputPort, Bit)]
        [(outputPort, Bit)]
        [(prefix ++ "0", Bit), (prefix ++ "1", Bit), (prefix ++ "2", Bit)]
        [ (prefix ++ "e0", Negate, [(prefix ++ "0", Bit)], [(prefix ++ "1", Bit)])
        , (prefix ++ "e1", Negate, [(prefix ++ "1", Bit)], [(prefix ++ "2", Bit)])
        ]
        [(inputPort, prefix ++ "0")]
        [(outputPort, prefix ++ "2")]

longChainA :: Int -> IO (OpenSystem Sort String String String String Label)
longChainA edgeCount =
    systemA
        [("x", Bit)]
        [("y", Bit)]
        [(vertexName index, Bit) | index <- [0 .. edgeCount]]
        [ ( "edge-" ++ show index
          , Negate
          , [(vertexName index, Bit)]
          , [(vertexName (index + 1), Bit)]
          )
        | index <- [0 .. edgeCount - 1]
        ]
        [("x", vertexName 0)]
        [("y", vertexName edgeCount)]
  where
    vertexName :: Int -> String
    vertexName index = "vertex-" ++ show index

sourceSystem ::
    Label ->
    String ->
    IO (OpenSystem Sort String String String String Label)
sourceSystem sourceLabel outputPort =
    systemA
        []
        [(outputPort, Bit)]
        [("value", Bit)]
        [("source", sourceLabel, [], [("value", Bit)])]
        []
        [(outputPort, "value")]

coinSystem :: Bool -> IO (OpenSystem Sort String String String String Label)
coinSystem duplicatedOutput =
    if duplicatedOutput
        then
            systemA
                []
                [("left", Bit), ("right", Bit)]
                [("value", Bit)]
                [("coin", Coin, [], [("value", Bit)])]
                []
                [("left", "value"), ("right", "value")]
        else sourceSystem Coin "out"

gateSystem ::
    String ->
    Label ->
    String ->
    String ->
    IO (OpenSystem Sort String String String String Label)
gateSystem prefix gateLabel inputPort outputPort =
    systemA
        [(inputPort, Bit)]
        [(outputPort, Bit)]
        [(prefix ++ "-in", Bit), (prefix ++ "-out", Bit)]
        [(prefix ++ "-edge", gateLabel, [(prefix ++ "-in", Bit)], [(prefix ++ "-out", Bit)])]
        [(inputPort, prefix ++ "-in")]
        [(outputPort, prefix ++ "-out")]

independentSystem :: IO (OpenSystem Sort String String String String Label)
independentSystem =
    systemA
        []
        [("left", Bit), ("right", Bit)]
        [("a", Bit), ("b", Bit)]
        [("coin-a", Coin, [], [("a", Bit)]), ("coin-b", Coin, [], [("b", Bit)])]
        []
        [("left", "a"), ("right", "b")]

scheduledSystem ::
    Bool ->
    IO (OpenSystem Sort String String String String Label)
scheduledSystem reverseReadyEdges =
    systemA
        []
        [("out", Bit)]
        [("a", Bit), ("b", Bit), ("combined", Bit)]
        ( readyEdges
            ++ [("combine", PairRead, [("a", Bit), ("b", Bit)], [("combined", Bit)])]
        )
        []
        [("out", "combined")]
  where
    fair = ("fair", Coin, [], [("a", Bit)])
    biased = ("biased", BiasedCoin, [], [("b", Bit)])
    readyEdges = if reverseReadyEdges then [biased, fair] else [fair, biased]

unitDomains :: IO (FiniteValueDomains Sort Bool)
unitDomains = do
    bit <- setA [False, True]
    empty <- setA []
    rightA "value domains" (finiteValueDomains [(Bit, bit), (EmptySort, empty)])

objectsA :: IO (AssignmentObject Sort Int Bool, AssignmentObject Sort Int Bool)
objectsA = do
    domains <- unitDomains
    unit <- rightA "unit assignment object" (portAssignmentObject domains [])
    bit <- rightA "bit assignment object" (portAssignmentObject domains [Bit])
    pure (unit, bit)

weightedCoinCircuit ::
    Rational ->
    IO (Circuit Primitive 'Stochastic (Assignment Int Bool) (Assignment Int Bool))
weightedCoinCircuit falseMass = do
    (unit, bit) <- objectsA
    raw <-
        rightA
            "weighted coin matrix rows"
            ( matrixFromRows
                (assignmentObjectValues unit)
                (assignmentObjectValues bit)
                [[nn falseMass, nn (1 - falseMass)]]
            )
    arrow <- rightA "weighted coin matrix" (stochasticMatrix raw)
    pure
        ( stochasticPrimitive
            (assignmentObjectValues unit)
            (assignmentObjectValues bit)
            (PrimitiveStochastic arrow)
        )

coinCircuit :: IO (Circuit Primitive 'Stochastic (Assignment Int Bool) (Assignment Int Bool))
coinCircuit = weightedCoinCircuit (1 % 2)

pairCoinCircuit :: IO (Circuit Primitive 'Stochastic (Assignment Int Bool) (Assignment Int Bool))
pairCoinCircuit = do
    domains <- unitDomains
    source <- rightA "pair-coin source" (portAssignmentObject domains [])
    target <- rightA "pair-coin target" (portAssignmentObject domains [Bit, Bit])
    raw <-
        rightA
            "pair-coin matrix rows"
            ( matrixFromRows
                (assignmentObjectValues source)
                (assignmentObjectValues target)
                [[nn (1 % 2), nn 0, nn 0, nn (1 % 2)]]
            )
    arrow <- rightA "pair-coin matrix" (stochasticMatrix raw)
    pure
        ( stochasticPrimitive
            (assignmentObjectValues source)
            (assignmentObjectValues target)
            (PrimitiveStochastic arrow)
        )

asymmetricCircuit :: IO (Circuit Primitive 'Stochastic (Assignment Int Bool) (Assignment Int Bool))
asymmetricCircuit = do
    (_, bit) <- objectsA
    raw <-
        rightA
            "asymmetric matrix rows"
            ( matrixFromRows
                (assignmentObjectValues bit)
                (assignmentObjectValues bit)
                [ [nn 1, nn 0]
                , [nn (1 % 3), nn (2 % 3)]
                ]
            )
    arrow <- rightA "asymmetric matrix" (stochasticMatrix raw)
    pure
        ( stochasticPrimitive
            (assignmentObjectValues bit)
            (assignmentObjectValues bit)
            (PrimitiveStochastic arrow)
        )

pairReadCircuit :: IO (Circuit Primitive 'Deterministic (Assignment Int Bool) (Assignment Int Bool))
pairReadCircuit = do
    domains <- unitDomains
    source <- rightA "pair-read source" (portAssignmentObject domains [Bit, Bit])
    target <- rightA "pair-read target" (portAssignmentObject domains [Bit])
    entries <-
        traverse
            ( \(leftValue, rightValue) -> do
                input <- assignmentFor source [(0, leftValue), (1, rightValue)]
                output <- assignmentFor target [(0, leftValue && not rightValue)]
                pure (input, output)
            )
            [(False, False), (False, True), (True, False), (True, True)]
    rightA
        "pair-read table"
        (deterministicTable (assignmentObjectValues source) (assignmentObjectValues target) entries)

sinkCircuit :: IO (Circuit Primitive 'Deterministic (Assignment Int Bool) (Assignment Int Bool))
sinkCircuit = do
    (unit, bit) <- objectsA
    emptyAssignment <- assignmentFor unit []
    entries <-
        traverse
            ( \value -> do
                input <- assignmentFor bit [(0, value)]
                pure (input, emptyAssignment)
            )
            [False, True]
    rightA
        "sink table"
        (deterministicTable (assignmentObjectValues bit) (assignmentObjectValues unit) entries)

notCircuit :: IO (Circuit Primitive 'Deterministic (Assignment Int Bool) (Assignment Int Bool))
notCircuit = do
    (_, bit) <- objectsA
    entries <-
        traverse
            ( \value -> do
                input <- rightA "not input" (assignment bit [(0, value)])
                output <- rightA "not output" (assignment bit [(0, not value)])
                pure (input, output)
            )
            [False, True]
    rightA "not table" (deterministicTable (assignmentObjectValues bit) (assignmentObjectValues bit) entries)

standardTable :: IO (LabelCircuitTable Primitive Sort Label Bool)
standardTable = do
    coin <- coinCircuit
    biasedCoin <- weightedCoinCircuit (1 % 4)
    pairCoin <- pairCoinCircuit
    negation <- notCircuit
    asymmetric <- asymmetricCircuit
    pairRead <- pairReadCircuit
    sink <- sinkCircuit
    rightA
        "label table"
        ( labelCircuitTable
            [ stochasticLabelCircuit Coin [] [Bit] coin
            , stochasticLabelCircuit BiasedCoin [] [Bit] biasedCoin
            , stochasticLabelCircuit PairCoin [] [Bit, Bit] pairCoin
            , deterministicLabelCircuit Negate [Bit] [Bit] negation
            , stochasticLabelCircuit Asymmetric [Bit] [Bit] asymmetric
            , deterministicLabelCircuit PairRead [Bit, Bit] [Bit] pairRead
            , deterministicLabelCircuit Sink [Bit] [] sink
            ]
        )

networkA ::
    OpenSystem Sort String String String String Label ->
    IO (SomeAcyclicOpenCircuit Primitive Sort String String String String Label Bool)
networkA system = do
    topology <- refineA system
    domains <- unitDomains
    table <- standardTable
    rightA "acyclic circuit" (acyclicOpenCircuit topology domains table)

denotationA ::
    OpenSystem Sort String String String String Label ->
    IO (StochasticMatrix NonNegativeRational (Assignment String Bool) (Assignment String Bool))
denotationA system = do
    SomeAcyclicOpenCircuit _ circuit <- networkA system
    rightA "acyclic denotation" (acyclicOpenCircuitDenotation primitiveInterpreter circuit)

data FixtureEdge
    = FixtureEdge
        [(String, Sort)]
        [(String, Sort)]
        (AssignmentObject Sort Int Bool)
        (AssignmentObject Sort Int Bool)
        (StochasticMatrix NonNegativeRational (Assignment Int Bool) (Assignment Int Bool))

completeValuationA ::
    OpenSystem Sort String String String String Label ->
    IO (StochasticMatrix NonNegativeRational (Assignment String Bool) (Assignment String Bool))
completeValuationA system = do
    domains <- unitDomains
    inputObject <- rightA "closed-form input object" (assignmentObject domains (openInput system))
    outputObject <- rightA "closed-form output object" (assignmentObject domains (openOutput system))
    apexObject <- rightA "closed-form apex object" (assignmentObject domains (hypergraphVertices (openApex system)))
    interpreted <- traverse (interpretFixtureEdge domains . snd) (hypergraphEdges (openApex system))
    rows <-
        traverse
            (rowFor inputObject outputObject apexObject interpreted)
            (finiteSetValues (assignmentObjectValues inputObject))
    raw <-
        rightA
            "closed-form matrix"
            ( matrixFromRows
                (assignmentObjectValues inputObject)
                (assignmentObjectValues outputObject)
                rows
            )
    rightA "closed-form stochastic matrix" (stochasticMatrix raw)
  where
    rowFor inputObject outputObject apexObject interpreted inputValue =
        traverse
            (entryFor inputObject outputObject apexObject interpreted inputValue)
            (finiteSetValues (assignmentObjectValues outputObject))

    entryFor inputObject outputObject apexObject interpreted inputValue outputValue =
        foldlM
            (accumulateValuation inputObject outputObject apexObject interpreted inputValue outputValue)
            zero
            (finiteSetValues (assignmentObjectValues apexObject))

    accumulateValuation inputObject outputObject apexObject interpreted inputValue outputValue total valuation = do
        valuationEntries <- rightA "closed-form apex assignment" (assignmentEntries apexObject valuation)
        inputMatches <- boundaryMatchesA inputObject inputValue (openInputLeg system) valuationEntries
        outputMatches <- boundaryMatchesA outputObject outputValue (openOutputLeg system) valuationEntries
        if inputMatches && outputMatches
            then do
                weight <- foldlM (fixtureEdgeWeight valuationEntries) one interpreted
                pure (total `plus` weight)
            else pure total

    boundaryMatchesA object boundaryValue leg valuationEntries = do
        boundaryEntries <- rightA "closed-form boundary assignment" (assignmentEntries object boundaryValue)
        pure (all (matches boundaryEntries) (interfaceMapEntries leg))
      where
        matches boundaryEntries (port, vertex) =
            case (lookup port boundaryEntries, lookup vertex valuationEntries) of
                (Just boundaryEntry, Just vertexEntry) -> boundaryEntry == vertexEntry
                _ -> False

    fixtureEdgeWeight valuationEntries total (FixtureEdge inputs outputs sourceObject targetObject arrow) = do
        sourceEntries <- traverse (coordinate valuationEntries) (zip [0 ..] inputs)
        targetEntries <- traverse (coordinate valuationEntries) (zip [0 ..] outputs)
        sourceValue <- assignmentFor sourceObject sourceEntries
        targetValue <- assignmentFor targetObject targetEntries
        case matrixEntry (forgetStochastic arrow) sourceValue targetValue of
            Nothing -> ioError (userError "closed-form local assignment left represented support")
            Just mass -> pure (total `times` mass)

    coordinate valuationEntries (position, (vertex, _)) =
        case lookup vertex valuationEntries of
            Nothing -> ioError (userError "closed-form valuation omitted a vertex")
            Just value -> pure (position, value)

interpretFixtureEdge :: FiniteValueDomains Sort Bool -> TypedHyperedge Sort Label String -> IO FixtureEdge
interpretFixtureEdge domains edge = do
    let inputs = hyperedgeInputs edge
        outputs = hyperedgeOutputs edge
    sourceObject <- rightA "closed-form local source" (portAssignmentObject domains (map snd inputs))
    targetObject <- rightA "closed-form local target" (portAssignmentObject domains (map snd outputs))
    arrow <- fixtureLabelArrow (hyperedgeLabel edge)
    pure (FixtureEdge inputs outputs sourceObject targetObject arrow)

fixtureLabelArrow ::
    Label ->
    IO (StochasticMatrix NonNegativeRational (Assignment Int Bool) (Assignment Int Bool))
fixtureLabelArrow label =
    case label of
        Coin -> coinCircuit >>= interpret
        BiasedCoin -> weightedCoinCircuit (1 % 4) >>= interpret
        PairCoin -> pairCoinCircuit >>= interpret
        Negate -> notCircuit >>= interpret
        Asymmetric -> asymmetricCircuit >>= interpret
        PairRead -> pairReadCircuit >>= interpret
        Sink -> sinkCircuit >>= interpret
        Fail -> ioError (userError "failure label has no closed-form fixture")
  where
    interpret circuit = rightA "closed-form local circuit" (interpretExactCircuit primitiveInterpreter circuit)

assignmentFor ::
    (Eq port) =>
    AssignmentObject Sort port Bool ->
    [(port, Bool)] ->
    IO (Assignment port Bool)
assignmentFor object = rightA "assignment" . assignment object

entryA ::
    StochasticMatrix NonNegativeRational source target ->
    source ->
    target ->
    Rational
entryA arrow source target =
    getNonNegativeRational
        (fromMaybe zero (matrixEntry (forgetStochastic arrow) source target))

nn :: Rational -> NonNegativeRational
nn value =
    case nonNegativeRational value of
        Right represented -> represented
        Left problem -> error (show problem)

testChainAccepted :: IO ()
testChainAccepted = chainA "v" "x" "y" >>= refineA >> pure ()

testAcceptedShapes :: IO ()
testAcceptedShapes = do
    system <-
        systemA
            [("in", Bit)]
            [("pass", Bit)]
            [("input", Bit), ("made", Bit), ("discarded", Bit)]
            [("source", Coin, [], [("made", Bit)]), ("sink", Sink, [("made", Bit)], [("discarded", Bit)])]
            [("in", "input")]
            [("pass", "input")]
    _ <- refineA system
    pure ()

testRepeatedReads :: IO ()
testRepeatedReads = do
    system <-
        systemA
            [("in", Bit)]
            [("a", Bit), ("b", Bit)]
            [("v", Bit), ("w", Bit)]
            [("read", PairRead, [("v", Bit), ("v", Bit)], [("w", Bit)])]
            [("in", "v")]
            [("a", "w"), ("b", "w")]
    _ <- refineA system
    pure ()

testInputRejected :: IO ()
testInputRejected = do
    system <- systemA [("a", Bit), ("b", Bit)] [] [("v", Bit)] [] [("a", "v"), ("b", "v")] []
    case acyclicOpenSystem system of
        Left (AcyclicInputLegNotInjective 0 ports) -> assertA "input witness changed" (NonEmpty.toList ports == [0, 1])
        _ -> ioError (userError "noninjective input accepted")

testUnproducedRejected :: IO ()
testUnproducedRejected = do
    system <- systemA [] [] [("v", Bit)] [] [] []
    case acyclicOpenSystem system of
        Left (AcyclicUnproducedVertex 0) -> pure ()
        _ -> ioError (userError "unproduced vertex accepted")

testBoundaryEdgeRejected :: IO ()
testBoundaryEdgeRejected = do
    system <- systemA [("in", Bit)] [] [("v", Bit)] [("e", Coin, [], [("v", Bit)])] [("in", "v")] []
    case acyclicOpenSystem system of
        Left (AcyclicBoundaryAndEdgeProducer 0 0 producers) -> assertA "producer witness changed" (NonEmpty.toList producers == [(0, 0)])
        _ -> ioError (userError "boundary and edge production accepted")

testTwoProducersRejected :: IO ()
testTwoProducersRejected = do
    system <- systemA [] [] [("v", Bit)] [("a", Coin, [], [("v", Bit)]), ("b", Coin, [], [("v", Bit)])] [] []
    case acyclicOpenSystem system of
        Left (AcyclicMultipleEdgeProducers 0 producers) -> assertA "multiple witness changed" (NonEmpty.toList producers == [(0, 0), (1, 0)])
        _ -> ioError (userError "multiple producers accepted")

testRepeatedOutputRejected :: IO ()
testRepeatedOutputRejected = do
    system <- systemA [] [] [("v", Bit)] [("e", Coin, [], [("v", Bit), ("v", Bit)])] [] []
    case acyclicOpenSystem system of
        Left (AcyclicRepeatedEdgeOutput 0 0 positions) -> assertA "output witness changed" (NonEmpty.toList positions == [0, 1])
        _ -> ioError (userError "repeated edge output accepted")

testProducerRejectionPrecedence :: IO ()
testProducerRejectionPrecedence = do
    repeatedInput <-
        systemA
            [("a", Bit), ("b", Bit)]
            []
            [("v", Bit)]
            [("edge", Coin, [], [("v", Bit), ("v", Bit)])]
            [("a", "v"), ("b", "v")]
            []
    case acyclicOpenSystem repeatedInput of
        Left (AcyclicInputLegNotInjective 0 _) -> pure ()
        _ -> ioError (userError "repeated input did not take producer precedence")

    boundaryAndRepeatedEdge <-
        systemA
            [("in", Bit)]
            []
            [("v", Bit)]
            [("edge", Coin, [], [("v", Bit), ("v", Bit)])]
            [("in", "v")]
            []
    case acyclicOpenSystem boundaryAndRepeatedEdge of
        Left (AcyclicBoundaryAndEdgeProducer 0 0 _) -> pure ()
        _ -> ioError (userError "boundary-and-edge production did not take precedence")

    repeatedAndMultipleEdges <-
        systemA
            []
            []
            [("v", Bit)]
            [ ("repeated", Coin, [], [("v", Bit), ("v", Bit)])
            , ("other", Coin, [], [("v", Bit)])
            ]
            []
            []
    case acyclicOpenSystem repeatedAndMultipleEdges of
        Left (AcyclicRepeatedEdgeOutput 0 0 _) -> pure ()
        _ -> ioError (userError "repeated edge output did not take producer precedence")

testSelfLoopRejected :: IO ()
testSelfLoopRejected = do
    system <- systemA [] [] [("v", Bit)] [("loop", Negate, [("v", Bit)], [("v", Bit)])] [] []
    -- Producer validation accepts the unique edge producer before cycle checking.
    case acyclicOpenSystem system of
        Left (AcyclicDirectedCycle witness) -> assertA "self-loop witness changed" (NonEmpty.toList witness == [0])
        _ -> ioError (userError "self-loop accepted")

testCycleRejected :: IO ()
testCycleRejected = do
    system <-
        systemA
            []
            []
            [("a", Bit), ("b", Bit), ("c", Bit)]
            [ ("e0", Negate, [("c", Bit)], [("a", Bit)])
            , ("e1", Negate, [("a", Bit)], [("b", Bit)])
            , ("e2", Negate, [("b", Bit)], [("c", Bit)])
            ]
            []
            []
    case acyclicOpenSystem system of
        Left (AcyclicDirectedCycle witness) -> assertA "cycle witness changed" (NonEmpty.toList witness == [0, 1, 2])
        _ -> ioError (userError "cycle accepted")

testEmptyAssignment :: IO ()
testEmptyAssignment = do
    (unit, _) <- objectsA
    assertA "empty interface did not have one assignment" (finiteSetCardinality (assignmentObjectValues unit) == 1)

testAssignmentReordering :: IO ()
testAssignmentReordering = do
    domains <- unitDomains
    leftInterface <- rightA "left positional interface" (interface [(0 :: Int, Bit), (1, Bit)])
    rightInterface <- rightA "right positional interface" (interface [(1 :: Int, Bit), (0, Bit)])
    leftObject <- rightA "left object" (assignmentObject domains leftInterface)
    rightObject <- rightA "right object" (assignmentObject domains rightInterface)
    left <- assignmentFor leftObject [(0, False), (1, True)]
    right <- assignmentFor rightObject [(1, True), (0, False)]
    assertA "assignment equality depended on layout" (left == right)
    entries <- rightA "assignment reindexing" (assignmentEntries rightObject left)
    assertA "assignment did not reindex" (entries == [(1, True), (0, False)])

testAssignmentErrors :: IO ()
testAssignmentErrors = do
    bit <- setA [False, True]
    case finiteValueDomains [(Bit, bit), (Bit, bit)] of
        Left (DuplicateValueDomainSort 1) -> pure ()
        _ -> ioError (userError "duplicate sort domain was accepted")

    restrictedBit <- setA [False]
    restrictedDomains <- rightA "restricted domains" (finiteValueDomains [(Bit, restrictedBit)])
    restrictedObject <- rightA "restricted assignment object" (portAssignmentObject restrictedDomains [Bit])
    case assignment restrictedObject [] of
        Left AssignmentDomainMismatch -> pure ()
        _ -> ioError (userError "assignment domain mismatch was not reported")
    case assignment restrictedObject [(0, True)] of
        Left (AssignmentValueOutsideDomain 0) -> pure ()
        _ -> ioError (userError "outside assignment value was accepted")

    (unitObject, bitObject) <- objectsA
    emptyAssignment <- assignmentFor unitObject []
    case assignmentEntries bitObject emptyAssignment of
        Left AssignmentObjectMismatch -> pure ()
        _ -> ioError (userError "assignment object mismatch was not reported")

testMissingDomain :: IO ()
testMissingDomain = do
    system <- systemA [("in", EmptySort)] [] [("v", EmptySort)] [] [("in", "v")] []
    topology <- refineA system
    bit <- setA [False, True]
    domains <- rightA "incomplete domains" (finiteValueDomains [(Bit, bit)])
    table <- rightA "empty table" (labelCircuitTable [])
    case acyclicOpenCircuit topology domains table of
        Left (AcyclicCircuitMissingValueDomain 0 EmptySort) -> pure ()
        _ -> ioError (userError "missing domain context changed")

testDuplicateLabel :: IO ()
testDuplicateLabel = do
    coin <- coinCircuit
    case labelCircuitTable [stochasticLabelCircuit Coin [] [Bit] coin, stochasticLabelCircuit Coin [] [Bit] coin] of
        Left (DuplicateLabelCircuitKey 0 1) -> pure ()
        _ -> ioError (userError "duplicate label key accepted")

testMissingLabel :: IO ()
testMissingLabel = do
    system <- coinSystem False
    topology <- refineA system
    domains <- unitDomains
    table <- rightA "missing table" (labelCircuitTable [])
    case acyclicOpenCircuit topology domains table of
        Left (AcyclicCircuitMissingLabelInterpretation 0 "source" Coin [] [Bit]) -> pure ()
        _ -> ioError (userError "missing label context changed")

testLabelEndpointMismatch :: IO ()
testLabelEndpointMismatch = do
    system <- coinSystem False
    topology <- refineA system
    domains <- unitDomains
    (unit, bit) <- objectsA
    sourceBad <- rightA "source-bad table" (labelCircuitTable [deterministicLabelCircuit Coin [] [Bit] (identityCircuit (assignmentObjectValues bit))])
    case acyclicOpenCircuit topology domains sourceBad of
        Left (AcyclicCircuitLabelSourceMismatch 0 "source") -> pure ()
        _ -> ioError (userError "source mismatch not retained")
    targetBad <- rightA "target-bad table" (labelCircuitTable [deterministicLabelCircuit Coin [] [Bit] (identityCircuit (assignmentObjectValues unit))])
    case acyclicOpenCircuit topology domains targetBad of
        Left (AcyclicCircuitLabelTargetMismatch 0 "source") -> pure ()
        _ -> ioError (userError "target mismatch not retained")

testPrimitiveFailure :: IO ()
testPrimitiveFailure = do
    system <- coinSystem False
    topology <- refineA system
    domains <- unitDomains
    (unit, bit) <- objectsA
    table <-
        rightA
            "failure table"
            ( labelCircuitTable
                [ stochasticLabelCircuit
                    Coin
                    []
                    [Bit]
                    (stochasticPrimitive (assignmentObjectValues unit) (assignmentObjectValues bit) PrimitiveFailure)
                ]
            )
    SomeAcyclicOpenCircuit _ circuit <- rightA "failure network" (acyclicOpenCircuit topology domains table)
    case acyclicOpenCircuitDenotation primitiveInterpreter circuit of
        Left (AcyclicEdgeExactError 0 "source" (ExactCircuitPrimitiveError "fixture primitive failure")) -> pure ()
        _ -> ioError (userError "primitive edge context changed")

testRunAcyclic :: IO ()
testRunAcyclic = do
    system <- systemA [("x", Bit)] [("y", Bit)] [("v", Bit)] [] [("x", "v")] [("y", "v")]
    topology <- refineA system
    restrictedBit <- setA [False]
    restrictedDomains <- rightA "run restricted domains" (finiteValueDomains [(Bit, restrictedBit)])
    table <- standardTable
    SomeAcyclicOpenCircuit purity circuit <-
        rightA "run acyclic circuit" (acyclicOpenCircuit topology restrictedDomains table)
    assertDeterministicA purity

    falseInput <- assignmentFor (acyclicOpenCircuitInputObject circuit) [("x", False)]
    distribution <- rightA "run represented input" (runAcyclicOpenCircuit primitiveInterpreter circuit falseInput)
    case NonEmpty.toList (exactOutcomes distribution) of
        [(outputValue, probability)] -> do
            entries <- rightA "run output assignment" (assignmentEntries (acyclicOpenCircuitOutputObject circuit) outputValue)
            assertA "run output value changed" (entries == [("y", False)])
            assertA "run output mass changed" (exactProbability probability == 1)
        _ -> ioError (userError "run returned a non-Dirac passthrough row")

    fullDomains <- unitDomains
    fullInputObject <- rightA "full run input object" (assignmentObject fullDomains (openInput system))
    outsideInput <- assignmentFor fullInputObject [("x", True)]
    case runAcyclicOpenCircuit primitiveInterpreter circuit outsideInput of
        Left AcyclicInputOutsideSource -> pure ()
        _ -> ioError (userError "run accepted an input outside its represented source")

testDeterministicDenotation :: IO ()
testDeterministicDenotation = do
    system <- gateSystem "deterministic" Negate "x" "y"
    SomeAcyclicOpenCircuit purity circuit <- networkA system
    case purity of
        SStochastic -> ioError (userError "deterministic network acquired stochastic purity")
        SDeterministic -> do
            deterministic <-
                rightA
                    "deterministic acyclic denotation"
                    (acyclicDeterministicDenotation primitiveInterpreter circuit)
            stochastic <-
                rightA
                    "stochastic view of deterministic acyclic denotation"
                    (acyclicOpenCircuitDenotation primitiveInterpreter circuit)
            assertA
                "deterministic denotation changed under embedding"
                (stochasticEquivalent (embedDeterministic deterministic) stochastic)

testArityEnumeration :: IO ()
testArityEnumeration = do
    domains <- unitDomains
    zeroObject <- rightA "zero arity" (portAssignmentObject domains [])
    tripleObject <- rightA "triple arity" (portAssignmentObject domains [Bit, Bit, Bit])
    assertA "zero arity support changed" (finiteSetCardinality (assignmentObjectValues zeroObject) == 1)
    assertA "arbitrary arity support changed" (finiteSetCardinality (assignmentObjectValues tripleObject) == 8)

testKnownCoinEquation :: IO ()
testKnownCoinEquation = do
    system <-
        systemA
            []
            [("left", Bit), ("right", Bit)]
            [("shared", Bit), ("left-value", Bit), ("right-value", Bit)]
            [ ("source", BiasedCoin, [], [("shared", Bit)])
            , ("left-edge", Negate, [("shared", Bit)], [("left-value", Bit)])
            , ("right-edge", Asymmetric, [("shared", Bit)], [("right-value", Bit)])
            ]
            []
            [("left", "left-value"), ("right", "right-value")]
    topological <- denotationA system
    closed <- completeValuationA system
    assertA
        "live-frontier and complete-valuation denotations differ"
        (stochasticEquivalent topological closed)

testNormalization :: IO ()
testNormalization = do
    system <- chainA "n" "x" "y"
    arrow <- denotationA system
    assertA
        "a represented row was not exact one"
        (all ((== nn 1) . foldr plus zero) (matrixRows (forgetStochastic arrow)))

testChainSemantics :: IO ()
testChainSemantics = do
    system <- chainA "c" "x" "y"
    arrow <- denotationA system
    SomeAcyclicOpenCircuit _ circuit <- networkA system
    falseInput <- assignmentFor (acyclicOpenCircuitInputObject circuit) [("x", False)]
    trueInput <- assignmentFor (acyclicOpenCircuitInputObject circuit) [("x", True)]
    falseOutput <- assignmentFor (acyclicOpenCircuitOutputObject circuit) [("y", False)]
    trueOutput <- assignmentFor (acyclicOpenCircuitOutputObject circuit) [("y", True)]
    assertA "double negation false changed" (entryA arrow falseInput falseOutput == 1)
    assertA "double negation true changed" (entryA arrow trueInput trueOutput == 1)
    negation <- notCircuit
    directCircuit <- rightA "direct double-negation circuit" (composeCircuit negation negation)
    directArrow <- rightA "direct double-negation matrix" (interpretExactCircuit primitiveInterpreter directCircuit)
    (_, bitObject) <- objectsA
    directFalse <- assignmentFor bitObject [(0, False)]
    directTrue <- assignmentFor bitObject [(0, True)]
    assertA
        "DAG chain differed from directly built circuit and matrix"
        ( entryA arrow falseInput falseOutput == entryA directArrow directFalse directFalse
            && entryA arrow trueInput trueOutput == entryA directArrow directTrue directTrue
        )

testSharing :: IO ()
testSharing = do
    system <- coinSystem True
    arrow <- denotationA system
    SomeAcyclicOpenCircuit _ circuit <- networkA system
    unit <- assignmentFor (acyclicOpenCircuitInputObject circuit) []
    ff <- assignmentFor (acyclicOpenCircuitOutputObject circuit) [("left", False), ("right", False)]
    tt <- assignmentFor (acyclicOpenCircuitOutputObject circuit) [("left", True), ("right", True)]
    assertA "shared false diagonal changed" (entryA arrow unit ff == 1 % 2)
    assertA "shared true diagonal changed" (entryA arrow unit tt == 1 % 2)

testInconsistentObservation :: IO ()
testInconsistentObservation = do
    system <- coinSystem True
    arrow <- denotationA system
    SomeAcyclicOpenCircuit _ circuit <- networkA system
    unit <- assignmentFor (acyclicOpenCircuitInputObject circuit) []
    inconsistent <-
        assignmentFor
            (acyclicOpenCircuitOutputObject circuit)
            [("left", False), ("right", True)]
    assertA "inconsistent duplicated observation gained mass" (entryA arrow unit inconsistent == 0)

testLongChainFrontier :: IO ()
testLongChainFrontier = do
    completed <-
        timeout 5000000 $ do
            system <- longChainA 12
            arrow <- denotationA system
            SomeAcyclicOpenCircuit _ circuit <- networkA system
            falseInput <- assignmentFor (acyclicOpenCircuitInputObject circuit) [("x", False)]
            falseOutput <- assignmentFor (acyclicOpenCircuitOutputObject circuit) [("y", False)]
            assertA "twelve-edge even chain changed value" (entryA arrow falseInput falseOutput == 1)
            distribution <- rightA "twelve-edge direct run" (runAcyclicOpenCircuit primitiveInterpreter circuit falseInput)
            case NonEmpty.toList (exactOutcomes distribution) of
                [(outputValue, probability)] ->
                    assertA
                        "twelve-edge direct run differed from its denotation row"
                        (outputValue == falseOutput && exactProbability probability == 1)
                _ -> ioError (userError "twelve-edge direct run was not Dirac")
    case completed of
        Nothing -> ioError (userError "twelve-edge live-frontier evaluation exceeded five seconds")
        Just () -> pure ()

testIndependent :: IO ()
testIndependent = do
    system <- independentSystem
    arrow <- denotationA system
    SomeAcyclicOpenCircuit _ circuit <- networkA system
    unit <- assignmentFor (acyclicOpenCircuitInputObject circuit) []
    ft <- assignmentFor (acyclicOpenCircuitOutputObject circuit) [("left", False), ("right", True)]
    assertA "independent product mass changed" (entryA arrow unit ft == 1 % 4)

testSharingDifference :: IO ()
testSharingDifference = do
    shared <- coinSystem True >>= denotationA
    independent <- independentSystem >>= denotationA
    assertA "sharing equaled independent execution" (not (stochasticEquivalent shared independent))

testDiscard :: IO ()
testDiscard = do
    system <- systemA [] [] [("v", Bit)] [("coin", Coin, [], [("v", Bit)])] [] []
    arrow <- denotationA system
    SomeAcyclicOpenCircuit _ circuit <- networkA system
    unitInput <- assignmentFor (acyclicOpenCircuitInputObject circuit) []
    unitOutput <- assignmentFor (acyclicOpenCircuitOutputObject circuit) []
    assertA "discard did not marginalize" (entryA arrow unitInput unitOutput == 1)

    zeroOutput <-
        systemA
            [("in", Bit)]
            []
            [("v", Bit)]
            [("sink", Sink, [("v", Bit)], [])]
            [("in", "v")]
            []
    zeroOutputArrow <- denotationA zeroOutput
    SomeAcyclicOpenCircuit _ zeroOutputCircuit <- networkA zeroOutput
    falseInput <- assignmentFor (acyclicOpenCircuitInputObject zeroOutputCircuit) [("in", False)]
    zeroOutputUnit <- assignmentFor (acyclicOpenCircuitOutputObject zeroOutputCircuit) []
    assertA
        "zero-output edge was not the unit channel"
        (entryA zeroOutputArrow falseInput zeroOutputUnit == 1)

testPartialOutputDiscard :: IO ()
testPartialOutputDiscard = do
    system <-
        systemA
            []
            [("kept", Bit)]
            [("kept-value", Bit), ("discarded-value", Bit)]
            [
                ( "pair-coin"
                , PairCoin
                , []
                , [("kept-value", Bit), ("discarded-value", Bit)]
                )
            ]
            []
            [("kept", "kept-value")]
    liveFrontier <- denotationA system
    closed <- completeValuationA system
    SomeAcyclicOpenCircuit _ circuit <- networkA system
    unit <- assignmentFor (acyclicOpenCircuitInputObject circuit) []
    falseOutput <- assignmentFor (acyclicOpenCircuitOutputObject circuit) [("kept", False)]
    trueOutput <- assignmentFor (acyclicOpenCircuitOutputObject circuit) [("kept", True)]
    assertA "partial output discard changed false marginal" (entryA liveFrontier unit falseOutput == 1 % 2)
    assertA "partial output discard changed true marginal" (entryA liveFrontier unit trueOutput == 1 % 2)
    assertA
        "partial output discard differed from complete valuation"
        (stochasticEquivalent liveFrontier closed)

testDiamond :: IO ()
testDiamond = do
    system <-
        systemA
            []
            [("c", Bit), ("d", Bit)]
            [("b", Bit), ("c-v", Bit), ("d-v", Bit)]
            [ ("source", Coin, [], [("b", Bit)])
            , ("left", Negate, [("b", Bit)], [("c-v", Bit)])
            , ("right", Negate, [("b", Bit)], [("d-v", Bit)])
            ]
            []
            [("c", "c-v"), ("d", "d-v")]
    arrow <- denotationA system
    SomeAcyclicOpenCircuit _ circuit <- networkA system
    unit <- assignmentFor (acyclicOpenCircuitInputObject circuit) []
    ff <- assignmentFor (acyclicOpenCircuitOutputObject circuit) [("c", False), ("d", False)]
    ft <- assignmentFor (acyclicOpenCircuitOutputObject circuit) [("c", False), ("d", True)]
    assertA "diamond conditional product changed" (entryA arrow unit ff == 1 % 2)
    assertA "diamond invented conditionally inconsistent mass" (entryA arrow unit ft == 0)

testScheduleIndependence :: IO ()
testScheduleIndependence = do
    first <- scheduledSystem False
    second <- scheduledSystem True
    firstArrow <- denotationA first
    secondArrow <- denotationA second
    assertA "ready-edge schedule changed denotation" (stochasticEquivalent firstArrow secondArrow)
    SomeAcyclicOpenCircuit _ firstCircuit <- networkA first
    unit <- assignmentFor (acyclicOpenCircuitInputObject firstCircuit) []
    trueOutput <- assignmentFor (acyclicOpenCircuitOutputObject firstCircuit) [("out", True)]
    assertA
        "distinguishable scheduled branches lost their asymmetric consumer"
        (entryA firstArrow unit trueOutput == 1 % 8)

testRenaming :: IO ()
testRenaming = do
    first <- coinSystem False
    second <-
        systemA [] [("out", Bit)] [("renamed", Bit)] [("renamed-edge", Coin, [], [("renamed", Bit)])] [] [("out", "renamed")]
    firstArrow <- denotationA first
    secondArrow <- denotationA second
    assertA "renaming changed denotation" (stochasticEquivalent firstArrow secondArrow)

testBoundaryPermutation :: IO ()
testBoundaryPermutation = do
    canonical <-
        systemA
            [("a", Bit), ("b", Bit)]
            [("a", Bit), ("b", Bit)]
            [("va", Bit), ("vb", Bit)]
            []
            [("a", "va"), ("b", "vb")]
            [("a", "va"), ("b", "vb")]
    permuted <-
        systemA
            [("a", Bit), ("b", Bit)]
            [("b", Bit), ("a", Bit)]
            [("va", Bit), ("vb", Bit)]
            []
            [("a", "va"), ("b", "vb")]
            [("b", "vb"), ("a", "va")]
    canonicalArrow <- denotationA canonical
    permutedArrow <- denotationA permuted
    assertA
        "boundary layout permutation changed named denotation"
        (stochasticEquivalent canonicalArrow permutedArrow)

testEmptyBoundaryUnit :: IO ()
testEmptyBoundaryUnit = do
    system <- systemA [] [] [] [] [] []
    arrow <- denotationA system
    SomeAcyclicOpenCircuit _ circuit <- networkA system
    unitInput <- assignmentFor (acyclicOpenCircuitInputObject circuit) []
    unitOutput <- assignmentFor (acyclicOpenCircuitOutputObject circuit) []
    assertA "empty topology did not denote the singleton unit channel" (entryA arrow unitInput unitOutput == 1)

testVacuous :: IO ()
testVacuous = do
    system <- systemA [("x", EmptySort)] [("y", EmptySort)] [("v", EmptySort)] [] [("x", "v")] [("y", "v")]
    arrow <- denotationA system
    assertA "empty-source matrix acquired rows" (null (matrixRows (forgetStochastic arrow)))
    assertA "vacuous identity law changed" (stochasticEquivalent arrow (identityStochastic (stochasticSource arrow)))

testCompositionTopology :: IO ()
testCompositionTopology = do
    left <- chainA "l" "x" "m" >>= refineA
    right <- chainA "r" "m" "z" >>= refineA
    _ <- rightA "acyclic composition" (composeAcyclicOpenSystem left right)
    pure ()

testCompositionBoundaryMismatch :: IO ()
testCompositionBoundaryMismatch = do
    left <- sourceSystem Coin "middle" >>= refineA
    rightSystem <-
        systemA
            [("middle", EmptySort)]
            []
            [("right-input", EmptySort)]
            []
            [("middle", "right-input")]
            []
    right <- refineA rightSystem
    case composeAcyclicOpenSystem left right of
        Left (AcyclicCompositionTopologyError OpenSequentialBoundaryMismatch) -> pure ()
        _ -> ioError (userError "composition boundary mismatch was not retained")

testCompositionSemantics :: IO ()
testCompositionSemantics = do
    leftSystem <- sourceSystem BiasedCoin "m"
    rightSystem <- gateSystem "composition" Negate "m" "z"
    leftTopology <- refineA leftSystem
    rightTopology <- refineA rightSystem
    compositeTopology <- rightA "topology composition" (composeAcyclicOpenSystem leftTopology rightTopology)
    domains <- unitDomains
    table <- standardTable
    SomeAcyclicOpenCircuit _ left <- rightA "left network" (acyclicOpenCircuit leftTopology domains table)
    SomeAcyclicOpenCircuit _ right <- rightA "right network" (acyclicOpenCircuit rightTopology domains table)
    SomeAcyclicOpenCircuit _ composite <- rightA "composite network" (acyclicOpenCircuit compositeTopology domains table)
    leftArrow <- rightA "left arrow" (acyclicOpenCircuitDenotation primitiveInterpreter left)
    rightArrow <- rightA "right arrow" (acyclicOpenCircuitDenotation primitiveInterpreter right)
    compositeArrow <- rightA "composite arrow" (acyclicOpenCircuitDenotation primitiveInterpreter composite)
    expected <- rightA "matrix composition" (composeStochastic leftArrow rightArrow)
    assertA "pushout and matrix composition differ" (stochasticEquivalent compositeArrow expected)
    unit <- assignmentFor (acyclicOpenCircuitInputObject composite) []
    falseOutput <- assignmentFor (acyclicOpenCircuitOutputObject composite) [("z", False)]
    trueOutput <- assignmentFor (acyclicOpenCircuitOutputObject composite) [("z", True)]
    assertA "nontrivial composition false mass changed" (entryA compositeArrow unit falseOutput == 3 % 4)
    assertA "nontrivial composition true mass changed" (entryA compositeArrow unit trueOutput == 1 % 4)

testCompositionSharing :: IO ()
testCompositionSharing = do
    producer <- coinSystem True
    consumer <-
        systemA
            [("left", Bit), ("right", Bit)]
            [("left-out", Bit), ("right-out", Bit)]
            [("a", Bit), ("b", Bit)]
            []
            [("left", "a"), ("right", "b")]
            [("left-out", "a"), ("right-out", "b")]
    producerTopology <- refineA producer
    consumerTopology <- refineA consumer
    compositeTopology <- rightA "sharing composition" (composeAcyclicOpenSystem producerTopology consumerTopology)
    domains <- unitDomains
    table <- standardTable
    SomeAcyclicOpenCircuit _ composite <- rightA "sharing composite circuit" (acyclicOpenCircuit compositeTopology domains table)
    arrow <- rightA "sharing composite arrow" (acyclicOpenCircuitDenotation primitiveInterpreter composite)
    unit <- assignmentFor (acyclicOpenCircuitInputObject composite) []
    inconsistent <- assignmentFor (acyclicOpenCircuitOutputObject composite) [("left-out", False), ("right-out", True)]
    assertA "pushout composition lost sharing" (entryA arrow unit inconsistent == 0)

testIdentity :: IO ()
testIdentity = do
    object <- interfaceA [("x", Bit)]
    let topology = identityAcyclicOpenSystem object :: AcyclicOpenSystem Sort String String String Data.Void.Void Label
    domains <- unitDomains
    table <- standardTable
    SomeAcyclicOpenCircuit purity circuit <- rightA "identity circuit" (acyclicOpenCircuit topology domains table)
    case purity of
        SStochastic -> ioError (userError "empty graph became stochastic")
        SDeterministic -> do
            arrow <- rightA "identity denotation" (acyclicOpenCircuitDenotation primitiveInterpreter circuit)
            assertA "identity topology changed identity matrix" (stochasticEquivalent arrow (identityStochastic (assignmentObjectValues (acyclicOpenCircuitInputObject circuit))))

testTensor :: IO ()
testTensor = do
    leftSystem <- sourceSystem Coin "out"
    rightSystem <- sourceSystem BiasedCoin "out"
    leftTopology <- refineA leftSystem
    rightTopology <- refineA rightSystem
    let tensorTopology = tensorAcyclicOpenSystem leftTopology rightTopology
    domains <- unitDomains
    table <- standardTable
    SomeAcyclicOpenCircuit _ left <- rightA "left tensor network" (acyclicOpenCircuit leftTopology domains table)
    SomeAcyclicOpenCircuit _ right <- rightA "right tensor network" (acyclicOpenCircuit rightTopology domains table)
    SomeAcyclicOpenCircuit _ tensor <- rightA "tensor network" (acyclicOpenCircuit tensorTopology domains table)
    leftArrow <- rightA "left tensor arrow" (acyclicOpenCircuitDenotation primitiveInterpreter left)
    rightArrow <- rightA "right tensor arrow" (acyclicOpenCircuitDenotation primitiveInterpreter right)
    tensorArrow <- rightA "tensor arrow" (acyclicOpenCircuitDenotation primitiveInterpreter tensor)
    leftUnit <- assignmentFor (acyclicOpenCircuitInputObject left) []
    rightUnit <- assignmentFor (acyclicOpenCircuitInputObject right) []
    let directTensor = tensorStochastic leftArrow rightArrow
        tensorSource = stochasticSource tensorArrow
        tensorTarget = stochasticTarget tensorArrow
        tensorOutputObject = acyclicOpenCircuitOutputObject tensor
        leftOutputObject = acyclicOpenCircuitOutputObject left
        rightOutputObject = acyclicOpenCircuitOutputObject right
    rows <-
        traverse
            ( \tensorInput -> do
                inputEntries <- rightA "tensor input reindexing" (assignmentEntries (acyclicOpenCircuitInputObject tensor) tensorInput)
                assertA "tensor unit input acquired entries" (null inputEntries)
                traverse
                    ( directTensorEntry
                        tensorOutputObject
                        leftOutputObject
                        rightOutputObject
                        leftUnit
                        rightUnit
                        directTensor
                    )
                    (finiteSetValues tensorTarget)
            )
            (finiteSetValues tensorSource)
    rawExpected <- rightA "reindexed direct tensor rows" (matrixFromRows tensorSource tensorTarget rows)
    expected <- rightA "reindexed direct tensor" (stochasticMatrix rawExpected)
    assertA
        "DAG tensor differed from tensorStochastic after named-assignment reindexing"
        (stochasticEquivalent tensorArrow expected)
  where
    directTensorEntry tensorOutputObject leftOutputObject rightOutputObject leftUnit rightUnit directTensor tensorOutput = do
        entries <- rightA "tensor output reindexing" (assignmentEntries tensorOutputObject tensorOutput)
        leftValue <- requiredEntry (Left "out") entries
        rightValue <- requiredEntry (Right "out") entries
        leftOutput <- assignmentFor leftOutputObject [("out", leftValue)]
        rightOutput <- assignmentFor rightOutputObject [("out", rightValue)]
        case matrixEntry (forgetStochastic directTensor) (leftUnit, rightUnit) (leftOutput, rightOutput) of
            Nothing -> ioError (userError "direct tensor reindexing left represented support")
            Just mass -> pure mass

    requiredEntry requested entries =
        case lookup requested entries of
            Nothing -> ioError (userError "tensor reindexing lost a named output")
            Just value -> pure value

testAssociations :: IO ()
testAssociations = do
    first <- sourceSystem BiasedCoin "x" >>= refineA
    second <- gateSystem "association-not" Negate "x" "y" >>= refineA
    third <- gateSystem "association-asymmetric" Asymmetric "y" "z" >>= refineA
    firstSecond <- rightA "first association prefix" (composeAcyclicOpenSystem first second)
    left <- rightA "left association" (composeAcyclicOpenSystem firstSecond third)
    secondThird <- rightA "second association suffix" (composeAcyclicOpenSystem second third)
    right <- rightA "right association" (composeAcyclicOpenSystem first secondThird)
    domains <- unitDomains
    table <- standardTable
    SomeAcyclicOpenCircuit _ firstCircuit <- rightA "first association circuit" (acyclicOpenCircuit first domains table)
    SomeAcyclicOpenCircuit _ secondCircuit <- rightA "second association circuit" (acyclicOpenCircuit second domains table)
    SomeAcyclicOpenCircuit _ thirdCircuit <- rightA "third association circuit" (acyclicOpenCircuit third domains table)
    SomeAcyclicOpenCircuit _ leftCircuit <- rightA "left associated circuit" (acyclicOpenCircuit left domains table)
    SomeAcyclicOpenCircuit _ rightCircuit <- rightA "right associated circuit" (acyclicOpenCircuit right domains table)
    firstArrow <- rightA "first association arrow" (acyclicOpenCircuitDenotation primitiveInterpreter firstCircuit)
    secondArrow <- rightA "second association arrow" (acyclicOpenCircuitDenotation primitiveInterpreter secondCircuit)
    thirdArrow <- rightA "third association arrow" (acyclicOpenCircuitDenotation primitiveInterpreter thirdCircuit)
    leftArrow <- rightA "left associated arrow" (acyclicOpenCircuitDenotation primitiveInterpreter leftCircuit)
    rightArrow <- rightA "right associated arrow" (acyclicOpenCircuitDenotation primitiveInterpreter rightCircuit)
    prefix <- rightA "direct association prefix" (composeStochastic firstArrow secondArrow)
    expected <- rightA "direct three-factor composition" (composeStochastic prefix thirdArrow)
    assertA "association changed observational semantics" (stochasticEquivalent leftArrow rightArrow)
    assertA "associated topology omitted or reordered a factor" (stochasticEquivalent leftArrow expected)
    unit <- assignmentFor (acyclicOpenCircuitInputObject leftCircuit) []
    trueOutput <- assignmentFor (acyclicOpenCircuitOutputObject leftCircuit) [("z", True)]
    assertA "nontrivial associated result changed" (entryA leftArrow unit trueOutput == 1 % 6)

testUnits :: IO ()
testUnits = do
    system <- sourceSystem BiasedCoin "y"
    topology <- refineA system
    let leftIdentity = identityAcyclicOpenSystem (openInput system) :: AcyclicOpenSystem Sort String String String Data.Void.Void Label
        rightIdentity = identityAcyclicOpenSystem (openOutput system) :: AcyclicOpenSystem Sort String String String Data.Void.Void Label
    left <- rightA "left unit topology" (composeAcyclicOpenSystem leftIdentity topology)
    right <- rightA "right unit topology" (composeAcyclicOpenSystem topology rightIdentity)
    domains <- unitDomains
    table <- standardTable
    SomeAcyclicOpenCircuit _ originalCircuit <- rightA "original unit circuit" (acyclicOpenCircuit topology domains table)
    SomeAcyclicOpenCircuit _ leftCircuit <- rightA "left unit circuit" (acyclicOpenCircuit left domains table)
    SomeAcyclicOpenCircuit _ rightCircuit <- rightA "right unit circuit" (acyclicOpenCircuit right domains table)
    originalArrow <- rightA "original unit arrow" (acyclicOpenCircuitDenotation primitiveInterpreter originalCircuit)
    leftArrow <- rightA "left unit arrow" (acyclicOpenCircuitDenotation primitiveInterpreter leftCircuit)
    rightArrow <- rightA "right unit arrow" (acyclicOpenCircuitDenotation primitiveInterpreter rightCircuit)
    assertA "left unit changed semantics" (stochasticEquivalent originalArrow leftArrow)
    assertA "right unit changed semantics" (stochasticEquivalent originalArrow rightArrow)
    unit <- assignmentFor (acyclicOpenCircuitInputObject originalCircuit) []
    falseOutput <- assignmentFor (acyclicOpenCircuitOutputObject originalCircuit) [("y", False)]
    assertA "unit fixture became denotationally trivial" (entryA originalArrow unit falseOutput == 1 % 4)
