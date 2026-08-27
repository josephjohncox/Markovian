{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module OpenSystems (runOpenSystemTests) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Void (Void, absurd)
import Markovian.Algebra.NonNegativeRational
import Markovian.Category.Finite.Set
import Markovian.Category.Matrix.Deterministic
import Markovian.Category.Matrix.Stochastic
import Markovian.Circuit
import Markovian.Circuit.Interpret.Exact
import Markovian.Open.Circuit.Exact
import Markovian.Open.Hypergraph
import Markovian.Open.Interface
import Markovian.Open.Pushout
import Markovian.Open.StructuredCospan

runOpenSystemTests :: (String -> IO () -> IO ()) -> IO ()
runOpenSystemTests run = do
    run "finite typed interfaces, hypergraphs, and pushout witnesses" testFinitePushout
    run "structured-cospan composition, tensor, units, and reversal" testOpenOperations
    run "structured-cospan associativity up to canonical-representation isomorphism" testOpenAssociativity
    run "open-system cells and double interchange" testDoubleInterchange
    run "open circuit denotational composition and proof boundary" testOpenCircuitDenotation
    run "open-system validation errors" testOpenValidation

data WireSort = BitWire | ControlWire
    deriving (Eq, Show)

data GateLabel = FlowGate
    deriving (Eq, Show)

data AssociativityOrigin
    = FirstOrigin String
    | SecondOrigin String
    | ThirdOrigin String
    deriving (Eq, Show)

data OpenPrimitive purity source target where
    OpenNot :: OpenPrimitive 'Deterministic Bool Bool

openPrimitiveInterpreter :: ExactPrimitiveInterpreter OpenPrimitive String
openPrimitiveInterpreter =
    ExactPrimitiveInterpreter
        { interpretDeterministicPrimitive = interpretOpenDeterministic
        , interpretStochasticPrimitive = \_ _ primitive -> case primitive of {}
        }

interpretOpenDeterministic ::
    FiniteSet source ->
    FiniteSet target ->
    OpenPrimitive 'Deterministic source target ->
    Either String (DeterministicMatrix NonNegativeRational source target)
interpretOpenDeterministic source target OpenNot =
    case deterministicFromFunction source target not of
        Left _ -> Left "invalid deterministic open primitive"
        Right arrow -> Right arrow

assertO :: String -> Bool -> IO ()
assertO message predicate =
    if predicate then pure () else ioError (userError message)

requireRightO :: (Show error) => String -> Either error value -> IO value
requireRightO _ (Right value) = pure value
requireRightO label (Left problem) = ioError (userError (label ++ ": " ++ show problem))

interfaceO :: [(String, WireSort)] -> IO (Interface WireSort String)
interfaceO = requireRightO "typed interface" . interface

segment ::
    String ->
    String ->
    String ->
    IO (OpenSystem WireSort String String String String GateLabel)
segment prefix inputPort outputPort = do
    input <- interfaceO [(inputPort, BitWire)]
    output <- interfaceO [(outputPort, BitWire)]
    vertices <- interfaceO [(prefix ++ "-in", BitWire), (prefix ++ "-out", BitWire)]
    graph <-
        requireRightO
            "segment hypergraph"
            ( typedHypergraph
                vertices
                [
                    ( prefix ++ "-edge"
                    , FlowGate
                    , [(prefix ++ "-in", BitWire)]
                    , [(prefix ++ "-out", BitWire)]
                    )
                ]
            )
    inputLeg <- requireRightO "segment input leg" (interfaceMap input vertices [(inputPort, prefix ++ "-in")])
    outputLeg <- requireRightO "segment output leg" (interfaceMap output vertices [(outputPort, prefix ++ "-out")])
    requireRightO "segment open system" (openSystem input output graph inputLeg outputLeg)

renameCell ::
    OpenSystem WireSort String String String String GateLabel ->
    OpenSystem WireSort String String String String GateLabel ->
    IO
        ( OpenSystemCell
            WireSort
            String
            String
            String
            String
            String
            String
            String
            String
            GateLabel
        )
renameCell source target = do
    let sourceVertices = map fst (interfaceEntries (hypergraphVertices (openApex source)))
        targetVertices = map fst (interfaceEntries (hypergraphVertices (openApex target)))
        sourceEdges = finiteSetValues (hypergraphEdgeIds (openApex source))
        targetEdges = finiteSetValues (hypergraphEdgeIds (openApex target))
    apexMap <-
        requireRightO
            "renaming hypergraph map"
            (hypergraphMap (openApex source) (openApex target) (zip sourceVertices targetVertices) (zip sourceEdges targetEdges))
    requireRightO
        "renaming open cell"
        ( openSystemCell
            source
            target
            (identityInterfaceMap (openInput source))
            (identityInterfaceMap (openOutput source))
            apexMap
        )

assertLeftUnitorIsomorphism ::
    OpenSystem WireSort String String String String GateLabel ->
    OpenSystem WireSort String String (PushoutPoint String String) (Either Void String) GateLabel ->
    IO ()
assertLeftUnitorIsomorphism original composed = do
    let composedVertices = map fst (interfaceEntries (hypergraphVertices (openApex composed)))
        originalVertices = map fst (interfaceEntries (hypergraphVertices (openApex original)))
    forwardVertices <- traverse leftClassEntry composedVertices
    let forwardEdges =
            [ (edge, either absurd id edge)
            | edge <- finiteSetValues (hypergraphEdgeIds (openApex composed))
            ]
    forwardApex <- requireRightO "left unitor apex map" (hypergraphMap (openApex composed) (openApex original) forwardVertices forwardEdges)
    backwardVertices <-
        traverse
            (\vertex -> (vertex,) <$> classContaining (Right vertex) composed)
            originalVertices
    let backwardEdges = [(edge, Right edge) | edge <- finiteSetValues (hypergraphEdgeIds (openApex original))]
    backwardApex <- requireRightO "inverse left unitor apex map" (hypergraphMap (openApex original) (openApex composed) backwardVertices backwardEdges)
    forwardCell <-
        requireRightO
            "left unitor cell"
            (openSystemCell composed original (identityInterfaceMap (openInput original)) (identityInterfaceMap (openOutput original)) forwardApex)
    backwardCell <-
        requireRightO
            "inverse left unitor cell"
            (openSystemCell original composed (identityInterfaceMap (openInput original)) (identityInterfaceMap (openOutput original)) backwardApex)
    assertCellIsomorphism "left unitor" forwardCell backwardCell
  where
    leftClassEntry point = do
        images <-
            traverse
                (either (boundaryImage (openInputLeg original)) pure)
                (NonEmpty.toList (pushoutPointMembers point))
        case images of
            firstImage : remaining
                | all (== firstImage) remaining -> pure (point, firstImage)
            _ -> ioError (userError "left unitor class did not have one canonical image")

assertRightUnitorIsomorphism ::
    OpenSystem WireSort String String String String GateLabel ->
    OpenSystem WireSort String String (PushoutPoint String String) (Either String Void) GateLabel ->
    IO ()
assertRightUnitorIsomorphism original composed = do
    let composedVertices = map fst (interfaceEntries (hypergraphVertices (openApex composed)))
        originalVertices = map fst (interfaceEntries (hypergraphVertices (openApex original)))
    forwardVertices <- traverse rightClassEntry composedVertices
    let forwardEdges =
            [ (edge, either id absurd edge)
            | edge <- finiteSetValues (hypergraphEdgeIds (openApex composed))
            ]
    forwardApex <- requireRightO "right unitor apex map" (hypergraphMap (openApex composed) (openApex original) forwardVertices forwardEdges)
    backwardVertices <-
        traverse
            (\vertex -> (vertex,) <$> classContaining (Left vertex) composed)
            originalVertices
    let backwardEdges = [(edge, Left edge) | edge <- finiteSetValues (hypergraphEdgeIds (openApex original))]
    backwardApex <- requireRightO "inverse right unitor apex map" (hypergraphMap (openApex original) (openApex composed) backwardVertices backwardEdges)
    forwardCell <-
        requireRightO
            "right unitor cell"
            (openSystemCell composed original (identityInterfaceMap (openInput original)) (identityInterfaceMap (openOutput original)) forwardApex)
    backwardCell <-
        requireRightO
            "inverse right unitor cell"
            (openSystemCell original composed (identityInterfaceMap (openInput original)) (identityInterfaceMap (openOutput original)) backwardApex)
    assertCellIsomorphism "right unitor" forwardCell backwardCell
  where
    rightClassEntry point = do
        images <-
            traverse
                (either pure (boundaryImage (openOutputLeg original)))
                (NonEmpty.toList (pushoutPointMembers point))
        case images of
            firstImage : remaining
                | all (== firstImage) remaining -> pure (point, firstImage)
            _ -> ioError (userError "right unitor class did not have one canonical image")

boundaryImage :: InterfaceMap WireSort String String -> String -> IO String
boundaryImage leg port =
    case interfaceMapImage leg port of
        Nothing -> ioError (userError "unitor boundary point had no apex image")
        Just vertex -> pure vertex

classContaining ::
    Either String String ->
    OpenSystem WireSort String String (PushoutPoint String String) edge GateLabel ->
    IO (PushoutPoint String String)
classContaining member system =
    case filter (elem member . pushoutPointMembers) (map fst (interfaceEntries (hypergraphVertices (openApex system)))) of
        [point] -> pure point
        _ -> ioError (userError "unitor quotient class was not unique")

assertCellIsomorphism ::
    (Eq sourceVertex, Eq targetVertex, Eq sourceEdge, Eq targetEdge) =>
    String ->
    OpenSystemCell WireSort String String String String sourceVertex sourceEdge targetVertex targetEdge GateLabel ->
    OpenSystemCell WireSort String String String String targetVertex targetEdge sourceVertex sourceEdge GateLabel ->
    IO ()
assertCellIsomorphism label forward backward = do
    sourceRoundTrip <- requireRightO (label ++ " source round trip") (verticalComposeOpenSystemCell forward backward)
    targetRoundTrip <- requireRightO (label ++ " target round trip") (verticalComposeOpenSystemCell backward forward)
    assertO
        (label ++ " source vertex maps were not inverse")
        (hypergraphVertexEntries (cellApexMap sourceRoundTrip) == hypergraphVertexEntries (identityHypergraphMap (openApex (cellSource forward))))
    assertO
        (label ++ " source edge maps were not inverse")
        (hypergraphEdgeEntries (cellApexMap sourceRoundTrip) == hypergraphEdgeEntries (identityHypergraphMap (openApex (cellSource forward))))
    assertO
        (label ++ " target vertex maps were not inverse")
        (hypergraphVertexEntries (cellApexMap targetRoundTrip) == hypergraphVertexEntries (identityHypergraphMap (openApex (cellTarget forward))))
    assertO
        (label ++ " target edge maps were not inverse")
        (hypergraphEdgeEntries (cellApexMap targetRoundTrip) == hypergraphEdgeEntries (identityHypergraphMap (openApex (cellTarget forward))))

testFinitePushout :: IO ()
testFinitePushout = do
    common <- interfaceO [("a", BitWire), ("b", BitWire)]
    left <- interfaceO [("l", BitWire)]
    right <- interfaceO [("r1", BitWire), ("r2", BitWire)]
    leftMap <- requireRightO "noninjective left span" (interfaceMap common left [("b", "l"), ("a", "l")])
    rightMap <- requireRightO "right span" (interfaceMap common right [("b", "r2"), ("a", "r1")])
    assertO "interface map did not canonicalize source row order" (map fst (interfaceMapEntries rightMap) == ["a", "b"])
    pushout <- requireRightO "finite pushout" (finitePushout leftMap rightMap)
    assertO "pushout quotient did not identify the generated class" (length (pushoutClasses pushout) == 1)
    quotientClass <-
        case pushoutClasses pushout of
            [point] -> pure point
            _ -> ioError (userError "expected one explicit quotient class")
    let members = NonEmpty.toList (pushoutPointMembers quotientClass)
    assertO
        "pushout quotient members were not in canonical carrier order"
        (members == [Left "l", Right "r1", Right "r2"])

    twoLeft <- interfaceO [("l1", BitWire), ("l2", BitWire)]
    oneRight <- interfaceO [("r", BitWire)]
    twoLeftMap <- requireRightO "two-left pushout span" (interfaceMap common twoLeft [("a", "l1"), ("b", "l2")])
    oneRightMap <- requireRightO "noninjective right pushout span" (interfaceMap common oneRight [("a", "r"), ("b", "r")])
    twoLeftPushout <- requireRightO "two-left finite pushout" (finitePushout twoLeftMap oneRightMap)
    case pushoutClasses twoLeftPushout of
        [point] ->
            assertO
                "relation discovery changed canonical left-then-right class order"
                (NonEmpty.toList (pushoutPointMembers point) == [Left "l1", Left "l2", Right "r"])
        _ -> ioError (userError "expected one two-left quotient class")
    assertO
        "pushout square did not commute at a"
        ( (interfaceMapImage leftMap "a" >>= pushoutPointForLeft pushout)
            == (interfaceMapImage rightMap "a" >>= pushoutPointForRight pushout)
        )
    assertO
        "pushout square did not commute at b"
        ( (interfaceMapImage leftMap "b" >>= pushoutPointForLeft pushout)
            == (interfaceMapImage rightMap "b" >>= pushoutPointForRight pushout)
        )
    sink <- interfaceO [("sink", BitWire)]
    leftCocone <- requireRightO "left pushout cocone" (interfaceMap left sink [("l", "sink")])
    rightCocone <- requireRightO "right pushout cocone" (interfaceMap right sink [("r1", "sink"), ("r2", "sink")])
    factor <- requireRightO "pushout universal factor" (factorPushout pushout leftCocone rightCocone)
    factoredLeft <- requireRightO "factored left cocone" (composeInterfaceMap (pushoutLeftInjection pushout) factor)
    factoredRight <- requireRightO "factored right cocone" (composeInterfaceMap (pushoutRightInjection pushout) factor)
    assertO "pushout factor did not recover left cocone" (interfaceMapEntries factoredLeft == interfaceMapEntries leftCocone)
    assertO "pushout factor did not recover right cocone" (interfaceMapEntries factoredRight == interfaceMapEntries rightCocone)

    reorderedRight <- interfaceO [("r2", BitWire), ("r1", BitWire)]
    coconeTarget <- interfaceO [("sink1", BitWire), ("sink2", BitWire)]
    reorderedCoconeTarget <- interfaceO [("sink2", BitWire), ("sink1", BitWire)]
    reorderedLeftCocone <- requireRightO "reordered-target left cocone" (interfaceMap left coconeTarget [("l", "sink1")])
    reorderedRightCocone <-
        requireRightO
            "reordered source and target right cocone"
            (interfaceMap reorderedRight reorderedCoconeTarget [("r1", "sink1"), ("r2", "sink1")])
    reorderedFactor <- requireRightO "layout-independent pushout factor" (factorPushout pushout reorderedLeftCocone reorderedRightCocone)
    assertO
        "pushout factor did not retain its selected target representation"
        (sameInterfaceLayout (interfaceMapTarget reorderedFactor) coconeTarget)

    twoSinks <- interfaceO [("sink1", BitWire), ("sink2", BitWire)]
    incompatibleLeft <- requireRightO "incompatible left cocone" (interfaceMap left twoSinks [("l", "sink1")])
    incompatibleRight <- requireRightO "incompatible right cocone" (interfaceMap right twoSinks [("r1", "sink1"), ("r2", "sink2")])
    case factorPushout pushout incompatibleLeft incompatibleRight of
        Left (PushoutCoconeDoesNotRespectQuotient 0) -> pure ()
        _ -> ioError (userError "pushout accepted an incompatible cocone")

testOpenOperations :: IO ()
testOpenOperations = do
    empty <- interfaceO []
    let emptyIdentity = identityOpenSystem empty :: OpenSystem WireSort String String String Void GateLabel
    assertO "empty interface was rejected as an open unit" (interfaceCardinality (openInput emptyIdentity) == 0)
    first <- segment "first" "x" "y"
    second <- segment "second" "y" "z"
    composite <- requireRightO "open sequential composition" (composeOpenSystem first second)
    assertO "open composition changed input boundary" (sameInterfaceLayout (openInput composite) (openInput first))
    assertO "open composition changed output boundary" (sameInterfaceLayout (openOutput composite) (openOutput second))
    assertO "open composition lost edges" (finiteSetCardinality (hypergraphEdgeIds (openApex composite)) == 2)
    assertO "open composition did not glue one boundary class" (interfaceCardinality (hypergraphVertices (openApex composite)) == 3)

    let parallel = tensorOpenSystem first second
    assertO "open tensor input was not disjoint union" (interfaceCardinality (openInput parallel) == 2)
    assertO "open tensor output was not disjoint union" (interfaceCardinality (openOutput parallel) == 2)
    assertO "open tensor lost edges" (finiteSetCardinality (hypergraphEdgeIds (openApex parallel)) == 2)

    let reversed = reverseOpenBoundary first
        restored = reverseOpenBoundary reversed
    assertO "boundary reversal was not involutive" (sameOpenSystemLayout first restored)
    assertO "boundary reversal changed apex hyperedges" (sameHypergraphLayout (openApex first) (openApex reversed))

    let leftIdentity = identityOpenSystem (openInput first) :: OpenSystem WireSort String String String Void GateLabel
        rightIdentity = identityOpenSystem (openOutput first) :: OpenSystem WireSort String String String Void GateLabel
    leftUnit <- requireRightO "left open unit" (composeOpenSystem leftIdentity first)
    rightUnit <- requireRightO "right open unit" (composeOpenSystem first rightIdentity)
    assertO "left open unit changed boundary" (sameInterfaceLayout (openInput leftUnit) (openInput first) && sameInterfaceLayout (openOutput leftUnit) (openOutput first))
    assertO "right open unit changed boundary" (sameInterfaceLayout (openInput rightUnit) (openInput first) && sameInterfaceLayout (openOutput rightUnit) (openOutput first))
    assertO "left open unit changed edge count" (finiteSetCardinality (hypergraphEdgeIds (openApex leftUnit)) == 1)
    assertO "right open unit changed edge count" (finiteSetCardinality (hypergraphEdgeIds (openApex rightUnit)) == 1)
    assertLeftUnitorIsomorphism first leftUnit
    assertRightUnitorIsomorphism first rightUnit

testOpenAssociativity :: IO ()
testOpenAssociativity = do
    first <- segment "a" "w" "x"
    second <- segment "b" "x" "y"
    third <- segment "c" "y" "z"
    firstSecond <- requireRightO "first open association" (composeOpenSystem first second)
    leftAssociated <- requireRightO "left-associated open composition" (composeOpenSystem firstSecond third)
    secondThird <- requireRightO "second open association" (composeOpenSystem second third)
    rightAssociated <- requireRightO "right-associated open composition" (composeOpenSystem first secondThird)
    assertO "open associativity changed input boundary" (sameInterfaceLayout (openInput leftAssociated) (openInput rightAssociated))
    assertO "open associativity changed output boundary" (sameInterfaceLayout (openOutput leftAssociated) (openOutput rightAssociated))
    assertO "left-associated gluing changed vertex count" (interfaceCardinality (hypergraphVertices (openApex leftAssociated)) == 4)
    assertO "right-associated gluing changed vertex count" (interfaceCardinality (hypergraphVertices (openApex rightAssociated)) == 4)
    assertO "open associativity changed edge count" (finiteSetCardinality (hypergraphEdgeIds (openApex leftAssociated)) == finiteSetCardinality (hypergraphEdgeIds (openApex rightAssociated)))

    let leftPoints = map fst (interfaceEntries (hypergraphVertices (openApex leftAssociated)))
        rightPoints = map fst (interfaceEntries (hypergraphVertices (openApex rightAssociated)))
    forwardVertices <-
        traverse
            ( \leftPoint -> do
                rightPoint <- findAssociatedRight (flattenLeftAssociated leftPoint) rightPoints
                pure (leftPoint, rightPoint)
            )
            leftPoints
    backwardVertices <-
        traverse
            ( \rightPoint -> do
                leftPoint <- findAssociatedLeft (flattenRightAssociated rightPoint) leftPoints
                pure (rightPoint, leftPoint)
            )
            rightPoints
    let forwardEdges =
            [ (edge, reassociateEdge edge)
            | edge <- finiteSetValues (hypergraphEdgeIds (openApex leftAssociated))
            ]
        backwardEdges =
            [ (edge, unassociateEdge edge)
            | edge <- finiteSetValues (hypergraphEdgeIds (openApex rightAssociated))
            ]
    forwardApex <-
        requireRightO
            "open associator apex map"
            (hypergraphMap (openApex leftAssociated) (openApex rightAssociated) forwardVertices forwardEdges)
    backwardApex <-
        requireRightO
            "inverse open associator apex map"
            (hypergraphMap (openApex rightAssociated) (openApex leftAssociated) backwardVertices backwardEdges)
    associatorCell <-
        requireRightO
            "open associator cell"
            ( openSystemCell
                leftAssociated
                rightAssociated
                (identityInterfaceMap (openInput leftAssociated))
                (identityInterfaceMap (openOutput leftAssociated))
                forwardApex
            )
    inverseAssociatorCell <-
        requireRightO
            "inverse open associator cell"
            ( openSystemCell
                rightAssociated
                leftAssociated
                (identityInterfaceMap (openInput rightAssociated))
                (identityInterfaceMap (openOutput rightAssociated))
                backwardApex
            )
    associatorRoundTrip <-
        requireRightO
            "open associator cell round trip"
            (verticalComposeOpenSystemCell associatorCell inverseAssociatorCell)
    assertO
        "open associator vertex maps were not inverse"
        ( hypergraphVertexEntries (cellApexMap associatorRoundTrip)
            == hypergraphVertexEntries (identityHypergraphMap (openApex leftAssociated))
        )
    assertO
        "open associator edge maps were not inverse"
        ( hypergraphEdgeEntries (cellApexMap associatorRoundTrip)
            == hypergraphEdgeEntries (identityHypergraphMap (openApex leftAssociated))
        )
    inverseAssociatorRoundTrip <-
        requireRightO
            "inverse open associator cell round trip"
            (verticalComposeOpenSystemCell inverseAssociatorCell associatorCell)
    assertO
        "inverse open associator vertex maps were not inverse"
        ( hypergraphVertexEntries (cellApexMap inverseAssociatorRoundTrip)
            == hypergraphVertexEntries (identityHypergraphMap (openApex rightAssociated))
        )
    assertO
        "inverse open associator edge maps were not inverse"
        ( hypergraphEdgeEntries (cellApexMap inverseAssociatorRoundTrip)
            == hypergraphEdgeEntries (identityHypergraphMap (openApex rightAssociated))
        )

testReorderedHorizontalBoundary :: IO ()
testReorderedHorizontalBoundary = do
    sourceMiddle <- interfaceO [("m1", BitWire), ("m2", BitWire)]
    reorderedSourceMiddle <- interfaceO [("m2", BitWire), ("m1", BitWire)]
    targetMiddle <- interfaceO [("n1", BitWire), ("n2", BitWire)]
    reorderedTargetMiddle <- interfaceO [("n2", BitWire), ("n1", BitWire)]
    firstVertical <-
        requireRightO
            "first multiport vertical map"
            (interfaceMap sourceMiddle targetMiddle [("m2", "n2"), ("m1", "n1")])
    secondVertical <-
        requireRightO
            "reordered multiport vertical map"
            (interfaceMap reorderedSourceMiddle reorderedTargetMiddle [("m1", "n1"), ("m2", "n2")])
    assertO "reordered vertical maps were not extensionally equal" (sameInterfaceMap firstVertical secondVertical)
    let firstSource = identityOpenSystem sourceMiddle :: OpenSystem WireSort String String String Void GateLabel
        firstTarget = identityOpenSystem targetMiddle :: OpenSystem WireSort String String String Void GateLabel
        secondSource = identityOpenSystem reorderedSourceMiddle :: OpenSystem WireSort String String String Void GateLabel
        secondTarget = identityOpenSystem reorderedTargetMiddle :: OpenSystem WireSort String String String Void GateLabel
    firstCell <-
        requireRightO
            "first multiport identity cell"
            (openSystemCell firstSource firstTarget firstVertical firstVertical (discreteHypergraphMap firstVertical))
    secondCell <-
        requireRightO
            "reordered multiport identity cell"
            (openSystemCell secondSource secondTarget secondVertical secondVertical (discreteHypergraphMap secondVertical))
    composite <-
        requireRightO
            "horizontal composition with reordered middle maps"
            (horizontalComposeOpenSystemCell firstCell secondCell)
    assertO
        "reordered horizontal composition changed its empty edge support"
        (finiteSetCardinality (hypergraphEdgeIds (openApex (cellSource composite))) == 0)

    finalMiddle <- interfaceO [("p1", BitWire), ("p2", BitWire)]
    reorderedFinalMiddle <- interfaceO [("p2", BitWire), ("p1", BitWire)]
    firstBottomVertical <-
        requireRightO
            "first lower multiport vertical map"
            (interfaceMap targetMiddle finalMiddle [("n2", "p2"), ("n1", "p1")])
    secondBottomVertical <-
        requireRightO
            "reordered lower multiport vertical map"
            (interfaceMap reorderedTargetMiddle reorderedFinalMiddle [("n1", "p1"), ("n2", "p2")])
    let firstBottomTarget = identityOpenSystem finalMiddle :: OpenSystem WireSort String String String Void GateLabel
        secondBottomTarget = identityOpenSystem reorderedFinalMiddle :: OpenSystem WireSort String String String Void GateLabel
    firstBottomCell <-
        requireRightO
            "first lower multiport identity cell"
            (openSystemCell firstTarget firstBottomTarget firstBottomVertical firstBottomVertical (discreteHypergraphMap firstBottomVertical))
    secondBottomCell <-
        requireRightO
            "reordered lower multiport identity cell"
            (openSystemCell secondTarget secondBottomTarget secondBottomVertical secondBottomVertical (discreteHypergraphMap secondBottomVertical))
    horizontalBottom <- requireRightO "reordered lower horizontal cell" (horizontalComposeOpenSystemCell firstBottomCell secondBottomCell)
    horizontalThenVertical <- requireRightO "reordered horizontal-then-vertical cells" (verticalComposeOpenSystemCell composite horizontalBottom)
    verticalFirst <- requireRightO "first reordered vertical composite" (verticalComposeOpenSystemCell firstCell firstBottomCell)
    verticalSecond <- requireRightO "second reordered vertical composite" (verticalComposeOpenSystemCell secondCell secondBottomCell)
    verticalThenHorizontal <- requireRightO "reordered vertical-then-horizontal cells" (horizontalComposeOpenSystemCell verticalFirst verticalSecond)
    assertO
        "reordered multiport interchange changed apex vertex maps"
        (hypergraphVertexEntries (cellApexMap horizontalThenVertical) == hypergraphVertexEntries (cellApexMap verticalThenHorizontal))
    assertO
        "reordered multiport interchange changed vertical boundaries"
        ( sameInterfaceMap (cellInputMap horizontalThenVertical) (cellInputMap verticalThenHorizontal)
            && sameInterfaceMap (cellOutputMap horizontalThenVertical) (cellOutputMap verticalThenHorizontal)
        )

testDoubleInterchange :: IO ()
testDoubleInterchange = do
    testReorderedHorizontalBoundary
    a1 <- segment "a1" "x" "y"
    b1 <- segment "b1" "x" "y"
    c1 <- segment "c1" "x" "y"
    a2 <- segment "a2" "y" "z"
    b2 <- segment "b2" "y" "z"
    c2 <- segment "c2" "y" "z"
    f1 <- renameCell a1 b1
    g1 <- renameCell b1 c1
    f2 <- renameCell a2 b2
    g2 <- renameCell b2 c2

    horizontalTop <- requireRightO "top horizontal cell" (horizontalComposeOpenSystemCell f1 f2)
    horizontalBottom <- requireRightO "bottom horizontal cell" (horizontalComposeOpenSystemCell g1 g2)
    horizontalThenVertical <- requireRightO "horizontal then vertical cells" (verticalComposeOpenSystemCell horizontalTop horizontalBottom)

    verticalLeft <- requireRightO "left vertical cells" (verticalComposeOpenSystemCell f1 g1)
    verticalRight <- requireRightO "right vertical cells" (verticalComposeOpenSystemCell f2 g2)
    verticalThenHorizontal <- requireRightO "vertical then horizontal cells" (horizontalComposeOpenSystemCell verticalLeft verticalRight)

    assertO
        "double-category interchange changed apex vertex map"
        (hypergraphVertexEntries (cellApexMap horizontalThenVertical) == hypergraphVertexEntries (cellApexMap verticalThenHorizontal))
    assertO
        "double-category interchange changed apex edge map"
        (hypergraphEdgeEntries (cellApexMap horizontalThenVertical) == hypergraphEdgeEntries (cellApexMap verticalThenHorizontal))
    assertO
        "double-category interchange changed vertical boundaries"
        ( interfaceMapEntries (cellInputMap horizontalThenVertical) == interfaceMapEntries (cellInputMap verticalThenHorizontal)
            && interfaceMapEntries (cellOutputMap horizontalThenVertical) == interfaceMapEntries (cellOutputMap verticalThenHorizontal)
        )

    let tensorCell = tensorOpenSystemCell f1 f2
    assertO "tensor of cells lost apex maps" (length (hypergraphVertexEntries (cellApexMap tensorCell)) == 4)
    let identityCell = identityOpenSystemCell a1
    identityThenF <- requireRightO "identity vertical cell" (verticalComposeOpenSystemCell identityCell f1)
    assertO "vertical cell identity changed vertex map" (hypergraphVertexEntries (cellApexMap identityThenF) == hypergraphVertexEntries (cellApexMap f1))

testOpenCircuitDenotation :: IO ()
testOpenCircuitDenotation = do
    firstTopology <- segment "d1" "x" "y"
    secondTopology <- segment "d2" "y" "z"
    boolean <- requireRightO "open Boolean state object" (finiteSet [False, True])
    let negation = deterministicPrimitive boolean boolean OpenNot
        first = openCircuit firstTopology negation
        second = openCircuit secondTopology negation
    composite <- requireRightO "decorated open composition" (composeOpenCircuit first second)
    compositeMatrix <- requireRightO "open composite denotation" (openCircuitDenotation openPrimitiveInterpreter composite)
    explicitDecoration <- requireRightO "explicit directed decoration" (composeCircuit negation negation)
    explicitMatrix <- requireRightO "explicit directed matrix" (interpretExactCircuit openPrimitiveInterpreter explicitDecoration)
    assertO "open denotational composition was unsound" (stochasticEquivalent compositeMatrix explicitMatrix)

    thirdTopology <- segment "d3" "z" "q"
    let third = openCircuit thirdTopology negation
    leftPrefix <- requireRightO "left-associated open circuit prefix" (composeOpenCircuit first second)
    leftAssociated <- requireRightO "left-associated open circuit" (composeOpenCircuit leftPrefix third)
    rightSuffix <- requireRightO "right-associated open circuit suffix" (composeOpenCircuit second third)
    rightAssociated <- requireRightO "right-associated open circuit" (composeOpenCircuit first rightSuffix)
    leftAssociatedMatrix <- requireRightO "left-associated open denotation" (openCircuitDenotation openPrimitiveInterpreter leftAssociated)
    rightAssociatedMatrix <- requireRightO "right-associated open denotation" (openCircuitDenotation openPrimitiveInterpreter rightAssociated)
    assertO "open denotation was not associative" (stochasticEquivalent leftAssociatedMatrix rightAssociatedMatrix)

    let leftIdentityTopology = identityOpenSystem (openInput firstTopology) :: OpenSystem WireSort String String String Void GateLabel
        rightIdentityTopology = identityOpenSystem (openOutput firstTopology) :: OpenSystem WireSort String String String Void GateLabel
        leftIdentityCircuit = openCircuit leftIdentityTopology (identityCircuit boolean)
        rightIdentityCircuit = openCircuit rightIdentityTopology (identityCircuit boolean)
    leftUnit <- requireRightO "left open-circuit unit" (composeOpenCircuit leftIdentityCircuit first)
    rightUnit <- requireRightO "right open-circuit unit" (composeOpenCircuit first rightIdentityCircuit)
    leftUnitMatrix <- requireRightO "left open-circuit unit denotation" (openCircuitDenotation openPrimitiveInterpreter leftUnit)
    rightUnitMatrix <- requireRightO "right open-circuit unit denotation" (openCircuitDenotation openPrimitiveInterpreter rightUnit)
    negationMatrix <- requireRightO "negation matrix" (interpretExactCircuit openPrimitiveInterpreter negation)
    assertO "left open-circuit unit changed denotation" (stochasticEquivalent leftUnitMatrix negationMatrix)
    assertO "right open-circuit unit changed denotation" (stochasticEquivalent rightUnitMatrix negationMatrix)

    let parallel = tensorOpenCircuit first second
    parallelMatrix <- requireRightO "open tensor denotation" (openCircuitDenotation openPrimitiveInterpreter parallel)
    assertO "open tensor denotation was unsound" (stochasticEquivalent parallelMatrix (tensorStochastic negationMatrix negationMatrix))

    let reversed = reverseOpenCircuitBoundary first
        restored = restoreOpenCircuitBoundary reversed
        directedDiscard = openCircuit firstTopology (discardCircuit boolean)
        reversedDiscard :: BoundaryReversedOpenCircuit OpenPrimitive 'Deterministic Bool () WireSort String String String String GateLabel
        reversedDiscard = reverseOpenCircuitBoundary directedDiscard
        restoredDiscard = restoreOpenCircuitBoundary reversedDiscard
    assertO "open-circuit boundary restoration changed topology" (sameOpenSystemLayout (openCircuitTopology first) (openCircuitTopology restored))
    restoredMatrix <- requireRightO "restored directed denotation" (openCircuitDenotation openPrimitiveInterpreter restored)
    assertO "boundary view changed original directed denotation" (stochasticEquivalent restoredMatrix negationMatrix)
    discardMatrix <- requireRightO "restored asymmetric directed denotation" (openCircuitDenotation openPrimitiveInterpreter restoredDiscard)
    expectedDiscardMatrix <- requireRightO "directed discard denotation" (interpretExactCircuit openPrimitiveInterpreter (discardCircuit boolean))
    assertO "boundary reversal changed directed state orientation" (stochasticEquivalent discardMatrix expectedDiscardMatrix)

testOpenValidation :: IO ()
testOpenValidation = do
    case interface [("x", BitWire), ("x", BitWire)] of
        Left (DuplicateInterfacePort 1) -> pure ()
        _ -> ioError (userError "duplicate interface port was accepted")
    bit <- interfaceO [("x", BitWire)]
    control <- interfaceO [("c", ControlWire)]
    case interfaceMap bit control [("x", "c")] of
        Left (InterfaceMapTypeMismatch 0) -> pure ()
        _ -> ioError (userError "type-changing interface map was accepted")
    vertices <- interfaceO [("v", BitWire)]
    case typedHypergraph vertices [("e", FlowGate, [("missing", BitWire)], [])] of
        Left (HyperedgeInputOutsideVertices 0 0) -> pure ()
        _ -> ioError (userError "hyperedge with missing input vertex was accepted")

    sourceVertices <- interfaceO [("s-in", BitWire), ("s-out", BitWire)]
    targetVertices <- interfaceO [("t-in", BitWire), ("t-out", BitWire)]
    sourceGraph <- requireRightO "source incidence graph" (typedHypergraph sourceVertices [("se", FlowGate, [("s-in", BitWire)], [("s-out", BitWire)])])
    reversedGraph <- requireRightO "reversed incidence graph" (typedHypergraph targetVertices [("te", FlowGate, [("t-out", BitWire)], [("t-in", BitWire)])])
    case hypergraphMap sourceGraph reversedGraph [("s-in", "t-in"), ("s-out", "t-out")] [("se", "te")] of
        Left (HypergraphInputIncidenceMismatch 0) -> pure ()
        _ -> ioError (userError "hypergraph map changed ordered incidence")

    otherCommon <- interfaceO [("other", BitWire)]
    leftTarget <- interfaceO [("l", BitWire)]
    rightTarget <- interfaceO [("r", BitWire)]
    leftMap <- requireRightO "left mismatched pushout span" (interfaceMap bit leftTarget [("x", "l")])
    rightMap <- requireRightO "right mismatched pushout span" (interfaceMap otherCommon rightTarget [("other", "r")])
    case finitePushout leftMap rightMap of
        Left PushoutSpanSourceMismatch -> pure ()
        _ -> ioError (userError "pushout accepted mismatched span sources")

    first <- segment "validation-first" "x" "y"
    second <- segment "validation-second" "other" "z"
    case composeOpenSystem first second of
        Left OpenSequentialBoundaryMismatch -> pure ()
        _ -> ioError (userError "open composition accepted mismatched boundaries")
    case openSystem (openInput first) (openOutput first) (openApex first) (openOutputLeg first) (openOutputLeg first) of
        Left OpenInputLegSourceMismatch -> pure ()
        _ -> ioError (userError "open system accepted the wrong input-leg source")

    swappedInputLeg <-
        requireRightO
            "swapped target input leg"
            ( interfaceMap
                (openInput first)
                (hypergraphVertices (openApex first))
                [("x", "validation-first-out")]
            )
    swappedOutputLeg <-
        requireRightO
            "swapped target output leg"
            ( interfaceMap
                (openOutput first)
                (hypergraphVertices (openApex first))
                [("y", "validation-first-in")]
            )
    swapped <-
        requireRightO
            "swapped-boundary open system"
            (openSystem (openInput first) (openOutput first) (openApex first) swappedInputLeg swappedOutputLeg)
    case openSystemCell
        first
        swapped
        (identityInterfaceMap (openInput first))
        (identityInterfaceMap (openOutput first))
        (identityHypergraphMap (openApex first)) of
        Left OpenCellInputSquareMismatch -> pure ()
        _ -> ioError (userError "open-system cell accepted a noncommuting input square")

flattenLeftAssociated ::
    PushoutPoint (PushoutPoint String String) String ->
    [AssociativityOrigin]
flattenLeftAssociated point =
    concatMap flattenOuter (NonEmpty.toList (pushoutPointMembers point))
  where
    flattenOuter (Left inner) =
        map
            (either FirstOrigin SecondOrigin)
            (NonEmpty.toList (pushoutPointMembers inner))
    flattenOuter (Right vertex) = [ThirdOrigin vertex]

flattenRightAssociated ::
    PushoutPoint String (PushoutPoint String String) ->
    [AssociativityOrigin]
flattenRightAssociated point =
    concatMap flattenOuter (NonEmpty.toList (pushoutPointMembers point))
  where
    flattenOuter (Left vertex) = [FirstOrigin vertex]
    flattenOuter (Right inner) =
        map
            (either SecondOrigin ThirdOrigin)
            (NonEmpty.toList (pushoutPointMembers inner))

findAssociatedRight ::
    [AssociativityOrigin] ->
    [PushoutPoint String (PushoutPoint String String)] ->
    IO (PushoutPoint String (PushoutPoint String String))
findAssociatedRight requested points =
    case filter ((== requested) . flattenRightAssociated) points of
        [point] -> pure point
        _ -> ioError (userError "open associator quotient class was not unique")

findAssociatedLeft ::
    [AssociativityOrigin] ->
    [PushoutPoint (PushoutPoint String String) String] ->
    IO (PushoutPoint (PushoutPoint String String) String)
findAssociatedLeft requested points =
    case filter ((== requested) . flattenLeftAssociated) points of
        [point] -> pure point
        _ -> ioError (userError "inverse open associator quotient class was not unique")

reassociateEdge :: Either (Either String String) String -> Either String (Either String String)
reassociateEdge (Left (Left edge)) = Left edge
reassociateEdge (Left (Right edge)) = Right (Left edge)
reassociateEdge (Right edge) = Right (Right edge)

unassociateEdge :: Either String (Either String String) -> Either (Either String String) String
unassociateEdge (Left edge) = Left (Left edge)
unassociateEdge (Right (Left edge)) = Left (Right edge)
unassociateEdge (Right (Right edge)) = Right edge
