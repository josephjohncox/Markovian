{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

{- | Exact finite semantics for boundary-functional acyclic open systems.

Labels are interpreted by ordered sort signatures. Each represented edge is a
separate circuit execution. Values produced by an edge are stored once, so
multiple later reads and duplicated output observations copy that value rather
than execute the producer again. Unobserved vertices are marginalized.
-}
module Markovian.Open.Acyclic.Circuit.Exact (
    FiniteValueDomains,
    FiniteValueDomainsError (..),
    finiteValueDomains,
    finiteValueDomainEntries,
    finiteValueDomain,
    Assignment,
    AssignmentError (..),
    AssignmentObject,
    AssignmentObjectError (..),
    assignmentObject,
    assignmentObjectInterface,
    assignmentObjectValues,
    assignment,
    assignmentEntries,
    portAssignmentObject,
    LabelCircuit,
    LabelCircuitTable,
    LabelCircuitTableError (..),
    deterministicLabelCircuit,
    stochasticLabelCircuit,
    labelCircuitTable,
    labelCircuitKeys,
    AcyclicOpenCircuit,
    SomeAcyclicOpenCircuit (..),
    AcyclicOpenCircuitError (..),
    acyclicOpenCircuit,
    acyclicOpenCircuitTopology,
    acyclicOpenCircuitInputObject,
    acyclicOpenCircuitOutputObject,
    AcyclicOpenInterpretationError (..),
    acyclicOpenCircuitDenotation,
    acyclicDeterministicDenotation,
    runAcyclicOpenCircuit,
) where

import Data.Foldable (foldlM)
import Data.Kind (Type)
import Markovian.Algebra.NonNegativeRational
import Markovian.Algebra.Semiring
import Markovian.Category.Finite.Set
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Deterministic
import Markovian.Category.Matrix.Stochastic
import Markovian.Circuit
import Markovian.Circuit.Interpret.Exact
import Markovian.Open.Acyclic
import Markovian.Open.Hypergraph
import Markovian.Open.Interface
import Markovian.Open.StructuredCospan
import Markovian.Probability.Exact

-- | Finite value carriers keyed by represented runtime sorts.
type role FiniteValueDomains nominal nominal

newtype FiniteValueDomains sort value
    = UnsafeFiniteValueDomains [(sort, FiniteSet value)]

-- | Duplicate sort keys are diagnosed by entry index.
newtype FiniteValueDomainsError
    = DuplicateValueDomainSort Int
    deriving (Eq, Show)

-- | Construct finite value domains. Empty carriers are legal.
finiteValueDomains ::
    (Eq sort, Eq value) =>
    [(sort, FiniteSet value)] ->
    Either FiniteValueDomainsError (FiniteValueDomains sort value)
finiteValueDomains entries =
    case firstDuplicateKey 0 [] entries of
        Nothing -> Right (UnsafeFiniteValueDomains entries)
        Just index -> Left (DuplicateValueDomainSort index)

-- | Read domain entries in represented sort-table order.
finiteValueDomainEntries ::
    FiniteValueDomains sort value ->
    [(sort, FiniteSet value)]
finiteValueDomainEntries (UnsafeFiniteValueDomains entries) = entries

-- | Look up one represented sort carrier.
finiteValueDomain ::
    (Eq sort) =>
    FiniteValueDomains sort value ->
    sort ->
    Maybe (FiniteSet value)
finiteValueDomain (UnsafeFiniteValueDomains entries) requested = lookup requested entries

-- | A named finite assignment. Equality ignores entry layout.
type role Assignment nominal nominal

newtype Assignment port value = UnsafeAssignment [(port, value)]

instance (Eq port, Eq value) => Eq (Assignment port value) where
    UnsafeAssignment left == UnsafeAssignment right =
        length left == length right
            && all (\(port, value) -> lookup port right == Just value) left

instance (Show port, Show value) => Show (Assignment port value) where
    showsPrec precedence (UnsafeAssignment entries) =
        showParen (precedence > 10) $ showString "Assignment " . shows entries

-- | Assignment validation failure.
data AssignmentError
    = AssignmentDomainMismatch
    | AssignmentValueOutsideDomain !Int
    | AssignmentObjectMismatch
    | AssignmentInternalInvariantFailure
    deriving (Eq, Show)

-- | A typed interface and the exact finite set of all its assignments.
type role AssignmentObject nominal nominal nominal

data AssignmentObject sort port value where
    UnsafeAssignmentObject ::
        (Eq sort, Eq port, Eq value) =>
        !(Interface sort port) ->
        ![(port, FiniteSet value)] ->
        !(FiniteSet (Assignment port value)) ->
        AssignmentObject sort port value

-- | Assignment-object construction failure.
data AssignmentObjectError sort
    = AssignmentObjectMissingDomain !Int !sort
    | AssignmentObjectInternalInvariantFailure
    deriving (Eq, Show)

-- | Enumerate all assignments in interface layout and domain layout order.
assignmentObject ::
    (Eq sort, Eq port, Eq value) =>
    FiniteValueDomains sort value ->
    Interface sort port ->
    Either (AssignmentObjectError sort) (AssignmentObject sort port value)
assignmentObject domains object = do
    coordinateDomains <- traverse coordinate (zip [0 ..] (interfaceEntries object))
    represented <-
        case finiteSet (map UnsafeAssignment (cartesianEntries coordinateDomains)) of
            Left _ -> Left AssignmentObjectInternalInvariantFailure
            Right values -> Right values
    Right (UnsafeAssignmentObject object coordinateDomains represented)
  where
    coordinate (index, (port, portSort)) =
        case finiteValueDomain domains portSort of
            Nothing -> Left (AssignmentObjectMissingDomain index portSort)
            Just values -> Right (port, values)

-- | Read the represented typed interface.
assignmentObjectInterface ::
    AssignmentObject sort port value ->
    Interface sort port
assignmentObjectInterface (UnsafeAssignmentObject object _ _) = object

-- | Read all represented assignments.
assignmentObjectValues ::
    AssignmentObject sort port value ->
    FiniteSet (Assignment port value)
assignmentObjectValues (UnsafeAssignmentObject _ _ values) = values

-- | Validate one total named assignment and canonicalize its entry order.
assignment ::
    (Eq port, Eq value) =>
    AssignmentObject sort port value ->
    [(port, value)] ->
    Either AssignmentError (Assignment port value)
assignment (UnsafeAssignmentObject object domains _) entries
    | not (validAssignmentDomain (map fst (interfaceEntries object)) entries) =
        Left AssignmentDomainMismatch
    | otherwise = UnsafeAssignment <$> traverse canonicalEntry (zip [0 ..] domains)
  where
    canonicalEntry (index, (port, values)) =
        case lookup port entries of
            Nothing -> Left AssignmentInternalInvariantFailure
            Just value
                | value `elem` finiteSetValues values -> Right (port, value)
                | otherwise -> Left (AssignmentValueOutsideDomain index)

-- | Reindex a valid assignment into the supplied object's interface layout.
assignmentEntries ::
    (Eq port, Eq value) =>
    AssignmentObject sort port value ->
    Assignment port value ->
    Either AssignmentError [(port, value)]
assignmentEntries (UnsafeAssignmentObject object domains _) (UnsafeAssignment entries)
    | not (validAssignmentDomain (map fst (interfaceEntries object)) entries) =
        Left AssignmentObjectMismatch
    | otherwise = traverse canonicalEntry (zip [0 ..] domains)
  where
    canonicalEntry (index, (port, values)) =
        case lookup port entries of
            Nothing -> Left AssignmentObjectMismatch
            Just value
                | value `elem` finiteSetValues values -> Right (port, value)
                | otherwise -> Left (AssignmentValueOutsideDomain index)

-- | Assignment object for positional edge ports @0 .. arity - 1@.
portAssignmentObject ::
    (Eq sort, Eq value) =>
    FiniteValueDomains sort value ->
    [sort] ->
    Either (AssignmentObjectError sort) (AssignmentObject sort Int value)
portAssignmentObject domains signature =
    case interface (zip [0 ..] signature) of
        Left _ -> Left AssignmentObjectInternalInvariantFailure
        Right object -> assignmentObject domains object

-- | One hidden label/signature/circuit entry.
data
    LabelCircuit
        (primitive :: Purity -> Type -> Type -> Type)
        sort
        label
        value
    where
    UnsafeDeterministicLabelCircuit ::
        !label ->
        ![sort] ->
        ![sort] ->
        !(Circuit primitive 'Deterministic (Assignment Int value) (Assignment Int value)) ->
        LabelCircuit primitive sort label value
    UnsafeStochasticLabelCircuit ::
        !label ->
        ![sort] ->
        ![sort] ->
        !(Circuit primitive 'Stochastic (Assignment Int value) (Assignment Int value)) ->
        LabelCircuit primitive sort label value

-- | Add a deterministic local interpretation.
deterministicLabelCircuit ::
    label ->
    [sort] ->
    [sort] ->
    Circuit primitive 'Deterministic (Assignment Int value) (Assignment Int value) ->
    LabelCircuit primitive sort label value
deterministicLabelCircuit = UnsafeDeterministicLabelCircuit

-- | Add a stochastic local interpretation.
stochasticLabelCircuit ::
    label ->
    [sort] ->
    [sort] ->
    Circuit primitive 'Stochastic (Assignment Int value) (Assignment Int value) ->
    LabelCircuit primitive sort label value
stochasticLabelCircuit = UnsafeStochasticLabelCircuit

-- | Opaque table keyed by label and both ordered signatures.
type role LabelCircuitTable nominal nominal nominal nominal

newtype LabelCircuitTable primitive sort label value
    = UnsafeLabelCircuitTable [LabelCircuit primitive sort label value]

-- | Duplicate key indices.
data LabelCircuitTableError
    = DuplicateLabelCircuitKey !Int !Int
    deriving (Eq, Show)

-- | Validate key uniqueness in entry order.
labelCircuitTable ::
    (Eq sort, Eq label) =>
    [LabelCircuit primitive sort label value] ->
    Either LabelCircuitTableError (LabelCircuitTable primitive sort label value)
labelCircuitTable entries = go 0 [] entries
  where
    go _ _ [] = Right (UnsafeLabelCircuitTable entries)
    go index seen (entry : remaining) =
        case keyIndex (labelCircuitKey entry) seen of
            Just firstIndex -> Left (DuplicateLabelCircuitKey firstIndex index)
            Nothing -> go (index + 1) (seen ++ [labelCircuitKey entry]) remaining

-- | Read table keys without exposing circuits.
labelCircuitKeys ::
    LabelCircuitTable primitive sort label value ->
    [(label, [sort], [sort])]
labelCircuitKeys (UnsafeLabelCircuitTable entries) = map labelCircuitKey entries

-- | Opaque purity-refined finite DAG network.
type role AcyclicOpenCircuit nominal nominal nominal nominal nominal nominal nominal nominal nominal

data AcyclicOpenCircuit primitive purity sort input output vertex edge label value where
    UnsafeDeterministicAcyclicOpenCircuit ::
        !(AcyclicOpenSystem sort input output vertex edge label) ->
        !(AssignmentObject sort input value) ->
        !(AssignmentObject sort output value) ->
        !(ApexValueDomains vertex value) ->
        ![SelectedCircuit primitive sort vertex edge value] ->
        AcyclicOpenCircuit primitive 'Deterministic sort input output vertex edge label value
    UnsafeStochasticAcyclicOpenCircuit ::
        !(AcyclicOpenSystem sort input output vertex edge label) ->
        !(AssignmentObject sort input value) ->
        !(AssignmentObject sort output value) ->
        !(ApexValueDomains vertex value) ->
        ![SelectedCircuit primitive sort vertex edge value] ->
        AcyclicOpenCircuit primitive 'Stochastic sort input output vertex edge label value

data ApexValueDomains vertex value where
    UnsafeApexValueDomains ::
        (Eq vertex, Eq value) =>
        ![(vertex, FiniteSet value)] ->
        ApexValueDomains vertex value

-- | Existential aggregate purity returned by the smart constructor.
data SomeAcyclicOpenCircuit primitive sort input output vertex edge label value where
    SomeAcyclicOpenCircuit ::
        SPurity purity ->
        AcyclicOpenCircuit primitive purity sort input output vertex edge label value ->
        SomeAcyclicOpenCircuit primitive sort input output vertex edge label value

data SelectedCircuit primitive sort vertex edge value where
    SelectedDeterministicCircuit ::
        !Int ->
        !edge ->
        ![(vertex, sort)] ->
        ![(vertex, sort)] ->
        !(Circuit primitive 'Deterministic (Assignment Int value) (Assignment Int value)) ->
        SelectedCircuit primitive sort vertex edge value
    SelectedStochasticCircuit ::
        !Int ->
        !edge ->
        ![(vertex, sort)] ->
        ![(vertex, sort)] ->
        !(Circuit primitive 'Stochastic (Assignment Int value) (Assignment Int value)) ->
        SelectedCircuit primitive sort vertex edge value

-- | Network construction failure with edge and layout context.
data AcyclicOpenCircuitError sort edge label
    = AcyclicCircuitMissingValueDomain !Int !sort
    | AcyclicCircuitMissingLabelInterpretation !Int !edge !label ![sort] ![sort]
    | AcyclicCircuitLabelSourceMismatch !Int !edge
    | AcyclicCircuitLabelTargetMismatch !Int !edge
    | AcyclicCircuitObjectFailure
    | AcyclicCircuitInternalInvariantFailure
    deriving (Eq, Show)

-- | Resolve every represented edge and compute aggregate purity.
acyclicOpenCircuit ::
    ( Eq sort
    , Eq input
    , Eq output
    , Eq vertex
    , Eq edge
    , Eq label
    , Eq value
    ) =>
    AcyclicOpenSystem sort input output vertex edge label ->
    FiniteValueDomains sort value ->
    LabelCircuitTable primitive sort label value ->
    Either
        (AcyclicOpenCircuitError sort edge label)
        (SomeAcyclicOpenCircuit primitive sort input output vertex edge label value)
acyclicOpenCircuit topology domains table = do
    apexDomains <- traverse apexDomain (zip [0 ..] (interfaceEntries apexVertices))
    inputObject <- mapObjectFailure (assignmentObject domains (openInput system))
    outputObject <- mapObjectFailure (assignmentObject domains (openOutput system))
    selectedByLayout <- traverse selectEdge (zip [0 ..] (hypergraphEdges apex))
    selected <- traverse (selectedForOrder selectedByLayout) (finiteSetValues (acyclicTopologicalOrder topology))
    if any selectedIsStochastic selected
        then
            Right
                ( SomeAcyclicOpenCircuit
                    SStochastic
                    (UnsafeStochasticAcyclicOpenCircuit topology inputObject outputObject (UnsafeApexValueDomains apexDomains) selected)
                )
        else
            Right
                ( SomeAcyclicOpenCircuit
                    SDeterministic
                    (UnsafeDeterministicAcyclicOpenCircuit topology inputObject outputObject (UnsafeApexValueDomains apexDomains) selected)
                )
  where
    system = forgetAcyclicOpenSystem topology
    apex = openApex system
    apexVertices = hypergraphVertices apex

    apexDomain (index, (vertex, vertexSort)) =
        case finiteValueDomain domains vertexSort of
            Nothing -> Left (AcyclicCircuitMissingValueDomain index vertexSort)
            Just values -> Right (vertex, values)

    selectEdge (edgeIndex, (edgeId, typedEdge)) = do
        let inputSignature = map snd (hyperedgeInputs typedEdge)
            outputSignature = map snd (hyperedgeOutputs typedEdge)
            key = (hyperedgeLabel typedEdge, inputSignature, outputSignature)
        entry <-
            case lookupLabelCircuit key table of
                Nothing ->
                    Left
                        ( AcyclicCircuitMissingLabelInterpretation
                            edgeIndex
                            edgeId
                            (hyperedgeLabel typedEdge)
                            inputSignature
                            outputSignature
                        )
                Just found -> Right found
        inputObject <- mapObjectFailure (portAssignmentObject domains inputSignature)
        outputObject <- mapObjectFailure (portAssignmentObject domains outputSignature)
        checkSelectedEndpoints
            edgeIndex
            edgeId
            (hyperedgeInputs typedEdge)
            (hyperedgeOutputs typedEdge)
            inputObject
            outputObject
            entry

-- | Read validated topology.
acyclicOpenCircuitTopology ::
    AcyclicOpenCircuit primitive purity sort input output vertex edge label value ->
    AcyclicOpenSystem sort input output vertex edge label
acyclicOpenCircuitTopology circuit =
    case circuit of
        UnsafeDeterministicAcyclicOpenCircuit topology _ _ _ _ -> topology
        UnsafeStochasticAcyclicOpenCircuit topology _ _ _ _ -> topology

-- | Read the exact finite input-assignment object.
acyclicOpenCircuitInputObject ::
    AcyclicOpenCircuit primitive purity sort input output vertex edge label value ->
    AssignmentObject sort input value
acyclicOpenCircuitInputObject circuit =
    case circuit of
        UnsafeDeterministicAcyclicOpenCircuit _ object _ _ _ -> object
        UnsafeStochasticAcyclicOpenCircuit _ object _ _ _ -> object

-- | Read the exact finite output-assignment object.
acyclicOpenCircuitOutputObject ::
    AcyclicOpenCircuit primitive purity sort input output vertex edge label value ->
    AssignmentObject sort output value
acyclicOpenCircuitOutputObject circuit =
    case circuit of
        UnsafeDeterministicAcyclicOpenCircuit _ _ object _ _ -> object
        UnsafeStochasticAcyclicOpenCircuit _ _ object _ _ -> object

-- | Exact local or global interpretation failure.
data AcyclicOpenInterpretationError edge primitiveError
    = AcyclicEdgeExactError !Int !edge !(ExactCircuitInterpretationError primitiveError)
    | AcyclicStructuralCircuitError !CircuitConstructionError
    | AcyclicMatrixCompositionError !MatrixError
    | AcyclicNormalizationInvariantFailure !(StochasticMatrixError NonNegativeRational)
    | AcyclicInputOutsideSource
    | AcyclicInterpretationInternalInvariantFailure
    deriving (Eq, Show)

{- | Exact finite live-frontier denotation. Values are discarded immediately
when no remaining edge or output observation can read them. No renormalization
is performed.
-}
acyclicOpenCircuitDenotation ::
    ExactPrimitiveInterpreter primitive primitiveError ->
    AcyclicOpenCircuit primitive purity sort input output vertex edge label value ->
    Either
        (AcyclicOpenInterpretationError edge primitiveError)
        (StochasticMatrix NonNegativeRational (Assignment input value) (Assignment output value))
acyclicOpenCircuitDenotation interpreter circuit = do
    interpreted <- traverse (interpretSelected interpreter) selected
    raw <-
        topologicalMatrix
            topology
            inputObject
            (assignmentObjectValues inputObject)
            outputObject
            apexDomains
            interpreted
    normalizeTopological raw
  where
    (topology, inputObject, outputObject, apexDomains, selected) = circuitParts circuit

-- | Retain one-hot evidence for a deterministically constructed network.
acyclicDeterministicDenotation ::
    ExactPrimitiveInterpreter primitive primitiveError ->
    AcyclicOpenCircuit primitive 'Deterministic sort input output vertex edge label value ->
    Either
        (AcyclicOpenInterpretationError edge primitiveError)
        (DeterministicMatrix NonNegativeRational (Assignment input value) (Assignment output value))
acyclicDeterministicDenotation interpreter circuit = do
    arrow <- acyclicOpenCircuitDenotation interpreter circuit
    case deterministicMatrix (forgetStochastic arrow) of
        Left _ -> Left AcyclicInterpretationInternalInvariantFailure
        Right deterministic -> Right deterministic

{- | Apply one represented input without constructing rows for other inputs.
The output support is still enumerated because the result is an exact finite
distribution.
-}
runAcyclicOpenCircuit ::
    (Eq input, Eq value) =>
    ExactPrimitiveInterpreter primitive primitiveError ->
    AcyclicOpenCircuit primitive purity sort input output vertex edge label value ->
    Assignment input value ->
    Either
        (AcyclicOpenInterpretationError edge primitiveError)
        (ExactFiniteDist (Assignment output value))
runAcyclicOpenCircuit interpreter circuit inputValue = do
    if inputValue `elem` finiteSetValues (assignmentObjectValues inputObject)
        then pure ()
        else Left AcyclicInputOutsideSource
    singletonSource <-
        case finiteSet [inputValue] of
            Left _ -> Left AcyclicInterpretationInternalInvariantFailure
            Right represented -> Right represented
    interpreted <- traverse (interpretSelected interpreter) selected
    raw <-
        topologicalMatrix
            topology
            inputObject
            singletonSource
            outputObject
            apexDomains
            interpreted
    arrow <- normalizeTopological raw
    case exactFiniteDist
        [ (outputValue, getNonNegativeRational mass)
        | outputValue <- finiteSetValues (stochasticTarget arrow)
        , Just mass <- [matrixEntry (forgetStochastic arrow) inputValue outputValue]
        ] of
        Left _ -> Left AcyclicInterpretationInternalInvariantFailure
        Right distribution -> Right distribution
  where
    (topology, inputObject, outputObject, apexDomains, selected) = circuitParts circuit

data InterpretedEdge sort vertex edge value
    = InterpretedEdge
        !Int
        !edge
        ![(vertex, sort)]
        ![(vertex, sort)]
        !(StochasticMatrix NonNegativeRational (Assignment Int value) (Assignment Int value))

interpretSelected ::
    ExactPrimitiveInterpreter primitive primitiveError ->
    SelectedCircuit primitive sort vertex edge value ->
    Either
        (AcyclicOpenInterpretationError edge primitiveError)
        (InterpretedEdge sort vertex edge value)
interpretSelected interpreter selected =
    case selected of
        SelectedDeterministicCircuit edgeIndex edgeId inputs outputs circuit -> do
            arrow <- mapEdgeError edgeIndex edgeId (interpretExactCircuit interpreter circuit)
            Right (InterpretedEdge edgeIndex edgeId inputs outputs arrow)
        SelectedStochasticCircuit edgeIndex edgeId inputs outputs circuit -> do
            arrow <- mapEdgeError edgeIndex edgeId (interpretExactCircuit interpreter circuit)
            Right (InterpretedEdge edgeIndex edgeId inputs outputs arrow)

topologicalMatrix ::
    AcyclicOpenSystem sort input output vertex edge label ->
    AssignmentObject sort input value ->
    FiniteSet (Assignment input value) ->
    AssignmentObject sort output value ->
    ApexValueDomains vertex value ->
    [InterpretedEdge sort vertex edge value] ->
    Either
        (AcyclicOpenInterpretationError edge primitiveError)
        (Matrix NonNegativeRational (Assignment input value) (Assignment output value))
topologicalMatrix
    topology
    UnsafeAssignmentObject{}
    source
    outputObject@UnsafeAssignmentObject{}
    (UnsafeApexValueDomains apexDomains)
    interpreted = do
        producedByInput <- traverse inputVertex (interfaceMapEntries inputLeg)
        let initialVertices = retainNeeded producedByInput interpreted
        initialSupport <- supportFor initialVertices
        initialization <-
            checkedMatrixFromFunction source initialSupport $ \inputValue valuation -> do
                matches <- initialBoundaryMatches initialVertices inputValue valuation
                Right (if matches then one else zero)
        (steps, finalSupport) <- buildSteps initialVertices initialSupport interpreted
        evolved <- foldlM composeChecked initialization steps
        observation <-
            checkedMatrixFromFunction finalSupport target $ \valuation outputValue -> do
                matches <- boundaryMatches outputLeg outputValue valuation
                Right (if matches then one else zero)
        composeChecked evolved observation
      where
        system = forgetAcyclicOpenSystem topology
        inputLeg = openInputLeg system
        outputLeg = openOutputLeg system
        target = assignmentObjectValues outputObject
        observedVertices = map snd (interfaceMapEntries outputLeg)

        inputVertex (_, vertex) = Right vertex

        supportFor representedVertices =
            case finiteSet
                ( map
                    UnsafeAssignment
                    ( cartesianEntries
                        [ (vertex, values)
                        | (vertex, values) <- apexDomains
                        , vertex `elem` representedVertices
                        ]
                    )
                ) of
                Left _ -> Left AcyclicInterpretationInternalInvariantFailure
                Right support -> Right support

        neededAfter remaining =
            distinctValues
                ( observedVertices
                    ++ concatMap interpretedInputVertices remaining
                )

        retainNeeded representedVertices remaining =
            [ vertex
            | vertex <- representedVertices
            , vertex `elem` neededAfter remaining
            ]

        initialBoundaryMatches representedVertices (UnsafeAssignment inputEntries) (UnsafeAssignment valuationEntries) =
            allM
                matches
                [ (port, vertex)
                | (port, vertex) <- interfaceMapEntries inputLeg
                , vertex `elem` representedVertices
                ]
          where
            matches (port, vertex) =
                case (lookup port inputEntries, lookup vertex valuationEntries) of
                    (Just inputValue, Just vertexValue) -> Right (inputValue == vertexValue)
                    _ -> Left AcyclicInterpretationInternalInvariantFailure

        buildSteps _ representedSupport [] =
            Right ([], representedSupport)
        buildSteps representedVertices representedSupport (edge@(InterpretedEdge _ _ _ outputs _) : remaining) = do
            let needed = neededAfter remaining
                retainedVertices =
                    [ vertex
                    | vertex <- representedVertices
                    , vertex `elem` needed
                    ]
                retainedOutputs =
                    [ vertex
                    | (vertex, _) <- outputs
                    , vertex `elem` needed
                    ]
                nextVertices = retainedVertices ++ retainedOutputs
            nextSupport <- supportFor nextVertices
            step <- edgeStep nextVertices representedSupport nextSupport edge
            (otherSteps, finalSupport) <- buildSteps nextVertices nextSupport remaining
            Right (step : otherSteps, finalSupport)

        edgeStep nextVertices previousSupport nextSupport (InterpretedEdge _ _ inputs outputs arrow) =
            checkedMatrixFromFunction previousSupport nextSupport $ \previous next -> do
                preserved <- preservesRetained nextVertices previous next
                if preserved
                    then do
                        localInput <- restrictValuation previous inputs
                        foldlM
                            (addCompatibleOutput next outputs arrow localInput)
                            zero
                            (finiteSetValues (stochasticTarget arrow))
                    else Right zero

        preservesRetained nextVertices (UnsafeAssignment previous) (UnsafeAssignment next) =
            allM
                ( \(vertex, value) ->
                    Right
                        ( vertex `notElem` nextVertices
                            || lookup vertex next == Just value
                        )
                )
                previous

        addCompatibleOutput next outputs arrow localInput total localOutput = do
            compatible <- outputCompatible next outputs localOutput
            if compatible
                then case matrixEntry (forgetStochastic arrow) localInput localOutput of
                    Nothing -> Left AcyclicInterpretationInternalInvariantFailure
                    Just mass -> Right (total `plus` mass)
                else Right total

        outputCompatible (UnsafeAssignment next) outputs (UnsafeAssignment localOutput) =
            allM matches (zip [0 ..] outputs)
          where
            matches (position, (vertex, _)) =
                case lookup vertex next of
                    Nothing -> Right True
                    Just nextValue ->
                        case lookup position localOutput of
                            Nothing -> Left AcyclicInterpretationInternalInvariantFailure
                            Just outputValue -> Right (outputValue == nextValue)

        composeChecked left right =
            case composeMatrix left right of
                Left problem -> Left (AcyclicMatrixCompositionError problem)
                Right composite -> Right composite

interpretedInputVertices :: InterpretedEdge sort vertex edge value -> [vertex]
interpretedInputVertices (InterpretedEdge _ _ inputs _ _) = map fst inputs

normalizeTopological ::
    Matrix NonNegativeRational source target ->
    Either
        (AcyclicOpenInterpretationError edge primitiveError)
        (StochasticMatrix NonNegativeRational source target)
normalizeTopological raw =
    case stochasticMatrix raw of
        Left problem -> Left (AcyclicNormalizationInvariantFailure problem)
        Right normalized -> Right normalized

boundaryMatches ::
    (Eq boundary, Eq vertex, Eq value) =>
    InterfaceMap sort boundary vertex ->
    Assignment boundary value ->
    Assignment vertex value ->
    Either (AcyclicOpenInterpretationError edge primitiveError) Bool
boundaryMatches leg (UnsafeAssignment boundaryEntries) (UnsafeAssignment vertexEntries) =
    allM matches (interfaceMapEntries leg)
  where
    matches (port, vertex) =
        case (lookup port boundaryEntries, lookup vertex vertexEntries) of
            (Just boundaryValue, Just vertexValue) -> Right (boundaryValue == vertexValue)
            _ -> Left AcyclicInterpretationInternalInvariantFailure

restrictValuation ::
    (Eq vertex) =>
    Assignment vertex value ->
    [(vertex, sort)] ->
    Either
        (AcyclicOpenInterpretationError edge primitiveError)
        (Assignment Int value)
restrictValuation (UnsafeAssignment valuation) ports =
    UnsafeAssignment <$> traverse coordinate (zip [0 ..] ports)
  where
    coordinate (position, (vertex, _)) =
        case lookup vertex valuation of
            Nothing -> Left AcyclicInterpretationInternalInvariantFailure
            Just value -> Right (position, value)

circuitParts ::
    AcyclicOpenCircuit primitive purity sort input output vertex edge label value ->
    ( AcyclicOpenSystem sort input output vertex edge label
    , AssignmentObject sort input value
    , AssignmentObject sort output value
    , ApexValueDomains vertex value
    , [SelectedCircuit primitive sort vertex edge value]
    )
circuitParts circuit =
    case circuit of
        UnsafeDeterministicAcyclicOpenCircuit topology input output apex selected ->
            (topology, input, output, apex, selected)
        UnsafeStochasticAcyclicOpenCircuit topology input output apex selected ->
            (topology, input, output, apex, selected)

checkSelectedEndpoints ::
    (Eq value) =>
    Int ->
    edge ->
    [(vertex, sort)] ->
    [(vertex, sort)] ->
    AssignmentObject sort Int value ->
    AssignmentObject sort Int value ->
    LabelCircuit primitive sort label value ->
    Either
        (AcyclicOpenCircuitError sort edge label)
        (edge, SelectedCircuit primitive sort vertex edge value)
checkSelectedEndpoints edgeIndex edgeId inputs outputs inputObject outputObject entry =
    case entry of
        UnsafeDeterministicLabelCircuit _ _ _ circuit -> do
            validateCircuitEndpoints edgeIndex edgeId inputObject outputObject circuit
            Right
                ( edgeId
                , SelectedDeterministicCircuit edgeIndex edgeId inputs outputs circuit
                )
        UnsafeStochasticLabelCircuit _ _ _ circuit -> do
            validateCircuitEndpoints edgeIndex edgeId inputObject outputObject circuit
            Right
                ( edgeId
                , SelectedStochasticCircuit edgeIndex edgeId inputs outputs circuit
                )

validateCircuitEndpoints ::
    (Eq value) =>
    Int ->
    edge ->
    AssignmentObject sort Int value ->
    AssignmentObject sort Int value ->
    Circuit primitive purity (Assignment Int value) (Assignment Int value) ->
    Either (AcyclicOpenCircuitError sort edge label) ()
validateCircuitEndpoints edgeIndex edgeId inputObject outputObject circuit
    | not (sameFiniteSet (circuitSource circuit) (assignmentObjectValues inputObject)) =
        Left (AcyclicCircuitLabelSourceMismatch edgeIndex edgeId)
    | not (sameFiniteSet (circuitTarget circuit) (assignmentObjectValues outputObject)) =
        Left (AcyclicCircuitLabelTargetMismatch edgeIndex edgeId)
    | otherwise = Right ()

selectedForOrder ::
    (Eq edge) =>
    [(edge, SelectedCircuit primitive sort vertex edge value)] ->
    edge ->
    Either
        (AcyclicOpenCircuitError sort edge label)
        (SelectedCircuit primitive sort vertex edge value)
selectedForOrder selected requested =
    case lookup requested selected of
        Nothing -> Left AcyclicCircuitInternalInvariantFailure
        Just circuit -> Right circuit

selectedIsStochastic :: SelectedCircuit primitive sort vertex edge value -> Bool
selectedIsStochastic selected =
    case selected of
        SelectedDeterministicCircuit{} -> False
        SelectedStochasticCircuit{} -> True

lookupLabelCircuit ::
    (Eq sort, Eq label) =>
    (label, [sort], [sort]) ->
    LabelCircuitTable primitive sort label value ->
    Maybe (LabelCircuit primitive sort label value)
lookupLabelCircuit requested (UnsafeLabelCircuitTable entries) = go entries
  where
    go [] = Nothing
    go (entry : remaining)
        | labelCircuitKey entry == requested = Just entry
        | otherwise = go remaining

labelCircuitKey :: LabelCircuit primitive sort label value -> (label, [sort], [sort])
labelCircuitKey entry =
    case entry of
        UnsafeDeterministicLabelCircuit label inputs outputs _ -> (label, inputs, outputs)
        UnsafeStochasticLabelCircuit label inputs outputs _ -> (label, inputs, outputs)

keyIndex :: (Eq key) => key -> [key] -> Maybe Int
keyIndex requested = go 0
  where
    go _ [] = Nothing
    go index (key : remaining)
        | key == requested = Just index
        | otherwise = go (index + 1) remaining

firstDuplicateKey ::
    (Eq sort) =>
    Int ->
    [sort] ->
    [(sort, value)] ->
    Maybe Int
firstDuplicateKey _ _ [] = Nothing
firstDuplicateKey index seen ((key, _) : remaining)
    | key `elem` seen = Just index
    | otherwise = firstDuplicateKey (index + 1) (seen ++ [key]) remaining

cartesianEntries :: [(port, FiniteSet value)] -> [[(port, value)]]
cartesianEntries [] = [[]]
cartesianEntries ((port, values) : remaining) =
    [ (port, value) : suffix
    | value <- finiteSetValues values
    , suffix <- cartesianEntries remaining
    ]

validAssignmentDomain ::
    (Eq port) =>
    [port] ->
    [(port, value)] ->
    Bool
validAssignmentDomain ports entries =
    length ports == length entries
        && all (\port -> count port == 1) ports
        && all (\(port, _) -> port `elem` ports) entries
  where
    count requested = length [() | (port, _) <- entries, port == requested]

checkedMatrixFromFunction ::
    FiniteSet source ->
    FiniteSet target ->
    (source -> target -> Either (AcyclicOpenInterpretationError edge primitiveError) NonNegativeRational) ->
    Either
        (AcyclicOpenInterpretationError edge primitiveError)
        (Matrix NonNegativeRational source target)
checkedMatrixFromFunction source target entry = do
    rows <-
        traverse
            (\sourceValue -> traverse (entry sourceValue) (finiteSetValues target))
            (finiteSetValues source)
    case matrixFromRows source target rows of
        Left problem -> Left (AcyclicMatrixCompositionError problem)
        Right matrix -> Right matrix

distinctValues :: (Eq value) => [value] -> [value]
distinctValues = go []
  where
    go _ [] = []
    go seen (value : remaining)
        | value `elem` seen = go seen remaining
        | otherwise = value : go (seen ++ [value]) remaining

allM :: (value -> Either error Bool) -> [value] -> Either error Bool
allM _ [] = Right True
allM predicate (value : remaining) = do
    accepted <- predicate value
    if accepted then allM predicate remaining else Right False

mapObjectFailure ::
    Either (AssignmentObjectError sort) value ->
    Either (AcyclicOpenCircuitError sort edge label) value
mapObjectFailure = either (const (Left AcyclicCircuitObjectFailure)) Right

mapEdgeError ::
    Int ->
    edge ->
    Either (ExactCircuitInterpretationError primitiveError) value ->
    Either (AcyclicOpenInterpretationError edge primitiveError) value
mapEdgeError edgeIndex edgeId = either (Left . AcyclicEdgeExactError edgeIndex edgeId) Right
