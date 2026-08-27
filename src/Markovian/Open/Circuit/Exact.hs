{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

{- | Circuit-decorated structured cospans with exact directed denotation.

The decoration supplies the only implemented stochastic denotation. Internal
hypergraph labels are syntax and are not black-boxed. Boundary reversal retains
the original directed decoration but deliberately exposes no reverse stochastic
denotation.
-}
module Markovian.Open.Circuit.Exact (
    OpenCircuit,
    OpenCircuitError (..),
    openCircuit,
    openCircuitTopology,
    openCircuitDecoration,
    composeOpenCircuit,
    tensorOpenCircuit,
    openCircuitDenotation,
    runOpenCircuit,
    BoundaryReversedOpenCircuit,
    reverseOpenCircuitBoundary,
    reversedOpenTopology,
    restoreOpenCircuitBoundary,
) where

import Markovian.Algebra.NonNegativeRational (NonNegativeRational)
import Markovian.Category.Matrix.Stochastic (StochasticMatrix)
import Markovian.Circuit
import Markovian.Circuit.Interpret.Exact
import Markovian.Open.Pushout (PushoutPoint)
import Markovian.Open.StructuredCospan
import Markovian.Probability.Exact (ExactFiniteDist)

-- | A structured cospan carrying one directed circuit decoration.
type role OpenCircuit nominal nominal nominal nominal nominal nominal nominal nominal nominal nominal

data
    OpenCircuit
        primitive
        purity
        stateInput
        stateOutput
        sort
        input
        output
        vertex
        edge
        label
    where
    UnsafeOpenCircuit ::
        !(OpenSystem sort input output vertex edge label) ->
        !(Circuit primitive purity stateInput stateOutput) ->
        OpenCircuit primitive purity stateInput stateOutput sort input output vertex edge label

-- | Open-circuit gluing or decoration failure.
data OpenCircuitError
    = OpenCircuitTopologyError !OpenSystemError
    | OpenCircuitDecorationError !CircuitConstructionError
    deriving (Eq, Show)

-- | Attach a directed circuit decoration to validated open topology.
openCircuit ::
    OpenSystem sort input output vertex edge label ->
    Circuit primitive purity stateInput stateOutput ->
    OpenCircuit primitive purity stateInput stateOutput sort input output vertex edge label
openCircuit = UnsafeOpenCircuit

-- | Read structured-cospan topology.
openCircuitTopology ::
    OpenCircuit primitive purity stateInput stateOutput sort input output vertex edge label ->
    OpenSystem sort input output vertex edge label
openCircuitTopology (UnsafeOpenCircuit topology _) = topology

-- | Read the directed circuit decoration.
openCircuitDecoration ::
    OpenCircuit primitive purity stateInput stateOutput sort input output vertex edge label ->
    Circuit primitive purity stateInput stateOutput
openCircuitDecoration (UnsafeOpenCircuit _ decoration) = decoration

-- | Sequential pushout gluing and directed circuit composition.
composeOpenCircuit ::
    ( Eq sort
    , Eq middle
    , Eq leftVertex
    , Eq rightVertex
    , Eq leftEdge
    , Eq rightEdge
    , Eq label
    ) =>
    OpenCircuit primitive leftPurity stateInput stateMiddle sort input middle leftVertex leftEdge label ->
    OpenCircuit primitive rightPurity stateMiddle stateOutput sort middle output rightVertex rightEdge label ->
    Either
        OpenCircuitError
        ( OpenCircuit
            primitive
            (JoinPurity leftPurity rightPurity)
            stateInput
            stateOutput
            sort
            input
            output
            (PushoutPoint leftVertex rightVertex)
            (Either leftEdge rightEdge)
            label
        )
composeOpenCircuit left right = do
    topology <-
        either
            (Left . OpenCircuitTopologyError)
            Right
            (composeOpenSystem (openCircuitTopology left) (openCircuitTopology right))
    decoration <-
        either
            (Left . OpenCircuitDecorationError)
            Right
            (composeCircuit (openCircuitDecoration left) (openCircuitDecoration right))
    Right (UnsafeOpenCircuit topology decoration)

-- | Parallel disjoint-union topology and independent circuit tensor.
tensorOpenCircuit ::
    OpenCircuit primitive leftPurity leftStateInput leftStateOutput sort leftInput leftOutput leftVertex leftEdge label ->
    OpenCircuit primitive rightPurity rightStateInput rightStateOutput sort rightInput rightOutput rightVertex rightEdge label ->
    OpenCircuit
        primitive
        (JoinPurity leftPurity rightPurity)
        (leftStateInput, rightStateInput)
        (leftStateOutput, rightStateOutput)
        sort
        (Either leftInput rightInput)
        (Either leftOutput rightOutput)
        (Either leftVertex rightVertex)
        (Either leftEdge rightEdge)
        label
tensorOpenCircuit left right =
    UnsafeOpenCircuit
        (tensorOpenSystem (openCircuitTopology left) (openCircuitTopology right))
        (tensorCircuit (openCircuitDecoration left) (openCircuitDecoration right))

-- | Exact denotation of the directed decoration. No graph black-boxing occurs.
openCircuitDenotation ::
    ExactPrimitiveInterpreter primitive primitiveError ->
    OpenCircuit primitive purity stateInput stateOutput sort input output vertex edge label ->
    Either
        (ExactCircuitInterpretationError primitiveError)
        (StochasticMatrix NonNegativeRational stateInput stateOutput)
openCircuitDenotation primitives =
    interpretExactCircuit primitives . openCircuitDecoration

-- | Apply the exact directed decoration.
runOpenCircuit ::
    (Eq stateInput) =>
    ExactPrimitiveInterpreter primitive primitiveError ->
    OpenCircuit primitive purity stateInput stateOutput sort input output vertex edge label ->
    stateInput ->
    Either
        (ExactCircuitInterpretationError primitiveError)
        (ExactFiniteDist stateOutput)
runOpenCircuit primitives circuit =
    runExactCircuit primitives (openCircuitDecoration circuit)

{- | Boundary-reversed view of an open circuit.

Only the topological boundary parameters are exchanged. The state parameters
retain the original directed circuit orientation. This type has no exact
stochastic-denotation observer.
-}
type role BoundaryReversedOpenCircuit nominal nominal nominal nominal nominal nominal nominal nominal nominal nominal

data
    BoundaryReversedOpenCircuit
        primitive
        purity
        stateInput
        stateOutput
        sort
        reversedInput
        reversedOutput
        vertex
        edge
        label
    where
    UnsafeBoundaryReversedOpenCircuit ::
        !(OpenSystem sort originalOutput originalInput vertex edge label) ->
        !(Circuit primitive purity originalStateInput originalStateOutput) ->
        BoundaryReversedOpenCircuit
            primitive
            purity
            originalStateInput
            originalStateOutput
            sort
            originalOutput
            originalInput
            vertex
            edge
            label

-- | Reverse structured-cospan boundaries without inventing reverse dynamics.
reverseOpenCircuitBoundary ::
    OpenCircuit primitive purity stateInput stateOutput sort input output vertex edge label ->
    BoundaryReversedOpenCircuit primitive purity stateInput stateOutput sort output input vertex edge label
reverseOpenCircuitBoundary circuit =
    UnsafeBoundaryReversedOpenCircuit
        (reverseOpenBoundary (openCircuitTopology circuit))
        (openCircuitDecoration circuit)

-- | Read reversed topology. No reversed stochastic matrix is available.
reversedOpenTopology ::
    BoundaryReversedOpenCircuit primitive purity stateInput stateOutput sort input output vertex edge label ->
    OpenSystem sort input output vertex edge label
reversedOpenTopology (UnsafeBoundaryReversedOpenCircuit topology _) = topology

-- | Undo boundary reversal and recover the original directed open circuit.
restoreOpenCircuitBoundary ::
    BoundaryReversedOpenCircuit primitive purity stateInput stateOutput sort input output vertex edge label ->
    OpenCircuit primitive purity stateInput stateOutput sort output input vertex edge label
restoreOpenCircuitBoundary (UnsafeBoundaryReversedOpenCircuit topology decoration) =
    UnsafeOpenCircuit (reverseOpenBoundary topology) decoration
