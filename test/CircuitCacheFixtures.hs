{-# LANGUAGE DataKinds #-}

module CircuitCacheFixtures (
    CacheNetwork,
    chainFixture,
    independentFixture,
    semanticLimits,
    infrastructureLimits,
    assertCache,
    rightCache,
    rowsCache,
) where

import Data.Foldable (foldlM)
import Markovian.Algebra.NonNegativeRational
import Markovian.Category.Finite.Set
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Stochastic
import Markovian.Circuit
import Markovian.Open.Acyclic
import Markovian.Open.Acyclic.Circuit.Exact
import Markovian.Open.Hypergraph
import Markovian.Open.Interface
import Markovian.Open.StructuredCospan

type CacheNetwork = SomeAcyclicOpenCircuit ExactTablePrimitive () Int Int Int Int Int Bool

semanticLimits :: CircuitSemanticLimits
semanticLimits = CircuitSemanticLimits 100000 1000000 1000000 1000000 256 256

infrastructureLimits :: CircuitCacheLimits
infrastructureLimits = CircuitCacheLimits 1000 1000000 10000000 1000000000

assertCache :: String -> Bool -> IO ()
assertCache message condition = if condition then pure () else fail message

rightCache :: (Show error) => Either error value -> IO value
rightCache = either (fail . show) pure

rowsCache :: StochasticMatrix NonNegativeRational source target -> [[Rational]]
rowsCache = map (map getNonNegativeRational) . matrixRows . forgetStochastic

chainFixture :: Int -> Int -> Rational -> Bool -> IO CacheNetwork
chainFixture edges depth probability reversePrimitiveSource = do
    domains <- domainFixture
    ports <- rightCache (portAssignmentObject domains [()])
    let values = assignmentObjectValues ports
    source <- if reversePrimitiveSource then rightCache (finiteSet (reverse (finiteSetValues values))) else pure values
    weights <-
        traverse
            (traverse (rightCache . nonNegativeRational))
            [[probability, 1 - probability], [1 - probability, probability]]
    matrix <- rightCache (matrixFromRows source values weights)
    primitive <- rightCache (stochasticMatrix matrix)
    let local = stochasticPrimitive values values (exactStochasticPrimitive primitive)
    repeated <- foldlM (\current _ -> rightCache (composeCircuit current local)) local [2 .. depth]
    table <- rightCache (labelCircuitTable [stochasticLabelCircuit 0 [()] [()] repeated])
    networkFixture
        domains
        table
        [(0, ())]
        [(0, ())]
        [(index, ()) | index <- [0 .. edges]]
        [(index, 0, [(index, ())], [(index + 1, ())]) | index <- [0 .. edges - 1]]
        [(0, 0)]
        [(0, edges)]

independentFixture :: Bool -> IO CacheNetwork
independentFixture sharing = do
    domains <- domainFixture
    input <- rightCache (portAssignmentObject domains [])
    output <- rightCache (portAssignmentObject domains [()])
    half <- rightCache (nonNegativeRational (1 / 2))
    matrix <- rightCache (matrixFromRows (assignmentObjectValues input) (assignmentObjectValues output) [[half, half]])
    primitive <- rightCache (stochasticMatrix matrix)
    let local = stochasticPrimitive (assignmentObjectValues input) (assignmentObjectValues output) (exactStochasticPrimitive primitive)
    table <- rightCache (labelCircuitTable [stochasticLabelCircuit 0 [] [()] local])
    if sharing
        then
            networkFixture
                domains
                table
                []
                [(0, ()), (1, ())]
                [(0, ())]
                [(0, 0, [], [(0, ())])]
                []
                [(0, 0), (1, 0)]
        else
            networkFixture
                domains
                table
                []
                [(0, ()), (1, ())]
                [(0, ()), (1, ())]
                [(0, 0, [], [(0, ())]), (1, 0, [], [(1, ())])]
                []
                [(0, 0), (1, 1)]

domainFixture :: IO (FiniteValueDomains () Bool)
domainFixture = do
    values <- rightCache (finiteSet [False, True])
    rightCache (finiteValueDomains [((), values)])

networkFixture ::
    FiniteValueDomains () Bool ->
    LabelCircuitTable ExactTablePrimitive () Int Bool ->
    [(Int, ())] ->
    [(Int, ())] ->
    [(Int, ())] ->
    [(Int, Int, [(Int, ())], [(Int, ())])] ->
    [(Int, Int)] ->
    [(Int, Int)] ->
    IO CacheNetwork
networkFixture domains table inputEntries outputEntries vertexEntries edges inputEntriesMap outputEntriesMap = do
    input <- rightCache (interface inputEntries)
    output <- rightCache (interface outputEntries)
    vertices <- rightCache (interface vertexEntries)
    graph <- rightCache (typedHypergraph vertices edges)
    inputLeg <- rightCache (interfaceMap input vertices inputEntriesMap)
    outputLeg <- rightCache (interfaceMap output vertices outputEntriesMap)
    system <- rightCache (openSystem input output graph inputLeg outputLeg)
    acyclic <- rightCache (acyclicOpenSystem system)
    rightCache (acyclicOpenCircuit acyclic domains table)
