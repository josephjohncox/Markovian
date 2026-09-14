{-# LANGUAGE GADTs #-}

module CircuitCache (runCircuitCacheTests) where

import CircuitCacheFixtures
import Control.Monad (forM_)
import Data.Ratio ((%))
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Stochastic
import Markovian.Open.Acyclic.Circuit.Exact

runCircuitCacheTests :: (String -> IO () -> IO ()) -> IO ()
runCircuitCacheTests run = do
    run "retained circuit cold and warm tables match original exact denotation" testDenotation
    run "retained table hits preserve independent stochastic edge occurrences" testIndependence
    run "retained circuit counts cold construction once and replays every hit" testCumulative
    run "retained circuit source limits all produce distinct cache keys" testLimitKeys
    run "retained circuit resource boundaries and failure atomicity" testBoundaries
    run "retained circuit primitive failures preserve edge context and precedence" testPrimitiveFailure
    run "retained circuit empty network has zero local-table work" testEmpty

sourceReceipt :: CircuitCacheReport -> [Integer]
sourceReceipt report =
    map
        toInteger
        [ cacheSourceNodes report
        , cacheSourceCells report
        , cacheSourceWork report
        , cacheSourcePrimitiveCells report
        , cacheMaximumNumeratorBits report
        , cacheMaximumDenominatorBits report
        ]

testDenotation :: IO ()
testDenotation = do
    SomeAcyclicOpenCircuit _ circuit <- chainFixture 3 2 (1 % 3) False
    original <- rightCache (acyclicOpenCircuitDenotation exactTableInterpreter circuit)
    let owner = retainAcyclicOpenCircuit ExactTableInterpreterV1 circuit
    (reference, _, uncached) <- rightCache (retainedAcyclicOpenCircuitDenotation UncachedCircuitTables semanticLimits infrastructureLimits owner)
    (cold, warmOwner, coldReport) <- rightCache (retainedAcyclicOpenCircuitDenotation RetainCircuitTables semanticLimits infrastructureLimits owner)
    (warm, _, warmReport) <- rightCache (retainedAcyclicOpenCircuitDenotation RetainCircuitTables semanticLimits infrastructureLimits warmOwner)
    assertCache "six independent transitions literal matrix" (rowsCache original == [[365 % 729, 364 % 729], [364 % 729, 365 % 729]])
    assertCache "cold/warm/reference layout and exact values" (all (sameMatrixLayout (forgetStochastic original) . forgetStochastic) [reference, cold, warm])
    assertCache "source receipts differ" (sourceReceipt uncached == sourceReceipt coldReport && sourceReceipt coldReport == sourceReceipt warmReport)
    assertCache "three uncached constructions" (cacheConstructions uncached == 3 && cacheHits uncached == 0)
    assertCache "one cold table and two hits" (cacheConstructions coldReport == 1 && cacheHits coldReport == 2)
    assertCache "three real warm hits" (cacheConstructions warmReport == 0 && cacheHits warmReport == 3)
    assertCache "warm replay charged actual executor operations" (cacheExecutorOperations warmReport > cacheResidentTraceSlots warmReport)

testIndependence :: IO ()
testIndependence = do
    SomeAcyclicOpenCircuit _ independent <- independentFixture False
    SomeAcyclicOpenCircuit _ shared <- independentFixture True
    (a, _, report) <- rightCache (retainedAcyclicOpenCircuitDenotation RetainCircuitTables semanticLimits infrastructureLimits (retainAcyclicOpenCircuit ExactTableInterpreterV1 independent))
    (b, _, _) <- rightCache (retainedAcyclicOpenCircuitDenotation RetainCircuitTables semanticLimits infrastructureLimits (retainAcyclicOpenCircuit ExactTableInterpreterV1 shared))
    assertCache "independent occurrences became a shared draw" (rowsCache a == [[1 % 4, 1 % 4, 1 % 4, 1 % 4]])
    assertCache "shared producer lost its diagonal" (rowsCache b == [[1 % 2, 0, 0, 1 % 2]])
    assertCache "second independent occurrence should hit" (cacheConstructions report == 1 && cacheHits report == 1)

testCumulative :: IO ()
testCumulative = do
    SomeAcyclicOpenCircuit _ circuit <- chainFixture 2 1 (1 % 2) False
    let owner = retainAcyclicOpenCircuit ExactTableInterpreterV1 circuit
        run mode limits = retainedAcyclicOpenCircuitDenotation mode limits infrastructureLimits owner
    (_, _, report) <- rightCache (run RetainCircuitTables semanticLimits)
    assertCache "independent exact ledger oracle" (sourceReceipt report == [2, 8, 8, 8, 1, 2])
    assertCache "actual table footprint" (cacheResidentEntries report == 1 && cacheResidentCells report == 4 && cacheResidentTraceSlots report == 10)
    forM_ [0 .. 7] $ \budget -> do
        let limits = semanticLimits{circuitWorkLimit = budget}
            expected = RetainedCircuitSourceError (if budget < 4 then 0 else 1) (if budget < 4 then 0 else 1) (CircuitSourceWorkLimitExceeded budget (budget + 1))
        assertCache ("exact source work boundary " ++ show budget) (failure (run UncachedCircuitTables limits) == Just expected && failure (run RetainCircuitTables limits) == Just expected)
    let primitiveLimits = semanticLimits{circuitPrimitiveCellLimit = 7}
        expectedPrimitive = RetainedCircuitSourceError 1 1 (CircuitSourcePrimitiveCellLimitExceeded 7 8)
    assertCache "later primitive-cell check skipped by hit" (failure (run RetainCircuitTables primitiveLimits) == Just expectedPrimitive && failure (run UncachedCircuitTables primitiveLimits) == Just expectedPrimitive)
    let simultaneous = primitiveLimits{circuitWorkLimit = 4}
    assertCache
        "hit replay changed primitive-before-work precedence"
        ( failure (run RetainCircuitTables simultaneous) == Just expectedPrimitive
            && failure (run UncachedCircuitTables simultaneous) == Just expectedPrimitive
        )

testLimitKeys :: IO ()
testLimitKeys = do
    SomeAcyclicOpenCircuit _ circuit <- chainFixture 1 1 (1 % 2) False
    let owner = retainAcyclicOpenCircuit ExactTableInterpreterV1 circuit
    (_, warm, _) <- rightCache (retainedAcyclicOpenCircuitDenotation RetainCircuitTables semanticLimits infrastructureLimits owner)
    let alternatives =
            [ semanticLimits{circuitNodeLimit = circuitNodeLimit semanticLimits + 1}
            , semanticLimits{circuitCellLimit = circuitCellLimit semanticLimits + 1}
            , semanticLimits{circuitWorkLimit = circuitWorkLimit semanticLimits + 1}
            , semanticLimits{circuitPrimitiveCellLimit = circuitPrimitiveCellLimit semanticLimits + 1}
            , semanticLimits{circuitNumeratorBitLimit = circuitNumeratorBitLimit semanticLimits + 1}
            , semanticLimits{circuitDenominatorBitLimit = circuitDenominatorBitLimit semanticLimits + 1}
            ]
    forM_ alternatives $ \limits -> do
        (_, _, report) <- rightCache (retainedAcyclicOpenCircuitDenotation RetainCircuitTables limits infrastructureLimits warm)
        assertCache "changed semantic field reused entry" (cacheConstructions report == 1 && cacheHits report == 0 && cacheResidentEntries report == 2)
    (_, _, same) <- rightCache (retainedAcyclicOpenCircuitDenotation RetainCircuitTables semanticLimits (infrastructureLimits{cacheExecutorLimit = cacheExecutorLimit infrastructureLimits + 1}) warm)
    assertCache "executor limit contaminated semantic key" (cacheHits same == 1 && cacheConstructions same == 0)

testBoundaries :: IO ()
testBoundaries = do
    SomeAcyclicOpenCircuit _ circuit <- chainFixture 1 1 (1 % 2) False
    let owner = retainAcyclicOpenCircuit ExactTableInterpreterV1 circuit
        run = retainedAcyclicOpenCircuitDenotation RetainCircuitTables
        exactSource = CircuitSemanticLimits 1 4 4 4 1 2
    (_, warm, receipt) <- rightCache (run exactSource infrastructureLimits owner)
    let exactInfrastructure = CircuitCacheLimits 1 4 10 (cacheExecutorReserved receipt)
    _ <- rightCache (run exactSource exactInfrastructure owner)
    forM_
        [ (exactSource{circuitNodeLimit = 0}, CircuitSourceNodeLimitExceeded 0 1)
        , (exactSource{circuitCellLimit = 3}, CircuitSourceCellLimitExceeded 3 4)
        , (exactSource{circuitWorkLimit = 3}, CircuitSourceWorkLimitExceeded 3 4)
        , (exactSource{circuitPrimitiveCellLimit = 3}, CircuitSourcePrimitiveCellLimitExceeded 3 4)
        , (exactSource{circuitDenominatorBitLimit = 1}, CircuitSourceDenominatorBitLimitExceeded 1 2)
        ]
        $ \(limits, expected) ->
            assertCache "source one-below cap" (failure (run limits infrastructureLimits owner) == Just (RetainedCircuitSourceError 0 0 expected))
    forM_
        [ (exactInfrastructure{cacheEntryLimit = 0}, CircuitCacheEntryLimitExceeded 0 1)
        , (exactInfrastructure{cacheCellLimit = 3}, CircuitCacheCellLimitExceeded 3 4)
        , (exactInfrastructure{cacheTraceLimit = 9}, CircuitCacheTraceLimitExceeded 9 10)
        , (exactInfrastructure{cacheExecutorLimit = cacheExecutorLimit exactInfrastructure - 1}, CircuitCacheExecutorLimitExceeded (cacheExecutorLimit exactInfrastructure - 1) (cacheExecutorLimit exactInfrastructure))
        ]
        $ \(limits, expected) ->
            assertCache "infrastructure one-below cap" (failure (run exactSource limits owner) == Just (RetainedCircuitInfrastructureError expected))
    assertCache "invalid limits precede zero infrastructure" (failure (run (exactSource{circuitNumeratorBitLimit = 0}) (CircuitCacheLimits 0 0 0 0) owner) == Just (RetainedCircuitInvalidLimits CircuitInvalidNumeratorBitLimit))
    assertCache
        "invalid denominator precedes infrastructure"
        ( failure (run (exactSource{circuitDenominatorBitLimit = 0}) (CircuitCacheLimits 0 0 0 0) owner)
            == Just (RetainedCircuitInvalidLimits CircuitInvalidDenominatorBitLimit)
        )
    (_, _, warmReceipt) <- rightCache (run exactSource infrastructureLimits warm)
    let warmInfrastructure = CircuitCacheLimits 1 4 10 (cacheExecutorReserved warmReceipt)
    _ <- rightCache (run exactSource warmInfrastructure warm)
    forM_
        [ (warmInfrastructure{cacheEntryLimit = 0}, CircuitCacheEntryLimitExceeded 0 1)
        , (warmInfrastructure{cacheCellLimit = 3}, CircuitCacheCellLimitExceeded 3 4)
        , (warmInfrastructure{cacheTraceLimit = 9}, CircuitCacheTraceLimitExceeded 9 10)
        ,
            ( warmInfrastructure{cacheExecutorLimit = cacheExecutorLimit warmInfrastructure - 1}
            , CircuitCacheExecutorLimitExceeded (cacheExecutorLimit warmInfrastructure - 1) (cacheExecutorLimit warmInfrastructure)
            )
        ]
        $ \(limits, expected) ->
            assertCache
                "warm infrastructure one-below cap"
                (failure (run exactSource limits warm) == Just (RetainedCircuitInfrastructureError expected))
    assertCache "failed changed-limit request should fail" (case failure (run (exactSource{circuitWorkLimit = 0}) infrastructureLimits warm) of Just _ -> True; Nothing -> False)
    (_, _, afterFailure) <- rightCache (run exactSource infrastructureLimits warm)
    assertCache "failure changed the retained input owner" (cacheHits afterFailure == 1 && cacheResidentEntries afterFailure == 1)

testPrimitiveFailure :: IO ()
testPrimitiveFailure = do
    SomeAcyclicOpenCircuit _ circuit <- chainFixture 1 1 (1 % 2) True
    let owner = retainAcyclicOpenCircuit ExactTableInterpreterV1 circuit
        expected = Just (RetainedCircuitSourceError 0 0 CircuitSourcePrimitiveSourceMismatch)
    forM_ [UncachedCircuitTables, RetainCircuitTables] $ \mode ->
        assertCache "primitive source mismatch or edge context changed" (failure (retainedAcyclicOpenCircuitDenotation mode (semanticLimits{circuitWorkLimit = 0}) infrastructureLimits owner) == expected)

testEmpty :: IO ()
testEmpty = do
    SomeAcyclicOpenCircuit _ circuit <- chainFixture 0 1 (1 % 2) False
    let limits = CircuitSemanticLimits 0 0 0 0 1 1
    (matrix, _, report) <- rightCache (retainedAcyclicOpenCircuitDenotation RetainCircuitTables limits (CircuitCacheLimits 0 0 0 0) (retainAcyclicOpenCircuit ExactTableInterpreterV1 circuit))
    assertCache "empty local schedule identity" (rowsCache matrix == [[1, 0], [0, 1]])
    assertCache "empty local schedule uses resources" (sourceReceipt report == [0, 0, 0, 0, 0, 0] && cacheExecutorOperations report == 0)

failure :: Either error value -> Maybe error
failure = either Just (const Nothing)
