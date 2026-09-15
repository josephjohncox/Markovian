{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- Direct-source controls for the private cache engine. This is intentionally
-- absent from the library's public module surface.
module CircuitCachePrivate (main) where

import CircuitCache (runCircuitCacheTests)
import CircuitCacheFixtures
import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Data.IORef
import Data.List (transpose)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ratio ((%))
import Markovian.Algebra.NonNegativeRational
import Markovian.Category.Finite.Set
import Markovian.Category.Finite.Set.Internal (FiniteSet (UnsafeFiniteSet))
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Deterministic
import Markovian.Category.Matrix.Stochastic
import Markovian.Category.Matrix.Stochastic.Internal (StochasticMatrix (UnsafeStochasticMatrix))
import Markovian.Circuit
import Markovian.Circuit.Interpret.Exact
import Markovian.Open.Acyclic.Circuit.Cache.Internal
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

main :: IO ()
main = do
    let run name action = action >> putStrLn name
    runCircuitCacheTests run
    run "cache every-constructor literal matrices and ordered event traces" testConstructors
    run "cache discarded rational source order is observable" testDiscarded
    run "cache exact key checks distinguish layouts and retained slots" testKeys
    run "cache cumulative planning fuel charges support scans" testPlanning
    run "cache exploding products reject without layout materialization" testExploding
    run "cache strict success completes input observations before publication" testStrictness
    run "cache infrastructure and source shape preflight precede table reads" testPreflight
    run "cache request spine is bounded including an infinite request list" testInfinite
    run "cache primitive validations preserve first failure position" testValidationOrder
    run "cache empty support tables and empty tensor factors retain exact shape" testEmptyLayouts

-- Literal stochastic input fixtures are independently normalized by the public
-- matrix constructor. The oracle matrices below are ordinary Rational lists.
primitive :: FiniteSet source -> FiniteSet target -> [[Rational]] -> IO (Circuit ExactTablePrimitive 'Stochastic source target)
primitive source target rows = do
    values <- traverse (traverse (rightCache . nonNegativeRational)) rows
    raw <- rightCache (matrixFromRows source target values)
    table <- rightCache (stochasticMatrix raw)
    pure (stochasticPrimitive source target (exactStochasticPrimitive table))

scalarEvents :: [Rational] -> [SourceEvent]
scalarEvents = concatMap (\value -> [SourceCharge 1, SourceRational value])

primitiveEvents :: [[Rational]] -> [SourceEvent]
primitiveEvents rows = [SourcePrimitiveEndpoints True True, SourcePrimitiveCells (fromIntegral (sum (map length rows)))] ++ scalarEvents (concat rows)

-- An independent finite sum/product expansion returns both its value and each
-- product/partial sum, in the declared mathematical row-column-middle order.
compositionOracle :: [[Rational]] -> [[Rational]] -> ([[Rational]], [Rational])
compositionOracle left right =
    let expanded = [[partials (zipWith (*) row column) | column <- transpose right] | row <- left]
     in (map (map fst) expanded, concatMap (concatMap snd) expanded)
  where
    partials [] = (0, [0])
    partials products =
        let accumulated = drop 1 (scanl (+) 0 products)
         in (last accumulated, concat (zipWith (\productValue total -> [productValue, total]) products accumulated))

tensorOracle :: [[Rational]] -> [[Rational]] -> [[Rational]]
tensorOracle left right = [[a * b | a <- rowA, b <- rowB] | rowA <- left, rowB <- right]

checkCircuit :: String -> Circuit ExactTablePrimitive purity source target -> [[Rational]] -> [SourceEvent] -> IO ()
checkCircuit name circuit expectedRows expectedEvents = do
    (_, PlanningArrow action) <- rightCache (foldCircuitWithNodeLimit 100000 (planAlgebra 1000000000) circuit)
    (plan, _) <- rightCache (runPlanning action 1000000000)
    (table, executed) <- rightCache (runWork (executePlan plan) (WorkState semanticLimits emptyReport True []))
    assertCache (name ++ " literal table") (map (map getNonNegativeRational) (matrixRows table) == expectedRows)
    assertCache (name ++ " complete source event order") (reverse (workEvents executed) == expectedEvents)
    assertCache (name ++ " exact trace reservation") (planEvents plan == fromIntegral (length expectedEvents))
    original <- rightCache (interpretExactCircuit exactTableInterpreter circuit)
    assertCache (name ++ " original matrix layout/entries") (sameMatrixLayout table (forgetStochastic original))
    let request = [TableRequest 0 0 circuit]
    (reference, _, referenceReport) <- rightCache (runCircuitCache UncachedCircuitTables semanticLimits infrastructureLimits ExactTableInterpreterV1 request emptyTableCache)
    (cold, cache, coldReport) <- rightCache (runCircuitCache RetainCircuitTables semanticLimits infrastructureLimits ExactTableInterpreterV1 request emptyTableCache)
    (warm, _, warmReport) <- rightCache (runCircuitCache RetainCircuitTables semanticLimits infrastructureLimits ExactTableInterpreterV1 request cache)
    assertCache (name ++ " all routes literal matrices") (all ((== [expectedRows]) . map rowsCache) [reference, cold, warm])
    let source report = (cacheSourceWork report, cacheSourcePrimitiveCells report, cacheMaximumNumeratorBits report, cacheMaximumDenominatorBits report)
    assertCache (name ++ " equal source receipts") (source referenceReport == source coldReport && source coldReport == source warmReport)
    assertCache (name ++ " bounded executor receipts") (all (\report -> cacheExecutorOperations report <= cacheExecutorReserved report) [referenceReport, coldReport, warmReport])

structuralCase :: String -> Circuit ExactTablePrimitive purity source target -> Int -> [Int] -> IO ()
structuralCase name circuit columns outputs =
    let rows = [[if column == output then 1 else 0 | column <- [0 .. columns - 1]] | output <- outputs]
     in checkCircuit name circuit rows (scalarEvents (concat rows))

testConstructors :: IO ()
testConstructors = do
    bits <- rightCache (finiteSet [False, True])
    unit <- rightCache (finiteSet [()])
    let aRows = [[1 % 3, 2 % 3], [2 % 3, 1 % 3]]
        bRows = [[1 % 4, 3 % 4], [3 % 4, 1 % 4]]
    a <- primitive bits bits aRows
    b <- primitive bits bits bRows
    checkCircuit "stochastic primitive" a aRows (primitiveEvents aRows)
    deterministic <- rightCache (deterministicFromFunction bits bits not)
    let dRows = [[0, 1], [1, 0]]
    checkCircuit "deterministic primitive" (deterministicPrimitive bits bits (exactDeterministicPrimitive deterministic)) dRows (primitiveEvents dRows)
    checkCircuit "deterministic table" (deterministicMatrixCircuit deterministic) dRows (scalarEvents (concat dRows))
    structuralCase "identity" (identityCircuit bits) 2 [0, 1]
    structuralCase "symmetry" (symmetryCircuit bits bits) 4 [0, 2, 1, 3]
    structuralCase "associate" (associateCircuit bits bits bits) 8 [0 .. 7]
    structuralCase "unassociate" (unassociateCircuit bits bits bits) 8 [0 .. 7]
    structuralCase "left unitor" (leftUnitorCircuit bits) 2 [0, 1]
    structuralCase "left inverse unitor" (leftUnitorInverseCircuit bits) 2 [0, 1]
    structuralCase "right unitor" (rightUnitorCircuit bits) 2 [0, 1]
    structuralCase "right inverse unitor" (rightUnitorInverseCircuit bits) 2 [0, 1]
    structuralCase "copy" (copyCircuit bits) 4 [0, 3]
    structuralCase "discard" (discardCircuit bits) 1 [0, 0]
    structuralCase "unit identity" (identityCircuit unit) 1 [0]
    composed <- rightCache (composeCircuit a b)
    let (composedRows, composedValues) = compositionOracle aRows bRows
    assertCache "independent composed rational golden" (composedRows == [[7 % 12, 5 % 12], [5 % 12, 7 % 12]])
    checkCircuit "compose" composed composedRows (primitiveEvents aRows ++ primitiveEvents bRows ++ scalarEvents composedValues)
    let tensorRows = tensorOracle aRows bRows
    checkCircuit "tensor" (tensorCircuit a b) tensorRows (primitiveEvents aRows ++ primitiveEvents bRows ++ scalarEvents (concat tensorRows))
    checkCircuit "weaken" (weakenPurity (deterministicMatrixCircuit deterministic)) dRows (scalarEvents (concat dRows))
    third <- rightCache (nonNegativeRational (1 % 3))
    twoThirds <- rightCache (nonNegativeRational (2 % 3))
    mixture <- rightCache (convexChoice ((third, a) :| [(twoThirds, b)]))
    let weightedValues = concat [[x / 3, x / 3, 2 * y / 3, x / 3 + 2 * y / 3] | (x, y) <- zip (concat aRows) (concat bRows)]
        mixedRows = [[5 % 18, 13 % 18], [13 % 18, 5 % 18]]
    checkCircuit
        "convex"
        mixture
        mixedRows
        (primitiveEvents aRows ++ primitiveEvents bRows ++ scalarEvents [1 % 3, 1 % 3, 2 % 3, 1] ++ scalarEvents weightedValues)
    let copyRows = [[1, 0, 0, 0], [0, 0, 0, 1]]
        (sharedRows, sharedValues) = compositionOracle aRows copyRows
    checkCircuit "share" (shareCircuit a) sharedRows (primitiveEvents aRows ++ scalarEvents (concat copyRows) ++ scalarEvents sharedValues)
    independent <- rightCache (fanoutCircuit a b)
    let (fanoutRows, fanoutValues) = compositionOracle copyRows tensorRows
    checkCircuit
        "fanout"
        independent
        fanoutRows
        (scalarEvents (concat copyRows) ++ primitiveEvents aRows ++ primitiveEvents bRows ++ scalarEvents (concat tensorRows) ++ scalarEvents fanoutValues)

failure :: Either error value -> Maybe error
failure = either Just (const Nothing)

testDiscarded :: IO ()
testDiscarded = do
    unit <- rightCache (finiteSet [()])
    three <- rightCache (finiteSet [0 :: Int, 1, 2])
    early <- primitive unit three [[1 % 256, 0, 255 % 256]]
    late <- primitive unit three [[0, 1 % 256, 255 % 256]]
    earlyDiscard <- rightCache (composeCircuit early (discardCircuit three))
    lateDiscard <- rightCache (composeCircuit late (discardCircuit three))
    forM_ [earlyDiscard, lateDiscard] $ \circuit -> do
        (values, _, _) <- rightCache (runCircuitCache RetainCircuitTables semanticLimits infrastructureLimits ExactTableInterpreterV1 [TableRequest 0 0 circuit] emptyTableCache)
        assertCache "discarded rational fixture final matrix" (map rowsCache values == [[[1]]])
    let limits = semanticLimits{circuitWorkLimit = 1, circuitDenominatorBitLimit = 8}
        run mode circuit = runCircuitCache mode limits infrastructureLimits ExactTableInterpreterV1 [TableRequest 0 0 circuit] emptyTableCache
    forM_ [UncachedCircuitTables, RetainCircuitTables] $ \mode -> do
        assertCache "earlier discarded denominator must win" (failure (run mode earlyDiscard) == Just (CacheSource 0 (CircuitSourceDenominatorBitLimitExceeded 8 9)))
        assertCache "earlier source work must win" (failure (run mode lateDiscard) == Just (CacheSource 0 (CircuitSourceWorkLimitExceeded 1 2)))
    bits <- rightCache (finiteSet [False, True])
    both <- primitive unit bits [[3 % 4, 1 % 4]]
    let bothLimited = semanticLimits{circuitNumeratorBitLimit = 1, circuitDenominatorBitLimit = 1}
    assertCache "numerator precedes denominator" (failure (runCircuitCache RetainCircuitTables bothLimited infrastructureLimits ExactTableInterpreterV1 [TableRequest 0 0 both] emptyTableCache) == Just (CacheSource 0 (CircuitSourceNumeratorBitLimitExceeded 1 2)))
    let numeratorExact = semanticLimits{circuitNumeratorBitLimit = 2, circuitDenominatorBitLimit = 3}
    _ <- rightCache (runCircuitCache RetainCircuitTables numeratorExact infrastructureLimits ExactTableInterpreterV1 [TableRequest 0 0 both] emptyTableCache)
    pure ()

testKeys :: IO ()
testKeys = do
    forward <- rightCache (finiteSet [False, True])
    reverseLayout <- rightCache (finiteSet [True, False])
    let key = CacheKey ExactTableInterpreterV1 3 forward forward 2 2 semanticLimits
        alternatives =
            [ CacheKey ExactTableInterpreterV1 4 forward forward 2 2 semanticLimits
            , CacheKey ExactTableInterpreterV1 3 reverseLayout forward 2 2 semanticLimits
            , CacheKey ExactTableInterpreterV1 3 forward reverseLayout 2 2 semanticLimits
            ]
    assertCache "identity key reflexivity" (sameKey key key)
    assertCache "slot or ordered layout key collapsed" (not (any (sameKey key) alternatives))
    let limits = semanticLimits{circuitWorkLimit = circuitWorkLimit semanticLimits + 1}
    assertCache "semantic key field collapsed" (not (sameKey key (CacheKey ExactTableInterpreterV1 3 forward forward 2 2 limits)))
    -- The private driver defends its owner invariant even if its own caller
    -- illegally replaces a circuit under a retained slot.
    first <- primitive forward forward [[1 % 3, 2 % 3], [2 % 3, 1 % 3]]
    reordered <- primitive reverseLayout forward [[2 % 3, 1 % 3], [1 % 3, 2 % 3]]
    (_, cache, _) <- rightCache (runCircuitCache RetainCircuitTables semanticLimits infrastructureLimits ExactTableInterpreterV1 [TableRequest 0 0 first] emptyTableCache)
    assertCache "private owner mismatch allocated an unreserved replacement" (failure (runCircuitCache RetainCircuitTables semanticLimits infrastructureLimits ExactTableInterpreterV1 [TableRequest 0 0 reordered] cache) == Just (CacheSource 0 CircuitSourceInvariantFailure))

testPlanning :: IO ()
testPlanning = do
    bits <- rightCache (finiteSet [False, True])
    circuit <- rightCache (composeCircuit (identityCircuit bits) (identityCircuit bits))
    let request = [TableRequest 0 0 circuit]
        run amount = planRequests semanticLimits (infrastructureLimits{cacheExecutorLimit = amount}) ExactTableInterpreterV1 request
    (_, spent) <- rightCache (run 15)
    assertCache "raw nodes plus support-spine visits" (spent == 15)
    assertCache "one-below cumulative planning fuel" (failure (run 14) == Just (CacheInfrastructure (CircuitCacheExecutorLimitExceeded 14 15)))

data HugeCircuit where
    HugeCircuit :: Circuit ExactTablePrimitive purity source target -> HugeCircuit

grow :: Int -> HugeCircuit -> HugeCircuit
grow 0 circuit = circuit
grow count (HugeCircuit circuit) = grow (count - 1) (HugeCircuit (tensorCircuit circuit circuit))

testExploding :: IO ()
testExploding = do
    bits <- rightCache (finiteSet [False, True])
    case grow 8 (HugeCircuit (identityCircuit bits)) of
        HugeCircuit circuit -> do
            let infrastructure = infrastructureLimits{cacheExecutorLimit = 2 ^ (10000 :: Int)}
                source = semanticLimits{circuitCellLimit = 0}
                check mode limits expected = do
                    observed <- timeout 2000000 (evaluate (failure (runCircuitCache mode source limits ExactTableInterpreterV1 [TableRequest 0 0 circuit] emptyTableCache)))
                    assertCache "an unadmitted 2^256 layout was enumerated" (observed == Just (Just expected))
            check UncachedCircuitTables infrastructure (CacheSource 0 (CircuitSourceCellLimitExceeded 0 1))
            check RetainCircuitTables (infrastructure{cacheCellLimit = 0}) (CacheInfrastructure (CircuitCacheCellLimitExceeded 0 1))
{-# NOINLINE observedScalar #-}
observedScalar :: IORef Int -> Rational -> NonNegativeRational
observedScalar counter value = unsafePerformIO $ do
    modifyIORef' counter (+ 1)
    rightCache (nonNegativeRational value)

observedCircuit :: IORef Int -> IO (Circuit ExactTablePrimitive 'Stochastic Bool Bool)
observedCircuit counter = do
    bits <- rightCache (finiteSet [False, True])
    let matrix = matrixFromFunction bits bits (\source target -> observedScalar counter (if source == target then 1 % 3 else 2 % 3))
    pure (stochasticPrimitive bits bits (exactStochasticPrimitive (UnsafeStochasticMatrix matrix)))

testStrictness :: IO ()
testStrictness = forM_ [UncachedCircuitTables, RetainCircuitTables] $ \mode -> do
    counter <- newIORef 0
    circuit <- observedCircuit counter
    result <- evaluate (runCircuitCache mode semanticLimits infrastructureLimits ExactTableInterpreterV1 [TableRequest 0 0 circuit] emptyTableCache)
    beforePayload <- readIORef counter
    (tables, cache, report) <- rightCache result
    _ <- evaluate (length (show (map rowsCache tables, report)))
    case cache of
        TableCache entries -> forM_ entries $ \(CacheEntry _ table events _ _) -> do
            _ <- evaluate (length (show (matrixRows table, events)))
            pure ()
    afterPayload <- readIORef counter
    assertCache "source observations deferred past outer Either" (beforePayload == 4 && afterPayload == 4)
    when (mode == RetainCircuitTables) $ do
        _ <- rightCache (runCircuitCache mode semanticLimits infrastructureLimits ExactTableInterpreterV1 [TableRequest 0 0 circuit] cache)
        afterHit <- readIORef counter
        assertCache "hit reran primitive table input computations" (afterHit == 4)

testPreflight :: IO ()
testPreflight = do
    counter <- newIORef 0
    circuit <- observedCircuit counter
    let request = [TableRequest 0 0 circuit]
        run semantic infrastructure = runCircuitCache RetainCircuitTables semantic infrastructure ExactTableInterpreterV1 request emptyTableCache
    forM_ [infrastructureLimits{cacheEntryLimit = 0}, infrastructureLimits{cacheCellLimit = 0}, infrastructureLimits{cacheTraceLimit = 0}, infrastructureLimits{cacheExecutorLimit = 0}] $ \limits -> do
        _ <- evaluate (failure (run semanticLimits limits))
        readIORef counter >>= assertCache "infrastructure rejection read table payload" . (== 0)
    _ <- evaluate (failure (run (semanticLimits{circuitCellLimit = 0}) infrastructureLimits))
    readIORef counter >>= assertCache "source structural rejection read table payload" . (== 0)
    _ <- evaluate (failure (run (semanticLimits{circuitWorkLimit = 0}) infrastructureLimits))
    readIORef counter >>= assertCache "work rejection evaluated its upcoming rational" . (== 0)

testInfinite :: IO ()
testInfinite = do
    unit <- rightCache (finiteSet [()])
    let request = TableRequest 0 0 (identityCircuit unit)
        limits = infrastructureLimits{cacheExecutorLimit = 100}
    observed <- timeout 1000000 (evaluate (failure (runCircuitCache RetainCircuitTables semanticLimits limits ExactTableInterpreterV1 (repeat request) emptyTableCache)))
    assertCache "infinite request spine was not bounded" (observed == Just (Just (CacheInfrastructure (CircuitCacheExecutorLimitExceeded 100 101))))

testValidationOrder :: IO ()
testValidationOrder = do
    forward <- rightCache (finiteSet [False, True])
    reversed <- rightCache (finiteSet [True, False])
    good <- primitive forward forward [[1 % 2, 1 % 2], [1 % 2, 1 % 2]]
    table <- rightCache (matrixFromRows reversed forward (replicate 2 (replicate 2 (either (error . show) id (nonNegativeRational (1 % 2))))))
    let bad = stochasticPrimitive forward forward (exactStochasticPrimitive (UnsafeStochasticMatrix table))
        requests = [TableRequest 0 0 good, TableRequest 1 1 bad]
    forM_ [UncachedCircuitTables, RetainCircuitTables] $ \mode -> do
        let run limits = runCircuitCache mode limits infrastructureLimits ExactTableInterpreterV1 requests emptyTableCache
        assertCache "earlier source work lost to later primitive validation" (failure (run (semanticLimits{circuitWorkLimit = 3})) == Just (CacheSource 0 (CircuitSourceWorkLimitExceeded 3 4)))
        assertCache "later primitive validation was moved after a source charge" (failure (run (semanticLimits{circuitWorkLimit = 4})) == Just (CacheSource 1 CircuitSourcePrimitiveSourceMismatch))
    let wrongTarget = stochasticPrimitive forward forward (exactStochasticPrimitive (UnsafeStochasticMatrix (matrixFromFunction forward reversed (\_ _ -> either (error . show) id (nonNegativeRational (1 % 2))))))
    assertCache "primitive target mismatch lost" (failure (runCircuitCache RetainCircuitTables semanticLimits infrastructureLimits ExactTableInterpreterV1 [TableRequest 0 0 wrongTarget] emptyTableCache) == Just (CacheSource 0 CircuitSourcePrimitiveTargetMismatch))

testEmptyLayouts :: IO ()
testEmptyLayouts = do
    empty <- rightCache (finiteSet ([] :: [Bool]))
    bits <- rightCache (finiteSet [False, True])
    checkCircuit "empty identity" (identityCircuit empty) [] []
    checkCircuit "empty tensor factor" (tensorCircuit (identityCircuit empty) (identityCircuit bits)) [] (scalarEvents [1, 0, 0, 1])
    structuralCase "empty target with empty source" (discardCircuit empty) 1 []
    -- A raw finite witness supplied by callers is already constructed input.
    -- This private poison witness proves shape/entry infrastructure does not
    -- inspect the endpoint layouts of an already planned rejected table.
    let poison = UnsafeFiniteSet (error "unadmitted derived endpoint spine" :: [Bool])
        plan = Plan poison poison 2 2 4 8 4 8 (error "unadmitted table body") :: Plan 'Deterministic Bool Bool
        key = CacheKey ExactTableInterpreterV1 0 poison poison 2 2 semanticLimits
        request = [PlannedRequest 0 key 1 plan]
    assertCache "cache preflight forced derived endpoint spine" (failure (reserveRequests RetainCircuitTables semanticLimits (infrastructureLimits{cacheCellLimit = 0}) 1 emptyTableCache request) == Just (CacheInfrastructure (CircuitCacheCellLimitExceeded 0 1)))
