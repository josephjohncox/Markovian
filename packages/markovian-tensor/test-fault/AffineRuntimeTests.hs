{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AffineRuntimeTests (affineRuntimeTests) where

import Control.Concurrent.MVar (modifyMVar_, withMVar)
import Control.Monad (forM_, join, unless)
import Data.IORef
import Foreign.ForeignPtr (ForeignPtr)
import Markovian.Tensor.Internal
import Markovian.Tensor.Shape
import Numeric.Natural (Natural)
import System.Mem.StableName (StableName, hashStableName, makeStableName)
import System.Timeout (timeout)

check :: (Eq value, Show value) => String -> value -> value -> IO ()
check label expected actual = unless (expected == actual) (fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual))

right :: (Show problem) => Either problem value -> IO value
right = either (fail . show) pure

reject :: String -> TensorError -> Either TensorError value -> IO ()
reject label expected result = case result of
    Left actual -> check label expected actual
    Right _ -> fail (label ++ ": unexpectedly accepted")

policy :: IO AffineLimits
policy = right (affineLimits maxBound maxBound maxBound maxBound maxBound maxBound)

oldLimits :: SessionLimits
oldLimits = tensorSessionLimits 16 (affineMachineMaximum + 100) (affineMachineMaximum + 100) 1000000 10000000 4096 10000000

newtype RegistryIdentity = RegistryIdentity (StableName (ForeignPtr Double)) deriving (Eq)

instance Show RegistryIdentity where
    show (RegistryIdentity name) = show (hashStableName name)

-- This transactional lane deliberately observes setup; natural-history probes do not.
snapshot :: TensorSession region -> IO (Natural, Natural, Natural, Natural, AffineUsage, Bool, [RegistryIdentity])
snapshot (TensorSession _ _ lock) = do
    result <- timeout 2000000 $ withMVar lock $ \state -> do
        identities <- mapM (fmap RegistryIdentity . makeStableName) (stateLiveAllocations state)
        pure (stateNextStorage state, statePayloadBytes state, stateBuffers state, stateScalarWork state, stateAffineUsage state, stateClosed state, identities)
    maybe (fail "affine transaction stranded its MVar") pure result

affineRuntimeTests :: IO ()
affineRuntimeTests = do
    scannerDiagnostics
    countSentinelBoundaries
    runtimeEntryAndDemand
    entryCounterBoundaries
    oldPayloadBoundaries
    metadataDiagnostics
    emptyMetadataDemand
    emptyDescriptorAnchors
    allocationRollback
    constructorCountReuse
    putStrLn "affine: private scanner, metadata, demand, rollback and constructor controls passed"

scannerDiagnostics :: IO ()
scannerDiagnostics = do
    p <- policy
    let usage = affineInitialUsage (Just p)
        scan :: SessionLimits -> AffineLimits -> SShape shape -> Either TensorError (Natural, Natural, AffineHeader)
        scan limits affine = affineScanShape (Just limits) affine usage (affineHeaderStart usage)
        dimension = knownShape @'[999]
        machine = knownShape @'[1152921504606846976]
    forM_ [(5, 1), (1, 5), (5, 5)] $ \(oldCap, newCap) -> do
        let limits = oldLimits{limitDimension = oldCap}
            affine = p{affineLimitDimension = newCap}
        reject "old dimension wins with bounded sentinel" (TensorShapeError (DimensionLimitExceeded oldCap (oldCap + 1))) (scan limits affine dimension)
    reject "affine dimension when old passes" (TensorAffineError (AffineLimitExceeded AffineDimension 5 6)) (scan oldLimits (p{affineLimitDimension = 5}) dimension)
    reject "legacy dimension retains actual999" (TensorShapeError (DimensionLimitExceeded 5 999)) (checkedShape (oldLimits{limitDimension = 5}) SF64 dimension)
    reject "old oversized dimension" (TensorShapeError (DimensionLimitExceeded 5 6)) (scan (oldLimits{limitDimension = 5}) p (knownShape @'[18446744073709551616]))
    reject "old rank first" (TensorShapeError (RankLimitExceeded 0 1)) (scan (oldLimits{limitRank = 0, limitDimension = 0}) (p{affineLimitRank = 0}) dimension)
    reject "affine rank before old dimension" (TensorAffineError (AffineLimitExceeded AffineRank 0 1)) (scan (oldLimits{limitDimension = 0}) (p{affineLimitRank = 0}) dimension)
    _ <- right (scan (oldLimits{limitRank = 0}) (p{affineLimitRank = 0}) SNil)
    reject "runtime F64 machine uses M+1" (TensorShapeError (MachineIndexOverflow (affineMachineMaximum + 1))) (scan oldLimits p machine)
    reject "old element before smaller affine cap" (TensorShapeError (ElementLimitExceeded 5 6)) (scan (oldLimits{limitElements = 5}) (p{affineLimitElements = 1}) (knownShape @'[10]))
    reject "old scalar zero cap first" (TensorShapeError (ElementLimitExceeded 0 1)) (scan (oldLimits{limitElements = 0}) (p{affineLimitElements = 0}) SNil)
    reject "affine scalar zero cap" (TensorAffineError (AffineLimitExceeded AffineElements 0 1)) (scan oldLimits (p{affineLimitElements = 0}) SNil)
    let threshold = affineMachineMaximum `div` 8 + 1
    forM_ [(threshold - 1, affineMachineMaximum), (threshold - 2, threshold - 3), (threshold - 3, threshold - 2)] $ \(oldCap, newCap) ->
        reject "ordered exact configured count diagnostics" (TensorShapeError (ElementLimitExceeded oldCap (oldCap + 1))) (scan (oldLimits{limitElements = oldCap}) (p{affineLimitElements = newCap}) machine)
    reject "affine cap before machine" (TensorAffineError (AffineLimitExceeded AffineElements (threshold - 1) threshold)) (scan oldLimits (p{affineLimitElements = threshold - 1}) machine)
    reject "nil credit before machine diagnostic" (TensorAffineError (AffineLimitExceeded AffineWork 896 897)) (scan oldLimits (p{affineLimitWork = 896}) machine)
    (_, zero, _) <- right (scan (oldLimits{limitElements = 0}) (p{affineLimitElements = 0}) (knownShape @'[9223372036854775807, 9223372036854775807, 0]))
    check "late zero at nil" 0 zero

countSentinelBoundaries :: IO ()
countSentinelBoundaries = do
    p <- policy
    let usage = affineInitialUsage (Just p)
        m = affineMachineMaximum
        threshold = m `div` 8 + 1
        scan :: Maybe SessionLimits -> AffineLimits -> SShape shape -> Either TensorError Natural
        scan old affine shape = (\(_, count, _) -> count) <$> affineScanShape old affine usage (affineHeaderStart usage) shape
        machine = knownShape @'[1152921504606846976]
        machineError = TensorShapeError (MachineIndexOverflow (m + 1))
        oldError cap = TensorShapeError (ElementLimitExceeded cap (cap + 1))
        newError cap = TensorAffineError (AffineLimitExceeded AffineElements cap (cap + 1))
        zero = knownShape @'[9223372036854775807, 9223372036854775807, 0]
        rows =
            [ ("cap-M-not-min-E-A", 2, \q -> scan (Just oldLimits{limitElements = 5}) q{affineLimitElements = 1} (knownShape @'[10]), Left (oldError 5))
            , ("pure-machine", 2, \q -> scan Nothing q machine, Left machineError)
            , ("runtime-machine", 2, \q -> scan (Just oldLimits{limitElements = m}) q machine, Left machineError)
            , ("huge-old-cap", 2, \q -> scan (Just oldLimits) q machine, Left machineError)
            , ("old-elements", 2, \q -> scan (Just oldLimits{limitElements = threshold - 1}) q machine, Left (oldError (threshold - 1)))
            , ("affine-elements", 2, \q -> scan (Just oldLimits) q{affineLimitElements = threshold - 1} machine, Left (newError (threshold - 1)))
            , ("pure-elements", 2, \q -> scan Nothing q{affineLimitElements = threshold - 1} machine, Left (newError (threshold - 1)))
            , ("old-larger-competes", 2, \q -> scan (Just oldLimits{limitElements = threshold - 2}) q{affineLimitElements = threshold - 3} machine, Left (oldError (threshold - 2)))
            , ("old-smaller-competes", 2, \q -> scan (Just oldLimits{limitElements = threshold - 3}) q{affineLimitElements = threshold - 2} machine, Left (oldError (threshold - 3)))
            , ("scalar-pure", 1, \q -> scan Nothing q{affineLimitElements = 0} SNil, Left (newError 0))
            , ("scalar-runtime", 1, \q -> scan (Just oldLimits) q{affineLimitElements = 0} SNil, Left (newError 0))
            , ("scalar-old", 1, \q -> scan (Just oldLimits{limitElements = 0}) q SNil, Left (oldError 0))
            , ("scalar-both", 1, \q -> scan (Just oldLimits{limitElements = 0}) q{affineLimitElements = 0} SNil, Left (oldError 0))
            , ("late-zero", 4, \q -> scan (Just oldLimits{limitElements = 0}) q{affineLimitElements = 0} zero, Right 0)
            , ("first-zero", 4, \q -> scan (Just oldLimits{limitElements = 0}) q{affineLimitElements = 0} (knownShape @'[0, 9223372036854775807, 9223372036854775807]), Right 0)
            , ("rank-axis0", 1, \q -> scan Nothing q{affineLimitRank = 0} (knownShape @'[999]), Left (TensorAffineError (AffineLimitExceeded AffineRank 0 1)))
            , ("dimension-axis0", 1, \q -> scan Nothing q{affineLimitDimension = 5} (knownShape @'[999]), Left (TensorAffineError (AffineLimitExceeded AffineDimension 5 6)))
            , ("rank-before-later-dimension", 2, \q -> scan Nothing q{affineLimitRank = 1} (knownShape @'[1, 999]), Left (TensorAffineError (AffineLimitExceeded AffineRank 1 2)))
            , ("list-short-1-of-2", 2, \q -> 0 <$ affineScanList q usage AffineStrides 2 (affineHeaderStart usage) [error "length admission value" :: Int], Left (TensorAffineError (AffineListLength AffineStrides 2 1)))
            , ("list-excess-expected0", 1, \q -> 0 <$ affineScanList q usage AffineStrides 0 (affineHeaderStart usage) (error "excess value" : error "excess tail" :: [Int]), Left (TensorAffineError (AffineListLength AffineStrides 0 1)))
            ]
        checkRow (name, e, operation, expected) = do
            let cells = 4672 + 3124 * e
                work = 512 + 384 * e
                live = 576 + 52 * e
                exact = p{affineLimitConstructedCells = cells, affineLimitWork = work, affineLimitLiveCells = live}
            check (name ++ " exact prefix diagnostic/count") expected (operation exact)
            forM_ [(AffineConstructedCells, exact{affineLimitConstructedCells = cells - 1}, cells - 1), (AffineWork, exact{affineLimitWork = work - 1}, work - 1), (AffineLiveCells, exact{affineLimitLiveCells = live - 1}, live - 1)] $ \(field, capped, cap) ->
                check (name ++ " rejected final cons/nil debit first") (Left (TensorAffineError (AffineLimitExceeded field cap (cap + 1)))) (operation capped)
    mapM_ checkRow rows
    check "T nil uncredited" (Left (TensorAffineError (AffineLimitExceeded AffineWork 896 897))) (scan Nothing p{affineLimitWork = 896} machine)
    check "scalar nil uncredited startup" (Left (TensorAffineError (AffineLimitExceeded AffineConstructedCells 4672 4673))) (scan Nothing p{affineLimitConstructedCells = 4672} SNil)
    check "first debit no input demand" (Left (TensorAffineError (AffineLimitExceeded AffineConstructedCells 4672 4673))) (scan Nothing p{affineLimitConstructedCells = 4672} (error "unpaid input" :: SShape '[]))
    budget <- right (affineBudget p)
    join $ right $ withAffineMap budget (knownShape @'[5]) (knownShape @'[3]) 4 [-2] $ \witness _ _ ->
        checkRow
            ( "transform-rank-b1-s1-r2"
            , 7
            , \q -> do
                start <- affineBudget q
                permuteAffineMap start witness (knownShape @'[1, 3]) (error "rank rejection before permutation") (\_ _ _ -> 0)
            , Left (TensorAffineError (AffineShapeDisagreement AffineTargetShape 0 1 2))
            )

runtimeEntryAndDemand :: IO ()
runtimeEntryAndDemand = do
    p <- policy
    budget <- right (affineBudget p)
    disabled <- withTensorSession oldLimits $ \session -> do
        result <- bindAffineView session (error "disabled map") (error "disabled base")
        reject "disabled before arguments" (TensorAffineError AffineDisabled) result
        pure (Right ())
    right disabled
    join $ right $ withAffineMap budget (knownShape @'[5]) (knownShape @'[3]) 4 [-2] $ \witness _ _ -> do
        forM_ [(50568, maxBound, maxBound, AffineConstructedCells, 50568), (maxBound, 6050, maxBound, AffineWork, 6050), (maxBound, maxBound, 1584, AffineLiveCells, 1584)] $ \(cells, work, live, field, cap) -> do
            limited <- right (affineLimits maxBound maxBound maxBound cells work live)
            result <- withTensorSession (tensorSessionLimitsWithAffine oldLimits limited) $ \session -> do
                before <- snapshot session
                rejected <- bindAffineView session witness (error "base wrapper before full admission")
                reject "full bind admission before actual wrapper" (TensorAffineError (AffineLimitExceeded field cap (cap + 1))) rejected
                check "failed bind unchanged" before =<< snapshot session
                pure (Right ())
            right result
        limited <- right (affineLimits maxBound maxBound maxBound 123441 14894 2129)
        result <- withTensorSession (tensorSessionLimitsWithAffine oldLimits limited) $ \session -> do
            (base, _) <- finiteTensorFromList session (knownShape @'[5]) [1 .. 5] >>= right
            -- n2 is required by this frozen reservation discriminator.
            _ <- finiteTensorFromList session (knownShape @'[3]) [10, 20, 30] >>= right
            owner <- right (tensorOwner "demand" (knownShape @'[5]))
            (bound, _) <- bindAffineView session witness (ownTensor owner base) >>= right
            before <- snapshot session
            rejected <- pullbackAffineView session bound (error "seed wrapper before full admission")
            reject "full pullback admission before seed wrapper" (TensorAffineError (AffineLimitExceeded AffineConstructedCells 123441 123442)) rejected
            check "failed pullback unchanged" before =<< snapshot session
            pure (Right ())
        right result

entryCounterBoundaries :: IO ()
entryCounterBoundaries = do
    p <- policy
    withTensorSession
        (tensorSessionLimitsWithAffine oldLimits p)
        ( \session@(TensorSession limits allocator lock) -> do
            original <- withMVar lock pure
            let m = affineMachineMaximum
                usage = stateAffineUsage original
                counter = TensorAffineError AffineRuntimeCounterOverflow
                cases =
                    [ ("closed precedes disabled and counter", original{stateClosed = True, statePayloadBytes = m + 1}, limits{limitAffine = Nothing}, TensorSessionClosed)
                    , ("disabled precedes counter", original{statePayloadBytes = m + 1}, limits{limitAffine = Nothing}, TensorAffineError AffineDisabled)
                    , ("payload counter", original{statePayloadBytes = m + 1}, limits, counter)
                    , ("scalar counter", original{stateScalarWork = m + 1}, limits, counter)
                    , ("buffer counter", original{stateBuffers = m + 1}, limits, counter)
                    , ("next ID counter", original{stateNextStorage = m + 1}, limits, counter)
                    , ("cells below minimum", original{stateAffineUsage = usage{affineUsedCells = 4671}}, limits, counter)
                    , ("work below minimum", original{stateAffineUsage = usage{affineUsedWork = 511}}, limits, counter)
                    , ("live below minimum", original{stateAffineUsage = usage{affineHighWaterCells = 575}}, limits, counter)
                    , ("cells above cap", original{stateAffineUsage = usage{affineUsedCells = m + 1}}, limits, counter)
                    , ("work above cap", original{stateAffineUsage = usage{affineUsedWork = m + 1}}, limits, counter)
                    , ("live above cap", original{stateAffineUsage = usage{affineHighWaterCells = m + 1}}, limits, counter)
                    ]
            forM_ cases $ \(label, candidate, candidateLimits, expected) -> do
                modifyMVar_ lock (const (pure candidate))
                let checkedSession = TensorSession candidateLimits allocator lock
                before <- snapshot checkedSession
                bindAffineView checkedSession (error "entry map demanded") (error "entry base demanded") >>= reject (label ++ " bind") expected
                pullbackAffineView checkedSession (error "entry binding demanded") (error "entry seed demanded") >>= reject (label ++ " pull") expected
                check (label ++ " complete unchanged state and restored lock") before =<< snapshot checkedSession
            forM_ [original{stateBuffers = m}, original{stateNextStorage = m}] $ \candidate -> do
                _ <- right (affineRuntimeEntry limits candidate False)
                modifyMVar_ lock (const (pure candidate))
                before <- snapshot session
                pullbackAffineView session (error "spare ID entry binding") (error "spare ID entry seed") >>= reject "pullback needs spare ID/buffer" counter
                check "M boundary unchanged" before =<< snapshot session
            _ <- right (affineRuntimeEntry limits original{stateBuffers = m - 1, stateNextStorage = m - 1} True)
            modifyMVar_ lock (const (pure original))
            pure (Right ())
        )
        >>= right

oldPayloadBoundaries :: IO ()
oldPayloadBoundaries = do
    p <- policy
    budget <- right (affineBudget p)
    calls <- newIORef (0 :: Int)
    let allocator = defaultTensorAllocator{allocatorAllocate = \count -> modifyIORef' calls (+ 1) >> allocatorAllocate defaultTensorAllocator count}
    action <- right $ withAffineMap budget (knownShape @'[5]) (knownShape @'[3]) 4 [-2] $ \witness _ _ ->
        withTensorSessionAllocator allocator (tensorSessionLimitsWithAffine oldLimits p) $ \session@(TensorSession limits actualAllocator lock) -> do
            (base, _) <- finiteTensorFromList session (knownShape @'[5]) [1 .. 5] >>= right
            (seed, _) <- finiteTensorFromList session (knownShape @'[3]) [10, 20, 30] >>= right
            owner <- right (tensorOwner "old payload boundary" (knownShape @'[5]))
            (bound, _) <- bindAffineView session witness (ownTensor owner base) >>= right
            let exact = limits{limitSinglePayloadBytes = 40, limitFreshPayloadBytes = 104, limitBuffers = 3, limitScalarWork = 16}
                cases =
                    [ ("single39", exact{limitSinglePayloadBytes = 39}, SinglePayloadLimitExceeded 39 40)
                    , ("fresh103", exact{limitFreshPayloadBytes = 103}, FreshPayloadLimitExceeded 103 104)
                    , ("buffers2", exact{limitBuffers = 2}, BufferLimitExceeded 2 3)
                    , ("scalar15", exact{limitScalarWork = 15}, ScalarWorkLimitExceeded 15 16)
                    ]
            before <- snapshot session
            countBefore <- readIORef calls
            forM_ cases $ \(label, capped, expected) -> do
                let limited = TensorSession capped actualAllocator lock
                pullbackAffineView limited bound (error "old payload rejection demanded seed") >>= reject label (TensorBudgetError expected)
                check (label ++ " allocator delta zero") countBefore =<< readIORef calls
                check (label ++ " complete state preserved") before =<< snapshot session
            let lowAffine = p{affineLimitConstructedCells = 123441}
                competing = TensorSession (exact{limitSinglePayloadBytes = 39, limitAffine = Just lowAffine}) actualAllocator lock
                fullAffine = TensorSession (exact{limitAffine = Just lowAffine}) actualAllocator lock
            let headerFirst = TensorSession (exact{limitSinglePayloadBytes = 39, limitAffine = Just p{affineLimitConstructedCells = 50569}}) actualAllocator lock
            pullbackAffineView headerFirst bound (error "header before old/affine/descriptor") >>= reject "header before old payload and affine suffix" (TensorAffineError (AffineLimitExceeded AffineConstructedCells 50569 50570))
            check "all competing stages preserve complete state" before =<< snapshot session
            pullbackAffineView competing bound (error "old beats affine and descriptor") >>= reject "old before full affine" (TensorBudgetError (SinglePayloadLimitExceeded 39 40))
            pullbackAffineView fullAffine bound (error "affine before descriptor") >>= reject "full affine before descriptor" (TensorAffineError (AffineLimitExceeded AffineConstructedCells 123441 123442))
            (gradient, report) <- pullbackAffineView (TensorSession exact actualAllocator lock) bound seed >>= right
            check "all exact old boundaries" (TensorOperationReport "vjp/affine-base" 8 (TensorMemoryReport 40 40 0 1)) (affineTensorReport report)
            check "old boundary full gradient" [30, 0, 20, 0, 10] =<< tensorToList (hostTensor (ownedFiniteTensor gradient))
            pure (Right ())
    action >>= right

metadataDiagnostics :: IO ()
metadataDiagnostics = do
    p <- policy
    budget <- right (affineBudget p)
    action <- right $ withAffineMap budget (knownShape @'[5]) (knownShape @'[3]) 4 [-2] $ \witness _ _ ->
        withTensorSession (tensorSessionLimitsWithAffine oldLimits p) $ \session -> do
            (base, _) <- finiteTensorFromList session (knownShape @'[5]) [1 .. 5] >>= right
            (seed, _) <- finiteTensorFromList session (knownShape @'[3]) [10, 20, 30] >>= right
            owner <- right (tensorOwner "metadata" (knownShape @'[5]))
            let HostTensor SF64 shape layout identifier _ _ pointer = hostTensor base
                altered count capacity candidate = FiniteTensor (HostTensor SF64 shape candidate identifier count capacity pointer)
                wrongDimensions xs = layout{layoutDimensions = xs}
                wrongStrides xs = layout{layoutStridesElements = xs}
                disagreement expected actual = TensorAffineError (AffineShapeDisagreement AffineBaseShape 0 expected actual)
                machine = TensorShapeError (MachineIndexOverflow (affineMachineMaximum + 1))
                cases =
                    [ ("count and capacity overflow", altered (affineMachineMaximum + 1) (affineMachineMaximum + 1) layout, machine)
                    , ("count before bad lengths", altered 4 5 (layout{layoutDimensions = [], layoutStridesElements = []}), disagreement 5 4)
                    , ("dimension length before stride length", altered 5 5 (layout{layoutDimensions = [], layoutStridesElements = []}), disagreement 1 0)
                    , ("dimension before offset and stride values", altered 5 5 (layout{layoutDimensions = [999], layoutOffsetElements = -1, layoutStridesElements = [error "stride before dimensions"]}), disagreement 5 999)
                    , ("negative stride overflow", altered 5 5 (wrongStrides [negate (toInteger affineMachineMaximum) - 1]), TensorAffineError (AffineArithmeticOverflow AffineStrides 0))
                    , ("extrema multiplication before bounds", altered 5 5 (wrongStrides [toInteger affineMachineMaximum]), TensorAffineError (AffineArithmeticOverflow AffineStrides 0))
                    , ("signed physical failure", altered 5 5 (wrongStrides [-1]), TensorAffineError (AffinePhysicalBounds 0 5 5))
                    , ("count mismatch", altered 4 5 layout, disagreement 5 4)
                    , ("count F+1", altered (affineMachineMaximum `div` 8 + 1) 5 layout, machine)
                    , ("count >M", altered (affineMachineMaximum + 100) 5 layout, machine)
                    , ("capacity before count disagreement", altered 4 (affineMachineMaximum `div` 8 + 1) layout, machine)
                    , ("capacity >M", altered 5 (affineMachineMaximum + 100) layout, machine)
                    , ("short dimensions", altered 5 5 (wrongDimensions []), disagreement 1 0)
                    , ("long dimensions", altered 5 5 (wrongDimensions (5 : error "excess dimension" : error "excess dimension tail")), disagreement 1 2)
                    , ("short strides", altered 5 5 (wrongStrides []), disagreement 1 0)
                    , ("long strides before dimension values", altered 5 5 (layout{layoutDimensions = [error "dimension before length"], layoutStridesElements = 1 : error "excess stride" : error "excess stride tail"}), disagreement 1 2)
                    , ("dimension disagreement", altered 5 5 (wrongDimensions [999]), disagreement 5 999)
                    , ("bounded enormous dimension disagreement", altered 5 5 (wrongDimensions [affineMachineMaximum + 100]), disagreement 5 (affineMachineMaximum + 1))
                    , ("negative offset", altered 5 5 (layout{layoutOffsetElements = -1}), TensorAffineError (AffineArithmeticOverflow AffineOffset 0))
                    , ("overflow offset", altered 5 5 (layout{layoutOffsetElements = toInteger affineMachineMaximum + 1}), TensorAffineError (AffineArithmeticOverflow AffineOffset 0))
                    , ("overflow stride", altered 5 5 (wrongStrides [toInteger affineMachineMaximum + 1]), TensorAffineError (AffineArithmeticOverflow AffineStrides 0))
                    , ("physical interval", altered 5 5 (layout{layoutOffsetElements = 1}), TensorAffineError (AffinePhysicalBounds 1 5 5))
                    , ("contiguity before all metadata", altered (affineMachineMaximum + 1) 5 (layout{layoutIsContiguous = False}), TensorAffineError AffineNonContiguousBase)
                    ]
            before <- snapshot session
            forM_ cases $ \(label, tensor, expected) -> do
                bindAffineView session witness (ownTensor owner tensor) >>= reject label expected
                check (label ++ " atomic state") before =<< snapshot session
            (bound, _) <- bindAffineView session witness (ownTensor owner base) >>= right
            let AffineMap bs vs rb rv bc vc o ss lo hi contiguous = witness
                badMap = AffineMap bs vs rb rv (bc + 1) vc o ss lo hi contiguous
            bindAffineView session badMap (ownTensor owner base) >>= reject "cached base count" (disagreement 5 6)
            let HostTensor SF64 seedShape seedLayout seedId _ seedCapacity seedPointer = hostTensor seed
                wrongSeed = FiniteTensor (HostTensor SF64 seedShape seedLayout seedId 2 seedCapacity seedPointer)
                HostTensor SF64 viewShape viewLayout viewId _ viewCapacity viewPointer = hostTensor (affineViewTensor bound)
                wrongView = FiniteTensor (HostTensor SF64 viewShape viewLayout viewId 2 viewCapacity viewPointer)
                badBinding = OwnedAffineView (affineViewBase bound) witness wrongView
                viewProblem = TensorAffineError (AffineShapeDisagreement AffineViewShape 0 3 2)
            afterBind <- snapshot session
            pullbackAffineView session bound wrongSeed >>= reject "actual seed count" viewProblem
            pullbackAffineView session badBinding (error "stored view must reject before seed") >>= reject "actual stored view before seed" viewProblem
            let cached baseCount viewCount = AffineMap bs vs rb rv baseCount viewCount o ss lo hi contiguous
                rebound = OwnedAffineView
                normalView = affineViewTensor bound
                viewDisagreement expected actual = TensorAffineError (AffineShapeDisagreement AffineViewShape 0 expected actual)
            forM_ [(bc + 1, vc + 1, disagreement 5 6), (bc, vc + 1, viewDisagreement 3 4), (bc, affineMachineMaximum + 1, machine)] $ \(bCount, vCount, expected) -> do
                pullbackAffineView session (rebound (affineViewBase bound) (cached bCount vCount) normalView) (error "cached failure before seed") >>= reject "pull cached B then V ranges/disagreement" expected
            pullbackAffineView session (rebound (ownTensor owner (altered 5 5 layout{layoutIsContiguous = False})) (cached (bc + 1) (vc + 1)) normalView) seed >>= reject "pull contiguity before cached counts" (TensorAffineError AffineNonContiguousBase)
            forM_ cases $ \(label, tensor, expected) -> do
                pullbackAffineView session (rebound (ownTensor owner tensor) witness wrongView) (error "base descriptor before seed") >>= reject ("pull base: " ++ label) expected
                check ("pull base state: " ++ label) afterBind =<< snapshot session
            let alteredView candidate count capacity = FiniteTensor (HostTensor SF64 viewShape candidate viewId count capacity viewPointer)
                alteredSeed candidate count capacity = FiniteTensor (HostTensor SF64 seedShape candidate seedId count capacity seedPointer)
                viewCases =
                    [ ("count range", viewLayout, affineMachineMaximum + 1, viewCapacity, machine)
                    , ("capacity range", viewLayout, 3, affineMachineMaximum + 1, machine)
                    , ("count disagreement", viewLayout, 2, viewCapacity, viewDisagreement 3 2)
                    , ("dimension short", viewLayout{layoutDimensions = []}, 3, viewCapacity, viewDisagreement 1 0)
                    , ("dimension long", viewLayout{layoutDimensions = [3, error "excess dimension"]}, 3, viewCapacity, viewDisagreement 1 2)
                    , ("stride short", viewLayout{layoutStridesElements = []}, 3, viewCapacity, viewDisagreement 1 0)
                    , ("stride long", viewLayout{layoutStridesElements = [-2, error "excess stride"]}, 3, viewCapacity, viewDisagreement 1 2)
                    , ("dimension mismatch", viewLayout{layoutDimensions = [999]}, 3, viewCapacity, viewDisagreement 3 999)
                    , ("offset negative", viewLayout{layoutOffsetElements = -1}, 3, viewCapacity, TensorAffineError (AffineArithmeticOverflow AffineOffset 0))
                    , ("stride range", viewLayout{layoutStridesElements = [negate (toInteger affineMachineMaximum) - 1]}, 3, viewCapacity, TensorAffineError (AffineArithmeticOverflow AffineStrides 0))
                    , ("physical bounds", viewLayout{layoutOffsetElements = 0}, 3, viewCapacity, TensorAffineError (AffinePhysicalBounds 0 3 5))
                    ]
            forM_ viewCases $ \(label, candidate, count, capacity, expected) -> do
                pullbackAffineView session (rebound (affineViewBase bound) witness (alteredView candidate count capacity)) wrongSeed >>= reject ("stored view before seed: " ++ label) expected
                -- Seed faults retain its real capacity3; offset4/stride-2 is only used in rejected rows.
                let seedCandidate = candidate{layoutOffsetElements = if label == "physical bounds" then 0 else layoutOffsetElements candidate}
                pullbackAffineView session bound (alteredSeed seedCandidate count capacity) >>= reject ("actual independent seed: " ++ label) expected
                check (label ++ " view/seed complete atomic state") afterBind =<< snapshot session
            check "metadata failures preserve all counters" afterBind =<< snapshot session
            pure (Right ())
    action >>= right

emptyMetadataDemand :: IO ()
emptyMetadataDemand = do
    p <- policy
    budget <- right (affineBudget p)
    action <- right $ withAffineMap budget (knownShape @'[0, 1]) (knownShape @'[0, 1]) 0 [0, 0] $ \witness _ _ ->
        withTensorSession (tensorSessionLimitsWithAffine oldLimits p) $ \session -> do
            (empty, _) <- finiteTensorFromList session (knownShape @'[0, 1]) [] >>= right
            owner <- right (tensorOwner "empty" (knownShape @'[0, 1]))
            let HostTensor SF64 shape layout identifier count capacity pointer = hostTensor empty
                supplied dims strides = FiniteTensor (HostTensor SF64 shape (layout{layoutDimensions = dims, layoutStridesElements = strides}) identifier count capacity pointer)
            (bound, _) <- bindAffineView session witness (ownTensor owner (supplied [0, 1] [error "irrelevant empty stride", 10 ^ (100 :: Int)])) >>= right
            (gradient, _) <- pullbackAffineView session bound (supplied [0, 1] [error "empty seed stride", error "empty seed stride 2"]) >>= right
            check "empty no payload" [] =<< tensorToList (hostTensor (ownedFiniteTensor gradient))
            bindAffineView session witness (ownTensor owner (supplied [0, 2] [error "wrong dimension before stride", 0])) >>= reject "empty dimension values still checked" (TensorAffineError (AffineShapeDisagreement AffineBaseShape 1 1 2))
            bindAffineView session witness (ownTensor owner (supplied [error "length before dimension", 1] [])) >>= reject "empty stride length before dimension" (TensorAffineError (AffineShapeDisagreement AffineBaseShape 0 2 0))
            pure (Right ())
    action >>= right

emptyDescriptorAnchors :: IO ()
emptyDescriptorAnchors = do
    p <- policy
    budget <- right (affineBudget p)
    action <- right $ withAffineMap budget (knownShape @'[0, 1]) (knownShape @'[0, 1]) 0 [0, 0] $ \witness _ _ ->
        withTensorSession (tensorSessionLimitsWithAffine oldLimits p) $ \session -> do
            -- The storage capacity5 is real and retained. Only the logical empty
            -- descriptor is private; no forged-capacity success is used.
            (backing, _) <- finiteTensorFromList session (knownShape @'[5]) [1 .. 5] >>= right
            let HostTensor SF64 _ _ identifier _ capacity pointer = hostTensor backing
                shape = knownShape @'[0, 1]
                layout = CheckedLayout [0, 1] 5 [error "empty stride zero", error "empty stride one"] True
                empty candidate = FiniteTensor (HostTensor SF64 shape candidate identifier 0 capacity pointer)
            owner <- right (tensorOwner "real empty capacity" shape)
            (bound, _) <- bindAffineView session witness (ownTensor owner (empty layout)) >>= right
            (g, _) <- pullbackAffineView session bound (empty layout) >>= right
            check "offset=capacity empty base/view/seed accepted without stride values" [] =<< tensorToList (hostTensor (ownedFiniteTensor g))
            before <- snapshot session
            let badCases input =
                    [ ("anchor beyond capacity", layout{layoutOffsetElements = 6}, TensorAffineError (AffinePhysicalBounds 6 0 5))
                    , ("negative anchor", layout{layoutOffsetElements = -1}, TensorAffineError (AffineArithmeticOverflow AffineOffset 0))
                    , ("nonmachine anchor", layout{layoutOffsetElements = toInteger affineMachineMaximum + 1}, TensorAffineError (AffineArithmeticOverflow AffineOffset 0))
                    , ("later short dimensions", layout{layoutDimensions = [0]}, TensorAffineError (AffineShapeDisagreement input 0 2 1))
                    , ("later long dimensions", layout{layoutDimensions = 0 : 1 : error "excess dimension value" : error "excess dimension tail"}, TensorAffineError (AffineShapeDisagreement input 0 2 3))
                    , ("later short strides", layout{layoutStridesElements = [error "short stride value"]}, TensorAffineError (AffineShapeDisagreement input 0 2 1))
                    , ("later long strides", layout{layoutStridesElements = error "stride0" : error "stride1" : error "excess stride value" : error "excess stride tail"}, TensorAffineError (AffineShapeDisagreement input 0 2 3))
                    , ("later dimension disagreement", layout{layoutDimensions = [0, 999]}, TensorAffineError (AffineShapeDisagreement input 1 1 999))
                    ]
            forM_ (badCases AffineBaseShape) $ \(label, candidate, expected) -> do
                bindAffineView session witness (ownTensor owner (empty candidate)) >>= reject ("empty base bind " ++ label) expected
                let wrongBase = OwnedAffineView (ownTensor owner (empty candidate)) witness (affineViewTensor bound)
                pullbackAffineView session wrongBase (error "base before seed") >>= reject ("empty base pull " ++ label) expected
                check (label ++ " base state") before =<< snapshot session
            forM_ (badCases AffineViewShape) $ \(label, candidate, expected) -> do
                let wrongView = OwnedAffineView (affineViewBase bound) witness (empty candidate)
                pullbackAffineView session wrongView (error "stored view before seed") >>= reject ("empty stored view " ++ label) expected
                pullbackAffineView session bound (empty candidate) >>= reject ("empty actual seed " ++ label) expected
                check (label ++ " view/seed state") before =<< snapshot session
            pure (Right ())
    action >>= right

allocationRollback :: IO ()
allocationRollback = do
    p <- policy
    budget <- right (affineBudget p)
    calls <- newIORef (0 :: Int)
    released <- newIORef (0 :: Int)
    failing <- newIORef False
    let allocator =
            TensorAllocator
                (\count -> do modifyIORef' calls (+ 1); bad <- readIORef failing; if bad then pure (Left "affine allocation fault") else allocatorAllocate defaultTensorAllocator count)
                (\pointer -> do modifyIORef' released (+ 1); allocatorFinalize defaultTensorAllocator pointer)
    action <- right $ withAffineMap budget (knownShape @'[5]) (knownShape @'[3]) 4 [-2] $ \witness _ _ ->
        withTensorSessionAllocator allocator (tensorSessionLimitsWithAffine oldLimits p) $ \session -> do
            (base, _) <- finiteTensorFromList session (knownShape @'[5]) [1 .. 5] >>= right
            (seed, _) <- finiteTensorFromList session (knownShape @'[3]) [10, 20, 30] >>= right
            owner <- right (tensorOwner "rollback" (knownShape @'[5]))
            (bound, _) <- bindAffineView session witness (ownTensor owner base) >>= right
            before <- snapshot session
            writeIORef failing True
            pullbackAffineView session bound seed >>= reject "allocator diagnostic" (HostAllocationFailure "allocation-1: affine allocation fault")
            check "allocation failure restores both ledgers and ID" before =<< snapshot session
            writeIORef failing False
            (gradient, _) <- pullbackAffineView session bound seed >>= right
            check "uncommitted ID reused" (StorageId 2) (tensorStorageId (hostTensor (ownedFiniteTensor gradient)))
            check "retry reads actual seed" [30, 0, 20, 0, 10] =<< tensorToList (hostTensor (ownedFiniteTensor gradient))
            pure (Right ())
    action >>= right
    check "allocation attempt count" 4 =<< readIORef calls
    check "only disclosed committed pointers cleaned" 3 =<< readIORef released

constructorCountReuse :: IO ()
constructorCountReuse = do
    result <- withTensorSession oldLimits $ \session -> do
        let shape = knownShape @'[9223372036854775807, 9223372036854775807, 9223372036854775807, 0]
            counts (FiniteTensor (HostTensor _ _ _ _ count capacity _)) = (count, capacity)
        (one, _) <- makeFinite session "count/one" 0 shape [] >>= right
        check "makeFinite admitted late-zero count" (0, 0) (counts one)
        ((left, rightTensor), _) <- makeTwo session "count/two" 0 shape [] [] >>= right
        check "makeTwo left count" (0, 0) (counts left)
        check "makeTwo right count" (0, 0) (counts rightTensor)
        ((empty, scalar), _) <- makeTwoShapes session "count/distinct" 1 shape [] SNil [9] >>= right
        check "makeTwoShapes own left count" (0, 0) (counts empty)
        check "makeTwoShapes own right count" (1, 1) (counts scalar)
        ((vector, other), _) <- makeTwoShapes session "count/ordinary" 3 (knownShape @'[2]) [1, 2] SNil [3] >>= right
        check "ordinary distinct counts" ((2, 2), (1, 1)) (counts vector, counts other)
        pure (Right ())
    right result
