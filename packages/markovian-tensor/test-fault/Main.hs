{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent.MVar (withMVar)
import Control.Exception (AsyncException (ThreadKilled), SomeException, displayException, fromException, throw, throwIO, try)
import Control.Monad (unless)
import Data.IORef
import Data.List (isInfixOf)
import Foreign.ForeignPtr (finalizeForeignPtr, mallocForeignPtrArray)
import Markovian.Tensor.Internal

limits :: SessionLimits
limits = tensorSessionLimits 2 8 64 512 4096 16 4096

data FaultState = FaultState
    { allocationCalls :: !(IORef Int)
    , finalizationCalls :: !(IORef Int)
    , liveAllocations :: !(IORef Int)
    , failAllocationAt :: !(IORef [Int])
    , failFinalizationAt :: !(IORef [Int])
    }

main :: IO ()
main = do
    firstAllocationFailure
    secondAllocationFailureAndRetry
    batchPreflightAndAllocationAtomicity
    cleanupFailureDiagnostics
    asynchronousStagingAndCleanup
    closeOnEveryExit
    putStrLn "markovian-tensor: deterministic allocator fault tests passed"

firstAllocationFailure :: IO ()
firstAllocationFailure = do
    faults <- newFaultState [1] []
    result <- withTensorSessionAllocator (faultAllocator faults) limits $ \session -> do
        failed <- allocatePayloads session "fault/first" 2 [([1], 8), ([2], 8)]
        assertAllocationFailure "first failure" "allocation-1: injected allocation failure 1" failed
        assertSession "first failure is atomic" session (0, 0, 0, 0)
        retried <- allocatePayloads session "fault/retry" 1 [([3], 8)]
        case retried of
            Right ([(StorageId 0, _)], _) -> pure ()
            _ -> fail "first-failure retry did not reuse uncommitted storage ID"
        pure (Right ())
    assertRight "first allocation session" result
    assertRef "first failure live allocations" 0 (liveAllocations faults)
    assertRef "first failure cleanup count" 1 (finalizationCalls faults)

secondAllocationFailureAndRetry :: IO ()
secondAllocationFailureAndRetry = do
    faults <- newFaultState [2] []
    result <- withTensorSessionAllocator (faultAllocator faults) limits $ \session -> do
        failed <- allocatePayloads session "fault/second" 2 [([1], 8), ([2], 8)]
        assertAllocationFailure "second failure" "allocation-2: injected allocation failure 2" failed
        assertSession "second failure rollback" session (0, 0, 0, 0)
        assertRef "first staged allocation finalized" 1 (finalizationCalls faults)
        assertRef "rollback has no live allocation" 0 (liveAllocations faults)
        retried <- allocatePayloads session "fault/retry-two" 2 [([3], 8), ([4], 8)]
        case retried of
            Right ([(StorageId 0, _), (StorageId 1, _)], report) -> do
                unless (reportAllocationCount (reportMemory report) == 2) (fail "retry report changed")
                assertSession "retry commits atomically" session (16, 2, 2, 2)
            _ -> fail "second-failure retry did not commit both outputs"
        pure (Right ())
    assertRight "second allocation session" result
    assertRef "second failure live allocations" 0 (liveAllocations faults)
    assertRef "rollback plus close finalizations" 3 (finalizationCalls faults)

batchPreflightAndAllocationAtomicity :: IO ()
batchPreflightAndAllocationAtomicity = do
    preflightFaults <- newFaultState [] []
    preflight <- withTensorSessionAllocator (faultAllocator preflightFaults) limits $ \session -> do
        rejected <- hostTensorBatchFromLists session [([1], [1]), ([9], [2])]
        case rejected of
            Left (TensorShapeError (DimensionLimitExceeded 8 9)) -> pure ()
            _ -> fail "batch did not reject the complete shape plan"
        assertRef "batch preflight allocator calls" 0 (allocationCalls preflightFaults)
        assertSession "batch preflight state" session (0, 0, 0, 0)
        pure (Right ())
    assertRight "batch preflight session" preflight

    allocationFaults <- newFaultState [2] []
    allocation <- withTensorSessionAllocator (faultAllocator allocationFaults) limits $ \session -> do
        rejected <- hostTensorBatchFromLists session [([1], [1]), ([1], [2])]
        assertAllocationFailure "batch second allocation" "allocation-2: injected allocation failure 2" rejected
        assertSession "batch allocation rollback" session (0, 0, 0, 0)
        assertRef "batch staged cleanup" 1 (finalizationCalls allocationFaults)
        retried <- hostTensorBatchFromLists session [([1], [3]), ([1], [4])]
        case retried of
            Right (tensors, report) -> do
                unless (length tensors == 2) (fail "batch retry changed output count")
                unless (reportAllocationCount (reportMemory report) == 2) (fail "batch retry report changed")
                assertSession "batch retry commits atomically" session (16, 2, 2, 2)
            Left problem -> fail ("batch retry failed: " ++ show problem)
        pure (Right ())
    assertRight "batch allocation session" allocation
    assertRef "batch allocation live after close" 0 (liveAllocations allocationFaults)
    assertRef "batch rollback plus close finalizations" 3 (finalizationCalls allocationFaults)

    thrownFaults <- newFaultState [] []
    thrown <- withTensorSessionAllocator (throwSecondAllocationAllocator thrownFaults) limits $ \session -> do
        rejected <- hostTensorBatchFromLists session [([1], [1]), ([1], [2])]
        case rejected of
            Left (HostAllocationFailure message)
                | "allocation-2: user error (injected thrown allocation 2)" `isInfixOf` message -> pure ()
            _ -> fail "thrown allocator failure did not become an atomic checked failure"
        assertSession "thrown allocation rollback" session (0, 0, 0, 0)
        pure (Right ())
    assertRight "thrown allocation session" thrown
    assertRef "thrown allocation staged cleanup" 1 (finalizationCalls thrownFaults)
    assertRef "thrown allocation live after close" 0 (liveAllocations thrownFaults)

cleanupFailureDiagnostics :: IO ()
cleanupFailureDiagnostics = do
    faults <- newFaultState [2] [1]
    result <- withTensorSessionAllocator (faultAllocator faults) limits $ \session -> do
        failed <- allocatePayloads session "fault/cleanup" 2 [([1], 8), ([2], 8)]
        case failed of
            Left (HostAllocationFailure message) ->
                unless (message == "allocation-2: injected allocation failure 2; cleanup-1: injected cleanup failure 1") (fail ("cleanup diagnostic changed: " ++ message))
            _ -> fail "cleanup failure did not preserve primary allocation failure"
        assertSession "cleanup failure remains uncommitted" session (0, 0, 0, 0)
        pure (Right ())
    assertRight "cleanup diagnostic session" result
    assertRef "cleanup-failure allocator still releases" 0 (liveAllocations faults)

asynchronousStagingAndCleanup :: IO ()
asynchronousStagingAndCleanup = do
    second <- newFaultState [] []
    secondResult <- try @AsyncException $ withTensorSessionAllocator (asyncSecondAllocationAllocator second) limits $ \session -> do
        _ <- allocatePayloads session "async/second" 2 [([1], 8), ([2], 8)]
        pure (Right ())
    case secondResult of
        Left ThreadKilled -> pure ()
        _ -> fail "second-allocation interruption was not rethrown"
    assertRef "second-allocation interruption zero live" 0 (liveAllocations second)
    assertRef "second-allocation interruption cleanup" 1 (finalizationCalls second)

    initialization <- newFaultState [] []
    let interruptedValues = [1, throw ThreadKilled]
    initializationResult <- try @AsyncException $ withTensorSessionAllocator (faultAllocator initialization) limits $ \session -> do
        _ <- allocatePayloads session "async/initialization" 2 [(interruptedValues, 16)]
        pure (Right ())
    case initializationResult of
        Left ThreadKilled -> pure ()
        _ -> fail "initialization interruption was not rethrown"
    assertRef "initialization interruption zero live" 0 (liveAllocations initialization)
    assertRef "initialization interruption cleanup" 1 (finalizationCalls initialization)

    cleanup <- newFaultState [] []
    cleanupResult <- try @TensorSessionException $ withTensorSessionAllocator (asyncFirstCleanupAllocator cleanup) limits $ \session -> do
        requireAllocation session
        _ <- allocatePayloads session "async/cleanup-second" 1 [([2], 8)]
        pure (Right ())
    case cleanupResult of
        Left (TensorSessionException primary diagnostics) -> do
            unless (case fromException primary :: Maybe AsyncException of Just ThreadKilled -> True; _ -> False) (fail "cleanup interruption primary changed")
            unless (any (isInfixOf "asynchronous interruption") diagnostics) (fail "cleanup interruption diagnostic missing")
        _ -> fail "cleanup interruption was not retained"
    assertRef "cleanup interruption retry and remaining attempts" 3 (finalizationCalls cleanup)
    assertRef "cleanup interruption zero live" 0 (liveAllocations cleanup)

    actionAndCleanup <- newFaultState [] [1]
    combined <- try @TensorSessionException $ withTensorSessionAllocator (faultAllocator actionAndCleanup) limits $ \session -> do
        requireAllocation session
        throwIO ThreadKilled
    case combined of
        Left (TensorSessionException primary ["cleanup-1: injected cleanup failure 1"]) ->
            unless (case fromException primary :: Maybe AsyncException of Just ThreadKilled -> True; _ -> False) (fail "action interruption primary changed")
        _ -> fail "action interruption plus cleanup failure was not retained"
    assertRef "action interruption plus cleanup zero live" 0 (liveAllocations actionAndCleanup)

    checkedAndInterruptedCleanup <- newFaultState [] []
    checkedCombined <- try @TensorSessionFailureException $ withTensorSessionAllocator (asyncFirstCleanupAllocator checkedAndInterruptedCleanup) limits $ \session -> do
        requireAllocation session
        pure (Left (HostAllocationFailure "checked-primary"))
    case checkedCombined of
        Left (TensorSessionFailureException (HostAllocationFailure "checked-primary") interruption diagnostics) -> do
            unless (case fromException interruption :: Maybe AsyncException of Just ThreadKilled -> True; _ -> False) (fail "checked primary cleanup interruption changed")
            unless (any (isInfixOf "asynchronous interruption") diagnostics) (fail "checked primary cleanup interruption diagnostic missing")
        _ -> fail "checked primary plus cleanup interruption was not retained"
    assertRef "checked primary cleanup interruption retry" 2 (finalizationCalls checkedAndInterruptedCleanup)
    assertRef "checked primary plus cleanup interruption zero live" 0 (liveAllocations checkedAndInterruptedCleanup)

closeOnEveryExit :: IO ()
closeOnEveryExit = do
    success <- newFaultState [] []
    assertRight "success close" =<< withTensorSessionAllocator (faultAllocator success) limits (allocateOneThen (Right ()))
    assertRef "success close live" 0 (liveAllocations success)

    leftExit <- newFaultState [] []
    result <- withTensorSessionAllocator (faultAllocator leftExit) limits (allocateOneThen (Left (HostAllocationFailure "action-left")))
    case result of
        Left (HostAllocationFailure "action-left") -> pure ()
        _ -> fail "Left exit primary changed"
    assertRef "Left close live" 0 (liveAllocations leftExit)

    exceptional <- newFaultState [] []
    thrown <- try @SomeException $ withTensorSessionAllocator (faultAllocator exceptional) limits $ \session -> do
        _ <- requireAllocation session
        error "action exception"
    case thrown of
        Left _ -> pure ()
        Right _ -> fail "action exception was swallowed"
    assertRef "exception close live" 0 (liveAllocations exceptional)

    exceptionalCleanup <- newFaultState [] [1]
    combinedException <- try @TensorSessionException $ withTensorSessionAllocator (faultAllocator exceptionalCleanup) limits $ \session -> do
        _ <- requireAllocation session
        error "action exception with cleanup"
    case combinedException of
        Left (TensorSessionException primary ["cleanup-1: injected cleanup failure 1"])
            | "action exception with cleanup" `isInfixOf` displayException primary -> pure ()
        _ -> fail "action exception plus cleanup diagnostic changed"
    assertRef "exception-plus-cleanup close live" 0 (liveAllocations exceptionalCleanup)

    cleanup <- newFaultState [] [1]
    cleanupResult <- withTensorSessionAllocator (faultAllocator cleanup) limits (allocateOneThen (Right ()))
    case cleanupResult of
        Left (TensorSessionCleanupFailure ["cleanup-1: injected cleanup failure 1"]) -> pure ()
        _ -> fail "close cleanup diagnostic changed"
    assertRef "cleanup-error close live" 0 (liveAllocations cleanup)

    primaryAndCleanup <- newFaultState [] [1]
    combined <- withTensorSessionAllocator (faultAllocator primaryAndCleanup) limits (allocateOneThen (Left (HostAllocationFailure "primary")))
    case combined of
        Left (TensorPrimaryAndCleanupFailure (HostAllocationFailure "primary") ["cleanup-1: injected cleanup failure 1"]) -> pure ()
        _ -> fail "primary plus cleanup diagnostic changed"
    assertRef "primary-plus-cleanup close live" 0 (liveAllocations primaryAndCleanup)
  where
    allocateOneThen final session = requireAllocation session >> pure final

requireAllocation :: TensorSession region -> IO ()
requireAllocation session = do
    allocated <- allocatePayloads session "fault/one" 1 [([1], 8)]
    case allocated of
        Right _ -> pure ()
        Left problem -> fail ("setup allocation failed: " ++ show problem)

newFaultState :: [Int] -> [Int] -> IO FaultState
newFaultState allocationFailures cleanupFailures =
    FaultState <$> newIORef 0 <*> newIORef 0 <*> newIORef 0 <*> newIORef allocationFailures <*> newIORef cleanupFailures

asyncSecondAllocationAllocator :: FaultState -> TensorAllocator
asyncSecondAllocationAllocator state =
    TensorAllocator
        { allocatorAllocate = \count -> do
            call <- next (allocationCalls state)
            if call == 2
                then throwIO ThreadKilled
                else do
                    pointer <- mallocForeignPtrArray count
                    modifyIORef' (liveAllocations state) (+ 1)
                    pure (Right pointer)
        , allocatorFinalize = allocatorFinalize (faultAllocator state)
        }

asyncFirstCleanupAllocator :: FaultState -> TensorAllocator
asyncFirstCleanupAllocator state =
    TensorAllocator
        { allocatorAllocate = allocatorAllocate (faultAllocator state)
        , allocatorFinalize = \pointer -> do
            call <- next (finalizationCalls state)
            if call == 1
                then throwIO ThreadKilled
                else do
                    finalizeForeignPtr pointer
                    modifyIORef' (liveAllocations state) (subtract 1)
                    pure (Right ())
        }

throwSecondAllocationAllocator :: FaultState -> TensorAllocator
throwSecondAllocationAllocator state =
    TensorAllocator
        { allocatorAllocate = \count -> do
            call <- next (allocationCalls state)
            if call == 2
                then ioError (userError "injected thrown allocation 2")
                else do
                    pointer <- mallocForeignPtrArray count
                    modifyIORef' (liveAllocations state) (+ 1)
                    pure (Right pointer)
        , allocatorFinalize = allocatorFinalize (faultAllocator state)
        }

faultAllocator :: FaultState -> TensorAllocator
faultAllocator state =
    TensorAllocator
        { allocatorAllocate = \count -> do
            call <- next (allocationCalls state)
            shouldFail <- consume call (failAllocationAt state)
            if shouldFail
                then pure (Left ("injected allocation failure " ++ show call))
                else do
                    pointer <- mallocForeignPtrArray count
                    modifyIORef' (liveAllocations state) (+ 1)
                    pure (Right pointer)
        , allocatorFinalize = \pointer -> do
            call <- next (finalizationCalls state)
            finalizeForeignPtr pointer
            modifyIORef' (liveAllocations state) (subtract 1)
            shouldFail <- consume call (failFinalizationAt state)
            pure $ if shouldFail then Left ("injected cleanup failure " ++ show call) else Right ()
        }

next :: IORef Int -> IO Int
next reference = atomicModifyIORef' reference $ \value -> let nextValue = value + 1 in (nextValue, nextValue)

consume :: Int -> IORef [Int] -> IO Bool
consume target reference = atomicModifyIORef' reference $ \values -> (filter (/= target) values, target `elem` values)

assertSession :: String -> TensorSession region -> (Integer, Integer, Integer, Int) -> IO ()
assertSession label (TensorSession _ _ lock) (bytes, buffers, work, live) = withMVar lock $ \state -> do
    actualLive <- readIORefForPointers (stateLiveAllocations state)
    let actual = (toInteger (statePayloadBytes state), toInteger (stateBuffers state), toInteger (stateScalarWork state), actualLive)
    unless (actual == (bytes, buffers, work, live)) (fail (label ++ ": expected " ++ show (bytes, buffers, work, live) ++ ", got " ++ show actual))
  where
    readIORefForPointers = pure . length

assertAllocationFailure :: String -> String -> Either TensorError value -> IO ()
assertAllocationFailure label expected result = case result of
    Left (HostAllocationFailure actual) -> unless (actual == expected) (fail (label ++ ": " ++ actual))
    _ -> fail (label ++ ": wrong result")

assertRef :: String -> Int -> IORef Int -> IO ()
assertRef label expected reference = do
    actual <- readIORef reference
    unless (actual == expected) (fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual))

assertRight :: String -> Either TensorError value -> IO ()
assertRight _ (Right _) = pure ()
assertRight label (Left problem) = fail (label ++ ": " ++ show problem)
