{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module PublicationTests (publicationTests) where

import Control.Concurrent (forkFinally, killThread, myThreadId, throwTo)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar, withMVar)
import Control.Exception (AsyncException (ThreadKilled), Exception (displayException), SomeException, evaluate, finally, fromException, mask, throw, throwIO, try)
import Control.Monad (forM_, unless)
import Data.IORef
import Data.Proxy (Proxy (..))
import Foreign.ForeignPtr (ForeignPtr, finalizeForeignPtr, mallocForeignPtrArray)
import Markovian.Tensor.Internal
import Markovian.Tensor.Shape (SShape (..))
import System.Timeout (timeout)

publicationTests :: IO ()
publicationTests = do
    forM_ [0, 1, 2] metadataRollback
    initializerRollback
    cleanupExceptionRestoresLock
    cleanupSuccessPreservesPrimary
    cleanupDiagnosticFaults
    maskedPreparationInterruption
    committedResultException
    emptyAndWorkOnlyCommits
    allExitCallerJoins
    -- Full-spine forcing is not deep pointer/value forcing.
    forceAllocationSpine [throw (userError "unforced pointer head") :: ForeignPtr Double]
    putStrLn "publication: metadata/initializer/cleanup/async/zero-work-only tests passed"

limits :: SessionLimits
limits = tensorSessionLimits 2 8 64 512 8192 2048 8192

-- The strict primitive field faults in the actual allocatePayloads report
-- preparation, after staging and registry construction, not in a model helper.
metadataRollback :: Int -> IO ()
metadataRollback count = do
    (allocator, allocated, finalized) <- counters
    outcome <- withTensorSessionAllocator allocator limits $ \session -> do
        _ <- require =<< allocatePayloads session "prior" 1 [([1], 8)]
        before <- snapshot session
        failure <- try @SomeException (allocatePayloads session (throw (userError "report preparation fault")) (fromIntegral count) (replicate count ([2], 8)))
        expectException "report preparation" failure
        equal "metadata restores old state and ID" before =<< boundedSnapshot session
        equal "metadata cleans only fresh pointers" count =<< readIORef finalized
        retry <- require =<< allocatePayloads session "retry" 1 [([3], 8)]
        equal "retry reuses uncommitted ID" [StorageId 1] (map fst (fst retry))
        pure (Right ())
    _ <- require outcome
    allocationCount <- readIORef allocated
    equal "metadata all allocated callbacks discharged" allocationCount =<< readIORef finalized

initializerRollback :: IO ()
initializerRollback = do
    (allocator, _, finalized) <- counters
    outcome <- withTensorSessionAllocator allocator limits $ \session -> do
        let TensorSession _ _ lock = session
        before <- snapshot session
        prepared <- prepareSessionCommit lock $ \state -> do
            result <- withStagedInitializers allocator [(1, const (pure ())), (1, const (throwIO (userError "initializer fault")))] (const (fail "must not prepare"))
            case result of
                Left (primary, _) -> do
                    equal "initializer index" "allocation-2: user error (initializer fault)" primary
                    pure (state, ())
                Right _ -> fail "initializer fault accepted"
        evaluate prepared
        equal "initializer rollback state" before =<< snapshot session
        equal "both staged pointers cleaned" 2 =<< readIORef finalized
        pure (Right ())
    _ <- require outcome
    pure ()

data PublicationFault
    = MetadataPrimaryFault
    | CleanupInterpretationFault
    | DiagnosticStringFault
    | DiagnosticRendererFault
    deriving (Eq, Show)

instance Exception PublicationFault

data ThrowingCleanupRenderer = ThrowingCleanupRenderer
    deriving (Show)

instance Exception ThrowingCleanupRenderer where
    displayException _ = throw DiagnosticRendererFault

-- Interpreting a bottom Either is distinct from inspecting a diagnostic String.
-- The successful-finalizer source mutant must fail this exact exception check,
-- rather than passing merely because the metadata primary was rethrown.
cleanupExceptionRestoresLock :: IO ()
cleanupExceptionRestoresLock =
    cleanupResultRestoresLock "cleanup interpretation" CleanupInterpretationFault (pure (throw CleanupInterpretationFault))

cleanupSuccessPreservesPrimary :: IO ()
cleanupSuccessPreservesPrimary =
    cleanupResultRestoresLock "successful cleanup" MetadataPrimaryFault (pure (Right ()))

cleanupResultRestoresLock :: String -> PublicationFault -> IO (Either String ()) -> IO ()
cleanupResultRestoresLock label expected finalize = withRetainedFinalizer finalize $ \allocator calls -> do
    outcome <- withTensorSessionAllocator allocator limits $ \session -> do
        before <- snapshot session
        failed <- try @SomeException (allocatePayloads session (throw MetadataPrimaryFault) 1 [([1], 8)])
        expectPublicationFault label expected failed
        equal (label ++ " restores state and MVar") before =<< boundedSnapshot session
        equal (label ++ " one cleanup owner") 1 =<< readIORef calls
        pure (Right ())
    _ <- require outcome
    equal (label ++ " no duplicate close cleanup") 1 =<< readIORef calls
    putStrLn (label ++ ": observed " ++ show expected ++ "; state restored; cleanup calls=1")

cleanupDiagnosticFaults :: IO ()
cleanupDiagnosticFaults = do
    cleanupDiagnosticFault "diagnostic String" DiagnosticStringFault (pure (Left (throw DiagnosticStringFault)))
    cleanupDiagnosticFault "exception renderer" DiagnosticRendererFault (throwIO ThrowingCleanupRenderer)

-- Production retains the diagnostic lazily in TensorSessionException. Allocation
-- has already restored the state and finished its cleanup attempt when the caller
-- demands displayException below. Do not move that demand into production.
cleanupDiagnosticFault :: String -> PublicationFault -> IO (Either String ()) -> IO ()
cleanupDiagnosticFault label expected finalize = withRetainedFinalizer finalize $ \allocator calls -> do
    outcome <- withTensorSessionAllocator allocator limits $ \session -> do
        before <- snapshot session
        failed <- try @SomeException (allocatePayloads session (throw MetadataPrimaryFault) 1 [([1], 8)])
        combined <- case failed of
            Left problem -> case fromException problem of
                Just value@(TensorSessionException primary diagnostics) -> do
                    equal (label ++ " retains metadata primary") (Just MetadataPrimaryFault) (fromException primary)
                    case diagnostics of
                        [diagnostic] -> equal (label ++ " defined diagnostic prefix") "cleanup-1: " (take 11 diagnostic)
                        _ -> fail (label ++ ": wrong diagnostic count")
                    pure value
                Nothing -> fail (label ++ ": expected deferred TensorSessionException, not a rendering fault")
            Right _ -> fail (label ++ ": metadata exception was swallowed")
        equal (label ++ " state restored before rendering") before =<< boundedSnapshot session
        equal (label ++ " cleanup completed before rendering") 1 =<< readIORef calls
        rendered <- try @SomeException (evaluate (length (displayException combined)))
        expectPublicationFault (label ++ " caller rendering") expected rendered
        equal (label ++ " caller rendering leaves state unchanged") before =<< boundedSnapshot session
        equal (label ++ " rendering does not retry cleanup") 1 =<< readIORef calls
        pure (Right ())
    _ <- require outcome
    equal (label ++ " close has no staged pointers") 1 =<< readIORef calls
    putStrLn (label ++ ": deferred " ++ show expected ++ " at caller rendering; state unchanged; cleanup calls=1")

-- Logical finalizer callbacks intentionally retain physical payloads. No test
-- reads them after cleanup; physical finalization happens only in this finally.
withRetainedFinalizer :: IO (Either String ()) -> (TensorAllocator -> IORef Int -> IO value) -> IO value
withRetainedFinalizer finalize action = do
    retained <- newIORef []
    calls <- newIORef 0
    let allocator =
            TensorAllocator
                { allocatorAllocate = \count -> do
                    pointer <- mallocForeignPtrArray count
                    modifyIORef' retained (pointer :)
                    pure (Right pointer)
                , allocatorFinalize = \_ -> do
                    modifyIORef' calls (+ 1)
                    finalize
                }
    action allocator calls `finally` (readIORef retained >>= mapM_ finalizeForeignPtr)

expectPublicationFault :: String -> PublicationFault -> Either SomeException value -> IO ()
expectPublicationFault label expected outcome = case outcome of
    Left problem -> equal (label ++ " exception identity") (Just expected) (fromException problem)
    Right _ -> fail (label ++ ": exception was swallowed")

-- Delivery is at an interruptible masked initializer, not an assumed async
-- delivery point in a pure loop. Every spawned worker is joined on every exit.
maskedPreparationInterruption :: IO ()
maskedPreparationInterruption = do
    (allocator, _, finalized) <- counters
    outcome <- withTensorSessionAllocator allocator limits $ \session -> do
        let TensorSession _ _ lock = session
        before <- snapshot session
        entered <- newEmptyMVar
        gate <- newEmptyMVar
        done <- newEmptyMVar
        mask $ \restore -> do
            worker <-
                forkFinally
                    ( prepareSessionCommit lock $ \state -> do
                        staged <- withStagedInitializers allocator [(1, \_ -> putMVar entered () >> takeMVar gate)] (const (pure ()))
                        pure (state, staged)
                    )
                    (putMVar done)
            let join = do
                    killThread worker
                    result <- takeMVar done
                    case result of
                        Left _ -> pure ()
                        Right _ -> fail "interrupted initializer unexpectedly completed"
            restore (takeMVar entered) `finally` join
        equal "async restores counters and lock" before =<< boundedSnapshot session
        equal "async one staged cleanup" 1 =<< readIORef finalized
        pure (Right ())
    _ <- require outcome
    pure ()

committedResultException :: IO ()
committedResultException = do
    (allocator, _, finalized) <- counters
    outcome <- withTensorSessionAllocator allocator limits $ \session -> do
        interrupted <- try @AsyncException $ do
            _ <- require =<< allocatePayloads session "committed" 1 [([1], 8)]
            throwIO ThreadKilled
        expectException "after committed return" interrupted
        equal "postcommit has no staged cleanup" 0 =<< readIORef finalized
        equal "postcommit retains accounting" (1, 8, 1, 1, 1, False) =<< snapshot session
        pure (Right ())
    _ <- require outcome
    equal "session owns postcommit cleanup" 1 =<< readIORef finalized

emptyAndWorkOnlyCommits :: IO ()
emptyAndWorkOnlyCommits = do
    (allocator, allocated, _) <- counters
    outcome <- withTensorSessionAllocator allocator limits $ \session -> do
        (_, report) <- require =<< hostTensorBatchFromLists session []
        equal "empty batch report" (TensorOperationReport "from-lists" 0 (TensorMemoryReport 0 0 0 0)) report
        (pointers, workReport) <- require =<< allocatePayloads session "work-only" 7 []
        equal "work-only no output" 0 (length pointers)
        equal "work-only report" (TensorOperationReport "work-only" 7 (TensorMemoryReport 0 0 0 0)) workReport
        equal "zero-buffer commits no allocator" 0 =<< readIORef allocated
        equal "work-only accounting" (0, 0, 0, 7, 0, False) =<< snapshot session
        _ <- closeTensorSession session
        rejected <- hostTensorBatchFromLists session []
        case rejected of
            Left TensorSessionClosed -> pure ()
            _ -> fail "closed empty batch bypassed checks"
        pure (Right ())
    _ <- require outcome
    pure ()

-- These joins belong to the caller, not to the session runner. The omitted-join
-- negative below uses only ordering markers, never an out-of-lifetime read.
allExitCallerJoins :: IO ()
allExitCallerJoins = do
    forM_ ["success", "Left", "synchronous", "asynchronous"] $ \exit -> do
        events <- newIORef ([] :: [String])
        let record event = modifyIORef' events (++ [event])
            allocator = defaultTensorAllocator{allocatorFinalize = \pointer -> record "close" >> allocatorFinalize defaultTensorAllocator pointer}
        outcome <- try @SomeException $ withTensorSessionAllocator allocator limits $ \session -> do
            (tensor, _) <- hostTensorFromList session SF64 (SCons (Proxy @1) SNil) [7] >>= require
            mask $ \restore -> do
                start <- newEmptyMVar
                done <- newEmptyMVar
                worker <- forkFinally (takeMVar start >> tensorToList tensor >>= equal "live caller child copied values" [7] >> record "dependent-work") (putMVar done)
                let join = do
                        killThread worker
                        _ <- takeMVar done
                        record "joined"
                    body = case exit of
                        "success" -> putMVar start () >> readMVar done >> pure (Right ())
                        "Left" -> pure (Left TensorSessionClosed)
                        "synchronous" -> throwIO MetadataPrimaryFault
                        _ -> do
                            caller <- myThreadId
                            senderDone <- newEmptyMVar
                            wait <- newEmptyMVar
                            sender <- forkFinally (throwTo caller ThreadKilled) (putMVar senderDone)
                            takeMVar wait `finally` (killThread sender >> takeMVar senderDone >> pure ())
                (restore body `finally` join) `finally` record "callback-exit"
        case (exit, outcome) of
            ("success", Right (Right ())) -> pure ()
            ("Left", Right (Left TensorSessionClosed)) -> pure ()
            ("synchronous", Left problem) -> equal "caller synchronous identity" (Just MetadataPrimaryFault) (fromException problem)
            ("asynchronous", Left problem) -> equal "caller delivered async identity" (Just ThreadKilled) (fromException problem)
            _ -> fail ("wrong caller exit channel: " ++ exit)
        observed <- readIORef events
        equal (exit ++ " join precedes callback exit and close") ["joined", "callback-exit", "close"] (filter (/= "dependent-work") observed)
        case dropWhile (/= "callback-exit") observed of
            ["callback-exit", "close"] -> pure ()
            _ -> fail "dependent work occurred after caller exit"
    negativeEvents <- newIORef ([] :: [String])
    let recordNegative event = modifyIORef' negativeEvents (++ [event])
        negativeAllocator = defaultTensorAllocator{allocatorFinalize = \pointer -> recordNegative "close" >> allocatorFinalize defaultTensorAllocator pointer}
    (worker, done) <-
        withTensorSessionAllocator
            negativeAllocator
            limits
            ( \session -> do
                _ <- hostTensorFromList session SF64 (SCons (Proxy @1) SNil) [7] >>= require
                gate <- newEmptyMVar
                completed <- newEmptyMVar
                -- This negative worker captures only a gate, not any tensor or action.
                child <- forkFinally (takeMVar gate) (putMVar completed)
                recordNegative "callback-exit"
                pure (Right (child, completed))
            )
            >>= require
    killThread worker
    _ <- takeMVar done
    recordNegative "joined"
    omittedJoin <- readIORef negativeEvents
    equal "actual omitted-join negative order" ["callback-exit", "close", "joined"] omittedJoin
    unless (omittedJoin /= ["joined", "callback-exit", "close"]) (fail "omitted-join ordering negative was not discriminated")

counters :: IO (TensorAllocator, IORef Int, IORef Int)
counters = do
    allocated <- newIORef 0
    finalized <- newIORef 0
    let allocator =
            TensorAllocator
                { allocatorAllocate = \count -> do
                    pointer <- mallocForeignPtrArray count
                    modifyIORef' allocated (+ 1)
                    pure (Right pointer)
                , allocatorFinalize = \pointer -> do
                    modifyIORef' finalized (+ 1)
                    finalizeForeignPtr pointer
                    pure (Right ())
                }
    pure (allocator, allocated, finalized)

snapshot :: TensorSession region -> IO (Integer, Integer, Integer, Integer, Int, Bool)
snapshot (TensorSession _ _ lock) = withMVar lock $ \state ->
    pure (toInteger (stateNextStorage state), toInteger (statePayloadBytes state), toInteger (stateBuffers state), toInteger (stateScalarWork state), length (stateLiveAllocations state), stateClosed state)

boundedSnapshot :: TensorSession region -> IO (Integer, Integer, Integer, Integer, Int, Bool)
boundedSnapshot session = timeout 5000000 (snapshot session) >>= maybe (fail "session MVar was stranded") pure

expectException :: String -> Either exception value -> IO ()
expectException _ (Left _) = pure ()
expectException label (Right _) = fail (label ++ ": exception was swallowed")

require :: (Show problem) => Either problem value -> IO value
require (Right value) = pure value
require (Left problem) = fail (show problem)

equal :: (Eq value, Show value) => String -> value -> value -> IO ()
equal label expected actual = unless (actual == expected) (fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual))
