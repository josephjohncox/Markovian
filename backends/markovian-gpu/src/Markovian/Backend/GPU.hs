{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Optional execution of the checked F64 matrix and matrix-VJP fragment.

CUDA admission creates a private context, module, non-default stream, and
known-answer self-test. An executor is serialized, explicitly closed, and
never exposes pointers or streams. Fallback can occur only before the first
user-kernel launch. Device results are checked finite host values; they are not
exact values and are not claimed to be bitwise reproducible across devices.
-}
module Markovian.Backend.GPU (
    gpuBackendCompiled,
    gpuFaultInjectionCompiled,
    gpuBackendAvailable,
    DeviceSelector (..),
    CUDADevice (..),
    CUDAProbe (..),
    probeCUDA,
    CUDAStage (..),
    CUDADiagnostic (..),
    CUDAFailure (..),
    CUDAError (..),
    CUDAAdmission (..),
    CUDAExecutor,
    withCUDAExecutor,
    cudaExecutorAdmission,
    DeviceLimits,
    deviceLimits,
    DeviceBudgetError (..),
    DevicePlanReport (..),
    renderDevicePlanReport,
    PreparedMatMul,
    PreparedMatMulVJP,
    prepareMatMul,
    prepareMatMulVJP,
    DeviceTensor,
    deviceTensorValues,
    DeviceVJP,
    deviceLeftGradient,
    deviceRightGradient,
    FallbackPolicy (..),
    BackendRequest (..),
    FallbackReason (..),
    BackendSelection (..),
    DeviceExecutionReport (..),
    DeviceError (..),
    runPreparedMatMulCPU,
    runPreparedMatMulCUDA,
    runPreparedMatMul,
    runPreparedMatMulVJPCPU,
    runPreparedMatMulVJPCUDA,
    runPreparedMatMulVJP,
) where

import Control.Monad (when)
#ifdef MARKOVIAN_CUDA
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (SomeException, bracket, displayException, mask, try)
import Control.Monad (forM)
import Data.Char (digitToInt, isHexDigit)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Word (Word8)
#endif
import Markovian.Backend.GPU.Artifact (
    kernelABI,
    profileSHA256,
    ptxSHA256,
    ptxTarget,
    requiredCapabilityMajor,
    requiredCapabilityMinor,
    requiredThreadsPerBlock,
 )
import Markovian.Tensor (
    FiniteTensor,
    TensorError,
    TensorOperationReport,
    TensorSession,
    hostTensor,
    tensorShape,
    tensorToList,
 )
import Markovian.Tensor qualified
import Markovian.Tensor.Primitive (matmul)
import Markovian.Tensor.Reverse (applyBinaryTape, matmulWithTape)

-- The public closed tape API is owned by markovian-tensor. GPU does not
-- import the tensor runtime's private allocator capability.
import Markovian.Tensor.Shape (shapeDimensions)
import Numeric.Natural (Natural)

#ifdef MARKOVIAN_CUDA
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CChar, CDouble (..), CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Marshal.Array (allocaArray, peekArray, pokeArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
#endif

-- Capability ------------------------------------------------------------------

-- | Whether this build contains the CUDA driver implementation.
gpuBackendCompiled :: Bool
#ifdef MARKOVIAN_CUDA
gpuBackendCompiled = True
#else
gpuBackendCompiled = False
#endif

-- | Whether this build contains the test-only deterministic driver fault hooks.
gpuFaultInjectionCompiled :: Bool
#if defined(MARKOVIAN_CUDA) && defined(MARKOVIAN_CUDA_FAULT_INJECTION)
gpuFaultInjectionCompiled = True
#else
gpuFaultInjectionCompiled = False
#endif

-- | Stable explicit device selection. UUIDs use lower-case hexadecimal bytes.
data DeviceSelector
    = DeterministicFirstDevice
    | DeviceOrdinal !Natural
    | DeviceByUUID !String
    deriving (Eq, Show)

-- | Probe information reported by the CUDA driver for one ordinal.
data CUDADevice = CUDADevice
    { cudaDeviceOrdinal :: !Natural
    , cudaDeviceUUID :: !String
    , cudaDeviceName :: !String
    , cudaComputeCapabilityMajor :: !Natural
    , cudaComputeCapabilityMinor :: !Natural
    , cudaTotalMemoryBytes :: !Natural
    , cudaMaximumThreadsPerBlock :: !Natural
    }
    deriving (Eq, Show)

-- | Driver-level enumeration. Module load and the self-test occur at admission.
data CUDAProbe = CUDAProbe
    { cudaProbeCompiledSupport :: !Bool
    , cudaProbeDriverVersion :: !(Maybe Natural)
    , cudaProbeDevices :: ![CUDADevice]
    , cudaProbeFailure :: !(Maybe CUDADiagnostic)
    }
    deriving (Eq, Show)

-- | Enumerate driver and device properties without claiming module admission.
probeCUDA :: IO CUDAProbe

-- | Whether a CUDA-enabled build currently enumerates a device.
gpuBackendAvailable :: IO Bool
gpuBackendAvailable = any supportsPinnedDevice . cudaProbeDevices <$> probeCUDA

supportsPinnedDevice :: CUDADevice -> Bool
supportsPinnedDevice device =
    cudaComputeCapabilityMajor device == fromInteger requiredCapabilityMajor
        && cudaComputeCapabilityMinor device == fromInteger requiredCapabilityMinor
        && cudaMaximumThreadsPerBlock device >= fromInteger requiredThreadsPerBlock

-- Failure stages --------------------------------------------------------------

-- | Stable transaction stage. Admission self-tests are not user launches.
data CUDAStage
    = CUDAInitialize
    | CUDADeviceCount
    | CUDADeviceSelect
    | CUDAContextCreate
    | CUDAContextPush
    | CUDAModuleLoad
    | CUDAFunctionLookup
    | CUDAStreamCreate
    | CUDAAdmissionSelfTest
    | CUDAAllocateLeft
    | CUDAAllocateRight
    | CUDAAllocateOutput
    | CUDACopyLeft
    | CUDACopyRight
    | CUDAUserLaunch
    | CUDAStreamSynchronize
    | CUDACopyOutput
    | CUDAFreeOutput
    | CUDAFreeRight
    | CUDAFreeLeft
    | CUDAStreamDestroy
    | CUDAModuleUnload
    | CUDAContextPop
    | CUDAContextDestroy
    | CUDAHostArgument
    | CUDADriverLoad
    | CUDASymbolResolve
    | CUDADriverUnload
    | CUDADeviceCompatibility
    | CUDAExecutorPoisoned
    | CUDAProbeStage
    | CUDAUnknownStage !Int
    deriving (Eq, Show)

-- | One driver code with its official name and description when available.
data CUDADiagnostic = CUDADiagnostic
    { cudaDiagnosticStage :: !CUDAStage
    , cudaDiagnosticCode :: !Int
    , cudaDiagnosticName :: !String
    , cudaDiagnosticDescription :: !String
    }
    deriving (Eq, Show)

-- | Primary and all retained cleanup failures for one transaction.
data CUDAFailure = CUDAFailure
    { cudaFailurePrimary :: !(Maybe CUDADiagnostic)
    , cudaFailureCleanup :: ![CUDADiagnostic]
    , cudaFailureCommittedUserLaunch :: !Bool
    , cudaFailureFallbackPermitted :: !Bool
    }
    deriving (Eq, Show)

-- | CUDA lifecycle and execution errors.
data CUDAError
    = CUDANotCompiled
    | CUDAProbeFailed !CUDADiagnostic
    | CUDADeviceNotFound !DeviceSelector
    | CUDADeviceUnsupported !CUDADevice !String
    | CUDAExecutorFailure !CUDAFailure
    | CUDAExecutorClosed
    | CUDAActionException !String
    | CUDAActionAndCleanupFailure !CUDAError !CUDAFailure
    deriving (Eq, Show)

-- | Successful admission for the exact selected device and committed kernel.
data CUDAAdmission = CUDAAdmission
    { cudaAdmissionDevice :: !CUDADevice
    , cudaAdmissionNativeVerifiedUUID :: !String
    , cudaAdmissionPTXTarget :: !String
    , cudaAdmissionKernelABI :: !String
    , cudaAdmissionPTXSHA256 :: !String
    , cudaAdmissionSelfTestPassed :: !Bool
    }
    deriving (Eq, Show)

-- Prepared fragment -----------------------------------------------------------

-- | Preparation limits for transfer bytes, scalar work, and user launches.
data DeviceLimits = DeviceLimits
    { limitTransferBytes :: !Natural
    , limitDeviceScalarWork :: !Natural
    , limitUserLaunches :: !Natural
    }
    deriving (Eq, Show)

-- | Construct limits in transfer-byte, scalar-work, and launch order.
deviceLimits :: Natural -> Natural -> Natural -> DeviceLimits
deviceLimits = DeviceLimits

-- | Pure preparation failures. No executor is opened and no transfer occurs.
data DeviceBudgetError
    = DeviceDimensionUnsupported !Natural
    | DeviceElementCountUnsupported !Natural
    | DeviceTransferLimitExceeded !Natural !Natural
    | DeviceWorkLimitExceeded !Natural !Natural
    | DeviceLaunchLimitExceeded !Natural !Natural
    deriving (Eq, Show)

-- | Deterministic timing-free plan for the supported closed operation.
data DevicePlanReport = DevicePlanReport
    { devicePlanOperation :: !String
    , devicePlanShapes :: ![[Natural]]
    , devicePlanTransferBytes :: !Natural
    , devicePlanHostMaterializationBytes :: !Natural
    , devicePlanScalarWork :: !Natural
    , devicePlanUserLaunches :: !Natural
    , devicePlanPTXTarget :: !String
    , devicePlanKernelABI :: !String
    , devicePlanPTXSHA256 :: !String
    }
    deriving (Eq, Show)

-- | Render a stable plan without addresses, ordinals chosen at runtime, or time.
renderDevicePlanReport :: DevicePlanReport -> String
renderDevicePlanReport report =
    unlines
        [ "device-plan-report"
        , "operation: " ++ devicePlanOperation report
        , "shapes: " ++ show (devicePlanShapes report)
        , "transfer-bytes: " ++ show (devicePlanTransferBytes report)
        , "host-materialization-bytes: " ++ show (devicePlanHostMaterializationBytes report)
        , "scalar-work: " ++ show (devicePlanScalarWork report)
        , "user-launches: " ++ show (devicePlanUserLaunches report)
        , "profile-sha256: " ++ profileSHA256
        , "ptx-target: " ++ devicePlanPTXTarget report
        , "kernel-abi: " ++ devicePlanKernelABI report
        , "ptx-sha256: " ++ devicePlanPTXSHA256 report
        ]

-- | Opaque checked matrix-product plan.
data PreparedMatMul region rows inner columns
    = PreparedMatMul
        !(FiniteTensor region 'Markovian.Tensor.F64 '[rows, inner])
        !(FiniteTensor region 'Markovian.Tensor.F64 '[inner, columns])
        !Natural
        !Natural
        !Natural
        !DevicePlanReport

type role PreparedMatMul nominal nominal nominal nominal

-- | Opaque checked VJP plan for the tensor runtime's matrix-product tape.
data PreparedMatMulVJP region rows inner columns
    = PreparedMatMulVJP
        !(FiniteTensor region 'Markovian.Tensor.F64 '[rows, inner])
        !(FiniteTensor region 'Markovian.Tensor.F64 '[inner, columns])
        !(FiniteTensor region 'Markovian.Tensor.F64 '[rows, columns])
        !Natural
        !Natural
        !Natural
        !DevicePlanReport

type role PreparedMatMulVJP nominal nominal nominal nominal

-- | Prepare one matrix product before any backend selection or transfer.
prepareMatMul ::
    DeviceLimits ->
    FiniteTensor region 'Markovian.Tensor.F64 '[rows, inner] ->
    FiniteTensor region 'Markovian.Tensor.F64 '[inner, columns] ->
    Either DeviceBudgetError (PreparedMatMul region rows inner columns)
prepareMatMul limits left right = do
    (rows, inner) <- rankTwo left
    (_, columns) <- rankTwo right
    let transfer = 8 * (rows * inner + inner * columns + rows * columns)
        work = 2 * rows * inner * columns
        report = plan "matmul" [shapeDimensions (tensorShape (hostTensor left)), shapeDimensions (tensorShape (hostTensor right)), [rows, columns]] transfer transfer work 1
    checkPlan limits [rows, inner, columns] [rows * inner, inner * columns, rows * columns] transfer work 1
    Right (PreparedMatMul left right rows inner columns report)

-- | Prepare the two matrix products implementing the declared matrix VJP.
prepareMatMulVJP ::
    DeviceLimits ->
    FiniteTensor region 'Markovian.Tensor.F64 '[rows, inner] ->
    FiniteTensor region 'Markovian.Tensor.F64 '[inner, columns] ->
    FiniteTensor region 'Markovian.Tensor.F64 '[rows, columns] ->
    Either DeviceBudgetError (PreparedMatMulVJP region rows inner columns)
prepareMatMulVJP limits left right seed = do
    (rows, inner) <- rankTwo left
    (_, columns) <- rankTwo right
    let leftCall = rows * columns + columns * inner + rows * inner
        rightCall = inner * rows + rows * columns + inner * columns
        transfer = 8 * (leftCall + rightCall)
        materialization = 8 * (inner * columns + rows * inner)
        work = 4 * rows * inner * columns
        report = plan "vjp/matmul" [shapeDimensions (tensorShape (hostTensor left)), shapeDimensions (tensorShape (hostTensor right)), shapeDimensions (tensorShape (hostTensor seed))] transfer materialization work 2
    checkPlan limits [rows, inner, columns] [rows * columns, rows * inner, inner * columns] transfer work 2
    Right (PreparedMatMulVJP left right seed rows inner columns report)

rankTwo :: FiniteTensor region dtype '[rows, columns] -> Either DeviceBudgetError (Natural, Natural)
rankTwo tensor = case shapeDimensions (tensorShape (hostTensor tensor)) of
    [rows, columns] -> Right (rows, columns)
    _ -> Left (DeviceDimensionUnsupported 0)

plan :: String -> [[Natural]] -> Natural -> Natural -> Natural -> Natural -> DevicePlanReport
plan operation shapes transfer materialization work launches =
    DevicePlanReport operation shapes transfer materialization work launches ptxTarget kernelABI ptxSHA256

checkPlan :: DeviceLimits -> [Natural] -> [Natural] -> Natural -> Natural -> Natural -> Either DeviceBudgetError ()
checkPlan limits dimensions elementCounts transfer work launches = do
    let maximumCInt = 2147483647
        maximumHostElements = fromIntegral ((maxBound :: Int) `div` 8)
        maximumElements = min maximumCInt maximumHostElements
    mapM_ (\dimension -> when (dimension == 0 || dimension > maximumCInt) (Left (DeviceDimensionUnsupported dimension))) dimensions
    mapM_ (\elements -> when (elements == 0 || elements > maximumElements) (Left (DeviceElementCountUnsupported elements))) elementCounts
    when (transfer > limitTransferBytes limits) (Left (DeviceTransferLimitExceeded (limitTransferBytes limits) transfer))
    when (work > limitDeviceScalarWork limits) (Left (DeviceWorkLimitExceeded (limitDeviceScalarWork limits) work))
    when (launches > limitUserLaunches limits) (Left (DeviceLaunchLimitExceeded (limitUserLaunches limits) launches))

-- Host results ----------------------------------------------------------------

-- | Finite host values with a type-level output shape. This is not a tensor allocation.
newtype DeviceTensor shape = DeviceTensor [Double]
    deriving (Eq, Show)

type role DeviceTensor nominal

-- | Read row-major finite values returned after synchronization and copy-back.
deviceTensorValues :: DeviceTensor shape -> [Double]
deviceTensorValues (DeviceTensor values) = values

-- | Both outputs of the matrix-product VJP. Failure never returns one gradient.
data DeviceVJP left right = DeviceVJP !(DeviceTensor left) !(DeviceTensor right)
    deriving (Eq, Show)

type role DeviceVJP nominal nominal

-- | Read the left-operand gradient.
deviceLeftGradient :: DeviceVJP left right -> DeviceTensor left
deviceLeftGradient (DeviceVJP left _) = left

-- | Read the right-operand gradient.
deviceRightGradient :: DeviceVJP left right -> DeviceTensor right
deviceRightGradient (DeviceVJP _ right) = right

-- Dispatch --------------------------------------------------------------------

-- | Whether a preferred CUDA request may execute the CPU reference instead.
data FallbackPolicy = NoFallback | FallbackBeforeUserLaunch
    deriving (Eq, Show)

-- | Explicit execution request. CPU fallback remains approximate F64 execution.
data BackendRequest
    = CPUOnly
    | PreferCUDA !DeviceSelector !FallbackPolicy
    | RequireCUDA !DeviceSelector
    deriving (Eq, Show)

-- | Recorded reason for an allowed pre-launch CPU fallback.
newtype FallbackReason = CUDARejectedBeforeUserLaunch CUDAError
    deriving (Eq, Show)

-- | Backend actually used for the represented operation.
data BackendSelection
    = CPURequested
    | CUDASelected !CUDAAdmission
    | CPUFallback !FallbackReason
    deriving (Eq, Show)

-- | Transfer-inclusive timings are runtime evidence, separate from the plan.
data DeviceExecutionReport = DeviceExecutionReport
    { deviceExecutionBackend :: !BackendSelection
    , deviceExecutionPlan :: !DevicePlanReport
    , deviceExecutionTransferInclusiveMilliseconds :: ![Double]
    , deviceExecutionCPUReports :: ![TensorOperationReport]
    }
    deriving (Eq, Show)

-- | Preparation, host-runtime, or device failure.
data DeviceError
    = DevicePreparationError !DeviceBudgetError
    | DeviceTensorError !TensorError
    | DeviceCUDAError !CUDAError
    | DeviceNonFiniteOutput !Natural
    deriving (Eq, Show)

-- Executor --------------------------------------------------------------------

-- | Opaque serialized owner of one admitted context, module, and stream.
#ifdef MARKOVIAN_CUDA
data CUDAExecutor scope = CUDAExecutor !(MVar ()) !(IORef Bool) !(Ptr ()) !CUDAAdmission
#else
data CUDAExecutor scope = CUDAExecutor
#endif

type role CUDAExecutor nominal

-- | Read admission evidence retained by an open executor.
cudaExecutorAdmission :: CUDAExecutor scope -> CUDAAdmission
#ifdef MARKOVIAN_CUDA
cudaExecutorAdmission (CUDAExecutor _ _ _ admission) = admission
#else
cudaExecutorAdmission CUDAExecutor = error "CUDA executor cannot exist in a CUDA-disabled build"
#endif

{- | Open and close one private serialized CUDA executor.

The rank-2 scope prevents the executor from being returned. Destruction takes
its execution lock, marks the owner closed, waits for any already-running FFI
call, and only then destroys the native resources. An action exception is
returned as a bounded diagnostic so teardown failures are not discarded.
-}
withCUDAExecutor :: forall value. DeviceSelector -> (forall scope. CUDAExecutor scope -> IO (Either CUDAError value)) -> IO (Either CUDAError value)
#ifdef MARKOVIAN_CUDA
withCUDAExecutor selector action = mask $ \restore -> do
    opened <- openCUDAExecutor selector
    case opened of
        Left problem -> pure (Left problem)
        Right executor@(CUDAExecutor lock closed pointer _) -> do
            outcome <- try (restore (action executor)) :: IO (Either SomeException (Either CUDAError value))
            cleanup <- withMVar lock $ \() -> do
                writeIORef closed True
                destroyExecutor pointer
            let actionResult = case outcome of
                    Left exception -> Left (CUDAActionException (take 4096 (displayException exception)))
                    Right result -> result
            pure (combineCleanup actionResult cleanup)
#else
withCUDAExecutor _ _ = pure (Left CUDANotCompiled)
#endif

#ifdef MARKOVIAN_CUDA
openCUDAExecutor :: DeviceSelector -> IO (Either CUDAError (CUDAExecutor scope))
openCUDAExecutor selector = do
    probe <- probeCUDA
    case cudaProbeFailure probe of
        Just problem -> pure (Left (CUDAProbeFailed problem))
        Nothing -> case selectDevice selector (cudaProbeDevices probe) of
            Nothing -> pure (Left (CUDADeviceNotFound selector))
            Just device
                | not (supportsPinnedDevice device) ->
                    pure (Left (CUDADeviceUnsupported device ("only profile " ++ profileSHA256 ++ " is admitted")))
                | otherwise -> case decodeUUID (cudaDeviceUUID device) of
                    Nothing -> pure (Left (CUDAExecutorFailure (hostFailure CUDADeviceCompatibility "probed device UUID is not 16 canonical bytes")))
                    Just expectedUUID ->
                        allocaArray 16 $ \expectedPointer ->
                            allocaArray 16 $ \verifiedPointer ->
                                alloca $ \executorPointer -> do
                                    pokeArray expectedPointer expectedUUID
                                    pokeArray verifiedPointer (replicate 16 0)
                                    poke executorPointer nullPtr
                                    status <- withStatus (c_cuda_executor_create (fromIntegral (cudaDeviceOrdinal device)) expectedPointer verifiedPointer executorPointer)
                                    pointer <- peek executorPointer
                                    failure <- statusFailure status
                                    case failure of
                                        Just problem -> pure (Left (CUDAExecutorFailure problem))
                                        Nothing
                                            | pointer == nullPtr -> pure (Left (CUDAExecutorFailure (hostFailure CUDAContextCreate "executor creation returned null")))
                                            | otherwise -> do
                                                verifiedUUID <- renderUUID <$> peekArray 16 verifiedPointer
                                                lock <- newMVar ()
                                                closed <- newIORef False
                                                let admission = CUDAAdmission device verifiedUUID ptxTarget kernelABI ptxSHA256 True
                                                pure (Right (CUDAExecutor lock closed pointer admission))

selectDevice :: DeviceSelector -> [CUDADevice] -> Maybe CUDADevice
selectDevice DeterministicFirstDevice devices = case devices of
    [] -> Nothing
    first : _ -> Just first
selectDevice (DeviceOrdinal ordinal) devices = find ((== ordinal) . cudaDeviceOrdinal) devices
selectDevice (DeviceByUUID uuid) devices = find ((== uuid) . cudaDeviceUUID) devices

combineCleanup :: Either CUDAError value -> Maybe CUDAFailure -> Either CUDAError value
combineCleanup actionResult Nothing = actionResult
combineCleanup (Right _) (Just cleanup) = Left (CUDAExecutorFailure (cleanup{cudaFailureFallbackPermitted = False}))
combineCleanup (Left actionProblem) (Just cleanup) = Left (CUDAActionAndCleanupFailure actionProblem (cleanup{cudaFailureFallbackPermitted = False}))

destroyExecutor :: Ptr () -> IO (Maybe CUDAFailure)
destroyExecutor pointer = withStatus (c_cuda_executor_destroy pointer) >>= statusFailure
#endif

-- CPU execution ---------------------------------------------------------------

-- | Execute the prepared product with the tensor package's CPU primitive.
runPreparedMatMulCPU ::
    TensorSession region ->
    PreparedMatMul region rows inner columns ->
    IO (Either DeviceError (DeviceTensor '[rows, columns], DeviceExecutionReport))
runPreparedMatMulCPU session (PreparedMatMul left right _ _ _ report) = do
    result <- matmul session left right
    case result of
        Left problem -> pure (Left (DeviceTensorError problem))
        Right (output, cpuReport) -> do
            values <- tensorToList (hostTensor output)
            pure (Right (DeviceTensor values, DeviceExecutionReport CPURequested report [] [cpuReport]))

-- | Execute the prepared tensor matrix-product VJP on the CPU reference path.
runPreparedMatMulVJPCPU ::
    TensorSession region ->
    PreparedMatMulVJP region rows inner columns ->
    IO (Either DeviceError (DeviceVJP '[rows, inner] '[inner, columns], DeviceExecutionReport))
runPreparedMatMulVJPCPU session (PreparedMatMulVJP left right seed _ _ _ report) = do
    forward <- matmulWithTape session left right
    case forward of
        Left problem -> pure (Left (DeviceTensorError problem))
        Right (_, tape, forwardReport) -> do
            reverseResult <- applyBinaryTape session tape seed
            case reverseResult of
                Left problem -> pure (Left (DeviceTensorError problem))
                Right ((leftGradient, rightGradient), reverseReport) -> do
                    leftValues <- tensorToList (hostTensor leftGradient)
                    rightValues <- tensorToList (hostTensor rightGradient)
                    pure (Right (DeviceVJP (DeviceTensor leftValues) (DeviceTensor rightValues), DeviceExecutionReport CPURequested report [] [forwardReport, reverseReport]))

-- CUDA execution --------------------------------------------------------------

-- | Execute one admitted user matrix-product launch and copy back finite values.
runPreparedMatMulCUDA ::
    CUDAExecutor scope ->
    PreparedMatMul region rows inner columns ->
    IO (Either CUDAError (DeviceTensor '[rows, columns], DeviceExecutionReport))
#ifdef MARKOVIAN_CUDA
runPreparedMatMulCUDA executor (PreparedMatMul left right rows inner columns report) = do
    leftValues <- tensorToList (hostTensor left)
    rightValues <- tensorToList (hostTensor right)
    result <- runExecutorMatMul executor rows inner columns leftValues rightValues
    pure $ do
        (values, milliseconds) <- result
        Right (DeviceTensor values, DeviceExecutionReport (CUDASelected (cudaExecutorAdmission executor)) report [milliseconds] [])
#else
runPreparedMatMulCUDA _ _ = pure (Left CUDANotCompiled)
#endif

-- | Execute both admitted matrix products for the declared VJP.
runPreparedMatMulVJPCUDA ::
    CUDAExecutor scope ->
    PreparedMatMulVJP region rows inner columns ->
    IO (Either CUDAError (DeviceVJP '[rows, inner] '[inner, columns], DeviceExecutionReport))
#ifdef MARKOVIAN_CUDA
runPreparedMatMulVJPCUDA executor (PreparedMatMulVJP left right seed rows inner columns report) = do
    leftValues <- tensorToList (hostTensor left)
    rightValues <- tensorToList (hostTensor right)
    seedValues <- tensorToList (hostTensor seed)
    let rightTransposed = transposeValues inner columns rightValues
        leftTransposed = transposeValues rows inner leftValues
    leftResult <- runExecutorMatMul executor rows columns inner seedValues rightTransposed
    case leftResult of
        Left problem -> pure (Left problem)
        Right (leftGradient, leftMilliseconds) -> do
            rightResult <- runExecutorMatMul executor inner rows columns leftTransposed seedValues
            pure $ do
                (rightGradient, rightMilliseconds) <- mapLeft markCUDAErrorCommitted rightResult
                Right (DeviceVJP (DeviceTensor leftGradient) (DeviceTensor rightGradient), DeviceExecutionReport (CUDASelected (cudaExecutorAdmission executor)) report [leftMilliseconds, rightMilliseconds] [])
#else
runPreparedMatMulVJPCUDA _ _ = pure (Left CUDANotCompiled)
#endif

#ifdef MARKOVIAN_CUDA
transposeValues :: Natural -> Natural -> [Double] -> [Double]
transposeValues rows columns values =
    [ values !! fromIntegral (row * columns + column)
    | column <- naturalIndices columns
    , row <- naturalIndices rows
    ]

naturalIndices :: Natural -> [Natural]
naturalIndices count = take (fromIntegral count) [0 ..]
#endif

-- | Dispatch a product with explicit pre-launch-only fallback.
runPreparedMatMul ::
    TensorSession region ->
    BackendRequest ->
    PreparedMatMul region rows inner columns ->
    IO (Either DeviceError (DeviceTensor '[rows, columns], DeviceExecutionReport))
runPreparedMatMul session request prepared = case request of
    CPUOnly -> runPreparedMatMulCPU session prepared
    RequireCUDA selector -> fmap (mapLeft DeviceCUDAError) (withCUDAExecutor selector (`runPreparedMatMulCUDA` prepared))
    PreferCUDA selector policy -> do
        cudaResult <- withCUDAExecutor selector (`runPreparedMatMulCUDA` prepared)
        dispatchFallback policy session prepared cudaResult

-- | Dispatch a matrix VJP with the same pre-launch-only fallback rule.
runPreparedMatMulVJP ::
    TensorSession region ->
    BackendRequest ->
    PreparedMatMulVJP region rows inner columns ->
    IO (Either DeviceError (DeviceVJP '[rows, inner] '[inner, columns], DeviceExecutionReport))
runPreparedMatMulVJP session request prepared = case request of
    CPUOnly -> runPreparedMatMulVJPCPU session prepared
    RequireCUDA selector -> fmap (mapLeft DeviceCUDAError) (withCUDAExecutor selector (`runPreparedMatMulVJPCUDA` prepared))
    PreferCUDA selector policy -> do
        cudaResult <- withCUDAExecutor selector (`runPreparedMatMulVJPCUDA` prepared)
        dispatchVJPFallback policy session prepared cudaResult

dispatchFallback ::
    FallbackPolicy ->
    TensorSession region ->
    PreparedMatMul region rows inner columns ->
    Either CUDAError (DeviceTensor '[rows, columns], DeviceExecutionReport) ->
    IO (Either DeviceError (DeviceTensor '[rows, columns], DeviceExecutionReport))
dispatchFallback _ _ _ (Right value) = pure (Right value)
dispatchFallback policy session prepared (Left problem)
    | policy == FallbackBeforeUserLaunch && cudaErrorAllowsFallback problem = do
        cpu <- runPreparedMatMulCPU session prepared
        pure (fmap (markFallback problem) cpu)
    | otherwise = pure (Left (DeviceCUDAError problem))

dispatchVJPFallback ::
    FallbackPolicy ->
    TensorSession region ->
    PreparedMatMulVJP region rows inner columns ->
    Either CUDAError (DeviceVJP '[rows, inner] '[inner, columns], DeviceExecutionReport) ->
    IO (Either DeviceError (DeviceVJP '[rows, inner] '[inner, columns], DeviceExecutionReport))
dispatchVJPFallback _ _ _ (Right value) = pure (Right value)
dispatchVJPFallback policy session prepared (Left problem)
    | policy == FallbackBeforeUserLaunch && cudaErrorAllowsFallback problem = do
        cpu <- runPreparedMatMulVJPCPU session prepared
        pure (fmap (markFallback problem) cpu)
    | otherwise = pure (Left (DeviceCUDAError problem))

markFallback :: CUDAError -> (value, DeviceExecutionReport) -> (value, DeviceExecutionReport)
markFallback problem (value, report) =
    (value, report{deviceExecutionBackend = CPUFallback (CUDARejectedBeforeUserLaunch problem)})

cudaErrorAllowsFallback :: CUDAError -> Bool
cudaErrorAllowsFallback CUDANotCompiled = True
cudaErrorAllowsFallback (CUDAProbeFailed _) = True
cudaErrorAllowsFallback (CUDADeviceNotFound _) = True
cudaErrorAllowsFallback (CUDADeviceUnsupported _ _) = True
cudaErrorAllowsFallback (CUDAExecutorFailure failure) = cudaFailureFallbackPermitted failure
cudaErrorAllowsFallback CUDAExecutorClosed = False
cudaErrorAllowsFallback (CUDAActionException _) = False
cudaErrorAllowsFallback (CUDAActionAndCleanupFailure _ _) = False

#ifdef MARKOVIAN_CUDA
markCUDAErrorCommitted :: CUDAError -> CUDAError
markCUDAErrorCommitted (CUDAExecutorFailure failure) =
    CUDAExecutorFailure failure{cudaFailureCommittedUserLaunch = True, cudaFailureFallbackPermitted = False}
markCUDAErrorCommitted (CUDAActionAndCleanupFailure actionProblem cleanup) =
    CUDAActionAndCleanupFailure (markCUDAErrorCommitted actionProblem) cleanup{cudaFailureFallbackPermitted = False}
markCUDAErrorCommitted problem = problem
#endif

mapLeft :: (left -> other) -> Either left right -> Either other right
mapLeft function (Left value) = Left (function value)
mapLeft _ (Right value) = Right value

#ifdef MARKOVIAN_CUDA
-- Driver bridge ---------------------------------------------------------------

probeCUDA = alloca $ \driverPointer -> alloca $ \countPointer -> alloca $ \stagePointer -> do
    code <- c_cuda_device_count driverPointer countPointer stagePointer
    if code /= 0
        then do
            stage <- decodeStage . fromIntegral <$> (peek stagePointer :: IO CInt)
            diagnostic <- makeDiagnostic stage (fromIntegral code)
            pure (CUDAProbe True Nothing [] (Just diagnostic))
        else do
            driver <- fromIntegral <$> (peek driverPointer :: IO CInt)
            count <- fromIntegral <$> (peek countPointer :: IO CInt)
            devices <- forM [0 .. count - 1] probeDevice
            case sequence devices of
                Left problem -> pure (CUDAProbe True (Just driver) [] (Just problem))
                Right values -> pure (CUDAProbe True (Just driver) values Nothing)

probeDevice :: Int -> IO (Either CUDADiagnostic CUDADevice)
probeDevice ordinal =
    alloca $ \majorPointer ->
        alloca $ \minorPointer ->
            alloca $ \memoryPointer ->
                alloca $ \threadsPointer ->
                    allocaArray 16 $ \uuidPointer ->
                        allocaArray 128 $ \namePointer ->
                            alloca $ \stagePointer -> do
                                code <- c_cuda_probe_device (fromIntegral ordinal) majorPointer minorPointer memoryPointer threadsPointer uuidPointer namePointer stagePointer
                                if code /= 0
                                    then do
                                        stage <- decodeStage . fromIntegral <$> (peek stagePointer :: IO CInt)
                                        Left <$> makeDiagnostic stage (fromIntegral code)
                                else do
                                    major <- fromIntegral <$> (peek majorPointer :: IO CInt)
                                    minor <- fromIntegral <$> (peek minorPointer :: IO CInt)
                                    memory <- fromIntegral <$> (peek memoryPointer :: IO CSize)
                                    threads <- fromIntegral <$> (peek threadsPointer :: IO CInt)
                                    uuid <- renderUUID <$> peekArray 16 uuidPointer
                                    name <- peekCString namePointer
                                    pure (Right (CUDADevice (fromIntegral ordinal) uuid name major minor memory threads))

renderUUID :: [Word8] -> String
renderUUID bytes = concatMap hexadecimal bytes
  where
    digits = "0123456789abcdef"
    hexadecimal byte = [digits !! fromIntegral (byte `div` 16), digits !! fromIntegral (byte `mod` 16)]

decodeUUID :: String -> Maybe [Word8]
decodeUUID = go
  where
    go [] = Just []
    go (high : low : rest)
        | isHexDigit high && isHexDigit low =
            (fromIntegral (digitToInt high * 16 + digitToInt low) :) <$> go rest
    go _ = Nothing

runExecutorMatMul :: CUDAExecutor scope -> Natural -> Natural -> Natural -> [Double] -> [Double] -> IO (Either CUDAError ([Double], Double))
runExecutorMatMul (CUDAExecutor lock closed pointer _) rows inner columns left right = withMVar lock $ \() -> do
    isClosed <- readIORef closed
    if isClosed
        then pure (Left CUDAExecutorClosed)
        else
            withHeapArray (map CDouble left) $ \leftPointer ->
                withHeapArray (map CDouble right) $ \rightPointer ->
                    withHeapOutput (fromIntegral (rows * columns)) $ \outputPointer ->
                        alloca $ \millisecondsPointer -> do
                            status <-
                                withStatus
                                    ( c_cuda_executor_matmul
                                        pointer
                                        (fromIntegral rows)
                                        (fromIntegral inner)
                                        (fromIntegral columns)
                                        leftPointer
                                        rightPointer
                                        outputPointer
                                        millisecondsPointer
                                    )
                            failure <- statusFailure status
                            case failure of
                                Just problem -> pure (Left (CUDAExecutorFailure problem))
                                Nothing -> do
                                    output <- map realToFrac <$> peekArray (fromIntegral (rows * columns)) outputPointer
                                    milliseconds <- realToFrac <$> (peek millisecondsPointer :: IO CDouble)
                                    case firstNonFinite output of
                                        Just index -> pure (Left (CUDAExecutorFailure (committedHostFailure CUDAStreamSynchronize ("nonfinite output at " ++ show index))))
                                        Nothing -> pure (Right (output, milliseconds))

withHeapArray :: [CDouble] -> (Ptr CDouble -> IO value) -> IO value
withHeapArray values action =
    bracket (mallocBytes (length values * 8)) free $ \pointer -> do
        pokeArray pointer values
        action pointer

withHeapOutput :: Int -> (Ptr CDouble -> IO value) -> IO value
withHeapOutput count = bracket (mallocBytes (count * 8)) free

firstNonFinite :: [Double] -> Maybe Natural
firstNonFinite = go 0
  where
    go _ [] = Nothing
    go index (value : rest)
        | isNaN value || isInfinite value = Just index
        | otherwise = go (index + 1) rest

data RawStatus = RawStatus !Int !Int !Bool ![(Int, Int)]

withStatus :: (Ptr CInt -> IO ()) -> IO RawStatus
withStatus action = allocaArray 20 $ \pointer -> do
    pokeArray pointer (replicate 20 0)
    action pointer
    values <- map fromIntegral <$> peekArray 20 pointer
    case values of
        primaryCode : primaryStage : committed : cleanupCount : rest ->
            pure (RawStatus primaryCode primaryStage (committed /= 0) (take cleanupCount (pairs rest)))
        _ -> pure (RawStatus 999999 0 False [])
  where
    pairs (stage : code : rest) = (stage, code) : pairs rest
    pairs _ = []

statusFailure :: RawStatus -> IO (Maybe CUDAFailure)
statusFailure (RawStatus primaryCode primaryStage committed cleanup) =
    if primaryCode == 0 && null cleanup
        then pure Nothing
        else do
            primary <-
                if primaryCode == 0
                    then pure Nothing
                    else Just <$> makeDiagnostic (decodeStage primaryStage) primaryCode
            cleanupDiagnostics <- mapM (\(stage, code) -> makeDiagnostic (decodeStage stage) code) cleanup
            let fallbackPermitted = not committed && null cleanupDiagnostics
            pure (Just (CUDAFailure primary cleanupDiagnostics committed fallbackPermitted))

makeDiagnostic :: CUDAStage -> Int -> IO CUDADiagnostic
makeDiagnostic stage code = do
    name <- c_cuda_error_name (fromIntegral code) >>= peekCString
    description <- c_cuda_error_string (fromIntegral code) >>= peekCString
    pure (CUDADiagnostic stage code name description)

hostFailure :: CUDAStage -> String -> CUDAFailure
hostFailure stage description = CUDAFailure (Just (CUDADiagnostic stage (-1) "MARKOVIAN_HOST_FAILURE" description)) [] False False

committedHostFailure :: CUDAStage -> String -> CUDAFailure
committedHostFailure stage description = CUDAFailure (Just (CUDADiagnostic stage (-1) "MARKOVIAN_HOST_FAILURE" description)) [] True False

decodeStage :: Int -> CUDAStage
decodeStage stage = case stage of
    1 -> CUDAInitialize
    2 -> CUDADeviceCount
    3 -> CUDADeviceSelect
    4 -> CUDAContextCreate
    5 -> CUDAContextPush
    6 -> CUDAModuleLoad
    7 -> CUDAFunctionLookup
    8 -> CUDAStreamCreate
    9 -> CUDAAdmissionSelfTest
    10 -> CUDAAllocateLeft
    11 -> CUDAAllocateRight
    12 -> CUDAAllocateOutput
    13 -> CUDACopyLeft
    14 -> CUDACopyRight
    15 -> CUDAUserLaunch
    16 -> CUDAStreamSynchronize
    17 -> CUDACopyOutput
    18 -> CUDAFreeOutput
    19 -> CUDAFreeRight
    20 -> CUDAFreeLeft
    21 -> CUDAStreamDestroy
    22 -> CUDAModuleUnload
    23 -> CUDAContextPop
    24 -> CUDAContextDestroy
    25 -> CUDAHostArgument
    26 -> CUDADriverLoad
    27 -> CUDASymbolResolve
    28 -> CUDADriverUnload
    29 -> CUDADeviceCompatibility
    30 -> CUDAExecutorPoisoned
    other -> CUDAUnknownStage other

foreign import ccall safe "markovian_cuda_device_count"
    c_cuda_device_count :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import ccall safe "markovian_cuda_probe_device"
    c_cuda_probe_device :: CInt -> Ptr CInt -> Ptr CInt -> Ptr CSize -> Ptr CInt -> Ptr Word8 -> Ptr CChar -> Ptr CInt -> IO CInt

foreign import ccall safe "markovian_cuda_executor_create"
    c_cuda_executor_create :: CInt -> Ptr Word8 -> Ptr Word8 -> Ptr (Ptr ()) -> Ptr CInt -> IO ()

foreign import ccall safe "markovian_cuda_executor_destroy"
    c_cuda_executor_destroy :: Ptr () -> Ptr CInt -> IO ()

foreign import ccall safe "markovian_cuda_executor_matmul"
    c_cuda_executor_matmul :: Ptr () -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO ()

foreign import ccall unsafe "markovian_cuda_error_name"
    c_cuda_error_name :: CInt -> IO CString

foreign import ccall unsafe "markovian_cuda_error_string"
    c_cuda_error_string :: CInt -> IO CString
#else
probeCUDA = pure (CUDAProbe False Nothing [] Nothing)
#endif
