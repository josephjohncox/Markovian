\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, bracket_)
import Control.Monad (forM_, unless, void, when)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Markovian.Backend.GPU
import Markovian.Tensor
import Paths_markovian_gpu (getDataFileName)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (exitFailure)
import System.IO.Error (tryIOError)

limits :: SessionLimits
limits = tensorSessionLimits 4 1024 1000000 8000000 64000000 256 100000000

deviceBudget :: DeviceLimits
deviceBudget = deviceLimits 1000000 1000000 2

matrixShape :: SShape '[2, 3]
matrixShape = SCons (Proxy @2) (SCons (Proxy @3) SNil)

rightShape :: SShape '[3, 2]
rightShape = SCons (Proxy @3) (SCons (Proxy @2) SNil)

outputShape :: SShape '[2, 2]
outputShape = SCons (Proxy @2) (SCons (Proxy @2) SNil)

main :: IO ()
main = do
    selector <- selectedDevice
    probe <- probeCUDA
    assertEqual "compiled probe classification" gpuBackendCompiled (cudaProbeCompiledSupport probe)
    sessionResult <- withTensorSession limits $ \session -> do
        left <- fst <$> expectRightIO "left tensor" (finiteTensorFromList session matrixShape [1, 2, 3, 4, 5, 6])
        right <- fst <$> expectRightIO "right tensor" (finiteTensorFromList session rightShape [7, 8, 9, 10, 11, 12])
        seed <- fst <$> expectRightIO "seed tensor" (finiteTensorFromList session outputShape [0.5, -1, 2, 0.25])
        prepared <- expectRight "prepare matmul" (prepareMatMul deviceBudget left right)
        preparedVJP <- expectRight "prepare matmul VJP" (prepareMatMulVJP deviceBudget left right seed)
        let expected = [58, 64, 139, 154]
            expectedLeft = [-4.5, -5.5, -6.5, 16, 20.5, 25]
            expectedRight = [8.5, 0, 11, -0.75, 13.5, -1.5]

        cpu <- expectRightIO "CPU matrix product" (runPreparedMatMul session CPUOnly prepared)
        assertApproxList "CPU matrix differential" expected (deviceTensorValues (fst cpu))
        cpuVJP <- expectRightIO "CPU matrix VJP" (runPreparedMatMulVJP session CPUOnly preparedVJP)
        assertVJP expectedLeft expectedRight (fst cpuVJP)
        goldenPath <- getDataFileName "test/golden/device-plan.txt"
        golden <- readFile goldenPath
        assertEqual "deterministic plan golden" golden (renderDevicePlanReport (deviceExecutionPlan (snd cpu)))
        checkFiniteDifferences left right seed expectedLeft expectedRight

        case prepareMatMul (deviceLimits 95 1000000 1) left right of
            Left (DeviceTransferLimitExceeded 95 128) -> pure ()
            other -> failTest ("transfer one-below boundary changed: " ++ showPrepared other)
        case prepareMatMul (deviceLimits 128 23 1) left right of
            Left (DeviceWorkLimitExceeded 23 24) -> pure ()
            other -> failTest ("work one-below boundary changed: " ++ showPrepared other)
        _ <- expectRight "matmul exact transfer/work/launch limits" (prepareMatMul (deviceLimits 128 24 1) left right)
        case prepareMatMul (deviceLimits 128 24 0) left right of
            Left (DeviceLaunchLimitExceeded 0 1) -> pure ()
            other -> failTest ("launch one-below boundary changed: " ++ showPrepared other)
        _ <- expectRight "VJP exact transfer/work/launch limits" (prepareMatMulVJP (deviceLimits 256 48 2) left right seed)
        case prepareMatMulVJP (deviceLimits 255 48 2) left right seed of
            Left (DeviceTransferLimitExceeded 255 256) -> pure ()
            other -> failTest ("VJP transfer one-below boundary changed: " ++ showPrepared other)
        case prepareMatMulVJP (deviceLimits 256 47 2) left right seed of
            Left (DeviceWorkLimitExceeded 47 48) -> pure ()
            other -> failTest ("VJP work one-below boundary changed: " ++ showPrepared other)
        case prepareMatMulVJP (deviceLimits 256 48 1) left right seed of
            Left (DeviceLaunchLimitExceeded 1 2) -> pure ()
            other -> failTest ("VJP launch one-below boundary changed: " ++ showPrepared other)

        if gpuBackendCompiled
            then do
                when gpuFaultInjectionCompiled (checkDynamicLoaderFailures session prepared)
                requireHardware <- (== Just "1") <$> lookupEnv "MARKOVIAN_CUDA_REQUIRE_HARDWARE"
                available <- gpuBackendAvailable
                if available
                    then do
                        cuda <- expectRightIO "required CUDA matrix product" (runPreparedMatMul session (RequireCUDA selector) prepared)
                        assertApproxList "CPU/CUDA matrix differential" expected (deviceTensorValues (fst cuda))
                        cudaVJP <- expectRightIO "required CUDA matrix VJP" (runPreparedMatMulVJP session (RequireCUDA selector) preparedVJP)
                        assertVJP expectedLeft expectedRight (fst cudaVJP)
                        case deviceExecutionBackend (snd cuda) of
                            CUDASelected admission -> do
                                assertEqual "PTX target" "sm_121" (cudaAdmissionPTXTarget admission)
                                assertEqual "native UUID admission binding" (cudaDeviceUUID (cudaAdmissionDevice admission)) (cudaAdmissionNativeVerifiedUUID admission)
                                assert "admission self-test" (cudaAdmissionSelfTestPassed admission)
                            other -> failTest ("required CUDA selected another backend: " ++ show other)
                        checkScopedFork selector prepared
                        when gpuFaultInjectionCompiled (checkFaultTransactions selector session prepared preparedVJP)
                    else
                        if requireHardware
                            then failTest ("protected CUDA runner did not admit its pinned device: " ++ show probe)
                            else checkEnabledUnavailable session prepared
            else checkDisabled session prepared
        pure (Right ())
    either (failTest . show) pure sessionResult
    putStrLn "markovian-gpu: device contract tests passed"

selectedDevice :: IO DeviceSelector
selectedDevice = do
    configured <- lookupEnv "MARKOVIAN_CUDA_DEVICE_UUID"
    pure (maybe DeterministicFirstDevice DeviceByUUID configured)

checkDisabled ::
    TensorSession region ->
    PreparedMatMul region rows inner columns ->
    IO ()
checkDisabled session prepared = do
    required <- runPreparedMatMul session (RequireCUDA DeterministicFirstDevice) prepared
    case required of
        Left (DeviceCUDAError CUDANotCompiled) -> pure ()
        other -> failTest ("disabled required-CUDA contract changed: " ++ show other)
    fallback <- expectRightIO "disabled prelaunch fallback" (runPreparedMatMul session (PreferCUDA DeterministicFirstDevice FallbackBeforeUserLaunch) prepared)
    case deviceExecutionBackend (snd fallback) of
        CPUFallback (CUDARejectedBeforeUserLaunch CUDANotCompiled) -> pure ()
        other -> failTest ("disabled fallback report changed: " ++ show other)
    denied <- runPreparedMatMul session (PreferCUDA DeterministicFirstDevice NoFallback) prepared
    case denied of
        Left (DeviceCUDAError CUDANotCompiled) -> pure ()
        other -> failTest ("disabled no-fallback contract changed: " ++ show other)

checkEnabledUnavailable ::
    TensorSession region ->
    PreparedMatMul region rows inner columns ->
    IO ()
checkEnabledUnavailable session prepared = do
    required <- runPreparedMatMul session (RequireCUDA DeterministicFirstDevice) prepared
    case required of
        Left (DeviceCUDAError problem) | isPrelaunchUnavailable problem -> pure ()
        other -> failTest ("required CUDA did not report explicit unavailability: " ++ show other)
    fallback <- expectRightIO "enabled missing-driver fallback" (runPreparedMatMul session (PreferCUDA DeterministicFirstDevice FallbackBeforeUserLaunch) prepared)
    case deviceExecutionBackend (snd fallback) of
        CPUFallback (CUDARejectedBeforeUserLaunch problem)
            | isPrelaunchUnavailable problem -> pure ()
        other -> failTest ("enabled missing-driver fallback changed: " ++ show other)
    denied <- runPreparedMatMul session (PreferCUDA DeterministicFirstDevice NoFallback) prepared
    case denied of
        Left (DeviceCUDAError problem) | isPrelaunchUnavailable problem -> pure ()
        other -> failTest ("enabled missing-driver no-fallback changed: " ++ show other)

isPrelaunchUnavailable :: CUDAError -> Bool
isPrelaunchUnavailable (CUDAProbeFailed _) = True
isPrelaunchUnavailable (CUDADeviceNotFound _) = True
isPrelaunchUnavailable (CUDADeviceUnsupported _ _) = True
isPrelaunchUnavailable (CUDAExecutorFailure failure) = cudaFailureFallbackPermitted failure
isPrelaunchUnavailable _ = False

checkDynamicLoaderFailures ::
    TensorSession region ->
    PreparedMatMul region rows inner columns ->
    IO ()
checkDynamicLoaderFailures session prepared = do
    checkLoaderFailure
        session
        prepared
        "/__markovian_missing_driver__/libcuda.so.1"
        CUDADriverLoad
        "MARKOVIAN_CUDA_DRIVER_UNAVAILABLE"
    checkLoaderFailure
        session
        prepared
        "@self"
        CUDASymbolResolve
        "MARKOVIAN_CUDA_ABI_SYMBOL_UNAVAILABLE"
    checkOwnedIncompleteDrivers session prepared
    checkUnsupportedDevice session prepared

checkOwnedIncompleteDrivers :: TensorSession region -> PreparedMatMul region rows inner columns -> IO ()
checkOwnedIncompleteDrivers session prepared = do
    early <- lookupEnv "MARKOVIAN_CUDA_EARLY_INCOMPLETE_DRIVER"
    late <- lookupEnv "MARKOVIAN_CUDA_LATE_INCOMPLETE_DRIVER"
    marker <- lookupEnv "MARKOVIAN_CUDA_INIT_MARKER"
    case (early, late, marker) of
        (Just earlyLibrary, Just lateLibrary, Just markerPath) -> do
            checkLoaderFailure session prepared earlyLibrary CUDASymbolResolve "MARKOVIAN_CUDA_ABI_SYMBOL_UNAVAILABLE"
            checkLoaderFailure session prepared lateLibrary CUDASymbolResolve "MARKOVIAN_CUDA_ABI_SYMBOL_UNAVAILABLE"
            markerRead <- tryIOError (readFile markerPath)
            case markerRead of
                Left _ -> pure ()
                Right contents -> failTest ("incomplete-driver symbol admission called cuInit: " ++ contents)
        (Nothing, Nothing, Nothing) -> pure ()
        _ -> failTest "incomplete-driver fixture environment is only partially configured"

checkUnsupportedDevice ::
    TensorSession region ->
    PreparedMatMul region rows inner columns ->
    IO ()
checkUnsupportedDevice session prepared =
    withEnvironment "MARKOVIAN_CUDA_FAULT_UNSUPPORTED_DEVICE" "1" $ do
        unsupportedProbe <- probeCUDA
        case cudaProbeDevices unsupportedProbe of
            [device] -> do
                assertEqual "unsupported fixture major" 8 (cudaComputeCapabilityMajor device)
                assertEqual "unsupported fixture minor" 0 (cudaComputeCapabilityMinor device)
            other -> failTest ("unsupported fixture probe changed: " ++ show other)
        available <- gpuBackendAvailable
        assert "unsupported device must not be reported available" (not available)
        required <- runPreparedMatMul session (RequireCUDA DeterministicFirstDevice) prepared
        case required of
            Left (DeviceCUDAError (CUDADeviceUnsupported device _)) ->
                assertEqual "unsupported required capability" (8, 0) (cudaComputeCapabilityMajor device, cudaComputeCapabilityMinor device)
            other -> failTest ("unsupported required-CUDA behavior changed: " ++ show other)
        fallback <- expectRightIO "unsupported-device prelaunch fallback" (runPreparedMatMul session (PreferCUDA DeterministicFirstDevice FallbackBeforeUserLaunch) prepared)
        case deviceExecutionBackend (snd fallback) of
            CPUFallback (CUDARejectedBeforeUserLaunch (CUDADeviceUnsupported _ _)) -> pure ()
            other -> failTest ("unsupported-device fallback changed: " ++ show other)
        denied <- runPreparedMatMul session (PreferCUDA DeterministicFirstDevice NoFallback) prepared
        case denied of
            Left (DeviceCUDAError (CUDADeviceUnsupported _ _)) -> pure ()
            other -> failTest ("unsupported-device no-fallback changed: " ++ show other)

checkLoaderFailure ::
    TensorSession region ->
    PreparedMatMul region rows inner columns ->
    String ->
    CUDAStage ->
    String ->
    IO ()
checkLoaderFailure session prepared library expectedStage expectedName =
    withEnvironment "MARKOVIAN_CUDA_DRIVER_LIBRARY" library $ do
        loaderProbe <- probeCUDA
        case cudaProbeFailure loaderProbe of
            Just diagnostic -> do
                assertEqual "dynamic-loader failure stage" expectedStage (cudaDiagnosticStage diagnostic)
                assertEqual "dynamic-loader failure name" expectedName (cudaDiagnosticName diagnostic)
            Nothing -> failTest ("dynamic-loader fixture unexpectedly probed a device: " ++ show loaderProbe)
        required <- runPreparedMatMul session (RequireCUDA DeterministicFirstDevice) prepared
        case required of
            Left (DeviceCUDAError (CUDAProbeFailed diagnostic)) ->
                assertEqual "required loader failure name" expectedName (cudaDiagnosticName diagnostic)
            other -> failTest ("required loader failure changed: " ++ show other)
        fallback <- expectRightIO "loader failure prelaunch fallback" (runPreparedMatMul session (PreferCUDA DeterministicFirstDevice FallbackBeforeUserLaunch) prepared)
        case deviceExecutionBackend (snd fallback) of
            CPUFallback (CUDARejectedBeforeUserLaunch (CUDAProbeFailed diagnostic)) ->
                assertEqual "fallback loader failure name" expectedName (cudaDiagnosticName diagnostic)
            other -> failTest ("loader failure fallback changed: " ++ show other)

withEnvironment :: String -> String -> IO value -> IO value
withEnvironment name value action = bracket acquire restore (const action)
  where
    acquire = do
        previous <- lookupEnv name
        setEnv name value
        pure previous
    restore Nothing = unsetEnv name
    restore (Just previous) = setEnv name previous

checkFiniteDifferences ::
    FiniteTensor region 'F64 '[2, 3] ->
    FiniteTensor region 'F64 '[3, 2] ->
    FiniteTensor region 'F64 '[2, 2] ->
    [Double] ->
    [Double] ->
    IO ()
checkFiniteDifferences left right seed expectedLeft expectedRight = do
    leftValues <- tensorToList (hostTensor left)
    rightValues <- tensorToList (hostTensor right)
    seedValues <- tensorToList (hostTensor seed)
    let epsilon = 1e-6
        objective xs ys = sum (zipWith (*) seedValues (oracleMatMul 2 3 2 xs ys))
        finiteDifferenceLeft index =
            (objective (perturb index epsilon leftValues) rightValues - objective (perturb index (-epsilon) leftValues) rightValues) / (2 * epsilon)
        finiteDifferenceRight index =
            (objective leftValues (perturb index epsilon rightValues) - objective leftValues (perturb index (-epsilon) rightValues)) / (2 * epsilon)
    assertApproxListWith 2e-8 "all-coordinate left finite difference" expectedLeft (map finiteDifferenceLeft [0 .. length leftValues - 1])
    assertApproxListWith 2e-8 "all-coordinate right finite difference" expectedRight (map finiteDifferenceRight [0 .. length rightValues - 1])

oracleMatMul :: Int -> Int -> Int -> [Double] -> [Double] -> [Double]
oracleMatMul rows inner columns left right =
    [ sum [left !! (row * inner + k) * right !! (k * columns + column) | k <- [0 .. inner - 1]]
    | row <- [0 .. rows - 1]
    , column <- [0 .. columns - 1]
    ]

perturb :: Int -> Double -> [Double] -> [Double]
perturb target delta = zipWith (\index value -> if index == target then value + delta else value) [0 ..]

checkScopedFork :: DeviceSelector -> PreparedMatMul region rows inner columns -> IO ()
checkScopedFork selector prepared = do
    result <- withCUDAExecutor selector $ \executor -> do
        started <- newEmptyMVar
        done <- newEmptyMVar
        _ <- forkIO $ do
            putMVar started ()
            outcome <- runPreparedMatMulCUDA executor prepared
            putMVar done (void outcome)
        takeMVar started
        pure (Right done)
    done <- expectRight "scoped fork owner" result
    child <- takeMVar done
    case child of
        Right () -> pure ()
        Left CUDAExecutorClosed -> pure ()
        Left problem -> failTest ("scoped fork returned unexpected failure: " ++ show problem)

checkFaultTransactions ::
    DeviceSelector ->
    TensorSession region ->
    PreparedMatMul region rows inner columns ->
    PreparedMatMulVJP region rows inner columns ->
    IO ()
checkFaultTransactions selector session prepared preparedVJP = do
    forM_ ([10, 11, 12, 13, 14] :: [Int]) $ \stage ->
        withFault (show stage ++ ":1") $ do
            result <- runPreparedMatMul session (PreferCUDA selector FallbackBeforeUserLaunch) prepared
            case result of
                Right (_, report) -> case deviceExecutionBackend report of
                    CPUFallback _ -> pure ()
                    other -> failTest ("prelaunch fault did not select fallback at stage " ++ show stage ++ ": " ++ show other)
                other -> failTest ("prelaunch fault did not return CPU result at stage " ++ show stage ++ ": " ++ show other)
    forM_ ([15, 16, 17, 18, 19, 20, 24, 28] :: [Int]) $ \stage ->
        expectFallbackDenied ("committed/cleanup stage " ++ show stage) $ withFault (show stage ++ ":1") $
            runPreparedMatMul session (PreferCUDA selector FallbackBeforeUserLaunch) prepared
    expectFallbackDenied "prelaunch primary plus cleanup failure" $ withFault "13:1,18:1" $
        runPreparedMatMul session (PreferCUDA selector FallbackBeforeUserLaunch) prepared
    expectFallbackDenied "second VJP launch" $ withFault "15:2" $
        runPreparedMatMulVJP session (PreferCUDA selector FallbackBeforeUserLaunch) preparedVJP
    expectFallbackDenied "post-first-launch second allocation" $ withFault "10:2" $
        runPreparedMatMulVJP session (PreferCUDA selector FallbackBeforeUserLaunch) preparedVJP
    withFault "10:1" $ do
        retried <- withCUDAExecutor selector $ \executor -> do
            first <- runPreparedMatMulCUDA executor prepared
            second <- runPreparedMatMulCUDA executor prepared
            pure (Right (first, second))
        case retried of
            Right (Left (CUDAExecutorFailure _), Right _) -> pure ()
            other -> failTest ("same-executor allocation retry changed: " ++ show other)
    withFault "18:1" $ do
        poisoned <- withCUDAExecutor selector $ \executor -> do
            first <- runPreparedMatMulCUDA executor prepared
            second <- runPreparedMatMulCUDA executor prepared
            pure (Right (first, second))
        case poisoned of
            Right (Left (CUDAExecutorFailure _), Left (CUDAExecutorFailure poisonFailure)) ->
                case cudaFailurePrimary poisonFailure of
                    Just diagnostic -> assertEqual "poisoned executor stage" CUDAExecutorPoisoned (cudaDiagnosticStage diagnostic)
                    Nothing -> failTest "poisoned executor omitted its primary diagnostic"
            other -> failTest ("cleanup uncertainty did not poison executor: " ++ show other)
    withFault "10:1,24:1" $ do
        result <- withCUDAExecutor selector $ \executor -> do
            outcome <- runPreparedMatMulCUDA executor prepared
            pure (void outcome)
        case result of
            Left (CUDAActionAndCleanupFailure (CUDAExecutorFailure primary) teardown) -> do
                assert "primary diagnostic retained" (isJust (cudaFailurePrimary primary))
                assert "teardown diagnostic retained" (not (null (cudaFailureCleanup teardown)))
                assert "bounded teardown diagnostics" (length (cudaFailureCleanup teardown) <= 8)
            other -> failTest ("primary plus teardown diagnostics changed: " ++ show other)
    withFault "24:1" $ do
        result <- withCUDAExecutor selector (\_ -> ioError (userError "injected action exception") :: IO (Either CUDAError ()))
        case result of
            Left (CUDAActionAndCleanupFailure (CUDAActionException message) teardown) -> do
                assert "action exception retained" ("injected action exception" `isInfixOf` message)
                assert "exception teardown retained" (not (null (cudaFailureCleanup teardown)))
            other -> failTest ("exception plus teardown diagnostics changed: " ++ show other)

withFault :: String -> IO value -> IO value
withFault specification = bracket_ (setEnv "MARKOVIAN_CUDA_FAULTS" specification) (unsetEnv "MARKOVIAN_CUDA_FAULTS")

expectFallbackDenied :: (Show value) => String -> IO (Either DeviceError value) -> IO ()
expectFallbackDenied label action = do
    result <- action
    case result of
        Left (DeviceCUDAError _) -> pure ()
        other -> failTest (label ++ " unexpectedly fell back: " ++ show other)

showPrepared :: (Show error) => Either error value -> String
showPrepared (Left problem) = "Left " ++ show problem
showPrepared (Right _) = "Right <prepared>"

assertVJP :: [Double] -> [Double] -> DeviceVJP left right -> IO ()
assertVJP expectedLeft expectedRight gradients = do
    assertApproxList "left VJP differential" expectedLeft (deviceTensorValues (deviceLeftGradient gradients))
    assertApproxList "right VJP differential" expectedRight (deviceTensorValues (deviceRightGradient gradients))

assertApproxList :: String -> [Double] -> [Double] -> IO ()
assertApproxList = assertApproxListWith 2e-12

assertApproxListWith :: Double -> String -> [Double] -> [Double] -> IO ()
assertApproxListWith relativeTolerance label expected actual = do
    assertEqual (label ++ " length") (length expected) (length actual)
    forM_ (zip3 [0 :: Int ..] expected actual) $ \(index, wanted, observed) ->
        let tolerance = relativeTolerance + relativeTolerance * max (abs wanted) (abs observed)
         in unless (abs (wanted - observed) <= tolerance) (failTest (label ++ " at " ++ show index ++ ": expected " ++ show wanted ++ ", got " ++ show observed))

assert :: String -> Bool -> IO ()
assert _ True = pure ()
assert label False = failTest label

assertEqual :: (Eq value, Show value) => String -> value -> value -> IO ()
assertEqual label expected actual = unless (expected == actual) (failTest (label ++ ": expected " ++ show expected ++ ", got " ++ show actual))

expectRightIO :: (Show error) => String -> IO (Either error value) -> IO value
expectRightIO label action = action >>= expectRight label

expectRight :: (Show error) => String -> Either error value -> IO value
expectRight _ (Right value) = pure value
expectRight label (Left problem) = failTest (label ++ ": " ++ show problem)

failTest :: String -> IO value
failTest message = do
    putStrLn ("FAIL: " ++ message)
    exitFailure
\end{code}
