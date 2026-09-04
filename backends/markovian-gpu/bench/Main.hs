{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (replicateM, unless, when)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Version (showVersion)
import GHC.Clock (getMonotonicTimeNSec)
import Markovian.Backend.GPU
import Markovian.Tensor
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.Info (arch, compilerName, compilerVersion, os)
import Text.Printf (printf)

shape64 :: SShape '[64, 64]
shape64 = SCons (Proxy @64) (SCons (Proxy @64) SNil)

main :: IO ()
main = do
    printEvidenceBindings
    requireHardware <- (== Just "1") <$> lookupEnv "MARKOVIAN_CUDA_REQUIRE_HARDWARE"
    when requireHardware requireEvidenceBindings
    configuredUUID <- lookupEnv "MARKOVIAN_CUDA_DEVICE_UUID"
    probe <- probeCUDA
    let selector = maybe DeterministicFirstDevice DeviceByUUID configuredUUID
        indices = [0 :: Int .. 63]
        valuesLeft = [fromIntegral ((row + column) `mod` 7 - 3) / 8 | row <- indices, column <- indices]
        valuesRight = [if row == column then 1 else fromIntegral ((row * 3 + column) `mod` 5 - 2) / 32 | row <- indices, column <- indices]
        limits = tensorSessionLimits 4 1024 1000000 8000000 128000000 1024 1000000000
        executionLimits = deviceLimits 1000000 100000000 2
        expectedExact = exactMatMul64 valuesLeft valuesRight
        expectedChecksum = exactSemanticChecksum expectedExact
    unless (expectedChecksum == 49439 / 128) (failBenchmark "profile exact semantic checksum changed")
    putStrLn "benchmark-report-version: 1"
    putStrLn ("host-os: " ++ os)
    putStrLn ("host-arch: " ++ arch)
    putStrLn ("haskell-compiler: " ++ compilerName ++ "-" ++ showVersion compilerVersion)
    putStrLn "clock: GHC.Clock.getMonotonicTimeNSec"
    putStrLn ("cuda-compiled: " ++ show gpuBackendCompiled)
    putStrLn ("cuda-fault-injection-compiled: " ++ show gpuFaultInjectionCompiled)
    putStrLn ("cuda-driver-version: " ++ show (cudaProbeDriverVersion probe))
    putStrLn ("selected-device: " ++ show selector)
    putStrLn "exact-semantic-checksum: 49439/128"
    printf "expected-semantic-checksum-decimal: %.17g\n" (fromRational expectedChecksum :: Double)
    sessionResult <- withTensorSession limits $ \session -> do
        left <- fst <$> requireTensor (finiteTensorFromList session shape64 valuesLeft)
        right <- fst <$> requireTensor (finiteTensorFromList session shape64 valuesRight)
        prepared <- requirePlan (prepareMatMul executionLimits left right)
        cpuWarmup <- requireDevice (runPreparedMatMul session CPUOnly prepared)
        assertOutput "CPU operation-order warmup refinement" expectedExact (deviceTensorValues (fst cpuWarmup))
        cpuSamples <- replicateM 20 (timeCPU expectedExact session prepared)
        printSamples "cpu-total" cpuSamples
        if gpuBackendCompiled
            then do
                cudaResult <- withCUDAExecutor selector $ \executor -> do
                    cudaWarmup <- requireCUDA (runPreparedMatMulCUDA executor prepared)
                    assertOutput "CUDA FMA warmup refinement" expectedExact (deviceTensorValues (fst cudaWarmup))
                    samples <- replicateM 20 (requireCUDA (runPreparedMatMulCUDA executor prepared))
                    mapM_ (assertOutput "CUDA FMA measured refinement" expectedExact . deviceTensorValues . fst) samples
                    let times = concatMap (deviceExecutionTransferInclusiveMilliseconds . snd) samples
                        checksum = observedSemanticChecksum (deviceTensorValues (fst (last samples)))
                        planReport = renderDevicePlanReport (deviceExecutionPlan (snd (last samples)))
                    requireProfileBinding planReport
                    printSamples "cuda-transfer-inclusive" times
                    printf "cuda-semantic-checksum: %.17g\n" checksum
                    let admission = cudaExecutorAdmission executor
                    putStrLn ("cuda-admission: " ++ show admission)
                    putStrLn ("native-observed-device-uuid: " ++ evidenceUUID (cudaAdmissionNativeVerifiedUUID admission))
                    case cudaProbeDriverVersion probe of
                        Just version -> putStrLn ("native-observed-driver-api-version: " ++ show version)
                        Nothing -> failBenchmark "admitted CUDA benchmark lacks a driver API version"
                    putStrLn planReport
                    pure (Right ())
                case cudaResult of
                    Left problem -> failBenchmark ("CUDA benchmark failed: " ++ show problem)
                    Right () -> pure ()
            else putStrLn "cuda-transfer-inclusive: disabled build (contract tested; no hardware timing claimed)"
        pure (Right ())
    either (failBenchmark . show) pure sessionResult

printEvidenceBindings :: IO ()
printEvidenceBindings = do
    session <- fromMaybe "unrecorded-local-session" <$> lookupEnv "MARKOVIAN_EVIDENCE_SESSION_ID"
    revision <- fromMaybe "unrecorded-local-revision" <$> lookupEnv "MARKOVIAN_EVIDENCE_SOURCE_REVISION"
    profile <- fromMaybe "unrecorded-local-profile" <$> lookupEnv "MARKOVIAN_CUDA_PROFILE_SHA256"
    device <- fromMaybe "unrecorded-local-device" <$> lookupEnv "MARKOVIAN_EVIDENCE_DEVICE_UUID"
    driver <- fromMaybe "unrecorded-local-driver" <$> lookupEnv "MARKOVIAN_EVIDENCE_DRIVER_VERSION"
    toolkit <- fromMaybe "unrecorded-local-toolkit" <$> lookupEnv "MARKOVIAN_EVIDENCE_TOOLKIT_VERSION"
    putStrLn ("evidence-session-id: " ++ session)
    putStrLn ("source-revision: " ++ revision)
    putStrLn ("profile-sha256: " ++ profile)
    putStrLn ("evidence-device-uuid: " ++ device)
    putStrLn ("evidence-driver-version: " ++ driver)
    putStrLn ("evidence-toolkit-version: " ++ toolkit)

requireEvidenceBindings :: IO ()
requireEvidenceBindings = do
    session <- lookupEnv "MARKOVIAN_EVIDENCE_SESSION_ID"
    revision <- lookupEnv "MARKOVIAN_EVIDENCE_SOURCE_REVISION"
    profile <- lookupEnv "MARKOVIAN_CUDA_PROFILE_SHA256"
    device <- lookupEnv "MARKOVIAN_EVIDENCE_DEVICE_UUID"
    driver <- lookupEnv "MARKOVIAN_EVIDENCE_DRIVER_VERSION"
    toolkit <- lookupEnv "MARKOVIAN_EVIDENCE_TOOLKIT_VERSION"
    unless (maybe False ((>= 8) . length) session) (failBenchmark "hardware evidence session identity is missing")
    unless (maybe False ((== 40) . length) revision) (failBenchmark "hardware evidence source revision is missing")
    unless (maybe False ((== 64) . length) profile) (failBenchmark "hardware evidence profile digest is missing")
    unless (maybe False ((>= 40) . length) device) (failBenchmark "hardware evidence device UUID is missing")
    unless (maybe False (not . null) driver) (failBenchmark "hardware evidence driver version is missing")
    unless (maybe False (not . null) toolkit) (failBenchmark "hardware evidence toolkit version is missing")

requireProfileBinding :: String -> IO ()
requireProfileBinding report = do
    expected <- lookupEnv "MARKOVIAN_CUDA_PROFILE_SHA256"
    case expected of
        Just digest -> unless (("profile-sha256: " ++ digest) `isInfixOf` report) (failBenchmark "runtime report profile digest changed")
        Nothing -> pure ()

evidenceUUID :: String -> String
evidenceUUID raw =
    let (first, rest1) = splitAt 8 raw
        (second, rest2) = splitAt 4 rest1
        (third, rest3) = splitAt 4 rest2
        (fourth, fifth) = splitAt 4 rest3
     in "GPU-" ++ first ++ "-" ++ second ++ "-" ++ third ++ "-" ++ fourth ++ "-" ++ fifth

requireTensor :: (Show error) => IO (Either error value) -> IO value
requireTensor action = action >>= either (failBenchmark . show) pure

requirePlan :: (Show error) => Either error value -> IO value
requirePlan = either (failBenchmark . show) pure

requireDevice :: (Show error) => IO (Either error value) -> IO value
requireDevice action = action >>= either (failBenchmark . show) pure

requireCUDA :: (Show error) => IO (Either error value) -> IO value
requireCUDA action = action >>= either (failBenchmark . show) pure

timeCPU :: [Rational] -> TensorSession region -> PreparedMatMul region rows inner columns -> IO Double
timeCPU expected session prepared = do
    start <- getMonotonicTimeNSec
    result <- runPreparedMatMulCPU session prepared
    output <- either (failBenchmark . show) pure result
    assertOutput "CPU measured output" expected (deviceTensorValues (fst output))
    end <- getMonotonicTimeNSec
    pure (fromIntegral (end - start) / 1000000)

exactMatMul64 :: [Double] -> [Double] -> [Rational]
exactMatMul64 left right =
    [ sum [toRational (left !! (row * 64 + k)) * toRational (right !! (k * 64 + column)) | k <- [0 .. 63]]
    | row <- [0 .. 63]
    , column <- [0 .. 63]
    ]

exactSemanticChecksum :: [Rational] -> Rational
exactSemanticChecksum = sum . zipWith (*) [1 ..]

observedSemanticChecksum :: [Double] -> Double
observedSemanticChecksum = sum . zipWith (*) [1 ..]

assertOutput :: String -> [Rational] -> [Double] -> IO ()
assertOutput label expected actual = do
    unless (length expected == length actual) (failBenchmark (label ++ " length changed"))
    mapM_ check (zip3 [0 :: Int ..] expected actual)
  where
    check (index, exact, observed) =
        let wanted = fromRational exact
            tolerance = 2e-12 + 2e-12 * max (abs wanted) (abs observed)
         in unless (abs (wanted - observed) <= tolerance) $
                failBenchmark (label ++ " coordinate " ++ show index ++ " changed: exact " ++ show exact ++ ", got " ++ show observed)

printSamples :: String -> [Double] -> IO ()
printSamples label samples = do
    unless (length samples == 20) (failBenchmark "benchmark sample count changed")
    let count = fromIntegral (length samples)
        average = sum samples / count
        variance = sum [(sample - average) ^ (2 :: Int) | sample <- samples] / (count - 1)
    putStrLn (label ++ " warmups: 1 (excluded)")
    putStrLn (label ++ " measured samples: 20")
    mapM_ (\(index, sample) -> printf "%s sample %02d: %.9f ms\n" label (index :: Int) sample) (zip [1 ..] samples)
    printf "%s mean: %.9f ms\n" label average
    printf "%s sample standard deviation: %.9f ms\n" label (sqrt variance)
    printf "%s minimum: %.9f ms\n" label (minimum samples)
    printf "%s maximum: %.9f ms\n" label (maximum samples)

failBenchmark :: String -> IO value
failBenchmark message = do
    putStrLn ("FAIL: " ++ message)
    exitFailure
