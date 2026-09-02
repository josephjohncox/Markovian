{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (replicateM, unless)
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
    configuredUUID <- lookupEnv "MARKOVIAN_CUDA_DEVICE_UUID"
    probe <- probeCUDA
    let selector = maybe DeterministicFirstDevice DeviceByUUID configuredUUID
        indices = [0 :: Int .. 63]
        valuesLeft = [fromIntegral ((row + column) `mod` 7 - 3) / 8 | row <- indices, column <- indices]
        valuesRight = [if row == column then 1 else fromIntegral ((row * 3 + column) `mod` 5 - 2) / 32 | row <- indices, column <- indices]
        limits = tensorSessionLimits 4 1024 1000000 8000000 128000000 1024 1000000000
        executionLimits = deviceLimits 1000000 100000000 2
        expected = oracleMatMul64 valuesLeft valuesRight
        expectedChecksum = semanticChecksum expected
    unless (expectedChecksum == 386.2421875) (failBenchmark "committed semantic checksum changed")
    putStrLn "benchmark-report-version: 1"
    putStrLn ("host-os: " ++ os)
    putStrLn ("host-arch: " ++ arch)
    putStrLn ("haskell-compiler: " ++ compilerName ++ "-" ++ showVersion compilerVersion)
    putStrLn "clock: GHC.Clock.getMonotonicTimeNSec"
    putStrLn ("cuda-compiled: " ++ show gpuBackendCompiled)
    putStrLn ("cuda-fault-injection-compiled: " ++ show gpuFaultInjectionCompiled)
    putStrLn ("cuda-driver-version: " ++ show (cudaProbeDriverVersion probe))
    putStrLn ("selected-device: " ++ show selector)
    printf "expected-semantic-checksum: %.17g\n" expectedChecksum
    sessionResult <- withTensorSession limits $ \session -> do
        left <- fst <$> requireTensor (finiteTensorFromList session shape64 valuesLeft)
        right <- fst <$> requireTensor (finiteTensorFromList session shape64 valuesRight)
        prepared <- requirePlan (prepareMatMul executionLimits left right)
        cpuWarmup <- requireDevice (runPreparedMatMul session CPUOnly prepared)
        assertOutput "CPU warmup" expected (deviceTensorValues (fst cpuWarmup))
        cpuSamples <- replicateM 20 (timeCPU expected session prepared)
        printSamples "cpu-total" cpuSamples
        if gpuBackendCompiled
            then do
                cudaResult <- withCUDAExecutor selector $ \executor -> do
                    cudaWarmup <- requireCUDA (runPreparedMatMulCUDA executor prepared)
                    assertOutput "CUDA warmup" expected (deviceTensorValues (fst cudaWarmup))
                    samples <- replicateM 20 (requireCUDA (runPreparedMatMulCUDA executor prepared))
                    mapM_ (assertOutput "CUDA measured output" expected . deviceTensorValues . fst) samples
                    let times = concatMap (deviceExecutionTransferInclusiveMilliseconds . snd) samples
                        checksum = semanticChecksum (deviceTensorValues (fst (last samples)))
                    printSamples "cuda-transfer-inclusive" times
                    printf "cuda-semantic-checksum: %.17g\n" checksum
                    putStrLn ("cuda-admission: " ++ show (cudaExecutorAdmission executor))
                    putStrLn (renderDevicePlanReport (deviceExecutionPlan (snd (last samples))))
                    pure (Right ())
                case cudaResult of
                    Left problem -> failBenchmark ("CUDA benchmark failed: " ++ show problem)
                    Right () -> pure ()
            else putStrLn "cuda-transfer-inclusive: disabled build (contract tested; no hardware timing claimed)"
        pure (Right ())
    either (failBenchmark . show) pure sessionResult

requireTensor :: (Show error) => IO (Either error value) -> IO value
requireTensor action = action >>= either (failBenchmark . show) pure

requirePlan :: (Show error) => Either error value -> IO value
requirePlan = either (failBenchmark . show) pure

requireDevice :: (Show error) => IO (Either error value) -> IO value
requireDevice action = action >>= either (failBenchmark . show) pure

requireCUDA :: (Show error) => IO (Either error value) -> IO value
requireCUDA action = action >>= either (failBenchmark . show) pure

timeCPU :: [Double] -> TensorSession region -> PreparedMatMul region rows inner columns -> IO Double
timeCPU expected session prepared = do
    start <- getMonotonicTimeNSec
    result <- runPreparedMatMulCPU session prepared
    output <- either (failBenchmark . show) pure result
    assertOutput "CPU measured output" expected (deviceTensorValues (fst output))
    end <- getMonotonicTimeNSec
    pure (fromIntegral (end - start) / 1000000)

oracleMatMul64 :: [Double] -> [Double] -> [Double]
oracleMatMul64 left right =
    [ sum [left !! (row * 64 + k) * right !! (k * 64 + column) | k <- [0 .. 63]]
    | row <- [0 .. 63]
    , column <- [0 .. 63]
    ]

semanticChecksum :: [Double] -> Double
semanticChecksum = sum . zipWith (*) [1 ..]

assertOutput :: String -> [Double] -> [Double] -> IO ()
assertOutput label expected actual = do
    unless (length expected == length actual) (failBenchmark (label ++ " length changed"))
    mapM_ check (zip3 [0 :: Int ..] expected actual)
  where
    check (index, wanted, observed) =
        let tolerance = 2e-12 + 2e-12 * max (abs wanted) (abs observed)
         in unless (abs (wanted - observed) <= tolerance) $
                failBenchmark (label ++ " coordinate " ++ show index ++ " changed: expected " ++ show wanted ++ ", got " ++ show observed)

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
