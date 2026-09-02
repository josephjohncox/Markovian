{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (replicateM, unless)
import Data.Proxy (Proxy (..))
import Data.Version (showVersion)
import GHC.Clock (getMonotonicTimeNSec)
import Markovian.Tensor
import Markovian.Tensor.Primitive
import System.Info (arch, compilerName, compilerVersion, os)
import Text.Printf (printf)

shape32 :: SShape '[32, 32]
shape32 = SCons (Proxy @32) (SCons (Proxy @32) SNil)

limits :: SessionLimits
limits = tensorSessionLimits 4 1024 10000000 80000000 800000000 1000 1000000000

main :: IO ()
main = do
    let leftValues = [fromIntegral ((index `mod` 17) - 8) / 17 | index <- [0 .. 1023 :: Int]]
        rightValues = [fromIntegral ((index `mod` 13) - 6) / 13 | index <- [0 .. 1023 :: Int]]
        reference = referenceMatmul 32 leftValues rightValues
        checksum = sum reference
        pinnedChecksum = -2.6063348416289536
    unless (approximatelyEqual pinnedChecksum checksum) $
        fail "independent reference changed from the committed semantic checksum"
    (_, warmChecksum, _) <- timed leftValues rightValues
    unless (approximatelyEqual checksum warmChecksum) $
        fail "warmup disagreed with the independent reference"
    samples <- replicateM 20 (timed leftValues rightValues)
    let durations = [duration | (duration, _, _) <- samples]
        checksums = [value | (_, value, _) <- samples]
        reports = [report | (_, _, report) <- samples]
    firstReport <- case reports of
        [] -> fail "benchmark produced no measured reports"
        report : _ -> pure report
    unless (all (approximatelyEqual checksum) checksums && all (== firstReport) reports) $
        fail "timed execution changed its semantic checksum or deterministic plan"
    let mean = sum durations / 20
        variance = sum [(sample - mean) ^ (2 :: Int) | sample <- durations] / 19
    putStrLn "markovian-tensor-benchmark"
    putStrLn ("toolchain: " ++ compilerName ++ "-" ++ showVersion compilerVersion)
    putStrLn ("host: " ++ os ++ "/" ++ arch)
    putStrLn "dtype: F64"
    putStrLn "shape: [32,32] x [32,32]"
    putStrLn "warmup-runs: 1 (excluded)"
    putStrLn "measured-runs: 20"
    mapM_ (\(index, sample) -> printf "sample-%02d-ns: %.0f\n" (index :: Int) sample) (zip [1 ..] durations)
    printf "mean-ns: %.3f\nsample-standard-deviation-ns: %.3f\nminimum-ns: %.0f\nmaximum-ns: %.0f\n" mean (sqrt variance) (minimum durations) (maximum durations)
    printf "semantic-checksum: %.17g\n" checksum
    putStr (renderTensorOperationReport firstReport)

timed :: [Double] -> [Double] -> IO (Double, Double, TensorOperationReport)
timed leftValues rightValues = do
    start <- getMonotonicTimeNSec
    result <- withTensorSession limits $ \session -> do
        left <- requireIO (finiteTensorFromList session shape32 leftValues)
        right <- requireIO (finiteTensorFromList session shape32 rightValues)
        productResult <- requireIO (matmul session (fst left) (fst right))
        values <- tensorToList (hostTensor (fst productResult))
        pure (Right (sum values, snd productResult))
    stop <- getMonotonicTimeNSec
    case result of
        Left problem -> fail (show problem)
        Right (checksum, report) -> pure (fromIntegral (stop - start), checksum, report)

referenceMatmul :: Int -> [Double] -> [Double] -> [Double]
referenceMatmul size left right =
    [ sum [left !! (row * size + k) * right !! (k * size + column) | k <- [0 .. size - 1]]
    | row <- [0 .. size - 1]
    , column <- [0 .. size - 1]
    ]

approximatelyEqual :: Double -> Double -> Bool
approximatelyEqual left right = abs (left - right) <= 2e-10 + 2e-8 * max (abs left) (abs right)

requireIO :: (Show error) => IO (Either error value) -> IO value
requireIO action = do
    result <- action
    case result of
        Left problem -> fail (show problem)
        Right value -> pure value
