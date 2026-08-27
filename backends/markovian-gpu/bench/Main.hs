module Main (main) where

import Control.Monad (replicateM)
import Markovian.Backend.GPU (
    GPUDenseResult (..),
    gpuBackendAvailable,
    gpuBackendCompiled,
    gpuDenseApply,
 )
import System.Exit (exitFailure)
import Text.Printf (printf)

main :: IO ()
main = do
    available <- gpuBackendAvailable
    if not gpuBackendCompiled || not available
        then putStrLn "CUDA benchmark skipped: backend is not compiled or no device is available"
        else do
            let size = 256
                matrix =
                    [ if row == column then 1 else 0
                    | row <- [0 .. size - 1]
                    , column <- [0 .. size - 1]
                    ]
                input = replicate size (1 / fromIntegral size)
            warmup <- gpuDenseApply size size matrix input
            case warmup of
                Left err -> do
                    putStrLn ("CUDA benchmark warmup failed: " ++ show err)
                    exitFailure
                Right _ -> do
                    let measuredRuns = 20
                    results <- replicateM measuredRuns (gpuDenseApply size size matrix input)
                    case sequenceA results of
                        Left err -> do
                            putStrLn ("CUDA benchmark failed: " ++ show err)
                            exitFailure
                        Right successful -> do
                            let durations = fmap gpuTransferInclusiveMilliseconds successful
                                outputs = fmap gpuDenseOutput successful
                                average = sum durations / fromIntegral measuredRuns
                                sampleVariance =
                                    sum (fmap (\duration -> (duration - average) ^ (2 :: Int)) durations)
                                        / fromIntegral (measuredRuns - 1)
                                sampleStandardDeviation = sqrt sampleVariance
                                minimumDuration = foldr min average durations
                                maximumDuration = foldr max average durations
                                maximumError =
                                    foldr
                                        max
                                        0
                                        [ abs (value - 1 / fromIntegral size)
                                        | output <- outputs
                                        , value <- output
                                        ]
                            putStrLn "warmup runs: 1 (excluded)"
                            printf "measured runs: %d\n" measuredRuns
                            mapM_
                                (\(sample, duration) -> printf "sample %02d: %.9f ms\n" (sample :: Int) duration)
                                (zip [1 ..] durations)
                            printf "transfer-inclusive mean: %.9f ms\n" average
                            printf "sample standard deviation: %.9f ms\n" sampleStandardDeviation
                            printf "minimum: %.9f ms\n" minimumDuration
                            printf "maximum: %.9f ms\n" maximumDuration
                            printf "maximum differential error: %.3e\n" maximumError
