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
            results <- replicateM 20 (gpuDenseApply size size matrix input)
            case sequenceA results of
                Left err -> do
                    putStrLn ("CUDA benchmark failed: " ++ show err)
                    exitFailure
                Right successful -> do
                    let durations = fmap gpuTransferInclusiveMilliseconds successful
                        outputs = fmap gpuDenseOutput successful
                        average = sum durations / fromIntegral (length durations)
                        maximumError =
                            foldr
                                max
                                0
                                [ abs (value - 1 / fromIntegral size)
                                | output <- outputs
                                , value <- output
                                ]
                    printf "transfer-inclusive mean: %.6f ms\n" average
                    printf "maximum differential error: %.3e\n" maximumError
