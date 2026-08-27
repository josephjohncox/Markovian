\begin{code}
module Main (main) where

import Markovian.Backend.GPU (
    GPUDenseError (..),
    GPUDenseResult (..),
    gpuBackendAvailable,
    gpuBackendCompiled,
    gpuDenseApply,
 )
import System.Exit (exitFailure)

main :: IO ()
main = do
    let rows = 2
        columns = 3
        matrix =
            [ 0.5
            , 0.5
            , 0
            , 0
            , 0.25
            , 0.75
            ]
        input = [0.25, 0.75]
        expected = [0.125, 0.3125, 0.5625]
    available <- gpuBackendAvailable
    result <- gpuDenseApply rows columns matrix input
    if gpuBackendCompiled
        then
            if available
                then case result of
                    Left err -> failTest ("CUDA differential execution failed: " ++ show err)
                    Right output -> do
                        assert "CUDA output differs from CPU reference" (closeVectors 1e-12 expected (gpuDenseOutput output))
                        assert "transfer-inclusive duration must be nonnegative" (gpuTransferInclusiveMilliseconds output >= 0)
                        putStrLn "PASS: CUDA dense differential"
                else failTest "CUDA build cannot see a GPU"
        else case result of
            Left GPUBackendNotCompiled -> putStrLn "PASS: CUDA-disabled package contract"
            unexpected -> failTest ("CUDA-disabled result changed: " ++ show unexpected)

closeVectors :: Double -> [Double] -> [Double] -> Bool
closeVectors tolerance expected actual =
    length expected == length actual
        && and (zipWith (\left right -> abs (left - right) <= tolerance) expected actual)

assert :: String -> Bool -> IO ()
assert _ True = pure ()
assert message False = failTest message

failTest :: String -> IO a
failTest message = do
    putStrLn ("FAIL: " ++ message)
    exitFailure
\end{code}
