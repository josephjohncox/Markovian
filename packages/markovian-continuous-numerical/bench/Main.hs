module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Markovian.Continuous.Numerical.Quadrature
import Markovian.Continuous.Numerical.Value
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

main :: IO ()
main = do
    interval <- require (compactDoubleInterval 0 1)
    tolerance <- require (integrationTolerance 1e-12 1e-12)
    let limits = QuadratureLimits 3015 100 30
        execute = runQuadrature limits tolerance interval
    expected <- execute
    _ <- timed execute
    samples <- replicateM 20 (timed execute)
    if all ((== expected) . snd) samples
        then do
            let durations = map fst samples
                mean = sum durations / 20
                variance = sum [(value - mean) ^ (2 :: Int) | value <- durations] / 19
            putStrLn "warmup runs: 1 (excluded)"
            putStrLn "measured runs: 20"
            mapM_ (\(index, value) -> printf "sample %02d: %.3f us\n" (index :: Int) value) (zip [1 ..] durations)
            printf "mean: %.3f us\nsample standard deviation: %.3f us\nminimum: %.3f us\nmaximum: %.3f us\n" mean (sqrt variance) (minimum durations) (maximum durations)
            putStrLn ("semantic report: " ++ show expected)
        else fail "semantic report changed during numerical benchmark"

runQuadrature :: QuadratureLimits -> IntegrationTolerance -> CompactDoubleInterval -> IO QuadratureReport
runQuadrature limits tolerance interval = do
    report <- require (integrateGK15 limits tolerance interval (Right . (** 20)))
    _ <- evaluate (length (show report))
    pure report

timed :: IO value -> IO (Double, value)
timed action = do
    start <- getCPUTime
    value <- action
    stop <- getCPUTime
    pure (fromIntegral (stop - start) / 1e6, value)

require :: (Show error) => Either error value -> IO value
require value = case value of Left err -> fail (show err); Right result -> pure result
