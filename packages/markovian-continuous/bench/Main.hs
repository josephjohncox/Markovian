module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Markovian.Continuous.Measure.Exact
import Markovian.Continuous.Polynomial
import Markovian.Continuous.Space (RealBorel)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

data Owner

main :: IO ()
main = do
    interval <- require (rationalInterval 0 1)
    law <- require (uniformReal limits (noiseOwner 1 :: NoiseOwner Owner) interval)
    polynomial <- require (rationalPolynomial limits [(8, 1), (2, 3), (0, 1)])
    expected <- require (expectPolynomial limits law polynomial)
    _ <- timed law polynomial
    samples <- replicateM 20 (timed law polynomial)
    let reports = map snd samples
    if all (== expected) reports
        then do
            let durations = map fst samples
                mean = sum durations / 20
                variance = sum [(value - mean) ^ (2 :: Int) | value <- durations] / 19
            putStrLn "warmup runs: 1 (excluded)"
            putStrLn "measured runs: 20"
            mapM_ (\(index, value) -> printf "sample %02d: %.3f us\n" (index :: Int) value) (zip [1 ..] durations)
            printf "mean: %.3f us\nsample standard deviation: %.3f us\nminimum: %.3f us\nmaximum: %.3f us\n" mean (sqrt variance) (minimum durations) (maximum durations)
            putStrLn ("semantic report: " ++ show expected)
        else fail "semantic report changed during exact benchmark"
    bivariateLaw <-
        require
            ( affineUniformLaw
                limits
                1
                [ (noiseOwner 2 :: NoiseOwner Owner, 2, interval)
                , (noiseOwner 3 :: NoiseOwner Owner, -1, interval)
                ]
            )
    bivariatePolynomial <- require (rationalBivariatePolynomial limits [(2, 3, 3), (1, 2, -5), (0, 0, 7)])
    let joint = shareAffineSource bivariateLaw
    expectedBivariate <- require (expectBivariatePolynomial limits joint bivariatePolynomial)
    _ <- timedBivariate joint bivariatePolynomial
    bivariateSamples <- replicateM 20 (timedBivariate joint bivariatePolynomial)
    if all ((== expectedBivariate) . snd) bivariateSamples
        then do
            putStrLn "bivariate measured runs: 20"
            putStrLn ("bivariate semantic accounting report: " ++ show expectedBivariate)
        else fail "bivariate accounting report changed during exact benchmark"
  where
    limits = ExactLimits 16 1000 100000 10000 8 100000 2048

timed :: ExactLaw RealBorel -> RationalPolynomial -> IO (Double, ExactIntegralReport)
timed law polynomial = do
    start <- getCPUTime
    report <- require (expectPolynomial (ExactLimits 16 1000 100000 10000 8 100000 2048) law polynomial)
    _ <- evaluate (length (show report))
    stop <- getCPUTime
    pure (fromIntegral (stop - start) / 1e6, report)

timedBivariate :: ExactJointLaw RealBorel RealBorel -> RationalBivariatePolynomial -> IO (Double, ExactIntegralReport)
timedBivariate law polynomial = do
    start <- getCPUTime
    report <- require (expectBivariatePolynomial (ExactLimits 16 1000 100000 10000 8 100000 2048) law polynomial)
    _ <- evaluate (length (show report))
    stop <- getCPUTime
    pure (fromIntegral (stop - start) / 1e6, report)

require :: (Show error) => Either error value -> IO value
require value = case value of Left err -> fail (show err); Right result -> pure result
