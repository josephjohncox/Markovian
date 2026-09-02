{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.List (intercalate)
import Markovian.Algebra.NonNegativeRational (NonNegativeRational, nonNegativeRational)
import Markovian.Category.Finite.Set (FiniteSet, finiteSet)
import Markovian.Category.Matrix (matrixFromRows)
import Markovian.Category.Matrix.Stochastic (StochasticMatrix, stochasticMatrix)
import Markovian.Feedback.Channel.Exact
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

main :: IO ()
main = do
    fixture <- buildFixture 16
    _ <- runOnce fixture
    samples <- replicateM 20 (runOnce fixture)
    let reports = map snd samples
        times = map fst samples
    if all (== head reports) (tail reports)
        then pure ()
        else fail "feedback semantic report changed between samples"
    putStrLn "markovian feedback exact benchmark"
    putStrLn "warmups: 1 (excluded)"
    putStrLn "samples: 20"
    putStrLn ("raw-seconds: " ++ intercalate "," (map (printf "%.9f") times))
    printf "mean-seconds: %.9f\n" (mean times)
    printf "sample-deviation-seconds: %.9f\n" (sampleDeviation times)
    putStrLn ("semantic-report: " ++ head reports)

runOnce :: Fixture -> IO (Double, String)
runOnce (Fixture limits inputs loops outputs routing) = do
    start <- getCPUTime
    checked <- either (fail . show) pure (closeProperFeedback limits inputs (loopLayout "benchmark" loops) outputs routing)
    let rendered = show (feedbackReport checked)
    _ <- evaluate (length rendered)
    end <- getCPUTime
    pure (fromIntegral (end - start) / 1.0e12, rendered)

mean :: [Double] -> Double
mean values = sum values / fromIntegral (length values)

sampleDeviation :: [Double] -> Double
sampleDeviation values =
    sqrt (sum [(value - average) ^ (2 :: Int) | value <- values] / fromIntegral (length values - 1))
  where
    average = mean values

data Input = Input deriving (Eq, Show)
newtype Loop = Loop Int deriving (Eq, Show)
data Output = Output deriving (Eq, Show)

data Fixture
    = Fixture
        FeedbackLimits
        (FiniteSet Input)
        (FiniteSet Loop)
        (FiniteSet Output)
        (StochasticMatrix NonNegativeRational (Either Input Loop) (Either Output Loop))

buildFixture :: Int -> IO Fixture
buildFixture size = do
    inputs <- either (fail . show) pure (finiteSet [Input])
    loops <- either (fail . show) pure (finiteSet (map Loop [0 .. size - 1]))
    outputs <- either (fail . show) pure (finiteSet [Output])
    sources <- either (fail . show) pure (finiteSet (Left Input : map (Right . Loop) [0 .. size - 1]))
    targets <- either (fail . show) pure (finiteSet (Left Output : map (Right . Loop) [0 .. size - 1]))
    let rows = externalRow : [loopRow index | index <- [0 .. size - 1]]
        externalRow = 0 : 1 : replicate (size - 1) 0
        loopRow index
            | index == size - 1 = 1 : replicate size 0
            | otherwise = 0 : replicate index 0 ++ [0, 1] ++ replicate (size - index - 2) 0
    raw <- either (fail . show) pure (matrixFromRows sources targets (map (map scalar) rows))
    routing <- either (fail . show) pure (stochasticMatrix raw)
    pure (Fixture (feedbackLimits 32 32 32 100 10000 10000 10000000 100000 8192) inputs loops outputs routing)

scalar value = either (error . show) id (nonNegativeRational value)
