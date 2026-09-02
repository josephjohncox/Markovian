{-# LANGUAGE BangPatterns #-}

{- | Bounded resumable Monte Carlo with explicit generator state and Welford
moments. Standard error is descriptive; it is not a deterministic bound.
-}
module Markovian.Continuous.Numerical.MonteCarlo (
    MonteCarloConfig,
    monteCarloConfig,
    MonteCarloState,
    MonteCarloReport,
    MonteCarloError (..),
    startMonteCarlo,
    resumeMonteCarlo,
    estimateExpectation,
    monteCarloEstimate,
    monteCarloSampleVariance,
    monteCarloStandardError,
    monteCarloSamples,
    monteCarloInitialGenerator,
    monteCarloFinalGenerator,
) where

import Control.Monad (when)
import Markovian.Continuous.Numerical.Generator
import Numeric.Natural (Natural)

-- | A requested sample count and an operation budget.
data MonteCarloConfig = MonteCarloConfig Natural Natural
    deriving stock (Eq, Show)

-- | Opaque resumable Welford state with an owned generator.
data MonteCarloState = MonteCarloState Natural Double Double GeneratorState Generator
    deriving stock (Eq, Show)

-- | Mean, optional variance, optional standard error, and generator endpoints.
data MonteCarloReport = MonteCarloReport Double (Maybe Double) (Maybe Double) Natural GeneratorState GeneratorState
    deriving stock (Eq, Show)

-- | A budget, sampling, observation, or accumulator failure.
data MonteCarloError
    = ZeroSamples
    | SampleBudgetExceeded Natural Natural
    | MonteCarloSamplingError Natural SamplingError
    | ObservationFailure Natural String
    | NonFiniteObservation Natural Double
    | NonFiniteAccumulator Natural
    deriving stock (Eq, Show)

-- | Validate a positive requested count against its budget.
monteCarloConfig :: Natural -> Natural -> Either MonteCarloError MonteCarloConfig
monteCarloConfig requested budget
    | requested == 0 = Left ZeroSamples
    | requested > budget = Left (SampleBudgetExceeded requested budget)
    | otherwise = Right (MonteCarloConfig requested budget)

-- | Start an empty accumulator without drawing a sample.
startMonteCarlo :: Generator -> MonteCarloState
startMonteCarlo generator = MonteCarloState 0 0 0 (generatorState generator) generator

-- | Add the configured sample count to an existing state.
resumeMonteCarlo :: MonteCarloConfig -> NumericalLaw -> (Double -> Either String Double) -> MonteCarloState -> Either MonteCarloError (MonteCarloReport, MonteCarloState)
resumeMonteCarlo (MonteCarloConfig requested budget) law observe state
    | requested > budget = Left (SampleBudgetExceeded requested budget)
    | otherwise = do
        finalState <- go requested state
        pure (report finalState, finalState)
  where
    go 0 current = Right current
    go remaining (MonteCarloState count mean m2 initial generator) = do
        (sample, nextGenerator) <- mapLeft (MonteCarloSamplingError count) (sampleLaw law generator)
        observation <- mapLeft (ObservationFailure count) (observe sample)
        when (isNaN observation || isInfinite observation) (Left (NonFiniteObservation count observation))
        let !nextCount = count + 1
            !delta = observation - mean
            !nextMean = mean + delta / fromIntegral nextCount
            !nextM2 = m2 + delta * (observation - nextMean)
        if any (\value -> isNaN value || isInfinite value) [nextMean, nextM2]
            then Left (NonFiniteAccumulator nextCount)
            else go (remaining - 1) (MonteCarloState nextCount nextMean nextM2 initial nextGenerator)

    report (MonteCarloState count mean m2 initial generator) =
        let variance = if count > 1 then Just (m2 / fromIntegral (count - 1)) else Nothing
            standardError = sqrt . (/ fromIntegral count) <$> variance
         in MonteCarloReport mean variance standardError count initial (generatorState generator)

-- | Run one bounded expectation from a supplied generator.
estimateExpectation :: MonteCarloConfig -> NumericalLaw -> (Double -> Either String Double) -> Generator -> Either MonteCarloError (MonteCarloReport, Generator)
estimateExpectation config law observe generator = do
    (result, MonteCarloState _ _ _ _ finalGenerator) <- resumeMonteCarlo config law observe (startMonteCarlo generator)
    pure (result, finalGenerator)

-- | Get the sample mean.
monteCarloEstimate :: MonteCarloReport -> Double
monteCarloEstimate (MonteCarloReport value _ _ _ _ _) = value

-- | Get unbiased sample variance when at least two samples exist.
monteCarloSampleVariance :: MonteCarloReport -> Maybe Double
monteCarloSampleVariance (MonteCarloReport _ value _ _ _ _) = value

-- | Get estimated standard error when variance exists.
monteCarloStandardError :: MonteCarloReport -> Maybe Double
monteCarloStandardError (MonteCarloReport _ _ value _ _ _) = value

-- | Get the cumulative sample count.
monteCarloSamples :: MonteCarloReport -> Natural
monteCarloSamples (MonteCarloReport _ _ _ value _ _) = value

-- | Get the initial generator state.
monteCarloInitialGenerator :: MonteCarloReport -> GeneratorState
monteCarloInitialGenerator (MonteCarloReport _ _ _ _ value _) = value

-- | Get the final generator state.
monteCarloFinalGenerator :: MonteCarloReport -> GeneratorState
monteCarloFinalGenerator (MonteCarloReport _ _ _ _ _ value) = value

mapLeft :: (left -> other) -> Either left right -> Either other right
mapLeft function value = case value of
    Left err -> Left (function err)
    Right result -> Right result
