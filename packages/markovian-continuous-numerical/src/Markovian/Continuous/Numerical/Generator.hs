{-# LANGUAGE BangPatterns #-}

-- | Explicit SplitMix64 generator ownership and named real laws.
module Markovian.Continuous.Numerical.Generator (
    Generator,
    GeneratorState,
    generatorFromSeed,
    generatorState,
    generatorStateWords,
    resumeGenerator,
    nextGeneratorWord,
    NumericalLaw,
    uniformLaw,
    normalLaw,
    exponentialLaw,
    SamplingError (..),
    densityAt,
    sampleLaw,
) where

import Data.Bits (shiftR, xor)
import Data.Word (Word64)
import Markovian.Continuous.Numerical.Value

-- | An opaque SplitMix64 generator.
data Generator = Generator !Word64 !Word64
    deriving stock (Eq, Show)

-- | An opaque resumable generator state.
data GeneratorState = GeneratorState !Word64 !Word64
    deriving stock (Eq, Show)

-- | A checked named one-dimensional law.
data NumericalLaw
    = UniformLaw !Double !Double
    | NormalLaw !Double !Double
    | ExponentialLaw !Double
    deriving stock (Eq, Show)

-- | A parameter or nonfinite sampling failure.
data SamplingError
    = SamplingValueError NumericalValueError
    | NonFiniteSample
    deriving stock (Eq, Show)

-- | Start a generator from one stable seed.
generatorFromSeed :: Word64 -> Generator
generatorFromSeed seed = Generator seed 0x9e3779b97f4a7c15

-- | Get the current opaque generator state.
generatorState :: Generator -> GeneratorState
generatorState (Generator seed gamma) = GeneratorState seed gamma

-- | Get the two stable SplitMix64 state words for known-answer evidence.
generatorStateWords :: GeneratorState -> (Word64, Word64)
generatorStateWords (GeneratorState seed gamma) = (seed, gamma)

-- | Resume a generator from an opaque state.
resumeGenerator :: GeneratorState -> Generator
resumeGenerator (GeneratorState seed gamma) = Generator seed gamma

-- | Validate a uniform law on a finite interval.
uniformLaw :: Double -> Double -> Either SamplingError NumericalLaw
uniformLaw lower upper = case compactDoubleInterval lower upper of
    Left err -> Left (SamplingValueError err)
    Right _ -> Right (UniformLaw lower upper)

-- | Validate a normal law with positive deviation.
normalLaw :: Double -> Double -> Either SamplingError NumericalLaw
normalLaw mean deviation = do
    checkedMean <- mapLeft SamplingValueError (finiteDouble mean)
    checkedDeviation <- mapLeft SamplingValueError (positiveFinite deviation)
    pure (NormalLaw (finiteValue checkedMean) (positiveValue checkedDeviation))

-- | Validate an exponential law with positive rate.
exponentialLaw :: Double -> Either SamplingError NumericalLaw
exponentialLaw rate = ExponentialLaw . positiveValue <$> mapLeft SamplingValueError (positiveFinite rate)

-- | Evaluate the named law's Lebesgue density at a finite point.
densityAt :: NumericalLaw -> Double -> Either SamplingError Double
densityAt law point = do
    checkedPoint <- mapLeft SamplingValueError (finiteDouble point)
    let x = finiteValue checkedPoint
        result = case law of
            UniformLaw lower upper -> if x < lower || x > upper then 0 else 1 / (upper - lower)
            NormalLaw mean deviation -> exp (-(0.5 * ((x - mean) / deviation) ^ (2 :: Int))) / (deviation * sqrt (2 * pi))
            ExponentialLaw rate -> if x < 0 then 0 else rate * exp (-(rate * x))
    if isNaN result || isInfinite result then Left NonFiniteSample else Right result

-- | Draw one value and return the successor generator.
sampleLaw :: NumericalLaw -> Generator -> Either SamplingError (Double, Generator)
sampleLaw law generator =
    case law of
        UniformLaw lower upper ->
            let (unit, next) = nextUnit generator
             in checked (lower + (upper - lower) * unit) next
        NormalLaw mean deviation ->
            let (first, next1) = nextOpenUnit generator
                (second, next2) = nextUnit next1
                standard = sqrt (-(2 * log first)) * cos (2 * pi * second)
             in checked (mean + deviation * standard) next2
        ExponentialLaw rate ->
            let (unit, next) = nextOpenUnit generator
             in checked ((-log unit) / rate) next
  where
    checked value next
        | isNaN value || isInfinite value = Left NonFiniteSample
        | otherwise = Right (if value == 0 then 0 else value, next)

{- | Draw one raw SplitMix64 word. This supports pinned known-answer tests;
distribution samplers remain the preferred numerical API.
-}
nextGeneratorWord :: Generator -> (Word64, Generator)
nextGeneratorWord = nextWord64

nextWord64 :: Generator -> (Word64, Generator)
nextWord64 (Generator seed gamma) =
    let !nextSeed = seed + gamma
     in (mix64 nextSeed, Generator nextSeed gamma)

nextUnit :: Generator -> (Double, Generator)
nextUnit generator =
    let (word, next) = nextWord64 generator
        value = fromIntegral (word `shiftR` 11) * (1 / 9007199254740992)
     in (value, next)

nextOpenUnit :: Generator -> (Double, Generator)
nextOpenUnit generator =
    let (word, next) = nextWord64 generator
        value = (fromIntegral (word `shiftR` 11) + 0.5) * (1 / 9007199254740992)
     in (value, next)

mix64 :: Word64 -> Word64
mix64 input =
    let first = (input `xor` (input `shiftR` 30)) * 0xbf58476d1ce4e5b9
        second = (first `xor` (first `shiftR` 27)) * 0x94d049bb133111eb
     in second `xor` (second `shiftR` 31)

mapLeft :: (left -> other) -> Either left right -> Either other right
mapLeft function value = case value of
    Left err -> Left (function err)
    Right result -> Right result
