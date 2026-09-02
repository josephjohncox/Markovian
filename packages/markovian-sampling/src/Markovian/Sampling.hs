{- | Explicit deterministic generators and unbiased finite-support sampling.

Sampling treats each exposed floating mass as its exact binary rational value
and renormalizes those values exactly. This keeps every positive exposed entry
reachable instead of truncating support to a fixed-width unit interval.
-}
module Markovian.Sampling (
    Generator,
    generatorFromSeed,
    generatorState,
    SamplingError (..),
    sampleFiniteDist,
    sampleExactFiniteDist,
) where

import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio (denominator, numerator)
import Data.Word (Word64)
import Markovian.Probability (FiniteDist, outcomes, probability)
import Markovian.Probability.Exact (ExactFiniteDist, exactOutcomes, exactProbability)

-- | The complete state of the deterministic SplitMix64 generator.
newtype Generator = Generator Word64
    deriving (Eq, Show)

-- | Construct a generator from any 64-bit seed, including zero.
generatorFromSeed :: Word64 -> Generator
generatorFromSeed = Generator

-- | Read the generator state needed to resume the stream exactly.
generatorState :: Generator -> Word64
generatorState (Generator state) = state

-- | Representation-invariant failures detected at the sampling boundary.
data SamplingError
    = -- | A floating support entry was not finite and strictly positive.
      InvalidFloatingSamplingMass !Integer !Double
    | -- | An exact support entry was not strictly positive.
      InvalidExactSamplingMass !Integer !Rational
    | -- | Validated support unexpectedly had nonpositive total mass.
      NonPositiveSamplingTotal !Rational
    deriving (Eq, Show)

{- | Sample one floating finite distribution.

The returned generator owns all unused stream state. A one-point distribution
does not consume generator state because its result contains no randomness.
-}
sampleFiniteDist :: Generator -> FiniteDist value -> Either SamplingError (value, Generator)
sampleFiniteDist generator distribution = do
    weighted <- traverse validateFloating (NonEmpty.zip (0 :| [1 ..]) (outcomes distribution))
    chooseWeighted generator weighted
  where
    validateFloating (index, (value, mass)) =
        let raw = probability mass
         in if isNaN raw || isInfinite raw || raw <= 0
                then Left (InvalidFloatingSamplingMass index raw)
                else Right (value, toRational raw)

-- | Sample one exact finite distribution without converting through floating point.
sampleExactFiniteDist :: Generator -> ExactFiniteDist value -> Either SamplingError (value, Generator)
sampleExactFiniteDist generator distribution = do
    weighted <- traverse validateExact (NonEmpty.zip (0 :| [1 ..]) (exactOutcomes distribution))
    chooseWeighted generator weighted
  where
    validateExact (index, (value, mass)) =
        let raw = exactProbability mass
         in if raw <= 0
                then Left (InvalidExactSamplingMass index raw)
                else Right (value, raw)

chooseWeighted :: Generator -> NonEmpty (value, Rational) -> Either SamplingError (value, Generator)
chooseWeighted generator weighted
    | total <= 0 = Left (NonPositiveSamplingTotal total)
    | otherwise =
        let commonDenominator = foldr (lcm . denominator . snd) 1 weighted
            integerWeights = fmap (toIntegerWeight commonDenominator) weighted
            integerTotal = sum (fmap snd integerWeights)
            (target, nextGenerator) = uniformBelow integerTotal generator
         in Right (select target integerWeights, nextGenerator)
  where
    total = sum (fmap snd weighted)

    toIntegerWeight commonDenominator (value, mass) =
        (value, numerator mass * (commonDenominator `div` denominator mass))

select :: Integer -> NonEmpty (value, Integer) -> value
select target ((firstValue, firstMass) :| remaining) = go firstValue firstMass remaining
  where
    go fallback _ [] = fallback
    go fallback cumulative ((value, mass) : rest)
        | target < cumulative = fallback
        | target < cumulative + mass = value
        | otherwise = go value (cumulative + mass) rest

uniformBelow :: Integer -> Generator -> (Integer, Generator)
uniformBelow bound generator
    | bound <= 1 = (0, generator)
    | otherwise = draw generator
  where
    bits = bitLength (bound - 1)
    mask = (1 `shiftL` bits) - 1

    draw current =
        let (raw, next) = drawBits bits current
            candidate = raw .&. mask
         in if candidate < bound then (candidate, next) else draw next

drawBits :: Int -> Generator -> (Integer, Generator)
drawBits requested = go requested 0 0
  where
    go remaining offset accumulator generator
        | remaining <= 0 = (accumulator, generator)
        | otherwise =
            let (word, next) = nextWord64 generator
                takeCount = min 64 remaining
                wordMask = (1 `shiftL` takeCount) - 1
                chunk = toInteger word .&. wordMask
             in go
                    (remaining - takeCount)
                    (offset + takeCount)
                    (accumulator .|. (chunk `shiftL` offset))
                    next

bitLength :: Integer -> Int
bitLength = go 0
  where
    go count value
        | value <= 0 = count
        | otherwise = go (count + 1) (value `shiftR` 1)

nextWord64 :: Generator -> (Word64, Generator)
nextWord64 (Generator state) = (mix64 nextState, Generator nextState)
  where
    nextState = state + 0x9e3779b97f4a7c15

mix64 :: Word64 -> Word64
mix64 input = third `xor` (third `shiftR` 31)
  where
    first = (input `xor` (input `shiftR` 30)) * 0xbf58476d1ce4e5b9
    second = (first `xor` (first `shiftR` 27)) * 0x94d049bb133111eb
    third = second
