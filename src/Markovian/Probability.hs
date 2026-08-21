{- | Validated floating probabilities and finite distributions.

Construction is fail-fast. A 'FiniteDist' preserves distinct support entries,
even when their values compare equal. Entries with zero representable mass
are removed.
-}
module Markovian.Probability (
    Prob,
    ProbabilityError (..),
    mkProb,
    probability,
    Weight,
    WeightError (..),
    mkWeight,
    weight,
    FiniteDist,
    DistributionError (..),
    finiteDist,
    dirac,
    outcomes,
)
where

import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty

-- | A finite probability in the closed interval @[0, 1]@.
newtype Prob = Prob Double
    deriving (Eq, Show)

-- | Errors returned by 'mkProb'.
data ProbabilityError
    = NonFiniteProbability !Double
    | ProbabilityOutOfRange !Double
    deriving (Eq, Show)

-- | Validate a floating probability.
mkProb :: Double -> Either ProbabilityError Prob
mkProb value
    | not (finite value) = Left (NonFiniteProbability value)
    | value < 0 || value > 1 = Left (ProbabilityOutOfRange value)
    | otherwise = Right (Prob value)

-- | Read the validated floating probability.
probability :: Prob -> Double
probability (Prob value) = value

-- | A finite, nonnegative, unnormalized weight.
newtype Weight = Weight Double
    deriving (Eq, Show)

-- | Errors returned by 'mkWeight'.
data WeightError
    = NonFiniteWeight !Double
    | NegativeWeight !Double
    deriving (Eq, Show)

{- | Validate an unnormalized floating weight. Zero is valid by itself, but a
finite distribution must contain positive total weight.
-}
mkWeight :: Double -> Either WeightError Weight
mkWeight value
    | not (finite value) = Left (NonFiniteWeight value)
    | value < 0 = Left (NegativeWeight value)
    | otherwise = Right (Weight value)

-- | Read the validated floating weight.
weight :: Weight -> Double
weight (Weight value) = value

{- | A nonempty finite distribution with normalized floating probabilities.

The constructor is not exported. Use 'finiteDist' or 'dirac'.
-}
newtype FiniteDist a = UnsafeFiniteDist (NonEmpty (a, Prob))
    deriving (Show)

instance Functor FiniteDist where
    fmap f (UnsafeFiniteDist entries) =
        UnsafeFiniteDist (fmap (\(value, mass) -> (f value, mass)) entries)

-- | Errors returned while constructing a finite distribution with 'finiteDist'.
data DistributionError
    = EmptySupport
    | InvalidWeight !Int !WeightError
    | ZeroTotalWeight
    | InvalidScaledTotal !Double
    deriving (Eq, Show)

{- | Construct a finite distribution from labeled values and floating weights.

Positive weights are divided by the largest weight before summation. This
prevents overflow when, for example, two weights are both the largest finite
'Double'. An entry is removed if its normalized mass rounds to zero.
Validation stops at the first invalid input.
-}
finiteDist :: [(a, Double)] -> Either DistributionError (FiniteDist a)
finiteDist [] = Left EmptySupport
finiteDist entries = do
    validated <- traverse validate (zip [0 ..] entries)
    case NonEmpty.nonEmpty (filter ((> 0) . weight . snd) validated) of
        Nothing -> Left ZeroTotalWeight
        Just positiveEntries -> normalize positiveEntries
  where
    validate (index, (value, rawWeight)) =
        case mkWeight rawWeight of
            Left err -> Left (InvalidWeight index err)
            Right validWeight -> Right (value, validWeight)

    normalize positiveEntries =
        let largest = maximum (fmap (weight . snd) positiveEntries)
            scaled = fmap (\(value, validWeight) -> (value, weight validWeight / largest)) positiveEntries
            scaledTotal = foldl' (\total (_, mass) -> total + mass) 0 scaled
            normalized = fmap (\(value, mass) -> (value, mass / scaledTotal)) scaled
            representable = filter ((> 0) . snd) (NonEmpty.toList normalized)
         in if scaledTotal <= 0 || not (finite scaledTotal)
                then Left (InvalidScaledTotal scaledTotal)
                else case NonEmpty.nonEmpty representable of
                    Nothing -> Left (InvalidScaledTotal scaledTotal)
                    Just positiveMass ->
                        Right
                            ( UnsafeFiniteDist
                                (fmap (\(value, mass) -> (value, Prob mass)) positiveMass)
                            )

-- | Construct a distribution with one certain outcome.
dirac :: a -> FiniteDist a
dirac value = UnsafeFiniteDist ((value, Prob 1) NonEmpty.:| [])

{- | Read all positive-mass outcomes. Support order is retained but is not part
of the distribution's mathematical meaning.
-}
outcomes :: FiniteDist a -> NonEmpty (a, Prob)
outcomes (UnsafeFiniteDist entries) = entries

finite :: Double -> Bool
finite value = not (isNaN value || isInfinite value)
