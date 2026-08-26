{- | Exact finite probabilities for reference interpreters and literal law tests.

This module uses 'Rational'. It does not share representation or equality with
the floating values in "Markovian.Probability".
-}
module Markovian.Probability.Exact (
    ExactProb,
    ExactProbabilityError (..),
    mkExactProb,
    exactProbability,
    ExactWeight,
    ExactWeightError (..),
    mkExactWeight,
    exactWeight,
    ExactFiniteDist,
    ExactDistributionError (..),
    exactFiniteDist,
    exactDirac,
    bindExactFiniteDist,
    exactOutcomes,
) where

import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty

-- Preserve tuple-pattern strictness at validated representation boundaries.
{-# ANN module ("HLint: ignore Use first" :: String) #-}

-- | An exact probability in the closed interval @[0, 1]@.
newtype ExactProb = ExactProb Rational
    deriving (Eq, Ord, Show)

-- | The supplied rational is outside the closed unit interval.
newtype ExactProbabilityError = ExactProbabilityOutOfRange Rational
    deriving (Eq, Show)

-- | Validate an exact probability.
mkExactProb :: Rational -> Either ExactProbabilityError ExactProb
mkExactProb value
    | value < 0 || value > 1 = Left (ExactProbabilityOutOfRange value)
    | otherwise = Right (ExactProb value)

-- | Read the validated exact probability.
exactProbability :: ExactProb -> Rational
exactProbability (ExactProb value) = value

-- | An exact, nonnegative, unnormalized weight.
newtype ExactWeight = ExactWeight Rational
    deriving (Eq, Ord, Show)

-- | The supplied rational is negative.
newtype ExactWeightError = NegativeExactWeight Rational
    deriving (Eq, Show)

{- | Validate an exact weight. Zero is valid by itself, but an exact finite
distribution must contain positive total weight.
-}
mkExactWeight :: Rational -> Either ExactWeightError ExactWeight
mkExactWeight value
    | value < 0 = Left (NegativeExactWeight value)
    | otherwise = Right (ExactWeight value)

-- | Read the validated exact weight.
exactWeight :: ExactWeight -> Rational
exactWeight (ExactWeight value) = value

{- | A nonempty finite distribution with exact normalized probabilities.

The constructor is not exported. Distinct labeled entries remain distinct, even
when their values compare equal. Zero-weight entries are removed.
-}
newtype ExactFiniteDist a = UnsafeExactFiniteDist (NonEmpty (a, ExactProb))
    deriving (Eq, Show)

instance Functor ExactFiniteDist where
    fmap f (UnsafeExactFiniteDist entries) =
        UnsafeExactFiniteDist (fmap (\(value, mass) -> (f value, mass)) entries)

-- | Errors returned by 'exactFiniteDist'.
data ExactDistributionError
    = -- | The supplied support is empty.
      EmptyExactSupport
    | -- | A weight at the supplied zero-based index is invalid.
      InvalidExactWeight !Integer !ExactWeightError
    | -- | Every supplied weight is zero.
      ZeroExactTotalWeight
    deriving (Eq, Show)

-- | Construct an exact distribution from labeled values and rational weights.
exactFiniteDist :: [(a, Rational)] -> Either ExactDistributionError (ExactFiniteDist a)
exactFiniteDist [] = Left EmptyExactSupport
exactFiniteDist entries = do
    validated <- traverse validate (zip [0 ..] entries)
    case NonEmpty.nonEmpty (filter ((> 0) . exactWeight . snd) validated) of
        Nothing -> Left ZeroExactTotalWeight
        Just positiveEntries ->
            let total = foldl' (\accumulator (_, mass) -> accumulator + exactWeight mass) 0 positiveEntries
                normalized = fmap (\(value, mass) -> (value, ExactProb (exactWeight mass / total))) positiveEntries
             in Right (UnsafeExactFiniteDist normalized)
  where
    validate (index, (value, rawWeight)) =
        case mkExactWeight rawWeight of
            Left err -> Left (InvalidExactWeight index err)
            Right validWeight -> Right (value, validWeight)

-- | Construct an exact distribution with one certain outcome.
exactDirac :: a -> ExactFiniteDist a
exactDirac value = UnsafeExactFiniteDist ((value, ExactProb 1) NonEmpty.:| [])

{- | Sequence exact stochastic choices.

The result preserves the left-to-right support order and labeled duplicates.
Exact multiplication preserves positive mass and literal normalization.
-}
bindExactFiniteDist :: ExactFiniteDist a -> (a -> ExactFiniteDist b) -> ExactFiniteDist b
bindExactFiniteDist (UnsafeExactFiniteDist entries) continuation =
    UnsafeExactFiniteDist (entries >>= expand)
  where
    expand (value, ExactProb outerMass) =
        fmap
            (\(nextValue, ExactProb innerMass) -> (nextValue, ExactProb (outerMass * innerMass)))
            (exactOutcomes (continuation value))

-- | Read all positive-mass exact outcomes in support order.
exactOutcomes :: ExactFiniteDist a -> NonEmpty (a, ExactProb)
exactOutcomes (UnsafeExactFiniteDist entries) = entries
