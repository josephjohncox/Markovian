{- | Exact nonnegative rational scalars.

Unlike a probability, a 'NonNegativeRational' can exceed one. Unlike an exact
finite distribution, it has extensional scalar equality and no support
representation.
-}
module Markovian.Algebra.NonNegativeRational (
    NonNegativeRational,
    NonNegativeRationalError (..),
    nonNegativeRational,
    getNonNegativeRational,
) where

import Markovian.Algebra.Semiring

-- | A rational known to be nonnegative.
newtype NonNegativeRational = UnsafeNonNegativeRational Rational
    deriving (Eq, Ord, Show)

-- | Failure to construct a nonnegative rational.
newtype NonNegativeRationalError = NegativeRational Rational
    deriving (Eq, Show)

-- | Validate an exact nonnegative rational.
nonNegativeRational :: Rational -> Either NonNegativeRationalError NonNegativeRational
nonNegativeRational value
    | value < 0 = Left (NegativeRational value)
    | otherwise = Right (UnsafeNonNegativeRational value)

-- | Read the represented rational.
getNonNegativeRational :: NonNegativeRational -> Rational
getNonNegativeRational (UnsafeNonNegativeRational value) = value

instance Semiring NonNegativeRational where
    zero = UnsafeNonNegativeRational 0
    one = UnsafeNonNegativeRational 1
    UnsafeNonNegativeRational left `plus` UnsafeNonNegativeRational right =
        UnsafeNonNegativeRational (left + right)
    UnsafeNonNegativeRational left `times` UnsafeNonNegativeRational right =
        UnsafeNonNegativeRational (left * right)

instance CommutativeSemiring NonNegativeRational

instance InvolutiveSemiring NonNegativeRational where
    involute = id

instance ExactNonNegativeSemifield NonNegativeRational where
    isZero (UnsafeNonNegativeRational value) = value == 0
    isPositive (UnsafeNonNegativeRational value) = value > 0
    divideNonZero _ (UnsafeNonNegativeRational 0) = Nothing
    divideNonZero (UnsafeNonNegativeRational numerator) (UnsafeNonNegativeRational denominator) =
        Just (UnsafeNonNegativeRational (numerator / denominator))

instance ConvexScalar NonNegativeRational
