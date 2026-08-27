{- | Lawful scalar contracts used by the exact finite matrix layers.

Instances must satisfy the usual semiring laws. 'CommutativeSemiring' adds
commutative multiplication. 'InvolutiveSemiring' is deliberately independent
of commutativity: its involution preserves zero, one, and addition, reverses
product order, and is involutive. 'ExactNonNegativeSemifield' additionally has
decidable zero and positivity, division by nonzero values, zero-sum-freeness,
and no zero divisors. None of these classes claims additive inverses.
-}
module Markovian.Algebra.Semiring (
    Semiring (..),
    CommutativeSemiring,
    InvolutiveSemiring (..),
    ExactNonNegativeSemifield (..),
    ConvexScalar,
    validConvexCoefficients,
) where

import Data.List.NonEmpty (NonEmpty)

{- | A semiring with a commutative additive monoid and a multiplicative monoid.

Multiplication distributes over addition on both sides. Zero annihilates
multiplication on both sides. These laws do not require additive inverses or
commutative multiplication.
-}
class (Eq scalar) => Semiring scalar where
    zero :: scalar
    one :: scalar
    plus :: scalar -> scalar -> scalar
    times :: scalar -> scalar -> scalar

-- | A semiring whose multiplication is commutative.
class (Semiring scalar) => CommutativeSemiring scalar

{- | A semiring with an involution that reverses product order.

Laws include @involute zero = zero@, @involute one = one@,
@involute (a `plus` b) = involute a `plus` involute b@,
@involute (a `times` b) = involute b `times` involute a@, and
@involute (involute a) = a@.
-}
class (Semiring scalar) => InvolutiveSemiring scalar where
    involute :: scalar -> scalar

{- | An exact nonnegative commutative semifield.

Zero and one are distinct. @isZero x@ is equivalent to @x == zero@.
@isPositive x@ is equivalent to @not (isZero x)@. Addition is zero-sum-free:
@isZero (a `plus` b)@ implies @isZero a@ and @isZero b@. Multiplication has no
zero divisors: @isZero (a `times` b)@ implies @isZero a@ or @isZero b@.

'divideNonZero' returns 'Nothing' exactly when its denominator is zero. For a
nonzero denominator @b@, a result @q@ satisfies @q `times` b == a@.
-}
class (CommutativeSemiring scalar) => ExactNonNegativeSemifield scalar where
    isZero :: scalar -> Bool
    isPositive :: scalar -> Bool
    divideNonZero :: scalar -> scalar -> Maybe scalar

-- | Exact nonnegative scalars that support finite convex combinations.
class (ExactNonNegativeSemifield scalar) => ConvexScalar scalar

{- | Check whether a nonempty coefficient family sums exactly to one.

This operation is fixed by the class laws. Instances cannot replace the check
with a weaker predicate.
-}
validConvexCoefficients :: (ConvexScalar scalar) => NonEmpty scalar -> Bool
validConvexCoefficients coefficients = foldr plus zero coefficients == one
