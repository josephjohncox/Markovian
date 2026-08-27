{-# LANGUAGE RoleAnnotations #-}

{- | Exact convex enrichment of finite stochastic matrices.

Raw matrix addition and convex mixture are separate operations. A convex family
is nonempty and its exact nonnegative coefficients sum to one.
-}
module Markovian.Category.Convex.Exact (
    ConvexFamily,
    ConvexFamilyError (..),
    convexFamily,
    convexTerms,
    convexMixture,
) where

import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Algebra.Semiring
import Markovian.Category.Finite.Set (sameFiniteSet)
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Stochastic
import Markovian.Category.Matrix.Stochastic.Internal (StochasticMatrix (UnsafeStochasticMatrix))

-- | A checked nonempty exact convex family of arrows with common objects.
type role ConvexFamily nominal nominal nominal

newtype ConvexFamily scalar source target
    = UnsafeConvexFamily (NonEmpty (scalar, StochasticMatrix scalar source target))

-- | Convex coefficient or represented-object mismatch.
data ConvexFamilyError
    = InvalidConvexCoefficients
    | ConvexSourceObjectMismatch !Int
    | ConvexTargetObjectMismatch !Int
    deriving (Eq, Show)

-- | Validate coefficients and common semantic endpoint supports.
convexFamily ::
    (ConvexScalar scalar) =>
    NonEmpty (scalar, StochasticMatrix scalar source target) ->
    Either ConvexFamilyError (ConvexFamily scalar source target)
convexFamily terms@((_, first) :| remaining)
    | not (validConvexCoefficients (fmap fst terms)) = Left InvalidConvexCoefficients
    | otherwise = checkObjects 1 remaining
  where
    checkObjects _ [] = Right (UnsafeConvexFamily terms)
    checkObjects index ((_, arrow) : rest)
        | not (sameFiniteSet (stochasticSource first) (stochasticSource arrow)) =
            Left (ConvexSourceObjectMismatch index)
        | not (sameFiniteSet (stochasticTarget first) (stochasticTarget arrow)) =
            Left (ConvexTargetObjectMismatch index)
        | otherwise = checkObjects (index + 1) rest

-- | Read terms in represented family order.
convexTerms ::
    ConvexFamily scalar source target ->
    NonEmpty (scalar, StochasticMatrix scalar source target)
convexTerms (UnsafeConvexFamily terms) = terms

{- | Evaluate an exact convex family. Row normalization follows from the checked
coefficient sum and normalization of every member.
-}
convexMixture ::
    (ConvexScalar scalar) =>
    ConvexFamily scalar source target ->
    StochasticMatrix scalar source target
convexMixture family =
    UnsafeStochasticMatrix $
        matrixFromFunction source target $ \sourceValue targetValue ->
            foldl'
                plus
                zero
                [ coefficient `times` entry
                | (coefficient, arrow) <- NonEmpty.toList terms
                , Just entry <- [matrixEntry (forgetStochastic arrow) sourceValue targetValue]
                ]
  where
    terms@((_, first) :| _) = convexTerms family
    source = stochasticSource first
    target = stochasticTarget first
