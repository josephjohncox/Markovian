{- | Explicit boundary for floating, GPU, and other approximate circuit backends.

This interface is deliberately separate from the exact circuit algebra.
An approximate backend must state its observation relation, precision, and
error policy. Possession of this value does not establish exact categorical
laws.
-}
module Markovian.Circuit.Interpret.Approximate (
    ApproximationErrorPolicy (..),
    ApproximationBoundaryError (..),
    ApproximateInterpreterBoundary,
    approximateInterpreterBoundary,
    approximationPrecisionBits,
    approximationErrorPolicy,
    approximatelyObserves,
) where

import Numeric.Natural (Natural)

-- | Declared numerical error policy for an approximate backend.
data ApproximationErrorPolicy
    = AbsoluteRationalTolerance !Rational
    | RelativeRationalTolerance !Rational
    | BackendDocumentedError !String
    deriving (Eq, Show)

-- | Invalid precision or error policy.
data ApproximationBoundaryError
    = ApproximationPrecisionMustBePositive
    | ApproximationToleranceMustBeNonnegative
    | ApproximationBackendPolicyMustBeDocumented
    deriving (Eq, Show)

{- | Observation contract between exact and approximate backend results.

The relation is supplied by the backend and is not ordinary equality. Precision
is stated in bits. Backends with data-dependent precision must choose a lower
bound and document the remaining policy in 'approximationErrorPolicy'.
-}
data ApproximateInterpreterBoundary exact approximate
    = ApproximateInterpreterBoundary
        !Natural
        !ApproximationErrorPolicy
        (exact -> approximate -> Bool)

-- | Validate an approximate backend boundary.
approximateInterpreterBoundary ::
    Natural ->
    ApproximationErrorPolicy ->
    (exact -> approximate -> Bool) ->
    Either ApproximationBoundaryError (ApproximateInterpreterBoundary exact approximate)
approximateInterpreterBoundary precision policy relation
    | precision == 0 = Left ApproximationPrecisionMustBePositive
    | not (validPolicy policy) =
        case policy of
            BackendDocumentedError _ -> Left ApproximationBackendPolicyMustBeDocumented
            _ -> Left ApproximationToleranceMustBeNonnegative
    | otherwise = Right (ApproximateInterpreterBoundary precision policy relation)

-- | Read the declared lower-bound precision in bits.
approximationPrecisionBits :: ApproximateInterpreterBoundary exact approximate -> Natural
approximationPrecisionBits (ApproximateInterpreterBoundary precision _ _) = precision

-- | Read the declared error policy.
approximationErrorPolicy :: ApproximateInterpreterBoundary exact approximate -> ApproximationErrorPolicy
approximationErrorPolicy (ApproximateInterpreterBoundary _ policy _) = policy

-- | Apply the backend's explicit observation relation.
approximatelyObserves ::
    ApproximateInterpreterBoundary exact approximate ->
    exact ->
    approximate ->
    Bool
approximatelyObserves (ApproximateInterpreterBoundary _ _ relation) = relation

validPolicy :: ApproximationErrorPolicy -> Bool
validPolicy (AbsoluteRationalTolerance tolerance) = tolerance >= 0
validPolicy (RelativeRationalTolerance tolerance) = tolerance >= 0
validPolicy (BackendDocumentedError documentation) = not (null documentation)
