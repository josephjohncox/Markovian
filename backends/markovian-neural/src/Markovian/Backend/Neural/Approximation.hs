{- | Explicit approximation boundary for neural reference computations.

The neural package defines this boundary locally so that its source distribution
can build without the root package. Construction requires a precision lower
bound, an error policy, and an observation relation. Checked 'Double' arithmetic
is not treated as exact equality.
-}
module Markovian.Backend.Neural.Approximation (
    ApproximationErrorPolicy (..),
    ApproximationBoundaryError (..),
    NeuralApproximationBoundary,
    neuralApproximationBoundary,
    neuralApproximationPrecisionBits,
    neuralApproximationErrorPolicy,
    neuralApproximatelyObserves,
) where

import Numeric.Natural (Natural)

-- | Declared numerical error policy for a neural computation.
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

{- | A declared relation between exact results and neural approximations.

The relation is supplied by the caller and is not ordinary equality. Precision
is stated in bits. Computations with data-dependent precision must choose a
lower bound and document the remaining policy in
'neuralApproximationErrorPolicy'.
-}
data NeuralApproximationBoundary exact approximate
    = NeuralApproximationBoundary
        !Natural
        !ApproximationErrorPolicy
        (exact -> approximate -> Bool)

-- | Construct a neural approximation boundary with explicit policies.
neuralApproximationBoundary ::
    Natural ->
    ApproximationErrorPolicy ->
    (exact -> approximate -> Bool) ->
    Either ApproximationBoundaryError (NeuralApproximationBoundary exact approximate)
neuralApproximationBoundary precision policy relation
    | precision == 0 = Left ApproximationPrecisionMustBePositive
    | not (validPolicy policy) =
        case policy of
            BackendDocumentedError _ -> Left ApproximationBackendPolicyMustBeDocumented
            _ -> Left ApproximationToleranceMustBeNonnegative
    | otherwise = Right (NeuralApproximationBoundary precision policy relation)

-- | Read the declared lower-bound precision in bits.
neuralApproximationPrecisionBits :: NeuralApproximationBoundary exact approximate -> Natural
neuralApproximationPrecisionBits (NeuralApproximationBoundary precision _ _) = precision

-- | Read the declared numerical error policy.
neuralApproximationErrorPolicy :: NeuralApproximationBoundary exact approximate -> ApproximationErrorPolicy
neuralApproximationErrorPolicy (NeuralApproximationBoundary _ policy _) = policy

-- | Apply the declared observation relation.
neuralApproximatelyObserves :: NeuralApproximationBoundary exact approximate -> exact -> approximate -> Bool
neuralApproximatelyObserves (NeuralApproximationBoundary _ _ relation) = relation

validPolicy :: ApproximationErrorPolicy -> Bool
validPolicy (AbsoluteRationalTolerance tolerance) = tolerance >= 0
validPolicy (RelativeRationalTolerance tolerance) = tolerance >= 0
validPolicy (BackendDocumentedError documentation) = not (null documentation)
