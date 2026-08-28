{- | Pure deterministic SGD for dense networks.

Every displacement is computed from the supplied pre-update parameter vector;
there is no mutable or sequential per-parameter state.
-}
module Markovian.Backend.Neural.Optimizer (
    OptimizerError (..),
    SGD,
    mkSGD,
    sgdLearningRate,
    applySGD,
) where

import Markovian.Backend.Neural.Dense (
    DenseError,
    DenseNetwork,
    denseParameterCount,
    denseParameters,
    replaceDenseParameters,
 )
import Markovian.Backend.Neural.Numeric (
    NeuralNumericError,
    checkedMultiply,
    checkedSubtract,
    validateFinite,
    validateFiniteVector,
 )

-- | Optimizer validation and arithmetic failures.
data OptimizerError
    = InvalidSGDLearningRate !Double
    | SGDGradientShapeMismatch !Int !Int
    | SGDNumericFailure !NeuralNumericError
    | SGDDenseFailure !DenseError
    deriving (Eq, Show)

-- | A finite, nonnegative SGD learning rate.
newtype SGD = SGD Double
    deriving (Eq, Show)

-- | Construct a checked SGD configuration.
mkSGD :: Double -> Either OptimizerError SGD
mkSGD rate
    | rate < 0 = Left (InvalidSGDLearningRate rate)
    | otherwise =
        case validateFinite "SGD learning rate" rate of
            Left _ -> Left (InvalidSGDLearningRate rate)
            Right () -> Right (SGD rate)

-- | Read the learning rate.
sgdLearningRate :: SGD -> Double
sgdLearningRate (SGD rate) = rate

-- | Apply @theta' = theta - rate * gradient@ atomically.
applySGD :: SGD -> [Double] -> DenseNetwork -> Either OptimizerError DenseNetwork
applySGD optimizer gradient network
    | length gradient /= denseParameterCount network =
        Left (SGDGradientShapeMismatch (denseParameterCount network) (length gradient))
    | otherwise = do
        mapNumeric (validateFiniteVector "SGD gradient" gradient)
        updated <- traverse update (zip (denseParameters network) gradient)
        either (Left . SGDDenseFailure) Right (replaceDenseParameters updated network)
  where
    update (parameter, derivative) = do
        displacement <- mapNumeric (checkedMultiply "SGD displacement" (sgdLearningRate optimizer) derivative)
        mapNumeric (checkedSubtract "SGD updated parameter" parameter displacement)

mapNumeric :: Either NeuralNumericError value -> Either OptimizerError value
mapNumeric = either (Left . SGDNumericFailure) Right
