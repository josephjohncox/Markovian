\begin{code}
{- | Framework-independent neural categorical contracts.

This package does not place tensors or autodiff in the semantic core. It makes
normalization, gradient, estimator, and approximation assumptions explicit for
later framework adapters.
-}
module Markovian.Backend.Neural (
    NeuralCategoricalError (..),
    NeuralNormalization (..),
    NeuralGradient (..),
    GradientEstimator (..),
    NeuralCategoricalContract (..),
    defaultNeuralCategoricalContract,
    NeuralCategorical (..),
    categoricalFromLogits,
    maximumCategoricalError,
) where

-- | Neural categorical construction and comparison failures.
data NeuralCategoricalError
    = EmptyNeuralLogits
    | NonFiniteNeuralLogit !Integer !Double
    | InvalidNeuralProbability !Integer !Double
    | NeuralCategoricalShapeMismatch !Int !Int
    deriving (Eq, Show)

-- | Normalization assumed by a neural categorical adapter.
data NeuralNormalization = StableSoftmax
    deriving (Eq, Show)

-- | Gradient meaning exposed to an adapter.
data NeuralGradient = AnalyticSoftmaxJacobian
    deriving (Eq, Show)

-- | Estimator assumption for stochastic downstream objectives.
data GradientEstimator
    = NoStochasticGradientEstimator
    | ScoreFunctionEstimator
        { scoreFunctionUsesBaseline :: !Bool
        }
    deriving (Eq, Show)

-- | Explicit neural approximation and gradient contract.
data NeuralCategoricalContract = NeuralCategoricalContract
    { neuralNormalization :: !NeuralNormalization
    , neuralGradient :: !NeuralGradient
    , neuralEstimator :: !GradientEstimator
    }
    deriving (Eq, Show)

-- | The reference contract used by direct softmax adapters.
defaultNeuralCategoricalContract :: NeuralCategoricalContract
defaultNeuralCategoricalContract =
    NeuralCategoricalContract
        { neuralNormalization = StableSoftmax
        , neuralGradient = AnalyticSoftmaxJacobian
        , neuralEstimator = ScoreFunctionEstimator True
        }

-- | Softmax probabilities and their exact analytic Jacobian in 'Double'.
data NeuralCategorical = NeuralCategorical
    { neuralProbabilities :: ![Double]
    , neuralSoftmaxJacobian :: ![[Double]]
    , neuralContract :: !NeuralCategoricalContract
    }
    deriving (Eq, Show)

{- | Construct a stable softmax categorical distribution.

The largest logit is subtracted before exponentiation. The returned Jacobian
uses @d p_i / d z_j = p_i * (delta_ij - p_j)@.
-}
categoricalFromLogits :: [Double] -> Either NeuralCategoricalError NeuralCategorical
categoricalFromLogits [] = Left EmptyNeuralLogits
categoricalFromLogits logits@(firstLogit : remainingLogits) = do
    traverse_ validateLogit (zip [0 ..] logits)
    let largest = foldl max firstLogit remainingLogits
        exponentials = fmap (exp . subtract largest) logits
        total = sum exponentials
        probabilities = fmap (/ total) exponentials
    traverse_ validateProbability (zip [0 ..] probabilities)
    Right
        NeuralCategorical
            { neuralProbabilities = probabilities
            , neuralSoftmaxJacobian = jacobian probabilities
            , neuralContract = defaultNeuralCategoricalContract
            }
  where
    validateLogit (index, value)
        | isNaN value || isInfinite value = Left (NonFiniteNeuralLogit index value)
        | otherwise = Right ()

    validateProbability (index, value)
        | isNaN value || isInfinite value || value < 0 || value > 1 =
            Left (InvalidNeuralProbability index value)
        | otherwise = Right ()

-- | Compare exact categorical masses with a neural approximation in max norm.
maximumCategoricalError :: [Rational] -> [Double] -> Either NeuralCategoricalError Double
maximumCategoricalError exact approximate
    | length exact /= length approximate =
        Left (NeuralCategoricalShapeMismatch (length exact) (length approximate))
    | otherwise = do
        traverse_ validateProbability (zip [0 ..] approximate)
        Right
            ( foldr
                max
                0
                (zipWith (\reference actual -> abs (fromRational reference - actual)) exact approximate)
            )
  where
    validateProbability (index, value)
        | isNaN value || isInfinite value || value < 0 || value > 1 =
            Left (InvalidNeuralProbability index value)
        | otherwise = Right ()

jacobian :: [Double] -> [[Double]]
jacobian probabilities =
    [ [ probabilityI * (indicator row column - probabilityJ)
      | (column, probabilityJ) <- zip [0 :: Int ..] probabilities
      ]
    | (row, probabilityI) <- zip [0 :: Int ..] probabilities
    ]
  where
    indicator left right = if left == right then 1 else 0

traverse_ :: (value -> Either error ()) -> [value] -> Either error ()
traverse_ _ [] = Right ()
traverse_ validate (value : remaining) = do
    validate value
    traverse_ validate remaining
\end{code}
