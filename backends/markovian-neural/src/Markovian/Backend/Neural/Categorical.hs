{- | Stable categorical probabilities and score gradients.

All gradients are with respect to the input logits. For selected action @a@,
'selectedActionLogProbabilityGradient' returns
@d log pi(a) / d z_j = indicator(a == j) - pi(j)@.
-}
module Markovian.Backend.Neural.Categorical (
    NeuralCategoricalError (..),
    NeuralCategorical,
    neuralLogProbabilities,
    neuralProbabilities,
    neuralSoftmaxJacobian,
    categoricalFromLogits,
    logSoftmaxFromLogits,
    selectedActionLogProbability,
    selectedActionLogProbabilityGradient,
    maximumCategoricalError,
) where

import Markovian.Backend.Neural.Numeric (
    NeuralNumericError,
    checkedSubtract,
    checkedSum,
    validateFiniteVector,
 )

-- | Categorical construction, indexing, and comparison failures.
data NeuralCategoricalError
    = EmptyNeuralLogits
    | NonFiniteNeuralLogit !Int !Double
    | InvalidNeuralProbability !Int !Double
    | NeuralCategoricalShapeMismatch !Int !Int
    | NeuralActionIndexOutOfBounds !Int !Int
    | NeuralCategoricalArithmetic !NeuralNumericError
    deriving (Eq, Show)

-- | Stable log probabilities, probabilities, and the softmax Jacobian.
data NeuralCategorical = NeuralCategorical
    { neuralLogProbabilities :: ![Double]
    -- ^ Stable log probabilities in action order.
    , neuralProbabilities :: ![Double]
    -- ^ Softmax probabilities in action order.
    , neuralSoftmaxJacobian :: ![[Double]]
    -- ^ Row-major probability Jacobian with respect to logits.
    }
    deriving (Eq, Show)

{- | Construct a categorical distribution with stable log-sum-exp.

The largest logit is subtracted before exponentiation. The Jacobian is
@d pi_i / d z_j = pi_i * (indicator(i == j) - pi_j)@.
-}
categoricalFromLogits :: [Double] -> Either NeuralCategoricalError NeuralCategorical
categoricalFromLogits [] = Left EmptyNeuralLogits
categoricalFromLogits logits = do
    validateLogits logits
    logProbabilities <- logSoftmaxFromValidated logits
    let probabilities = fmap exp logProbabilities
    validateProbabilities probabilities
    probabilityJacobian <- traverse (jacobianRow probabilities) (zip [0 :: Int ..] probabilities)
    Right
        NeuralCategorical
            { neuralLogProbabilities = logProbabilities
            , neuralProbabilities = probabilities
            , neuralSoftmaxJacobian = probabilityJacobian
            }

-- | Compute stable log-softmax values.
logSoftmaxFromLogits :: [Double] -> Either NeuralCategoricalError [Double]
logSoftmaxFromLogits [] = Left EmptyNeuralLogits
logSoftmaxFromLogits logits = do
    validateLogits logits
    logSoftmaxFromValidated logits

-- | Return the selected action's stable log probability.
selectedActionLogProbability :: [Double] -> Int -> Either NeuralCategoricalError Double
selectedActionLogProbability logits action = do
    values <- logSoftmaxFromLogits logits
    indexAction action values

-- | Return the selected action score gradient with respect to all logits.
selectedActionLogProbabilityGradient :: [Double] -> Int -> Either NeuralCategoricalError [Double]
selectedActionLogProbabilityGradient logits action = do
    categorical <- categoricalFromLogits logits
    _ <- indexAction action (neuralProbabilities categorical)
    traverse component (zip [0 :: Int ..] (neuralProbabilities categorical))
  where
    component (index, probability) =
        mapNumeric (checkedSubtract "selected-action score gradient" (indicator action index) probability)

-- | Compare exact categorical masses with an approximation in max norm.
maximumCategoricalError :: [Rational] -> [Double] -> Either NeuralCategoricalError Double
maximumCategoricalError exact approximate
    | length exact /= length approximate =
        Left (NeuralCategoricalShapeMismatch (length exact) (length approximate))
    | otherwise = do
        validateProbabilities approximate
        let references = fmap fromRational exact
        case validateFiniteVector "exact categorical references converted to Double" references of
            Left err -> Left (NeuralCategoricalArithmetic err)
            Right () -> do
                differences <-
                    traverse
                        (\(reference, actual) -> mapNumeric (checkedSubtract "categorical approximation error" reference actual))
                        (zip references approximate)
                Right (foldr (max . abs) 0 differences)

logSoftmaxFromValidated :: [Double] -> Either NeuralCategoricalError [Double]
logSoftmaxFromValidated logits = do
    let largest = maximum logits
    shifted <- traverse (\logit -> mapNumeric (checkedSubtract "shifted logit" logit largest)) logits
    let exponentials = fmap exp shifted
    total <- mapNumeric (checkedSum "softmax exponential sum" exponentials)
    let logTotal = log total
    case validateFiniteVector "log-softmax" (fmap (subtract logTotal) shifted) of
        Left err -> Left (NeuralCategoricalArithmetic err)
        Right () -> Right (fmap (subtract logTotal) shifted)

jacobianRow :: [Double] -> (Int, Double) -> Either NeuralCategoricalError [Double]
jacobianRow probabilities (row, probabilityI) =
    traverse component (zip [0 :: Int ..] probabilities)
  where
    component (column, probabilityJ) = do
        difference <- mapNumeric (checkedSubtract "softmax Jacobian difference" (indicator row column) probabilityJ)
        let result = probabilityI * difference
        if finite result
            then Right result
            else Left (InvalidNeuralProbability column result)

validateLogits :: [Double] -> Either NeuralCategoricalError ()
validateLogits = go 0
  where
    go _ [] = Right ()
    go index (value : remaining)
        | finite value = go (index + 1) remaining
        | otherwise = Left (NonFiniteNeuralLogit index value)

validateProbabilities :: [Double] -> Either NeuralCategoricalError ()
validateProbabilities = go 0
  where
    go _ [] = Right ()
    go index (value : remaining)
        | finite value && value >= 0 && value <= 1 = go (index + 1) remaining
        | otherwise = Left (InvalidNeuralProbability index value)

indexAction :: Int -> [value] -> Either NeuralCategoricalError value
indexAction action values
    | action < 0 || action >= count = Left (NeuralActionIndexOutOfBounds action count)
    | otherwise = Right (values !! action)
  where
    count = length values

mapNumeric :: Either NeuralNumericError value -> Either NeuralCategoricalError value
mapNumeric = either (Left . NeuralCategoricalArithmetic) Right

indicator :: Int -> Int -> Double
indicator left right = if left == right then 1 else 0

finite :: Double -> Bool
finite value = not (isNaN value || isInfinite value)
