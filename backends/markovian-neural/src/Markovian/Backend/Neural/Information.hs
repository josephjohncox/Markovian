{- | Approximate information quantities for finite categorical models.

The exact semantic core stores rational probabilities, but Shannon information
uses logarithms and is generally irrational. These computations therefore live
at the explicit 'Double' approximation boundary.
-}
module Markovian.Backend.Neural.Information (
    NeuralInformationError (..),
    entropyFromLogits,
    crossEntropyFromLogits,
    klDivergenceFromLogits,
    mutualInformationFromJointLogits,
    entropyLogitGradient,
    crossEntropyPredictionGradient,
) where

import Data.List (transpose)
import Markovian.Backend.Neural.Categorical (
    NeuralCategoricalError,
    categoricalFromLogits,
    neuralLogProbabilities,
    neuralProbabilities,
    neuralSoftmaxJacobian,
 )
import Markovian.Backend.Neural.Numeric (
    NeuralNumericError,
    checkedMultiply,
    checkedSubtract,
    checkedSum,
 )

-- | Construction, shape, or checked-arithmetic failure.
data NeuralInformationError
    = InformationCategorical !NeuralCategoricalError
    | InformationShapeMismatch !Int !Int
    | InformationDimensionsMustBePositive !Int !Int
    | InformationJointSizeOverflow !Int !Int
    | InformationJointShapeMismatch !Int !Int
    | InformationArithmetic !NeuralNumericError
    deriving (Eq, Show)

-- | Shannon entropy in nats, computed from stable log-softmax values.
entropyFromLogits :: [Double] -> Either NeuralInformationError Double
entropyFromLogits logits = do
    categorical <- mapCategorical (categoricalFromLogits logits)
    entropyFromCategorical
        (neuralProbabilities categorical)
        (neuralLogProbabilities categorical)

-- | Cross entropy in nats from target logits and prediction logits.
crossEntropyFromLogits :: [Double] -> [Double] -> Either NeuralInformationError Double
crossEntropyFromLogits targetLogits predictionLogits = do
    target <- mapCategorical (categoricalFromLogits targetLogits)
    prediction <- mapCategorical (categoricalFromLogits predictionLogits)
    checkedSameShape
        (neuralProbabilities target)
        (neuralLogProbabilities prediction)
    terms <-
        traverse
            ( \(probability, logProbability) ->
                negate <$> mapNumeric (checkedMultiply "cross-entropy term" probability logProbability)
            )
            (zip (neuralProbabilities target) (neuralLogProbabilities prediction))
    mapNumeric (checkedSum "cross entropy" terms)

{- | Kullback--Leibler divergence in nats.

Floating roundoff can produce a tiny negative result near equality. The
function does not clamp it because doing so would hide the approximation.
-}
klDivergenceFromLogits :: [Double] -> [Double] -> Either NeuralInformationError Double
klDivergenceFromLogits sourceLogits targetLogits = do
    crossEntropy <- crossEntropyFromLogits sourceLogits targetLogits
    sourceEntropy <- entropyFromLogits sourceLogits
    mapNumeric (checkedSubtract "KL divergence" crossEntropy sourceEntropy)

{- | Mutual information in nats from row-major joint logits.

The dimensions name the left and right finite carriers. Zero floating masses
created by exponential underflow contribute zero, consistently with the finite
@0 log 0 = 0@ convention. Positive cells always have positive marginals.
-}
mutualInformationFromJointLogits ::
    Int ->
    Int ->
    [Double] ->
    Either NeuralInformationError Double
mutualInformationFromJointLogits leftSize rightSize jointLogits
    | leftSize <= 0 || rightSize <= 0 =
        Left (InformationDimensionsMustBePositive leftSize rightSize)
    | leftSize > maxBound `div` rightSize =
        Left (InformationJointSizeOverflow leftSize rightSize)
    | length jointLogits /= expectedSize =
        Left (InformationJointShapeMismatch expectedSize (length jointLogits))
    | otherwise = do
        joint <- mapCategorical (categoricalFromLogits jointLogits)
        let probabilityRows = chunksOf rightSize (neuralProbabilities joint)
            logProbabilityRows = chunksOf rightSize (neuralLogProbabilities joint)
        rowMarginals <- traverse (mapNumeric . checkedSum "mutual-information row marginal") probabilityRows
        columnMarginals <-
            traverse
                (mapNumeric . checkedSum "mutual-information column marginal")
                (transpose probabilityRows)
        rowTerms <-
            traverse
                (mutualInformationRow columnMarginals)
                (zip3 probabilityRows logProbabilityRows rowMarginals)
        mapNumeric (checkedSum "mutual information" (concat rowTerms))
  where
    expectedSize = leftSize * rightSize

{- | Gradient of Shannon entropy with respect to the input logits.

For softmax Jacobian @J@, component @j@ is
@-sum_i J_ij * (log p_i + 1)@.
-}
entropyLogitGradient :: [Double] -> Either NeuralInformationError [Double]
entropyLogitGradient logits = do
    categorical <- mapCategorical (categoricalFromLogits logits)
    let logProbabilities = neuralLogProbabilities categorical
        jacobianColumns = transpose (neuralSoftmaxJacobian categorical)
    traverse (gradientComponent logProbabilities) jacobianColumns

{- | Gradient of target-to-prediction cross entropy with respect to prediction
logits. For target probabilities @p@ and prediction probabilities @q@, the
result is @q - p@.
-}
crossEntropyPredictionGradient ::
    [Double] ->
    [Double] ->
    Either NeuralInformationError [Double]
crossEntropyPredictionGradient targetLogits predictionLogits = do
    target <- mapCategorical (categoricalFromLogits targetLogits)
    prediction <- mapCategorical (categoricalFromLogits predictionLogits)
    let targetProbabilities = neuralProbabilities target
        predictionProbabilities = neuralProbabilities prediction
    checkedSameShape targetProbabilities predictionProbabilities
    traverse
        ( \(predicted, expected) ->
            mapNumeric (checkedSubtract "cross-entropy prediction gradient" predicted expected)
        )
        (zip predictionProbabilities targetProbabilities)

entropyFromCategorical ::
    [Double] ->
    [Double] ->
    Either NeuralInformationError Double
entropyFromCategorical probabilities logProbabilities = do
    checkedSameShape probabilities logProbabilities
    terms <-
        traverse
            ( \(probability, logProbability) ->
                negate <$> mapNumeric (checkedMultiply "entropy term" probability logProbability)
            )
            (zip probabilities logProbabilities)
    mapNumeric (checkedSum "entropy" terms)

gradientComponent ::
    [Double] ->
    [Double] ->
    Either NeuralInformationError Double
gradientComponent logProbabilities jacobianColumn = do
    checkedSameShape logProbabilities jacobianColumn
    terms <-
        traverse
            ( \(derivative, logProbability) ->
                mapNumeric
                    ( checkedMultiply
                        "entropy logit-gradient term"
                        derivative
                        (logProbability + 1)
                    )
            )
            (zip jacobianColumn logProbabilities)
    negate <$> mapNumeric (checkedSum "entropy logit gradient" terms)

mutualInformationRow ::
    [Double] ->
    ([Double], [Double], Double) ->
    Either NeuralInformationError [Double]
mutualInformationRow columnMarginals (probabilities, logProbabilities, rowMarginal) = do
    checkedSameShape probabilities logProbabilities
    checkedSameShape probabilities columnMarginals
    traverse term (zip3 probabilities logProbabilities columnMarginals)
  where
    term (probability, logProbability, columnMarginal)
        | probability == 0 = Right 0
        | otherwise = do
            withoutRow <-
                mapNumeric
                    (checkedSubtract "mutual-information row ratio" logProbability (log rowMarginal))
            logRatio <-
                mapNumeric
                    (checkedSubtract "mutual-information column ratio" withoutRow (log columnMarginal))
            mapNumeric (checkedMultiply "mutual-information term" probability logRatio)

chunksOf :: Int -> [value] -> [[value]]
chunksOf _ [] = []
chunksOf size values =
    let (current, remaining) = splitAt size values
     in current : chunksOf size remaining

checkedSameShape :: [left] -> [right] -> Either NeuralInformationError ()
checkedSameShape left right
    | length left == length right = Right ()
    | otherwise = Left (InformationShapeMismatch (length left) (length right))

mapCategorical :: Either NeuralCategoricalError value -> Either NeuralInformationError value
mapCategorical = either (Left . InformationCategorical) Right

mapNumeric :: Either NeuralNumericError value -> Either NeuralInformationError value
mapNumeric = either (Left . InformationArithmetic) Right
