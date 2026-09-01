{- | Small linear policy and value-function references.

Policy parameters are row-major: all feature weights for action zero come
first, followed by all weights for action one, and so on. Constructors validate
positive dimensions, exact parameter counts, and finite values.
-}
module Markovian.Backend.Neural.Policy (
    NeuralPolicyError (..),
    LinearCategoricalPolicy,
    mkLinearCategoricalPolicy,
    linearPolicyActionCount,
    linearPolicyFeatureCount,
    linearPolicyParameters,
    linearPolicyLogits,
    linearPolicySelectedLogProbability,
    linearPolicyScoreGradient,
    applyLinearPolicyAscent,
    LinearValueFunction,
    mkLinearValueFunction,
    linearValueFeatureCount,
    linearValueParameters,
    evaluateLinearValue,
    linearValueGradient,
    applyLinearValueAscent,
) where

import Data.List (elemIndex)
import Markovian.Backend.Neural.Categorical (
    NeuralCategoricalError,
    selectedActionLogProbability,
    selectedActionLogProbabilityGradient,
 )
import Markovian.Backend.Neural.Mask (
    ActionMask,
    ActionMaskError,
    actionMaskContains,
    actionMaskIndices,
    actionMaskWidth,
    gatherActionMask,
    scatterActionMask,
 )
import Markovian.Backend.Neural.Numeric (
    NeuralNumericError,
    checkedAdd,
    checkedMultiply,
    checkedSum,
    validateFinite,
    validateFiniteVector,
 )

-- | Shape, categorical, and finite-arithmetic failures.
data NeuralPolicyError
    = InvalidPolicyActionCount !Int
    | InvalidPolicyFeatureCount !Int
    | PolicyDimensionOverflow !Int !Int
    | PolicyParameterShapeMismatch !Int !Int
    | PolicyFeatureShapeMismatch !Int !Int
    | PolicyGradientShapeMismatch !Int !Int
    | PolicyActionMaskWidthMismatch !Int !Int
    | PolicyActionNotInMask !Int
    | PolicyActionMaskFailure !ActionMaskError
    | InvalidValueFeatureCount !Int
    | ValueParameterShapeMismatch !Int !Int
    | ValueFeatureShapeMismatch !Int !Int
    | ValueGradientShapeMismatch !Int !Int
    | InvalidPolicyLearningRate !Double
    | InvalidValueLearningRate !Double
    | CategoricalPolicyFailure !NeuralCategoricalError
    | PolicyNumericFailure !NeuralNumericError
    deriving (Eq, Show)

-- | A row-major linear categorical policy.
data LinearCategoricalPolicy = LinearCategoricalPolicy !Int !Int ![Double]
    deriving (Eq, Show)

-- | Construct a finite linear categorical policy.
mkLinearCategoricalPolicy :: Int -> Int -> [Double] -> Either NeuralPolicyError LinearCategoricalPolicy
mkLinearCategoricalPolicy actions features parameters
    | actions <= 0 = Left (InvalidPolicyActionCount actions)
    | features <= 0 = Left (InvalidPolicyFeatureCount features)
    | actions > maxBound `div` features = Left (PolicyDimensionOverflow actions features)
    | actual /= expected = Left (PolicyParameterShapeMismatch expected actual)
    | otherwise = do
        mapNumeric (validateFiniteVector "linear policy parameters" parameters)
        Right (LinearCategoricalPolicy actions features parameters)
  where
    expected = actions * features
    actual = length parameters

-- | Number of policy actions.
linearPolicyActionCount :: LinearCategoricalPolicy -> Int
linearPolicyActionCount (LinearCategoricalPolicy actions _ _) = actions

-- | Number of features expected by the policy.
linearPolicyFeatureCount :: LinearCategoricalPolicy -> Int
linearPolicyFeatureCount (LinearCategoricalPolicy _ features _) = features

-- | Policy parameters in documented row-major order.
linearPolicyParameters :: LinearCategoricalPolicy -> [Double]
linearPolicyParameters (LinearCategoricalPolicy _ _ parameters) = parameters

-- | Evaluate all action logits for one finite feature vector.
linearPolicyLogits :: LinearCategoricalPolicy -> [Double] -> Either NeuralPolicyError [Double]
linearPolicyLogits policy@(LinearCategoricalPolicy actions features parameters) inputs = do
    validatePolicyFeatures policy inputs
    traverse dotRow [0 .. actions - 1]
  where
    dotRow action =
        checkedDot
            "linear policy logit"
            (take features (drop (action * features) parameters))
            inputs

-- | Evaluate a selected action log probability.
linearPolicySelectedLogProbability :: LinearCategoricalPolicy -> [Double] -> ActionMask -> Int -> Either NeuralPolicyError Double
linearPolicySelectedLogProbability policy features mask action = do
    (maskedLogits, localAction) <- maskedPolicyLogits policy features mask action
    mapCategorical (selectedActionLogProbability maskedLogits localAction)

{- | Differentiate a selected action log probability with respect to every
row-major policy parameter.
-}
linearPolicyScoreGradient :: LinearCategoricalPolicy -> [Double] -> ActionMask -> Int -> Either NeuralPolicyError [Double]
linearPolicyScoreGradient policy features mask action = do
    (maskedLogits, localAction) <- maskedPolicyLogits policy features mask action
    maskedGradient <- mapCategorical (selectedActionLogProbabilityGradient maskedLogits localAction)
    globalGradient <- mapMask (scatterActionMask mask maskedGradient)
    rows <-
        traverse
            ( \(globalAction, score) ->
                if actionMaskContains globalAction mask
                    then traverse (mapNumeric . checkedMultiply "linear policy score gradient" score) features
                    else Right (replicate (linearPolicyFeatureCount policy) 0.0)
            )
            (zip [0 ..] globalGradient)
    Right (concat rows)

maskedPolicyLogits :: LinearCategoricalPolicy -> [Double] -> ActionMask -> Int -> Either NeuralPolicyError ([Double], Int)
maskedPolicyLogits policy features mask action
    | actionMaskWidth mask /= linearPolicyActionCount policy =
        Left (PolicyActionMaskWidthMismatch (linearPolicyActionCount policy) (actionMaskWidth mask))
    | otherwise = do
        logits <- linearPolicyLogits policy features
        maskedLogits <- mapMask (gatherActionMask mask logits)
        if actionMaskContains action mask
            then case elemIndex action (actionMaskIndices mask) of
                Just localAction -> Right (maskedLogits, localAction)
                Nothing -> Left (PolicyActionNotInMask action)
            else Left (PolicyActionNotInMask action)

-- | Apply one checked gradient-ascent step.
applyLinearPolicyAscent :: Double -> [Double] -> LinearCategoricalPolicy -> Either NeuralPolicyError LinearCategoricalPolicy
applyLinearPolicyAscent rate gradient (LinearCategoricalPolicy actions features parameters)
    | length gradient /= length parameters =
        Left (PolicyGradientShapeMismatch (length parameters) (length gradient))
    | otherwise = do
        validateRate InvalidPolicyLearningRate rate
        mapNumeric (validateFiniteVector "linear policy gradient" gradient)
        updated <- traverse update (zip parameters gradient)
        mkLinearCategoricalPolicy actions features updated
  where
    update (parameter, derivative) = do
        displacement <- mapNumeric (checkedMultiply "linear policy update displacement" rate derivative)
        mapNumeric (checkedAdd "linear policy updated parameter" parameter displacement)

-- | A scalar linear value function.
data LinearValueFunction = LinearValueFunction !Int ![Double]
    deriving (Eq, Show)

-- | Construct a finite scalar linear value function.
mkLinearValueFunction :: Int -> [Double] -> Either NeuralPolicyError LinearValueFunction
mkLinearValueFunction features parameters
    | features <= 0 = Left (InvalidValueFeatureCount features)
    | actual /= features = Left (ValueParameterShapeMismatch features actual)
    | otherwise = do
        mapNumeric (validateFiniteVector "linear value parameters" parameters)
        Right (LinearValueFunction features parameters)
  where
    actual = length parameters

-- | Number of features expected by the value function.
linearValueFeatureCount :: LinearValueFunction -> Int
linearValueFeatureCount (LinearValueFunction features _) = features

-- | Value-function parameters.
linearValueParameters :: LinearValueFunction -> [Double]
linearValueParameters (LinearValueFunction _ parameters) = parameters

-- | Evaluate the scalar value for one feature vector.
evaluateLinearValue :: LinearValueFunction -> [Double] -> Either NeuralPolicyError Double
evaluateLinearValue valueFunction@(LinearValueFunction _ parameters) features = do
    validateValueFeatures valueFunction features
    checkedDot "linear value" parameters features

-- | Differentiate the scalar linear value with respect to its parameters.
linearValueGradient :: LinearValueFunction -> [Double] -> Either NeuralPolicyError [Double]
linearValueGradient valueFunction features = do
    validateValueFeatures valueFunction features
    Right features

-- | Apply one checked gradient-ascent step to value parameters.
applyLinearValueAscent :: Double -> [Double] -> LinearValueFunction -> Either NeuralPolicyError LinearValueFunction
applyLinearValueAscent rate gradient (LinearValueFunction features parameters)
    | length gradient /= length parameters =
        Left (ValueGradientShapeMismatch (length parameters) (length gradient))
    | otherwise = do
        validateRate InvalidValueLearningRate rate
        mapNumeric (validateFiniteVector "linear value gradient" gradient)
        updated <- traverse update (zip parameters gradient)
        mkLinearValueFunction features updated
  where
    update (parameter, derivative) = do
        displacement <- mapNumeric (checkedMultiply "linear value update displacement" rate derivative)
        mapNumeric (checkedAdd "linear value updated parameter" parameter displacement)

validatePolicyFeatures :: LinearCategoricalPolicy -> [Double] -> Either NeuralPolicyError ()
validatePolicyFeatures (LinearCategoricalPolicy _ expected _) features
    | actual /= expected = Left (PolicyFeatureShapeMismatch expected actual)
    | otherwise = mapNumeric (validateFiniteVector "linear policy features" features)
  where
    actual = length features

validateValueFeatures :: LinearValueFunction -> [Double] -> Either NeuralPolicyError ()
validateValueFeatures (LinearValueFunction expected _) features
    | actual /= expected = Left (ValueFeatureShapeMismatch expected actual)
    | otherwise = mapNumeric (validateFiniteVector "linear value features" features)
  where
    actual = length features

checkedDot :: String -> [Double] -> [Double] -> Either NeuralPolicyError Double
checkedDot label left right = do
    products <- traverse (mapNumeric . uncurry (checkedMultiply label)) (zip left right)
    mapNumeric (checkedSum label products)

validateRate :: (Double -> NeuralPolicyError) -> Double -> Either NeuralPolicyError ()
validateRate constructor rate
    | rate < 0 = Left (constructor rate)
    | otherwise = either (const (Left (constructor rate))) Right (validateFinite "learning rate" rate)

mapCategorical :: Either NeuralCategoricalError value -> Either NeuralPolicyError value
mapCategorical = either (Left . CategoricalPolicyFailure) Right

mapNumeric :: Either NeuralNumericError value -> Either NeuralPolicyError value
mapNumeric = either (Left . PolicyNumericFailure) Right

mapMask :: Either ActionMaskError value -> Either NeuralPolicyError value
mapMask = either (Left . PolicyActionMaskFailure) Right
