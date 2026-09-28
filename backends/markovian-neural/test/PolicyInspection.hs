module PolicyInspection (runTests) where

import Markovian.Backend.Neural.Categorical (
    categoricalFromLogits,
    neuralLogProbabilities,
    neuralProbabilities,
 )
import Markovian.Backend.Neural.Mask (mkActionMask)
import Markovian.Backend.Neural.Numeric (NeuralNumericError (..))
import Markovian.Backend.Neural.Policy (
    NeuralPolicyError (..),
    inspectLinearPolicy,
    linearPolicyInspectedAction,
    linearPolicyInspectedActions,
    linearPolicyInspectedLogit,
    linearPolicyInspectedMaskedActions,
    linearPolicyInspectedTerms,
    linearPolicyLogits,
    linearPolicyMaskedAction,
    linearPolicyMaskedLogProbability,
    linearPolicyMaskedProbability,
    linearPolicyParameters,
    linearPolicySelectedLogProbability,
    mkLinearCategoricalPolicy,
 )
import TestSupport (assert, assertVectorClose, requireRight)

runTests :: IO ()
runTests = do
    reorderedMask
    singleAction
    underflow
    rejectionChecks
    putStrLn "PASS: linear policy inspection"

reorderedMask :: IO ()
reorderedMask = do
    let parameters = [0.2, -0.4, 0.7, 0.1, -0.3, 0.5]
        features = [1, -0.25]
    policy <- requireRight "inspection policy" (mkLinearCategoricalPolicy 3 2 parameters)
    mask <- requireRight "reordered inspection mask" (mkActionMask 3 [2, 0])
    inspection <- requireRight "reordered policy inspection" (inspectLinearPolicy policy features mask)
    logits <- requireRight "ordinary policy logits" (linearPolicyLogits policy features)
    let actions = linearPolicyInspectedActions inspection
        masked = linearPolicyInspectedMaskedActions inspection
        inspectedLogits = fmap linearPolicyInspectedLogit actions
    assert "all global actions retain their indices" (fmap linearPolicyInspectedAction actions == [0, 1, 2])
    assert "masked actions retain caller order" (fmap linearPolicyMaskedAction masked == [2, 0])
    assert "inspection uses exactly the inference logits" (inspectedLogits == logits)
    sequence_
        [ assert
            ("checked row-major terms for action " ++ show action)
            (linearPolicyInspectedTerms entry == zipWith (*) row features)
        | (action, entry, row) <-
            zip3 [0 :: Int ..] actions [[0.2, -0.4], [0.7, 0.1], [-0.3, 0.5]]
        ]
    maskedLogits <-
        case logits of
            [first, _, third] -> pure [third, first]
            _ -> fail "three-action policy returned the wrong logit count"
    categorical <- requireRight "masked categorical reference" (categoricalFromLogits maskedLogits)
    assertVectorClose
        "masked stable log probabilities"
        0
        (neuralLogProbabilities categorical)
        (fmap linearPolicyMaskedLogProbability masked)
    assertVectorClose
        "masked stable probabilities"
        0
        (neuralProbabilities categorical)
        (fmap linearPolicyMaskedProbability masked)
    selected <- requireRight "selected policy log probability" (linearPolicySelectedLogProbability policy features mask 2)
    case masked of
        first : _ -> assert "inspection agrees with selected-action path" (selected == linearPolicyMaskedLogProbability first)
        [] -> assert "reordered mask returned no actions" False
    assert "inspection leaves frozen parameters untouched" (linearPolicyParameters policy == parameters)

singleAction :: IO ()
singleAction = do
    policy <- requireRight "single-action policy" (mkLinearCategoricalPolicy 2 1 [8, -5])
    mask <- requireRight "single-action mask" (mkActionMask 2 [1])
    inspection <- requireRight "single-action inspection" (inspectLinearPolicy policy [1] mask)
    let masked = linearPolicyInspectedMaskedActions inspection
    assert "one admissible action remains explicit" (fmap linearPolicyMaskedAction masked == [1])
    assert "one admissible action has log probability zero" (fmap linearPolicyMaskedLogProbability masked == [0])
    assert "one admissible action has probability one" (fmap linearPolicyMaskedProbability masked == [1])

underflow :: IO ()
underflow = do
    policy <- requireRight "underflow policy" (mkLinearCategoricalPolicy 2 1 [-1000, 0])
    mask <- requireRight "underflow mask" (mkActionMask 2 [0, 1])
    inspection <- requireRight "underflow inspection" (inspectLinearPolicy policy [1] mask)
    let masked = linearPolicyInspectedMaskedActions inspection
    assert "underflow retains finite stable log probability" (fmap linearPolicyMaskedLogProbability masked == [-1000, 0])
    assert "underflow may yield zero probability" (fmap linearPolicyMaskedProbability masked == [0, 1])

rejectionChecks :: IO ()
rejectionChecks = do
    policy <- requireRight "rejection policy" (mkLinearCategoricalPolicy 2 1 [1, 1e308])
    onlyFirst <- requireRight "inactive-overflow mask" (mkActionMask 2 [0])
    case inspectLinearPolicy policy [2] onlyFirst of
        Left (PolicyNumericFailure (NonFiniteArithmeticResult "linear policy logit" _)) -> pure ()
        result -> assert ("overflowing inactive action was accepted: " ++ show result) False
    wrongWidth <- requireRight "wrong-width inspection mask" (mkActionMask 3 [0])
    case inspectLinearPolicy policy [1] wrongWidth of
        Left (PolicyActionMaskWidthMismatch 2 3) -> pure ()
        result -> assert ("wrong-width inspection mask was accepted: " ++ show result) False
    case inspectLinearPolicy policy [] onlyFirst of
        Left (PolicyFeatureShapeMismatch 1 0) -> pure ()
        result -> assert ("wrong feature width was accepted: " ++ show result) False
    case inspectLinearPolicy policy [0 / 0] onlyFirst of
        Left (PolicyNumericFailure (NonFiniteVectorElement "linear policy features" 0 _)) -> pure ()
        result -> assert ("nonfinite feature was accepted: " ++ show result) False
    sumOverflow <- requireRight "sum-overflow policy" (mkLinearCategoricalPolicy 1 2 [1e308, 1e308])
    onlyAction <- requireRight "sum-overflow mask" (mkActionMask 1 [0])
    case inspectLinearPolicy sumOverflow [1, 1] onlyAction of
        Left (PolicyNumericFailure (NonFiniteArithmeticResult "linear policy logit" _)) -> pure ()
        result -> assert ("overflowing checked sum was accepted: " ++ show result) False
    case (linearPolicyLogits sumOverflow [1, 1], inspectLinearPolicy sumOverflow [1, 1] onlyAction) of
        (Left inferenceError, Left inspectionError) ->
            assert "inspection and inference retain the same checked-sum error" (inspectionError == inferenceError)
        _ -> assert "inspection and inference disagreed on checked-sum failure" False
