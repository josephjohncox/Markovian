module PolicyGradient (tests) where

import Data.List (transpose)
import Markovian.Backend.Neural (
    NeuralCategoricalError (..),
    NeuralPolicyError (..),
    categoricalFromLogits,
    linearPolicyParameters,
    linearPolicyScoreGradient,
    linearPolicySelectedLogProbability,
    mkActionMask,
    mkLinearCategoricalPolicy,
    neuralProbabilities,
    neuralSoftmaxJacobian,
    selectedActionLogProbability,
    selectedActionLogProbabilityGradient,
 )
import TestSupport (
    assert,
    assertCloseWith,
    assertVectorCloseWith,
    centralDifference,
    requireRight,
 )

tests :: IO ()
tests = do
    categoricalChecks
    jacobianFiniteDifference
    scoreFiniteDifference
    linearPolicyFiniteDifference
    expectedGradientFiniteDifference
    rejectionChecks
    putStrLn "PASS: categorical policy gradients"

categoricalChecks :: IO ()
categoricalChecks = do
    reference <- requireRight "reference softmax" (categoricalFromLogits [0, log 3])
    shifted <- requireRight "shifted softmax" (categoricalFromLogits [100, 100 + log 3])
    assertCloseWith "softmax normalization" 1e-15 1e-15 1 (sum (neuralProbabilities reference))
    assertVectorCloseWith
        "softmax shift invariance"
        1e-14
        1e-14
        (neuralProbabilities reference)
        (neuralProbabilities shifted)
    assert "softmax Jacobian shape" (all ((== 2) . length) (neuralSoftmaxJacobian reference))
    assert
        "softmax Jacobian row sums"
        (all ((<= 1e-15) . abs . sum) (neuralSoftmaxJacobian reference))

jacobianFiniteDifference :: IO ()
jacobianFiniteDifference = do
    let logits = [-0.4, 0.2, 1.1]
    categorical <- requireRight "Jacobian categorical" (categoricalFromLogits logits)
    numericalColumns <-
        traverse
            ( \index -> do
                let point = logits !! index
                    step = 1e-6 * max 1 (abs point)
                above <- requireRight "Jacobian above" (categoricalFromLogits (replace index (point + step) logits))
                below <- requireRight "Jacobian below" (categoricalFromLogits (replace index (point - step) logits))
                pure (zipWith (\high low -> (high - low) / (2 * step)) (neuralProbabilities above) (neuralProbabilities below))
            )
            [0 .. length logits - 1]
    sequence_
        [ assertVectorCloseWith
            ("softmax Jacobian row " ++ show row)
            2e-10
            2e-8
            analytic
            numerical
        | (row, (analytic, numerical)) <-
            zip [0 :: Int ..] (zip (neuralSoftmaxJacobian categorical) (transpose numericalColumns))
        ]

scoreFiniteDifference :: IO ()
scoreFiniteDifference = do
    let logits = [-0.4, 0.2, 1.1]
        action = 1
    analytic <- requireRight "selected score" (selectedActionLogProbabilityGradient logits action)
    numerical <-
        traverse
            ( \index ->
                centralDifference
                    1e-6
                    (\point -> requireRight "score perturbation" (selectedActionLogProbability (replace index point logits) action))
                    (logits !! index)
            )
            [0 .. length logits - 1]
    assertVectorCloseWith "selected-action score finite difference" 2e-10 2e-8 analytic numerical

linearPolicyFiniteDifference :: IO ()
linearPolicyFiniteDifference = do
    let parameters = [0.2, -0.4, 0.7, 0.1, -0.3, 0.5]
        features = [1.0, -0.25]
        action = 2
    mask <- requireRight "policy mask" (mkActionMask 3 [2, 0])
    policy <- requireRight "linear policy" (mkLinearCategoricalPolicy 3 2 parameters)
    analytic <- requireRight "linear policy score" (linearPolicyScoreGradient policy features mask action)
    numerical <-
        traverse
            ( \index ->
                centralDifference
                    1e-6
                    ( \point -> do
                        perturbed <- requireRight "linear policy perturbation" (mkLinearCategoricalPolicy 3 2 (replace index point parameters))
                        requireRight "linear masked log probability" (linearPolicySelectedLogProbability perturbed features mask action)
                    )
                    (parameters !! index)
            )
            [0 .. length parameters - 1]
    assertVectorCloseWith "linear masked score finite difference" 2e-10 3e-8 analytic numerical
    assert "unavailable action parameter gradient is zero" (take 2 (drop 2 analytic) == [0, 0])
    assert "policy parameter accessor" (linearPolicyParameters policy == parameters)

expectedGradientFiniteDifference :: IO ()
expectedGradientFiniteDifference = do
    let logits = [-0.3, 0.8]
        rewards = [2.0, -1.0]
    categorical <- requireRight "expected-gradient categorical" (categoricalFromLogits logits)
    scores <-
        traverse
            (requireRight "expected-gradient score" . selectedActionLogProbabilityGradient logits)
            [0, 1]
    let analytic =
            foldl
                (zipWith (+))
                [0, 0]
                [ fmap (* (probability * reward)) score
                | (probability, reward, score) <- zip3 (neuralProbabilities categorical) rewards scores
                ]
    numerical <-
        traverse
            ( \index ->
                centralDifference
                    1e-6
                    (\point -> expectedReward (replace index point logits) rewards)
                    (logits !! index)
            )
            [0, 1]
    assertVectorCloseWith "analytic expected policy gradient" 2e-10 3e-8 analytic numerical
    assertCloseWith "expected gradient sums to zero" 1e-14 1e-14 0 (sum analytic)

rejectionChecks :: IO ()
rejectionChecks = do
    case selectedActionLogProbabilityGradient [0, 1] 2 of
        Left (NeuralActionIndexOutOfBounds 2 2) -> pure ()
        result -> assert ("out-of-bounds action was accepted: " ++ show result) False
    case categoricalFromLogits [0, 0 / 0] of
        Left (NonFiniteNeuralLogit 1 _) -> pure ()
        result -> assert ("non-finite logit was accepted: " ++ show result) False
    case categoricalFromLogits [1.7976931348623157e308, -1.7976931348623157e308] of
        Left _ -> pure ()
        Right _ -> assert "overflowing log-softmax subtraction was accepted" False
    case mkLinearCategoricalPolicy 2 2 [0, 0, 0] of
        Left _ -> pure ()
        Right _ -> assert "wrong policy parameter shape was accepted" False
    policy <- requireRight "mask rejection policy" (mkLinearCategoricalPolicy 2 1 [0, 0])
    onlyFirst <- requireRight "single-action mask" (mkActionMask 2 [0])
    case linearPolicyScoreGradient policy [1] onlyFirst 1 of
        Left (PolicyActionNotInMask 1) -> pure ()
        result -> assert ("unavailable policy action was accepted: " ++ show result) False
    wrongWidth <- requireRight "wrong-width mask" (mkActionMask 3 [2])
    case linearPolicyScoreGradient policy [1] wrongWidth 2 of
        Left (PolicyActionMaskWidthMismatch 2 3) -> pure ()
        result -> assert ("wrong-width policy mask was accepted: " ++ show result) False

expectedReward :: [Double] -> [Double] -> IO Double
expectedReward logits rewards = do
    categorical <- requireRight "finite-difference expected reward" (categoricalFromLogits logits)
    pure (sum (zipWith (*) (neuralProbabilities categorical) rewards))

replace :: Int -> Double -> [Double] -> [Double]
replace selected replacement =
    zipWith (\index value -> if index == selected then replacement else value) [0 :: Int ..]
