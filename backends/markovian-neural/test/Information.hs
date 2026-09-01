module Information (tests) where

import Data.List (transpose)
import Markovian.Backend.Neural (
    NeuralInformationError (..),
    categoricalFromLogits,
    crossEntropyFromLogits,
    crossEntropyPredictionGradient,
    entropyFromLogits,
    entropyLogitGradient,
    klDivergenceFromLogits,
    mutualInformationFromJointLogits,
    neuralProbabilities,
    neuralSoftmaxJacobian,
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
    entropyChecks
    divergenceChecks
    productCheck
    mutualInformationChecks
    gradientChecks
    fusionChecks
    rejectionChecks
    putStrLn "PASS: categorical information"

entropyChecks :: IO ()
entropyChecks = do
    uniform <- requireRight "uniform entropy" (entropyFromLogits [0, 0])
    shifted <- requireRight "shifted entropy" (entropyFromLogits [100, 100])
    concentrated <- requireRight "concentrated entropy" (entropyFromLogits [1000, -1000])
    assertCloseWith "uniform entropy is log two" 1e-15 1e-15 (log 2) uniform
    assertCloseWith "entropy is shift invariant" 1e-15 1e-15 uniform shifted
    assertCloseWith "concentrated entropy tends to zero" 1e-15 1e-15 0 concentrated

divergenceChecks :: IO ()
divergenceChecks = do
    let source = [-0.4, 0.2, 1.1]
        target = [0.7, -0.2, 0.3]
    entropy <- requireRight "source entropy" (entropyFromLogits source)
    crossEntropy <- requireRight "cross entropy" (crossEntropyFromLogits source target)
    divergence <- requireRight "KL divergence" (klDivergenceFromLogits source target)
    selfDivergence <- requireRight "self KL divergence" (klDivergenceFromLogits source source)
    assertCloseWith "cross entropy decomposition" 1e-14 1e-14 crossEntropy (entropy + divergence)
    assert "KL divergence is positive on the fixture" (divergence > 0)
    assertCloseWith "self KL divergence is zero" 1e-14 1e-14 0 selfDivergence

productCheck :: IO ()
productCheck = do
    let left = [-0.3, 0.8]
        right = [0.1, -0.5, 0.4]
        productLogits = [leftLogit + rightLogit | leftLogit <- left, rightLogit <- right]
    leftEntropy <- requireRight "left product entropy" (entropyFromLogits left)
    rightEntropy <- requireRight "right product entropy" (entropyFromLogits right)
    productEntropy <- requireRight "product entropy" (entropyFromLogits productLogits)
    assertCloseWith
        "entropy is additive for independent products"
        2e-14
        2e-14
        (leftEntropy + rightEntropy)
        productEntropy

mutualInformationChecks :: IO ()
mutualInformationChecks = do
    let left = [-0.3, 0.8]
        right = [0.1, -0.5, 0.4]
        independentLogits = [leftLogit + rightLogit | leftLogit <- left, rightLogit <- right]
        sharedBitLogits = [0, -1000, -1000, 0]
    independent <-
        requireRight
            "independent mutual information"
            (mutualInformationFromJointLogits 2 3 independentLogits)
    shared <-
        requireRight
            "shared-bit mutual information"
            (mutualInformationFromJointLogits 2 2 sharedBitLogits)
    assertCloseWith "independent mutual information is zero" 2e-14 2e-14 0 independent
    assertCloseWith "shared fair-bit mutual information is log two" 2e-14 2e-14 (log 2) shared

gradientChecks :: IO ()
gradientChecks = do
    let logits = [-0.4, 0.2, 1.1]
        target = [0.6, -0.7, 0.1]
        prediction = [-0.2, 0.9, 0.4]
    entropyGradient <- requireRight "entropy gradient" (entropyLogitGradient logits)
    numericalEntropy <- finiteDifferenceVector "entropy finite difference" entropyFromLogits logits
    assertVectorCloseWith "entropy gradient finite difference" 3e-10 4e-8 entropyGradient numericalEntropy
    crossEntropyGradient <-
        requireRight
            "cross-entropy prediction gradient"
            (crossEntropyPredictionGradient target prediction)
    numericalCrossEntropy <-
        finiteDifferenceVector
            "cross-entropy finite difference"
            (crossEntropyFromLogits target)
            prediction
    assertVectorCloseWith
        "cross-entropy gradient finite difference"
        3e-10
        4e-8
        crossEntropyGradient
        numericalCrossEntropy
    assertCloseWith "entropy gradient is shift orthogonal" 1e-14 1e-14 0 (sum entropyGradient)
    assertCloseWith "cross-entropy gradient is shift orthogonal" 1e-14 1e-14 0 (sum crossEntropyGradient)

fusionChecks :: IO ()
fusionChecks = do
    let target = [0.6, -0.7, 0.1]
        prediction = [-0.2, 0.9, 0.4]
        shiftedPrediction = fmap (+ 100) prediction
    fused <- requireRight "fused cross-entropy VJP" (crossEntropyPredictionGradient target prediction)
    targetCategorical <- requireRight "fusion target" (categoricalFromLogits target)
    predictionCategorical <- requireRight "fusion prediction" (categoricalFromLogits prediction)
    let explicit =
            explicitJacobianVjp
                (neuralProbabilities targetCategorical)
                (neuralProbabilities predictionCategorical)
                (neuralSoftmaxJacobian predictionCategorical)
    assertVectorCloseWith "q-p equals explicit Jacobian VJP" 2e-15 2e-15 fused explicit
    shifted <- requireRight "finite-logit shifted fusion" (crossEntropyPredictionGradient target shiftedPrediction)
    assertVectorCloseWith "finite-logit fusion is shift invariant" 2e-14 2e-14 fused shifted

    underflowTarget <- requireRight "underflow target" (categoricalFromLogits [0, -1000])
    underflowPrediction <- requireRight "underflow prediction" (categoricalFromLogits [-1000, 0])
    underflowFused <- requireRight "underflow fused VJP" (crossEntropyPredictionGradient [0, -1000] [-1000, 0])
    let underflowExplicit =
            explicitJacobianVjp
                (neuralProbabilities underflowTarget)
                (neuralProbabilities underflowPrediction)
                (neuralSoftmaxJacobian underflowPrediction)
    assert "extreme prediction did not underflow" (neuralProbabilities underflowPrediction == [0, 1])
    assert "fused underflow gradient became non-finite" (not (any isNonFinite underflowFused))
    assert "explicit underflow Jacobian VJP stayed spuriously finite" (any isNonFinite underflowExplicit)

rejectionChecks :: IO ()
rejectionChecks = do
    case crossEntropyFromLogits [0, 0] [0, 0, 0] of
        Left (InformationShapeMismatch 2 3) -> pure ()
        result -> assert ("cross entropy accepted mismatched shapes: " ++ show result) False
    case entropyFromLogits [] of
        Left (InformationCategorical _) -> pure ()
        result -> assert ("entropy accepted an empty categorical: " ++ show result) False
    case mutualInformationFromJointLogits 0 2 [0, 0] of
        Left (InformationDimensionsMustBePositive 0 2) -> pure ()
        result -> assert ("mutual information accepted a zero dimension: " ++ show result) False
    case mutualInformationFromJointLogits 2 2 [0, 0, 0] of
        Left (InformationJointShapeMismatch 4 3) -> pure ()
        result -> assert ("mutual information accepted a malformed joint: " ++ show result) False
    case crossEntropyPredictionGradient [0, 0] [0 / 0, 0] of
        Left (InformationCategorical _) -> pure ()
        result -> assert ("fusion accepted NaN logits: " ++ show result) False
    case crossEntropyPredictionGradient [0, 0] [1 / 0, 0] of
        Left (InformationCategorical _) -> pure ()
        result -> assert ("fusion accepted infinite logits: " ++ show result) False

explicitJacobianVjp :: [Double] -> [Double] -> [[Double]] -> [Double]
explicitJacobianVjp target prediction jacobian =
    [ sum
        [ derivative * (negate expected / predicted)
        | (derivative, expected, predicted) <- zip3 column target prediction
        ]
    | column <- transpose jacobian
    ]

isNonFinite :: Double -> Bool
isNonFinite value = isNaN value || isInfinite value

finiteDifferenceVector ::
    String ->
    ([Double] -> Either NeuralInformationError Double) ->
    [Double] ->
    IO [Double]
finiteDifferenceVector label function point =
    traverse
        ( \index ->
            centralDifference
                1e-6
                (requireRight label . (function . (\value -> replace index value point)))
                (point !! index)
        )
        [0 .. length point - 1]

replace :: Int -> Double -> [Double] -> [Double]
replace selected replacement =
    zipWith (\index value -> if index == selected then replacement else value) [0 :: Int ..]
