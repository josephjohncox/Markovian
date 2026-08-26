\begin{code}
module Main (main) where

import Markovian.Backend.Neural (
    GradientEstimator (..),
    NeuralCategorical (..),
    NeuralCategoricalContract (..),
    NeuralCategoricalError (..),
    NeuralGradient (..),
    NeuralNormalization (..),
    categoricalFromLogits,
    maximumCategoricalError,
 )
import System.Exit (exitFailure)

main :: IO ()
main = do
    case categoricalFromLogits [] of
        Left EmptyNeuralLogits -> pure ()
        result -> failTest ("empty logits were not rejected: " ++ show result)
    case categoricalFromLogits [0, 0 / 0] of
        Left (NonFiniteNeuralLogit 1 _) -> pure ()
        result -> failTest ("non-finite logits were not rejected: " ++ show result)

    reference <- requireRight "reference softmax" (categoricalFromLogits [0, log 3])
    shifted <- requireRight "shifted softmax" (categoricalFromLogits [100, 100 + log 3])
    let probabilities = neuralProbabilities reference
        shiftedProbabilities = neuralProbabilities shifted
        jacobian = neuralSoftmaxJacobian reference
        contract = neuralContract reference
    assert "softmax did not normalize" (abs (sum probabilities - 1) <= 1e-15)
    assert "softmax shift invariance failed" (closeVectors 1e-14 probabilities shiftedProbabilities)
    assert "softmax Jacobian shape changed" (length jacobian == 2 && all ((== 2) . length) jacobian)
    assert "softmax Jacobian rows must sum to zero" (all ((<= 1e-15) . abs . sum) jacobian)
    assert "neural normalization contract changed" (neuralNormalization contract == StableSoftmax)
    assert "neural gradient contract changed" (neuralGradient contract == AnalyticSoftmaxJacobian)
    assert "neural estimator contract changed" (neuralEstimator contract == ScoreFunctionEstimator True)

    approximation <- requireRight "neural approximation error" (maximumCategoricalError [1 / 4, 3 / 4] probabilities)
    assert "neural approximation exceeds tolerance" (approximation <= 1e-15)
    case maximumCategoricalError [1] probabilities of
        Left (NeuralCategoricalShapeMismatch 1 2) -> pure ()
        result -> failTest ("neural shape mismatch was not rejected: " ++ show result)
    case maximumCategoricalError [1] [0 / 0] of
        Left (InvalidNeuralProbability 0 _) -> pure ()
        result -> failTest ("invalid neural probability was not rejected: " ++ show result)

    putStrLn "PASS: neural categorical contracts"

requireRight :: (Show error) => String -> Either error value -> IO value
requireRight _ (Right value) = pure value
requireRight label (Left err) = failTest (label ++ ": " ++ show err)

closeVectors :: Double -> [Double] -> [Double] -> Bool
closeVectors tolerance expected actual =
    length expected == length actual
        && and (zipWith (\left right -> abs (left - right) <= tolerance) expected actual)

assert :: String -> Bool -> IO ()
assert _ True = pure ()
assert message False = failTest message

failTest :: String -> IO a
failTest message = do
    putStrLn ("FAIL: " ++ message)
    exitFailure
\end{code}
