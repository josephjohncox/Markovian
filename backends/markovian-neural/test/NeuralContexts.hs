{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- Inferred teaching wrappers, compiled in the existing neural test component.
module NeuralContexts where

import Markovian.Backend.Neural.ActorCritic
import Markovian.Backend.Neural.DQN
import Markovian.Backend.Neural.Dense
import Markovian.Backend.Neural.Information
import Markovian.Backend.Neural.Optimizer
import Markovian.Backend.Neural.Reinforce
import Markovian.Backend.Neural.Replay

-- Preserve the named intermediate bindings shown in the book.
{-# ANN module ("HLint: ignore Redundant pure" :: String) #-}

networkExample inputSize hiddenSizes outputSize parameters features outputCotangent = do
    network <-
        mkDenseNetwork
            inputSize
            hiddenSizes
            outputSize
            parameters
    output <- denseForward network features
    parameterVJP <- denseParameterVJP network features outputCotangent
    inputVJP <- denseInputVJP network features outputCotangent
    pure (output, parameterVJP, inputVJP)

informationExample logits sourceLogits targetLogits predictionLogits = do
    entropy <- entropyFromLogits logits
    divergence <- klDivergenceFromLogits sourceLogits targetLogits
    gradient <-
        crossEntropyPredictionGradient targetLogits predictionLogits
    pure (entropy, divergence, gradient)

reinforceExample config policy baseline episodeSteps terminalPayoff = do
    update <-
        updateReinforce
            config
            policy
            (Just baseline)
            episodeSteps
            (TerminalBoundary terminalPayoff)
    pure update

actorCriticExample config policy valueFunction transition = do
    update <-
        updateActorCritic
            config
            policy
            valueFunction
            transition
    pure update

replayExample capacity transition requestedIds = do
    buffer0 <- mkReplayBuffer capacity
    let (entryId, buffer1) = appendReplay transition buffer0
    batch <- selectReplay WithoutReplacement requestedIds buffer1
    pure (entryId, batch)

dqnExample onlineNetwork targetNetwork config transitions = do
    state <- mkDQNState onlineNetwork targetNetwork
    update <- updateDQNBatch config state transitions
    pure update

trainingExample network features outputCotangent optimizer = do
    prediction <- either (fail . show) pure (denseForward network features)
    parameterCotangent <- either (fail . show) pure (denseParameterVJP network features outputCotangent)
    inputCotangent <- either (fail . show) pure (denseInputVJP network features outputCotangent)
    updated <- either (fail . show) pure (applySGD optimizer parameterCotangent network)
    pure (prediction, inputCotangent, updated)

entropyExample logits targetLogits predictionLogits leftSize rightSize jointLogits = do
    entropy <- entropyFromLogits logits
    crossEntropy <- crossEntropyFromLogits targetLogits predictionLogits
    divergence <- klDivergenceFromLogits targetLogits predictionLogits
    mutualInformation <-
        mutualInformationFromJointLogits leftSize rightSize jointLogits
    dEntropy <- entropyLogitGradient logits
    dCrossEntropy <-
        crossEntropyPredictionGradient targetLogits predictionLogits
    pure (entropy, crossEntropy, divergence, mutualInformation, dEntropy, dCrossEntropy)

pullExample network input outputCotangent = do
    inputCotangent <- denseInputVJP network input outputCotangent
    parameterCotangent <- denseParameterVJP network input outputCotangent
    pure (inputCotangent, parameterCotangent)
