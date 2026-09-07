{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- Inferred teaching parameters retain the public functions' constraints.
module LearningContexts where

import Markovian.Interpreter.Sampled
import Markovian.Interpreter.Sampled.Step
import Markovian.Learning.EpsilonGreedy
import Markovian.Learning.ExpectedSarsa
import Markovian.Learning.QLearning
import Markovian.Learning.QLearning.Episodic
import Markovian.Learning.Sarsa
import Markovian.Learning.TD0

-- Preserve the named intermediate bindings shown in the book.
{-# ANN module ("HLint: ignore Redundant pure" :: String) #-}

sampleExample objective model policy generator = do
    sample <- sampleReturn objective model policy generator
    pure (sampledReturn sample, sampledTrace sample, sampledGenerator sample)

stepExample model state selectedAction generator = do
    (traceStep, nextGenerator) <-
        sampleMDPStep model state selectedAction generator
    pure (traceStep, nextGenerator)

tdExample rate discount model observedTransition valueTable = do
    updated <-
        updateTD0 rate discount model observedTransition valueTable
    pure updated

sarsaExample rate discount model observedTransition nextAction table = do
    updated <-
        updateSarsa
            rate
            discount
            model
            observedTransition
            (SarsaNextAction nextAction)
            table
    pure updated

expectedSarsaExample rate discount model observedTransition epsilon table = do
    updated <-
        updateExpectedSarsa
            rate
            discount
            model
            observedTransition
            (ExpectedSarsaContinuing epsilon)
            table
    pure updated

qExample rate discount model observedTransition table = do
    updated <- updateQ rate discount model observedTransition table
    pure updated

epsilonExample epsilon table state availableActions = do
    distribution <-
        epsilonGreedyDistribution epsilon table state availableActions
    pure distribution

epsilonSampleExample epsilon table state availableActions generator = do
    (selected, nextGenerator) <-
        sampleEpsilonGreedy epsilon table state availableActions generator
    pure (selected, nextGenerator)

episodicExample discount alpha epsilon episodeLimit episodeStepLimit model generator = do
    let config =
            qLearningConfig
                discount
                (ConstantLearningRate alpha)
                (ConstantExploration epsilon)
                episodeLimit
                episodeStepLimit
    result <- learnEpisodes config model generator
    pure result

resumeExample config model previousTable nextEpisodeIndex previousUpdateCount previousGenerator = do
    resumed <-
        learnEpisodesFrom
            config
            model
            previousTable
            nextEpisodeIndex
            previousUpdateCount
            previousGenerator
    pure resumed
