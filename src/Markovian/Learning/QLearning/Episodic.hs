-- | Seeded, bounded episodic tabular Q-learning.
module Markovian.Learning.QLearning.Episodic (
    QLearningStep (..),
    QLearningEpisode (..),
    QLearningResult (..),
    EpisodicQLearningError (..),
    learnEpisodes,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Horizon (horizonValue)
import Markovian.Learning.QLearning (
    ObservedTransition (..),
    QLearningConfig,
    QTable,
    QUpdateError,
    QUpdateResult (..),
    emptyQTable,
    explorationRateAt,
    explorationRateValue,
    learningRateAt,
    qEpisodeLimit,
    qEpisodeStepLimit,
    qExplorationSchedule,
    qLearningDiscount,
    qLearningRateSchedule,
    qValue,
    qValueAt,
    updateQ,
 )
import Markovian.MDP (
    ActionId,
    Decision (..),
    MDP,
    ModelError,
    inspectMDP,
    mdpInitialState,
    stepMDP,
 )
import Markovian.MRP (successorState, transitionReward)
import Markovian.Objective (discountValue)
import Markovian.Probability (DistributionError, finiteDist)
import Markovian.Reward (Reward, RewardError, mkReward, rewardValue)
import Markovian.Sampling (
    Generator,
    SamplingError,
    sampleFiniteDist,
 )
import Markovian.Trace (
    StopReason (..),
    Trace (..),
    TraceStep (..),
 )
import Numeric.Natural (Natural)

-- | One realized transition and its pure Q-update result.
data QLearningStep state action = QLearningStep
    { qLearningTraceStep :: !(TraceStep state (ActionId action) Reward)
    , qLearningUpdate :: !(QUpdateResult state action)
    }
    deriving (Eq, Show)

-- | One bounded episode from the model's initial state.
data QLearningEpisode state action = QLearningEpisode
    { qLearningEpisodeIndex :: !Natural
    , qLearningEpisodeTrace :: !(Trace state (ActionId action) Reward)
    , qLearningEpisodeReturn :: !Reward
    , qLearningEpisodeSteps :: ![QLearningStep state action]
    }
    deriving (Eq, Show)

-- | Final table, deterministic episode history, and resumable generator.
data QLearningResult state action = QLearningResult
    { learnedQTable :: !(QTable state action)
    , learnedEpisodes :: ![QLearningEpisode state action]
    , learnedUpdateCount :: !Natural
    , learnedGenerator :: !Generator
    }
    deriving (Eq, Show)

-- | Distinct failures from bounded episodic learning.
data EpisodicQLearningError action
    = EpisodicModelError !(ModelError action)
    | EpisodicDistributionError !DistributionError
    | EpisodicSamplingError !SamplingError
    | EpisodicUpdateError !(QUpdateError action)
    | EpisodicArithmeticError !RewardError
    deriving (Eq, Show)

-- | Learn from an empty Q-table for the configured number of episodes.
learnEpisodes ::
    (Eq state, Eq action) =>
    QLearningConfig ->
    MDP state action ->
    Generator ->
    Either (EpisodicQLearningError action) (QLearningResult state action)
learnEpisodes config model initialGenerator =
    runEpisodes
        0
        (horizonValue (qEpisodeLimit config))
        0
        emptyQTable
        initialGenerator
        []
  where
    runEpisodes episodeIndex remaining updateCount table generator reversedEpisodes
        | remaining == 0 =
            Right
                QLearningResult
                    { learnedQTable = table
                    , learnedEpisodes = reverse reversedEpisodes
                    , learnedUpdateCount = updateCount
                    , learnedGenerator = generator
                    }
        | otherwise = do
            (episode, nextTable, nextGenerator, nextUpdateCount) <-
                runEpisode episodeIndex updateCount table generator
            runEpisodes
                (episodeIndex + 1)
                (remaining - 1)
                nextUpdateCount
                nextTable
                nextGenerator
                (episode : reversedEpisodes)

    runEpisode episodeIndex updateCount table generator =
        go
            (horizonValue (qEpisodeStepLimit config))
            (mdpInitialState model)
            generator
            table
            updateCount
            1
            0
            []
            []
      where
        go remaining state currentGenerator currentTable currentUpdate discountPower accumulated reversedTrace reversedUpdates = do
            decision <- mapModelError (inspectMDP model state)
            case decision of
                TerminalDecision payoff -> do
                    total <- validatedReward (accumulated + discountPower * rewardValue payoff)
                    let trace = Trace (reverse reversedTrace) state (TerminalStop payoff)
                        episode =
                            QLearningEpisode
                                { qLearningEpisodeIndex = episodeIndex
                                , qLearningEpisodeTrace = trace
                                , qLearningEpisodeReturn = total
                                , qLearningEpisodeSteps = reverse reversedUpdates
                                }
                    Right (episode, currentTable, currentGenerator, currentUpdate)
                ActionDecision available
                    | remaining == 0 -> do
                        total <- validatedReward accumulated
                        let trace = Trace (reverse reversedTrace) state HorizonStop
                            episode =
                                QLearningEpisode
                                    { qLearningEpisodeIndex = episodeIndex
                                    , qLearningEpisodeTrace = trace
                                    , qLearningEpisodeReturn = total
                                    , qLearningEpisodeSteps = reverse reversedUpdates
                                    }
                        Right (episode, currentTable, currentGenerator, currentUpdate)
                    | otherwise -> do
                        (selected, afterAction) <-
                            chooseAction
                                episodeIndex
                                state
                                available
                                currentTable
                                currentGenerator
                        transition <- mapModelError (stepMDP model state selected)
                        (outcome, afterTransition) <- mapSamplingError (sampleFiniteDist afterAction transition)
                        let reward = transitionReward outcome
                            successor = successorState outcome
                            observed = ObservedTransition state selected reward successor
                            rate = learningRateAt (qLearningRateSchedule config) currentUpdate
                        updated <-
                            mapUpdateError
                                (updateQ rate (qLearningDiscount config) model observed currentTable)
                        let nextAccumulated = accumulated + discountPower * rewardValue reward
                            nextPower = discountPower * discountValue (qLearningDiscount config)
                            traceStep = TraceStep selected reward successor
                            learningStep = QLearningStep traceStep updated
                        _ <- validatedReward nextAccumulated
                        go
                            (remaining - 1)
                            successor
                            afterTransition
                            (qUpdateTable updated)
                            (currentUpdate + 1)
                            nextPower
                            nextAccumulated
                            (traceStep : reversedTrace)
                            (learningStep : reversedUpdates)

    chooseAction episodeIndex state available table generator =
        let epsilon = explorationRateAt (qExplorationSchedule config) episodeIndex
         in chooseWithRate epsilon
      where
        chooseWithRate epsilon
            | explorationRateValue epsilon == 0 =
                Right (greedyAction table state available, generator)
            | explorationRateValue epsilon == 1 =
                sampleUniform available generator
            | otherwise = do
                exploreDistribution <-
                    mapDistributionError
                        ( finiteDist
                            [ (False, 1 - explorationRateValue epsilon)
                            , (True, explorationRateValue epsilon)
                            ]
                        )
                (explore, afterDecision) <-
                    mapSamplingError (sampleFiniteDist generator exploreDistribution)
                if explore
                    then sampleUniform available afterDecision
                    else Right (greedyAction table state available, afterDecision)

    sampleUniform available generator = do
        distribution <-
            mapDistributionError
                (finiteDist [(selected, 1) | selected <- NonEmpty.toList available])
        mapSamplingError (sampleFiniteDist generator distribution)

    validatedReward value =
        case mkReward value of
            Left err -> Left (EpisodicArithmeticError err)
            Right reward -> Right reward

greedyAction ::
    (Eq state, Eq action) =>
    QTable state action ->
    state ->
    NonEmpty (ActionId action) ->
    ActionId action
greedyAction table state (first :| remaining) = foldl choose first remaining
  where
    choose best candidate
        | qValue (qValueAt table state candidate) > qValue (qValueAt table state best) = candidate
        | otherwise = best

mapModelError :: Either (ModelError action) value -> Either (EpisodicQLearningError action) value
mapModelError = either (Left . EpisodicModelError) Right

mapDistributionError :: Either DistributionError value -> Either (EpisodicQLearningError action) value
mapDistributionError = either (Left . EpisodicDistributionError) Right

mapSamplingError :: Either SamplingError value -> Either (EpisodicQLearningError action) value
mapSamplingError = either (Left . EpisodicSamplingError) Right

mapUpdateError :: Either (QUpdateError action) value -> Either (EpisodicQLearningError action) value
mapUpdateError = either (Left . EpisodicUpdateError) Right
