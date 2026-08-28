-- | Seeded, bounded episodic tabular Q-learning.
module Markovian.Learning.QLearning.Episodic (
    QLearningStep (..),
    QLearningEpisode (..),
    QLearningResult (..),
    EpisodicQLearningError (..),
    learnEpisodes,
    learnEpisodesFrom,
) where

import Markovian.Horizon (horizonValue)
import Markovian.Interpreter.Sampled.Step (SampledStepError (..), sampleMDPStep)
import Markovian.Learning.EpsilonGreedy (EpsilonGreedyError (..), sampleEpsilonGreedy)
import Markovian.Learning.QLearning (
    ObservedTransition (..),
    QLearningConfig,
    QTable,
    QUpdateError,
    QUpdateResult (..),
    emptyQTable,
    explorationRateAt,
    learningRateAt,
    qEpisodeLimit,
    qEpisodeStepLimit,
    qExplorationSchedule,
    qLearningDiscount,
    qLearningRateSchedule,
    updateQ,
 )
import Markovian.MDP (
    ActionId,
    Decision (..),
    MDP,
    ModelError (..),
    inspectMDP,
    mdpInitialState,
 )
import Markovian.Objective (discountValue)
import Markovian.Probability (DistributionError)
import Markovian.Reward (Reward, RewardError, mkReward, rewardValue)
import Markovian.Sampling (
    Generator,
    SamplingError,
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
    | EpisodicExplorationUnderflow !Double !Int
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
learnEpisodes config model = learnEpisodesFrom config model emptyQTable 0 0

{- | Resume learning from an explicit table, episode index, update count, and
owned generator. The configured episode limit counts episodes in this call.
-}
learnEpisodesFrom ::
    (Eq state, Eq action) =>
    QLearningConfig ->
    MDP state action ->
    QTable state action ->
    Natural ->
    Natural ->
    Generator ->
    Either (EpisodicQLearningError action) (QLearningResult state action)
learnEpisodesFrom config model initialTable initialEpisodeIndex initialUpdateCount initialGenerator =
    runEpisodes
        initialEpisodeIndex
        (horizonValue (qEpisodeLimit config))
        initialUpdateCount
        initialTable
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
                        (traceStep, afterTransition) <-
                            mapStepError (sampleMDPStep model state selected afterAction)
                        let reward = traceTransitionReward traceStep
                            successor = traceSuccessorState traceStep
                            observed = ObservedTransition state selected reward successor
                            rate = learningRateAt (qLearningRateSchedule config) currentUpdate
                        updated <-
                            mapUpdateError
                                (updateQ rate (qLearningDiscount config) model observed currentTable)
                        let nextAccumulated = accumulated + discountPower * rewardValue reward
                            nextPower = discountPower * discountValue (qLearningDiscount config)
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
        mapBehaviorError
            ( sampleEpsilonGreedy
                (explorationRateAt (qExplorationSchedule config) episodeIndex)
                table
                state
                available
                generator
            )

    validatedReward value =
        case mkReward value of
            Left err -> Left (EpisodicArithmeticError err)
            Right reward -> Right reward

mapModelError :: Either (ModelError action) value -> Either (EpisodicQLearningError action) value
mapModelError = either (Left . EpisodicModelError) Right

mapUpdateError :: Either (QUpdateError action) value -> Either (EpisodicQLearningError action) value
mapUpdateError = either (Left . EpisodicUpdateError) Right

mapBehaviorError :: Either (EpsilonGreedyError action) value -> Either (EpisodicQLearningError action) value
mapBehaviorError = either convert Right
  where
    convert (DuplicateEpsilonGreedyAction duplicate) = Left (EpisodicModelError (DuplicateAvailableAction duplicate))
    convert (PositiveExplorationMassUnderflow epsilon count) = Left (EpisodicExplorationUnderflow epsilon count)
    convert (EpsilonGreedyDistributionError err) = Left (EpisodicDistributionError err)
    convert (EpsilonGreedySamplingError err) = Left (EpisodicSamplingError err)

mapStepError :: Either (SampledStepError action) value -> Either (EpisodicQLearningError action) value
mapStepError = either convert Right
  where
    convert (SampledStepModelError err) = Left (EpisodicModelError err)
    convert (SampledStepSamplingError err) = Left (EpisodicSamplingError err)
