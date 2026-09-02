{- | Seeded, bounded episodic Expected SARSA.

Behavior actions are sampled from the canonical epsilon-greedy distribution.
The continuing target uses the expectation under that same distribution, not
the sampled behavior action and not a greedy Q-learning maximum.
-}
module Markovian.Learning.ExpectedSarsa.Episodic (
    ExpectedSarsaConfig,
    expectedSarsaConfig,
    expectedSarsaDiscount,
    expectedSarsaLearningRateSchedule,
    expectedSarsaExplorationSchedule,
    expectedSarsaEpisodeLimit,
    expectedSarsaEpisodeStepLimit,
    ExpectedSarsaLearningStep (..),
    ExpectedSarsaEpisode (..),
    ExpectedSarsaLearningResult (..),
    EpisodicExpectedSarsaError (..),
    learnExpectedSarsaEpisodes,
    learnExpectedSarsaEpisodesFrom,
) where

import Markovian.Action (ActionId)
import Markovian.Horizon (Horizon, horizonValue)
import Markovian.Interpreter.Sampled.Step (SampledStepError, sampleMDPStep)
import Markovian.Learning.EpsilonGreedy (EpsilonGreedyError, sampleEpsilonGreedy)
import Markovian.Learning.ExpectedSarsa
import Markovian.Learning.Tabular
import Markovian.MDP (Decision (..), MDP, ModelError, inspectMDP, mdpInitialState)
import Markovian.Objective (Discount, discountValue)
import Markovian.Reward (Reward, RewardError, mkReward, rewardValue)
import Markovian.Sampling (Generator)
import Markovian.Trace (StopReason (..), Trace (..), TraceStep (..))
import Numeric.Natural (Natural)

-- | Bounded epsilon-greedy Expected SARSA configuration.
data ExpectedSarsaConfig = ExpectedSarsaConfig
    { expectedSarsaDiscount :: !Discount
    -- ^ Discount used by the expected action-value target.
    , expectedSarsaLearningRateSchedule :: !LearningRateSchedule
    -- ^ Learning rate indexed by the global update count.
    , expectedSarsaExplorationSchedule :: !ExplorationSchedule
    -- ^ Epsilon shared by behavior and target distributions.
    , expectedSarsaEpisodeLimit :: !Horizon
    -- ^ Number of episodes performed by one runner call.
    , expectedSarsaEpisodeStepLimit :: !Horizon
    -- ^ Maximum sampled transitions in each episode.
    }
    deriving (Eq, Show)

-- | Construct a bounded Expected SARSA configuration.
expectedSarsaConfig ::
    Discount -> LearningRateSchedule -> ExplorationSchedule -> Horizon -> Horizon -> ExpectedSarsaConfig
expectedSarsaConfig = ExpectedSarsaConfig

-- | One realized transition and its Expected SARSA update.
data ExpectedSarsaLearningStep state action = ExpectedSarsaLearningStep
    { expectedSarsaTraceStep :: !(TraceStep state (ActionId action) Reward)
    , expectedSarsaStepUpdate :: !(ExpectedSarsaUpdateResult state action)
    }
    deriving (Eq, Show)

-- | One bounded Expected SARSA episode.
data ExpectedSarsaEpisode state action = ExpectedSarsaEpisode
    { expectedSarsaEpisodeIndex :: !Natural
    , expectedSarsaEpisodeTrace :: !(Trace state (ActionId action) Reward)
    , expectedSarsaEpisodeReturn :: !Reward
    , expectedSarsaEpisodeSteps :: ![ExpectedSarsaLearningStep state action]
    }
    deriving (Eq, Show)

-- | Final table, history, counters, and resumable generator.
data ExpectedSarsaLearningResult state action = ExpectedSarsaLearningResult
    { learnedExpectedSarsaTable :: !(QTable state action)
    , learnedExpectedSarsaEpisodes :: ![ExpectedSarsaEpisode state action]
    , learnedExpectedSarsaUpdateCount :: !Natural
    , learnedExpectedSarsaGenerator :: !Generator
    }
    deriving (Eq, Show)

-- | Failures from bounded Expected SARSA execution.
data EpisodicExpectedSarsaError action
    = EpisodicExpectedSarsaModelError !(ModelError action)
    | EpisodicExpectedSarsaBehaviorError !(EpsilonGreedyError action)
    | EpisodicExpectedSarsaStepError !(SampledStepError action)
    | EpisodicExpectedSarsaUpdateError !(ExpectedSarsaUpdateError action)
    | EpisodicExpectedSarsaArithmeticError !RewardError
    deriving (Eq, Show)

-- | Learn from an empty action-value table.
learnExpectedSarsaEpisodes ::
    (Eq state, Eq action) =>
    ExpectedSarsaConfig ->
    MDP state action ->
    Generator ->
    Either (EpisodicExpectedSarsaError action) (ExpectedSarsaLearningResult state action)
learnExpectedSarsaEpisodes config model =
    learnExpectedSarsaEpisodesFrom config model emptyQTable 0 0

-- | Resume from explicit table, episode index, update count, and generator.
learnExpectedSarsaEpisodesFrom ::
    (Eq state, Eq action) =>
    ExpectedSarsaConfig ->
    MDP state action ->
    QTable state action ->
    Natural ->
    Natural ->
    Generator ->
    Either (EpisodicExpectedSarsaError action) (ExpectedSarsaLearningResult state action)
learnExpectedSarsaEpisodesFrom config model initialTable initialEpisode initialCount initialGenerator =
    runEpisodes initialEpisode (horizonValue (expectedSarsaEpisodeLimit config)) initialCount initialTable initialGenerator []
  where
    runEpisodes episode remaining count table generator reversed
        | remaining == 0 = Right (ExpectedSarsaLearningResult table (reverse reversed) count generator)
        | otherwise = do
            (finished, nextTable, nextGenerator, nextCount) <- runEpisode episode count table generator
            runEpisodes (episode + 1) (remaining - 1) nextCount nextTable nextGenerator (finished : reversed)

    runEpisode episode count table generator =
        go (horizonValue (expectedSarsaEpisodeStepLimit config)) (mdpInitialState model) generator table count 1 0 [] []
      where
        epsilon = explorationRateAt (expectedSarsaExplorationSchedule config) episode

        go remaining state currentGenerator currentTable currentCount discountPower accumulated reversedTrace reversedUpdates = do
            decision <- mapModel (inspectMDP model state)
            case decision of
                TerminalDecision payoff -> do
                    total <- checkedReward (accumulated + discountPower * rewardValue payoff)
                    let trace = Trace (reverse reversedTrace) state (TerminalStop payoff)
                    Right
                        ( ExpectedSarsaEpisode episode trace total (reverse reversedUpdates)
                        , currentTable
                        , currentGenerator
                        , currentCount
                        )
                ActionDecision available
                    | remaining == 0 -> do
                        total <- checkedReward accumulated
                        let trace = Trace (reverse reversedTrace) state HorizonStop
                        Right
                            ( ExpectedSarsaEpisode episode trace total (reverse reversedUpdates)
                            , currentTable
                            , currentGenerator
                            , currentCount
                            )
                    | otherwise -> do
                        (selected, afterAction) <- choose epsilon currentTable state available currentGenerator
                        (traceStep, afterTransition) <- mapStep (sampleMDPStep model state selected afterAction)
                        let reward = traceTransitionReward traceStep
                            successor = traceSuccessorState traceStep
                            observed = ObservedTransition state selected reward successor
                        successorDecision <- mapModel (inspectMDP model successor)
                        let bootstrap =
                                case successorDecision of
                                    TerminalDecision _ -> ExpectedSarsaTerminal
                                    ActionDecision _ -> ExpectedSarsaContinuing epsilon
                            rate = learningRateAt (expectedSarsaLearningRateSchedule config) currentCount
                        updated <-
                            mapUpdate
                                ( updateExpectedSarsa
                                    rate
                                    (expectedSarsaDiscount config)
                                    model
                                    observed
                                    bootstrap
                                    currentTable
                                )
                        let nextAccumulated = accumulated + discountPower * rewardValue reward
                            nextPower = discountPower * discountValue (expectedSarsaDiscount config)
                        _ <- checkedReward nextAccumulated
                        go
                            (remaining - 1)
                            successor
                            afterTransition
                            (expectedSarsaUpdateTable updated)
                            (currentCount + 1)
                            nextPower
                            nextAccumulated
                            (traceStep : reversedTrace)
                            (ExpectedSarsaLearningStep traceStep updated : reversedUpdates)

    choose epsilon table state available generator =
        either
            (Left . EpisodicExpectedSarsaBehaviorError)
            Right
            (sampleEpsilonGreedy epsilon table state available generator)

    checkedReward value = either (Left . EpisodicExpectedSarsaArithmeticError) Right (mkReward value)

mapModel :: Either (ModelError action) value -> Either (EpisodicExpectedSarsaError action) value
mapModel = either (Left . EpisodicExpectedSarsaModelError) Right

mapStep :: Either (SampledStepError action) value -> Either (EpisodicExpectedSarsaError action) value
mapStep = either (Left . EpisodicExpectedSarsaStepError) Right

mapUpdate :: Either (ExpectedSarsaUpdateError action) value -> Either (EpisodicExpectedSarsaError action) value
mapUpdate = either (Left . EpisodicExpectedSarsaUpdateError) Right
