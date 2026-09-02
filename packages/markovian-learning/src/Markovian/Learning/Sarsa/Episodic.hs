{- | Seeded, bounded episodic SARSA with carried on-policy actions.

At every continuing successor, the epsilon-greedy behavior action @A'@ is
selected before the current update. That same action appears in the SARSA
target and is carried into the next sampled step.
-}
module Markovian.Learning.Sarsa.Episodic (
    SarsaConfig,
    sarsaConfig,
    sarsaDiscount,
    sarsaLearningRateSchedule,
    sarsaExplorationSchedule,
    sarsaEpisodeLimit,
    sarsaEpisodeStepLimit,
    SarsaLearningStep (..),
    SarsaEpisode (..),
    SarsaLearningResult (..),
    EpisodicSarsaError (..),
    learnSarsaEpisodes,
    learnSarsaEpisodesFrom,
) where

import Markovian.Action (ActionId)
import Markovian.Horizon (Horizon, horizonValue)
import Markovian.Interpreter.Sampled.Step (SampledStepError, sampleMDPStep)
import Markovian.Learning.EpsilonGreedy (EpsilonGreedyError, sampleEpsilonGreedy)
import Markovian.Learning.Sarsa
import Markovian.Learning.Tabular
import Markovian.MDP (Decision (..), MDP, ModelError, inspectMDP, mdpInitialState)
import Markovian.Objective (Discount, discountValue)
import Markovian.Reward (Reward, RewardError, mkReward, rewardValue)
import Markovian.Sampling (Generator)
import Markovian.Trace (StopReason (..), Trace (..), TraceStep (..))
import Numeric.Natural (Natural)

-- | Bounded epsilon-greedy SARSA configuration.
data SarsaConfig = SarsaConfig
    { sarsaDiscount :: !Discount
    -- ^ Discount used by the sampled next-action target.
    , sarsaLearningRateSchedule :: !LearningRateSchedule
    -- ^ Learning rate indexed by the global update count.
    , sarsaExplorationSchedule :: !ExplorationSchedule
    -- ^ Epsilon used by the on-policy behavior distribution.
    , sarsaEpisodeLimit :: !Horizon
    -- ^ Number of episodes performed by one runner call.
    , sarsaEpisodeStepLimit :: !Horizon
    -- ^ Maximum sampled transitions in each episode.
    }
    deriving (Eq, Show)

-- | Construct a bounded SARSA configuration.
sarsaConfig :: Discount -> LearningRateSchedule -> ExplorationSchedule -> Horizon -> Horizon -> SarsaConfig
sarsaConfig = SarsaConfig

-- | One realized transition and its carried-action SARSA update.
data SarsaLearningStep state action = SarsaLearningStep
    { sarsaTraceStep :: !(TraceStep state (ActionId action) Reward)
    , sarsaStepUpdate :: !(SarsaUpdateResult state action)
    }
    deriving (Eq, Show)

-- | One bounded SARSA episode.
data SarsaEpisode state action = SarsaEpisode
    { sarsaEpisodeIndex :: !Natural
    , sarsaEpisodeTrace :: !(Trace state (ActionId action) Reward)
    , sarsaEpisodeReturn :: !Reward
    , sarsaEpisodeSteps :: ![SarsaLearningStep state action]
    }
    deriving (Eq, Show)

-- | Final table, history, counters, and resumable generator.
data SarsaLearningResult state action = SarsaLearningResult
    { learnedSarsaTable :: !(QTable state action)
    , learnedSarsaEpisodes :: ![SarsaEpisode state action]
    , learnedSarsaUpdateCount :: !Natural
    , learnedSarsaGenerator :: !Generator
    }
    deriving (Eq, Show)

-- | Failures from bounded SARSA execution.
data EpisodicSarsaError action
    = EpisodicSarsaModelError !(ModelError action)
    | EpisodicSarsaBehaviorError !(EpsilonGreedyError action)
    | EpisodicSarsaStepError !(SampledStepError action)
    | EpisodicSarsaUpdateError !(SarsaUpdateError action)
    | EpisodicSarsaArithmeticError !RewardError
    | EpisodicSarsaCarriedActionUnavailable !(ActionId action)
    deriving (Eq, Show)

-- | Learn from an empty action-value table.
learnSarsaEpisodes ::
    (Eq state, Eq action) =>
    SarsaConfig ->
    MDP state action ->
    Generator ->
    Either (EpisodicSarsaError action) (SarsaLearningResult state action)
learnSarsaEpisodes config model = learnSarsaEpisodesFrom config model emptyQTable 0 0

-- | Resume SARSA from explicit table, episode index, update count, and generator.
learnSarsaEpisodesFrom ::
    (Eq state, Eq action) =>
    SarsaConfig ->
    MDP state action ->
    QTable state action ->
    Natural ->
    Natural ->
    Generator ->
    Either (EpisodicSarsaError action) (SarsaLearningResult state action)
learnSarsaEpisodesFrom config model initialTable initialEpisode initialCount initialGenerator =
    runEpisodes initialEpisode (horizonValue (sarsaEpisodeLimit config)) initialCount initialTable initialGenerator []
  where
    runEpisodes episode remaining count table generator reversed
        | remaining == 0 = Right (SarsaLearningResult table (reverse reversed) count generator)
        | otherwise = do
            (finished, nextTable, nextGenerator, nextCount) <- runEpisode episode count table generator
            runEpisodes (episode + 1) (remaining - 1) nextCount nextTable nextGenerator (finished : reversed)

    runEpisode episode count table generator =
        go (horizonValue (sarsaEpisodeStepLimit config)) (mdpInitialState model) Nothing generator table count 1 0 [] []
      where
        epsilon = explorationRateAt (sarsaExplorationSchedule config) episode

        go remaining state carried currentGenerator currentTable currentCount discountPower accumulated reversedTrace reversedUpdates = do
            decision <- mapModel (inspectMDP model state)
            case decision of
                TerminalDecision payoff -> do
                    total <- checkedReward (accumulated + discountPower * rewardValue payoff)
                    let trace = Trace (reverse reversedTrace) state (TerminalStop payoff)
                    Right
                        ( SarsaEpisode episode trace total (reverse reversedUpdates)
                        , currentTable
                        , currentGenerator
                        , currentCount
                        )
                ActionDecision available
                    | remaining == 0 -> do
                        total <- checkedReward accumulated
                        let trace = Trace (reverse reversedTrace) state HorizonStop
                        Right
                            ( SarsaEpisode episode trace total (reverse reversedUpdates)
                            , currentTable
                            , currentGenerator
                            , currentCount
                            )
                    | otherwise -> do
                        (selected, afterCurrentAction) <-
                            case carried of
                                Just selected
                                    | selected `elem` available -> Right (selected, currentGenerator)
                                    | otherwise -> Left (EpisodicSarsaCarriedActionUnavailable selected)
                                Nothing -> choose epsilon currentTable state available currentGenerator
                        (traceStep, afterTransition) <- mapStep (sampleMDPStep model state selected afterCurrentAction)
                        let reward = traceTransitionReward traceStep
                            successor = traceSuccessorState traceStep
                            observed = ObservedTransition state selected reward successor
                        successorDecision <- mapModel (inspectMDP model successor)
                        (bootstrap, nextCarried, afterNextAction) <-
                            case successorDecision of
                                TerminalDecision _ -> Right (SarsaTerminal, Nothing, afterTransition)
                                ActionDecision successorActions -> do
                                    (nextAction, nextGenerator) <-
                                        choose epsilon currentTable successor successorActions afterTransition
                                    Right (SarsaNextAction nextAction, Just nextAction, nextGenerator)
                        let rate = learningRateAt (sarsaLearningRateSchedule config) currentCount
                        updated <-
                            mapUpdate
                                (updateSarsa rate (sarsaDiscount config) model observed bootstrap currentTable)
                        let nextAccumulated = accumulated + discountPower * rewardValue reward
                            nextPower = discountPower * discountValue (sarsaDiscount config)
                        _ <- checkedReward nextAccumulated
                        go
                            (remaining - 1)
                            successor
                            nextCarried
                            afterNextAction
                            (sarsaUpdateTable updated)
                            (currentCount + 1)
                            nextPower
                            nextAccumulated
                            (traceStep : reversedTrace)
                            (SarsaLearningStep traceStep updated : reversedUpdates)

    choose epsilon table state available generator =
        either (Left . EpisodicSarsaBehaviorError) Right (sampleEpsilonGreedy epsilon table state available generator)

    checkedReward value = either (Left . EpisodicSarsaArithmeticError) Right (mkReward value)

mapModel :: Either (ModelError action) value -> Either (EpisodicSarsaError action) value
mapModel = either (Left . EpisodicSarsaModelError) Right

mapStep :: Either (SampledStepError action) value -> Either (EpisodicSarsaError action) value
mapStep = either (Left . EpisodicSarsaStepError) Right

mapUpdate :: Either (SarsaUpdateError action) value -> Either (EpisodicSarsaError action) value
mapUpdate = either (Left . EpisodicSarsaUpdateError) Right
