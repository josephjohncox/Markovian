{- | Seeded, bounded episodic TD(0) evaluation of a supplied behavior policy.

The supplied policy is both the behavior policy and the policy whose state
values are learned. No greedy target policy is introduced.
-}
module Markovian.Learning.TD0.Episodic (
    TD0Config,
    td0Config,
    td0Discount,
    td0LearningRateSchedule,
    td0EpisodeLimit,
    td0EpisodeStepLimit,
    TD0LearningStep (..),
    TD0Episode (..),
    TD0LearningResult (..),
    EpisodicTD0Error (..),
    learnTD0Episodes,
    learnTD0EpisodesFrom,
) where

import Markovian.Action (ActionId)
import Markovian.Horizon (Horizon, horizonValue)
import Markovian.Interpreter.Sampled.Step (SampledStepError (..), sampleMDPStep)
import Markovian.Learning.TD0 (TD0UpdateError, TD0UpdateResult (..), updateTD0)
import Markovian.Learning.Tabular
import Markovian.MDP (Decision (..), MDP, ModelError, inspectMDP, mdpInitialState)
import Markovian.Objective (Discount, discountValue)
import Markovian.Policy (Policy, PolicyError, policyActions, validatePolicySupport)
import Markovian.Reward (Reward, RewardError, mkReward, rewardValue)
import Markovian.Sampling (Generator, SamplingError, sampleFiniteDist)
import Markovian.Trace (StopReason (..), Trace (..), TraceStep (..))
import Numeric.Natural (Natural)

-- | Bounded TD(0) configuration. The behavior policy is supplied separately.
data TD0Config = TD0Config
    { td0Discount :: !Discount
    -- ^ Discount used by the state-value target.
    , td0LearningRateSchedule :: !LearningRateSchedule
    -- ^ Learning rate indexed by the global update count.
    , td0EpisodeLimit :: !Horizon
    -- ^ Number of episodes performed by one runner call.
    , td0EpisodeStepLimit :: !Horizon
    -- ^ Maximum sampled transitions in each episode.
    }
    deriving (Eq, Show)

-- | Construct a bounded TD(0) configuration.
td0Config :: Discount -> LearningRateSchedule -> Horizon -> Horizon -> TD0Config
td0Config = TD0Config

-- | One realized transition and its state-value update.
data TD0LearningStep state action = TD0LearningStep
    { td0TraceStep :: !(TraceStep state (ActionId action) Reward)
    , td0StepUpdate :: !(TD0UpdateResult state)
    }
    deriving (Eq, Show)

-- | One bounded TD(0) episode.
data TD0Episode state action = TD0Episode
    { td0EpisodeIndex :: !Natural
    , td0EpisodeTrace :: !(Trace state (ActionId action) Reward)
    , td0EpisodeReturn :: !Reward
    , td0EpisodeSteps :: ![TD0LearningStep state action]
    }
    deriving (Eq, Show)

-- | Final state-value table, episode history, counters, and generator.
data TD0LearningResult state action = TD0LearningResult
    { learnedVTable :: !(VTable state)
    , learnedTD0Episodes :: ![TD0Episode state action]
    , learnedTD0UpdateCount :: !Natural
    , learnedTD0Generator :: !Generator
    }
    deriving (Eq, Show)

-- | Failures from bounded TD(0) execution.
data EpisodicTD0Error action
    = EpisodicTD0ModelError !(ModelError action)
    | EpisodicTD0PolicyError !(PolicyError action)
    | EpisodicTD0SamplingError !SamplingError
    | EpisodicTD0StepError !(SampledStepError action)
    | EpisodicTD0UpdateError !(TD0UpdateError action)
    | EpisodicTD0ArithmeticError !RewardError
    deriving (Eq, Show)

-- | Learn from an empty state-value table.
learnTD0Episodes ::
    (Eq state, Eq action) =>
    TD0Config ->
    MDP state action ->
    Policy state action ->
    Generator ->
    Either (EpisodicTD0Error action) (TD0LearningResult state action)
learnTD0Episodes config model selectedPolicy =
    learnTD0EpisodesFrom config model selectedPolicy emptyVTable 0 0

-- | Resume TD(0) from explicit table, episode index, update count, and generator.
learnTD0EpisodesFrom ::
    (Eq state, Eq action) =>
    TD0Config ->
    MDP state action ->
    Policy state action ->
    VTable state ->
    Natural ->
    Natural ->
    Generator ->
    Either (EpisodicTD0Error action) (TD0LearningResult state action)
learnTD0EpisodesFrom config model selectedPolicy initialTable initialEpisode initialCount initialGenerator =
    runEpisodes initialEpisode (horizonValue (td0EpisodeLimit config)) initialCount initialTable initialGenerator []
  where
    runEpisodes episode remaining count table generator reversed
        | remaining == 0 =
            Right (TD0LearningResult table (reverse reversed) count generator)
        | otherwise = do
            (finished, nextTable, nextGenerator, nextCount) <- runEpisode episode count table generator
            runEpisodes (episode + 1) (remaining - 1) nextCount nextTable nextGenerator (finished : reversed)

    runEpisode episode count table generator =
        go (horizonValue (td0EpisodeStepLimit config)) (mdpInitialState model) generator table count 1 0 [] []
      where
        go remaining state currentGenerator currentTable currentCount discountPower accumulated reversedTrace reversedUpdates = do
            decision <- mapModel (inspectMDP model state)
            case decision of
                TerminalDecision payoff -> do
                    total <- checkedReward (accumulated + discountPower * rewardValue payoff)
                    let trace = Trace (reverse reversedTrace) state (TerminalStop payoff)
                    Right
                        ( TD0Episode episode trace total (reverse reversedUpdates)
                        , currentTable
                        , currentGenerator
                        , currentCount
                        )
                ActionDecision available
                    | remaining == 0 -> do
                        total <- checkedReward accumulated
                        let trace = Trace (reverse reversedTrace) state HorizonStop
                        Right
                            ( TD0Episode episode trace total (reverse reversedUpdates)
                            , currentTable
                            , currentGenerator
                            , currentCount
                            )
                    | otherwise -> do
                        let distribution = policyActions selectedPolicy state
                        mapPolicy (validatePolicySupport available distribution)
                        (selected, afterAction) <- mapSampling (sampleFiniteDist currentGenerator distribution)
                        (traceStep, afterTransition) <- mapStep (sampleMDPStep model state selected afterAction)
                        let reward = traceTransitionReward traceStep
                            successor = traceSuccessorState traceStep
                            observed = ObservedTransition state selected reward successor
                            rate = learningRateAt (td0LearningRateSchedule config) currentCount
                        updated <- mapUpdate (updateTD0 rate (td0Discount config) model observed currentTable)
                        let nextAccumulated = accumulated + discountPower * rewardValue reward
                            nextPower = discountPower * discountValue (td0Discount config)
                        _ <- checkedReward nextAccumulated
                        go
                            (remaining - 1)
                            successor
                            afterTransition
                            (td0UpdateTable updated)
                            (currentCount + 1)
                            nextPower
                            nextAccumulated
                            (traceStep : reversedTrace)
                            (TD0LearningStep traceStep updated : reversedUpdates)

    checkedReward value = either (Left . EpisodicTD0ArithmeticError) Right (mkReward value)

mapModel :: Either (ModelError action) value -> Either (EpisodicTD0Error action) value
mapModel = either (Left . EpisodicTD0ModelError) Right

mapPolicy :: Either (PolicyError action) value -> Either (EpisodicTD0Error action) value
mapPolicy = either (Left . EpisodicTD0PolicyError) Right

mapSampling :: Either SamplingError value -> Either (EpisodicTD0Error action) value
mapSampling = either (Left . EpisodicTD0SamplingError) Right

mapStep :: Either (SampledStepError action) value -> Either (EpisodicTD0Error action) value
mapStep = either (Left . EpisodicTD0StepError) Right

mapUpdate :: Either (TD0UpdateError action) value -> Either (EpisodicTD0Error action) value
mapUpdate = either (Left . EpisodicTD0UpdateError) Right
