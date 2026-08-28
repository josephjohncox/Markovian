-- | Exact finite-horizon dynamic programming over closed compiled policies.
module Markovian.Interpreter.DynamicProgramming.Exact (
    ExactFiniteDPError (..),
    ExactFiniteDPReport (..),
    evaluateCompiledExactFinite,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Compile.Exact (
    CompiledExactMRP,
    CompiledExactMRPState (..),
    CompiledExactOutcome (..),
    CompiledExactStep (..),
    CompiledRuntimeError,
    StateIndex,
    compiledMRPInitialState,
    compiledMRPStateEntries,
    stepCompiledExactMRP,
 )
import Markovian.Horizon (horizonValue)
import Markovian.Objective.Exact (
    ExactFiniteObjective,
    exactDiscountValue,
    exactObjectiveDiscount,
    exactObjectiveHorizon,
 )
import Markovian.Probability.Exact (exactOutcomes, exactProbability)
import Markovian.Reward.Exact (ExactReward, exactReward, exactRewardValue)
import Numeric.Natural (Natural)

-- | Failures from exact finite-horizon dynamic programming.
data ExactFiniteDPError
    = ExactFiniteDPRuntimeError !CompiledRuntimeError
    | MissingExactFiniteDPValue !StateIndex
    deriving (Eq, Show)

-- | Exact values after a reported number of Bellman backups.
data ExactFiniteDPReport = ExactFiniteDPReport
    { exactFiniteDPObjective :: !ExactFiniteObjective
    , exactFiniteDPIterations :: !Natural
    , exactFiniteDPValues :: !(NonEmpty (StateIndex, ExactReward))
    , exactFiniteDPInitialValue :: !ExactReward
    }
    deriving (Eq, Show)

{- | Evaluate a closed compiled exact policy by backward finite-horizon induction.

Iteration zero assigns terminal payoffs to terminal states and zero to
continuing states. Each later iteration performs one Bellman expectation
backup while keeping terminal values clamped to their payoffs.
-}
evaluateCompiledExactFinite ::
    ExactFiniteObjective ->
    CompiledExactMRP state ->
    Either ExactFiniteDPError ExactFiniteDPReport
evaluateCompiledExactFinite objective compiled = do
    values <- iterateValues iterations initialValues
    initialResult <- requireValue (compiledMRPInitialState compiled) values
    Right
        ExactFiniteDPReport
            { exactFiniteDPObjective = objective
            , exactFiniteDPIterations = iterations
            , exactFiniteDPValues = values
            , exactFiniteDPInitialValue = initialResult
            }
  where
    iterations = horizonValue (exactObjectiveHorizon objective)
    discount = exactDiscountValue (exactObjectiveDiscount objective)
    entries = compiledMRPStateEntries compiled
    initialValues = fmap baseValue entries

    baseValue (index, state) =
        case state of
            CompiledMRPTerminalState _ payoff -> (index, payoff)
            CompiledMRPContinuingState{} -> (index, exactReward 0)

    iterateValues 0 values = Right values
    iterateValues remaining values = do
        updated <- traverse (backup values) entries
        iterateValues (remaining - 1) updated

    backup previous (index, state) =
        case state of
            CompiledMRPTerminalState _ payoff -> Right (index, payoff)
            CompiledMRPContinuingState{} -> do
                step <- mapRuntimeError (stepCompiledExactMRP compiled index)
                case step of
                    CompiledExactTerminalStep payoff -> Right (index, payoff)
                    CompiledExactTransitionStep distribution -> do
                        contributions <-
                            traverse
                                (outcomeContribution previous)
                                (NonEmpty.toList (exactOutcomes distribution))
                        Right (index, exactReward (sum contributions))

    outcomeContribution previous (outcome, mass) = do
        future <- requireValue (compiledSuccessorState outcome) previous
        Right
            ( exactProbability mass
                * ( exactRewardValue (compiledTransitionReward outcome)
                        + discount * exactRewardValue future
                  )
            )

requireValue ::
    StateIndex ->
    NonEmpty (StateIndex, ExactReward) ->
    Either ExactFiniteDPError ExactReward
requireValue requested values =
    case lookup requested (NonEmpty.toList values) of
        Nothing -> Left (MissingExactFiniteDPValue requested)
        Just value -> Right value

mapRuntimeError :: Either CompiledRuntimeError value -> Either ExactFiniteDPError value
mapRuntimeError = either (Left . ExactFiniteDPRuntimeError) Right
