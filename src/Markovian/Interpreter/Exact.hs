-- | Exact finite-support expectation for finite-horizon objectives.
module Markovian.Interpreter.Exact (
    ExactEvaluationError (..),
    ExactTraceResult (..),
    exactTraceDistribution,
    exactTraceDistributionFrom,
    expectedExactReturn,
    expectedExactReturnFrom,
) where

import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Horizon (horizonValue)
import Markovian.MDP (ActionId)
import Markovian.MDP.Exact (
    ExactDecision (..),
    ExactMDP,
    ExactModelError,
    ExactStateStatus (..),
    exactMDPInitialState,
    exactMDPStateStatus,
    exactSuccessorState,
    exactTransitionReward,
    inspectExactMDP,
    stepExactMDP,
 )
import Markovian.Objective.Exact (
    ExactFiniteObjective,
    exactDiscountValue,
    exactObjectiveDiscount,
    exactObjectiveHorizon,
 )
import Markovian.Policy.Exact (
    ExactPolicy,
    ExactPolicyError,
    exactPolicyActions,
    validateExactPolicySupport,
 )
import Markovian.Probability.Exact (
    ExactDistributionError,
    ExactFiniteDist,
    exactDirac,
    exactFiniteDist,
    exactOutcomes,
    exactProbability,
 )
import Markovian.Reward.Exact (
    ExactReward,
    exactReward,
    exactRewardValue,
 )
import Markovian.Trace (
    StopReason (..),
    Trace (..),
    TraceStep (..),
 )

-- | Errors returned by exact finite-horizon evaluation.
data ExactEvaluationError action
    = -- | The exact model rejected a requested state or action.
      ExactEvaluationModelError !(ExactModelError action)
    | -- | The exact policy failed support validation.
      ExactEvaluationPolicyError !(ExactPolicyError action)
    | -- | Exact trace branches could not be assembled.
      ExactEvaluationDistributionError !ExactDistributionError
    deriving (Eq, Show)

-- | One exact trace paired with its discounted finite return.
data ExactTraceResult state action = ExactTraceResult
    { exactTrace :: !(Trace state (ActionId action) ExactReward)
    , exactTraceReturn :: !ExactReward
    }
    deriving (Eq, Show)

-- | Enumerate exact bounded traces from the model's initial state.
exactTraceDistribution ::
    (Eq action) =>
    ExactFiniteObjective ->
    ExactMDP state action ->
    ExactPolicy state action ->
    Either (ExactEvaluationError action) (ExactFiniteDist (ExactTraceResult state action))
exactTraceDistribution objective model selectedPolicy =
    exactTraceDistributionFrom objective model selectedPolicy (exactMDPInitialState model)

-- | Enumerate exact bounded traces from one state.
exactTraceDistributionFrom ::
    (Eq action) =>
    ExactFiniteObjective ->
    ExactMDP state action ->
    ExactPolicy state action ->
    state ->
    Either (ExactEvaluationError action) (ExactFiniteDist (ExactTraceResult state action))
exactTraceDistributionFrom objective model selectedPolicy =
    go (horizonValue (exactObjectiveHorizon objective))
  where
    discount = exactDiscountValue (exactObjectiveDiscount objective)

    go remaining state =
        case exactMDPStateStatus model state of
            ExactTerminal payoff ->
                Right
                    ( exactDirac
                        (ExactTraceResult (Trace [] state (TerminalStop payoff)) payoff)
                    )
            ExactContinuing
                | remaining == 0 ->
                    Right
                        ( exactDirac
                            (ExactTraceResult (Trace [] state HorizonStop) (exactReward 0))
                        )
                | otherwise -> do
                    decision <- mapModelError (inspectExactMDP model state)
                    case decision of
                        ExactTerminalDecision payoff ->
                            Right
                                ( exactDirac
                                    (ExactTraceResult (Trace [] state (TerminalStop payoff)) payoff)
                                )
                        ExactActionDecision available -> do
                            let selected = exactPolicyActions selectedPolicy state
                            mapPolicyError (validateExactPolicySupport available selected)
                            branches <-
                                fmap
                                    concat
                                    ( traverse
                                        (actionTraceBranches (remaining - 1) state)
                                        (NonEmpty.toList (exactOutcomes selected))
                                    )
                            mapDistributionError (exactFiniteDist branches)

    actionTraceBranches remaining state (selectedAction, actionMass) = do
        transition <- mapModelError (stepExactMDP model state selectedAction)
        fmap
            concat
            ( traverse
                (outcomeTraceBranches remaining selectedAction actionMass)
                (NonEmpty.toList (exactOutcomes transition))
            )

    outcomeTraceBranches remaining selectedAction actionMass (outcome, outcomeMass) = do
        futureDistribution <- go remaining (exactSuccessorState outcome)
        let immediate = exactTransitionReward outcome
            successor = exactSuccessorState outcome
        Right
            [ ( ExactTraceResult
                    { exactTrace = prependStep selectedAction immediate successor (exactTrace future)
                    , exactTraceReturn =
                        exactReward
                            ( exactRewardValue immediate
                                + discount * exactRewardValue (exactTraceReturn future)
                            )
                    }
              , exactProbability actionMass
                    * exactProbability outcomeMass
                    * exactProbability futureMass
              )
            | (future, futureMass) <- NonEmpty.toList (exactOutcomes futureDistribution)
            ]

    prependStep selectedAction immediate successor futureTrace =
        futureTrace
            { traceSteps =
                TraceStep selectedAction immediate successor : traceSteps futureTrace
            }

-- | Evaluate from an exact model's initial state.
expectedExactReturn ::
    (Eq action) =>
    ExactFiniteObjective ->
    ExactMDP state action ->
    ExactPolicy state action ->
    Either (ExactEvaluationError action) ExactReward
expectedExactReturn objective model selectedPolicy =
    expectedExactReturnFrom objective model selectedPolicy (exactMDPInitialState model)

{- | Evaluate an exact policy from one state.

Terminal status is inspected before the horizon boundary. Every recursive call
decreases the remaining transition count.
-}
expectedExactReturnFrom ::
    (Eq action) =>
    ExactFiniteObjective ->
    ExactMDP state action ->
    ExactPolicy state action ->
    state ->
    Either (ExactEvaluationError action) ExactReward
expectedExactReturnFrom objective model selectedPolicy initial =
    fmap exactReward (go (horizonValue (exactObjectiveHorizon objective)) initial)
  where
    discount = exactDiscountValue (exactObjectiveDiscount objective)

    go remaining state =
        case exactMDPStateStatus model state of
            ExactTerminal payoff -> Right (exactRewardValue payoff)
            ExactContinuing
                | remaining == 0 -> Right 0
                | otherwise -> do
                    decision <- mapModelError (inspectExactMDP model state)
                    case decision of
                        ExactTerminalDecision payoff -> Right (exactRewardValue payoff)
                        ExactActionDecision available -> do
                            let selected = exactPolicyActions selectedPolicy state
                            mapPolicyError (validateExactPolicySupport available selected)
                            contributions <-
                                traverse
                                    (actionContribution (remaining - 1) state)
                                    (NonEmpty.toList (exactOutcomes selected))
                            Right (sum contributions)

    actionContribution remaining state (selectedAction, actionMass) = do
        transition <- mapModelError (stepExactMDP model state selectedAction)
        outcomes <-
            traverse
                (outcomeContribution remaining)
                (NonEmpty.toList (exactOutcomes transition))
        Right (exactProbability actionMass * sum outcomes)

    outcomeContribution remaining (outcome, outcomeMass) = do
        future <- go remaining (exactSuccessorState outcome)
        let immediate = exactRewardValue (exactTransitionReward outcome)
        Right (exactProbability outcomeMass * (immediate + discount * future))

mapModelError :: Either (ExactModelError action) value -> Either (ExactEvaluationError action) value
mapModelError = either (Left . ExactEvaluationModelError) Right

mapPolicyError :: Either (ExactPolicyError action) value -> Either (ExactEvaluationError action) value
mapPolicyError = either (Left . ExactEvaluationPolicyError) Right

mapDistributionError :: Either ExactDistributionError value -> Either (ExactEvaluationError action) value
mapDistributionError = either (Left . ExactEvaluationDistributionError) Right
