-- | Exact finite-support expectation for finite-horizon objectives.
module Markovian.Interpreter.Exact (
    ExactEvaluationError (..),
    expectedExactReturn,
    expectedExactReturnFrom,
) where

import Data.List.NonEmpty qualified as NonEmpty
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
import Markovian.Objective (horizonValue)
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
    exactOutcomes,
    exactProbability,
 )
import Markovian.Reward.Exact (
    ExactReward,
    exactReward,
    exactRewardValue,
 )

-- | Errors returned by exact finite-horizon evaluation.
data ExactEvaluationError action
    = -- | The exact model rejected a requested state or action.
      ExactEvaluationModelError !(ExactModelError action)
    | -- | The exact policy failed support validation.
      ExactEvaluationPolicyError !(ExactPolicyError action)
    deriving (Eq, Show)

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
