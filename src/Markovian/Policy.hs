-- | Policy kernels, support validation, and one-layer MDP closure.
module Markovian.Policy (
    Policy,
    policy,
    policyActions,
    PolicyError (..),
    validatePolicySupport,
    PolicyMRP,
    closePolicy,
    policyMRPInitialState,
    policyMRPStateStatus,
    stepPolicyMRP,
    ConditionalRewardError (..),
    conditionalExpectedReward,
) where

import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Kernel (Kernel, runKernel)
import Markovian.MDP (
    ActionId,
    Decision (..),
    MDP,
    ModelError (..),
    inspectMDP,
    mdpInitialState,
    mdpStateStatus,
    stepMDP,
 )
import Markovian.MRP (
    MRPStep (..),
    StateStatus,
    TransitionOutcome,
    successorState,
    transitionReward,
 )
import Markovian.Probability (
    DistributionError,
    FiniteDist,
    finiteDist,
    outcomes,
    probability,
 )
import Markovian.Reward (
    Reward,
    RewardError,
    mkReward,
    rewardValue,
 )

-- | A policy maps a state to a stochastic choice of action IDs.
newtype Policy state action = Policy (Kernel state (ActionId action))

-- | Construct a policy from a validated finite kernel.
policy :: Kernel state (ActionId action) -> Policy state action
policy = Policy

-- | Get the policy's action distribution for one state.
policyActions :: Policy state action -> state -> FiniteDist (ActionId action)
policyActions (Policy actionKernel) = runKernel actionKernel

-- | Errors from policy support validation and closure.
data PolicyError action
    = -- | The underlying model boundary rejected the state or action.
      PolicyModelError !(ModelError action)
    | -- | One action ID occurs more than once in the policy distribution.
      DuplicatePolicyAction !(ActionId action)
    | -- | The policy selected an ID outside the model's available support.
      PolicyUnavailableAction !(ActionId action)
    | -- | Floating policy and transition masses could not be normalized.
      PolicyNormalizationError !DistributionError
    deriving (Eq, Show)

{- | An MDP closed under one policy.

Closure is fallible because policy support is validated at each requested
state. Terminal states return their payoff without running the policy.
-}
data PolicyMRP state action
    = PolicyMRP
        !state
        !(state -> StateStatus)
        !(state -> Either (PolicyError action) (MRPStep state))

-- | Close an MDP under a policy while retaining one-layer validation errors.
closePolicy :: (Eq action) => MDP state action -> Policy state action -> PolicyMRP state action
closePolicy model selectedPolicy =
    PolicyMRP (mdpInitialState model) (mdpStateStatus model) step
  where
    step state =
        case inspectMDP model state of
            Left err -> Left (PolicyModelError err)
            Right (TerminalDecision payoff) -> Right (TerminalStep payoff)
            Right (ActionDecision available) -> do
                let selected = policyActions selectedPolicy state
                    selectedEntries = NonEmpty.toList (outcomes selected)
                validatePolicySupport available selected
                branches <- traverse (runSelected state) selectedEntries
                case finiteDist (concat branches) of
                    Left err -> Left (PolicyNormalizationError err)
                    Right closed -> Right (TransitionStep closed)

    runSelected state (selected, selectedMass) =
        case stepMDP model state selected of
            Left err -> Left (PolicyModelError err)
            Right transition ->
                Right
                    [ (outcome, probability selectedMass * probability transitionMass)
                    | (outcome, transitionMass) <- NonEmpty.toList (outcomes transition)
                    ]

-- | Validate one floating policy distribution against available action IDs.
validatePolicySupport ::
    (Eq action) =>
    NonEmpty.NonEmpty (ActionId action) ->
    FiniteDist (ActionId action) ->
    Either (PolicyError action) ()
validatePolicySupport available selected = do
    case firstDuplicate (NonEmpty.toList available) of
        Just duplicate -> Left (PolicyModelError (DuplicateAvailableAction duplicate))
        Nothing -> pure ()
    let selectedIds = fmap fst (NonEmpty.toList (outcomes selected))
    case firstDuplicate selectedIds of
        Just duplicate -> Left (DuplicatePolicyAction duplicate)
        Nothing -> pure ()
    case firstUnavailable (NonEmpty.toList available) selectedIds of
        Just unavailable -> Left (PolicyUnavailableAction unavailable)
        Nothing -> pure ()

-- | Read the closed process's initial state.
policyMRPInitialState :: PolicyMRP state action -> state
policyMRPInitialState (PolicyMRP initial _ _) = initial

-- | Inspect termination without running the policy.
policyMRPStateStatus :: PolicyMRP state action -> state -> StateStatus
policyMRPStateStatus (PolicyMRP _ status _) = status

-- | Observe one closed policy layer.
stepPolicyMRP :: PolicyMRP state action -> state -> Either (PolicyError action) (MRPStep state)
stepPolicyMRP (PolicyMRP _ _ step) = step

-- | Errors from a conditional transition-reward query.
data ConditionalRewardError
    = -- | The requested successor has zero marginal probability.
      ZeroMassTransition
    | -- | Floating arithmetic did not produce a finite reward.
      InvalidConditionalReward !RewardError
    deriving (Eq, Show)

{- | Compute expected transition reward conditional on one successor state.

A successor outside the positive-mass support returns 'ZeroMassTransition'.
-}
conditionalExpectedReward ::
    (Eq state) =>
    FiniteDist (TransitionOutcome state) ->
    state ->
    Either ConditionalRewardError Reward
conditionalExpectedReward distribution requestedSuccessor =
    case matching of
        [] -> Left ZeroMassTransition
        positive ->
            let marginal = sum (fmap snd positive)
                expected =
                    sum
                        [ mass / marginal * rewardValue reward
                        | (reward, mass) <- positive
                        ]
             in case mkReward expected of
                    Left err -> Left (InvalidConditionalReward err)
                    Right reward -> Right reward
  where
    matching =
        [ (transitionReward outcome, probability mass)
        | (outcome, mass) <- NonEmpty.toList (outcomes distribution)
        , successorState outcome == requestedSuccessor
        ]

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate [] = Nothing
firstDuplicate (value : remaining)
    | value `elem` remaining = Just value
    | otherwise = firstDuplicate remaining

firstUnavailable :: (Eq value) => [value] -> [value] -> Maybe value
firstUnavailable available = go
  where
    go [] = Nothing
    go (value : remaining)
        | value `elem` available = go remaining
        | otherwise = Just value
