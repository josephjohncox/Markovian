-- | Exact policy closure for literal support and observable laws.
module Markovian.Policy.Exact (
    ExactPolicyError (..),
    closeExactPolicy,
    exactConditionalExpectedReward,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Markovian.MDP (ActionId)
import Markovian.Policy (ConditionalRewardError (..))
import Markovian.Probability.Exact (
    ExactFiniteDist,
    bindExactFiniteDist,
    exactOutcomes,
    exactProbability,
 )
import Markovian.Reward.Exact (
    ExactReward,
    exactReward,
    exactRewardValue,
 )

-- | Exact policy-support validation errors.
data ExactPolicyError action
    = -- | One action occurs more than once in the model's available support.
      DuplicateExactAvailableAction !(ActionId action)
    | -- | One action occurs more than once in the policy distribution.
      DuplicateExactPolicyAction !(ActionId action)
    | -- | The policy selects an action outside the available support.
      ExactPolicyUnavailableAction !(ActionId action)
    deriving (Eq, Show)

{- | Close one exact policy distribution over one state's action kernels.

The result preserves joint output labels, duplicate outcomes, and exact mass.
The selected action ID is removed from the closed distribution.
-}
closeExactPolicy ::
    (Eq action) =>
    NonEmpty (ActionId action) ->
    ExactFiniteDist (ActionId action) ->
    (ActionId action -> ExactFiniteDist output) ->
    Either (ExactPolicyError action) (ExactFiniteDist output)
closeExactPolicy available selected transition = do
    case firstDuplicate (NonEmpty.toList available) of
        Just duplicate -> Left (DuplicateExactAvailableAction duplicate)
        Nothing -> pure ()
    let selectedIds = fmap fst (NonEmpty.toList (exactOutcomes selected))
    case firstDuplicate selectedIds of
        Just duplicate -> Left (DuplicateExactPolicyAction duplicate)
        Nothing -> pure ()
    case firstUnavailable (NonEmpty.toList available) selectedIds of
        Just unavailable -> Left (ExactPolicyUnavailableAction unavailable)
        Nothing -> pure ()
    pure (bindExactFiniteDist selected transition)

{- | Compute an exact expected reward conditional on one successor state.

The query returns 'ZeroMassTransition' when the successor has no positive
marginal mass.
-}
exactConditionalExpectedReward ::
    (Eq state) =>
    ExactFiniteDist (ExactReward, state) ->
    state ->
    Either ConditionalRewardError ExactReward
exactConditionalExpectedReward distribution requestedSuccessor =
    case matching of
        [] -> Left ZeroMassTransition
        positive ->
            let marginal = sum (fmap snd positive)
                expected =
                    sum
                        [ mass / marginal * exactRewardValue reward
                        | (reward, mass) <- positive
                        ]
             in Right (exactReward expected)
  where
    matching =
        [ (reward, exactProbability mass)
        | ((reward, successor), mass) <- NonEmpty.toList (exactOutcomes distribution)
        , successor == requestedSuccessor
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
