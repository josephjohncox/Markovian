{- | Canonical epsilon-greedy behavior distributions for tabular action values.

Greedy ties retain the first action in model availability order. Sampling owns
an explicit generator and returns its successor state.
-}
module Markovian.Learning.EpsilonGreedy (
    EpsilonGreedyError (..),
    greedyAction,
    epsilonGreedyDistribution,
    sampleEpsilonGreedy,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Learning.Tabular (
    ExplorationRate,
    QTable,
    explorationRateValue,
    qValue,
    qValueAt,
 )
import Markovian.MDP (ActionId)
import Markovian.Probability (DistributionError, FiniteDist, finiteDist)
import Markovian.Sampling (Generator, SamplingError, sampleFiniteDist)

-- | Validation and sampling failures for epsilon-greedy behavior.
data EpsilonGreedyError action
    = DuplicateEpsilonGreedyAction !(ActionId action)
    | PositiveExplorationMassUnderflow !Double !Int
    | EpsilonGreedyDistributionError !DistributionError
    | EpsilonGreedySamplingError !SamplingError
    deriving (Eq, Show)

-- | Select the first action having the greatest stored value.
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

{- | Construct the canonical epsilon-greedy distribution.

Each available action receives @epsilon / |A(s)|@. The first greedy action
also receives @1 - epsilon@. Support order follows model availability order.
-}
epsilonGreedyDistribution ::
    (Eq state, Eq action) =>
    ExplorationRate ->
    QTable state action ->
    state ->
    NonEmpty (ActionId action) ->
    Either (EpsilonGreedyError action) (FiniteDist (ActionId action))
epsilonGreedyDistribution epsilon table state available = do
    case firstDuplicate (NonEmpty.toList available) of
        Just duplicate -> Left (DuplicateEpsilonGreedyAction duplicate)
        Nothing -> pure ()
    let choices = NonEmpty.toList available
        greedy = greedyAction table state available
        epsilonValue = explorationRateValue epsilon
        actionCount = length choices
        explorationWeight = epsilonValue / fromIntegral actionCount
        exploitationWeight = 1 - epsilonValue
        weighted =
            [ (selected, explorationWeight + if selected == greedy then exploitationWeight else 0)
            | selected <- choices
            ]
    if epsilonValue > 0 && actionCount > 1 && explorationWeight == 0
        then Left (PositiveExplorationMassUnderflow epsilonValue actionCount)
        else either (Left . EpsilonGreedyDistributionError) Right (finiteDist weighted)

-- | Sample the canonical epsilon-greedy behavior distribution.
sampleEpsilonGreedy ::
    (Eq state, Eq action) =>
    ExplorationRate ->
    QTable state action ->
    state ->
    NonEmpty (ActionId action) ->
    Generator ->
    Either (EpsilonGreedyError action) (ActionId action, Generator)
sampleEpsilonGreedy epsilon table state available generator = do
    distribution <- epsilonGreedyDistribution epsilon table state available
    either (Left . EpsilonGreedySamplingError) Right (sampleFiniteDist generator distribution)

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate [] = Nothing
firstDuplicate (value : remaining)
    | value `elem` remaining = Just value
    | otherwise = firstDuplicate remaining
