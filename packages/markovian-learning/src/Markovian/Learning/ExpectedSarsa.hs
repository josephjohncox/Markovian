{- | One pure Expected SARSA action-value update.

The continuing target is an expectation under the same canonical
epsilon-greedy distribution used by behavior runners. It is not a greedy
Q-learning target and does not sample a next action for the update.
-}
module Markovian.Learning.ExpectedSarsa (
    ExpectedSarsaBootstrap (..),
    ExpectedSarsaUpdateError (..),
    ExpectedSarsaUpdateResult (..),
    updateExpectedSarsa,
) where

import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Action (ActionId)
import Markovian.Learning.EpsilonGreedy (EpsilonGreedyError, epsilonGreedyDistribution)
import Markovian.Learning.Tabular
import Markovian.MDP (Decision (..), MDP, ModelError, inspectMDP)
import Markovian.Objective (Discount, discountValue)
import Markovian.Probability (outcomes, probability)
import Markovian.Reward (Reward, rewardValue)

-- | Terminal or continuing successor information for Expected SARSA.
data ExpectedSarsaBootstrap
    = -- | The observed successor is terminal; epsilon is not requested.
      ExpectedSarsaTerminal
    | -- | Use this epsilon for the continuing successor distribution.
      ExpectedSarsaContinuing !ExplorationRate
    deriving (Eq, Show)

-- | Failures from one pure Expected SARSA update.
data ExpectedSarsaUpdateError action
    = ExpectedSarsaUpdateModelError !(ModelError action)
    | ExpectedSarsaUpdateSourceTerminal !Reward
    | ExpectedSarsaUpdateUnavailableAction !(ActionId action)
    | ExpectedSarsaUpdateExpectedTerminal
    | ExpectedSarsaUpdateExpectedExplorationRate
    | ExpectedSarsaUpdateBehaviorError !(EpsilonGreedyError action)
    | ExpectedSarsaUpdateArithmeticError !QValueError
    deriving (Eq, Show)

-- | The complete algebraic result of one Expected SARSA update.
data ExpectedSarsaUpdateResult state action = ExpectedSarsaUpdateResult
    { expectedSarsaUpdateKey :: !(QKey state action)
    , expectedSarsaUpdateBootstrap :: !ExpectedSarsaBootstrap
    , expectedSarsaUpdateOldValue :: !QValue
    , expectedSarsaUpdateTarget :: !QValue
    , expectedSarsaUpdateNewValue :: !QValue
    , expectedSarsaUpdateTable :: !(QTable state action)
    }
    deriving (Eq, Show)

{- | Apply one Expected SARSA update.

For terminal payoff @g@, @target = r + gamma*g@. For a continuing successor,
@target = r + gamma * sum_a pi_epsilon(a|s') Q(s',a)@.
-}
updateExpectedSarsa ::
    (Eq state, Eq action) =>
    LearningRate ->
    Discount ->
    MDP state action ->
    ObservedTransition state action ->
    ExpectedSarsaBootstrap ->
    QTable state action ->
    Either (ExpectedSarsaUpdateError action) (ExpectedSarsaUpdateResult state action)
updateExpectedSarsa rate discount model observed bootstrap table = do
    sourceDecision <- mapModelError (inspectMDP model (observedState observed))
    case sourceDecision of
        TerminalDecision payoff -> Left (ExpectedSarsaUpdateSourceTerminal payoff)
        ActionDecision available
            | observedAction observed `notElem` available ->
                Left (ExpectedSarsaUpdateUnavailableAction (observedAction observed))
            | otherwise -> pure ()
    successorDecision <- mapModelError (inspectMDP model (observedSuccessor observed))
    future <-
        case (successorDecision, bootstrap) of
            (TerminalDecision payoff, ExpectedSarsaTerminal) -> Right (rewardValue payoff)
            (TerminalDecision _, ExpectedSarsaContinuing _) -> Left ExpectedSarsaUpdateExpectedTerminal
            (ActionDecision _, ExpectedSarsaTerminal) -> Left ExpectedSarsaUpdateExpectedExplorationRate
            (ActionDecision available, ExpectedSarsaContinuing epsilon) -> do
                distribution <-
                    either
                        (Left . ExpectedSarsaUpdateBehaviorError)
                        Right
                        (epsilonGreedyDistribution epsilon table (observedSuccessor observed) available)
                Right
                    ( sum
                        [ probability mass * qValue (qValueAt table (observedSuccessor observed) selected)
                        | (selected, mass) <- NonEmpty.toList (outcomes distribution)
                        ]
                    )
    let targetRaw = rewardValue (observedReward observed) + discountValue discount * future
        old = qValueAt table (observedState observed) (observedAction observed)
    target <- validateQ targetRaw
    updated <- validateQ (qValue old + learningRateValue rate * (qValue target - qValue old))
    let key = QKey (observedState observed) (observedAction observed)
    Right
        ExpectedSarsaUpdateResult
            { expectedSarsaUpdateKey = key
            , expectedSarsaUpdateBootstrap = bootstrap
            , expectedSarsaUpdateOldValue = old
            , expectedSarsaUpdateTarget = target
            , expectedSarsaUpdateNewValue = updated
            , expectedSarsaUpdateTable = setQValue key updated table
            }
  where
    validateQ value =
        case mkQValue value of
            Left err -> Left (ExpectedSarsaUpdateArithmeticError err)
            Right valid -> Right valid

mapModelError :: Either (ModelError action) value -> Either (ExpectedSarsaUpdateError action) value
mapModelError = either (Left . ExpectedSarsaUpdateModelError) Right
