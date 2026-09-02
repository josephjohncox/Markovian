{- | One pure on-policy SARSA action-value update.

A continuing bootstrap carries the exact next action selected by the behavior
policy. This makes the target policy observable and prevents accidental
replacement by a greedy maximum.
-}
module Markovian.Learning.Sarsa (
    SarsaBootstrap (..),
    SarsaUpdateError (..),
    SarsaUpdateResult (..),
    updateSarsa,
) where

import Markovian.Action (ActionId)
import Markovian.Learning.Tabular
import Markovian.MDP (Decision (..), MDP, ModelError, inspectMDP)
import Markovian.Objective (Discount, discountValue)
import Markovian.Reward (Reward, rewardValue)

-- | Terminal or continuing successor information for a SARSA target.
data SarsaBootstrap action
    = -- | The observed successor is terminal; no dummy action is required.
      SarsaTerminal
    | -- | The behavior policy selected this action at the continuing successor.
      SarsaNextAction !(ActionId action)
    deriving (Eq, Show)

-- | Failures from one pure SARSA update.
data SarsaUpdateError action
    = SarsaUpdateModelError !(ModelError action)
    | SarsaUpdateSourceTerminal !Reward
    | SarsaUpdateUnavailableAction !(ActionId action)
    | SarsaUpdateExpectedTerminal
    | SarsaUpdateExpectedNextAction
    | SarsaUpdateUnavailableNextAction !(ActionId action)
    | SarsaUpdateArithmeticError !QValueError
    deriving (Eq, Show)

-- | The complete algebraic result of one SARSA update.
data SarsaUpdateResult state action = SarsaUpdateResult
    { sarsaUpdateKey :: !(QKey state action)
    , sarsaUpdateBootstrap :: !(SarsaBootstrap action)
    , sarsaUpdateOldValue :: !QValue
    , sarsaUpdateTarget :: !QValue
    , sarsaUpdateNewValue :: !QValue
    , sarsaUpdateTable :: !(QTable state action)
    }
    deriving (Eq, Show)

{- | Apply one on-policy SARSA update.

For terminal payoff @g@, @target = r + gamma*g@. For a continuing successor
and the explicitly sampled next action @a'@,
@target = r + gamma*Q(s',a')@.
-}
updateSarsa ::
    (Eq state, Eq action) =>
    LearningRate ->
    Discount ->
    MDP state action ->
    ObservedTransition state action ->
    SarsaBootstrap action ->
    QTable state action ->
    Either (SarsaUpdateError action) (SarsaUpdateResult state action)
updateSarsa rate discount model observed bootstrap table = do
    sourceDecision <- mapModelError (inspectMDP model (observedState observed))
    case sourceDecision of
        TerminalDecision payoff -> Left (SarsaUpdateSourceTerminal payoff)
        ActionDecision available
            | observedAction observed `notElem` available ->
                Left (SarsaUpdateUnavailableAction (observedAction observed))
            | otherwise -> pure ()
    successorDecision <- mapModelError (inspectMDP model (observedSuccessor observed))
    future <-
        case (successorDecision, bootstrap) of
            (TerminalDecision payoff, SarsaTerminal) -> Right (rewardValue payoff)
            (TerminalDecision _, SarsaNextAction _) -> Left SarsaUpdateExpectedTerminal
            (ActionDecision _, SarsaTerminal) -> Left SarsaUpdateExpectedNextAction
            (ActionDecision available, SarsaNextAction selected)
                | selected `elem` available -> Right (qValue (qValueAt table (observedSuccessor observed) selected))
                | otherwise -> Left (SarsaUpdateUnavailableNextAction selected)
    let targetRaw = rewardValue (observedReward observed) + discountValue discount * future
        old = qValueAt table (observedState observed) (observedAction observed)
    target <- validateQ targetRaw
    updated <- validateQ (qValue old + learningRateValue rate * (qValue target - qValue old))
    let key = QKey (observedState observed) (observedAction observed)
    Right
        SarsaUpdateResult
            { sarsaUpdateKey = key
            , sarsaUpdateBootstrap = bootstrap
            , sarsaUpdateOldValue = old
            , sarsaUpdateTarget = target
            , sarsaUpdateNewValue = updated
            , sarsaUpdateTable = setQValue key updated table
            }
  where
    validateQ value =
        case mkQValue value of
            Left err -> Left (SarsaUpdateArithmeticError err)
            Right valid -> Right valid

mapModelError :: Either (ModelError action) value -> Either (SarsaUpdateError action) value
mapModelError = either (Left . SarsaUpdateModelError) Right
