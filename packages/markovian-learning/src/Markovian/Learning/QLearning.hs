{- | One pure off-policy tabular Q-learning update.

The behavior policy is intentionally absent from 'updateQ'. For a continuing
successor, the target is greedy even when an episodic runner behaves
epsilon-greedily. Shared validated table and schedule types are re-exported for
compatibility.
-}
module Markovian.Learning.QLearning (
    module Markovian.Learning.Tabular,
    QLearningConfig,
    qLearningConfig,
    qLearningDiscount,
    qLearningRateSchedule,
    qExplorationSchedule,
    qEpisodeLimit,
    qEpisodeStepLimit,
    QUpdateError (..),
    QUpdateResult (..),
    updateQ,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Markovian.Action (ActionId)
import Markovian.Horizon (Horizon)
import Markovian.Learning.Tabular
import Markovian.MDP (
    Decision (..),
    MDP,
    ModelError,
    inspectMDP,
 )
import Markovian.Objective (Discount, discountValue)
import Markovian.Reward (Reward, rewardValue)

-- | Complete bounded episodic Q-learning configuration.
data QLearningConfig = QLearningConfig
    { qLearningDiscount :: !Discount
    -- ^ Discount used by the greedy Q-learning target.
    , qLearningRateSchedule :: !LearningRateSchedule
    -- ^ Learning rate indexed by the global update count.
    , qExplorationSchedule :: !ExplorationSchedule
    -- ^ Epsilon used only by the episodic behavior policy.
    , qEpisodeLimit :: !Horizon
    -- ^ Number of episodes performed by one runner call.
    , qEpisodeStepLimit :: !Horizon
    -- ^ Maximum sampled transitions in each episode.
    }
    deriving (Eq, Show)

-- | Construct Q-learning configuration from validated values.
qLearningConfig ::
    Discount ->
    LearningRateSchedule ->
    ExplorationSchedule ->
    Horizon ->
    Horizon ->
    QLearningConfig
qLearningConfig = QLearningConfig

-- | Failures from one pure Q-learning update.
data QUpdateError action
    = QUpdateModelError !(ModelError action)
    | QUpdateSourceTerminal !Reward
    | QUpdateUnavailableAction !(ActionId action)
    | QUpdateArithmeticError !QValueError
    deriving (Eq, Show)

-- | The complete algebraic result of one pure Q-learning update.
data QUpdateResult state action = QUpdateResult
    { qUpdateKey :: !(QKey state action)
    , qUpdateOldValue :: !QValue
    , qUpdateTarget :: !QValue
    , qUpdateNewValue :: !QValue
    , qUpdateTable :: !(QTable state action)
    }
    deriving (Eq, Show)

{- | Apply one off-policy tabular Q-learning update.

For a terminal successor with payoff @g@:

@target = r + gamma * g@

For a continuing successor:

@target = r + gamma * max_a Q(s', a)@

The maximum uses only actions exposed by the model. Exact ties retain the first
available action, although the target value itself is tie-independent.
-}
updateQ ::
    (Eq state, Eq action) =>
    LearningRate ->
    Discount ->
    MDP state action ->
    ObservedTransition state action ->
    QTable state action ->
    Either (QUpdateError action) (QUpdateResult state action)
updateQ rate discount model observed table = do
    sourceDecision <- mapModelError (inspectMDP model (observedState observed))
    case sourceDecision of
        TerminalDecision payoff -> Left (QUpdateSourceTerminal payoff)
        ActionDecision available
            | observedAction observed `notElem` available ->
                Left (QUpdateUnavailableAction (observedAction observed))
            | otherwise -> pure ()
    successorDecision <- mapModelError (inspectMDP model (observedSuccessor observed))
    let future =
            case successorDecision of
                TerminalDecision payoff -> rewardValue payoff
                ActionDecision available -> maximumAvailable table (observedSuccessor observed) available
        targetRaw = rewardValue (observedReward observed) + discountValue discount * future
        old = qValueAt table (observedState observed) (observedAction observed)
    target <- validateQValue targetRaw
    updated <-
        validateQValue
            (qValue old + learningRateValue rate * (qValue target - qValue old))
    let key = QKey (observedState observed) (observedAction observed)
    Right
        QUpdateResult
            { qUpdateKey = key
            , qUpdateOldValue = old
            , qUpdateTarget = target
            , qUpdateNewValue = updated
            , qUpdateTable = setQValue key updated table
            }
  where
    validateQValue value =
        case mkQValue value of
            Left err -> Left (QUpdateArithmeticError err)
            Right valid -> Right valid

maximumAvailable ::
    (Eq state, Eq action) =>
    QTable state action ->
    state ->
    NonEmpty (ActionId action) ->
    Double
maximumAvailable table state (first :| remaining) =
    foldl choose (qValue (qValueAt table state first)) remaining
  where
    choose best candidate = max best (qValue (qValueAt table state candidate))

mapModelError :: Either (ModelError action) value -> Either (QUpdateError action) value
mapModelError = either (Left . QUpdateModelError) Right
