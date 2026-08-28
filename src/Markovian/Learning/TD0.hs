{- | One pure on-policy TD(0) state-value update.

The behavior policy chooses the observed action outside this function. TD(0)
then learns the value of that supplied policy from the observed transition; it
does not maximize over successor actions.
-}
module Markovian.Learning.TD0 (
    TD0UpdateError (..),
    TD0UpdateResult (..),
    updateTD0,
) where

import Markovian.Learning.Tabular
import Markovian.MDP (ActionId, Decision (..), MDP, ModelError, inspectMDP)
import Markovian.Objective (Discount, discountValue)
import Markovian.Reward (Reward, rewardValue)

-- | Failures from one pure TD(0) update.
data TD0UpdateError action
    = TD0UpdateModelError !(ModelError action)
    | TD0UpdateSourceTerminal !Reward
    | TD0UpdateUnavailableAction !(ActionId action)
    | TD0UpdateArithmeticError !VValueError
    deriving (Eq, Show)

-- | The complete algebraic result of one TD(0) update.
data TD0UpdateResult state = TD0UpdateResult
    { td0UpdateState :: !state
    , td0UpdateOldValue :: !VValue
    , td0UpdateTarget :: !VValue
    , td0UpdateNewValue :: !VValue
    , td0UpdateTable :: !(VTable state)
    }
    deriving (Eq, Show)

{- | Apply one on-policy TD(0) update.

For terminal payoff @g@, @target = r + gamma*g@. For a continuing successor,
@target = r + gamma*V(s')@. The policy is represented by the observed action;
terminal successors therefore require no policy or action-support query.
-}
updateTD0 ::
    (Eq state, Eq action) =>
    LearningRate ->
    Discount ->
    MDP state action ->
    ObservedTransition state action ->
    VTable state ->
    Either (TD0UpdateError action) (TD0UpdateResult state)
updateTD0 rate discount model observed table = do
    sourceDecision <- mapModelError (inspectMDP model (observedState observed))
    case sourceDecision of
        TerminalDecision payoff -> Left (TD0UpdateSourceTerminal payoff)
        ActionDecision available
            | observedAction observed `notElem` available ->
                Left (TD0UpdateUnavailableAction (observedAction observed))
            | otherwise -> pure ()
    successorDecision <- mapModelError (inspectMDP model (observedSuccessor observed))
    let future =
            case successorDecision of
                TerminalDecision payoff -> rewardValue payoff
                ActionDecision _ -> vValue (vValueAt table (observedSuccessor observed))
        targetRaw = rewardValue (observedReward observed) + discountValue discount * future
        old = vValueAt table (observedState observed)
    target <- validateV targetRaw
    updated <- validateV (vValue old + learningRateValue rate * (vValue target - vValue old))
    Right
        TD0UpdateResult
            { td0UpdateState = observedState observed
            , td0UpdateOldValue = old
            , td0UpdateTarget = target
            , td0UpdateNewValue = updated
            , td0UpdateTable = setVValue (observedState observed) updated table
            }
  where
    validateV value =
        case mkVValue value of
            Left err -> Left (TD0UpdateArithmeticError err)
            Right valid -> Right valid

mapModelError :: Either (ModelError action) value -> Either (TD0UpdateError action) value
mapModelError = either (Left . TD0UpdateModelError) Right
