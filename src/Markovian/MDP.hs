-- | One-step Markov decision process interfaces.
module Markovian.MDP (
    ActionId,
    actionId,
    actionValue,
    Decision (..),
    ModelError (..),
    MDP,
    mdp,
    mdpInitialState,
    mdpStateStatus,
    inspectMDP,
    stepMDP,
)
where

import Data.List.NonEmpty (NonEmpty (..))
import Markovian.Kernel (Kernel, runKernel)
import Markovian.MRP (StateStatus (..), TransitionOutcome)
import Markovian.Probability (FiniteDist)
import Markovian.Reward (Reward)

-- | The stable identity of an agent choice. It is not a stochastic outcome.
newtype ActionId action = ActionId action
    deriving (Eq, Ord, Show)

-- | Construct an action ID.
actionId :: action -> ActionId action
actionId = ActionId

-- | Read an action ID.
actionValue :: ActionId action -> action
actionValue (ActionId value) = value

-- | The result of inspecting one MDP state.
data Decision action
    = TerminalDecision !Reward
    | ActionDecision !(NonEmpty (ActionId action))
    deriving (Eq, Show)

-- | Errors from the one-step MDP boundary.
data ModelError action
    = EmptyActionSupport
    | ActionRequestedAtTerminal !Reward
    | UnavailableAction !(ActionId action)
    deriving (Eq, Show)

{- | A generative one-step MDP. Available actions and stochastic transition
outcomes are stored behind separate operations.
-}
data MDP state action
    = MDP
        !state
        !(state -> StateStatus)
        !(state -> Either (ModelError action) (Decision action))
        !(state -> ActionId action -> Either (ModelError action) (FiniteDist (TransitionOutcome state)))

{- | Construct a one-step MDP. Empty action support is reported when a
continuing state is inspected. A transition is run only for an available
action ID and never for a terminal state.
-}
mdp ::
    (Eq action) =>
    state ->
    (state -> StateStatus) ->
    (state -> [ActionId action]) ->
    Kernel (state, ActionId action) (TransitionOutcome state) ->
    MDP state action
mdp initial status available transition = MDP initial status inspect step
  where
    inspect state =
        case status state of
            Terminal payoff -> Right (TerminalDecision payoff)
            Continuing ->
                case available state of
                    [] -> Left EmptyActionSupport
                    first : rest -> Right (ActionDecision (first :| rest))

    step state selected =
        case inspect state of
            Left err -> Left err
            Right (TerminalDecision payoff) -> Left (ActionRequestedAtTerminal payoff)
            Right (ActionDecision choices)
                | selected `elem` choices -> Right (runKernel transition (state, selected))
                | otherwise -> Left (UnavailableAction selected)

-- | Read the initial state.
mdpInitialState :: MDP state action -> state
mdpInitialState (MDP initial _ _ _) = initial

-- | Inspect termination without requesting actions or a transition.
mdpStateStatus :: MDP state action -> state -> StateStatus
mdpStateStatus (MDP _ status _ _) = status

-- | Inspect one state for either terminal payoff or nonempty available actions.
inspectMDP :: MDP state action -> state -> Either (ModelError action) (Decision action)
inspectMDP (MDP _ _ inspect _) = inspect

-- | Run one action's stochastic transition kernel for one layer.
stepMDP ::
    MDP state action ->
    state ->
    ActionId action ->
    Either (ModelError action) (FiniteDist (TransitionOutcome state))
stepMDP (MDP _ _ _ step) = step
