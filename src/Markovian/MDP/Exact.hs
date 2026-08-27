-- | Exact one-step Markov decision process interfaces.
module Markovian.MDP.Exact (
    ExactStateStatus (..),
    ExactTransitionOutcome,
    exactTransitionOutcome,
    exactTransitionReward,
    exactSuccessorState,
    ExactDecision (..),
    ExactModelError (..),
    ExactMDP,
    exactMDP,
    exactMDPInitialState,
    exactMDPStateStatus,
    inspectExactMDP,
    stepExactMDP,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Markovian.Kernel.Exact (ExactKernel, runExactKernel)
import Markovian.MDP (ActionId)
import Markovian.Probability.Exact (ExactFiniteDist)
import Markovian.Reward.Exact (ExactReward)

-- | Whether an exact state can transition or carries a terminal payoff.
data ExactStateStatus
    = ExactContinuing
    | ExactTerminal !ExactReward
    deriving (Eq, Show)

-- | One exact transition reward paired with its successor state.
data ExactTransitionOutcome state = ExactTransitionOutcome
    { exactTransitionReward :: !ExactReward
    , exactSuccessorState :: !state
    }
    deriving (Eq, Show)

-- | Construct one exact transition outcome.
exactTransitionOutcome :: ExactReward -> state -> ExactTransitionOutcome state
exactTransitionOutcome = ExactTransitionOutcome

-- | The result of inspecting one exact MDP state.
data ExactDecision action
    = ExactTerminalDecision !ExactReward
    | ExactActionDecision !(NonEmpty (ActionId action))
    deriving (Eq, Show)

-- | Errors from the exact one-step model boundary.
data ExactModelError action
    = EmptyExactActionSupport
    | DuplicateExactModelAction !(ActionId action)
    | ExactActionRequestedAtTerminal !ExactReward
    | ExactUnavailableAction !(ActionId action)
    deriving (Eq, Show)

-- | A generative exact one-step MDP.
data ExactMDP state action
    = ExactMDP
        !state
        !(state -> ExactStateStatus)
        !(state -> Either (ExactModelError action) (ExactDecision action))
        !(state -> ActionId action -> Either (ExactModelError action) (ExactFiniteDist (ExactTransitionOutcome state)))

{- | Construct an exact MDP. Continuing action support must be nonempty and
contain no duplicate action IDs.
-}
exactMDP ::
    (Eq action) =>
    state ->
    (state -> ExactStateStatus) ->
    (state -> [ActionId action]) ->
    ExactKernel (state, ActionId action) (ExactTransitionOutcome state) ->
    ExactMDP state action
exactMDP initial status available transition = ExactMDP initial status inspect step
  where
    inspect state =
        case status state of
            ExactTerminal payoff -> Right (ExactTerminalDecision payoff)
            ExactContinuing ->
                case available state of
                    [] -> Left EmptyExactActionSupport
                    choices@(first : rest) ->
                        case firstDuplicate choices of
                            Just duplicate -> Left (DuplicateExactModelAction duplicate)
                            Nothing -> Right (ExactActionDecision (first :| rest))

    step state selected =
        case inspect state of
            Left err -> Left err
            Right (ExactTerminalDecision payoff) -> Left (ExactActionRequestedAtTerminal payoff)
            Right (ExactActionDecision choices)
                | selected `elem` choices -> Right (runExactKernel transition (state, selected))
                | otherwise -> Left (ExactUnavailableAction selected)

    firstDuplicate [] = Nothing
    firstDuplicate (choice : remaining)
        | choice `elem` remaining = Just choice
        | otherwise = firstDuplicate remaining

-- | Read the exact model's initial state.
exactMDPInitialState :: ExactMDP state action -> state
exactMDPInitialState (ExactMDP initial _ _ _) = initial

-- | Inspect exact state termination without requesting an action.
exactMDPStateStatus :: ExactMDP state action -> state -> ExactStateStatus
exactMDPStateStatus (ExactMDP _ status _ _) = status

-- | Inspect one exact state for terminal payoff or available actions.
inspectExactMDP :: ExactMDP state action -> state -> Either (ExactModelError action) (ExactDecision action)
inspectExactMDP (ExactMDP _ _ inspect _) = inspect

-- | Run one exact action transition for one layer.
stepExactMDP ::
    ExactMDP state action ->
    state ->
    ActionId action ->
    Either (ExactModelError action) (ExactFiniteDist (ExactTransitionOutcome state))
stepExactMDP (ExactMDP _ _ _ step) = step
