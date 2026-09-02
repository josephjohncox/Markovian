-- | One-step Markov reward process interfaces.
module Markovian.MRP (
    StateStatus (..),
    TransitionOutcome,
    transitionOutcome,
    transitionReward,
    successorState,
    MRPStep (..),
    MRP,
    mrp,
    mrpInitialState,
    mrpStateStatus,
    stepMRP,
)
where

import Markovian.Kernel (Kernel, runKernel)
import Markovian.Probability (FiniteDist)
import Markovian.Reward (Reward)

{- | Whether a state can make a transition. A terminal payoff belongs only to
the terminal state and is distinct from every transition reward.
-}
data StateStatus
    = Continuing
    | Terminal !Reward
    deriving (Eq, Show)

-- | One stochastic transition result. Reward and successor remain correlated.
data TransitionOutcome state = TransitionOutcome
    { transitionReward :: !Reward
    -- ^ Reward emitted by this transition.
    , successorState :: !state
    -- ^ State reached by this transition.
    }
    deriving (Eq, Show)

-- | Construct one transition result.
transitionOutcome :: Reward -> state -> TransitionOutcome state
transitionOutcome = TransitionOutcome

-- | The result of observing exactly one MRP layer.
data MRPStep state
    = TerminalStep !Reward
    | TransitionStep !(FiniteDist (TransitionOutcome state))
    deriving (Show)

{- | A generative one-step Markov reward process.

'stepMRP' does not run the kernel for a terminal state.
-}
data MRP state
    = MRP
        !state
        !(state -> StateStatus)
        !(Kernel state (TransitionOutcome state))

-- | Construct a one-step MRP.
mrp :: state -> (state -> StateStatus) -> Kernel state (TransitionOutcome state) -> MRP state
mrp = MRP

-- | Read the initial state.
mrpInitialState :: MRP state -> state
mrpInitialState (MRP initial _ _) = initial

-- | Inspect state termination without requesting a transition.
mrpStateStatus :: MRP state -> state -> StateStatus
mrpStateStatus (MRP _ status _) = status

{- | Observe one layer. A terminal state yields its payoff. A continuing state
yields one finite distribution of transition outcomes.
-}
stepMRP :: MRP state -> state -> MRPStep state
stepMRP (MRP _ status transition) state =
    case status state of
        Terminal payoff -> TerminalStep payoff
        Continuing -> TransitionStep (runKernel transition state)
