{- | Structured bounded-interpreter traces.

A trace records every selected action and realized transition. The stop reason
makes the terminal-before-horizon convention observable.
-}
module Markovian.Trace (
    StopReason (..),
    TraceStep (..),
    Trace (..),
) where

-- | Why a bounded interpreter stopped.
data StopReason reward
    = -- | The transition budget was exhausted in a continuing state.
      HorizonStop
    | -- | A terminal state was reached and contributed its payoff.
      TerminalStop !reward
    deriving (Eq, Show)

-- | One realized transition in a bounded trace.
data TraceStep state action reward = TraceStep
    { traceAction :: !action
    , traceTransitionReward :: !reward
    , traceSuccessorState :: !state
    }
    deriving (Eq, Show)

-- | A finite sequence of transitions and its final state and stop reason.
data Trace state action reward = Trace
    { traceSteps :: ![TraceStep state action reward]
    , traceStopState :: !state
    , traceStopReason :: !(StopReason reward)
    }
    deriving (Eq, Show)
