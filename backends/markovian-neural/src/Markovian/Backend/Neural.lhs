\begin{code}
{- | Framework-independent 'Double' policy-gradient references.

The package keeps approximate numerical work outside the exact root semantic
core. It provides stable categorical operations, checked row-major dense
networks with manual VJPs, policy-gradient updates, bounded replay, target
networks, and one atomic DQN batch step. It uses neither tensors nor automatic
differentiation.
-}
module Markovian.Backend.Neural (
    module Markovian.Backend.Neural.ActorCritic,
    module Markovian.Backend.Neural.Approximation,
    module Markovian.Backend.Neural.Categorical,
    module Markovian.Backend.Neural.Dense,
    module Markovian.Backend.Neural.DQN,
    module Markovian.Backend.Neural.Numeric,
    module Markovian.Backend.Neural.Optimizer,
    module Markovian.Backend.Neural.Policy,
    module Markovian.Backend.Neural.Reinforce,
    module Markovian.Backend.Neural.Replay,
    module Markovian.Backend.Neural.TargetNetwork,
    module Markovian.Backend.Neural.Transition,
) where

import Markovian.Backend.Neural.ActorCritic
import Markovian.Backend.Neural.Approximation
import Markovian.Backend.Neural.Categorical
import Markovian.Backend.Neural.Dense
import Markovian.Backend.Neural.DQN
import Markovian.Backend.Neural.Numeric
import Markovian.Backend.Neural.Optimizer
import Markovian.Backend.Neural.Policy
import Markovian.Backend.Neural.Reinforce
import Markovian.Backend.Neural.Replay
import Markovian.Backend.Neural.TargetNetwork
import Markovian.Backend.Neural.Transition
\end{code}
