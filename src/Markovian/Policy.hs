-- | Policy kernels over action IDs.
module Markovian.Policy (
    Policy,
    policy,
    policyActions,
)
where

import Markovian.Kernel (Kernel, runKernel)
import Markovian.MDP (ActionId)
import Markovian.Probability (FiniteDist)

{- | A policy maps a state to a stochastic choice of action IDs. This first
slice does not close a policy over an MDP; policy support validation remains
at that later closure boundary.
-}
newtype Policy state action = Policy (Kernel state (ActionId action))

-- | Construct a policy from a validated finite kernel.
policy :: Kernel state (ActionId action) -> Policy state action
policy = Policy

-- | Get the policy's action distribution for one state.
policyActions :: Policy state action -> state -> FiniteDist (ActionId action)
policyActions (Policy actionKernel) = runKernel actionKernel
