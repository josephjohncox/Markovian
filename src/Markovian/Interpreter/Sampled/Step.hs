{- | One validated, explicitly seeded sampled MDP step.

The selected action is validated by 'stepMDP'. Reward and successor are sampled
as one joint transition outcome, so their correlation and generator sequencing
are preserved.
-}
module Markovian.Interpreter.Sampled.Step (
    SampledStepError (..),
    sampleMDPStep,
) where

import Markovian.MDP (ActionId, MDP, ModelError, stepMDP)
import Markovian.MRP (successorState, transitionReward)
import Markovian.Reward (Reward)
import Markovian.Sampling (Generator, SamplingError, sampleFiniteDist)
import Markovian.Trace (TraceStep (..))

-- | Failures from model validation or joint-outcome sampling.
data SampledStepError action
    = SampledStepModelError !(ModelError action)
    | SampledStepSamplingError !SamplingError
    deriving (Eq, Show)

-- | Validate and sample one selected action, returning the next generator.
sampleMDPStep ::
    MDP state action ->
    state ->
    ActionId action ->
    Generator ->
    Either (SampledStepError action) (TraceStep state (ActionId action) Reward, Generator)
sampleMDPStep model state selected generator = do
    transition <- either (Left . SampledStepModelError) Right (stepMDP model state selected)
    (outcome, nextGenerator) <-
        either (Left . SampledStepSamplingError) Right (sampleFiniteDist generator transition)
    Right
        ( TraceStep selected (transitionReward outcome) (successorState outcome)
        , nextGenerator
        )
