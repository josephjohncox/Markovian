-- | Seeded finite-support interpretation of floating finite objectives.
module Markovian.Interpreter.Sampled (
    SampledEvaluationError (..),
    SampledResult (..),
    sampleReturn,
    sampleReturnFrom,
) where

import Markovian.Action (ActionId)
import Markovian.Horizon (horizonValue)
import Markovian.Interpreter.Sampled.Step (
    SampledStepError (..),
    sampleMDPStep,
 )
import Markovian.MDP (
    Decision (..),
    MDP,
    ModelError,
    inspectMDP,
    mdpInitialState,
 )
import Markovian.Objective (
    FiniteObjective,
    discountValue,
    objectiveDiscount,
    objectiveHorizon,
 )
import Markovian.Policy (
    Policy,
    PolicyError,
    policyActions,
    validatePolicySupport,
 )
import Markovian.Reward (Reward, RewardError, mkReward, rewardValue)
import Markovian.Sampling (
    Generator,
    SamplingError,
    sampleFiniteDist,
 )
import Markovian.Trace (
    StopReason (..),
    Trace (..),
    TraceStep (..),
 )

-- | Failures from one sampled finite-horizon evaluation.
data SampledEvaluationError action
    = -- | The model rejected a requested state or action.
      SampledModelError !(ModelError action)
    | -- | The policy failed support validation.
      SampledPolicyError !(PolicyError action)
    | -- | The categorical sampler detected an invariant failure.
      SampledSamplingError !SamplingError
    | -- | Floating return accumulation became non-finite.
      SampledArithmeticError !RewardError
    deriving (Eq, Show)

-- | One sampled return, its complete trace, and resumable generator state.
data SampledResult state action = SampledResult
    { sampledReturn :: !Reward
    , sampledTrace :: !(Trace state (ActionId action) Reward)
    , sampledGenerator :: !Generator
    }
    deriving (Eq, Show)

-- | Sample from a model's initial state.
sampleReturn ::
    (Eq action) =>
    FiniteObjective ->
    MDP state action ->
    Policy state action ->
    Generator ->
    Either (SampledEvaluationError action) (SampledResult state action)
sampleReturn objective model selectedPolicy =
    sampleReturnFrom objective model selectedPolicy (mdpInitialState model)

{- | Sample from one state.

Terminal status is inspected before the horizon boundary. Transition rewards
and reached terminal payoffs use the same discount placement as the exact
reference evaluator.
-}
sampleReturnFrom ::
    (Eq action) =>
    FiniteObjective ->
    MDP state action ->
    Policy state action ->
    state ->
    Generator ->
    Either (SampledEvaluationError action) (SampledResult state action)
sampleReturnFrom objective model selectedPolicy initial generator =
    go
        (horizonValue (objectiveHorizon objective))
        initial
        generator
        1
        0
        []
  where
    discount = discountValue (objectiveDiscount objective)

    go remaining state currentGenerator discountPower accumulated reversedSteps = do
        decision <- mapModelError (inspectMDP model state)
        case decision of
            TerminalDecision payoff -> do
                total <- validatedReward (accumulated + discountPower * rewardValue payoff)
                Right
                    SampledResult
                        { sampledReturn = total
                        , sampledTrace = Trace (reverse reversedSteps) state (TerminalStop payoff)
                        , sampledGenerator = currentGenerator
                        }
            ActionDecision available
                | remaining == 0 -> do
                    total <- validatedReward accumulated
                    Right
                        SampledResult
                            { sampledReturn = total
                            , sampledTrace = Trace (reverse reversedSteps) state HorizonStop
                            , sampledGenerator = currentGenerator
                            }
                | otherwise -> do
                    let selected = policyActions selectedPolicy state
                    mapPolicyError (validatePolicySupport available selected)
                    (selectedAction, afterAction) <- mapSamplingError (sampleFiniteDist currentGenerator selected)
                    (step, afterTransition) <- mapStepError (sampleMDPStep model state selectedAction afterAction)
                    let reward = traceTransitionReward step
                        successor = traceSuccessorState step
                        nextAccumulated = accumulated + discountPower * rewardValue reward
                        nextPower = discountPower * discount
                    _ <- validatedReward nextAccumulated
                    go
                        (remaining - 1)
                        successor
                        afterTransition
                        nextPower
                        nextAccumulated
                        (step : reversedSteps)

    validatedReward value =
        case mkReward value of
            Left err -> Left (SampledArithmeticError err)
            Right reward -> Right reward

mapModelError :: Either (ModelError action) value -> Either (SampledEvaluationError action) value
mapModelError = either (Left . SampledModelError) Right

mapPolicyError :: Either (PolicyError action) value -> Either (SampledEvaluationError action) value
mapPolicyError = either (Left . SampledPolicyError) Right

mapSamplingError :: Either SamplingError value -> Either (SampledEvaluationError action) value
mapSamplingError = either (Left . SampledSamplingError) Right

mapStepError :: Either (SampledStepError action) value -> Either (SampledEvaluationError action) value
mapStepError = either convert Right
  where
    convert (SampledStepModelError err) = Left (SampledModelError err)
    convert (SampledStepSamplingError err) = Left (SampledSamplingError err)
