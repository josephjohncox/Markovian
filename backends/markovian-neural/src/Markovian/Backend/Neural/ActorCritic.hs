{- | One-step linear actor-critic reference update.

The target is @y = r + gamma * g@ for a terminal payoff @g@ and
@y = r + gamma * stop(V(s'))@ for a continuing successor. The TD error is
@delta = y - V(s)@. The actor ascends
@stop(delta) * log pi(a | s)@ and the critic descends
@0.5 * delta^2@. Both gradients use the same pre-update snapshot.
-}
module Markovian.Backend.Neural.ActorCritic (
    ActorCriticError (..),
    ActorCriticConfig,
    mkActorCriticConfig,
    actorCriticDiscount,
    actorCriticActorLearningRate,
    actorCriticCriticLearningRate,
    ActorCriticSuccessor (..),
    ActorCriticTransition (..),
    ActorCriticUpdate (..),
    updateActorCritic,
) where

import Markovian.Backend.Neural.Numeric (
    NeuralNumericError,
    checkedAdd,
    checkedMultiply,
    checkedSubtract,
    validateFinite,
 )
import Markovian.Backend.Neural.Policy (
    LinearCategoricalPolicy,
    LinearValueFunction,
    NeuralPolicyError,
    applyLinearPolicyAscent,
    applyLinearValueAscent,
    evaluateLinearValue,
    linearPolicyFeatureCount,
    linearPolicyScoreGradient,
    linearValueFeatureCount,
    linearValueGradient,
 )
import Markovian.Backend.Neural.Transition (ActionMask)

-- | Actor-critic configuration, shape, and finite-arithmetic failures.
data ActorCriticError
    = InvalidActorCriticDiscount !Double
    | InvalidActorLearningRate !Double
    | InvalidCriticLearningRate !Double
    | ActorCriticFeatureMismatch !Int !Int
    | ActorCriticPolicyFailure !NeuralPolicyError
    | ActorCriticNumericFailure !NeuralNumericError
    deriving (Eq, Show)

-- | Validated discount and learning rates.
data ActorCriticConfig = ActorCriticConfig !Double !Double !Double
    deriving (Eq, Show)

{- | Construct a finite configuration. The discount must be in @[0, 1]@ and
learning rates must be nonnegative.
-}
mkActorCriticConfig :: Double -> Double -> Double -> Either ActorCriticError ActorCriticConfig
mkActorCriticConfig discount actorRate criticRate
    | not (validUnit discount) = Left (InvalidActorCriticDiscount discount)
    | not (validRate actorRate) = Left (InvalidActorLearningRate actorRate)
    | not (validRate criticRate) = Left (InvalidCriticLearningRate criticRate)
    | otherwise = Right (ActorCriticConfig discount actorRate criticRate)

-- | Discount in @[0, 1]@.
actorCriticDiscount :: ActorCriticConfig -> Double
actorCriticDiscount (ActorCriticConfig value _ _) = value

-- | Actor gradient-ascent rate.
actorCriticActorLearningRate :: ActorCriticConfig -> Double
actorCriticActorLearningRate (ActorCriticConfig _ value _) = value

-- | Critic gradient-descent rate for half-squared TD error.
actorCriticCriticLearningRate :: ActorCriticConfig -> Double
actorCriticCriticLearningRate (ActorCriticConfig _ _ value) = value

{- | Successor observation. Terminal payoff and continuing bootstrap are
separate so terminal updates never require successor features.
-}
data ActorCriticSuccessor
    = ActorCriticTerminal !Double
    | ActorCriticContinuing ![Double]
    deriving (Eq, Show)

-- | Immutable one-step observation.
data ActorCriticTransition = ActorCriticTransition
    { actorCriticFeatures :: ![Double]
    , actorCriticActionMask :: !ActionMask
    , actorCriticAction :: !Int
    , actorCriticReward :: !Double
    , actorCriticSuccessor :: !ActorCriticSuccessor
    }
    deriving (Eq, Show)

-- | Target, detached TD error, gradients, and post-update models.
data ActorCriticUpdate = ActorCriticUpdate
    { actorCriticTarget :: !Double
    , actorCriticValue :: !Double
    , actorCriticTDError :: !Double
    , actorCriticActorGradient :: ![Double]
    , actorCriticCriticGradient :: ![Double]
    , actorCriticUpdatedPolicy :: !LinearCategoricalPolicy
    , actorCriticUpdatedValueFunction :: !LinearValueFunction
    }
    deriving (Eq, Show)

{- | Apply one atomic reference update. The critic gradient field is the ascent
direction @delta * grad V(s)@, equivalent to descending the half-squared TD
error while treating the target as detached.
-}
updateActorCritic :: ActorCriticConfig -> LinearCategoricalPolicy -> LinearValueFunction -> ActorCriticTransition -> Either ActorCriticError ActorCriticUpdate
updateActorCritic config policy valueFunction transition = do
    validateFeatureAgreement policy valueFunction
    mapNumeric (validateFinite "actor-critic reward" (actorCriticReward transition))
    currentValue <-
        mapPolicy (evaluateLinearValue valueFunction (actorCriticFeatures transition))
    target <- targetValue
    tdError <- mapNumeric (checkedSubtract "actor-critic TD error" target currentValue)
    score <-
        mapPolicy
            ( linearPolicyScoreGradient
                policy
                (actorCriticFeatures transition)
                (actorCriticActionMask transition)
                (actorCriticAction transition)
            )
    actorGradient <- scaleVector "actor-critic actor gradient" tdError score
    valueDerivative <-
        mapPolicy (linearValueGradient valueFunction (actorCriticFeatures transition))
    criticGradient <- scaleVector "actor-critic critic gradient" tdError valueDerivative
    updatedPolicy <-
        mapPolicy
            ( applyLinearPolicyAscent
                (actorCriticActorLearningRate config)
                actorGradient
                policy
            )
    updatedValueFunction <-
        mapPolicy
            ( applyLinearValueAscent
                (actorCriticCriticLearningRate config)
                criticGradient
                valueFunction
            )
    Right
        ActorCriticUpdate
            { actorCriticTarget = target
            , actorCriticValue = currentValue
            , actorCriticTDError = tdError
            , actorCriticActorGradient = actorGradient
            , actorCriticCriticGradient = criticGradient
            , actorCriticUpdatedPolicy = updatedPolicy
            , actorCriticUpdatedValueFunction = updatedValueFunction
            }
  where
    discount = actorCriticDiscount config
    targetValue = do
        bootstrap <-
            case actorCriticSuccessor transition of
                ActorCriticTerminal payoff -> do
                    mapNumeric (validateFinite "actor-critic terminal payoff" payoff)
                    Right payoff
                ActorCriticContinuing successorFeatures ->
                    mapPolicy (evaluateLinearValue valueFunction successorFeatures)
        discounted <- mapNumeric (checkedMultiply "actor-critic discounted bootstrap" discount bootstrap)
        mapNumeric (checkedAdd "actor-critic target" (actorCriticReward transition) discounted)

validateFeatureAgreement :: LinearCategoricalPolicy -> LinearValueFunction -> Either ActorCriticError ()
validateFeatureAgreement policy valueFunction
    | policyFeatures == valueFeatures = Right ()
    | otherwise = Left (ActorCriticFeatureMismatch policyFeatures valueFeatures)
  where
    policyFeatures = linearPolicyFeatureCount policy
    valueFeatures = linearValueFeatureCount valueFunction

scaleVector :: String -> Double -> [Double] -> Either ActorCriticError [Double]
scaleVector label scalar = traverse (mapNumeric . checkedMultiply label scalar)

validUnit :: Double -> Bool
validUnit value = finite value && value >= 0 && value <= 1

validRate :: Double -> Bool
validRate value = finite value && value >= 0

finite :: Double -> Bool
finite value = not (isNaN value || isInfinite value)

mapPolicy :: Either NeuralPolicyError value -> Either ActorCriticError value
mapPolicy = either (Left . ActorCriticPolicyFailure) Right

mapNumeric :: Either NeuralNumericError value -> Either ActorCriticError value
mapNumeric = either (Left . ActorCriticNumericFailure) Right
