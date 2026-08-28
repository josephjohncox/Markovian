{- | Executable finite-episode REINFORCE reference updates.

For rewards @r_t@ and boundary value @b_T@, return-to-go is
@G_t = r_t + gamma * G_(t+1)@ with @G_T = b_T@. For the discounted
start-return objective, the actor uses
@sum_t gamma^t * (G_t - V(s_t)) * grad log pi(a_t | s_t)@. When supplied, the linear
baseline is detached from that actor gradient and is updated from the same
pre-update snapshot with @sum_t (G_t - V(s_t)) * grad V(s_t)@.
-}
module Markovian.Backend.Neural.Reinforce (
    ReinforceError (..),
    ReinforceConfig,
    mkReinforceConfig,
    reinforceStepLimit,
    reinforceDiscount,
    reinforceActorLearningRate,
    reinforceBaselineLearningRate,
    EpisodeBoundary (..),
    ReinforceStep (..),
    ReinforceUpdate (..),
    reinforceReturnToGo,
    updateReinforce,
) where

import Markovian.Backend.Neural.Numeric (
    NeuralNumericError,
    checkedAdd,
    checkedMultiply,
    checkedSubtract,
    validateFinite,
    validateFiniteVector,
 )
import Markovian.Backend.Neural.Policy (
    LinearCategoricalPolicy,
    LinearValueFunction,
    NeuralPolicyError,
    applyLinearPolicyAscent,
    applyLinearValueAscent,
    evaluateLinearValue,
    linearPolicyFeatureCount,
    linearPolicyParameters,
    linearPolicyScoreGradient,
    linearValueFeatureCount,
    linearValueParameters,
 )
import Markovian.Backend.Neural.Transition (ActionMask)

-- | REINFORCE configuration, shape, boundary, and arithmetic failures.
data ReinforceError
    = InvalidReinforceStepLimit !Int
    | InvalidReinforceDiscount !Double
    | InvalidReinforceActorLearningRate !Double
    | InvalidReinforceBaselineLearningRate !Double
    | ReinforceStepLimitExceeded !Int !Int
    | MissingTruncatedBootstrap
    | ReinforceBaselineFeatureMismatch !Int !Int
    | ReinforceInternalLengthMismatch !Int !Int
    | ReinforcePolicyFailure !NeuralPolicyError
    | ReinforceNumericFailure !NeuralNumericError
    deriving (Eq, Show)

-- | Validated bound, discount, and learning rates.
data ReinforceConfig = ReinforceConfig !Int !Double !Double !Double
    deriving (Eq, Show)

{- | Construct a configuration. The step limit and rates must be nonnegative;
the discount must be in @[0, 1]@. All 'Double' fields must be finite.
-}
mkReinforceConfig :: Int -> Double -> Double -> Double -> Either ReinforceError ReinforceConfig
mkReinforceConfig stepLimit discount actorRate baselineRate
    | stepLimit < 0 = Left (InvalidReinforceStepLimit stepLimit)
    | not (validUnit discount) = Left (InvalidReinforceDiscount discount)
    | not (validRate actorRate) = Left (InvalidReinforceActorLearningRate actorRate)
    | not (validRate baselineRate) = Left (InvalidReinforceBaselineLearningRate baselineRate)
    | otherwise = Right (ReinforceConfig stepLimit discount actorRate baselineRate)

-- | Maximum accepted episode length.
reinforceStepLimit :: ReinforceConfig -> Int
reinforceStepLimit (ReinforceConfig value _ _ _) = value

-- | Discount in @[0, 1]@.
reinforceDiscount :: ReinforceConfig -> Double
reinforceDiscount (ReinforceConfig _ value _ _) = value

-- | Actor gradient-ascent rate.
reinforceActorLearningRate :: ReinforceConfig -> Double
reinforceActorLearningRate (ReinforceConfig _ _ value _) = value

-- | Baseline gradient-ascent rate for the negative half-squared loss.
reinforceBaselineLearningRate :: ReinforceConfig -> Double
reinforceBaselineLearningRate (ReinforceConfig _ _ _ value) = value

{- | Value after the last observed reward.

A truncated trajectory must carry a bootstrap. The explicit missing case is
provided so callers cannot silently treat truncation as termination.
-}
data EpisodeBoundary
    = TerminalBoundary !Double
    | TruncatedBoundary !Double
    | TruncatedBoundaryWithoutBootstrap
    deriving (Eq, Show)

-- | Immutable policy observation and reward for one episode step.
data ReinforceStep = ReinforceStep
    { reinforceFeatures :: ![Double]
    , reinforceActionMask :: !ActionMask
    , reinforceAction :: !Int
    , reinforceReward :: !Double
    }
    deriving (Eq, Show)

-- | Diagnostics and simultaneously computed post-update models.
data ReinforceUpdate = ReinforceUpdate
    { reinforceUpdatedPolicy :: !LinearCategoricalPolicy
    , reinforceUpdatedBaseline :: !(Maybe LinearValueFunction)
    , reinforceReturns :: ![Double]
    , reinforceAdvantages :: ![Double]
    , reinforceActorGradient :: ![Double]
    , reinforceBaselineGradient :: !(Maybe [Double])
    }
    deriving (Eq, Show)

-- | Compute bounded discounted return-to-go values.
reinforceReturnToGo :: ReinforceConfig -> [Double] -> EpisodeBoundary -> Either ReinforceError [Double]
reinforceReturnToGo config rewards boundary = do
    validateLength config rewards
    mapNumeric (validateFiniteVector "REINFORCE rewards" rewards)
    finalValue <- boundaryValue boundary
    (_, returns) <- foldRightM step (finalValue, []) rewards
    Right returns
  where
    discount = reinforceDiscount config
    step reward (future, accumulated) = do
        discounted <- mapNumeric (checkedMultiply "REINFORCE discounted future return" discount future)
        current <- mapNumeric (checkedAdd "REINFORCE return" reward discounted)
        Right (current, current : accumulated)

{- | Apply one episode update to a linear categorical policy and optional linear
baseline. Actor and baseline gradients are both evaluated from their immutable
pre-update parameters. If any check fails, no post-update model is returned.
-}
updateReinforce :: ReinforceConfig -> LinearCategoricalPolicy -> Maybe LinearValueFunction -> [ReinforceStep] -> EpisodeBoundary -> Either ReinforceError ReinforceUpdate
updateReinforce config policy baseline steps boundary = do
    validateLength config steps
    validateBaselineShape policy baseline
    returns <- reinforceReturnToGo config (fmap reinforceReward steps) boundary
    let emptyActorGradient = replicate (length (linearPolicyParameters policy)) 0
        emptyBaselineGradient =
            fmap (\valueFunction -> replicate (length (linearValueParameters valueFunction)) 0) baseline
    (advantages, actorGradient, baselineGradient) <-
        accumulate 1 steps returns [] emptyActorGradient emptyBaselineGradient
    updatedPolicy <-
        mapPolicy
            (applyLinearPolicyAscent (reinforceActorLearningRate config) actorGradient policy)
    updatedBaseline <-
        case (baseline, baselineGradient) of
            (Nothing, Nothing) -> Right Nothing
            (Just valueFunction, Just gradient) ->
                Just
                    <$> mapPolicy
                        (applyLinearValueAscent (reinforceBaselineLearningRate config) gradient valueFunction)
            _ -> Left (ReinforceBaselineFeatureMismatch 0 1)
    Right
        ReinforceUpdate
            { reinforceUpdatedPolicy = updatedPolicy
            , reinforceUpdatedBaseline = updatedBaseline
            , reinforceReturns = returns
            , reinforceAdvantages = advantages
            , reinforceActorGradient = actorGradient
            , reinforceBaselineGradient = baselineGradient
            }
  where
    accumulate _ [] [] reversedAdvantages actorGradient baselineGradient =
        Right (reverse reversedAdvantages, actorGradient, baselineGradient)
    accumulate discountPower (episodeStep : remainingSteps) (returnValue : remainingReturns) reversedAdvantages actorGradient baselineGradient = do
        mapNumeric (validateFinite "REINFORCE reward" (reinforceReward episodeStep))
        score <-
            mapPolicy
                ( linearPolicyScoreGradient
                    policy
                    (reinforceFeatures episodeStep)
                    (reinforceActionMask episodeStep)
                    (reinforceAction episodeStep)
                )
        baselineValue <-
            case baseline of
                Nothing -> Right 0
                Just valueFunction ->
                    mapPolicy (evaluateLinearValue valueFunction (reinforceFeatures episodeStep))
        advantage <- mapNumeric (checkedSubtract "REINFORCE advantage" returnValue baselineValue)
        discountedAdvantage <-
            mapNumeric (checkedMultiply "REINFORCE discounted advantage" discountPower advantage)
        actorContribution <- scaleVector "REINFORCE actor contribution" discountedAdvantage score
        nextActorGradient <- addVectors "REINFORCE actor gradient" actorGradient actorContribution
        nextBaselineGradient <-
            case baselineGradient of
                Nothing -> Right Nothing
                Just gradient -> do
                    contribution <-
                        scaleVector "REINFORCE baseline contribution" advantage (reinforceFeatures episodeStep)
                    Just <$> addVectors "REINFORCE baseline gradient" gradient contribution
        nextDiscountPower <-
            mapNumeric (checkedMultiply "REINFORCE discount power" discountPower (reinforceDiscount config))
        accumulate
            nextDiscountPower
            remainingSteps
            remainingReturns
            (advantage : reversedAdvantages)
            nextActorGradient
            nextBaselineGradient
    accumulate _ remainingSteps remainingReturns _ _ _ =
        Left (ReinforceInternalLengthMismatch (length remainingSteps) (length remainingReturns))

validateLength :: ReinforceConfig -> [value] -> Either ReinforceError ()
validateLength config values
    | actual > limit = Left (ReinforceStepLimitExceeded limit actual)
    | otherwise = Right ()
  where
    limit = reinforceStepLimit config
    actual = length values

validateBaselineShape :: LinearCategoricalPolicy -> Maybe LinearValueFunction -> Either ReinforceError ()
validateBaselineShape _ Nothing = Right ()
validateBaselineShape policy (Just baseline)
    | policyFeatures == baselineFeatures = Right ()
    | otherwise = Left (ReinforceBaselineFeatureMismatch policyFeatures baselineFeatures)
  where
    policyFeatures = linearPolicyFeatureCount policy
    baselineFeatures = linearValueFeatureCount baseline

boundaryValue :: EpisodeBoundary -> Either ReinforceError Double
boundaryValue TruncatedBoundaryWithoutBootstrap = Left MissingTruncatedBootstrap
boundaryValue (TerminalBoundary value) = validateBoundary "terminal payoff" value
boundaryValue (TruncatedBoundary value) = validateBoundary "truncated bootstrap" value

validateBoundary :: String -> Double -> Either ReinforceError Double
validateBoundary label value = do
    mapNumeric (validateFinite label value)
    Right value

scaleVector :: String -> Double -> [Double] -> Either ReinforceError [Double]
scaleVector label scalar = traverse (mapNumeric . checkedMultiply label scalar)

addVectors :: String -> [Double] -> [Double] -> Either ReinforceError [Double]
addVectors label left right
    | length left /= length right = Left (ReinforceInternalLengthMismatch (length left) (length right))
    | otherwise = traverse (mapNumeric . uncurry (checkedAdd label)) (zip left right)

foldRightM :: (value -> state -> Either error state) -> state -> [value] -> Either error state
foldRightM _ initial [] = Right initial
foldRightM operation initial (value : remaining) = do
    accumulated <- foldRightM operation initial remaining
    operation value accumulated

validUnit :: Double -> Bool
validUnit value = finite value && value >= 0 && value <= 1

validRate :: Double -> Bool
validRate value = finite value && value >= 0

finite :: Double -> Bool
finite value = not (isNaN value || isInfinite value)

mapPolicy :: Either NeuralPolicyError value -> Either ReinforceError value
mapPolicy = either (Left . ReinforcePolicyFailure) Right

mapNumeric :: Either NeuralNumericError value -> Either ReinforceError value
mapNumeric = either (Left . ReinforceNumericFailure) Right
