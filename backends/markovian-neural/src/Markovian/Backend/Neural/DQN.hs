{- | One checked dense-network DQN batch update.

Targets are detached. A complete nonempty batch is evaluated from one online
parameter snapshot and one target snapshot, its mean gradient is aggregated,
and exactly one atomic SGD step is attempted. Target scheduling occurs only
after that step succeeds.
-}
module Markovian.Backend.Neural.DQN (
    DQNError (..),
    DQNTargetSelection (..),
    DQNConfig,
    mkDQNConfig,
    dqnDiscount,
    dqnOptimizer,
    dqnTargetSelection,
    DQNState,
    mkDQNState,
    dqnOnlineNetwork,
    dqnTargetNetwork,
    dqnTransitionTarget,
    DQNBatchEvaluation,
    dqnBatchTargets,
    dqnBatchPredictions,
    dqnBatchMeanHalfSquaredLoss,
    dqnBatchMeanGradient,
    evaluateDQNBatch,
    DQNUpdate,
    dqnUpdatedState,
    dqnUpdateEvaluation,
    updateDQNBatch,
) where

import Markovian.Backend.Neural.Dense (
    DenseError,
    DenseNetwork,
    denseForward,
    denseOutputSize,
    denseParameterCount,
    denseParameterVJP,
    sameDenseTopology,
 )
import Markovian.Backend.Neural.Mask (
    ActionMask,
    ActionMaskError (..),
    actionMaskIndices,
    actionMaskWidth,
    gatherActionMask,
 )
import Markovian.Backend.Neural.Numeric (
    NeuralNumericError,
    checkedAdd,
    checkedDivide,
    checkedMultiply,
    checkedSubtract,
    validateFinite,
 )
import Markovian.Backend.Neural.Optimizer (
    OptimizerError,
    SGD,
    applySGD,
 )
import Markovian.Backend.Neural.TargetNetwork (
    TargetNetwork,
    TargetNetworkError,
    TargetUpdateSchedule,
    afterSuccessfulUpdate,
    mkTargetNetwork,
    targetNetworkSnapshot,
 )
import Markovian.Backend.Neural.Transition (
    NeuralTransition,
    foldSuccessorSnapshot,
    transitionAction,
    transitionActionMask,
    transitionFeatures,
    transitionReward,
    transitionSuccessor,
 )

-- | DQN configuration, shape, and finite-arithmetic failures.
data DQNError
    = InvalidDQNDiscount !Double
    | EmptyDQNBatch
    | DQNOnlineTargetTopologyMismatch
    | DQNMaskWidthMismatch !Int !Int
    | DQNMaskFailure !ActionMaskError
    | DQNSelectedActionOutOfBounds !Int !Int
    | DQNGradientShapeMismatch !Int !Int
    | DQNDenseFailure !DenseError
    | DQNOptimizerFailure !OptimizerError
    | DQNTargetNetworkFailure !TargetNetworkError
    | DQNNumericFailure !NeuralNumericError
    deriving (Eq, Show)

-- | Bootstrap action selection rule.
data DQNTargetSelection
    = StandardDQN
    | DoubleDQN
    deriving (Eq, Show)

-- | Validated discount, optimizer, target rule, and post-update schedule.
data DQNConfig = DQNConfig !Double !SGD !DQNTargetSelection !TargetUpdateSchedule
    deriving (Eq, Show)

-- | Construct a finite discount configuration with @0 <= gamma <= 1@.
mkDQNConfig :: Double -> SGD -> DQNTargetSelection -> TargetUpdateSchedule -> Either DQNError DQNConfig
mkDQNConfig discount optimizer selection schedule
    | validUnit discount = Right (DQNConfig discount optimizer selection schedule)
    | otherwise = Left (InvalidDQNDiscount discount)

-- | Discount in @[0,1]@.
dqnDiscount :: DQNConfig -> Double
dqnDiscount (DQNConfig value _ _ _) = value

-- | Checked deterministic optimizer.
dqnOptimizer :: DQNConfig -> SGD
dqnOptimizer (DQNConfig _ value _ _) = value

-- | Standard or Double-DQN target selection.
dqnTargetSelection :: DQNConfig -> DQNTargetSelection
dqnTargetSelection (DQNConfig _ _ value _) = value

-- | An online network plus a topology-compatible target snapshot and counter.
data DQNState = DQNState !DenseNetwork !TargetNetwork
    deriving (Eq, Show)

-- | Construct a state from explicit topology-compatible online and target snapshots.
mkDQNState :: DenseNetwork -> DenseNetwork -> Either DQNError DQNState
mkDQNState online target
    | sameDenseTopology online target = Right (DQNState online (mkTargetNetwork target))
    | otherwise = Left DQNOnlineTargetTopologyMismatch

-- | Current online network.
dqnOnlineNetwork :: DQNState -> DenseNetwork
dqnOnlineNetwork (DQNState online _) = online

-- | Current target state and successful-update counter.
dqnTargetNetwork :: DQNState -> TargetNetwork
dqnTargetNetwork (DQNState _ target) = target

{- | Compute one detached terminal or masked continuing target.

Standard DQN maximizes target-network values. Double DQN selects with the
online network and evaluates that index with the target network. Strict @>@
replacement retains the first mask entry on ties.
-}
dqnTransitionTarget :: Double -> DQNTargetSelection -> DenseNetwork -> DenseNetwork -> NeuralTransition -> Either DQNError Double
dqnTransitionTarget discount selection online target transition
    | not (validUnit discount) = Left (InvalidDQNDiscount discount)
    | not (sameDenseTopology online target) = Left DQNOnlineTargetTopologyMismatch
    | otherwise = do
        bootstrap <-
            foldSuccessorSnapshot terminalBootstrap continuingBootstrap (transitionSuccessor transition)
        discounted <- mapNumeric (checkedMultiply "DQN discounted bootstrap" discount bootstrap)
        mapNumeric (checkedAdd "DQN target" (transitionReward transition) discounted)
  where
    terminalBootstrap payoff = do
        mapNumeric (validateFinite "DQN terminal payoff" payoff)
        Right payoff
    continuingBootstrap successorFeatures mask = do
        targetValues <- mapDense (denseForward target successorFeatures)
        validateMask targetValues mask
        case selection of
            StandardDQN -> snd <$> maskedArgmax targetValues mask
            DoubleDQN -> do
                onlineValues <- mapDense (denseForward online successorFeatures)
                validateMask onlineValues mask
                (selected, _) <- maskedArgmax onlineValues mask
                case valueAtIndex selected targetValues of
                    Nothing -> Left (DQNSelectedActionOutOfBounds selected (length targetValues))
                    Just value -> Right value

-- | Pre-update detached targets, predictions, mean loss, and mean gradient.
data DQNBatchEvaluation = DQNBatchEvaluation
    { dqnBatchTargets :: ![Double]
    -- ^ Detached target for each batch entry.
    , dqnBatchPredictions :: ![Double]
    -- ^ Selected online prediction for each batch entry.
    , dqnBatchMeanHalfSquaredLoss :: !Double
    -- ^ Mean of the per-entry half-squared losses.
    , dqnBatchMeanGradient :: ![Double]
    -- ^ One mean parameter gradient in dense parameter order.
    }
    deriving (Eq, Show)

-- | Evaluate a complete batch without changing either network.
evaluateDQNBatch :: DQNConfig -> DQNState -> [NeuralTransition] -> Either DQNError DQNBatchEvaluation
evaluateDQNBatch _ _ [] = Left EmptyDQNBatch
evaluateDQNBatch config state transitions = do
    observations <- traverse evaluateOne transitions
    let targets = fmap observationTarget observations
        predictions = fmap observationPrediction observations
        losses = fmap observationHalfSquaredLoss observations
        gradients = fmap observationGradient observations
        parameterCount = denseParameterCount online
    totalLoss <- mapNumeric (foldChecked "DQN batch loss sum" losses)
    meanLoss <- divideByBatch "DQN mean half-squared loss" totalLoss
    totalGradient <- foldGradients parameterCount gradients
    meanGradient <- traverse (divideByBatch "DQN mean gradient") totalGradient
    Right
        DQNBatchEvaluation
            { dqnBatchTargets = targets
            , dqnBatchPredictions = predictions
            , dqnBatchMeanHalfSquaredLoss = meanLoss
            , dqnBatchMeanGradient = meanGradient
            }
  where
    online = dqnOnlineNetwork state
    target = targetNetworkSnapshot (dqnTargetNetwork state)
    batchSize = length transitions
    divideByBatch label value = mapNumeric (checkedDivide label value (fromIntegral batchSize))
    evaluateOne transition = do
        values <- mapDense (denseForward online (transitionFeatures transition))
        validateMask values (transitionActionMask transition)
        let action = transitionAction transition
        if action < 0 || action >= length values
            then Left (DQNSelectedActionOutOfBounds action (length values))
            else Right ()
        targetValue <-
            dqnTransitionTarget
                (dqnDiscount config)
                (dqnTargetSelection config)
                online
                target
                transition
        let prediction = values !! action
        residual <- mapNumeric (checkedSubtract "DQN residual" prediction targetValue)
        square <- mapNumeric (checkedMultiply "DQN squared residual" residual residual)
        halfLoss <- mapNumeric (checkedMultiply "DQN half-squared loss" 0.5 square)
        let outputCotangent =
                [ if index == action then residual else 0
                | index <- [0 .. denseOutputSize online - 1]
                ]
        gradient <- mapDense (denseParameterVJP online (transitionFeatures transition) outputCotangent)
        Right (Observation targetValue prediction halfLoss gradient)

-- Internal immutable per-sample result.
data Observation = Observation
    { observationTarget :: !Double
    , observationPrediction :: !Double
    , observationHalfSquaredLoss :: !Double
    , observationGradient :: ![Double]
    }

-- | Evaluation diagnostics and post-update state.
data DQNUpdate = DQNUpdate
    { dqnUpdatedState :: !DQNState
    -- ^ State after the online update and successful target event.
    , dqnUpdateEvaluation :: !DQNBatchEvaluation
    -- ^ Evaluation computed from the immutable pre-update snapshots.
    }
    deriving (Eq, Show)

-- | Apply one mean-gradient SGD step, then one successful-update target event.
updateDQNBatch :: DQNConfig -> DQNState -> [NeuralTransition] -> Either DQNError DQNUpdate
updateDQNBatch config state transitions = do
    evaluation <- evaluateDQNBatch config state transitions
    updatedOnline <-
        either
            (Left . DQNOptimizerFailure)
            Right
            (applySGD (dqnOptimizer config) (dqnBatchMeanGradient evaluation) (dqnOnlineNetwork state))
    let DQNConfig _ _ _ schedule = config
    updatedTarget <-
        either
            (Left . DQNTargetNetworkFailure)
            Right
            (afterSuccessfulUpdate schedule updatedOnline (dqnTargetNetwork state))
    Right
        DQNUpdate
            { dqnUpdatedState = DQNState updatedOnline updatedTarget
            , dqnUpdateEvaluation = evaluation
            }

maskedArgmax :: [Double] -> ActionMask -> Either DQNError (Int, Double)
maskedArgmax values mask = do
    gathered <- mapMask (gatherActionMask mask values)
    case zip (actionMaskIndices mask) gathered of
        [] -> Left (DQNMaskFailure EmptyActionMask) -- impossible for a validated mask
        first : remaining -> Right (foldl choose first remaining)
  where
    choose best@(_, bestValue) candidate@(_, candidateValue) =
        if candidateValue > bestValue then candidate else best

validateMask :: [Double] -> ActionMask -> Either DQNError ()
validateMask values mask
    | length values == actionMaskWidth mask = Right ()
    | otherwise = Left (DQNMaskWidthMismatch (length values) (actionMaskWidth mask))

valueAtIndex :: Int -> [value] -> Maybe value
valueAtIndex requested
    | requested < 0 = const Nothing
    | otherwise = go requested
  where
    go _ [] = Nothing
    go 0 (value : _) = Just value
    go remaining (_ : values) = go (remaining - 1) values

foldGradients :: Int -> [[Double]] -> Either DQNError [Double]
foldGradients parameterCount = go (replicate parameterCount 0)
  where
    go accumulated [] = Right accumulated
    go accumulated (gradient : remaining)
        | length gradient /= parameterCount = Left (DQNGradientShapeMismatch parameterCount (length gradient))
        | otherwise = do
            next <- traverse (mapNumeric . uncurry (checkedAdd "DQN gradient sum")) (zip accumulated gradient)
            go next remaining

foldChecked :: String -> [Double] -> Either NeuralNumericError Double
foldChecked label = go 0
  where
    go total [] = Right total
    go total (value : remaining) = do
        next <- checkedAdd label total value
        go next remaining

validUnit :: Double -> Bool
validUnit value = not (isNaN value || isInfinite value) && value >= 0 && value <= 1

mapDense :: Either DenseError value -> Either DQNError value
mapDense = either (Left . DQNDenseFailure) Right

mapNumeric :: Either NeuralNumericError value -> Either DQNError value
mapNumeric = either (Left . DQNNumericFailure) Right

mapMask :: Either ActionMaskError value -> Either DQNError value
mapMask = either (Left . DQNMaskFailure) Right
