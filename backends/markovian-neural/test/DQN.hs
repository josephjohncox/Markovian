module DQN (tests) where

import Markovian.Backend.Neural (
    DQNConfig,
    DQNError (..),
    DQNTargetSelection (..),
    DenseNetwork,
    NeuralTransition,
    applySGD,
    denseParameters,
    dqnBatchMeanGradient,
    dqnBatchMeanHalfSquaredLoss,
    dqnBatchTargets,
    dqnOnlineNetwork,
    dqnTargetNetwork,
    dqnTransitionTarget,
    dqnUpdateEvaluation,
    dqnUpdatedState,
    evaluateDQNBatch,
    mkActionMask,
    mkContinuingTransition,
    mkDQNConfig,
    mkDQNState,
    mkDenseNetwork,
    mkSGD,
    mkTerminalTransition,
    noAutomaticTargetUpdates,
    periodicHardTargetUpdates,
    targetNetworkSnapshot,
    targetSuccessfulUpdateCount,
    updateDQNBatch,
 )
import TestSupport (assert, assertClose, assertVectorClose, assertVectorCloseWith, requireRight)

tests :: IO ()
tests = do
    targetChecks
    maskTieChecks
    batchFiniteDifference
    batchSnapshotCheck
    tinyLearningStep
    putStrLn "PASS: DQN"

targetChecks :: IO ()
targetChecks = do
    currentMask <- requireRight "current mask" (mkActionMask 2 [0, 1])
    successorMask <- requireRight "successor mask" (mkActionMask 2 [0, 1])
    online <- requireRight "online target fixture" (mkDenseNetwork 2 [] 2 [0, 2, 0, 1, 0, 0])
    target <- requireRight "target target fixture" (mkDenseNetwork 2 [] 2 [0, 4, 0, 5, 0, 0])
    continuing <-
        requireRight
            "continuing DQN transition"
            (mkContinuingTransition [1, 0] currentMask 1 1 [0, 1] successorMask)
    standard <- requireRight "standard DQN target" (dqnTransitionTarget 0.5 StandardDQN online target continuing)
    double <- requireRight "Double DQN target" (dqnTransitionTarget 0.5 DoubleDQN online target continuing)
    assertClose "standard DQN masked maximum" 1e-15 3.5 standard
    assertClose "Double DQN online argmax, target evaluation" 1e-15 3 double
    terminal <- requireRight "terminal DQN transition" (mkTerminalTransition [1, 0] currentMask 0 2 7)
    terminalTarget <- requireRight "terminal DQN target" (dqnTransitionTarget 0.5 StandardDQN online target terminal)
    assertClose "terminal payoff timing" 1e-15 5.5 terminalTarget

maskTieChecks :: IO ()
maskTieChecks = do
    sourceMask <- requireRight "tie source mask" (mkActionMask 2 [0, 1])
    reverseMask <- requireRight "reverse tie mask" (mkActionMask 2 [1, 0])
    forwardMask <- requireRight "forward tie mask" (mkActionMask 2 [0, 1])
    online <- requireRight "tie online" (mkDenseNetwork 2 [] 2 (replicate 6 0))
    target <- requireRight "tie target" (mkDenseNetwork 2 [] 2 [0, 4, 0, 5, 0, 0])
    reverseTransition <- requireRight "reverse tie transition" (mkContinuingTransition [1, 0] sourceMask 0 0 [0, 1] reverseMask)
    forwardTransition <- requireRight "forward tie transition" (mkContinuingTransition [1, 0] sourceMask 0 0 [0, 1] forwardMask)
    reverseTarget <- requireRight "reverse tie target" (dqnTransitionTarget 1 DoubleDQN online target reverseTransition)
    forwardTarget <- requireRight "forward tie target" (dqnTransitionTarget 1 DoubleDQN online target forwardTransition)
    assertClose "mask-order tie selects first reverse entry" 0 5 reverseTarget
    assertClose "mask-order tie selects first forward entry" 0 4 forwardTarget
    wrongWidth <- requireRight "wrong-width DQN mask" (mkActionMask 3 [0, 1])
    wrongWidthTransition <-
        requireRight
            "wrong-width DQN transition"
            (mkContinuingTransition [1, 0] sourceMask 0 0 [0, 1] wrongWidth)
    case dqnTransitionTarget 1 DoubleDQN online target wrongWidthTransition of
        Left (DQNMaskWidthMismatch 2 3) -> pure ()
        result -> assert ("wrong-width DQN mask was accepted: " ++ show result) False

batchFiniteDifference :: IO ()
batchFiniteDifference = do
    mask <- requireRight "finite-difference mask" (mkActionMask 2 [0, 1])
    first <- requireRight "finite-difference first" (mkContinuingTransition [1, 0] mask 0 1.5 [0, 1] mask)
    second <- requireRight "finite-difference second" (mkTerminalTransition [0, 1] mask 1 (-0.25) 1)
    let transitions = [first, second]
        parameters = [0.2, -0.1, 0.4, 0.3, 0.05, -0.2]
    online <- requireRight "finite-difference online" (mkDenseNetwork 2 [] 2 parameters)
    target <- requireRight "finite-difference target" (mkDenseNetwork 2 [] 2 (replicate 6 0))
    optimizer <- requireRight "finite-difference optimizer" (mkSGD 0.1)
    config <- requireRight "finite-difference config" (mkDQNConfig 0.5 optimizer DoubleDQN noAutomaticTargetUpdates)
    state <- requireRight "finite-difference state" (mkDQNState online target)
    evaluation <- requireRight "finite-difference evaluation" (evaluateDQNBatch config state transitions)
    numerical <-
        traverse
            ( \index -> do
                let point = parameters !! index
                    step = 1e-6 * max 1 (abs point)
                above <- lossAt config target transitions (replace index (point + step) parameters)
                below <- lossAt config target transitions (replace index (point - step) parameters)
                pure ((above - below) / (2 * step))
            )
            [0 .. length parameters - 1]
    assertVectorCloseWith "DQN continuing Double-target mean-loss gradient" 2e-10 2e-8 (dqnBatchMeanGradient evaluation) numerical

batchSnapshotCheck :: IO ()
batchSnapshotCheck = do
    mask <- requireRight "snapshot mask" (mkActionMask 2 [0, 1])
    first <- requireRight "snapshot first transition" (mkTerminalTransition [1, 0] mask 0 2 0)
    second <- requireRight "snapshot second transition" (mkTerminalTransition [1, 0] mask 0 4 0)
    online <- requireRight "snapshot online" (mkDenseNetwork 2 [] 2 (replicate 6 0))
    target <- requireRight "snapshot target" (mkDenseNetwork 2 [] 2 (replicate 6 0))
    optimizer <- requireRight "snapshot optimizer" (mkSGD 0.1)
    config <- requireRight "snapshot config" (mkDQNConfig 0 optimizer StandardDQN noAutomaticTargetUpdates)
    state <- requireRight "snapshot state" (mkDQNState online target)
    update <- requireRight "snapshot batch update" (updateDQNBatch config state [first, second])
    assertVectorClose
        "complete batch uses one pre-update snapshot"
        1e-15
        [0.3, 0, 0, 0, 0.3, 0]
        (denseParameters (dqnOnlineNetwork (dqnUpdatedState update)))

tinyLearningStep :: IO ()
tinyLearningStep = do
    mask <- requireRight "tiny mask" (mkActionMask 2 [0, 1])
    transition <- requireRight "tiny transition" (mkTerminalTransition [1, 0] mask 0 2 0)
    online <- requireRight "tiny online" (mkDenseNetwork 2 [] 2 (replicate 6 0))
    target <- requireRight "tiny target" (mkDenseNetwork 2 [] 2 (replicate 6 0))
    optimizer <- requireRight "tiny optimizer" (mkSGD 0.1)
    schedule <- requireRight "tiny schedule" (periodicHardTargetUpdates 1)
    config <- requireRight "tiny config" (mkDQNConfig 0.5 optimizer StandardDQN schedule)
    state <- requireRight "tiny state" (mkDQNState online target)
    update <- requireRight "tiny DQN update" (updateDQNBatch config state [transition])
    let evaluation = dqnUpdateEvaluation update
        updated = dqnUpdatedState update
    assertVectorClose "tiny target" 0 [2] (dqnBatchTargets evaluation)
    assertClose "tiny pre-update loss" 0 2 (dqnBatchMeanHalfSquaredLoss evaluation)
    assertVectorClose
        "one-hot linear Q update"
        1e-15
        [0.2, 0, 0, 0, 0.2, 0]
        (denseParameters (dqnOnlineNetwork updated))
    assert "successful update count" (targetSuccessfulUpdateCount (dqnTargetNetwork updated) == 1)
    assertVectorClose
        "period-one target synchronization"
        1e-15
        (denseParameters (dqnOnlineNetwork updated))
        (denseParameters (targetNetworkSnapshot (dqnTargetNetwork updated)))
    case updateDQNBatch config state [] of
        Left _ -> pure ()
        Right _ -> assert "empty batch update succeeded" False
    assert "failed update did not mutate original schedule" (targetSuccessfulUpdateCount (dqnTargetNetwork state) == 0)
    -- Also check the optimizer convention directly: one mean gradient, one step.
    direct <- requireRight "direct tiny SGD" (applySGD optimizer (dqnBatchMeanGradient evaluation) online)
    assert "DQN uses one aggregated SGD step" (direct == dqnOnlineNetwork updated)

lossAt :: DQNConfig -> DenseNetwork -> [NeuralTransition] -> [Double] -> IO Double
lossAt config target transitions parameters = do
    online <- requireRight "finite-difference perturbed online" (mkDenseNetwork 2 [] 2 parameters)
    state <- requireRight "finite-difference perturbed state" (mkDQNState online target)
    evaluation <- requireRight "finite-difference perturbed loss" (evaluateDQNBatch config state transitions)
    pure (dqnBatchMeanHalfSquaredLoss evaluation)

replace :: Int -> Double -> [Double] -> [Double]
replace selected replacement = zipWith (\index value -> if index == selected then replacement else value) [0 :: Int ..]
