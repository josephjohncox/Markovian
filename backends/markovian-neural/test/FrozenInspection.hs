module FrozenInspection (runTests) where

import Markovian.Backend.Neural.DQN (
    DQNTargetSelection (StandardDQN),
    dqnUpdatedState,
    mkDQNConfig,
    mkDQNState,
    updateDQNBatch,
 )
import Markovian.Backend.Neural.Dense (DenseNetwork, mkDenseNetwork)
import Markovian.Backend.Neural.Inspection (
    InspectionError (..),
    actionLogProbabilityDelta,
    actionProbabilityAfter,
    actionProbabilityBefore,
    actionProbabilityDelta,
    actionProbabilityIndex,
    actionValueAfter,
    actionValueBefore,
    actionValueDelta,
    actionValueIndex,
    auditDQN,
    auditLinearPolicy,
    dqnAuditAfterLabel,
    dqnAuditAfterTargetUpdateCount,
    dqnAuditBeforeLabel,
    dqnAuditBeforeTargetUpdateCount,
    dqnAuditProbes,
    dqnProbeInput,
    dqnProbeOnline,
    dqnProbeTarget,
    inspectionProbeFeatures,
    inspectionProbeId,
    inspectionProbeMask,
    inspectionProbeReplayId,
    linearPolicyAuditAfterLabel,
    linearPolicyAuditBeforeLabel,
    linearPolicyAuditProbes,
    linearPolicyProbeActions,
    mkInspectionProbe,
 )
import Markovian.Backend.Neural.Mask (mkActionMask)
import Markovian.Backend.Neural.Optimizer (mkSGD)
import Markovian.Backend.Neural.Policy (linearPolicyParameters, mkLinearCategoricalPolicy)
import Markovian.Backend.Neural.Reinforce (
    EpisodeBoundary (TerminalBoundary),
    ReinforceStep (..),
    mkReinforceConfig,
    reinforceUpdatedPolicy,
    updateReinforce,
 )
import Markovian.Backend.Neural.Replay (appendReplay, mkReplayBuffer)
import Markovian.Backend.Neural.TargetNetwork (periodicHardTargetUpdates)
import Markovian.Backend.Neural.Transition (mkTerminalTransition)
import TestSupport (assert, assertVectorClose, requireRight)

runTests :: IO ()
runTests = do
    dqnSnapshotChecks
    dqnProvenanceChecks
    validationChecks
    policySnapshotChecks
    putStrLn "PASS: frozen neural inspection"

dqnSnapshotChecks :: IO ()
dqnSnapshotChecks = do
    beforeOnline <- dense2 "before online" [1, 0, 0, 1, 0, 0]
    afterOnline <- dense2 "after online" [2, 0, 0, 3, 0, 0]
    beforeTarget <- dense2 "before target" [0, 0, 0, 0, 10, 20]
    afterTarget <- dense2 "after target" [0, 0, 0, 0, 15, 25]
    before <- requireRight "before DQN" (mkDQNState beforeOnline beforeTarget)
    after <- requireRight "after DQN" (mkDQNState afterOnline afterTarget)
    mask <- requireRight "reordered mask" (mkActionMask 2 [1, 0])
    single <- requireRight "single-action mask" (mkActionMask 2 [0])
    let firstProbe = mkInspectionProbe "first" [4, -2] mask Nothing
        secondProbe = mkInspectionProbe "second" [1, 1] single Nothing
    report <- requireRight "DQN audit" (auditDQN 2 "before" before "after" after [firstProbe, secondProbe])
    assert "DQN labels" (dqnAuditBeforeLabel report == "before" && dqnAuditAfterLabel report == "after")
    assert "initial DQN target counters" (dqnAuditBeforeTargetUpdateCount report == 0 && dqnAuditAfterTargetUpdateCount report == 0)
    case dqnAuditProbes report of
        [first, second] -> do
            assert "DQN probe identity and features" (inspectionProbeId (dqnProbeInput first) == "first" && inspectionProbeFeatures (dqnProbeInput first) == [4, -2])
            assert "DQN probe mask preserved" (inspectionProbeMask (dqnProbeInput first) == mask)
            let online = dqnProbeOnline first
                target = dqnProbeTarget first
            assert "DQN all admissible actions follow mask order" (map actionValueIndex online == [1, 0] && map actionValueIndex target == [1, 0])
            assertVectorClose "DQN online before" 0 [-2, 4] (map actionValueBefore online)
            assertVectorClose "DQN online after" 0 [-6, 8] (map actionValueAfter online)
            assertVectorClose "DQN online deltas" 0 [-4, 4] (map actionValueDelta online)
            assertVectorClose "DQN target before" 0 [20, 10] (map actionValueBefore target)
            assertVectorClose "DQN target after" 0 [25, 15] (map actionValueAfter target)
            assertVectorClose "DQN target deltas" 0 [5, 5] (map actionValueDelta target)
            assert "single admissible action" (map actionValueIndex (dqnProbeOnline second) == [0])
            assertVectorClose "second probe uses same input at both snapshots" 0 [1] (map actionValueDelta (dqnProbeOnline second))
        found -> assert ("unexpected DQN probe count: " ++ show (length found)) False

dqnProvenanceChecks :: IO ()
dqnProvenanceChecks = do
    mask <- requireRight "provenance mask" (mkActionMask 2 [0, 1])
    network <- dense2 "provenance network" (replicate 6 0)
    before <- requireRight "provenance before state" (mkDQNState network network)
    transition <- requireRight "provenance transition" (mkTerminalTransition [1, 0] mask 0 1 0)
    buffer <- requireRight "provenance buffer" (mkReplayBuffer 2)
    let (replayId, _) = appendReplay transition buffer
        probe = mkInspectionProbe "replay evidence" [1, 0] mask (Just replayId)
    optimizer <- requireRight "provenance optimizer" (mkSGD 0.1)
    schedule <- requireRight "provenance schedule" (periodicHardTargetUpdates 1)
    config <- requireRight "provenance config" (mkDQNConfig 0 optimizer StandardDQN schedule)
    update <- requireRight "provenance update" (updateDQNBatch config before [transition])
    report <- requireRight "DQN count audit" (auditDQN 1 "initial" before "one update" (dqnUpdatedState update) [probe])
    assert "DQN successful-update counters are provenance" (dqnAuditBeforeTargetUpdateCount report == 0 && dqnAuditAfterTargetUpdateCount report == 1)
    case dqnAuditProbes report of
        [found] -> assert "replay ID preserved without deriving a checkpoint" (inspectionProbeReplayId (dqnProbeInput found) == Just replayId)
        _ -> assert "one replay probe expected" False

validationChecks :: IO ()
validationChecks = do
    mask <- requireRight "validation mask" (mkActionMask 2 [0, 1])
    wrongMask <- requireRight "validation wrong mask" (mkActionMask 3 [0, 1])
    network <- dense2 "validation network" (replicate 6 0)
    state <- requireRight "validation state" (mkDQNState network network)
    let probe = mkInspectionProbe "bad width" [1] mask Nothing
        wrongMaskProbe = mkInspectionProbe "bad mask" [1, 2] wrongMask Nothing
        result limit = auditDQN limit "before" state "after" state
    case result 0 [probe] of
        Left (InvalidInspectionProbeLimit 0) -> pure ()
        found -> assert ("nonpositive limit: " ++ show found) False
    case result 2 [] of
        Left EmptyInspectionProbes -> pure ()
        _ -> assert "empty probes were accepted" False
    case result 1 [probe, probe] of
        Left (InspectionProbeLimitExceeded 1) -> pure ()
        found -> assert ("probe limit did not precede model evaluation: " ++ show found) False
    case result 1 [wrongMaskProbe] of
        Left (InspectionMaskWidthMismatch 2 3) -> pure ()
        found -> assert ("mask width mismatch: " ++ show found) False
    different <- requireRight "different dense topology" (mkDenseNetwork 2 [2] 2 (replicate 12 0))
    differentState <- requireRight "different DQN state" (mkDQNState different different)
    case auditDQN 1 "before" state "after" differentState [mkInspectionProbe "valid" [1, 2] mask Nothing] of
        Left DQNInspectionTopologyMismatch -> pure ()
        found -> assert ("DQN topology mismatch: " ++ show found) False
    scalarMask <- requireRight "scalar mask" (mkActionMask 1 [0])
    beforeOverflow <- requireRight "negative large network" (mkDenseNetwork 1 [] 1 [0, -1e308])
    afterOverflow <- requireRight "positive large network" (mkDenseNetwork 1 [] 1 [0, 1e308])
    beforeOverflowState <- requireRight "negative large state" (mkDQNState beforeOverflow beforeOverflow)
    afterOverflowState <- requireRight "positive large state" (mkDQNState afterOverflow afterOverflow)
    let overflowProbe = mkInspectionProbe "checked delta" [0] scalarMask Nothing
    case auditDQN 1 "negative" beforeOverflowState "positive" afterOverflowState [overflowProbe] of
        Left (InspectionNumericFailure _) -> pure ()
        found -> assert ("nonfinite DQN delta: " ++ show found) False

policySnapshotChecks :: IO ()
policySnapshotChecks = do
    before <- requireRight "before policy" (mkLinearCategoricalPolicy 2 1 [0, 0])
    config <- requireRight "policy update config" (mkReinforceConfig 1 0 (log 3) 0)
    trainingMask <- requireRight "policy training mask" (mkActionMask 2 [0, 1])
    update <- requireRight "policy update" (updateReinforce config before Nothing [ReinforceStep [1] trainingMask 1 1] (TerminalBoundary 0))
    let after = reinforceUpdatedPolicy update
    mask <- requireRight "policy reordered mask" (mkActionMask 2 [1, 0])
    single <- requireRight "policy single mask" (mkActionMask 2 [0])
    let firstProbe = mkInspectionProbe "policy first" [1] mask Nothing
        secondProbe = mkInspectionProbe "policy single" [1] single Nothing
    report <- requireRight "policy audit" (auditLinearPolicy 2 "pre" before "post" after [firstProbe, secondProbe])
    assert "policy labels" (linearPolicyAuditBeforeLabel report == "pre" && linearPolicyAuditAfterLabel report == "post")
    assert "policy before snapshot unchanged" (linearPolicyParameters before == [0, 0])
    assertVectorClose "policy update parameters" 1e-15 [-(log 3 / 2), log 3 / 2] (linearPolicyParameters after)
    case linearPolicyAuditProbes report of
        [first, second] -> do
            let actions = linearPolicyProbeActions first
            assert "policy mask order" (map actionProbabilityIndex actions == [1, 0])
            assertVectorClose "policy baseline probabilities" 1e-15 [0.5, 0.5] (map actionProbabilityBefore actions)
            assertVectorClose "policy after probabilities" 1e-15 [0.75, 0.25] (map actionProbabilityAfter actions)
            assertVectorClose "policy probability deltas" 1e-15 [0.25, -0.25] (map actionProbabilityDelta actions)
            assertVectorClose "policy log probability deltas" 1e-15 [log 1.5, log 0.5] (map actionLogProbabilityDelta actions)
            assert "single policy action" (map actionProbabilityIndex (linearPolicyProbeActions second) == [0])
            assertVectorClose "one-action policy delta" 0 [0] (map actionProbabilityDelta (linearPolicyProbeActions second))
        found -> assert ("unexpected policy probe count: " ++ show (length found)) False
    wrongShape <- requireRight "policy action mismatch fixture" (mkLinearCategoricalPolicy 3 1 [0, 0, 0])
    case auditLinearPolicy 1 "pre" before "post" wrongShape [firstProbe] of
        Left (LinearPolicyInspectionShapeMismatch 1 2 1 3) -> pure ()
        found -> assert ("policy shape mismatch: " ++ show found) False
    wrongMask <- requireRight "policy wrong mask" (mkActionMask 3 [0])
    case auditLinearPolicy 1 "pre" before "post" after [mkInspectionProbe "wrong mask" [1] wrongMask Nothing] of
        Left (InspectionMaskWidthMismatch 2 3) -> pure ()
        found -> assert ("policy mask mismatch: " ++ show found) False

dense2 :: String -> [Double] -> IO DenseNetwork
dense2 label = requireRight label . mkDenseNetwork 2 [] 2
