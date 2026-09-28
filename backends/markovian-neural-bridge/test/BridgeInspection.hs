module BridgeInspection (tests) where

import Data.Maybe (isNothing)
import Markovian.Action (actionId)
import Markovian.Backend.Neural.Bridge.ExactSupportMask (
    denseActionOutputLayout,
    policyActionOutputLayout,
 )
import Markovian.Backend.Neural.Bridge.Inspection (
    NamedInspectionError (..),
    auditNamedDQN,
    auditNamedLinearPolicy,
    namedAuditActionAtIndex,
    namedAuditActions,
    namedAuditResult,
 )
import Markovian.Backend.Neural.DQN (mkDQNState)
import Markovian.Backend.Neural.Dense (mkDenseNetwork)
import Markovian.Backend.Neural.Inspection (
    InspectionError (..),
    actionProbabilityIndex,
    actionValueIndex,
    dqnAuditProbes,
    dqnProbeOnline,
    inspectionProbeReplayId,
    linearPolicyAuditProbes,
    linearPolicyProbeActions,
    linearPolicyProbeInput,
    mkInspectionProbe,
 )
import Markovian.Backend.Neural.Mask (mkActionMask)
import Markovian.Backend.Neural.Policy (mkLinearCategoricalPolicy)
import Markovian.Backend.Neural.Replay (appendReplay, mkReplayBuffer)
import Markovian.Backend.Neural.Transition (mkTerminalTransition)
import Markovian.Compile.Exact (finiteActionIndex)

data TestAction = A | B | C
    deriving (Eq, Show)

tests :: IO ()
tests = do
    beforePolicy <- requireRight "before policy" (mkLinearCategoricalPolicy 2 1 [0, 1])
    afterPolicy <- requireRight "after policy" (mkLinearCategoricalPolicy 2 1 [2, 3])
    mask <- requireRight "reordered mask" (mkActionMask 2 [1, 0])
    exactActions <- requireRight "exact actions" (finiteActionIndex [actionId A, actionId B])
    reversedActions <- requireRight "reversed exact actions" (finiteActionIndex [actionId B, actionId A])
    beforePolicyLayout <- requireRight "before policy layout" (policyActionOutputLayout exactActions beforePolicy)
    afterPolicyLayout <- requireRight "after policy layout" (policyActionOutputLayout exactActions afterPolicy)
    reversedPolicyLayout <- requireRight "reversed policy layout" (policyActionOutputLayout reversedActions afterPolicy)
    transition <- requireRight "provenance transition" (mkTerminalTransition [1] mask 1 0 0)
    replay <- requireRight "replay buffer" (mkReplayBuffer 1)
    let (replayId, _) = appendReplay transition replay
        probe = mkInspectionProbe "fixed-observation" [1] mask (Just replayId)
    policyAudit <-
        requireRight
            "named policy audit"
            (auditNamedLinearPolicy 1 "before" beforePolicy beforePolicyLayout "after" afterPolicy afterPolicyLayout [probe])
    assert "global action names" (namedAuditActions policyAudit == [actionId A, actionId B])
    assert "name resolves global index" (namedAuditActionAtIndex policyAudit 1 == Just (actionId B))
    assert "negative action index is absent" (isNothing (namedAuditActionAtIndex policyAudit (-1)))
    assert "out-of-range action index is absent" (isNothing (namedAuditActionAtIndex policyAudit 2))
    case linearPolicyAuditProbes (namedAuditResult policyAudit) of
        [entry] -> do
            assert "typed replay ID retained" (inspectionProbeReplayId (linearPolicyProbeInput entry) == Just replayId)
            assert "mask order retained" (map actionProbabilityIndex (linearPolicyProbeActions entry) == [1, 0])
        _ -> fail "expected one policy probe"
    case auditNamedLinearPolicy 1 "before" beforePolicy beforePolicyLayout "after" afterPolicy reversedPolicyLayout [probe] of
        Left NamedActionLayoutMismatch -> pure ()
        _ -> fail "reordered action names were accepted"

    beforeHead <- requireRight "before dense head" (mkDenseNetwork 1 [] 2 [1, 2, 0, 0])
    afterHead <- requireRight "after dense head" (mkDenseNetwork 1 [] 2 [2, 4, 0, 0])
    beforeState <- requireRight "before DQN state" (mkDQNState beforeHead beforeHead)
    afterState <- requireRight "after DQN state" (mkDQNState afterHead afterHead)
    beforeDenseLayout <- requireRight "before dense layout" (denseActionOutputLayout exactActions beforeHead)
    afterDenseLayout <- requireRight "after dense layout" (denseActionOutputLayout exactActions afterHead)
    reversedDenseLayout <- requireRight "reversed dense layout" (denseActionOutputLayout reversedActions afterHead)
    dqnAudit <-
        requireRight
            "named DQN audit"
            (auditNamedDQN 1 "before" beforeState beforeDenseLayout "after" afterState afterDenseLayout [probe])
    case dqnAuditProbes (namedAuditResult dqnAudit) of
        [entry] -> assert "DQN mask order retained" (map actionValueIndex (dqnProbeOnline entry) == [1, 0])
        _ -> fail "expected one DQN probe"
    case auditNamedDQN 1 "before" beforeState beforeDenseLayout "after" afterState reversedDenseLayout [probe] of
        Left NamedActionLayoutMismatch -> pure ()
        _ -> fail "reordered DQN action names were accepted"

    threeHead <- requireRight "three-action head" (mkDenseNetwork 1 [] 3 [0, 0, 0, 0, 0, 0])
    threeActions <- requireRight "three exact actions" (finiteActionIndex [actionId A, actionId B, actionId C])
    wrongLayout <- requireRight "three-action layout" (denseActionOutputLayout threeActions threeHead)
    case auditNamedDQN 1 "before" beforeState beforeDenseLayout "after" afterState wrongLayout [probe] of
        Left (NamedAfterHeadWidthMismatch 2 3) -> pure ()
        _ -> fail "layout for a different head width was accepted"
    case auditNamedDQN 1 "before" beforeState wrongLayout "after" afterState afterDenseLayout [probe] of
        Left (NamedBeforeHeadWidthMismatch 2 3) -> pure ()
        _ -> fail "before layout for a different head width was accepted"
    wrongMask <- requireRight "wrong audit mask" (mkActionMask 3 [0, 1])
    let invalidProbe = mkInspectionProbe "invalid mask" [1] wrongMask Nothing
    case auditNamedDQN 1 "before" beforeState beforeDenseLayout "after" afterState afterDenseLayout [invalidProbe] of
        Left (NamedCoreInspectionFailure (InspectionMaskWidthMismatch 2 3)) -> pure ()
        _ -> fail "core audit failure was not preserved"
    putStrLn "PASS: named frozen-checkpoint audits"

assert :: String -> Bool -> IO ()
assert _ True = pure ()
assert message False = fail message

requireRight :: (Show error) => String -> Either error value -> IO value
requireRight _ (Right value) = pure value
requireRight label (Left err) = fail (label ++ ": " ++ show err)
