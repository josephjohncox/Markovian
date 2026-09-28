{- | Read-only comparisons of explicitly supplied frozen neural snapshots.

The caller supplies both snapshots and one fixed set of probes. Action indices
are numeric model outputs; this module cannot establish that two independently
named action layouts or learned hidden units have the same meaning. A replay
ordinal is provenance within a caller-managed buffer lineage, not a checkpoint
identifier. DQN target counters record successful updates, not the cause of a
particular output difference.
-}
module Markovian.Backend.Neural.Inspection (
    InspectionError (..),
    InspectionProbe,
    mkInspectionProbe,
    inspectionProbeId,
    inspectionProbeFeatures,
    inspectionProbeMask,
    inspectionProbeReplayId,
    ActionValueComparison,
    actionValueIndex,
    actionValueBefore,
    actionValueAfter,
    actionValueDelta,
    DQNProbeComparison,
    dqnProbeInput,
    dqnProbeOnline,
    dqnProbeTarget,
    DQNAudit,
    dqnAuditBeforeLabel,
    dqnAuditAfterLabel,
    dqnAuditBeforeTargetUpdateCount,
    dqnAuditAfterTargetUpdateCount,
    dqnAuditProbes,
    auditDQN,
    ActionProbabilityComparison,
    actionProbabilityIndex,
    actionProbabilityBefore,
    actionProbabilityAfter,
    actionProbabilityDelta,
    actionLogProbabilityBefore,
    actionLogProbabilityAfter,
    actionLogProbabilityDelta,
    LinearPolicyProbeComparison,
    linearPolicyProbeInput,
    linearPolicyProbeActions,
    LinearPolicyAudit,
    linearPolicyAuditBeforeLabel,
    linearPolicyAuditAfterLabel,
    linearPolicyAuditProbes,
    auditLinearPolicy,
) where

import Data.Foldable (traverse_)
import Markovian.Backend.Neural.DQN (
    DQNState,
    dqnOnlineNetwork,
    dqnTargetNetwork,
 )
import Markovian.Backend.Neural.Dense (
    DenseError,
    DenseNetwork,
    denseForward,
    denseOutputSize,
    sameDenseTopology,
 )
import Markovian.Backend.Neural.Mask (
    ActionMask,
    ActionMaskError,
    actionMaskIndices,
    actionMaskWidth,
    gatherActionMask,
 )
import Markovian.Backend.Neural.Numeric (
    NeuralNumericError,
    checkedSubtract,
 )
import Markovian.Backend.Neural.Policy (
    LinearCategoricalPolicy,
    NeuralPolicyError,
    inspectLinearPolicy,
    linearPolicyActionCount,
    linearPolicyFeatureCount,
    linearPolicyInspectedMaskedActions,
    linearPolicyMaskedAction,
    linearPolicyMaskedLogProbability,
    linearPolicyMaskedProbability,
 )
import Markovian.Backend.Neural.Replay (ReplayEntryId)
import Markovian.Backend.Neural.TargetNetwork (
    targetNetworkSnapshot,
    targetSuccessfulUpdateCount,
 )
import Numeric.Natural (Natural)

-- | Probe bounds, incompatible snapshots or masks, and checked evaluation failures.
data InspectionError
    = InvalidInspectionProbeLimit !Int
    | EmptyInspectionProbes
    | InspectionProbeLimitExceeded !Int
    | DQNInspectionTopologyMismatch
    | LinearPolicyInspectionShapeMismatch !Int !Int !Int !Int
    | InspectionMaskWidthMismatch !Int !Int
    | InspectionDenseFailure !DenseError
    | InspectionMaskFailure !ActionMaskError
    | InspectionPolicyFailure !NeuralPolicyError
    | InspectionNumericFailure !NeuralNumericError
    deriving (Eq, Show)

{- | One caller-identified observation and ordered action mask. The optional
replay ordinal is meaningful only within the caller's replay-buffer lineage.
-}
data InspectionProbe identifier = InspectionProbe
    { inspectionProbeId :: identifier
    , inspectionProbeFeatures :: ![Double]
    , inspectionProbeMask :: !ActionMask
    , inspectionProbeReplayId :: !(Maybe ReplayEntryId)
    }
    deriving (Eq, Show)

{- | Bind provenance to an observation. Audit functions validate the features
and mask against both snapshots before returning any report.
-}
mkInspectionProbe :: identifier -> [Double] -> ActionMask -> Maybe ReplayEntryId -> InspectionProbe identifier
mkInspectionProbe = InspectionProbe

-- | An admissible DQN action value in mask order at both checkpoints.
data ActionValueComparison = ActionValueComparison
    { actionValueIndex :: !Int
    , actionValueBefore :: !Double
    , actionValueAfter :: !Double
    , actionValueDelta :: !Double
    }
    deriving (Eq, Show)

{- | Online and target values for one shared probe. Both lists follow the
probe's mask order and contain every admissible action.
-}
data DQNProbeComparison identifier = DQNProbeComparison
    { dqnProbeInput :: !(InspectionProbe identifier)
    , dqnProbeOnline :: ![ActionValueComparison]
    , dqnProbeTarget :: ![ActionValueComparison]
    }
    deriving (Eq, Show)

-- | A complete frozen DQN comparison, with target successful-update counters.
data DQNAudit identifier = DQNAudit
    { dqnAuditBeforeLabel :: !String
    , dqnAuditAfterLabel :: !String
    , dqnAuditBeforeTargetUpdateCount :: !Natural
    , dqnAuditAfterTargetUpdateCount :: !Natural
    , dqnAuditProbes :: ![DQNProbeComparison identifier]
    }
    deriving (Eq, Show)

{- | Compare two supplied DQN states over the same nonempty bounded probes.

The target can change through synchronization as well as online updates. These
results measure snapshot behavior; they do not attribute it to an SGD step or
measure policy quality.
-}
auditDQN :: Int -> String -> DQNState -> String -> DQNState -> [InspectionProbe identifier] -> Either InspectionError (DQNAudit identifier)
auditDQN limit beforeLabel beforeState afterLabel afterState probes = do
    validateProbeCount limit probes
    if sameDenseTopology beforeOnline afterOnline
        && sameDenseTopology beforeTarget afterTarget
        && sameDenseTopology beforeOnline beforeTarget
        && sameDenseTopology afterOnline afterTarget
        then Right ()
        else Left DQNInspectionTopologyMismatch
    traverse_ (validateMaskWidth (denseOutputSize beforeOnline)) probes
    comparisons <- traverse compareProbe probes
    Right
        DQNAudit
            { dqnAuditBeforeLabel = beforeLabel
            , dqnAuditAfterLabel = afterLabel
            , dqnAuditBeforeTargetUpdateCount = targetSuccessfulUpdateCount (dqnTargetNetwork beforeState)
            , dqnAuditAfterTargetUpdateCount = targetSuccessfulUpdateCount (dqnTargetNetwork afterState)
            , dqnAuditProbes = comparisons
            }
  where
    beforeOnline = dqnOnlineNetwork beforeState
    afterOnline = dqnOnlineNetwork afterState
    beforeTarget = targetNetworkSnapshot (dqnTargetNetwork beforeState)
    afterTarget = targetNetworkSnapshot (dqnTargetNetwork afterState)
    compareProbe probe = do
        online <- compareDense probe beforeOnline afterOnline
        target <- compareDense probe beforeTarget afterTarget
        Right (DQNProbeComparison probe online target)

compareDense :: InspectionProbe identifier -> DenseNetwork -> DenseNetwork -> Either InspectionError [ActionValueComparison]
compareDense probe before after = do
    beforeAll <- mapDense (denseForward before (inspectionProbeFeatures probe))
    afterAll <- mapDense (denseForward after (inspectionProbeFeatures probe))
    let mask = inspectionProbeMask probe
    beforeMasked <- mapMask (gatherActionMask mask beforeAll)
    afterMasked <- mapMask (gatherActionMask mask afterAll)
    traverse
        ( \(index, beforeValue, afterValue) -> do
            delta <- mapNumeric (checkedSubtract "DQN audit action value delta" afterValue beforeValue)
            Right (ActionValueComparison index beforeValue afterValue delta)
        )
        (zip3 (actionMaskIndices mask) beforeMasked afterMasked)

-- | A masked policy action's probability and log-probability comparison.
data ActionProbabilityComparison = ActionProbabilityComparison
    { actionProbabilityIndex :: !Int
    , actionProbabilityBefore :: !Double
    , actionProbabilityAfter :: !Double
    , actionProbabilityDelta :: !Double
    , actionLogProbabilityBefore :: !Double
    , actionLogProbabilityAfter :: !Double
    , actionLogProbabilityDelta :: !Double
    }
    deriving (Eq, Show)

-- | All admissible policy actions for one shared probe, in mask order.
data LinearPolicyProbeComparison identifier = LinearPolicyProbeComparison
    { linearPolicyProbeInput :: !(InspectionProbe identifier)
    , linearPolicyProbeActions :: ![ActionProbabilityComparison]
    }
    deriving (Eq, Show)

-- | A complete comparison of two explicitly supplied linear policy snapshots.
data LinearPolicyAudit identifier = LinearPolicyAudit
    { linearPolicyAuditBeforeLabel :: !String
    , linearPolicyAuditAfterLabel :: !String
    , linearPolicyAuditProbes :: ![LinearPolicyProbeComparison identifier]
    }
    deriving (Eq, Show)

{- | Compare masked action probabilities for a bounded fixed probe set. This
does not estimate reward or improvement on a training distribution.
-}
auditLinearPolicy :: Int -> String -> LinearCategoricalPolicy -> String -> LinearCategoricalPolicy -> [InspectionProbe identifier] -> Either InspectionError (LinearPolicyAudit identifier)
auditLinearPolicy limit beforeLabel beforePolicy afterLabel afterPolicy probes = do
    validateProbeCount limit probes
    let beforeFeatures = linearPolicyFeatureCount beforePolicy
        afterFeatures = linearPolicyFeatureCount afterPolicy
        beforeActions = linearPolicyActionCount beforePolicy
        afterActions = linearPolicyActionCount afterPolicy
    if beforeFeatures == afterFeatures && beforeActions == afterActions
        then Right ()
        else Left (LinearPolicyInspectionShapeMismatch beforeFeatures beforeActions afterFeatures afterActions)
    traverse_ (validateMaskWidth beforeActions) probes
    comparisons <- traverse compareProbe probes
    Right
        LinearPolicyAudit
            { linearPolicyAuditBeforeLabel = beforeLabel
            , linearPolicyAuditAfterLabel = afterLabel
            , linearPolicyAuditProbes = comparisons
            }
  where
    compareProbe probe = do
        let inputs = inspectionProbeFeatures probe
            mask = inspectionProbeMask probe
        before <- mapPolicy (inspectLinearPolicy beforePolicy inputs mask)
        after <- mapPolicy (inspectLinearPolicy afterPolicy inputs mask)
        actions <-
            traverse
                ( \(beforeAction, afterAction) -> do
                    let beforeProbability = linearPolicyMaskedProbability beforeAction
                        afterProbability = linearPolicyMaskedProbability afterAction
                        beforeLogProbability = linearPolicyMaskedLogProbability beforeAction
                        afterLogProbability = linearPolicyMaskedLogProbability afterAction
                    probabilityDelta <- mapNumeric (checkedSubtract "policy audit action probability delta" afterProbability beforeProbability)
                    logProbabilityDelta <- mapNumeric (checkedSubtract "policy audit action log-probability delta" afterLogProbability beforeLogProbability)
                    Right
                        ActionProbabilityComparison
                            { actionProbabilityIndex = linearPolicyMaskedAction beforeAction
                            , actionProbabilityBefore = beforeProbability
                            , actionProbabilityAfter = afterProbability
                            , actionProbabilityDelta = probabilityDelta
                            , actionLogProbabilityBefore = beforeLogProbability
                            , actionLogProbabilityAfter = afterLogProbability
                            , actionLogProbabilityDelta = logProbabilityDelta
                            }
                )
                (zip (linearPolicyInspectedMaskedActions before) (linearPolicyInspectedMaskedActions after))
        Right (LinearPolicyProbeComparison probe actions)

validateProbeCount :: Int -> [InspectionProbe identifier] -> Either InspectionError ()
validateProbeCount limit _ | limit <= 0 = Left (InvalidInspectionProbeLimit limit)
validateProbeCount _ [] = Left EmptyInspectionProbes
validateProbeCount limit probes = go 0 probes
  where
    go _ [] = Right ()
    go count (_ : remaining)
        | count >= limit = Left (InspectionProbeLimitExceeded limit)
        | otherwise = go (count + 1) remaining

validateMaskWidth :: Int -> InspectionProbe identifier -> Either InspectionError ()
validateMaskWidth expected probe
    | actionMaskWidth (inspectionProbeMask probe) == expected = Right ()
    | otherwise = Left (InspectionMaskWidthMismatch expected (actionMaskWidth (inspectionProbeMask probe)))

mapDense :: Either DenseError value -> Either InspectionError value
mapDense = either (Left . InspectionDenseFailure) Right

mapMask :: Either ActionMaskError value -> Either InspectionError value
mapMask = either (Left . InspectionMaskFailure) Right

mapPolicy :: Either NeuralPolicyError value -> Either InspectionError value
mapPolicy = either (Left . InspectionPolicyFailure) Right

mapNumeric :: Either NeuralNumericError value -> Either InspectionError value
mapNumeric = either (Left . InspectionNumericFailure) Right
