{- | Attach a checked exact action layout to a frozen neural-model audit.

The neural audit reports global action indices. These wrappers require equal
caller-supplied action-layout witnesses, each checked against its head width,
before returning exact names for those indices. The caller still owns the
models' action meanings, feature map, and snapshot provenance.
-}
module Markovian.Backend.Neural.Bridge.Inspection (
    NamedInspectionError (..),
    NamedAudit,
    namedAuditResult,
    namedAuditActions,
    namedAuditActionAtIndex,
    auditNamedDQN,
    auditNamedLinearPolicy,
) where

import Markovian.Action (ActionId)
import Markovian.Backend.Neural.Bridge.ExactSupportMask (
    ActionOutputLayout,
    actionOutputLayoutActions,
    actionOutputLayoutWidth,
    sameActionOutputLayout,
 )
import Markovian.Backend.Neural.DQN (DQNState, dqnOnlineNetwork)
import Markovian.Backend.Neural.Dense (denseOutputSize)
import Markovian.Backend.Neural.Inspection (
    DQNAudit,
    InspectionError,
    InspectionProbe,
    LinearPolicyAudit,
    auditDQN,
    auditLinearPolicy,
 )
import Markovian.Backend.Neural.Policy (LinearCategoricalPolicy, linearPolicyActionCount)

-- | Layout or underlying numerical audit failure.
data NamedInspectionError
    = NamedBeforeHeadWidthMismatch !Int !Int
    | NamedAfterHeadWidthMismatch !Int !Int
    | NamedActionLayoutMismatch
    | NamedCoreInspectionFailure !InspectionError
    deriving (Eq, Show)

-- | An audit and the caller-supplied action order checked against both head widths.
data NamedAudit action result = NamedAudit !(ActionOutputLayout action) !result
    deriving (Eq, Show)

-- | The underlying index-based numerical report.
namedAuditResult :: NamedAudit action result -> result
namedAuditResult (NamedAudit _ result) = result

-- | Exact action IDs in global neural-output order.
namedAuditActions :: NamedAudit action result -> [ActionId action]
namedAuditActions (NamedAudit layout _) = actionOutputLayoutActions layout

-- | Resolve a reported global action index without guessing its meaning.
namedAuditActionAtIndex :: NamedAudit action result -> Int -> Maybe (ActionId action)
namedAuditActionAtIndex audit index
    | index < 0 = Nothing
    | otherwise = go index (namedAuditActions audit)
  where
    go _ [] = Nothing
    go 0 (action : _) = Just action
    go remaining (_ : actions) = go (remaining - 1) actions

-- | Compare frozen DQN snapshots only under an identical exact action layout.
auditNamedDQN ::
    (Eq action) =>
    Int ->
    String ->
    DQNState ->
    ActionOutputLayout action ->
    String ->
    DQNState ->
    ActionOutputLayout action ->
    [InspectionProbe probeId] ->
    Either NamedInspectionError (NamedAudit action (DQNAudit probeId))
auditNamedDQN limit beforeLabel before beforeLayout afterLabel after afterLayout probes = do
    validateLayouts
        (denseOutputSize (dqnOnlineNetwork before))
        beforeLayout
        (denseOutputSize (dqnOnlineNetwork after))
        afterLayout
    result <- mapCore (auditDQN limit beforeLabel before afterLabel after probes)
    Right (NamedAudit beforeLayout result)

-- | Compare frozen linear policies only under an identical exact action layout.
auditNamedLinearPolicy ::
    (Eq action) =>
    Int ->
    String ->
    LinearCategoricalPolicy ->
    ActionOutputLayout action ->
    String ->
    LinearCategoricalPolicy ->
    ActionOutputLayout action ->
    [InspectionProbe probeId] ->
    Either NamedInspectionError (NamedAudit action (LinearPolicyAudit probeId))
auditNamedLinearPolicy limit beforeLabel before beforeLayout afterLabel after afterLayout probes = do
    validateLayouts
        (linearPolicyActionCount before)
        beforeLayout
        (linearPolicyActionCount after)
        afterLayout
    result <- mapCore (auditLinearPolicy limit beforeLabel before afterLabel after probes)
    Right (NamedAudit beforeLayout result)

validateLayouts ::
    (Eq action) =>
    Int ->
    ActionOutputLayout action ->
    Int ->
    ActionOutputLayout action ->
    Either NamedInspectionError ()
validateLayouts beforeWidth beforeLayout afterWidth afterLayout
    | actionOutputLayoutWidth beforeLayout /= beforeWidth =
        Left (NamedBeforeHeadWidthMismatch beforeWidth (actionOutputLayoutWidth beforeLayout))
    | actionOutputLayoutWidth afterLayout /= afterWidth =
        Left (NamedAfterHeadWidthMismatch afterWidth (actionOutputLayoutWidth afterLayout))
    | not (sameActionOutputLayout beforeLayout afterLayout) =
        Left NamedActionLayoutMismatch
    | otherwise = Right ()

mapCore :: Either InspectionError result -> Either NamedInspectionError result
mapCore = either (Left . NamedCoreInspectionFailure) Right
