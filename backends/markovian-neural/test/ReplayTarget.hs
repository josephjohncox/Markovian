module ReplayTarget (tests) where

import Markovian.Backend.Neural (
    ReplacementMode (..),
    ReplayError (..),
    TargetNetworkError (..),
    TransitionError (..),
    afterSuccessfulUpdate,
    appendReplay,
    denseParameters,
    hardSynchronizeTarget,
    mkActionMask,
    mkDenseNetwork,
    mkReplayBuffer,
    mkTargetNetwork,
    mkTerminalTransition,
    periodicHardTargetUpdates,
    polyakSynchronizeTarget,
    polyakTargetUpdates,
    replayEntries,
    replayEntryId,
    replayEntryIdOrdinal,
    replaySize,
    selectReplay,
    targetNetworkSnapshot,
    targetSuccessfulUpdateCount,
 )
import TestSupport (assert, assertVectorClose, requireRight)

tests :: IO ()
tests = do
    replayChecks
    targetChecks
    putStrLn "PASS: replay and target networks"

replayChecks :: IO ()
replayChecks = do
    case mkReplayBuffer 0 of
        Left (InvalidReplayCapacity 0) -> pure ()
        result -> assert ("zero replay capacity accepted: " ++ show result) False
    case mkActionMask [] of
        Left EmptyActionMask -> pure ()
        result -> assert ("empty action mask accepted: " ++ show result) False
    case mkActionMask [0, 0] of
        Left (DuplicateActionIndex 0) -> pure ()
        result -> assert ("duplicate action mask accepted: " ++ show result) False
    mask <- requireRight "replay mask" (mkActionMask [0])
    case mkTerminalTransition [] mask 0 0 0 of
        Left (EmptyFeatureVector _) -> pure ()
        result -> assert ("empty transition features accepted: " ++ show result) False
    case mkTerminalTransition [0 / 0] mask 0 0 0 of
        Left (TransitionNumericFailure _) -> pure ()
        result -> assert ("non-finite transition features accepted: " ++ show result) False
    firstTransition <- requireRight "first transition" (mkTerminalTransition [1] mask 0 1 0)
    secondTransition <- requireRight "second transition" (mkTerminalTransition [2] mask 0 2 0)
    thirdTransition <- requireRight "third transition" (mkTerminalTransition [3] mask 0 3 0)
    empty <- requireRight "replay capacity" (mkReplayBuffer 2)
    let (firstId, one) = appendReplay firstTransition empty
        (secondId, two) = appendReplay secondTransition one
        (thirdId, three) = appendReplay thirdTransition two
    assert "bounded replay size" (replaySize three == 2)
    assert
        "FIFO eviction and stable IDs"
        ( fmap (replayEntryIdOrdinal . replayEntryId) (replayEntries three) == [1, 2]
            && fmap replayEntryIdOrdinal [firstId, secondId, thirdId] == [0, 1, 2]
        )
    ordered <- requireRight "ordered replay selection" (selectReplay WithoutReplacement [thirdId, secondId] three)
    assert
        "requested replay order"
        (fmap (replayEntryIdOrdinal . replayEntryId) ordered == [2, 1])
    repeated <- requireRight "replacement replay selection" (selectReplay WithReplacement [secondId, secondId] three)
    assert "with-replacement selection" (length repeated == 2)
    case selectReplay WithoutReplacement [secondId, secondId] three of
        Left (DuplicateReplaySelection 1) -> pure ()
        result -> assert ("duplicate without-replacement selection accepted: " ++ show result) False
    case selectReplay WithReplacement [firstId] three of
        Left (UnknownReplayEntryId 0) -> pure ()
        result -> assert ("evicted replay ID selected: " ++ show result) False
    otherEmpty <- requireRight "other replay" (mkReplayBuffer 4)
    let (_, otherOne) = appendReplay firstTransition otherEmpty
        (_, otherTwo) = appendReplay secondTransition otherOne
        (_, otherThree) = appendReplay thirdTransition otherTwo
        (foreignId, _) = appendReplay firstTransition otherThree
    case selectReplay WithReplacement [foreignId] three of
        Left (UnknownReplayEntryId 3) -> pure ()
        result -> assert ("foreign unknown replay ID selected: " ++ show result) False

targetChecks :: IO ()
targetChecks = do
    online0 <- requireRight "target online zero" (mkDenseNetwork 1 [] 1 [0, 0])
    online2 <- requireRight "target online two" (mkDenseNetwork 1 [] 1 [2, 2])
    incompatible <- requireRight "incompatible target" (mkDenseNetwork 2 [] 1 [0, 0, 0])
    let initial = mkTargetNetwork online0
    case hardSynchronizeTarget incompatible initial of
        Left TargetNetworkTopologyMismatch -> pure ()
        result -> assert ("incompatible hard synchronization accepted: " ++ show result) False
    case periodicHardTargetUpdates 0 of
        Left (InvalidPeriodicHardSyncPeriod 0) -> pure ()
        result -> assert ("zero target period accepted: " ++ show result) False
    case polyakTargetUpdates 0 of
        Left (InvalidPolyakCoefficient 0) -> pure ()
        result -> assert ("zero soft-target coefficient accepted: " ++ show result) False
    case polyakSynchronizeTarget 1.1 online2 initial of
        Left (InvalidPolyakCoefficient 1.1) -> pure ()
        result -> assert ("large soft-target coefficient accepted: " ++ show result) False
    hard <- requireRight "hard target sync" (hardSynchronizeTarget online2 initial)
    assertVectorClose "hard target values" 0 [2, 2] (denseParameters (targetNetworkSnapshot hard))
    polyak <- requireRight "Polyak target sync" (polyakSynchronizeTarget 0.25 online2 initial)
    assertVectorClose "Polyak target values" 1e-15 [0.5, 0.5] (denseParameters (targetNetworkSnapshot polyak))
    schedule <- requireRight "periodic target schedule" (periodicHardTargetUpdates 2)
    afterOne <- requireRight "target after one" (afterSuccessfulUpdate schedule online2 initial)
    assert "target count after one" (targetSuccessfulUpdateCount afterOne == 1)
    assertVectorClose "no early hard sync" 0 [0, 0] (denseParameters (targetNetworkSnapshot afterOne))
    afterTwo <- requireRight "target after two" (afterSuccessfulUpdate schedule online2 afterOne)
    assert "target count after two" (targetSuccessfulUpdateCount afterTwo == 2)
    assertVectorClose "periodic hard sync timing" 0 [2, 2] (denseParameters (targetNetworkSnapshot afterTwo))
    case afterSuccessfulUpdate schedule incompatible initial of
        Left TargetNetworkTopologyMismatch -> pure ()
        result -> assert ("incompatible scheduled update accepted: " ++ show result) False
    assert "failed event leaves caller state unchanged" (targetSuccessfulUpdateCount initial == 0)
