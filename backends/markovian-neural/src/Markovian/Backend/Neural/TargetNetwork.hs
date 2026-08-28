{- | Dense target-network snapshots and deterministic synchronization.

Synchronization validates complete topology. Successful-update counters advance
only through 'afterSuccessfulUpdate'; a failed online update therefore cannot
advance a schedule or trigger synchronization.
-}
module Markovian.Backend.Neural.TargetNetwork (
    TargetNetworkError (..),
    TargetNetwork,
    mkTargetNetwork,
    targetNetworkSnapshot,
    targetSuccessfulUpdateCount,
    hardSynchronizeTarget,
    polyakSynchronizeTarget,
    TargetUpdateSchedule,
    noAutomaticTargetUpdates,
    periodicHardTargetUpdates,
    polyakTargetUpdates,
    afterSuccessfulUpdate,
) where

import Markovian.Backend.Neural.Dense (
    DenseError,
    DenseNetwork,
    denseParameters,
    replaceDenseParameters,
    sameDenseTopology,
 )
import Markovian.Backend.Neural.Numeric (
    NeuralNumericError,
    checkedAdd,
    checkedMultiply,
 )
import Numeric.Natural (Natural)

-- | Topology, schedule, and arithmetic failures.
data TargetNetworkError
    = TargetNetworkTopologyMismatch
    | InvalidPeriodicHardSyncPeriod !Int
    | InvalidPolyakCoefficient !Double
    | TargetNetworkDenseFailure !DenseError
    | TargetNetworkNumericFailure !NeuralNumericError
    deriving (Eq, Show)

-- | A target snapshot and number of committed online updates.
data TargetNetwork = TargetNetwork !DenseNetwork !Natural
    deriving (Eq, Show)

-- | Start from a snapshot with no successful online updates.
mkTargetNetwork :: DenseNetwork -> TargetNetwork
mkTargetNetwork network = TargetNetwork network 0

-- | Current immutable target snapshot.
targetNetworkSnapshot :: TargetNetwork -> DenseNetwork
targetNetworkSnapshot (TargetNetwork network _) = network

-- | Number of committed online updates observed by this target state.
targetSuccessfulUpdateCount :: TargetNetwork -> Natural
targetSuccessfulUpdateCount (TargetNetwork _ count) = count

-- | Replace the target parameters with a topology-compatible online snapshot.
hardSynchronizeTarget :: DenseNetwork -> TargetNetwork -> Either TargetNetworkError TargetNetwork
hardSynchronizeTarget online (TargetNetwork target count)
    | not (sameDenseTopology online target) = Left TargetNetworkTopologyMismatch
    | otherwise = Right (TargetNetwork online count)

-- | Apply @target <- tau * online + (1-tau) * target@ without changing count.
polyakSynchronizeTarget :: Double -> DenseNetwork -> TargetNetwork -> Either TargetNetworkError TargetNetwork
polyakSynchronizeTarget tau online (TargetNetwork target count)
    | not (validTau tau) = Left (InvalidPolyakCoefficient tau)
    | not (sameDenseTopology online target) = Left TargetNetworkTopologyMismatch
    | otherwise = do
        parameters <- traverse blend (zip (denseParameters online) (denseParameters target))
        updated <- either (Left . TargetNetworkDenseFailure) Right (replaceDenseParameters parameters target)
        Right (TargetNetwork updated count)
  where
    blend (onlineParameter, targetParameter) = do
        onlinePart <- mapNumeric (checkedMultiply "Polyak online contribution" tau onlineParameter)
        targetPart <- mapNumeric (checkedMultiply "Polyak target contribution" (1 - tau) targetParameter)
        mapNumeric (checkedAdd "Polyak parameter" onlinePart targetPart)

{- | A validated automatic target-update schedule.

The constructor is private so periods and coefficients remain valid.
-}
data TargetUpdateSchedule
    = NoAutomaticTargetUpdates
    | PeriodicHardTargetUpdates !Int
    | PolyakTargetUpdates !Double
    deriving (Eq, Show)

-- | Never synchronize automatically, but still count successful updates.
noAutomaticTargetUpdates :: TargetUpdateSchedule
noAutomaticTargetUpdates = NoAutomaticTargetUpdates

-- | Synchronize after post-update counts divisible by a positive period.
periodicHardTargetUpdates :: Int -> Either TargetNetworkError TargetUpdateSchedule
periodicHardTargetUpdates period
    | period <= 0 = Left (InvalidPeriodicHardSyncPeriod period)
    | otherwise = Right (PeriodicHardTargetUpdates period)

-- | Polyak-synchronize after every successful update with @0 < tau <= 1@.
polyakTargetUpdates :: Double -> Either TargetNetworkError TargetUpdateSchedule
polyakTargetUpdates tau
    | validTau tau = Right (PolyakTargetUpdates tau)
    | otherwise = Left (InvalidPolyakCoefficient tau)

-- | Record one committed online update, then apply its configured schedule.
afterSuccessfulUpdate :: TargetUpdateSchedule -> DenseNetwork -> TargetNetwork -> Either TargetNetworkError TargetNetwork
afterSuccessfulUpdate schedule online (TargetNetwork target count)
    | not (sameDenseTopology online target) = Left TargetNetworkTopologyMismatch
    | otherwise =
        let nextCount = count + 1
            counted = TargetNetwork target nextCount
         in case schedule of
                NoAutomaticTargetUpdates -> Right counted
                PeriodicHardTargetUpdates period
                    | nextCount `mod` fromIntegral period == 0 -> hardSynchronizeTarget online counted
                    | otherwise -> Right counted
                PolyakTargetUpdates tau -> polyakSynchronizeTarget tau online counted

validTau :: Double -> Bool
validTau value = not (isNaN value || isInfinite value) && value > 0 && value <= 1

mapNumeric :: Either NeuralNumericError value -> Either TargetNetworkError value
mapNumeric = either (Left . TargetNetworkNumericFailure) Right
