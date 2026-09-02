-- | Deterministic reporting for the bounded serial-inventory benchmark.
module Markovian.Benchmark.Inventory.Report (
    InventoryBenchmarkReport,
    inventoryBenchmarkReport,
    renderInventoryBenchmarkReport,
    inventoryBenchmarkWarmupCount,
    inventoryBenchmarkSampleCount,
) where

import Data.List (intercalate)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio (denominator, numerator)
import Markovian.Benchmark.Inventory.Serial.Exact (
    BaseStockTargetGrid,
    SerialInventoryError,
    SerialInventoryParameters,
    SerialInventorySolution,
    TruncatedDemand,
    baseStockTargetCandidates,
    baseStockTargetPairs,
    boundedDemandOutcomes,
    compareSerialInventoryBounds,
    horizonExceededMass,
    omittedDemandMass,
    retainedDemandMass,
    serialInventoryBacklogCost,
    serialInventoryBaseStockCost,
    serialInventoryBaseStockReturn,
    serialInventoryBoundsStable,
    serialInventoryDemandCap,
    serialInventoryDiscount,
    serialInventoryDownstreamHoldingCost,
    serialInventoryHorizon,
    serialInventoryInitialValueError,
    serialInventoryOracleCost,
    serialInventoryOracleReturn,
    serialInventoryOrderCap,
    serialInventoryOrderCapBinds,
    serialInventoryPolicyRegret,
    serialInventorySelectedTargets,
    serialInventorySolutionInitialState,
    serialInventorySolutionParameters,
    serialInventorySolutionTargetGrid,
    serialInventorySolverStatus,
    serialInventoryStateActionCount,
    serialInventoryStateCount,
    serialInventoryTargetGridBinds,
    serialInventoryUpstreamHoldingCost,
    truncatedGeometricDemand,
 )
import Markovian.Objective.Exact (exactDiscountValue)
import Numeric.Natural (Natural)

-- | One warm-up is executed and excluded from benchmark statistics.
inventoryBenchmarkWarmupCount :: Natural
inventoryBenchmarkWarmupCount = 1

-- | Number of complete measured build, solve, and report executions.
inventoryBenchmarkSampleCount :: Natural
inventoryBenchmarkSampleCount = 20

-- | A complete deterministic semantic report, stored as rendered lines.
newtype InventoryBenchmarkReport = InventoryBenchmarkReport [String]
    deriving (Eq, Show)

{- | Validate the primary/widened relationship and assemble its report.

No independent fixture is accepted: all model and grid fields come from the
opaque solutions that the checked comparison validates.
-}
inventoryBenchmarkReport ::
    SerialInventorySolution ->
    SerialInventorySolution ->
    Either SerialInventoryError InventoryBenchmarkReport
inventoryBenchmarkReport primary widened = do
    stability <- compareSerialInventoryBounds primary widened
    Right
        ( InventoryBenchmarkReport
            [ "inventory benchmark report"
            , "provenance: synthetic bounded serial fixture"
            , "exactness boundary: exact values apply only to the conditional bounded-demand model"
            , "initial state: " ++ show (serialInventorySolutionInitialState primary)
            , "primary parameters: " ++ renderParameters primaryParameters
            , "widened parameters: " ++ renderParameters widenedParameters
            , "primary target grid (period 1 upward): " ++ renderGrid (serialInventorySolutionTargetGrid primary)
            , "widened target grid (period 1 upward): " ++ renderGrid (serialInventorySolutionTargetGrid widened)
            , "solver method: exact backward induction over periodsRemaining"
            , "event timing: receive supplierOrderDue; choose supplierOrder q and downstreamShipment x; ship x; observe demand; charge successor holding/backlog cost; set supplierOrderDue'=q"
            , "transition equations: upstreamOnHand'=upstreamOnHand+supplierOrderDue-x; downstreamNetInventory'=downstreamNetInventory+x-demand; periodsRemaining'=periodsRemaining-1"
            , "cost equation: h0*upstreamOnHand' + (h0+h1)*max(downstreamNetInventory',0) + p*max(-downstreamNetInventory',0)"
            , "discount convention: transition reward is negative cost; each successor value is multiplied by "
                ++ renderRational (exactDiscountValue (serialInventoryDiscount primaryParameters))
            , "terminal convention: periodsRemaining=0 has zero terminal payoff and no action"
            , "one-period retained demand mass: " ++ renderRational (retainedDemandMass primaryDemand)
            , "one-period omitted demand mass: " ++ renderRational (omittedDemandMass primaryDemand)
            , "horizon exceedance probability: "
                ++ renderRational (horizonExceededMass (serialInventoryHorizon primaryParameters) primaryDemand)
            , "normalized bounded demand probabilities: " ++ renderDemand primaryDemand
            , "primary oracle return: " ++ renderRational (serialInventoryOracleReturn primary)
            , "primary oracle cost: " ++ renderRational (serialInventoryOracleCost primary)
            , "primary base-stock return: " ++ renderRational (serialInventoryBaseStockReturn primary)
            , "primary base-stock cost: " ++ renderRational (serialInventoryBaseStockCost primary)
            , "primary initial-state value error: " ++ renderRational (serialInventoryInitialValueError primary)
            , "primary policy regret: " ++ renderRational (serialInventoryPolicyRegret primary)
            , "primary selected targets (period 1 upward): " ++ show (baseStockTargetPairs (serialInventorySelectedTargets primary))
            , "primary target grid binds: " ++ renderBool (serialInventoryTargetGridBinds primary)
            , "primary order cap binds: " ++ renderBool (serialInventoryOrderCapBinds primary)
            , "primary model size: states=" ++ show (serialInventoryStateCount primary) ++ ", state-actions=" ++ show (serialInventoryStateActionCount primary)
            , "primary solver status: " ++ show (serialInventorySolverStatus primary)
            , "widened oracle return: " ++ renderRational (serialInventoryOracleReturn widened)
            , "widened oracle cost: " ++ renderRational (serialInventoryOracleCost widened)
            , "widened base-stock return: " ++ renderRational (serialInventoryBaseStockReturn widened)
            , "widened base-stock cost: " ++ renderRational (serialInventoryBaseStockCost widened)
            , "widened initial-state value error: " ++ renderRational (serialInventoryInitialValueError widened)
            , "widened policy regret: " ++ renderRational (serialInventoryPolicyRegret widened)
            , "widened selected targets (period 1 upward): " ++ show (baseStockTargetPairs (serialInventorySelectedTargets widened))
            , "widened target grid binds: " ++ renderBool (serialInventoryTargetGridBinds widened)
            , "widened order cap binds: " ++ renderBool (serialInventoryOrderCapBinds widened)
            , "widened model size: states=" ++ show (serialInventoryStateCount widened) ++ ", state-actions=" ++ show (serialInventoryStateActionCount widened)
            , "widened solver status: " ++ show (serialInventorySolverStatus widened)
            , "primary versus widened bounds: "
                ++ if serialInventoryBoundsStable stability then "stable" else "unstable"
            ]
        )
  where
    primaryParameters = serialInventorySolutionParameters primary
    widenedParameters = serialInventorySolutionParameters widened
    primaryDemand = truncatedGeometricDemand (serialInventoryDemandCap primaryParameters)

-- | Render the report with stable field order and a trailing newline.
renderInventoryBenchmarkReport :: InventoryBenchmarkReport -> String
renderInventoryBenchmarkReport (InventoryBenchmarkReport reportLines) = unlines reportLines

renderParameters :: SerialInventoryParameters -> String
renderParameters parameters =
    intercalate
        ", "
        [ "horizon=" ++ show (serialInventoryHorizon parameters)
        , "discount=" ++ renderRational (exactDiscountValue (serialInventoryDiscount parameters))
        , "orderCap=" ++ show (serialInventoryOrderCap parameters)
        , "demandCap=" ++ show (serialInventoryDemandCap parameters)
        , "h0=" ++ renderRational (serialInventoryUpstreamHoldingCost parameters)
        , "h1=" ++ renderRational (serialInventoryDownstreamHoldingCost parameters)
        , "p=" ++ renderRational (serialInventoryBacklogCost parameters)
        ]

renderGrid :: BaseStockTargetGrid -> String
renderGrid = show . fmap NonEmpty.toList . NonEmpty.toList . baseStockTargetCandidates

renderDemand :: TruncatedDemand -> String
renderDemand demand =
    intercalate
        ", "
        [ show demandValue ++ ":" ++ renderRational mass
        | (demandValue, mass) <- boundedDemandOutcomes demand
        ]

renderRational :: Rational -> String
renderRational value = show (numerator value) ++ "/" ++ show (denominator value)

renderBool :: Bool -> String
renderBool True = "yes"
renderBool False = "no"
