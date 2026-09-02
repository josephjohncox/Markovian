{- | A bounded synthetic two-echelon serial-inventory fixture.

The model is finite because its horizon, order quantity, and conditioned demand
support are finite. Exact returns describe only that conditional bounded-demand
model; omitted demand mass is reported separately and is not a value-error
bound.
-}
module Markovian.Benchmark.Inventory.Serial.Exact (
    SerialInventoryParameters,
    SerialInventoryParameterInput (..),
    SerialInventoryParameterError (..),
    validateSerialInventoryParameters,
    serialInventoryHorizon,
    serialInventoryDiscount,
    serialInventoryOrderCap,
    serialInventoryDemandCap,
    serialInventoryUpstreamHoldingCost,
    serialInventoryDownstreamHoldingCost,
    serialInventoryBacklogCost,
    SerialInventoryState (..),
    SerialInventoryAction (..),
    TruncatedDemand,
    truncatedGeometricDemand,
    retainedDemandMass,
    omittedDemandMass,
    boundedDemandOutcomes,
    horizonExceededMass,
    SerialInventoryFixture,
    SerialInventoryError (..),
    serialInventoryFixture,
    serialInventoryFixtureParameters,
    serialInventoryInitialState,
    serialInventoryMDP,
    serialReachableStates,
    PeriodBaseStockTargets,
    periodBaseStockTargets,
    baseStockTargetPairs,
    BaseStockTargetGrid,
    baseStockTargetGrid,
    baseStockTargetCandidates,
    baseStockAction,
    SerialInventorySolverStatus (..),
    SerialInventorySolution,
    serialInventorySolutionParameters,
    serialInventorySolutionInitialState,
    serialInventorySolutionTargetGrid,
    serialInventoryOracleReturn,
    serialInventoryOracleCost,
    serialInventoryBaseStockReturn,
    serialInventoryBaseStockCost,
    serialInventoryInitialValueError,
    serialInventoryPolicyRegret,
    serialInventorySelectedTargets,
    serialInventoryStateCount,
    serialInventoryStateActionCount,
    serialInventorySolverStatus,
    serialInventoryTargetGridBinds,
    serialInventoryOrderCapBinds,
    solveSerialInventory,
    SerialInventoryStability (..),
    compareSerialInventoryBounds,
    serialInventoryMaximumStateCount,
    serialInventoryMaximumStateActionCount,
    serialInventoryMaximumTargetScheduleCount,
) where

import Control.Monad (foldM)
import Data.List (foldl', sort, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Action (actionId, actionValue)
import Markovian.Kernel.Exact (exactKernel)
import Markovian.MDP.Exact (
    ExactMDP,
    ExactStateStatus (..),
    ExactTransitionOutcome,
    exactMDP,
    exactSuccessorState,
    exactTransitionOutcome,
    exactTransitionReward,
 )
import Markovian.Objective.Exact (
    ExactDiscount,
    exactDiscountValue,
 )
import Markovian.Probability.Exact (
    ExactDistributionError,
    ExactFiniteDist,
    exactFiniteDist,
 )
import Markovian.Reward.Exact (exactReward, exactRewardValue)
import Numeric.Natural (Natural)

-- | Unvalidated serial-inventory parameters.
data SerialInventoryParameterInput = SerialInventoryParameterInput
    { inputSerialInventoryHorizon :: !Integer
    -- ^ Positive number of transitions in the fixture.
    , inputSerialInventoryDiscount :: !ExactDiscount
    -- ^ Exact discount used by the finite-horizon objective; one is valid.
    , inputSerialInventoryOrderCap :: !Integer
    -- ^ Nonnegative supplier-order cap.
    , inputSerialInventoryDemandCap :: !Integer
    -- ^ Nonnegative maximum represented demand.
    , inputSerialInventoryUpstreamHoldingCost :: !Rational
    -- ^ Nonnegative upstream holding cost @h0@.
    , inputSerialInventoryDownstreamHoldingCost :: !Rational
    -- ^ Nonnegative incremental downstream holding cost @h1@.
    , inputSerialInventoryBacklogCost :: !Rational
    -- ^ Nonnegative downstream backlog cost @p@.
    }
    deriving (Eq, Show)

-- | Parameter-validation failures.
data SerialInventoryParameterError
    = SerialInventoryNonPositiveHorizon !Integer
    | SerialInventoryNegativeOrderCap !Integer
    | SerialInventoryNegativeDemandCap !Integer
    | SerialInventoryNegativeUpstreamHoldingCost !Rational
    | SerialInventoryNegativeDownstreamHoldingCost !Rational
    | SerialInventoryNegativeBacklogCost !Rational
    deriving (Eq, Show)

-- | Validated bounded serial-inventory parameters.
data SerialInventoryParameters = SerialInventoryParameters
    { serialInventoryHorizon :: !Natural
    -- ^ The serial inventory horizon.
    , serialInventoryDiscount :: !ExactDiscount
    -- ^ The serial inventory discount.
    , serialInventoryOrderCap :: !Natural
    -- ^ The serial inventory order cap.
    , serialInventoryDemandCap :: !Natural
    -- ^ The serial inventory demand cap.
    , serialInventoryUpstreamHoldingCost :: !Rational
    -- ^ The serial inventory upstream holding cost.
    , serialInventoryDownstreamHoldingCost :: !Rational
    -- ^ The serial inventory downstream holding cost.
    , serialInventoryBacklogCost :: !Rational
    -- ^ The serial inventory backlog cost.
    }
    deriving (Eq, Show)

-- | Validate all bounds and nonnegative cost coefficients.
validateSerialInventoryParameters ::
    SerialInventoryParameterInput ->
    Either SerialInventoryParameterError SerialInventoryParameters
validateSerialInventoryParameters input
    | inputSerialInventoryHorizon input <= 0 =
        Left (SerialInventoryNonPositiveHorizon (inputSerialInventoryHorizon input))
    | inputSerialInventoryOrderCap input < 0 =
        Left (SerialInventoryNegativeOrderCap (inputSerialInventoryOrderCap input))
    | inputSerialInventoryDemandCap input < 0 =
        Left (SerialInventoryNegativeDemandCap (inputSerialInventoryDemandCap input))
    | inputSerialInventoryUpstreamHoldingCost input < 0 =
        Left (SerialInventoryNegativeUpstreamHoldingCost (inputSerialInventoryUpstreamHoldingCost input))
    | inputSerialInventoryDownstreamHoldingCost input < 0 =
        Left (SerialInventoryNegativeDownstreamHoldingCost (inputSerialInventoryDownstreamHoldingCost input))
    | inputSerialInventoryBacklogCost input < 0 =
        Left (SerialInventoryNegativeBacklogCost (inputSerialInventoryBacklogCost input))
    | otherwise =
        Right
            SerialInventoryParameters
                { serialInventoryHorizon = fromInteger (inputSerialInventoryHorizon input)
                , serialInventoryDiscount = inputSerialInventoryDiscount input
                , serialInventoryOrderCap = fromInteger (inputSerialInventoryOrderCap input)
                , serialInventoryDemandCap = fromInteger (inputSerialInventoryDemandCap input)
                , serialInventoryUpstreamHoldingCost = inputSerialInventoryUpstreamHoldingCost input
                , serialInventoryDownstreamHoldingCost = inputSerialInventoryDownstreamHoldingCost input
                , serialInventoryBacklogCost = inputSerialInventoryBacklogCost input
                }

-- | A complete bounded state at the start of one period.
data SerialInventoryState = SerialInventoryState
    { periodsRemaining :: !Natural
    , upstreamOnHand :: !Natural
    , supplierOrderDue :: !Natural
    , downstreamNetInventory :: !Integer
    }
    deriving (Eq, Ord, Show)

-- | A supplier order and an immediate internal downstream shipment.
data SerialInventoryAction = SerialInventoryAction
    { supplierOrder :: !Natural
    , downstreamShipment :: !Natural
    }
    deriving (Eq, Ord, Show)

-- | The geometric demand law conditioned on demand not exceeding its cap.
data TruncatedDemand = TruncatedDemand
    { truncatedDemandCap :: !Natural
    , truncatedDemandRetainedMass :: !Rational
    , truncatedDemandOutcomes :: !(NonEmpty (Natural, Rational))
    }
    deriving (Eq, Show)

-- | Construct @P(D=d)=2^-(d+1)@ conditioned on @0 <= d <= demandCap@.
truncatedGeometricDemand :: Natural -> TruncatedDemand
truncatedGeometricDemand demandCap =
    TruncatedDemand
        { truncatedDemandCap = demandCap
        , truncatedDemandRetainedMass = retained
        , truncatedDemandOutcomes = fmap normalize raw
        }
  where
    raw = fmap (\demand -> (demand, inversePowerOfTwo (demand + 1))) (0 :| [1 .. demandCap])
    retained = sum (fmap snd (NonEmpty.toList raw))
    normalize (demand, mass) = (demand, mass / retained)

-- | Probability retained from the unbounded one-period geometric law.
retainedDemandMass :: TruncatedDemand -> Rational
retainedDemandMass = truncatedDemandRetainedMass

-- | Probability omitted from the unbounded one-period geometric law.
omittedDemandMass :: TruncatedDemand -> Rational
omittedDemandMass demand = 1 - retainedDemandMass demand

-- | Normalized outcomes of the conditional bounded-demand model.
boundedDemandOutcomes :: TruncatedDemand -> [(Natural, Rational)]
boundedDemandOutcomes = NonEmpty.toList . truncatedDemandOutcomes

{- | Probability that at least one of the independent unbounded geometric
observations exceeds the cap during the supplied number of periods.

This truncation probability is not a value-error bound.
-}
horizonExceededMass :: Natural -> TruncatedDemand -> Rational
horizonExceededMass horizon demand = 1 - retainedDemandMass demand ^ horizon

-- | Fixture construction, compilation, policy-grid, and solver failures.
data SerialInventoryError
    = SerialInventoryInitialHorizonMismatch !Natural !Natural
    | SerialInventoryStateBudgetExceeded !Natural
    | SerialInventoryStateActionBudgetExceeded !Natural
    | SerialInventoryPolicyGridBudgetExceeded !Natural
    | SerialInventoryTargetPeriodMismatch !Natural !Natural
    | SerialInventoryActionRequestedAtTerminal
    | SerialInventoryMissingReachableSuccessor !SerialInventoryState
    | SerialInventoryDemandDistributionError !ExactDistributionError
    | SerialInventoryEmptyTargetGrid
    | SerialInventoryDuplicateTargetCandidate !Natural !(Natural, Natural)
    | SerialInventoryNegativeRegret !Rational
    | SerialInventoryInitialStateComparisonMismatch !SerialInventoryState !SerialInventoryState
    | SerialInventoryHorizonComparisonMismatch !Natural !Natural
    | SerialInventoryDiscountComparisonMismatch !ExactDiscount !ExactDiscount
    | SerialInventoryDemandCapComparisonMismatch !Natural !Natural
    | SerialInventoryCostComparisonMismatch !(Rational, Rational, Rational) !(Rational, Rational, Rational)
    | SerialInventoryOrderCapNotIncreased !Natural !Natural
    | SerialInventoryTargetGridNotWidened
    | SerialInventoryTargetGridNotSuperset !Natural !(Natural, Natural)
    | SerialInventorySolverNotStable !SerialInventorySolverStatus
    deriving (Eq, Show)

-- | A validated initial state and its complete reachable support.
data SerialInventoryFixture = SerialInventoryFixture
    { serialInventoryFixtureParameters :: !SerialInventoryParameters
    -- ^ The serial inventory fixture parameters.
    , serialInventoryInitialState :: !SerialInventoryState
    -- ^ The serial inventory initial state.
    , serialInventoryFixtureDemand :: !TruncatedDemand
    , serialInventoryFixtureDemandDistribution :: !(ExactFiniteDist Natural)
    , serialInventoryFixtureReachable :: !(NonEmpty SerialInventoryState)
    }

instance Show SerialInventoryFixture where
    show fixture =
        "SerialInventoryFixture {parameters = "
            ++ show (serialInventoryFixtureParameters fixture)
            ++ ", initialState = "
            ++ show (serialInventoryInitialState fixture)
            ++ ", reachableStateCount = "
            ++ show (length (serialInventoryFixtureReachable fixture))
            ++ "}"

-- | Construct and close the complete bounded reachable support.
serialInventoryFixture ::
    SerialInventoryParameters ->
    SerialInventoryState ->
    Either SerialInventoryError SerialInventoryFixture
serialInventoryFixture parameters initial
    | periodsRemaining initial /= serialInventoryHorizon parameters =
        Left
            ( SerialInventoryInitialHorizonMismatch
                (serialInventoryHorizon parameters)
                (periodsRemaining initial)
            )
    | otherwise = do
        demandDistribution <-
            mapLeft
                SerialInventoryDemandDistributionError
                (exactFiniteDist (boundedDemandOutcomes demand))
        reachable <- reachableSupport parameters demand initial
        let actionCount = stateActionCount parameters reachable
        if actionCount > serialInventoryMaximumStateActionCount
            then Left (SerialInventoryStateActionBudgetExceeded actionCount)
            else
                Right
                    SerialInventoryFixture
                        { serialInventoryFixtureParameters = parameters
                        , serialInventoryInitialState = initial
                        , serialInventoryFixtureDemand = demand
                        , serialInventoryFixtureDemandDistribution = demandDistribution
                        , serialInventoryFixtureReachable = reachable
                        }
  where
    demand = truncatedGeometricDemand (serialInventoryDemandCap parameters)

-- | Read the complete reachable support in deterministic breadth-first order.
serialReachableStates :: SerialInventoryFixture -> NonEmpty SerialInventoryState
serialReachableStates = serialInventoryFixtureReachable

-- | Interpret the fixture as an exact finite MDP.
serialInventoryMDP :: SerialInventoryFixture -> ExactMDP SerialInventoryState SerialInventoryAction
serialInventoryMDP fixture =
    exactMDP
        (serialInventoryInitialState fixture)
        status
        available
        (exactKernel transition)
  where
    parameters = serialInventoryFixtureParameters fixture
    demandDistribution = serialInventoryFixtureDemandDistribution fixture
    status state
        | periodsRemaining state == 0 = ExactTerminal (exactReward 0)
        | otherwise = ExactContinuing
    available state = fmap actionId (physicalActions parameters state)
    transition (state, selected) =
        fmap
            (transitionOutcome parameters state (actionValue selected))
            demandDistribution

-- | One pair of upstream and downstream targets for each remaining-period index.
newtype PeriodBaseStockTargets = PeriodBaseStockTargets (NonEmpty (Natural, Natural))
    deriving (Eq, Show)

-- | Construct targets indexed by one period remaining, two periods remaining, and so on.
periodBaseStockTargets :: NonEmpty (Natural, Natural) -> PeriodBaseStockTargets
periodBaseStockTargets = PeriodBaseStockTargets

-- | Read targets in increasing periods-remaining order.
baseStockTargetPairs :: PeriodBaseStockTargets -> NonEmpty (Natural, Natural)
baseStockTargetPairs (PeriodBaseStockTargets targets) = targets

{- | A duplicate-free candidate set for each periods-remaining index.

Each period's candidates are stored in ascending order. This makes exact ties
independent of caller list order and keeps schedule counts set-based.
-}
newtype BaseStockTargetGrid = BaseStockTargetGrid (NonEmpty (NonEmpty (Natural, Natural)))
    deriving (Eq, Show)

-- | Validate and canonicalize a period-specific nonempty target grid.
baseStockTargetGrid ::
    NonEmpty (NonEmpty (Natural, Natural)) ->
    Either SerialInventoryError BaseStockTargetGrid
baseStockTargetGrid candidates =
    BaseStockTargetGrid <$> traverse validatePeriod (zipWithPeriod candidates)
  where
    validatePeriod (period, periodCandidates) =
        let ordered = sort (NonEmpty.toList periodCandidates)
         in case firstDuplicate ordered of
                Nothing ->
                    case NonEmpty.nonEmpty ordered of
                        Nothing -> Left SerialInventoryEmptyTargetGrid
                        Just uniqueCandidates -> Right uniqueCandidates
                Just duplicate -> Left (SerialInventoryDuplicateTargetCandidate period duplicate)

-- | Read canonical candidate sets in increasing periods-remaining order.
baseStockTargetCandidates :: BaseStockTargetGrid -> NonEmpty (NonEmpty (Natural, Natural))
baseStockTargetCandidates (BaseStockTargetGrid candidates) = candidates

-- | Compute the bounded period-specific base-stock action.
baseStockAction ::
    SerialInventoryParameters ->
    PeriodBaseStockTargets ->
    SerialInventoryState ->
    Either SerialInventoryError SerialInventoryAction
baseStockAction parameters targets state
    | periodsRemaining state == 0 = Left SerialInventoryActionRequestedAtTerminal
    | otherwise = do
        (upstreamTarget, downstreamTarget) <- targetAt (periodsRemaining state) targets
        let availableUpstream = upstreamOnHand state + supplierOrderDue state
            desiredShipment =
                naturalDifference
                    (toInteger downstreamTarget)
                    (downstreamNetInventory state)
            shipment = min availableUpstream desiredShipment
            inventoryPosition =
                toInteger (upstreamOnHand state)
                    + toInteger (supplierOrderDue state)
                    + downstreamNetInventory state
            desiredOrder = naturalDifference (toInteger upstreamTarget) inventoryPosition
        Right
            SerialInventoryAction
                { supplierOrder = min (serialInventoryOrderCap parameters) desiredOrder
                , downstreamShipment = shipment
                }

-- | Status of the finite-horizon oracle calculation.
data SerialInventorySolverStatus
    = SerialInventoryBackwardInductionComplete
    | SerialInventoryBackwardInductionIncomplete
    deriving (Eq, Show)

{- | Complete exact oracle and bounded base-stock comparison.

The constructor is private. Initial-state and target-grid provenance are kept
with the computed observations, and redundant costs and regrets are derived by
accessors.
-}
data SerialInventorySolution = SerialInventorySolution
    { solutionParameters :: !SerialInventoryParameters
    , solutionInitialState :: !SerialInventoryState
    , solutionTargetGrid :: !BaseStockTargetGrid
    , solutionOracleReturn :: !Rational
    , solutionBaseStockReturn :: !Rational
    , solutionSelectedTargets :: !PeriodBaseStockTargets
    , solutionStateCount :: !Natural
    , solutionStateActionCount :: !Natural
    , solutionSolverStatus :: !SerialInventorySolverStatus
    , solutionTargetGridBinds :: !Bool
    , solutionOrderCapBinds :: !Bool
    }
    deriving (Eq, Show)

-- | Return the serial inventory solution parameters.
serialInventorySolutionParameters :: SerialInventorySolution -> SerialInventoryParameters
serialInventorySolutionParameters = solutionParameters

-- | Return the serial inventory solution initial state.
serialInventorySolutionInitialState :: SerialInventorySolution -> SerialInventoryState
serialInventorySolutionInitialState = solutionInitialState

-- | Return the serial inventory solution target grid.
serialInventorySolutionTargetGrid :: SerialInventorySolution -> BaseStockTargetGrid
serialInventorySolutionTargetGrid = solutionTargetGrid

-- | Return the serial inventory oracle return.
serialInventoryOracleReturn :: SerialInventorySolution -> Rational
serialInventoryOracleReturn = solutionOracleReturn

-- | Return the serial inventory oracle cost.
serialInventoryOracleCost :: SerialInventorySolution -> Rational
serialInventoryOracleCost = negate . solutionOracleReturn

-- | Return the serial inventory base stock return.
serialInventoryBaseStockReturn :: SerialInventorySolution -> Rational
serialInventoryBaseStockReturn = solutionBaseStockReturn

-- | Return the serial inventory base stock cost.
serialInventoryBaseStockCost :: SerialInventorySolution -> Rational
serialInventoryBaseStockCost = negate . solutionBaseStockReturn

-- | Return the serial inventory policy regret.
serialInventoryPolicyRegret :: SerialInventorySolution -> Rational
serialInventoryPolicyRegret solution = solutionOracleReturn solution - solutionBaseStockReturn solution

-- | Return the serial inventory initial value error.
serialInventoryInitialValueError :: SerialInventorySolution -> Rational
serialInventoryInitialValueError = serialInventoryPolicyRegret

-- | Return the serial inventory selected targets.
serialInventorySelectedTargets :: SerialInventorySolution -> PeriodBaseStockTargets
serialInventorySelectedTargets = solutionSelectedTargets

-- | Return the serial inventory state count.
serialInventoryStateCount :: SerialInventorySolution -> Natural
serialInventoryStateCount = solutionStateCount

-- | Return the serial inventory state action count.
serialInventoryStateActionCount :: SerialInventorySolution -> Natural
serialInventoryStateActionCount = solutionStateActionCount

-- | Return the serial inventory solver status.
serialInventorySolverStatus :: SerialInventorySolution -> SerialInventorySolverStatus
serialInventorySolverStatus = solutionSolverStatus

-- | Return the serial inventory target grid binds.
serialInventoryTargetGridBinds :: SerialInventorySolution -> Bool
serialInventoryTargetGridBinds = solutionTargetGridBinds

-- | Return the serial inventory order cap binds.
serialInventoryOrderCapBinds :: SerialInventorySolution -> Bool
serialInventoryOrderCapBinds = solutionOrderCapBinds

-- | Solve by exact backward induction and select the best target schedule.
solveSerialInventory ::
    BaseStockTargetGrid ->
    SerialInventoryFixture ->
    Either SerialInventoryError SerialInventorySolution
solveSerialInventory grid fixture = do
    let parameters = serialInventoryFixtureParameters fixture
        states = NonEmpty.toList (serialReachableStates fixture)
    validateGridHorizon (serialInventoryHorizon parameters) grid
    let scheduleCount = targetScheduleCount grid
    if scheduleCount > serialInventoryMaximumTargetScheduleCount
        then Left (SerialInventoryPolicyGridBudgetExceeded scheduleCount)
        else Right ()
    schedules <- targetSchedules grid
    oracleReturn <- evaluateOracle fixture
    evaluated <- traverse (evaluateCandidate fixture) schedules
    (selectedTargets, bestReturn) <-
        case evaluated of
            [] -> Left SerialInventoryEmptyTargetGrid
            first : remaining -> Right (foldl' selectBetter first remaining)
    let regret = oracleReturn - bestReturn
        gridBinds =
            not
                ( any
                    (\(targets, value) -> value == bestReturn && not (targetsAtBoundary grid targets))
                    evaluated
                )
    if regret < 0
        then Left (SerialInventoryNegativeRegret regret)
        else
            Right
                SerialInventorySolution
                    { solutionParameters = parameters
                    , solutionInitialState = serialInventoryInitialState fixture
                    , solutionTargetGrid = grid
                    , solutionOracleReturn = oracleReturn
                    , solutionBaseStockReturn = bestReturn
                    , solutionSelectedTargets = selectedTargets
                    , solutionStateCount = fromIntegral (length states)
                    , solutionStateActionCount = stateActionCount parameters (serialReachableStates fixture)
                    , solutionSolverStatus = SerialInventoryBackwardInductionComplete
                    , solutionTargetGridBinds = gridBinds
                    , solutionOrderCapBinds = orderCapBinds parameters selectedTargets states
                    }

-- | Primary-versus-widened exact bound diagnostics.
data SerialInventoryStability = SerialInventoryStability
    { serialInventoryBoundsStable :: !Bool
    , serialInventoryOracleReturnChanged :: !Bool
    , serialInventoryBaseStockReturnChanged :: !Bool
    , serialInventoryWidenedTargetGridBinds :: !Bool
    , serialInventoryWidenedOrderCapBinds :: !Bool
    }
    deriving (Eq, Show)

{- | Validate provenance and compare exact values after widening both bounds.

The widened order cap must be strictly larger. Every widened period candidate
set must contain its primary set, and at least one period set must be larger.
Both solutions must come from completed backward induction.
-}
compareSerialInventoryBounds ::
    SerialInventorySolution ->
    SerialInventorySolution ->
    Either SerialInventoryError SerialInventoryStability
compareSerialInventoryBounds primary widened = do
    validateSameModel primary widened
    validateWidening primary widened
    validateStableSolver primary
    validateStableSolver widened
    Right
        SerialInventoryStability
            { serialInventoryBoundsStable =
                not oracleChanged
                    && not baseStockChanged
                    && not (serialInventoryTargetGridBinds widened)
                    && not (serialInventoryOrderCapBinds widened)
            , serialInventoryOracleReturnChanged = oracleChanged
            , serialInventoryBaseStockReturnChanged = baseStockChanged
            , serialInventoryWidenedTargetGridBinds = serialInventoryTargetGridBinds widened
            , serialInventoryWidenedOrderCapBinds = serialInventoryOrderCapBinds widened
            }
  where
    oracleChanged = serialInventoryOracleReturn primary /= serialInventoryOracleReturn widened
    baseStockChanged = serialInventoryBaseStockReturn primary /= serialInventoryBaseStockReturn widened

validateSameModel ::
    SerialInventorySolution ->
    SerialInventorySolution ->
    Either SerialInventoryError ()
validateSameModel primary widened
    | primaryInitial /= widenedInitial =
        Left (SerialInventoryInitialStateComparisonMismatch primaryInitial widenedInitial)
    | serialInventoryHorizon primaryParameters /= serialInventoryHorizon widenedParameters =
        Left
            ( SerialInventoryHorizonComparisonMismatch
                (serialInventoryHorizon primaryParameters)
                (serialInventoryHorizon widenedParameters)
            )
    | serialInventoryDiscount primaryParameters /= serialInventoryDiscount widenedParameters =
        Left
            ( SerialInventoryDiscountComparisonMismatch
                (serialInventoryDiscount primaryParameters)
                (serialInventoryDiscount widenedParameters)
            )
    | serialInventoryDemandCap primaryParameters /= serialInventoryDemandCap widenedParameters =
        Left
            ( SerialInventoryDemandCapComparisonMismatch
                (serialInventoryDemandCap primaryParameters)
                (serialInventoryDemandCap widenedParameters)
            )
    | primaryCosts /= widenedCosts =
        Left (SerialInventoryCostComparisonMismatch primaryCosts widenedCosts)
    | otherwise = Right ()
  where
    primaryInitial = serialInventorySolutionInitialState primary
    widenedInitial = serialInventorySolutionInitialState widened
    primaryParameters = serialInventorySolutionParameters primary
    widenedParameters = serialInventorySolutionParameters widened
    primaryCosts = inventoryCosts primaryParameters
    widenedCosts = inventoryCosts widenedParameters

validateWidening ::
    SerialInventorySolution ->
    SerialInventorySolution ->
    Either SerialInventoryError ()
validateWidening primary widened
    | widenedCap <= primaryCap = Left (SerialInventoryOrderCapNotIncreased primaryCap widenedCap)
    | otherwise = do
        strictPeriods <- traverse validatePeriod (zip [1 ..] periodPairs)
        if or strictPeriods
            then Right ()
            else Left SerialInventoryTargetGridNotWidened
  where
    primaryCap = serialInventoryOrderCap (serialInventorySolutionParameters primary)
    widenedCap = serialInventoryOrderCap (serialInventorySolutionParameters widened)
    validatePeriod (periodIndex, (primaryCandidates, widenedCandidates)) =
        case filter (`notElem` widenedList) primaryList of
            missing : _ -> Left (SerialInventoryTargetGridNotSuperset periodIndex missing)
            [] -> Right (length widenedList > length primaryList)
      where
        primaryList = NonEmpty.toList primaryCandidates
        widenedList = NonEmpty.toList widenedCandidates
    periodPairs =
        NonEmpty.toList
            ( NonEmpty.zip
                (baseStockTargetCandidates (serialInventorySolutionTargetGrid primary))
                (baseStockTargetCandidates (serialInventorySolutionTargetGrid widened))
            )

validateStableSolver :: SerialInventorySolution -> Either SerialInventoryError ()
validateStableSolver solution =
    case serialInventorySolverStatus solution of
        SerialInventoryBackwardInductionComplete -> Right ()
        status -> Left (SerialInventorySolverNotStable status)

inventoryCosts :: SerialInventoryParameters -> (Rational, Rational, Rational)
inventoryCosts parameters =
    ( serialInventoryUpstreamHoldingCost parameters
    , serialInventoryDownstreamHoldingCost parameters
    , serialInventoryBacklogCost parameters
    )

-- | Maximum number of states accepted by fixture construction.
serialInventoryMaximumStateCount :: Natural
serialInventoryMaximumStateCount = 5000

-- | Maximum number of reachable state-action pairs accepted by a fixture.
serialInventoryMaximumStateActionCount :: Natural
serialInventoryMaximumStateActionCount = 100000

-- | Maximum number of target schedules enumerated by one solve.
serialInventoryMaximumTargetScheduleCount :: Natural
serialInventoryMaximumTargetScheduleCount = 100000

inversePowerOfTwo :: Natural -> Rational
inversePowerOfTwo power = 1 / (2 ^ power)

physicalActions :: SerialInventoryParameters -> SerialInventoryState -> [SerialInventoryAction]
physicalActions parameters state
    | periodsRemaining state == 0 = []
    | otherwise =
        [ SerialInventoryAction order shipment
        | order <- [0 .. serialInventoryOrderCap parameters]
        , shipment <- [0 .. upstreamOnHand state + supplierOrderDue state]
        ]

transitionOutcome ::
    SerialInventoryParameters ->
    SerialInventoryState ->
    SerialInventoryAction ->
    Natural ->
    ExactTransitionOutcome SerialInventoryState
transitionOutcome parameters state action demand =
    exactTransitionOutcome (exactReward (negate cost)) successor
  where
    (successor, cost) = inventoryTransition parameters state action demand

-- The sole executable definition of successor state and one-period cost.
inventoryTransition ::
    SerialInventoryParameters ->
    SerialInventoryState ->
    SerialInventoryAction ->
    Natural ->
    (SerialInventoryState, Rational)
inventoryTransition parameters state action demand =
    (successor, cost)
  where
    nextUpstream = upstreamOnHand state + supplierOrderDue state - downstreamShipment action
    nextDownstream = downstreamNetInventory state + toInteger (downstreamShipment action) - toInteger demand
    successor =
        SerialInventoryState
            { periodsRemaining = periodsRemaining state - 1
            , upstreamOnHand = nextUpstream
            , supplierOrderDue = supplierOrder action
            , downstreamNetInventory = nextDownstream
            }
    cost =
        serialInventoryUpstreamHoldingCost parameters * fromIntegral nextUpstream
            + (serialInventoryUpstreamHoldingCost parameters + serialInventoryDownstreamHoldingCost parameters)
                * fromIntegral (max nextDownstream 0)
            + serialInventoryBacklogCost parameters * fromIntegral (max (negate nextDownstream) 0)

reachableSupport ::
    SerialInventoryParameters ->
    TruncatedDemand ->
    SerialInventoryState ->
    Either SerialInventoryError (NonEmpty SerialInventoryState)
reachableSupport parameters demand initial = go [initial] [initial]
  where
    go [] visited =
        case NonEmpty.nonEmpty visited of
            Nothing -> Right (initial :| [])
            Just reachable -> Right reachable
    go (state : pending) visited =
        let successors =
                [ fst (inventoryTransition parameters state action demandValue)
                | action <- physicalActions parameters state
                , (demandValue, _) <- boundedDemandOutcomes demand
                ]
            unseen = unique [successor | successor <- successors, successor `notElem` visited]
            nextVisited = visited ++ unseen
         in if fromIntegral (length nextVisited) > serialInventoryMaximumStateCount
                then Left (SerialInventoryStateBudgetExceeded (fromIntegral (length nextVisited)))
                else go (pending ++ unseen) nextVisited

stateActionCount :: SerialInventoryParameters -> NonEmpty SerialInventoryState -> Natural
stateActionCount parameters =
    fromIntegral . sum . fmap (length . physicalActions parameters) . NonEmpty.toList

targetAt :: Natural -> PeriodBaseStockTargets -> Either SerialInventoryError (Natural, Natural)
targetAt requested (PeriodBaseStockTargets targets) =
    case valueAtNatural (requested - 1) (NonEmpty.toList targets) of
        Nothing -> Left (SerialInventoryTargetPeriodMismatch requested (fromIntegral (length targets)))
        Just target -> Right target

naturalDifference :: Integer -> Integer -> Natural
naturalDifference target actual = fromInteger (max 0 (target - actual))

validateGridHorizon :: Natural -> BaseStockTargetGrid -> Either SerialInventoryError ()
validateGridHorizon expected (BaseStockTargetGrid candidates)
    | fromIntegral (length candidates) == expected = Right ()
    | otherwise = Left (SerialInventoryTargetPeriodMismatch expected (fromIntegral (length candidates)))

targetScheduleCount :: BaseStockTargetGrid -> Natural
targetScheduleCount (BaseStockTargetGrid candidates) =
    product (fmap (fromIntegral . length) (NonEmpty.toList candidates))

targetSchedules :: BaseStockTargetGrid -> Either SerialInventoryError [PeriodBaseStockTargets]
targetSchedules (BaseStockTargetGrid candidates) =
    case fmap NonEmpty.toList (NonEmpty.toList candidates) of
        [] -> Left SerialInventoryEmptyTargetGrid
        choices ->
            Right
                [ PeriodBaseStockTargets (first :| remaining)
                | first : remaining <- sequence choices
                ]

evaluateOracle ::
    SerialInventoryFixture ->
    Either SerialInventoryError Rational
evaluateOracle fixture = do
    values <- foldM evaluateState [] orderedStates
    case lookup (serialInventoryInitialState fixture) values of
        Nothing -> Left (SerialInventoryMissingReachableSuccessor (serialInventoryInitialState fixture))
        Just value -> Right value
  where
    parameters = serialInventoryFixtureParameters fixture
    demand = serialInventoryFixtureDemand fixture
    orderedStates = sortOn periodsRemaining (NonEmpty.toList (serialReachableStates fixture))
    gamma = exactDiscountValue (serialInventoryDiscount parameters)

    evaluateState values state
        | periodsRemaining state == 0 = Right ((state, 0) : values)
        | otherwise = do
            actionValues <- traverse (evaluateAction values state) (physicalActions parameters state)
            case actionValues of
                [] -> Left (SerialInventoryMissingReachableSuccessor state)
                first : remaining -> Right ((state, foldl' max first remaining) : values)

    evaluateAction values state action =
        fmap
            sum
            ( traverse
                (uncurry (contribution values state action))
                (boundedDemandOutcomes demand)
            )

    contribution values state action demandValue mass =
        let outcome = transitionOutcome parameters state action demandValue
            successor = exactSuccessorState outcome
         in case lookup successor values of
                Nothing -> Left (SerialInventoryMissingReachableSuccessor successor)
                Just successorValue ->
                    Right
                        ( mass
                            * ( exactRewardValue (exactTransitionReward outcome)
                                    + gamma * successorValue
                              )
                        )

evaluateCandidate ::
    SerialInventoryFixture ->
    PeriodBaseStockTargets ->
    Either SerialInventoryError (PeriodBaseStockTargets, Rational)
evaluateCandidate fixture targets = do
    values <- foldM evaluateState [] orderedStates
    initialValue <-
        case lookup (serialInventoryInitialState fixture) values of
            Nothing -> Left (SerialInventoryMissingReachableSuccessor (serialInventoryInitialState fixture))
            Just value -> Right value
    Right (targets, initialValue)
  where
    parameters = serialInventoryFixtureParameters fixture
    demand = serialInventoryFixtureDemand fixture
    orderedStates = sortOn periodsRemaining (NonEmpty.toList (serialReachableStates fixture))
    gamma = exactDiscountValue (serialInventoryDiscount parameters)

    evaluateState values state
        | periodsRemaining state == 0 = Right ((state, 0) : values)
        | otherwise = do
            action <- baseStockAction parameters targets state
            contributions <-
                traverse
                    (uncurry (contribution values state action))
                    (boundedDemandOutcomes demand)
            Right ((state, sum contributions) : values)

    contribution values state action demandValue mass =
        let outcome = transitionOutcome parameters state action demandValue
            successor = exactSuccessorState outcome
         in case lookup successor values of
                Nothing -> Left (SerialInventoryMissingReachableSuccessor successor)
                Just successorValue ->
                    Right
                        ( mass
                            * ( exactRewardValue (exactTransitionReward outcome)
                                    + gamma * successorValue
                              )
                        )

selectBetter ::
    (PeriodBaseStockTargets, Rational) ->
    (PeriodBaseStockTargets, Rational) ->
    (PeriodBaseStockTargets, Rational)
selectBetter selected candidate
    | snd candidate > snd selected = candidate
    | otherwise = selected

targetsAtBoundary :: BaseStockTargetGrid -> PeriodBaseStockTargets -> Bool
targetsAtBoundary (BaseStockTargetGrid grids) (PeriodBaseStockTargets targets) =
    or (zipWith atBoundary (NonEmpty.toList grids) (NonEmpty.toList targets))
  where
    atBoundary candidates (upstreamTarget, downstreamTarget) =
        let pairs = NonEmpty.toList candidates
            upstreamValues = fmap fst pairs
            downstreamValues = fmap snd pairs
         in upstreamTarget == maximum upstreamValues
                || downstreamTarget == maximum downstreamValues

orderCapBinds ::
    SerialInventoryParameters ->
    PeriodBaseStockTargets ->
    [SerialInventoryState] ->
    Bool
orderCapBinds parameters targets = any clipped
  where
    clipped state
        | periodsRemaining state <= 1 = False
        | otherwise =
            case targetAt (periodsRemaining state) targets of
                Left _ -> False
                Right (upstreamTarget, _) ->
                    let position =
                            toInteger (upstreamOnHand state)
                                + toInteger (supplierOrderDue state)
                                + downstreamNetInventory state
                     in naturalDifference (toInteger upstreamTarget) position
                            > serialInventoryOrderCap parameters

zipWithPeriod :: NonEmpty value -> NonEmpty (Natural, value)
zipWithPeriod = NonEmpty.zip (1 :| [2 ..])

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate (left : right : remaining)
    | left == right = Just right
    | otherwise = firstDuplicate (right : remaining)
firstDuplicate _ = Nothing

unique :: (Eq value) => [value] -> [value]
unique = foldl' (\values value -> if value `elem` values then values else values ++ [value]) []

valueAtNatural :: Natural -> [value] -> Maybe value
valueAtNatural _ [] = Nothing
valueAtNatural 0 (value : _) = Just value
valueAtNatural index (_ : values) = valueAtNatural (index - 1) values

mapLeft :: (error -> otherError) -> Either error value -> Either otherError value
mapLeft wrap = either (Left . wrap) Right
