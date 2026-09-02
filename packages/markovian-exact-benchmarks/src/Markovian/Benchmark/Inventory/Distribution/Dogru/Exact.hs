{- | Exact bounded adaptation of Doğru's one-warehouse, two-retailer model.

The source is Mustafa Kemal Doğru, /Optimal Control of One-Warehouse
Multi-Retailer Systems: An Assessment of the Balance Assumption/ (2006),
DOI 10.6100/IR601558.  Chapters 2--4 define the physical system, the balance
relaxation, and the physicalized lower-bound heuristic.  This module implements
only @l0=1@ and @l1=l2=0@ on an explicit finite horizon.

The repository state uses local warehouse stock
@w = I0 - IP1 - IP2@.  Thus source equations (4.5)--(4.7) give the physical
constraint @s1+s2 <= w+o@.  No successor is clamped or redirected.
-}
module Markovian.Benchmark.Inventory.Distribution.Dogru.Exact (
    DogruParameterInput (..),
    DogruParameters,
    DogruError (..),
    validateDogruParameters,
    dogruScenario2Input,
    dogruHorizon,
    dogruOrderCap,
    dogruReturnCap,
    dogruWarehouseHoldingCost,
    dogruRetailerHoldingCost,
    dogruRetailerPenaltyCost,
    dogruStateBudget,
    dogruStateActionBudget,
    dogruSolverWorkBudget,
    DogruDemand (..),
    dogruDemandOutcomes,
    dogruJointDemandOutcomes,
    dogruDemandMean,
    dogruDemandSecondMoment,
    dogruDemandVariance,
    dogruOmittedDemandMass,
    DogruState (..),
    DogruPhysicalAction (..),
    DogruRelaxedAction (..),
    dogruSourceWarehouseCoordinate,
    dogruPhysicalActions,
    dogruRelaxedActions,
    dogruPhysicalTransition,
    dogruRelaxedTransition,
    dogruPhysicalOutcomes,
    dogruRelaxedOutcomes,
    dogruBalancedAction,
    dogruPhysicalizedLBAction,
    DogruFixture,
    dogruFixture,
    dogruFixtureParameters,
    dogruInitialState,
    dogruPhysicalReachableStates,
    dogruRelaxedReachableStates,
    dogruPhysicalStateLayout,
    dogruRelaxedStateLayout,
    dogruPhysicalActionLayout,
    dogruRelaxedActionLayout,
    dogruPhysicalStateActionCount,
    dogruRelaxedStateActionCount,
    sameDogruReachableSupport,
    sameDogruReachableLayout,
    DogruSolution,
    solveDogru,
    dogruSolutionFixture,
    dogruRelaxedOracleCost,
    dogruPhysicalOracleCost,
    dogruBalancedPolicyCost,
    dogruPhysicalizedLBHeuristicCost,
    dogruRelaxationError,
    dogruPolicyRegret,
    dogruTotalGap,
    dogruRelativeRelaxationError,
    dogruRelativeTotalGap,
    dogruRelativePolicyRegret,
    dogruOrderCapSelected,
    dogruReturnCapSelected,
    dogruCheckedSolverWork,
    DogruStability (..),
    compareDogruBounds,
) where

import Control.Monad (foldM)
import Data.Bifunctor qualified as Bifunctor
import Data.List (foldl', sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio ((%))
import Markovian.Category.Finite.Set (FiniteSet, FiniteSetError, finiteSet, sameFiniteLayout, sameFiniteSet)
import Numeric.Natural (Natural)

-- | Unvalidated source coefficients and repository computation bounds.
data DogruParameterInput = DogruParameterInput
    { inputDogruHorizon :: !Integer
    , inputDogruOrderCap :: !Integer
    , inputDogruReturnCap :: !Integer
    , inputDogruWarehouseHoldingCost :: !Rational
    , inputDogruRetailerHoldingCost :: !Rational
    , inputDogruRetailerPenaltyCost :: !Rational
    , inputDogruStateBudget :: !Integer
    , inputDogruStateActionBudget :: !Integer
    , inputDogruSolverWorkBudget :: !Integer
    }
    deriving (Eq, Show)

-- | Validated dogru parameters.
data DogruParameters = DogruParameters
    { dogruHorizon :: !Natural
    -- ^ The dogru horizon.
    , dogruOrderCap :: !Natural
    -- ^ The dogru order cap.
    , dogruReturnCap :: !Natural
    -- ^ The dogru return cap.
    , dogruWarehouseHoldingCost :: !Rational
    -- ^ The dogru warehouse holding cost.
    , dogruRetailerHoldingCost :: !Rational
    -- ^ The dogru retailer holding cost.
    , dogruRetailerPenaltyCost :: !Rational
    -- ^ The dogru retailer penalty cost.
    , dogruStateBudget :: !Natural
    -- ^ The dogru state budget.
    , dogruStateActionBudget :: !Natural
    -- ^ The dogru state action budget.
    , dogruSolverWorkBudget :: !Natural
    -- ^ The dogru solver work budget.
    }
    deriving (Eq, Show)

-- | Failures from the dogru operations.
data DogruError
    = DogruNonPositiveHorizon !Integer
    | DogruNegativeOrderCap !Integer
    | DogruNegativeReturnCap !Integer
    | DogruNegativeCost !String !Rational
    | DogruNonPositiveBudget !String !Integer
    | DogruInitialHorizonMismatch !Natural !Natural
    | DogruNegativeWarehouseStock !DogruState
    | DogruUnavailablePhysicalAction !DogruState !DogruPhysicalAction
    | DogruUnavailableRelaxedAction !DogruState !DogruRelaxedAction
    | DogruStateBudgetExceeded !Natural
    | DogruStateActionBudgetExceeded !Natural
    | DogruSolverWorkBudgetExceeded !Natural
    | DogruPhysicalStateLayoutError !(FiniteSetError DogruState)
    | DogruRelaxedStateLayoutError !(FiniteSetError DogruState)
    | DogruPhysicalActionLayoutError !(FiniteSetError DogruPhysicalAction)
    | DogruRelaxedActionLayoutError !(FiniteSetError DogruRelaxedAction)
    | DogruMissingSuccessor !DogruState
    | DogruNoAction !DogruState
    | DogruNegativeGap !String !Rational
    | DogruNonPositiveRelaxedOracleCost !Rational
    | DogruBoundsNotWidened
    | DogruModelMismatch !String
    deriving (Eq, Show)

-- | Validate the dogru parameters.
validateDogruParameters :: DogruParameterInput -> Either DogruError DogruParameters
validateDogruParameters input
    | inputDogruHorizon input <= 0 = Left (DogruNonPositiveHorizon (inputDogruHorizon input))
    | inputDogruOrderCap input < 0 = Left (DogruNegativeOrderCap (inputDogruOrderCap input))
    | inputDogruReturnCap input < 0 = Left (DogruNegativeReturnCap (inputDogruReturnCap input))
    | otherwise = do
        checkCost "warehouse holding" (inputDogruWarehouseHoldingCost input)
        checkCost "retailer holding" (inputDogruRetailerHoldingCost input)
        checkCost "retailer penalty" (inputDogruRetailerPenaltyCost input)
        stateBudget <- checkBudget "state" (inputDogruStateBudget input)
        stateActionBudget <- checkBudget "state-action" (inputDogruStateActionBudget input)
        solverBudget <- checkBudget "solver-work" (inputDogruSolverWorkBudget input)
        Right
            DogruParameters
                { dogruHorizon = fromInteger (inputDogruHorizon input)
                , dogruOrderCap = fromInteger (inputDogruOrderCap input)
                , dogruReturnCap = fromInteger (inputDogruReturnCap input)
                , dogruWarehouseHoldingCost = inputDogruWarehouseHoldingCost input
                , dogruRetailerHoldingCost = inputDogruRetailerHoldingCost input
                , dogruRetailerPenaltyCost = inputDogruRetailerPenaltyCost input
                , dogruStateBudget = stateBudget
                , dogruStateActionBudget = stateActionBudget
                , dogruSolverWorkBudget = solverBudget
                }
  where
    checkCost label value
        | value < 0 = Left (DogruNegativeCost label value)
        | otherwise = Right ()
    checkBudget label value
        | value <= 0 = Left (DogruNonPositiveBudget label value)
        | otherwise = Right (fromInteger value)

{- | Chapter 4 scenario 2 coefficients and repository bounds.

The thesis labels this row @cv_i=2@.  That label is retained as provenance; it
is not the exact coefficient of variation of the displayed finite law.
-}
dogruScenario2Input :: Integer -> Integer -> Integer -> DogruParameterInput
dogruScenario2Input horizon orderCap returnCap =
    DogruParameterInput
        { inputDogruHorizon = horizon
        , inputDogruOrderCap = orderCap
        , inputDogruReturnCap = returnCap
        , inputDogruWarehouseHoldingCost = 9 % 10
        , inputDogruRetailerHoldingCost = 1 % 10
        , inputDogruRetailerPenaltyCost = 4
        , inputDogruStateBudget = 100000
        , inputDogruStateActionBudget = 5000000
        , inputDogruSolverWorkBudget = 200000000
        }

-- | One independent retailer-demand pair.
data DogruDemand = DogruDemand
    { dogruRetailer1Demand :: !Natural
    , dogruRetailer2Demand :: !Natural
    }
    deriving (Eq, Ord, Show)

-- | Table 4.1 scenario 2 finite marginal law on 0, 1, 2, and 3.
dogruDemandOutcomes :: NonEmpty (Natural, Rational)
dogruDemandOutcomes = (0, 78 % 100) :| [(1, 7 % 100), (2, 7 % 100), (3, 8 % 100)]

-- | Exact product law; retailer demands are independent in the source scenario.
dogruJointDemandOutcomes :: NonEmpty (DogruDemand, Rational)
dogruJointDemandOutcomes =
    case [ (DogruDemand left right, leftMass * rightMass)
         | (left, leftMass) <- NonEmpty.toList dogruDemandOutcomes
         , (right, rightMass) <- NonEmpty.toList dogruDemandOutcomes
         ] of
        first : remaining -> first :| remaining
        [] -> error "dogruDemandOutcomes is statically nonempty"

-- | Return the dogru demand mean.
dogruDemandMean :: Rational
dogruDemandMean = sum [fromIntegral demand * mass | (demand, mass) <- NonEmpty.toList dogruDemandOutcomes]

-- | Return the dogru demand second moment.
dogruDemandSecondMoment :: Rational
dogruDemandSecondMoment = sum [fromIntegral (demand * demand) * mass | (demand, mass) <- NonEmpty.toList dogruDemandOutcomes]

-- | Return the dogru demand variance.
dogruDemandVariance :: Rational
dogruDemandVariance = dogruDemandSecondMoment - dogruDemandMean * dogruDemandMean

-- | The displayed source law is already finite and normalized.
dogruOmittedDemandMass :: Rational
dogruOmittedDemandMass = 0

-- | Beginning-of-period local-coordinate state for @l0=1, l1=l2=0@.
data DogruState = DogruState
    { dogruPeriodsRemaining :: !Natural
    , dogruWarehouseStock :: !Natural
    , dogruSupplierOrderDue :: !Natural
    , dogruRetailer1Inventory :: !Integer
    , dogruRetailer2Inventory :: !Integer
    }
    deriving (Eq, Ord, Show)

-- | Physical order and nonnegative retailer shipments.
data DogruPhysicalAction = DogruPhysicalAction
    { dogruPhysicalOrder :: !Natural
    , dogruPhysicalShipment1 :: !Natural
    , dogruPhysicalShipment2 :: !Natural
    }
    deriving (Eq, Ord, Show)

-- | Balance-relaxed order and signed retailer shipments.
data DogruRelaxedAction = DogruRelaxedAction
    { dogruRelaxedOrder :: !Natural
    , dogruRelaxedShipment1 :: !Integer
    , dogruRelaxedShipment2 :: !Integer
    }
    deriving (Eq, Ord, Show)

-- | Recover the thesis echelon coordinate @I0 = w + IP1 + IP2@.
dogruSourceWarehouseCoordinate :: DogruState -> Integer
dogruSourceWarehouseCoordinate state =
    toInteger (dogruWarehouseStock state)
        + dogruRetailer1Inventory state
        + dogruRetailer2Inventory state

orderValues :: DogruParameters -> DogruState -> [Natural]
orderValues parameters state
    | dogruPeriodsRemaining state == 1 = [0]
    | otherwise = [0 .. dogruOrderCap parameters]

physicalActionCountAt :: DogruParameters -> DogruState -> Natural
physicalActionCountAt parameters state
    | dogruPeriodsRemaining state == 0 = 0
    | otherwise = fromIntegral (length (orderValues parameters state)) * triangular (available + 1)
  where
    available = dogruWarehouseStock state + dogruSupplierOrderDue state

relaxedActionCountAt :: DogruParameters -> DogruState -> Natural
relaxedActionCountAt parameters state
    | dogruPeriodsRemaining state == 0 = 0
    | otherwise = fromIntegral (length (orderValues parameters state)) * triangular (available + 2 * dogruReturnCap parameters + 1)
  where
    available = dogruWarehouseStock state + dogruSupplierOrderDue state

triangular :: Natural -> Natural
triangular width = width * (width + 1) `div` 2

-- | Enumerate physical actions after a count preflight.
dogruPhysicalActions :: DogruParameters -> DogruState -> Either DogruError [DogruPhysicalAction]
dogruPhysicalActions parameters state
    | count > dogruStateActionBudget parameters = Left (DogruStateActionBudgetExceeded count)
    | dogruPeriodsRemaining state == 0 = Right []
    | otherwise = Right (physicalActionsUnchecked parameters state)
  where
    count = physicalActionCountAt parameters state

physicalActionsUnchecked :: DogruParameters -> DogruState -> [DogruPhysicalAction]
physicalActionsUnchecked parameters state
    | dogruPeriodsRemaining state == 0 = []
    | otherwise =
        [ DogruPhysicalAction order first second
        | order <- orderValues parameters state
        , first <- [0 .. available]
        , second <- [0 .. available - first]
        ]
  where
    available = dogruWarehouseStock state + dogruSupplierOrderDue state

-- | Enumerate signed relaxed actions after a count preflight.
dogruRelaxedActions :: DogruParameters -> DogruState -> Either DogruError [DogruRelaxedAction]
dogruRelaxedActions parameters state
    | count > dogruStateActionBudget parameters = Left (DogruStateActionBudgetExceeded count)
    | dogruPeriodsRemaining state == 0 = Right []
    | otherwise = Right (relaxedActionsUnchecked parameters state)
  where
    count = relaxedActionCountAt parameters state

relaxedActionsUnchecked :: DogruParameters -> DogruState -> [DogruRelaxedAction]
relaxedActionsUnchecked parameters state
    | dogruPeriodsRemaining state == 0 = []
    | otherwise =
        [ DogruRelaxedAction order first second
        | order <- orderValues parameters state
        , first <- [lower .. upper]
        , second <- [lower .. upper]
        , first + second <= available
        ]
  where
    available = toInteger (dogruWarehouseStock state + dogruSupplierOrderDue state)
    lower = negate (toInteger (dogruReturnCap parameters))
    upper = available + toInteger (dogruReturnCap parameters)

-- | Checked physical successor and same-demand realized cost.
dogruPhysicalTransition :: DogruParameters -> DogruState -> DogruPhysicalAction -> DogruDemand -> Either DogruError (DogruState, Rational)
dogruPhysicalTransition parameters state action demand = do
    actions <- dogruPhysicalActions parameters state
    if action `elem` actions
        then Right (physicalTransitionUnchecked parameters state action demand)
        else Left (DogruUnavailablePhysicalAction state action)

-- | Checked relaxed successor and same-demand realized cost.
dogruRelaxedTransition :: DogruParameters -> DogruState -> DogruRelaxedAction -> DogruDemand -> Either DogruError (DogruState, Rational)
dogruRelaxedTransition parameters state action demand = do
    actions <- dogruRelaxedActions parameters state
    if action `elem` actions
        then Right (relaxedTransitionUnchecked parameters state action demand)
        else Left (DogruUnavailableRelaxedAction state action)

physicalTransitionUnchecked :: DogruParameters -> DogruState -> DogruPhysicalAction -> DogruDemand -> (DogruState, Rational)
physicalTransitionUnchecked parameters state action =
    transitionValue
        parameters
        state
        (dogruPhysicalOrder action)
        (toInteger (dogruPhysicalShipment1 action))
        (toInteger (dogruPhysicalShipment2 action))

relaxedTransitionUnchecked :: DogruParameters -> DogruState -> DogruRelaxedAction -> DogruDemand -> (DogruState, Rational)
relaxedTransitionUnchecked parameters state action =
    transitionValue parameters state (dogruRelaxedOrder action) (dogruRelaxedShipment1 action) (dogruRelaxedShipment2 action)

transitionValue :: DogruParameters -> DogruState -> Natural -> Integer -> Integer -> DogruDemand -> (DogruState, Rational)
transitionValue parameters state order shipment1 shipment2 demand = (successor, cost)
  where
    warehouseAfter = toInteger (dogruWarehouseStock state + dogruSupplierOrderDue state) - shipment1 - shipment2
    retailer1After = dogruRetailer1Inventory state + shipment1 - toInteger (dogruRetailer1Demand demand)
    retailer2After = dogruRetailer2Inventory state + shipment2 - toInteger (dogruRetailer2Demand demand)
    successor =
        DogruState
            { dogruPeriodsRemaining = dogruPeriodsRemaining state - 1
            , dogruWarehouseStock = fromInteger warehouseAfter
            , dogruSupplierOrderDue = order
            , dogruRetailer1Inventory = retailer1After
            , dogruRetailer2Inventory = retailer2After
            }
    retailerCost inventory =
        (dogruWarehouseHoldingCost parameters + dogruRetailerHoldingCost parameters) * fromInteger (max inventory 0)
            + dogruRetailerPenaltyCost parameters * fromInteger (max (negate inventory) 0)
    cost =
        dogruWarehouseHoldingCost parameters * fromInteger warehouseAfter
            + retailerCost retailer1After
            + retailerCost retailer2After

-- | Demand, mass, successor, and cost remain one correlated outcome.
dogruPhysicalOutcomes :: DogruParameters -> DogruState -> DogruPhysicalAction -> Either DogruError (NonEmpty (DogruDemand, Rational, DogruState, Rational))
dogruPhysicalOutcomes parameters state action =
    traverse makeOutcome dogruJointDemandOutcomes
  where
    makeOutcome (demand, mass) = do
        (successor, cost) <- dogruPhysicalTransition parameters state action demand
        Right (demand, mass, successor, cost)

-- | Demand, mass, successor, and cost remain one correlated outcome.
dogruRelaxedOutcomes :: DogruParameters -> DogruState -> DogruRelaxedAction -> Either DogruError (NonEmpty (DogruDemand, Rational, DogruState, Rational))
dogruRelaxedOutcomes parameters state action =
    traverse makeOutcome dogruJointDemandOutcomes
  where
    makeOutcome (demand, mass) = do
        (successor, cost) <- dogruRelaxedTransition parameters state action demand
        Right (demand, mass, successor, cost)

baseStockOrder :: DogruParameters -> DogruState -> Natural
baseStockOrder parameters state
    | dogruPeriodsRemaining state == 1 = 0
    | otherwise = min (dogruOrderCap parameters) (fromInteger (max 0 (3 - position)))
  where
    position = dogruSourceWarehouseCoordinate state + toInteger (dogruSupplierOrderDue state)

-- | Source base stock @y0*=3@ with the exact relaxed myopic allocation.
dogruBalancedAction :: DogruParameters -> DogruState -> Either DogruError DogruRelaxedAction
dogruBalancedAction parameters state = do
    actions <- dogruRelaxedActions parameters state
    selectMinimumImmediateRelaxed parameters state (filter ((== baseStockOrder parameters state) . dogruRelaxedOrder) actions)

{- | Physicalized lower-bound action.

Chapter 3, section 3.3.4 sets infeasible negative shipments to zero and
re-solves the myopic allocation over the remaining retailers.  For the
implemented identical two-retailer scenario this is the exact physical myopic
minimum with the same source base-stock order.  It is evaluated separately
from the physical oracle.
-}
dogruPhysicalizedLBAction :: DogruParameters -> DogruState -> Either DogruError DogruPhysicalAction
dogruPhysicalizedLBAction parameters state = do
    actions <- dogruPhysicalActions parameters state
    selectMinimumImmediatePhysical parameters state (filter ((== baseStockOrder parameters state) . dogruPhysicalOrder) actions)

selectMinimumImmediatePhysical :: DogruParameters -> DogruState -> [DogruPhysicalAction] -> Either DogruError DogruPhysicalAction
selectMinimumImmediatePhysical _ state [] = Left (DogruNoAction state)
selectMinimumImmediatePhysical parameters state (first : remaining) =
    Right (foldl' better first remaining)
  where
    better selected candidate
        | expectedImmediatePhysical parameters state candidate < expectedImmediatePhysical parameters state selected = candidate
        | otherwise = selected

selectMinimumImmediateRelaxed :: DogruParameters -> DogruState -> [DogruRelaxedAction] -> Either DogruError DogruRelaxedAction
selectMinimumImmediateRelaxed _ state [] = Left (DogruNoAction state)
selectMinimumImmediateRelaxed parameters state (first : remaining) =
    Right (foldl' better first remaining)
  where
    better selected candidate
        | expectedImmediateRelaxed parameters state candidate < expectedImmediateRelaxed parameters state selected = candidate
        | otherwise = selected

expectedImmediatePhysical :: DogruParameters -> DogruState -> DogruPhysicalAction -> Rational
expectedImmediatePhysical parameters state action =
    sum
        [ mass * snd (physicalTransitionUnchecked parameters state action demand)
        | (demand, mass) <- NonEmpty.toList dogruJointDemandOutcomes
        ]

expectedImmediateRelaxed :: DogruParameters -> DogruState -> DogruRelaxedAction -> Rational
expectedImmediateRelaxed parameters state action =
    sum
        [ mass * snd (relaxedTransitionUnchecked parameters state action demand)
        | (demand, mass) <- NonEmpty.toList dogruJointDemandOutcomes
        ]

-- | Both action systems and their separate complete finite layouts.
data DogruFixture = DogruFixture
    { dogruFixtureParameters :: !DogruParameters
    -- ^ The dogru fixture parameters.
    , dogruInitialState :: !DogruState
    -- ^ The dogru initial state.
    , dogruPhysicalReachableStates :: !(NonEmpty DogruState)
    -- ^ The dogru physical reachable states.
    , dogruRelaxedReachableStates :: !(NonEmpty DogruState)
    -- ^ The dogru relaxed reachable states.
    , dogruPhysicalStateLayout :: !(FiniteSet DogruState)
    -- ^ The dogru physical state layout.
    , dogruRelaxedStateLayout :: !(FiniteSet DogruState)
    -- ^ The dogru relaxed state layout.
    , dogruPhysicalActionLayout :: !(FiniteSet DogruPhysicalAction)
    -- ^ The dogru physical action layout.
    , dogruRelaxedActionLayout :: !(FiniteSet DogruRelaxedAction)
    -- ^ The dogru relaxed action layout.
    , dogruPhysicalStateActionCount :: !Natural
    -- ^ The dogru physical state action count.
    , dogruRelaxedStateActionCount :: !Natural
    -- ^ The dogru relaxed state action count.
    }

instance Show DogruFixture where
    show fixture =
        "DogruFixture {physicalStates="
            ++ show (length (dogruPhysicalReachableStates fixture))
            ++ ", relaxedStates="
            ++ show (length (dogruRelaxedReachableStates fixture))
            ++ ", physicalStateActions="
            ++ show (dogruPhysicalStateActionCount fixture)
            ++ ", relaxedStateActions="
            ++ show (dogruRelaxedStateActionCount fixture)
            ++ "}"

-- | Return the dogru fixture.
dogruFixture :: DogruParameters -> DogruState -> Either DogruError DogruFixture
dogruFixture parameters initial
    | dogruPeriodsRemaining initial /= dogruHorizon parameters = Left (DogruInitialHorizonMismatch (dogruHorizon parameters) (dogruPeriodsRemaining initial))
    | otherwise = do
        (physicalStates, physicalCount) <- physicalReachable parameters initial
        (relaxedStates, relaxedCount) <- relaxedReachable parameters initial
        physicalStateSet <- mapLeft DogruPhysicalStateLayoutError (finiteSet (NonEmpty.toList physicalStates))
        relaxedStateSet <- mapLeft DogruRelaxedStateLayoutError (finiteSet (NonEmpty.toList relaxedStates))
        physicalActionSet <- mapLeft DogruPhysicalActionLayoutError (finiteSet (unique (concatMap (physicalActionsUnchecked parameters) (NonEmpty.toList physicalStates))))
        relaxedActionSet <- mapLeft DogruRelaxedActionLayoutError (finiteSet (unique (concatMap (relaxedActionsUnchecked parameters) (NonEmpty.toList relaxedStates))))
        Right
            DogruFixture
                { dogruFixtureParameters = parameters
                , dogruInitialState = initial
                , dogruPhysicalReachableStates = physicalStates
                , dogruRelaxedReachableStates = relaxedStates
                , dogruPhysicalStateLayout = physicalStateSet
                , dogruRelaxedStateLayout = relaxedStateSet
                , dogruPhysicalActionLayout = physicalActionSet
                , dogruRelaxedActionLayout = relaxedActionSet
                , dogruPhysicalStateActionCount = physicalCount
                , dogruRelaxedStateActionCount = relaxedCount
                }

physicalReachable :: DogruParameters -> DogruState -> Either DogruError (NonEmpty DogruState, Natural)
physicalReachable parameters initial = go 0 [initial] [initial]
  where
    go count [] visited = finish count visited
    go count (state : pending) visited = do
        let nextCount = count + physicalActionCountAt parameters state
        checkActionBudget parameters nextCount
        actions <- dogruPhysicalActions parameters state
        let successors =
                [ fst (physicalTransitionUnchecked parameters state action demand)
                | action <- actions
                , (demand, _) <- NonEmpty.toList dogruJointDemandOutcomes
                ]
        continue nextCount pending visited successors
    continue count pending visited successors =
        let candidates = unique successors
            unseen = sortedDifference candidates visited
            nextVisited = sortedMerge visited unseen
            nextSize = fromIntegral (length nextVisited)
         in if nextSize > dogruStateBudget parameters
                then Left (DogruStateBudgetExceeded nextSize)
                else go count (pending ++ unseen) nextVisited
    finish count values = maybe (Left (DogruNegativeWarehouseStock initial)) (\states -> Right (states, count)) (NonEmpty.nonEmpty values)

relaxedReachable :: DogruParameters -> DogruState -> Either DogruError (NonEmpty DogruState, Natural)
relaxedReachable parameters initial = go 0 [initial] [initial]
  where
    go count [] visited = finish count visited
    go count (state : pending) visited = do
        let nextCount = count + relaxedActionCountAt parameters state
        checkActionBudget parameters nextCount
        actions <- dogruRelaxedActions parameters state
        let successors =
                [ fst (relaxedTransitionUnchecked parameters state action demand)
                | action <- actions
                , (demand, _) <- NonEmpty.toList dogruJointDemandOutcomes
                ]
        continue nextCount pending visited successors
    continue count pending visited successors =
        let candidates = unique successors
            unseen = sortedDifference candidates visited
            nextVisited = sortedMerge visited unseen
            nextSize = fromIntegral (length nextVisited)
         in if nextSize > dogruStateBudget parameters
                then Left (DogruStateBudgetExceeded nextSize)
                else go count (pending ++ unseen) nextVisited
    finish count values = maybe (Left (DogruNegativeWarehouseStock initial)) (\states -> Right (states, count)) (NonEmpty.nonEmpty values)

checkActionBudget :: DogruParameters -> Natural -> Either DogruError ()
checkActionBudget parameters count
    | count > dogruStateActionBudget parameters = Left (DogruStateActionBudgetExceeded count)
    | otherwise = Right ()

-- | Compare the dogru reachable support.
sameDogruReachableSupport :: DogruFixture -> Bool
sameDogruReachableSupport fixture = sameFiniteSet (dogruPhysicalStateLayout fixture) (dogruRelaxedStateLayout fixture)

-- | Compare the dogru reachable layout.
sameDogruReachableLayout :: DogruFixture -> Bool
sameDogruReachableLayout fixture = sameFiniteLayout (dogruPhysicalStateLayout fixture) (dogruRelaxedStateLayout fixture)

-- | Completed exact physical, bounded-relaxed, and policy comparison.
data DogruSolution = DogruSolution
    { dogruSolutionFixture :: !DogruFixture
    -- ^ The dogru solution fixture.
    , dogruRelaxedOracleCost :: !Rational
    -- ^ The dogru relaxed oracle cost.
    , dogruPhysicalOracleCost :: !Rational
    -- ^ The dogru physical oracle cost.
    , dogruBalancedPolicyCost :: !Rational
    -- ^ The dogru balanced policy cost.
    , dogruPhysicalizedLBHeuristicCost :: !Rational
    -- ^ The dogru physicalized lbheuristic cost.
    , dogruOrderCapSelected :: !Bool
    -- ^ The dogru order cap selected.
    , dogruReturnCapSelected :: !Bool
    -- ^ The dogru return cap selected.
    , dogruCheckedSolverWork :: !Natural
    -- ^ The dogru checked solver work.
    }
    deriving (Show)

instance Eq DogruSolution where
    left == right =
        dogruInitialState (dogruSolutionFixture left) == dogruInitialState (dogruSolutionFixture right)
            && dogruFixtureParameters (dogruSolutionFixture left) == dogruFixtureParameters (dogruSolutionFixture right)
            && dogruRelaxedOracleCost left == dogruRelaxedOracleCost right
            && dogruPhysicalOracleCost left == dogruPhysicalOracleCost right
            && dogruBalancedPolicyCost left == dogruBalancedPolicyCost right
            && dogruPhysicalizedLBHeuristicCost left == dogruPhysicalizedLBHeuristicCost right
            && dogruOrderCapSelected left == dogruOrderCapSelected right
            && dogruReturnCapSelected left == dogruReturnCapSelected right
            && dogruCheckedSolverWork left == dogruCheckedSolverWork right

-- | Return the dogru relaxation error.
dogruRelaxationError :: DogruSolution -> Rational
dogruRelaxationError solution = dogruPhysicalOracleCost solution - dogruRelaxedOracleCost solution

-- | Return the dogru policy regret.
dogruPolicyRegret :: DogruSolution -> Rational
dogruPolicyRegret solution = dogruPhysicalizedLBHeuristicCost solution - dogruPhysicalOracleCost solution

-- | Return the dogru total gap.
dogruTotalGap :: DogruSolution -> Rational
dogruTotalGap solution = dogruPhysicalizedLBHeuristicCost solution - dogruRelaxedOracleCost solution

-- | Return the dogru relative relaxation error.
dogruRelativeRelaxationError :: DogruSolution -> Rational
dogruRelativeRelaxationError solution = 100 * dogruRelaxationError solution / dogruRelaxedOracleCost solution

-- | Return the dogru relative total gap.
dogruRelativeTotalGap :: DogruSolution -> Rational
dogruRelativeTotalGap solution = 100 * dogruTotalGap solution / dogruRelaxedOracleCost solution

-- | Return the dogru relative policy regret.
dogruRelativePolicyRegret :: DogruSolution -> Rational
dogruRelativePolicyRegret solution = 100 * dogruPolicyRegret solution / dogruPhysicalOracleCost solution

-- | Solve the dogru.
solveDogru :: DogruFixture -> Either DogruError DogruSolution
solveDogru fixture = do
    let parameters = dogruFixtureParameters fixture
        work =
            2
                * fromIntegral (length dogruJointDemandOutcomes)
                * (dogruPhysicalStateActionCount fixture + dogruRelaxedStateActionCount fixture)
    if work > dogruSolverWorkBudget parameters
        then Left (DogruSolverWorkBudgetExceeded work)
        else Right ()
    (physicalCost, physicalSelections) <- evaluatePhysicalOracle fixture
    (relaxedCost, relaxedSelections) <- evaluateRelaxedOracle fixture
    balancedCost <- evaluateRelaxedPolicy fixture dogruBalancedAction
    heuristicCost <- evaluatePhysicalPolicy fixture dogruPhysicalizedLBAction
    if relaxedCost <= 0
        then Left (DogruNonPositiveRelaxedOracleCost relaxedCost)
        else Right ()
    checkGap "relaxation" (physicalCost - relaxedCost)
    checkGap "policy regret" (heuristicCost - physicalCost)
    let physicalPath = physicalSelectedPath parameters (dogruInitialState fixture) physicalSelections
        relaxedPath = relaxedSelectedPath parameters (dogruInitialState fixture) relaxedSelections
    Right
        DogruSolution
            { dogruSolutionFixture = fixture
            , dogruRelaxedOracleCost = relaxedCost
            , dogruPhysicalOracleCost = physicalCost
            , dogruBalancedPolicyCost = balancedCost
            , dogruPhysicalizedLBHeuristicCost = heuristicCost
            , dogruOrderCapSelected =
                any
                    (\(state, order) -> dogruPeriodsRemaining state > 1 && order == dogruOrderCap parameters)
                    ( fmap (Bifunctor.second dogruPhysicalOrder) physicalPath
                        ++ fmap (Bifunctor.second dogruRelaxedOrder) relaxedPath
                    )
            , dogruReturnCapSelected =
                any
                    (\(_, action) -> dogruRelaxedShipment1 action == lower || dogruRelaxedShipment2 action == lower)
                    relaxedPath
            , dogruCheckedSolverWork = work
            }
  where
    lower = negate (toInteger (dogruReturnCap (dogruFixtureParameters fixture)))
    checkGap label value
        | value < 0 = Left (DogruNegativeGap label value)
        | otherwise = Right ()

-- Cost minimization; states are evaluated in increasing remaining horizon.
evaluatePhysicalOracle :: DogruFixture -> Either DogruError (Rational, [(DogruState, DogruPhysicalAction)])
evaluatePhysicalOracle fixture = do
    (values, selections) <- foldM step ([], []) ordered
    initialValue <- lookupValue (dogruInitialState fixture) values
    Right (initialValue, selections)
  where
    parameters = dogruFixtureParameters fixture
    ordered = sortOn dogruPeriodsRemaining (NonEmpty.toList (dogruPhysicalReachableStates fixture))
    step (values, selections) state
        | dogruPeriodsRemaining state == 0 = Right ((state, 0) : values, selections)
        | otherwise = do
            actions <- dogruPhysicalActions parameters state
            evaluated <- traverse (evaluateAction values state) actions
            (action, value) <- minimumAction state evaluated
            Right ((state, value) : values, (state, action) : selections)
    evaluateAction values state action = do
        value <- expectedPhysical values state action
        Right (action, value)
    expectedPhysical values state action =
        fmap sum (traverse contribution (NonEmpty.toList dogruJointDemandOutcomes))
      where
        contribution (demand, mass) = do
            let (successor, cost) = physicalTransitionUnchecked parameters state action demand
            future <- successorValue values successor
            Right (mass * (cost + future))

evaluateRelaxedOracle :: DogruFixture -> Either DogruError (Rational, [(DogruState, DogruRelaxedAction)])
evaluateRelaxedOracle fixture = do
    (values, selections) <- foldM step ([], []) ordered
    initialValue <- lookupValue (dogruInitialState fixture) values
    Right (initialValue, selections)
  where
    parameters = dogruFixtureParameters fixture
    ordered = sortOn dogruPeriodsRemaining (NonEmpty.toList (dogruRelaxedReachableStates fixture))
    step (values, selections) state
        | dogruPeriodsRemaining state == 0 = Right ((state, 0) : values, selections)
        | otherwise = do
            actions <- dogruRelaxedActions parameters state
            evaluated <- traverse (evaluateAction values state) actions
            (action, value) <- minimumAction state evaluated
            Right ((state, value) : values, (state, action) : selections)
    evaluateAction values state action = do
        value <- expectedRelaxed values state action
        Right (action, value)
    expectedRelaxed values state action =
        fmap sum (traverse contribution (NonEmpty.toList dogruJointDemandOutcomes))
      where
        contribution (demand, mass) = do
            let (successor, cost) = relaxedTransitionUnchecked parameters state action demand
            future <- successorValue values successor
            Right (mass * (cost + future))

evaluatePhysicalPolicy :: DogruFixture -> (DogruParameters -> DogruState -> Either DogruError DogruPhysicalAction) -> Either DogruError Rational
evaluatePhysicalPolicy fixture policy = do
    values <- foldM step [] ordered
    lookupValue (dogruInitialState fixture) values
  where
    parameters = dogruFixtureParameters fixture
    ordered = sortOn dogruPeriodsRemaining (NonEmpty.toList (dogruPhysicalReachableStates fixture))
    step values state
        | dogruPeriodsRemaining state == 0 = Right ((state, 0) : values)
        | otherwise = do
            action <- policy parameters state
            contributions <- traverse (contribution values state action) (NonEmpty.toList dogruJointDemandOutcomes)
            Right ((state, sum contributions) : values)
    contribution values state action (demand, mass) = do
        let (successor, cost) = physicalTransitionUnchecked parameters state action demand
        future <- successorValue values successor
        Right (mass * (cost + future))

evaluateRelaxedPolicy :: DogruFixture -> (DogruParameters -> DogruState -> Either DogruError DogruRelaxedAction) -> Either DogruError Rational
evaluateRelaxedPolicy fixture policy = do
    values <- foldM step [] ordered
    lookupValue (dogruInitialState fixture) values
  where
    parameters = dogruFixtureParameters fixture
    ordered = sortOn dogruPeriodsRemaining (NonEmpty.toList (dogruRelaxedReachableStates fixture))
    step values state
        | dogruPeriodsRemaining state == 0 = Right ((state, 0) : values)
        | otherwise = do
            action <- policy parameters state
            contributions <- traverse (contribution values state action) (NonEmpty.toList dogruJointDemandOutcomes)
            Right ((state, sum contributions) : values)
    contribution values state action (demand, mass) = do
        let (successor, cost) = relaxedTransitionUnchecked parameters state action demand
        future <- successorValue values successor
        Right (mass * (cost + future))

physicalSelectedPath :: DogruParameters -> DogruState -> [(DogruState, DogruPhysicalAction)] -> [(DogruState, DogruPhysicalAction)]
physicalSelectedPath parameters initial selections = go [initial] [] []
  where
    go [] _ chosen = chosen
    go (state : pending) visited chosen
        | state `elem` visited = go pending visited chosen
        | dogruPeriodsRemaining state == 0 = go pending (state : visited) chosen
        | otherwise =
            case lookup state selections of
                Nothing -> go pending (state : visited) chosen
                Just action ->
                    let successors =
                            unique
                                [ fst (physicalTransitionUnchecked parameters state action demand)
                                | (demand, _) <- NonEmpty.toList dogruJointDemandOutcomes
                                ]
                     in go (pending ++ successors) (state : visited) ((state, action) : chosen)

relaxedSelectedPath :: DogruParameters -> DogruState -> [(DogruState, DogruRelaxedAction)] -> [(DogruState, DogruRelaxedAction)]
relaxedSelectedPath parameters initial selections = go [initial] [] []
  where
    go [] _ chosen = chosen
    go (state : pending) visited chosen
        | state `elem` visited = go pending visited chosen
        | dogruPeriodsRemaining state == 0 = go pending (state : visited) chosen
        | otherwise =
            case lookup state selections of
                Nothing -> go pending (state : visited) chosen
                Just action ->
                    let successors =
                            unique
                                [ fst (relaxedTransitionUnchecked parameters state action demand)
                                | (demand, _) <- NonEmpty.toList dogruJointDemandOutcomes
                                ]
                     in go (pending ++ successors) (state : visited) ((state, action) : chosen)

minimumAction :: DogruState -> [(action, Rational)] -> Either DogruError (action, Rational)
minimumAction state [] = Left (DogruNoAction state)
minimumAction _ (first : remaining) = Right (foldl' select first remaining)
  where
    select chosen candidate
        | snd candidate < snd chosen = candidate
        | otherwise = chosen

successorValue :: [(DogruState, Rational)] -> DogruState -> Either DogruError Rational
successorValue values successor
    | dogruPeriodsRemaining successor == 0 = Right 0
    | otherwise = maybe (Left (DogruMissingSuccessor successor)) Right (lookup successor values)

lookupValue :: DogruState -> [(DogruState, Rational)] -> Either DogruError Rational
lookupValue state values = maybe (Left (DogruMissingSuccessor state)) Right (lookup state values)

-- | Data for the dogru stability.
data DogruStability = DogruStability
    { dogruBoundsStable :: !Bool
    , dogruRelaxedValueChanged :: !Bool
    , dogruPhysicalValueChanged :: !Bool
    , dogruBalancedValueChanged :: !Bool
    , dogruHeuristicValueChanged :: !Bool
    , dogruWidenedOrderCapSelected :: !Bool
    , dogruWidenedReturnCapSelected :: !Bool
    }
    deriving (Eq, Show)

-- | Compare the dogru bounds.
compareDogruBounds :: DogruSolution -> DogruSolution -> Either DogruError DogruStability
compareDogruBounds primary widened = do
    let primaryFixture = dogruSolutionFixture primary
        widenedFixture = dogruSolutionFixture widened
        primaryParameters = dogruFixtureParameters primaryFixture
        widenedParameters = dogruFixtureParameters widenedFixture
    if dogruInitialState primaryFixture /= dogruInitialState widenedFixture
        then Left (DogruModelMismatch "initial state")
        else Right ()
    if modelCoefficients primaryParameters /= modelCoefficients widenedParameters
        then Left (DogruModelMismatch "horizon or costs")
        else Right ()
    if dogruOrderCap widenedParameters <= dogruOrderCap primaryParameters || dogruReturnCap widenedParameters <= dogruReturnCap primaryParameters
        then Left DogruBoundsNotWidened
        else Right ()
    let relaxedChanged = dogruRelaxedOracleCost primary /= dogruRelaxedOracleCost widened
        physicalChanged = dogruPhysicalOracleCost primary /= dogruPhysicalOracleCost widened
        balancedChanged = dogruBalancedPolicyCost primary /= dogruBalancedPolicyCost widened
        heuristicChanged = dogruPhysicalizedLBHeuristicCost primary /= dogruPhysicalizedLBHeuristicCost widened
        stable =
            not relaxedChanged
                && not physicalChanged
                && not balancedChanged
                && not heuristicChanged
                && not (dogruOrderCapSelected widened)
                && not (dogruReturnCapSelected widened)
    Right
        DogruStability
            { dogruBoundsStable = stable
            , dogruRelaxedValueChanged = relaxedChanged
            , dogruPhysicalValueChanged = physicalChanged
            , dogruBalancedValueChanged = balancedChanged
            , dogruHeuristicValueChanged = heuristicChanged
            , dogruWidenedOrderCapSelected = dogruOrderCapSelected widened
            , dogruWidenedReturnCapSelected = dogruReturnCapSelected widened
            }

modelCoefficients :: DogruParameters -> (Natural, Rational, Rational, Rational)
modelCoefficients parameters =
    ( dogruHorizon parameters
    , dogruWarehouseHoldingCost parameters
    , dogruRetailerHoldingCost parameters
    , dogruRetailerPenaltyCost parameters
    )

unique :: (Ord value) => [value] -> [value]
unique = removeAdjacentDuplicates . sortOn id
  where
    removeAdjacentDuplicates [] = []
    removeAdjacentDuplicates (first : remaining) = first : go first remaining
    go _ [] = []
    go previous (value : values)
        | previous == value = go previous values
        | otherwise = value : go value values

sortedDifference :: (Ord value) => [value] -> [value] -> [value]
sortedDifference candidates [] = candidates
sortedDifference [] _ = []
sortedDifference candidates@(candidate : remainingCandidates) visited@(value : remainingValues)
    | candidate < value = candidate : sortedDifference remainingCandidates visited
    | candidate == value = sortedDifference remainingCandidates remainingValues
    | otherwise = sortedDifference candidates remainingValues

sortedMerge :: (Ord value) => [value] -> [value] -> [value]
sortedMerge [] right = right
sortedMerge left [] = left
sortedMerge left@(leftValue : leftValues) right@(rightValue : rightValues)
    | leftValue < rightValue = leftValue : sortedMerge leftValues right
    | leftValue == rightValue = leftValue : sortedMerge leftValues rightValues
    | otherwise = rightValue : sortedMerge left rightValues

mapLeft :: (error -> other) -> Either error value -> Either other value
mapLeft wrap = either (Left . wrap) Right
