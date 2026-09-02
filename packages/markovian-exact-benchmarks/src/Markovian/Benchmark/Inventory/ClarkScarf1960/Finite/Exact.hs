{- | Clark--Scarf (1960), Section III, specialized to an exact finite lattice.

The state and transition follow Clark and Scarf, /Management Science/ 6(4),
1960, pages 481--482, equations (11)--(14).  Integrals are replaced by finite
rational sums, and external orders are explicitly capped.  These finite bounds
are repository computation assumptions, not assumptions or numerical data from
the paper.
-}
module Markovian.Benchmark.Inventory.ClarkScarf1960.Finite.Exact (
    ClarkScarfParameterInput (..),
    ClarkScarfParameters,
    ClarkScarfError (..),
    validateClarkScarfParameters,
    clarkScarfHorizon,
    clarkScarfDiscount,
    clarkScarfOrderCap,
    clarkScarfIsolatedTargetCap,
    clarkScarfExternalSetupCost,
    clarkScarfExternalUnitCost,
    clarkScarfTransportUnitCost,
    clarkScarfDownstreamHoldingCost,
    clarkScarfDownstreamShortageCost,
    clarkScarfUpstreamHoldingCost,
    clarkScarfUpstreamShortageCost,
    clarkScarfStateBudget,
    clarkScarfStateActionBudget,
    clarkScarfSolverWorkBudget,
    ClarkScarfDemand,
    conditionedClarkScarfDemand,
    clarkScarfDemandOutcomes,
    clarkScarfRetainedDemandMass,
    clarkScarfOmittedDemandMass,
    clarkScarfHorizonExceededMass,
    ClarkScarfState (..),
    ClarkScarfAction (..),
    validClarkScarfState,
    ClarkScarfFixture,
    clarkScarfFixture,
    clarkScarfFixtureParameters,
    clarkScarfFixtureDemand,
    clarkScarfInitialState,
    clarkScarfReachableStates,
    clarkScarfStateLayout,
    clarkScarfActionLayout,
    clarkScarfStateActionCount,
    clarkScarfActions,
    clarkScarfNaturalCost,
    clarkScarfExpectedNaturalCost,
    clarkScarfTransition,
    clarkScarfMDP,
) where

import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Action (actionId, actionValue)
import Markovian.Category.Finite.Set (FiniteSet, FiniteSetError, finiteSet)
import Markovian.Kernel.Exact (exactKernel)
import Markovian.MDP.Exact (
    ExactMDP,
    ExactStateStatus (..),
    exactMDP,
    exactTransitionOutcome,
 )
import Markovian.Objective.Exact (ExactDiscount)
import Markovian.Probability.Exact (ExactDistributionError, ExactFiniteDist, exactFiniteDist)
import Markovian.Reward.Exact (exactReward)
import Numeric.Natural (Natural)

-- | Unvalidated model and computation assumptions.
data ClarkScarfParameterInput = ClarkScarfParameterInput
    { inputClarkScarfHorizon :: !Integer
    , inputClarkScarfDiscount :: !ExactDiscount
    , inputClarkScarfOrderCap :: !Integer
    , inputClarkScarfIsolatedTargetCap :: !Integer
    , inputClarkScarfExternalSetupCost :: !Rational
    , inputClarkScarfExternalUnitCost :: !Rational
    , inputClarkScarfTransportUnitCost :: !Rational
    , inputClarkScarfDownstreamHoldingCost :: !Rational
    , inputClarkScarfDownstreamShortageCost :: !Rational
    , inputClarkScarfUpstreamHoldingCost :: !Rational
    , inputClarkScarfUpstreamShortageCost :: !Rational
    , inputClarkScarfStateBudget :: !Integer
    , inputClarkScarfStateActionBudget :: !Integer
    , inputClarkScarfSolverWorkBudget :: !Integer
    }
    deriving (Eq, Show)

-- | Validated source-model coefficients and repository computation bounds.
data ClarkScarfParameters = ClarkScarfParameters
    { clarkScarfHorizon :: !Natural
    -- ^ The clark scarf horizon.
    , clarkScarfDiscount :: !ExactDiscount
    -- ^ The clark scarf discount.
    , clarkScarfOrderCap :: !Natural
    -- ^ The clark scarf order cap.
    , clarkScarfIsolatedTargetCap :: !Integer
    -- ^ The clark scarf isolated target cap.
    , clarkScarfExternalSetupCost :: !Rational
    -- ^ The clark scarf external setup cost.
    , clarkScarfExternalUnitCost :: !Rational
    -- ^ The clark scarf external unit cost.
    , clarkScarfTransportUnitCost :: !Rational
    -- ^ The clark scarf transport unit cost.
    , clarkScarfDownstreamHoldingCost :: !Rational
    -- ^ The clark scarf downstream holding cost.
    , clarkScarfDownstreamShortageCost :: !Rational
    -- ^ The clark scarf downstream shortage cost.
    , clarkScarfUpstreamHoldingCost :: !Rational
    -- ^ The clark scarf upstream holding cost.
    , clarkScarfUpstreamShortageCost :: !Rational
    -- ^ The clark scarf upstream shortage cost.
    , clarkScarfStateBudget :: !Natural
    -- ^ The clark scarf state budget.
    , clarkScarfStateActionBudget :: !Natural
    -- ^ The clark scarf state action budget.
    , clarkScarfSolverWorkBudget :: !Natural
    -- ^ The clark scarf solver work budget.
    }
    deriving (Eq, Show)

-- | Construction, model, layout, and bound failures.
data ClarkScarfError
    = ClarkScarfNonPositiveHorizon !Integer
    | ClarkScarfNegativeOrderCap !Integer
    | ClarkScarfNegativeTargetCap !Integer
    | ClarkScarfNegativeCost !String !Rational
    | ClarkScarfNonPositiveBudget !String !Integer
    | ClarkScarfEmptyDemand
    | ClarkScarfNegativeDemand !Integer
    | ClarkScarfNonPositiveDemandMass !Integer !Rational
    | ClarkScarfDuplicateDemand !Natural
    | ClarkScarfDemandMassNotOne !Rational
    | ClarkScarfInvalidRetainedMass !Rational
    | ClarkScarfDemandDistributionError !ExactDistributionError
    | ClarkScarfInitialHorizonMismatch !Natural !Natural
    | ClarkScarfInvalidState !ClarkScarfState
    | ClarkScarfUnavailableAction !ClarkScarfState !ClarkScarfAction
    | ClarkScarfStateBudgetExceeded !Natural
    | ClarkScarfStateActionBudgetExceeded !Natural
    | ClarkScarfSolverWorkBudgetExceeded !Natural
    | ClarkScarfStateLayoutError !(FiniteSetError ClarkScarfState)
    | ClarkScarfActionLayoutError !(FiniteSetError ClarkScarfAction)
    | ClarkScarfMissingSuccessor !ClarkScarfState
    | ClarkScarfNoAction !ClarkScarfState
    | ClarkScarfModelError !String
    | ClarkScarfComparisonMismatch !String
    | ClarkScarfBoundsNotWidened
    deriving (Eq, Show)

-- | Validate the clark scarf parameters.
validateClarkScarfParameters :: ClarkScarfParameterInput -> Either ClarkScarfError ClarkScarfParameters
validateClarkScarfParameters input
    | inputClarkScarfHorizon input <= 0 = Left (ClarkScarfNonPositiveHorizon (inputClarkScarfHorizon input))
    | inputClarkScarfOrderCap input < 0 = Left (ClarkScarfNegativeOrderCap (inputClarkScarfOrderCap input))
    | inputClarkScarfIsolatedTargetCap input < 0 = Left (ClarkScarfNegativeTargetCap (inputClarkScarfIsolatedTargetCap input))
    | otherwise = do
        checkCost "external setup" (inputClarkScarfExternalSetupCost input)
        checkCost "external unit" (inputClarkScarfExternalUnitCost input)
        checkCost "transport unit" (inputClarkScarfTransportUnitCost input)
        checkCost "downstream holding" (inputClarkScarfDownstreamHoldingCost input)
        checkCost "downstream shortage" (inputClarkScarfDownstreamShortageCost input)
        checkCost "upstream holding" (inputClarkScarfUpstreamHoldingCost input)
        checkCost "upstream shortage" (inputClarkScarfUpstreamShortageCost input)
        stateBudget <- checkBudget "state" (inputClarkScarfStateBudget input)
        stateActionBudget <- checkBudget "state-action" (inputClarkScarfStateActionBudget input)
        solverBudget <- checkBudget "solver-work" (inputClarkScarfSolverWorkBudget input)
        Right
            ClarkScarfParameters
                { clarkScarfHorizon = fromInteger (inputClarkScarfHorizon input)
                , clarkScarfDiscount = inputClarkScarfDiscount input
                , clarkScarfOrderCap = fromInteger (inputClarkScarfOrderCap input)
                , clarkScarfIsolatedTargetCap = inputClarkScarfIsolatedTargetCap input
                , clarkScarfExternalSetupCost = inputClarkScarfExternalSetupCost input
                , clarkScarfExternalUnitCost = inputClarkScarfExternalUnitCost input
                , clarkScarfTransportUnitCost = inputClarkScarfTransportUnitCost input
                , clarkScarfDownstreamHoldingCost = inputClarkScarfDownstreamHoldingCost input
                , clarkScarfDownstreamShortageCost = inputClarkScarfDownstreamShortageCost input
                , clarkScarfUpstreamHoldingCost = inputClarkScarfUpstreamHoldingCost input
                , clarkScarfUpstreamShortageCost = inputClarkScarfUpstreamShortageCost input
                , clarkScarfStateBudget = stateBudget
                , clarkScarfStateActionBudget = stateActionBudget
                , clarkScarfSolverWorkBudget = solverBudget
                }
  where
    checkCost label value
        | value < 0 = Left (ClarkScarfNegativeCost label value)
        | otherwise = Right ()
    checkBudget label value
        | value <= 0 = Left (ClarkScarfNonPositiveBudget label value)
        | otherwise = Right (fromInteger value)

-- | A finite rational demand law and its retained mass before conditioning.
data ClarkScarfDemand = ClarkScarfDemand
    { clarkScarfDemandOutcomes :: !(NonEmpty (Natural, Rational))
    -- ^ The clark scarf demand outcomes.
    , clarkScarfRetainedDemandMass :: !Rational
    -- ^ The clark scarf retained demand mass.
    }
    deriving (Eq, Show)

-- | Validate a normalized finite law. The retained mass records truncation only.
conditionedClarkScarfDemand :: Rational -> [(Integer, Rational)] -> Either ClarkScarfError ClarkScarfDemand
conditionedClarkScarfDemand retained raw
    | retained <= 0 || retained > 1 = Left (ClarkScarfInvalidRetainedMass retained)
    | null raw = Left ClarkScarfEmptyDemand
    | otherwise = do
        converted <- traverse convert raw
        case firstDuplicate (fmap fst converted) of
            Just duplicate -> Left (ClarkScarfDuplicateDemand duplicate)
            Nothing ->
                let total = sum (fmap snd converted)
                 in if total /= 1
                        then Left (ClarkScarfDemandMassNotOne total)
                        else case NonEmpty.nonEmpty converted of
                            Nothing -> Left ClarkScarfEmptyDemand
                            Just outcomes -> Right (ClarkScarfDemand outcomes retained)
  where
    convert (demand, mass)
        | demand < 0 = Left (ClarkScarfNegativeDemand demand)
        | mass <= 0 = Left (ClarkScarfNonPositiveDemandMass demand mass)
        | otherwise = Right (fromInteger demand, mass)

-- | Return the clark scarf omitted demand mass.
clarkScarfOmittedDemandMass :: ClarkScarfDemand -> Rational
clarkScarfOmittedDemandMass demand = 1 - clarkScarfRetainedDemandMass demand

{- | Chance that at least one unconditioned draw falls outside retained support.
This is not a value-error bound.
-}
clarkScarfHorizonExceededMass :: Natural -> ClarkScarfDemand -> Rational
clarkScarfHorizonExceededMass horizon demand = 1 - clarkScarfRetainedDemandMass demand ^ horizon

-- | Beginning-of-period Section III state @(x1,w1,x2)@ with a finite horizon.
data ClarkScarfState = ClarkScarfState
    { clarkScarfPeriodsRemaining :: !Natural
    , clarkScarfDownstreamNetStock :: !Integer
    , clarkScarfDownstreamInTransit :: !Natural
    , clarkScarfEchelonTwoStock :: !Integer
    }
    deriving (Eq, Ord, Show)

-- | External order @z@ and downstream post-dispatch target @y@.
data ClarkScarfAction = ClarkScarfAction
    { clarkScarfExternalOrder :: !Natural
    , clarkScarfDispatchTarget :: !Integer
    }
    deriving (Eq, Ord, Show)

-- | Return the valid clark scarf state.
validClarkScarfState :: ClarkScarfState -> Bool
validClarkScarfState state =
    clarkScarfEchelonTwoStock state
        - clarkScarfDownstreamNetStock state
        - toInteger (clarkScarfDownstreamInTransit state)
        >= 0

-- | Validated model, complete reachable support, and explicit layouts.
data ClarkScarfFixture = ClarkScarfFixture
    { clarkScarfFixtureParameters :: !ClarkScarfParameters
    -- ^ The clark scarf fixture parameters.
    , clarkScarfFixtureDemand :: !ClarkScarfDemand
    -- ^ The clark scarf fixture demand.
    , clarkScarfInitialState :: !ClarkScarfState
    -- ^ The clark scarf initial state.
    , fixtureDemandDistribution :: !(ExactFiniteDist Natural)
    , clarkScarfReachableStates :: !(NonEmpty ClarkScarfState)
    -- ^ The clark scarf reachable states.
    , clarkScarfStateLayout :: !(FiniteSet ClarkScarfState)
    -- ^ The clark scarf state layout.
    , clarkScarfActionLayout :: !(FiniteSet ClarkScarfAction)
    -- ^ The clark scarf action layout.
    , clarkScarfStateActionCount :: !Natural
    -- ^ The clark scarf state action count.
    }

instance Show ClarkScarfFixture where
    show fixture =
        "ClarkScarfFixture {initialState = "
            ++ show (clarkScarfInitialState fixture)
            ++ ", states = "
            ++ show (length (clarkScarfReachableStates fixture))
            ++ ", stateActions = "
            ++ show (clarkScarfStateActionCount fixture)
            ++ "}"

-- | Return the clark scarf fixture.
clarkScarfFixture ::
    ClarkScarfParameters ->
    ClarkScarfDemand ->
    ClarkScarfState ->
    Either ClarkScarfError ClarkScarfFixture
clarkScarfFixture parameters demand initial
    | clarkScarfPeriodsRemaining initial /= clarkScarfHorizon parameters =
        Left (ClarkScarfInitialHorizonMismatch (clarkScarfHorizon parameters) (clarkScarfPeriodsRemaining initial))
    | not (validClarkScarfState initial) = Left (ClarkScarfInvalidState initial)
    | otherwise = do
        distribution <- mapLeft ClarkScarfDemandDistributionError (exactFiniteDist (NonEmpty.toList (clarkScarfDemandOutcomes demand)))
        reachable <- reachableSupport parameters demand initial
        let states = NonEmpty.toList reachable
        (stateActions, actionLists) <- collectActionLists parameters states
        let actions = unique (concat actionLists)
        stateLayout <- mapLeft ClarkScarfStateLayoutError (finiteSet states)
        actionLayout <- mapLeft ClarkScarfActionLayoutError (finiteSet actions)
        Right
            ClarkScarfFixture
                { clarkScarfFixtureParameters = parameters
                , clarkScarfFixtureDemand = demand
                , clarkScarfInitialState = initial
                , fixtureDemandDistribution = distribution
                , clarkScarfReachableStates = reachable
                , clarkScarfStateLayout = stateLayout
                , clarkScarfActionLayout = actionLayout
                , clarkScarfStateActionCount = stateActions
                }

-- | Preflight and enumerate feasible equation-(14) actions in @(z,y)@ order.
clarkScarfActions :: ClarkScarfParameters -> ClarkScarfState -> Either ClarkScarfError [ClarkScarfAction]
clarkScarfActions parameters state
    | not (validClarkScarfState state) = Left (ClarkScarfInvalidState state)
    | actionCount > clarkScarfStateActionBudget parameters =
        Left (ClarkScarfStateActionBudgetExceeded actionCount)
    | otherwise = Right (physicalActions parameters state)
  where
    actionCount = clarkScarfActionCountAt parameters state

clarkScarfActionCountAt :: ClarkScarfParameters -> ClarkScarfState -> Natural
clarkScarfActionCountAt parameters state = (clarkScarfOrderCap parameters + 1) * targetCount
  where
    position = clarkScarfDownstreamNetStock state + toInteger (clarkScarfDownstreamInTransit state)
    targetCount
        | clarkScarfPeriodsRemaining state == 0 = 0
        | otherwise = fromInteger (clarkScarfEchelonTwoStock state - position + 1)

physicalActions :: ClarkScarfParameters -> ClarkScarfState -> [ClarkScarfAction]
physicalActions parameters state
    | clarkScarfPeriodsRemaining state == 0 = []
    | otherwise =
        [ ClarkScarfAction order target
        | order <- [0 .. clarkScarfOrderCap parameters]
        , target <- [position .. clarkScarfEchelonTwoStock state]
        ]
  where
    position = clarkScarfDownstreamNetStock state + toInteger (clarkScarfDownstreamInTransit state)

-- | Realized finite-sum form of equation (1): beginning holding plus shortage.
clarkScarfNaturalCost :: Rational -> Rational -> Integer -> Natural -> Rational
clarkScarfNaturalCost holding shortage stock demand =
    holding * fromInteger (max stock 0)
        + shortage * fromInteger (max (toInteger demand - stock) 0)

-- | Return the clark scarf expected natural cost.
clarkScarfExpectedNaturalCost :: ClarkScarfDemand -> Rational -> Rational -> Integer -> Rational
clarkScarfExpectedNaturalCost demand holding shortage stock =
    sum
        [ mass * clarkScarfNaturalCost holding shortage stock demandValue
        | (demandValue, mass) <- NonEmpty.toList (clarkScarfDemandOutcomes demand)
        ]

-- | The equation-(14) successor and same-demand realized cost.
clarkScarfTransition ::
    ClarkScarfParameters ->
    ClarkScarfState ->
    ClarkScarfAction ->
    Natural ->
    Either ClarkScarfError (ClarkScarfState, Rational)
clarkScarfTransition parameters state action demand = do
    actions <- clarkScarfActions parameters state
    if action `elem` actions
        then Right (clarkScarfTransitionValue parameters state action demand)
        else Left (ClarkScarfUnavailableAction state action)

clarkScarfTransitionValue ::
    ClarkScarfParameters ->
    ClarkScarfState ->
    ClarkScarfAction ->
    Natural ->
    (ClarkScarfState, Rational)
clarkScarfTransitionValue parameters state action demand = (successor, cost)
  where
    x1 = clarkScarfDownstreamNetStock state
    w1 = clarkScarfDownstreamInTransit state
    x2 = clarkScarfEchelonTwoStock state
    y = clarkScarfDispatchTarget action
    order = clarkScarfExternalOrder action
    dispatch = y - x1 - toInteger w1
    successor =
        ClarkScarfState
            { clarkScarfPeriodsRemaining = clarkScarfPeriodsRemaining state - 1
            , clarkScarfDownstreamNetStock = x1 + toInteger w1 - toInteger demand
            , clarkScarfDownstreamInTransit = fromInteger dispatch
            , clarkScarfEchelonTwoStock = x2 + toInteger order - toInteger demand
            }
    externalCost
        | order == 0 = 0
        | otherwise = clarkScarfExternalSetupCost parameters + clarkScarfExternalUnitCost parameters * fromIntegral order
    cost =
        externalCost
            + clarkScarfTransportUnitCost parameters * fromInteger dispatch
            + clarkScarfNaturalCost
                (clarkScarfDownstreamHoldingCost parameters)
                (clarkScarfDownstreamShortageCost parameters)
                x1
                demand
            + clarkScarfNaturalCost
                (clarkScarfUpstreamHoldingCost parameters)
                (clarkScarfUpstreamShortageCost parameters)
                x2
                demand

-- | Exact MDP view. Reward and successor remain paired by one demand draw.
clarkScarfMDP :: ClarkScarfFixture -> ExactMDP ClarkScarfState ClarkScarfAction
clarkScarfMDP fixture = exactMDP (clarkScarfInitialState fixture) status available (exactKernel transition)
  where
    parameters = clarkScarfFixtureParameters fixture
    status state
        | clarkScarfPeriodsRemaining state == 0 = ExactTerminal (exactReward 0)
        | otherwise = ExactContinuing
    available state =
        case clarkScarfActions parameters state of
            Left _ -> []
            Right actions -> fmap actionId actions
    transition (state, selected) =
        fmap makeOutcome (fixtureDemandDistribution fixture)
      where
        makeOutcome demand =
            let (successor, cost) = clarkScarfTransitionValue parameters state (actionValue selected) demand
             in exactTransitionOutcome (exactReward (negate cost)) successor

reachableSupport ::
    ClarkScarfParameters ->
    ClarkScarfDemand ->
    ClarkScarfState ->
    Either ClarkScarfError (NonEmpty ClarkScarfState)
reachableSupport parameters demand initial = go 0 [initial] [initial]
  where
    demandValues = fmap fst (NonEmpty.toList (clarkScarfDemandOutcomes demand))
    go _ [] visited =
        case NonEmpty.nonEmpty visited of
            Nothing -> Right (initial :| [])
            Just values -> Right values
    go usedActions (state : pending) visited = do
        let nextUsedActions = usedActions + clarkScarfActionCountAt parameters state
        if nextUsedActions > clarkScarfStateActionBudget parameters
            then Left (ClarkScarfStateActionBudgetExceeded nextUsedActions)
            else Right ()
        actions <- clarkScarfActions parameters state
        let successors =
                [ fst (clarkScarfTransitionValue parameters state action demandValue)
                | action <- actions
                , demandValue <- demandValues
                ]
        let unseen = unique [successor | successor <- successors, successor `notElem` visited]
            next = visited ++ unseen
            count = fromIntegral (length next)
        if count > clarkScarfStateBudget parameters
            then Left (ClarkScarfStateBudgetExceeded count)
            else go nextUsedActions (pending ++ unseen) next

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate [] = Nothing
firstDuplicate (value : remaining)
    | value `elem` remaining = Just value
    | otherwise = firstDuplicate remaining

collectActionLists ::
    ClarkScarfParameters ->
    [ClarkScarfState] ->
    Either ClarkScarfError (Natural, [[ClarkScarfAction]])
collectActionLists parameters = go 0 []
  where
    go count lists [] = Right (count, reverse lists)
    go count lists (state : states) =
        let nextCount = count + clarkScarfActionCountAt parameters state
         in if nextCount > clarkScarfStateActionBudget parameters
                then Left (ClarkScarfStateActionBudgetExceeded nextCount)
                else go nextCount (physicalActions parameters state : lists) states

unique :: (Eq value) => [value] -> [value]
unique = foldl' (\values value -> if value `elem` values then values else values ++ [value]) []

mapLeft :: (error -> other) -> Either error value -> Either other value
mapLeft wrap = either (Left . wrap) Right
