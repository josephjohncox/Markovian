{-# LANGUAGE TupleSections #-}

{- | Bounded exact two-stage serial execution with fixed integer-ratio batches.

The period order follows Doğru, van Houtum, and de Kok, BETA Working Paper
134 (2005), section 2, pages 4--5: observe and order, receive due material,
observe demand, then assess end-of-period cost.  This module is a finite-horizon
adaptation.  It does not implement the paper's stationary average-cost proof.
-}
module Markovian.Benchmark.Inventory.Serial.FixedBatch.Exact (
    FixedBatchParameterInput (..),
    FixedBatchParameters,
    FixedBatchError (..),
    validateFixedBatchParameters,
    fixedBatchHorizon,
    fixedBatchSupplierLeadTime,
    fixedBatchQ1,
    fixedBatchQ2,
    fixedBatchExternalBatchCap,
    fixedBatchDemandCap,
    fixedBatchStage1HoldingCost,
    fixedBatchStage2HoldingCost,
    fixedBatchBacklogCost,
    fixedBatchStateBudget,
    fixedBatchStateActionBudget,
    fixedBatchSolverWorkBudget,
    fixedBatchGridBudget,
    fixedBatchConvolutionBudget,
    fixedBatchMaximumLeadTime,
    fixedBatchMaximumDemandOutcomes,
    FixedBatchState,
    fixedBatchState,
    fixedBatchPeriodsRemaining,
    fixedBatchStage1NetInventory,
    fixedBatchStage2OnHand,
    fixedBatchPipeline,
    FixedBatchAction (..),
    FixedBatchOutcome (..),
    FixedBatchDemand,
    conditionedGeometricDemand,
    fixedBatchDemandOutcomes,
    fixedBatchRetainedMass,
    fixedBatchOmittedMass,
    fixedBatchPathExceedanceMass,
    fixedBatchActions,
    fixedBatchTransition,
    fixedBatchOutcomes,
    FixedBatchFixture,
    fixedBatchFixture,
    fixedBatchFixtureParameters,
    fixedBatchFixtureDemand,
    fixedBatchInitialState,
    fixedBatchReachableStates,
    fixedBatchStateLayout,
    fixedBatchActionLayout,
    fixedBatchStateActionCount,
    sameFixedBatchStateSupport,
    sameFixedBatchStateLayout,
    ReorderLevels (..),
    ReorderGrid,
    reorderGrid,
    reorderGridCandidates,
    fixedBatchOrderQuantity,
    PolicyAction (..),
    fixedBatchPolicyAction,
    FixedBatchSolution,
    solveFixedBatch,
    fixedBatchSolutionFixture,
    fixedBatchSolutionGrid,
    fixedBatchOracleReturn,
    fixedBatchPolicyReturn,
    fixedBatchPolicyRegret,
    fixedBatchSelectedLevels,
    fixedBatchOracleExternalCapSelected,
    fixedBatchPolicyExternalCapClipped,
    fixedBatchPolicyReleaseClipped,
    fixedBatchGridBinds,
    fixedBatchCheckedSolverWork,
    evaluateFixedBatchPolicy,
    FixedBatchStability (..),
    compareFixedBatchBounds,
    FixedBatchDemandDiagnostic (..),
    compareFixedBatchDemandCaps,
) where

import Control.Monad (foldM)
import Data.List (foldl', sort, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Category.Finite.Set (FiniteSet, FiniteSetError, finiteSet, sameFiniteLayout, sameFiniteSet)
import Numeric.Natural (Natural)

-- | Source-model coefficients and repository computation bounds before validation.
data FixedBatchParameterInput = FixedBatchParameterInput
    { inputFixedBatchHorizon :: !Integer
    , inputFixedBatchSupplierLeadTime :: !Integer
    , inputFixedBatchQ1 :: !Integer
    , inputFixedBatchQ2 :: !Integer
    , inputFixedBatchExternalBatchCap :: !Integer
    , inputFixedBatchDemandCap :: !Integer
    , inputFixedBatchStage1HoldingCost :: !Rational
    , inputFixedBatchStage2HoldingCost :: !Rational
    , inputFixedBatchBacklogCost :: !Rational
    , inputFixedBatchStateBudget :: !Integer
    , inputFixedBatchStateActionBudget :: !Integer
    , inputFixedBatchSolverWorkBudget :: !Integer
    , inputFixedBatchGridBudget :: !Integer
    , inputFixedBatchConvolutionBudget :: !Integer
    }
    deriving (Eq, Show)

data FixedBatchParameters = FixedBatchParameters
    { fixedBatchHorizon :: !Natural
    , fixedBatchSupplierLeadTime :: !Natural
    , fixedBatchQ1 :: !Natural
    , fixedBatchQ2 :: !Natural
    , fixedBatchExternalBatchCap :: !Natural
    , fixedBatchDemandCap :: !Natural
    , fixedBatchStage1HoldingCost :: !Rational
    , fixedBatchStage2HoldingCost :: !Rational
    , fixedBatchBacklogCost :: !Rational
    , fixedBatchStateBudget :: !Natural
    , fixedBatchStateActionBudget :: !Natural
    , fixedBatchSolverWorkBudget :: !Natural
    , fixedBatchGridBudget :: !Natural
    , fixedBatchConvolutionBudget :: !Natural
    }
    deriving (Eq, Show)

data FixedBatchError
    = FixedBatchNonPositive !String !Integer
    | FixedBatchNegative !String !Integer
    | FixedBatchNonPositiveCost !String !Rational
    | FixedBatchNonIntegralBatchRatio !Integer !Integer
    | FixedBatchDemandCapExcludesOne !Integer
    | FixedBatchLeadTimeBudgetExceeded !Natural
    | FixedBatchDemandOutcomeBudgetExceeded !Natural
    | FixedBatchPipelineLengthMismatch !Natural !Natural
    | FixedBatchPipelineCongruence !Natural
    | FixedBatchStage2Congruence !Natural
    | FixedBatchPeriodsExceedHorizon !Natural !Natural
    | FixedBatchInitialHorizonMismatch !Natural !Natural
    | FixedBatchUnavailableAction !FixedBatchState !FixedBatchAction
    | FixedBatchStateOutsideFixture !FixedBatchState
    | FixedBatchTerminalAction !FixedBatchState
    | FixedBatchStateBudgetExceeded !Natural
    | FixedBatchStateActionBudgetExceeded !Natural
    | FixedBatchSolverWorkBudgetExceeded !Natural
    | FixedBatchGridBudgetExceeded !Natural
    | FixedBatchConvolutionBudgetExceeded !Natural
    | FixedBatchDuplicateGridPoint !ReorderLevels
    | FixedBatchEmptyGrid
    | FixedBatchStateLayoutError !(FiniteSetError FixedBatchState)
    | FixedBatchActionLayoutError !(FiniteSetError FixedBatchAction)
    | FixedBatchMissingSuccessor !FixedBatchState
    | FixedBatchNoAction !FixedBatchState
    | FixedBatchNegativeRegret !Rational
    | FixedBatchModelMismatch !String
    | FixedBatchBoundsNotWidened
    | FixedBatchGridNotSuperset !ReorderLevels
    | FixedBatchDemandCapNotIncreased !Natural !Natural
    deriving (Eq, Show)

validateFixedBatchParameters :: FixedBatchParameterInput -> Either FixedBatchError FixedBatchParameters
validateFixedBatchParameters input = do
    horizon <- positive "horizon" (inputFixedBatchHorizon input)
    leadTime <- positive "supplier lead time L2" (inputFixedBatchSupplierLeadTime input)
    if leadTime > fixedBatchMaximumLeadTime
        then Left (FixedBatchLeadTimeBudgetExceeded leadTime)
        else Right ()
    q1 <- positive "Q1" (inputFixedBatchQ1 input)
    q2 <- positive "Q2" (inputFixedBatchQ2 input)
    if q2 `mod` q1 /= 0
        then Left (FixedBatchNonIntegralBatchRatio (inputFixedBatchQ1 input) (inputFixedBatchQ2 input))
        else Right ()
    externalCap <- nonnegative "external batch cap" (inputFixedBatchExternalBatchCap input)
    demandCap <- nonnegative "demand cap" (inputFixedBatchDemandCap input)
    if demandCap < 1
        then Left (FixedBatchDemandCapExcludesOne (inputFixedBatchDemandCap input))
        else Right ()
    if demandCap + 1 > fixedBatchMaximumDemandOutcomes
        then Left (FixedBatchDemandOutcomeBudgetExceeded (demandCap + 1))
        else Right ()
    checkCost "stage-1 echelon holding" (inputFixedBatchStage1HoldingCost input)
    checkCost "stage-2 echelon holding" (inputFixedBatchStage2HoldingCost input)
    checkCost "backlog penalty" (inputFixedBatchBacklogCost input)
    stateBudget <- positive "state budget" (inputFixedBatchStateBudget input)
    stateActionBudget <- positive "state-action budget" (inputFixedBatchStateActionBudget input)
    solverBudget <- positive "solver-work budget" (inputFixedBatchSolverWorkBudget input)
    gridBudget <- positive "reorder-grid budget" (inputFixedBatchGridBudget input)
    convolutionBudget <- positive "newsvendor convolution budget" (inputFixedBatchConvolutionBudget input)
    Right
        FixedBatchParameters
            { fixedBatchHorizon = horizon
            , fixedBatchSupplierLeadTime = leadTime
            , fixedBatchQ1 = q1
            , fixedBatchQ2 = q2
            , fixedBatchExternalBatchCap = externalCap
            , fixedBatchDemandCap = demandCap
            , fixedBatchStage1HoldingCost = inputFixedBatchStage1HoldingCost input
            , fixedBatchStage2HoldingCost = inputFixedBatchStage2HoldingCost input
            , fixedBatchBacklogCost = inputFixedBatchBacklogCost input
            , fixedBatchStateBudget = stateBudget
            , fixedBatchStateActionBudget = stateActionBudget
            , fixedBatchSolverWorkBudget = solverBudget
            , fixedBatchGridBudget = gridBudget
            , fixedBatchConvolutionBudget = convolutionBudget
            }
  where
    positive label value
        | value <= 0 = Left (FixedBatchNonPositive label value)
        | otherwise = Right (fromInteger value)
    nonnegative label value
        | value < 0 = Left (FixedBatchNegative label value)
        | otherwise = Right (fromInteger value)
    checkCost label value
        | value <= 0 = Left (FixedBatchNonPositiveCost label value)
        | otherwise = Right ()

-- | Validated beginning-of-period state. The pipeline head is due this period.
data FixedBatchState = FixedBatchState
    { fixedBatchPeriodsRemaining :: !Natural
    , fixedBatchStage1NetInventory :: !Integer
    , fixedBatchStage2OnHand :: !Natural
    , fixedBatchPipeline :: ![Natural]
    }
    deriving (Eq, Ord, Show)

fixedBatchState :: FixedBatchParameters -> Natural -> Integer -> Natural -> [Natural] -> Either FixedBatchError FixedBatchState
fixedBatchState parameters remaining stage1 stage2 pipeline
    | remaining > fixedBatchHorizon parameters = Left (FixedBatchPeriodsExceedHorizon (fixedBatchHorizon parameters) remaining)
    | fromIntegral (length pipeline) /= fixedBatchSupplierLeadTime parameters =
        Left (FixedBatchPipelineLengthMismatch (fixedBatchSupplierLeadTime parameters) (fromIntegral (length pipeline)))
    | stage2 `mod` fixedBatchQ1 parameters /= 0 = Left (FixedBatchStage2Congruence stage2)
    | Just quantity <- firstNotMultiple (fixedBatchQ2 parameters) pipeline = Left (FixedBatchPipelineCongruence quantity)
    | otherwise = Right (FixedBatchState remaining stage1 stage2 pipeline)

-- | Physical release to stage 1 and external order placed in batches.
data FixedBatchAction = FixedBatchAction
    { fixedBatchStage1Release :: !Natural
    , fixedBatchExternalOrder :: !Natural
    }
    deriving (Eq, Ord, Show)

-- | One demand draw, mass, realized successor, and its correlated cost.
data FixedBatchOutcome = FixedBatchOutcome
    { fixedBatchOutcomeDemand :: !Natural
    , fixedBatchOutcomeMass :: !Rational
    , fixedBatchOutcomeSuccessor :: !FixedBatchState
    , fixedBatchOutcomeCost :: !Rational
    }
    deriving (Eq, Show)

data FixedBatchDemand = FixedBatchDemand
    { fixedBatchDemandOutcomes :: !(NonEmpty (Natural, Rational))
    , fixedBatchRetainedMass :: !Rational
    }
    deriving (Eq, Show)

-- | Condition @P(D=d)=2^-(d+1)@ on @0<=d<=cap@.
conditionedGeometricDemand :: Natural -> FixedBatchDemand
conditionedGeometricDemand cap = FixedBatchDemand (fmap normalize raw) retained
  where
    raw = fmap (\demand -> (demand, 1 / (2 ^ (demand + 1)))) (0 :| [1 .. cap])
    retained = sum (fmap snd (NonEmpty.toList raw))
    normalize (demand, mass) = (demand, mass / retained)

fixedBatchOmittedMass :: FixedBatchDemand -> Rational
fixedBatchOmittedMass demand = 1 - fixedBatchRetainedMass demand

-- | Unconditioned chance that at least one draw exceeds the cap; not a value-error bound.
fixedBatchPathExceedanceMass :: Natural -> FixedBatchDemand -> Rational
fixedBatchPathExceedanceMass horizon demand = 1 - fixedBatchRetainedMass demand ^ horizon

fixedBatchActions :: FixedBatchParameters -> FixedBatchState -> Either FixedBatchError [FixedBatchAction]
fixedBatchActions parameters state = do
    _ <- fixedBatchState parameters (fixedBatchPeriodsRemaining state) (fixedBatchStage1NetInventory state) (fixedBatchStage2OnHand state) (fixedBatchPipeline state)
    if count > fixedBatchStateActionBudget parameters
        then Left (FixedBatchStateActionBudgetExceeded count)
        else Right (fixedBatchActionsUnchecked parameters state)
  where
    count = fixedBatchActionCountAt parameters state

fixedBatchActionCountAt :: FixedBatchParameters -> FixedBatchState -> Natural
fixedBatchActionCountAt parameters state
    | fixedBatchPeriodsRemaining state == 0 = 0
    | otherwise = (available `div` fixedBatchQ1 parameters + 1) * (fixedBatchExternalBatchCap parameters + 1)
  where
    available = fixedBatchStage2OnHand state + dueQuantity state

fixedBatchActionsUnchecked :: FixedBatchParameters -> FixedBatchState -> [FixedBatchAction]
fixedBatchActionsUnchecked parameters state
    | fixedBatchPeriodsRemaining state == 0 = []
    | otherwise =
        [ FixedBatchAction release (externalBatches * fixedBatchQ2 parameters)
        | release <- multiplesThrough (fixedBatchQ1 parameters) available
        , externalBatches <- [0 .. fixedBatchExternalBatchCap parameters]
        ]
  where
    available = fixedBatchStage2OnHand state + dueQuantity state

fixedBatchTransition :: FixedBatchParameters -> FixedBatchState -> FixedBatchAction -> Natural -> Either FixedBatchError (FixedBatchState, Rational)
fixedBatchTransition parameters state action demand
    | fixedBatchPeriodsRemaining state == 0 = Left (FixedBatchTerminalAction state)
    | otherwise = do
        actions <- fixedBatchActions parameters state
        if action `notElem` actions
            then Left (FixedBatchUnavailableAction state action)
            else Right (transitionUnchecked parameters state action demand)

fixedBatchOutcomes :: FixedBatchFixture -> FixedBatchState -> FixedBatchAction -> Either FixedBatchError (NonEmpty FixedBatchOutcome)
fixedBatchOutcomes fixture state action = do
    if state `elem` NonEmpty.toList (fixedBatchReachableStates fixture)
        then Right ()
        else Left (FixedBatchStateOutsideFixture state)
    _ <- fixedBatchTransition parameters state action 0
    pure (fmap outcome (fixedBatchDemandOutcomes (fixedBatchFixtureDemand fixture)))
  where
    parameters = fixedBatchFixtureParameters fixture
    outcome (demand, mass) =
        let (successor, cost) = transitionUnchecked parameters state action demand
         in FixedBatchOutcome demand mass successor cost

transitionUnchecked :: FixedBatchParameters -> FixedBatchState -> FixedBatchAction -> Natural -> (FixedBatchState, Rational)
transitionUnchecked parameters state action demand = (successor, cost)
  where
    available = fixedBatchStage2OnHand state + dueQuantity state
    nextStage2 = available - fixedBatchStage1Release action
    nextStage1 = fixedBatchStage1NetInventory state + toInteger (fixedBatchStage1Release action) - toInteger demand
    nextPipeline = drop 1 (fixedBatchPipeline state) ++ [fixedBatchExternalOrder action]
    successor = FixedBatchState (fixedBatchPeriodsRemaining state - 1) nextStage1 nextStage2 nextPipeline
    installationStage1 = fixedBatchStage1HoldingCost parameters + fixedBatchStage2HoldingCost parameters
    cost =
        fixedBatchStage2HoldingCost parameters * fromIntegral nextStage2
            + installationStage1 * fromInteger (max nextStage1 0)
            + fixedBatchBacklogCost parameters * fromInteger (max (negate nextStage1) 0)

dueQuantity :: FixedBatchState -> Natural
dueQuantity state = case fixedBatchPipeline state of
    due : _ -> due
    [] -> 0

-- | Complete reachable closure and represented finite layouts.
data FixedBatchFixture = FixedBatchFixture
    { fixedBatchFixtureParameters :: !FixedBatchParameters
    , fixedBatchFixtureDemand :: !FixedBatchDemand
    , fixedBatchInitialState :: !FixedBatchState
    , fixedBatchReachableStates :: !(NonEmpty FixedBatchState)
    , fixedBatchStateLayout :: !(FiniteSet FixedBatchState)
    , fixedBatchActionLayout :: !(FiniteSet FixedBatchAction)
    , fixedBatchStateActionCount :: !Natural
    }

instance Show FixedBatchFixture where
    show fixture =
        "FixedBatchFixture {states="
            ++ show (length (fixedBatchReachableStates fixture))
            ++ ", stateActions="
            ++ show (fixedBatchStateActionCount fixture)
            ++ "}"

fixedBatchFixture :: FixedBatchParameters -> FixedBatchState -> Either FixedBatchError FixedBatchFixture
fixedBatchFixture parameters initial
    | fixedBatchPeriodsRemaining initial /= fixedBatchHorizon parameters =
        Left (FixedBatchInitialHorizonMismatch (fixedBatchHorizon parameters) (fixedBatchPeriodsRemaining initial))
    | otherwise = do
        _ <- fixedBatchState parameters (fixedBatchPeriodsRemaining initial) (fixedBatchStage1NetInventory initial) (fixedBatchStage2OnHand initial) (fixedBatchPipeline initial)
        (states, actionCount) <- reachable parameters demand initial
        stateLayout <- mapLeft FixedBatchStateLayoutError (finiteSet (NonEmpty.toList states))
        actionLayout <- mapLeft FixedBatchActionLayoutError (finiteSet (unique (concatMap (fixedBatchActionsUnchecked parameters) (NonEmpty.toList states))))
        Right
            FixedBatchFixture
                { fixedBatchFixtureParameters = parameters
                , fixedBatchFixtureDemand = demand
                , fixedBatchInitialState = initial
                , fixedBatchReachableStates = states
                , fixedBatchStateLayout = stateLayout
                , fixedBatchActionLayout = actionLayout
                , fixedBatchStateActionCount = actionCount
                }
  where
    demand = conditionedGeometricDemand (fixedBatchDemandCap parameters)

reachable :: FixedBatchParameters -> FixedBatchDemand -> FixedBatchState -> Either FixedBatchError (NonEmpty FixedBatchState, Natural)
reachable parameters demand initial = go 0 [initial] [initial]
  where
    go count [] visited = maybe (Right (initial :| [], count)) (\states -> Right (states, count)) (NonEmpty.nonEmpty visited)
    go count (state : pending) visited = do
        let actionCount = fixedBatchActionCountAt parameters state
            nextCount = count + actionCount
        if nextCount > fixedBatchStateActionBudget parameters
            then Left (FixedBatchStateActionBudgetExceeded nextCount)
            else Right ()
        let actions = fixedBatchActionsUnchecked parameters state
            successors =
                [ fst (transitionUnchecked parameters state action demandValue)
                | action <- actions
                , (demandValue, _) <- NonEmpty.toList (fixedBatchDemandOutcomes demand)
                ]
            unseen = filter (`notElem` visited) (unique successors)
            nextVisited = visited ++ unseen
            size = fromIntegral (length nextVisited)
        if size > fixedBatchStateBudget parameters
            then Left (FixedBatchStateBudgetExceeded size)
            else go nextCount (pending ++ unseen) nextVisited

sameFixedBatchStateSupport :: FixedBatchFixture -> FixedBatchFixture -> Bool
sameFixedBatchStateSupport left right = sameFiniteSet (fixedBatchStateLayout left) (fixedBatchStateLayout right)

sameFixedBatchStateLayout :: FixedBatchFixture -> FixedBatchFixture -> Bool
sameFixedBatchStateLayout left right = sameFiniteLayout (fixedBatchStateLayout left) (fixedBatchStateLayout right)

-- | Constant two-stage echelon reorder levels.
data ReorderLevels = ReorderLevels
    { fixedBatchR1 :: !Integer
    , fixedBatchR2 :: !Integer
    }
    deriving (Eq, Ord, Show)

newtype ReorderGrid = ReorderGrid (NonEmpty ReorderLevels)
    deriving (Eq, Show)

reorderGrid :: FixedBatchParameters -> NonEmpty ReorderLevels -> Either FixedBatchError ReorderGrid
reorderGrid parameters candidates
    | count > fixedBatchGridBudget parameters = Left (FixedBatchGridBudgetExceeded count)
    | Just duplicate <- firstDuplicate ordered = Left (FixedBatchDuplicateGridPoint duplicate)
    | otherwise = maybe (Left FixedBatchEmptyGrid) (Right . ReorderGrid) (NonEmpty.nonEmpty ordered)
  where
    ordered = sort (NonEmpty.toList candidates)
    count = fromIntegral (length ordered)

reorderGridCandidates :: ReorderGrid -> NonEmpty ReorderLevels
reorderGridCandidates (ReorderGrid candidates) = candidates

-- | Smallest nonnegative batch multiple that moves a position strictly above R.
fixedBatchOrderQuantity :: Integer -> Integer -> Natural -> Natural
fixedBatchOrderQuantity position reorderLevel batch
    | position > reorderLevel = 0
    | otherwise = batch * fromInteger (1 + (reorderLevel - position) `div` toInteger batch)

-- | Feasible policy action and explicit clipping diagnostics.
data PolicyAction = PolicyAction
    { fixedBatchPolicyPhysicalAction :: !FixedBatchAction
    , fixedBatchReleaseWasClipped :: !Bool
    , fixedBatchExternalWasClipped :: !Bool
    }
    deriving (Eq, Show)

fixedBatchPolicyAction :: FixedBatchParameters -> ReorderLevels -> FixedBatchState -> Either FixedBatchError PolicyAction
fixedBatchPolicyAction parameters levels state
    | fixedBatchPeriodsRemaining state == 0 = Left (FixedBatchTerminalAction state)
    | otherwise =
        Right
            PolicyAction
                { fixedBatchPolicyPhysicalAction = FixedBatchAction feasibleRelease feasibleExternal
                , fixedBatchReleaseWasClipped = desiredRelease > feasibleRelease
                , fixedBatchExternalWasClipped = desiredExternal > feasibleExternal
                }
  where
    ip1 = fixedBatchStage1NetInventory state
    ip2 = fixedBatchStage1NetInventory state + toInteger (fixedBatchStage2OnHand state + sum (fixedBatchPipeline state))
    desiredRelease = fixedBatchOrderQuantity ip1 (fixedBatchR1 levels) (fixedBatchQ1 parameters)
    desiredExternal = fixedBatchOrderQuantity ip2 (fixedBatchR2 levels) (fixedBatchQ2 parameters)
    available = fixedBatchStage2OnHand state + dueQuantity state
    feasibleRelease = largestMultipleAtMost (fixedBatchQ1 parameters) (min desiredRelease available)
    externalLimit = fixedBatchExternalBatchCap parameters * fixedBatchQ2 parameters
    feasibleExternal = min desiredExternal externalLimit

-- | Opaque completed finite-horizon oracle and stationary-policy-grid comparison.
data FixedBatchSolution = FixedBatchSolution
    { fixedBatchSolutionFixture :: !FixedBatchFixture
    , fixedBatchSolutionGrid :: !ReorderGrid
    , fixedBatchOracleReturn :: !Rational
    , fixedBatchPolicyReturn :: !Rational
    , fixedBatchSelectedLevels :: !ReorderLevels
    , fixedBatchOracleExternalCapSelected :: !Bool
    , fixedBatchPolicyExternalCapClipped :: !Bool
    , fixedBatchPolicyReleaseClipped :: !Bool
    , fixedBatchGridBinds :: !Bool
    , fixedBatchCheckedSolverWork :: !Natural
    }
    deriving (Show)

instance Eq FixedBatchSolution where
    left == right =
        fixedBatchFixtureParameters (fixedBatchSolutionFixture left) == fixedBatchFixtureParameters (fixedBatchSolutionFixture right)
            && fixedBatchInitialState (fixedBatchSolutionFixture left) == fixedBatchInitialState (fixedBatchSolutionFixture right)
            && fixedBatchSolutionGrid left == fixedBatchSolutionGrid right
            && observations left == observations right
      where
        observations solution =
            ( fixedBatchOracleReturn solution
            , fixedBatchPolicyReturn solution
            , fixedBatchSelectedLevels solution
            , fixedBatchOracleExternalCapSelected solution
            , fixedBatchPolicyExternalCapClipped solution
            , fixedBatchPolicyReleaseClipped solution
            , fixedBatchGridBinds solution
            , fixedBatchCheckedSolverWork solution
            )

fixedBatchPolicyRegret :: FixedBatchSolution -> Rational
fixedBatchPolicyRegret solution = fixedBatchOracleReturn solution - fixedBatchPolicyReturn solution

solveFixedBatch :: ReorderGrid -> FixedBatchFixture -> Either FixedBatchError FixedBatchSolution
solveFixedBatch grid fixture = do
    let parameters = fixedBatchFixtureParameters fixture
        demandCount = fromIntegral (length (fixedBatchDemandOutcomes (fixedBatchFixtureDemand fixture)))
        gridCount = fromIntegral (length (reorderGridCandidates grid))
        work = demandCount * (fixedBatchStateActionCount fixture + gridCount * fixedBatchStateActionCount fixture)
    if work > fixedBatchSolverWorkBudget parameters
        then Left (FixedBatchSolverWorkBudgetExceeded work)
        else Right ()
    (oracleReturn, oracleSelections) <- evaluateOracle fixture
    evaluated <- traverse (evaluateCandidate fixture) (NonEmpty.toList (reorderGridCandidates grid))
    (levels, policyReturn, _, _) <- bestPolicy evaluated
    let regret = oracleReturn - policyReturn
    if regret < 0
        then Left (FixedBatchNegativeRegret regret)
        else Right ()
    let selectedPath = policySelectedPath parameters levels (fixedBatchInitialState fixture)
        oraclePath = oracleSelectedPath parameters (fixedBatchInitialState fixture) oracleSelections
        boundary = levelsAtBoundary grid levels
    Right
        FixedBatchSolution
            { fixedBatchSolutionFixture = fixture
            , fixedBatchSolutionGrid = grid
            , fixedBatchOracleReturn = oracleReturn
            , fixedBatchPolicyReturn = policyReturn
            , fixedBatchSelectedLevels = levels
            , fixedBatchOracleExternalCapSelected =
                any ((== fixedBatchExternalBatchCap parameters * fixedBatchQ2 parameters) . fixedBatchExternalOrder . snd) oraclePath
            , fixedBatchPolicyExternalCapClipped = any (fixedBatchExternalWasClipped . snd) selectedPath
            , fixedBatchPolicyReleaseClipped = any (fixedBatchReleaseWasClipped . snd) selectedPath
            , fixedBatchGridBinds = boundary
            , fixedBatchCheckedSolverWork = work
            }

evaluateFixedBatchPolicy :: FixedBatchFixture -> ReorderLevels -> Either FixedBatchError Rational
evaluateFixedBatchPolicy fixture levels = do
    (value, _, _) <- evaluatePolicy fixture levels
    Right value

evaluateOracle :: FixedBatchFixture -> Either FixedBatchError (Rational, [(FixedBatchState, FixedBatchAction)])
evaluateOracle fixture = do
    (values, selections) <- foldM step ([], []) ordered
    value <- lookupValue (fixedBatchInitialState fixture) values
    Right (value, selections)
  where
    parameters = fixedBatchFixtureParameters fixture
    demand = NonEmpty.toList (fixedBatchDemandOutcomes (fixedBatchFixtureDemand fixture))
    ordered = sortOn fixedBatchPeriodsRemaining (NonEmpty.toList (fixedBatchReachableStates fixture))
    step (values, selections) state
        | fixedBatchPeriodsRemaining state == 0 = Right ((state, 0) : values, selections)
        | otherwise = do
            actions <- fixedBatchActions parameters state
            evaluated <- traverse (\action -> fmap (action,) (actionReturn values state action)) actions
            (action, value) <- maximumAction state evaluated
            Right ((state, value) : values, (state, action) : selections)
      where
        actionReturn priorValues currentState action = fmap sum (traverse (contribution priorValues currentState action) demand)
    contribution values state action (demandValue, mass) = do
        let (successor, cost) = transitionUnchecked parameters state action demandValue
        future <- lookupValue successor values
        Right (mass * (negate cost + future))

evaluateCandidate :: FixedBatchFixture -> ReorderLevels -> Either FixedBatchError (ReorderLevels, Rational, Bool, Bool)
evaluateCandidate fixture levels = do
    (value, releaseClip, externalClip) <- evaluatePolicy fixture levels
    Right (levels, value, releaseClip, externalClip)

evaluatePolicy :: FixedBatchFixture -> ReorderLevels -> Either FixedBatchError (Rational, Bool, Bool)
evaluatePolicy fixture levels = do
    (values, releaseClip, externalClip) <- foldM step ([], False, False) ordered
    value <- lookupValue (fixedBatchInitialState fixture) values
    Right (value, releaseClip, externalClip)
  where
    parameters = fixedBatchFixtureParameters fixture
    demand = NonEmpty.toList (fixedBatchDemandOutcomes (fixedBatchFixtureDemand fixture))
    ordered = sortOn fixedBatchPeriodsRemaining (NonEmpty.toList (fixedBatchReachableStates fixture))
    step (values, releaseSeen, externalSeen) state
        | fixedBatchPeriodsRemaining state == 0 = Right ((state, 0) : values, releaseSeen, externalSeen)
        | otherwise = do
            selected <- fixedBatchPolicyAction parameters levels state
            let action = fixedBatchPolicyPhysicalAction selected
            contributions <- traverse (contribution values state action) demand
            Right
                ( (state, sum contributions) : values
                , releaseSeen || fixedBatchReleaseWasClipped selected
                , externalSeen || fixedBatchExternalWasClipped selected
                )
    contribution values state action (demandValue, mass) = do
        let (successor, cost) = transitionUnchecked parameters state action demandValue
        future <- lookupValue successor values
        Right (mass * (negate cost + future))

maximumAction :: FixedBatchState -> [(FixedBatchAction, Rational)] -> Either FixedBatchError (FixedBatchAction, Rational)
maximumAction state [] = Left (FixedBatchNoAction state)
maximumAction _ (first : remaining) = Right (foldl' choose first remaining)
  where
    choose selected candidate
        | snd candidate > snd selected = candidate
        | otherwise = selected

bestPolicy :: [(ReorderLevels, Rational, Bool, Bool)] -> Either FixedBatchError (ReorderLevels, Rational, Bool, Bool)
bestPolicy [] = Left FixedBatchEmptyGrid
bestPolicy (first : remaining) = Right (foldl' choose first remaining)
  where
    choose selected candidate
        | policyValue candidate > policyValue selected = candidate
        | otherwise = selected
    policyValue (_, value, _, _) = value

lookupValue :: FixedBatchState -> [(FixedBatchState, Rational)] -> Either FixedBatchError Rational
lookupValue state values = maybe (Left (FixedBatchMissingSuccessor state)) Right (lookup state values)

policySelectedPath :: FixedBatchParameters -> ReorderLevels -> FixedBatchState -> [(FixedBatchState, PolicyAction)]
policySelectedPath parameters levels initial = go [initial] [] []
  where
    demandValues = fmap fst (NonEmpty.toList (fixedBatchDemandOutcomes (conditionedGeometricDemand (fixedBatchDemandCap parameters))))
    go [] _ selected = selected
    go (state : pending) visited selected
        | state `elem` visited = go pending visited selected
        | fixedBatchPeriodsRemaining state == 0 = go pending (state : visited) selected
        | otherwise =
            case fixedBatchPolicyAction parameters levels state of
                Left _ -> selected
                Right policyAction ->
                    let action = fixedBatchPolicyPhysicalAction policyAction
                        successors = unique [fst (transitionUnchecked parameters state action demandValue) | demandValue <- demandValues]
                     in go (pending ++ successors) (state : visited) ((state, policyAction) : selected)

oracleSelectedPath :: FixedBatchParameters -> FixedBatchState -> [(FixedBatchState, FixedBatchAction)] -> [(FixedBatchState, FixedBatchAction)]
oracleSelectedPath parameters initial selections = go [initial] [] []
  where
    demandValues = fmap fst (NonEmpty.toList (fixedBatchDemandOutcomes (conditionedGeometricDemand (fixedBatchDemandCap parameters))))
    go [] _ selected = selected
    go (state : pending) visited selected
        | state `elem` visited = go pending visited selected
        | fixedBatchPeriodsRemaining state == 0 = go pending (state : visited) selected
        | otherwise = case lookup state selections of
            Nothing -> selected
            Just action ->
                let successors = unique [fst (transitionUnchecked parameters state action demandValue) | demandValue <- demandValues]
                 in go (pending ++ successors) (state : visited) ((state, action) : selected)

levelsAtBoundary :: ReorderGrid -> ReorderLevels -> Bool
levelsAtBoundary grid levels =
    fixedBatchR1 levels == minimum r1s
        || fixedBatchR1 levels == maximum r1s
        || fixedBatchR2 levels == minimum r2s
        || fixedBatchR2 levels == maximum r2s
  where
    candidates = NonEmpty.toList (reorderGridCandidates grid)
    r1s = fmap fixedBatchR1 candidates
    r2s = fmap fixedBatchR2 candidates

-- | Checked action/grid widening evidence.
data FixedBatchStability = FixedBatchStability
    { fixedBatchBoundsStable :: !Bool
    , fixedBatchOracleReturnChanged :: !Bool
    , fixedBatchPolicyReturnChanged :: !Bool
    , fixedBatchSelectedLevelsChanged :: !Bool
    , fixedBatchWidenedExternalCapSelected :: !Bool
    , fixedBatchWidenedGridBinds :: !Bool
    }
    deriving (Eq, Show)

compareFixedBatchBounds :: FixedBatchSolution -> FixedBatchSolution -> Either FixedBatchError FixedBatchStability
compareFixedBatchBounds primary widened = do
    validateSameModelExceptBounds primary widened
    let primaryParameters = fixedBatchFixtureParameters (fixedBatchSolutionFixture primary)
        widenedParameters = fixedBatchFixtureParameters (fixedBatchSolutionFixture widened)
    if fixedBatchExternalBatchCap widenedParameters <= fixedBatchExternalBatchCap primaryParameters
        then Left FixedBatchBoundsNotWidened
        else Right ()
    let widenedCandidates = NonEmpty.toList (reorderGridCandidates (fixedBatchSolutionGrid widened))
    case filter (`notElem` widenedCandidates) (NonEmpty.toList (reorderGridCandidates (fixedBatchSolutionGrid primary))) of
        missing : _ -> Left (FixedBatchGridNotSuperset missing)
        [] -> Right ()
    if length widenedCandidates <= length (reorderGridCandidates (fixedBatchSolutionGrid primary))
        then Left FixedBatchBoundsNotWidened
        else Right ()
    let oracleChanged = fixedBatchOracleReturn primary /= fixedBatchOracleReturn widened
        policyChanged = fixedBatchPolicyReturn primary /= fixedBatchPolicyReturn widened
        levelsChanged = fixedBatchSelectedLevels primary /= fixedBatchSelectedLevels widened
        capSelected = fixedBatchOracleExternalCapSelected widened || fixedBatchPolicyExternalCapClipped widened
        gridBinds = fixedBatchGridBinds widened
    Right
        FixedBatchStability
            { fixedBatchBoundsStable = not oracleChanged && not policyChanged && not levelsChanged && not capSelected && not gridBinds
            , fixedBatchOracleReturnChanged = oracleChanged
            , fixedBatchPolicyReturnChanged = policyChanged
            , fixedBatchSelectedLevelsChanged = levelsChanged
            , fixedBatchWidenedExternalCapSelected = capSelected
            , fixedBatchWidenedGridBinds = gridBinds
            }

validateSameModelExceptBounds :: FixedBatchSolution -> FixedBatchSolution -> Either FixedBatchError ()
validateSameModelExceptBounds left right
    | fixedBatchInitialState leftFixture /= fixedBatchInitialState rightFixture = Left (FixedBatchModelMismatch "initial state")
    | modelCoefficients leftParameters /= modelCoefficients rightParameters = Left (FixedBatchModelMismatch "horizon, lead time, batches, demand, costs, or computation budgets")
    | otherwise = Right ()
  where
    leftFixture = fixedBatchSolutionFixture left
    rightFixture = fixedBatchSolutionFixture right
    leftParameters = fixedBatchFixtureParameters leftFixture
    rightParameters = fixedBatchFixtureParameters rightFixture

modelCoefficients :: FixedBatchParameters -> (Natural, Natural, Natural, Natural, Rational, Rational, Rational, Natural, Natural, Natural, Natural, Natural)
modelCoefficients parameters =
    ( fixedBatchHorizon parameters
    , fixedBatchSupplierLeadTime parameters
    , fixedBatchQ1 parameters
    , fixedBatchQ2 parameters
    , fixedBatchStage1HoldingCost parameters
    , fixedBatchStage2HoldingCost parameters
    , fixedBatchBacklogCost parameters
    , fixedBatchDemandCap parameters
    , fixedBatchStateBudget parameters
    , fixedBatchStateActionBudget parameters
    , fixedBatchSolverWorkBudget parameters
    , fixedBatchConvolutionBudget parameters
    )

-- | Exact finite-demand-cap delta. It is not an unbounded-model error bound.
data FixedBatchDemandDiagnostic = FixedBatchDemandDiagnostic
    { fixedBatchDemandOracleDelta :: !Rational
    , fixedBatchDemandPolicyDelta :: !Rational
    , fixedBatchDemandRegretDelta :: !Rational
    , fixedBatchDemandSelectedLevelsChanged :: !Bool
    , fixedBatchDemandPrimaryOmittedMass :: !Rational
    , fixedBatchDemandWidenedOmittedMass :: !Rational
    }
    deriving (Eq, Show)

compareFixedBatchDemandCaps :: FixedBatchSolution -> FixedBatchSolution -> Either FixedBatchError FixedBatchDemandDiagnostic
compareFixedBatchDemandCaps primary widened = do
    let primaryParameters = fixedBatchFixtureParameters (fixedBatchSolutionFixture primary)
        widenedParameters = fixedBatchFixtureParameters (fixedBatchSolutionFixture widened)
        primaryCap = fixedBatchDemandCap primaryParameters
        widenedCap = fixedBatchDemandCap widenedParameters
    if demandModelCoefficients primaryParameters /= demandModelCoefficients widenedParameters
        then Left (FixedBatchModelMismatch "demand-cap comparison parameters")
        else Right ()
    if fixedBatchInitialState (fixedBatchSolutionFixture primary) /= fixedBatchInitialState (fixedBatchSolutionFixture widened)
        then Left (FixedBatchModelMismatch "demand-cap comparison initial state")
        else Right ()
    if fixedBatchSolutionGrid primary /= fixedBatchSolutionGrid widened
        then Left (FixedBatchModelMismatch "demand-cap comparison grid")
        else Right ()
    if widenedCap <= primaryCap
        then Left (FixedBatchDemandCapNotIncreased primaryCap widenedCap)
        else Right ()
    Right
        FixedBatchDemandDiagnostic
            { fixedBatchDemandOracleDelta = fixedBatchOracleReturn widened - fixedBatchOracleReturn primary
            , fixedBatchDemandPolicyDelta = fixedBatchPolicyReturn widened - fixedBatchPolicyReturn primary
            , fixedBatchDemandRegretDelta = fixedBatchPolicyRegret widened - fixedBatchPolicyRegret primary
            , fixedBatchDemandSelectedLevelsChanged = fixedBatchSelectedLevels primary /= fixedBatchSelectedLevels widened
            , fixedBatchDemandPrimaryOmittedMass = fixedBatchOmittedMass (fixedBatchFixtureDemand (fixedBatchSolutionFixture primary))
            , fixedBatchDemandWidenedOmittedMass = fixedBatchOmittedMass (fixedBatchFixtureDemand (fixedBatchSolutionFixture widened))
            }

demandModelCoefficients :: FixedBatchParameters -> (Natural, Natural, Natural, Natural, Natural, Rational, Rational, Rational, Natural, Natural, Natural, Natural, Natural)
demandModelCoefficients parameters =
    ( fixedBatchHorizon parameters
    , fixedBatchSupplierLeadTime parameters
    , fixedBatchQ1 parameters
    , fixedBatchQ2 parameters
    , fixedBatchExternalBatchCap parameters
    , fixedBatchStage1HoldingCost parameters
    , fixedBatchStage2HoldingCost parameters
    , fixedBatchBacklogCost parameters
    , fixedBatchStateBudget parameters
    , fixedBatchStateActionBudget parameters
    , fixedBatchSolverWorkBudget parameters
    , fixedBatchGridBudget parameters
    , fixedBatchConvolutionBudget parameters
    )

-- | Maximum accepted external lead time before any pipeline allocation.
fixedBatchMaximumLeadTime :: Natural
fixedBatchMaximumLeadTime = 32

-- | Maximum conditioned demand support cardinality before support allocation.
fixedBatchMaximumDemandOutcomes :: Natural
fixedBatchMaximumDemandOutcomes = 1024

multiplesThrough :: Natural -> Natural -> [Natural]
multiplesThrough batch limit = fmap (* batch) [0 .. limit `div` batch]

largestMultipleAtMost :: Natural -> Natural -> Natural
largestMultipleAtMost batch value = batch * (value `div` batch)

firstNotMultiple :: Natural -> [Natural] -> Maybe Natural
firstNotMultiple _ [] = Nothing
firstNotMultiple divisor (value : values)
    | value `mod` divisor /= 0 = Just value
    | otherwise = firstNotMultiple divisor values

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate (left : right : remaining)
    | left == right = Just right
    | otherwise = firstDuplicate (right : remaining)
firstDuplicate _ = Nothing

unique :: (Ord value) => [value] -> [value]
unique = removeDuplicates . sortOn id
  where
    removeDuplicates [] = []
    removeDuplicates (first : remaining) = first : go first remaining
    go _ [] = []
    go previous (value : values)
        | previous == value = go previous values
        | otherwise = value : go value values

mapLeft :: (error -> other) -> Either error value -> Either other value
mapLeft wrap = either (Left . wrap) Right
