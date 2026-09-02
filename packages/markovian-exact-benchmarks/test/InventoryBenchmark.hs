module InventoryBenchmark (runInventoryBenchmarkTests) where

import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio ((%))
import Markovian.Action (ActionId, actionId, actionValue)
import Markovian.Benchmark.Inventory.Report (
    inventoryBenchmarkReport,
    inventoryBenchmarkSampleCount,
    inventoryBenchmarkWarmupCount,
    renderInventoryBenchmarkReport,
 )
import Markovian.Benchmark.Inventory.Serial.Exact (
    BaseStockTargetGrid,
    SerialInventoryAction (..),
    SerialInventoryError (..),
    SerialInventoryFixture,
    SerialInventoryParameterError (..),
    SerialInventoryParameterInput (..),
    SerialInventoryParameters,
    SerialInventorySolution,
    SerialInventoryState (..),
    baseStockAction,
    baseStockTargetGrid,
    boundedDemandOutcomes,
    compareSerialInventoryBounds,
    horizonExceededMass,
    omittedDemandMass,
    periodBaseStockTargets,
    retainedDemandMass,
    serialInventoryBaseStockReturn,
    serialInventoryBoundsStable,
    serialInventoryFixture,
    serialInventoryInitialValueError,
    serialInventoryMDP,
    serialInventoryOracleReturn,
    serialInventoryOrderCap,
    serialInventoryOrderCapBinds,
    serialInventoryPolicyRegret,
    serialInventorySelectedTargets,
    serialInventoryTargetGridBinds,
    serialReachableStates,
    solveSerialInventory,
    truncatedGeometricDemand,
    validateSerialInventoryParameters,
 )
import Markovian.Compile.Exact (compileExactMDP)
import Markovian.Horizon (mkHorizon)
import Markovian.Interpreter.Control.Exact (
    ExactPolicyIterationConfig,
    ExactPolicyIterationReport (..),
    ExactPolicyIterationStopReason (..),
    exactPolicyIterationConfig,
    solveCompiledExactPolicyIteration,
 )
import Markovian.MDP.Exact (
    ExactDecision (..),
    ExactMDP,
    ExactModelError (..),
    exactSuccessorState,
    exactTransitionReward,
    inspectExactMDP,
    stepExactMDP,
 )
import Markovian.Objective.Exact (
    ExactContractionDiscount,
    ExactDiscount,
    exactDiscountValue,
    mkExactContractionDiscount,
    mkExactDiscount,
 )
import Markovian.Probability.Exact (exactOutcomes, exactProbability)
import Markovian.Reward.Exact (exactRewardValue)
import Numeric.Natural (Natural)
import Utf8Golden (readUtf8File)

runInventoryBenchmarkTests :: (String -> IO () -> IO ()) -> IO ()
runInventoryBenchmarkTests run = do
    run "serial inventory timing and one-step cost" testTimingAndCost
    run "serial inventory internal-shipment conservation" testShipmentConservation
    run "serial inventory supplier-order delay" testSupplierOrderDelay
    run "serial inventory physical action feasibility" testPhysicalFeasibility
    run "serial inventory exact demand truncation" testDemandTruncation
    run "serial inventory reachable closure and terminal timing" testReachableClosureAndTerminal
    run "serial inventory independent exact oracle" testIndependentOracle
    run "serial inventory base-stock feasibility and regret" testBaseStockFeasibilityAndRegret
    run "serial inventory known target selection" testKnownTargetSelection
    run "serial inventory target-set order invariance" testTargetSetOrderInvariance
    run "serial inventory support-order invariance" testSupportOrderInvariance
    run "serial inventory widened-bound diagnostics" testWidenedBounds
    run "serial inventory rejects unrelated bound evidence" testUnrelatedBounds
    run "serial inventory checked-constructor rejections" testCheckedRejections
    run "serial inventory unit finite-horizon discount" testUnitDiscount
    run "inventory benchmark deterministic report" testDeterministicReport
    run "inventory benchmark configuration smoke" testBenchmarkConfiguration

assert :: String -> Bool -> IO ()
assert message condition = if condition then pure () else fail message

requireRight :: (Show error) => String -> Either error value -> IO value
requireRight _ (Right value) = pure value
requireRight label (Left err) = fail (label ++ ": " ++ show err)

mustRight :: (Show error) => Either error value -> value
mustRight (Right value) = value
mustRight (Left err) = error (show err)

smallDiscount :: ExactDiscount
smallDiscount = mustRight (mkExactDiscount (1 % 2))

smallContractionDiscount :: ExactContractionDiscount
smallContractionDiscount = mustRight (mkExactContractionDiscount (1 % 2))

smallParameters :: SerialInventoryParameters
smallParameters =
    mustRight
        ( validateSerialInventoryParameters
            SerialInventoryParameterInput
                { inputSerialInventoryHorizon = 2
                , inputSerialInventoryDiscount = smallDiscount
                , inputSerialInventoryOrderCap = 1
                , inputSerialInventoryDemandCap = 1
                , inputSerialInventoryUpstreamHoldingCost = 1
                , inputSerialInventoryDownstreamHoldingCost = 1
                , inputSerialInventoryBacklogCost = 4
                }
        )

smallInitial :: SerialInventoryState
smallInitial =
    SerialInventoryState
        { periodsRemaining = 2
        , upstreamOnHand = 1
        , supplierOrderDue = 0
        , downstreamNetInventory = 0
        }

smallFixture :: SerialInventoryFixture
smallFixture = mustRight (serialInventoryFixture smallParameters smallInitial)

primaryGrid :: BaseStockTargetGrid
primaryGrid = targetGrid 2 2

widenedParameters :: SerialInventoryParameters
widenedParameters =
    mustRight
        ( validateSerialInventoryParameters
            SerialInventoryParameterInput
                { inputSerialInventoryHorizon = 2
                , inputSerialInventoryDiscount = smallDiscount
                , inputSerialInventoryOrderCap = 2
                , inputSerialInventoryDemandCap = 1
                , inputSerialInventoryUpstreamHoldingCost = 1
                , inputSerialInventoryDownstreamHoldingCost = 1
                , inputSerialInventoryBacklogCost = 4
                }
        )

widenedFixture :: SerialInventoryFixture
widenedFixture = mustRight (serialInventoryFixture widenedParameters smallInitial)

widenedGrid :: BaseStockTargetGrid
widenedGrid = targetGrid 2 3

solverConfig :: ExactPolicyIterationConfig
solverConfig = exactPolicyIterationConfig smallContractionDiscount (mustRight (mkHorizon 8))

primarySolution :: SerialInventorySolution
primarySolution = mustRight (solveSerialInventory primaryGrid smallFixture)

widenedSolution :: SerialInventorySolution
widenedSolution = mustRight (solveSerialInventory widenedGrid widenedFixture)

targetGrid :: Natural -> Natural -> BaseStockTargetGrid
targetGrid horizon maximumTarget =
    mustRight (baseStockTargetGrid (candidatePairs :| replicate (fromIntegral horizon - 1) candidatePairs))
  where
    candidatePairs =
        case NonEmpty.nonEmpty [(upstream, downstream) | upstream <- [0 .. maximumTarget], downstream <- [0 .. maximumTarget]] of
            Nothing -> (0, 0) :| []
            Just pairs -> pairs

testTimingAndCost :: IO ()
testTimingAndCost = do
    parameters <-
        requireRight
            "timing parameters"
            ( validateSerialInventoryParameters
                SerialInventoryParameterInput
                    { inputSerialInventoryHorizon = 1
                    , inputSerialInventoryDiscount = smallDiscount
                    , inputSerialInventoryOrderCap = 2
                    , inputSerialInventoryDemandCap = 0
                    , inputSerialInventoryUpstreamHoldingCost = 1
                    , inputSerialInventoryDownstreamHoldingCost = 2
                    , inputSerialInventoryBacklogCost = 5
                    }
            )
    let initial = SerialInventoryState 1 3 1 (-1)
        selected = actionId (SerialInventoryAction 2 2)
    fixture <- requireRight "timing fixture" (serialInventoryFixture parameters initial)
    transition <- requireRight "timing transition" (stepExactMDP (serialInventoryMDP fixture) initial selected)
    case NonEmpty.toList (exactOutcomes transition) of
        [(outcome, mass)] -> do
            assert "timing transition was not certain" (exactProbability mass == 1)
            assert "one-step cost changed" (exactRewardValue (exactTransitionReward outcome) == -5)
            assert
                "event timing changed successor fields"
                (exactSuccessorState outcome == SerialInventoryState 0 2 2 1)
        outcomes -> fail ("unexpected timing support: " ++ show outcomes)

testShipmentConservation :: IO ()
testShipmentConservation = do
    let model = serialInventoryMDP smallFixture
    mapM_ (checkState model) (NonEmpty.toList (serialReachableStates smallFixture))
  where
    checkState model state
        | periodsRemaining state == 0 = pure ()
        | otherwise = do
            decision <- requireRight "conservation inspection" (inspectExactMDP model state)
            case decision of
                ExactTerminalDecision _ -> fail "continuing state became terminal"
                ExactActionDecision actions -> mapM_ (checkAction model state) (NonEmpty.toList actions)
    checkAction model state selected = do
        transition <- requireRight "conservation transition" (stepExactMDP model state selected)
        mapM_
            ( \(outcome, _) ->
                let successor = exactSuccessorState outcome
                    demand =
                        downstreamNetInventory state
                            + toInteger (downstreamShipment (actionValue selected))
                            - downstreamNetInventory successor
                    before = toInteger (upstreamOnHand state + supplierOrderDue state) + downstreamNetInventory state
                    after = toInteger (upstreamOnHand successor) + downstreamNetInventory successor
                 in assert "internal shipment created inventory" (after == before - demand)
            )
            (NonEmpty.toList (exactOutcomes transition))

testSupplierOrderDelay :: IO ()
testSupplierOrderDelay = do
    parameters <-
        requireRight
            "delay parameters"
            ( validateSerialInventoryParameters
                SerialInventoryParameterInput
                    { inputSerialInventoryHorizon = 2
                    , inputSerialInventoryDiscount = smallDiscount
                    , inputSerialInventoryOrderCap = 1
                    , inputSerialInventoryDemandCap = 0
                    , inputSerialInventoryUpstreamHoldingCost = 0
                    , inputSerialInventoryDownstreamHoldingCost = 0
                    , inputSerialInventoryBacklogCost = 1
                    }
            )
    let initial = SerialInventoryState 2 0 0 0
    fixture <- requireRight "delay fixture" (serialInventoryFixture parameters initial)
    firstTransition <-
        requireRight
            "first delayed transition"
            (stepExactMDP (serialInventoryMDP fixture) initial (actionId (SerialInventoryAction 1 0)))
    let firstSuccessor = exactSuccessorState (fst (NonEmpty.head (exactOutcomes firstTransition)))
    assert "supplier order arrived in the order period" (upstreamOnHand firstSuccessor == 0)
    assert "supplier order was not recorded as due" (supplierOrderDue firstSuccessor == 1)
    decision <- requireRight "delayed availability" (inspectExactMDP (serialInventoryMDP fixture) firstSuccessor)
    case decision of
        ExactTerminalDecision _ -> fail "delayed successor became terminal early"
        ExactActionDecision actions ->
            assert
                "due supplier order was not shippable next period"
                (actionId (SerialInventoryAction 0 1) `elem` actions)

testPhysicalFeasibility :: IO ()
testPhysicalFeasibility = do
    let model = serialInventoryMDP smallFixture
    mapM_
        ( \state ->
            if periodsRemaining state == 0
                then pure ()
                else do
                    decision <- requireRight "feasibility inspection" (inspectExactMDP model state)
                    case decision of
                        ExactTerminalDecision _ -> fail "continuing state became terminal"
                        ExactActionDecision actions ->
                            mapM_
                                ( \selected -> do
                                    let action = actionValue selected
                                    assert "order exceeds cap" (supplierOrder action <= serialInventoryOrderCap smallParameters)
                                    assert
                                        "shipment exceeds available upstream inventory"
                                        (downstreamShipment action <= upstreamOnHand state + supplierOrderDue state)
                                )
                                (NonEmpty.toList actions)
        )
        (NonEmpty.toList (serialReachableStates smallFixture))
    case stepExactMDP model smallInitial (actionId (SerialInventoryAction 0 2)) of
        Left (ExactUnavailableAction _) -> pure ()
        result -> fail ("infeasible action was not rejected: " ++ show result)

testDemandTruncation :: IO ()
testDemandTruncation = do
    let demand = truncatedGeometricDemand 2
    assert "retained geometric mass changed" (retainedDemandMass demand == 7 % 8)
    assert "omitted geometric tail changed" (omittedDemandMass demand == 1 % 8)
    assert "bounded demand did not normalize" (sum (fmap snd (boundedDemandOutcomes demand)) == 1)
    assert
        "bounded geometric probabilities changed"
        (boundedDemandOutcomes demand == [(0, 4 % 7), (1, 2 % 7), (2, 1 % 7)])
    assert "horizon truncation calculation changed" (horizonExceededMass 2 demand == 15 % 64)

testReachableClosureAndTerminal :: IO ()
testReachableClosureAndTerminal = do
    let model = serialInventoryMDP smallFixture
        support = NonEmpty.toList (serialReachableStates smallFixture)
    mapM_
        ( \state ->
            if periodsRemaining state == 0
                then do
                    decision <- requireRight "terminal inspection" (inspectExactMDP model state)
                    case decision of
                        ExactTerminalDecision payoff -> assert "terminal payoff was not zero" (exactRewardValue payoff == 0)
                        ExactActionDecision _ -> fail "terminal state exposed actions"
                    case stepExactMDP model state (actionId (SerialInventoryAction 0 0)) of
                        Left (ExactActionRequestedAtTerminal payoff) ->
                            assert "terminal action error changed payoff" (exactRewardValue payoff == 0)
                        result -> fail ("terminal state accepted an action: " ++ show result)
                else do
                    decision <- requireRight "closure inspection" (inspectExactMDP model state)
                    case decision of
                        ExactTerminalDecision _ -> fail "reachable continuing state became terminal"
                        ExactActionDecision actions ->
                            mapM_
                                ( \selected -> do
                                    transition <- requireRight "closure transition" (stepExactMDP model state selected)
                                    mapM_
                                        ( \(outcome, _) ->
                                            assert
                                                ("successor missing without clamping: " ++ show (exactSuccessorState outcome))
                                                (exactSuccessorState outcome `elem` support)
                                        )
                                        (NonEmpty.toList (exactOutcomes transition))
                                )
                                (NonEmpty.toList actions)
        )
        support
    assert "bounded support unexpectedly clamped backlog" (any ((== -2) . downstreamNetInventory) support)

testIndependentOracle :: IO ()
testIndependentOracle = do
    independent <- independentMaximum (serialInventoryMDP smallFixture) smallDiscount smallInitial
    assert
        "policy iteration disagreed with independent finite-horizon maximizer"
        (serialInventoryOracleReturn primarySolution == independent)

testBaseStockFeasibilityAndRegret :: IO ()
testBaseStockFeasibilityAndRegret = do
    let model = serialInventoryMDP smallFixture
    mapM_
        ( \state ->
            if periodsRemaining state == 0
                then pure ()
                else do
                    selected <- requireRight "base-stock action" (baseStockAction smallParameters (serialInventorySelectedTargets primarySolution) state)
                    decision <- requireRight "base-stock feasibility inspection" (inspectExactMDP model state)
                    case decision of
                        ExactTerminalDecision _ -> fail "base-stock continuing state became terminal"
                        ExactActionDecision actions -> assert "clipped base-stock action is infeasible" (actionId selected `elem` actions)
        )
        (NonEmpty.toList (serialReachableStates smallFixture))
    assert
        "oracle return is below base-stock return"
        (serialInventoryOracleReturn primarySolution >= serialInventoryBaseStockReturn primarySolution)
    assert "base-stock regret is negative" (serialInventoryPolicyRegret primarySolution >= 0)
    assert
        "value error and regret diverged"
        (serialInventoryInitialValueError primarySolution == serialInventoryPolicyRegret primarySolution)

testKnownTargetSelection :: IO ()
testKnownTargetSelection =
    assert
        ("small fixture selected unexpected targets: " ++ show (serialInventorySelectedTargets primarySolution))
        (serialInventorySelectedTargets primarySolution == periodBaseStockTargets ((0, 0) :| [(0, 1)]))

testTargetSetOrderInvariance :: IO ()
testTargetSetOrderInvariance = do
    let candidates =
            (0, 0)
                :| [ (upstream, downstream)
                   | upstream <- [0 .. 2]
                   , downstream <- [0 .. 2]
                   , (upstream, downstream) /= (0, 0)
                   ]
        reversed = NonEmpty.reverse candidates
    reorderedGrid <- requireRight "reordered target set" (baseStockTargetGrid (reversed :| [reversed]))
    reordered <- requireRight "reordered target solve" (solveSerialInventory reorderedGrid smallFixture)
    assert "candidate order changed selected exact tie" (serialInventorySelectedTargets reordered == serialInventorySelectedTargets primarySolution)
    assert "candidate order changed base-stock return" (serialInventoryBaseStockReturn reordered == serialInventoryBaseStockReturn primarySolution)

testSupportOrderInvariance :: IO ()
testSupportOrderInvariance = do
    let model = serialInventoryMDP smallFixture
        states = NonEmpty.toList (serialReachableStates smallFixture)
    actions <- allActions model states
    leftCompiled <- requireRight "forward inventory compilation" (compileExactMDP states actions model)
    rightCompiled <- requireRight "reversed inventory compilation" (compileExactMDP (reverse states) (reverse actions) model)
    leftReport <- requireRight "forward inventory solve" (solveCompiledExactPolicyIteration solverConfig leftCompiled)
    rightReport <- requireRight "reversed inventory solve" (solveCompiledExactPolicyIteration solverConfig rightCompiled)
    assert "forward support solve was not stable" (exactPolicyIterationStopReason leftReport == ExactPolicyIterationStable)
    assert "reversed support solve was not stable" (exactPolicyIterationStopReason rightReport == ExactPolicyIterationStable)
    assert
        "state-support order changed the exact initial return"
        (exactPolicyIterationInitialValue leftReport == exactPolicyIterationInitialValue rightReport)

testWidenedBounds :: IO ()
testWidenedBounds = do
    stability <- requireRight "widened bound comparison" (compareSerialInventoryBounds primarySolution widenedSolution)
    assert ("widened fixture was not stable: " ++ show stability) (serialInventoryBoundsStable stability)
    assert "primary target grid unexpectedly binds" (not (serialInventoryTargetGridBinds primarySolution))
    assert "widened target grid unexpectedly binds" (not (serialInventoryTargetGridBinds widenedSolution))
    assert "widened order cap unexpectedly binds" (not (serialInventoryOrderCapBinds widenedSolution))

testUnrelatedBounds :: IO ()
testUnrelatedBounds = do
    unrelatedFixture <-
        requireRight
            "unrelated fixture"
            (serialInventoryFixture widenedParameters smallInitial{upstreamOnHand = 0})
    unrelated <- requireRight "unrelated solve" (solveSerialInventory widenedGrid unrelatedFixture)
    case compareSerialInventoryBounds primarySolution unrelated of
        Left (SerialInventoryInitialStateComparisonMismatch expected actual) -> do
            assert "comparison reported the wrong primary initial state" (expected == smallInitial)
            assert "comparison reported the wrong unrelated initial state" (actual == smallInitial{upstreamOnHand = 0})
        result -> fail ("unrelated initial state produced bound evidence: " ++ show result)
    let withoutOrigin =
            (0, 1)
                :| [ (upstream, downstream)
                   | upstream <- [0 .. 3]
                   , downstream <- [0 .. 3]
                   , (upstream, downstream) /= (0, 0)
                   , (upstream, downstream) /= (0, 1)
                   ]
    nonSupersetGrid <- requireRight "non-superset grid" (baseStockTargetGrid (withoutOrigin :| [withoutOrigin]))
    nonSuperset <- requireRight "non-superset solve" (solveSerialInventory nonSupersetGrid widenedFixture)
    case compareSerialInventoryBounds primarySolution nonSuperset of
        Left (SerialInventoryTargetGridNotSuperset 1 (0, 0)) -> pure ()
        result -> fail ("non-superset grid produced bound evidence: " ++ show result)

testCheckedRejections :: IO ()
testCheckedRejections = do
    let validInput =
            SerialInventoryParameterInput
                { inputSerialInventoryHorizon = 1
                , inputSerialInventoryDiscount = smallDiscount
                , inputSerialInventoryOrderCap = 0
                , inputSerialInventoryDemandCap = 0
                , inputSerialInventoryUpstreamHoldingCost = 0
                , inputSerialInventoryDownstreamHoldingCost = 0
                , inputSerialInventoryBacklogCost = 0
                }
        expectParameterError label expected input =
            case validateSerialInventoryParameters input of
                Left actual | actual == expected -> pure ()
                result -> fail (label ++ ": " ++ show result)
    expectParameterError "horizon rejection" (SerialInventoryNonPositiveHorizon 0) validInput{inputSerialInventoryHorizon = 0}
    expectParameterError "order-cap rejection" (SerialInventoryNegativeOrderCap (-1)) validInput{inputSerialInventoryOrderCap = -1}
    expectParameterError "demand-cap rejection" (SerialInventoryNegativeDemandCap (-1)) validInput{inputSerialInventoryDemandCap = -1}
    expectParameterError "upstream-cost rejection" (SerialInventoryNegativeUpstreamHoldingCost (-1)) validInput{inputSerialInventoryUpstreamHoldingCost = -1}
    expectParameterError "downstream-cost rejection" (SerialInventoryNegativeDownstreamHoldingCost (-1)) validInput{inputSerialInventoryDownstreamHoldingCost = -1}
    expectParameterError "backlog-cost rejection" (SerialInventoryNegativeBacklogCost (-1)) validInput{inputSerialInventoryBacklogCost = -1}
    parameters <- requireRight "checked rejection parameters" (validateSerialInventoryParameters validInput)
    case serialInventoryFixture parameters (SerialInventoryState 2 0 0 0) of
        Left (SerialInventoryInitialHorizonMismatch 1 2) -> pure ()
        result -> fail ("initial-horizon mismatch was accepted: " ++ show result)
    let onePeriodTargets = periodBaseStockTargets ((0, 0) :| [])
    case baseStockAction parameters onePeriodTargets (SerialInventoryState 0 0 0 0) of
        Left SerialInventoryActionRequestedAtTerminal -> pure ()
        result -> fail ("terminal base-stock action was accepted: " ++ show result)
    case baseStockAction parameters onePeriodTargets (SerialInventoryState 2 0 0 0) of
        Left (SerialInventoryTargetPeriodMismatch 2 1) -> pure ()
        result -> fail ("missing target period was accepted: " ++ show result)
    case baseStockTargetGrid (((0, 0) :| [(0, 0)]) :| []) of
        Left (SerialInventoryDuplicateTargetCandidate 1 (0, 0)) -> pure ()
        result -> fail ("duplicate target was accepted: " ++ show result)
    fixture <- requireRight "checked rejection fixture" (serialInventoryFixture parameters (SerialInventoryState 1 0 0 0))
    stateBudgetParameters <-
        requireRight
            "state-budget parameters"
            ( validateSerialInventoryParameters
                validInput
                    { inputSerialInventoryOrderCap = 100
                    , inputSerialInventoryDemandCap = 0
                    }
            )
    case serialInventoryFixture stateBudgetParameters (SerialInventoryState 1 50 0 0) of
        Left (SerialInventoryStateBudgetExceeded count) -> assert "state budget stopped too early" (count > 5000)
        result -> fail ("oversized reachable support was accepted: " ++ show result)
    wrongHorizonGrid <- requireRight "wrong-horizon grid" (baseStockTargetGrid (((0, 0) :| []) :| [(0, 0) :| []]))
    case solveSerialInventory wrongHorizonGrid fixture of
        Left (SerialInventoryTargetPeriodMismatch 1 2) -> pure ()
        result -> fail ("target-period mismatch was accepted: " ++ show result)
    budgetParameters <-
        requireRight
            "budget parameters"
            (validateSerialInventoryParameters validInput{inputSerialInventoryHorizon = 5})
    budgetFixture <- requireRight "budget fixture" (serialInventoryFixture budgetParameters (SerialInventoryState 5 0 0 0))
    let eleven = (0, 0) :| [(target, 0) | target <- [1 .. 10]]
    budgetGrid <- requireRight "budget grid" (baseStockTargetGrid (eleven :| replicate 4 eleven))
    case solveSerialInventory budgetGrid budgetFixture of
        Left (SerialInventoryPolicyGridBudgetExceeded count) -> assert "wrong rejected schedule count" (count == 161051)
        result -> fail ("oversized target schedule grid was accepted: " ++ show result)
    case compareSerialInventoryBounds primarySolution primarySolution of
        Left (SerialInventoryOrderCapNotIncreased 1 1) -> pure ()
        result -> fail ("non-widened comparison was accepted: " ++ show result)

testUnitDiscount :: IO ()
testUnitDiscount = do
    unitDiscount <- requireRight "unit discount" (mkExactDiscount 1)
    parameters <-
        requireRight
            "unit-discount parameters"
            ( validateSerialInventoryParameters
                SerialInventoryParameterInput
                    { inputSerialInventoryHorizon = 1
                    , inputSerialInventoryDiscount = unitDiscount
                    , inputSerialInventoryOrderCap = 0
                    , inputSerialInventoryDemandCap = 0
                    , inputSerialInventoryUpstreamHoldingCost = 0
                    , inputSerialInventoryDownstreamHoldingCost = 0
                    , inputSerialInventoryBacklogCost = 1
                    }
            )
    fixture <- requireRight "unit-discount fixture" (serialInventoryFixture parameters (SerialInventoryState 1 0 0 0))
    solution <- requireRight "unit-discount solve" (solveSerialInventory (targetGrid 1 1) fixture)
    assert "unit-discount finite solve failed" (serialInventoryOracleReturn solution == 0)

testDeterministicReport :: IO ()
testDeterministicReport = do
    report <- requireRight "inventory report" (inventoryBenchmarkReport primarySolution widenedSolution)
    let rendered = renderInventoryBenchmarkReport report
        requiredFields =
            [ "provenance: synthetic bounded serial fixture"
            , "event timing:"
            , "terminal convention:"
            , "one-period retained demand mass:"
            , "one-period omitted demand mass:"
            , "horizon exceedance probability:"
            , "normalized bounded demand probabilities:"
            , "primary oracle return:"
            , "primary base-stock return:"
            , "primary policy regret:"
            , "primary target grid (period 1 upward):"
            , "widened target grid (period 1 upward):"
            , "solver method: exact backward induction"
            , "primary selected targets"
            , "primary model size:"
            , "primary solver status:"
            , "primary versus widened bounds: stable"
            , "exact values apply only to the conditional bounded-demand model"
            ]
        lowered = fmap toLower rendered
    golden <- readUtf8File "test/golden/inventory-report.txt"
    assert "inventory report does not match its golden rendering" (rendered == golden)
    mapM_ (\field -> assert ("report omitted field: " ++ field) (field `isInfixOf` rendered)) requiredFields
    assert "report used an unverified named model label" (not (forbiddenModelLabel `isInfixOf` lowered))
    assert "report used a deferred multi-retailer label" (not (deferredModelLabel `isInfixOf` lowered))
  where
    forbiddenModelLabel = "clark"
    deferredModelLabel = "do" ++ "ğru"

testBenchmarkConfiguration :: IO ()
testBenchmarkConfiguration = do
    assert "benchmark warm-up count changed" (inventoryBenchmarkWarmupCount == 1)
    assert "benchmark sample count changed" (inventoryBenchmarkSampleCount == 20)

independentMaximum ::
    ExactMDP SerialInventoryState SerialInventoryAction ->
    ExactDiscount ->
    SerialInventoryState ->
    IO Rational
independentMaximum model discount state = do
    decision <- requireRight "independent oracle inspection" (inspectExactMDP model state)
    case decision of
        ExactTerminalDecision payoff -> pure (exactRewardValue payoff)
        ExactActionDecision actions -> do
            values <- mapM actionValueAt (NonEmpty.toList actions)
            case values of
                [] -> fail "independent oracle saw empty continuing action set"
                first : remaining -> pure (foldl max first remaining)
  where
    gamma = exactDiscountValue discount
    actionValueAt selected = do
        transition <- requireRight "independent oracle transition" (stepExactMDP model state selected)
        contributions <-
            mapM
                ( \(outcome, mass) -> do
                    successor <- independentMaximum model discount (exactSuccessorState outcome)
                    pure
                        ( exactProbability mass
                            * (exactRewardValue (exactTransitionReward outcome) + gamma * successor)
                        )
                )
                (NonEmpty.toList (exactOutcomes transition))
        pure (sum contributions)

allActions ::
    ExactMDP SerialInventoryState SerialInventoryAction ->
    [SerialInventoryState] ->
    IO [ActionId SerialInventoryAction]
allActions model states = fmap (unique . concat) (mapM actionsAt states)
  where
    actionsAt state = do
        decision <- requireRight "action-support inspection" (inspectExactMDP model state)
        case decision of
            ExactTerminalDecision _ -> pure []
            ExactActionDecision actions -> pure (NonEmpty.toList actions)

unique :: (Eq value) => [value] -> [value]
unique = foldl (\values value -> if value `elem` values then values else values ++ [value]) []
