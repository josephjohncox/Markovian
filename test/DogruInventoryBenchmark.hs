module DogruInventoryBenchmark (runDogruInventoryTests) where

import Data.List (isInfixOf)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Markovian.Benchmark.Inventory.Distribution.Dogru.Exact
import Markovian.Benchmark.Inventory.Distribution.Dogru.Report
import Markovian.Category.Finite.Set (finiteSet, finiteSetValues, sameFiniteLayout, sameFiniteSet)
import Numeric.Natural (Natural)

runDogruInventoryTests :: (String -> IO () -> IO ()) -> IO ()
runDogruInventoryTests run = do
    run "Dogru source scenario and independent demand" testSourceScenario
    run "Dogru timing, cost, and correlated outcomes" testTimingAndCorrelation
    run "Dogru physical conservation and signed relaxation" testActionSystems
    run "Dogru balanced and physicalized policies differ" testPolicies
    run "Dogru source newsvendor threshold and terminal convention" testThresholdAndTerminal
    run "Dogru complete reachable closure and finite layouts" testReachabilityAndLayouts
    run "Dogru exact physical-versus-relaxed comparison" testComparison
    run "Dogru independent physical and relaxed minimizers" testIndependentMaximizer
    run "Dogru malformed input and budgets" testRejections
    run "Dogru widened bounds" testWidening
    run "Dogru deterministic cited report" testReport
    run "Dogru benchmark sample contract" testBenchmarkCounts

assert :: String -> Bool -> IO ()
assert message condition = if condition then pure () else fail message

requireRight :: (Show error) => String -> Either error value -> IO value
requireRight _ (Right value) = pure value
requireRight label (Left err) = fail (label ++ ": " ++ show err)

mustRight :: (Show error) => Either error value -> value
mustRight (Right value) = value
mustRight (Left err) = error (show err)

primaryParameters :: DogruParameters
primaryParameters = mustRight (validateDogruParameters (dogruScenario2Input 2 4 1))

widenedParameters :: DogruParameters
widenedParameters = mustRight (validateDogruParameters (dogruScenario2Input 2 5 2))

initialState :: DogruState
initialState = DogruState 2 0 0 (-1) 1

primaryFixture :: DogruFixture
primaryFixture = mustRight (dogruFixture primaryParameters initialState)

widenedFixture :: DogruFixture
widenedFixture = mustRight (dogruFixture widenedParameters initialState)

primarySolution :: DogruSolution
primarySolution = mustRight (solveDogru primaryFixture)

widenedSolution :: DogruSolution
widenedSolution = mustRight (solveDogru widenedFixture)

testSourceScenario :: IO ()
testSourceScenario = do
    assert "source demand mass changed" (sum (fmap snd (NonEmpty.toList dogruDemandOutcomes)) == 1)
    assert "source demand mean changed" (dogruDemandMean == 9 % 20)
    assert "source demand second moment changed" (dogruDemandSecondMoment == 107 % 100)
    assert "source demand variance changed" (dogruDemandVariance == 347 % 400)
    assert "source displayed law was treated as truncated" (dogruOmittedDemandMass == 0)
    assert "joint demand is not a 4x4 product" (length dogruJointDemandOutcomes == 16)
    mapM_
        ( \(DogruDemand left right, mass) ->
            assert
                "joint demand lost independence"
                (mass == marginal left * marginal right)
        )
        (NonEmpty.toList dogruJointDemandOutcomes)
  where
    marginal demand = fromMaybe 0 (lookup demand (NonEmpty.toList dogruDemandOutcomes))

testTimingAndCorrelation :: IO ()
testTimingAndCorrelation = do
    let parameters = mustRight (validateDogruParameters (dogruScenario2Input 1 4 3))
        state = DogruState 1 2 1 (-1) 2
        action = DogruPhysicalAction 0 1 2
    (successor, cost) <- requireRight "timing transition" (dogruPhysicalTransition parameters state action (DogruDemand 2 1))
    assert "warehouse timing changed" (dogruWarehouseStock successor == 0)
    assert "retailer timing changed" (dogruRetailer1Inventory successor == -2 && dogruRetailer2Inventory successor == 3)
    assert "supplier lead time changed" (dogruSupplierOrderDue successor == 0)
    assert "realized cost changed" (cost == 11)
    outcomes <- requireRight "correlated outcomes" (dogruPhysicalOutcomes parameters state action)
    assert "joint demand outcomes were marginalized" (length outcomes == 16)
    mapM_
        ( \(demand, _, pairedSuccessor, pairedCost) ->
            assert
                "cost and successor use different demand"
                (Right (pairedSuccessor, pairedCost) == dogruPhysicalTransition parameters state action demand)
        )
        (NonEmpty.toList outcomes)

testActionSystems :: IO ()
testActionSystems = do
    let state = DogruState 1 1 1 0 0
        physical = DogruPhysicalAction 0 1 1
        overAllocated = DogruPhysicalAction 0 2 1
        relaxed = DogruRelaxedAction 0 (-1) 3
    _ <- requireRight "physical capacity" (dogruPhysicalTransition primaryParameters state physical (DogruDemand 0 0))
    case dogruPhysicalTransition primaryParameters state overAllocated (DogruDemand 0 0) of
        Left (DogruUnavailablePhysicalAction _ _) -> pure ()
        result -> fail ("physical over-allocation accepted: " ++ show result)
    (relaxedSuccessor, _) <- requireRight "signed relaxed action" (dogruRelaxedTransition primaryParameters state relaxed (DogruDemand 0 0))
    assert "relaxed return did not conserve warehouse stock" (dogruWarehouseStock relaxedSuccessor == 0)
    assert "source-coordinate map changed" (dogruSourceWarehouseCoordinate state == 1)

testPolicies :: IO ()
testPolicies = do
    let state = DogruState 1 0 0 1 0
    balanced <- requireRight "balanced action" (dogruBalancedAction primaryParameters state)
    physicalized <- requireRight "physicalized action" (dogruPhysicalizedLBAction primaryParameters state)
    assert "balanced signed allocation counterexample changed" (balanced == DogruRelaxedAction 0 (-1) 1)
    assert "physicalized heuristic retained a negative shipment" (physicalized == DogruPhysicalAction 0 0 0)

testThresholdAndTerminal :: IO ()
testThresholdAndTerminal = do
    let cumulative2 = sum [mass | (demand, mass) <- NonEmpty.toList dogruDemandOutcomes, demand <= 2]
        cumulative3 = sum [mass | (demand, mass) <- NonEmpty.toList dogruDemandOutcomes, demand <= 3]
        terminal = DogruState 1 2 0 0 0
    assert "source scenario threshold below three changed" (cumulative2 == 23 % 25 && cumulative2 < 49 % 50)
    assert "source scenario threshold at three changed" (cumulative3 == 1)
    physical <- requireRight "terminal physical actions" (dogruPhysicalActions primaryParameters terminal)
    relaxed <- requireRight "terminal relaxed actions" (dogruRelaxedActions primaryParameters terminal)
    assert "terminal convention admitted a physical order" (all ((== 0) . dogruPhysicalOrder) physical)
    assert "terminal convention admitted a relaxed order" (all ((== 0) . dogruRelaxedOrder) relaxed)

testReachabilityAndLayouts :: IO ()
testReachabilityAndLayouts = do
    mapM_ checkPhysical (NonEmpty.toList (dogruPhysicalReachableStates primaryFixture))
    mapM_ checkRelaxed (NonEmpty.toList (dogruRelaxedReachableStates primaryFixture))
    let layout = dogruPhysicalStateLayout primaryFixture
    reordered <- requireRight "reordered physical layout" (finiteSet (reverse (finiteSetValues layout)))
    assert "labelled physical support changed on reorder" (sameFiniteSet layout reordered)
    assert "represented physical layout ignored order" (not (sameFiniteLayout layout reordered))
    assert "physical and relaxed supports unexpectedly coincide" (not (sameDogruReachableSupport primaryFixture))
    assert "physical and relaxed represented layouts unexpectedly coincide" (not (sameDogruReachableLayout primaryFixture))
    let actionLayout = dogruRelaxedActionLayout primaryFixture
    reorderedActions <- requireRight "reordered relaxed action layout" (finiteSet (reverse (finiteSetValues actionLayout)))
    assert "labelled relaxed action support changed on reorder" (sameFiniteSet actionLayout reorderedActions)
    assert "represented relaxed action layout ignored order" (not (sameFiniteLayout actionLayout reorderedActions))
  where
    checkPhysical state = do
        actions <- requireRight "physical actions" (dogruPhysicalActions primaryParameters state)
        case actions of
            [] -> assert "nonterminal physical state has no action" (dogruPeriodsRemaining state == 0)
            action : _ -> do
                let (demand, _) = NonEmpty.head dogruJointDemandOutcomes
                (successor, _) <- requireRight "physical successor" (dogruPhysicalTransition primaryParameters state action demand)
                assert "physical successor omitted or redirected" (successor `elem` NonEmpty.toList (dogruPhysicalReachableStates primaryFixture))
    checkRelaxed state = do
        actions <- requireRight "relaxed actions" (dogruRelaxedActions primaryParameters state)
        case actions of
            [] -> assert "nonterminal relaxed state has no action" (dogruPeriodsRemaining state == 0)
            action : _ -> do
                let (demand, _) = NonEmpty.head dogruJointDemandOutcomes
                (successor, _) <- requireRight "relaxed successor" (dogruRelaxedTransition primaryParameters state action demand)
                assert "relaxed successor omitted or redirected" (successor `elem` NonEmpty.toList (dogruRelaxedReachableStates primaryFixture))

testComparison :: IO ()
testComparison = do
    assert "bounded relaxed oracle changed" (dogruRelaxedOracleCost primarySolution == 742797 % 100000)
    assert "bounded physical oracle changed" (dogruPhysicalOracleCost primarySolution == 113477 % 10000)
    assert "balanced policy changed" (dogruBalancedPolicyCost primarySolution == 742797 % 100000)
    assert "physicalized LB heuristic changed" (dogruPhysicalizedLBHeuristicCost primarySolution == 1157133 % 100000)
    assert "C_R <= C_P failed" (dogruRelaxedOracleCost primarySolution <= dogruPhysicalOracleCost primarySolution)
    assert "C_P <= C_H failed" (dogruPhysicalOracleCost primarySolution <= dogruPhysicalizedLBHeuristicCost primarySolution)
    assert "gap decomposition failed" (dogruRelaxationError primarySolution + dogruPolicyRegret primarySolution == dogruTotalGap primarySolution)

testIndependentMaximizer :: IO ()
testIndependentMaximizer = do
    let onePeriodParameters = mustRight (validateDogruParameters (dogruScenario2Input 1 1 1))
        onePeriodState = DogruState 1 0 0 0 0
        onePeriodFixture = mustRight (dogruFixture onePeriodParameters onePeriodState)
        onePeriodSolution = mustRight (solveDogru onePeriodFixture)
    independentPhysical <- requireRight "independent one-period physical" (independentPhysicalCost onePeriodParameters onePeriodState)
    independentRelaxed <- requireRight "independent one-period relaxed" (independentRelaxedCost onePeriodParameters onePeriodState)
    assert "independent one-period physical minimizer differs" (dogruPhysicalOracleCost onePeriodSolution == independentPhysical)
    assert "independent one-period relaxed minimizer differs" (dogruRelaxedOracleCost onePeriodSolution == independentRelaxed)
    let twoPeriodParameters = mustRight (validateDogruParameters (dogruScenario2Input 2 1 1))
        twoPeriodState = DogruState 2 0 0 0 0
        twoPeriodFixture = mustRight (dogruFixture twoPeriodParameters twoPeriodState)
        twoPeriodSolution = mustRight (solveDogru twoPeriodFixture)
    twoPhysical <- requireRight "independent two-period physical" (independentPhysicalCost twoPeriodParameters twoPeriodState)
    twoRelaxed <- requireRight "independent two-period relaxed" (independentRelaxedCost twoPeriodParameters twoPeriodState)
    assert "independent two-period physical recursion differs" (dogruPhysicalOracleCost twoPeriodSolution == twoPhysical)
    assert "independent two-period relaxed recursion differs" (dogruRelaxedOracleCost twoPeriodSolution == twoRelaxed)

independentPhysicalCost :: DogruParameters -> DogruState -> Either DogruError Rational
independentPhysicalCost parameters state
    | dogruPeriodsRemaining state == 0 = Right 0
    | otherwise = do
        actions <- dogruPhysicalActions parameters state
        costs <- traverse actionCost actions
        case costs of
            [] -> Left (DogruModelMismatch "independent physical recursion found no action")
            first : remaining -> Right (foldl min first remaining)
  where
    actionCost action =
        fmap
            sum
            ( traverse
                ( \(demand, mass) -> do
                    let (successor, immediate) =
                            independentTransition
                                parameters
                                state
                                (dogruPhysicalOrder action)
                                (toInteger (dogruPhysicalShipment1 action))
                                (toInteger (dogruPhysicalShipment2 action))
                                demand
                    future <- independentPhysicalCost parameters successor
                    Right (mass * (immediate + future))
                )
                (NonEmpty.toList dogruJointDemandOutcomes)
            )

independentRelaxedCost :: DogruParameters -> DogruState -> Either DogruError Rational
independentRelaxedCost parameters state
    | dogruPeriodsRemaining state == 0 = Right 0
    | otherwise = do
        actions <- dogruRelaxedActions parameters state
        costs <- traverse actionCost actions
        case costs of
            [] -> Left (DogruModelMismatch "independent relaxed recursion found no action")
            first : remaining -> Right (foldl min first remaining)
  where
    actionCost action =
        fmap
            sum
            ( traverse
                ( \(demand, mass) -> do
                    let (successor, immediate) =
                            independentTransition
                                parameters
                                state
                                (dogruRelaxedOrder action)
                                (dogruRelaxedShipment1 action)
                                (dogruRelaxedShipment2 action)
                                demand
                    future <- independentRelaxedCost parameters successor
                    Right (mass * (immediate + future))
                )
                (NonEmpty.toList dogruJointDemandOutcomes)
            )

independentTransition :: DogruParameters -> DogruState -> Natural -> Integer -> Integer -> DogruDemand -> (DogruState, Rational)
independentTransition parameters state order shipment1 shipment2 demand = (successor, cost)
  where
    warehouse = toInteger (dogruWarehouseStock state + dogruSupplierOrderDue state) - shipment1 - shipment2
    retailer1 = dogruRetailer1Inventory state + shipment1 - toInteger (dogruRetailer1Demand demand)
    retailer2 = dogruRetailer2Inventory state + shipment2 - toInteger (dogruRetailer2Demand demand)
    successor =
        DogruState
            (dogruPeriodsRemaining state - 1)
            (fromInteger warehouse)
            order
            retailer1
            retailer2
    retailerCost inventory =
        (dogruWarehouseHoldingCost parameters + dogruRetailerHoldingCost parameters) * fromInteger (max 0 inventory)
            + dogruRetailerPenaltyCost parameters * fromInteger (max 0 (negate inventory))
    cost =
        dogruWarehouseHoldingCost parameters * fromInteger warehouse
            + retailerCost retailer1
            + retailerCost retailer2

testRejections :: IO ()
testRejections = do
    case validateDogruParameters (dogruScenario2Input 0 4 3) of
        Left (DogruNonPositiveHorizon 0) -> pure ()
        result -> fail ("zero horizon accepted: " ++ show result)
    case validateDogruParameters (dogruScenario2Input 2 4 (-1)) of
        Left (DogruNegativeReturnCap (-1)) -> pure ()
        result -> fail ("negative return cap accepted: " ++ show result)
    tinyStateParameters <- requireRight "tiny state budget" (validateDogruParameters (dogruScenario2Input 2 4 1){inputDogruStateBudget = 1})
    case dogruFixture tinyStateParameters initialState of
        Left (DogruStateBudgetExceeded count) -> assert "state budget count did not cross limit" (count > 1)
        result -> fail ("state budget accepted: " ++ show result)
    tinyActionParameters <- requireRight "tiny action budget" (validateDogruParameters (dogruScenario2Input 2 4 1){inputDogruStateActionBudget = 1})
    case dogruFixture tinyActionParameters initialState of
        Left (DogruStateActionBudgetExceeded count) -> assert "action budget count did not cross limit" (count > 1)
        result -> fail ("action budget accepted: " ++ show result)
    tinySolverParameters <- requireRight "tiny solver budget" (validateDogruParameters (dogruScenario2Input 1 1 1){inputDogruSolverWorkBudget = 1})
    tinyFixture <- requireRight "tiny solver fixture" (dogruFixture tinySolverParameters (DogruState 1 0 0 0 0))
    case solveDogru tinyFixture of
        Left (DogruSolverWorkBudgetExceeded count) -> assert "solver budget count did not cross limit" (count > 1)
        result -> fail ("solver budget accepted: " ++ show result)

testWidening :: IO ()
testWidening = do
    stability <- requireRight "widened comparison" (compareDogruBounds primarySolution widenedSolution)
    assert ("widened bounds not stable: " ++ show stability) (dogruBoundsStable stability)
    assert "widened order cap selected" (not (dogruWidenedOrderCapSelected stability))
    assert "widened return cap selected" (not (dogruWidenedReturnCapSelected stability))
    case compareDogruBounds primarySolution primarySolution of
        Left DogruBoundsNotWidened -> pure ()
        result -> fail ("unwidened evidence accepted: " ++ show result)

testReport :: IO ()
testReport = do
    report <- requireRight "Dogru report" (dogruReport primarySolution widenedSolution)
    repeated <- requireRight "repeated Dogru report" (dogruReport primarySolution widenedSolution)
    let rendered = renderDogruReport report
    golden <- readFile "test/golden/dogru-inventory-report.txt"
    assert "report was nondeterministic" (report == repeated)
    assert "report differs from golden" (rendered == golden)
    mapM_
        (\field -> assert ("report omitted: " ++ field) (field `isInfixOf` rendered))
        [ "DOI 10.6100/IR601558"
        , "finite-horizon adaptation, not reproduction"
        , "w=I0-IP1-IP2"
        , "equations (2.3)–(2.5), printed page 24"
        , "equations (2.6)–(2.7), printed page 26"
        , "Table 4.1, page 95; Table 4.3, page 98"
        , "observe state and known due order o; choose supplier order q and shipments s1,s2; receive the due supplier order"
        , "no state clamping"
        , "cv_i=2 (label only"
        , "widened-bound diagnostic: stable and nonbinding"
        , "not proof of an unbounded"
        ]

testBenchmarkCounts :: IO ()
testBenchmarkCounts = do
    assert "warm-up count changed" (dogruBenchmarkWarmupCount == 1)
    assert "sample count changed" (dogruBenchmarkSampleCount == 20)
