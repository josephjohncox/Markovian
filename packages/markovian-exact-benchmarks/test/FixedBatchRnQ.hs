module FixedBatchRnQ (runFixedBatchRnQTests) where

import Data.List (isInfixOf, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio ((%))
import Markovian.Benchmark.Inventory.Serial.FixedBatch.Exact
import Markovian.Benchmark.Inventory.Serial.FixedBatch.Newsvendor.Exact
import Markovian.Benchmark.Inventory.Serial.FixedBatch.Report
import Markovian.Category.Finite.Set (finiteSet, finiteSetValues, sameFiniteLayout, sameFiniteSet)
import Utf8Golden (readUtf8File)

runFixedBatchRnQTests :: (String -> IO () -> IO ()) -> IO ()
runFixedBatchRnQTests run = do
    run "fixed-batch parameters, states, and all finite budgets" testValidationAndBudgets
    run "fixed-batch congruence, physical availability, and conservation" testActionsAndConservation
    run "fixed-batch source event order and supplier lead time" testTimingAndLeadTime
    run "fixed-batch realized cost remains paired with successor" testCorrelation
    run "fixed-batch conditioned demand and truncation masses" testDemand
    run "fixed-batch complete closure and finite layouts" testReachabilityAndLayouts
    run "fixed-batch terminal convention and no salvage" testTerminal
    run "fixed-batch (R,nQ) threshold and clipping diagnostics" testPolicyThresholdAndClipping
    run "fixed-batch policy actions are feasible on all reachable states" testPolicyFeasibility
    run "fixed-batch oracle matches independent backward recursion" testOracleDifferential
    run "fixed-batch fixed-policy evaluator matches independent recursion" testPolicyDifferential
    run "fixed-batch grid canonicalization, ties, and regret" testGridAndRegret
    run "fixed-batch stationary Cartesian domain regressions" testStationaryDomains
    run "fixed-batch stationary exact checked-work boundary" testStationaryWorkBudget
    run "fixed-batch stationary subsystem shortfalls and costs" testShortfallsAndCosts
    run "fixed-batch stationary forward differences and inequalities" testDifferencesAndInequalities
    run "fixed-batch Q=1 reorder/base-stock relation" testUnitBatchRelation
    run "fixed-batch finite-horizon and stationary selections differ" testFiniteStationaryCounterexample
    run "fixed-batch checked action and grid widening" testBoundWidening
    run "fixed-batch demand-cap diagnostic is not an error bound" testDemandWidening
    run "fixed-batch stationary report provenance" testReportProvenance
    run "fixed-batch deterministic cited report" testReport
    run "fixed-batch benchmark sample contract" testBenchmarkCounts

assert :: String -> Bool -> IO ()
assert message condition = if condition then pure () else fail message

requireRight :: (Show error) => String -> Either error value -> IO value
requireRight _ (Right value) = pure value
requireRight label (Left err) = fail (label ++ ": " ++ show err)

mustRight :: (Show error) => Either error value -> value
mustRight (Right value) = value
mustRight (Left err) = error (show err)

parameterInput :: Integer -> Integer -> Integer -> FixedBatchParameterInput
parameterInput externalCap demandCap horizon =
    FixedBatchParameterInput
        { inputFixedBatchHorizon = horizon
        , inputFixedBatchSupplierLeadTime = 2
        , inputFixedBatchQ1 = 1
        , inputFixedBatchQ2 = 2
        , inputFixedBatchExternalBatchCap = externalCap
        , inputFixedBatchDemandCap = demandCap
        , inputFixedBatchStage1HoldingCost = 1
        , inputFixedBatchStage2HoldingCost = 1
        , inputFixedBatchBacklogCost = 4
        , inputFixedBatchStateBudget = 500000
        , inputFixedBatchStateActionBudget = 20000000
        , inputFixedBatchSolverWorkBudget = 2000000000
        , inputFixedBatchGridBudget = 1000
        , inputFixedBatchConvolutionBudget = 2000000
        }

parametersFor :: Integer -> Integer -> Integer -> FixedBatchParameters
parametersFor externalCap demandCap horizon = mustRight (validateFixedBatchParameters (parameterInput externalCap demandCap horizon))

crossGrid :: FixedBatchParameters -> Integer -> Integer -> ReorderGrid
crossGrid parameters lower upper =
    mustRight
        ( reorderGrid
            parameters
            (ReorderLevels lower lower :| [ReorderLevels r1 r2 | r1 <- [lower .. upper], r2 <- [lower .. upper], (r1, r2) /= (lower, lower)])
        )

stationaryDomain :: FixedBatchParameters -> Integer -> Integer -> NewsvendorDomain
stationaryDomain parameters lower upper =
    mustRight (newsvendorDomain parameters (lower :| [lower + 1 .. upper]) (lower :| [lower + 1 .. upper]))

fixtureFor :: FixedBatchParameters -> FixedBatchFixture
fixtureFor parameters =
    let state = mustRight (fixedBatchState parameters (fixedBatchHorizon parameters) 0 2 (replicate (fromIntegral (fixedBatchSupplierLeadTime parameters)) 0))
     in mustRight (fixedBatchFixture parameters state)

primaryParameters :: FixedBatchParameters
primaryParameters = parametersFor 2 3 3

widenedParameters :: FixedBatchParameters
widenedParameters = parametersFor 3 3 3

demandWidenedParameters :: FixedBatchParameters
demandWidenedParameters = parametersFor 2 4 3

primaryFixture :: FixedBatchFixture
primaryFixture = fixtureFor primaryParameters

widenedFixture :: FixedBatchFixture
widenedFixture = fixtureFor widenedParameters

demandWidenedFixture :: FixedBatchFixture
demandWidenedFixture = fixtureFor demandWidenedParameters

primaryGrid :: ReorderGrid
primaryGrid = crossGrid primaryParameters (-2) 4

widenedGrid :: ReorderGrid
widenedGrid = crossGrid widenedParameters (-3) 5

demandWidenedGrid :: ReorderGrid
demandWidenedGrid = crossGrid demandWidenedParameters (-2) 4

primarySolution :: FixedBatchSolution
primarySolution = mustRight (solveFixedBatch primaryGrid primaryFixture)

widenedSolution :: FixedBatchSolution
widenedSolution = mustRight (solveFixedBatch widenedGrid widenedFixture)

demandWidenedSolution :: FixedBatchSolution
demandWidenedSolution = mustRight (solveFixedBatch demandWidenedGrid demandWidenedFixture)

primaryNewsvendor :: NewsvendorSolution
primaryNewsvendor = mustRight (solveNewsvendorGrid primaryParameters (stationaryDomain primaryParameters (-2) 4))

demandWidenedNewsvendor :: NewsvendorSolution
demandWidenedNewsvendor = mustRight (solveNewsvendorGrid demandWidenedParameters (stationaryDomain demandWidenedParameters (-2) 4))

separationParameters :: FixedBatchParameters
separationParameters = parametersFor 2 2 2

separationGrid :: ReorderGrid
separationGrid = crossGrid separationParameters (-4) 7

separationFinite :: FixedBatchSolution
separationFinite = mustRight (solveFixedBatch separationGrid (fixtureFor separationParameters))

separationStationary :: NewsvendorSolution
separationStationary = mustRight (solveNewsvendorGrid separationParameters (stationaryDomain separationParameters (-4) 7))

testValidationAndBudgets :: IO ()
testValidationAndBudgets = do
    expectLeft "zero Q1" isNonPositive (validateFixedBatchParameters (parameterInput 2 3 3){inputFixedBatchQ1 = 0})
    expectLeft "nonintegral ratio" isRatio (validateFixedBatchParameters (parameterInput 2 3 3){inputFixedBatchQ1 = 2, inputFixedBatchQ2 = 3})
    expectLeft "zero lead time" isNonPositive (validateFixedBatchParameters (parameterInput 2 3 3){inputFixedBatchSupplierLeadTime = 0})
    expectLeft "lead-time budget" isLeadBudget (validateFixedBatchParameters (parameterInput 2 3 3){inputFixedBatchSupplierLeadTime = toInteger fixedBatchMaximumLeadTime + 1})
    expectLeft "demand excludes one" isDemandOne (validateFixedBatchParameters (parameterInput 2 0 3))
    expectLeft "demand support budget" isDemandBudget (validateFixedBatchParameters (parameterInput 2 (toInteger fixedBatchMaximumDemandOutcomes) 3))
    expectLeft "negative cap" isNegative (validateFixedBatchParameters (parameterInput (-1) 3 3))
    expectLeft "zero cost" isCost (validateFixedBatchParameters (parameterInput 2 3 3){inputFixedBatchBacklogCost = 0})
    expectLeft "pipeline length" isPipelineLength (fixedBatchState primaryParameters 3 0 2 [0])
    expectLeft "pipeline congruence" isPipelineCongruence (fixedBatchState primaryParameters 3 0 2 [0, 1])
    let congruenceParameters = mustRight (validateFixedBatchParameters (parameterInput 2 3 3){inputFixedBatchQ1 = 2, inputFixedBatchQ2 = 4})
    expectLeft "stage-2 congruence" isStage2Congruence (fixedBatchState congruenceParameters 3 0 3 [0, 0])
    let outsideState = mustRight (fixedBatchState primaryParameters 1 99 0 [0, 0])
    expectLeft "state outside fixture" isOutsideFixture (fixedBatchOutcomes primaryFixture outsideState (FixedBatchAction 0 0))
    let tinyStateParameters = mustRight (validateFixedBatchParameters (parameterInput 2 3 3){inputFixedBatchStateBudget = 1})
        tinyState = mustRight (fixedBatchState tinyStateParameters 3 0 2 [0, 0])
    expectLeft "state budget" isStateBudget (fixedBatchFixture tinyStateParameters tinyState)
    let tinyActionParameters = mustRight (validateFixedBatchParameters (parameterInput 2 3 3){inputFixedBatchStateActionBudget = 1})
        tinyActionState = mustRight (fixedBatchState tinyActionParameters 3 0 2 [0, 0])
    expectLeft "state-action budget" isActionBudget (fixedBatchActions tinyActionParameters tinyActionState)
    let tinyGridParameters = mustRight (validateFixedBatchParameters (parameterInput 2 3 3){inputFixedBatchGridBudget = 1})
    expectLeft "grid budget" isGridBudget (reorderGrid tinyGridParameters (ReorderLevels 0 0 :| [ReorderLevels 1 1]))
    expectLeft "duplicate grid" isDuplicateGrid (reorderGrid primaryParameters (ReorderLevels 0 0 :| [ReorderLevels 0 0]))
    let tinySolverParameters = mustRight (validateFixedBatchParameters (parameterInput 1 1 1){inputFixedBatchSolverWorkBudget = 1})
        tinySolverFixture = fixtureFor tinySolverParameters
        tinySolverGrid = crossGrid tinySolverParameters 0 1
    expectLeft "solver budget" isSolverBudget (solveFixedBatch tinySolverGrid tinySolverFixture)
    let tinyConvolution = mustRight (validateFixedBatchParameters (parameterInput 1 3 1){inputFixedBatchConvolutionBudget = 1})
    expectLeft "convolution budget" isConvolutionBudget (subsystem2Shortfalls tinyConvolution (ReorderLevels 0 0))
  where
    isNonPositive (FixedBatchNonPositive _ _) = True
    isNonPositive _ = False
    isRatio (FixedBatchNonIntegralBatchRatio _ _) = True
    isRatio _ = False
    isLeadBudget (FixedBatchLeadTimeBudgetExceeded _) = True
    isLeadBudget _ = False
    isDemandOne (FixedBatchDemandCapExcludesOne _) = True
    isDemandOne _ = False
    isDemandBudget (FixedBatchDemandOutcomeBudgetExceeded _) = True
    isDemandBudget _ = False
    isNegative (FixedBatchNegative _ _) = True
    isNegative _ = False
    isCost (FixedBatchNonPositiveCost _ _) = True
    isCost _ = False
    isPipelineLength (FixedBatchPipelineLengthMismatch _ _) = True
    isPipelineLength _ = False
    isPipelineCongruence (FixedBatchPipelineCongruence _) = True
    isPipelineCongruence _ = False
    isStage2Congruence (FixedBatchStage2Congruence _) = True
    isStage2Congruence _ = False
    isOutsideFixture (FixedBatchStateOutsideFixture _) = True
    isOutsideFixture _ = False
    isStateBudget (FixedBatchStateBudgetExceeded _) = True
    isStateBudget _ = False
    isActionBudget (FixedBatchStateActionBudgetExceeded _) = True
    isActionBudget _ = False
    isGridBudget (FixedBatchGridBudgetExceeded _) = True
    isGridBudget _ = False
    isDuplicateGrid (FixedBatchDuplicateGridPoint _) = True
    isDuplicateGrid _ = False
    isSolverBudget (FixedBatchSolverWorkBudgetExceeded _) = True
    isSolverBudget _ = False
    isConvolutionBudget (FixedBatchConvolutionBudgetExceeded _) = True
    isConvolutionBudget _ = False

testActionsAndConservation :: IO ()
testActionsAndConservation = do
    let state = mustRight (fixedBatchState primaryParameters 2 (-1) 2 [2, 0])
    actions <- requireRight "actions" (fixedBatchActions primaryParameters state)
    assert "Q1 divisibility failed" (all (\action -> fixedBatchStage1Release action `mod` fixedBatchQ1 primaryParameters == 0) actions)
    assert "Q2 divisibility failed" (all (\action -> fixedBatchExternalOrder action `mod` fixedBatchQ2 primaryParameters == 0) actions)
    assert "physical release exceeded available material" (all ((<= 4) . fixedBatchStage1Release) actions)
    expectLeft "unavailable release" isUnavailable (fixedBatchTransition primaryParameters state (FixedBatchAction 5 0) 1)
    let action = FixedBatchAction 3 4
    (successor, _) <- requireRight "conservation transition" (fixedBatchTransition primaryParameters state action 2)
    let total current = fixedBatchStage1NetInventory current + toInteger (fixedBatchStage2OnHand current + sum (fixedBatchPipeline current))
    assert "material conservation failed" (total successor == total state + toInteger (fixedBatchExternalOrder action) - 2)
  where
    isUnavailable (FixedBatchUnavailableAction _ _) = True
    isUnavailable _ = False

testTimingAndLeadTime :: IO ()
testTimingAndLeadTime = do
    let parameters = parametersFor 2 2 3
        state0 = mustRight (fixedBatchState parameters 3 0 0 [0, 0])
        order = FixedBatchAction 0 2
    (state1, _) <- requireRight "first lead step" (fixedBatchTransition parameters state0 order 0)
    assert "new order arrived after one period" (fixedBatchStage2OnHand state1 == 0 && fixedBatchPipeline state1 == [0, 2])
    (state2, _) <- requireRight "second lead step" (fixedBatchTransition parameters state1 (FixedBatchAction 0 0) 0)
    assert "new order was physically received too early" (fixedBatchStage2OnHand state2 == 0 && fixedBatchPipeline state2 == [2, 0])
    (state3, _) <- requireRight "arrival step" (fixedBatchTransition parameters state2 (FixedBatchAction 2 0) 0)
    assert "L2 arrival was not available for physical release" (fixedBatchStage1NetInventory state3 == 2 && fixedBatchStage2OnHand state3 == 0)

testCorrelation :: IO ()
testCorrelation = do
    let parameters = parametersFor 1 2 1
        state = mustRight (fixedBatchState parameters 1 0 1 [0, 0])
        fixture = mustRight (fixedBatchFixture parameters state)
        action = FixedBatchAction 1 0
    outcomes <- requireRight "outcomes" (fixedBatchOutcomes fixture state action)
    assert "conditioned support cardinality changed" (length outcomes == 3)
    mapM_
        ( \outcome -> do
            pair <- requireRight "paired transition" (fixedBatchTransition parameters state action (fixedBatchOutcomeDemand outcome))
            assert "cost/successor correlation was marginalized" (pair == (fixedBatchOutcomeSuccessor outcome, fixedBatchOutcomeCost outcome))
        )
        (NonEmpty.toList outcomes)
    let costs = fmap fixedBatchOutcomeCost (NonEmpty.toList outcomes)
        successors = fmap (fixedBatchStage1NetInventory . fixedBatchOutcomeSuccessor) (NonEmpty.toList outcomes)
    assert "counterexample lost demand-dependent costs" (length (unique costs) > 1)
    assert "counterexample lost demand-dependent successors" (length (unique successors) > 1)

testDemand :: IO ()
testDemand = do
    let demand = conditionedGeometricDemand 2
    assert "conditioned demand did not normalize" (sum (fmap snd (NonEmpty.toList (fixedBatchDemandOutcomes demand))) == 1)
    assert "retained mass changed" (fixedBatchRetainedMass demand == 7 % 8)
    assert "omitted mass changed" (fixedBatchOmittedMass demand == 1 % 8)
    assert "path exceedance changed" (fixedBatchPathExceedanceMass 3 demand == 169 % 512)

testReachabilityAndLayouts :: IO ()
testReachabilityAndLayouts = do
    let states = NonEmpty.toList (fixedBatchReachableStates primaryFixture)
    mapM_ checkState states
    assert "reachable state layout contains duplicates" (length states == length (unique states))
    let stateLayout = fixedBatchStateLayout primaryFixture
    reorderedStates <- requireRight "reordered states" (finiteSet (reverse (finiteSetValues stateLayout)))
    assert "labelled support changed after reorder" (sameFiniteSet stateLayout reorderedStates)
    assert "represented state layout ignored reorder" (not (sameFiniteLayout stateLayout reorderedStates))
    let actionLayout = fixedBatchActionLayout primaryFixture
    reorderedActions <- requireRight "reordered actions" (finiteSet (reverse (finiteSetValues actionLayout)))
    assert "labelled action support changed after reorder" (sameFiniteSet actionLayout reorderedActions)
    assert "represented action layout ignored reorder" (not (sameFiniteLayout actionLayout reorderedActions))
  where
    checkState state
        | fixedBatchPeriodsRemaining state == 0 = pure ()
        | otherwise = do
            actions <- requireRight "reachable actions" (fixedBatchActions primaryParameters state)
            mapM_
                ( \action ->
                    mapM_
                        ( \(demand, _) -> do
                            (successor, _) <- requireRight "reachable successor" (fixedBatchTransition primaryParameters state action demand)
                            assert "successor omitted or redirected" (successor `elem` NonEmpty.toList (fixedBatchReachableStates primaryFixture))
                        )
                        (NonEmpty.toList (fixedBatchDemandOutcomes (fixedBatchFixtureDemand primaryFixture)))
                )
                actions

testTerminal :: IO ()
testTerminal = do
    let terminal = mustRight (fixedBatchState primaryParameters 0 7 8 [2, 4])
    actions <- requireRight "terminal actions" (fixedBatchActions primaryParameters terminal)
    assert "terminal exposed an action" (null actions)
    expectLeft "terminal transition" isTerminal (fixedBatchTransition primaryParameters terminal (FixedBatchAction 0 0) 0)
    assert "finite result gained terminal salvage" (bruteOracle primaryParameters terminal == Right 0)
  where
    isTerminal (FixedBatchTerminalAction _) = True
    isTerminal _ = False

testPolicyThresholdAndClipping :: IO ()
testPolicyThresholdAndClipping = do
    assert "order at IP=R was not one batch" (fixedBatchOrderQuantity 3 3 2 == 2)
    assert "order at IP=R+1 was nonzero" (fixedBatchOrderQuantity 4 3 2 == 0)
    assert "multi-batch threshold formula changed" (fixedBatchOrderQuantity (-2) 3 2 == 6)
    let state = mustRight (fixedBatchState primaryParameters 2 (-5) 0 [0, 0])
    selected <- requireRight "clipped policy" (fixedBatchPolicyAction primaryParameters (ReorderLevels 0 8) state)
    assert "release clipping not reported" (fixedBatchReleaseWasClipped selected)
    assert "external cap clipping not reported" (fixedBatchExternalWasClipped selected)
    assert "clipped policy became infeasible" (fixedBatchPolicyPhysicalAction selected == FixedBatchAction 0 4)

testPolicyFeasibility :: IO ()
testPolicyFeasibility =
    mapM_ check (NonEmpty.toList (fixedBatchReachableStates primaryFixture))
  where
    levels = ReorderLevels 1 2
    check state
        | fixedBatchPeriodsRemaining state == 0 = pure ()
        | otherwise = do
            policyAction <- requireRight "policy action" (fixedBatchPolicyAction primaryParameters levels state)
            actions <- requireRight "physical actions" (fixedBatchActions primaryParameters state)
            assert "policy selected unavailable action" (fixedBatchPolicyPhysicalAction policyAction `elem` actions)

testOracleDifferential :: IO ()
testOracleDifferential = do
    let parameters = parametersFor 1 2 2
        fixture = fixtureFor parameters
        grid = crossGrid parameters (-2) 3
    solution <- requireRight "small solve" (solveFixedBatch grid fixture)
    independent <- requireRight "independent oracle" (bruteOracle parameters (fixedBatchInitialState fixture))
    assert "backward induction differs from independent recursion" (fixedBatchOracleReturn solution == independent)

testPolicyDifferential :: IO ()
testPolicyDifferential = do
    let parameters = parametersFor 1 2 2
        fixture = fixtureFor parameters
        levels = ReorderLevels 0 1
    direct <- requireRight "compiled policy" (evaluateFixedBatchPolicy fixture levels)
    independent <- requireRight "independent policy" (brutePolicy parameters levels (fixedBatchInitialState fixture))
    assert "fixed-policy evaluators differ" (direct == independent)

testGridAndRegret :: IO ()
testGridAndRegret = do
    assert "policy regret became negative" (fixedBatchPolicyRegret primarySolution >= 0)
    let candidates = NonEmpty.toList (reorderGridCandidates primaryGrid)
        reversed = case reverse candidates of
            first : remaining -> first :| remaining
            [] -> error "primary grid is statically nonempty"
    reorderedGrid <- requireRight "reordered grid" (reorderGrid primaryParameters reversed)
    reorderedSolution <- requireRight "reordered solve" (solveFixedBatch reorderedGrid primaryFixture)
    reorderedStationary <- requireRight "reordered stationary solve" (solveNewsvendorGrid primaryParameters (stationaryDomain primaryParameters (-2) 4))
    assert "grid order changed canonical exact tie selection" (fixedBatchSelectedLevels reorderedSolution == fixedBatchSelectedLevels primarySolution)
    assert "grid order changed exact return" (fixedBatchPolicyReturn reorderedSolution == fixedBatchPolicyReturn primarySolution)
    assert "grid order changed stationary selection" (newsvendorSelectedLevels reorderedStationary == newsvendorSelectedLevels primaryNewsvendor)
    assert "grid order changed stationary cost" (newsvendorSubsystem2Cost reorderedStationary == newsvendorSubsystem2Cost primaryNewsvendor)

testStationaryDomains :: IO ()
testStationaryDomains = do
    sparse <- requireRight "sparse stationary domain" (newsvendorDomain primaryParameters ((-2) :| [0]) (1 :| [3]))
    sparseSolution <- requireRight "sparse stationary solve" (solveNewsvendorGrid primaryParameters sparse)
    assert "sparse stationary selection left its actual domain" (newsvendorDomainContains sparse (newsvendorSelectedLevels sparseSolution))
    assert "sparse R1 layout changed" (NonEmpty.toList (newsvendorR1Layout sparse) == [-2, 0])
    assert "sparse R2 layout changed" (NonEmpty.toList (newsvendorR2Layout sparse) == [1, 3])
    let diagonalPairs = [ReorderLevels (-2) 1, ReorderLevels 0 3]
        recombined = ReorderLevels (-2) 3
    assert "diagonal regression accidentally represented the Cartesian recombination as a pair" (recombined `notElem` diagonalPairs)
    assert "explicit Cartesian domain omitted a represented recombination" (newsvendorDomainContains sparse recombined)
    holed <- requireRight "holed stationary domain" (newsvendorDomain primaryParameters ((-2) :| [-1, 1]) ((-2) :| [0, 2]))
    holedSolution <- requireRight "holed stationary solve" (solveNewsvendorGrid primaryParameters holed)
    assert "holed stationary selection left its actual domain" (newsvendorDomainContains holed (newsvendorSelectedLevels holedSolution))
    expectLeft "infinite stationary axis" isGridBudget (newsvendorDomain primaryParameters (0 :| [1 ..]) (0 :| []))
  where
    isGridBudget (FixedBatchGridBudgetExceeded _) = True
    isGridBudget _ = False

testStationaryWorkBudget :: IO ()
testStationaryWorkBudget = do
    let generous = parametersFor 1 2 1
        domain = stationaryDomain generous (-1) 2
    baseline <- requireRight "stationary work baseline" (solveNewsvendorGrid generous domain)
    let checked = newsvendorCheckedTerms baseline
        exactParameters = mustRight (validateFixedBatchParameters (parameterInput 1 2 1){inputFixedBatchSolverWorkBudget = toInteger checked})
        belowParameters = mustRight (validateFixedBatchParameters (parameterInput 1 2 1){inputFixedBatchSolverWorkBudget = toInteger checked - 1})
    assert "memoized stationary charged-term count changed" (checked == 174)
    exact <- requireRight "exact stationary work limit" (solveNewsvendorGrid exactParameters (stationaryDomain exactParameters (-1) 2))
    assert "exact stationary work limit reported a different charge" (newsvendorCheckedTerms exact == checked)
    case solveNewsvendorGrid belowParameters (stationaryDomain belowParameters (-1) 2) of
        Left (FixedBatchSolverWorkBudgetExceeded required) -> assert "one-below stationary rejection lost exact required work" (required == checked)
        result -> fail ("one-below stationary work returned a partial solution: " ++ show result)

testShortfallsAndCosts :: IO ()
testShortfallsAndCosts = do
    let parameters = parametersFor 1 1 1
    law1 <- requireRight "subsystem-1 shortfalls" (subsystem1Shortfalls parameters (-1))
    assert "hand subsystem-1 law changed" (NonEmpty.toList (shortfallOutcomes law1) == [(ShortfallPair 0 0, 2 % 3), (ShortfallPair 0 1, 1 % 3)])
    cost1 <- requireRight "subsystem-1 cost" (subsystem1Cost parameters (-1))
    let manual1 = 1 * ((-1) + 1 - 1 % 3) + 6 * (1 % 3)
    assert "equation (9) subsystem-1 cost changed" (cost1 == manual1)
    let levels = ReorderLevels 0 0
    law2 <- requireRight "subsystem-2 shortfalls" (subsystem2Shortfalls parameters levels)
    let manualLaw = independentSubsystem2Law parameters levels
    assert "subsystem-2 shortfall recursion differs from independent enumeration" (NonEmpty.toList (shortfallOutcomes law2) == manualLaw)
    assert "subsystem-2 law not normalized" (sum (fmap snd manualLaw) == 1)

testDifferencesAndInequalities :: IO ()
testDifferencesAndInequalities = do
    difference1 <- requireRight "c1" (subsystem1ForwardDifference primaryParameters 1)
    c1Left <- requireRight "C1 left" (subsystem1Cost primaryParameters 1)
    c1Right <- requireRight "C1 right" (subsystem1Cost primaryParameters 2)
    assert "c1 is not an exact forward difference" (difference1 == c1Right - c1Left)
    let levels = ReorderLevels 1 2
    difference2 <- requireRight "c2" (subsystem2ForwardDifference primaryParameters levels)
    c2Left <- requireRight "C2 left" (subsystem2Cost primaryParameters levels)
    c2Right <- requireRight "C2 right" (subsystem2Cost primaryParameters levels{fixedBatchR2 = 3})
    assert "c2 is not an exact forward difference" (difference2 == c2Right - c2Left)
    inequality1 <- requireRight "inequality 1" (subsystem1Inequality primaryParameters 1)
    inequality2 <- requireRight "inequality 2" (subsystem2Inequality primaryParameters levels)
    assert "weak c1 inequality disagrees" (newsvendorWeakSatisfied inequality1 == (difference1 >= 0))
    assert "strict c2 inequality disagrees" (newsvendorStrictSatisfied inequality2 == (difference2 > 0))
    let plateauParameters = mustRight (validateFixedBatchParameters (parameterInput 1 1 1){inputFixedBatchBacklogCost = 1})
    plateau <- requireRight "discrete plateau" (subsystem1Inequality plateauParameters (-1))
    assert "weak/strict discrete plateau disappeared" (newsvendorWeakSatisfied plateau && not (newsvendorStrictSatisfied plateau) && newsvendorForwardDifference plateau == 0)

testUnitBatchRelation :: IO ()
testUnitBatchRelation = do
    let parameters = mustRight (validateFixedBatchParameters (parameterInput 1 2 1){inputFixedBatchQ2 = 1})
        reorderLevel = 3
        baseStock = reorderLevel + 1
    assert "Q=1 relation S=R+1 changed at threshold" (fixedBatchOrderQuantity reorderLevel reorderLevel (fixedBatchQ1 parameters) == fromInteger (baseStock - reorderLevel))
    assert "Q=1 base-stock target was not reached" (reorderLevel + toInteger (fixedBatchOrderQuantity reorderLevel reorderLevel 1) == baseStock)

testFiniteStationaryCounterexample :: IO ()
testFiniteStationaryCounterexample = do
    assert "finite/stationary counterexample disappeared" (fixedBatchSelectedLevels separationFinite == ReorderLevels 0 (-4) && newsvendorSelectedLevels separationStationary == ReorderLevels 0 1)

testBoundWidening :: IO ()
testBoundWidening = do
    stability <- requireRight "bound widening" (compareFixedBatchBounds primarySolution widenedSolution)
    assert ("widened solution not stable: " ++ show stability) (fixedBatchBoundsStable stability)
    assert "widened cap selected" (not (fixedBatchWidenedExternalCapSelected stability))
    assert "widened grid selected boundary" (not (fixedBatchWidenedGridBinds stability))
    expectLeft "same solution widening" isNotWidened (compareFixedBatchBounds primarySolution primarySolution)
    let unrelatedParameters = parametersFor 3 2 3
        unrelated = mustRight (solveFixedBatch (crossGrid unrelatedParameters (-3) 5) (fixtureFor unrelatedParameters))
    expectLeft "unrelated widening" isMismatch (compareFixedBatchBounds primarySolution unrelated)
  where
    isNotWidened FixedBatchBoundsNotWidened = True
    isNotWidened _ = False
    isMismatch (FixedBatchModelMismatch _) = True
    isMismatch _ = False

testDemandWidening :: IO ()
testDemandWidening = do
    diagnostic <- requireRight "demand widening" (compareFixedBatchDemandCaps primarySolution demandWidenedSolution)
    assert "widened omitted mass did not decrease" (fixedBatchDemandWidenedOmittedMass diagnostic < fixedBatchDemandPrimaryOmittedMass diagnostic)
    assert "demand widening unexpectedly changed selected levels" (not (fixedBatchDemandSelectedLevelsChanged diagnostic))
    assert "oracle delta incorrectly labeled as omitted mass" (fixedBatchDemandOracleDelta diagnostic /= fixedBatchDemandWidenedOmittedMass diagnostic - fixedBatchDemandPrimaryOmittedMass diagnostic)
    expectLeft "same demand cap" isNotIncreased (compareFixedBatchDemandCaps primarySolution primarySolution)
  where
    isNotIncreased (FixedBatchDemandCapNotIncreased _ _) = True
    isNotIncreased _ = False

testReportProvenance :: IO ()
testReportProvenance = do
    let solveMismatch input = do
            parameters <- requireRight "mismatch parameters" (validateFixedBatchParameters input)
            domain <- requireRight "mismatch domain" (newsvendorDomain parameters ((-2) :| [-1 .. 4]) ((-2) :| [-1 .. 4]))
            requireRight "mismatch stationary solve" (solveNewsvendorGrid parameters domain)
        check label candidate =
            expectLeft
                label
                isModelMismatch
                (fixedBatchReport primarySolution widenedSolution demandWidenedSolution candidate demandWidenedNewsvendor separationFinite separationStationary)
    horizonMismatch <- solveMismatch (parameterInput 2 3 2)
    check "stationary horizon provenance" horizonMismatch
    leadMismatch <- solveMismatch ((parameterInput 2 3 3){inputFixedBatchSupplierLeadTime = 1})
    check "stationary lead-time provenance" leadMismatch
    externalCapMismatch <- solveMismatch (parameterInput 3 3 3)
    check "stationary external-cap provenance" externalCapMismatch
    stage1CostMismatch <- solveMismatch ((parameterInput 2 3 3){inputFixedBatchStage1HoldingCost = 2})
    check "stationary stage-1 cost provenance" stage1CostMismatch
    stage2CostMismatch <- solveMismatch ((parameterInput 2 3 3){inputFixedBatchStage2HoldingCost = 2})
    check "stationary stage-2 cost provenance" stage2CostMismatch
    backlogCostMismatch <- solveMismatch ((parameterInput 2 3 3){inputFixedBatchBacklogCost = 5})
    check "stationary backlog-cost provenance" backlogCostMismatch
    q1Mismatch <- solveMismatch ((parameterInput 2 3 3){inputFixedBatchQ1 = 2, inputFixedBatchQ2 = 2})
    check "stationary Q1 provenance" q1Mismatch
    q2Mismatch <- solveMismatch ((parameterInput 2 3 3){inputFixedBatchQ2 = 1})
    check "stationary Q2 provenance" q2Mismatch
    demandMismatch <- solveMismatch (parameterInput 2 2 3)
    check "stationary demand-cap provenance" demandMismatch
    wrongDomain <- requireRight "wrong stationary domain" (newsvendorDomain primaryParameters ((-1) :| [0 .. 4]) ((-2) :| [-1 .. 4]))
    wrongDomainSolution <- requireRight "wrong-domain stationary solve" (solveNewsvendorGrid primaryParameters wrongDomain)
    check "stationary search-domain provenance" wrongDomainSolution
    expectLeft
        "demand-widened stationary provenance"
        isModelMismatch
        (fixedBatchReport primarySolution widenedSolution demandWidenedSolution primaryNewsvendor primaryNewsvendor separationFinite separationStationary)
  where
    isModelMismatch (FixedBatchModelMismatch _) = True
    isModelMismatch _ = False

testReport :: IO ()
testReport = do
    report <- requireRight "report" (fixedBatchReport primarySolution widenedSolution demandWidenedSolution primaryNewsvendor demandWidenedNewsvendor separationFinite separationStationary)
    repeated <- requireRight "repeated report" (fixedBatchReport primarySolution widenedSolution demandWidenedSolution primaryNewsvendor demandWidenedNewsvendor separationFinite separationStationary)
    let rendered = renderFixedBatchReport report
    golden <- readUtf8File "test/golden/fixed-batch-rnq-report.txt"
    assert "report is nondeterministic" (report == repeated)
    assert "report differs from golden" (rendered == golden)
    mapM_
        (\field -> assert ("report omitted " ++ field) (field `isInfixOf` rendered))
        [ "BETA Working Paper 134"
        , "DOI 10.1016/j.orl.2008.06.003"
        , "finite-horizon oracle"
        , "separate stationary newsvendor section"
        , "semantics=R1×R2"
        , "finite-horizon selected levels ReorderLevels {fixedBatchR1 = 0, fixedBatchR2 = -4}; stationary selected levels ReorderLevels {fixedBatchR1 = 0, fixedBatchR2 = 1}"
        , "not value-error bounds"
        , "no generic N-stage"
        , "repository-authored fixture"
        ]

testBenchmarkCounts :: IO ()
testBenchmarkCounts = do
    assert "warm-up count changed" (fixedBatchBenchmarkWarmupCount == 1)
    assert "sample count changed" (fixedBatchBenchmarkSampleCount == 20)

bruteOracle :: FixedBatchParameters -> FixedBatchState -> Either FixedBatchError Rational
bruteOracle parameters state
    | fixedBatchPeriodsRemaining state == 0 = Right 0
    | otherwise = do
        actions <- fixedBatchActions parameters state
        values <- traverse actionValue actions
        case values of
            [] -> Left (FixedBatchNoAction state)
            first : remaining -> Right (foldl max first remaining)
  where
    demand = NonEmpty.toList (fixedBatchDemandOutcomes (conditionedGeometricDemand (fixedBatchDemandCap parameters)))
    actionValue action = fmap sum (traverse (contribution action) demand)
    contribution action (demandValue, mass) = do
        (successor, cost) <- fixedBatchTransition parameters state action demandValue
        future <- bruteOracle parameters successor
        Right (mass * (negate cost + future))

brutePolicy :: FixedBatchParameters -> ReorderLevels -> FixedBatchState -> Either FixedBatchError Rational
brutePolicy parameters levels state
    | fixedBatchPeriodsRemaining state == 0 = Right 0
    | otherwise = do
        selected <- fixedBatchPolicyAction parameters levels state
        fmap sum (traverse (contribution (fixedBatchPolicyPhysicalAction selected)) demand)
  where
    demand = NonEmpty.toList (fixedBatchDemandOutcomes (conditionedGeometricDemand (fixedBatchDemandCap parameters)))
    contribution action (demandValue, mass) = do
        (successor, cost) <- fixedBatchTransition parameters state action demandValue
        future <- brutePolicy parameters levels successor
        Right (mass * (negate cost + future))

independentSubsystem2Law :: FixedBatchParameters -> ReorderLevels -> [(ShortfallPair, Rational)]
independentSubsystem2Law parameters levels =
    aggregate
        [ ( ShortfallPair b1 b0
          , massA * massB * massC / fromIntegral ratio / fromIntegral q1
          )
        | (demandA, massA) <- demand
        , (demandB, massB) <- demand
        , (demandC, massC) <- demand
        , z <- [0 .. ratio - 1]
        , uniform <- [1 .. q1]
        , let leadDemand = demandA + demandB
              b1 = fromInteger (max 0 (toInteger leadDemand - (fixedBatchR2 levels - fixedBatchR1 levels) - toInteger (z * q1)))
              b0 = fromInteger (max 0 (toInteger b1 + toInteger demandC - fixedBatchR1 levels - toInteger uniform))
        ]
  where
    demand = NonEmpty.toList (fixedBatchDemandOutcomes (conditionedGeometricDemand (fixedBatchDemandCap parameters)))
    q1 = fixedBatchQ1 parameters
    ratio = fixedBatchQ2 parameters `div` q1

aggregate :: (Ord value) => [(value, Rational)] -> [(value, Rational)]
aggregate = combine . sort
  where
    combine [] = []
    combine ((value, mass) : remaining) = gather value mass remaining
    gather value mass [] = [(value, mass)]
    gather value mass ((nextValue, nextMass) : remaining)
        | value == nextValue = gather value (mass + nextMass) remaining
        | otherwise = (value, mass) : gather nextValue nextMass remaining

expectLeft :: String -> (error -> Bool) -> Either error value -> IO ()
expectLeft _ predicate (Left err) = assert "unexpected error constructor" (predicate err)
expectLeft label _ (Right _) = fail (label ++ " was accepted")

unique :: (Eq value) => [value] -> [value]
unique = foldl (\values value -> if value `elem` values then values else values ++ [value]) []
