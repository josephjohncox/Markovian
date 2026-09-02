module ClarkScarf1960 (runClarkScarf1960Tests) where

import Data.List (isInfixOf)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio ((%))
import Markovian.Action (actionId)
import Markovian.Benchmark.Inventory.ClarkScarf1960.Finite.Exact
import Markovian.Benchmark.Inventory.ClarkScarf1960.Oracle.Exact
import Markovian.Benchmark.Inventory.ClarkScarf1960.Report
import Markovian.Category.Finite.Set (finiteSet, finiteSetValues, sameFiniteLayout, sameFiniteSet)
import Markovian.MDP.Exact (exactSuccessorState, exactTransitionReward, stepExactMDP)
import Markovian.Objective.Exact (ExactDiscount, mkExactDiscount)
import Markovian.Probability.Exact (exactOutcomes, exactProbability)
import Markovian.Reward.Exact (exactRewardValue)
import Utf8Golden (readUtf8File)

runClarkScarf1960Tests :: (String -> IO () -> IO ()) -> IO ()
runClarkScarf1960Tests run = do
    run "Clark-Scarf source equation (1) finite expectation" testNaturalCost
    run "Clark-Scarf equation (14) timing and lead times" testTiming
    run "Clark-Scarf conservation, backlog, and reachable closure" testConservation
    run "Clark-Scarf same-demand reward/successor correlation" testCorrelation
    run "Clark-Scarf three exact oracle paths and zero regret" testThreePaths
    run "Clark-Scarf equations (20)-(21) opportunity-loss independence" testOpportunityLoss
    run "Clark-Scarf clipped downstream target" testClipping
    run "Clark-Scarf setup-cost ordering distinction" testSetupCost
    run "Clark-Scarf finite layout checks" testLayouts
    run "Clark-Scarf malformed input and budget rejection" testRejections
    run "Clark-Scarf isolated-target work includes negative positions" testNegativePositionWorkBound
    run "Clark-Scarf widened-cap diagnostics" testWidening
    run "Clark-Scarf deterministic cited report" testReport
    run "Clark-Scarf benchmark sample contract" testBenchmarkCounts

assert :: String -> Bool -> IO ()
assert message condition = if condition then pure () else fail message

requireRight :: (Show error) => String -> Either error value -> IO value
requireRight _ (Right value) = pure value
requireRight label (Left err) = fail (label ++ ": " ++ show err)

mustRight :: (Show error) => Either error value -> value
mustRight (Right value) = value
mustRight (Left err) = error (show err)

unitDiscount :: ExactDiscount
unitDiscount = mustRight (mkExactDiscount 1)

fixtureDemand :: ClarkScarfDemand
fixtureDemand = mustRight (conditionedClarkScarfDemand (3 % 4) [(0, 1 % 2), (1, 1 % 2)])

parameterInput :: Integer -> Integer -> Rational -> ClarkScarfParameterInput
parameterInput orderCap targetCap setup =
    ClarkScarfParameterInput
        { inputClarkScarfHorizon = 3
        , inputClarkScarfDiscount = unitDiscount
        , inputClarkScarfOrderCap = orderCap
        , inputClarkScarfIsolatedTargetCap = targetCap
        , inputClarkScarfExternalSetupCost = setup
        , inputClarkScarfExternalUnitCost = 0
        , inputClarkScarfTransportUnitCost = 0
        , inputClarkScarfDownstreamHoldingCost = 1
        , inputClarkScarfDownstreamShortageCost = 4
        , inputClarkScarfUpstreamHoldingCost = 1
        , inputClarkScarfUpstreamShortageCost = 0
        , inputClarkScarfStateBudget = 100000
        , inputClarkScarfStateActionBudget = 3000000
        , inputClarkScarfSolverWorkBudget = 100000000000
        }

primaryParameters :: ClarkScarfParameters
primaryParameters = mustRight (validateClarkScarfParameters (parameterInput 2 4 0))

widenedParameters :: ClarkScarfParameters
widenedParameters = mustRight (validateClarkScarfParameters (parameterInput 3 5 0))

initialState :: ClarkScarfState
initialState = ClarkScarfState 3 0 0 1

primaryFixture :: ClarkScarfFixture
primaryFixture = mustRight (clarkScarfFixture primaryParameters fixtureDemand initialState)

widenedFixture :: ClarkScarfFixture
widenedFixture = mustRight (clarkScarfFixture widenedParameters fixtureDemand initialState)

primarySolution :: ClarkScarfSolution
primarySolution = mustRight (solveClarkScarf primaryFixture)

widenedSolution :: ClarkScarfSolution
widenedSolution = mustRight (solveClarkScarf widenedFixture)

testNaturalCost :: IO ()
testNaturalCost = do
    let expected = clarkScarfExpectedNaturalCost fixtureDemand 2 5 1
        realized = sum [mass * clarkScarfNaturalCost 2 5 1 demand | (demand, mass) <- NonEmpty.toList (clarkScarfDemandOutcomes fixtureDemand)]
    assert "finite equation-(1) expectation changed" (expected == 2 && realized == expected)
    assert "backlog shortage cost changed" (clarkScarfNaturalCost 2 5 (-2) 1 == 15)

testTiming :: IO ()
testTiming = do
    parameters <-
        requireRight
            "timing parameters"
            ( validateClarkScarfParameters
                (parameterInput 2 4 3)
                    { inputClarkScarfHorizon = 1
                    , inputClarkScarfExternalUnitCost = 2
                    , inputClarkScarfTransportUnitCost = 5
                    , inputClarkScarfDownstreamHoldingCost = 2
                    , inputClarkScarfDownstreamShortageCost = 7
                    , inputClarkScarfUpstreamHoldingCost = 1
                    , inputClarkScarfUpstreamShortageCost = 11
                    }
            )
    let state = ClarkScarfState 1 1 2 5
        action = ClarkScarfAction 2 4
    (successor, cost) <- requireRight "timing transition" (clarkScarfTransition parameters state action 3)
    assert "two-period downstream timing changed" (clarkScarfDownstreamNetStock successor == 0 && clarkScarfDownstreamInTransit successor == 1)
    assert "one-period upstream timing changed" (clarkScarfEchelonTwoStock successor == 4)
    assert "period count changed" (clarkScarfPeriodsRemaining successor == 0)
    assert "equation-(14) realized cost changed" (cost == 33)
    assert "state invariant was not preserved" (validClarkScarfState successor)

testConservation :: IO ()
testConservation =
    mapM_ checkState (NonEmpty.toList (clarkScarfReachableStates primaryFixture))
  where
    parameters = clarkScarfFixtureParameters primaryFixture
    checkState state = do
        actions <- requireRight "checked action enumeration" (clarkScarfActions parameters state)
        mapM_
            ( \action ->
                mapM_
                    ( \(demand, _) -> do
                        (successor, _) <- requireRight "reachable transition" (clarkScarfTransition parameters state action demand)
                        let before = clarkScarfEchelonTwoStock state
                            after = clarkScarfEchelonTwoStock successor
                        assert "external order/echelon conservation changed" (after == before + toInteger (clarkScarfExternalOrder action) - toInteger demand)
                        assert "reachable successor violated invariant" (validClarkScarfState successor)
                        assert "reachable support redirected or omitted a successor" (successor `elem` NonEmpty.toList (clarkScarfReachableStates primaryFixture))
                    )
                    (NonEmpty.toList (clarkScarfDemandOutcomes fixtureDemand))
            )
            actions

testCorrelation :: IO ()
testCorrelation = do
    let state = ClarkScarfState 1 0 0 1
        action = ClarkScarfAction 0 0
        onePeriodParameters = mustRight (validateClarkScarfParameters (parameterInput 1 2 0){inputClarkScarfHorizon = 1})
        asymmetricDemand = mustRight (conditionedClarkScarfDemand 1 [(0, 1 % 3), (1, 2 % 3)])
        fixture = mustRight (clarkScarfFixture onePeriodParameters asymmetricDemand state)
    distribution <- requireRight "correlated MDP transition" (stepExactMDP (clarkScarfMDP fixture) state (actionId action))
    let outcomes = NonEmpty.toList (exactOutcomes distribution)
    assert "demand outcomes were marginalized" (length outcomes == 2)
    mapM_
        ( \(outcome, probability) -> do
            let successor = exactSuccessorState outcome
                inferredDemand = negate (clarkScarfDownstreamNetStock successor)
                expectedCost = mustRight (clarkScarfTransition onePeriodParameters state action (fromInteger inferredDemand))
            assert "successor probability changed" (exactProbability probability `elem` [1 % 3, 2 % 3])
            assert "reward was paired with a different demand" (negate (exactRewardValue (exactTransitionReward outcome)) == snd expectedCost)
        )
        outcomes
    let rewardMean = sum [exactProbability probability * exactRewardValue (exactTransitionReward outcome) | (outcome, probability) <- outcomes]
        successorMean = sum [exactProbability probability * fromInteger (clarkScarfDownstreamNetStock (exactSuccessorState outcome)) | (outcome, probability) <- outcomes]
        pairedMoment = sum [exactProbability probability * exactRewardValue (exactTransitionReward outcome) * fromInteger (clarkScarfDownstreamNetStock (exactSuccessorState outcome)) | (outcome, probability) <- outcomes]
    assert "independent reward/successor marginals reproduced the joint moment" (pairedMoment /= rewardMean * successorMean)

testThreePaths :: IO ()
testThreePaths = do
    assert "three-path maximum differential was nonzero" (clarkScarfMaximumDifferential primarySolution == 0)
    assert "decomposed policy had exact regret" (clarkScarfInitialRegret primarySolution == 0)
    assert "fixture initial exact cost changed" (clarkScarfInitialCost primarySolution == 21 % 2)
    mapM_
        ( \check -> do
            assert "generic MDP and equation (14) differ" (clarkScarfGenericMDPCost check == clarkScarfEquation14Cost check)
            assert "equations (15)/(26) decomposition differs" (clarkScarfEquation14Cost check == clarkScarfDecomposedCost check)
            assert "decomposed policy differs in joint model" (clarkScarfEquation14Cost check == clarkScarfDecomposedPolicyCost check)
        )
        (NonEmpty.toList (clarkScarfStateChecks primarySolution))

testOpportunityLoss :: IO ()
testOpportunityLoss = do
    canonical <- requireRight "canonical opportunity loss" (clarkScarfEquation21OpportunityLoss primaryParameters fixtureDemand 3 0 0 0)
    alternate <- requireRight "alternate opportunity loss" (clarkScarfEquation21OpportunityLoss primaryParameters fixtureDemand 3 (-1) 0 0)
    assert "fixture did not exercise positive opportunity loss" (canonical == 9 % 2)
    assert "equation-(21) loss depended on (x1,w1)" (alternate == canonical)
    case clarkScarfEquation21OpportunityLoss primaryParameters fixtureDemand 3 1 1 1 of
        Left (ClarkScarfInvalidState _) -> pure ()
        result -> fail ("invalid opportunity-loss state accepted: " ++ show result)

testClipping :: IO ()
testClipping = do
    let initialCheck = NonEmpty.last (clarkScarfStateChecks primarySolution)
        isolatedTarget = clarkScarfEquation15Target primaryParameters fixtureDemand 3 0 0
    assert "isolated target fixture changed" (isolatedTarget == 2)
    case clarkScarfDecomposedAction initialCheck of
        Nothing -> fail "initial decomposed action missing"
        Just action -> assert "downstream isolated target was not clipped to x2" (clarkScarfDispatchTarget action == 1)

testSetupCost :: IO ()
testSetupCost = do
    zero <- solutionAtSetup 0
    positive <- solutionAtSetup 2
    assert "setup fixture lost zero-setup one-unit order" (initialOrder zero == 1)
    assert "positive setup did not produce a batched order" (initialOrder positive == 2)
  where
    solutionAtSetup setup = do
        parameters <- requireRight "setup parameters" (validateClarkScarfParameters (parameterInput 4 5 setup){inputClarkScarfUpstreamShortageCost = 4})
        fixture <- requireRight "setup fixture" (clarkScarfFixture parameters (mustRight (conditionedClarkScarfDemand 1 [(0, 1 % 2), (1, 1 % 2)])) (ClarkScarfState 3 0 0 0))
        requireRight "setup solution" (solveClarkScarf fixture)
    initialOrder solution =
        maybe
            0
            clarkScarfExternalOrder
            (clarkScarfDecomposedAction (NonEmpty.last (clarkScarfStateChecks solution)))

testLayouts :: IO ()
testLayouts = do
    let layout = clarkScarfStateLayout primaryFixture
        values = finiteSetValues layout
    reordered <- requireRight "reordered state layout" (finiteSet (reverse values))
    assert "state support changed under reorder" (sameFiniteSet layout reordered)
    assert "represented state layout ignored order" (not (sameFiniteLayout layout reordered))
    assert "action layout contains a duplicate" (length (finiteSetValues (clarkScarfActionLayout primaryFixture)) == length (unique (finiteSetValues (clarkScarfActionLayout primaryFixture))))

testRejections :: IO ()
testRejections = do
    case conditionedClarkScarfDemand 1 [(0, 1 % 2), (0, 1 % 2)] of
        Left (ClarkScarfDuplicateDemand 0) -> pure ()
        result -> fail ("duplicate demand accepted: " ++ show result)
    case conditionedClarkScarfDemand 1 [(0, 1 % 3)] of
        Left (ClarkScarfDemandMassNotOne total) | total == 1 % 3 -> pure ()
        result -> fail ("nonnormalized demand accepted: " ++ show result)
    case validateClarkScarfParameters (parameterInput (-1) 4 0) of
        Left (ClarkScarfNegativeOrderCap (-1)) -> pure ()
        result -> fail ("negative order cap accepted: " ++ show result)
    case clarkScarfFixture primaryParameters fixtureDemand (ClarkScarfState 3 1 1 1) of
        Left (ClarkScarfInvalidState _) -> pure ()
        result -> fail ("invalid invariant accepted: " ++ show result)
    case clarkScarfActions primaryParameters (ClarkScarfState 1 0 0 10000000) of
        Left (ClarkScarfStateActionBudgetExceeded count) ->
            assert "action preflight count did not cross limit" (count > clarkScarfStateActionBudget primaryParameters)
        result -> fail ("oversized action layout was allocated: " ++ show result)
    tinyStateParameters <- requireRight "tiny state budget" (validateClarkScarfParameters (parameterInput 2 4 0){inputClarkScarfStateBudget = 1})
    case clarkScarfFixture tinyStateParameters fixtureDemand initialState of
        Left (ClarkScarfStateBudgetExceeded count) -> assert "state budget count did not cross limit" (count > 1)
        result -> fail ("state budget accepted: " ++ show result)
    tinySolverParameters <- requireRight "tiny solver budget" (validateClarkScarfParameters (parameterInput 2 4 0){inputClarkScarfSolverWorkBudget = 1})
    tinyFixture <- requireRight "tiny solver fixture" (clarkScarfFixture tinySolverParameters fixtureDemand initialState)
    case solveClarkScarf tinyFixture of
        Left (ClarkScarfSolverWorkBudgetExceeded count) -> assert "solver budget count did not cross limit" (count > 1)
        result -> fail ("solver budget accepted: " ++ show result)

testNegativePositionWorkBound :: IO ()
testNegativePositionWorkBound = do
    let input =
            (parameterInput 0 0 0)
                { inputClarkScarfHorizon = 1
                , inputClarkScarfSolverWorkBudget = 1000000000
                }
        demand = mustRight (conditionedClarkScarfDemand 1 [(0, 1)])
        state = ClarkScarfState 1 (-10000) 0 (-10000)
    generousParameters <- requireRight "negative-position parameters" (validateClarkScarfParameters input)
    generousFixture <- requireRight "negative-position fixture" (clarkScarfFixture generousParameters demand state)
    baseline <- requireRight "negative-position solve" (solveClarkScarf generousFixture)
    let checked = clarkScarfCheckedWork baseline
    assert "negative inventory position was omitted from isolated-target work" (checked > 10000)
    exactParameters <- requireRight "exact-work parameters" (validateClarkScarfParameters input{inputClarkScarfSolverWorkBudget = toInteger checked})
    exactFixture <- requireRight "exact-work fixture" (clarkScarfFixture exactParameters demand state)
    exact <- requireRight "exact-work solve" (solveClarkScarf exactFixture)
    assert "exact-work acceptance changed reported work" (clarkScarfCheckedWork exact == checked)
    belowParameters <- requireRight "one-below parameters" (validateClarkScarfParameters input{inputClarkScarfSolverWorkBudget = toInteger checked - 1})
    belowFixture <- requireRight "one-below fixture" (clarkScarfFixture belowParameters demand state)
    case solveClarkScarf belowFixture of
        Left (ClarkScarfSolverWorkBudgetExceeded required) -> assert "one-below rejection lost required work" (required == checked)
        result -> fail ("one-below work returned a partial solution: " ++ show result)

testWidening :: IO ()
testWidening = do
    stability <- requireRight "widened comparison" (compareClarkScarfBounds primarySolution widenedSolution)
    assert ("widened caps were not stable: " ++ show stability) (clarkScarfBoundsStable stability)
    assert "widened order cap was selected" (not (clarkScarfWidenedOrderCapSelected stability))
    assert "widened target cap was selected" (not (clarkScarfWidenedTargetCapSelected stability))
    case compareClarkScarfBounds primarySolution primarySolution of
        Left ClarkScarfBoundsNotWidened -> pure ()
        result -> fail ("non-widened evidence accepted: " ++ show result)

testReport :: IO ()
testReport = do
    report <- requireRight "Clark-Scarf report" (clarkScarfReport primarySolution widenedSolution)
    repeated <- requireRight "repeated Clark-Scarf report" (clarkScarfReport primarySolution widenedSolution)
    let rendered = renderClarkScarfReport report
    golden <- readUtf8File "test/golden/clark-scarf-1960-report.txt"
    assert "report was nondeterministic" (report == repeated)
    assert "report differs from golden" (rendered == golden)
    mapM_
        (\field -> assert ("report omitted: " ++ field) (field `isInfixOf` rendered))
        [ "Clark–Scarf (1960), Section III, finite lattice specialization."
        , "DOI 10.1287/mnsc.6.4.475"
        , "b64d82098b47dffa7cc4b87a4bbc6c833bb90295ccbede0a1897c8af44956239"
        , "equations (15)/(20)/(21)/(26) decomposition"
        , "not a published numeric reproduction"
        , "no state clamping"
        , "widened-bound diagnostic: stable and nonbinding"
        , "not an unbounded proof"
        ]

testBenchmarkCounts :: IO ()
testBenchmarkCounts = do
    assert "warm-up count changed" (clarkScarfBenchmarkWarmupCount == 1)
    assert "sample count changed" (clarkScarfBenchmarkSampleCount == 20)

unique :: (Eq value) => [value] -> [value]
unique = foldl (\values value -> if value `elem` values then values else values ++ [value]) []
