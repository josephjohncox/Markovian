module Main (main) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio ((%))
import Markovian.Horizon (
    HorizonError (..),
    horizonValue,
    mkHorizon,
 )
import Markovian.Interpreter.Exact (
    ExactEvaluationError (..),
    ExactTraceResult (..),
    exactTraceDistribution,
    expectedExactReturn,
    expectedExactReturnFrom,
 )
import Markovian.Interpreter.Sampled (
    SampledEvaluationError (..),
    SampledResult (..),
    sampleReturn,
 )
import Markovian.Kernel (kernel)
import Markovian.Kernel.Exact (
    composeExactKernel,
    exactDeterministic,
    exactKernel,
    runExactKernel,
 )
import Markovian.MDP (
    Decision (..),
    MDP,
    ModelError (..),
    actionId,
    actionValue,
    inspectMDP,
    mdp,
    stepMDP,
 )
import Markovian.MDP.Exact (
    ExactMDP,
    ExactModelError (..),
    ExactStateStatus (..),
    exactMDP,
    exactTransitionOutcome,
 )
import Markovian.MRP (
    MRPStep (..),
    StateStatus (..),
    mrp,
    stepMRP,
    successorState,
    transitionOutcome,
    transitionReward,
 )
import Markovian.Objective (
    DiscountError (..),
    asContractionDiscount,
    contractionDiscountValue,
    discountValue,
    finiteObjective,
    mkContractionDiscount,
    mkDiscount,
 )
import Markovian.Objective.Exact (
    ExactDiscountError (..),
    ExactFiniteObjective,
    asExactContractionDiscount,
    exactContractionDiscountValue,
    exactDiscountValue,
    exactFiniteObjective,
    mkExactContractionDiscount,
    mkExactDiscount,
 )
import Markovian.Policy (
    ConditionalRewardError (..),
    PolicyError (..),
    closePolicy,
    conditionalExpectedReward,
    policy,
    policyActions,
    stepPolicyMRP,
 )
import Markovian.Policy.Exact (
    ExactConditionalRewardError (..),
    ExactPolicyError (..),
    closeExactPolicy,
    exactConditionalExpectedReward,
    exactPolicy,
 )
import Markovian.Probability (
    DistributionError (..),
    ProbabilityError (..),
    WeightError (..),
    dirac,
    finiteDist,
    mkProb,
    mkWeight,
    outcomes,
    probability,
    weight,
 )
import Markovian.Probability.Exact (
    ExactDistributionError (..),
    ExactProbabilityError (..),
    ExactWeightError (..),
    bindExactFiniteDist,
    exactDirac,
    exactFiniteDist,
    exactOutcomes,
    exactProbability,
    exactWeight,
    mkExactProb,
    mkExactWeight,
 )
import Markovian.Reward (
    RewardError (..),
    mkReward,
    rewardValue,
 )
import Markovian.Reward.Exact (exactReward, exactRewardValue)
import Markovian.Sampling (
    generatorFromSeed,
    generatorState,
    sampleExactFiniteDist,
    sampleFiniteDist,
 )
import Markovian.Trace (
    StopReason (..),
    Trace (..),
    TraceStep (..),
 )
import System.Exit (exitFailure)

main :: IO ()
main = do
    run "probability and weight validation" testValidation
    run "empty support rejection" testEmptySupport
    run "overflow-safe normalization" testNormalization
    run "normalization removes rounded-zero mass" testRoundedZeroMass
    run "terminal reward timing" testTerminalRewardTiming
    run "self-loop step remains one layer" testSelfLoop
    run "actions are separate from transition outcomes" testActionOutcomeSeparation
    run "floating policy closure and validation" testFloatingPolicyClosure
    run "exact policy closure and observables" testExactPolicyClosure
    run "exact evaluator terminal and horizon zero" testExactEvaluatorBoundaries
    run "exact evaluator reward timing and discount" testExactEvaluatorRewardTiming
    run "exact evaluator weighted support and errors" testExactEvaluatorWeighted
    run "exact evaluator bounded self-loop" testExactEvaluatorSelfLoop
    run "seeded finite-support sampling" testFiniteSampling
    run "sampled interpreter boundaries and traces" testSampledInterpreter
    run "exact trace expectation" testExactTraceExpectation
    run "exact probability and reward values" testExactValues
    run "exact finite distribution functor laws" testExactFunctorLaws
    run "exact kernel Kleisli laws" testExactKernelLaws
    run "floating objective values" testFloatingObjectives
    run "exact objective values" testExactObjectives
    run "horizon values" testHorizons

run :: String -> IO () -> IO ()
run name test = do
    test
    putStrLn ("PASS: " ++ name)

failTest :: String -> IO a
failTest message = do
    putStrLn ("FAIL: " ++ message)
    exitFailure

assert :: String -> Bool -> IO ()
assert message condition =
    if condition then pure () else failTest message

requireRight :: (Show err) => String -> Either err value -> IO value
requireRight _ (Right value) = pure value
requireRight label (Left err) = failTest (label ++ ": " ++ show err)

testValidation :: IO ()
testValidation = do
    validProbability <- requireRight "valid probability" (mkProb 0.25)
    assert "mkProb must preserve a valid value" (probability validProbability == 0.25)

    zeroProbability <- requireRight "negative-zero probability" (mkProb (-0.0))
    zeroWeight <- requireRight "negative-zero weight" (mkWeight (-0.0))
    zeroReward <- requireRight "negative-zero reward" (mkReward (-0.0))
    assert "probability must canonicalize negative zero" (not (isNegativeZero (probability zeroProbability)))
    assert "weight must canonicalize negative zero" (not (isNegativeZero (weight zeroWeight)))
    assert "reward must canonicalize negative zero" (not (isNegativeZero (rewardValue zeroReward)))

    case mkProb (0 / 0) of
        Left (NonFiniteProbability _) -> pure ()
        result -> failTest ("NaN probability was not rejected: " ++ show result)

    case mkProb 1.01 of
        Left (ProbabilityOutOfRange _) -> pure ()
        result -> failTest ("out-of-range probability was not rejected: " ++ show result)

    case mkProb (-0.01) of
        Left (ProbabilityOutOfRange _) -> pure ()
        result -> failTest ("negative probability was not rejected: " ++ show result)

    case mkWeight (-1) of
        Left (NegativeWeight _) -> pure ()
        result -> failTest ("negative weight was not rejected: " ++ show result)

    case mkWeight (1 / 0) of
        Left (NonFiniteWeight _) -> pure ()
        result -> failTest ("infinite weight was not rejected: " ++ show result)

    case finiteDist [('x', -1)] of
        Left (InvalidWeight 0 (NegativeWeight _)) -> pure ()
        result -> failTest ("distribution did not report its invalid weight: " ++ show result)

    case finiteDist [('x', 1 / 0)] of
        Left (InvalidWeight 0 (NonFiniteWeight _)) -> pure ()
        result -> failTest ("distribution did not report its non-finite weight: " ++ show result)

    case finiteDist [('x', 0), ('y', 0)] of
        Left ZeroTotalWeight -> pure ()
        result -> failTest ("zero-total distribution was not rejected: " ++ show result)

    case mkReward (0 / 0) of
        Left (NonFiniteReward _) -> pure ()
        result -> failTest ("NaN reward was not rejected: " ++ show result)

    case mkReward (1 / 0) of
        Left (NonFiniteReward _) -> pure ()
        result -> failTest ("infinite reward was not rejected: " ++ show result)

testEmptySupport :: IO ()
testEmptySupport = do
    case finiteDist ([] :: [(Int, Double)]) of
        Left EmptySupport -> pure ()
        result -> failTest ("empty distribution support was not rejected: " ++ show result)

    zeroReward <- requireRight "zero reward" (mkReward 0)
    let emptyActionModel :: MDP TestState TestAction
        emptyActionModel =
            mdp
                Start
                (const Continuing)
                (const [])
                (kernel (\_ -> dirac (transitionOutcome zeroReward End)))
    case inspectMDP emptyActionModel Start of
        Left EmptyActionSupport -> pure ()
        result -> failTest ("empty MDP action support was not rejected: " ++ show result)

testNormalization :: IO ()
testNormalization = do
    weighted <- requireRight "weights 1 and 3" (finiteDist [('a', 1), ('b', 3)])
    let masses = fmap (probability . snd) (NonEmpty.toList (outcomes weighted))
    assert "weights 1 and 3 must normalize to 0.25 and 0.75" (masses == [0.25, 0.75])

    let largest = maximumFiniteDouble
    overflowPair <- requireRight "two maximum finite weights" (finiteDist [('a', largest), ('b', largest)])
    let overflowMasses = fmap (probability . snd) (NonEmpty.toList (outcomes overflowPair))
    assert "maximum finite weights must normalize without overflow" (overflowMasses == [0.5, 0.5])

testRoundedZeroMass :: IO ()
testRoundedZeroMass = do
    let smallest = minimumPositiveDouble
        largest = maximumFiniteDouble
    extreme <- requireRight "extreme finite weights" (finiteDist [('s', smallest), ('l', largest)])
    let retained = NonEmpty.toList (outcomes extreme)
    assert "the minimum positive Double must be positive" (smallest > 0)
    assert "the extreme scaled weight must round to zero" (smallest / largest == 0)
    assert "every retained outcome must have positive mass" (all ((> 0) . probability . snd) retained)
    assert "an outcome whose normalized mass rounds to zero must be removed" (fmap fst retained == ['l'])

testTerminalRewardTiming :: IO ()
testTerminalRewardTiming = do
    stepReward <- requireRight "transition reward" (mkReward 2)
    terminalPayoff <- requireRight "terminal payoff" (mkReward 7)
    let status Start = Continuing
        status End = Terminal terminalPayoff
        process =
            mrp
                Start
                status
                (kernel (\_ -> dirac (transitionOutcome stepReward End)))

    case stepMRP process Start of
        TerminalStep _ -> failTest "continuing state was reported as terminal"
        TransitionStep distribution ->
            case NonEmpty.toList (outcomes distribution) of
                [(outcome, _)] -> do
                    assert "transition reward changed" (rewardValue (transitionReward outcome) == 2)
                    assert "transition successor changed" (successorState outcome == End)
                values -> failTest ("expected one transition outcome, got " ++ show (length values))

    let terminalOnly = mrp End status (kernel (\_ -> error "terminal kernel was evaluated"))
    case stepMRP terminalOnly End of
        TerminalStep payoff ->
            assert "terminal payoff must be returned at the terminal boundary" (rewardValue payoff == 7)
        TransitionStep _ -> failTest "terminal state requested a transition"

testSelfLoop :: IO ()
testSelfLoop = do
    loopReward <- requireRight "self-loop reward" (mkReward 1)
    let process =
            mrp
                Start
                (const Continuing)
                (kernel (\state -> dirac (transitionOutcome loopReward state)))
    case stepMRP process Start of
        TerminalStep _ -> failTest "self-loop was reported as terminal"
        TransitionStep distribution ->
            case fmap fst (NonEmpty.toList (outcomes distribution)) of
                [outcome] -> assert "self-loop successor changed" (successorState outcome == Start)
                values -> failTest ("expected one self-loop outcome, got " ++ show (length values))

testActionOutcomeSeparation :: IO ()
testActionOutcomeSeparation = do
    lowReward <- requireRight "low reward" (mkReward 1)
    highReward <- requireRight "high reward" (mkReward 3)
    terminalPayoff <- requireRight "terminal reward" (mkReward 5)
    stochasticOutcomes <-
        requireRight
            "stochastic transition outcomes"
            ( finiteDist
                [ (transitionOutcome lowReward End, 1)
                , (transitionOutcome highReward End, 1)
                ]
            )
    let finish = actionId Finish
        status Start = Continuing
        status End = Terminal terminalPayoff
        model =
            mdp
                Start
                status
                (\state -> case state of Start -> [finish]; End -> [])
                (kernel (\_ -> stochasticOutcomes))
        controller = policy (kernel (const (dirac finish)))

    case inspectMDP model Start of
        Right (ActionDecision available) ->
            assert "one controllable action was expected" (NonEmpty.toList available == [finish])
        result -> failTest ("available action inspection failed: " ++ show result)

    assert
        "the policy must distribute over action IDs"
        (fmap fst (NonEmpty.toList (outcomes (policyActions controller Start))) == [finish])

    case stepMDP model Start finish of
        Left err -> failTest ("available action was rejected: " ++ show err)
        Right distribution -> do
            let transitionResults = fmap fst (NonEmpty.toList (outcomes distribution))
            assert "one action must be able to have two stochastic outcomes" (length transitionResults == 2)
            assert "both outcomes must retain their successor" (all ((== End) . successorState) transitionResults)
            assert
                "outcome rewards must remain stochastic outcomes, not action IDs"
                (fmap (rewardValue . transitionReward) transitionResults == [1, 3])

    case inspectMDP model End of
        Right (TerminalDecision payoff) ->
            assert "terminal decision must expose terminal payoff" (rewardValue payoff == 5)
        result -> failTest ("terminal MDP state was not identified: " ++ show result)

    case stepMDP model End finish of
        Left (ActionRequestedAtTerminal payoff) ->
            assert "terminal action error must retain terminal payoff" (rewardValue payoff == 5)
        result -> failTest ("terminal transition request was not rejected: " ++ show result)

testExactEvaluatorBoundaries :: IO ()
testExactEvaluatorBoundaries = do
    objective <- makeExactObjective 0 (1 % 2)
    let terminalModel :: ExactMDP EvalState EvalAction
        terminalModel =
            exactMDP
                EvalTerminal
                (\state -> case state of EvalTerminal -> ExactTerminal (exactReward 9); _ -> ExactContinuing)
                (\_ -> error "horizon-zero or terminal action support was evaluated")
                (exactKernel (\_ -> error "horizon-zero or terminal transition was evaluated"))
        unusedPolicy = exactPolicy (exactKernel (\_ -> error "horizon-zero or terminal policy was evaluated"))
    terminalValue <- requireRight "exact terminal boundary" (expectedExactReturn objective terminalModel unusedPolicy)
    assert "terminal payoff must survive horizon zero" (exactRewardValue terminalValue == 9)
    continuingValue <- requireRight "exact continuing horizon zero" (expectedExactReturnFrom objective terminalModel unusedPolicy EvalStart)
    assert "continuing horizon zero must return zero" (exactRewardValue continuingValue == 0)

testExactEvaluatorRewardTiming :: IO ()
testExactEvaluatorRewardTiming = do
    halfObjective <- makeExactObjective 1 (1 % 2)
    zeroDiscountObjective <- makeExactObjective 1 0
    let onlyAction = actionId EvalOnly
        model :: ExactMDP EvalState EvalAction
        model =
            exactMDP
                EvalStart
                (\state -> case state of EvalTerminal -> ExactTerminal (exactReward 7); _ -> ExactContinuing)
                (\state -> case state of EvalTerminal -> []; _ -> [onlyAction])
                (exactKernel (\_ -> exactDirac (exactTransitionOutcome (exactReward 2) EvalTerminal)))
        selectedPolicy = exactPolicy (exactKernel (const (exactDirac onlyAction)))
    discounted <- requireRight "discounted exact return" (expectedExactReturn halfObjective model selectedPolicy)
    assert "transition and terminal rewards must each occur once" (exactRewardValue discounted == 11 % 2)
    immediate <- requireRight "zero-discount exact return" (expectedExactReturn zeroDiscountObjective model selectedPolicy)
    assert "zero discount must retain only immediate reward" (exactRewardValue immediate == 2)

testExactEvaluatorWeighted :: IO ()
testExactEvaluatorWeighted = do
    objective <- makeExactObjective 1 1
    selected <- requireRight "weighted exact evaluator policy" (exactFiniteDist [(lowAction, 1), (highAction, 3)])
    highOutcomes <-
        requireRight
            "weighted exact evaluator transition"
            ( exactFiniteDist
                [ (exactTransitionOutcome (exactReward 4) EvalTerminal, 1)
                , (exactTransitionOutcome (exactReward 8) EvalTerminal, 1)
                ]
            )
    let model :: ExactMDP EvalState EvalAction
        model =
            exactMDP
                EvalStart
                (\state -> case state of EvalTerminal -> ExactTerminal (exactReward 0); _ -> ExactContinuing)
                (\state -> case state of EvalTerminal -> []; _ -> [lowAction, highAction])
                ( exactKernel
                    ( \(_, selectedAction) ->
                        case actionValue selectedAction of
                            EvalLow -> exactDirac (exactTransitionOutcome (exactReward 2) EvalTerminal)
                            EvalHigh -> highOutcomes
                            EvalOnly -> error "unavailable exact evaluator action reached transition"
                            EvalMissing -> error "missing exact evaluator action reached transition"
                    )
                )
        selectedPolicy = exactPolicy (exactKernel (const selected))
    result <- requireRight "weighted exact expected return" (expectedExactReturn objective model selectedPolicy)
    assert "weighted exact expected return changed" (exactRewardValue result == 5)

    let missingAction = actionId EvalMissing
        unavailablePolicy = exactPolicy (exactKernel (const (exactDirac missingAction)))
    case expectedExactReturn objective model unavailablePolicy of
        Left (ExactEvaluationPolicyError (ExactPolicyUnavailableAction unavailable)) ->
            assert "evaluator unavailable action changed" (unavailable == missingAction)
        unexpected -> failTest ("evaluator did not retain policy error: " ++ show unexpected)

    let duplicateModel =
            exactMDP
                EvalStart
                (const ExactContinuing)
                (const [lowAction, lowAction])
                (exactKernel (\_ -> exactDirac (exactTransitionOutcome (exactReward 0) EvalTerminal)))
    case expectedExactReturn objective duplicateModel selectedPolicy of
        Left (ExactEvaluationModelError (DuplicateExactModelAction duplicate)) ->
            assert "evaluator duplicate model action changed" (duplicate == lowAction)
        unexpected -> failTest ("evaluator did not retain model error: " ++ show unexpected)
  where
    lowAction = actionId EvalLow
    highAction = actionId EvalHigh

testExactEvaluatorSelfLoop :: IO ()
testExactEvaluatorSelfLoop = do
    objective <- makeExactObjective 3 (1 % 2)
    let onlyAction = actionId EvalOnly
        model :: ExactMDP EvalState EvalAction
        model =
            exactMDP
                EvalLoop
                (const ExactContinuing)
                (const [onlyAction])
                (exactKernel (\_ -> exactDirac (exactTransitionOutcome (exactReward 1) EvalLoop)))
        selectedPolicy = exactPolicy (exactKernel (const (exactDirac onlyAction)))
    result <- requireRight "bounded exact self-loop" (expectedExactReturn objective model selectedPolicy)
    assert "bounded exact self-loop return changed" (exactRewardValue result == 7 % 4)

testFiniteSampling :: IO ()
testFiniteSampling = do
    floating <- requireRight "floating sampling support" (finiteDist [('a', 1), ('b', 3)])
    first <- requireRight "first floating sample" (sampleFiniteDist (generatorFromSeed 42) floating)
    second <- requireRight "second floating sample" (sampleFiniteDist (generatorFromSeed 42) floating)
    assert "equal floating seeds must produce equal samples and generator state" (first == second)
    sampled <-
        traverse
            (\seed -> requireRight "floating support sample" (sampleFiniteDist (generatorFromSeed seed) floating))
            [0 .. 100]
    assert "floating samples must remain in exposed support" (all ((`elem` "ab") . fst) sampled)

    exact <- requireRight "exact sampling support" (exactFiniteDist [('x', 1), ('y', 2)])
    exactFirst <- requireRight "first exact sample" (sampleExactFiniteDist (generatorFromSeed 99) exact)
    exactSecond <- requireRight "second exact sample" (sampleExactFiniteDist (generatorFromSeed 99) exact)
    assert "equal exact seeds must produce equal samples and generator state" (exactFirst == exactSecond)
    exactSamples <-
        traverse
            (\seed -> requireRight "exact support sample" (sampleExactFiniteDist (generatorFromSeed seed) exact))
            [0 .. 100]
    assert "exact samples must remain in exposed support" (all ((`elem` "xy") . fst) exactSamples)

    let initialGenerator = generatorFromSeed 7
    certain <- requireRight "certain sample" (sampleFiniteDist initialGenerator (dirac 'z'))
    assert "a certain sample must not consume generator state" (generatorState (snd certain) == generatorState initialGenerator)

testSampledInterpreter :: IO ()
testSampledInterpreter = do
    horizonOne <- requireRight "sample horizon one" (mkHorizon 1)
    horizonZero <- requireRight "sample horizon zero" (mkHorizon 0)
    horizonThree <- requireRight "sample horizon three" (mkHorizon 3)
    half <- requireRight "sample half discount" (mkDiscount 0.5)
    let objectiveOne = finiteObjective horizonOne half
        objectiveZero = finiteObjective horizonZero half
        objectiveThree = finiteObjective horizonThree half
        finish = actionId Finish
        initialGenerator = generatorFromSeed 123
    low <- requireRight "sample low reward" (mkReward 2)
    high <- requireRight "sample high reward" (mkReward 4)
    terminal <- requireRight "sample terminal payoff" (mkReward 7)
    transitions <-
        requireRight
            "sample weighted transition"
            ( finiteDist
                [ (transitionOutcome low End, 1)
                , (transitionOutcome high End, 1)
                ]
            )
    let status Start = Continuing
        status End = Terminal terminal
        model =
            mdp
                Start
                status
                (\state -> case state of Start -> [finish]; End -> [])
                (kernel (const transitions))
        selectedPolicy = policy (kernel (const (dirac finish)))

    sampled <- requireRight "sampled return" (sampleReturn objectiveOne model selectedPolicy initialGenerator)
    repeated <- requireRight "repeated sampled return" (sampleReturn objectiveOne model selectedPolicy initialGenerator)
    assert "equal seeds must produce equal traces, returns, and generator states" (sampled == repeated)
    assert "sampled weighted return left its support" (rewardValue (sampledReturn sampled) `elem` [5.5, 7.5])
    case sampledTrace sampled of
        Trace [TraceStep selected reward successor] stopState (TerminalStop payoff) -> do
            assert "sampled trace action changed" (selected == finish)
            assert "sampled trace reward left support" (rewardValue reward `elem` [2, 4])
            assert "sampled trace successor changed" (successor == End)
            assert "sampled trace stop state changed" (stopState == End)
            assert "sampled terminal payoff changed" (rewardValue payoff == 7)
        trace -> failTest ("sampled trace shape changed: " ++ show trace)

    let unusedPolicy = policy (kernel (\_ -> error "horizon-zero policy was evaluated"))
    stopped <- requireRight "sampled horizon zero" (sampleReturn objectiveZero model unusedPolicy initialGenerator)
    assert "continuing horizon zero must return zero" (rewardValue (sampledReturn stopped) == 0)
    case sampledTrace stopped of
        Trace [] Start HorizonStop -> pure ()
        trace -> failTest ("horizon-zero stop trace changed: " ++ show trace)

    let terminalModel :: MDP TestState TestAction
        terminalModel =
            mdp
                End
                status
                (\_ -> error "terminal action support was evaluated")
                (kernel (\_ -> error "terminal transition was evaluated"))
    terminalResult <- requireRight "sampled terminal boundary" (sampleReturn objectiveZero terminalModel unusedPolicy initialGenerator)
    assert "terminal payoff must survive sampled horizon zero" (rewardValue (sampledReturn terminalResult) == 7)
    assert
        "terminal evaluation must not consume generator state"
        (generatorState (sampledGenerator terminalResult) == generatorState initialGenerator)

    one <- requireRight "sample self-loop reward" (mkReward 1)
    let loopModel =
            mdp
                Start
                (const Continuing)
                (const [finish])
                (kernel (const (dirac (transitionOutcome one Start))))
    loopResult <- requireRight "sampled bounded self-loop" (sampleReturn objectiveThree loopModel selectedPolicy initialGenerator)
    assert "sampled bounded self-loop return changed" (rewardValue (sampledReturn loopResult) == 1.75)
    assert "sampled bounded self-loop trace length changed" (length (traceSteps (sampledTrace loopResult)) == 3)

    let unavailable = actionId MissingFinish
        unavailablePolicy = policy (kernel (const (dirac unavailable)))
    case sampleReturn objectiveOne model unavailablePolicy initialGenerator of
        Left (SampledPolicyError (PolicyUnavailableAction rejected)) ->
            assert "sampled unavailable action changed" (rejected == unavailable)
        result -> failTest ("sampled policy error was not retained: " ++ show result)

    let emptyModel = mdp Start (const Continuing) (const []) (kernel (const (dirac (transitionOutcome one Start))))
    case sampleReturn objectiveOne emptyModel selectedPolicy initialGenerator of
        Left (SampledModelError EmptyActionSupport) -> pure ()
        result -> failTest ("sampled model error was not retained: " ++ show result)

testExactTraceExpectation :: IO ()
testExactTraceExpectation = do
    objective <- makeExactObjective 1 1
    selected <- requireRight "exact trace policy" (exactFiniteDist [(lowAction, 1), (highAction, 3)])
    highOutcomes <-
        requireRight
            "exact trace transition"
            ( exactFiniteDist
                [ (exactTransitionOutcome (exactReward 4) EvalTerminal, 1)
                , (exactTransitionOutcome (exactReward 8) EvalTerminal, 1)
                ]
            )
    let model :: ExactMDP EvalState EvalAction
        model =
            exactMDP
                EvalStart
                (\state -> case state of EvalTerminal -> ExactTerminal (exactReward 0); _ -> ExactContinuing)
                (\state -> case state of EvalTerminal -> []; _ -> [lowAction, highAction])
                ( exactKernel
                    ( \(_, selectedAction) ->
                        case actionValue selectedAction of
                            EvalLow -> exactDirac (exactTransitionOutcome (exactReward 2) EvalTerminal)
                            EvalHigh -> highOutcomes
                            EvalOnly -> error "unavailable exact trace action"
                            EvalMissing -> error "missing exact trace action"
                    )
                )
        selectedPolicy = exactPolicy (exactKernel (const selected))
    direct <- requireRight "direct exact trace expectation" (expectedExactReturn objective model selectedPolicy)
    traces <- requireRight "exact trace distribution" (exactTraceDistribution objective model selectedPolicy)
    let tracedExpectation =
            sum
                [ exactProbability mass * exactRewardValue (exactTraceReturn result)
                | (result, mass) <- NonEmpty.toList (exactOutcomes traces)
                ]
        traceResults = fmap fst (NonEmpty.toList (exactOutcomes traces))
    assert "exact trace expectation must match direct evaluation" (tracedExpectation == exactRewardValue direct)
    assert "exact weighted trace expectation changed" (tracedExpectation == 5)
    assert "each exact bounded trace must contain one transition" (all ((== 1) . length . traceSteps . exactTrace) traceResults)
    assert
        "each exact trace must stop at the terminal state"
        ( all
            (\result -> traceStopState (exactTrace result) == EvalTerminal)
            traceResults
        )
  where
    lowAction = actionId EvalLow
    highAction = actionId EvalHigh

makeExactObjective :: Integer -> Rational -> IO ExactFiniteObjective
makeExactObjective rawHorizon rawDiscount = do
    horizon <- requireRight "exact evaluator horizon" (mkHorizon rawHorizon)
    discount <- requireRight "exact evaluator discount" (mkExactDiscount rawDiscount)
    pure (exactFiniteObjective horizon discount)

testFloatingPolicyClosure :: IO ()
testFloatingPolicyClosure = do
    lowReward <- requireRight "closure low reward" (mkReward 2)
    highReward <- requireRight "closure high reward" (mkReward 8)
    terminalPayoff <- requireRight "closure terminal payoff" (mkReward 11)
    selectedActions <- requireRight "floating policy actions" (finiteDist [(lowAction, 1), (highAction, 3)])
    duplicateActions <- requireRight "duplicate floating policy actions" (finiteDist [(lowAction, 1), (lowAction, 1)])
    let status ClosureStart = Continuing
        status ClosureMiddle = Continuing
        status ClosureEnd = Terminal terminalPayoff
        status ClosureMissing = Continuing
        transition =
            kernel
                ( \(_, selected) ->
                    case actionValue selected of
                        LowAction -> dirac (transitionOutcome lowReward ClosureEnd)
                        HighAction -> dirac (transitionOutcome highReward ClosureEnd)
                        MissingAction -> error "unavailable action reached transition kernel"
                )
        model =
            mdp
                ClosureStart
                status
                ( \state -> case state of
                    ClosureStart -> [lowAction, highAction]
                    ClosureMiddle -> [lowAction, highAction]
                    ClosureEnd -> []
                    ClosureMissing -> [lowAction]
                )
                transition
        closed = closePolicy model (policy (kernel (const selectedActions)))

    case stepPolicyMRP closed ClosureStart of
        Left err -> failTest ("floating policy closure failed: " ++ show err)
        Right (TerminalStep _) -> failTest "continuing closure returned a terminal step"
        Right (TransitionStep distribution) -> do
            let closedOutcomes = NonEmpty.toList (outcomes distribution)
            assert "closure must preserve two reward outcomes" (length closedOutcomes == 2)
            assert "closure must preserve the common successor" (all ((== ClosureEnd) . successorState . fst) closedOutcomes)
            assert "closure reward labels changed" (fmap (rewardValue . transitionReward . fst) closedOutcomes == [2, 8])
            assert "closure masses changed" (fmap (probability . snd) closedOutcomes == [0.25, 0.75])
            conditional <- requireRight "conditional closure reward" (conditionalExpectedReward distribution ClosureEnd)
            assert "conditional closure reward changed" (rewardValue conditional == 6.5)
            case conditionalExpectedReward distribution ClosureMissing of
                Left ZeroMassTransition -> pure ()
                result -> failTest ("zero-mass conditional reward was not rejected: " ++ show result)

    let terminalClosed = closePolicy model (policy (kernel (\_ -> error "terminal policy was evaluated")))
    case stepPolicyMRP terminalClosed ClosureEnd of
        Right (TerminalStep payoff) -> assert "terminal closure payoff changed" (rewardValue payoff == 11)
        result -> failTest ("terminal closure requested a policy: " ++ show result)

    let duplicateModel = mdp ClosureStart status (const [lowAction, lowAction]) transition
    case inspectMDP duplicateModel ClosureStart of
        Left (DuplicateAvailableAction duplicate) -> assert "duplicate model action changed" (duplicate == lowAction)
        result -> failTest ("duplicate available action was not rejected: " ++ show result)

    case stepPolicyMRP (closePolicy model (policy (kernel (const duplicateActions)))) ClosureStart of
        Left (DuplicatePolicyAction duplicate) -> assert "duplicate policy action changed" (duplicate == lowAction)
        result -> failTest ("duplicate policy action was not rejected: " ++ show result)

    case stepPolicyMRP (closePolicy model (policy (kernel (const (dirac missingAction))))) ClosureStart of
        Left (PolicyUnavailableAction unavailable) -> assert "unavailable policy action changed" (unavailable == missingAction)
        result -> failTest ("unavailable policy action was not rejected: " ++ show result)
  where
    lowAction = actionId LowAction
    highAction = actionId HighAction
    missingAction = actionId MissingAction

testExactPolicyClosure :: IO ()
testExactPolicyClosure = do
    selected <-
        requireRight
            "exact closure policy"
            (exactFiniteDist [(lowAction, 1), (highAction, 3)])
    duplicateSelected <-
        requireRight
            "duplicate exact closure policy"
            (exactFiniteDist [(lowAction, 1), (lowAction, 1)])
    let available = lowAction NonEmpty.:| [highAction]
        transition selectedAction =
            case actionValue selectedAction of
                LowAction -> exactDirac (exactReward 2, ClosureMiddle)
                HighAction -> exactDirac (exactReward 8, ClosureMiddle)
                MissingAction -> error "unavailable exact action reached transition"
        secondTransition selectedAction =
            case actionValue selectedAction of
                LowAction -> exactDirac (exactReward 1, ClosureEnd)
                HighAction -> exactDirac (exactReward 3, ClosureEnd)
                MissingAction -> error "unavailable exact action reached second transition"
    closed <- requireRight "exact policy closure" (closeExactPolicy available selected transition)
    secondClosed <- requireRight "second exact policy closure" (closeExactPolicy available selected secondTransition)
    let closedOutcomes = NonEmpty.toList (exactOutcomes closed)
        closedMasses = fmap (exactProbability . snd) closedOutcomes
        closedRewards = fmap (exactRewardValue . fst . fst) closedOutcomes
        observable (reward, successor) = exactRewardValue reward + if successor == ClosureMiddle then 10 else 0
        expectation distribution f =
            sum
                [ exactProbability mass * f outcome
                | (outcome, mass) <- NonEmpty.toList (exactOutcomes distribution)
                ]
        directExpectation =
            sum
                [ exactProbability policyMass * expectation (transition selectedAction) observable
                | (selectedAction, policyMass) <- NonEmpty.toList (exactOutcomes selected)
                ]
        closedTraces =
            bindExactFiniteDist closed $ \(firstReward, firstState) ->
                fmap
                    (\(secondReward, secondState) -> (firstReward, firstState, secondReward, secondState))
                    secondClosed
        directTraces =
            bindExactFiniteDist selected $ \firstAction ->
                bindExactFiniteDist (transition firstAction) $ \(firstReward, firstState) ->
                    bindExactFiniteDist selected $ \secondAction ->
                        fmap
                            (\(secondReward, secondState) -> (firstReward, firstState, secondReward, secondState))
                            (secondTransition secondAction)
        traceObservable (firstReward, _, secondReward, secondState) =
            exactRewardValue firstReward
                + exactRewardValue secondReward
                + if secondState == ClosureEnd then 100 else 0
    assert "exact closure must preserve distinct reward outcomes" (closedRewards == [2, 8])
    assert "exact closure must preserve literal masses" (closedMasses == [1 % 4, 3 % 4])
    assert "exact closure observable must match direct execution" (expectation closed observable == directExpectation)
    assert "exact closed traces must match direct MDP traces" (closedTraces == directTraces)
    assert
        "bounded exact trace observables must match direct execution"
        (expectation closedTraces traceObservable == expectation directTraces traceObservable)
    conditional <- requireRight "exact conditional reward" (exactConditionalExpectedReward closed ClosureMiddle)
    assert "exact conditional reward changed" (exactRewardValue conditional == 13 % 2)
    case exactConditionalExpectedReward closed ClosureMissing of
        Left ExactZeroMassTransition -> pure ()
        result -> failTest ("exact zero-mass conditional reward was not rejected: " ++ show result)

    case closeExactPolicy (lowAction NonEmpty.:| [lowAction]) selected transition of
        Left (DuplicateExactAvailableAction duplicate) -> assert "duplicate exact available action changed" (duplicate == lowAction)
        result -> failTest ("duplicate exact available action was not rejected: " ++ show result)
    case closeExactPolicy available duplicateSelected transition of
        Left (DuplicateExactPolicyAction duplicate) -> assert "duplicate exact policy action changed" (duplicate == lowAction)
        result -> failTest ("duplicate exact policy action was not rejected: " ++ show result)
    case closeExactPolicy available (exactDirac missingAction) transition of
        Left (ExactPolicyUnavailableAction unavailable) -> assert "unavailable exact action changed" (unavailable == missingAction)
        result -> failTest ("unavailable exact action was not rejected: " ++ show result)
  where
    lowAction = actionId LowAction
    highAction = actionId HighAction
    missingAction = actionId MissingAction

testExactValues :: IO ()
testExactValues = do
    exactThird <- requireRight "exact probability" (mkExactProb (1 % 3))
    assert "exact probability must preserve one third" (exactProbability exactThird == 1 % 3)

    case mkExactProb ((-1) % 10) of
        Left (ExactProbabilityOutOfRange value) ->
            assert "exact probability error must retain its input" (value == (-1) % 10)
        result -> failTest ("negative exact probability was not rejected: " ++ show result)

    case mkExactProb (11 % 10) of
        Left (ExactProbabilityOutOfRange _) -> pure ()
        result -> failTest ("large exact probability was not rejected: " ++ show result)

    exactWeightValue <- requireRight "exact weight" (mkExactWeight (7 % 5))
    assert "exact weight must preserve its rational" (exactWeight exactWeightValue == 7 % 5)

    case mkExactWeight ((-1) % 5) of
        Left (NegativeExactWeight _) -> pure ()
        result -> failTest ("negative exact weight was not rejected: " ++ show result)

    case exactFiniteDist ([] :: [(Char, Rational)]) of
        Left EmptyExactSupport -> pure ()
        result -> failTest ("empty exact support was not rejected: " ++ show result)

    case exactFiniteDist [('a', 1), ('b', (-1) % 2)] of
        Left (InvalidExactWeight 1 (NegativeExactWeight _)) -> pure ()
        result -> failTest ("invalid exact weight index changed: " ++ show result)

    case exactFiniteDist [('a', 0), ('b', 0)] of
        Left ZeroExactTotalWeight -> pure ()
        result -> failTest ("zero exact total was not rejected: " ++ show result)

    distribution <- requireRight "exact weights 1 and 3" (exactFiniteDist [('a', 1), ('b', 3)])
    let masses = fmap (exactProbability . snd) (NonEmpty.toList (exactOutcomes distribution))
    assert "exact weights 1 and 3 must normalize literally" (masses == [1 % 4, 3 % 4])

    filtered <- requireRight "exact zero removal" (exactFiniteDist [('z', 0), ('p', 2)])
    assert "zero exact weights must leave support" (fmap fst (NonEmpty.toList (exactOutcomes filtered)) == ['p'])

    duplicates <- requireRight "exact duplicate labels" (exactFiniteDist [('a', 1), ('a', 1)])
    let duplicateOutcomes = NonEmpty.toList (exactOutcomes duplicates)
    assert "exact duplicate labels must remain distinct" (fmap fst duplicateOutcomes == ['a', 'a'])
    assert "exact duplicate masses must remain literal" (fmap (exactProbability . snd) duplicateOutcomes == [1 % 2, 1 % 2])

    let certain = fmap (exactProbability . snd) (NonEmpty.toList (exactOutcomes (exactDirac 'x')))
    assert "exact Dirac mass must equal one" (certain == [1])
    assert "exact rewards must preserve rational values" (exactRewardValue (exactReward ((-7) % 3)) == (-7) % 3)

testExactFunctorLaws :: IO ()
testExactFunctorLaws = do
    distribution <-
        requireRight
            "exact functor distribution"
            (exactFiniteDist ([(-2, 1), (3, 2)] :: [(Integer, Rational)]))
    assert "exact distribution must satisfy functor identity" (fmap id distribution == distribution)
    let addThree value = value + 3
        double value = value * 2
    assert
        "exact distribution must satisfy functor composition"
        (fmap (double . addThree) distribution == (fmap double . fmap addThree) distribution)

testExactKernelLaws :: IO ()
testExactKernelLaws = do
    coin <-
        requireRight
            "exact kernel coin"
            (exactFiniteDist ([(0, 1), (1, 1)] :: [(Integer, Rational)]))
    weighted <-
        requireRight
            "exact kernel weighted branch"
            (exactFiniteDist ([(2, 1), (5, 3)] :: [(Integer, Rational)]))
    let first = exactKernel (\input -> fmap (+ input) coin)
        second = exactKernel (\input -> fmap (* input) weighted)
        third = exactKernel (\input -> fmap (subtract input) coin)
        identityKernel = exactDeterministic id
        leftIdentity = composeExactKernel identityKernel first
        rightIdentity = composeExactKernel first identityKernel
        leftAssociated = composeExactKernel (composeExactKernel first second) third
        rightAssociated = composeExactKernel first (composeExactKernel second third)
        inputs = [-2, 0, 5]
        agrees left right input = runExactKernel left input == runExactKernel right input
    assert "exact kernel left identity must hold literally" (all (agrees leftIdentity first) inputs)
    assert "exact kernel right identity must hold literally" (all (agrees rightIdentity first) inputs)
    assert
        "exact kernel associativity must hold literally"
        (all (agrees leftAssociated rightAssociated) inputs)

testFloatingObjectives :: IO ()
testFloatingObjectives = do
    case mkDiscount (0 / 0) of
        Left (NonFiniteDiscount _) -> pure ()
        result -> failTest ("NaN discount was not rejected: " ++ show result)

    case mkDiscount (1 / 0) of
        Left (NonFiniteDiscount _) -> pure ()
        result -> failTest ("infinite discount was not rejected: " ++ show result)

    case mkDiscount (-0.1) of
        Left (DiscountOutOfRange _) -> pure ()
        result -> failTest ("negative discount was not rejected: " ++ show result)

    case mkDiscount 1.1 of
        Left (DiscountOutOfRange _) -> pure ()
        result -> failTest ("large discount was not rejected: " ++ show result)

    zeroDiscount <- requireRight "zero discount" (mkDiscount 0)
    negativeZeroDiscount <- requireRight "negative-zero discount" (mkDiscount (-0.0))
    unitDiscount <- requireRight "unit discount" (mkDiscount 1)
    assert "zero finite-horizon discount must be valid" (discountValue zeroDiscount == 0)
    assert "discount must canonicalize negative zero" (not (isNegativeZero (discountValue negativeZeroDiscount)))
    assert "unit finite-horizon discount must be valid" (discountValue unitDiscount == 1)

    case asContractionDiscount unitDiscount of
        Left (DiscountNotContractive 1) -> pure ()
        result -> failTest ("unit discount was accepted as contractive: " ++ show result)

    contraction <- requireRight "floating contraction discount" (mkContractionDiscount 0.9)
    assert "floating contraction discount changed" (contractionDiscountValue contraction == 0.9)

testExactObjectives :: IO ()
testExactObjectives = do
    case mkExactDiscount ((-1) % 10) of
        Left (ExactDiscountOutOfRange _) -> pure ()
        result -> failTest ("negative exact discount was not rejected: " ++ show result)

    case mkExactDiscount (11 % 10) of
        Left (ExactDiscountOutOfRange _) -> pure ()
        result -> failTest ("large exact discount was not rejected: " ++ show result)

    unitDiscount <- requireRight "exact unit discount" (mkExactDiscount 1)
    assert "exact unit discount must be valid" (exactDiscountValue unitDiscount == 1)

    case asExactContractionDiscount unitDiscount of
        Left (ExactDiscountNotContractive 1) -> pure ()
        result -> failTest ("exact unit discount was accepted as contractive: " ++ show result)

    contraction <- requireRight "exact contraction discount" (mkExactContractionDiscount (9 % 10))
    assert "exact contraction discount changed" (exactContractionDiscountValue contraction == 9 % 10)

testHorizons :: IO ()
testHorizons = do
    case mkHorizon (-1) of
        Left (NegativeHorizon (-1)) -> pure ()
        result -> failTest ("negative horizon was not rejected: " ++ show result)

    zeroHorizon <- requireRight "zero horizon" (mkHorizon 0)
    assert "zero horizon must be valid" (horizonValue zeroHorizon == 0)

    let largeValue = 10 ^ (30 :: Int)
    largeHorizon <- requireRight "large horizon" (mkHorizon largeValue)
    assert "horizon must not impose a machine-sized bound" (horizonValue largeHorizon == fromInteger largeValue)

data TestState = Start | End
    deriving (Eq, Show)

data TestAction = Finish | MissingFinish
    deriving (Eq, Show)

data ClosureState = ClosureStart | ClosureMiddle | ClosureEnd | ClosureMissing
    deriving (Eq, Show)

data ClosureAction = LowAction | HighAction | MissingAction
    deriving (Eq, Show)

data EvalState = EvalStart | EvalTerminal | EvalLoop
    deriving (Eq, Show)

data EvalAction = EvalOnly | EvalLow | EvalHigh | EvalMissing
    deriving (Eq, Show)

minimumPositiveDouble :: Double
minimumPositiveDouble =
    let sample = 0 :: Double
        digits = floatDigits sample
        (lowerExponent, _) = floatRange sample
     in encodeFloat 1 (lowerExponent - digits)

maximumFiniteDouble :: Double
maximumFiniteDouble =
    let sample = 0 :: Double
        radix = floatRadix sample
        digits = floatDigits sample
        (_, upperExponent) = floatRange sample
     in encodeFloat (radix ^ digits - 1) (upperExponent - digits)
