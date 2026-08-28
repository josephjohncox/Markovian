module ExactControl (runExactControlTests) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio ((%))
import Markovian.Compile.Exact (
    ActionIndex,
    CompiledExactMDP,
    CompiledExactOutcome (..),
    CompiledRuntimeError (..),
    ExactMDPCompileError (..),
    FiniteIndexError (..),
    StateIndex,
    actionAtIndex,
    closeCompiledExactPolicy,
    compileExactMDP,
    compiledActionIndex,
    compiledSourceState,
    compiledStateEntries,
    foldCompiledExactState,
    lookupActionIndex,
    stateIndexValue,
    stepCompiledExactMDP,
 )
import Markovian.Horizon (mkHorizon)
import Markovian.Interpreter.Bellman.Exact (mkExactBellmanTolerance)
import Markovian.Interpreter.Control.Exact (
    ExactControlError (..),
    ExactGreedyReport (..),
    ExactPolicyIterationReport (..),
    ExactPolicyIterationStopReason (..),
    ExactValueIterationReport (..),
    ExactValueIterationStopReason (..),
    exactActionValueAction,
    exactGreedyActionValues,
    exactPolicyIterationConfig,
    exactValueIterationConfig,
    extractExactGreedyActions,
    solveCompiledExactControl,
    solveCompiledExactPolicyIteration,
 )
import Markovian.Interpreter.DynamicProgramming.Exact (evaluateCompiledExactFinite, exactFiniteDPInitialValue)
import Markovian.Kernel.Exact (exactKernel)
import Markovian.MDP (ActionId, actionId, actionValue)
import Markovian.MDP.Exact (
    ExactMDP,
    ExactModelError (..),
    ExactStateStatus (..),
    exactMDP,
    exactTransitionOutcome,
 )
import Markovian.Objective.Exact (
    ExactContractionDiscount,
    exactContractionDiscountValue,
    exactFiniteObjective,
    mkExactContractionDiscount,
    mkExactDiscount,
 )
import Markovian.Policy.Exact (exactPolicy)
import Markovian.Probability.Exact (exactDirac, exactFiniteDist, exactOutcomes, exactProbability)
import Markovian.Reward.Exact (ExactReward, exactReward, exactRewardValue)

data ControlState = Initial | Choice | Terminal | Loop
    deriving (Eq, Show)

data ControlAction = First | Second | Third
    deriving (Eq, Show)

runExactControlTests :: (String -> IO () -> IO ()) -> IO ()
runExactControlTests run = do
    run "policy-free exact compilation" testPolicyFreeCompilation
    run "exact compilation errors and joint support" testCompilationEvidence
    run "exact value iteration boundaries and timing" testValueIterationBoundaries
    run "exact residual and contraction bounds" testResidualBounds
    run "exact greedy order and successor values" testGreedySelection
    run "exact control index permutation invariance" testPermutationInvariance
    run "exact deterministic policy iteration" testPolicyIteration
    run "exact policy iteration versus finite value iteration" testPolicyAndValueIteration

assert :: String -> Bool -> IO ()
assert message condition =
    if condition then pure () else fail message

requireRight :: (Show error) => String -> Either error value -> IO value
requireRight _ (Right value) = pure value
requireRight label (Left err) = fail (label ++ ": " ++ show err)

mustRight :: (Show error) => Either error value -> value
mustRight (Right value) = value
mustRight (Left err) = error (show err)

firstAction :: ActionId ControlAction
firstAction = actionId First

secondAction :: ActionId ControlAction
secondAction = actionId Second

testPolicyFreeCompilation :: IO ()
testPolicyFreeCompilation = do
    let model = terminalOnlyModel 9
    compiled <- requireRight "terminal-only compilation" (compileExactMDP [Terminal] [] model)
    assert "terminal-only compilation changed its state count" (length (compiledStateEntries compiled) == 1)

    let allActionsModel =
            exactMDP
                Initial
                (\state -> if state == Terminal then ExactTerminal (exactReward 0) else ExactContinuing)
                (\state -> if state == Terminal then error "terminal actions requested" else [secondAction, firstAction])
                ( exactKernel
                    ( \(_, selected) ->
                        case actionValue selected of
                            First -> exactDirac (exactTransitionOutcome (exactReward 1) Terminal)
                            Second -> exactDirac (exactTransitionOutcome (exactReward 2) Terminal)
                            Third -> error "unavailable transition requested"
                    )
                )
    allCompiled <- requireRight "all-action compilation" (compileExactMDP [Initial, Terminal] [firstAction, secondAction] allActionsModel)
    let continuing =
            concatMap
                (\(_, state) -> foldCompiledExactState (\_ _ -> []) (\_ available _ -> [available]) state)
                (NonEmpty.toList (compiledStateEntries allCompiled))
    case continuing of
        [available] -> do
            let decoded = fmap (actionAtIndex (compiledActionIndex allCompiled)) (NonEmpty.toList available)
            assert "model availability order was not retained" (decoded == [Just secondAction, Just firstAction])
        _ -> fail "compiled continuing state was missing"

testCompilationEvidence :: IO ()
testCompilationEvidence = do
    let duplicateOutcome = exactTransitionOutcome (exactReward 3) Terminal
        joint =
            mustRight
                ( exactFiniteDist
                    [ (exactTransitionOutcome (exactReward 1) Initial, 1)
                    , (duplicateOutcome, 2)
                    , (duplicateOutcome, 3)
                    ]
                )
        model =
            exactMDP
                Initial
                (\state -> if state == Terminal then ExactTerminal (exactReward 7) else ExactContinuing)
                (\state -> if state == Terminal then [] else [firstAction])
                (exactKernel (const joint))
    compiled <- requireRight "joint-support compilation" (compileExactMDP [Initial, Terminal] [firstAction] model)
    initialIndex <- stateIndexFor compiled Initial
    firstIndex <- actionIndexFor compiled firstAction
    transition <- requireRight "compiled joint transition" (stepCompiledExactMDP compiled initialIndex firstIndex)
    let outcomes = NonEmpty.toList (exactOutcomes transition)
    assert "compiled support merged duplicate labels" (length outcomes == 3)
    assert
        "compiled support changed reward order"
        (fmap (exactRewardValue . compiledTransitionReward . fst) outcomes == [1, 3, 3])
    assert
        "compiled support changed exact masses"
        (fmap (exactProbability . snd) outcomes == [1 % 6, 1 % 3, 1 % 2])
    case outcomes of
        (firstOutcome, _) : _ ->
            assert
                "compiled reward/successor correlation changed"
                (stateIndexValue (compiledSuccessorState firstOutcome) == stateIndexValue initialIndex)
        [] -> fail "compiled transition support became empty"

    case compileExactMDP [Initial, Initial] [firstAction] model of
        Left (ExactMDPStateIndexError (DuplicateFiniteIndexValue Initial)) -> pure ()
        result -> fail ("duplicate state support result changed: " ++ either show (const "success") result)
    case compileExactMDP [Initial, Terminal] [firstAction, firstAction] model of
        Left (ExactMDPActionIndexError (DuplicateFiniteIndexValue duplicate)) ->
            assert "duplicate action ID changed" (duplicate == firstAction)
        result -> fail ("duplicate action support result changed: " ++ either show (const "success") result)
    case compileExactMDP [Terminal] [firstAction] model of
        Left (ExactMDPUnindexedInitialState Initial) -> pure ()
        result -> fail ("missing initial state result changed: " ++ either show (const "success") result)
    case compileExactMDP [Initial] [firstAction] model of
        Left (ExactMDPUnindexedSuccessor Initial selected Terminal) ->
            assert "missing successor action changed" (selected == firstAction)
        result -> fail ("missing successor result changed: " ++ either show (const "success") result)

    let missingActionModel =
            exactMDP
                Initial
                (const ExactContinuing)
                (const [secondAction])
                (exactKernel (const (exactDirac (exactTransitionOutcome (exactReward 0) Initial))))
    case compileExactMDP [Initial] [firstAction] missingActionModel of
        Left (ExactMDPUnindexedAvailableAction Initial selected) ->
            assert "missing available action changed" (selected == secondAction)
        result -> fail ("missing available action result changed: " ++ either show (const "success") result)

    let emptyActionModel :: ExactMDP ControlState ControlAction
        emptyActionModel =
            exactMDP
                Initial
                (const ExactContinuing)
                (const [])
                (exactKernel (const (exactDirac (exactTransitionOutcome (exactReward 0) Initial))))
    case compileExactMDP [Initial] [] emptyActionModel of
        Left (ExactMDPModelError Initial EmptyExactActionSupport) -> pure ()
        result -> fail ("empty continuing support result changed: " ++ either show (const "success") result)

    let duplicateActionModel =
            exactMDP
                Initial
                (const ExactContinuing)
                (const [firstAction, firstAction])
                (exactKernel (const (exactDirac (exactTransitionOutcome (exactReward 0) Initial))))
    case compileExactMDP [Initial] [firstAction] duplicateActionModel of
        Left (ExactMDPModelError Initial (DuplicateExactModelAction duplicate)) ->
            assert "duplicate model action changed" (duplicate == firstAction)
        result -> fail ("duplicate model action result changed: " ++ either show (const "success") result)

    compiledWithUnused <-
        requireRight
            "unused global action compilation"
            (compileExactMDP [Initial, Terminal] [firstAction, secondAction] model)
    secondIndex <- actionIndexFor compiledWithUnused secondAction
    case stepCompiledExactMDP compiledWithUnused initialIndex secondIndex of
        Left (MissingCompiledTransition requested selected) ->
            assert
                "unavailable compiled action error changed"
                (stateIndexValue requested == stateIndexValue initialIndex && selected == secondIndex)
        result -> fail ("unavailable compiled action step changed: " ++ either show (const "success") result)

    compiledTerminal <- requireRight "terminal runtime compilation" (compileExactMDP [Terminal] [firstAction] (terminalOnlyModel 7))
    terminalIndex <- stateIndexFor compiledTerminal Terminal
    terminalAction <- actionIndexFor compiledTerminal firstAction
    case stepCompiledExactMDP compiledTerminal terminalIndex terminalAction of
        Left (CompiledActionRequestedAtTerminal requested payoff) ->
            assert
                "terminal compiled step error changed"
                (stateIndexValue requested == stateIndexValue terminalIndex && exactRewardValue payoff == 7)
        result -> fail ("terminal compiled step changed: " ++ either show (const "success") result)

testValueIterationBoundaries :: IO ()
testValueIterationBoundaries = do
    gamma <- requireRight "half discount" (mkExactContractionDiscount (1 % 2))
    tolerance <- requireRight "control tolerance" (mkExactBellmanTolerance (1 % 1000))
    zeroLimit <- requireRight "zero limit" (mkHorizon 0)
    oneLimit <- requireRight "one limit" (mkHorizon 1)
    let timingModel =
            exactMDP
                Initial
                (\state -> if state == Terminal then ExactTerminal (exactReward 7) else ExactContinuing)
                (\state -> if state == Terminal then [] else [firstAction])
                (exactKernel (const (exactDirac (exactTransitionOutcome (exactReward 2) Terminal))))
    compiled <- requireRight "timing compilation" (compileExactMDP [Initial, Terminal] [firstAction] timingModel)
    zero <- requireRight "zero-limit control" (solveCompiledExactControl (exactValueIterationConfig gamma tolerance zeroLimit) compiled)
    assert "zero limit performed a backup" (exactValueIterationBackups zero == 0)
    assert "zero limit did not report its limit" (exactValueIterationStopReason zero == ExactValueIterationLimit)
    assert "zero-limit continuing value changed" (exactRewardValue (exactValueIterationInitialValue zero) == 0)
    one <- requireRight "one-backup control" (solveCompiledExactControl (exactValueIterationConfig gamma tolerance oneLimit) compiled)
    assert "terminal payoff timing changed" (exactRewardValue (exactValueIterationInitialValue one) == 11 % 2)
    assert "one-backup timing model did not converge" (exactValueIterationStopReason one == ExactValueIterationConverged)

    zeroDiscount <- requireRight "zero discount" (mkExactContractionDiscount 0)
    let negativeModel =
            exactMDP
                Initial
                (\state -> if state == Terminal then ExactTerminal (exactReward 100) else ExactContinuing)
                (\state -> if state == Terminal then [] else [firstAction])
                (exactKernel (const (exactDirac (exactTransitionOutcome (exactReward (-3)) Terminal))))
    negativeCompiled <- requireRight "negative compilation" (compileExactMDP [Initial, Terminal] [firstAction] negativeModel)
    negative <- requireRight "negative control" (solveCompiledExactControl (exactValueIterationConfig zeroDiscount tolerance oneLimit) negativeCompiled)
    assert "negative reward or zero discount changed" (exactRewardValue (exactValueIterationInitialValue negative) == -3)

    terminalCompiled <- requireRight "terminal control compilation" (compileExactMDP [Terminal] [] (terminalOnlyModel 9))
    terminal <- requireRight "terminal-only control" (solveCompiledExactControl (exactValueIterationConfig gamma tolerance zeroLimit) terminalCompiled)
    assert "terminal-only residual is not zero" (exactValueIterationResidual terminal == 0)
    assert "terminal-only payoff changed" (exactRewardValue (exactValueIterationInitialValue terminal) == 9)

testResidualBounds :: IO ()
testResidualBounds = do
    gamma <- requireRight "loop discount" (mkExactContractionDiscount (1 % 2))
    tolerance <- requireRight "loop tolerance" (mkExactBellmanTolerance (1 % 100))
    zeroLimit <- requireRight "loop zero limit" (mkHorizon 0)
    compiled <- requireRight "loop compilation" (compileExactMDP [Loop] [firstAction] selfLoopModel)
    report <- requireRight "loop control" (solveCompiledExactControl (exactValueIterationConfig gamma tolerance zeroLimit) compiled)
    assert "self-loop residual changed" (exactValueIterationResidual report == 1)
    assert "self-loop value-error bound changed" (exactValueIterationValueErrorBound report == 2)
    assert "self-loop greedy-policy bound changed" (exactValueIterationGreedyPerformanceBound report == 4)
    let actualError = abs (2 - exactRewardValue (exactValueIterationInitialValue report))
    assert "reported value-error bound is unsound" (actualError <= exactValueIterationValueErrorBound report)

testGreedySelection :: IO ()
testGreedySelection = do
    gamma <- requireRight "greedy discount" (mkExactContractionDiscount (1 % 2))
    let model =
            exactMDP
                Initial
                (\state -> if state == Terminal then ExactTerminal (exactReward 0) else ExactContinuing)
                ( \state ->
                    case state of
                        Initial -> [firstAction, secondAction]
                        Choice -> [firstAction]
                        _ -> []
                )
                ( exactKernel
                    ( \(state, selected) ->
                        case (state, actionValue selected) of
                            (Initial, First) -> exactDirac (exactTransitionOutcome (exactReward 3) Terminal)
                            (Initial, Second) -> exactDirac (exactTransitionOutcome (exactReward 0) Choice)
                            (Choice, First) -> exactDirac (exactTransitionOutcome (exactReward 0) Terminal)
                            _ -> error "unavailable greedy transition"
                    )
                )
    compiled <- requireRight "greedy compilation" (compileExactMDP [Initial, Choice, Terminal] [secondAction, firstAction] model)
    supplied <- valueVector compiled (\state -> if state == Choice then 10 else 0)
    greedy <- requireRight "successor-value greedy extraction" (extractExactGreedyActions gamma compiled supplied)
    assert "greedy choice ignored successor value" (lookup Initial (exactGreedyChoices greedy) == Just secondAction)
    terminalIndex <- stateIndexFor compiled Terminal
    let invalidTerminalValues =
            fmap
                (\(index, value) -> if index == terminalIndex then (index, exactReward 100) else (index, value))
                supplied
    case extractExactGreedyActions gamma compiled invalidTerminalValues of
        Left (ExactControlTerminalValueMismatch rejected expected actual) ->
            assert
                "terminal-vector mismatch diagnostics changed"
                (rejected == terminalIndex && exactRewardValue expected == 0 && exactRewardValue actual == 100)
        result -> fail ("invalid terminal value was accepted: " ++ either show (const "success") result)

    let tieModel =
            exactMDP
                Initial
                (\state -> if state == Terminal then ExactTerminal (exactReward 0) else ExactContinuing)
                (\state -> if state == Terminal then [] else [secondAction, firstAction])
                (exactKernel (const (exactDirac (exactTransitionOutcome (exactReward 1) Terminal))))
    tied <- requireRight "tie compilation" (compileExactMDP [Initial, Terminal] [firstAction, secondAction] tieModel)
    tiedValues <- valueVector tied (const 0)
    tiedGreedy <- requireRight "tie extraction" (extractExactGreedyActions gamma tied tiedValues)
    assert "tie did not retain first available action" (lookup Initial (exactGreedyChoices tiedGreedy) == Just secondAction)
    let indexedActions =
            [ fmap exactActionValueAction (NonEmpty.toList values)
            | (_, values) <- exactGreedyActionValues tiedGreedy
            ]
    assert "action values did not preserve availability order" (length indexedActions == 1)

testPermutationInvariance :: IO ()
testPermutationInvariance = do
    gamma <- requireRight "permutation discount" (mkExactContractionDiscount (1 % 2))
    tolerance <- requireRight "permutation tolerance" (mkExactBellmanTolerance (1 % 1000))
    limit <- requireRight "permutation limit" (mkHorizon 3)
    let model = oneStepChoiceModel
    leftCompiled <- requireRight "left permutation" (compileExactMDP [Initial, Terminal] [firstAction, secondAction] model)
    rightCompiled <- requireRight "right permutation" (compileExactMDP [Terminal, Initial] [secondAction, firstAction] model)
    leftReport <- requireRight "left permutation control" (solveCompiledExactControl (exactValueIterationConfig gamma tolerance limit) leftCompiled)
    rightReport <- requireRight "right permutation control" (solveCompiledExactControl (exactValueIterationConfig gamma tolerance limit) rightCompiled)
    assert
        "state or global action permutation changed the initial value"
        (exactValueIterationInitialValue leftReport == exactValueIterationInitialValue rightReport)
    leftGreedy <- requireRight "left permutation greedy" (extractExactGreedyActions gamma leftCompiled (exactValueIterationValues leftReport))
    rightGreedy <- requireRight "right permutation greedy" (extractExactGreedyActions gamma rightCompiled (exactValueIterationValues rightReport))
    assert "index permutation changed the decoded greedy action" (lookup Initial (exactGreedyChoices leftGreedy) == lookup Initial (exactGreedyChoices rightGreedy))

testPolicyIteration :: IO ()
testPolicyIteration = do
    gamma <- requireRight "policy-iteration discount" (mkExactContractionDiscount (1 % 2))
    limit <- requireRight "policy-iteration limit" (mkHorizon 10)
    compiled <- requireRight "policy-iteration compilation" (compileExactMDP policyStates [firstAction, secondAction] policyModel)
    report <- requireRight "exact policy iteration" (solveCompiledExactPolicyIteration (exactPolicyIterationConfig gamma limit) compiled)
    assert "policy iteration did not become stable" (exactPolicyIterationStopReason report == ExactPolicyIterationStable)
    assert "stable exact policy has nonzero optimality residual" (exactPolicyIterationResidual report == 0)

    returns <- traverse (deterministicReturn gamma compiled) deterministicPolicies
    let exhaustiveBest = maximum returns
    assert
        "policy iteration differs from exhaustive deterministic policies"
        (exactRewardValue (exactPolicyIterationInitialValue report) == exhaustiveBest)
    assert "policy iteration selected the wrong initial action" (lookup Initial (exactPolicyIterationChoices report) == Just firstAction)
    assert "policy iteration selected the wrong successor action" (lookup Choice (exactPolicyIterationChoices report) == Just firstAction)

    zeroLimit <- requireRight "zero policy-iteration limit" (mkHorizon 0)
    limited <- requireRight "limited policy iteration" (solveCompiledExactPolicyIteration (exactPolicyIterationConfig gamma zeroLimit) compiled)
    assert "zero policy-iteration limit did not stop" (exactPolicyIterationStopReason limited == ExactPolicyIterationLimit)
    assert "zero policy-iteration limit changed the initial availability choice" (lookup Initial (exactPolicyIterationChoices limited) == Just secondAction)

testPolicyAndValueIteration :: IO ()
testPolicyAndValueIteration = do
    gamma <- requireRight "differential discount" (mkExactContractionDiscount (1 % 2))
    tolerance <- requireRight "differential tolerance" (mkExactBellmanTolerance (1 % 1000))
    limit <- requireRight "differential limit" (mkHorizon 10)
    compiled <- requireRight "differential compilation" (compileExactMDP policyStates [secondAction, firstAction] policyModel)
    policyReport <- requireRight "differential policy iteration" (solveCompiledExactPolicyIteration (exactPolicyIterationConfig gamma limit) compiled)
    valueReport <- requireRight "differential value iteration" (solveCompiledExactControl (exactValueIterationConfig gamma tolerance limit) compiled)
    let difference =
            abs
                ( exactRewardValue (exactPolicyIterationInitialValue policyReport)
                    - exactRewardValue (exactValueIterationInitialValue valueReport)
                )
    assert "value iteration exceeded its reported exact bound" (difference <= exactValueIterationValueErrorBound valueReport)

terminalOnlyModel :: Rational -> ExactMDP ControlState ControlAction
terminalOnlyModel payoff =
    exactMDP
        Terminal
        (const (ExactTerminal (exactReward payoff)))
        (\_ -> error "terminal action support was inspected")
        (exactKernel (\_ -> error "terminal transition was inspected"))

selfLoopModel :: ExactMDP ControlState ControlAction
selfLoopModel =
    exactMDP
        Loop
        (const ExactContinuing)
        (const [firstAction])
        (exactKernel (const (exactDirac (exactTransitionOutcome (exactReward 1) Loop))))

oneStepChoiceModel :: ExactMDP ControlState ControlAction
oneStepChoiceModel =
    exactMDP
        Initial
        (\state -> if state == Terminal then ExactTerminal (exactReward 0) else ExactContinuing)
        (\state -> if state == Terminal then [] else [firstAction, secondAction])
        ( exactKernel
            ( \(_, selected) ->
                case actionValue selected of
                    First -> exactDirac (exactTransitionOutcome (exactReward 1) Terminal)
                    Second -> exactDirac (exactTransitionOutcome (exactReward 2) Terminal)
                    Third -> error "unavailable one-step action"
            )
        )

policyStates :: [ControlState]
policyStates = [Initial, Choice, Terminal]

policyModel :: ExactMDP ControlState ControlAction
policyModel =
    exactMDP
        Initial
        (\state -> if state == Terminal then ExactTerminal (exactReward 4) else ExactContinuing)
        ( \state ->
            case state of
                Initial -> [secondAction, firstAction]
                Choice -> [secondAction, firstAction]
                _ -> []
        )
        ( exactKernel
            ( \(state, selected) ->
                case (state, actionValue selected) of
                    (Initial, First) -> exactDirac (exactTransitionOutcome (exactReward 1) Terminal)
                    (Initial, Second) -> exactDirac (exactTransitionOutcome (exactReward 0) Choice)
                    (Choice, First) -> exactDirac (exactTransitionOutcome (exactReward 2) Terminal)
                    (Choice, Second) -> exactDirac (exactTransitionOutcome (exactReward 1) Terminal)
                    _ -> error "unavailable policy-iteration transition"
            )
        )

deterministicPolicies :: [(ControlAction, ControlAction)]
deterministicPolicies =
    [ (initialAction, choiceAction)
    | initialAction <- [First, Second]
    , choiceAction <- [First, Second]
    ]

deterministicReturn ::
    ExactContractionDiscount ->
    CompiledExactMDP ControlState ControlAction ->
    (ControlAction, ControlAction) ->
    IO Rational
deterministicReturn gamma compiled (initialAction, choiceAction) = do
    horizon <- requireRight "exhaustive horizon" (mkHorizon 2)
    discount <- requireRight "exhaustive discount" (mkExactDiscount (exactContractionDiscountValue gamma))
    let selected state =
            case state of
                Initial -> exactDirac (actionId initialAction)
                Choice -> exactDirac (actionId choiceAction)
                _ -> error "terminal exhaustive policy inspected"
        policy = exactPolicy (exactKernel selected)
    closed <- requireRight "exhaustive policy closure" (closeCompiledExactPolicy compiled policy)
    report <- requireRight "exhaustive policy evaluation" (evaluateCompiledExactFinite (exactFiniteObjective horizon discount) closed)
    pure (exactRewardValue (exactFiniteDPInitialValue report))

stateIndexFor ::
    (Eq state, Show state) =>
    CompiledExactMDP state action ->
    state ->
    IO StateIndex
stateIndexFor compiled requested =
    case [ stateIndex
         | (stateIndex, state) <- NonEmpty.toList (compiledStateEntries compiled)
         , compiledSourceState state == requested
         ] of
        stateIndex : _ -> pure stateIndex
        [] -> fail ("state not compiled: " ++ show requested)

actionIndexFor ::
    (Eq action) =>
    CompiledExactMDP state action ->
    ActionId action ->
    IO ActionIndex
actionIndexFor compiled requested =
    case lookupActionIndex (compiledActionIndex compiled) requested of
        Nothing -> fail "action not compiled"
        Just actionIndex -> pure actionIndex

valueVector ::
    CompiledExactMDP ControlState action ->
    (ControlState -> Rational) ->
    IO (NonEmpty (StateIndex, ExactReward))
valueVector compiled valueFor =
    pure
        ( fmap
            (\(stateIndex, state) -> (stateIndex, exactReward (valueFor (compiledSourceState state))))
            (compiledStateEntries compiled)
        )
