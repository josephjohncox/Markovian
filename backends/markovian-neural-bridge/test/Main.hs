module Main (main) where

import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Action (ActionId, actionId, actionValue)
import Markovian.Backend.Neural (
    ActionMask,
    DQNTargetSelection (..),
    dqnTransitionTarget,
    linearPolicyScoreGradient,
    linearPolicySelectedLogProbability,
    mkContinuingTransition,
    mkDenseNetwork,
    mkLinearCategoricalPolicy,
 )
import Markovian.Backend.Neural.Bridge.ExactSupportMask (
    ExactStateMask,
    ExactSupportMask,
    SupportMaskError (..),
    SupportMaskLimitDimension (..),
    SupportMaskLimits (..),
    compileAllExactSupportMasks,
    compileExactSupportMaskAt,
    denseActionOutputLayout,
    exactSupportMaskActions,
    exactSupportMaskFlags,
    exactSupportNeuralMask,
    foldExactStateMask,
    policyActionOutputLayout,
    sameActionOutputLayout,
    sameActionOutputSupport,
    sameExactSupportMaskLayout,
 )
import Markovian.Compile.Exact (
    CompiledExactMDP,
    CompiledExactOutcome (..),
    StateIndex,
    compileExactMDP,
    compiledActionIndex,
    compiledStateIndex,
    finiteActionIndexCardinality,
    finiteActionIndexValues,
    finiteStateIndex,
    lookupActionIndex,
    lookupStateIndex,
    sameFiniteActionIndex,
    sameFiniteActionIndexLayout,
    stateIndexValue,
    stepCompiledExactMDP,
 )
import Markovian.Kernel.Exact (exactKernel)
import Markovian.MDP.Exact (
    ExactMDP,
    ExactStateStatus (..),
    exactMDP,
    exactTransitionOutcome,
 )
import Markovian.Probability.Exact (exactDirac, exactOutcomes, exactProbability)
import Markovian.Reward.Exact (exactReward, exactRewardValue)

-- Keep the exact action labels distinct from neural output positions.
data TestAction = A | B | C
    deriving (Eq, Show)

data TestState = Source | Terminal
    deriving (Eq, Show)

main :: IO ()
main = do
    layoutAndCompilationChecks
    policyDifferentialChecks
    dqnDifferentialChecks
    putStrLn "PASS: exact-support neural bridge"

layoutAndCompilationChecks :: IO ()
layoutAndCompilationChecks = do
    compiled <- compiledFixture [a, b, c]
    policy <- requireRight "three-action policy" (mkLinearCategoricalPolicy 3 1 [0, 0, 0])
    layout <- requireRight "policy action layout" (policyActionOutputLayout (compiledActionIndex compiled) policy)
    assert "global cardinality" (finiteActionIndexCardinality (compiledActionIndex compiled) == 3)
    assert "global values" (finiteActionIndexValues (compiledActionIndex compiled) == [a, b, c])
    sourceIndex <- stateIndexFor compiled Source
    terminalIndex <- stateIndexFor compiled Terminal
    sourceState <- requireRight "source support mask" (compileExactSupportMaskAt fixtureLimits compiled layout sourceIndex)
    sourceMask <- requireContinuing "source support" sourceState
    assert "availability order" (exactSupportMaskActions sourceMask == [c, a])
    assert "global mask flags" (exactSupportMaskFlags sourceMask == [True, False, True])
    terminalState <- requireRight "terminal support result" (compileExactSupportMaskAt fixtureLimits compiled layout terminalIndex)
    assert "terminal represented explicitly" (foldExactStateMask True (const False) terminalState)

    allMasks <- requireRight "all state masks" (compileAllExactSupportMasks fixtureLimits compiled layout)
    allMasksAgain <- requireRight "all state masks repeated" (compileAllExactSupportMasks fixtureLimits compiled layout)
    assert "deterministic recompilation" (allMasks == allMasksAgain)
    assert "deterministic state order" (fmap (stateIndexValue . fst) (NonEmpty.toList allMasks) == [0, 1])
    assert "membership iff exact availability" (exactSupportMaskFlags sourceMask == fmap (`elem` exactSupportMaskActions sourceMask) [a, b, c])

    permuted <- compiledFixture [b, c, a]
    permutedLayout <- requireRight "permuted policy layout" (policyActionOutputLayout (compiledActionIndex permuted) policy)
    permutedState <- requireRight "permuted support mask" (compileExactSupportMaskAt fixtureLimits permuted permutedLayout sourceIndex)
    permutedMask <- requireContinuing "permuted source support" permutedState
    assert "consistent permutation preserves labelled order" (exactSupportMaskActions permutedMask == [c, a])
    assert "support-mask layout detects global permutation" (not (sameExactSupportMaskLayout sourceMask permutedMask))
    assert "support-mask layout is deterministic" (sameExactSupportMaskLayout sourceMask sourceMask)
    assert "consistent permutation changes flags by orientation" (exactSupportMaskFlags permutedMask == [False, True, True])
    assert "semantic global support ignores order" (sameFiniteActionIndex (compiledActionIndex compiled) (compiledActionIndex permuted))
    assert "global layout records order" (not (sameFiniteActionIndexLayout (compiledActionIndex compiled) (compiledActionIndex permuted)))
    assert "output support ignores order" (sameActionOutputSupport layout permutedLayout)
    assert "output layout records order" (not (sameActionOutputLayout layout permutedLayout))
    case compileExactSupportMaskAt fixtureLimits compiled permutedLayout sourceIndex of
        Left SupportMaskGlobalLayoutMismatch -> pure ()
        result -> fail ("reordered global layout was accepted: " ++ show result)

    foreignStates <- requireRight "foreign state index" (finiteStateIndex [0 :: Int, 1, 2])
    foreignIndex <- maybe (fail "foreign state index missing") pure (lookupStateIndex foreignStates 2)
    case compileExactSupportMaskAt fixtureLimits compiled layout foreignIndex of
        Left (SupportMaskInvalidStateIndex invalid) -> assert "invalid state identity" (stateIndexValue invalid == 2)
        result -> fail ("invalid state index was accepted: " ++ show result)

    shortPolicy <- requireRight "short policy" (mkLinearCategoricalPolicy 2 1 [0, 0])
    case policyActionOutputLayout (compiledActionIndex compiled) shortPolicy of
        Left (SupportMaskHeadWidthMismatch 3 2) -> pure ()
        result -> fail ("short policy head was accepted: " ++ show result)
    shortDense <- requireRight "short dense head" (mkDenseNetwork 1 [] 2 [0, 0, 0, 0])
    case denseActionOutputLayout (compiledActionIndex compiled) shortDense of
        Left (SupportMaskHeadWidthMismatch 3 2) -> pure ()
        result -> fail ("short dense head was accepted: " ++ show result)

    -- This fixture's complete preflight charges two states, five global/local
    -- action entries, and 35 conservative traversal-work units.
    _ <- requireRight "exact state boundary" (compileAllExactSupportMasks fixtureLimits compiled layout)
    expectLimit
        "state one below"
        SupportMaskStates
        1
        2
        (compileAllExactSupportMasks fixtureLimits{maximumSupportMaskStates = 1} compiled layout)
    expectLimit
        "action one below"
        SupportMaskActions
        4
        5
        (compileAllExactSupportMasks fixtureLimits{maximumSupportMaskActions = 4} compiled layout)
    expectLimit
        "work one below"
        SupportMaskWork
        34
        35
        (compileAllExactSupportMasks fixtureLimits{maximumSupportMaskWork = 34} compiled layout)
    case compileAllExactSupportMasks fixtureLimits{maximumSupportMaskWork = 0} compiled layout of
        Left (SupportMaskLimitExceeded SupportMaskWork 0 35) -> pure ()
        result -> fail ("exhaustion returned a partial mask collection: " ++ show result)

policyDifferentialChecks :: IO ()
policyDifferentialChecks = do
    compiled <- compiledFixture [a, b, c]
    let parameters = [0.2, 9.0, -0.3]
        features = [1.25]
    policy <- requireRight "masked policy" (mkLinearCategoricalPolicy 3 1 parameters)
    layout <- requireRight "masked policy layout" (policyActionOutputLayout (compiledActionIndex compiled) policy)
    sourceIndex <- stateIndexFor compiled Source
    stateMask <- requireRight "masked policy state" (compileExactSupportMaskAt fixtureLimits compiled layout sourceIndex)
    support <- requireContinuing "masked policy support" stateMask
    let mask = exactSupportNeuralMask support
    _ <- requireRight "generated mask policy log probability" (linearPolicySelectedLogProbability policy features mask 2)
    analytic <- requireRight "generated mask policy score" (linearPolicyScoreGradient policy features mask 2)
    numerical <- traverse (centralDifference parameters features mask) [0 .. length parameters - 1]
    assertVectorClose "generated-mask score finite difference" 3e-8 analytic numerical
    case analytic of
        [_aRow, unavailableRow, _cRow] -> do
            assert "unavailable parameter row is exact zero" (unavailableRow == 0.0)
            assert "unavailable parameter row is positive zero" ((1 / unavailableRow) == (1 / 0.0))
        _ -> fail "policy gradient layout changed"

dqnDifferentialChecks :: IO ()
dqnDifferentialChecks = do
    compiled <- compiledFixture [a, b, c]
    online <- requireRight "tie-order online head" (mkDenseNetwork 1 [] 3 [0, 0, 0, 4, 100, 4])
    target <- requireRight "masked target head" (mkDenseNetwork 1 [] 3 [0, 0, 0, 3, 100, 7])
    layout <- requireRight "dense action layout" (denseActionOutputLayout (compiledActionIndex compiled) online)
    sourceIndex <- stateIndexFor compiled Source
    stateMask <- requireRight "DQN support state" (compileExactSupportMaskAt fixtureLimits compiled layout sourceIndex)
    support <- requireContinuing "DQN support" stateMask
    let mask = exactSupportNeuralMask support
    transition <- requireRight "generated-mask DQN transition" (mkContinuingTransition [0] mask 0 0 [0] mask)
    doubleTarget <- requireRight "generated-mask Double-DQN target" (dqnTransitionTarget 1 DoubleDQN online target transition)
    standardTarget <- requireRight "generated-mask standard target" (dqnTransitionTarget 1 StandardDQN online target transition)
    exactAvailableReturns <- traverse (compiledOneStepReturn compiled sourceIndex) (exactSupportMaskActions support)
    let exactAvailableMaximum = maximum exactAvailableReturns
    assertClose "exact maximum versus converted masked target" 0 (fromRational exactAvailableMaximum) standardTarget
    assertClose "exact availability order reaches Double-DQN tie" 0 7 doubleTarget
    assert "larger unavailable neural value is excluded" (doubleTarget < 100 && standardTarget < 100)

fixtureLimits :: SupportMaskLimits
fixtureLimits = SupportMaskLimits 2 5 35

expectLimit ::
    (Show value) =>
    String ->
    SupportMaskLimitDimension ->
    Integer ->
    Integer ->
    Either SupportMaskError value ->
    IO ()
expectLimit label expectedDimension expectedLimit expectedActual result =
    case result of
        Left (SupportMaskLimitExceeded dimension limit actual) ->
            assert
                label
                ( dimension == expectedDimension
                    && toInteger limit == expectedLimit
                    && toInteger actual == expectedActual
                )
        other -> fail (label ++ ": unexpected result " ++ show other)

compiledOneStepReturn ::
    CompiledExactMDP TestState TestAction ->
    StateIndex ->
    ActionId TestAction ->
    IO Rational
compiledOneStepReturn compiled stateIndex selected = do
    actionIndex <-
        maybe
            (fail ("compiled action missing: " ++ show selected))
            pure
            (lookupActionIndex (compiledActionIndex compiled) selected)
    outcomes <- requireRight "compiled exact one-step outcomes" (stepCompiledExactMDP compiled stateIndex actionIndex)
    pure $
        sum
            [ exactProbability probability * exactRewardValue (compiledTransitionReward outcome)
            | (outcome, probability) <- NonEmpty.toList (exactOutcomes outcomes)
            ]

compiledFixture :: [ActionId TestAction] -> IO (CompiledExactMDP TestState TestAction)
compiledFixture globalOrder =
    requireRight "compiled exact fixture" (compileExactMDP [Source, Terminal] globalOrder exactFixture)

exactFixture :: ExactMDP TestState TestAction
exactFixture =
    exactMDP
        Source
        (\state -> if state == Terminal then ExactTerminal (exactReward 0) else ExactContinuing)
        (\state -> if state == Terminal then [] else [c, a])
        ( exactKernel
            ( \(_, selected) ->
                case actionValue selected of
                    A -> exactDirac (exactTransitionOutcome (exactReward 3) Terminal)
                    C -> exactDirac (exactTransitionOutcome (exactReward 7) Terminal)
                    B -> error "unavailable exact action requested"
            )
        )

a, b, c :: ActionId TestAction
a = actionId A
b = actionId B
c = actionId C

stateIndexFor :: CompiledExactMDP TestState TestAction -> TestState -> IO StateIndex
stateIndexFor compiled state =
    maybe (fail ("missing state index: " ++ show state)) pure (lookupStateIndex (compiledStateIndex compiled) state)

requireContinuing :: String -> ExactStateMask action -> IO (ExactSupportMask action)
requireContinuing label =
    maybe (fail (label ++ ": terminal result")) pure . foldExactStateMask Nothing Just

centralDifference :: [Double] -> [Double] -> ActionMask -> Int -> IO Double
centralDifference parameters features mask index =
    case valueAt index parameters of
        Nothing -> fail "finite-difference parameter index missing"
        Just point -> do
            let step = 1e-6 * max 1 (abs point)
            above <- objective (replace index (point + step) parameters)
            below <- objective (replace index (point - step) parameters)
            pure ((above - below) / (2 * step))
  where
    objective candidate = do
        policy <- requireRight "perturbed masked policy" (mkLinearCategoricalPolicy 3 1 candidate)
        requireRight "perturbed masked objective" (linearPolicySelectedLogProbability policy features mask 2)

valueAt :: Int -> [value] -> Maybe value
valueAt requested
    | requested < 0 = const Nothing
    | otherwise = go requested
  where
    go _ [] = Nothing
    go 0 (value : _) = Just value
    go remaining (_ : values) = go (remaining - 1) values

replace :: Int -> value -> [value] -> [value]
replace selected replacement =
    zipWith (\index value -> if index == selected then replacement else value) [0 :: Int ..]

assertVectorClose :: String -> Double -> [Double] -> [Double] -> IO ()
assertVectorClose label tolerance expected actual = do
    assert (label ++ ": vector length") (length expected == length actual)
    sequence_
        [ assertClose (label ++ " coordinate " ++ show index) tolerance left right
        | (index, (left, right)) <- zip [0 :: Int ..] (zip expected actual)
        ]

assertClose :: String -> Double -> Double -> Double -> IO ()
assertClose label tolerance expected actual =
    assert (label ++ ": expected " ++ show expected ++ ", got " ++ show actual) (abs (expected - actual) <= tolerance)

assert :: String -> Bool -> IO ()
assert _ True = pure ()
assert message False = fail message

requireRight :: (Show error) => String -> Either error value -> IO value
requireRight _ (Right value) = pure value
requireRight label (Left err) = fail (label ++ ": " ++ show err)
