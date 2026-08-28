module TabularLearning (runTabularLearningTests) where

import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio ((%))
import Markovian.Compile.Exact qualified as ExactCompile
import Markovian.Horizon (mkHorizon)
import Markovian.Interpreter.Control.Exact qualified as ExactControl
import Markovian.Kernel (kernel)
import Markovian.Kernel.Exact qualified as ExactKernel
import Markovian.Learning.EpsilonGreedy (
    EpsilonGreedyError (..),
    epsilonGreedyDistribution,
    sampleEpsilonGreedy,
 )
import Markovian.Learning.ExpectedSarsa
import Markovian.Learning.ExpectedSarsa.Episodic
import Markovian.Learning.QLearning
import Markovian.Learning.QLearning.Episodic
import Markovian.Learning.Sarsa
import Markovian.Learning.Sarsa.Episodic
import Markovian.Learning.TD0
import Markovian.Learning.TD0.Episodic
import Markovian.MDP (ActionId, MDP, actionId, mdp)
import Markovian.MDP.Exact qualified as ExactMDP
import Markovian.MRP (StateStatus (..), transitionOutcome)
import Markovian.Objective (mkDiscount)
import Markovian.Objective.Exact qualified as ExactObjective
import Markovian.Policy (policy)
import Markovian.Probability (dirac, outcomes, probability)
import Markovian.Probability.Exact qualified as ExactProbability
import Markovian.Reward (mkReward)
import Markovian.Reward.Exact qualified as ExactReward
import Markovian.Sampling (generatorFromSeed)
import Markovian.Trace (Trace (..), TraceStep (..))

data TestState = Source | Successor | TerminalState | Loop
    deriving (Eq, Show)

data TestAction = First | Second | Missing
    deriving (Eq, Show)

runTabularLearningTests :: (String -> IO () -> IO ()) -> IO ()
runTabularLearningTests run = do
    run "tabular continuing update equations" testContinuingTargets
    run "tabular terminal targets and boundaries" testTerminalTargets
    run "epsilon-greedy distribution semantics" testEpsilonGreedy
    run "tabular update rejection and atomicity" testFailures
    run "seeded SARSA carried-action execution" testSarsaCarriedActions
    run "bounded resumable tabular execution" testBoundedAndResumable

assert :: String -> Bool -> IO ()
assert message condition = unless condition (ioError (userError message))

requireRight :: (Show err) => String -> Either err value -> IO value
requireRight _ (Right value) = pure value
requireRight label (Left err) = ioError (userError (label ++ ": " ++ show err))

firstAction :: ActionId TestAction
firstAction = actionId First

secondAction :: ActionId TestAction
secondAction = actionId Second

missingAction :: ActionId TestAction
missingAction = actionId Missing

makeModel :: IO (MDP TestState TestAction)
makeModel = do
    zero <- requireRight "zero reward" (mkReward 0)
    terminal <- requireRight "terminal payoff" (mkReward 7)
    let status TerminalState = Terminal terminal
        status _ = Continuing
        available Source = [firstAction]
        available Successor = [firstAction, secondAction]
        available Loop = [firstAction, secondAction]
        available TerminalState = error "terminal action support was inspected"
    pure
        ( mdp
            Source
            status
            available
            (kernel (\(_, _) -> dirac (transitionOutcome zero TerminalState)))
        )

testContinuingTargets :: IO ()
testContinuingTargets = do
    model <- makeModel
    rate <- requireRight "learning rate" (mkLearningRate 0.5)
    discount <- requireRight "discount" (mkDiscount 0.5)
    epsilon <- requireRight "epsilon" (mkExplorationRate 0.2)
    reward <- requireRight "reward" (mkReward 2)
    qValues <-
        requireRight
            "Q table"
            ( qTable
                [ (QKey Source firstAction, 1)
                , (QKey Successor firstAction, 4)
                , (QKey Successor secondAction, 10)
                ]
            )
    vValues <- requireRight "V table" (vTable [(Source, 1), (Successor, 6)])
    let observed = ObservedTransition Source firstAction reward Successor
    qResult <- requireRight "Q-learning update" (updateQ rate discount model observed qValues)
    tdResult <- requireRight "TD(0) update" (updateTD0 rate discount model observed vValues)
    sarsaResult <-
        requireRight
            "SARSA update"
            (updateSarsa rate discount model observed (SarsaNextAction firstAction) qValues)
    expectedResult <-
        requireRight
            "Expected SARSA update"
            ( updateExpectedSarsa
                rate
                discount
                model
                observed
                (ExpectedSarsaContinuing epsilon)
                qValues
            )
    assert "Q-learning must use the greedy successor target" (qValue (qUpdateTarget qResult) == 7)
    assert "Q-learning update equation changed" (qValue (qUpdateNewValue qResult) == 4)
    assert "TD(0) must bootstrap from V(s')" (vValue (td0UpdateTarget tdResult) == 5)
    assert "TD(0) update equation changed" (vValue (td0UpdateNewValue tdResult) == 3)
    assert "SARSA must use the supplied next action" (qValue (sarsaUpdateTarget sarsaResult) == 4)
    assert "SARSA update equation changed" (qValue (sarsaUpdateNewValue sarsaResult) == 2.5)
    assert "Expected SARSA must use the epsilon-greedy expectation" (close (qValue (expectedSarsaUpdateTarget expectedResult)) 6.7)
    assert "Expected SARSA update equation changed" (close (qValue (expectedSarsaUpdateNewValue expectedResult)) 3.85)

    exactDiscount <- requireRight "exact differential discount" (ExactObjective.mkExactContractionDiscount (1 % 2))
    let exactModel =
            ExactMDP.exactMDP
                Source
                (\state -> if state == TerminalState then ExactMDP.ExactTerminal (ExactReward.exactReward 7) else ExactMDP.ExactContinuing)
                ( \state ->
                    case state of
                        Source -> [firstAction]
                        Successor -> [firstAction, secondAction]
                        _ -> []
                )
                ( ExactKernel.exactKernel
                    ( \(state, _) ->
                        ExactProbability.exactDirac
                            ( ExactMDP.exactTransitionOutcome
                                (ExactReward.exactReward (if state == Source then 2 else 0))
                                (if state == Source then Successor else TerminalState)
                            )
                    )
                )
    exactCompiled <-
        requireRight
            "exact differential compilation"
            (ExactCompile.compileExactMDP [Source, Successor, TerminalState] [firstAction, secondAction] exactModel)
    let exactValues =
            fmap
                ( \(index, state) ->
                    ( index
                    , ExactReward.exactReward
                        ( case ExactCompile.compiledSourceState state of
                            Source -> 1
                            Successor -> 10
                            _ -> 7
                        )
                    )
                )
                (ExactCompile.compiledStateEntries exactCompiled)
    exactActions <- requireRight "exact differential action values" (ExactControl.exactActionValues exactDiscount exactCompiled exactValues)
    case exactActions of
        (_, firstExact :| _) : _ ->
            assert
                "Q-learning target differs from exact Bellman action value"
                (qValue (qUpdateTarget qResult) == fromRational (ExactReward.exactRewardValue (ExactControl.exactActionValueReward firstExact)))
        _ -> ioError (userError "exact differential returned no source action")

testTerminalTargets :: IO ()
testTerminalTargets = do
    model <- makeModel
    rate <- requireRight "learning rate" (mkLearningRate 0.5)
    discount <- requireRight "discount" (mkDiscount 0.5)
    reward <- requireRight "reward" (mkReward 2)
    qValues <- requireRight "terminal Q table" (qTable [(QKey Source firstAction, 1)])
    vValues <- requireRight "terminal V table" (vTable [(Source, 1)])
    let observed = ObservedTransition Source firstAction reward TerminalState
    qResult <- requireRight "terminal Q-learning" (updateQ rate discount model observed qValues)
    tdResult <- requireRight "terminal TD(0)" (updateTD0 rate discount model observed vValues)
    sarsaResult <- requireRight "terminal SARSA" (updateSarsa rate discount model observed SarsaTerminal qValues)
    expectedResult <-
        requireRight
            "terminal Expected SARSA"
            (updateExpectedSarsa rate discount model observed ExpectedSarsaTerminal qValues)
    assert "Q-learning terminal target changed" (qValue (qUpdateTarget qResult) == 5.5)
    assert "TD(0) terminal target changed" (vValue (td0UpdateTarget tdResult) == 5.5)
    assert "SARSA terminal target changed" (qValue (sarsaUpdateTarget sarsaResult) == 5.5)
    assert "Expected SARSA terminal target changed" (qValue (expectedSarsaUpdateTarget expectedResult) == 5.5)
    assert "terminal Q update changed" (qValue (qUpdateNewValue qResult) == 3.25)
    assert "terminal TD update changed" (vValue (td0UpdateNewValue tdResult) == 3.25)

testEpsilonGreedy :: IO ()
testEpsilonGreedy = do
    epsilon <- requireRight "epsilon" (mkExplorationRate 0.2)
    tied <- requireRight "tied Q table" (qTable [(QKey Successor firstAction, 4), (QKey Successor secondAction, 4)])
    distribution <-
        requireRight
            "epsilon-greedy distribution"
            (epsilonGreedyDistribution epsilon tied Successor (firstAction :| [secondAction]))
    let entries = NonEmpty.toList (outcomes distribution)
    case entries of
        [(first, firstMass), (second, secondMass)] -> do
            assert "epsilon-greedy support order changed" ([first, second] == [firstAction, secondAction])
            assert "first tie must receive exploitation mass" (close (probability firstMass) 0.9)
            assert "non-greedy action exploration mass changed" (close (probability secondMass) 0.1)
        _ -> ioError (userError "epsilon-greedy distribution support changed")
    smallest <- requireRight "smallest positive epsilon" (mkExplorationRate minimumPositive)
    case epsilonGreedyDistribution smallest tied Successor (firstAction :| [secondAction]) of
        Left (PositiveExplorationMassUnderflow rejected 2) ->
            assert "underflow diagnostic changed epsilon" (rejected == minimumPositive)
        result -> ioError (userError ("positive epsilon underflow was accepted: " ++ showResult result))
    case epsilonGreedyDistribution epsilon tied Successor (firstAction :| [firstAction]) of
        Left (DuplicateEpsilonGreedyAction duplicate) ->
            assert "duplicate epsilon-greedy action changed" (duplicate == firstAction)
        result -> ioError (userError ("duplicate actions were accepted: " ++ showResult result))

testFailures :: IO ()
testFailures = do
    model <- makeModel
    rate <- requireRight "learning rate" (mkLearningRate 1)
    discount <- requireRight "discount" (mkDiscount 1)
    reward <- requireRight "reward" (mkReward 1)
    table <- requireRight "Q table" (qTable [(QKey Source firstAction, 3)])
    let observed = ObservedTransition Source firstAction reward Successor
    case updateSarsa rate discount model observed (SarsaNextAction missingAction) table of
        Left (SarsaUpdateUnavailableNextAction rejected) ->
            assert "unavailable SARSA action changed" (rejected == missingAction)
        result -> ioError (userError ("unavailable next action was accepted: " ++ show result))
    case updateExpectedSarsa rate discount model observed ExpectedSarsaTerminal table of
        Left ExpectedSarsaUpdateExpectedExplorationRate -> pure ()
        result -> ioError (userError ("continuing Expected SARSA omitted epsilon: " ++ show result))
    largestReward <- requireRight "largest reward" (mkReward largestFinite)
    huge <- requireRight "huge table" (qTable [(QKey Successor firstAction, largestFinite)])
    let overflowObserved = ObservedTransition Source firstAction largestReward Successor
    case updateQ rate discount model overflowObserved huge of
        Left (QUpdateArithmeticError (NonFiniteQValue _)) -> pure ()
        result -> ioError (userError ("overflowing target was accepted: " ++ show result))
    let originalEntries = qEntries huge
    assert "failed update mutated its input table" (qEntries huge == originalEntries)

testSarsaCarriedActions :: IO ()
testSarsaCarriedActions = do
    zero <- requireRight "loop reward" (mkReward 0)
    horizonOne <- requireRight "one episode" (mkHorizon 1)
    horizonTwo <- requireRight "two steps" (mkHorizon 2)
    rate <- requireRight "learning rate" (mkLearningRate 1)
    epsilon <- requireRight "full exploration" (mkExplorationRate 1)
    discount <- requireRight "discount" (mkDiscount 0)
    let loopModel =
            mdp
                Loop
                (const Continuing)
                (const [firstAction, secondAction])
                (kernel (\(_, _) -> dirac (transitionOutcome zero Loop)))
        config =
            sarsaConfig
                discount
                (ConstantLearningRate rate)
                (ConstantExploration epsilon)
                horizonOne
                horizonTwo
        seed = generatorFromSeed 73
        available = firstAction :| [secondAction]
    (expectedFirst, afterFirst) <- requireRight "first direct action" (sampleEpsilonGreedy epsilon emptyQTable Loop available seed)
    (expectedSecond, afterSecond) <- requireRight "second direct action" (sampleEpsilonGreedy epsilon emptyQTable Loop available afterFirst)
    (_, expectedGenerator) <- requireRight "boundary direct action" (sampleEpsilonGreedy epsilon emptyQTable Loop available afterSecond)
    result <- requireRight "seeded SARSA episode" (learnSarsaEpisodes config loopModel seed)
    case learnedSarsaEpisodes result of
        [episode] -> do
            let actions = fmap traceAction (traceSteps (sarsaEpisodeTrace episode))
                bootstraps = fmap (sarsaUpdateBootstrap . sarsaStepUpdate) (sarsaEpisodeSteps episode)
            assert "seeded SARSA trace actions changed" (actions == [expectedFirst, expectedSecond])
            assert "first SARSA target did not carry the next behavior action" (take 1 bootstraps == [SarsaNextAction expectedSecond])
            assert "SARSA step bound changed" (length actions == 2)
        episodes -> ioError (userError ("expected one SARSA episode, got " ++ show (length episodes)))
    assert "SARSA generator sequencing changed" (learnedSarsaGenerator result == expectedGenerator)

testBoundedAndResumable :: IO ()
testBoundedAndResumable = do
    zero <- requireRight "zero reward" (mkReward 0)
    rate <- requireRight "learning rate" (mkLearningRate 0.5)
    epsilon <- requireRight "epsilon" (mkExplorationRate 0.25)
    discount <- requireRight "discount" (mkDiscount 0.5)
    zeroEpisodes <- requireRight "zero episodes" (mkHorizon 0)
    oneEpisode <- requireRight "one episode" (mkHorizon 1)
    twoEpisodes <- requireRight "two episodes" (mkHorizon 2)
    zeroSteps <- requireRight "zero steps" (mkHorizon 0)
    oneStep <- requireRight "one step" (mkHorizon 1)
    let loopModel =
            mdp
                Loop
                (const Continuing)
                (const [firstAction, secondAction])
                (kernel (\(_, _) -> dirac (transitionOutcome zero Loop)))
        seed = generatorFromSeed 99
        rates = ConstantLearningRate rate
        exploration = ConstantExploration epsilon
        noEpisodeConfig = qLearningConfig discount rates exploration zeroEpisodes oneStep
        noStepConfig = qLearningConfig discount rates exploration oneEpisode zeroSteps
    noEpisodes <- requireRight "zero-episode Q-learning" (learnEpisodes noEpisodeConfig loopModel seed)
    noSteps <- requireRight "zero-step Q-learning" (learnEpisodes noStepConfig loopModel seed)
    assert "zero episodes changed the table" (learnedQTable noEpisodes == emptyQTable)
    assert "zero episodes consumed the generator" (learnedGenerator noEpisodes == seed)
    assert "zero steps changed the table" (learnedQTable noSteps == emptyQTable)
    assert "zero steps consumed the generator" (learnedGenerator noSteps == seed)

    let twoConfig = qLearningConfig discount rates exploration twoEpisodes oneStep
        oneConfig = qLearningConfig discount rates exploration oneEpisode oneStep
    uninterrupted <- requireRight "uninterrupted Q-learning" (learnEpisodes twoConfig loopModel seed)
    firstHalf <- requireRight "first Q-learning half" (learnEpisodes oneConfig loopModel seed)
    secondHalf <-
        requireRight
            "resumed Q-learning half"
            ( learnEpisodesFrom
                oneConfig
                loopModel
                (learnedQTable firstHalf)
                1
                (learnedUpdateCount firstHalf)
                (learnedGenerator firstHalf)
            )
    assert "split Q-learning table differs" (learnedQTable secondHalf == learnedQTable uninterrupted)
    assert "split Q-learning count differs" (learnedUpdateCount secondHalf == learnedUpdateCount uninterrupted)
    assert "split Q-learning generator differs" (learnedGenerator secondHalf == learnedGenerator uninterrupted)

    let tdConfig = td0Config discount rates oneEpisode zeroSteps
        suppliedPolicy = policy (kernel (const (dirac firstAction)))
        expectedConfig = expectedSarsaConfig discount rates exploration oneEpisode zeroSteps
    tdResult <- requireRight "zero-step TD(0)" (learnTD0Episodes tdConfig loopModel suppliedPolicy seed)
    expectedResult <- requireRight "zero-step Expected SARSA" (learnExpectedSarsaEpisodes expectedConfig loopModel seed)
    assert "zero-step TD(0) consumed the generator" (learnedTD0Generator tdResult == seed)
    assert "zero-step Expected SARSA consumed the generator" (learnedExpectedSarsaGenerator expectedResult == seed)

    tdWhole <- requireRight "uninterrupted TD(0)" (learnTD0Episodes (td0Config discount rates twoEpisodes oneStep) loopModel suppliedPolicy seed)
    tdFirst <- requireRight "first TD(0) half" (learnTD0Episodes (td0Config discount rates oneEpisode oneStep) loopModel suppliedPolicy seed)
    tdSecond <-
        requireRight
            "resumed TD(0) half"
            ( learnTD0EpisodesFrom
                (td0Config discount rates oneEpisode oneStep)
                loopModel
                suppliedPolicy
                (learnedVTable tdFirst)
                1
                (learnedTD0UpdateCount tdFirst)
                (learnedTD0Generator tdFirst)
            )
    assert "split TD(0) table differs" (learnedVTable tdSecond == learnedVTable tdWhole)
    assert "split TD(0) count differs" (learnedTD0UpdateCount tdSecond == learnedTD0UpdateCount tdWhole)
    assert "split TD(0) generator differs" (learnedTD0Generator tdSecond == learnedTD0Generator tdWhole)

    let sarsaOne = sarsaConfig discount rates exploration oneEpisode oneStep
        sarsaTwo = sarsaConfig discount rates exploration twoEpisodes oneStep
    sarsaWhole <- requireRight "uninterrupted SARSA" (learnSarsaEpisodes sarsaTwo loopModel seed)
    sarsaFirst <- requireRight "first SARSA half" (learnSarsaEpisodes sarsaOne loopModel seed)
    sarsaSecond <-
        requireRight
            "resumed SARSA half"
            ( learnSarsaEpisodesFrom
                sarsaOne
                loopModel
                (learnedSarsaTable sarsaFirst)
                1
                (learnedSarsaUpdateCount sarsaFirst)
                (learnedSarsaGenerator sarsaFirst)
            )
    assert "split SARSA table differs" (learnedSarsaTable sarsaSecond == learnedSarsaTable sarsaWhole)
    assert "split SARSA count differs" (learnedSarsaUpdateCount sarsaSecond == learnedSarsaUpdateCount sarsaWhole)
    assert "split SARSA generator differs" (learnedSarsaGenerator sarsaSecond == learnedSarsaGenerator sarsaWhole)

    let expectedOne = expectedSarsaConfig discount rates exploration oneEpisode oneStep
        expectedTwo = expectedSarsaConfig discount rates exploration twoEpisodes oneStep
    expectedWhole <- requireRight "uninterrupted Expected SARSA" (learnExpectedSarsaEpisodes expectedTwo loopModel seed)
    expectedFirst <- requireRight "first Expected SARSA half" (learnExpectedSarsaEpisodes expectedOne loopModel seed)
    expectedSecond <-
        requireRight
            "resumed Expected SARSA half"
            ( learnExpectedSarsaEpisodesFrom
                expectedOne
                loopModel
                (learnedExpectedSarsaTable expectedFirst)
                1
                (learnedExpectedSarsaUpdateCount expectedFirst)
                (learnedExpectedSarsaGenerator expectedFirst)
            )
    assert "split Expected SARSA table differs" (learnedExpectedSarsaTable expectedSecond == learnedExpectedSarsaTable expectedWhole)
    assert
        "split Expected SARSA count differs"
        (learnedExpectedSarsaUpdateCount expectedSecond == learnedExpectedSarsaUpdateCount expectedWhole)
    assert
        "split Expected SARSA generator differs"
        (learnedExpectedSarsaGenerator expectedSecond == learnedExpectedSarsaGenerator expectedWhole)

close :: Double -> Double -> Bool
close left right = abs (left - right) <= 1e-12

minimumPositive :: Double
minimumPositive =
    let sample = 0 :: Double
        digits = floatDigits sample
        (lowerExponent, _) = floatRange sample
     in encodeFloat 1 (lowerExponent - digits)

largestFinite :: Double
largestFinite = encodeFloat (2 ^ (53 :: Int) - 1) (snd (floatRange (0 :: Double)) - 53)

showResult :: Either err value -> String
showResult (Left _) = "unexpected left"
showResult (Right _) = "unexpected right"
