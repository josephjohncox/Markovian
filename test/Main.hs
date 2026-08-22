module Main (main) where

import Control.Monad (replicateM)
import Control.Monad.Bayes.Sampler.Strict (sampleIO)
import Control.Monad.Random (evalRandIO)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Markovian (
    Action (..),
    Process (..),
    buildMDPF,
    evaluateMDPExpect,
    evaluateMDPSample,
 )
import Markovian.Kernel (kernel)
import Markovian.MDP (
    Decision (..),
    MDP,
    ModelError (..),
    actionId,
    inspectMDP,
    mdp,
    stepMDP,
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
import Markovian.Policy (policy, policyActions)
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
 )
import Markovian.Reward (
    RewardError (..),
    mkReward,
    rewardValue,
 )
import QLearning (qLearning)
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
    run "legacy terminal value" testLegacyTerminalValue
    run "legacy deterministic chain" testLegacyDeterministicChain
    run "legacy expectation and sample support" testLegacyExpectationAndSampleSupport
    run "legacy Q-learning identity boundaries" testLegacyQLearningIdentity

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

testLegacyTerminalValue :: IO ()
testLegacyTerminalValue = do
    expectedValue <- sampleIO (evaluateMDPExpect legacyTerminalProcess)
    sampledValue <- sampleIO (evaluateMDPSample legacyTerminalProcess)
    assert "legacy expectation must retain the terminal reward" (expectedValue == 7)
    assert "legacy sampling must retain the terminal reward" (sampledValue == 7)

testLegacyDeterministicChain :: IO ()
testLegacyDeterministicChain = do
    expectedValue <- sampleIO (evaluateMDPExpect legacyChainProcess)
    sampledValue <- sampleIO (evaluateMDPSample legacyChainProcess)
    assert "legacy expectation must sum deterministic state rewards" (expectedValue == 6)
    assert "legacy sampling must sum deterministic state rewards" (sampledValue == 6)

testLegacyExpectationAndSampleSupport :: IO ()
testLegacyExpectationAndSampleSupport = do
    expectedValue <- sampleIO (evaluateMDPExpect legacyChoiceProcess)
    sampledValues <- replicateM 64 (sampleIO (evaluateMDPSample legacyChoiceProcess))
    assert "legacy expectation must normalize to 12.5" (expectedValue == 12.5)
    assert
        "legacy samples must remain in the declared support"
        (all (`elem` [10, 15]) sampledValues)

testLegacyQLearningIdentity :: IO ()
testLegacyQLearningIdentity = do
    let initialTable = Map.singleton (LegacyChainStart, "next") 3.5
        chainTree = buildMDPF legacyChainProcess LegacyChainStart
        terminalTree = buildMDPF legacyTerminalProcess LegacyTerminal
    zeroEpisodeTable <- evalRandIO (qLearning chainTree 0 initialTable)
    terminalTable <- evalRandIO (qLearning terminalTree 5 initialTable)
    assert "zero Q-learning episodes must preserve the table" (zeroEpisodeTable == initialTable)
    assert "a terminal initial state must preserve the table" (terminalTable == initialTable)

legacyTerminalProcess :: Process LegacyState
legacyTerminalProcess =
    Process
        { initialState = LegacyTerminal
        , isTerminal = const True
        , processReward = const 7
        , processActions = const Vector.empty
        }

legacyChainProcess :: Process LegacyState
legacyChainProcess =
    Process
        { initialState = LegacyChainStart
        , isTerminal = (== LegacyTerminal)
        , processReward = chainReward
        , processActions = chainActions
        }
  where
    chainReward LegacyChainStart = 1
    chainReward LegacyChainMiddle = 2
    chainReward LegacyTerminal = 3
    chainReward _ = 0

    chainActions LegacyChainStart = Vector.singleton (Action "next" 1 LegacyChainMiddle)
    chainActions LegacyChainMiddle = Vector.singleton (Action "finish" 1 LegacyTerminal)
    chainActions _ = Vector.empty

legacyChoiceProcess :: Process LegacyState
legacyChoiceProcess =
    Process
        { initialState = LegacyChoice
        , isTerminal = (`elem` [LegacyLow, LegacyHigh])
        , processReward = choiceReward
        , processActions = choiceActions
        }
  where
    choiceReward LegacyLow = 10
    choiceReward LegacyHigh = 15
    choiceReward _ = 0

    choiceActions LegacyChoice =
        Vector.fromList
            [ Action "low" 0.5 LegacyLow
            , Action "high" 0.5 LegacyHigh
            ]
    choiceActions _ = Vector.empty

data LegacyState
    = LegacyTerminal
    | LegacyChainStart
    | LegacyChainMiddle
    | LegacyChoice
    | LegacyLow
    | LegacyHigh
    deriving (Eq, Ord, Show)

data TestState = Start | End
    deriving (Eq, Show)

data TestAction = Finish
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
