module FeedbackExact (runFeedbackExactTests) where

import Control.Monad (unless)
import Data.Ratio ((%))
import Markovian.Algebra.NonNegativeRational
import Markovian.Category.Finite.Set
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Stochastic
import Markovian.Feedback.Channel.Exact
import Markovian.Feedback.Delay.Exact qualified as Delay
import Markovian.Feedback.Timed.Exact qualified as Timed
import Markovian.Horizon (mkHorizon)
import Markovian.Objective.Exact (mkExactDiscount)
import Markovian.Reward.Exact (exactReward, exactRewardValue)
import Numeric.Natural (Natural)

runFeedbackExactTests :: (String -> IO () -> IO ()) -> IO ()
runFeedbackExactTests run = do
    run "feedback raw trace normalization counterexample" testRawTrace
    run "feedback nilpotent first exit" testNilpotent
    run "feedback proper geometric first exit" testProper
    run "feedback eventual contraction witness" testEventualContraction
    run "feedback independent multi-output path enumeration" testMultiOutputPaths
    run "feedback closed class rejection" testClosed
    run "feedback atomic resource budgets" testResourceBudgets
    run "feedback endpoint layouts and empty loop" testLayoutsAndEmptyLoop
    run "feedback delayed zero and finite horizon" testDelayed
    run "feedback delayed stochastic correlation" testDelayedStochastic
    run "feedback timed reward and duration" testTimed
    run "feedback timed probabilistic duration mixture" testTimedDurationMixture
    run "feedback timed cyclic rejection" testTimedCyclic
    run "feedback complete discarded-intermediate accounting" testCompleteAccounting
    run "feedback fixed-point counterexamples" testFixedPointCounterexamples

data Input = Input deriving (Eq, Show)
data Loop = Loop | LoopA | LoopB deriving (Eq, Show)
data Output = Output deriving (Eq, Show)
data CorrelatedOutput = CorrelatedLeft | CorrelatedRight deriving (Eq, Show)

largeLimits :: FeedbackLimits
largeLimits = feedbackLimits 100 100 100 100 1000000 1000000 10000000 100000 4096

set :: (Eq a, Show a) => [a] -> FiniteSet a
set values = right ("finite set " ++ show values) (finiteSet values)

scalar :: Rational -> NonNegativeRational
scalar value = right "nonnegative rational" (nonNegativeRational value)

channel :: FiniteSet source -> FiniteSet target -> [[Rational]] -> StochasticMatrix NonNegativeRational source target
channel source target rows =
    case matrixFromRows source target (map (map scalar) rows) of
        Left failure -> error ("matrix construction failed: " ++ show failure)
        Right matrix -> right "stochastic channel" (stochasticMatrix matrix)

entry :: StochasticMatrix NonNegativeRational source target -> source -> target -> Rational
entry matrix source target = maybe 0 getNonNegativeRational (matrixEntry (forgetStochastic matrix) source target)

testRawTrace :: IO ()
testRawTrace = do
    let unit = set [()]
        loops = set [False, True]
        productObject = set [((), False), ((), True)]
        identity = identityMatrix productObject :: Matrix NonNegativeRational ((), Bool) ((), Bool)
    traced <- rightIO "raw trace" (traceMatrix unit unit loops identity)
    assert "raw matrix trace of two-state identity was not mass two" (matrixEntry traced () () == Just (scalar 2))
    case stochasticMatrix traced of
        Left (StochasticRowNotNormalized 0 mass) -> assert "wrong trace mass" (mass == scalar 2)
        Right _ -> fail "raw trace unexpectedly normalized"
        Left failure -> fail ("wrong raw trace failure: " ++ show failure)

testNilpotent :: IO ()
testNilpotent = do
    checked <- rightIO "nilpotent feedback" (closeProperFeedback largeLimits inputs loopOwner outputs nilpotentRouting)
    assert "nilpotent first-exit mass changed" (entry (feedbackChannel checked) Input Output == 1)
    assert "nilpotence class changed" (exactFeedbackClass (feedbackReport checked) == NilpotentLoop 1)
    repeated <- rightIO "repeated nilpotent feedback" (closeProperFeedback largeLimits inputs loopOwner outputs nilpotentRouting)
    assert "feedback report was nondeterministic" (feedbackReport checked == feedbackReport repeated)

testProper :: IO ()
testProper = do
    checked <- rightIO "proper feedback" (closeProperFeedback largeLimits inputs loopOwner outputs properRouting)
    assert "geometric first-exit mass changed" (entry (feedbackChannel checked) Input Output == 1)
    assert "proper class changed" (exactFeedbackClass (feedbackReport checked) == ProperTransientLoop 1 (1 % 2))

testEventualContraction :: IO ()
testEventualContraction = do
    let loops = set [LoopA, LoopB]
        source = set [Left Input, Right LoopA, Right LoopB]
        target = set [Left Output, Right LoopA, Right LoopB]
        routing = channel source target [[0, 1, 0], [0, 0, 1], [1 % 2, 0, 1 % 2]]
    case closeProperFeedback (feedbackLimits 1 2 1 10 9 6 57 1000 1024) inputs (loopLayout "eventual" loops) outputs routing of
        Left (ExactFeedbackLimitError (FeedbackLimitExceeded FeedbackArithmeticWork 57 58)) -> pure ()
        other -> fail ("two-state one-below arithmetic budget was not atomic: " ++ showResult other)
    checked <- rightIO "eventual contraction exact budget" (closeProperFeedback (feedbackLimits 1 2 1 10 9 6 58 1000 1024) inputs (loopLayout "eventual" loops) outputs routing)
    assert "two-state arithmetic charge changed" (exactFeedbackArithmeticWorkCharged (feedbackReport checked) == 58)
    assert "one-step norm was incorrectly required" (exactFeedbackClass (feedbackReport checked) == ProperTransientLoop 2 (1 % 2))
    assert "eventual first-exit mass changed" (entry (feedbackChannel checked) Input Output == 1)

-- This oracle enumerates acyclic paths and never calls the absorption solver.
testMultiOutputPaths :: IO ()
testMultiOutputPaths = do
    let loopSet = set [LoopA, LoopB]
        outputSet = set [CorrelatedLeft, CorrelatedRight]
        source = set [Left Input, Right LoopA, Right LoopB]
        target = set [Left CorrelatedLeft, Left CorrelatedRight, Right LoopA, Right LoopB]
        routing = channel source target [[0, 0, 1, 0], [1 % 2, 0, 0, 1 % 2], [0, 1, 0, 0]]
        oracle = pathMass (3 :: Natural) routing (Left Input)
    checked <- rightIO "multi-output feedback" (closeProperFeedback largeLimits inputs (loopLayout "multi" loopSet) outputSet routing)
    assert "independent path oracle left mismatch" (entry (feedbackChannel checked) Input CorrelatedLeft == oracle CorrelatedLeft)
    assert "independent path oracle right mismatch" (entry (feedbackChannel checked) Input CorrelatedRight == oracle CorrelatedRight)
    assert "multi-output absorption masses changed" (oracle CorrelatedLeft == 1 % 2 && oracle CorrelatedRight == 1 % 2)

    let reversedLoops = set [LoopB, LoopA]
        reversedSource = set [Left Input, Right LoopB, Right LoopA]
        reversedTarget = set [Left CorrelatedLeft, Left CorrelatedRight, Right LoopB, Right LoopA]
        reversedRouting = channel reversedSource reversedTarget [[0, 0, 0, 1], [0, 1, 0, 0], [1 % 2, 0, 1 % 2, 0]]
    permuted <- rightIO "permuted multi-output feedback" (closeProperFeedback largeLimits inputs (loopLayout "multi" reversedLoops) outputSet reversedRouting)
    assert "loop layout permutation changed absorption" (entry (feedbackChannel permuted) Input CorrelatedLeft == 1 % 2 && entry (feedbackChannel permuted) Input CorrelatedRight == 1 % 2)
  where
    pathMass 0 _ _ _ = 0
    pathMass fuel routing source output =
        entry routing source (Left output)
            + sum
                [ entry routing source (Right nextLoop) * pathMass (fuel - 1) routing (Right nextLoop) output
                | nextLoop <- [LoopA, LoopB]
                ]

testClosed :: IO ()
testClosed =
    case closeProperFeedback largeLimits inputs loopOwner outputs closedRouting of
        Left (FeedbackClosedInternalClass [Loop]) -> pure ()
        other -> fail ("closed class was accepted: " ++ showResult other)

testResourceBudgets :: IO ()
testResourceBudgets = do
    expectLimit FeedbackSourceCount 0 1 (feedbackLimits 0 10 10 10 100 1000 10000 1000 1024)
    expectLimit FeedbackLoopCount 0 1 (feedbackLimits 10 0 10 10 100 1000 10000 1000 1024)
    expectLimit FeedbackOutputCount 0 1 (feedbackLimits 10 10 0 10 100 1000 10000 1000 1024)
    expectLimit FeedbackMatrixCellCount 3 4 (feedbackLimits 10 10 10 10 3 1000 10000 1000 1024)
    expectLimit FeedbackGraphWork 1 2 (feedbackLimits 10 10 10 10 4 1 10000 1000 1024)
    expectLimit FeedbackArithmeticWork 9 10 (feedbackLimits 10 10 10 10 4 2 9 1000 1024)
    case closeProperFeedback (feedbackLimits 10 10 10 10 4 2 11 1000 0) inputs loopOwner outputs nilpotentRouting of
        Left (ExactFeedbackLimitError (FeedbackRationalBitsExceeded _ 0 1)) -> pure ()
        other -> fail ("rational bit budget did not fail atomically: " ++ showResult other)
    checked <- rightIO "exact resource budgets" (closeProperFeedback (feedbackLimits 1 1 1 10 4 2 10 1000 1) inputs loopOwner outputs nilpotentRouting)
    assert "exact-limit execution changed denotation" (entry (feedbackChannel checked) Input Output == 1)
    assert "exact arithmetic accounting changed" (exactFeedbackArithmeticWorkCharged (feedbackReport checked) == 10)
  where
    expectLimit dimension limit required limits =
        case closeProperFeedback limits inputs loopOwner outputs nilpotentRouting of
            Left (ExactFeedbackLimitError (FeedbackLimitExceeded actualDimension actualLimit actualRequired)) ->
                assert "wrong resource limit failure" (actualDimension == dimension && actualLimit == limit && actualRequired == required)
            other -> fail ("one-below resource budget did not fail first: " ++ showResult other)

testLayoutsAndEmptyLoop :: IO ()
testLayoutsAndEmptyLoop = do
    let reversedSource = set [Right Loop, Left Input]
        routing = channel reversedSource (set [Left Output, Right Loop]) [[1, 0], [0, 1]]
    case closeProperFeedback largeLimits inputs loopOwner outputs routing of
        Left FeedbackSourceLayoutMismatch -> pure ()
        other -> fail ("source layout mismatch was accepted: " ++ showResult other)
    let noLoops = set ([] :: [Loop])
        direct = channel (set [Left Input]) (set [Left Output]) [[1]]
    checked <- rightIO "empty-loop feedback" (closeProperFeedback largeLimits inputs (loopLayout "empty" noLoops) outputs direct)
    assert "empty-loop class changed" (exactFeedbackClass (feedbackReport checked) == EmptyLoop)
    assert "empty-loop direct mass changed" (entry (feedbackChannel checked) Input Output == 1)

testDelayed :: IO ()
testDelayed = do
    let inputSet = set [()]
        stateSet = set [False, True]
        outputSet = set [False, True]
        seed = channel inputSet stateSet [[1, 0]]
        bodySource = set [((), False), ((), True)]
        bodyTarget = set [(False, False), (False, True), (True, False), (True, True)]
        body = channel bodySource bodyTarget [[0, 1, 0, 0], [0, 0, 1, 0]]
    checked <- rightIO "delayed feedback" (Delay.closeDelayedFeedback largeLimits inputSet (loopLayout "delay" stateSet) outputSet seed body)
    zero <- rightIO "zero horizon" (mkHorizon 0)
    (zeroTrace, zeroReport) <- rightIO "zero delayed trace" (Delay.observeDelayedTrace largeLimits zero checked)
    let emptyTrace = Delay.DelayedFeedbackTrace False []
    assert "zero horizon consumed a body step" (entry zeroTrace () emptyTrace == 1)
    assert "zero horizon report changed" (Delay.delayedFeedbackTicks zeroReport == 0)
    two <- rightIO "two horizon" (mkHorizon 2)
    case Delay.observeDelayedTrace (feedbackLimits 10 10 10 1 100 100 100000 1000 1024) two checked of
        Left (Delay.DelayedFeedbackLimitError (FeedbackLimitExceeded FeedbackTickCount 1 2)) -> pure ()
        other -> fail ("delayed tick budget was not enforced: " ++ showDelayedTrace other)
    case Delay.observeDelayedTrace (feedbackLimits 10 10 10 2 100 100 100000 31 1024) two checked of
        Left (Delay.DelayedFeedbackLimitError (FeedbackLimitExceeded FeedbackTraceOutcomeCount 31 32)) -> pure ()
        other -> fail ("delayed outcome budget was not enforced: " ++ showDelayedTrace other)
    (traceLaw, _) <- rightIO "two delayed trace" (Delay.observeDelayedTrace largeLimits two checked)
    let expectedTrace = Delay.DelayedFeedbackTrace False [Delay.FeedbackStep False True, Delay.FeedbackStep True False]
    assert "joint delayed trace changed" (entry traceLaw () expectedTrace == 1)
    ticks <- rightIO "positive delayed ticks" (Delay.feedbackTicks 2)
    (finalLaw, _) <- rightIO "delayed final" (Delay.observeDelayedFinal largeLimits ticks checked)
    assert "delayed final output-state correlation changed" (entry finalLaw () (True, False) == 1)
    case Delay.feedbackTicks 0 of
        Left Delay.FeedbackTicksMustBePositive -> pure ()
        other -> fail ("zero final ticks accepted: " ++ show other)

testDelayedStochastic :: IO ()
testDelayedStochastic = do
    let inputSet = set [()]
        stateSet = set [False, True]
        outputSet = set [False, True]
        seed = channel inputSet stateSet [[1 % 2, 1 % 2]]
        bodySource = set [((), False), ((), True)]
        bodyTarget = set [(False, False), (False, True), (True, False), (True, True)]
        body = channel bodySource bodyTarget [[1 % 2, 0, 0, 1 % 2], [0, 0, 1, 0]]
    checked <- rightIO "stochastic delayed feedback" (Delay.closeDelayedFeedback largeLimits inputSet (loopLayout "stochastic-delay" stateSet) outputSet seed body)
    ticks <- rightIO "one stochastic tick" (Delay.feedbackTicks 1)
    (finalLaw, _) <- rightIO "stochastic delayed final" (Delay.observeDelayedFinal largeLimits ticks checked)
    assert "delayed output-successor correlation changed" (entry finalLaw () (False, False) == 1 % 4 && entry finalLaw () (True, True) == 1 % 4 && entry finalLaw () (True, False) == 1 % 2 && entry finalLaw () (False, True) == 0)

testTimed :: IO ()
testTimed = do
    discount <- rightIO "timed discount" (mkExactDiscount (1 % 2))
    let events = set [Timed.Continue (exactReward 2) Loop, Timed.Exit (exactReward 3) Output]
        routing = channel (set [Left Input, Right Loop]) events [[1, 0], [0, 1]]
    case Timed.closeNilpotentTimedFeedback (feedbackLimits 10 10 10 10 100 100 100000 3 1024) discount inputs loopOwner outputs routing of
        Left (Timed.TimedFeedbackLimitError (FeedbackLimitExceeded FeedbackTraceOutcomeCount 3 4)) -> pure ()
        other -> fail ("timed path budget was not enforced: " ++ showTimed other)
    checked <- rightIO "timed feedback" (Timed.closeNilpotentTimedFeedback largeLimits discount inputs loopOwner outputs routing)
    let result = Timed.TimedFeedbackExit (exactReward (7 % 2)) 2 Output
    assert "timed reward-duration-output law changed" (entry (Timed.timedFeedbackChannel checked) Input result == 1)
    assert "timed maximum duration changed" (Timed.timedFeedbackMaximumDuration (Timed.timedFeedbackReport checked) == 2)
    assert "timed reward accessor changed" (exactRewardValue (Timed.timedFeedbackReward result) == 7 % 2)

    let correlatedOutputs = set [CorrelatedLeft, CorrelatedRight]
        correlatedEvents = set [Timed.Exit (exactReward 1) CorrelatedLeft, Timed.Exit (exactReward 2) CorrelatedRight]
        correlatedRouting = channel (set [Left Input, Right Loop]) correlatedEvents [[1 % 2, 1 % 2], [1 % 2, 1 % 2]]
    correlated <- rightIO "correlated timed feedback" (Timed.closeNilpotentTimedFeedback largeLimits discount inputs loopOwner correlatedOutputs correlatedRouting)
    let leftResult = Timed.TimedFeedbackExit (exactReward 1) 1 CorrelatedLeft
        rightResult = Timed.TimedFeedbackExit (exactReward 2) 1 CorrelatedRight
    assert "timed reward-output correlation was replaced by marginals" (entry (Timed.timedFeedbackChannel correlated) Input leftResult == 1 % 2 && entry (Timed.timedFeedbackChannel correlated) Input rightResult == 1 % 2)

testTimedDurationMixture :: IO ()
testTimedDurationMixture = do
    discount <- rightIO "duration-mixture discount" (mkExactDiscount (1 % 2))
    let events = set [Timed.Exit (exactReward 1) Output, Timed.Continue (exactReward 2) Loop, Timed.Exit (exactReward 3) Output]
        routing = channel (set [Left Input, Right Loop]) events [[1 % 2, 1 % 2, 0], [0, 0, 1]]
    checked <- rightIO "timed duration mixture" (Timed.closeNilpotentTimedFeedback largeLimits discount inputs loopOwner outputs routing)
    let immediate = Timed.TimedFeedbackExit (exactReward 1) 1 Output
        delayed = Timed.TimedFeedbackExit (exactReward (7 % 2)) 2 Output
    assert "probabilistic duration mixture lost immediate branch" (entry (Timed.timedFeedbackChannel checked) Input immediate == 1 % 2)
    assert "probabilistic duration mixture lost delayed branch" (entry (Timed.timedFeedbackChannel checked) Input delayed == 1 % 2)

testTimedCyclic :: IO ()
testTimedCyclic = do
    discount <- rightIO "cyclic discount" (mkExactDiscount 1)
    let events = set [Timed.Continue (exactReward 1) Loop, Timed.Exit (exactReward 1) Output]
        routing = channel (set [Left Input, Right Loop]) events [[1, 0], [1 % 2, 1 % 2]]
    case Timed.closeNilpotentTimedFeedback largeLimits discount inputs loopOwner outputs routing of
        Left Timed.TimedFeedbackRequiresNilpotent -> pure ()
        other -> fail ("cyclic timed feedback was accepted: " ++ showTimed other)

testCompleteAccounting :: IO ()
testCompleteAccounting = do
    testGaussianAccounting
    testDelayedAccounting
    testTimedAccounting
  where
    testGaussianAccounting = do
        let loopSet = set [LoopA, LoopB]
            source = set [Left Input, Right LoopA, Right LoopB]
            target = set [Left Output, Right LoopA, Right LoopB]
            -- D = [[1/2,1/3],[1/3,1/2]] and C = [[1/6],[1/6]].
            -- The independent products in D^2 include denominator 36 and
            -- Gaussian elimination forms 5/18; the retained exit mass is 1.
            routing = channel source target [[0, 1, 0], [1 % 6, 1 % 2, 1 % 3], [1 % 6, 1 % 3, 1 % 2]]
            limits work = feedbackLimits 1 2 1 10 9 6 work 1000
        checked <- rightIO "accounted Gaussian feedback" (closeProperFeedback (limits 58 6) inputs (loopLayout "accounting" loopSet) outputs routing)
        let report = feedbackReport checked
            accounting = exactFeedbackAccounting report
        assert "independent exact arithmetic oracle changed" (exactFeedbackArithmeticWorkCharged report == 58)
        assert "input maximum omitted" (feedbackMaximumInputBits accounting == 3)
        assert "discarded matrix-power maximum omitted" (feedbackMaximumMatrixPowerBits accounting == 6)
        assert "discarded Gaussian maximum omitted" (feedbackMaximumGaussianIntermediateBits accounting == 5)
        assert "retained exact maximum changed" (feedbackMaximumRetainedResultBits accounting == 1)
        assert "overall exact maximum changed" (feedbackMaximumObservedBits accounting == 6)
        assert "discarded exact intermediates did not exceed retained results" (feedbackMaximumGaussianIntermediateBits accounting > feedbackMaximumRetainedResultBits accounting)
        golden <- readFile "test/golden/feedback-accounting.txt"
        assert "feedback accounting golden changed" (show accounting ++ "\n" == golden)
        repeated <- rightIO "repeated accounted Gaussian feedback" (closeProperFeedback (limits 58 6) inputs (loopLayout "accounting" loopSet) outputs routing)
        assert "complete feedback report was nondeterministic" (feedbackReport repeated == report)
        case closeProperFeedback (limits 57 6) inputs (loopLayout "accounting" loopSet) outputs routing of
            Left (ExactFeedbackLimitError (FeedbackLimitExceeded FeedbackArithmeticWork 57 58)) -> pure ()
            other -> fail ("exact one-below work was not atomic: " ++ showResult other)
        case closeProperFeedback (limits 58 5) inputs (loopLayout "accounting" loopSet) outputs routing of
            Left (ExactFeedbackLimitError (FeedbackRationalBitsExceeded "matrix power sum" 5 6)) -> pure ()
            other -> fail ("exact one-below rational limit was not atomic: " ++ showResult other)

    testDelayedAccounting = do
        let inputSet = set [()]
            stateSet = set [False, True]
            outputSet = set [()]
            seed = channel inputSet stateSet [[1 % 3, 2 % 3]]
            body = channel (set [((), False), ((), True)]) (set [((), False), ((), True)]) [[1, 0], [1, 0]]
            limits work = feedbackLimits 1 2 1 1 6 100 work 2
        closed <- rightIO "accounted delayed feedback" (Delay.closeDelayedFeedback largeLimits inputSet (loopLayout "delayed-accounting" stateSet) outputSet seed body)
        ticks <- rightIO "accounted delayed tick" (Delay.feedbackTicks 1)
        (result, report) <- rightIO "accounted delayed final" (Delay.observeDelayedFinal (limits 14 2) ticks closed)
        let accounting = Delay.delayedFeedbackAccounting report
        assert "delayed accounting oracle result changed" (entry result () ((), False) == 1 && entry result () ((), True) == 0)
        -- Four target/source-state pairs each charge branch, product, and
        -- sum, followed by two independently accounted normalization sums.
        assert "independent delayed work oracle changed" (Delay.delayedFeedbackArithmeticWorkCharged report == 14)
        assert "discarded delayed path maximum omitted" (feedbackMaximumDelayedPathIntermediateBits accounting == 2)
        assert "delayed retained maximum changed" (feedbackMaximumRetainedResultBits accounting == 1)
        assert "delayed overall maximum changed" (feedbackMaximumObservedBits accounting == 2)
        assert "discarded delayed path did not exceed retained result" (feedbackMaximumDelayedPathIntermediateBits accounting > feedbackMaximumRetainedResultBits accounting)
        case Delay.observeDelayedFinal (limits 13 2) ticks closed of
            Left (Delay.DelayedFeedbackLimitError (FeedbackLimitExceeded FeedbackArithmeticWork 13 14)) -> pure ()
            other -> fail ("delayed one-below work was not atomic: " ++ showDelayedFinal other)
        case Delay.observeDelayedFinal (limits 14 1) ticks closed of
            Left (Delay.DelayedFeedbackLimitError (FeedbackRationalBitsExceeded "delayed seed input" 1 2)) -> pure ()
            other -> fail ("delayed one-below rational limit was not atomic: " ++ showDelayedFinal other)

    testTimedAccounting = do
        discount <- rightIO "accounted timed discount" (mkExactDiscount (1 % 2))
        let loopSet = set [LoopA, LoopB]
            events = set [Timed.Continue (exactReward 0) LoopA, Timed.Continue (exactReward 0) LoopB, Timed.Exit (exactReward 0) Output]
            routing = channel (set [Left Input, Right LoopA, Right LoopB]) events [[1 % 3, 2 % 3, 0], [0, 0, 1], [0, 0, 1]]
            limits work = feedbackLimits 1 2 1 10 9 12 work 9
        checked <- rightIO "accounted timed feedback" (Timed.closeNilpotentTimedFeedback (limits 51 2) discount inputs (loopLayout "timed-accounting" loopSet) outputs routing)
        let report = Timed.timedFeedbackReport checked
            accounting = Timed.timedFeedbackAccounting report
            outcome = Timed.TimedFeedbackExit (exactReward 0) 2 Output
        assert "timed accounting oracle result changed" (entry (Timed.timedFeedbackChannel checked) Input outcome == 1)
        -- 16 discarded D^2 product/sum operations, four continuation
        -- aggregates, 28 charged path visits/operations, two mass sums, and
        -- one independently accounted normalization sum.
        assert "independent timed work oracle changed" (Timed.timedFeedbackArithmeticWorkCharged report == 51)
        assert "discarded timed path maximum omitted" (feedbackMaximumTimedPathIntermediateBits accounting == 2)
        assert "timed retained maximum changed" (feedbackMaximumRetainedResultBits accounting == 1)
        assert "timed overall maximum changed" (feedbackMaximumObservedBits accounting == 2)
        assert "discarded timed path did not exceed retained result" (feedbackMaximumTimedPathIntermediateBits accounting > feedbackMaximumRetainedResultBits accounting)
        case Timed.closeNilpotentTimedFeedback (limits 50 2) discount inputs (loopLayout "timed-accounting" loopSet) outputs routing of
            Left (Timed.TimedFeedbackLimitError (FeedbackLimitExceeded FeedbackArithmeticWork 50 51)) -> pure ()
            other -> fail ("timed one-below work was not atomic: " ++ showTimed other)
        case Timed.closeNilpotentTimedFeedback (limits 51 1) discount inputs (loopLayout "timed-accounting" loopSet) outputs routing of
            Left (Timed.TimedFeedbackLimitError (FeedbackRationalBitsExceeded "timed discount input" 1 2)) -> pure ()
            other -> fail ("timed one-below rational limit was not atomic: " ++ showTimed other)

testFixedPointCounterexamples :: IO ()
testFixedPointCounterexamples = do
    assert "not unexpectedly has a fixed point" (null [value | value <- [False, True], not value == value])
    assert "identity did not have two fixed points" (length [value | value <- [False, True], value == value] == 2)
    let iterateNot fuel seedValue = iterate not seedValue !! fuel
    assert "finite fuel was mistaken for a fixed point" (iterateNot 3 False /= iterateNot 4 False)

inputs :: FiniteSet Input
inputs = set [Input]

outputs :: FiniteSet Output
outputs = set [Output]

loopOwner :: LoopLayout String Loop
loopOwner = loopLayout "loop-owner" (set [Loop])

nilpotentRouting :: StochasticMatrix NonNegativeRational (Either Input Loop) (Either Output Loop)
nilpotentRouting = channel (set [Left Input, Right Loop]) (set [Left Output, Right Loop]) [[0, 1], [1, 0]]

properRouting :: StochasticMatrix NonNegativeRational (Either Input Loop) (Either Output Loop)
properRouting = channel (set [Left Input, Right Loop]) (set [Left Output, Right Loop]) [[0, 1], [1 % 2, 1 % 2]]

closedRouting :: StochasticMatrix NonNegativeRational (Either Input Loop) (Either Output Loop)
closedRouting = channel (set [Left Input, Right Loop]) (set [Left Output, Right Loop]) [[1, 0], [0, 1]]

assert :: String -> Bool -> IO ()
assert message condition = unless condition (fail message)

right :: String -> Either error value -> value
right label result = case result of
    Left _ -> error (label ++ " failed")
    Right value -> value

rightIO :: (Show error) => String -> Either error value -> IO value
rightIO label result = case result of
    Left failure -> fail (label ++ ": " ++ show failure)
    Right value -> pure value

showResult :: Either (ExactFeedbackError Loop) (CheckedFeedback String Input Loop Output) -> String
showResult (Left failure) = show failure
showResult (Right _) = "Right CheckedFeedback"

showTimed :: Either (Timed.TimedFeedbackError Loop Output) (Timed.CheckedTimedFeedback String Input Loop Output) -> String
showTimed (Left failure) = show failure
showTimed (Right _) = "Right CheckedTimedFeedback"

showDelayedTrace ::
    Either
        Delay.DelayedFeedbackError
        (StochasticMatrix NonNegativeRational () (Delay.DelayedFeedbackTrace Bool Bool), Delay.DelayedFeedbackReport String) ->
    String
showDelayedTrace (Left failure) = show failure
showDelayedTrace (Right _) = "Right delayed trace"

showDelayedFinal ::
    Either
        Delay.DelayedFeedbackError
        (StochasticMatrix NonNegativeRational () ((), Bool), Delay.DelayedFeedbackReport String) ->
    String
showDelayedFinal (Left failure) = show failure
showDelayedFinal (Right _) = "Right delayed final"
