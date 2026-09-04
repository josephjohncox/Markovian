module FeedbackValueExact (runFeedbackValueExactTests) where

import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator, (%))
import Markovian.Algebra.NonNegativeRational
import Markovian.Category.Finite.Set
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Stochastic
import Markovian.Feedback.Channel.Exact (FeedbackAccounting (..), FeedbackLimitDimension (..), FeedbackLimitError (..), FeedbackLimits, LoopLayout, feedbackLimits, loopLayout)
import Markovian.Feedback.Timed.Exact qualified as Timed
import Markovian.Feedback.Value.Exact qualified as Value
import Markovian.Objective.Exact (ExactContractionDiscount, mkExactContractionDiscount, mkExactDiscount)
import Markovian.Reward.Exact (exactReward, exactRewardValue)
import Numeric.Natural (Natural)

runFeedbackValueExactTests :: (String -> IO () -> IO ()) -> IO ()
runFeedbackValueExactTests run = do
    run "affine feedback geometric hand coefficients" testGeometric
    run "affine feedback closed class and infinite exit time" testClosedClass
    run "affine feedback partial exit" testPartialExit
    run "affine feedback zero discount" testZeroDiscount
    run "affine feedback multi-state literal equations" testMultiStateEquations
    run "affine feedback explicit finite-unrolling oracle" testFiniteUnrollingOracle
    run "affine feedback adversarial graph-work bound" testGraphWorkBound
    run "affine feedback nilpotent timed differential" testNilpotentDifferential
    run "affine feedback malformed channels and layouts" testMalformed
    run "affine feedback empty-loop direct coefficients" testEmptyLoop
    run "affine feedback exact and one-below ledgers" testLimits
    run "affine feedback discarded elimination accounting" testEliminationAccounting
    run "affine feedback total failure precedence" testFailurePrecedence

data Input = Input deriving (Eq, Show)
data Loop = Loop | LoopA | LoopB | OutsideLoop deriving (Eq, Show)
data Output = Output | OtherOutput deriving (Eq, Show)

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

discount :: Rational -> ExactContractionDiscount
discount value = right "strict discount" (mkExactContractionDiscount value)

inputs :: FiniteSet Input
inputs = set [Input]

outputs :: FiniteSet Output
outputs = set [Output]

loopOwner :: LoopLayout String Loop
loopOwner = loopLayout "affine-loop" (set [Loop])

coefficientA :: (Eq source) => Value.AffineFeedbackCoefficients source output -> source -> Rational
coefficientA coefficients source =
    maybe (error "missing affine A coefficient") exactRewardValue (Value.affineConstantCoefficient coefficients source)

coefficientK :: (Eq source, Eq output) => Value.AffineFeedbackCoefficients source output -> source -> output -> Rational
coefficientK coefficients source output =
    fromMaybe (error "missing affine K coefficient") (Value.affineContinuationCoefficient coefficients source output)

testGeometric :: IO ()
testGeometric = do
    let events = set [Value.Exit (exactReward 1) Output, Value.Continue (exactReward 1) Loop]
        routing = channel (set [Left Input, Right Loop]) events [[1 % 2, 1 % 2], [1 % 2, 1 % 2]]
    checked <- rightIO "geometric affine feedback" (Value.closeAffineFeedback largeLimits (discount (1 % 2)) inputs loopOwner outputs routing)
    let external = Value.affineFeedbackExternalCoefficients checked
        internal = Value.affineFeedbackInternalCoefficients checked
    assert "geometric external A changed" (coefficientA external Input == 4 % 3)
    assert "geometric internal A changed" (coefficientA internal Loop == 4 % 3)
    assert "geometric external K changed" (coefficientK external Input Output == 1 % 3)
    assert "geometric internal K changed" (coefficientK internal Loop Output == 1 % 3)
    assert "literal equation count changed" (Value.affineFeedbackValidatedEquationCount (Value.affineFeedbackReport checked) == 4)
    repeated <- rightIO "repeated geometric affine feedback" (Value.closeAffineFeedback largeLimits (discount (1 % 2)) inputs loopOwner outputs routing)
    assert "affine report was nondeterministic" (Value.affineFeedbackReport checked == Value.affineFeedbackReport repeated)

testClosedClass :: IO ()
testClosedClass = do
    let events = set [Value.Continue (exactReward 1) Loop]
        routing = channel (set [Left Input, Right Loop]) events [[1], [1]]
    checked <- rightIO "closed affine feedback" (Value.closeAffineFeedback largeLimits (discount (1 % 2)) inputs loopOwner outputs routing)
    let external = Value.affineFeedbackExternalCoefficients checked
        internal = Value.affineFeedbackInternalCoefficients checked
    assert "closed external A changed" (coefficientA external Input == 2)
    assert "closed internal A changed" (coefficientA internal Loop == 2)
    assert "closed external K was nonzero" (coefficientK external Input Output == 0)
    assert "closed internal K was nonzero" (coefficientK internal Loop Output == 0)
    nearOne <- rightIO "near-one closed affine feedback" (Value.closeAffineFeedback largeLimits (discount (999 % 1000)) inputs loopOwner outputs routing)
    assert "near-one strict discount changed exact closed value" (coefficientA (Value.affineFeedbackExternalCoefficients nearOne) Input == 1000)

testPartialExit :: IO ()
testPartialExit = do
    let events = set [Value.Exit (exactReward 0) Output, Value.Continue (exactReward 0) Loop]
        routing = channel (set [Left Input, Right Loop]) events [[1 % 2, 1 % 2], [0, 1]]
    checked <- rightIO "partial-exit affine feedback" (Value.closeAffineFeedback largeLimits (discount (1 % 2)) inputs loopOwner outputs routing)
    let external = Value.affineFeedbackExternalCoefficients checked
        internal = Value.affineFeedbackInternalCoefficients checked
    assert "partial exit A changed" (coefficientA external Input == 0 && coefficientA internal Loop == 0)
    assert "partial exit coefficient changed" (coefficientK external Input Output == 1 % 4)
    assert "infinite branch acquired an exit coefficient" (coefficientK internal Loop Output == 0)

testZeroDiscount :: IO ()
testZeroDiscount = do
    let events = set [Value.Exit (exactReward 10) Output, Value.Continue (exactReward 2) Loop]
        routing = channel (set [Left Input, Right Loop]) events [[1 % 2, 1 % 2], [0, 1]]
    checked <- rightIO "zero-discount affine feedback" (Value.closeAffineFeedback largeLimits (discount 0) inputs loopOwner outputs routing)
    let external = Value.affineFeedbackExternalCoefficients checked
        internal = Value.affineFeedbackInternalCoefficients checked
    assert "zero discount did not retain immediate correlated reward" (coefficientA external Input == 6)
    assert "zero discount internal immediate reward changed" (coefficientA internal Loop == 2)
    assert "zero discount retained an exit continuation" (coefficientK external Input Output == 0 && coefficientK internal Loop Output == 0)

testMultiStateEquations :: IO ()
testMultiStateEquations = do
    let loops = set [LoopA, LoopB]
        owner = loopLayout "cycle" loops
        events = set [Value.Continue (exactReward 1) LoopA, Value.Continue (exactReward 2) LoopB, Value.Exit (exactReward 3) Output]
        routing = channel (set [Left Input, Right LoopA, Right LoopB]) events [[0, 1, 0], [0, 1 % 2, 1 % 2], [1 % 2, 1 % 2, 0]]
        gamma = 1 % 2
    checked <- rightIO "multi-state affine feedback" (Value.closeAffineFeedback largeLimits (discount gamma) inputs owner outputs routing)
    let external = Value.affineFeedbackExternalCoefficients checked
        internal = Value.affineFeedbackInternalCoefficients checked
        aX = coefficientA external Input
        aA = coefficientA internal LoopA
        aB = coefficientA internal LoopB
        kX = coefficientK external Input Output
        kA = coefficientK internal LoopA Output
        kB = coefficientK internal LoopB Output
        rA = 5 % 2
        rB = 3 % 2
    assert "external A literal equation failed" (aX == 2 + gamma * aB)
    assert "internal A row A literal equation failed" (aA == rA + gamma * ((1 % 2) * aB))
    assert "internal A row B literal equation failed" (aB == rB + gamma * ((1 % 2) * aA + (1 % 2) * aB))
    assert "external K literal equation failed" (kX == gamma * kB)
    assert "internal K row A literal equation failed" (kA == gamma * (1 % 2) + gamma * (1 % 2) * kB)
    assert "internal K row B literal equation failed" (kB == gamma * ((1 % 2) * kA + (1 % 2) * kB))

    let reversedLoops = set [LoopB, LoopA]
        reversedOwner = loopLayout "cycle" reversedLoops
        reversedEvents = set [Value.Exit (exactReward 3) Output, Value.Continue (exactReward 2) LoopB, Value.Continue (exactReward 1) LoopA]
        reversedRouting = channel (set [Left Input, Right LoopB, Right LoopA]) reversedEvents [[0, 1, 0], [0, 1 % 2, 1 % 2], [1 % 2, 1 % 2, 0]]
    permuted <- rightIO "permuted multi-state affine feedback" (Value.closeAffineFeedback largeLimits (discount gamma) inputs reversedOwner outputs reversedRouting)
    let permutedExternal = Value.affineFeedbackExternalCoefficients permuted
        permutedInternal = Value.affineFeedbackInternalCoefficients permuted
    assert "layout permutation changed external coefficients" (coefficientA permutedExternal Input == aX && coefficientK permutedExternal Input Output == kX)
    assert "layout permutation changed internal A coefficients" (coefficientA permutedInternal LoopA == aA && coefficientA permutedInternal LoopB == aB)
    assert "layout permutation changed internal K coefficients" (coefficientK permutedInternal LoopA Output == kA && coefficientK permutedInternal LoopB Output == kB)

data OracleLimits = OracleLimits
    { maximumOracleHorizon :: !Natural
    , maximumOracleWork :: !Natural
    , maximumOracleRationalBits :: !Natural
    }

data OracleError
    = OracleHorizonExceeded !Natural !Natural
    | OracleWorkExceeded !Natural !Natural
    | OracleRationalBitsExceeded !Natural !Natural
    | OracleWorkPlanMismatch !Natural !Natural
    deriving (Eq, Show)

data OracleReport = OracleReport
    { oracleHorizon :: !Natural
    , oracleWorkPerformed :: !Natural
    , oracleMaximumRationalBits :: !Natural
    }
    deriving (Eq, Show)

data OracleResult = OracleResult
    { oracleExternalA :: ![Rational]
    , oracleInternalA :: ![Rational]
    , oracleExternalK :: ![[Rational]]
    , oracleInternalK :: ![[Rational]]
    , oracleReport :: !OracleReport
    }

data OracleLedger = OracleLedger
    { oracleLedgerWork :: !Natural
    , oracleLedgerMaximumBits :: !Natural
    }

newtype OracleMeter value = OracleMeter
    { runOracleMeter :: Natural -> OracleLedger -> Either OracleError (value, OracleLedger)
    }

instance Functor OracleMeter where
    fmap function computation = OracleMeter $ \bitLimit ledger -> do
        (value, next) <- runOracleMeter computation bitLimit ledger
        pure (function value, next)

instance Applicative OracleMeter where
    pure value = OracleMeter $ \_ ledger -> Right (value, ledger)
    function <*> argument = OracleMeter $ \bitLimit ledger -> do
        (apply, afterFunction) <- runOracleMeter function bitLimit ledger
        (value, afterArgument) <- runOracleMeter argument bitLimit afterFunction
        pure (apply value, afterArgument)

instance Monad OracleMeter where
    computation >>= continue = OracleMeter $ \bitLimit ledger -> do
        (value, next) <- runOracleMeter computation bitLimit ledger
        runOracleMeter (continue value) bitLimit next

oracleOperation :: Rational -> OracleMeter Rational
oracleOperation value = OracleMeter $ \bitLimit ledger ->
    let bits = rationalBitCount value
        work = oracleLedgerWork ledger + 1
        next = OracleLedger work (max bits (oracleLedgerMaximumBits ledger))
     in if bits <= bitLimit
            then Right (value, next)
            else Left (OracleRationalBitsExceeded bitLimit bits)

oracleAdd :: Rational -> Rational -> OracleMeter Rational
oracleAdd left rightValue = oracleOperation (left + rightValue)

oracleMultiply :: Rational -> Rational -> OracleMeter Rational
oracleMultiply left rightValue = oracleOperation (left * rightValue)

oracleDot :: [Rational] -> [Rational] -> OracleMeter Rational
oracleDot left rightValues = oracleFold addTerm 0 (zip left rightValues)
  where
    addTerm total (x, y) = do
        productValue <- oracleMultiply x y
        oracleAdd total productValue

oracleStep ::
    Rational ->
    [Rational] ->
    [[Rational]] ->
    [[Rational]] ->
    [Rational] ->
    [[Rational]] ->
    OracleMeter ([Rational], [[Rational]])
oracleStep gamma rewards continuation exits constants coefficients = do
    nextConstants <- traverse constantRow (zip rewards continuation)
    nextCoefficients <- traverse coefficientRow (zip exits continuation)
    pure (nextConstants, nextCoefficients)
  where
    columns = oracleColumns 2 coefficients
    constantRow (reward, row) = do
        continuationValue <- oracleDot row constants
        discounted <- oracleMultiply gamma continuationValue
        oracleAdd reward discounted
    coefficientRow (exitRow, row) = traverse (coefficientEntry row) (zip exitRow columns)
    coefficientEntry row (exitMass, column) = do
        direct <- oracleMultiply gamma exitMass
        continuationValue <- oracleDot row column
        discounted <- oracleMultiply gamma continuationValue
        oracleAdd direct discounted

iterateOracle ::
    Natural ->
    Rational ->
    [Rational] ->
    [[Rational]] ->
    [[Rational]] ->
    ([Rational], [[Rational]]) ->
    OracleMeter ([Rational], [[Rational]])
iterateOracle 0 _ _ _ _ values = pure values
iterateOracle remaining gamma rewards continuation exits (constants, coefficients) = do
    next <- oracleStep gamma rewards continuation exits constants coefficients
    iterateOracle (remaining - 1) gamma rewards continuation exits next

-- Independent finite unrolling for the fixed two-loop, two-output fixture.
-- Its operation plan is N*(2*(2*2+2)+2*2*(2*2+3)) plus the external row
-- work: N*40+20. It meters each operation but does not call feedback code.
runFiniteOracle :: OracleLimits -> Natural -> Either OracleError OracleResult
runFiniteOracle limits horizon
    | horizon > maximumOracleHorizon limits = Left (OracleHorizonExceeded (maximumOracleHorizon limits) horizon)
    | requiredWork > maximumOracleWork limits = Left (OracleWorkExceeded (maximumOracleWork limits) requiredWork)
    | otherwise = do
        ((internalA, internalK, externalA, externalK), ledger) <-
            runOracleMeter computation (maximumOracleRationalBits limits) (OracleLedger 0 0)
        if oracleLedgerWork ledger == requiredWork
            then
                Right
                    OracleResult
                        { oracleExternalA = externalA
                        , oracleInternalA = internalA
                        , oracleExternalK = externalK
                        , oracleInternalK = internalK
                        , oracleReport = OracleReport horizon requiredWork (oracleLedgerMaximumBits ledger)
                        }
            else Left (OracleWorkPlanMismatch requiredWork (oracleLedgerWork ledger))
  where
    requiredWork = horizon * 40 + 20
    gamma = 1 % 2
    internalRewards = [9 % 8, 2]
    internalContinuation = [[1 % 2, 1 % 4], [1 % 4, 1 % 4]]
    internalExits = [[1 % 8, 1 % 8], [1 % 4, 1 % 4]]
    computation = do
        (internalA, internalK) <- iterateOracle horizon gamma internalRewards internalContinuation internalExits ([0, 0], replicate 2 [0, 0])
        (externalA, externalK) <- oracleStep gamma [13 % 8] [[3 % 8, 3 % 8]] [[1 % 8, 1 % 8]] internalA internalK
        pure (internalA, internalK, externalA, externalK)

testFiniteUnrollingOracle :: IO ()
testFiniteUnrollingOracle = do
    let gamma = 1 % 2
        horizon = 4
        exactOracleLimits = OracleLimits 4 180 18
        events =
            set
                [ Value.Continue (exactReward 0) LoopA
                , Value.Continue (exactReward 1) LoopA
                , Value.Continue (exactReward 2) LoopA
                , Value.Continue (exactReward 0) LoopB
                , Value.Continue (exactReward 1) LoopB
                , Value.Continue (exactReward 2) LoopB
                , Value.Exit (exactReward 3) Output
                , Value.Exit (exactReward 4) OtherOutput
                ]
        rows =
            [ replicate 8 (1 % 8)
            , [1 % 2, 0, 0, 0, 1 % 4, 0, 1 % 8, 1 % 8]
            , [0, 1 % 4, 0, 1 % 4, 0, 0, 1 % 4, 1 % 4]
            ]
        loops = set [LoopA, LoopB]
        outputSet = set [Output, OtherOutput]
        routing = channel (set [Left Input, Right LoopA, Right LoopB]) events rows
    oracle <- rightIO "bounded finite-unrolling oracle" (runFiniteOracle exactOracleLimits horizon)
    assert "finite oracle report changed" (oracleReport oracle == OracleReport 4 180 18)
    case runFiniteOracle (OracleLimits 3 180 18) horizon of
        Left (OracleHorizonExceeded 3 4) -> pure ()
        other -> fail ("finite oracle one-below horizon was accepted: " ++ showOracle other)
    case runFiniteOracle (OracleLimits 4 179 18) horizon of
        Left (OracleWorkExceeded 179 180) -> pure ()
        other -> fail ("finite oracle one-below work was accepted: " ++ showOracle other)
    case runFiniteOracle (OracleLimits 4 180 17) horizon of
        Left (OracleRationalBitsExceeded 17 18) -> pure ()
        other -> fail ("finite oracle one-below rational limit was accepted: " ++ showOracle other)
    checked <- rightIO "multi-state finite-unrolling affine feedback" (Value.closeAffineFeedback largeLimits (discount gamma) inputs (loopLayout "oracle" loops) outputSet routing)
    let external = Value.affineFeedbackExternalCoefficients checked
        internal = Value.affineFeedbackInternalCoefficients checked
        actualAU = map (coefficientA internal) [LoopA, LoopB]
        actualKU = [[coefficientK internal loopValue output | output <- [Output, OtherOutput]] | loopValue <- [LoopA, LoopB]]
        actualAX = [coefficientA external Input]
        actualKX = [[coefficientK external Input output | output <- [Output, OtherOutput]]]
        rewardNorm = 2
        exitNorm = 1 % 2
        externalContinuationNorm = 3 % 4
    assert "finite oracle internal A bound failed" (vectorMaximumNorm (subtractVector actualAU (oracleInternalA oracle)) <= gamma ^ horizon * rewardNorm / (1 - gamma))
    assert "finite oracle internal K bound failed" (matrixMaximumRowNorm (subtractMatrix actualKU (oracleInternalK oracle)) <= gamma ^ (horizon + 1) * exitNorm / (1 - gamma))
    assert "finite oracle external A bound failed" (vectorMaximumNorm (subtractVector actualAX (oracleExternalA oracle)) <= externalContinuationNorm * gamma ^ (horizon + 1) * rewardNorm / (1 - gamma))
    assert "finite oracle external K bound failed" (matrixMaximumRowNorm (subtractMatrix actualKX (oracleExternalK oracle)) <= externalContinuationNorm * gamma ^ (horizon + 2) * exitNorm / (1 - gamma))

testGraphWorkBound :: IO ()
testGraphWorkBound = do
    let loops = set [LoopA, LoopB]
        outputSet = set [Output, OtherOutput]
        events =
            set
                [ Value.Continue (exactReward 0) LoopA
                , Value.Continue (exactReward 1) LoopA
                , Value.Continue (exactReward 2) LoopA
                , Value.Continue (exactReward 0) LoopB
                , Value.Continue (exactReward 1) LoopB
                , Value.Continue (exactReward 2) LoopB
                , Value.Exit (exactReward 3) Output
                , Value.Exit (exactReward 4) OtherOutput
                ]
        routing = channel (set [Left Input, Right LoopA, Right LoopB]) events (replicate 3 (replicate 8 (1 % 8)))
        -- Graph bound: E*T 32 + S 3 + E 8 + 4*S*E 96 + S*E*T 96.
        -- Arithmetic count: aggregation 72, solve 42, external
        -- derivation/checks 40, and internal checks 40.
        exactLimits = feedbackLimits 1 2 2 0 24 235 194 8 4096
    checked <- rightIO "exact adversarial graph budget" (Value.closeAffineFeedback exactLimits (discount (1 % 2)) inputs (loopLayout "graph" loops) outputSet routing)
    let report = Value.affineFeedbackReport checked
    assert "adversarial graph visit bound changed" (Value.affineFeedbackGraphWorkCharged report == 235)
    assert "independent adversarial arithmetic count changed" (Value.affineFeedbackArithmeticWorkCharged report == 194)
    case Value.closeAffineFeedback (feedbackLimits 1 2 2 0 24 234 194 8 4096) (discount (1 % 2)) inputs (loopLayout "graph" loops) outputSet routing of
        Left (Value.AffineFeedbackLimitError (FeedbackLimitExceeded FeedbackGraphWork 234 235)) -> pure ()
        other -> fail ("adversarial one-below graph budget was accepted: " ++ showAffineTwoOutput other)

testNilpotentDifferential :: IO ()
testNilpotentDifferential = do
    let gamma = 1 % 2
        events = set [Value.Continue (exactReward 2) Loop, Value.Exit (exactReward 3) Output]
        routing = channel (set [Left Input, Right Loop]) events [[1, 0], [0, 1]]
    affine <- rightIO "nilpotent affine feedback" (Value.closeAffineFeedback largeLimits (discount gamma) inputs loopOwner outputs routing)
    timed <- rightIO "nilpotent timed feedback" (Timed.closeNilpotentTimedFeedback largeLimits (right "finite discount" (mkExactDiscount gamma)) inputs loopOwner outputs routing)
    let timedChannel = Timed.timedFeedbackChannel timed
        outcomes = finiteSetValues (stochasticTarget timedChannel)
        expectedReward = sum [entry timedChannel Input outcome * exactRewardValue (Timed.timedFeedbackReward outcome) | outcome <- outcomes]
        expectedContinuation = sum [entry timedChannel Input outcome * gamma ^ Timed.timedFeedbackDuration outcome | outcome <- outcomes]
        external = Value.affineFeedbackExternalCoefficients affine
    assert "nilpotent timed reward differential failed" (coefficientA external Input == expectedReward && expectedReward == 7 % 2)
    assert "nilpotent timed continuation differential failed" (coefficientK external Input Output == expectedContinuation && expectedContinuation == 1 % 4)

testMalformed :: IO ()
testMalformed = do
    let source = set [Left Input, Right Loop]
        badContinueEvents = set [Value.Continue (exactReward 0) OutsideLoop]
        badContinue = channel source badContinueEvents [[1], [1]]
    case Value.closeAffineFeedback largeLimits (discount (1 % 2)) inputs loopOwner outputs badContinue of
        Left (Value.AffineFeedbackContinueOutsideLoop OutsideLoop) -> pure ()
        other -> fail ("outside continuation target was accepted: " ++ showAffine other)
    let badExitEvents = set [Value.Exit (exactReward 0) OtherOutput]
        badExit = channel source badExitEvents [[1], [1]]
    case Value.closeAffineFeedback largeLimits (discount (1 % 2)) inputs loopOwner outputs badExit of
        Left (Value.AffineFeedbackExitOutsideOutput OtherOutput) -> pure ()
        other -> fail ("outside exit target was accepted: " ++ showAffine other)
    let reversed = channel (set [Right Loop, Left Input]) (set [Value.Exit (exactReward 0) Output]) [[1], [1]]
    case Value.closeAffineFeedback largeLimits (discount (1 % 2)) inputs loopOwner outputs reversed of
        Left Value.AffineFeedbackSourceLayoutMismatch -> pure ()
        other -> fail ("source layout mismatch was accepted: " ++ showAffine other)
    let rawTarget :: FiniteSet (Value.FeedbackEvent Loop Output)
        rawTarget = set [Value.Exit (exactReward 0) Output]
        raw = right "raw malformed event matrix" (matrixFromRows inputs rawTarget [[scalar (1 % 2)]])
    case stochasticMatrix raw of
        Left (StochasticRowNotNormalized 0 mass) -> assert "wrong malformed normalization mass" (mass == scalar (1 % 2))
        Left failure -> fail ("wrong malformed normalization failure: " ++ show failure)
        Right _ -> fail "unnormalized event channel was accepted"

testEmptyLoop :: IO ()
testEmptyLoop = do
    let noLoops = set ([] :: [Loop])
        events = set [Value.Exit (exactReward 3) Output]
        routing = channel (set [Left Input]) events [[1]]
    checked <- rightIO "empty-loop affine feedback" (Value.closeAffineFeedback largeLimits (discount (1 % 2)) inputs (loopLayout "empty-affine" noLoops) outputs routing)
    let external = Value.affineFeedbackExternalCoefficients checked
    assert "empty-loop direct A changed" (coefficientA external Input == 3)
    assert "empty-loop direct K changed" (coefficientK external Input Output == 1 % 2)

testLimits :: IO ()
testLimits = do
    let events = set [Value.Exit (exactReward 1) Output, Value.Continue (exactReward 1) Loop]
        routing = channel (set [Left Input, Right Loop]) events [[1 % 2, 1 % 2], [1 % 2, 1 % 2]]
        gamma = discount (1 % 2)
    -- Independent count: aggregation 12, multi-RHS solve 6, external
    -- derivation and check 18, and internal equation checks 9.
    let expectedWork = 45
        expectedBits = 4
        exactLimits = feedbackLimits 1 1 1 0 4 32 expectedWork 2 expectedBits
    exact <- rightIO "exact affine ledger" (Value.closeAffineFeedback exactLimits gamma inputs loopOwner outputs routing)
    let report = Value.affineFeedbackReport exact
        accounting = Value.affineFeedbackAccounting report
    assert "independent geometric arithmetic count changed" (Value.affineFeedbackArithmeticWorkCharged report == expectedWork)
    assert "geometric rational maximum changed" (feedbackMaximumObservedBits accounting == expectedBits)
    assert "geometric graph visit bound changed" (Value.affineFeedbackGraphWorkCharged report == 32)
    case Value.closeAffineFeedback (feedbackLimits 1 1 1 0 4 32 44 2 expectedBits) gamma inputs loopOwner outputs routing of
        Left (Value.AffineFeedbackLimitError (FeedbackLimitExceeded FeedbackArithmeticWork 44 45)) -> pure ()
        other -> fail ("affine one-below work limit was not atomic: " ++ showAffine other)
    case Value.closeAffineFeedback (feedbackLimits 1 1 1 0 4 32 expectedWork 2 3) gamma inputs loopOwner outputs routing of
        Left (Value.AffineFeedbackLimitError (FeedbackRationalBitsExceeded _ 3 4)) -> pure ()
        other -> fail ("affine one-below rational limit was not atomic: " ++ showAffine other)
    expectPreflight FeedbackSourceCount 0 1 (feedbackLimits 0 1 1 0 4 32 expectedWork 2 expectedBits) gamma routing
    expectPreflight FeedbackLoopCount 0 1 (feedbackLimits 1 0 1 0 4 32 expectedWork 2 expectedBits) gamma routing
    expectPreflight FeedbackOutputCount 0 1 (feedbackLimits 1 1 0 0 4 32 expectedWork 2 expectedBits) gamma routing
    expectPreflight FeedbackTraceOutcomeCount 1 2 (feedbackLimits 1 1 1 0 4 32 expectedWork 1 expectedBits) gamma routing
    expectPreflight FeedbackMatrixCellCount 3 4 (feedbackLimits 1 1 1 0 3 32 expectedWork 2 expectedBits) gamma routing
    expectPreflight FeedbackGraphWork 31 32 (feedbackLimits 1 1 1 0 4 31 expectedWork 2 expectedBits) gamma routing
  where
    expectPreflight dimension limit required limits gamma routing =
        case Value.closeAffineFeedback limits gamma inputs loopOwner outputs routing of
            Left (Value.AffineFeedbackLimitError (FeedbackLimitExceeded actualDimension actualLimit actualRequired)) ->
                assert "wrong affine preflight failure" (actualDimension == dimension && actualLimit == limit && actualRequired == required)
            other -> fail ("affine preflight one-below limit was accepted: " ++ showAffine other)

testEliminationAccounting :: IO ()
testEliminationAccounting = do
    let loops = set [LoopA, LoopB]
        events = set [Value.Continue (exactReward 0) LoopA, Value.Continue (exactReward 0) LoopB, Value.Exit (exactReward 0) Output]
        routing = channel (set [Left Input, Right LoopA, Right LoopB]) events [[0, 0, 1], [1 % 2, 1 % 3, 1 % 6], [1 % 3, 1 % 2, 1 % 6]]
    -- Independent count: aggregation 27, solve 34, external
    -- derivation/checks 26, and internal equation checks 26.
    let exactLimits = feedbackLimits 1 2 1 0 9 78 113 3 7
        owner = loopLayout "elimination" loops
        gamma = discount (1 % 2)
    checked <- rightIO "exact elimination affine ledger" (Value.closeAffineFeedback exactLimits gamma inputs owner outputs routing)
    let report = Value.affineFeedbackReport checked
        accounting = Value.affineFeedbackAccounting report
    assert "independent elimination arithmetic count changed" (Value.affineFeedbackArithmeticWorkCharged report == 113)
    assert "elimination graph visit bound changed" (Value.affineFeedbackGraphWorkCharged report == 78)
    assert "discarded affine Gaussian intermediates were omitted" (feedbackMaximumGaussianIntermediateBits accounting == 7 && feedbackMaximumRetainedResultBits accounting == 3)
    golden <- readFile "test/golden/affine-feedback-accounting.txt"
    assert "affine accounting golden changed" (show report ++ "\n" == golden)
    case Value.closeAffineFeedback (feedbackLimits 1 2 1 0 9 78 112 3 7) gamma inputs owner outputs routing of
        Left (Value.AffineFeedbackLimitError (FeedbackLimitExceeded FeedbackArithmeticWork 112 113)) -> pure ()
        other -> fail ("elimination one-below work was accepted: " ++ showAffine other)
    case Value.closeAffineFeedback (feedbackLimits 1 2 1 0 9 78 113 3 6) gamma inputs owner outputs routing of
        Left (Value.AffineFeedbackLimitError (FeedbackRationalBitsExceeded "affine Gaussian elimination difference" 6 7)) -> pure ()
        other -> fail ("discarded Gaussian one-below rational limit was accepted: " ++ showAffine other)

testFailurePrecedence :: IO ()
testFailurePrecedence = do
    let malformedEvents = set [Value.Continue (exactReward (1 % 2048)) OutsideLoop, Value.Exit (exactReward 0) OtherOutput]
        malformedRouting = channel (set [Right Loop, Left Input]) malformedEvents [[1, 0], [1, 0]]
        gamma = discount (1 % 1024)
        expectLimit dimension limit required limits =
            case Value.closeAffineFeedback limits gamma inputs loopOwner outputs malformedRouting of
                Left (Value.AffineFeedbackLimitError (FeedbackLimitExceeded actualDimension actualLimit actualRequired)) ->
                    assert "combined-invalid preflight precedence changed" (actualDimension == dimension && actualLimit == limit && actualRequired == required)
                other -> fail ("combined-invalid preflight did not fail first: " ++ showAffine other)
    expectLimit FeedbackSourceCount 0 1 (feedbackLimits 0 0 0 0 0 0 0 0 0)
    expectLimit FeedbackLoopCount 0 1 (feedbackLimits 1 0 0 0 0 0 0 0 0)
    expectLimit FeedbackOutputCount 0 1 (feedbackLimits 1 1 0 0 0 0 0 0 0)
    expectLimit FeedbackTraceOutcomeCount 1 2 (feedbackLimits 1 1 1 0 0 0 0 1 0)
    expectLimit FeedbackMatrixCellCount 3 4 (feedbackLimits 1 1 1 0 3 0 0 2 0)
    expectLimit FeedbackGraphWork 31 32 (feedbackLimits 1 1 1 0 4 31 0 2 0)
    case Value.closeAffineFeedback (feedbackLimits 1 1 1 0 4 32 0 2 0) gamma inputs loopOwner outputs malformedRouting of
        Left (Value.AffineFeedbackContinueOutsideLoop OutsideLoop) -> pure ()
        other -> fail ("malformed event did not precede layout and rational input: " ++ showAffine other)

    let validEvents = set [Value.Exit (exactReward (1 % 2048)) Output]
        reversedRouting = channel (set [Right Loop, Left Input]) validEvents [[1], [1]]
    case Value.closeAffineFeedback largeLimits gamma inputs loopOwner outputs reversedRouting of
        Left Value.AffineFeedbackSourceLayoutMismatch -> pure ()
        other -> fail ("layout did not precede rational input: " ++ showAffine other)
    let validRouting = channel (set [Left Input, Right Loop]) validEvents [[1], [1]]
    case Value.closeAffineFeedback (feedbackLimits 1 1 1 0 2 17 100 1 1) gamma inputs loopOwner outputs validRouting of
        Left (Value.AffineFeedbackLimitError (FeedbackRationalBitsExceeded "affine feedback discount input" 1 11)) -> pure ()
        other -> fail ("discount input did not begin rational-ledger precedence: " ++ showAffine other)

oracleColumns :: Int -> [[Rational]] -> [[Rational]]
oracleColumns count rows = [[row !! column | row <- rows] | column <- [0 .. count - 1]]

oracleFold :: (accumulator -> value -> OracleMeter accumulator) -> accumulator -> [value] -> OracleMeter accumulator
oracleFold _ initial [] = pure initial
oracleFold step initial (value : remaining) = do
    next <- step initial value
    oracleFold step next remaining

absolute :: Rational -> Rational
absolute value = if value < 0 then negate value else value

vectorMaximumNorm :: [Rational] -> Rational
vectorMaximumNorm [] = 0
vectorMaximumNorm values = maximum (map absolute values)

matrixMaximumRowNorm :: [[Rational]] -> Rational
matrixMaximumRowNorm [] = 0
matrixMaximumRowNorm rows = maximum [sum (map absolute row) | row <- rows]

subtractVector :: [Rational] -> [Rational] -> [Rational]
subtractVector = zipWith (-)

subtractMatrix :: [[Rational]] -> [[Rational]] -> [[Rational]]
subtractMatrix = zipWith subtractVector

rationalBitCount :: Rational -> Natural
rationalBitCount value = fromIntegral (max (integerBits (abs (numerator value))) (integerBits (denominator value)))
  where
    integerBits 0 = 1
    integerBits integer = length (takeWhile (> 0) (iterate (`div` 2) integer))

entry :: StochasticMatrix NonNegativeRational source target -> source -> target -> Rational
entry matrix source target = maybe 0 getNonNegativeRational (matrixEntry (forgetStochastic matrix) source target)

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

showAffine :: Either (Value.AffineFeedbackError Loop Output) (Value.CheckedAffineFeedback String Input Loop Output) -> String
showAffine (Left failure) = show failure
showAffine (Right _) = "Right CheckedAffineFeedback"

showAffineTwoOutput :: Either (Value.AffineFeedbackError Loop Output) (Value.CheckedAffineFeedback String Input Loop Output) -> String
showAffineTwoOutput = showAffine

showOracle :: Either OracleError OracleResult -> String
showOracle (Left failure) = show failure
showOracle (Right _) = "Right OracleResult"
