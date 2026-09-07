module FeedbackRewardJVP (runFeedbackRewardJVPTests, runRewardJVPLesson) where

import Control.Monad (forM_, unless)
import Data.Either (fromRight)
import Data.Maybe (fromJust, isNothing)
import Data.Ratio ((%))
import Markovian.Algebra.NonNegativeRational
import Markovian.Category.Finite.Set
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Stochastic
import Markovian.Feedback.Value.Exact
import Markovian.Objective.Exact (mkExactContractionDiscount)
import Markovian.Reward.Exact (exactReward, exactRewardValue)
import Numeric.Natural (Natural)

-- Independent fixtures use at most two loops, so a literal determinant oracle
-- can replace production Gaussian elimination. Events retain reward/route pairs.
data Fixture = Fixture [Int] [Int] [Int] [FeedbackEvent Int Int] [[Rational]] [[Rational]] Rational

large :: FeedbackLimits
large = feedbackLimits 100 100 100 0 1000000 1000000 1000000 1000 4096

set :: (Eq a) => [a] -> FiniteSet a
set = right . finiteSet

right :: Either error a -> a
right = fromRight (error "fixture construction failed")

retry :: Fixture
retry = Fixture [0] [0] [0] [Exit (exactReward 1) 0, Continue (exactReward 1) 0] (replicate 2 [1 % 2, 1 % 2]) (replicate 2 [1, 1]) (1 % 2)

solve :: FeedbackLimits -> Fixture -> Either (AffineRewardJVPError Int Int) (CheckedAffineRewardJVP String Int Int Int)
solve limits (Fixture xs us ys es ps hs gamma) =
    closeAffineFeedbackRewardJVP limits (right (mkExactContractionDiscount gamma)) (set xs) (loopLayout "reward-jvp" (set us)) (set ys) channel direction
  where
    sources = set (map Left xs ++ map Right us)
    events = set es
    channel = right (stochasticMatrix (right (matrixFromRows sources events (map (map (right . nonNegativeRational)) ps))))
    direction = right (matrixFromRows sources events hs)

constant :: AffineFeedbackCoefficients Int Int -> Int -> Rational
constant coefficients = exactRewardValue . fromJust . affineConstantCoefficient coefficients

continuation :: AffineFeedbackCoefficients Int Int -> Int -> Int -> Rational
continuation coefficients source = fromJust . affineContinuationCoefficient coefficients source

assert :: String -> Bool -> IO ()
assert label condition = unless condition (fail label)

checked :: String -> Either (AffineRewardJVPError Int Int) a -> IO a
checked label = either (fail . ((label ++ ": ") ++) . show) pure

runFeedbackRewardJVPTests :: (String -> IO () -> IO ()) -> IO ()
runFeedbackRewardJVPTests run = do
    run "reward JVP independent determinant, equations and dual unrolling" testOracles
    run "reward JVP exact and every one-below work prefix" testLimits
    run "reward JVP canonical layouts and failure precedence" testPrecedence
    run "reward JVP discarded rational growth" testGrowth
    run "reward JVP executable retry lesson" runRewardJVPLesson

fixtures :: [Fixture]
fixtures =
    [ retry
    , Fixture
        [0, 1]
        [0, 1]
        [0, 1]
        [Continue (exactReward (-2)) 0, Continue (exactReward 3) 0, Continue (exactReward 0) 1, Exit (exactReward 4) 0, Exit (exactReward (-1)) 1]
        [[1 % 4, 0, 1 % 4, 1 % 4, 1 % 4], [0, 1, 0, 0, 0], [1 % 4, 1 % 4, 1 % 4, 0, 1 % 4], [0, 0, 1 % 2, 1 % 2, 0]]
        [[-1, 99, 2, -3, 4], [8, -2, 3, 4, 5], [3, -4, 5, 6, -7], [-9, 8, -1, 2, 3]]
        (1 % 2)
    , Fixture [0] [0] [] [Continue (exactReward (-2)) 0] [[1], [1]] [[-3], [2]] (999 % 1000)
    , Fixture [0] [] [0] [Exit (exactReward (-3)) 0] [[1]] [[-5]] (1 % 2)
    , Fixture [] [0] [0] [Continue (exactReward 2) 0] [[1]] [[-1]] (1 % 2)
    , Fixture [] [] [] [] [] [] 0
    , changeDirection (replicate 2 [0, 0]) retry
    , changeGamma 0 retry
    ]

changeDirection :: [[Rational]] -> Fixture -> Fixture
changeDirection hs (Fixture xs us ys es ps _ gamma) = Fixture xs us ys es ps hs gamma

changeGamma :: Rational -> Fixture -> Fixture
changeGamma gamma (Fixture xs us ys es ps hs _) = Fixture xs us ys es ps hs gamma

-- Closed-form 0-, 1-, or 2-dimensional linear algebra, not the library solver.
determinantSolve :: Rational -> [[Rational]] -> [Rational] -> [Rational]
determinantSolve _ [] [] = []
determinantSolve gamma [[d]] [m] = [m / (1 - gamma * d)]
determinantSolve gamma [[aa, ab], [ba, bb]] [m, n] =
    let a = 1 - gamma * aa
        b = negate gamma * ab
        c = negate gamma * ba
        d = 1 - gamma * bb
        det = a * d - b * c
     in [(d * m - b * n) / det, (a * n - c * m) / det]
determinantSolve _ _ _ = error "oracle fixture exceeds two loops"

reward :: FeedbackEvent Int Int -> Rational
reward (Continue r _) = exactRewardValue r
reward (Exit r _) = exactRewardValue r

loopMass :: Int -> FeedbackEvent Int Int -> Rational
loopMass target (Continue _ candidate) = if target == candidate then 1 else 0
loopMass _ (Exit _ _) = 0

exitMass :: Int -> FeedbackEvent Int Int -> Rational
exitMass target (Exit _ candidate) = if target == candidate then 1 else 0
exitMass _ (Continue _ _) = 0

testOracles :: IO ()
testOracles = forM_ (zip [0 :: Int ..] fixtures) $ \(index, fixture@(Fixture xs us ys es ps hs gamma)) -> do
    result <- checked ("fixture " ++ show index) (solve large fixture)
    let base = affineRewardJVPBase result
        bx = affineFeedbackExternalCoefficients base
        bu = affineFeedbackInternalCoefficients base
        dx = affineRewardJVPExternal result
        du = affineRewardJVPInternal result
        masses = [[sum [p * loopMass u e | (p, e) <- zip row es] | u <- us] | row <- ps]
        exits = [[sum [p * exitMass y e | (p, e) <- zip row es] | y <- ys] | row <- ps]
        ms = [sum [p * reward e | (p, e) <- zip row es] | row <- ps]
        hm = [sum (zipWith (*) row h) | (row, h) <- zip ps hs]
        (b, d) = splitAt (length xs) masses
        (mx, mu) = splitAt (length xs) ms
        (hx, hu) = splitAt (length xs) hm
        expectedAU = determinantSolve gamma d mu
        expectedDU = determinantSolve gamma d hu
        external m row v = m + gamma * sum (zipWith (*) row v)
        expectedAX = zipWith (\m row -> external m row expectedAU) mx b
        expectedDX = zipWith (\h row -> external h row expectedDU) hx b
        expectedKU = [determinantSolve gamma d [gamma * row !! column | row <- drop (length xs) exits] | column <- [0 .. length ys - 1]]
    assert "independent base determinant oracle" (map (constant bu) us == expectedAU && map (constant bx) xs == expectedAX)
    assert "independent derivative determinant oracle" (map (constant du) us == expectedDU && map (constant dx) xs == expectedDX)
    forM_ (zip3 ys [0 ..] expectedKU) $ \(y, column, values) -> do
        assert "independent base internal K" (map (\u -> continuation bu u y) us == values)
        assert "independent base external K" (map (\x -> continuation bx x y) xs == zipWith (\row exitRow -> gamma * (exitRow !! column + sum (zipWith (*) row values))) b (take (length xs) exits))
    assert "all derivative K are zero" (all (== 0) ([continuation dx x y | x <- xs, y <- ys] ++ [continuation du u y | u <- us, y <- ys]))
    assert "eight literal equation families" (affineRewardJVPValidatedEquations (affineRewardJVPReport result) == 8)
    assert "absent labels stay absent" (isNothing (affineConstantCoefficient dx 999) && isNothing (affineContinuationCoefficient du 999 999))
    -- Event-level dual unrolling starts with zero continuation after n events.
    -- This oracle never aggregates routes or solves a linear system.
    let n = 8
        pairs = unroll n fixture
        exactA = expectedAX ++ expectedAU
        exactD = expectedDX ++ expectedDU
        maxR = maximum (0 : map (abs . reward) es)
        maxH = maximum (0 : map abs (concat hs))
        bound magnitude = gamma ^ n * magnitude / (1 - gamma)
    assert "dual finite unrolling base error bound" (and (zipWith (\a (v, _) -> abs (a - v) <= bound maxR) exactA pairs))
    assert "dual finite unrolling derivative error bound" (and (zipWith (\a (_, v) -> abs (a - v) <= bound maxH) exactD pairs))
    -- Reversing all layouts and corresponding rows/columns is lawful, unlike
    -- reordering only the raw direction checked below.
    let permuted = Fixture (reverse xs) (reverse us) (reverse ys) (reverse es) (map reverse (reverse (take (length xs) ps) ++ reverse (drop (length xs) ps))) (map reverse (reverse (take (length xs) hs) ++ reverse (drop (length xs) hs))) gamma
    other <- checked "permuted fixture" (solve large permuted)
    assert "consistent layout permutation" (map (constant (affineRewardJVPExternal other)) xs == expectedDX && map (constant (affineRewardJVPInternal other)) us == expectedDU)

unroll :: Int -> Fixture -> [(Rational, Rational)]
unroll n (Fixture xs us _ es ps hs gamma) = iterate step (replicate (length xs + length us) (0, 0)) !! n
  where
    step values = [foldr add (0, 0) [scale p (add (reward e, h) (scale gamma (route e values))) | (p, e, h) <- zip3 row es directions] | (row, directions) <- zip ps hs]
    route (Exit _ _) _ = (0, 0)
    route (Continue _ u) values = fromJust (lookup u (zip us (drop (length xs) values)))
    add (a, da) (b, db) = (a + b, da + db)
    scale p (a, da) = (p * a, p * da)

-- Independent work count for retry: base 45; hm 8; one-RHS solve 4;
-- derivative external A 3; derivative equation families 4+4+5+5 = 18.
-- Reservations: 8+4+8+4=24 cells and 32+2+2+16=52 graph visits.
testLimits :: IO ()
testLimits = do
    let exact = feedbackLimits 1 1 1 0 24 52 78 2 4
    result <- checked "simultaneously exact limits" (solve exact retry)
    let report = affineRewardJVPReport result
        accounting = affineRewardJVPAccounting report
        prefix = affineFeedbackReport (affineRewardJVPBase result)
    assert "literal reservation and work golden" (report == AffineRewardJVPReport "reward-jvp" 24 52 8 (FeedbackAccounting 78 2 0 3 0 0 4 3 4))
    assert "base prefix retains old arithmetic" (affineFeedbackArithmeticWorkCharged prefix == 45 && affineFeedbackMatrixCellCount prefix == 4 && affineFeedbackGraphWorkCharged prefix == 32)
    assert "operation-wide, not independent ledgers" (feedbackArithmeticWork accounting > affineFeedbackArithmeticWorkCharged prefix)
    forM_ [0 .. 77] $ \work -> expectLimit FeedbackArithmeticWork work (work + 1) (feedbackLimits 1 1 1 0 24 52 work 2 4) retry
    expectLimit FeedbackSourceCount 0 1 (feedbackLimits 0 0 0 0 0 0 0 0 0) retry
    expectLimit FeedbackLoopCount 0 1 (feedbackLimits 1 0 0 0 0 0 0 0 0) retry
    expectLimit FeedbackOutputCount 0 1 (feedbackLimits 1 1 0 0 0 0 0 0 0) retry
    expectLimit FeedbackTraceOutcomeCount 1 2 (feedbackLimits 1 1 1 0 0 0 0 1 0) retry
    expectLimit FeedbackMatrixCellCount 23 24 (feedbackLimits 1 1 1 0 23 0 0 2 0) retry
    expectLimit FeedbackGraphWork 51 52 (feedbackLimits 1 1 1 0 24 51 0 2 0) retry
    expectBits 3 4 (solve (feedbackLimits 1 1 1 0 24 52 78 2 3) retry)
    -- No shortcut for zero h or gamma: the operation count is unchanged.
    forM_ [changeDirection (replicate 2 [0, 0]) retry, changeGamma 0 retry] $ \fixture -> do
        zero <- checked "zero retains arithmetic plan" (solve exact fixture)
        assert "zero shortcut altered ledger" (feedbackArithmeticWork (affineRewardJVPAccounting (affineRewardJVPReport zero)) == 78)

expectLimit :: FeedbackLimitDimension -> Natural -> Natural -> FeedbackLimits -> Fixture -> IO ()
expectLimit dimension cap required limits fixture =
    case solve limits fixture of
        Left (AffineRewardJVPBaseError (AffineFeedbackLimitError (FeedbackLimitExceeded actual a b))) -> assert "limit precedence" (actual == dimension && a == cap && b == required)
        Left failure -> fail ("wrong failure: " ++ show failure)
        Right _ -> fail "atomic limit failure returned a result"

expectBits :: Natural -> Natural -> Either (AffineRewardJVPError Int Int) a -> IO ()
expectBits cap required result = case result of
    Left (AffineRewardJVPBaseError (AffineFeedbackLimitError (FeedbackRationalBitsExceeded _ a b))) -> assert "rational cap" (a == cap && b == required)
    Left failure -> fail ("wrong rational failure: " ++ show failure)
    Right _ -> fail "rational failure returned a result"

testPrecedence :: IO ()
testPrecedence = do
    let xs = set [0 :: Int]
        us = set [0 :: Int]
        ys = set [0 :: Int]
        sources = set [Left 0, Right 0]
        reverseSources = set [Right 0, Left 0]
        events = set [Exit (exactReward 1) 0, Continue (exactReward 1) 0]
        reverseEvents = set (reverse (finiteSetValues events))
        changedRewards = set [Exit (exactReward 2) 0, Continue (exactReward 1) 0]
        badEvents = set [Continue (exactReward (1 % 2048)) 99, Exit (exactReward 0) 99]
        channel src es = right (stochasticMatrix (right (matrixFromRows src es (replicate 2 (map (right . nonNegativeRational) [1 % 2, 1 % 2])))))
        direction src es = right (matrixFromRows src es (replicate 2 [1 % 4096, 1]))
        call limits src es = closeAffineFeedbackRewardJVP limits (right (mkExactContractionDiscount (1 % 1024))) xs (loopLayout "reward-jvp" us) ys (channel src es)
        expect label failure result = case result of
            Left actual -> assert label (actual == failure)
            Right _ -> fail (label ++ " accepted")
        noRational = feedbackLimits 1 1 1 0 24 52 0 2 0
    expect "base target before layouts" (AffineRewardJVPBaseError (AffineFeedbackContinueOutsideLoop 99)) (call noRational reverseSources badEvents (direction reverseSources reverseEvents))
    expect "base source before direction" (AffineRewardJVPBaseError AffineFeedbackSourceLayoutMismatch) (call noRational reverseSources events (direction reverseSources reverseEvents))
    expect "direction source before events and arithmetic" AffineRewardJVPDirectionSourceLayoutMismatch (call noRational sources events (direction reverseSources reverseEvents))
    expect "direction event order before arithmetic" AffineRewardJVPDirectionEventLayoutMismatch (call noRational sources events (direction sources reverseEvents))
    expect "full reward labels, not routes only" AffineRewardJVPDirectionEventLayoutMismatch (call noRational sources events (direction sources changedRewards))
    expect "gamma first input" (AffineRewardJVPBaseError (AffineFeedbackLimitError (FeedbackRationalBitsExceeded "affine feedback discount input" 0 11))) (call noRational sources events (direction sources events))
    expect "direction input before base work" (AffineRewardJVPBaseError (AffineFeedbackLimitError (FeedbackRationalBitsExceeded "affine reward direction input" 12 13))) (call (feedbackLimits 1 1 1 0 24 52 0 2 12) sources events (direction sources events))
    -- An oversized zero-probability direction is observed, not silently skipped.
    let zeroMass = Fixture [0] [] [0] [Exit (exactReward 0) 0, Exit (exactReward 1) 0] [[1, 0]] [[0, 1 % 4096]] 0
    expectBits 12 13 (solve (feedbackLimits 1 0 1 0 100 100 100 2 12) zeroMass)

testGrowth :: IO ()
testGrowth = do
    let fixture = Fixture [0] [0, 1] [0] [Continue (exactReward 0) 0, Continue (exactReward 0) 1, Exit (exactReward 0) 0] [[0, 0, 1], [1 % 2, 1 % 3, 1 % 6], [1 % 3, 1 % 2, 1 % 6]] [[0, 0, 0], [0, 0, 0], [0, 0, 0]] (1 % 2)
    let exact = feedbackLimits 1 2 1 0 49 120 202 3 7
    result <- checked "discarded Gaussian growth" (solve exact fixture)
    let account = affineRewardJVPAccounting (affineRewardJVPReport result)
    assert "two-loop operation-wide golden" (affineRewardJVPReport result == AffineRewardJVPReport "reward-jvp" 49 120 8 (FeedbackAccounting 202 3 0 7 0 0 7 3 7))
    expectLimit FeedbackMatrixCellCount 48 49 (feedbackLimits 1 2 1 0 48 120 202 3 7) fixture
    expectLimit FeedbackGraphWork 119 120 (feedbackLimits 1 2 1 0 49 119 202 3 7) fixture
    forM_ [0 .. 201] $ \work -> expectLimit FeedbackArithmeticWork work (work + 1) (feedbackLimits 1 2 1 0 49 120 work 3 7) fixture
    assert "discarded values contribute to maximum" (feedbackMaximumGaussianIntermediateBits account == 7 && feedbackMaximumRetainedResultBits account == 3)
    expectBits 6 7 (solve (feedbackLimits 1 2 1 0 1000 1000 10000 3 6) fixture)
    -- Derivative-only growth: base is identically zero, signed hm cancels, but
    -- its first weighted product is observed before cancellation.
    let cancellation = Fixture [0] [] [0] [Exit (exactReward 0) 0, Exit (exactReward 1) 0, Exit (exactReward (-1)) 0] [[0, 1 % 2, 1 % 2]] [[0, 7 % 8, -(7 % 8)]] 0
    grown <- checked "derivative cancellation" (solve large cancellation)
    let ledger = affineRewardJVPAccounting (affineRewardJVPReport grown)
    assert "cancelled derivative remains zero" (constant (affineRewardJVPExternal grown) 0 == 0)
    assert "discarded derivative product grows" (feedbackMaximumInputBits ledger == 4 && feedbackMaximumOtherIntermediateBits ledger == 5 && feedbackMaximumRetainedResultBits ledger == 1)
    expectBits 4 5 (solve (feedbackLimits 1 0 1 0 100 100 1000 3 4) cancellation)
    -- Base prefix is 15 units, then six hm units; growth occurs at unit 18.
    expectLimit FeedbackArithmeticWork 17 18 (feedbackLimits 1 0 1 0 100 100 17 3 4) cancellation

runRewardJVPLesson :: IO ()
runRewardJVPLesson = do
    result <- checked "retry reward JVP" (solve large retry)
    let value = constant (affineFeedbackExternalCoefficients (affineRewardJVPBase result)) 0
        derivative = constant (affineRewardJVPExternal result) 0
        finite = case unroll 4 retry of
            valueAtInput : _ -> valueAtInput
            [] -> error "retry input missing"
        -- Separate symbolic probability direction, deliberately not an API call.
        a = 1 :: Rational
        gamma = 1 % 2
        p = 1 % 2
        probabilityDerivative = a * gamma / (1 - gamma * p) ^ (2 :: Int)
        finiteProbabilityDerivative = sum [a * fromIntegral k * gamma ^ k * p ^ (k - 1) | k <- [1 :: Int .. 3]]
    assert "retry literal solved values" (value == 4 % 3 && derivative == 4 % 3 && probabilityDerivative == 8 % 9)
    assert "finite derivative differs from solved derivative" (finite == (85 % 64, 85 % 64) && derivative - snd finite == 1 % 192 && finiteProbabilityDerivative == 27 % 32 && finiteProbabilityDerivative /= probabilityDerivative)
    zero <- checked "gamma-zero exercise" (solve large (changeGamma 0 retry))
    let probabilitySlope amplitude disc prob = amplitude * disc / (1 - disc * prob) ^ (2 :: Int)
    assert "gamma-zero checked answer" (constant (affineRewardJVPExternal zero) 0 == 1 && probabilitySlope a 0 p == 0)
    putStrLn ("Retry solved V = " ++ show value)
    putStrLn ("API reward derivative dV/da = " ++ show derivative)
    putStrLn ("Four-event unrolling (V, dV/da) = " ++ show finite)
    putStrLn ("Symbolic only dV/dp = " ++ show probabilityDerivative)
    putStrLn ("Symbolic four-event dV/dp = " ++ show finiteProbabilityDerivative)
