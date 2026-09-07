module PairedDifference (runPairedDifferenceTests, runPairedDifferenceLesson) where

import Control.Monad (forM_, unless)
import Data.Ratio ((%))
import Markovian.Continuous.Kernel.JointAffine.Exact
import Markovian.Continuous.Measure.Exact
import Markovian.Continuous.Space (RealBorel)
import Numeric.Natural (Natural)
import Paths_markovian_continuous (getDataFileName)

limits :: ExactLimits
limits = ExactLimits 16 1000 100000 10000 100 1000000 2048

jointLimits :: ExactJointAffineLimits
jointLimits = exactJointAffineLimits 1000 1000 2 10000 1000000 2048

data Scope

type Row = (Natural, Rational, Rational, Rational, Rational)
type Fixture = (Rational, Rational, [Row])

makeJoint :: Fixture -> IO (ExactJointLaw RealBorel RealBorel)
makeJoint (x, y, rows) = do
    noises <- traverse makeRow rows
    kernel <- right (exactJointAffineKernel jointLimits (affineInputCoordinate 0 x) (affineInputCoordinate 0 y) noises)
    fst <$> right (materializeJointAffineKernel jointLimits kernel 0)
  where
    makeRow (owner, a, b, lo, hi) = do
        interval <- right (rationalInterval lo hi)
        pure (noiseOwner owner :: NoiseOwner Scope, interval, a, b)

-- Direct affine uniform formula, independent of production polynomial
-- expansion and moment evaluation. In particular the difference variance
-- is computed from difference coefficients, not the covariance identity.
oracle :: Fixture -> [Rational]
oracle (x, y, rows) = [mx, my, vx, vy, cov, md, vd, baseline, vd - baseline]
  where
    mx = x + sum [a * (lo + hi) / 2 | (_, a, _, lo, hi) <- rows]
    my = y + sum [b * (lo + hi) / 2 | (_, _, b, lo, hi) <- rows]
    vx = sum [a * a * (hi - lo) ^ (2 :: Int) / 12 | (_, a, _, lo, hi) <- rows]
    vy = sum [b * b * (hi - lo) ^ (2 :: Int) / 12 | (_, _, b, lo, hi) <- rows]
    cov = sum [a * b * (hi - lo) ^ (2 :: Int) / 12 | (_, a, b, lo, hi) <- rows]
    md = x - y + sum [(a - b) * (lo + hi) / 2 | (_, a, b, lo, hi) <- rows]
    vd = sum [(a - b) ^ (2 :: Int) * (hi - lo) ^ (2 :: Int) / 12 | (_, a, b, lo, hi) <- rows]
    baseline = vx + vy

values :: PairedDifferenceReport -> [Rational]
values r = map ($ r) [pairedMeanFirst, pairedMeanSecond, pairedVarianceFirst, pairedVarianceSecond, pairedCovariance, pairedMeanDifference, pairedVarianceDifference, pairedIndependentVariance, pairedVarianceExcess]

sharedFixture, oppositeFixture, independentFixture, signedFixture :: Fixture
sharedFixture = (0, 0, [(1, 1, 1, 0, 1)])
oppositeFixture = (0, 1, [(1, 1, -1, 0, 1)])
independentFixture = (0, 0, [(1, 1, 0, 0, 1), (2, 0, 1, 0, 1)])
signedFixture = (-(2 % 3), 3 % 2, [(4, -(3 % 2), 2 % 3, -2, 3), (1, 2, -4, 1 % 3, 5 % 3), (7, 0, -2, -1, 1)])

runPairedDifferenceTests :: IO ()
runPairedDifferenceTests = do
    forM_ [sharedFixture, oppositeFixture, independentFixture, signedFixture, (3, -2, []), (0, 0, []), (1, 2, [(1, -2, -3, -1, 2)]), (0, 0, [(1, 1, 1, 0, 1), (2, 1, -1, 0, 1)])] $ \fixture@(x, y, rows) -> do
        law <- makeJoint fixture
        report <- right (pairedDifferenceReport limits law)
        assert "all nine fields agree with direct moment oracle" (values report == oracle fixture)
        assert "signed comparison" (pairedVarianceComparison report == compare (last (oracle fixture)) 0)
        reordered <- makeJoint (x, y, reverse rows) >>= right . pairedDifferenceReport limits
        assert "canonical reordered values" (values report == values reordered)
        assert "canonical reordered ledger" (pairedDifferenceAccounting report == pairedDifferenceAccounting reordered)
        assert "five moments and twelve derived units" (pairedMomentCount (pairedDifferenceAccounting report) == 5 && pairedDerivedWork (pairedDifferenceAccounting report) == 12)
    unit <- right (rationalInterval 0 1)
    u <- right (uniformReal limits (noiseOwner 1 :: NoiseOwner Scope) unit)
    v <- right (uniformReal limits (noiseOwner 2 :: NoiseOwner Scope) unit)
    shared <- right (pairedDifferenceReport limits (shareAffineSource u))
    independent <- right (independentPair u v) >>= right . pairedDifferenceReport limits
    assert "shareAffineSource consumer" (values shared == oracle sharedFixture)
    assert "independentPair consumer" (values independent == oracle independentFixture)
    law <- makeJoint signedFixture
    report <- right (pairedDifferenceReport limits law)
    goldenPath <- getDataFileName "test/golden/paired-difference-accounting.txt"
    golden <- readFile goldenPath
    assert ("full paired ledger golden: " ++ show (pairedDifferenceAccounting report)) (show (pairedDifferenceAccounting report) ++ "\n" == golden)
    boundaries law report
    failureEvidence
    putStrLn "PASS: paired difference independent oracle, layouts, ledger and admission boundaries"

boundaries :: ExactJointLaw RealBorel RealBorel -> PairedDifferenceReport -> IO ()
boundaries law report = do
    let accounting = pairedDifferenceAccounting report
        work = pairedWork accounting
        raw = pairedRawExpansionPairs accounting
        canonical = pairedMaximumCanonicalTerms accounting
        bits = pairedMaximumRationalBits accounting
        exact = ExactLimits 2 5 raw canonical 3 work bits
        run l = pairedDifferenceReport l law
    admitted <- right (run exact)
    assert "simultaneously exact limits" (pairedDifferenceAccounting admitted == accounting && values admitted == values report)
    left (DegreeLimitExceeded 1 2) (run exact{limitDegree = 1})
    left (TermLimitExceeded 4 5) (run exact{limitTerms = 4})
    left (RawExpansionTermLimitExceeded (raw - 1) raw) (run exact{limitRawExpansionTerms = raw - 1})
    left (CanonicalExpansionTermLimitExceeded (canonical - 1) canonical) (run exact{limitCanonicalExpansionTerms = canonical - 1})
    left (NoiseOwnerLimitExceeded 2 3) (run exact{limitNoiseOwners = 2})
    left (WorkLimitExceeded (work - 1) work) (run exact{limitWork = work - 1})
    left (RationalBitLimitExceeded (bits - 1) bits) (run exact{limitRationalBits = bits - 1})
    forM_ [1 .. raw - 1] $ \budget -> left (RawExpansionTermLimitExceeded budget (budget + 1)) (run limits{limitRawExpansionTerms = budget})
    forM_ [1 .. canonical - 1] $ \budget -> left (CanonicalExpansionTermLimitExceeded budget (budget + 1)) (run limits{limitCanonicalExpansionTerms = budget})
    forM_ [1 .. bits - 1] $ \budget -> left (RationalBitLimitExceeded budget (budget + 1)) (run limits{limitRationalBits = budget})
    -- Every prefix budget fails atomically, including each of the twelve
    -- derived units: no independently reset moment budget can pass this.
    forM_ [1 .. work - 1] $ \budget -> left (WorkLimitExceeded budget (budget + 1)) (run limits{limitWork = budget})

failureEvidence :: IO ()
failureEvidence = do
    law <- makeJoint sharedFixture
    sharedReport <- right (pairedDifferenceReport limits law)
    let sharedWork = pairedWork (pairedDifferenceAccounting sharedReport)
    left (WorkLimitExceeded (sharedWork - 11) (sharedWork - 10)) (pairedDifferenceReport limits{limitWork = sharedWork - 11, limitRationalBits = 3} law)
    left (RationalBitLimitExceeded 3 4) (pairedDifferenceReport limits{limitWork = sharedWork - 10, limitRationalBits = 3} law)
    zero <- right (diracReal limits 0)
    left (DegreeLimitExceeded 0 1) (pairedDifferenceReport limits{limitDegree = 0} (shareAffineSource zero))
    left (TermLimitExceeded 1 2) (pairedDifferenceReport limits{limitTerms = 1} (shareAffineSource zero))
    cancellation <- makeJoint (0, 0, [(1, 1, 1, 0, 1), (2, 1, -1, 0, 1)])
    cancelled <- right (pairedDifferenceReport limits cancellation)
    let cancelledRaw = pairedRawExpansionPairs (pairedDifferenceAccounting cancelled)
    assert "cancellation does not erase generated pairs" (cancelledRaw > pairedMaximumCanonicalTerms (pairedDifferenceAccounting cancelled))
    left (RawExpansionTermLimitExceeded (cancelledRaw - 1) cancelledRaw) (pairedDifferenceReport limits{limitRawExpansionTerms = cancelledRaw - 1} cancellation)
    left (InvalidLimit "term limit must be positive") (pairedDifferenceReport limits{limitTerms = 0, limitWork = 0} law)
    left (InvalidLimit "execution limits exceed machine indexing") (pairedDifferenceReport limits{limitDegree = fromIntegral (maxBound :: Int) + 1} law)
    left (DegreeLimitExceeded 1 2) (pairedDifferenceReport limits{limitDegree = 1, limitTerms = 4, limitWork = 1} law)
    left (TermLimitExceeded 4 5) (pairedDifferenceReport limits{limitTerms = 4, limitWork = 1} law)
    left (WorkLimitExceeded 1 2) (pairedDifferenceReport limits{limitWork = 1, limitRationalBits = 1} law)
    -- Coefficient 8 is admitted at four bits; the discarded support product
    -- 8*8 requires seven. At the same step, work must fail before arithmetic.
    growth <- makeJoint (0, 0, [(1, 8, 8, 0, 8)])
    left (WorkLimitExceeded 12 13) (pairedDifferenceReport limits{limitWork = 12, limitRationalBits = 4} growth)
    left (RationalBitLimitExceeded 4 5) (pairedDifferenceReport limits{limitWork = 13, limitRationalBits = 4} growth)
    -- Large constant squares cancel from variances, but still bound bits.
    deterministic <- right (diracReal limits 8)
    full <- right (pairedDifferenceReport limits (shareAffineSource deterministic))
    assert "cancelled moment growth retained" (pairedVarianceDifference full == 0 && pairedMaximumRationalBits (pairedDifferenceAccounting full) >= 7)
    left (RationalBitLimitExceeded 4 5) (pairedDifferenceReport limits{limitRationalBits = 4} (shareAffineSource deterministic))
    -- Raw-pair charge precedes work, even when both would fail.
    left (RawExpansionTermLimitExceeded 1 2) (pairedDifferenceReport limits{limitRawExpansionTerms = 1, limitWork = 22} law)

runPairedDifferenceLesson :: IO ()
runPairedDifferenceLesson = forM_ [("U versus U", sharedFixture), ("U versus 1-U", oppositeFixture), ("independent uniforms", independentFixture)] $ \(label, fixture) -> do
    report <- makeJoint fixture >>= right . pairedDifferenceReport limits
    assert "lesson independent exact answer" (values report == oracle fixture)
    putStrLn label
    putStrLn ("  means = " ++ show (pairedMeanFirst report, pairedMeanSecond report))
    putStrLn ("  variances = " ++ show (pairedVarianceFirst report, pairedVarianceSecond report))
    putStrLn ("  covariance = " ++ show (pairedCovariance report))
    putStrLn ("  mean difference = " ++ show (pairedMeanDifference report))
    putStrLn ("  difference variance = " ++ show (pairedVarianceDifference report))
    putStrLn ("  independent baseline = " ++ show (pairedIndependentVariance report))
    putStrLn ("  signed excess/comparison = " ++ show (pairedVarianceExcess report, pairedVarianceComparison report))

right :: (Show e) => Either e a -> IO a
right = either (fail . show) pure

left :: (Eq e, Show e) => e -> Either e a -> IO ()
left expected result = case result of
    Left actual -> assert ("expected " ++ show expected ++ ", got " ++ show actual) (actual == expected)
    Right _ -> fail ("expected atomic failure " ++ show expected)

assert :: String -> Bool -> IO ()
assert label condition = unless condition (fail label)
