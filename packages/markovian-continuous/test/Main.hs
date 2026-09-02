module Main (main) where

import Control.Monad (forM_, replicateM, unless)
import Data.List (foldl')
import Data.Ratio ((%))
import Markovian.Continuous.Condition.Exact
import Markovian.Continuous.Kernel.Exact
import Markovian.Continuous.Map
import Markovian.Continuous.Measure.Exact
import Markovian.Continuous.Polynomial
import Markovian.Continuous.Space
import Numeric.Natural (Natural)
import Paths_markovian_continuous (getDataFileName)
import System.Exit (exitFailure)

data Coin = Heads | Tails deriving (Eq, Show)

data OwnerA
data OwnerB
data KernelA
data KernelB
data KernelC

limits :: ExactLimits
limits = ExactLimits 16 20000 200000 20000 32 200000 2048

main :: IO ()
main = do
    check "real space witness" (spaceDescription realBorel == "real Borel")
    checkRight "finite discrete witness" (finiteDiscrete 2 [Heads, Tails]) (const True)
    checkLeft "infinite discrete input is bounded" (finiteDiscrete 3 ([0 ..] :: [Integer])) (== DiscreteLayoutLimitExceeded 3 4)
    check "affine map composition" (applyRealMap (composeRealMap (affineRealMap 2 1) (affineRealMap 3 4)) 5 == 37)
    interval <- requireRight "interval" (rationalInterval 0 1)
    uniformA <- requireRight "uniform A" (uniformReal limits (noiseOwner 1 :: NoiseOwner OwnerA) interval)
    forM_ [0 .. 8] $ \degree -> do
        polynomial <- requireRight "moment polynomial" (rationalPolynomial limits [(degree, 1)])
        report <- requireRight "uniform moment" (expectPolynomial limits uniformA polynomial)
        check ("uniform moment " ++ show degree) (exactIntegralValue report == 1 % fromIntegral (degree + 1))
    affine <- requireRight "affine pushforward" (affinePushforward limits 2 3 uniformA)
    square <- requireRight "square polynomial" (rationalPolynomial limits [(2, 1)])
    affineSquare <- requireRight "affine square" (expectPolynomial limits affine square)
    check "affine pushforward expectation" (exactIntegralValue affineSquare == 49 % 3)

    xy <- requireRight "xy polynomial" (rationalBivariatePolynomial limits [(1, 1, 1)])
    shared <- requireRight "shared expectation" (expectBivariatePolynomial limits (shareAffineSource uniformA) xy)
    uniformB <- requireRight "uniform B" (uniformReal limits (noiseOwner 2 :: NoiseOwner OwnerB) interval)
    independent <- requireRight "independent pair" (independentPair uniformA uniformB)
    independentResult <- requireRight "independent expectation" (expectBivariatePolynomial limits independent xy)
    check "shared noise gives E[U U] = 1/3" (exactIntegralValue shared == 1 % 3)
    check "independent noise gives E[U1 U2] = 1/4" (exactIntegralValue independentResult == 1 % 4)

    -- Complete powers on both coordinates use an independent raw symbolic
    -- enumeration oracle. The oracle does not use the package expansion or
    -- canonicalization implementation.
    sharedBivariateLaw <-
        requireRight
            "shared bivariate law"
            ( affineUniformLaw
                limits
                1
                [ (noiseOwner 101 :: NoiseOwner OwnerA, 2, interval)
                , (noiseOwner 102 :: NoiseOwner OwnerA, -1, interval)
                ]
            )
    completeBivariate <-
        requireRight
            "complete bivariate polynomial"
            (rationalBivariatePolynomial limits [(2, 3, 3), (1, 2, -5), (0, 0, 7)])
    sharedBivariateReport <-
        requireRight
            "complete shared bivariate expansion"
            (expectBivariatePolynomial limits (shareAffineSource sharedBivariateLaw) completeBivariate)
    let sharedOracleForm = (1, [(101, 2, (0, 1)), (102, -1, (0, 1))])
        sharedOracle = oracleBivariate sharedOracleForm sharedOracleForm [(2, 3, 3), (1, 2, -5), (0, 0, 7)]
    check "complete shared-owner raw expansion oracle" (exactIntegralValue sharedBivariateReport == oracleIntegralValue sharedOracle)
    check "shared oracle reports every raw enumeration" (oracleRawEnumerationCount sharedOracle == 271)
    check "shared oracle canonicalizes before integration" (not (null (oracleCanonicalTerms sharedOracle)) && fromIntegral (length (oracleCanonicalTerms sharedOracle)) < oracleRawEnumerationCount sharedOracle)

    independentLeft <-
        requireRight
            "independent bivariate left"
            ( affineUniformLaw
                limits
                (-1)
                [ (noiseOwner 201 :: NoiseOwner OwnerA, 3 % 2, interval)
                , (noiseOwner 202 :: NoiseOwner OwnerA, 1 % 3, interval)
                ]
            )
    independentRight <-
        requireRight
            "independent bivariate right"
            ( affineUniformLaw
                limits
                2
                [ (noiseOwner 301 :: NoiseOwner OwnerB, -(2 % 3), interval)
                , (noiseOwner 302 :: NoiseOwner OwnerB, 5 % 4, interval)
                ]
            )
    completeIndependent <- requireRight "complete independent owners" (independentPair independentLeft independentRight)
    independentBivariateReport <-
        requireRight
            "complete independent bivariate expansion"
            (expectBivariatePolynomial limits completeIndependent completeBivariate)
    let independentLeftForm = (-1, [(201, 3 % 2, (0, 1)), (202, 1 % 3, (0, 1))])
        independentRightForm = (2, [(301, -(2 % 3), (0, 1)), (302, 5 % 4, (0, 1))])
        independentOracle = oracleBivariate independentLeftForm independentRightForm [(2, 3, 3), (1, 2, -5), (0, 0, 7)]
    check "complete independent-owner raw expansion oracle" (exactIntegralValue independentBivariateReport == oracleIntegralValue independentOracle)
    check "independent oracle reports every raw enumeration" (oracleRawEnumerationCount independentOracle == 271)
    check "independent oracle retains canonical monomial evidence" (all ((/= 0) . snd) (oracleCanonicalTerms independentOracle))
    check "bivariate accounting is cumulative and complete" (accountingAddsUp independentBivariateReport)
    goldenPath <- getDataFileName "test/golden/bivariate-accounting.txt"
    goldenAccounting <- readFile goldenPath
    check "bivariate accounting golden" (accountingReportText independentBivariateReport == goldenAccounting)
    repeatedBivariateReport <- requireRight "deterministic bivariate report" (expectBivariatePolynomial limits completeIndependent completeBivariate)
    check "bivariate report is deterministic" (repeatedBivariateReport == independentBivariateReport)

    let rawExact = exactIntegralRawExpansionPairs independentBivariateReport
        canonicalExact = exactIntegralMaximumCanonicalTerms independentBivariateReport
        workExact = exactIntegralWork independentBivariateReport
        rationalExact = exactIntegralMaximumRationalBits independentBivariateReport
    rawBoundaryReport <-
        requireRight
            "exact raw expansion limit"
            (expectBivariatePolynomial limits{limitRawExpansionTerms = rawExact} completeIndependent completeBivariate)
    check "exact raw expansion limit preserves report" (rawBoundaryReport == independentBivariateReport)
    checkLeft
        "one-below raw expansion limit is atomic"
        (expectBivariatePolynomial limits{limitRawExpansionTerms = rawExact - 1} completeIndependent completeBivariate)
        (== RawExpansionTermLimitExceeded (rawExact - 1) rawExact)
    _ <-
        requireRight
            "exact canonical expansion limit"
            (expectBivariatePolynomial limits{limitCanonicalExpansionTerms = canonicalExact} completeIndependent completeBivariate)
    checkLeft
        "one-below canonical expansion limit is atomic"
        (expectBivariatePolynomial limits{limitCanonicalExpansionTerms = canonicalExact - 1} completeIndependent completeBivariate)
        (== CanonicalExpansionTermLimitExceeded (canonicalExact - 1) canonicalExact)
    _ <-
        requireRight
            "exact cumulative work limit"
            (expectBivariatePolynomial limits{limitWork = workExact} completeIndependent completeBivariate)
    checkLeft
        "one-below cumulative work limit is atomic"
        (expectBivariatePolynomial limits{limitWork = workExact - 1} completeIndependent completeBivariate)
        (== WorkLimitExceeded (workExact - 1) workExact)
    _ <-
        requireRight
            "exact intermediate rational limit"
            (expectBivariatePolynomial limits{limitRationalBits = rationalExact} completeIndependent completeBivariate)
    checkLeft
        "one-below intermediate rational limit is atomic"
        (expectBivariatePolynomial limits{limitRationalBits = rationalExact - 1} completeIndependent completeBivariate)
        (== RationalBitLimitExceeded (rationalExact - 1) rationalExact)

    deterministicEight <- requireRight "deterministic eight law" (diracReal limits 8)
    cancellationPair <- requireRight "cancellation pair" (independentPair uniformA deterministicEight)
    cancellationPolynomial <- requireRight "cancellation polynomial" (rationalBivariatePolynomial limits [(1, 1, 1), (1, 0, -8)])
    cancellationReport <- requireRight "cross-term cancellation" (expectBivariatePolynomial limits cancellationPair cancellationPolynomial)
    check "cross-term cancellation is canonical" (exactIntegralValue cancellationReport == 0 && exactIntegralCanonicalTerms cancellationReport == 0 && exactIntegralCoefficientCancellations cancellationReport > 0)
    check "discarded rational maximum exceeds retained zero" (exactIntegralMaximumRationalBits cancellationReport > 1)
    check "duplicate monomials are combined" (exactIntegralCoefficientAdditions independentBivariateReport > 0 && exactIntegralCanonicalCombinations independentBivariateReport > exactIntegralCanonicalTerms independentBivariateReport)

    likelihood <- requireRight "likelihood" (affineFiniteLikelihood limits [Heads, Tails] interval [(Heads, 1, 0), (Tails, -1, 1)])
    headsPosterior <- requireRight "heads posterior" (conditionFiniteObservation limits uniformA likelihood Heads)
    tailsPosterior <- requireRight "tails posterior" (conditionFiniteObservation limits uniformA likelihood Tails)
    identityPolynomial <- requireRight "identity polynomial" (rationalPolynomial limits [(1, 1)])
    headsMean <- requireRight "heads mean" (posteriorExpectation limits headsPosterior identityPolynomial)
    tailsMean <- requireRight "tails mean" (posteriorExpectation limits tailsPosterior identityPolynomial)
    check "heads evidence" (posteriorEvidence headsPosterior == 1 % 2)
    check "Bayes reconstruction" (posteriorEvidence headsPosterior * headsMean == 1 % 3)
    check "heads posterior mean" (headsMean == 2 % 3)
    check "tails posterior mean" (tailsMean == 1 % 3)
    disintegration <- requireRight "finite disintegration" (finiteObservationDisintegration limits uniformA likelihood)
    check "all positive rows retained" (length (positivePosteriorRows disintegration) == 2)

    zeroLikelihood <- requireRight "zero row likelihood" (affineFiniteLikelihood limits [Heads, Tails] interval [(Heads, 0, 0), (Tails, 0, 1)])
    checkLeft "zero evidence rejected" (conditionFiniteObservation limits uniformA zeroLikelihood Heads) (== ZeroEvidence)

    firstKernel <- requireRight "first kernel" (affineUniformKernel limits 2 1 [(noiseOwner 10 :: NoiseOwner KernelA, 1, interval)])
    secondKernel <- requireRight "second kernel" (affineUniformKernel limits 3 4 [(noiseOwner 11 :: NoiseOwner KernelB, 1, interval)])
    composed <- requireRight "kernel composition" (composeContinuousKernel limits firstKernel secondKernel)
    composedLaw <- requireRight "kernel law" (kernelLawAt limits composed 2)
    meanPolynomial <- requireRight "mean polynomial" (rationalPolynomial limits [(1, 1)])
    composedMean <- requireRight "composed mean" (expectPolynomial limits composedLaw meanPolynomial)
    check "kernel composition expectation" (exactIntegralValue composedMean == 21)
    let identityKernel = deterministicAffineKernel 1 0
    leftIdentity <- requireRight "kernel left identity" (composeContinuousKernel limits identityKernel firstKernel)
    rightIdentity <- requireRight "kernel right identity" (composeContinuousKernel limits firstKernel identityKernel)
    directLaw <- requireRight "direct kernel law" (kernelLawAt limits firstKernel 2)
    leftLaw <- requireRight "left identity law" (kernelLawAt limits leftIdentity 2)
    rightLaw <- requireRight "right identity law" (kernelLawAt limits rightIdentity 2)
    directMean <- requireRight "direct kernel mean" (expectPolynomial limits directLaw meanPolynomial)
    leftMean <- requireRight "left identity mean" (expectPolynomial limits leftLaw meanPolynomial)
    rightMean <- requireRight "right identity mean" (expectPolynomial limits rightLaw meanPolynomial)
    check "kernel identity laws" (exactIntegralValue directMean == exactIntegralValue leftMean && exactIntegralValue directMean == exactIntegralValue rightMean)
    thirdKernel <- requireRight "third kernel" (affineUniformKernel limits (-1) 2 [(noiseOwner 12 :: NoiseOwner KernelC, 1, interval)])
    firstThenSecond <- requireRight "first then second" (composeContinuousKernel limits firstKernel secondKernel)
    secondThenThird <- requireRight "second then third" (composeContinuousKernel limits secondKernel thirdKernel)
    associatedLeft <- requireRight "left-associated kernels" (composeContinuousKernel limits firstThenSecond thirdKernel)
    associatedRight <- requireRight "right-associated kernels" (composeContinuousKernel limits firstKernel secondThenThird)
    associatedLeftLaw <- requireRight "left-associated law" (kernelLawAt limits associatedLeft 2)
    associatedRightLaw <- requireRight "right-associated law" (kernelLawAt limits associatedRight 2)
    associatedLeftValue <- requireRight "left-associated value" (expectPolynomial limits associatedLeftLaw square)
    associatedRightValue <- requireRight "right-associated value" (expectPolynomial limits associatedRightLaw square)
    check "representative kernel associativity" (exactIntegralValue associatedLeftValue == exactIntegralValue associatedRightValue)
    checkLeft "owner collision is atomic" (composeContinuousKernel limits firstKernel firstKernel) (== NoiseOwnerCollision)
    checkLeft "duplicate renaming source" (alphaRenameKernel limits [(noiseOwner 10 :: NoiseOwner KernelA, noiseOwner 20 :: NoiseOwner KernelB), (noiseOwner 10, noiseOwner 21)] firstKernel) (== DuplicateNoiseMapping)
    checkLeft "non-injective owner renaming" (alphaRenameKernel limits [(noiseOwner 10 :: NoiseOwner KernelA, noiseOwner 20 :: NoiseOwner KernelB), (noiseOwner 11, noiseOwner 20)] composed) (== NonInjectiveNoiseRenaming)
    checkLeft "renaming cannot collide with an unmapped owner" (alphaRenameKernel limits [(noiseOwner 10 :: NoiseOwner KernelA, noiseOwner 11 :: NoiseOwner KernelB)] composed) (== NonInjectiveNoiseRenaming)

    let rawLimited = limits{limitTerms = 2}
    checkLeft "zero polynomial entries consume traversal budget" (rationalPolynomial rawLimited [(0, 0), (1, 0), (2, 1)]) (== TermLimitExceeded 2 3)
    checkLeft "infinite polynomial input is bounded" (rationalPolynomial rawLimited [(degree, 0) | degree <- [0 ..]]) (== TermLimitExceeded 2 3)
    checkLeft "infinite noise input is bounded" (affineUniformLaw rawLimited 0 (repeat (noiseOwner 99 :: NoiseOwner OwnerA, 0, interval))) (== TermLimitExceeded 2 3)

    narrowInterval <- requireRight "narrow interval" (rationalInterval 0 (1 % 4))
    narrowLaw <- requireRight "narrow law" (uniformReal limits (noiseOwner 30 :: NoiseOwner OwnerA) narrowInterval)
    steepLikelihood <- requireRight "steep likelihood" (affineFiniteLikelihood limits [Heads, Tails] narrowInterval [(Heads, 2, 0), (Tails, -2, 1)])
    steepPosterior <- requireRight "steep posterior" (conditionFiniteObservation limits narrowLaw steepLikelihood Heads)
    largeCoefficient <- requireRight "large coefficient" (rationalPolynomial limits [(1, 8)])
    let productLimited = limits{limitRationalBits = 4}
    checkLeft "posterior checks weighted coefficient before cancellation" (posteriorExpectation productLimited steepPosterior largeCoefficient) isRationalLimit

    let rowBudget = limits{limitWork = 11}
    checkLeft "disintegration uses one operation-wide work account" (finiteObservationDisintegration rowBudget uniformA likelihood) (== WorkLimitExceeded 11 21)

    let supportLimited = limits{limitRationalBits = 4}
    checkLeft "affine support products are checked" (affineUniformLaw supportLimited 0 [(noiseOwner 40 :: NoiseOwner OwnerA, 8, rightValue (rationalInterval 0 8))]) isRationalLimit

    let degreeLimited = limits{limitDegree = 1}
    checkLeft "one-below degree budget" (rationalPolynomial degreeLimited [(2, 1)]) (== DegreeLimitExceeded 1 2)
    let workLimited = limits{limitWork = 2}
    checkLeft "work exhaustion returns no report" (expectPolynomial workLimited uniformA square) isWorkLimit
    putStrLn "continuous exact tests passed"

type OracleForm = (Rational, [(Natural, Rational, (Rational, Rational))])

type OracleTerm = ([(Natural, Natural)], Rational)

data OracleEvidence = OracleEvidence
    { oracleRawEnumerationCount :: !Natural
    , oracleCanonicalTerms :: ![OracleTerm]
    , oracleIntegralValue :: !Rational
    }

oracleBivariate :: OracleForm -> OracleForm -> [(Natural, Natural, Rational)] -> OracleEvidence
oracleBivariate left right polynomial =
    OracleEvidence
        { oracleRawEnumerationCount = fromIntegral (length rawTerms)
        , oracleCanonicalTerms = canonicalTerms
        , oracleIntegralValue = sum [coefficient * oracleMonomialMoment intervals powers | (powers, coefficient) <- canonicalTerms]
        }
  where
    intervals = oracleIntervals left ++ oracleIntervals right
    rawTerms =
        [ (foldl' (flip oracleInsertExponent) leftPowers rightPowers, polynomialCoefficient * leftCoefficient * rightCoefficient)
        | (leftPower, rightPower, polynomialCoefficient) <- polynomial
        , (leftPowers, leftCoefficient) <- oraclePower left leftPower
        , (rightPowers, rightCoefficient) <- oraclePower right rightPower
        ]
    canonicalTerms = foldl' (flip oracleInsertTerm) [] rawTerms

oracleInsertTerm :: OracleTerm -> [OracleTerm] -> [OracleTerm]
oracleInsertTerm (_, 0) terms = terms
oracleInsertTerm candidate [] = [candidate]
oracleInsertTerm candidate@(powers, coefficient) allTerms@(term@(existing, value) : rest) =
    case compare powers existing of
        LT -> candidate : allTerms
        GT -> term : oracleInsertTerm candidate rest
        EQ
            | coefficient + value == 0 -> rest
            | otherwise -> (powers, coefficient + value) : rest

oraclePower :: OracleForm -> Natural -> [OracleTerm]
oraclePower (constant, noise) degree =
    [ foldl' select ([], 1) selected
    | selected <- replicateM (fromIntegral degree) choices
    ]
  where
    choices = (Nothing, constant) : [(Just owner, coefficient) | (owner, coefficient, _) <- noise]
    select (powers, coefficient) (owner, selectedCoefficient) =
        ( maybe powers (`oracleInsertPower` powers) owner
        , coefficient * selectedCoefficient
        )

oracleInsertPower :: Natural -> [(Natural, Natural)] -> [(Natural, Natural)]
oracleInsertPower owner = oracleInsertExponent (owner, 1)

oracleInsertExponent :: (Natural, Natural) -> [(Natural, Natural)] -> [(Natural, Natural)]
oracleInsertExponent term [] = [term]
oracleInsertExponent term@(owner, addedPower) ((existing, power) : rest)
    | owner < existing = term : (existing, power) : rest
    | owner == existing = (owner, power + addedPower) : rest
    | otherwise = (existing, power) : oracleInsertExponent term rest

oracleIntervals :: OracleForm -> [(Natural, (Rational, Rational))]
oracleIntervals (_, noise) = [(owner, interval) | (owner, _, interval) <- noise]

oracleMonomialMoment :: [(Natural, (Rational, Rational))] -> [(Natural, Natural)] -> Rational
oracleMonomialMoment intervals = product . map ownerMoment
  where
    ownerMoment (owner, degree) = case lookup owner intervals of
        Nothing -> error "oracle owner is missing"
        Just (lower, upper) ->
            (upper ^ (degree + 1) - lower ^ (degree + 1))
                / (fromIntegral (degree + 1) * (upper - lower))

accountingReportText :: ExactIntegralReport -> String
accountingReportText report =
    unlines
        [ "value\t" ++ show (exactIntegralValue report)
        , "degree\t" ++ show (exactIntegralDegree report)
        , "input-terms\t" ++ show (exactIntegralTerms report)
        , "noise-owners\t" ++ show (exactIntegralNoiseOwners report)
        , "work\t" ++ show (exactIntegralWork report)
        , "raw-expansion-pairs\t" ++ show (exactIntegralRawExpansionPairs report)
        , "canonical-terms\t" ++ show (exactIntegralCanonicalTerms report)
        , "maximum-canonical-terms\t" ++ show (exactIntegralMaximumCanonicalTerms report)
        , "monomial-power-merges\t" ++ show (exactIntegralMonomialPowerMerges report)
        , "monomial-power-comparisons\t" ++ show (exactIntegralMonomialPowerComparisons report)
        , "coefficient-multiplications\t" ++ show (exactIntegralCoefficientMultiplications report)
        , "coefficient-additions\t" ++ show (exactIntegralCoefficientAdditions report)
        , "coefficient-cancellations\t" ++ show (exactIntegralCoefficientCancellations report)
        , "canonical-combinations\t" ++ show (exactIntegralCanonicalCombinations report)
        , "input-arithmetic-operations\t" ++ show (exactIntegralInputArithmeticOperations report)
        , "moment-power-operations\t" ++ show (exactIntegralMomentPowerOperations report)
        , "moment-arithmetic-operations\t" ++ show (exactIntegralMomentArithmeticOperations report)
        , "moment-products\t" ++ show (exactIntegralMomentProducts report)
        , "final-summations\t" ++ show (exactIntegralFinalSummations report)
        , "maximum-rational-bits\t" ++ show (exactIntegralMaximumRationalBits report)
        ]

accountingAddsUp :: ExactIntegralReport -> Bool
accountingAddsUp report =
    exactIntegralWork report
        == exactIntegralRawExpansionPairs report
            + exactIntegralMonomialPowerMerges report
            + exactIntegralMonomialPowerComparisons report
            + exactIntegralCoefficientMultiplications report
            + exactIntegralCoefficientAdditions report
            + exactIntegralCoefficientCancellations report
            + exactIntegralCanonicalCombinations report
            + exactIntegralInputArithmeticOperations report
            + exactIntegralMomentPowerOperations report
            + exactIntegralMomentArithmeticOperations report
            + exactIntegralMomentProducts report
            + exactIntegralFinalSummations report

isWorkLimit :: ExactError -> Bool
isWorkLimit err = case err of
    WorkLimitExceeded _ _ -> True
    _ -> False

isRationalLimit :: ExactError -> Bool
isRationalLimit err = case err of
    RationalBitLimitExceeded _ _ -> True
    _ -> False

rightValue :: Either err value -> value
rightValue result = case result of
    Left _ -> error "test fixture construction failed"
    Right value -> value

check :: String -> Bool -> IO ()
check label condition = unless condition $ do
    putStrLn ("FAIL: " ++ label)
    exitFailure

checkRight :: (Show err) => String -> Either err value -> (value -> Bool) -> IO ()
checkRight label result predicate = case result of
    Left err -> failWith label (show err)
    Right value -> check label (predicate value)

checkLeft :: String -> Either err value -> (err -> Bool) -> IO ()
checkLeft label result predicate = case result of
    Left err -> check label (predicate err)
    Right _ -> failWith label "unexpected Right"

requireRight :: (Show err) => String -> Either err value -> IO value
requireRight label result = case result of
    Left err -> failWith label (show err)
    Right value -> pure value

failWith :: String -> String -> IO value
failWith label detail = do
    putStrLn ("FAIL: " ++ label ++ ": " ++ detail)
    exitFailure
