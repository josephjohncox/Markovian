module Main (main) where

import Control.Monad (forM_, unless)
import Data.Bits (shiftR, xor)
import Data.Char (ord)
import Data.Maybe (fromMaybe, isNothing)
import Data.Ratio ((%))
import Data.Word (Word64)
import Markovian.Continuous.Measure.Exact
import Markovian.Continuous.Numerical.Generator
import Markovian.Continuous.Numerical.MonteCarlo
import Markovian.Continuous.Numerical.Quadrature
import Markovian.Continuous.Numerical.Value
import Markovian.Continuous.Polynomial
import System.Exit (exitFailure)

data ExactOwner
data ExactOwnerB

main :: IO ()
main = do
    (_, rounding) <- requireRight "rational conversion" (approximateRational (1 % 10))
    check "rounding report reconstructs exact value" (toRational (roundedValue rounding) + exactRoundingDifference rounding == 1 % 10)
    interval <- requireRight "double interval" (compactDoubleInterval (-1) 1)
    checkLeft "nonfinite interval width rejected" (compactDoubleInterval (-1.7976931348623157e308) 1.7976931348623157e308) isInvalidInterval
    checkLeft "uniform rejects nonfinite width" (uniformLaw (-1.7976931348623157e308) 1.7976931348623157e308) isSamplingValueError
    unitInterval <- requireRight "unit interval" (compactDoubleInterval 0 1)
    tolerance <- requireRight "tolerance" (integrationTolerance 1e-13 1e-13)
    let limits = QuadratureLimits 1005 33 20
    polynomial <- requireRight "polynomial quadrature" (integrateGK15 limits tolerance interval (Right . (** 4)))
    close "integral x^4" 1e-13 (2 / 5) (quadratureValue polynomial)
    check "quadrature termination named estimate" (quadratureTermination polynomial == EstimatedToleranceMet)
    check "deterministic quadrature report" (integrateGK15 limits tolerance interval (Right . (** 4)) == Right polynomial)
    spike <- requireRight "missed spike counterexample" (integrateGK15 limits tolerance interval (\x -> Right (if x >= 0.123456 && x <= 0.123457 then 1 else 0)))
    check "estimated error is not a certified bound" (quadratureValue spike == 0 && estimatedAbsoluteError spike == 0)
    checkLeft "callback failure has no estimate" (integrateGK15 limits tolerance interval (\_ -> Left "fixture")) isCallbackFailure
    let tinyLimits = QuadratureLimits 14 1 1
    checkLeft "evaluation preflight" (integrateGK15 tinyLimits tolerance interval Right) (== InvalidQuadratureLimits)
    let noSplitEvaluations = QuadratureLimits 15 10 10
    checkLeft "exact evaluation budget boundary" (integrateGK15 noSplitEvaluations tolerance unitInterval (Right . (** 20))) isEvaluationBudget
    let noSubdivisions = QuadratureLimits 45 0 10
    checkLeft "exact subdivision budget boundary" (integrateGK15 noSubdivisions tolerance unitInterval (Right . (** 20))) isSubdivisionBudget
    wideInterval <- requireRight "wide interval" (compactDoubleInterval (-2) 2)
    let rootNodes = [0, 0.20778495500789847, 0.40584515137739717, 0.5860872354676911, 0.7415311855993945, 0.8648644233597691, 0.9491079123427585, 0.9914553711208126]
        rootPoints = 0 : concat [[-(2 * node), 2 * node] | node <- drop 1 rootNodes]
        overflowAfterSplit x = Right (if x `elem` rootPoints then if x == 0 then 1 else 0 else 8e307)
    checkLeft "nonfinite aggregate estimate rejected" (integrateGK15 limits tolerance wideInterval overflowAfterSplit) isAggregateFailure
    overflowingTolerance <- requireRight "large relative tolerance" (integrationTolerance 1e-13 3)
    checkLeft "nonfinite tolerance threshold rejected" (integrateGK15 limits overflowingTolerance unitInterval (const (Right 8e307))) isAggregateFailure

    exactInterval <- requireRight "exact interval" (rationalInterval 0 1)
    let exactLimits = ExactLimits 8 100 10000 1000 4 10000 512
    exactLaw <- requireRight "exact law" (uniformReal exactLimits (noiseOwner 1 :: NoiseOwner ExactOwner) exactInterval)
    exactSquare <- requireRight "exact square" (rationalPolynomial exactLimits [(2, 1)])
    exactReport <- requireRight "exact square expectation" (expectPolynomial exactLimits exactLaw exactSquare)
    numericalSquare <- requireRight "numerical square" (integrateGK15 limits tolerance unitInterval (Right . (\x -> x * x)))
    close "exact/numerical differential" 1e-13 (fromRational (exactIntegralValue exactReport)) (quadratureValue numericalSquare)

    -- A bounded independent bivariate affine fixture. Tensor-product Simpson
    -- is a test oracle only; it is not a multidimensional cubature API.
    exactLeft <-
        requireRight
            "exact bivariate left"
            (affineUniformLaw exactLimits (1 % 3) [(noiseOwner 11 :: NoiseOwner ExactOwner, 5 % 4, exactInterval)])
    exactRight <-
        requireRight
            "exact bivariate right"
            (affineUniformLaw exactLimits (-(2 % 5)) [(noiseOwner 12 :: NoiseOwner ExactOwnerB, 7 % 6, exactInterval)])
    exactJoint <- requireRight "exact bivariate independent owners" (independentPair exactLeft exactRight)
    exactBivariatePolynomial <- requireRight "exact bivariate polynomial" (rationalBivariatePolynomial exactLimits [(2, 1, 1), (1, 2, 1)])
    exactBivariateReport <- requireRight "exact bivariate expectation" (expectBivariatePolynomial exactLimits exactJoint exactBivariatePolynomial)
    let leftOffset = 1 / 3
        leftScale = 5 / 4
        rightOffset = -(2 / 5)
        rightScale = 7 / 6
        fixtureParameters = [leftOffset, leftScale, rightOffset, rightScale]
        numericalBivariate parameters = case parameters of
            [fixtureLeftOffset, fixtureLeftScale, fixtureRightOffset, fixtureRightScale] ->
                simpson2D
                    40
                    ( \u v ->
                        let x = fixtureLeftOffset + fixtureLeftScale * u
                            y = fixtureRightOffset + fixtureRightScale * v
                         in x * x * y + x * y * y
                    )
                    0
                    1
                    0
                    1
            _ -> 0 / 0
        leftMean = leftOffset + leftScale / 2
        leftSecond = leftOffset * leftOffset + leftOffset * leftScale + leftScale * leftScale / 3
        rightMean = rightOffset + rightScale / 2
        rightSecond = rightOffset * rightOffset + rightOffset * rightScale + rightScale * rightScale / 3
        analyticGradient =
            [ (2 * leftOffset + leftScale) * rightMean + rightSecond
            , (leftOffset + 2 * leftScale / 3) * rightMean + rightSecond / 2
            , leftSecond + leftMean * (2 * rightOffset + rightScale)
            , leftSecond / 2 + leftMean * (rightOffset + 2 * rightScale / 3)
            ]
    close "exact/bivariate numerical oracle" 2e-12 (fromRational (exactIntegralValue exactBivariateReport)) (numericalBivariate fixtureParameters)
    forM_ (zip3 [0 ..] ["left offset", "left scale", "right offset", "right scale"] analyticGradient) $ \(coordinate, label, expectedDerivative) -> do
        let parameter = fixtureParameters !! coordinate
            step = 1e-6 * max 1 (abs parameter)
            plus = replaceAt coordinate (parameter + step) fixtureParameters
            minus = replaceAt coordinate (parameter - step) fixtureParameters
            finiteDifference = (numericalBivariate plus - numericalBivariate minus) / (2 * step)
        check ("finite perturbations for " ++ label) (not (isNaN finiteDifference || isInfinite finiteDifference))
        closeRelative ("bivariate finite difference " ++ label) 3e-8 expectedDerivative finiteDifference

    -- Independent composite Simpson differential.
    expReport <- requireRight "exp quadrature" (integrateGK15 limits tolerance unitInterval (Right . exp))
    close "GK15/Simpson differential" 1e-12 (simpson 1000 exp 0 1) (quadratureValue expReport)

    -- Every selected parameter coordinate in E[(a*x+b+U)^2].
    let objective a b = quadratureValue <$> integrateGK15 limits tolerance unitInterval (\u -> Right ((a * 2 + b + u) ^ (2 :: Int)))
        h value = 1e-6 * max 1 (abs value)
    baseAPlus <- requireRight "a plus" (objective (3 + h 3) 4)
    baseAMinus <- requireRight "a minus" (objective (3 - h 3) 4)
    baseBPlus <- requireRight "b plus" (objective 3 (4 + h 4))
    baseBMinus <- requireRight "b minus" (objective 3 (4 - h 4))
    closeRelative "finite difference a" 2e-8 42 ((baseAPlus - baseAMinus) / (2 * h 3))
    closeRelative "finite difference b" 2e-8 21 ((baseBPlus - baseBMinus) / (2 * h 4))

    uniform <- requireRight "uniform law" (uniformLaw 0 1)
    normal <- requireRight "normal law" (normalLaw 0 1)
    normalDensity <- requireRight "normal density" (densityAt normal 0)
    close "normal density at zero" 2e-15 (1 / sqrt (2 * pi)) normalDensity
    exponential <- requireRight "exponential law" (exponentialLaw 2)
    exponentialDensity <- requireRight "exponential density" (densityAt exponential 0)
    close "exponential density at zero" 1e-15 2 exponentialDensity
    let seed = generatorFromSeed 123456
        (knownWord1, knownNext1) = nextGeneratorWord seed
        (knownWord2, knownNext2) = nextGeneratorWord knownNext1
    check "SplitMix64 first known-answer word" (knownWord1 == 0x39e65b817d6592e9)
    check "SplitMix64 second known-answer word" (knownWord2 == 0x41f614e28788bc09)
    check "SplitMix64 first final-state vector" (generatorStateWords (generatorState knownNext1) == (0x9e3779b97f4c5e55, 0x9e3779b97f4a7c15))
    check "SplitMix64 second final-state vector" (generatorStateWords (generatorState knownNext2) == (0x3c6ef372fe96da6a, 0x9e3779b97f4a7c15))
    (sample1, next1) <- requireRight "sample" (sampleLaw uniform seed)
    (sample2, next2) <- requireRight "repeat sample" (sampleLaw uniform (generatorFromSeed 123456))
    check "same seed sample" (sample1 == sample2 && generatorState next1 == generatorState next2)
    let unitFrom word = fromIntegral (word `shiftR` 11) * (1 / 9007199254740992)
        openUnitFrom word = (fromIntegral (word `shiftR` 11) + 0.5) * (1 / 9007199254740992)
    check "uniform sample matches independent word conversion" (sample1 == unitFrom knownWord1)
    (normalSample, normalNext) <- requireRight "normal sample" (sampleLaw normal seed)
    let expectedNormal = sqrt (-(2 * log (openUnitFrom knownWord1))) * cos (2 * pi * unitFrom knownWord2)
    close "normal sample independent Box-Muller check" 1e-15 expectedNormal normalSample
    check "normal consumes exactly two words" (generatorState normalNext == generatorState knownNext2)
    (exponentialSample, exponentialNext) <- requireRight "exponential sample" (sampleLaw exponential seed)
    close "exponential sample independent inverse-CDF check" 1e-15 ((-log (openUnitFrom knownWord1)) / 2) exponentialSample
    check "exponential consumes exactly one word" (generatorState exponentialNext == generatorState knownNext1)

    extremeUniform <- requireRight "finite extreme uniform" (uniformLaw 8e307 1.6e308)
    (extremeSample, _) <- requireRight "stable extreme uniform sample" (sampleLaw extremeUniform seed)
    check "stable interpolation remains inside extreme interval" (extremeSample >= 8e307 && extremeSample <= 1.6e308 && not (isInfinite extremeSample))

    checkLeft "one-below MC budget" (monteCarloConfig 100 99) (== SampleBudgetExceeded 100 99)
    configOne <- requireRight "one-sample config" (monteCarloConfig 1 1)
    (reportOne, _) <- requireRight "one-sample run" (estimateExpectation configOne uniform Right seed)
    check "one sample has no variance" (isNothing (monteCarloSampleVariance reportOne) && isNothing (monteCarloStandardError reportOne))
    config100 <- requireRight "MC config" (monteCarloConfig 100 100)
    (report100, _) <- requireRight "MC run" (estimateExpectation config100 uniform (Right . (\x -> x * x)) seed)
    let samples = takeSamples 100 uniform seed
        referenceMean = sum (map (\x -> x * x) samples) / 100
        referenceVariance = sum [((x * x) - referenceMean) ^ (2 :: Int) | x <- samples] / 99
    close "MC independent mean" 1e-15 referenceMean (monteCarloEstimate report100)
    close "MC independent variance" 1e-14 referenceVariance (fromMaybe (0 / 0) (monteCarloSampleVariance report100))

    config50 <- requireRight "split config" (monteCarloConfig 50 50)
    (firstHalfReport, firstState) <- requireRight "first half" (resumeMonteCarlo config50 uniform Right (startMonteCarlo seed))
    (resumedReport, _) <- requireRight "second half" (resumeMonteCarlo config50 uniform Right firstState)
    configAll <- requireRight "all config" (monteCarloConfig 100 100)
    (oneShotReport, _) <- requireRight "one shot" (estimateExpectation configAll uniform Right seed)
    check "resumed samples accumulate" (monteCarloSamples firstHalfReport == 50 && resumedReport == oneShotReport)
    checkLeft "nonfinite observation atomic failure" (estimateExpectation config100 uniform (const (Right (0 / 0))) seed) isNonFiniteObservation
    let completeReportEvidence = unlines [show rounding] ++ unlines [show polynomial, show exactReport, show numericalSquare, show exactBivariateReport, show expReport, show reportOne, show report100, show resumedReport]
        reportChecksum = semanticChecksum completeReportEvidence
    unless (reportChecksum == 17256650109524261746) (failWith "continuous numerical complete-report semantic checksum" (show reportChecksum))
    putStrLn "continuous numerical tests passed"

semanticChecksum :: String -> Word64
semanticChecksum = foldl step 14695981039346656037
  where
    step checksum character = (checksum `xor` fromIntegral (ord character)) * 1099511628211

isCallbackFailure :: QuadratureError -> Bool
isCallbackFailure err = case err of IntegrandFailure{} -> True; _ -> False
isEvaluationBudget :: QuadratureError -> Bool
isEvaluationBudget err = case err of EvaluationBudgetExhausted{} -> True; _ -> False
isSubdivisionBudget :: QuadratureError -> Bool
isSubdivisionBudget err = case err of SubdivisionBudgetExhausted{} -> True; _ -> False
isNonFiniteObservation :: MonteCarloError -> Bool
isNonFiniteObservation err = case err of NonFiniteObservation{} -> True; _ -> False
isInvalidInterval :: NumericalValueError -> Bool
isInvalidInterval err = case err of InvalidDoubleInterval{} -> True; _ -> False
isSamplingValueError :: SamplingError -> Bool
isSamplingValueError err = case err of SamplingValueError InvalidDoubleInterval{} -> True; _ -> False
isAggregateFailure :: QuadratureError -> Bool
isAggregateFailure err = case err of NonFiniteAggregate{} -> True; _ -> False

takeSamples :: Int -> NumericalLaw -> Generator -> [Double]
takeSamples count law generator = go count generator []
  where
    go 0 _ samples = reverse samples
    go remaining current samples = case sampleLaw law current of
        Left _ -> []
        Right (sample, next) -> go (remaining - 1) next (sample : samples)

simpson :: Int -> (Double -> Double) -> Double -> Double -> Double
simpson n function lower upper =
    let width = (upper - lower) / fromIntegral n
        weighted index = (if odd index then 4 else 2) * function (lower + fromIntegral index * width)
     in width / 3 * (function lower + function upper + sum [weighted index | index <- [1 .. n - 1]])

simpson2D :: Int -> (Double -> Double -> Double) -> Double -> Double -> Double -> Double -> Double
simpson2D subdivisions function leftLower leftUpper rightLower rightUpper =
    simpson subdivisions (\left -> simpson subdivisions (function left) rightLower rightUpper) leftLower leftUpper

replaceAt :: Int -> value -> [value] -> [value]
replaceAt index replacement values =
    [ if current == index then replacement else value
    | (current, value) <- zip [0 ..] values
    ]

close :: String -> Double -> Double -> Double -> IO ()
close label tolerance expected actual = check label (abs (actual - expected) <= tolerance)

closeRelative :: String -> Double -> Double -> Double -> IO ()
closeRelative label tolerance expected actual = check label (abs (actual - expected) <= 2e-10 + tolerance * max (abs expected) (abs actual))

check :: String -> Bool -> IO ()
check label condition = unless condition $ putStrLn ("FAIL: " ++ label) >> exitFailure

checkLeft :: (Show value) => String -> Either err value -> (err -> Bool) -> IO ()
checkLeft label result predicate = case result of Left err -> check label (predicate err); Right value -> failWith label (show value)

requireRight :: (Show err) => String -> Either err value -> IO value
requireRight label result = case result of Left err -> failWith label (show err); Right value -> pure value

failWith :: String -> String -> IO value
failWith label detail = putStrLn ("FAIL: " ++ label ++ ": " ++ detail) >> exitFailure
