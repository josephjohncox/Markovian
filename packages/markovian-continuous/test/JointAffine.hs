module JointAffine (runJointAffineTests) where

import Control.Monad (unless)
import Data.List (foldl')
import Markovian.Continuous.Kernel.JointAffine.Exact
import Markovian.Continuous.Measure.Exact
import Markovian.Continuous.Polynomial
import Markovian.Continuous.Space (RealBorel)
import Numeric.Natural (Natural)
import Paths_markovian_continuous (getDataFileName)
import System.Exit (exitFailure)

data OwnerScope
data RenamedScope
data OtherScope

largeJointLimits :: ExactJointAffineLimits
largeJointLimits = exactJointAffineLimits 100 100 2 1000 10000 1024

exactLimits :: ExactLimits
exactLimits = ExactLimits 16 1000 100000 10000 100 100000 2048

runJointAffineTests :: IO ()
runJointAffineTests = do
    sharedAndIndependentMoments
    supportAndReports
    validationAndLocality
    precedenceTests
    exactBoundaries

fixture :: [(NoiseOwner OwnerScope, RationalInterval, Rational, Rational)] -> Either ExactJointAffineError (ExactJointAffineKernel OwnerScope RealBorel RealBorel RealBorel)
fixture =
    exactJointAffineKernel
        largeJointLimits
        (affineInputCoordinate 2 1)
        (affineInputCoordinate (-1) 3)

fixtureRows :: IO [(NoiseOwner OwnerScope, RationalInterval, Rational, Rational)]
fixtureRows = do
    symmetric <- requireRight "symmetric interval" (rationalInterval (-1) 1)
    wide <- requireRight "wide interval" (rationalInterval 0 2)
    unit <- requireRight "unit interval" (rationalInterval 0 1)
    pure
        [ (noiseOwner 1, symmetric, -3, 3)
        , (noiseOwner 2, wide, 2, -2)
        , (noiseOwner 3, unit, 0, 4)
        ]

sharedAndIndependentMoments :: IO ()
sharedAndIndependentMoments = do
    rows <- fixtureRows
    kernel <- requireRight "joint affine fixture" (fixture rows)
    (law, _) <- requireRight "joint affine materialization" (materializeJointAffineKernel largeJointLimits kernel 2)
    polynomial <- requireRight "complete joint polynomial" (rationalBivariatePolynomial exactLimits [(2, 3, 2), (1, 1, -5), (0, 0, 7)])
    report <- requireRight "shared-owner joint moment" (expectBivariatePolynomial exactLimits law polynomial)
    let rewardForm = (5, [(1, -3, (-1, 1)), (2, 2, (0, 2))])
        successorForm = (1, [(1, 3, (-1, 1)), (2, -2, (0, 2)), (3, 4, (0, 1))])
        oracle = oracleBivariate rewardForm successorForm [(2, 3, 2), (1, 1, -5), (0, 0, 7)]
    assert "shared-owner multinomial oracle changed" (exactIntegralValue report == oracle)

    unit <- requireRight "independent unit interval" (rationalInterval 0 1)
    independent <-
        requireRight
            "independent-coordinate kernel"
            ( exactJointAffineKernel
                largeJointLimits
                (affineInputCoordinate 0 1)
                (affineInputCoordinate 0 2)
                [ (noiseOwner 20 :: NoiseOwner OwnerScope, unit, 3, 0)
                , (noiseOwner 21 :: NoiseOwner OwnerScope, unit, 0, 4)
                ]
            )
    (independentLaw, _) <- requireRight "independent materialization" (materializeJointAffineKernel largeJointLimits independent 99)
    independentReport <- requireRight "independent moment" (expectBivariatePolynomial exactLimits independentLaw polynomial)
    let independentOracle = oracleBivariate (1, [(20, 3, (0, 1))]) (2, [(21, 4, (0, 1))]) [(2, 3, 2), (1, 1, -5), (0, 0, 7)]
    assert "distinct-owner multinomial oracle changed" (exactIntegralValue independentReport == independentOracle)

supportAndReports :: IO ()
supportAndReports = do
    rows <- fixtureRows
    kernel <- requireRight "support fixture" (fixture rows)
    inputInterval <- requireRight "compact input" (rationalInterval (-2) 3)
    ((rewardSupport, successorSupport), report) <- requireRight "joint affine extrema" (jointAffineSupportExtrema largeJointLimits kernel inputInterval)
    assert "reward extrema changed" (intervalBounds rewardSupport == (-6, 14))
    assert "successor extrema changed" (intervalBounds successorSupport == (-7, 12))
    let (rewardCorners, successorCorners) = enumerateCorners rows inputInterval
    assert "reward extrema disagree with corner enumeration" (intervalBounds rewardSupport == (minimum rewardCorners, maximum rewardCorners))
    assert "successor extrema disagree with corner enumeration" (intervalBounds successorSupport == (minimum successorCorners, maximum successorCorners))
    assert "support report work does not add up" (jointAffineWork report == jointAffinePreflightWork report + jointAffineArithmeticWork report)
    repeated <- requireRight "repeated extrema" (jointAffineSupportExtrema largeJointLimits kernel inputInterval)
    assert "joint affine report is nondeterministic" (snd repeated == report)
    goldenPath <- getDataFileName "test/golden/joint-affine-accounting.txt"
    golden <- readFile goldenPath
    assert "joint affine accounting golden changed" (reportText report == golden)

validationAndLocality :: IO ()
validationAndLocality = do
    rows <- fixtureRows
    kernel <- requireRight "validation fixture" (fixture rows)
    unit <- requireRight "validation unit interval" (rationalInterval 0 1)
    let hiddenDuplicate =
            [ (noiseOwner 50 :: NoiseOwner OwnerScope, unit, 0, 0)
            , (noiseOwner 50 :: NoiseOwner OwnerScope, unit, 0, 0)
            ]
    requireLeft "zero coefficients hid duplicate owner" (fixture hiddenDuplicate) (== JointAffineDuplicateOwner)

    reordered <- requireRight "reordered owner table" (fixture (reverse rows))
    (originalLaw, originalReport) <- requireRight "original reordered comparison" (materializeJointAffineKernel largeJointLimits kernel 2)
    (reorderedLaw, reorderedReport) <- requireRight "reordered materialization" (materializeJointAffineKernel largeJointLimits reordered 2)
    polynomial <- requireRight "reordered polynomial" (rationalBivariatePolynomial exactLimits [(2, 2, 1), (1, 1, 3)])
    originalMoment <- requireRight "original reordered moment" (expectBivariatePolynomial exactLimits originalLaw polynomial)
    reorderedMoment <- requireRight "reordered moment" (expectBivariatePolynomial exactLimits reorderedLaw polynomial)
    assert "canonical owner order changed semantics" (exactIntegralValue originalMoment == exactIntegralValue reorderedMoment)
    assert "canonical owner order changed report" (originalReport == reorderedReport)

    let sameScopeRenaming =
            [ (noiseOwner 1 :: NoiseOwner OwnerScope, noiseOwner 11 :: NoiseOwner OwnerScope)
            , (noiseOwner 2, noiseOwner 12)
            , (noiseOwner 3, noiseOwner 13)
            ]
    alphaRenamed <- requireRight "same-scope alpha renaming" (alphaRenameJointAffineKernel largeJointLimits sameScopeRenaming kernel)
    (alphaLaw, _) <- requireRight "alpha materialization" (materializeJointAffineKernel largeJointLimits alphaRenamed 2)
    alphaMoment <- requireRight "alpha moment" (expectBivariatePolynomial exactLimits alphaLaw polynomial)
    assert "same-scope alpha renaming changed joint moments" (exactIntegralValue originalMoment == exactIntegralValue alphaMoment)
    unchangedScope <- requireRight "empty same-scope alpha renaming" (alphaRenameJointAffineKernel largeJointLimits [] kernel)
    assert "empty alpha renaming changed owner scope report" (jointAffineOperation (jointAffineKernelReport unchangedScope) == JointAffineAlphaRenaming)

    let alphaReport = jointAffineKernelReport alphaRenamed
        alphaWork = jointAffineWork alphaReport
        exactAlphaLimits = exactJointAffineLimits 3 3 2 10 alphaWork 1024
    _ <- requireRight "exact alpha-renaming work" (alphaRenameJointAffineKernel exactAlphaLimits sameScopeRenaming kernel)
    requireLeft
        "one-below alpha-renaming work"
        (alphaRenameJointAffineKernel (exactJointAffineLimits 3 3 2 10 (alphaWork - 1) 1024) sameScopeRenaming kernel)
        (== JointAffineLimitExceeded JointAffineWork (alphaWork - 1) alphaWork)
    alphaGoldenPath <- getDataFileName "test/golden/joint-affine-renaming-accounting.txt"
    alphaGolden <- readFile alphaGoldenPath
    assert "alpha-renaming accounting golden changed" (reportText alphaReport == alphaGolden)

    let fullRenaming =
            [ (noiseOwner 1 :: NoiseOwner OwnerScope, noiseOwner 101 :: NoiseOwner RenamedScope)
            , (noiseOwner 2, noiseOwner 102)
            , (noiseOwner 3, noiseOwner 103)
            ]
    renamed <- requireRight "complete scope renaming" (reScopeJointAffineKernel largeJointLimits fullRenaming kernel)
    assert "scope-renaming report operation changed" (jointAffineOperation (jointAffineKernelReport renamed) == JointAffineScopeRenaming)
    (renamedLaw, _) <- requireRight "re-scoped materialization" (materializeJointAffineKernel largeJointLimits renamed 2)
    renamedMoment <- requireRight "re-scoped moment" (expectBivariatePolynomial exactLimits renamedLaw polynomial)
    assert "complete scope renaming changed joint moments" (exactIntegralValue originalMoment == exactIntegralValue renamedMoment)
    let scopeWork = jointAffineWork (jointAffineKernelReport renamed)
    _ <- requireRight "exact scope-renaming work" (reScopeJointAffineKernel (exactJointAffineLimits 3 3 2 10 scopeWork 1024) fullRenaming kernel)
    requireLeft
        "one-below scope-renaming work"
        (reScopeJointAffineKernel (exactJointAffineLimits 3 3 2 10 (scopeWork - 1) 1024) fullRenaming kernel)
        (== JointAffineLimitExceeded JointAffineWork (scopeWork - 1) scopeWork)
    requireLeft "empty mapping cannot change a nonempty scope" (reScopeJointAffineKernel largeJointLimits [] kernel) (== JointAffineIncompleteRenaming)
    requireLeft
        "omitted retained owner cannot change scope"
        (reScopeJointAffineKernel largeJointLimits (take 2 fullRenaming) kernel)
        (== JointAffineIncompleteRenaming)
    requireLeft
        "duplicate alpha source"
        (alphaRenameJointAffineKernel largeJointLimits [(noiseOwner 1 :: NoiseOwner OwnerScope, noiseOwner 11), (noiseOwner 1, noiseOwner 12)] kernel)
        (== JointAffineDuplicateRenamingSource)
    requireLeft
        "noninjective alpha target"
        (alphaRenameJointAffineKernel largeJointLimits [(noiseOwner 1 :: NoiseOwner OwnerScope, noiseOwner 11), (noiseOwner 2, noiseOwner 11)] kernel)
        (== JointAffineNonInjectiveRenaming)
    requireLeft
        "alpha target collided with unmapped owner"
        (alphaRenameJointAffineKernel largeJointLimits [(noiseOwner 1 :: NoiseOwner OwnerScope, noiseOwner 2)] kernel)
        (== JointAffineNonInjectiveRenaming)
    requireLeft
        "alpha source escaped local owner table"
        (alphaRenameJointAffineKernel largeJointLimits [(noiseOwner 999 :: NoiseOwner OwnerScope, noiseOwner 1000)] kernel)
        (== JointAffineUnknownRenamingSource)

    -- Equal numeric names in different nominal scopes are admitted separately;
    -- there is no operation that treats them as cross-kernel sharing evidence.
    other <-
        requireRight
            "owner-local second kernel"
            ( exactJointAffineKernel
                largeJointLimits
                (affineInputCoordinate 1 0)
                (affineInputCoordinate 1 0)
                [(noiseOwner 1 :: NoiseOwner OtherScope, unit, 1, -1)]
            )
    assert "owner-local scopes changed counts" (jointAffineRetainedOwnerCount (jointAffineKernelReport other) == 1)

precedenceTests :: IO ()
precedenceTests = do
    degenerateLaw <- requireRight "precedence degenerate law" (diracReal exactLimits 16)
    let degenerateInterval = lawSupport degenerateLaw
        duplicateRows =
            [ (noiseOwner 7 :: NoiseOwner OwnerScope, degenerateInterval, 1024, 0)
            , (noiseOwner 7 :: NoiseOwner OwnerScope, degenerateInterval, 0, 0)
            ]
        duplicateWith limits = exactJointAffineKernel limits (affineInputCoordinate 1 0) (affineInputCoordinate 1 0) duplicateRows
    requireLeft "construction raw limit precedes duplicate" (duplicateWith (exactJointAffineLimits 1 2 2 8 100 100)) (== JointAffineLimitExceeded JointAffineRawEntries 1 2)
    requireLeft "construction owner limit precedes duplicate" (duplicateWith (exactJointAffineLimits 2 1 2 8 100 100)) (== JointAffineLimitExceeded JointAffineOwners 1 2)
    requireLeft "construction output limit precedes duplicate" (duplicateWith (exactJointAffineLimits 2 2 1 8 100 100)) (== JointAffineLimitExceeded JointAffineOutputs 1 2)
    requireLeft "construction coefficient limit precedes duplicate" (duplicateWith (exactJointAffineLimits 2 2 2 7 100 100)) (== JointAffineLimitExceeded JointAffineCoefficients 7 8)
    requireLeft "construction work precedes duplicate" (duplicateWith (exactJointAffineLimits 2 2 2 8 5 100)) (== JointAffineLimitExceeded JointAffineWork 5 6)
    requireLeft "construction duplicate precedes interval and rational bits" (duplicateWith (exactJointAffineLimits 2 2 2 8 6 1)) (== JointAffineDuplicateOwner)
    requireLeft
        "construction interval validity precedes rational bits"
        ( exactJointAffineKernel
            (exactJointAffineLimits 1 1 2 6 2 1)
            (affineInputCoordinate 1 0)
            (affineInputCoordinate 1 0)
            [(noiseOwner 8 :: NoiseOwner OwnerScope, degenerateInterval, 1024, 0)]
        )
        (== JointAffineInvalidInterval 16 16)

    rows <- fixtureRows
    kernel <- requireRight "precedence kernel" (fixture rows)
    let duplicateMapping =
            [ (noiseOwner 1 :: NoiseOwner OwnerScope, noiseOwner 11)
            , (noiseOwner 1, noiseOwner 12)
            ]
        duplicatePlanWork = 52
    requireLeft
        "stored limits precede rename spine"
        (alphaRenameJointAffineKernel (exactJointAffineLimits 2 3 2 10 100 1024) (repeat (noiseOwner 1, noiseOwner 11)) kernel)
        (== JointAffineLimitExceeded JointAffineRawEntries 2 3)
    requireLeft
        "rename spine precedes semantic scans"
        (alphaRenameJointAffineKernel (exactJointAffineLimits 3 3 2 10 100 1024) (replicate 4 (noiseOwner 1, noiseOwner 11)) kernel)
        (== JointAffineLimitExceeded JointAffineRawEntries 3 4)
    requireLeft
        "rename work precedes duplicate source"
        (alphaRenameJointAffineKernel (exactJointAffineLimits 3 3 2 10 (duplicatePlanWork - 1) 1024) duplicateMapping kernel)
        (== JointAffineLimitExceeded JointAffineWork (duplicatePlanWork - 1) duplicatePlanWork)
    requireLeft
        "duplicate source precedes duplicate target"
        (alphaRenameJointAffineKernel largeJointLimits [(noiseOwner 1, noiseOwner 11), (noiseOwner 1, noiseOwner 11)] kernel)
        (== JointAffineDuplicateRenamingSource)
    requireLeft
        "unknown source precedes final collision"
        (alphaRenameJointAffineKernel largeJointLimits [(noiseOwner 999, noiseOwner 2)] kernel)
        (== JointAffineUnknownRenamingSource)
    requireLeft
        "scope completeness precedes final collision"
        (reScopeJointAffineKernel largeJointLimits [(noiseOwner 1 :: NoiseOwner OwnerScope, noiseOwner 3 :: NoiseOwner RenamedScope), (noiseOwner 2, noiseOwner 4)] kernel)
        (== JointAffineIncompleteRenaming)

    let hugeInput = 16
    requireLeft
        "material stored limits precede input bits"
        (materializeJointAffineKernel (exactJointAffineLimits 2 3 2 10 100 3) kernel hugeInput)
        (== JointAffineLimitExceeded JointAffineRawEntries 2 3)
    requireLeft
        "material work precedes input bits"
        (materializeJointAffineKernel (exactJointAffineLimits 3 3 2 10 39 3) kernel hugeInput)
        (== JointAffineLimitExceeded JointAffineWork 39 40)
    requireLeft
        "material input bits follow work"
        (materializeJointAffineKernel (exactJointAffineLimits 3 3 2 10 40 3) kernel hugeInput)
        (== JointAffineLimitExceeded JointAffineRationalBits 3 5)

    hugeInterval <- requireRight "precedence large interval" (rationalInterval 0 16)
    requireLeft
        "support stored limits precede malformed interval"
        (jointAffineSupportExtrema (exactJointAffineLimits 2 3 2 10 100 3) kernel degenerateInterval)
        (== JointAffineLimitExceeded JointAffineRawEntries 2 3)
    requireLeft
        "support work precedes malformed interval"
        (jointAffineSupportExtrema (exactJointAffineLimits 3 3 2 10 43 3) kernel degenerateInterval)
        (== JointAffineLimitExceeded JointAffineWork 43 44)
    requireLeft
        "support malformed interval precedes endpoint bits"
        (jointAffineSupportExtrema (exactJointAffineLimits 3 3 2 10 44 3) kernel degenerateInterval)
        (== JointAffineInvalidInterval 16 16)
    requireLeft
        "support interval bits follow work"
        (jointAffineSupportExtrema (exactJointAffineLimits 3 3 2 10 44 3) kernel hugeInterval)
        (== JointAffineLimitExceeded JointAffineRationalBits 3 5)

exactBoundaries :: IO ()
exactBoundaries = do
    rows <- fixtureRows
    kernel <- requireRight "boundary fixture" (fixture rows)
    let construction = jointAffineKernelReport kernel
        exactFor report =
            exactJointAffineLimits
                (jointAffineRawEntryCount report)
                (jointAffineDeclaredOwnerCount report)
                (jointAffineOutputCount report)
                (jointAffineCoefficientCount report)
                (jointAffineWork report)
                (jointAffineMaximumRationalBits report)
    _ <- requireRight "exact construction limits" (fixtureWith (exactFor construction) rows)
    requireLeft "one-below raw entry limit" (fixtureWith (exactJointAffineLimits 2 3 2 10 12 1024) rows) (== JointAffineLimitExceeded JointAffineRawEntries 2 3)
    requireLeft "one-below owner limit" (fixtureWith (exactJointAffineLimits 3 2 2 10 12 1024) rows) (== JointAffineLimitExceeded JointAffineOwners 2 3)
    requireLeft "one-below output limit" (fixtureWith (exactJointAffineLimits 3 3 1 10 12 1024) rows) (== JointAffineLimitExceeded JointAffineOutputs 1 2)
    requireLeft "one-below coefficient limit" (fixtureWith (exactJointAffineLimits 3 3 2 9 12 1024) rows) (== JointAffineLimitExceeded JointAffineCoefficients 9 10)
    requireLeft "one-below construction work" (fixtureWith (exactJointAffineLimits 3 3 2 10 11 1024) rows) (== JointAffineLimitExceeded JointAffineWork 11 12)
    let constructionBits = jointAffineMaximumRationalBits construction
    requireLeft
        "one-below construction rational bits"
        (fixtureWith (exactJointAffineLimits 3 3 2 10 12 (constructionBits - 1)) rows)
        (== JointAffineLimitExceeded JointAffineRationalBits (constructionBits - 1) constructionBits)

    (_, materialReport) <- requireRight "material boundary report" (materializeJointAffineKernel largeJointLimits kernel 2)
    _ <- requireRight "exact material work" (materializeJointAffineKernel (exactFor materialReport) kernel 2)
    requireLeft
        "one-below material work"
        (materializeJointAffineKernel (replaceWork (exactFor materialReport) (jointAffineWork materialReport - 1)) kernel 2)
        (== JointAffineLimitExceeded JointAffineWork (jointAffineWork materialReport - 1) (jointAffineWork materialReport))
    let materialBits = jointAffineMaximumRationalBits materialReport
    requireLeft
        "one-below material rational bits"
        (materializeJointAffineKernel (replaceBits (exactFor materialReport) (materialBits - 1)) kernel 2)
        (== JointAffineLimitExceeded JointAffineRationalBits (materialBits - 1) materialBits)

    inputInterval <- requireRight "boundary input interval" (rationalInterval (-2) 3)
    (_, supportReport) <- requireRight "support boundary report" (jointAffineSupportExtrema largeJointLimits kernel inputInterval)
    _ <- requireRight "exact support work" (jointAffineSupportExtrema (exactFor supportReport) kernel inputInterval)
    requireLeft
        "one-below support work"
        (jointAffineSupportExtrema (replaceWork (exactFor supportReport) (jointAffineWork supportReport - 1)) kernel inputInterval)
        (== JointAffineLimitExceeded JointAffineWork (jointAffineWork supportReport - 1) (jointAffineWork supportReport))
    let supportBits = jointAffineMaximumRationalBits supportReport
    requireLeft
        "one-below support rational bits"
        (jointAffineSupportExtrema (replaceBits (exactFor supportReport) (supportBits - 1)) kernel inputInterval)
        (== JointAffineLimitExceeded JointAffineRationalBits (supportBits - 1) supportBits)

    requireLeft
        "infinite owner table is bounded"
        (fixtureWith (exactJointAffineLimits 3 3 2 10 100 1024) (repeat (noiseOwner 90, rightValue (rationalInterval 0 1), 0, 0)))
        (== JointAffineLimitExceeded JointAffineRawEntries 3 4)
    let beyondMachine = fromIntegral (maxBound :: Int) + 1
    requireLeft
        "machine-sized limits overflow"
        (fixtureWith (exactJointAffineLimits beyondMachine 3 2 10 100 1024) rows)
        (== JointAffineMachineIndexExceeded JointAffineRawEntries beyondMachine)

fixtureWith :: ExactJointAffineLimits -> [(NoiseOwner OwnerScope, RationalInterval, Rational, Rational)] -> Either ExactJointAffineError (ExactJointAffineKernel OwnerScope RealBorel RealBorel RealBorel)
fixtureWith limits = exactJointAffineKernel limits (affineInputCoordinate 2 1) (affineInputCoordinate (-1) 3)

enumerateCorners :: [(NoiseOwner owner, RationalInterval, Rational, Rational)] -> RationalInterval -> ([Rational], [Rational])
enumerateCorners rows inputInterval =
    unzip
        [ ( 2 * input + 1 + sum (zipWith (*) rewardValues noises)
          , (-1) * input + 3 + sum (zipWith (*) successorValues noises)
          )
        | input <- endpoints inputInterval
        , noises <- sequence [endpoints interval | (_, interval, _, _) <- rows]
        ]
  where
    rewardValues = [coefficient | (_, _, coefficient, _) <- rows]
    successorValues = [coefficient | (_, _, _, coefficient) <- rows]
    endpoints interval = let (lower, upper) = intervalBounds interval in [lower, upper]

-- Independent multinomial expansion.  It does not call the package's symbolic
-- expansion, binomial, or canonicalization implementation.
type OracleForm = (Rational, [(Natural, Rational, (Rational, Rational))])
type OracleTerm = ([(Natural, Natural)], Rational)

oracleBivariate :: OracleForm -> OracleForm -> [(Natural, Natural, Rational)] -> Rational
oracleBivariate left right polynomial =
    sum
        [ polynomialCoefficient * leftCoefficient * rightCoefficient * ownerMoments (mergePowers leftPowers rightPowers)
        | (leftPower, rightPower, polynomialCoefficient) <- polynomial
        , (leftPowers, leftCoefficient) <- multinomialPower left leftPower
        , (rightPowers, rightCoefficient) <- multinomialPower right rightPower
        ]
  where
    intervals = [(owner, bounds) | (owner, _, bounds) <- snd left ++ snd right]
    ownerMoments powers = product [uniformMoment (lookupBounds owner intervals) degree | (owner, degree) <- powers]

multinomialPower :: OracleForm -> Natural -> [OracleTerm]
multinomialPower (constant, noises) degree =
    [ ([(owner, powerValue) | ((owner, _, _), powerValue) <- zip noises noiseExponents, powerValue /= 0], coefficient exponents)
    | exponents@(_ : noiseExponents) <- weakCompositions (length noises + 1) degree
    ]
  where
    coefficient [] = 0
    coefficient exponents@(constantPower : noisePowers) =
        fromIntegral (factorial degree `div` product (map factorial exponents))
            * constant ^ constantPower
            * product [value ^ powerValue | ((_, value, _), powerValue) <- zip noises noisePowers]

weakCompositions :: Int -> Natural -> [[Natural]]
weakCompositions 1 total = [[total]]
weakCompositions slots total = [first : rest | first <- [0 .. total], rest <- weakCompositions (slots - 1) (total - first)]

factorial :: Natural -> Natural
factorial n = product [1 .. n]

mergePowers :: [(Natural, Natural)] -> [(Natural, Natural)] -> [(Natural, Natural)]
mergePowers = foldl' (flip insertPower)
  where
    insertPower pair [] = [pair]
    insertPower pair@(owner, power) allPowers@((other, otherPower) : rest)
        | owner < other = pair : allPowers
        | owner == other = (owner, power + otherPower) : rest
        | otherwise = (other, otherPower) : insertPower pair rest

lookupBounds :: Natural -> [(Natural, (Rational, Rational))] -> (Rational, Rational)
lookupBounds owner intervals = case lookup owner intervals of
    Just bounds -> bounds
    Nothing -> error "oracle owner interval missing"

uniformMoment :: (Rational, Rational) -> Natural -> Rational
uniformMoment _ 0 = 1
uniformMoment (lower, upper) degree =
    (upper ^ (degree + 1) - lower ^ (degree + 1))
        / (fromIntegral (degree + 1) * (upper - lower))

reportText :: ExactJointAffineReport -> String
reportText report =
    unlines
        [ "operation\t" ++ show (jointAffineOperation report)
        , "raw-entries\t" ++ show (jointAffineRawEntryCount report)
        , "declared-owners\t" ++ show (jointAffineDeclaredOwnerCount report)
        , "retained-owners\t" ++ show (jointAffineRetainedOwnerCount report)
        , "outputs\t" ++ show (jointAffineOutputCount report)
        , "coefficients\t" ++ show (jointAffineCoefficientCount report)
        , "preflight-work\t" ++ show (jointAffinePreflightWork report)
        , "arithmetic-work\t" ++ show (jointAffineArithmeticWork report)
        , "work\t" ++ show (jointAffineWork report)
        , "maximum-rational-bits\t" ++ show (jointAffineMaximumRationalBits report)
        ]

replaceWork :: ExactJointAffineLimits -> Natural -> ExactJointAffineLimits
replaceWork _ work =
    -- The test creates these limits from a report, whose first four limits are
    -- fixed by this three-row fixture.
    exactJointAffineLimits 3 3 2 10 work 1024

replaceBits :: ExactJointAffineLimits -> Natural -> ExactJointAffineLimits
replaceBits _ = exactJointAffineLimits 3 3 2 10 10000

rightValue :: Either error value -> value
rightValue result = case result of
    Left _ -> error "fixture construction failed"
    Right value -> value

assert :: String -> Bool -> IO ()
assert label condition = unless condition $ do
    putStrLn ("FAIL: " ++ label)
    exitFailure

requireRight :: (Show error) => String -> Either error value -> IO value
requireRight label result = case result of
    Left err -> failWith label (show err)
    Right value -> pure value

requireLeft :: (Show error) => String -> Either error value -> (error -> Bool) -> IO ()
requireLeft label result predicate = case result of
    Left err -> assert (label ++ ": " ++ show err) (predicate err)
    Right _ -> failWith label "unexpected Right"

failWith :: String -> String -> IO value
failWith label detail = do
    putStrLn ("FAIL: " ++ label ++ ": " ++ detail)
    exitFailure
