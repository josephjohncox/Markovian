module JointAffine (runJointAffineTests) where

import Control.Monad (forM_, unless)
import Data.List (permutations)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
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
    declarationCompleteRenamers
    successorSharingAndFreshness
    successorMappingFailures
    successorExactBoundaries
    successorHistoryProjections
    successorSignedOracles
    successorPermutationAndIdentity
    putStrLn "PASS: joint affine Gate A and D079 substitution, declarations, independent oracles, ledger and projection history"

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
    assert "unchanged no-zero scope work 78" (scopeWork == 78)
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
    -- substitution requires explicit complete requests, never numbers alone.
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

-- D079 Gate B fixtures ----------------------------------------------------------

type TestKernel owner = ExactJointAffineKernel owner RealBorel RealBorel RealBorel
type TestSubstitution = ExactSuccessorSubstitution RealBorel RealBorel RealBorel

subLimits :: ExactSuccessorSubstitutionLimits
subLimits = exactSuccessorSubstitutionLimits 100 100 2 1000 100000 1024

kernelWith :: Rational -> Rational -> Rational -> Rational -> [(NoiseOwner owner, RationalInterval, Rational, Rational)] -> IO (TestKernel owner)
kernelWith r d s f = requireRight "substitution operand" . exactJointAffineKernel largeJointLimits (affineInputCoordinate r d) (affineInputCoordinate s f)

unitInterval :: RationalInterval
unitInterval = rightValue (rationalInterval 0 1)

substitutionPair :: IO (TestKernel OwnerScope, TestKernel OtherScope)
substitutionPair = do
    left <- kernelWith 0 2 0 0 [(noiseOwner 1, unitInterval, 0, 1)]
    right <- kernelWith 1 0 0 0 [(noiseOwner 2, unitInterval, 0, 1)]
    pure (left, right)

sharedRequest :: ExactSuccessorOwnerRequest OwnerScope OtherScope
sharedRequest = sharedSuccessorOwners [(noiseOwner 2, noiseOwner 1)]

freshRequest :: ExactSuccessorOwnerRequest OwnerScope OtherScope
freshRequest = freshSuccessorOwners [(noiseOwner 2, 3)]

subReportValues :: ExactSuccessorSubstitutionReport -> [Natural]
subReportValues report =
    map
        ($ report)
        [ substitutionLeftDeclaredOwners
        , substitutionRightDeclaredOwners
        , substitutionMappingEntries
        , substitutionRawEntries
        , substitutionOwnerReservationSlots
        , substitutionReservedNames
        , substitutionSharedOwners
        , substitutionFreshOwners
        , substitutionDeclaredResultOwners
        , substitutionRetainedResultOwners
        , substitutionZeroFilteredResultOwners
        , substitutionOutputs
        , substitutionCoefficientSlots
        , substitutionPreflightWork
        , substitutionCoefficientMultiplications
        , substitutionCoefficientAdditions
        , substitutionArithmeticWork
        , substitutionWork
        , substitutionMaximumRationalBits
        ]

momentOf :: TestSubstitution -> Rational -> [(Natural, Natural, Rational)] -> IO Rational
momentOf result input terms = do
    (law, _) <- requireRight "substitution projection" (materializeSuccessorSubstitution largeJointLimits result input)
    polynomial <- requireRight "substitution polynomial" (rationalBivariatePolynomial exactLimits terms)
    exactIntegralValue <$> requireRight "substitution moment" (expectBivariatePolynomial exactLimits law polynomial)

declarationCompleteRenamers :: IO ()
declarationCompleteRenamers = do
    kernel <- kernelWith 0 0 0 0 [(noiseOwner 1 :: NoiseOwner OwnerScope, unitInterval, 1, 0), (noiseOwner 2, unitInterval, 0, 0)]
    -- n=2,m=1: B=6; alpha=6+1+2+4+8=21; scope=23.
    let alpha cap mapping = alphaRenameJointAffineKernel (exactJointAffineLimits 2 2 2 8 cap 2) mapping kernel
    renamed <- requireRight "declared zero alpha membership exact work 21" (alpha 21 [(noiseOwner 2, noiseOwner 20)])
    assert "declared zero alpha work" (jointAffineWork (jointAffineKernelReport renamed) == 21)
    requireLeft "declared zero alpha one below" (alpha 20 [(noiseOwner 2, noiseOwner 20)]) (== JointAffineLimitExceeded JointAffineWork 20 21)
    requireLeft "unmapped zero collision" (alpha 21 [(noiseOwner 1, noiseOwner 2)]) (== JointAffineNonInjectiveRenaming)
    requireLeft "unknown before zero collision" (alpha 21 [(noiseOwner 99, noiseOwner 2)]) (== JointAffineUnknownRenamingSource)
    requireLeft "complete map cannot omit zero" (reScopeJointAffineKernel largeJointLimits [(noiseOwner 1, noiseOwner 10 :: NoiseOwner RenamedScope)] kernel) (== JointAffineIncompleteRenaming)
    onlyZero <- kernelWith 0 0 0 0 [(noiseOwner 1 :: NoiseOwner OwnerScope, unitInterval, 0, 0)]
    requireLeft "empty rescope cannot erase declaration-only namespace" (reScopeJointAffineKernel largeJointLimits [] onlyZero) (== JointAffineIncompleteRenaming)
    -- n=m=2: alpha=32, scope=36. The baseline retained-only plans
    -- would reserve alpha=22 and scope=24 but reject the zero-row source.
    _ <- requireRight "declared alpha two-row exact work 32" (alpha 32 [(noiseOwner 1, noiseOwner 10), (noiseOwner 2, noiseOwner 20)])
    requireLeft "declared alpha two-row one below" (alpha 31 [(noiseOwner 1, noiseOwner 10), (noiseOwner 2, noiseOwner 20)]) (== JointAffineLimitExceeded JointAffineWork 31 32)
    let mapping = [(noiseOwner 1, noiseOwner 10 :: NoiseOwner RenamedScope), (noiseOwner 2, noiseOwner 20)]
    scoped <- requireRight "declared complete scope exact work 36" (reScopeJointAffineKernel (exactJointAffineLimits 2 2 2 8 36 2) mapping kernel)
    assert "declared complete scope work" (jointAffineWork (jointAffineKernelReport scoped) == 36)
    requireLeft "declared scope one below" (reScopeJointAffineKernel (exactJointAffineLimits 2 2 2 8 35 2) mapping kernel) (== JointAffineLimitExceeded JointAffineWork 35 36)
    right <- kernelWith 0 0 0 0 [(noiseOwner 3 :: NoiseOwner OtherScope, unitInterval, 0, 0)]
    requireLeft "renamed zero remains collision evidence" (substituteLeftSuccessor subLimits renamed right (freshSuccessorOwners [(noiseOwner 3, 20)])) (== SuccessorSubstitutionFreshTargetCollision 1 SuccessorSubstitutionLeft 2)
    _ <- requireRight "shared renamed zero remains law evidence" (substituteLeftSuccessor subLimits renamed right (sharedSuccessorOwners [(noiseOwner 3, noiseOwner 20)]))
    -- Construction still observes rows in caller order, not canonical order.
    bad <- requireRight "degenerate construction interval" (diracReal exactLimits 16)
    requireLeft
        "constructor caller order rational before later invalid interval"
        ( exactJointAffineKernel
            (exactJointAffineLimits 2 2 2 8 6 2)
            (affineInputCoordinate 0 0)
            (affineInputCoordinate 0 0)
            [(noiseOwner 20 :: NoiseOwner OwnerScope, unitInterval, 16, 0), (noiseOwner 1, lawSupport bad, 0, 0)]
        )
        (== JointAffineLimitExceeded JointAffineRationalBits 2 5)
    requireLeft
        "constructor caller order invalid interval before later rational"
        ( exactJointAffineKernel
            (exactJointAffineLimits 2 2 2 8 6 2)
            (affineInputCoordinate 0 0)
            (affineInputCoordinate 0 0)
            [(noiseOwner 1 :: NoiseOwner OwnerScope, lawSupport bad, 0, 0), (noiseOwner 20, unitInterval, 16, 0)]
        )
        (== JointAffineInvalidInterval 16 16)

successorSharingAndFreshness :: IO ()
successorSharingAndFreshness = do
    (left, right) <- substitutionPair
    forM_ [(sharedRequest, SharedRightOwners, 1 % 3, [2, 1, 0, 1, 1, 0]), (freshRequest, FreshRightOwners, 1 % 4, [3, 0, 1, 2, 2, 0])] $ \(request, mode, expectedMoment, names) -> do
        result <- requireRight "exact 77-work substitution" (substituteLeftSuccessor (exactSuccessorSubstitutionLimits 3 3 2 20 77 2) left right request)
        let report = successorSubstitutionReport result
        assert "frozen substitution mode" (substitutionMode report == mode)
        assert "frozen substitution report" (subReportValues report == [1, 1, 1, 3, 3] ++ names ++ [2, 20, 65, 6, 6, 12, 77, 2])
        reward <- momentOf result 0 [(1, 0, 1)]
        successor <- momentOf result 0 [(0, 1, 1)]
        joint <- momentOf result 0 [(1, 1, 1)]
        assert "lost left reward must not accumulate to 5/2" (reward == 1 % 2 && successor == 1 % 2)
        assert "shared 1/3 versus independent 1/4" (joint == expectedMoment)
        (_, materialReport) <- requireRight "basic materialization" (materializeSuccessorSubstitution largeJointLimits result 0)
        (_, extremaReport) <- requireRight "basic extrema" (successorSubstitutionSupportExtrema largeJointLimits result unitInterval)
        let d = if mode == SharedRightOwners then 1 else 2
        assert "reservation aliases do not add projection rows" (jointAffineRawEntryCount materialReport == d && jointAffineCoefficientCount materialReport == 4 + 2 * d)
        assert "projection declared-count work" (jointAffineWork materialReport == d + d * d + 4 + 8 * d && jointAffineWork extremaReport == d + d * d + 8 * (d + 1))
    rightProjection <- kernelWith 0 0 1 0 []
    projected <- requireRight "substitution into (0,y)" (substituteLeftSuccessor subLimits left rightProjection (sharedSuccessorOwners []))
    r <- momentOf projected 0 [(1, 0, 1)]
    s <- momentOf projected 0 [(0, 1, 1)]
    assert "(0,y) is not reward-preserving joint identity" (r == 0 && s == 1 % 2)

successorMappingFailures :: IO ()
successorMappingFailures = do
    forM_ [False, True] $ \zeros -> do
        let q = if zeros then 0 else 1
        left <- kernelWith 0 0 0 0 [(noiseOwner 1 :: NoiseOwner OwnerScope, unitInterval, 0, q), (noiseOwner 4, unitInterval, 0, 0)]
        right <- kernelWith 1 0 0 0 [(noiseOwner 2 :: NoiseOwner OtherScope, unitInterval, 0, q), (noiseOwner 5, unitInterval, 0, 0)]
        let run = substituteLeftSuccessor subLimits left right
            fresh pairs = freshSuccessorOwners [(noiseOwner source, target) | (source, target) <- pairs]
            shared pairs = sharedSuccessorOwners [(noiseOwner source, noiseOwner target) | (source, target) <- pairs]
        requireLeft "missing right including zero" (run (fresh [(2, 10)])) (== SuccessorSubstitutionIncompleteMapping 2)
        requireLeft "source duplicate precedes target and lexicographic pair" (run (fresh [(5, 10), (2, 10), (5, 11), (2, 11)])) (== SuccessorSubstitutionDuplicateSource 1 3)
        requireLeft "noninjective target precedes unknown source" (run (fresh [(99, 10), (2, 10)])) (== SuccessorSubstitutionNonInjectiveTarget 1 2)
        requireLeft "unknown source precedes incomplete and collision" (run (fresh [(99, 1)])) (== SuccessorSubstitutionUnknownSource 1)
        requireLeft "incomplete before unknown shared target" (run (shared [(2, 99)])) (== SuccessorSubstitutionIncompleteMapping 2)
        requireLeft "unknown shared target mapping order" (run (shared [(5, 99), (2, 98)])) (== SuccessorSubstitutionUnknownSharedTarget 1)
        forM_ [(2, SuccessorSubstitutionRight, 1), (5, SuccessorSubstitutionRight, 2), (1, SuccessorSubstitutionLeft, 1), (4, SuccessorSubstitutionLeft, 2)] $ \(target, participant, index) ->
            requireLeft "fresh collision both complete manifests" (run (fresh [(2, target), (5, 10)])) (== SuccessorSubstitutionFreshTargetCollision 1 participant index)
        requireLeft "mapping-first earlier right beats later left" (run (fresh [(2, 5), (5, 1)])) (== SuccessorSubstitutionFreshTargetCollision 1 SuccessorSubstitutionRight 2)
        _ <- requireRight "shared zero target is permitted" (run (shared [(2, 4), (5, 1)]))
        pure ()
    left <- kernelWith 0 0 0 0 [(noiseOwner 1 :: NoiseOwner OwnerScope, unitInterval, 0, 0)]
    wide <- requireRight "wide mismatch interval" (rationalInterval 0 2)
    right <- kernelWith 0 0 0 0 [(noiseOwner 1 :: NoiseOwner OtherScope, wide, 0, 0)]
    requireLeft "distinct scopes with equal names are not fresh" (substituteLeftSuccessor subLimits left right (freshSuccessorOwners [(noiseOwner 1, 1)])) (== SuccessorSubstitutionFreshTargetCollision 1 SuccessorSubstitutionLeft 1)
    requireLeft "zero shared interval mismatch" (substituteLeftSuccessor subLimits left right (sharedSuccessorOwners [(noiseOwner 1, noiseOwner 1)])) (== SuccessorSubstitutionSharedIntervalMismatch 1 unitInterval wide)
    _ <- requireRight "equal old names differing laws freshen" (substituteLeftSuccessor subLimits left right (freshSuccessorOwners [(noiseOwner 1, 2)]))
    half <- requireRight "reduced interval" (rationalInterval (1 % 2) 1)
    equalHalf <- requireRight "equivalent interval" (rationalInterval (2 % 4) 1)
    lhalf <- kernelWith 0 0 0 0 [(noiseOwner 1 :: NoiseOwner OwnerScope, half, 0, 0)]
    rhalf <- kernelWith 0 0 0 0 [(noiseOwner 2 :: NoiseOwner OtherScope, equalHalf, 0, 0)]
    _ <- requireRight "exact reduced source law equality" (substituteLeftSuccessor subLimits lhalf rhalf sharedRequest)
    (originalLeft, originalRight) <- substitutionPair
    _ <- requireRight "request valid for actual operands" (substituteLeftSuccessor subLimits originalLeft originalRight sharedRequest)
    differentNames <- kernelWith 0 0 0 0 [(noiseOwner 9 :: NoiseOwner OwnerScope, unitInterval, 0, 0)]
    requireLeft "equal phantoms and counts cannot reuse checked mapping" (substituteLeftSuccessor subLimits differentNames originalRight sharedRequest) (== SuccessorSubstitutionUnknownSharedTarget 1)
    differentLaw <- kernelWith 0 0 0 0 [(noiseOwner 1 :: NoiseOwner OwnerScope, wide, 0, 0)]
    requireLeft "same request rechecks actual intervals" (substituteLeftSuccessor subLimits differentLaw originalRight sharedRequest) (== SuccessorSubstitutionSharedIntervalMismatch 1 wide unitInterval)

limitsFrom :: [Natural] -> ExactSuccessorSubstitutionLimits
limitsFrom [r, o, d, c, w, b] = exactSuccessorSubstitutionLimits r o d c w b
limitsFrom _ = error "six test dimensions required"

jointLimitsFrom :: [Natural] -> ExactJointAffineLimits
jointLimitsFrom [r, o, d, c, w, b] = exactJointAffineLimits r o d c w b
jointLimitsFrom _ = error "six projection dimensions required"

atDimension :: Int -> Natural -> [Natural] -> [Natural]
atDimension index value values = [if position == index then value else old | (position, old) <- zip [0 ..] values]

dimensions :: [ExactJointAffineLimitDimension]
dimensions = [JointAffineRawEntries, JointAffineOwners, JointAffineOutputs, JointAffineCoefficients, JointAffineWork, JointAffineRationalBits]

successorExactBoundaries :: IO ()
successorExactBoundaries = do
    (left, right) <- substitutionPair
    let exact = [3, 3, 2, 20, 77, 2]
        wrap = SuccessorSubstitutionAdmission
        run values = substituteLeftSuccessor (limitsFrom values) left right sharedRequest
        beyond = fromIntegral (maxBound :: Int) + 1
    forM_ (zip3 [0 ..] dimensions exact) $ \(index, dimension, value) -> do
        requireLeft "substitution each one-below dimension" (run (atDimension index (value - 1) exact)) (== wrap (JointAffineLimitExceeded dimension (value - 1) value))
        requireLeft "substitution each zero limit" (run (atDimension index 0 exact)) (== wrap (InvalidJointAffineLimit dimension 0))
        requireLeft "substitution each machine limit" (run (atDimension index beyond exact)) (== wrap (JointAffineMachineIndexExceeded dimension beyond))
    -- Each configured/admission dimension precedes every later competitor.
    forM_ [0 .. 5] $ \first -> forM_ [first + 1 .. 5] $ \later -> do
        let dimension = dimensions !! first
            value = exact !! first
        requireLeft "configured zero total precedence" (run (atDimension later 0 (atDimension first 0 exact))) (== wrap (InvalidJointAffineLimit dimension 0))
        requireLeft "admission dimension total precedence" (run (atDimension later (exact !! later - 1) (atDimension first (value - 1) exact))) (== wrap (JointAffineLimitExceeded dimension (value - 1) value))
    requireLeft
        "count-only poisoned entries before owner preflight"
        (substituteLeftSuccessor (exactSuccessorSubstitutionLimits 3 2 2 20 77 2) left right (freshSuccessorOwners [error "mapping entry forced before complete preflight"]))
        (== wrap (JointAffineLimitExceeded JointAffineOwners 2 3))
    requireLeft
        "infinite mapping bounded by remaining raw"
        (substituteLeftSuccessor (limitsFrom exact) left right (freshSuccessorOwners (repeat (error "mapping entry forced during count"))))
        (== wrap (JointAffineLimitExceeded JointAffineRawEntries 3 4))
    requireLeft
        "input raw before poisoned mapping spine"
        (substituteLeftSuccessor (exactSuccessorSubstitutionLimits 1 1 2 20 77 2) left right (freshSuccessorOwners (error "mapping spine forced before inputs")))
        (== wrap (JointAffineLimitExceeded JointAffineRawEntries 1 2))
    let duplicate = sharedSuccessorOwners [(noiseOwner 2, noiseOwner 1), (noiseOwner 2, noiseOwner 1)]
    requireLeft "108 combined work before duplicate" (substituteLeftSuccessor (exactSuccessorSubstitutionLimits 4 4 2 20 107 2) left right duplicate) (== wrap (JointAffineLimitExceeded JointAffineWork 107 108))
    requireLeft "duplicate after exact 108 work" (substituteLeftSuccessor (exactSuccessorSubstitutionLimits 4 4 2 20 108 2) left right duplicate) (== SuccessorSubstitutionDuplicateSource 1 2)
    requireLeft "empty mapping requires work 54" (substituteLeftSuccessor (exactSuccessorSubstitutionLimits 2 2 2 20 53 2) left right (sharedSuccessorOwners [])) (== wrap (JointAffineLimitExceeded JointAffineWork 53 54))
    requireLeft "empty mapping after work 54 is incomplete" (substituteLeftSuccessor (exactSuccessorSubstitutionLimits 2 2 2 20 54 2) left right (sharedSuccessorOwners [])) (== SuccessorSubstitutionIncompleteMapping 1)
    forM_ [freshSuccessorOwners [(noiseOwner 2, 1)], sharedSuccessorOwners [(noiseOwner 2, noiseOwner 99)]] $ \request ->
        requireLeft "work before mapping target failure" (substituteLeftSuccessor (exactSuccessorSubstitutionLimits 3 3 2 20 76 2) left right request) (== wrap (JointAffineLimitExceeded JointAffineWork 76 77))
    wide <- requireRight "work mismatch interval" (rationalInterval 0 2)
    mismatch <- kernelWith 0 0 0 0 [(noiseOwner 2 :: NoiseOwner OtherScope, wide, 0, 0)]
    requireLeft "work before interval mismatch" (substituteLeftSuccessor (exactSuccessorSubstitutionLimits 3 3 2 20 76 2) left mismatch sharedRequest) (== wrap (JointAffineLimitExceeded JointAffineWork 76 77))
    ownerless <- kernelWith 0 0 1 0 [] :: IO (TestKernel OwnerScope)
    forM_ [sharedSuccessorOwners [], freshSuccessorOwners []] $ \request -> do
        result <- requireRight "ownerless exact work 14" (substituteLeftSuccessor (exactSuccessorSubstitutionLimits 1 1 2 12 14 1) ownerless ownerless request)
        assert "ownerless fixed arithmetic including zeros" (subReportValues (successorSubstitutionReport result) == [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 12, 8, 4, 2, 6, 14, 1])
        requireLeft "ownerless one-below work" (substituteLeftSuccessor (exactSuccessorSubstitutionLimits 1 1 2 12 13 1) ownerless ownerless request) (== wrap (JointAffineLimitExceeded JointAffineWork 13 14))

successorHistoryProjections :: IO ()
successorHistoryProjections = do
    left <- kernelWith 0 0 0 0 [(noiseOwner 1 :: NoiseOwner OwnerScope, unitInterval, 0, 3 % 2)]
    right <- kernelWith (3 % 2) 0 0 0 [(noiseOwner 2 :: NoiseOwner OtherScope, unitInterval, -2, 0)]
    let run work bits = substituteLeftSuccessor (exactSuccessorSubstitutionLimits 3 3 2 20 work bits) left right sharedRequest
        wrap = SuccessorSubstitutionAdmission
    result <- requireRight "9/4 -> 1/4 four-bit substitution" (run 77 4)
    assert "discarded 9/4 retained in history" (substitutionMaximumRationalBits (successorSubstitutionReport result) == 4)
    requireLeft "three-bit substitution fails before cancellation" (run 77 3) (== wrap (JointAffineLimitExceeded JointAffineRationalBits 3 4))
    requireLeft "substitution work before canceled growth" (run 76 3) (== wrap (JointAffineLimitExceeded JointAffineWork 76 77))
    -- Same test shape for both projections; no assumptions about law equality.
    let material limits = snd <$> materializeSuccessorSubstitution limits result 0
        extrema limits = snd <$> successorSubstitutionSupportExtrema limits result unitInterval
    forM_ [(material, 14, 12), (extrema, 18, 16)] $ \(project, total, arithmeticWork) -> do
        let exact = [1, 1, 2, 6, total, 4]
        report <- requireRight "historical four-bit projection exact" (project (jointLimitsFrom exact))
        assert "projection report frozen d/t/history" (jointAffineRawEntryCount report == 1 && jointAffineDeclaredOwnerCount report == 1 && jointAffineRetainedOwnerCount report == 1 && jointAffinePreflightWork report == 2 && jointAffineArithmeticWork report == arithmeticWork && jointAffineWork report == total && jointAffineMaximumRationalBits report == 4)
        requireLeft "projection three-bit history fails" (project (jointLimitsFrom (atDimension 5 3 exact))) (== JointAffineLimitExceeded JointAffineRationalBits 3 4)
        requireLeft "history precedes total-work failure" (project (jointLimitsFrom [1, 1, 2, 6, total - 1, 3])) (== JointAffineLimitExceeded JointAffineRationalBits 3 4)
        requireLeft "baseline work precedes history" (project (jointLimitsFrom [1, 1, 2, 6, 1, 3])) (== JointAffineLimitExceeded JointAffineWork 1 2)
        forM_ (zip3 [0 ..] dimensions exact) $ \(index, dimension, value) -> do
            let expected = if value == 1 then InvalidJointAffineLimit dimension 0 else JointAffineLimitExceeded dimension (value - 1) value
            requireLeft "projection each one-below dimension" (project (jointLimitsFrom (atDimension index (value - 1) exact))) (== expected)
            requireLeft "projection each invalid zero limit" (project (jointLimitsFrom (atDimension index 0 exact))) (== InvalidJointAffineLimit dimension 0)
            let beyond = fromIntegral (maxBound :: Int) + 1
            requireLeft "projection each machine limit" (project (jointLimitsFrom (atDimension index beyond exact))) (== JointAffineMachineIndexExceeded dimension beyond)
    requireLeft "projection work before input bits" (materializeSuccessorSubstitution (exactJointAffineLimits 1 1 2 6 13 4) result 16) (== JointAffineLimitExceeded JointAffineWork 13 14)
    requireLeft "projection input bits after work" (materializeSuccessorSubstitution (exactJointAffineLimits 1 1 2 6 14 4) result 16) (== JointAffineLimitExceeded JointAffineRationalBits 4 5)
    degenerate <- requireRight "projection degenerate interval" (diracReal exactLimits 16)
    requireLeft "projection work before invalid interval" (successorSubstitutionSupportExtrema (exactJointAffineLimits 1 1 2 6 17 4) result (lawSupport degenerate)) (== JointAffineLimitExceeded JointAffineWork 17 18)
    requireLeft "projection invalid interval before endpoint bits" (successorSubstitutionSupportExtrema (exactJointAffineLimits 1 1 2 6 18 4) result (lawSupport degenerate)) (== JointAffineInvalidInterval 16 16)
    huge <- requireRight "projection large interval" (rationalInterval 0 16)
    requireLeft "projection endpoint bits" (successorSubstitutionSupportExtrema (exactJointAffineLimits 1 1 2 6 18 4) result huge) (== JointAffineLimitExceeded JointAffineRationalBits 4 5)
    -- Discarded left reward changes history, not denotation, even ownerless.
    discarded <- kernelWith 0 1024 0 0 [] :: IO (TestKernel OwnerScope)
    small <- kernelWith 0 0 0 0 [] :: IO (TestKernel OwnerScope)
    resultDiscarded <- requireRight "large discarded left reward" (substituteLeftSuccessor subLimits discarded small (sharedSuccessorOwners []))
    resultSmall <- requireRight "small discarded left reward" (substituteLeftSuccessor subLimits small small (sharedSuccessorOwners []))
    md <- momentOf resultDiscarded 0 [(1, 1, 1)]
    ms <- momentOf resultSmall 0 [(1, 1, 1)]
    assert "left reward history changes but denotation does not" (md == ms && md == 0 && substitutionMaximumRationalBits (successorSubstitutionReport resultDiscarded) == 11 && substitutionMaximumRationalBits (successorSubstitutionReport resultSmall) == 1)
    forM_ [\limits -> snd <$> materializeSuccessorSubstitution limits resultDiscarded 0, \limits -> snd <$> successorSubstitutionSupportExtrema limits resultDiscarded unitInterval] $ \project -> do
        requireLeft "discarded reward projection admission" (project (exactJointAffineLimits 1 1 2 4 8 10)) (== JointAffineLimitExceeded JointAffineRationalBits 10 11)
        report <- requireRight "discarded reward projection maximum" (project (exactJointAffineLimits 1 1 2 4 8 11))
        assert "discarded reward report history" (jointAffineMaximumRationalBits report == 11)
    cancelRight <- kernelWith 2 0 0 0 [(noiseOwner 2 :: NoiseOwner OtherScope, unitInterval, -3, 0)]
    canceled <- requireRight "complete cancellation" (substituteLeftSuccessor subLimits left cancelRight sharedRequest)
    assert "zero result declaration publicly counted" (substitutionDeclaredResultOwners (successorSubstitutionReport canceled) == 1 && substitutionRetainedResultOwners (successorSubstitutionReport canceled) == 0)
    (_, m) <- requireRight "cancellation material declared dimensions" (materializeSuccessorSubstitution (exactJointAffineLimits 1 1 2 6 6 2) canceled 0)
    (_, e) <- requireRight "cancellation extrema declared dimensions" (successorSubstitutionSupportExtrema (exactJointAffineLimits 1 1 2 6 10 2) canceled unitInterval)
    assert "canceled rows no executable scans" (jointAffineWork m == 6 && jointAffineWork e == 10)

-- Direct test-side substitution of coefficient data; no production sorting or
-- canonicalization. Source powers are then integrated by the Gate A oracle.
type InputOracleForm = (Rational, Rational, [(Natural, Rational, (Rational, Rational))])

substituteOracle :: InputOracleForm -> InputOracleForm -> [(Natural, Natural)] -> Rational -> OracleForm
substituteOracle (a, b, leftRows) (c, d, rightRows) routing input =
    (c * (a * input + b) + d, [(owner, c * q, bounds) | (owner, q, bounds) <- leftRows] ++ [(target owner, r, bounds) | (owner, r, bounds) <- rightRows])
  where
    target owner = case lookup owner routing of
        Just name -> name
        Nothing -> error "oracle incomplete relation"

-- Combine repeated appearances with an independent association-list fold,
-- since multinomialPower treats its input list as distinct variables.
combineOracle :: OracleForm -> OracleForm
combineOracle (constant, rows) = (constant, foldl' add [] rows)
  where
    add [] row = [row]
    add ((owner, c, bounds) : rest) row@(other, r, _)
        | owner == other = (owner, c + r, bounds) : rest
        | otherwise = (owner, c, bounds) : add rest row

successorSignedOracles :: IO ()
successorSignedOracles = do
    symmetric <- requireRight "signed source interval" (rationalInterval (-1) 1)
    inputInterval <- requireRight "signed input interval" (rationalInterval (-2) 3)
    left <- kernelWith 17 19 (-2) 1 [(noiseOwner 1 :: NoiseOwner OwnerScope, symmetric, 23, 3)]
    right <- kernelWith (-1) 2 2 (-3) [(noiseOwner 2 :: NoiseOwner OtherScope, symmetric, 4, -1)]
    forM_ [(sharedRequest, [(2, 1)], (-4, 8), (-18, 12), 5 % 3, 2 % 3), (freshRequest, [(2, 3)], (-10, 14), (-20, 14), -(22 % 3), -(25 % 3))] $ \(request, routing, rewardBounds, successorBounds, covariance, joint) -> do
        result <- requireRight "signed substitution" (substituteLeftSuccessor subLimits left right request)
        ((r, s), _) <- requireRight "signed substitution extrema" (successorSubstitutionSupportExtrema largeJointLimits result inputInterval)
        let leftForm = (-2, 1, [(1, 3, (-1, 1))])
            rightReward = (-1, 2, [(2, 4, (-1, 1))])
            rightSuccessor = (2, -3, [(2, -1, (-1, 1))])
            rf x = combineOracle (substituteOracle leftForm rightReward routing x)
            sf x = combineOracle (substituteOracle leftForm rightSuccessor routing x)
            distinct = if routing == [(2, 1)] then [1] else [1, 3]
            values = [(evaluate (rf x) assignment, evaluate (sf x) assignment) | x <- [-2, 3], assignment <- sequence [[(owner, -1), (owner, 1)] | owner <- distinct]]
            evaluate (constant, rows) assignment = constant + sum [coefficient * fromMaybe (error "missing corner") (lookup owner assignment) | (owner, coefficient, _) <- rows]
        assert "signed frozen extrema" (intervalBounds r == rewardBounds && intervalBounds s == successorBounds)
        assert "independent witnessed-source corners" (rewardBounds == (minimum (map fst values), maximum (map fst values)) && successorBounds == (minimum (map snd values), maximum (map snd values)))
        forM_ [0, 2] $ \input -> forM_ [[(1, 1, 1)], [(2, 0, 1)], [(0, 2, 1)], [(2, 3, 2), (1, 1, -5), (0, 0, 7)]] $ \terms -> do
            actual <- momentOf result input terms
            assert "independent formula and multinomial substitution oracle" (actual == oracleBivariate (rf input) (sf input) terms)
        meanR <- momentOf result 0 [(1, 0, 1)]
        meanS <- momentOf result 0 [(0, 1, 1)]
        cross <- momentOf result 0 [(1, 1, 1)]
        assert "signed exact mean/covariance/cross goldens" (meanR == 1 && meanS == -1 && cross == joint && cross - meanR * meanS == covariance)

successorPermutationAndIdentity :: IO ()
successorPermutationAndIdentity = do
    let leftRows = [(noiseOwner 1 :: NoiseOwner OwnerScope, unitInterval, 0, 2), (noiseOwner 4, unitInterval, 0, -1), (noiseOwner 7, unitInterval, 0, 0)]
        rightRows = [(noiseOwner 2 :: NoiseOwner OtherScope, unitInterval, 3, 1), (noiseOwner 5, unitInterval, -2, 0)]
        mappings = [(noiseOwner 2, noiseOwner 4), (noiseOwner 5, noiseOwner 1)]
        build ls rs mapping = do
            left <- kernelWith 0 31 2 1 ls
            right <- kernelWith 3 (-1) (-2) 4 rs
            requireRight "permuted substitution" (substituteLeftSuccessor subLimits left right (sharedSuccessorOwners mapping))
    reference <- build leftRows rightRows mappings
    refMoment <- momentOf reference 1 [(2, 3, 1), (1, 1, 2)]
    (refBounds, refProjection) <- requireRight "reference permutation bounds" (successorSubstitutionSupportExtrema largeJointLimits reference unitInterval)
    forM_ (permutations leftRows) $ \ls -> forM_ (permutations rightRows) $ \rs -> forM_ (permutations mappings) $ \mapping -> do
        result <- build ls rs mapping
        moment <- momentOf result 1 [(2, 3, 1), (1, 1, 2)]
        (bounds, projection) <- requireRight "permutation extrema" (successorSubstitutionSupportExtrema largeJointLimits result unitInterval)
        assert "permutation semantics/extrema/reports" (moment == refMoment && bounds == refBounds && projection == refProjection && subReportValues (successorSubstitutionReport result) == subReportValues (successorSubstitutionReport reference))
    let freshMappings = [(noiseOwner 2, 20), (noiseOwner 5, 50)]
        buildFresh ls rs mapping = do
            left <- kernelWith 0 31 2 1 ls
            right <- kernelWith 3 (-1) (-2) 4 rs
            requireRight "fresh permuted substitution" (substituteLeftSuccessor subLimits left right (freshSuccessorOwners mapping))
    freshReference <- buildFresh leftRows rightRows freshMappings
    freshMoment <- momentOf freshReference 1 [(2, 3, 1), (1, 1, 2)]
    freshBounds <- requireRight "fresh permutation reference extrema" (successorSubstitutionSupportExtrema largeJointLimits freshReference unitInterval)
    forM_ (permutations leftRows) $ \ls -> forM_ (permutations rightRows) $ \rs -> forM_ (permutations freshMappings) $ \mapping -> do
        result <- buildFresh ls rs mapping
        moment <- momentOf result 1 [(2, 3, 1), (1, 1, 2)]
        bounds <- requireRight "fresh permutation extrema" (successorSubstitutionSupportExtrema largeJointLimits result unitInterval)
        assert "fresh permutation semantics/extrema/reports" (moment == freshMoment && bounds == freshBounds && subReportValues (successorSubstitutionReport result) == subReportValues (successorSubstitutionReport freshReference))
    -- nL=3,nR=2,m=2: structural phases 51+38+10+50+5+7+98=259;
    -- arithmetic 6+6+10=22, combined W=281.
    let report = successorSubstitutionReport reference
    assert "larger bounded ledger golden" (substitutionPreflightWork report == 259 && substitutionArithmeticWork report == 22 && substitutionWork report == 281)
    -- d=3,t=2 despite the reservation-only names and one declared zero.
    forM_ [(\limits -> snd <$> materializeSuccessorSubstitution limits reference 1, 32, 20), (\limits -> snd <$> successorSubstitutionSupportExtrema limits reference unitInterval, 36, 24)] $ \(project, total, arithmeticWork) -> do
        let exact = [3, 3, 2, 10, total, 5]
        projected <- requireRight "larger d/t exact projection dimensions" (project (jointLimitsFrom exact))
        assert "larger projection formula independent d/t" (jointAffineRawEntryCount projected == 3 && jointAffineRetainedOwnerCount projected == 2 && jointAffinePreflightWork projected == 12 && jointAffineArithmeticWork projected == arithmeticWork)
        forM_ (zip3 [0 ..] dimensions exact) $ \(index, dimension, value) ->
            requireLeft "larger projection each one below" (project (jointLimitsFrom (atDimension index (value - 1) exact))) (== JointAffineLimitExceeded dimension (value - 1) value)
        forM_ [0 .. 5] $ \first -> forM_ [first + 1 .. 5] $ \later -> do
            -- Historical bits precede total work, unlike earlier dimensions.
            let winner = if first == 4 && later == 5 then 5 else first
                dimension = dimensions !! winner
                value = exact !! winner
            requireLeft "projection competing dimension precedence" (project (jointLimitsFrom (atDimension later (exact !! later - 1) (atDimension first (exact !! first - 1) exact)))) (== JointAffineLimitExceeded dimension (value - 1) value)
    left <- kernelWith 0 31 2 1 leftRows
    right <- kernelWith 3 (-1) (-2) 4 rightRows
    renamedLeft <- requireRight "coherent alpha left" (alphaRenameJointAffineKernel largeJointLimits [(noiseOwner 1, noiseOwner 11), (noiseOwner 4, noiseOwner 14), (noiseOwner 7, noiseOwner 17)] left)
    renamedRight <- requireRight "coherent alpha right" (alphaRenameJointAffineKernel largeJointLimits [(noiseOwner 2, noiseOwner 12), (noiseOwner 5, noiseOwner 15)] right)
    renamed <- requireRight "coherently renamed substitution" (substituteLeftSuccessor subLimits renamedLeft renamedRight (sharedSuccessorOwners [(noiseOwner 12, noiseOwner 14), (noiseOwner 15, noiseOwner 11)]))
    renamedMoment <- momentOf renamed 1 [(2, 3, 1), (1, 1, 2)]
    assert "coherent alpha preserves substitution" (renamedMoment == refMoment)
    identity <- kernelWith 0 99 1 0 [] :: IO (TestKernel OwnerScope)
    result <- requireRight "ownerless left identity uses fresh mode" (substituteLeftSuccessor subLimits identity right (freshSuccessorOwners [(noiseOwner 2, 12), (noiseOwner 5, 15)]))
    requireLeft "ownerless left identity has no shared targets" (substituteLeftSuccessor subLimits identity right (sharedSuccessorOwners [(noiseOwner 2, noiseOwner 12), (noiseOwner 5, noiseOwner 15)])) (== SuccessorSubstitutionUnknownSharedTarget 1)
    forM_ [0, 1, -2] $ \input -> do
        actual <- momentOf result input [(2, 3, 1), (1, 1, 2)]
        assert "deterministic identity successor preserves right forms" (actual == oracleBivariate (3 * input - 1, [(12, 3, (0, 1)), (15, -2, (0, 1))]) (-(2 * input) + 4, [(12, 1, (0, 1)), (15, 0, (0, 1))]) [(2, 3, 1), (1, 1, 2)])
