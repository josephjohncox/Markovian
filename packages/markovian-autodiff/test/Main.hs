{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (forM_, unless)
import Markovian.Autodiff
import Markovian.Backend.Neural.Dense (denseForward, denseInputVJP, denseParameterVJP, mkDenseNetwork)
import NeuralDifferential qualified
import Paths_markovian_autodiff (getDataFileName)
import System.Exit (exitFailure)

type BranchParameters =
    'ParameterProduct
        ('ParameterProduct (Owner "weight" 'Scalar) 'NoParameters)
        'NoParameters

type SquareParameters =
    'ParameterProduct
        ('ParameterProduct 'NoParameters 'NoParameters)
        'NoParameters

type SmoothParameters = 'ParameterProduct SquareParameters 'NoParameters

type LinearBiasParameters = Owner "bias" 'Scalar

type LinearParameters =
    'ParameterProduct
        ('ParameterProduct BranchParameters LinearBiasParameters)
        'NoParameters

limits :: CompilerLimits
limits = compilerLimits 128 128 32 8 64 64 256 32 10000 256

branch :: (Num scalar) => Program scalar 'Polynomial BranchParameters 'Scalar 'Scalar
branch = compose (fanout (parameter @"weight" SScalar SScalar) (identity SScalar)) multiplyScalar

shared :: (Num scalar) => Program scalar 'Polynomial BranchParameters ('Product 'Scalar 'Scalar) ('Product 'Scalar 'Scalar)
shared = shareParameters branch branch

branchParameters :: scalar -> ParameterValue scalar BranchParameters
branchParameters weight = parameterProduct (parameterProduct (ownedParameters (scalarValue weight)) noParameters) noParameters

square :: (Num scalar) => Program scalar 'Polynomial SquareParameters 'Scalar 'Scalar
square = compose (fanout (identity SScalar) (identity SScalar)) multiplyScalar

smooth :: Program Double 'Smooth SmoothParameters 'Scalar 'Scalar
smooth = compose (liftPolynomial square) tanhScalar

squareParameters :: ParameterValue scalar SquareParameters
squareParameters = parameterProduct (parameterProduct noParameters noParameters) noParameters

smoothParameters :: ParameterValue Double SmoothParameters
smoothParameters = parameterProduct squareParameters noParameters

linearDense :: Program Double 'Polynomial LinearParameters 'Scalar 'Scalar
linearDense = compose (fanout branch (parameter @"bias" SScalar SScalar)) addScalar

linearParameters :: Double -> Double -> ParameterValue Double LinearParameters
linearParameters weight bias = parameterProduct (parameterProduct (branchParameters weight) (ownedParameters (scalarValue bias))) noParameters

main :: IO ()
main = do
    exactPolynomialLaw
    sharedOwnerLaw
    exactJvpVjpPairing
    exactPrimitivePairings
    doubleFiniteDifferences
    storedRecomputedParity
    neuralDenseDifferential
    NeuralDifferential.tests
    vectorLaw
    boundaries
    putStrLn "markovian-autodiff: all focused tests passed"

exactPolynomialLaw :: IO ()
exactPolynomialLaw = do
    executable <- expectRight "compile exact square" (compileExactPolynomial limits StorePullbacks square)
    run <- expectRight "run exact square" (runExact executable squareParameters (scalarValue (3 :: Rational)))
    assertEqual "exact square primal" (scalarValue 9) (exactRunOutput run)
    (_, inputGradient) <- expectRight "exact square VJP" (applyExactVJP run (scalarValue 1))
    assertEqual "formal polynomial derivative" (scalarValue 6) inputGradient
    direct <- expectRight "direct exact interpreter" (interpretExactPolynomial limits square squareParameters (scalarValue 3))
    assertEqual "compiled/direct exact primal" direct (exactRunOutput run)
    let firstReport = renderCompileReport (exactCompileReport executable)
    goldenPath <- getDataFileName "test/golden/square-compile-report.txt"
    golden <- readFile goldenPath
    assertEqual "deterministic compile report golden" golden firstReport
    unless ("exact-rational-formal-polynomial" `contains` firstReport) (failTest "report semantics missing")

sharedOwnerLaw :: IO ()
sharedOwnerLaw = do
    executable <- expectRight "compile shared owner" (compileExactPolynomial limits StorePullbacks shared)
    run <- expectRight "run shared owner" (runExact executable (branchParameters (3 :: Rational)) (productValue (scalarValue 2) (scalarValue 5)))
    assertEqual "shared primal" (productValue (scalarValue 6) (scalarValue 15)) (exactRunOutput run)
    (parameterGradient, inputGradient) <- expectRight "shared VJP" (applyExactVJP run (productValue (scalarValue 1) (scalarValue 1)))
    assertEqual "shared owner diagonal accumulation" [7] (parameterScalars parameterGradient)
    assertEqual "shared input gradient" (productValue (scalarValue 3) (scalarValue 3)) inputGradient
    unless (parameterScalars parameterGradient /= [5]) (failTest "missing-diagonal counterexample did not distinguish the incorrect rule")

exactJvpVjpPairing :: IO ()
exactJvpVjpPairing = do
    executable <- expectRight "compile pairing fixture" (compileExactPolynomial limits StorePullbacks shared)
    let weight = 3 :: Rational
        left = 2 :: Rational
        right = 5 :: Rational
        dWeight = 7 :: Rational
        dLeft = 11 :: Rational
        dRight = 13 :: Rational
        seedLeft = 17 :: Rational
        seedRight = 19 :: Rational
    (oracleOutput, oracleTangent) <-
        expectRight
            "independent syntax-recursive JVP"
            ( interpretExactPolynomialJVP
                shared
                (branchParameters weight)
                (branchParameters dWeight)
                (productValue (scalarValue left) (scalarValue right))
                (productValue (scalarValue dLeft) (scalarValue dRight))
            )
    assertEqual "independent oracle primal" (productValue (scalarValue (weight * left)) (scalarValue (weight * right))) oracleOutput
    let (jvpLeft, jvpRight) = case valueScalars oracleTangent of
            [leftTangent, rightTangent] -> (leftTangent, rightTangent)
            _ -> error "independent oracle returned the wrong tangent extent"
        forwardPairing = seedLeft * jvpLeft + seedRight * jvpRight
    run <- expectRight "run pairing fixture" (runExact executable (branchParameters weight) (productValue (scalarValue left) (scalarValue right)))
    (parameterGradient, inputGradient) <- expectRight "pairing VJP" (applyExactVJP run (productValue (scalarValue seedLeft) (scalarValue seedRight)))
    gradientWeight <- expectSingleton "pairing parameter gradient" (parameterScalars parameterGradient)
    (gradientLeft, gradientRight) <- expectPair "pairing input gradient" (valueScalars inputGradient)
    let reversePairing = dWeight * gradientWeight + dLeft * gradientLeft + dRight * gradientRight
    assertEqual "exact independent JVP/VJP pairing" forwardPairing reversePairing

exactPrimitivePairings :: IO ()
exactPrimitivePairings = do
    left <- expectRight "pairing left vector" (vectorValue @2 2 [2, 3 :: Rational])
    right <- expectRight "pairing right vector" (vectorValue @2 2 [5, 7 :: Rational])
    dLeft <- expectRight "pairing left vector direction" (vectorValue @2 2 [11, 13 :: Rational])
    dRight <- expectRight "pairing right vector direction" (vectorValue @2 2 [17, 19 :: Rational])
    vectorSeed <- expectRight "pairing vector seed" (vectorValue @2 2 [23, 29 :: Rational])
    constant <- expectRight "pairing constant vector" (vectorValue @2 2 [31, 37 :: Rational])
    exactPairing "identity" (identity SScalar) noParameters noParameters (scalarValue 2) (scalarValue 3) (scalarValue 5)
    exactPairing "constant scalar" (constantScalar SScalar 7) noParameters noParameters (scalarValue 2) (scalarValue 3) (scalarValue 5)
    exactPairing "constant vector" (constantVector SScalar (SVector @2) constant) noParameters noParameters (scalarValue 2) (scalarValue 3) vectorSeed
    exactPairing "parameter" (parameter @"pairing-parameter" SScalar SScalar) (ownedParameters (scalarValue 7)) (ownedParameters (scalarValue 11)) (scalarValue 2) (scalarValue 3) (scalarValue 5)
    exactPairing "negate" negateScalar noParameters noParameters (scalarValue 2) (scalarValue 3) (scalarValue 5)
    exactPairing "add scalar" addScalar noParameters noParameters (productValue (scalarValue 2) (scalarValue 5)) (productValue (scalarValue 3) (scalarValue 7)) (scalarValue 11)
    exactPairing "multiply scalar" multiplyScalar noParameters noParameters (productValue (scalarValue 2) (scalarValue 5)) (productValue (scalarValue 3) (scalarValue 7)) (scalarValue 11)
    exactPairing "add vector" (addVector (SVector @2)) noParameters noParameters (productValue left right) (productValue dLeft dRight) vectorSeed
    exactPairing "hadamard" (hadamard (SVector @2)) noParameters noParameters (productValue left right) (productValue dLeft dRight) vectorSeed
    exactPairing "dot" (dot (SVector @2)) noParameters noParameters (productValue left right) (productValue dLeft dRight) (scalarValue 23)
    exactPairing "sum" (sumVector (SVector @2)) noParameters noParameters left dLeft (scalarValue 23)
    exactPairing "first" (first SScalar SScalar) noParameters noParameters (productValue (scalarValue 2) (scalarValue 5)) (productValue (scalarValue 3) (scalarValue 7)) (scalarValue 11)
    exactPairing "second" (second SScalar SScalar) noParameters noParameters (productValue (scalarValue 2) (scalarValue 5)) (productValue (scalarValue 3) (scalarValue 7)) (scalarValue 11)
    exactPairing "parallel" (parallel negateScalar negateScalar) (parameterProduct noParameters noParameters) (parameterProduct noParameters noParameters) (productValue (scalarValue 2) (scalarValue 5)) (productValue (scalarValue 3) (scalarValue 7)) (productValue (scalarValue 11) (scalarValue 13))

exactPairing ::
    String ->
    Program Rational 'Polynomial parameters input output ->
    ParameterValue Rational parameters ->
    ParameterValue Rational parameters ->
    Value Rational input ->
    Value Rational input ->
    Value Rational output ->
    IO ()
exactPairing label program parameters parameterDirection input inputDirection seed = do
    executable <- expectRight (label ++ " compile") (compileExactPolynomial limits StorePullbacks program)
    run <- expectRight (label ++ " run") (runExact executable parameters input)
    (oracleOutput, oracleTangent) <- expectRight (label ++ " independent JVP") (interpretExactPolynomialJVP program parameters parameterDirection input inputDirection)
    assertEqual (label ++ " independent primal") oracleOutput (exactRunOutput run)
    (parameterGradient, inputGradient) <- expectRight (label ++ " VJP") (applyExactVJP run seed)
    forwardPairing <- expectRight (label ++ " forward pairing extent") (dotCoordinates (valueScalars seed) (valueScalars oracleTangent))
    parameterPairing <- expectRight (label ++ " parameter pairing extent") (dotCoordinates (parameterScalars parameterDirection) (parameterScalars parameterGradient))
    inputPairing <- expectRight (label ++ " input pairing extent") (dotCoordinates (valueScalars inputDirection) (valueScalars inputGradient))
    assertEqual (label ++ " exact JVP/VJP pairing") forwardPairing (parameterPairing + inputPairing)

dotCoordinates :: [Rational] -> [Rational] -> Either String Rational
dotCoordinates left right
    | length left == length right = Right (sum (zipWith (*) left right))
    | otherwise = Left "independent exact pairing coordinate mismatch"

doubleFiniteDifferences :: IO ()
doubleFiniteDifferences = do
    forM_ [StorePullbacks, RecomputePullbacks] $ \policy -> do
        executable <- expectRight "compile Double shared" (compileDoublePolynomial limits policy shared)
        let weight = 1.25
            left = -0.75
            right = 2.0
            seed = productValue (scalarValue 0.7) (scalarValue (-1.1))
        run <- expectRight "run Double shared" (runDouble executable (branchParameters weight) (productValue (scalarValue left) (scalarValue right)))
        (parameterGradient, inputGradient) <- expectRight "Double shared VJP" (applyDoubleVJP run seed)
        dw <- expectSingleton "Double parameter gradient" (parameterScalars parameterGradient)
        (dx1, dx2) <- expectPair "Double input gradient" (valueScalars inputGradient)
        let objective w x1 x2 = 0.7 * w * x1 - 1.1 * w * x2
            checks =
                [ ("weight", dw, finiteDifference (\w -> objective w left right) weight)
                , ("left input", dx1, finiteDifference (\x -> objective weight x right) left)
                , ("right input", dx2, finiteDifference (objective weight left) right)
                ]
        forM_ checks $ \(name, actual, expected) -> assertApprox (show policy ++ " all-coordinate " ++ name) expected actual

storedRecomputedParity :: IO ()
storedRecomputedParity = do
    stored <- expectRight "compile stored smooth" (compileDoubleSmooth limits StorePullbacks smooth)
    recomputed <- expectRight "compile recomputed smooth" (compileDoubleSmooth limits RecomputePullbacks smooth)
    storedRun <- expectRight "run stored smooth" (runDouble stored smoothParameters (scalarValue 0.7))
    recomputedRun <- expectRight "run recomputed smooth" (runDouble recomputed smoothParameters (scalarValue 0.7))
    assertEqual "stored/recomputed primal" (doubleRunOutput storedRun) (doubleRunOutput recomputedRun)
    storedGradient <- expectRight "stored smooth VJP" (applyDoubleVJP storedRun (scalarValue 1))
    recomputedGradient <- expectRight "recomputed smooth VJP" (applyDoubleVJP recomputedRun (scalarValue 1))
    assertEqual "stored/recomputed VJP" storedGradient recomputedGradient
    -- Repeated use of the same opaque tape is deterministic.
    let tape = doubleRunTape storedRun
    once <- expectRight "first repeated tape application" (applyDoubleTape tape (scalarValue 1))
    twice <- expectRight "second repeated tape application" (applyDoubleTape tape (scalarValue 1))
    assertEqual "repeated tape" once twice
    let expected = finiteDifference (\x -> tanh (x * x)) 0.7
        actual = scalarFromValue (snd once)
    assertApprox "smooth every-coordinate finite difference" expected actual

neuralDenseDifferential :: IO ()
neuralDenseDifferential = do
    let weight = 1.25
        bias = -0.4
        input = 0.7
        seed = -1.3
    executable <- expectRight "compile linear dense differential" (compileDoublePolynomial limits StorePullbacks linearDense)
    run <- expectRight "run linear dense differential" (runDouble executable (linearParameters weight bias) (scalarValue input))
    (parameterGradient, inputGradient) <- expectRight "autodiff linear dense VJP" (applyDoubleVJP run (scalarValue seed))
    network <- expectRight "manual linear dense network" (mkDenseNetwork 1 [] 1 [weight, bias])
    manualOutput <- expectRight "manual linear dense primal" (denseForward network [input])
    manualParameters <- expectRight "manual linear dense parameter VJP" (denseParameterVJP network [input] [seed])
    manualInput <- expectRight "manual linear dense input VJP" (denseInputVJP network [input] [seed])
    autoOutput <- expectSingleton "autodiff linear dense primal" (valueScalars (doubleRunOutput run))
    autoInput <- expectSingleton "autodiff linear dense input VJP" (valueScalars inputGradient)
    assertApproxList "manual/autodiff dense primal" manualOutput [autoOutput]
    assertApproxList "manual/autodiff dense parameter VJP" manualParameters (parameterScalars parameterGradient)
    assertApproxList "manual/autodiff dense input VJP" manualInput [autoInput]

vectorLaw :: IO ()
vectorLaw = do
    left <- expectRight "left vector" (vectorValue @3 3 [1, 2, 3 :: Rational])
    right <- expectRight "right vector" (vectorValue @3 3 [4, 5, 6 :: Rational])
    let program = dot (SVector @3)
    executable <- expectRight "compile exact dot" (compileExactPolynomial limits StorePullbacks program)
    run <- expectRight "run exact dot" (runExact executable noParameters (productValue left right))
    assertEqual "dot primal" (scalarValue 32) (exactRunOutput run)
    (_, gradient) <- expectRight "dot VJP" (applyExactVJP run (scalarValue 2))
    assertEqual "dot VJP" [8, 10, 12, 2, 4, 6] (valueScalars gradient)

boundaries :: IO ()
boundaries = do
    case compileExactPolynomial (compilerLimits 1 128 32 8 64 64 256 32 10000 256) StorePullbacks square of
        Left _ -> pure ()
        Right _ -> failTest "one-below node budget unexpectedly compiled"
    case compileExactPolynomial limits StorePullbacks (parallel branch branch) of
        Left _ -> pure ()
        Right _ -> failTest "independent duplicate owner unexpectedly compiled"
    case vectorValue @2 1 [1, 2 :: Rational] of
        Left (VectorDimensionExceedsLimit 1 2) -> pure ()
        other -> failTest ("vector preflight boundary mismatch: " ++ show other)

    _ <- expectRight "exact scalar-work limit" (compileExactPolynomial (compilerLimits 128 128 32 8 64 64 256 32 13 256) StorePullbacks square)
    case compileExactPolynomial (compilerLimits 128 128 32 8 64 64 256 32 12 256) StorePullbacks square of
        Left problem | "ExecutionWorkLimitExceeded 12 13" `contains` show problem -> pure ()
        other -> failTest ("one-below execution-work boundary: " ++ showEither other)

    let rationalLimits = compilerLimits 32 32 8 2 8 8 64 8 100 8
    multiplication <- expectRight "compile bounded rational multiplication" (compileExactPolynomial rationalLimits StorePullbacks multiplyScalar)
    exactLimit <- expectRight "run exact rational magnitude limit" (runExact multiplication noParameters (productValue (scalarValue 15) (scalarValue 15)))
    assertEqual "exact rational limit output" (scalarValue 225) (exactRunOutput exactLimit)
    case runExact multiplication noParameters (productValue (scalarValue 16) (scalarValue 16)) of
        Left problem | "RationalMagnitudeLimitExceeded" `contains` show problem -> pure ()
        other -> failTest ("rational-growth boundary: " ++ showEither other)

    let hugeShape = SVector @18446744073709551616
        hugeProgram = identity hugeShape :: Program Rational 'Polynomial 'NoParameters ('Vector 18446744073709551616) ('Vector 18446744073709551616)
        huge = 18446744073709551616
    case compileExactPolynomial (compilerLimits 4 1 2 0 huge huge 8 4 10 64) StorePullbacks hugeProgram of
        Left problem | "MachineVectorExtentExceeded" `contains` show problem -> pure ()
        other -> failTest ("machine-vector boundary: " ++ showEither other)

    assertEqual "named Double comparison policy" "abs<=2.0e-10 or rel<=2.0e-8" (renderDoubleComparisonPolicy doubleComparisonPolicy)

    _ <- expectRight "private bounded exact SSA evidence" checkExactSSAIdentities

    executable <- expectRight "compile nonfinite fixture" (compileDoublePolynomial limits StorePullbacks (identity SScalar))
    case runDouble executable noParameters (scalarValue (0 / 0)) of
        Left _ -> pure ()
        Right _ -> failTest "nonfinite input escaped"
    -- Floating reassociation is deliberately not an optimizer law.
    let leftAssociated = ((1e16 + (-1e16)) + 1 :: Double)
        rightAssociated = (1e16 + ((-1e16) + 1) :: Double)
    unless (leftAssociated /= rightAssociated) (failTest "floating reassociation counterexample collapsed")

finiteDifference :: (Double -> Double) -> Double -> Double
finiteDifference function coordinate =
    let step = 1e-6 * max 1 (abs coordinate)
     in (function (coordinate + step) - function (coordinate - step)) / (2 * step)

assertApproxList :: String -> [Double] -> [Double] -> IO ()
assertApproxList label expected actual = do
    unless (length expected == length actual) (failTest (label ++ ": unequal list lengths"))
    forM_ (zip3 [0 :: Int ..] expected actual) $ \(index, wanted, got) -> assertApprox (label ++ " " ++ show index) wanted got

assertApprox :: String -> Double -> Double -> IO ()
assertApprox label expected actual =
    let difference = abs (expected - actual)
        tolerance = 2e-10 + 2e-8 * max (abs expected) (abs actual)
     in unless (difference <= tolerance) (failTest (label ++ ": expected " ++ show expected ++ ", got " ++ show actual))

assertEqual :: (Eq value, Show value) => String -> value -> value -> IO ()
assertEqual label expected actual = unless (expected == actual) (failTest (label ++ ": expected " ++ show expected ++ ", got " ++ show actual))

expectRight :: (Show error) => String -> Either error value -> IO value
expectRight _ (Right value) = pure value
expectRight label (Left problem) = failTest (label ++ ": " ++ show problem)

contains :: String -> String -> Bool
contains needle haystack = any (needle `prefixOf`) (tails haystack)
  where
    prefixOf [] _ = True
    prefixOf _ [] = False
    prefixOf (x : xs) (y : ys) = x == y && prefixOf xs ys
    tails [] = [[]]
    tails values@(_ : rest) = values : tails rest

expectSingleton :: String -> [value] -> IO value
expectSingleton _ [value] = pure value
expectSingleton label _ = failTest (label ++ ": wrong coordinate count")

expectPair :: String -> [value] -> IO (value, value)
expectPair _ [left, right] = pure (left, right)
expectPair label _ = failTest (label ++ ": wrong coordinate count")

showEither :: (Show error) => Either error value -> String
showEither (Left problem) = "Left " ++ show problem
showEither (Right _) = "Right <value>"

failTest :: String -> IO a
failTest message = do
    putStrLn ("FAIL: " ++ message)
    exitFailure
