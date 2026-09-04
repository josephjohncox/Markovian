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

type QuotedSquareParameters =
    'ParameterProduct
        ('ParameterProduct 'NoParameters 'NoParameters)
        SquareParameters

type RootScalar = RootEnvironment 'Scalar

type ParameterizedLetParameters =
    'ParameterProduct
        ('ParameterProduct 'NoParameters (Owner "let-bound" 'Scalar))
        (Owner "let-body" 'Scalar)

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

quotedSquare :: Quote Rational 'Polynomial RootScalar QuotedSquareParameters 'Scalar
quotedSquare = withQuoteScope $ \scope ->
    let environment = rootEnvironment SScalar
        bound = project (pathHere SScalar)
        boundPath = pathRight scope environment SScalar
        body = composeQuote (fanoutQuote (project boundPath) (project boundPath)) multiplyScalar
     in letQuote scope bound body

quotedSquareParameters :: ParameterValue Rational QuotedSquareParameters
quotedSquareParameters = parameterProduct (parameterProduct noParameters noParameters) squareParameters

usedOnceQuote :: Quote Rational 'Polynomial RootScalar SquareParameters 'Scalar
usedOnceQuote = withQuoteScope $ \scope ->
    let environment = rootEnvironment SScalar
     in letQuote scope (project (pathHere SScalar)) (project (pathRight scope environment SScalar))

unusedQuote :: Quote Rational 'Polynomial RootScalar SquareParameters 'Scalar
unusedQuote = withQuoteScope $ \scope ->
    let oldPath = pathLeft scope (pathHere SScalar) SScalar
     in letQuote scope (project (pathHere SScalar)) (project oldPath)

nestedPathQuote :: Quote Rational 'Polynomial RootScalar QuotedSquareParameters 'Scalar
nestedPathQuote = withQuoteScope $ \outerScope -> withQuoteScope $ \innerScope ->
    let outerBound = project (pathHere SScalar)
        outerOldPath = pathLeft outerScope (pathHere SScalar) SScalar
        innerBound = project outerOldPath
        originalPath = pathLeft innerScope outerOldPath SScalar
        inner = letQuote innerScope innerBound (project originalPath)
     in letQuote outerScope outerBound inner

parameterizedLet :: Quote Rational 'Polynomial RootScalar ParameterizedLetParameters 'Scalar
parameterizedLet = withQuoteScope $ \scope ->
    let environment = rootEnvironment SScalar
        bound = quoteProgram (parameter @"let-bound" SScalar SScalar)
        bodyEnvironment = extendEnvironment scope environment SScalar
        body = quoteProgramAt bodyEnvironment (parameter @"let-body" (SProduct SScalar SScalar) SScalar)
     in letQuote scope bound body

quoteLimits :: QuotationLimits
quoteLimits = quotationLimits 10000 128 64 32 256 64 64 256 10000 10000 30000 256

-- Declared finite grammar used for all-coordinate exact pairing evidence.
boundedQuotationGrammar :: [(String, Quote Rational 'Polynomial RootScalar 'NoParameters 'Scalar)]
boundedQuotationGrammar =
    [ ("path", project (pathHere SScalar))
    , ("identity", quoteProgram (identity SScalar))
    , ("negate", quoteProgram negateScalar)
    , ("constant", quoteProgram (constantScalar SScalar 7))
    ]

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
    quotationEvidence
    quotationBudgets
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

quotationEvidence :: IO ()
quotationEvidence = do
    forM_ boundedQuotationGrammar $ \(label, quotation) -> do
        (grammarPrimal, grammarTangent) <- expectRight (label ++ " grammar JVP") (interpretExactQuoteJVP quoteLimits quotation noParameters noParameters (scalarValue 3) (scalarValue 5))
        (grammarExecutable, _) <- expectRight (label ++ " grammar compile") (compileExactQuote quoteLimits StorePullbacks quotation)
        grammarRun <- expectRight (label ++ " grammar run") (runExact grammarExecutable noParameters (scalarValue 3))
        assertEqual (label ++ " grammar primal") grammarPrimal (exactRunOutput grammarRun)
        (_, grammarInputCotangent) <- expectRight (label ++ " grammar VJP") (applyExactVJP grammarRun (scalarValue 11))
        assertEqual
            (label ++ " grammar coordinate pairing")
            (11 * scalarFromValue grammarTangent)
            (5 * scalarFromValue grammarInputCotangent)

    assertEqual "used-once quotation" (Right (scalarValue 3)) (interpretExactQuote quoteLimits usedOnceQuote squareParameters (scalarValue 3))
    assertEqual "unused quotation is call-by-value" (Right (scalarValue 3)) (interpretExactQuote quoteLimits unusedQuote squareParameters (scalarValue 3))
    assertEqual "nested quotation path" (Right (scalarValue 5)) (interpretExactQuote quoteLimits nestedPathQuote quotedSquareParameters (scalarValue 5))
    (_, nestedTangent) <- expectRight "nested path JVP" (interpretExactQuoteJVP quoteLimits nestedPathQuote quotedSquareParameters quotedSquareParameters (scalarValue 5) (scalarValue 7))
    (nestedExecutable, _) <- expectRight "nested path compile" (compileExactQuote quoteLimits StorePullbacks nestedPathQuote)
    nestedRun <- expectRight "nested path run" (runExact nestedExecutable quotedSquareParameters (scalarValue 5))
    (_, nestedCotangent) <- expectRight "nested path VJP" (applyExactVJP nestedRun (scalarValue 11))
    assertEqual "nested path zero-filled projection cotangent" (scalarValue 11) nestedCotangent
    assertEqual "nested path coordinate pairing" (11 * scalarFromValue nestedTangent) (7 * scalarFromValue nestedCotangent)

    let parameterValues = parameterProduct (parameterProduct noParameters (ownedParameters (scalarValue 7))) (ownedParameters (scalarValue 11))
        parameterDirections = parameterProduct (parameterProduct noParameters (ownedParameters (scalarValue 13))) (ownedParameters (scalarValue 17))
    assertEqual "parameterized let primal" (Right (scalarValue 11)) (interpretExactQuote quoteLimits parameterizedLet parameterValues (scalarValue 3))
    assertEqual
        "parameterized let JVP"
        (Right (scalarValue 11, scalarValue 17))
        (interpretExactQuoteJVP quoteLimits parameterizedLet parameterValues parameterDirections (scalarValue 3) (scalarValue 19))
    (parameterizedExecutable, _) <- expectRight "parameterized let compile" (compileExactQuote quoteLimits StorePullbacks parameterizedLet)
    parameterizedRun <- expectRight "parameterized let run" (runExact parameterizedExecutable parameterValues (scalarValue 3))
    (parameterizedCotangent, parameterizedInputCotangent) <- expectRight "parameterized let VJP" (applyExactVJP parameterizedRun (scalarValue 23))
    assertEqual "parameterized let associated cotangent tree" [0, 23] (parameterScalars parameterizedCotangent)
    assertEqual "parameterized let input cotangent" (scalarValue 0) parameterizedInputCotangent

    firstExecutionReport <- expectRight "quotation JVP execution preflight" (preflightExactQuoteJVPExecution quoteLimits quotedSquare quotedSquareParameters quotedSquareParameters (scalarValue 3) (scalarValue 1))
    secondExecutionReport <- expectRight "repeated quotation JVP execution preflight" (preflightExactQuoteJVPExecution quoteLimits quotedSquare quotedSquareParameters quotedSquareParameters (scalarValue 3) (scalarValue 1))
    assertEqual "deterministic quotation execution report" firstExecutionReport secondExecutionReport

    (oraclePrimal, oracleTangent) <-
        expectRight
            "bounded quotation primal/JVP"
            ( interpretExactQuoteJVP
                quoteLimits
                quotedSquare
                quotedSquareParameters
                quotedSquareParameters
                (scalarValue 3)
                (scalarValue 1)
            )
    assertEqual "quoted square exact primal" (scalarValue 9) oraclePrimal
    assertEqual "quoted square formal JVP" (scalarValue 6) oracleTangent

    (executable, compilationReport) <- expectRight "compile exact quotation" (compileExactQuote quoteLimits StorePullbacks quotedSquare)
    assertEqual "quotation compile report carries preflight" (quoteReportCounts <$> preflightQuote quoteLimits quotedSquare) (Right (quoteReportCounts (quoteCompilationPreflight compilationReport)))
    run <- expectRight "run exact quotation" (runExact executable quotedSquareParameters (scalarValue 3))
    assertEqual "quotation direct/lowered primal" oraclePrimal (exactRunOutput run)
    (parameterGradient, inputGradient) <- expectRight "quotation diagonal VJP" (applyExactVJP run (scalarValue 5))
    assertEqual "quotation parameter cotangent extent" [] (parameterScalars parameterGradient)
    assertEqual "quotation diagonal cotangent addition" (scalarValue 30) inputGradient
    let forwardPairing = 5 * scalarFromValue oracleTangent
        reversePairing = scalarFromValue inputGradient
    assertEqual "quotation finite-coordinate JVP/VJP pairing" forwardPairing reversePairing

    -- Each constant fits eight bits, but the evaluated bound does not.  The
    -- body ignores it; call-by-value still reports the bound failure.
    let failingBound = compose (fanout (constantScalar SScalar (16 :: Rational)) (constantScalar SScalar 16)) multiplyScalar
        failingQuote = withQuoteScope $ \scope ->
            let oldPath = pathLeft scope (pathHere SScalar) SScalar
             in letQuote scope (quoteProgram failingBound) (project oldPath)
        failingParameters = parameterProduct (parameterProduct noParameters squareParameters) noParameters
        activeLimits = quotationLimits 1000 64 32 8 128 32 64 128 1000 100 3000 8
    case interpretExactQuote activeLimits failingQuote failingParameters (scalarValue 1) of
        Left (QuoteRationalMagnitudeLimitExceeded "multiply" 0 8) -> pure ()
        other -> failTest ("unused bound did not fail first: " ++ show other)
    (failingExecutable, _) <-
        expectRight
            "compile unused failing bound"
            (compileExactQuote activeLimits StorePullbacks failingQuote)
    case runExact failingExecutable failingParameters (scalarValue 1) of
        Left problem | "RationalMagnitudeLimitExceeded \"multiply\"" `contains` show problem -> pure ()
        other -> failTest ("lowered unused bound did not fail: " ++ showEither other)

quotationBudgets :: IO ()
quotationBudgets = do
    usedOnceReport <- expectRight "used-once quotation account" (preflightQuote quoteLimits usedOnceQuote)
    unusedReport <- expectRight "unused quotation account" (preflightQuote quoteLimits unusedQuote)
    assertEqual "used-once quotation account" [5, 3, 2, 1, 5, 3, 2, 5, 11, 12, 38, 0] (quoteReportCounts usedOnceReport)
    assertEqual "unused quotation account" [6, 3, 2, 2, 5, 3, 2, 5, 11, 12, 39, 0] (quoteReportCounts unusedReport)

    report <- expectRight "complete quotation account" (preflightQuote quoteLimits quotedSquare)
    let exact = reportLimits report
    exactReport <- expectRight "exact quotation budgets" (preflightQuote exact quotedSquare)
    assertEqual "deterministic exact quotation account" (quoteReportCounts report) (quoteReportCounts exactReport)
    (_, firstCompilationReport) <- expectRight "exact-budget quotation compile" (compileExactQuote exact StorePullbacks quotedSquare)
    (_, secondCompilationReport) <- expectRight "repeated exact-budget quotation compile" (compileExactQuote exact StorePullbacks quotedSquare)
    assertEqual "deterministic quotation compilation report" firstCompilationReport secondCompilationReport
    executionReport <- expectRight "complete execution account" (preflightExactQuoteJVPExecution quoteLimits quotedSquare quotedSquareParameters quotedSquareParameters (scalarValue 3) (scalarValue 1))
    _ <- expectRight "exact execution account" (preflightExactQuoteJVPExecution (reportLimits executionReport) quotedSquare quotedSquareParameters quotedSquareParameters (scalarValue 3) (scalarValue 1))
    case preflightExactQuoteJVPExecution (setTraversal executionReport) quotedSquare quotedSquareParameters quotedSquareParameters (scalarValue 3) (scalarValue 1) of
        Left (QuoteTraversalLimitExceeded active required) | active + 1 == required -> pure ()
        other -> failTest ("execution traversal one below: " ++ show other)
    expectQuoteFailure "traversal" (QuoteTraversalLimitExceeded (quoteTraversalWork report - 1) (quoteTraversalWork report)) (setTraversal report)
    expectQuoteFailure "quote nodes" (QuoteNodeLimitExceeded (quoteNodeCount report - 1) (quoteNodeCount report)) (setNodes report)
    expectQuoteFailure "source depth" (QuoteSourceDepthLimitExceeded (quoteSourceDepth report - 1) (quoteSourceDepth report)) (setSourceDepth report)
    expectQuoteFailure "path depth" (QuotePathDepthLimitExceeded (quoteMaximumPathDepth report - 1) (quoteMaximumPathDepth report)) (setPathDepth report)
    expectQuoteFailure "target nodes" (QuoteTargetNodeLimitExceeded (quotePredictedTargetNodes report - 1) (quotePredictedTargetNodes report)) (setTargetNodes report)
    expectQuoteFailure "target depth" (QuoteTargetDepthLimitExceeded (quotePredictedTargetDepth report - 1) (quotePredictedTargetDepth report)) (setTargetDepth report)
    expectQuoteFailure "coordinate extent" (QuoteCoordinateExtentLimitExceeded (quoteMaximumCoordinateExtent report - 1) (quoteMaximumCoordinateExtent report)) (setCoordinates report)
    expectQuoteFailure "transformed nodes" (QuoteTransformedNodeLimitExceeded (quoteTransformedNodes report - 1) (quoteTransformedNodes report)) (setTransformed report)
    expectQuoteFailure "allocation" (QuoteAllocationLimitExceeded (quoteAllocationCount report - 1) (quoteAllocationCount report)) (setAllocation report)
    expectQuoteFailure "runtime" (QuoteRuntimeWorkLimitExceeded (quoteRuntimeWork report - 1) (quoteRuntimeWork report)) (setRuntime report)
    expectQuoteFailure "total work" (QuoteTotalWorkLimitExceeded (quoteTotalWork report - 1) (quoteTotalWork report)) (setTotal report)

    let largeLiteral = quoteProgram (constantScalar SScalar (128 :: Rational))
        literalLimits = quotationLimits 20 2 3 1 2 2 2 1 10 3 43
    _ <- expectRight "quotation exact rational-bit preflight" (preflightQuote (literalLimits 8) largeLiteral)
    case preflightQuote (literalLimits 7) largeLiteral of
        Left (QuoteRationalMagnitudeLimitExceeded "source/constant-scalar" _ 7) -> pure ()
        other -> failTest ("quotation rational-bit preflight: " ++ show other)
    case preflightExactQuoteExecution (quotationLimits 20 2 3 1 2 2 2 1 10 10 60 7) (project (pathHere SScalar)) noParameters (scalarValue 128) of
        Left (QuoteRationalMagnitudeLimitExceeded "input" 0 7) -> pure ()
        other -> failTest ("execution rational-bit preflight: " ++ show other)

    -- The frozen entry order is traversal, total work, quotation nodes, then source depth.
    case preflightQuote (quotationLimits 0 0 0 0 0 0 0 0 0 0 0 0) quotedSquare of
        Left (QuoteTraversalLimitExceeded 0 1) -> pure ()
        other -> failTest ("traversal precedence changed: " ++ show other)
    case preflightQuote (quotationLimits 1 0 0 0 0 0 0 0 0 0 0 0) quotedSquare of
        Left (QuoteTotalWorkLimitExceeded 0 1) -> pure ()
        other -> failTest ("total-work precedence changed: " ++ show other)

    -- The node charge fails before the embedded bottom is descended into.
    let poisoned = quoteProgramAt (rootEnvironment SScalar) (error "preflight descended after exhaustion") :: Quote Rational 'Polynomial RootScalar 'NoParameters 'Scalar
        stopBeforeDescent = quotationLimits 1 0 1 0 0 0 0 0 0 0 1 0
    case preflightQuote stopBeforeDescent poisoned of
        Left (QuoteNodeLimitExceeded 0 1) -> pure ()
        other -> failTest ("charge-before-descent precedence changed: " ++ show other)

    let machineQuote = quoteProgram (identity (SVector @9223372036854775808))
    case preflightQuote quoteLimits machineQuote of
        Left (QuoteMachineExtentExceeded 9223372036854775808) -> pure ()
        other -> failTest ("quotation machine extent admission changed: " ++ show other)
  where
    values report =
        ( quoteTraversalWork report
        , quoteNodeCount report
        , quoteSourceDepth report
        , quoteMaximumPathDepth report
        , quotePredictedTargetNodes report
        , quotePredictedTargetDepth report
        , quoteMaximumCoordinateExtent report
        , quoteTransformedNodes report
        , quoteAllocationCount report
        , quoteRuntimeWork report
        , quoteTotalWork report
        )
    make (traversal, nodes, sourceDepth, pathDepth, targetNodes, targetDepth, coordinates, transformed, allocation, runtime, total) =
        quotationLimits traversal nodes sourceDepth pathDepth targetNodes targetDepth coordinates transformed allocation runtime total 16
    reportLimits = make . values
    setTraversal report = let (a, b, c, d, e, f, g, h, i, j, k) = values report in make (a - 1, b, c, d, e, f, g, h, i, j, k)
    setNodes report = let (a, b, c, d, e, f, g, h, i, j, k) = values report in make (a, b - 1, c, d, e, f, g, h, i, j, k)
    setSourceDepth report = let (a, b, c, d, e, f, g, h, i, j, k) = values report in make (a, b, c - 1, d, e, f, g, h, i, j, k)
    setPathDepth report = let (a, b, c, d, e, f, g, h, i, j, k) = values report in make (a, b, c, d - 1, e, f, g, h, i, j, k)
    setTargetNodes report = let (a, b, c, d, e, f, g, h, i, j, k) = values report in make (a, b, c, d, e - 1, f, g, h, i, j, k)
    setTargetDepth report = let (a, b, c, d, e, f, g, h, i, j, k) = values report in make (a, b, c, d, e, f - 1, g, h, i, j, k)
    setCoordinates report = let (a, b, c, d, e, f, g, h, i, j, k) = values report in make (a, b, c, d, e, f, g - 1, h, i, j, k)
    setTransformed report = let (a, b, c, d, e, f, g, h, i, j, k) = values report in make (a, b, c, d, e, f, g, h - 1, i, j, k)
    setAllocation report = let (a, b, c, d, e, f, g, h, i, j, k) = values report in make (a, b, c, d, e, f, g, h, i - 1, j, k)
    setRuntime report = let (a, b, c, d, e, f, g, h, i, j, k) = values report in make (a, b, c, d, e, f, g, h, i, j - 1, k)
    setTotal report = let (a, b, c, d, e, f, g, h, i, j, k) = values report in make (a, b, c, d, e, f, g, h, i, j, k - 1)
    expectQuoteFailure label expected active =
        case preflightQuote active quotedSquare of
            Left actual -> assertEqual (label ++ " one below") expected actual
            Right actual -> failTest (label ++ " one below unexpectedly passed: " ++ show actual)

quoteReportCounts :: QuoteReport -> [Integer]
quoteReportCounts report =
    map
        toInteger
        [ quoteTraversalWork report
        , quoteNodeCount report
        , quoteSourceDepth report
        , quoteMaximumPathDepth report
        , quotePredictedTargetNodes report
        , quotePredictedTargetDepth report
        , quoteMaximumCoordinateExtent report
        , quoteTransformedNodes report
        , quoteAllocationCount report
        , quoteRuntimeWork report
        , quoteTotalWork report
        , quoteMaximumRationalBits report
        ]

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
