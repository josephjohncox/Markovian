{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module NeuralDifferential (tests) where

import Control.Monad (forM, forM_, unless)
import GHC.TypeLits (KnownSymbol)
import Markovian.Autodiff
import Markovian.Backend.Neural.Dense (
    DenseError (..),
    DenseNetwork,
    denseForward,
    denseInputVJP,
    denseParameterVJP,
    mkDenseNetwork,
 )
import Paths_markovian_autodiff (getDataFileName)
import System.Exit (exitFailure)

-- This fixture is deliberately a closed 2 -> 2 tanh -> 2 network. Each scalar
-- parameter has a distinct nominal owner. The source language has no general
-- matrix primitive or neural-network callback.
type ScaledParameters owner =
    'ParameterProduct
        ('ParameterProduct (Owner owner 'Scalar) 'NoParameters)
        'NoParameters

type WeightedSumParameters weight0 weight1 =
    'ParameterProduct
        ('ParameterProduct (ScaledParameters weight0) (ScaledParameters weight1))
        'NoParameters

type DenseRowParameters weight0 weight1 bias =
    'ParameterProduct
        ( 'ParameterProduct
            (WeightedSumParameters weight0 weight1)
            (Owner bias 'Scalar)
        )
        'NoParameters

type DenseLayerParameters weight00 weight01 bias0 weight10 weight11 bias1 =
    'ParameterProduct
        (DenseRowParameters weight00 weight01 bias0)
        (DenseRowParameters weight10 weight11 bias1)

type HiddenParameters =
    'ParameterProduct
        (DenseLayerParameters "hidden-0-weight-0" "hidden-0-weight-1" "hidden-0-bias" "hidden-1-weight-0" "hidden-1-weight-1" "hidden-1-bias")
        ('ParameterProduct 'NoParameters 'NoParameters)

type MultilayerParameters =
    'ParameterProduct
        HiddenParameters
        (DenseLayerParameters "output-0-weight-0" "output-0-weight-1" "output-0-bias" "output-1-weight-0" "output-1-weight-1" "output-1-bias")

type Pair = 'Product 'Scalar 'Scalar

tests :: IO ()
tests = do
    network <- expectRight "manual multilayer network" (mkDenseNetwork 2 [2] 2 denseParameterValues)
    manualOutput <- expectRight "manual multilayer primal" (denseForward network inputValues)
    stored <- checkPolicy network manualOutput StorePullbacks
    recomputed <- checkPolicy network manualOutput RecomputePullbacks
    assertApproxList "stored/recomputed multilayer primal" (evidenceOutput stored) (evidenceOutput recomputed)
    assertNestedApprox "stored/recomputed multilayer input VJPs" (evidenceInputVJPs stored) (evidenceInputVJPs recomputed)
    assertNestedApprox "stored/recomputed multilayer parameter VJPs" (evidenceParameterVJPs stored) (evidenceParameterVJPs recomputed)
    checkReportGolden (evidenceReport stored) (evidenceReport recomputed)
    boundaryFailures network
    putStrLn "PASS: bounded multilayer neural differential"

data PolicyEvidence = PolicyEvidence
    { evidenceOutput :: ![Double]
    , evidenceInputVJPs :: ![[Double]]
    , evidenceParameterVJPs :: ![[Double]]
    , evidenceReport :: !String
    }

checkPolicy :: DenseNetwork -> [Double] -> TapePolicy -> IO PolicyEvidence
checkPolicy network manualOutput policy = do
    executable <- expectRight (show policy ++ " compile multilayer fixture") (compileDoubleSmooth multilayerLimits policy multilayerProgram)
    duplicateExecutable <- expectRight (show policy ++ " repeat compile multilayer fixture") (compileDoubleSmooth multilayerLimits policy multilayerProgram)
    let report = renderCompileReport (doubleCompileReport executable)
    assertEqual (show policy ++ " deterministic compile report") report (renderCompileReport (doubleCompileReport duplicateExecutable))
    input <- pairValue inputValues
    run <- expectRight (show policy ++ " run multilayer fixture") (runDouble executable multilayerParameterValue input)
    let automaticOutput = valueScalars (doubleRunOutput run)
    assertApproxList (show policy ++ " manual/autodiff all primal coordinates") manualOutput automaticOutput
    gradients <-
        forM (zip [0 :: Int ..] outputBasisSeeds) $ \(outputIndex, seedValues) -> do
            assertApproxList (seedLabel policy outputIndex ++ " all primal coordinates") manualOutput automaticOutput
            seed <- pairValue seedValues
            (automaticParameters, automaticInput) <- expectRight (seedLabel policy outputIndex ++ " autodiff VJP") (applyDoubleVJP run seed)
            repeated <- expectRight (seedLabel policy outputIndex ++ " repeated tape VJP") (applyDoubleTape (doubleRunTape run) seed)
            assertEqual (seedLabel policy outputIndex ++ " reusable tape") (automaticParameters, automaticInput) repeated
            manualParametersDense <- expectRight (seedLabel policy outputIndex ++ " manual parameter VJP") (denseParameterVJP network inputValues seedValues)
            manualInput <- expectRight (seedLabel policy outputIndex ++ " manual input VJP") (denseInputVJP network inputValues seedValues)
            finiteInputs <- finiteDifferenceCoordinates (objective network seedValues) inputValues
            finiteParametersDense <- finiteDifferenceCoordinates (parameterObjective seedValues) denseParameterValues
            let automaticParameterValues = parameterScalars automaticParameters
                automaticInputValues = valueScalars automaticInput
                manualParameters = denseToAutodiffOrder manualParametersDense
                finiteParameters = denseToAutodiffOrder finiteParametersDense
            assertCoordinateLists (seedLabel policy outputIndex ++ " input manual/autodiff") inputLabels manualInput automaticInputValues
            assertCoordinateLists (seedLabel policy outputIndex ++ " input finite/autodiff") inputLabels finiteInputs automaticInputValues
            assertCoordinateLists (seedLabel policy outputIndex ++ " input finite/manual") inputLabels finiteInputs manualInput
            assertCoordinateLists (seedLabel policy outputIndex ++ " parameter manual/autodiff") autodiffParameterLabels manualParameters automaticParameterValues
            assertCoordinateLists (seedLabel policy outputIndex ++ " parameter finite/autodiff") autodiffParameterLabels finiteParameters automaticParameterValues
            assertCoordinateLists (seedLabel policy outputIndex ++ " parameter finite/manual") autodiffParameterLabels finiteParameters manualParameters
            pure (automaticInputValues, automaticParameterValues)
    pure
        PolicyEvidence
            { evidenceOutput = automaticOutput
            , evidenceInputVJPs = fmap fst gradients
            , evidenceParameterVJPs = fmap snd gradients
            , evidenceReport = report
            }

multilayerLimits :: CompilerLimits
multilayerLimits = compilerLimits 512 256 64 12 256 256 1024 64 100000 256

multilayerProgram :: Program Double 'Smooth MultilayerParameters Pair Pair
multilayerProgram = compose hiddenStage outputLayer
  where
    hiddenStage =
        compose
            ( fanout
                (denseRow @"hidden-0-weight-0" @"hidden-0-weight-1" @"hidden-0-bias")
                (denseRow @"hidden-1-weight-0" @"hidden-1-weight-1" @"hidden-1-bias")
            )
            (parallel tanhScalar tanhScalar)
    outputLayer =
        fanout
            (denseRow @"output-0-weight-0" @"output-0-weight-1" @"output-0-bias")
            (denseRow @"output-1-weight-0" @"output-1-weight-1" @"output-1-bias")

denseRow ::
    forall weight0 weight1 bias.
    (KnownSymbol weight0, KnownSymbol weight1, KnownSymbol bias) =>
    Program Double 'Smooth (DenseRowParameters weight0 weight1 bias) Pair 'Scalar
denseRow = compose (fanout weightedSum biasValue) addScalar
  where
    weightedSum = compose (fanout (scaledCoordinate @weight0 True) (scaledCoordinate @weight1 False)) addScalar
    biasValue = parameter @bias (SProduct SScalar SScalar) SScalar

scaledCoordinate ::
    forall owner.
    (KnownSymbol owner) =>
    Bool ->
    Program Double 'Smooth (ScaledParameters owner) Pair 'Scalar
scaledCoordinate selectLeft = compose (fanout weight coordinate) multiplyScalar
  where
    pairShape = SProduct SScalar SScalar
    weight = parameter @owner pairShape SScalar
    coordinate
        | selectLeft = first SScalar SScalar
        | otherwise = second SScalar SScalar

multilayerParameterValue :: ParameterValue Double MultilayerParameters
multilayerParameterValue = case denseParameterValues of
    [hidden00, hidden01, hidden10, hidden11, hiddenBias0, hiddenBias1, output00, output01, output10, output11, outputBias0, outputBias1] ->
        parameterProduct
            ( parameterProduct
                ( parameterProduct
                    (rowParameters hidden00 hidden01 hiddenBias0)
                    (rowParameters hidden10 hidden11 hiddenBias1)
                )
                (parameterProduct noParameters noParameters)
            )
            ( parameterProduct
                (rowParameters output00 output01 outputBias0)
                (rowParameters output10 output11 outputBias1)
            )
    _ -> error "internal multilayer parameter fixture length changed"

rowParameters :: Double -> Double -> Double -> ParameterValue Double (DenseRowParameters weight0 weight1 bias)
rowParameters weight0 weight1 bias =
    parameterProduct
        ( parameterProduct
            ( parameterProduct
                (parameterProduct (scaledParameters weight0) (scaledParameters weight1))
                noParameters
            )
            (ownedParameters (scalarValue bias))
        )
        noParameters

scaledParameters :: Double -> ParameterValue Double (ScaledParameters owner)
scaledParameters value = parameterProduct (parameterProduct (ownedParameters (scalarValue value)) noParameters) noParameters

-- markovian-neural stores each layer as all row-major weights followed by all
-- biases. The source owner tree is row-local, so this explicit permutation is
-- part of the fixture oracle rather than an implicit layout claim.
denseToAutodiffOrder :: [value] -> [value]
denseToAutodiffOrder values = fmap (values !!) [0, 1, 4, 2, 3, 5, 6, 7, 10, 8, 9, 11]

denseParameterValues :: [Double]
denseParameterValues = [0.2, -0.3, 0.4, 0.1, 0.05, -0.2, -0.6, 0.7, 0.8, -0.5, 0.15, -0.25]

inputValues :: [Double]
inputValues = [0.25, -0.75]

outputBasisSeeds :: [[Double]]
outputBasisSeeds = [[1, 0], [0, 1]]

inputLabels :: [String]
inputLabels = ["input/0", "input/1"]

autodiffParameterLabels :: [String]
autodiffParameterLabels =
    [ "hidden/0/weight/0"
    , "hidden/0/weight/1"
    , "hidden/0/bias"
    , "hidden/1/weight/0"
    , "hidden/1/weight/1"
    , "hidden/1/bias"
    , "output/0/weight/0"
    , "output/0/weight/1"
    , "output/0/bias"
    , "output/1/weight/0"
    , "output/1/weight/1"
    , "output/1/bias"
    ]

parameterObjective :: [Double] -> [Double] -> Either DifferenceError Double
parameterObjective seed parameters = do
    network <- mapObjectiveError (mkDenseNetwork 2 [2] 2 parameters)
    objective network seed inputValues

objective :: DenseNetwork -> [Double] -> [Double] -> Either DifferenceError Double
objective network seed inputs = do
    output <- mapObjectiveError (denseForward network inputs)
    checkedObjective (sum (zipWith (*) seed output))

data DifferenceError
    = NonFinitePerturbation !Int !Double !Double
    | NeuralObjectiveFailure !DenseError
    | NonFiniteObjective !Double
    deriving (Eq, Show)

finiteDifferenceCoordinates :: ([Double] -> Either DifferenceError Double) -> [Double] -> IO [Double]
finiteDifferenceCoordinates evaluate coordinates =
    forM [0 .. length coordinates - 1] $ \index ->
        expectRight ("central finite difference coordinate " ++ show index) (centralCoordinate evaluate coordinates index)

centralCoordinate :: ([Double] -> Either DifferenceError Double) -> [Double] -> Int -> Either DifferenceError Double
centralCoordinate evaluate coordinates index = do
    let point = coordinates !! index
        step = 1e-6 * max 1 (abs point)
        abovePoint = point + step
        belowPoint = point - step
    unlessFinitePerturbation index abovePoint belowPoint
    above <- evaluate (replace index abovePoint coordinates)
    below <- evaluate (replace index belowPoint coordinates)
    checkedObjective ((above - below) / (2 * step))

unlessFinitePerturbation :: Int -> Double -> Double -> Either DifferenceError ()
unlessFinitePerturbation index above below
    | finite above && finite below = Right ()
    | otherwise = Left (NonFinitePerturbation index above below)

checkedObjective :: Double -> Either DifferenceError Double
checkedObjective value
    | finite value = Right value
    | otherwise = Left (NonFiniteObjective value)

mapObjectiveError :: Either DenseError value -> Either DifferenceError value
mapObjectiveError = either (Left . NeuralObjectiveFailure) Right

finite :: Double -> Bool
finite value = not (isNaN value || isInfinite value)

replace :: Int -> value -> [value] -> [value]
replace selected replacement =
    zipWith (\index value -> if index == selected then replacement else value) [0 :: Int ..]

pairValue :: [Double] -> IO (Value Double Pair)
pairValue [left, right] = pure (productValue (scalarValue left) (scalarValue right))
pairValue values = failTest ("pair fixture has " ++ show (length values) ++ " coordinates")

boundaryFailures :: DenseNetwork -> IO ()
boundaryFailures network = do
    case mkDenseNetwork 2 [2] 2 (init denseParameterValues) of
        Left (DenseParameterShapeMismatch 12 11) -> pure ()
        other -> failTest ("multilayer parameter shape failure changed: " ++ show other)
    case denseForward network [0.25] of
        Left (DenseInputShapeMismatch 2 1) -> pure ()
        other -> failTest ("multilayer input shape failure changed: " ++ show other)
    case denseParameterVJP network inputValues [1] of
        Left (DenseOutputCotangentShapeMismatch 2 1) -> pure ()
        other -> failTest ("multilayer cotangent shape failure changed: " ++ show other)
    case compileDoubleSmooth multilayerLimits StorePullbacks (denseRow @"duplicate-owner" @"duplicate-owner" @"distinct-bias") of
        Left problem
            | "DuplicateIndependentOwner" `contains` show problem -> pure ()
        other -> failTest ("multilayer duplicate owner failure changed: " ++ showEither other)
    let maximumFinite = encodeFloat (2 ^ (53 :: Int) - 1) (1024 - 53) :: Double
        singletonObjective values = case values of
            [value] -> checkedObjective value
            _ -> Left (NonFiniteObjective (0 / 0))
    case centralCoordinate singletonObjective [maximumFinite] 0 of
        Left (NonFinitePerturbation 0 above below)
            | isInfinite above && finite below -> pure ()
        other -> failTest ("nonfinite finite-difference perturbation escaped: " ++ show other)

checkReportGolden :: String -> String -> IO ()
checkReportGolden stored recomputed = do
    path <- getDataFileName "test/golden/multilayer-neural-report.txt"
    golden <- readFile path
    let actual =
            unlines
                [ "multilayer-neural-differential-report"
                , "topology: 2 -> 2 tanh -> 2"
                , "output-basis-seeds: 2"
                , "primal-coordinates-per-seed: 2"
                , "input-vjp-coordinates-per-seed: 2"
                , "parameter-vjp-coordinates-per-seed: 12"
                , "parameter-layout: explicit-owner-tree-to-neural-row-major-permutation"
                , "finite-difference-step: 1.0e-6 * max 1 abs(coordinate)"
                , "absolute-tolerance: 2.0e-10"
                , "relative-tolerance: 2.0e-8"
                , "oracle: markovian-neural manual dense runtime and independent central differences"
                ]
                ++ "stored\n"
                ++ stored
                ++ "recomputed\n"
                ++ recomputed
    assertEqual "multilayer deterministic report golden" golden actual

assertCoordinateLists :: String -> [String] -> [Double] -> [Double] -> IO ()
assertCoordinateLists label labels expected actual = do
    unless (length labels == length expected && length expected == length actual) (failTest (label ++ ": unequal coordinate counts"))
    forM_ (zip3 labels expected actual) $ \(coordinate, wanted, got) -> assertApprox (label ++ " " ++ coordinate) wanted got

assertNestedApprox :: String -> [[Double]] -> [[Double]] -> IO ()
assertNestedApprox label expected actual = do
    unless (length expected == length actual) (failTest (label ++ ": unequal seed counts"))
    forM_ (zip3 [0 :: Int ..] expected actual) $ \(index, wanted, got) -> assertApproxList (label ++ " seed " ++ show index) wanted got

assertApproxList :: String -> [Double] -> [Double] -> IO ()
assertApproxList label expected = assertCoordinateLists label (fmap show [0 :: Int .. length expected - 1]) expected

assertApprox :: String -> Double -> Double -> IO ()
assertApprox label expected actual =
    let absoluteTolerance = 2e-10
        relativeTolerance = 2e-8
        difference = abs (expected - actual)
        tolerance = absoluteTolerance + relativeTolerance * max (abs expected) (abs actual)
     in unless (finite expected && finite actual && difference <= tolerance) (failTest (label ++ ": expected " ++ show expected ++ ", got " ++ show actual ++ ", tolerance " ++ show tolerance))

assertEqual :: (Eq value, Show value) => String -> value -> value -> IO ()
assertEqual label expected actual = unless (expected == actual) (failTest (label ++ ": expected " ++ show expected ++ ", got " ++ show actual))

expectRight :: (Show error) => String -> Either error value -> IO value
expectRight _ (Right value) = pure value
expectRight label (Left problem) = failTest (label ++ ": " ++ show problem)

seedLabel :: TapePolicy -> Int -> String
seedLabel policy outputIndex = show policy ++ " output-basis/" ++ show outputIndex

contains :: String -> String -> Bool
contains needle haystack = any (needle `prefixOf`) (tails haystack)
  where
    prefixOf [] _ = True
    prefixOf _ [] = False
    prefixOf (x : xs) (y : ys) = x == y && prefixOf xs ys
    tails [] = [[]]
    tails values@(_ : rest) = values : tails rest

showEither :: (Show error) => Either error value -> String
showEither (Left problem) = "Left " ++ show problem
showEither (Right _) = "Right <value>"

failTest :: String -> IO a
failTest message = do
    putStrLn ("FAIL: " ++ message)
    exitFailure
