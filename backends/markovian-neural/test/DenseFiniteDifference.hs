module DenseFiniteDifference (tests) where

import Markovian.Backend.Neural (
    DenseError (..),
    DenseNetwork,
    OptimizerError (..),
    addFiniteDouble,
    applySGD,
    approximatelyEqual,
    denseForward,
    denseInputVJP,
    denseParameterVJP,
    denseParameters,
    finiteDouble,
    finiteDoubleValue,
    mkDenseNetwork,
    mkNumericalTolerance,
    mkSGD,
 )
import TestSupport (assert, assertVectorClose, assertVectorCloseWith, requireRight)

tests :: IO ()
tests = do
    vjpFiniteDifferences
    linearAndSGD
    rejectionChecks
    putStrLn "PASS: dense finite differences"

vjpFiniteDifferences :: IO ()
vjpFiniteDifferences = do
    checkVJP "linear" [] [0.2, -0.3, 0.1, -0.4, 0.5, -0.2]
    checkVJP "one hidden layer" [3] [0.2, -0.3, 0.1, -0.4, 0.5, -0.2, 0.7, -0.1, 0.05, 0.3, -0.6, 0.2, -0.5, 0.4, 0.1, -0.2, 0.3]
    checkVJP "two hidden layers" [2, 2] [fromIntegral index / 20 - 0.4 | index <- [0 :: Int .. 17]]

checkVJP :: String -> [Int] -> [Double] -> IO ()
checkVJP label hidden parameters = do
    let inputs = [0.25, -0.75]
        cotangent = [0.7, -1.2]
    network <- requireRight (label ++ " dense network") (mkDenseNetwork 2 hidden 2 parameters)
    analyticParameters <- requireRight (label ++ " parameter VJP") (denseParameterVJP network inputs cotangent)
    numericalParameters <-
        traverse
            (centralParameterObjective hidden parameters inputs cotangent)
            [0 .. length parameters - 1]
    assertVectorCloseWith (label ++ " dense parameter VJP") 2e-10 2e-8 analyticParameters numericalParameters

    analyticInputs <- requireRight (label ++ " input VJP") (denseInputVJP network inputs cotangent)
    numericalInputs <-
        traverse
            ( \index -> do
                let point = inputs !! index
                    step = scaledStep point
                above <- objective network (replace index (point + step) inputs) cotangent
                below <- objective network (replace index (point - step) inputs) cotangent
                pure ((above - below) / (2 * step))
            )
            [0 .. length inputs - 1]
    assertVectorCloseWith (label ++ " dense input VJP") 2e-10 2e-8 analyticInputs numericalInputs

linearAndSGD :: IO ()
linearAndSGD = do
    network <- requireRight "linear dense" (mkDenseNetwork 2 [] 1 [2, -1, 0.5])
    output <- requireRight "linear forward" (denseForward network [3, 4])
    assertVectorClose "linear output" 1e-15 [2.5] output
    optimizer <- requireRight "SGD" (mkSGD 0.1)
    updated <- requireRight "SGD update" (applySGD optimizer [1, -2, 3] network)
    assertVectorClose "SGD parameters" 1e-15 [1.9, -0.8, 0.2] (denseParameters updated)
    finiteOne <- requireRight "finite one" (finiteDouble 1)
    finiteTwo <- requireRight "finite two" (finiteDouble 2)
    finiteThree <- requireRight "checked finite addition" (addFiniteDouble finiteOne finiteTwo)
    assert "opaque finite arithmetic" (finiteDoubleValue finiteThree == 3)
    case finiteDouble (0 / 0) of
        Left _ -> pure ()
        Right _ -> assert "non-finite opaque scalar accepted" False
    tolerance <- requireRight "tolerance" (mkNumericalTolerance 1e-12 1e-9)
    close <- requireRight "approximate comparison" (approximatelyEqual tolerance 1000 (1000 + 5e-7))
    assert "absolute-plus-relative tolerance" close

rejectionChecks :: IO ()
rejectionChecks = do
    case mkDenseNetwork 2 [0] 1 [] of
        Left (InvalidDenseHiddenSize 0 0) -> pure ()
        result -> assert ("invalid hidden width accepted: " ++ show result) False
    network <- requireRight "rejection network" (mkDenseNetwork 1 [] 1 [0, 0])
    optimizer <- requireRight "rejection SGD" (mkSGD 0.1)
    case applySGD optimizer [0 / 0, 0] network of
        Left (SGDNumericFailure _) -> pure ()
        result -> assert ("non-finite gradient accepted: " ++ show result) False
    case denseForward network [] of
        Left (DenseInputShapeMismatch 1 0) -> pure ()
        result -> assert ("wrong dense input shape accepted: " ++ show result) False
    case denseParameterVJP network [1] [] of
        Left (DenseOutputCotangentShapeMismatch 1 0) -> pure ()
        result -> assert ("wrong dense cotangent shape accepted: " ++ show result) False

centralParameterObjective :: [Int] -> [Double] -> [Double] -> [Double] -> Int -> IO Double
centralParameterObjective hidden parameters inputs cotangent index = do
    let point = parameters !! index
        step = scaledStep point
    aboveNetwork <- requireRight "parameter above" (mkDenseNetwork 2 hidden 2 (replace index (point + step) parameters))
    belowNetwork <- requireRight "parameter below" (mkDenseNetwork 2 hidden 2 (replace index (point - step) parameters))
    above <- objective aboveNetwork inputs cotangent
    below <- objective belowNetwork inputs cotangent
    pure ((above - below) / (2 * step))

objective :: DenseNetwork -> [Double] -> [Double] -> IO Double
objective network inputs cotangent = do
    output <- requireRight "dense objective" (denseForward network inputs)
    pure (sum (zipWith (*) output cotangent))

scaledStep :: Double -> Double
scaledStep point = 1e-6 * max 1 (abs point)

replace :: Int -> Double -> [Double] -> [Double]
replace selected replacement =
    zipWith (\index value -> if index == selected then replacement else value) [0 :: Int ..]
