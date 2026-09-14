{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GraphTests (graphContractTests) where

import Control.Exception (bracket)
import Control.Monad (forM_, unless, when)
import Markovian.Backend.GPU
import Markovian.Backend.GPU.Graph
import Markovian.Tensor
import Markovian.Tensor.Affine
import Numeric.Natural (Natural)
import Paths_markovian_gpu (getDataFileName)
import System.Environment (lookupEnv, setEnv, unsetEnv)

sessionLimits :: SessionLimits
sessionLimits = tensorSessionLimits 4 1024 1000000 8000000 64000000 10000 100000000

roomy :: GraphLimits
roomy = either (error . show) id (graphLimits 100000 1024 1000000 100000000 100000000 100000000 100000000 100000)

right :: (Show e) => Either e a -> IO a
right = either (fail . show) pure

perform :: (Show e) => Either e (IO a) -> IO a
perform = either (fail . show) id

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label wanted got = unless (wanted == got) (fail (label ++ ": expected " ++ show wanted ++ ", got " ++ show got))

assertExact :: String -> [Rational] -> [Double] -> IO ()
assertExact label expected actual = do
    assertEqual (label ++ " length") (length expected) (length actual)
    forM_ (zip3 [0 :: Int ..] expected actual) $ \(coordinate, exact, observed) -> do
        let wanted = fromRational exact
            tolerance = 2e-12 + 2e-12 * max (abs wanted) (abs observed)
        unless
            (not (isNaN observed || isInfinite observed) && abs (wanted - observed) <= tolerance)
            (fail (label ++ " coordinate " ++ show coordinate ++ ": " ++ show (wanted, observed)))

-- Independent forward-mode exact differentiation. No production graph,
-- preparation, view-address routine, reverse operation, or tape is used.
data Dual = Dual Rational Rational deriving (Eq, Show)
instance Num Dual where
    Dual a da + Dual b db = Dual (a + b) (da + db)
    Dual a da * Dual b db = Dual (a * b) (da * b + a * db)
    negate (Dual a da) = Dual (-a) (-da)
    fromInteger n = Dual (fromInteger n) 0
    abs (Dual a da) = Dual (abs a) (signum a * da)
    signum (Dual a _) = Dual (signum a) 0

exactProduct :: (Num a) => Int -> Int -> Int -> [a] -> [a] -> [a]
exactProduct rows inner columns left rightWords =
    [sum [left !! (r * inner + k) * rightWords !! (k * columns + c) | k <- [0 .. inner - 1]] | r <- [0 .. rows - 1], c <- [0 .. columns - 1]]

type Oracle = forall a. (Num a) => [[a]] -> [a]

oracleResults :: Oracle -> [[Double]] -> [Double] -> ([Rational], [[Rational]])
oracleResults oracle inputWords seedWords =
    ( oracle (map (map toRational) inputWords)
    , [[derivative input coordinate | coordinate <- [0 .. length wordsList - 1]] | (input, wordsList) <- zip [0 ..] inputWords]
    )
  where
    derivative input coordinate =
        let dualInputs =
                [ [ Dual (toRational word) (if i == input && j == coordinate then 1 else 0)
                  | (j, word) <- zip [0 :: Int ..] wordsList
                  ]
                | (i, wordsList) <- zip [0 :: Int ..] inputWords
                ]
            derivatives = [d | Dual _ d <- oracle dualInputs]
         in sum (zipWith (*) (map toRational seedWords) derivatives)

fixture :: String -> DeviceSelector -> Bool -> TensorSession region -> Graph '[] region r c -> FiniteTensor region 'F64 '[r, c] -> Oracle -> [[Double]] -> [Double] -> IO ()
fixture label selector hardware session graph seed oracle inputs seedWords = do
    forward <- right (prepareGraph roomy graph)
    reversePlan <- right (prepareGraphVJP roomy graph seed)
    let (expected, expectedGradients) = oracleResults oracle inputs seedWords
    forM_ (CPUOnly : [RequireCUDA selector | hardware]) $ \request -> do
        actual <- runGraph session request forward >>= right
        assertExact (label ++ " " ++ show request ++ " forward dyadic refinement") expected (graphResultValues actual)
        reverseResult <- runGraphVJP session request reversePlan >>= right
        assertExact (label ++ " VJP forward") expected (graphVJPValues reverseResult)
        let forwardExecution = graphResultReport actual
            reverseExecution = graphVJPReport reverseResult
            expectedTimings plan =
                if request == CPUOnly
                    then 0
                    else
                        fromIntegral (graphPlanForwardLaunches plan + graphPlanReverseLaunches plan)
        forM_ [forwardExecution, reverseExecution] $ \execution -> do
            assertEqual
                (label ++ " timings match admitted launch schedule")
                (expectedTimings (graphExecutionPlan execution))
                (length (graphExecutionTransferInclusiveMilliseconds execution))
            assertEqual
                (label ++ " report input declarations")
                (fromIntegral (length inputs))
                (graphPlanInputs (graphExecutionPlan execution))
        assertEqual (label ++ " all declarations") (length inputs) (length (graphVJPGradients reverseResult))
        forM_ (zip3 [0 :: Natural ..] expectedGradients (graphVJPGradients reverseResult)) $ \(index, wanted, gradient) -> do
            assertEqual "declaration ordinal" index (graphGradientInput gradient)
            assertEqual "gradient shape accounts every coordinate" (fromIntegral (length wanted)) (product (graphGradientShape gradient))
            assertExact (label ++ " all-coordinate input " ++ show index) wanted (graphGradientValues gradient)
        -- A second independent direction checks the complete scalar pairing.
        let direction = [[fromIntegral (1 + i + j) / 8 | j <- [0 .. length xs - 1]] | (i, xs) <- zip [0 :: Int ..] inputs] :: [[Rational]]
            inputDuals = zipWith (zipWith (Dual . toRational)) inputs direction
            jvp = [d | Dual _ d <- oracle inputDuals]
            forwardPairing = sum (zipWith (*) (map toRational seedWords) jvp)
            reversePairing = sum (zipWith (\ds gradient -> sum (zipWith (*) (map fromRational ds) (graphGradientValues gradient))) direction (graphVJPGradients reverseResult))
        assertExact (label ++ " all-coordinate pairing") [forwardPairing] [reversePairing]

graphContractTests :: DeviceSelector -> Bool -> IO ()
graphContractTests selector hardware = do
    admissionTests
    session <- withTensorSession sessionLimits $ \tensorSession -> do
        let aWords = [1, 2, 3, 4, 5, 6]
            bWords = [0.5, -1, 2, 0.25, 1, -0.5]
            cWords = [1, 2, 0.5, -1]
            seedWords = [0.5, -1, 2, 0.25]
        a <- fst <$> (finiteTensorFromList tensorSession (knownShape @'[2, 3]) aWords >>= right)
        b <- fst <$> (finiteTensorFromList tensorSession (knownShape @'[3, 2]) bWords >>= right)
        c <- fst <$> (finiteTensorFromList tensorSession (knownShape @'[2, 2]) cWords >>= right)
        seed <- fst <$> (finiteTensorFromList tensorSession (knownShape @'[2, 2]) seedWords >>= right)
        let chain = matrixMultiply (matrixMultiply (matrixInput a) (matrixInput b)) (matrixInput c)
            chainOracle [x, y, z] = exactProduct 2 2 2 (exactProduct 2 3 2 x y) z
            chainOracle _ = error "test input arity"
        fixture "two-product chain" selector hardware tensorSession chain seed chainOracle [aWords, bWords, cWords] seedWords
        let three =
                matrixLet (matrixInput c) $
                    matrixMultiply (matrixMultiply (matrixMultiply (matrixRef matrixHere) (matrixRef matrixHere)) (matrixRef matrixHere)) (matrixRef matrixHere)
            fourthPower [x] = exactProduct 2 2 2 (exactProduct 2 2 2 (exactProduct 2 2 2 x x) x) x
            fourthPower _ = error "test input arity"
        fixture "three products and shared input" selector hardware tensorSession three seed fourthPower [cWords] seedWords
        let shared = matrixLet (matrixInput c) (matrixMultiply (matrixRef matrixHere) (matrixRef matrixHere))
            square [x] = exactProduct 2 2 2 x x
            square _ = error "test input arity"
        fixture "shared square" selector hardware tensorSession shared seed square [cWords] seedWords
        let sharedProduct = matrixLet (matrixMultiply (matrixInput c) (matrixInput c)) (matrixMultiply (matrixRef matrixHere) (matrixRef matrixHere))
            sharedProductOracle [x, y] = let xy = exactProduct 2 2 2 x y in exactProduct 2 2 2 xy xy
            sharedProductOracle _ = error "test input arity"
        fixture "shared intermediate with distinct identical-storage declarations" selector hardware tensorSession sharedProduct seed sharedProductOracle [cWords, cWords] seedWords
        let unused = matrixLet (matrixInput a) (matrixInput c)
            unusedOracle [_, x] = x
            unusedOracle _ = error "test input arity"
        fixture "unused declaration has full zero gradient" selector hardware tensorSession unused seed unusedOracle [aWords, cWords] seedWords
        let reordered =
                matrixLet (matrixInput b) $
                    matrixLet (matrixInput a) $
                        matrixMultiply (matrixRef matrixHere) (matrixRef (matrixThere matrixHere))
            reorderedOracle [y, x] = exactProduct 2 3 2 x y
            reorderedOracle _ = error "test input arity"
        fixture "reordered lexical declarations" selector hardware tensorSession reordered seed reorderedOracle [bWords, aWords] seedWords
        planBoundaryTests seed shared
        affineTests selector hardware tensorSession a b seed aWords bWords seedWords
        zeroMultiplyTests selector hardware tensorSession c
        numericTests tensorSession
        when (hardware && gpuFaultInjectionCompiled) $ do
            faultTests selector tensorSession chain seed
            hostCleanupTest selector tensorSession
        pure (Right ())
    _ <- right session
    putStrLn "markovian-gpu: graph dyadic, all-coordinate VJP and budget tests passed"

admissionTests :: IO ()
admissionTests = do
    none <- right (graphLimits 0 0 0 0 0 0 0 0)
    -- Even the root constructor is unpaid; tensor/syntax thunks stay untouched.
    case prepareGraph none (error "unpaid graph" :: Graph '[] () 2 2) of
        Left (GraphLimitExceeded GraphSyntax 0 1) -> pure ()
        other -> fail ("syntax sentinel failed: " ++ showPlan other)
    case prepareGraph none (matrixInput (error "unpaid tensor") :: Graph '[] () 2 2) of
        Left (GraphLimitExceeded GraphSyntax 0 1) -> pure ()
        other -> fail ("input payload forced before admission: " ++ showPlan other)
    one <- right (graphLimits 1 0 0 0 0 0 0 0)
    case prepareGraph one (matrixMultiply (error "unpaid left") (error "unpaid right") :: Graph '[] () 2 2) of
        Left (GraphLimitExceeded GraphSyntax 1 2) -> pure ()
        other -> fail ("child sentinel failed: " ++ showPlan other)
    let cyclic = matrixMultiply cyclic cyclic :: Graph '[] () 2 2
    case prepareGraph one cyclic of
        Left (GraphLimitExceeded GraphSyntax 1 2) -> pure ()
        other -> fail ("recursive syntax did not stop at budget: " ++ showPlan other)
    forM_ [minBound .. maxBound] $ \field -> do
        let supplied = replaceAt (fromEnum field) (fromIntegral (maxBound :: Int) + 1) (replicate 8 0)
        case limitsFrom supplied of
            Left (GraphInvalidLimit actual _) -> assertEqual "configured limit precedence" field actual
            other -> fail ("huge configured limit accepted " ++ show other)

planBoundaryTests :: FiniteTensor region 'F64 '[2, 2] -> Graph '[] region 2 2 -> IO ()
planBoundaryTests seed graph = do
    forward <- right (prepareGraph roomy graph)
    reversePlan <- right (prepareGraphVJP roomy graph seed)
    -- Independent fixed square table: syntax=Let+Input+Multiply+2*(Ref+Here)=7.
    -- Forward: I=4, one S=12 call, publication=4, so H=8*(4+2*64)=1056;
    -- W=4+2*(16+4)=44. Reverse adds zero8, seed insertion4, publication4,
    -- transposes8, two S=12 calls120, edge sums8; snapshots additionally4.
    let expectedForward = GraphPlanReport 7 2 1 [2, 2] 96 1056 96 44 1 0 0 0
        expectedReverse = GraphPlanReport 7 2 1 [2, 2] 288 3520 96 176 1 2 0 8
    assertEqual "independent forward schedule count" expectedForward (preparedGraphReport forward)
    assertEqual "independent VJP schedule count" expectedReverse (preparedGraphVJPReport reversePlan)
    golden <- readFile =<< graphGoldenPath
    assertEqual "fixed plan golden" golden (renderGraphPlanReport expectedForward ++ renderGraphPlanReport expectedReverse)
    forM_ [(False, [7, 2, 4, 96, 1056, 96, 44, 1]), (True, [7, 2, 4, 288, 3520, 96, 176, 3])] $ \(reverseMode, exactCaps) -> do
        exact <- right (limitsFrom exactCaps)
        let prepareWith budget = if reverseMode then fmap preparedGraphVJPReport (prepareGraphVJP budget graph seed) else fmap preparedGraphReport (prepareGraph budget graph)
        _ <- right (prepareWith exact)
        forM_ [minBound .. maxBound] $ \field -> do
            let i = fromEnum field
                cap = exactCaps !! i - 1
            below <- right (limitsFrom (replaceAt i cap exactCaps))
            case prepareWith below of
                Left (GraphLimitExceeded actual actualCap _) -> do
                    assertEqual "one-below field" field actual
                    assertEqual "one-below cap" cap actualCap
                other -> fail ("one-below budget returned " ++ show other)
        allBelow <- right (limitsFrom (map (subtract 1) exactCaps))
        case prepareWith allBelow of
            Left (GraphLimitExceeded GraphSyntax _ _) -> pure ()
            other -> fail ("first competing limit changed " ++ show other)

-- Imported path resolver keeps golden reads independent of cwd/archive layout.
graphGoldenPath :: IO FilePath
graphGoldenPath = getDataFileName "test/golden/graph-plan.txt"

limitsFrom :: [Natural] -> Either GraphError GraphLimits
limitsFrom [a, b, c, d, e, f, g, h] = graphLimits a b c d e f g h
limitsFrom _ = error "test limit arity"

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n value xs = take n xs ++ value : drop (n + 1) xs

showPlan :: Either GraphError a -> String
showPlan (Left problem) = show problem
showPlan (Right _) = "accepted plan"

affineTests :: DeviceSelector -> Bool -> TensorSession region -> FiniteTensor region 'F64 '[2, 3] -> FiniteTensor region 'F64 '[3, 2] -> FiniteTensor region 'F64 '[2, 2] -> [Double] -> [Double] -> [Double] -> IO ()
affineTests selector hardware session a b seed aWords bWords seedWords = do
    policy <- right (affineLimits 4 1024 1000000 maxBound maxBound maxBound)
    budget <- right (affineBudget policy)
    perform $ withAffineMap budget (knownShape @'[2, 3]) (knownShape @'[2, 3]) 5 [-3, -1] $ \witness next _ -> do
        let (baseShape, viewShape, offset, strides) = affineMapDescriptor witness
        assertEqual "descriptor base" [2, 3] (shapeDimensions baseShape)
        assertEqual "descriptor view" [2, 3] (shapeDimensions viewShape)
        assertEqual "descriptor signed metadata" (5, [-3, -1]) (offset, strides)
        let graph = matrixMultiply (matrixView witness (matrixInput a)) (matrixInput b)
            oracle [x, y] = exactProduct 2 3 2 [x !! i | i <- [5, 4, 3, 2, 1, 0]] y
            oracle _ = error "test input arity"
        fixture "negative affine rectangle" selector hardware session graph seed oracle [aWords, bWords] seedWords
        perform $ permuteAffineMap next witness (knownShape @'[3, 2]) [1, 0] $ \transposed _ _ -> do
            let gram =
                    matrixLet (matrixInput a) $
                        matrixMultiply (matrixRef matrixHere) (matrixView transposed (matrixRef matrixHere))
                gramOracle [x] = exactProduct 2 3 2 x [x !! i | i <- [5, 2, 4, 1, 3, 0]]
                gramOracle _ = error "test input arity"
            fixture "permuted signed shared affine" selector hardware session gram seed gramOracle [aWords] seedWords
    perform $ withAffineMap budget (knownShape @'[2, 3]) (knownShape @'[2, 3]) 0 [3, 1] $ \baseMap next _ ->
        perform $ sliceAffineMap next baseMap (knownShape @'[2, 2]) [0, 0] [1, 2] $ \slicedMap _ _ -> do
            let graph = matrixView slicedMap (matrixInput a)
                oracle [x] = [x !! i | i <- [0, 2, 3, 5]]
                oracle _ = error "test input arity"
            fixture "strided slice zero-multiply" selector hardware session graph seed oracle [aWords] seedWords
            plan <- right (prepareGraphVJP roomy graph seed)
            -- I=6,O=4,N=10,V=4,B=6; geometry=16+6=22;
            -- forward H/W=8; reverse H=32,W=36; snapshots10.
            assertEqual
                "independent affine VJP table"
                (GraphPlanReport 2 2 1 [2, 2] 0 720 0 120 0 0 4 6)
                (preparedGraphVJPReport plan)

zeroMultiplyTests :: DeviceSelector -> Bool -> TensorSession region -> FiniteTensor region 'F64 '[2, 2] -> IO ()
zeroMultiplyTests selector hardware session tensor = do
    plan <- right (prepareGraph roomy (matrixInput tensor))
    cpu <- runGraph session CPUOnly plan >>= right
    assertEqual "zero-multiply CPU backend" CPURequested (graphExecutionBackend (graphResultReport cpu))
    assertEqual "zero-multiply CPU timings" [] (graphExecutionTransferInclusiveMilliseconds (graphResultReport cpu))
    if hardware
        then do
            cuda <- runGraph session (RequireCUDA selector) plan >>= right
            case graphExecutionBackend (graphResultReport cuda) of
                CUDASelected _ -> pure ()
                other -> fail ("zero-multiply required CUDA admission omitted " ++ show other)
            assertEqual "zero-multiply CUDA timings" [] (graphExecutionTransferInclusiveMilliseconds (graphResultReport cuda))
        else unless gpuBackendCompiled $ do
            required <- runGraph session (RequireCUDA selector) plan
            assertEqual "zero-multiply disabled requirement" (Left (GraphCUDAError CUDANotCompiled)) required
            fallback <- runGraph session (PreferCUDA selector FallbackBeforeUserLaunch) plan >>= right
            assertEqual "zero-multiply explicit fallback" (CPUFallback (CUDARejectedBeforeUserLaunch CUDANotCompiled)) (graphExecutionBackend (graphResultReport fallback))
            denied <- runGraph session (PreferCUDA selector NoFallback) plan
            assertEqual "zero-multiply fallback denied" (Left (GraphCUDAError CUDANotCompiled)) denied

numericTests :: TensorSession region -> IO ()
numericTests session = do
    huge <- fst <$> (finiteTensorFromList session (knownShape @'[1, 1]) [1e308] >>= right)
    two <- fst <$> (finiteTensorFromList session (knownShape @'[1, 1]) [2] >>= right)
    let graph = matrixLet (matrixMultiply (matrixInput huge) (matrixInput two)) (matrixInput two)
    plan <- right (prepareGraph roomy graph)
    output <- runGraph session CPUOnly plan
    case output of
        Left _ -> pure ()
        Right value -> fail ("unused overflowing binding escaped: " ++ show value)
    zero <- fst <$> (finiteTensorFromList session (knownShape @'[1, 1]) [0] >>= right)
    reversePlan <- right (prepareGraphVJP roomy (matrixMultiply (matrixInput huge) (matrixInput zero)) two)
    reverseOutput <- runGraphVJP session CPUOnly reversePlan
    case reverseOutput of
        Left _ -> pure ()
        Right value -> fail ("right-gradient overflow published a partial VJP: " ++ show value)
    empty <- fst <$> (finiteTensorFromList session (knownShape @'[0, 3]) [] >>= right)
    case prepareGraph roomy (matrixInput empty) of
        Left (GraphDimensionUnsupported 0) -> pure ()
        other -> fail ("zero matrix dimension accepted " ++ showPlan other)
    -- Cancellation fixes the CPU operation order independently of exact denotation.
    left <- fst <$> (finiteTensorFromList session (knownShape @'[1, 3]) [9007199254740992, 1, -9007199254740992] >>= right)
    rightWords <- fst <$> (finiteTensorFromList session (knownShape @'[3, 1]) [1, 1, 1] >>= right)
    cancellation <- right (prepareGraph roomy (matrixMultiply (matrixInput left) (matrixInput rightWords)))
    actual <- runGraph session CPUOnly cancellation >>= right
    assertEqual "exact dyadic cancellation remains one" [1] (exactProduct 1 3 1 [9007199254740992, 1, -9007199254740992] [1, 1, 1] :: [Rational])
    assertEqual "CPU separate operation-order policy" [0] (graphResultValues actual)

faultTests :: DeviceSelector -> TensorSession region -> Graph '[] region r c -> FiniteTensor region 'F64 '[r, c] -> IO ()
faultTests selector session graph seed = do
    plan <- right (prepareGraph roomy graph)
    reversePlan <- right (prepareGraphVJP roomy graph seed)
    forM_ [10, 11, 12, 13, 14 :: Int] $ \stage -> withFault (show stage ++ ":1") $ do
        fallback <- runGraph session (PreferCUDA selector FallbackBeforeUserLaunch) plan >>= right
        case graphExecutionBackend (graphResultReport fallback) of
            CPUFallback _ -> pure ()
            other -> fail ("graph prelaunch fallback absent " ++ show other)
    -- Same-executor occurrence counters discriminate reopening per node.
    forM_ ["10:2", "11:2", "12:2", "13:2", "14:2", "15:2", "16:2", "17:2", "18:2", "19:2", "20:2", "24:1", "28:1"] $ \fault ->
        withFault fault $ do
            result <- runGraph session (PreferCUDA selector FallbackBeforeUserLaunch) plan
            checkCommitted fault result
    forM_ ["10:3", "15:3", "15:6", "18:6"] $ \fault -> withFault fault $ do
        result <- runGraphVJP session (PreferCUDA selector FallbackBeforeUserLaunch) reversePlan
        checkCommitted fault result
    forM_ ["13:1,18:1", "10:1,24:1"] $ \fault -> withFault fault $ do
        result <- runGraph session (PreferCUDA selector FallbackBeforeUserLaunch) plan
        case result of { Left (GraphCUDAError _) -> pure (); other -> fail ("cleanup uncertainty fell back " ++ show other) }
  where
    checkCommitted label result = case result of
        Left (GraphCUDAError (CUDAExecutorFailure failure)) -> do
            assertEqual (label ++ " launch history") True (cudaFailureCommittedUserLaunch failure)
            assertEqual (label ++ " fallback prohibition") False (cudaFailureFallbackPermitted failure)
        Left (GraphCUDAError (CUDAActionAndCleanupFailure _ cleanup)) ->
            assertEqual (label ++ " cleanup fallback prohibition") False (cudaFailureFallbackPermitted cleanup)
        other -> fail (label ++ " late graph failure escaped " ++ show other)

withFault :: String -> IO a -> IO a
withFault specification action = bracket (lookupEnv name <* setEnv name specification) restore (const action)
  where
    name = "MARKOVIAN_CUDA_FAULTS"
    restore Nothing = unsetEnv name
    restore (Just value) = setEnv name value

-- Shared-edge addition overflows only after both finite CUDA VJP products.
-- A later teardown fault must retain that host primary, not replace it.
hostCleanupTest :: DeviceSelector -> TensorSession region -> IO ()
hostCleanupTest selector session = do
    value <- fst <$> (finiteTensorFromList session (knownShape @'[1, 1]) [1e154] >>= right)
    let graph = matrixLet (matrixInput value) (matrixMultiply (matrixRef matrixHere) (matrixRef matrixHere))
    plan <- right (prepareGraphVJP roomy graph value)
    withFault "24:1" $ do
        result <- runGraphVJP session (PreferCUDA selector FallbackBeforeUserLaunch) plan
        case result of
            Left (GraphHostAndCleanupFailure (GraphNonFinite _ _) (CUDAExecutorFailure cleanup)) -> do
                assertEqual "host primary retains global launch history" True (cudaFailureCommittedUserLaunch cleanup)
                assertEqual "host primary plus cleanup prohibits fallback" False (cudaFailureFallbackPermitted cleanup)
            other -> fail ("host numeric primary/teardown pairing changed " ++ show other)
