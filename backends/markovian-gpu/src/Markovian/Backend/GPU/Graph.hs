{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Closed, bounded F64 matrix graphs with explicit lexical sharing.

Each multiplication uses the existing CUDA kernel, with host materialization
between calls. Affine maps and cotangent sums execute on the host. Graph limits
reserve a complete schedule before reading tensors or admitting an executor.
They count logical F64 payload and the documented scalar work, not GHC heap
allocation or CPU instructions. See @docs/plans/D082-CUDA-GRAPHS.md@.

All tensor-dependent work must complete inside the originating tensor session.
Nominal indices do not prevent escaped IO actions or existential packaging.
Results contain fully evaluated ordinary copied values and can leave the session.
-}
module Markovian.Backend.GPU.Graph (
    MatrixRef,
    matrixHere,
    matrixThere,
    Graph,
    matrixInput,
    matrixRef,
    matrixMultiply,
    matrixLet,
    matrixView,
    GraphLimits,
    GraphLimit (..),
    graphLimits,
    GraphError (..),
    GraphPlanReport (..),
    renderGraphPlanReport,
    PreparedGraph,
    PreparedGraphVJP,
    prepareGraph,
    prepareGraphVJP,
    preparedGraphReport,
    preparedGraphVJPReport,
    GraphExecutionReport (..),
    GraphResult,
    graphResultValues,
    graphResultReport,
    GraphGradient,
    graphGradientInput,
    graphGradientShape,
    graphGradientValues,
    GraphVJP,
    graphVJPValues,
    graphVJPGradients,
    graphVJPReport,
    runGraph,
    runGraphVJP,
) where

import Control.Exception (evaluate)
import Control.Monad (foldM)
import Data.IORef (newIORef, readIORef, writeIORef)
import GHC.TypeLits (Nat, SomeNat (..), someNatVal)
import Markovian.Backend.GPU
import Markovian.Tensor
import Markovian.Tensor.Affine (AffineMap, affineMapDescriptor)
import Numeric.Natural (Natural)

-- Lexical paths cannot refer to nodes outside their typed environment.

-- | A typed lexical path to an already evaluated matrix binding.
data MatrixRef (env :: [(Nat, Nat)]) (rows :: Nat) (columns :: Nat) where
    Here :: MatrixRef ('(r, c) ': env) r c
    There :: MatrixRef env r c -> MatrixRef ('(a, b) ': env) r c

type role MatrixRef nominal nominal nominal

-- | Refer to the nearest matrix binding.
matrixHere :: MatrixRef ('(r, c) ': env) r c
matrixHere = Here

-- | Move a reference past one newer binding.
matrixThere :: MatrixRef env r c -> MatrixRef ('(a, b) ': env) r c
matrixThere = There

-- | Closed matrix syntax; only explicit lexical references share nodes.
data Graph (env :: [(Nat, Nat)]) region (rows :: Nat) (columns :: Nat) where
    Input :: FiniteTensor region 'F64 '[r, c] -> Graph env region r c
    Ref :: MatrixRef env r c -> Graph env region r c
    Multiply :: Graph env region r k -> Graph env region k c -> Graph env region r c
    Let :: Graph env region a b -> Graph ('(a, b) ': env) region r c -> Graph env region r c
    View :: AffineMap map '[r, c] '[p, q] -> Graph env region r c -> Graph env region p q

type role Graph nominal nominal nominal nominal

-- | Declare one input with independent logical-coordinate gradients.
matrixInput :: FiniteTensor region 'F64 '[r, c] -> Graph env region r c
matrixInput = Input

-- | Use an existing lexical binding without recomputing it.
matrixRef :: MatrixRef env r c -> Graph env region r c
matrixRef = Ref

-- | Multiply compatible matrices, evaluating the left expression first.
matrixMultiply :: Graph env region r k -> Graph env region k c -> Graph env region r c
matrixMultiply = Multiply

-- | Evaluate a binding before its body, including when the body ignores it.
matrixLet :: Graph env region a b -> Graph ('(a, b) ': env) region r c -> Graph env region r c
matrixLet = Let

-- | Gather through an admitted D-081 injective map of logical base coordinates.
matrixView :: AffineMap map '[r, c] '[p, q] -> Graph env region r c -> Graph env region p q
matrixView = View

-- Limits ---------------------------------------------------------------------

-- | Preparation limits in their public validation order.
data GraphLimit
    = GraphSyntax
    | GraphDimension
    | GraphElements
    | GraphTransferBytes
    | GraphHostPayloadBytes
    | GraphDevicePayloadBytes
    | GraphScalarWork
    | GraphLaunches
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Opaque machine-bounded limits for one prepared graph and run.
newtype GraphLimits = GraphLimits [Natural]
    deriving (Eq, Show)

-- | Atomic preparation or execution failure; no partial result accompanies it.
data GraphError
    = GraphInvalidLimit !GraphLimit !Natural
    | GraphLimitExceeded !GraphLimit !Natural !Natural
    | GraphDimensionUnsupported !Natural
    | GraphElementsUnsupported !Natural
    | GraphInvalidPlan !String
    | GraphInvalidAffineAddress !Natural !Integer
    | GraphAffineOverlap !Natural !Natural !Integer
    | GraphTensorError !TensorError
    | GraphDeviceError !DeviceError
    | GraphCUDAError !CUDAError
    | GraphHostAndCleanupFailure !GraphError !CUDAError
    | GraphNonFinite !Natural !Natural
    deriving (Eq, Show)

{- | Syntax/path visits, dimensions, elements, transfer bytes, cumulative host
bytes, peak device bytes, scalar work, launches. Zero limits are valid.
-}
graphLimits :: Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Either GraphError GraphLimits
graphLimits a b c d e f g h = do
    let values = [a, b, c, d, e, f, g, h]
    mapM_ (\(field, value) -> if value > machine then Left (GraphInvalidLimit field value) else Right ()) (zip [minBound .. maxBound] values)
    Right (GraphLimits values)

machine :: Natural
machine = fromIntegral (maxBound :: Int)

plus :: Natural -> Natural -> Natural
plus a b
    | a > machine || b > machine || a > machine - b = machine + 1
    | otherwise = a + b

times :: Natural -> Natural -> Natural
times a b
    | a == 0 || b == 0 = 0
    | a > machine || b > machine || a > machine `div` b = machine + 1
    | otherwise = a * b

sumBounded :: [Natural] -> Natural
sumBounded = foldl' plus 0

limitAt :: GraphLimits -> GraphLimit -> Natural
limitAt (GraphLimits values) field = values !! fromEnum field

check :: GraphLimits -> GraphLimit -> Natural -> Either GraphError ()
check limits field required
    | required > cap = Left (GraphLimitExceeded field cap required)
    | otherwise = Right ()
  where
    cap = limitAt limits field

{- | Fully evaluated timing-free schedule reservations. Host and work include
one failed pre-launch attempt followed by CPU fallback; transfers do not.
-}
data GraphPlanReport = GraphPlanReport
    { graphPlanSyntaxVisits :: !Natural
    , graphPlanNodes :: !Natural
    , graphPlanInputs :: !Natural
    , graphPlanOutputShape :: ![Natural]
    , graphPlanTransferBytes :: !Natural
    , graphPlanHostPayloadBytes :: !Natural
    , graphPlanDevicePayloadBytes :: !Natural
    , graphPlanScalarWork :: !Natural
    , graphPlanForwardLaunches :: !Natural
    , graphPlanReverseLaunches :: !Natural
    , graphPlanAffineCoordinates :: !Natural
    , graphPlanAccumulationCoordinates :: !Natural
    }
    deriving (Eq, Show)

-- | Render the deterministic reservation without timing or device addresses.
renderGraphPlanReport :: GraphPlanReport -> String
renderGraphPlanReport = (++ "\n") . show

data Dims = Dims !Natural !Natural deriving (Eq, Show)
elements :: Dims -> Natural
elements (Dims r c) = times r c

dimensions :: Dims -> [Natural]
dimensions (Dims r c) = [r, c]

data InputValue region where
    InputValue :: FiniteTensor region 'F64 '[r, c] -> InputValue region

data Node region
    = InputNode !Dims !(InputValue region)
    | ViewNode !Dims !Int !Integer !Integer !Integer ![Int]
    | MultiplyNode !Dims !Natural !Int !Int

nodeDims :: Node region -> Dims
nodeDims (InputNode ds _) = ds
nodeDims (ViewNode ds _ _ _ _ _) = ds
nodeDims (MultiplyNode ds _ _ _) = ds

-- | An immutable fully admitted forward schedule retaining its region inputs.
data PreparedGraph region rows columns = PreparedGraph ![Node region] !Int !GraphPlanReport

type role PreparedGraph nominal nominal nominal

-- | An admitted forward/reverse schedule and its finite output cotangent.
data PreparedGraphVJP region rows columns = PreparedGraphVJP !(PreparedGraph region rows columns) !(FiniteTensor region 'F64 '[rows, columns])

type role PreparedGraphVJP nominal nominal nominal

-- | Observe the forward schedule reservation.
preparedGraphReport :: PreparedGraph region r c -> GraphPlanReport
preparedGraphReport (PreparedGraph _ _ report) = report

-- | Observe the complete forward-and-reverse reservation.
preparedGraphVJPReport :: PreparedGraphVJP region r c -> GraphPlanReport
preparedGraphVJPReport (PreparedGraphVJP graph _) = preparedGraphReport graph

-- The admission scan neither observes shapes nor forces input tensors/maps.
scanSyntax :: GraphLimits -> Graph env region r c -> Either GraphError Natural
scanSyntax limits = walk 0
  where
    tick n = let !next = plus n 1 in check limits GraphSyntax next >> Right next
    path :: Natural -> MatrixRef env' a b -> Either GraphError Natural
    path n ref = do
        next <- tick n
        case ref of { Here -> Right next; There rest -> path next rest }
    walk :: Natural -> Graph env' region' a b -> Either GraphError Natural
    walk n term = do
        next <- tick n
        case term of
            Input _ -> Right next
            Ref ref -> path next ref
            Multiply left right -> walk next left >>= (`walk` right)
            Let bound body -> walk next bound >>= (`walk` body)
            View _ base -> walk next base

admitDims :: GraphLimits -> [Natural] -> Either GraphError Dims
admitDims limits [r, c] = do
    mapM_
        ( \dimension -> do
            check limits GraphDimension dimension
            if dimension == 0 || dimension > 2147483647 then Left (GraphDimensionUnsupported dimension) else Right ()
        )
        [r, c]
    let !count = times r c
    check limits GraphElements count
    if count > 2147483647 || count > machine `div` 8 then Left (GraphElementsUnsupported count) else Right (Dims r c)
admitDims _ _ = Left (GraphInvalidPlan "matrix rank")

data Build region = Build !Int ![Node region]

appendNode :: Node region -> Build region -> (Int, Build region)
appendNode node (Build count nodes) = (count, Build (count + 1) (node : nodes))

buildNode :: Build region -> Int -> Either GraphError (Node region)
buildNode (Build count nodes) index
    | index < 0 || index >= count = Left (GraphInvalidPlan "dependency order")
    | otherwise = Right (nodes !! (count - index - 1))

lower :: GraphLimits -> [Int] -> Build region -> Graph env region r c -> Either GraphError (Int, Build region)
lower limits env state term = case term of
    Input tensor -> do
        ds <- admitDims limits (shapeDimensions (tensorShape (hostTensor tensor)))
        Right (appendNode (InputNode ds (InputValue tensor)) state)
    Ref ref -> do
        index <- resolve env ref
        _ <- buildNode state index
        Right (index, state)
    Let bound body -> do
        (index, next) <- lower limits env state bound
        lower limits (index : env) next body
    Multiply left right -> do
        (li, s1) <- lower limits env state left
        (ri, s2) <- lower limits env s1 right
        l <- buildNode s2 li
        r <- buildNode s2 ri
        let Dims rows inner = nodeDims l
            Dims inner' columns = nodeDims r
        if inner /= inner'
            then Left (GraphInvalidPlan "multiply dimensions")
            else do
                ds <- admitDims limits [rows, columns]
                Right (appendNode (MultiplyNode ds inner li ri) s2)
    View witness base -> do
        (index, next) <- lower limits env state base
        parent <- buildNode next index
        let (baseShape, viewShape, offset, strides) = affineMapDescriptor witness
        source <- admitDims limits (shapeDimensions baseShape)
        ds <- admitDims limits (shapeDimensions viewShape)
        if source /= nodeDims parent
            then Left (GraphInvalidPlan "affine base dimensions")
            else case strides of
                [sr, sc]
                    | all (\n -> abs n <= toInteger machine) [offset, sr, sc] ->
                        Right (appendNode (ViewNode ds index offset sr sc []) next)
                _ -> Left (GraphInvalidPlan "affine descriptor")
  where
    resolve :: [Int] -> MatrixRef env' a b -> Either GraphError Int
    resolve (index : _) Here = Right index
    resolve (_ : rest) (There ref) = resolve rest ref
    resolve _ _ = Left (GraphInvalidPlan "lexical reference")

-- Accounting follows the contract's independent event table.
data Cost = Cost !Natural !Natural !Natural !Natural !Natural

-- host words, transfers, device peak, work, launches
zeroCost :: Cost
zeroCost = Cost 0 0 0 0 0

addCost :: Cost -> Cost -> Cost
addCost (Cost a b c d e) (Cost f g h i j) = Cost (plus a f) (plus b g) (max c h) (plus d i) (plus e j)

hostCost :: Natural -> Natural -> Cost
hostCost wordsCount work = Cost wordsCount 0 0 work 0

matrixCost :: Natural -> Natural -> Natural -> Cost
matrixCost r k c =
    let !s = sumBounded [times r k, times k c, times r c]
     in Cost (times 5 s) (times 8 s) (times 8 s) (times 2 (times r (times k c))) 1

planReport :: Bool -> Natural -> [Node region] -> Int -> GraphPlanReport
planReport reverseMode syntax nodes output =
    let !inputs = sumBounded [elements ds | InputNode ds _ <- nodes]
        !outputSize = elements (nodeDims (nodes !! output))
        !allElements = sumBounded (map (elements . nodeDims) nodes)
        !viewWork = sumBounded [plus (times 4 v) (pairs v) | ViewNode ds _ _ _ _ _ <- nodes, let v = elements ds]
        !viewCoordinates = sumBounded [elements ds | ViewNode ds _ _ _ _ _ <- nodes]
        forward (InputNode _ _) = zeroCost
        forward (ViewNode ds _ _ _ _ _) = hostCost (elements ds) (elements ds)
        forward (MultiplyNode (Dims r c) k _ _) = matrixCost r k c
        parentSize index = elements (nodeDims (nodes !! index))
        accumulation (InputNode _ _) = 0
        accumulation (ViewNode _ parent _ _ _ _) = parentSize parent
        accumulation (MultiplyNode _ _ left right) = plus (parentSize left) (parentSize right)
        backward (InputNode _ _) = zeroCost
        backward (ViewNode ds parent _ _ _ _) =
            let b = parentSize parent; v = elements ds
             in addCost (hostCost b (plus b v)) (hostCost b b)
        backward (MultiplyNode (Dims r c) k left right) =
            let operands = plus (parentSize left) (parentSize right)
             in foldl' addCost zeroCost [hostCost operands operands, matrixCost r c k, matrixCost k r c, hostCost operands operands]
        !forwardCost = foldl' addCost (hostCost outputSize outputSize) (map forward nodes)
        Cost _ _ _ _ forwardLaunches = forwardCost
        !reverseCost =
            if reverseMode
                then foldl' addCost (hostCost (sumBounded [allElements, outputSize, inputs]) (sumBounded [allElements, outputSize, inputs])) (map backward nodes)
                else zeroCost
        Cost _ _ _ _ reverseLaunches = reverseCost
        Cost h t d w _ = addCost forwardCost reverseCost
        !snapshotWords = plus inputs (if reverseMode then outputSize else 0)
     in GraphPlanReport
            syntax
            (fromIntegral (length nodes))
            (fromIntegral (length [() | InputNode _ _ <- nodes]))
            (dimensions (nodeDims (nodes !! output)))
            t
            (times 8 (plus snapshotWords (times 2 h)))
            d
            (sumBounded [snapshotWords, times 2 w, viewWork])
            forwardLaunches
            reverseLaunches
            viewCoordinates
            (if reverseMode then sumBounded (map accumulation nodes) else 0)
  where
    pairs v
        | even v = times (v `div` 2) (v - 1)
        | otherwise = times v ((v - 1) `div` 2)

materializeMaps :: [Node region] -> Either GraphError [Node region]
materializeMaps nodes = mapM materialize (zip [0 ..] nodes)
  where
    materialize (index, node@(MultiplyNode _ _ left right))
        | left < 0 || right < 0 || left >= index || right >= index = Left (GraphInvalidPlan "multiply dependency order")
        | otherwise = Right node
    materialize (_, node@(InputNode _ _)) = Right node
    materialize (index, ViewNode ds@(Dims _ columns) parent offset sr sc _)
        | parent < 0 || parent >= index = Left (GraphInvalidPlan "view dependency order")
        | otherwise = do
            let base = elements (nodeDims (nodes !! parent))
            addresses <- mapM (address base columns offset sr sc) [0 .. elements ds - 1]
            unique 0 addresses
            Right (ViewNode ds parent offset sr sc addresses)
    address base columns offset sr sc linear =
        let (r, c) = linear `quotRem` columns
            !value = offset + toInteger r * sr + toInteger c * sc
         in if value < 0 || value >= toInteger base
                then Left (GraphInvalidAffineAddress linear value)
                else Right (fromInteger value)
    unique _ [] = Right ()
    unique !i (value : rest) = compareRest i (i + 1) value rest >> unique (i + 1) rest
    compareRest _ _ _ [] = Right ()
    compareRest i !j value (other : rest)
        | value == other = Left (GraphAffineOverlap i j (toInteger value))
        | otherwise = compareRest i (j + 1) value rest

prepare :: Bool -> GraphLimits -> Graph '[] region r c -> Either GraphError (PreparedGraph region r c)
prepare reverseMode limits graph = do
    syntax <- scanSyntax limits graph
    (output, Build _ reversed) <- lower limits [] (Build 0 []) graph
    let nodes = reverse reversed
        !report = planReport reverseMode syntax nodes output
    check limits GraphTransferBytes (graphPlanTransferBytes report)
    check limits GraphHostPayloadBytes (graphPlanHostPayloadBytes report)
    check limits GraphDevicePayloadBytes (graphPlanDevicePayloadBytes report)
    check limits GraphScalarWork (graphPlanScalarWork report)
    check limits GraphLaunches (plus (graphPlanForwardLaunches report) (graphPlanReverseLaunches report))
    checked <- materializeMaps nodes
    forceNodes checked `seq` forceNaturals (graphPlanOutputShape report) `seq` Right (PreparedGraph checked output report)

forceNaturals :: [Natural] -> ()
forceNaturals = foldr seq ()

forceNodes :: [Node region] -> ()
forceNodes [] = ()
forceNodes (node : rest) = case node of
    ViewNode _ _ _ _ _ addresses -> foldr seq () addresses `seq` forceNodes rest
    _ -> node `seq` forceNodes rest

-- | Admit syntax, shapes, resources and affine maps before any tensor read.
prepareGraph :: GraphLimits -> Graph '[] region r c -> Either GraphError (PreparedGraph region r c)
prepareGraph = prepare False

-- | Reserve forward evaluation and every declared input gradient atomically.
prepareGraphVJP :: GraphLimits -> Graph '[] region r c -> FiniteTensor region 'F64 '[r, c] -> Either GraphError (PreparedGraphVJP region r c)
prepareGraphVJP limits graph seed = do
    prepared <- prepare True limits graph
    ds <- admitDims limits (shapeDimensions (tensorShape (hostTensor seed)))
    if dimensions ds /= graphPlanOutputShape (preparedGraphReport prepared)
        then Left (GraphInvalidPlan "seed dimensions")
        else Right (PreparedGraphVJP prepared seed)

-- Execution ------------------------------------------------------------------

-- | Actual backend admission and per-matrix transfer-inclusive timings.
data GraphExecutionReport = GraphExecutionReport
    { graphExecutionBackend :: !BackendSelection
    , graphExecutionPlan :: !GraphPlanReport
    , graphExecutionTransferInclusiveMilliseconds :: ![Double]
    }
    deriving (Eq, Show)

-- | Copied finite forward values with their execution report.
data GraphResult rows columns = GraphResult ![Double] !GraphExecutionReport
    deriving (Eq, Show)

type role GraphResult nominal nominal

-- | Read the fully evaluated row-major forward values.
graphResultValues :: GraphResult r c -> [Double]
graphResultValues (GraphResult values _) = values

-- | Read backend selection, reservation and measured matrix-call timings.
graphResultReport :: GraphResult r c -> GraphExecutionReport
graphResultReport (GraphResult _ report) = report

-- | Copied gradient for one input declaration, in logical row-major order.
data GraphGradient = GraphGradient !Natural ![Natural] ![Double]
    deriving (Eq, Show)

-- | Read the zero-based input declaration ordinal.
graphGradientInput :: GraphGradient -> Natural
graphGradientInput (GraphGradient index _ _) = index

-- | Read the input gradient matrix dimensions.
graphGradientShape :: GraphGradient -> [Natural]
graphGradientShape (GraphGradient _ shape _) = shape

-- | Read every finite logical input gradient coordinate.
graphGradientValues :: GraphGradient -> [Double]
graphGradientValues (GraphGradient _ _ values) = values

-- | Copied forward result and gradients for all input declarations.
data GraphVJP rows columns = GraphVJP ![Double] ![GraphGradient] !GraphExecutionReport
    deriving (Eq, Show)

type role GraphVJP nominal nominal

-- | Read the fully evaluated forward result returned with the VJP.
graphVJPValues :: GraphVJP r c -> [Double]
graphVJPValues (GraphVJP values _ _) = values

-- | Read all input gradients in declaration order, including unused inputs.
graphVJPGradients :: GraphVJP r c -> [GraphGradient]
graphVJPGradients (GraphVJP _ gradients _) = gradients

-- | Read the complete forward-and-reverse execution report.
graphVJPReport :: GraphVJP r c -> GraphExecutionReport
graphVJPReport (GraphVJP _ _ report) = report

finiteValues :: Natural -> [Double] -> Either GraphError ()
finiteValues node = go 0
  where
    go _ [] = Right ()
    go !index (value : rest)
        | isNaN value || isInfinite value = Left (GraphNonFinite node index)
        | otherwise = value `seq` go (index + 1) rest

snapshots :: [Node region] -> IO (Either GraphError [[Double]])
snapshots = go 0
  where
    go _ [] = pure (Right [])
    go index (InputNode _ (InputValue tensor) : rest) = do
        values <- tensorToList (hostTensor tensor)
        case finiteValues index values of
            Left problem -> pure (Left problem)
            Right () -> fmap ((values :) <$>) (go (index + 1) rest)
    go index (_ : rest) = fmap (([] :) <$>) (go (index + 1) rest)

-- All shapes were admitted before this singleton reconstruction and staging.
executeMatrix :: TensorSession region -> Maybe (CUDAExecutor scope) -> Natural -> Natural -> Natural -> [Double] -> [Double] -> IO (Either GraphError ([Double], [Double]))
executeMatrix session executor r k c left right =
    case (someNatVal (toInteger r), someNatVal (toInteger k), someNatVal (toInteger c)) of
        (Just (SomeNat rp), Just (SomeNat kp), Just (SomeNat cp)) -> do
            l <- finiteTensorFromList session (SCons rp (SCons kp SNil)) left
            case l of
                Left problem -> pure (Left (GraphTensorError problem))
                Right (lt, _) -> do
                    rr <- finiteTensorFromList session (SCons kp (SCons cp SNil)) right
                    case rr of
                        Left problem -> pure (Left (GraphTensorError problem))
                        Right (rt, _) -> case prepareMatMul (deviceLimits machine machine machine) lt rt of
                            Left problem -> pure (Left (GraphDeviceError (DevicePreparationError problem)))
                            Right prepared -> do
                                result <- case executor of
                                    Nothing -> runPreparedMatMulCPU session prepared
                                    Just cuda -> fmap (either (Left . DeviceCUDAError) Right) (runPreparedMatMulCUDA cuda prepared)
                                pure $ case result of
                                    Left (DeviceCUDAError problem) -> Left (GraphCUDAError problem)
                                    Left problem -> Left (GraphDeviceError problem)
                                    Right (value, report) -> Right (deviceTensorValues value, deviceExecutionTransferInclusiveMilliseconds report)
        _ -> pure (Left (GraphInvalidPlan "admitted dimensions could not be reified"))

type MatrixRunner = Natural -> Natural -> Natural -> [Double] -> [Double] -> IO (Either GraphError ([Double], [Double]))

replace :: Int -> value -> [value] -> [value]
replace index value values = case splitAt index values of
    (prefix, _ : suffix) -> prefix ++ value : suffix
    _ -> values -- indices are private, admitted dependencies

transposeWords :: Natural -> Natural -> [Double] -> [Double]
transposeWords rows columns values =
    [values !! fromIntegral (r * columns + c) | c <- [0 .. columns - 1], r <- [0 .. rows - 1]]

-- Graph-level launch history upgrades a later pre-launch failure.
committed :: CUDAError -> CUDAError
committed (CUDAExecutorFailure failure) = CUDAExecutorFailure failure{cudaFailureCommittedUserLaunch = True, cudaFailureFallbackPermitted = False}
committed (CUDAActionAndCleanupFailure primary cleanup) = CUDAActionAndCleanupFailure (committed primary) cleanup{cudaFailureFallbackPermitted = False}
committed other = other

runSchedule :: MatrixRunner -> [Node region] -> Int -> [[Double]] -> Maybe [Double] -> IO (Either GraphError ([Double], [GraphGradient], [Double]))
runSchedule matrix nodes output inputSnapshots seed = do
    outcome <- runScheduleSteps matrix nodes output inputSnapshots seed
    -- The Either, every coordinate and the terminating spines are forced while
    -- still inside the executor callback. Teardown cannot precede validation.
    evaluate (forceOutcome outcome)
    pure outcome
  where
    forceWords [] = ()
    forceWords (value : rest) = value `seq` forceWords rest
    forceGradients [] = ()
    forceGradients (GraphGradient index shape values : rest) =
        index `seq` forceNaturals shape `seq` forceWords values `seq` forceGradients rest
    forceOutcome (Left problem) = problem `seq` ()
    forceOutcome (Right (values, gradients, timings)) =
        forceWords values `seq` forceGradients gradients `seq` forceWords timings

runScheduleSteps :: MatrixRunner -> [Node region] -> Int -> [[Double]] -> Maybe [Double] -> IO (Either GraphError ([Double], [GraphGradient], [Double]))
runScheduleSteps matrix nodes output inputSnapshots seed = do
    forward <- foldM forwardStep (Right ([], [])) (zip3 [0 ..] nodes inputSnapshots)
    case forward of
        Left problem -> pure (Left problem)
        Right (values, timings) -> case seed of
            Nothing -> pure $ do
                let result = values !! output
                finiteValues (fromIntegral output) result
                finiteValues 0 timings
                Right (result, [], timings)
            Just incoming -> do
                let zeros = [replicate (fromIntegral (elements (nodeDims node))) 0 | node <- nodes]
                    initial = replace output incoming zeros
                -- Force zero payloads before reverse work or publication.
                case mapM_ (uncurry finiteValues) (zip [0 ..] initial) of
                    Left problem -> pure (Left problem)
                    Right () -> do
                        backward <- foldM (reverseStep values) (Right (initial, timings)) (reverse (zip [0 ..] nodes))
                        pure $ do
                            (gradients, allTimes) <- backward
                            let inputGradients =
                                    [ GraphGradient inputIndex (dimensions ds) (gradients !! index)
                                    | (inputIndex, (index, InputNode ds _)) <- zip [0 ..] [(i, node) | (i, node@(InputNode _ _)) <- zip [0 ..] nodes]
                                    ]
                                result = values !! output
                            finiteValues (fromIntegral output) result
                            mapM_ forceGradient inputGradients
                            finiteValues 0 allTimes
                            Right (result, inputGradients, allTimes)
  where
    forceGradient gradient = forceNaturals (graphGradientShape gradient) `seq` finiteValues (graphGradientInput gradient) (graphGradientValues gradient)
    forwardStep (Left problem) _ = pure (Left problem)
    forwardStep (Right (values, timings)) (index, node, input) = do
        result <- case node of
            InputNode _ _ -> pure (Right (input, []))
            ViewNode _ parent _ _ _ addresses -> pure (Right (map ((values !! parent) !!) addresses, []))
            MultiplyNode (Dims r c) k left right -> matrix r k c (values !! left) (values !! right)
        pure $ do
            (next, elapsed) <- result
            finiteValues index next
            finiteValues index elapsed
            Right (values ++ [next], timings ++ elapsed)
    reverseStep _ (Left problem) _ = pure (Left problem)
    reverseStep values (Right (gradients, timings)) (index, node) = case node of
        InputNode _ _ -> pure (Right (gradients, timings))
        ViewNode _ parent _ _ _ addresses -> do
            let size = elements (nodeDims (nodes !! parent))
                incoming = gradients !! index
                scattered = foldl' (\buffer (address, value) -> replace address value buffer) (replicate (fromIntegral size) 0) (zip addresses incoming)
            pure $ do
                finiteValues (fromIntegral index) scattered
                updated <- accumulate parent scattered gradients
                Right (updated, timings)
        MultiplyNode (Dims r c) k left right -> do
            let incoming = gradients !! index
                rightT = transposeWords k c (values !! right)
                leftT = transposeWords r k (values !! left)
            case finiteValues (fromIntegral index) rightT >> finiteValues (fromIntegral index) leftT of
                Left problem -> pure (Left problem)
                Right () -> do
                    l <- matrix r c k incoming rightT
                    case l of
                        Left problem -> pure (Left problem)
                        Right (lg, lt) -> case finiteValues (fromIntegral index) lg >> accumulate left lg gradients of
                            Left problem -> pure (Left problem)
                            Right afterLeft -> do
                                rr <- matrix k r c leftT incoming
                                pure $ do
                                    (rg, rt) <- rr
                                    finiteValues (fromIntegral index) rg
                                    afterRight <- accumulate right rg afterLeft
                                    Right (afterRight, timings ++ lt ++ rt)
    accumulate parent contribution gradients = do
        let updated = zipWith (+) (gradients !! parent) contribution
        finiteValues (fromIntegral parent) updated
        Right (replace parent updated gradients)

allowsFallback :: CUDAError -> Bool
allowsFallback CUDANotCompiled = True
allowsFallback (CUDAProbeFailed _) = True
allowsFallback (CUDADeviceNotFound _) = True
allowsFallback (CUDADeviceUnsupported _ _) = True
allowsFallback (CUDAExecutorFailure failure) = cudaFailureFallbackPermitted failure && not (cudaFailureCommittedUserLaunch failure)
allowsFallback _ = False

execute :: TensorSession region -> BackendRequest -> PreparedGraph region r c -> Maybe [Double] -> IO (Either GraphError ([Double], [GraphGradient], GraphExecutionReport))
execute session request (PreparedGraph nodes output report) seed = do
    readInputs <- snapshots nodes
    case readInputs of
        Left problem -> pure (Left problem)
        Right inputs -> dispatch inputs
  where
    cpu inputs selection = do
        result <- runSchedule (executeMatrix session Nothing) nodes output inputs seed
        pure (fmap (\(values, gradients, timesList) -> (values, gradients, GraphExecutionReport selection report timesList)) result)
    cuda inputs selector = do
        launched <- newIORef False
        hostPrimary <- newIORef Nothing
        result <- withCUDAExecutor selector $ \executor -> do
            let matrix r k c left right = do
                    previous <- readIORef launched
                    outcome <- executeMatrix session (Just executor) r k c left right
                    case outcome of
                        Right _ -> writeIORef launched True >> pure outcome
                        Left (GraphCUDAError problem) -> pure (Left (GraphCUDAError (if previous then committed problem else problem)))
                        _ -> pure outcome
            outcome <- runSchedule matrix nodes output inputs seed
            case outcome of
                Left (GraphCUDAError _) -> pure ()
                Left problem -> writeIORef hostPrimary (Just problem)
                Right _ -> pure ()
            pure $ case outcome of
                Left (GraphCUDAError problem) -> Left problem
                _ -> Right (fmap (\(values, gradients, timesList) -> (values, gradients, GraphExecutionReport (CUDASelected (cudaExecutorAdmission executor)) report timesList)) outcome)
        anyLaunch <- readIORef launched
        primary <- readIORef hostPrimary
        pure $ case result of
            Left problem ->
                let cleanup = if anyLaunch then committed problem else problem
                 in Left (maybe (GraphCUDAError cleanup) (`GraphHostAndCleanupFailure` cleanup) primary)
            Right outcome -> outcome
    dispatch inputs = case request of
        CPUOnly -> cpu inputs CPURequested
        RequireCUDA selector -> cuda inputs selector
        PreferCUDA selector policy -> do
            result <- cuda inputs selector
            case result of
                Left (GraphCUDAError problem)
                    | policy == FallbackBeforeUserLaunch && allowsFallback problem ->
                        cpu inputs (CPUFallback (CUDARejectedBeforeUserLaunch problem))
                _ -> pure result

-- | Run one prepared forward schedule with explicit backend/fallback policy.
runGraph :: TensorSession region -> BackendRequest -> PreparedGraph region r c -> IO (Either GraphError (GraphResult r c))
runGraph session request graph = do
    result <- execute session request graph Nothing
    pure (fmap (\(values, _, report) -> GraphResult values report) result)

-- | Run the prepared forward and reverse schedules; force every result before closing CUDA.
runGraphVJP :: TensorSession region -> BackendRequest -> PreparedGraphVJP region r c -> IO (Either GraphError (GraphVJP r c))
runGraphVJP session request (PreparedGraphVJP graph seed) = do
    values <- tensorToList (hostTensor seed)
    case finiteValues 0 values of
        Left problem -> pure (Left problem)
        Right () -> do
            result <- execute session request graph (Just values)
            pure (fmap (\(output, gradients, report) -> GraphVJP output gradients report) result)
