{- | A small checked dense feed-forward network.

A network has zero or more @tanh@ hidden layers and one linear output layer.
For each layer, parameters are stored as all row-major weights (one row per
output unit), followed by that layer's biases. Layers occur in forward order.
Both parameter and input vector-Jacobian products are computed manually.
-}
module Markovian.Backend.Neural.Dense (
    DenseError (..),
    HiddenActivation (..),
    DenseNetwork,
    mkDenseNetwork,
    denseInputSize,
    denseHiddenSizes,
    denseOutputSize,
    denseHiddenActivations,
    denseParameterCount,
    denseParameters,
    replaceDenseParameters,
    sameDenseTopology,
    denseForward,
    denseParameterVJP,
    denseInputVJP,
    denseReverseCircuit,
) where

import Markovian.Backend.Neural.Numeric (
    NeuralNumericError,
    checkedAdd,
    checkedMultiply,
    checkedSubtract,
    checkedSum,
    validateFiniteVector,
 )
import Markovian.Reverse (
    CotangentEqualityMode (ApproximateCotangentEquality),
    CotangentSpace,
    ParametricReverseCircuit,
    declaredCotangentSpace,
    finiteLayout,
    primitiveReverseCircuit,
    reverseEvaluation,
 )

-- | The supported hidden activation.
data HiddenActivation = Tanh
    deriving (Eq, Show)

-- | Shape and checked-arithmetic failures.
data DenseError
    = InvalidDenseInputSize !Int
    | InvalidDenseHiddenSize !Int !Int
    | InvalidDenseOutputSize !Int
    | DenseParameterCountOverflow
    | DenseParameterShapeMismatch !Int !Int
    | DenseInputShapeMismatch !Int !Int
    | DenseOutputCotangentShapeMismatch !Int !Int
    | DenseReverseDeclarationFailure !String
    | DenseNumericFailure !NeuralNumericError
    deriving (Eq, Show)

{- | A validated dense topology and finite parameter snapshot.

The constructor is private so dimensions and parameters remain valid together.
-}
data DenseNetwork = DenseNetwork !Int ![Int] !Int ![Double]
    deriving (Eq, Show)

-- | Construct a network with @tanh@ hidden layers and a linear output head.
mkDenseNetwork :: Int -> [Int] -> Int -> [Double] -> Either DenseError DenseNetwork
mkDenseNetwork inputSize hiddenSizes outputSize parameters
    | inputSize <= 0 = Left (InvalidDenseInputSize inputSize)
    | Just (index, size) <- firstInvalid hiddenSizes = Left (InvalidDenseHiddenSize index size)
    | outputSize <= 0 = Left (InvalidDenseOutputSize outputSize)
    | otherwise = do
        expected <- parameterCountFor inputSize hiddenSizes outputSize
        let actual = length parameters
        if actual /= expected
            then Left (DenseParameterShapeMismatch expected actual)
            else do
                mapNumeric (validateFiniteVector "dense parameters" parameters)
                Right (DenseNetwork inputSize hiddenSizes outputSize parameters)

-- | Input width.
denseInputSize :: DenseNetwork -> Int
denseInputSize (DenseNetwork size _ _ _) = size

-- | Hidden widths in forward order.
denseHiddenSizes :: DenseNetwork -> [Int]
denseHiddenSizes (DenseNetwork _ sizes _ _) = sizes

-- | Output width.
denseOutputSize :: DenseNetwork -> Int
denseOutputSize (DenseNetwork _ _ size _) = size

-- | One explicit activation marker for each hidden layer.
denseHiddenActivations :: DenseNetwork -> [HiddenActivation]
denseHiddenActivations network = replicate (length (denseHiddenSizes network)) Tanh

-- | Total number of scalar parameters.
denseParameterCount :: DenseNetwork -> Int
denseParameterCount = length . denseParameters

-- | Parameters in documented layer, row-major-weight, then bias order.
denseParameters :: DenseNetwork -> [Double]
denseParameters (DenseNetwork _ _ _ parameters) = parameters

-- | Replace all parameters while retaining and revalidating topology.
replaceDenseParameters :: [Double] -> DenseNetwork -> Either DenseError DenseNetwork
replaceDenseParameters parameters network =
    mkDenseNetwork
        (denseInputSize network)
        (denseHiddenSizes network)
        (denseOutputSize network)
        parameters

-- | Test topology equality without comparing parameter values.
sameDenseTopology :: DenseNetwork -> DenseNetwork -> Bool
sameDenseTopology left right =
    denseInputSize left == denseInputSize right
        && denseHiddenSizes left == denseHiddenSizes right
        && denseOutputSize left == denseOutputSize right

-- | Evaluate one finite input vector.
denseForward :: DenseNetwork -> [Double] -> Either DenseError [Double]
denseForward network inputs = do
    cache <- forwardCache network inputs
    case reverse cache of
        [] -> Right inputs -- unreachable because every network has an output layer
        entry : _ -> Right (cacheOutput entry)

-- | Compute @v^T (d output / d parameters)@ in parameter order.
denseParameterVJP :: DenseNetwork -> [Double] -> [Double] -> Either DenseError [Double]
denseParameterVJP network inputs outputCotangent = do
    validateCotangent network outputCotangent
    cache <- forwardCache network inputs
    (_, forwardSegments) <- backpropagate outputCotangent (reverse cache) []
    Right (concat forwardSegments)

-- | Compute @v^T (d output / d input)@.
denseInputVJP :: DenseNetwork -> [Double] -> [Double] -> Either DenseError [Double]
denseInputVJP network inputs outputCotangent = do
    validateCotangent network outputCotangent
    cache <- forwardCache network inputs
    (inputGradient, _) <- backpropagate outputCotangent (reverse cache) []
    Right inputGradient

{- | Adapt one validated dense topology to the backend-independent reverse
core. The parameter argument is a complete replacement parameter vector; the
captured pullback uses the same checked manual VJPs as 'denseParameterVJP' and
'denseInputVJP'. This is an explicit adapter, not automatic differentiation.

The cotangent equality is approximate with a documented mixed tolerance. All
vector lengths and finite values are checked before module operations.
-}
denseReverseCircuit ::
    DenseNetwork ->
    Either
        DenseError
        ( ParametricReverseCircuit
            DenseError
            Double
            [Double]
            [Double]
            [Double]
            [Double]
            [Double]
            [Double]
        )
denseReverseCircuit topology = do
    parameterSpace <- vectorCotangentSpace "neural/dense/parameters" (denseParameterCount topology) DenseParameterShapeMismatch
    inputSpace <- vectorCotangentSpace "neural/dense/input" (denseInputSize topology) DenseInputShapeMismatch
    outputSpace <- vectorCotangentSpace "neural/dense/output" (denseOutputSize topology) DenseOutputCotangentShapeMismatch
    Right $
        primitiveReverseCircuit parameterSpace inputSpace outputSpace $ \parameters inputs -> do
            network <- replaceDenseParameters parameters topology
            output <- denseForward network inputs
            Right
                ( reverseEvaluation output $ \outputCotangent -> do
                    parameterCotangent <- denseParameterVJP network inputs outputCotangent
                    inputCotangent <- denseInputVJP network inputs outputCotangent
                    Right (parameterCotangent, inputCotangent)
                )

vectorCotangentSpace ::
    String ->
    Int ->
    (Int -> Int -> DenseError) ->
    Either DenseError (CotangentSpace DenseError Double [Double])
vectorCotangentSpace owner extent mismatch = do
    layout <- maybe (Left (DenseReverseDeclarationFailure (owner ++ ": invalid layout"))) Right (finiteLayout owner (fromIntegral extent))
    maybe
        (Left (DenseReverseDeclarationFailure (owner ++ ": invalid cotangent declaration")))
        Right
        ( declaredCotangentSpace
            owner
            layout
            validate
            (replicate extent 0)
            add
            scale
            equivalent
            (ApproximateCotangentEquality "finite vectors: abs 2e-10 + rel 2e-8")
        )
  where
    validate values
        | length values /= extent = Left (mismatch extent (length values))
        | otherwise = mapNumeric (validateFiniteVector owner values)
    add left right = do
        validate left
        validate right
        traverse (mapNumeric . uncurry (checkedAdd (owner ++ " cotangent addition"))) (zip left right)
    scale scalar values = do
        validate values
        traverse (mapNumeric . checkedMultiply (owner ++ " cotangent scaling") scalar) values
    equivalent left right =
        length left == extent
            && length right == extent
            && and (zipWith close left right)
    close left right =
        abs (left - right) <= 2e-10 + 2e-8 * max (abs left) (abs right)

-- Internal layer values.
data Layer = Layer !Int !Int ![Double] ![Double] !Bool

data CacheEntry = CacheEntry
    { cacheInput :: ![Double]
    , cacheLayer :: !Layer
    , cacheOutput :: ![Double]
    }

forwardCache :: DenseNetwork -> [Double] -> Either DenseError [CacheEntry]
forwardCache network inputs
    | length inputs /= denseInputSize network =
        Left (DenseInputShapeMismatch (denseInputSize network) (length inputs))
    | otherwise = do
        mapNumeric (validateFiniteVector "dense inputs" inputs)
        layers <- networkLayers network
        go inputs layers []
  where
    go _ [] reversedEntries = Right (reverse reversedEntries)
    go current (layer : remaining) reversedEntries = do
        preactivation <- evaluateLayer layer current
        output <- activate layer preactivation
        let entry = CacheEntry current layer output
        go output remaining (entry : reversedEntries)

networkLayers :: DenseNetwork -> Either DenseError [Layer]
networkLayers network = Right (build 0 dimensions (denseParameters network))
  where
    widths = denseInputSize network : denseHiddenSizes network ++ [denseOutputSize network]
    dimensions = zip widths (drop 1 widths)
    finalIndex = length dimensions - 1
    build _ [] _ = []
    build index ((inputWidth, outputWidth) : remaining) parameters =
        let weightCount = inputWidth * outputWidth
            (weights, afterWeights) = splitAt weightCount parameters
            (biases, afterLayer) = splitAt outputWidth afterWeights
         in Layer inputWidth outputWidth weights biases (index < finalIndex)
                : build (index + 1) remaining afterLayer

evaluateLayer :: Layer -> [Double] -> Either DenseError [Double]
evaluateLayer (Layer inputWidth outputWidth weights biases _) inputs =
    traverse evaluateUnit [0 .. outputWidth - 1]
  where
    evaluateUnit unit = do
        let row = take inputWidth (drop (unit * inputWidth) weights)
        products <- traverse (mapNumeric . uncurry (checkedMultiply "dense affine product")) (zip row inputs)
        total <- mapNumeric (checkedSum "dense affine sum" products)
        mapNumeric (checkedAdd "dense affine bias" total (biases !! unit))

activate :: Layer -> [Double] -> Either DenseError [Double]
activate (Layer _ _ _ _ False) values = Right values
activate (Layer _ _ _ _ True) values = do
    let outputs = fmap tanh values
    mapNumeric (validateFiniteVector "dense tanh output" outputs)
    Right outputs

backpropagate :: [Double] -> [CacheEntry] -> [[Double]] -> Either DenseError ([Double], [[Double]])
backpropagate upstream [] segments = Right (upstream, segments)
backpropagate upstream (entry : remaining) segments = do
    delta <- activationVJP entry upstream
    let Layer inputWidth outputWidth weights _ _ = cacheLayer entry
    weightGradient <-
        fmap
            concat
            ( traverse
                (\unit -> traverse (mapNumeric . checkedMultiply "dense weight VJP" (delta !! unit)) (cacheInput entry))
                [0 .. outputWidth - 1]
            )
    inputGradient <-
        traverse
            ( \inputIndex -> do
                products <-
                    traverse
                        ( \unit ->
                            mapNumeric
                                ( checkedMultiply
                                    "dense input VJP"
                                    (weights !! (unit * inputWidth + inputIndex))
                                    (delta !! unit)
                                )
                        )
                        [0 .. outputWidth - 1]
                mapNumeric (checkedSum "dense input VJP sum" products)
            )
            [0 .. inputWidth - 1]
    backpropagate inputGradient remaining ((weightGradient ++ delta) : segments)

activationVJP :: CacheEntry -> [Double] -> Either DenseError [Double]
activationVJP entry upstream =
    case cacheLayer entry of
        Layer _ outputWidth _ _ False
            | length upstream == outputWidth -> Right upstream
        Layer _ outputWidth _ _ True
            | length upstream == outputWidth ->
                traverse derivative (zip upstream (cacheOutput entry))
        _ -> Left (DenseOutputCotangentShapeMismatch (length (cacheOutput entry)) (length upstream))
  where
    derivative (cotangent, output) = do
        square <- mapNumeric (checkedMultiply "dense tanh derivative square" output output)
        slope <- mapNumeric (checkedSubtract "dense tanh derivative" 1 square)
        mapNumeric (checkedMultiply "dense tanh VJP" cotangent slope)

validateCotangent :: DenseNetwork -> [Double] -> Either DenseError ()
validateCotangent network cotangent
    | length cotangent /= denseOutputSize network =
        Left (DenseOutputCotangentShapeMismatch (denseOutputSize network) (length cotangent))
    | otherwise = mapNumeric (validateFiniteVector "dense output cotangent" cotangent)

parameterCountFor :: Int -> [Int] -> Int -> Either DenseError Int
parameterCountFor inputSize hiddenSizes outputSize =
    if total > toInteger (maxBound :: Int)
        then Left DenseParameterCountOverflow
        else Right (fromInteger total)
  where
    widths = inputSize : hiddenSizes ++ [outputSize]
    total = sum [toInteger output * (toInteger input + 1) | (input, output) <- zip widths (drop 1 widths)]

firstInvalid :: [Int] -> Maybe (Int, Int)
firstInvalid = go 0
  where
    go _ [] = Nothing
    go index (value : remaining)
        | value <= 0 = Just (index, value)
        | otherwise = go (index + 1) remaining

mapNumeric :: Either NeuralNumericError value -> Either DenseError value
mapNumeric = either (Left . DenseNumericFailure) Right
