{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (forM_, unless, when)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat)
import Markovian.Tensor
import Markovian.Tensor.Ownership
import Markovian.Tensor.Primitive
import Markovian.Tensor.Reverse
import Paths_markovian_tensor (getDataFileName)
import System.Exit (exitFailure)

largeLimits :: SessionLimits
largeLimits = tensorSessionLimits 8 1024 1000000 8000000 64000000 256 100000000

matrixShape :: forall rows columns. (KnownNat rows, KnownNat columns) => SShape '[rows, columns]
matrixShape = SCons (Proxy @rows) (SCons (Proxy @columns) SNil)

vectorShape :: forall length. (KnownNat length) => SShape '[length]
vectorShape = SCons (Proxy @length) SNil

main :: IO ()
main = do
    shapeAndLayoutTests
    primitiveTests
    reverseTests
    budgetTests
    ownershipTests
    numericTests
    putStrLn "markovian-tensor: all focused tests passed"

shapeAndLayoutTests :: IO ()
shapeAndLayoutTests = expectSession "shape/layout session" $ \session -> do
    (scalar, _) <- expectRightIO "scalar" (finiteTensorFromList session SNil [7])
    assertEqual "rank-zero scalar element" [7] =<< tensorToList (hostTensor scalar)
    oversized <- finiteTensorFromList session SNil (repeat 1)
    case oversized of
        Left (InputLengthExceedsShape 1) -> pure ()
        other -> failTest ("bounded infinite input rejection: " ++ showEither other)
    (empty, _) <- expectRightIO "zero dimension" (finiteTensorFromList session (matrixShape @0 @3) [])
    assertEqual "zero-dimensional payload" [] =<< tensorToList (hostTensor empty)
    case reshapeFiniteContiguous session (matrixShape @2000 @0) empty of
        Left (TensorShapeError (DimensionLimitExceeded 1024 2000)) -> pure ()
        other -> failTest ("reshape target shape boundary: " ++ showEither other)

    (matrix, _) <- expectRightIO "matrix" (finiteTensorFromList session (matrixShape @2 @3) [1 .. 6])
    let transposed = transposeFinite2D matrix
    assertEqual "transpose coordinates" [1, 4, 2, 5, 3, 6] =<< tensorToList (hostTensor transposed)
    unless (sameStorage (hostTensor matrix) (hostTensor transposed)) (failTest "transpose did not share storage")
    case reshapeFiniteContiguous session (vectorShape @6) transposed of
        Left (TensorLayoutError NonContiguousReshape) -> pure ()
        other -> failTest ("non-contiguous reshape boundary: " ++ showEither other)
    copied <- expectRightIO "contiguous copy" (contiguousCopy session transposed)
    let (copy, _) = copied
    whenSameStorage "copy unexpectedly shared storage" (hostTensor transposed) (hostTensor copy)
    reshaped <- expectRight "reshape copied" (reshapeFiniteContiguous session (vectorShape @6) copy)
    assertEqual "reshape values" [1, 4, 2, 5, 3, 6] =<< tensorToList (hostTensor reshaped)
    pure (Right ())

primitiveTests :: IO ()
primitiveTests = expectSession "primitive session" $ \session -> do
    (left, _) <- expectRightIO "left" (finiteTensorFromList session (matrixShape @2 @2) [1, 2, 3, 4])
    (right, _) <- expectRightIO "right" (finiteTensorFromList session (matrixShape @2 @2) [5, 6, 7, 8])
    (added, addReport) <- expectRightIO "add" (add session left right)
    assertEqual "add reference" [6, 8, 10, 12] =<< tensorToList (hostTensor added)
    goldenPath <- getDataFileName "test/golden/add-report.txt"
    golden <- readFile goldenPath
    assertEqual "add deterministic report golden" golden (renderTensorOperationReport addReport)
    (productTensor, _) <- expectRightIO "multiply" (multiply session left right)
    assertEqual "multiply reference" [5, 12, 21, 32] =<< tensorToList (hostTensor productTensor)
    (hyperbolic, _) <- expectRightIO "tanh" (tanhTensor session left)
    assertApproxList "tanh reference" (map tanh [1, 2, 3, 4]) =<< tensorToList (hostTensor hyperbolic)
    (summed, _) <- expectRightIO "sum" (sumAll session left)
    assertEqual "sum left order" [10] =<< tensorToList (hostTensor summed)
    (matrixProduct, report) <- expectRightIO "matmul" (matmul session left right)
    assertEqual "matmul independent reference" [19, 22, 43, 50] =<< tensorToList (hostTensor matrixProduct)
    assertEqual "matmul work" 20 (reportScalarWork report)

    (zeroLeft, _) <- expectRightIO "zero-inner left" (finiteTensorFromList session (matrixShape @2 @0) [])
    (zeroRight, _) <- expectRightIO "zero-inner right" (finiteTensorFromList session (matrixShape @0 @3) [])
    (zeroProduct, _) <- expectRightIO "zero-inner matmul" (matmul session zeroLeft zeroRight)
    assertEqual "zero-inner matrix" (replicate 6 0) =<< tensorToList (hostTensor zeroProduct)
    pure (Right ())

reverseTests :: IO ()
reverseTests = expectSession "reverse session" $ \session -> do
    (left, _) <- expectRightIO "reverse left" (finiteTensorFromList session (matrixShape @2 @2) [1, 2, 3, 4])
    (right, _) <- expectRightIO "reverse right" (finiteTensorFromList session (matrixShape @2 @2) [5, 6, 7, 8])
    (_, tape, _) <- expectRightIO "matmul tape" (matmulWithTape session left right)
    (seed, _) <- expectRightIO "matmul seed" (finiteTensorFromList session (matrixShape @2 @2) [0.5, -1, 2, 0.25])
    ((leftGradient, rightGradient), _) <- expectRightIO "matmul VJP" (applyBinaryTape session tape seed)
    actualLeft <- tensorToList (hostTensor leftGradient)
    actualRight <- tensorToList (hostTensor rightGradient)
    let a = [1, 2, 3, 4]
        b = [5, 6, 7, 8]
        lambda = [0.5, -1, 2, 0.25]
        objective aValues bValues = sum (zipWith (*) lambda (referenceMatmul 2 2 2 aValues bValues))
    forM_ (zip3 [0 :: Int ..] actualLeft a) $ \(index, actual, coordinate) ->
        assertApprox ("matmul left finite difference " ++ show index) (finiteDifference (\value -> objective (replace index value a) b) coordinate) actual
    forM_ (zip3 [0 :: Int ..] actualRight b) $ \(index, actual, coordinate) ->
        assertApprox ("matmul right finite difference " ++ show index) (finiteDifference (\value -> objective a (replace index value b)) coordinate) actual

    (_, tanhTape, _) <- expectRightIO "tanh tape" (tanhWithTape session left)
    (ones, _) <- expectRightIO "ones" (finiteTensorFromList session (matrixShape @2 @2) (replicate 4 1))
    (tanhGradient, _) <- expectRightIO "tanh VJP" (applyUnaryTape session tanhTape ones)
    actualTanh <- tensorToList (hostTensor tanhGradient)
    forM_ (zip3 [0 :: Int ..] actualTanh a) $ \(index, actual, coordinate) ->
        assertApprox ("tanh finite difference " ++ show index) (finiteDifference tanh coordinate) actual

    (_, addTape, _) <- expectRightIO "add tape" (addWithTape session left right)
    ((leftSeed, rightSeed), addVjpReport) <- expectRightIO "add VJP" (applyBinaryTape session addTape seed)
    unless (sameStorage (hostTensor leftSeed) (hostTensor rightSeed)) (failTest "add VJP should safely share immutable seed storage")
    assertEqual "add VJP no payload allocation" 0 (reportAllocationCount (reportMemory addVjpReport))
    addLeftGradient <- tensorToList (hostTensor leftSeed)
    addRightGradient <- tensorToList (hostTensor rightSeed)
    let addObjective aValues bValues = sum (zipWith (*) lambda (zipWith (+) aValues bValues))
    checkAllCoordinates "add left" addLeftGradient a (\index value -> addObjective (replace index value a) b)
    checkAllCoordinates "add right" addRightGradient b (\index value -> addObjective a (replace index value b))

    (_, multiplyTape, _) <- expectRightIO "multiply tape" (multiplyWithTape session left right)
    ((multiplyLeft, multiplyRight), _) <- expectRightIO "multiply VJP" (applyBinaryTape session multiplyTape seed)
    multiplyLeftGradient <- tensorToList (hostTensor multiplyLeft)
    multiplyRightGradient <- tensorToList (hostTensor multiplyRight)
    let multiplyObjective aValues bValues = sum (zipWith (*) lambda (zipWith (*) aValues bValues))
        leftDirection = [0.1, -0.2, 0.3, -0.4]
        rightDirection = [-0.25, 0.5, -0.75, 1]
        jvp = zipWith3 (\da db (x, y) -> da * y + x * db) leftDirection rightDirection (zip a b)
        forwardPairing = sum (zipWith (*) lambda jvp)
        reversePairing = sum (zipWith (*) multiplyLeftGradient leftDirection) + sum (zipWith (*) multiplyRightGradient rightDirection)
    assertApprox "multiply independent JVP/VJP pairing" forwardPairing reversePairing
    checkAllCoordinates "multiply left" multiplyLeftGradient a (\index value -> multiplyObjective (replace index value a) b)
    checkAllCoordinates "multiply right" multiplyRightGradient b (\index value -> multiplyObjective a (replace index value b))

    (_, sumTape, _) <- expectRightIO "sum tape" (sumWithTape session left)
    (sumSeed, _) <- expectRightIO "sum seed" (finiteTensorFromList session SNil [0.75])
    (sumGradient, _) <- expectRightIO "sum VJP" (applyUnaryTape session sumTape sumSeed)
    actualSum <- tensorToList (hostTensor sumGradient)
    checkAllCoordinates "sum" actualSum a (\index value -> 0.75 * sum (replace index value a))
    pure (Right ())

budgetTests :: IO ()
budgetTests = do
    let limits = tensorSessionLimits 2 2 4 32 128 8 1000
    result <- withTensorSession limits $ \session -> do
        (left, _) <- expectRightIO "budget left" (finiteTensorFromList session (matrixShape @2 @2) [1, 2, 3, 4])
        (right, _) <- expectRightIO "budget right" (finiteTensorFromList session (matrixShape @2 @2) [5, 6, 7, 8])
        (output, tape, _) <- expectRightIO "budget matmul" (matmulWithTape session left right)
        -- Matmul VJP needs two 32-byte outputs. At this point the three
        -- buffers consume 96 bytes, so preflight rejects atomically.
        vjpResult <- applyBinaryTape session tape output
        case vjpResult of
            Left (TensorBudgetError (FreshPayloadLimitExceeded 128 160)) -> pure ()
            other -> failTest ("matmul VJP atomic boundary: " ++ showEither other)
        -- A one-output 32-byte copy still succeeds at the exact limit. This
        -- would fail if one VJP payload had been allocated before rejection.
        _ <- expectRightIO "allocation count remains zero after failed preflight" (contiguousCopy session left)
        pure (Right ())
    case result of
        Left problem -> failTest ("atomic VJP budget session: " ++ show problem)
        Right () -> pure ()

    -- Shape products are capped before multiplication reaches an allocation or
    -- machine conversion.
    let huge = 18446744073709551616
        hugeLimits = tensorSessionLimits 1 huge huge huge huge 1 huge
        hugeShape = SCons (Proxy @18446744073709551616) SNil
    hugeResult <- withTensorSession hugeLimits $ \session -> do
        admission <- finiteTensorFromList session hugeShape []
        pure (admission >> Right ())
    case hugeResult of
        Left (TensorShapeError (MachineIndexOverflow _)) -> pure ()
        other -> failTest ("machine-index shape boundary: " ++ showEither other)

    let noPayload = tensorSessionLimits 1 4 4 0 0 0 0
    untouched <- withTensorSession noPayload $ \session -> do
        admission <- finiteTensorFromList session (vectorShape @1) (error "input materialized before budget preflight")
        pure (admission >> Right ())
    case untouched of
        Left (TensorBudgetError (SinglePayloadLimitExceeded 0 8)) -> pure ()
        other -> failTest ("pre-materialization payload boundary: " ++ showEither other)

ownershipTests :: IO ()
ownershipTests = expectSession "ownership session" $ \session -> do
    (tensor, _) <- expectRightIO "owned tensor" (finiteTensorFromList session (vectorShape @2) [1, 2])
    ownerA <- expectRight "owner A" (tensorOwner "owner-a" (vectorShape @2))
    ownerB <- expectRight "owner B" (tensorOwner "owner-b" (vectorShape @2))
    let a = ownTensor ownerA tensor
        b = ownTensor ownerB tensor
    assertEqual "distinct semantic owners" ("owner-a", "owner-b") (ownerKey (ownedTensorOwner a), ownerKey (ownedTensorOwner b))
    unless (sameStorage (hostTensor (ownedFiniteTensor a)) (hostTensor (ownedFiniteTensor b))) (failTest "ownership fixture did not share storage")
    pure (Right ())

numericTests :: IO ()
numericTests = expectSession "numeric session" $ \session -> do
    (raw, _) <- expectRightIO "raw NaN" (hostTensorFromList session SF64 SNil [0 / 0])
    refinement <- finiteTensor raw
    case refinement of
        Left (TensorNumericError (NonFiniteInput "finite-tensor" 0)) -> pure ()
        other -> failTest ("raw/finite refinement boundary: " ++ showEither other)
    (large, _) <- expectRightIO "large finite" (finiteTensorFromList session SNil [1e308])
    overflow <- multiply session large large
    case overflow of
        Left (TensorNumericError (NonFiniteIntermediate "multiply" 0)) -> pure ()
        other -> failTest ("overflow boundary: " ++ showEither other)
    pure (Right ())

referenceMatmul :: Int -> Int -> Int -> [Double] -> [Double] -> [Double]
referenceMatmul rows inner columns left right =
    [ sum [left !! (row * inner + k) * right !! (k * columns + column) | k <- [0 .. inner - 1]]
    | row <- [0 .. rows - 1]
    , column <- [0 .. columns - 1]
    ]

replace :: Int -> value -> [value] -> [value]
replace target replacement = zipWith (\index value -> if index == target then replacement else value) [0 ..]

finiteDifference :: (Double -> Double) -> Double -> Double
finiteDifference function coordinate =
    let step = 1e-6 * max 1 (abs coordinate)
     in (function (coordinate + step) - function (coordinate - step)) / (2 * step)

checkAllCoordinates :: String -> [Double] -> [Double] -> (Int -> Double -> Double) -> IO ()
checkAllCoordinates label gradients coordinates objective = do
    unless (length gradients == length coordinates) (failTest (label ++ ": wrong gradient length"))
    forM_ (zip3 [0 :: Int ..] gradients coordinates) $ \(index, actual, coordinate) ->
        assertApprox (label ++ " finite difference " ++ show index) (finiteDifference (objective index) coordinate) actual

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

expectSession :: String -> (forall region. TensorSession region -> IO (Either TensorError ())) -> IO ()
expectSession label action = do
    result <- withTensorSession largeLimits action
    case result of
        Left problem -> failTest (label ++ ": " ++ show problem)
        Right () -> pure ()

expectRightIO :: (Show error) => String -> IO (Either error value) -> IO value
expectRightIO label action = action >>= expectRight label

expectRight :: (Show error) => String -> Either error value -> IO value
expectRight _ (Right value) = pure value
expectRight label (Left problem) = failTest (label ++ ": " ++ show problem)

whenSameStorage :: String -> HostTensor region dtype left -> HostTensor region other right -> IO ()
whenSameStorage message left right = when (sameStorage left right) (failTest message)

showEither :: (Show error) => Either error value -> String
showEither (Left problem) = "Left " ++ show problem
showEither (Right _) = "Right <value>"

failTest :: String -> IO value
failTest message = do
    putStrLn ("FAIL: " ++ message)
    exitFailure
