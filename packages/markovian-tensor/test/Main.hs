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
    viewReverseEquivalenceTests
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

viewReverseEquivalenceTests :: IO ()
viewReverseEquivalenceTests = expectSession "view reverse-equivalence session" $ \session -> do
    (leftBase, _) <- expectRightIO "view left base" (finiteTensorFromList session (matrixShape @2 @2) [1, 2, 3, 4])
    (rightBase, _) <- expectRightIO "view right base" (finiteTensorFromList session (matrixShape @2 @2) [5, 6, 7, 8])
    (seedBase, _) <- expectRightIO "view seed base" (finiteTensorFromList session (matrixShape @2 @2) [0.5, 2, -1, 0.25])
    let leftView = transposeFinite2D leftBase
        rightView = transposeFinite2D rightBase
        seedView = transposeFinite2D seedBase
        leftValues = [1, 3, 2, 4]
        rightValues = [5, 7, 6, 8]
        seedValues = [0.5, -1, 2, 0.25]
    assertEqual "view left coordinate oracle" leftValues =<< tensorToList (hostTensor leftView)
    assertEqual "view right coordinate oracle" rightValues =<< tensorToList (hostTensor rightView)
    assertEqual "view seed coordinate oracle" seedValues =<< tensorToList (hostTensor seedView)
    (leftMaterialized, _) <- expectRightIO "materialize view left" (contiguousCopy session leftView)
    (rightMaterialized, _) <- expectRightIO "materialize view right" (contiguousCopy session rightView)
    (seedMaterialized, _) <- expectRightIO "materialize view seed" (contiguousCopy session seedView)

    (directAdd, directAddTape, _) <- expectRightIO "direct view add tape" (addWithTape session leftView rightView)
    (referenceAdd, referenceAddTape, _) <- expectRightIO "materialized view add tape" (addWithTape session leftMaterialized rightMaterialized)
    directAddValues <- tensorToList (hostTensor directAdd)
    referenceAddValues <- tensorToList (hostTensor referenceAdd)
    assertEqual "view add primal materialize-first equivalence" referenceAddValues directAddValues
    ((directAddLeft, directAddRight), _) <- expectRightIO "direct view add VJP" (applyBinaryTape session directAddTape seedView)
    ((referenceAddLeft, referenceAddRight), _) <- expectRightIO "materialized view add VJP" (applyBinaryTape session referenceAddTape seedMaterialized)
    directAddLeftValues <- tensorToList (hostTensor directAddLeft)
    directAddRightValues <- tensorToList (hostTensor directAddRight)
    referenceAddLeftValues <- tensorToList (hostTensor referenceAddLeft)
    referenceAddRightValues <- tensorToList (hostTensor referenceAddRight)
    assertEqual "view add left materialize-first pullback" referenceAddLeftValues directAddLeftValues
    assertEqual "view add right materialize-first pullback" referenceAddRightValues directAddRightValues
    unless (sameStorage (hostTensor directAddLeft) (hostTensor seedView) && sameStorage (hostTensor directAddRight) (hostTensor seedView)) (failTest "view add pullback did not retain accepted immutable seed sharing")
    let addObjective leftCoordinates rightCoordinates = sum (zipWith (*) seedValues (zipWith (+) leftCoordinates rightCoordinates))
    checkAllCoordinates "transpose-view add left" directAddLeftValues leftValues (\index value -> addObjective (replace index value leftValues) rightValues)
    checkAllCoordinates "transpose-view add right" directAddRightValues rightValues (\index value -> addObjective leftValues (replace index value rightValues))

    (directMultiply, directMultiplyTape, _) <- expectRightIO "direct view multiply tape" (multiplyWithTape session leftView rightView)
    (referenceMultiply, referenceMultiplyTape, _) <- expectRightIO "materialized view multiply tape" (multiplyWithTape session leftMaterialized rightMaterialized)
    directMultiplyValues <- tensorToList (hostTensor directMultiply)
    referenceMultiplyValues <- tensorToList (hostTensor referenceMultiply)
    assertEqual "view multiply primal materialize-first equivalence" referenceMultiplyValues directMultiplyValues
    ((directMultiplyLeft, directMultiplyRight), _) <- expectRightIO "direct view multiply VJP" (applyBinaryTape session directMultiplyTape seedView)
    ((referenceMultiplyLeft, referenceMultiplyRight), _) <- expectRightIO "materialized view multiply VJP" (applyBinaryTape session referenceMultiplyTape seedMaterialized)
    directMultiplyLeftValues <- tensorToList (hostTensor directMultiplyLeft)
    directMultiplyRightValues <- tensorToList (hostTensor directMultiplyRight)
    referenceMultiplyLeftValues <- tensorToList (hostTensor referenceMultiplyLeft)
    referenceMultiplyRightValues <- tensorToList (hostTensor referenceMultiplyRight)
    assertEqual "view multiply left materialize-first pullback" referenceMultiplyLeftValues directMultiplyLeftValues
    assertEqual "view multiply right materialize-first pullback" referenceMultiplyRightValues directMultiplyRightValues
    assertFreshCotangentPair "direct view multiply" (hostTensor directMultiplyLeft) (hostTensor directMultiplyRight) (hostTensor leftView) (hostTensor rightView) (hostTensor seedView)
    assertFreshCotangentPair "materialized view multiply" (hostTensor referenceMultiplyLeft) (hostTensor referenceMultiplyRight) (hostTensor leftMaterialized) (hostTensor rightMaterialized) (hostTensor seedMaterialized)
    let multiplyObjective leftCoordinates rightCoordinates = sum (zipWith (*) seedValues (zipWith (*) leftCoordinates rightCoordinates))
    checkAllCoordinates "transpose-view multiply left" directMultiplyLeftValues leftValues (\index value -> multiplyObjective (replace index value leftValues) rightValues)
    checkAllCoordinates "transpose-view multiply right" directMultiplyRightValues rightValues (\index value -> multiplyObjective leftValues (replace index value rightValues))

    (directMatmul, directMatmulTape, _) <- expectRightIO "direct view matmul tape" (matmulWithTape session leftView rightView)
    (referenceMatmulOutput, referenceMatmulTape, _) <- expectRightIO "materialized view matmul tape" (matmulWithTape session leftMaterialized rightMaterialized)
    directMatmulValues <- tensorToList (hostTensor directMatmul)
    referenceMatmulValues <- tensorToList (hostTensor referenceMatmulOutput)
    assertEqual "view matmul primal materialize-first equivalence" referenceMatmulValues directMatmulValues
    ((directMatmulLeft, directMatmulRight), _) <- expectRightIO "direct view matmul VJP" (applyBinaryTape session directMatmulTape seedView)
    ((referenceMatmulLeft, referenceMatmulRight), _) <- expectRightIO "materialized view matmul VJP" (applyBinaryTape session referenceMatmulTape seedMaterialized)
    directMatmulLeftValues <- tensorToList (hostTensor directMatmulLeft)
    directMatmulRightValues <- tensorToList (hostTensor directMatmulRight)
    referenceMatmulLeftValues <- tensorToList (hostTensor referenceMatmulLeft)
    referenceMatmulRightValues <- tensorToList (hostTensor referenceMatmulRight)
    assertEqual "view matmul left materialize-first pullback" referenceMatmulLeftValues directMatmulLeftValues
    assertEqual "view matmul right materialize-first pullback" referenceMatmulRightValues directMatmulRightValues
    assertFreshCotangentPair "direct view matmul" (hostTensor directMatmulLeft) (hostTensor directMatmulRight) (hostTensor leftView) (hostTensor rightView) (hostTensor seedView)
    assertFreshCotangentPair "materialized view matmul" (hostTensor referenceMatmulLeft) (hostTensor referenceMatmulRight) (hostTensor leftMaterialized) (hostTensor rightMaterialized) (hostTensor seedMaterialized)
    let matmulObjective leftCoordinates rightCoordinates = sum (zipWith (*) seedValues (referenceMatmul 2 2 2 leftCoordinates rightCoordinates))
    checkAllCoordinates "transpose-view matmul left" directMatmulLeftValues leftValues (\index value -> matmulObjective (replace index value leftValues) rightValues)
    checkAllCoordinates "transpose-view matmul right" directMatmulRightValues rightValues (\index value -> matmulObjective leftValues (replace index value rightValues))

    (rectangularLeftBase, _) <- expectRightIO "rectangular view left base" (finiteTensorFromList session (matrixShape @2 @3) [0.125, 0.25, 0.375, 0.5, 0.625, 0.75])
    (rectangularRightBase, _) <- expectRightIO "rectangular view right base" (finiteTensorFromList session (matrixShape @4 @2) [0.125, -0.25, 0.375, -0.5, 0.625, -0.75, 0.875, -1])
    (rectangularSeedBase, _) <- expectRightIO "rectangular view seed base" (finiteTensorFromList session (matrixShape @4 @3) [0.5, -0.25, 0.125, 0.25, -0.5, 0.75, -0.75, 0.125, 0.5, -0.125, 0.75, -0.25])
    let rectangularLeftView = transposeFinite2D rectangularLeftBase
        rectangularRightView = transposeFinite2D rectangularRightBase
        rectangularSeedView = transposeFinite2D rectangularSeedBase
        rectangularLeftValues = [0.125, 0.5, 0.25, 0.625, 0.375, 0.75]
        rectangularRightValues = [0.125, 0.375, 0.625, 0.875, -0.25, -0.5, -0.75, -1]
        rectangularSeedValues = [0.5, 0.25, -0.75, -0.125, -0.25, -0.5, 0.125, 0.75, 0.125, 0.75, 0.5, -0.25]
    assertEqual "rectangular view left coordinate oracle" rectangularLeftValues =<< tensorToList (hostTensor rectangularLeftView)
    assertEqual "rectangular view right coordinate oracle" rectangularRightValues =<< tensorToList (hostTensor rectangularRightView)
    assertEqual "rectangular view seed coordinate oracle" rectangularSeedValues =<< tensorToList (hostTensor rectangularSeedView)
    (rectangularLeftMaterialized, _) <- expectRightIO "materialize rectangular view left" (contiguousCopy session rectangularLeftView)
    (rectangularRightMaterialized, _) <- expectRightIO "materialize rectangular view right" (contiguousCopy session rectangularRightView)
    (rectangularSeedMaterialized, _) <- expectRightIO "materialize rectangular view seed" (contiguousCopy session rectangularSeedView)
    (directRectangularMatmul, directRectangularTape, _) <- expectRightIO "direct rectangular view matmul tape" (matmulWithTape session rectangularLeftView rectangularRightView)
    (referenceRectangularMatmul, referenceRectangularTape, _) <- expectRightIO "materialized rectangular view matmul tape" (matmulWithTape session rectangularLeftMaterialized rectangularRightMaterialized)
    directRectangularOutputValues <- tensorToList (hostTensor directRectangularMatmul)
    referenceRectangularOutputValues <- tensorToList (hostTensor referenceRectangularMatmul)
    assertEqual "rectangular view matmul primal materialize-first equivalence" referenceRectangularOutputValues directRectangularOutputValues
    ((directRectangularLeft, directRectangularRight), _) <- expectRightIO "direct rectangular view matmul VJP" (applyBinaryTape session directRectangularTape rectangularSeedView)
    ((referenceRectangularLeft, referenceRectangularRight), _) <- expectRightIO "materialized rectangular view matmul VJP" (applyBinaryTape session referenceRectangularTape rectangularSeedMaterialized)
    directRectangularLeftValues <- tensorToList (hostTensor directRectangularLeft)
    directRectangularRightValues <- tensorToList (hostTensor directRectangularRight)
    referenceRectangularLeftValues <- tensorToList (hostTensor referenceRectangularLeft)
    referenceRectangularRightValues <- tensorToList (hostTensor referenceRectangularRight)
    assertEqual "rectangular view matmul left materialize-first pullback" referenceRectangularLeftValues directRectangularLeftValues
    assertEqual "rectangular view matmul right materialize-first pullback" referenceRectangularRightValues directRectangularRightValues
    assertFreshCotangentPair "direct rectangular view matmul" (hostTensor directRectangularLeft) (hostTensor directRectangularRight) (hostTensor rectangularLeftView) (hostTensor rectangularRightView) (hostTensor rectangularSeedView)
    assertFreshCotangentPair "materialized rectangular view matmul" (hostTensor referenceRectangularLeft) (hostTensor referenceRectangularRight) (hostTensor rectangularLeftMaterialized) (hostTensor rectangularRightMaterialized) (hostTensor rectangularSeedMaterialized)
    let rectangularMatmulObjective leftCoordinates rightCoordinates = sum (zipWith (*) rectangularSeedValues (referenceMatmul 3 2 4 leftCoordinates rightCoordinates))
    checkAllCoordinates "rectangular transpose-view matmul left" directRectangularLeftValues rectangularLeftValues (\index value -> rectangularMatmulObjective (replace index value rectangularLeftValues) rectangularRightValues)
    checkAllCoordinates "rectangular transpose-view matmul right" directRectangularRightValues rectangularRightValues (\index value -> rectangularMatmulObjective rectangularLeftValues (replace index value rectangularRightValues))

    (directTanh, directTanhTape, _) <- expectRightIO "direct view tanh tape" (tanhWithTape session leftView)
    (referenceTanh, referenceTanhTape, _) <- expectRightIO "materialized view tanh tape" (tanhWithTape session leftMaterialized)
    directTanhOutputValues <- tensorToList (hostTensor directTanh)
    referenceTanhOutputValues <- tensorToList (hostTensor referenceTanh)
    assertEqual "view tanh primal materialize-first equivalence" referenceTanhOutputValues directTanhOutputValues
    (directTanhGradient, _) <- expectRightIO "direct view tanh VJP" (applyUnaryTape session directTanhTape seedView)
    (referenceTanhGradient, _) <- expectRightIO "materialized view tanh VJP" (applyUnaryTape session referenceTanhTape seedMaterialized)
    directTanhValues <- tensorToList (hostTensor directTanhGradient)
    referenceTanhValues <- tensorToList (hostTensor referenceTanhGradient)
    assertEqual "view tanh materialize-first pullback" referenceTanhValues directTanhValues
    let tanhObjective coordinates = sum (zipWith (*) seedValues (map tanh coordinates))
    checkAllCoordinates "transpose-view tanh" directTanhValues leftValues (\index value -> tanhObjective (replace index value leftValues))

    (directSum, directSumTape, _) <- expectRightIO "direct view sum tape" (sumWithTape session leftView)
    (referenceSum, referenceSumTape, _) <- expectRightIO "materialized view sum tape" (sumWithTape session leftMaterialized)
    directSumOutputValues <- tensorToList (hostTensor directSum)
    referenceSumOutputValues <- tensorToList (hostTensor referenceSum)
    assertEqual "view sum primal materialize-first equivalence" referenceSumOutputValues directSumOutputValues
    (sumSeed, _) <- expectRightIO "view sum seed" (finiteTensorFromList session SNil [0.75])
    (directSumGradient, _) <- expectRightIO "direct view sum VJP" (applyUnaryTape session directSumTape sumSeed)
    (referenceSumGradient, _) <- expectRightIO "materialized view sum VJP" (applyUnaryTape session referenceSumTape sumSeed)
    directSumValues <- tensorToList (hostTensor directSumGradient)
    referenceSumValues <- tensorToList (hostTensor referenceSumGradient)
    assertEqual "view sum materialize-first pullback" referenceSumValues directSumValues
    checkAllCoordinates "transpose-view sum" directSumValues leftValues (\index value -> 0.75 * sum (replace index value leftValues))
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

assertFreshCotangentPair :: String -> HostTensor region leftDtype leftShape -> HostTensor region rightDtype rightShape -> HostTensor region firstDtype firstShape -> HostTensor region secondDtype secondShape -> HostTensor region seedDtype seedShape -> IO ()
assertFreshCotangentPair label left right firstOperand secondOperand seed = do
    whenSameStorage (label ++ " left cotangent reused first operand storage") left firstOperand
    whenSameStorage (label ++ " left cotangent reused second operand storage") left secondOperand
    whenSameStorage (label ++ " left cotangent reused seed storage") left seed
    whenSameStorage (label ++ " right cotangent reused first operand storage") right firstOperand
    whenSameStorage (label ++ " right cotangent reused second operand storage") right secondOperand
    whenSameStorage (label ++ " right cotangent reused seed storage") right seed
    whenSameStorage (label ++ " cotangents shared storage") left right

showEither :: (Show error) => Either error value -> String
showEither (Left problem) = "Left " ++ show problem
showEither (Right _) = "Right <value>"

failTest :: String -> IO value
failTest message = do
    putStrLn ("FAIL: " ++ message)
    exitFailure
