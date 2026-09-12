{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AffineContractTests (affineContractTests) where

import Control.Monad (forM_, join, replicateM, unless, when)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Markovian.Tensor
import Markovian.Tensor.Affine
import Markovian.Tensor.Ownership
import Markovian.Tensor.Primitive
import Markovian.Tensor.Reverse
import Numeric.Natural (Natural)

assertEqual :: (Eq value, Show value) => String -> value -> value -> IO ()
assertEqual label expected actual = unless (expected == actual) (fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual))

-- Same host-F64 tolerance as Main's primitive-law tests; reports, selection
-- coordinates, IEEE zero signs and the fixed Rational oracle remain exact.
assertApproximateList :: String -> [Double] -> [Double] -> IO ()
assertApproximateList label expected actual = do
    assertEqual (label ++ " length") (length expected) (length actual)
    forM_ (zip expected actual) $ \(wanted, got) ->
        unless (abs (wanted - got) <= 2e-10 + 2e-8 * max (abs wanted) (abs got)) (fail (label ++ ": expected " ++ show wanted ++ ", got " ++ show got))

right :: (Show problem) => Either problem value -> IO value
right = either (fail . show) pure

reject :: String -> TensorError -> Either TensorError value -> IO ()
reject label expected result = case result of
    Left actual -> assertEqual label expected actual
    Right _ -> fail (label ++ ": unexpectedly accepted")

policyWith :: Int -> Int -> Int -> IO AffineLimits
policyWith cells work live = right (affineLimits maxBound maxBound maxBound cells work live)

roomy :: IO AffineLimits
roomy = policyWith maxBound maxBound maxBound

sessionLimits :: AffineLimits -> SessionLimits
sessionLimits = tensorSessionLimitsWithAffine (tensorSessionLimits 16 (fromIntegral (maxBound :: Int)) (fromIntegral (maxBound :: Int)) 1000000 10000000 4096 10000000)

mapReport :: AffineLimits -> SShape base -> SShape view -> Int -> [Int] -> Either TensorError AffineMapReport
mapReport policy base view offset strides = do
    budget <- affineBudget policy
    withAffineMap budget base view offset strides (\_ _ report -> report)

affineContractTests :: IO ()
affineContractTests = do
    policyAndPrefixTests
    reservationTests
    geometryTests
    signedRuntimeTests
    transformRuntimeTests
    mixedHistoryTests
    completeGeometryTests
    signedPrimitiveTests
    putStrLn "affine: public policy, prefix, geometry, runtime and mixed-history contracts passed"

policyAndPrefixTests :: IO ()
policyAndPrefixTests = do
    reject "all negatives precede minima" (TensorAffineError (AffineInvalidLimit AffineWork (-1))) (affineLimits 0 0 0 0 (-1) 0)
    reject "cells minimum first" (TensorAffineError (AffineLimitTooSmall AffineConstructedCells 4672 4671)) (affineLimits 0 0 0 4671 0 0)
    reject "work minimum" (TensorAffineError (AffineLimitTooSmall AffineWork 512 511)) (affineLimits 0 0 0 4672 511 0)
    reject "live minimum" (TensorAffineError (AffineLimitTooSmall AffineLiveCells 576 575)) (affineLimits 0 0 0 4672 512 575)
    startup <- policyWith 4672 512 576
    budget <- right (affineBudget startup)
    assertEqual "startup usage" (AffineUsage 4672 512 576) (affineBudgetUsage budget)
    reject
        "first debit before shape or signed input"
        (TensorAffineError (AffineLimitExceeded AffineConstructedCells 4672 4673))
        (withAffineMap budget (error "unpaid base" :: SShape '[]) (error "unpaid view" :: SShape '[]) (error "unpaid offset") (error "unpaid list") (\_ _ _ -> ()))
    two <- policyWith maxBound 1280 732
    let poisoned = SCons (Proxy @1) (SCons (Proxy @1) (error "third node unpaid" :: SShape '[1]))
    reject "two coupons only" (TensorAffineError (AffineLimitExceeded AffineWork 1280 1281)) (mapReport two poisoned SNil 0 [])
    rankZero <- right (affineLimits 0 maxBound maxBound maxBound maxBound maxBound)
    reject "rank before dimension" (TensorAffineError (AffineLimitExceeded AffineRank 0 1)) (mapReport rankZero (knownShape @'[999]) SNil 0 [])
    _ <- right (mapReport rankZero SNil SNil 0 [])
    dimFive <- right (affineLimits 1 5 maxBound maxBound maxBound maxBound)
    reject "dimension before later rank" (TensorAffineError (AffineLimitExceeded AffineDimension 5 6)) (mapReport dimFive (knownShape @'[999, 1]) SNil 0 [])
    reject "oversized supplied Nat is saturated" (TensorAffineError (AffineLimitExceeded AffineDimension 5 6)) (mapReport dimFive (knownShape @'[18446744073709551616]) SNil 0 [])
    zeroElements <- right (affineLimits 3 maxBound 0 maxBound maxBound maxBound)
    reject "scalar is one" (TensorAffineError (AffineLimitExceeded AffineElements 0 1)) (mapReport zeroElements SNil SNil 0 [])
    _ <- right (mapReport zeroElements (knownShape @'[9223372036854775807, 9223372036854775807, 0]) (knownShape @'[0]) 0 [0])
    policy <- roomy
    reject "F64 machine wrapper" (TensorShapeError (MachineIndexOverflow 9223372036854775808)) (mapReport policy (knownShape @'[1152921504606846976]) SNil 0 [])
    reject
        "long list does not inspect excess value/tail"
        (TensorAffineError (AffineListLength AffineStrides 1 2))
        (mapReport policy (knownShape @'[5]) (knownShape @'[3]) 0 (1 : error "excess element" : error "excess tail"))
    reject "infinite list bounded by expected+1" (TensorAffineError (AffineListLength AffineStrides 1 2)) (mapReport policy (knownShape @'[5]) (knownShape @'[3]) 0 (repeat 1))

reservationTests :: IO ()
reservationTests = do
    -- Literal r4 fixtures, independently frozen in FIXTURES-OPERATIVE.md.
    check (knownShape @'[5]) (knownShape @'[3]) 4 [-2] (AffineCharge 63583 7796 1215 235) (AffineUsage 68255 8308 1215)
    check (knownShape @'[5]) (knownShape @'[0]) 0 [0] (AffineCharge 44863 5456 1215 235) (AffineUsage 49535 5968 1215)
    check SNil SNil 0 [] (AffineCharge 33660 4078 1036 195) (AffineUsage 38332 4590 1036)
    check (knownShape @'[5]) (knownShape @'[1]) 2 [-7] (AffineCharge 47295 5760 1215 235) (AffineUsage 51967 6272 1215)
    overlap <- policyWith 104142 12778 1342
    reject "exact overlap reaches semantic suffix" (TensorAffineError (AffineOverlap 1 2 1)) (mapReport overlap (knownShape @'[4]) (knownShape @'[2, 2]) 0 [1, 1])
    empty <- policyWith 59278 7170 1342
    reject "exact raw-empty" (TensorAffineError AffineEmptyDescriptor) (mapReport empty (knownShape @'[5]) (knownShape @'[0, 1]) 0 [0, 7])
    below <- policyWith 59278 7169 1342
    reject "full plan before raw-empty" (TensorAffineError (AffineLimitExceeded AffineWork 7169 7170)) (mapReport below (knownShape @'[5]) (knownShape @'[0, 1]) 0 [0, 7])
  where
    check :: SShape base -> SShape view -> Int -> [Int] -> AffineCharge -> AffineUsage -> IO ()
    check base view offset strides charge cumulative@(AffineUsage cells work live) = do
        exact <- policyWith (fromIntegral cells) (fromIntegral work) (fromIntegral live)
        report <- right (mapReport exact base view offset strides)
        assertEqual "fixed reservation" (AffineMapReport charge cumulative) report
        forM_ [(AffineConstructedCells, cells - 1, work, live), (AffineWork, cells, work - 1, live), (AffineLiveCells, cells, work, live - 1), (AffineConstructedCells, cells - 1, work - 1, live - 1)] $ \(field, c, w, q) -> do
            limited <- policyWith (fromIntegral c) (fromIntegral w) (fromIntegral q)
            let cap = case field of AffineConstructedCells -> c; AffineWork -> w; _ -> q
            reject "one-below reservation before signed values" (TensorAffineError (AffineLimitExceeded field cap (cap + 1))) (mapReport limited base view (error "unadmitted signed offset") strides)

geometryTests :: IO ()
geometryTests = do
    policy <- roomy
    budget <- right (affineBudget policy)
    reject "signed range before raw empty" (TensorAffineError (AffineSignedRange AffineStrides 1)) (mapReport policy (knownShape @'[5]) (knownShape @'[0, 1]) 1 [0, minBound])
    reject "offset signed range first" (TensorAffineError (AffineSignedRange AffineOffset 0)) (mapReport policy (knownShape @'[5]) (knownShape @'[0]) minBound [minBound])
    reject "negative extrema" (TensorAffineError (AffineAddressBounds (-2) 0 5)) (mapReport policy (knownShape @'[5]) (knownShape @'[3]) 0 [-1])
    reject "extent overflow before bounds" (TensorAffineError (AffineArithmeticOverflow AffineOffset 0)) (mapReport policy (knownShape @'[5]) (knownShape @'[2]) maxBound [1])
    join $ right $ withAffineMap budget (knownShape @'[10]) (knownShape @'[3]) 0 [2] $ \parent next _ -> do
        reject "parent coordinate, not capacity" (TensorAffineError (AffineParentDomain 0 1 3 3)) (sliceAffineMap next parent (knownShape @'[3]) [1] [1] (\_ _ _ -> ()))
        _ <- right (sliceAffineMap next parent (knownShape @'[2]) [1] [-1] (\_ _ _ -> ()))
        _ <- right (sliceAffineMap next parent (knownShape @'[0]) [3] [-1] (\_ _ _ -> ()))
        reject "zero step even empty" (TensorAffineError (AffineZeroStep 0)) (sliceAffineMap next parent (knownShape @'[0]) [3] [0] (\_ _ _ -> ()))
        reject "signed steps before zero steps" (TensorAffineError (AffineSignedRange AffineSteps 0)) (sliceAffineMap next parent (knownShape @'[0]) [3] [minBound] (\_ _ _ -> ()))
        reject "transform rank before input list" (TensorAffineError (AffineShapeDisagreement AffineTargetShape 0 1 0)) (permuteAffineMap next parent SNil (error "unpaid permutation") (\_ _ _ -> ()))
    join $ right $ withAffineMap budget SNil SNil 0 [] $ \parent next _ ->
        reject "rank-zero reverse always fails" (TensorAffineError (AffineAxisOutOfRange 0 0)) (reverseAffineMap next parent 0 (\_ _ _ -> ()))
    join $ right $ withAffineMap budget (knownShape @'[6]) (knownShape @'[2, 3]) 0 [3, 1] $ \parent next _ -> do
        reject "permutation ranges before duplicates" (TensorAffineError (AffineAxisOutOfRange 2 2)) (permuteAffineMap next parent (knownShape @'[2, 3]) [2, 2] (\_ _ _ -> ()))
        reject "first duplicate pair" (TensorAffineError (AffineDuplicateAxis 0 1)) (permuteAffineMap next parent (knownShape @'[2, 3]) [0, 0] (\_ _ _ -> ()))
        reject "permutation target dimension" (TensorAffineError (AffineShapeDisagreement AffineTargetShape 0 3 2)) (permuteAffineMap next parent (knownShape @'[2, 3]) [1, 0] (\_ _ _ -> ()))
        _ <- right (permuteAffineMap next parent (knownShape @'[3, 2]) [1, 0] (\_ _ _ -> ()))
        pure ()

signedRuntimeTests :: IO ()
signedRuntimeTests = do
    policy <- roomy
    budget <- right (affineBudget policy)
    action <- right $ withAffineMap budget (knownShape @'[5]) (knownShape @'[3]) 4 [-2] $ \witness _ _ ->
        withTensorSession (sessionLimits policy) $ \session -> do
            (base, _) <- finiteTensorFromList session (knownShape @'[5]) [1, 2, 3, 4, 5] >>= right
            (seed, _) <- finiteTensorFromList session (knownShape @'[3]) [10, 20, 30] >>= right
            owner <- right (tensorOwner "original" (knownShape @'[5]))
            (binding, bindReport) <- bindAffineView session witness (ownTensor owner base) >>= right
            assertEqual "bind exact report" (AffineOperationReport (AffineCharge 45897 5539 1585 326) (AffineUsage 50569 6051 1585) (TensorOperationReport "affine/bind" 0 (TensorMemoryReport 0 0 0 0))) bindReport
            assertEqual "signed gather" [5, 3, 1] =<< tensorToList (hostTensor (affineViewTensor binding))
            unless (sameStorage (hostTensor base) (hostTensor (affineViewTensor binding))) (fail "bind copied payload")
            (gradient, report) <- pullbackAffineView session binding seed >>= right
            assertEqual "full base gradient" [30, 0, 20, 0, 10] =<< tensorToList (hostTensor (ownedFiniteTensor gradient))
            assertEqual "actual owner" "original" (ownerKey (ownedTensorOwner gradient))
            assertEqual "pullback exact report" (AffineOperationReport (AffineCharge 72873 8843 2129 377) (AffineUsage 123442 14894 2129) (TensorOperationReport "vjp/affine-base" 8 (TensorMemoryReport 40 40 0 1))) report
            when (sameStorage (hostTensor base) (hostTensor (ownedFiniteTensor gradient)) || sameStorage (hostTensor seed) (hostTensor (ownedFiniteTensor gradient))) (fail "pullback not fresh")
            let objective [a, _, c, _, e] = 10 * e + 20 * c + 30 * a
                objective _ = error "fixed objective length"
                quadratic [a, _, c, _, e] = e * e + 2 * c * c + 3 * a * a
                quadratic _ = error "fixed quadratic length"
                perturb axis delta = [x + if i == axis then delta else 0 | (i, x) <- zip [0 :: Int ..] [1, 2, 3, 4, 5]]
                differences f = [(f (perturb i (1 / 1024)) - f (perturb i ((-1) / 1024))) * 512 | i <- [0 .. 4]]
            assertEqual "all five independent F64 finite differences" ([30, 0, 20, 0, 10] :: [Double]) (differences objective)
            assertEqual "quadratic all five F64 differences" ([6, 0, 12, 0, 10] :: [Double]) (differences quadratic)
            assertEqual "all five independent Rational differences" ([30, 0, 20, 0, 10] :: [Rational]) (differences objective)
            assertEqual "quadratic all five Rational differences" ([6, 0, 12, 0, 10] :: [Rational]) (differences quadratic)
            let selection :: [[Rational]]
                selection = [[0, 0, 0, 0, 1], [0, 0, 1, 0, 0], [1, 0, 0, 0, 0]]
                cotangent = [10, 20, 30]
                direction = [2, -1, 3, 4, -2]
                dot xs ys = sum (zipWith (*) xs ys)
                matrixGradient = foldr (zipWith (+)) (replicate 5 0) (zipWith (\weight row -> map (weight *) row) cotangent selection)
                selectedDirection = map (`dot` direction) selection
            assertEqual "independent selection-matrix full transpose" [30, 0, 20, 0, 10] matrixGradient
            assertEqual "independent selected direction" [-2, 3, 2] selectedDirection
            assertEqual "exact left pairing" 100 (dot cotangent selectedDirection)
            assertEqual "exact right pairing" 100 (dot matrixGradient direction)
            (quadraticSeed, _) <- finiteTensorFromList session (knownShape @'[3]) [10, 12, 6] >>= right
            (quadraticGradient, _) <- pullbackAffineView session binding quadraticSeed >>= right
            assertEqual "nonlinear seed full gradient" [6, 0, 12, 0, 10] =<< tensorToList (hostTensor (ownedFiniteTensor quadraticGradient))
            (negativeSeed, _) <- finiteTensorFromList session (knownShape @'[3]) [-0.0, 20, 30] >>= right
            (negativeGradient, _) <- pullbackAffineView session binding negativeSeed >>= right
            values <- tensorToList (hostTensor (ownedFiniteTensor negativeGradient))
            assertEqual "selected negative zero and omitted positive zeros" [False, False, False, False, True] (map isNegativeZero values)
            (copy, _) <- contiguousCopy session (affineViewTensor binding) >>= right
            assertEqual "signed copy" [5, 3, 1] =<< tensorToList (hostTensor copy)
            (sumTensor, _) <- sumAll session (affineViewTensor binding) >>= right
            assertEqual "signed reduction" [9] =<< tensorToList (hostTensor sumTensor)
            let secondOwned = ownTensor (either (error . show) id (tensorOwner "different owner" (knownShape @'[5]))) base
            (rebound, _) <- bindAffineView session witness secondOwned >>= right
            (otherGradient, _) <- pullbackAffineView session rebound seed >>= right
            assertEqual "same storage distinct explicit owner" "different owner" (ownerKey (ownedTensorOwner otherGradient))
            pure (Right ())
    action >>= right

transformRuntimeTests :: IO ()
transformRuntimeTests = do
    policy <- roomy
    budget <- right (affineBudget policy)
    join $ right $ withAffineMap budget (knownShape @'[5]) (knownShape @'[3]) 4 [-2] $ \parent next _ -> do
        join $ right $ reverseAffineMap next parent 0 $ \child childBudget report -> do
            assertEqual "transform fixed reservation" (AffineMapReport (AffineCharge 81898 10057 1442 283) (AffineUsage 150153 18365 1442)) report
            withTensorSession
                (sessionLimits policy)
                ( \session -> do
                    (base, _) <- finiteTensorFromList session (knownShape @'[5]) [11, 22, 33, 44, 55] >>= right
                    owner <- right (tensorOwner "base" (knownShape @'[5]))
                    (bound, _) <- bindAffineView session child (ownTensor owner base) >>= right
                    assertEqual "reversed signed map" [11, 33, 55] =<< tensorToList (hostTensor (affineViewTensor bound))
                    join $ right $ reverseAffineMap childBudget child 0 $ \twice _ _ -> do
                        (boundAgain, _) <- bindAffineView session twice (ownTensor owner base) >>= right
                        assertEqual "double reverse values" [55, 33, 11] =<< tensorToList (hostTensor (affineViewTensor boundAgain))
                    pure (Right ())
                )
                >>= right
    selected <- right $ withAffineMap budget (knownShape @'[5]) (knownShape @'[2]) 2 [1] $ \witness _ _ ->
        withTensorSession (sessionLimits policy) $ \session -> do
            (base, _) <- finiteTensorFromList session (knownShape @'[5]) [11, 22, 33, 44, 55] >>= right
            owner <- right (tensorOwner "outer" (knownShape @'[5]))
            (bound, _) <- bindAffineView session witness (ownTensor owner base) >>= right
            reshaped <- right (reshapeFiniteContiguous session (knownShape @'[1, 2]) (affineViewTensor bound))
            assertEqual "offset reshape" [33, 44] =<< tensorToList (hostTensor reshaped)
            join $ right $ withAffineMap budget (knownShape @'[2]) (knownShape @'[1]) 1 [1] $ \inner _ _ -> do
                innerOwner <- right (tensorOwner "new logical base" (knownShape @'[2]))
                (innerBound, _) <- bindAffineView session inner (ownTensor innerOwner (affineViewTensor bound)) >>= right
                assertEqual "physical shift" [44] =<< tensorToList (hostTensor (affineViewTensor innerBound))
                (seed, _) <- finiteTensorFromList session (knownShape @'[1]) [9] >>= right
                (gradient, _) <- pullbackAffineView session innerBound seed >>= right
                assertEqual "redeclared logical base not capacity" [0, 9] =<< tensorToList (hostTensor (ownedFiniteTensor gradient))
            pure (Right ())
    selected >>= right

-- The supplied coordinate lists below are independent of the runtime address kernel.
completeGeometryTests :: IO ()
completeGeometryTests = do
    p <- roomy
    budget <- right (affineBudget p)
    let run action = action >>= right
    join $ right $ withAffineMap budget (knownShape @'[6]) (knownShape @'[2, 3]) 0 [3, 1] $ \parent next _ -> do
        join $ right $ permuteAffineMap next parent (knownShape @'[3, 2]) [1, 0] $ \witness childBudget _ ->
            run $ withTensorSession (sessionLimits p) $ \session -> do
                (base, _) <- finiteTensorFromList session (knownShape @'[6]) [0 .. 5] >>= right
                owner <- right (tensorOwner "permutation original" (knownShape @'[6]))
                (bound, _) <- bindAffineView session witness (ownTensor owner base) >>= right
                assertEqual "multiaxis permutation coordinates" [0, 3, 1, 4, 2, 5] =<< tensorToList (hostTensor (affineViewTensor bound))
                (seed, _) <- finiteTensorFromList session (knownShape @'[3, 2]) [1 .. 6] >>= right
                (gradient, _) <- pullbackAffineView session bound seed >>= right
                assertEqual "permutation all base coordinates" [1, 3, 5, 2, 4, 6] =<< tensorToList (hostTensor (ownedFiniteTensor gradient))
                join $ right $ reverseAffineMap childBudget witness 0 $ \reversed _ _ -> do
                    (chained, _) <- bindAffineView session reversed (ownTensor owner base) >>= right
                    assertEqual "permutation reverse chain" [2, 5, 1, 4, 0, 3] =<< tensorToList (hostTensor (affineViewTensor chained))
                    (g, _) <- pullbackAffineView session chained seed >>= right
                    assertEqual "chain original base gradient" [5, 3, 1, 6, 4, 2] =<< tensorToList (hostTensor (ownedFiniteTensor g))
                    assertEqual "chain retains original base storage" (tensorStorageId (hostTensor base)) (tensorStorageId (hostTensor (ownedFiniteTensor (affineViewBase chained))))
                pure (Right ())
    join $ right $ withAffineMap budget (knownShape @'[10]) (knownShape @'[3]) 0 [2] $ \parent next _ -> do
        join $ right $ sliceAffineMap next parent (knownShape @'[2]) [1] [-1] $ \witness _ _ ->
            run $ withTensorSession (sessionLimits p) $ \session -> do
                (base, _) <- finiteTensorFromList session (knownShape @'[10]) [0 .. 9] >>= right
                owner <- right (tensorOwner "slice original" (knownShape @'[10]))
                (bound, _) <- bindAffineView session witness (ownTensor owner base) >>= right
                assertEqual "negative parent slice" [2, 0] =<< tensorToList (hostTensor (affineViewTensor bound))
                (seed, _) <- finiteTensorFromList session (knownShape @'[2]) [7, 9] >>= right
                (gradient, _) <- pullbackAffineView session bound seed >>= right
                assertEqual "slice all ten original coordinates" [9, 0, 7, 0, 0, 0, 0, 0, 0, 0] =<< tensorToList (hostTensor (ownedFiniteTensor gradient))
                pure (Right ())
        reject "empty anchor beyond source" (TensorAffineError (AffineParentDomain 0 4 4 3)) (sliceAffineMap next parent (knownShape @'[0]) [4] [1] (\_ _ _ -> ()))
        reject "nonempty at empty anchor" (TensorAffineError (AffineParentDomain 0 3 3 3)) (sliceAffineMap next parent (knownShape @'[1]) [3] [1] (\_ _ _ -> ()))
    join $ right $ withAffineMap budget (knownShape @'[6]) (knownShape @'[2, 3]) 0 [3, 1] $ \parent next _ -> do
        reject "globally empty still checks later domain" (TensorAffineError (AffineParentDomain 1 3 3 3)) (sliceAffineMap next parent (knownShape @'[0, 1]) [2, 3] [1, 1] (\_ _ _ -> ()))
        join $ right $ sliceAffineMap next parent (knownShape @'[0, 1]) [2, 2] [1, 1] $ \empty nextBudget _ ->
            reject "empty reverse invalid axis" (TensorAffineError (AffineAxisOutOfRange 2 2)) (reverseAffineMap nextBudget empty 2 (\_ _ _ -> ()))
    checkSmall p budget SNil SNil [4] 0 [] [9] [9]
    checkSmall p budget (knownShape @'[5]) (knownShape @'[1]) [1 .. 5] 2 [-7] [9] [0, 0, 9, 0, 0]
    checkSmall p budget (knownShape @'[5]) (knownShape @'[0]) [1 .. 5] 0 [0] [] [0, 0, 0, 0, 0]
    checkSmall p budget (knownShape @'[0]) (knownShape @'[0]) [] 0 [0] [] []
  where
    checkSmall :: AffineLimits -> AffineBudget -> SShape base -> SShape view -> [Double] -> Int -> [Int] -> [Double] -> [Double] -> IO ()
    checkSmall p budget bs vs values offset strides seeds expected = do
        action <- right $ withAffineMap budget bs vs offset strides $ \witness _ _ ->
            withTensorSession (sessionLimits p) $ \session -> do
                (base, _) <- finiteTensorFromList session bs values >>= right
                (otherBase, _) <- finiteTensorFromList session bs values >>= right
                (seed, _) <- finiteTensorFromList session vs seeds >>= right
                owner <- right (tensorOwner "first allocation" bs)
                otherOwner <- right (tensorOwner "distinct allocation" bs)
                (bound, _) <- bindAffineView session witness (ownTensor owner base) >>= right
                (rebound, _) <- bindAffineView session witness (ownTensor otherOwner otherBase) >>= right
                assertEqual "rebind supplied base identity" (tensorStorageId (hostTensor otherBase)) (tensorStorageId (hostTensor (ownedFiniteTensor (affineViewBase rebound))))
                let initialIds = map (tensorStorageId . hostTensor) [base, otherBase] ++ [tensorStorageId (hostTensor seed), tensorStorageId (hostTensor (affineViewTensor bound))]
                    pull prior binding = do
                        (gradient, _) <- pullbackAffineView session binding seed >>= right
                        let tensor = hostTensor (ownedFiniteTensor gradient)
                        actual <- tensorToList tensor
                        assertEqual "small complete pullback" expected actual
                        assertEqual "omitted positive zeros" (replicate (length actual) False) (map isNegativeZero actual)
                        when (tensorStorageId tensor `elem` prior) (fail "gradient reused base/view/seed/prior ID")
                        pure (tensorStorageId tensor : prior)
                prior <- pull initialIds bound
                priorAgain <- pull prior bound
                _ <- pull priorAgain rebound
                (otherGradient, _) <- pullbackAffineView session rebound seed >>= right
                assertEqual "distinct supplied owner retained" "distinct allocation" (ownerKey (ownedTensorOwner otherGradient))
                pure (Right ())
        action >>= right

signedView :: TensorSession (region :: Type) -> AffineBudget -> SShape base -> SShape view -> [Double] -> Int -> [Int] -> IO (FiniteTensor region 'F64 view)
signedView session budget bs vs values offset strides = do
    join $ right $ withAffineMap budget bs vs offset strides $ \witness _ _ -> do
        (base, _) <- finiteTensorFromList session bs values >>= right
        owner <- right (tensorOwner "signed operand" bs)
        (bound, _) <- bindAffineView session witness (ownTensor owner base) >>= right
        pure (affineViewTensor bound)

signedPrimitiveTests :: IO ()
signedPrimitiveTests = do
    p <- roomy
    budget <- right (affineBudget p)
    withTensorSession
        (sessionLimits p)
        ( \session -> do
            left <- signedView session budget (knownShape @'[5]) (knownShape @'[3]) [1, 99, 2, 98, 3] 4 [-2]
            rightOperand <- signedView session budget (knownShape @'[5]) (knownShape @'[3]) [97, 4, 5, 6, 96] 1 [1]
            seed <- signedView session budget (knownShape @'[5]) (knownShape @'[3]) [0.5, 95, -1, 94, 2] 4 [-2]
            let xs = [3, 2, 1]
                ys = [4, 5, 6]
                zs = [2, -1, 0.5]
                observe label expected tensor = assertEqual label expected =<< tensorToList (hostTensor tensor)
                report name work bytes buffers = TensorOperationReport name work (TensorMemoryReport bytes (if buffers == 0 then 0 else bytes `div` buffers) 0 buffers)
            observe "actual signed independent seed offset4 count3 capacity5" zs seed
            (a, at, ar) <- addWithTape session left rightOperand >>= right
            observe "signed add" (zipWith (+) xs ys) a
            assertEqual "signed add report" (report "add" 3 24 1) ar
            ((al, av), avr) <- applyBinaryTape session at seed >>= right
            observe "signed add left VJP" zs al
            observe "signed add right VJP" zs av
            assertEqual "signed add no allocation report" (report "vjp/add" 0 0 0) avr
            unless (sameStorage (hostTensor al) (hostTensor seed) && sameStorage (hostTensor av) (hostTensor seed)) (fail "AddTape failed actual seed sharing")
            (m, mt, mr) <- multiplyWithTape session left rightOperand >>= right
            observe "signed multiply" (zipWith (*) xs ys) m
            assertEqual "signed multiply report" (report "multiply" 3 24 1) mr
            ((ml, mv), mvr) <- applyBinaryTape session mt seed >>= right
            observe "signed multiply left VJP" (zipWith (*) zs ys) ml
            observe "signed multiply right VJP" (zipWith (*) zs xs) mv
            assertEqual "signed multiply VJP report" (report "vjp/multiply" 6 48 2) mvr
            forM_ [("negate", negateTensor, map negate xs), ("tanh", tanhTensor, map tanh xs), ("contiguous-copy", contiguousCopy, xs)] $ \(name, operation, expected) -> do
                (output, actualReport) <- operation session left >>= right
                if name == "tanh"
                    then assertApproximateList "signed tanh" expected =<< tensorToList (hostTensor output)
                    else observe ("signed " ++ name) expected output
                assertEqual (name ++ " report") (report name 3 24 1) actualReport
            (t, tt, _) <- tanhWithTape session left >>= right
            assertApproximateList "signed tanh tape primal" (map tanh xs) =<< tensorToList (hostTensor t)
            (tg, tr) <- applyUnaryTape session tt seed >>= right
            tanhGradient <- tensorToList (hostTensor tg)
            assertApproximateList "signed tanh full VJP" (zipWith (\z x -> z * (1 - tanh x * tanh x)) zs xs) tanhGradient
            let objective coordinates = sum (zipWith (*) zs (map tanh coordinates))
                changed axis value = [if i == axis then value else x | (i, x) <- zip [0 :: Int ..] xs]
                differences = [let h = 1e-6 * max 1 (abs x) in (objective (changed i (x + h)) - objective (changed i (x - h))) / (2 * h) | (i, x) <- zip [0 :: Int ..] xs]
            assertApproximateList "signed tanh all logical finite differences" differences tanhGradient
            assertEqual "signed tanh VJP report" (report "vjp/tanh" 9 24 1) tr
            (s, st, sr) <- sumWithTape session left >>= right
            observe "signed sum primal" [6] s
            assertEqual "signed sum report" (report "sum-all" 3 8 1) sr
            scalarSeed <- signedView session budget (knownShape @'[3]) SNil [91, 0.75, 92] 1 []
            (sg, sgr) <- applyUnaryTape session st scalarSeed >>= right
            observe "offset scalar sum VJP" [0.75, 0.75, 0.75] sg
            assertEqual "sum VJP report" (report "vjp/sum-all" 3 24 1) sgr
            lhs <- signedView session budget (knownShape @'[12]) (knownShape @'[3, 2]) [0 .. 11] 10 [-4, -2]
            rhs <- signedView session budget (knownShape @'[16]) (knownShape @'[2, 4]) [0 .. 15] 1 [8, 2]
            matrixSeed <- signedView session budget (knownShape @'[24]) (knownShape @'[3, 4]) [0 .. 23] 22 [-8, -2]
            let l = [10, 8, 6, 4, 2, 0]
                r = [1, 3, 5, 7, 9, 11, 13, 15]
                z = [22, 20, 18, 16, 14, 12, 10, 8, 6, 4, 2, 0]
                primal = [sum [l !! (i * 2 + k) * r !! (k * 4 + j) | k <- [0 .. 1]] | i <- [0 .. 2], j <- [0 .. 3]]
                dl = [sum [z !! (i * 4 + j) * r !! (k * 4 + j) | j <- [0 .. 3]] | i <- [0 .. 2], k <- [0 .. 1]]
                dr = [sum [l !! (i * 2 + k) * z !! (i * 4 + j) | i <- [0 .. 2]] | k <- [0 .. 1], j <- [0 .. 3]]
            observe "signed rectangular left list" l lhs
            observe "offset rectangular right list" r rhs
            observe "signed rectangular seed list" z matrixSeed
            (productTensor, tape, productReport) <- matmulWithTape session lhs rhs >>= right
            observe "signed rectangular independent primal" primal productTensor
            assertEqual "rectangular primal report" (report "matmul" 60 96 1) productReport
            ((dlTensor, drTensor), gradientReport) <- applyBinaryTape session tape matrixSeed >>= right
            observe "rectangular all six left cotangents" dl dlTensor
            observe "rectangular all eight right cotangents" dr drTensor
            assertEqual "rectangular VJP report" (TensorOperationReport "vjp/matmul" 110 (TensorMemoryReport 112 64 0 2)) gradientReport
            pure (Right ())
        )
        >>= right

mixedHistoryTests :: IO ()
mixedHistoryTests = do
    forM_ [(316994, 38066, 10305, Nothing), (316993, 38066, 10305, Just AffineConstructedCells), (316994, 38065, 10305, Just AffineWork), (316994, 38066, 10304, Just AffineLiveCells), (316993, 38065, 10304, Just AffineConstructedCells)] $ \(cells, work, live, failureField) -> do
        policy <- policyWith cells work live
        planning <- roomy >>= right . affineBudget
        action <- right $ withAffineMap planning (knownShape @'[0]) (knownShape @'[0]) 0 [0] $ \witness _ _ ->
            withTensorSession (sessionLimits policy) $ \session -> do
                -- No private registry observation: base and seed are among these 1024.
                tensors <- replicateM 1024 (finiteTensorFromList session (knownShape @'[0]) [] >>= right)
                case tensors of
                    (base, _) : (seed, _) : _ -> do
                        owner <- right (tensorOwner "mixed original" (knownShape @'[0]))
                        (bound, bindReport) <- bindAffineView session witness (ownTensor owner base) >>= right
                        assertEqual "mixed bind" (AffineUsage 50569 6051 1585) (affineOperationCumulative bindReport)
                        result <- pullbackAffineView session bound seed
                        case failureField of
                            Just field -> do
                                let cap :: Natural
                                    cap = fromIntegral (case field of AffineConstructedCells -> cells; AffineWork -> work; _ -> live)
                                reject "mixed one-below" (TensorAffineError (AffineLimitExceeded field cap (cap + 1))) result
                            Nothing -> do
                                (gradient, report) <- right result
                                assertEqual "mixed full affine pullback" (AffineOperationReport (AffineCharge 266425 32015 10305 4465) (AffineUsage 316994 38066 10305) (TensorOperationReport "vjp/affine-base" 0 (TensorMemoryReport 0 0 0 1))) report
                                assertEqual "mixed empty gradient" [] =<< tensorToList (hostTensor (ownedFiniteTensor gradient))
                                when (sameStorage (hostTensor base) (hostTensor (ownedFiniteTensor gradient))) (fail "empty pullback reused storage ID")
                        pure (Right ())
                    _ -> fail "mixed history fixture length"
        action >>= right
