{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module LayoutRepresentationTests (layoutRepresentationTests) where

import Control.Monad (unless)
import Markovian.Tensor.Internal
import Markovian.Tensor.Shape (KnownShape (knownShape), SShape (SNil))

-- Private, established-valid layouts exercise the representation independently
-- of the future public affine admission path. No post-finalization reads occur.
layoutRepresentationTests :: IO ()
layoutRepresentationTests = do
    outcome <- withTensorSession limits $ \session -> do
        (base, _) <- require =<< finiteTensorFromList session (knownShape @'[5]) [11, 22, 33, 44, 55]
        case hostTensor base of
            HostTensor SF64 _ _ identifier _ capacity pointer -> do
                equal "constructor capacity" 5 capacity
                let selected = HostTensor SF64 (knownShape @'[2]) (CheckedLayout [2] 2 [1] True) identifier 2 capacity pointer
                    signed = HostTensor SF64 (knownShape @'[3]) (CheckedLayout [3] 4 [-2] False) identifier 3 capacity pointer
                equal "selection logical count" 2 (tensorElementCount selected)
                equal "signed description" "shape=[3];offset-elements=4;strides-elements=[-2];contiguous=False" (layoutDescription (tensorLayout signed))
                equal "signed offsets" [4, 2, 0] (logicalOffsets 3 (tensorLayout signed))
                equal "signed observation" [55, 33, 11] =<< tensorToList signed
                equal "selection observation" [33, 44] =<< tensorToList selected
                reshaped <- require (reshapeContiguous session (knownShape @'[1, 2]) selected)
                equal "reshape offset" 2 (layoutOffsetElements (tensorLayout reshaped))
                equal "reshape count" 2 (tensorElementCount reshaped)
                equal "reshape storage" True (sameStorage selected reshaped)
                equal "reshape values" [33, 44] =<< tensorToList reshaped
                case reshaped of
                    HostTensor _ _ _ _ _ retainedCapacity _ -> equal "reshape capacity" 5 retainedCapacity
                let transposed = transpose2D reshaped
                equal "transpose values" [33, 44] =<< tensorToList transposed
                equal "transpose offset" 2 (layoutOffsetElements (tensorLayout transposed))
                equal "transpose remains conservative" False (layoutIsContiguous (tensorLayout (transpose2D transposed)))
                equal "contiguity before count" (Left (TensorLayoutError NonContiguousReshape)) (fmap tensorElementCount (reshapeContiguous session (knownShape @'[1]) transposed))
                equal "target shape before contiguity" (Left (TensorShapeError (DimensionLimitExceeded 8 9))) (fmap tensorElementCount (reshapeContiguous session (knownShape @'[9]) transposed))
                equal "contiguous count mismatch" (Left (TensorShapeError (ShapeMismatch [2] [1]))) (fmap tensorElementCount (reshapeContiguous session (knownShape @'[1]) selected))
                finiteSigned <- require =<< finiteTensor signed
                (copy, _) <- require =<< contiguousCopy session finiteSigned
                equal "signed copy" [55, 33, 11] =<< tensorToList (hostTensor copy)
                equal "copy fresh" False (sameStorage signed (hostTensor copy))
                case hostTensor copy of
                    HostTensor _ _ _ _ count backing _ -> equal "copy count/capacity" (3, 3) (count, backing)
                (total, _) <- require =<< sumAll session finiteSigned
                equal "signed reduction" [99] =<< tensorToList (hostTensor total)
                let scalar = HostTensor SF64 SNil (CheckedLayout [] 3 [] True) identifier 1 capacity pointer
                    empty = HostTensor SF64 (knownShape @'[0]) (CheckedLayout [0] 5 [1] True) identifier 0 capacity pointer
                equal "scalar offset" [44] =<< tensorToList scalar
                equal "empty observation" [] =<< tensorToList empty
                emptyReshaped <- require (reshapeContiguous session (knownShape @'[0, 2]) empty)
                equal "empty reshape anchor" 5 (layoutOffsetElements (tensorLayout emptyReshaped))
                equal "empty reshape values" [] =<< tensorToList emptyReshaped
        pure (Right ())
    _ <- require outcome
    -- Legacy empty dimensions and strides deliberately remain unbounded.
    let huge = 18446744073709551616
        emptyShape = knownShape @'[0, 18446744073709551616, 18446744073709551616]
        oldLimits = tensorSessionLimits 3 huge 0 0 0 1 0
    emptyOutcome <- withTensorSession oldLimits $ \session -> do
        (empty, _) <- require =<< hostTensorFromList session SF64 emptyShape []
        equal "legacy huge empty values" [] =<< tensorToList empty
        equal "legacy huge empty layout" [toInteger huge * toInteger huge, toInteger huge, 1] (layoutStridesElements (tensorLayout empty))
        equal "legacy huge empty shape" [0, huge, huge] (layoutDimensions (tensorLayout empty))
        pure (Right ())
    _ <- require emptyOutcome
    putStrLn "layout representation: signed/offset/count-capacity/empty legacy tests passed (private layouts, not affine API admission)"
  where
    limits = tensorSessionLimits 3 8 64 512 8192 32 8192

require :: (Show problem) => Either problem value -> IO value
require (Right value) = pure value
require (Left problem) = fail (show problem)

equal :: (Eq value, Show value) => String -> value -> value -> IO ()
equal label expected actual = unless (actual == expected) (fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual))
