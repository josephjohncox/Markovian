{-# LANGUAGE DataKinds #-}

{- | Deterministic CPU reference primitives.

All reductions use the documented left-to-right loop order. There is no
implicit broadcasting, mutation, arbitrary stride constructor, or nonfinite
numerical result. Output tensors are fresh and contiguous.
-}
module Markovian.Tensor.Primitive (
    add,
    multiply,
    negateTensor,
    tanhTensor,
    sumAll,
    matmul,
    contiguousCopy,
) where

import Markovian.Tensor.Internal
