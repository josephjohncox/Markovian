{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

{- |
Internal cross-package capability for the closed primitive tape set.

This module exists only so @markovian-tensor-reverse@ can own the public
"Markovian.Tensor.Reverse" API without reconstructing multi-output VJPs from
sequential public allocations.  The implementation remains the tensor
runtime's exception-safe, atomic staged allocator.  Applications must use
"Markovian.Tensor.Reverse" instead.
-}
module Markovian.Tensor.Internal.Reverse (
    -- | Internal opaque unary tape capability.
    UnaryTape,
    -- | Internal opaque binary tape capability.
    BinaryTape,
    -- | Internal atomic @tanh@ tape operation.
    tanhWithTape,
    -- | Internal atomic reduction tape operation.
    sumWithTape,
    -- | Internal atomic addition tape operation.
    addWithTape,
    -- | Internal atomic pointwise-product tape operation.
    multiplyWithTape,
    -- | Internal atomic matrix-product tape operation.
    matmulWithTape,
    -- | Internal checked unary pullback operation.
    applyUnaryTape,
    -- | Internal checked atomic binary pullback operation.
    applyBinaryTape,
) where

import Markovian.Tensor.Internal
