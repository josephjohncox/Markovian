{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

{- |
Internal same-package capability for the closed primitive tape set.

The public "Markovian.Tensor.Reverse" module wraps these implementation types
so Haddock signatures do not leak private names. The implementation remains
the tensor runtime's exception-safe, atomic staged allocator. Applications
must use "Markovian.Tensor.Reverse" instead.
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
