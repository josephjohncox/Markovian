{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

{- | Opaque CPU tapes for the runtime's closed primitive set.

The pullbacks implement transposed Jacobian actions under the standard finite
coordinate pairing. They are not matrix daggers, Bayesian inverses, payoff
pullbacks, feedback, strategic duals, or disintegrations. This module does not
differentiate arbitrary Haskell and does not expose captured callbacks.

This public closed-tape API is owned by @markovian-tensor@. Its private
same-package capability delegates to the tensor runtime's atomic staged
allocator; multi-output pullbacks are not assembled by sequential public
allocations. The separate @markovian-tensor-reverse@ package owns only the
adapter from these tapes to bounded reverse programs.
-}
module Markovian.Tensor.Reverse (
    UnaryTape,
    BinaryTape,
    tanhWithTape,
    sumWithTape,
    addWithTape,
    multiplyWithTape,
    matmulWithTape,
    applyUnaryTape,
    applyBinaryTape,
) where

import Markovian.Tensor (
    DType (F64),
    FiniteTensor,
    TensorError,
    TensorOperationReport,
    TensorSession,
 )
import Markovian.Tensor.Internal.Reverse qualified as Internal

-- | Opaque tape for one closed unary host primitive.
type UnaryTape region input output = Internal.UnaryTape region input output

-- | Opaque tape for one closed binary host primitive.
type BinaryTape region left right output = Internal.BinaryTape region left right output

-- | Evaluate host @tanh@ and atomically retain its closed pullback tape.
tanhWithTape :: TensorSession region -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 shape, UnaryTape region shape shape, TensorOperationReport))
tanhWithTape = Internal.tanhWithTape

-- | Evaluate host reduction and atomically retain its closed pullback tape.
sumWithTape :: TensorSession region -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 '[], UnaryTape region shape '[], TensorOperationReport))
sumWithTape = Internal.sumWithTape

-- | Evaluate host addition and atomically retain its closed pullback tape.
addWithTape :: TensorSession region -> FiniteTensor region 'F64 shape -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 shape, BinaryTape region shape shape shape, TensorOperationReport))
addWithTape = Internal.addWithTape

-- | Evaluate host pointwise multiplication and atomically retain its closed pullback tape.
multiplyWithTape :: TensorSession region -> FiniteTensor region 'F64 shape -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 shape, BinaryTape region shape shape shape, TensorOperationReport))
multiplyWithTape = Internal.multiplyWithTape

-- | Evaluate host matrix multiplication and atomically retain its closed pullback tape.
matmulWithTape :: TensorSession region -> FiniteTensor region 'F64 '[rows, inner] -> FiniteTensor region 'F64 '[inner, columns] -> IO (Either TensorError (FiniteTensor region 'F64 '[rows, columns], BinaryTape region '[rows, inner] '[inner, columns] '[rows, columns], TensorOperationReport))
matmulWithTape = Internal.matmulWithTape

-- | Apply one closed unary tape under the session's checked limits.
applyUnaryTape :: TensorSession region -> UnaryTape region input output -> FiniteTensor region 'F64 output -> IO (Either TensorError (FiniteTensor region 'F64 input, TensorOperationReport))
applyUnaryTape = Internal.applyUnaryTape

-- | Apply one closed binary tape and allocate both cotangents atomically.
applyBinaryTape :: TensorSession region -> BinaryTape region left right output -> FiniteTensor region 'F64 output -> IO (Either TensorError ((FiniteTensor region 'F64 left, FiniteTensor region 'F64 right), TensorOperationReport))
applyBinaryTape = Internal.applyBinaryTape
