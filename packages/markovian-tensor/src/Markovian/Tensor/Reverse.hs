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
newtype UnaryTape region input output = UnaryTape (Internal.UnaryTape region input output)

type role UnaryTape nominal nominal nominal

-- | Opaque tape for one closed binary host primitive.
newtype BinaryTape region left right output = BinaryTape (Internal.BinaryTape region left right output)

type role BinaryTape nominal nominal nominal nominal

-- | Evaluate host @tanh@ and atomically retain its closed pullback tape.
tanhWithTape :: TensorSession region -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 shape, UnaryTape region shape shape, TensorOperationReport))
tanhWithTape session input =
    fmap (\(output, tape, report) -> (output, UnaryTape tape, report))
        <$> Internal.tanhWithTape session input

-- | Evaluate host reduction and atomically retain its closed pullback tape.
sumWithTape :: TensorSession region -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 '[], UnaryTape region shape '[], TensorOperationReport))
sumWithTape session input =
    fmap (\(output, tape, report) -> (output, UnaryTape tape, report))
        <$> Internal.sumWithTape session input

-- | Evaluate host addition and atomically retain its closed pullback tape.
addWithTape :: TensorSession region -> FiniteTensor region 'F64 shape -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 shape, BinaryTape region shape shape shape, TensorOperationReport))
addWithTape session left right =
    fmap (\(output, tape, report) -> (output, BinaryTape tape, report))
        <$> Internal.addWithTape session left right

-- | Evaluate host pointwise multiplication and atomically retain its closed pullback tape.
multiplyWithTape :: TensorSession region -> FiniteTensor region 'F64 shape -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 shape, BinaryTape region shape shape shape, TensorOperationReport))
multiplyWithTape session left right =
    fmap (\(output, tape, report) -> (output, BinaryTape tape, report))
        <$> Internal.multiplyWithTape session left right

-- | Evaluate host matrix multiplication and atomically retain its closed pullback tape.
matmulWithTape :: TensorSession region -> FiniteTensor region 'F64 '[rows, inner] -> FiniteTensor region 'F64 '[inner, columns] -> IO (Either TensorError (FiniteTensor region 'F64 '[rows, columns], BinaryTape region '[rows, inner] '[inner, columns] '[rows, columns], TensorOperationReport))
matmulWithTape session left right =
    fmap (\(output, tape, report) -> (output, BinaryTape tape, report))
        <$> Internal.matmulWithTape session left right

-- | Apply one closed unary tape under the session's checked limits.
applyUnaryTape :: TensorSession region -> UnaryTape region input output -> FiniteTensor region 'F64 output -> IO (Either TensorError (FiniteTensor region 'F64 input, TensorOperationReport))
applyUnaryTape session (UnaryTape tape) = Internal.applyUnaryTape session tape

-- | Apply one closed binary tape and allocate both cotangents atomically.
applyBinaryTape :: TensorSession region -> BinaryTape region left right output -> FiniteTensor region 'F64 output -> IO (Either TensorError ((FiniteTensor region 'F64 left, FiniteTensor region 'F64 right), TensorOperationReport))
applyBinaryTape session (BinaryTape tape) = Internal.applyBinaryTape session tape
