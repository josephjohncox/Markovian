{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{- | Checked host-only F64 tensor storage.

The runtime is single-device (managed CPU memory), deterministic, and
single-threaded at each primitive. Rank-zero tensors contain one scalar;
they are not the zero-coordinate unit used by reverse-program products.
Constructors validate shape, machine-index, payload, work, and finite-value
limits. Numerical operations return no tensor or report on failure.

Session lifetime is a supported-use discipline, not an enforced effect or linear
lifetime system. Complete region-dependent work within the live callback and
join or cancel-and-join dependent children on every exit. Never use escaped
actions or existentially retained tensors for later observation. Nominal indices
do not prevent these packages from typechecking. Unsupported post-close use is
not uniformly rejected with TensorSessionClosed, and ordinary observers do not
provide close/read synchronization. Default GC-pinned storage retention is not
a supported post-finalization feature.
-}
module Markovian.Tensor (
    DType (F64),
    SDType (SF64),
    Scalar,
    SessionLimits,
    tensorSessionLimits,
    TensorSession,
    TensorSessionException (..),
    TensorSessionFailureException (..),
    withTensorSession,
    TensorMemoryReport (..),
    TensorOperationReport (..),
    renderTensorOperationReport,
    ShapeError (..),
    LayoutError (..),
    BudgetError (..),
    NumericError (..),
    TensorError (..),
    AffineProblem (..),
    AffineLimit (..),
    AffineInput (..),
    CheckedLayout,
    layoutDescription,
    type StorageId,
    HostTensor,
    DynamicHostTensor,
    dynamicHostTensor,
    withDynamicHostTensor,
    dynamicHostTensorDimensions,
    dynamicHostTensorToList,
    hostTensorBatchFromLists,
    FiniteTensor,
    hostTensorFromList,
    finiteTensorFromList,
    finiteTensor,
    hostTensor,
    tensorToList,
    tensorShape,
    tensorDType,
    tensorLayout,
    tensorStorageId,
    sameStorage,
    transpose2D,
    transposeFinite2D,
    reshapeContiguous,
    reshapeFiniteContiguous,
    module Markovian.Tensor.Shape,
) where

import Markovian.Tensor.Internal
import Markovian.Tensor.Shape
