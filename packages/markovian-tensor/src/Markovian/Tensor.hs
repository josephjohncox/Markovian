{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{- | Checked host-only F64 tensor storage.

The runtime is single-device (managed CPU memory), deterministic, and
single-threaded at each primitive. Rank-zero tensors contain one scalar;
they are not the zero-coordinate unit used by reverse-program products.
Constructors validate shape, machine-index, payload, work, and finite-value
limits. Numerical operations return no tensor or report on failure.
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
