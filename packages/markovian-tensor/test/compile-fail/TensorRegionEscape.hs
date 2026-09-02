{-# LANGUAGE DataKinds #-}

module TensorRegionEscape where

import Data.Coerce (coerce)
import Markovian.Tensor
import Markovian.Tensor.Ownership
import Markovian.Tensor.Primitive

badRegionEscape :: IO (Either TensorError (FiniteTensor region 'F64 '[]))
badRegionEscape = withTensorSession (tensorSessionLimits 1 1 1 8 8 1 1) $ \session ->
    fmap (fmap fst) (finiteTensorFromList session SNil [1])

badShapeCoercion :: HostTensor region 'F64 '[1] -> HostTensor region 'F64 '[2]
badShapeCoercion = coerce

badStorageRegionCoercion :: StorageId left -> StorageId right
badStorageRegionCoercion = coerce

data OwnerA

data OwnerB

badOwnerCoercion :: TensorOwner OwnerA '[1] -> TensorOwner OwnerB '[1]
badOwnerCoercion = coerce

badMatmulInnerDimensions session left right =
    matmul session (left :: FiniteTensor region 'F64 '[2, 3]) (right :: FiniteTensor region 'F64 '[4, 2])
