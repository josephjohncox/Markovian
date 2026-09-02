{-# LANGUAGE DataKinds #-}

module TensorBoundary where

import Markovian.Tensor (CheckedLayout (..), DynamicHostTensor (..), FiniteTensor (..), HostTensor (..), StorageId (..))
import Markovian.Tensor.Ownership (TensorOwner (..))

badHostConstructor = HostTensor
badDynamicConstructor = DynamicHostTensor
badFiniteConstructor = FiniteTensor
badLayoutConstructor = CheckedLayout
badStorageConstructor = StorageId
badOwnerConstructor = TensorOwner
