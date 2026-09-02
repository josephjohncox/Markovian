{-# LANGUAGE DataKinds #-}

module SafeTensorOpacity where

import qualified Data.ByteString as BS
import Markovian.Tensor
import Markovian.Tensor.SafeTensors

badLimits :: SafeTensorLimits
badLimits = SafeTensorLimits 1 1 1 1 1 1 1 1

badName :: SafeTensorName
badName = SafeTensorName BS.empty

badFile :: SafeTensorFile region
badFile = SafeTensorFile []

badSomeTensor :: HostTensor region 'F64 '[] -> SomeHostTensor region
badSomeTensor = SomeHostTensor . dynamicHostTensor
