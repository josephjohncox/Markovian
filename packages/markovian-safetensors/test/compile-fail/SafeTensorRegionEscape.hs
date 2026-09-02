{-# LANGUAGE DataKinds #-}

module SafeTensorRegionEscape where

import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Markovian.Tensor
import Markovian.Tensor.Ownership
import Markovian.Tensor.SafeTensors

limits :: SafeTensorLimits
limits = either (error . renderSafeTensorError) id (safeTensorLimits 64 56 1 8 1 8 8 8)

badFileEscape :: IO (Either TensorError (SafeTensorFile region))
badFileEscape = withTensorSession (tensorSessionLimits 1 8 8 64 64 1 8) $ \session -> do
    result <- decodeSafeTensors session limits (BS.replicate 8 0)
    pure $ case result of
        Left problem -> Left (HostAllocationFailure (renderSafeTensorError problem))
        Right file -> Right file

badTensorRegionCoercion :: SomeHostTensor left -> SomeHostTensor right
badTensorRegionCoercion = coerce

badFileRegionCoercion :: SafeTensorFile left -> SafeTensorFile right
badFileRegionCoercion = coerce

data Owner

badNameOwner :: SafeTensorName -> TensorOwner Owner '[1]
badNameOwner = coerce
