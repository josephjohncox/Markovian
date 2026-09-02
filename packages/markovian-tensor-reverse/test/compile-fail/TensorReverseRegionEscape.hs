{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module TensorReverseRegionEscape where

import Data.Coerce (coerce)
import Markovian.Tensor
import Markovian.Tensor.Reverse (UnaryTape (..))
import Markovian.Tensor.Reverse.Program

badExecutorEscape :: IO (Either TensorReverseError (TensorReverseExecutor region))
badExecutorEscape = withTensorReverseExecutor (tensorSessionLimits 1 1 1 8 8 1 1) (pure . Right)

badTapeConstructor = TanhTape

badTapeCoercion :: UnaryTape region '[1] '[1] -> UnaryTape region '[2] '[2]
badTapeCoercion = coerce
