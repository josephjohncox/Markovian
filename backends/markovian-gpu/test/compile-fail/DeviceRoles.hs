{-# LANGUAGE DataKinds #-}

module DeviceRoles where

import Data.Coerce (coerce)
import Markovian.Backend.GPU

badVJPCoercion :: DeviceVJP '[2, 3] '[3, 2] -> DeviceVJP '[3, 2] '[2, 3]
badVJPCoercion = coerce

badExecutorScopeCoercion :: CUDAExecutor first -> CUDAExecutor second
badExecutorScopeCoercion = coerce
