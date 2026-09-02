{-# LANGUAGE DataKinds #-}

module DeviceBoundary where

import Data.Coerce (coerce)
import Markovian.Backend.GPU

badPreparedConstructor :: PreparedMatMul region rows inner columns
badPreparedConstructor = PreparedMatMul undefined undefined 0 0 0 undefined

badVJPConstructor :: PreparedMatMulVJP region rows inner columns
badVJPConstructor = PreparedMatMulVJP undefined undefined undefined 0 0 0 undefined

badDeviceTensorConstructor :: DeviceTensor shape
badDeviceTensorConstructor = DeviceTensor []

badExecutorConstructor :: CUDAExecutor ()
badExecutorConstructor = CUDAExecutor

badScopedEscape :: IO (Either CUDAError (CUDAExecutor ()))
badScopedEscape = withCUDAExecutor DeterministicFirstDevice (pure . Right)

badShapeCoercion :: DeviceTensor '[2, 2] -> DeviceTensor '[4]
badShapeCoercion = coerce
