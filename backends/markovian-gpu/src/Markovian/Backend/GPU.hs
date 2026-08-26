{-# LANGUAGE CPP #-}

{- | Optional CUDA dense-kernel backend.

The CUDA implementation is enabled with the package flag @cuda@. Reported
runtime includes context creation, host-to-device transfer, kernel execution,
device-to-host transfer, and cleanup.
-}
module Markovian.Backend.GPU (
    gpuBackendCompiled,
    gpuBackendAvailable,
    GPUDenseError (..),
    GPUDenseResult (..),
    gpuDenseApply,
) where

#ifdef MARKOVIAN_CUDA
import Foreign.C.Types (CDouble (..), CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray, withArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
#endif

-- | Whether this package build contains the CUDA driver implementation.
gpuBackendCompiled :: Bool
#ifdef MARKOVIAN_CUDA
gpuBackendCompiled = True
#else
gpuBackendCompiled = False
#endif

-- | GPU validation and driver failures.
data GPUDenseError
    = GPUBackendNotCompiled
    | GPUBackendUnavailable
    | GPUInvalidShape !Int !Int !Int !Int
    | GPUNonFiniteInput !Int !Double
    | GPUDriverError !Int
    deriving (Eq, Show)

-- | One GPU output and transfer-inclusive wall-clock duration in milliseconds.
data GPUDenseResult = GPUDenseResult
    { gpuDenseOutput :: ![Double]
    , gpuTransferInclusiveMilliseconds :: !Double
    }
    deriving (Eq, Show)

-- | Query whether an enabled CUDA build can see at least one device.
gpuBackendAvailable :: IO Bool
#ifdef MARKOVIAN_CUDA
gpuBackendAvailable = (/= 0) <$> c_gpu_available
#else
gpuBackendAvailable = pure False
#endif

{- | Apply a row-major stochastic matrix to one source distribution.

For a matrix with @rows@ source states and @columns@ target states, the input
has length @rows@ and the output has length @columns@.
-}
gpuDenseApply ::
    Int ->
    Int ->
    [Double] ->
    [Double] ->
    IO (Either GPUDenseError GPUDenseResult)
gpuDenseApply rows columns matrix input =
    case validate of
        Left err -> pure (Left err)
        Right () -> run
  where
    validate
        | rows <= 0 || columns <= 0 = invalidShape
        | toInteger rows > maximumCInt || toInteger columns > maximumCInt = invalidShape
        | toInteger (length matrix) /= toInteger rows * toInteger columns = invalidShape
        | length input /= rows = invalidShape
        | otherwise = validateFinite 0 (matrix ++ input)

    invalidShape = Left (GPUInvalidShape rows columns (length matrix) (length input))
    maximumCInt = 2147483647

    validateFinite _ [] = Right ()
    validateFinite index (value : remaining)
        | isNaN value || isInfinite value = Left (GPUNonFiniteInput index value)
        | otherwise = validateFinite (index + 1) remaining

#ifdef MARKOVIAN_CUDA
    run = do
        available <- gpuBackendAvailable
        if not available
            then pure (Left GPUBackendUnavailable)
            else
                withArray (fmap CDouble matrix) $ \matrixPointer ->
                    withArray (fmap CDouble input) $ \inputPointer ->
                        allocaArray columns $ \outputPointer ->
                            alloca $ \millisecondsPointer -> do
                                result <-
                                    c_gpu_dense_apply
                                        (fromIntegral rows)
                                        (fromIntegral columns)
                                        matrixPointer
                                        inputPointer
                                        outputPointer
                                        millisecondsPointer
                                if result /= 0
                                    then pure (Left (GPUDriverError (fromIntegral result)))
                                    else do
                                        output <- fmap (fmap realToFrac) (peekArray columns outputPointer)
                                        milliseconds <- realToFrac <$> peek millisecondsPointer
                                        pure
                                            ( Right
                                                GPUDenseResult
                                                    { gpuDenseOutput = output
                                                    , gpuTransferInclusiveMilliseconds = milliseconds
                                                    }
                                            )
#else
    run = pure (Left GPUBackendNotCompiled)
#endif

#ifdef MARKOVIAN_CUDA
foreign import ccall unsafe "markovian_gpu_available"
    c_gpu_available :: IO CInt

foreign import ccall unsafe "markovian_gpu_dense_apply"
    c_gpu_dense_apply ::
        CInt ->
        CInt ->
        Ptr CDouble ->
        Ptr CDouble ->
        Ptr CDouble ->
        Ptr CDouble ->
        IO CInt
#endif
