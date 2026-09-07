{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- IO joins distinct checked error types without discarding failures.
module DenseContexts where

import Markovian.Backend.CPU.Exact

-- Preserve the named intermediate bindings shown in the book.
{-# ANN module ("HLint: ignore Redundant pure" :: String) #-}

lowerExample primitives circuit input = do
    dense <- either (fail . show) pure (lowerExactCircuit primitives circuit)
    result <- either (fail . show) pure (runDenseExactKernel dense input)
    pure result
