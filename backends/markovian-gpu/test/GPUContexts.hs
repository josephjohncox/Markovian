{-# OPTIONS_GHC -Wno-missing-signatures #-}

module GPUContexts where

import Markovian.Backend.GPU

-- Preserve the named intermediate bindings shown in the book.
{-# ANN module ("HLint: ignore Redundant pure" :: String) #-}

prepareExample limits left right = do
    prepared <- prepareMatMul limits left right
    pure prepared

runExample session prepared = do
    result <-
        runPreparedMatMul
            session
            (PreferCUDA DeterministicFirstDevice FallbackBeforeUserLaunch)
            prepared
    pure result
