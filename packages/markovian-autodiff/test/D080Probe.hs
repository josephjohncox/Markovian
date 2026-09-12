-- Source-only support for the CPP-instrumented home-module boundary build.
-- Never a library module: normal builds do not import this sink.
module D080Probe (probeEvent, resetEvents, readEvents) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE events #-}
events :: IORef [String]
events = unsafePerformIO (newIORef [])

{-# NOINLINE probeEvent #-}
probeEvent :: String -> a -> a
probeEvent name value = unsafePerformIO $ do
    atomicModifyIORef' events (\previous -> (name : previous, ()))
    pure value

resetEvents :: IO ()
resetEvents = writeIORef events []

readEvents :: IO [String]
readEvents = reverse <$> readIORef events
