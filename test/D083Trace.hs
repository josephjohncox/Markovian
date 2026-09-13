{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

-- Test-build arithmetic hooks. This module is never part of the library.
module D083Trace (Event (..), mass, binary, resetEvents, readEvents, capture) where

import Control.Exception (evaluate)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

data Event
    = Mass String String Rational
    | Binary String String Rational Rational Rational
    deriving (Eq, Show)

events :: IORef [Event]
events = unsafePerformIO (newIORef [])
{-# NOINLINE events #-}

emit :: Event -> Rational -> Rational
emit event value = value `seq` unsafePerformIO (atomicModifyIORef' events (\previous -> (event : previous, value)))
{-# NOINLINE emit #-}

mass :: String -> String -> Rational -> Rational
mass producer stage value = emit (Mass producer stage value) value
{-# NOINLINE mass #-}

binary :: String -> String -> (Rational -> Rational -> Rational) -> Rational -> Rational -> Rational
binary producer stage operation left right =
    let result = operation left right
     in emit (Binary producer stage left right result) result
{-# NOINLINE binary #-}

resetEvents :: IO ()
resetEvents = writeIORef events []

readEvents :: IO [Event]
readEvents = reverse <$> readIORef events

-- Force the complete small test value before reading the log, including every
-- report field and every list element. No production evaluation hook is added.
capture :: (Show value) => value -> IO (value, [Event])
capture value = do
    resetEvents
    _ <- evaluate (length (show value))
    observed <- readEvents
    pure (value, observed)
