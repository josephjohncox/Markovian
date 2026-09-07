module Main (main) where

import Control.Monad (forM_, unless)
import Markovian.Feedback.Internal
import Numeric.Natural (Natural)

main :: IO ()
main = do
    let machine = fromIntegral (maxBound :: Int) :: Natural
        limits = feedbackLimits machine machine machine 0 machine machine machine machine machine
        cases = [0, 1, 2, 9, 100, machine `div` 16, machine `div` 2, machine - 2, machine]
    forM_ [(x, u, y, e) | x <- cases, u <- cases, y <- cases, e <- cases] $ \(x, u, y, e) -> do
        let s = toInteger x + toInteger u
            t = toInteger u + toInteger y
            cells = 2 * s * toInteger e + s * t + 2 * s * (1 + toInteger y) + toInteger u * (toInteger u + toInteger y + 2)
            baseGraph = toInteger e * t + s + toInteger e + 4 * s * toInteger e + s * toInteger e * t
            graph = baseGraph + s + toInteger e + 4 * s * toInteger e
            bound = toInteger machine
            -- Width sums also precede allocation, including empty products.
            cellOverflow = any (> bound) [s, t, 1 + toInteger y, toInteger u + toInteger y + 2, cells]
            expected
                | cellOverflow = Left (FeedbackNaturalOverflow FeedbackMatrixCellCount)
                | max (4 * s) graph > bound = Left (FeedbackNaturalOverflow FeedbackGraphWork)
                | otherwise = Right (fromInteger (s * toInteger e), fromInteger cells, fromInteger baseGraph, fromInteger graph)
            actual = affineRewardJVPReservations limits x u y e
        unless (actual == expected) (fail ("JVP saturated reservation differs from Integer oracle: " ++ show (x, u, y, e, actual, expected)))
    putStrLn "PASS: reward JVP machine-saturating reservations against 6561 Integer fixtures"
