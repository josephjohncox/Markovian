module TestSupport (
    assert,
    assertClose,
    assertCloseWith,
    assertVectorClose,
    assertVectorCloseWith,
    centralDifference,
    requireRight,
) where

import System.Exit (exitFailure)

assert :: String -> Bool -> IO ()
assert _ True = pure ()
assert message False = failTest message

assertClose :: String -> Double -> Double -> Double -> IO ()
assertClose label tolerance = assertCloseWith label tolerance tolerance

assertCloseWith :: String -> Double -> Double -> Double -> Double -> IO ()
assertCloseWith label absolute relative expected actual =
    assert
        (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
        (abs (expected - actual) <= absolute + relative * max (abs expected) (abs actual))

assertVectorClose :: String -> Double -> [Double] -> [Double] -> IO ()
assertVectorClose label tolerance = assertVectorCloseWith label tolerance tolerance

assertVectorCloseWith :: String -> Double -> Double -> [Double] -> [Double] -> IO ()
assertVectorCloseWith label absolute relative expected actual = do
    assert (label ++ ": vector lengths differ") (length expected == length actual)
    sequence_
        [ assertCloseWith (label ++ " at index " ++ show index) absolute relative left right
        | (index, (left, right)) <- zip [0 :: Int ..] (zip expected actual)
        ]

centralDifference :: Double -> (Double -> IO Double) -> Double -> IO Double
centralDifference epsilon function point = do
    let step = epsilon * max 1 (abs point)
    above <- function (point + step)
    below <- function (point - step)
    pure ((above - below) / (2 * step))

requireRight :: (Show error) => String -> Either error value -> IO value
requireRight _ (Right value) = pure value
requireRight label (Left err) = failTest (label ++ ": " ++ show err)

failTest :: String -> IO a
failTest message = do
    putStrLn ("FAIL: " ++ message)
    exitFailure
