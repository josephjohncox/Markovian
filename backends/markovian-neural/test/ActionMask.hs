module ActionMask (tests) where

import Markovian.Backend.Neural (
    ActionMaskError (..),
    actionMaskContains,
    actionMaskFlags,
    actionMaskIndices,
    actionMaskWidth,
    gatherActionMask,
    mkActionMask,
    scatterActionMask,
 )
import TestSupport (assert, requireRight)

tests :: IO ()
tests = do
    constructorFailures
    structuralOperations
    maskingCounterexamples
    putStrLn "PASS: sized structural action masks"

constructorFailures :: IO ()
constructorFailures = do
    expectLeft "zero width" (InvalidActionMaskWidth 0) (mkActionMask 0 [0])
    expectLeft "negative width" (InvalidActionMaskWidth (-1)) (mkActionMask (-1) [0])
    expectLeft "empty support" EmptyActionMask (mkActionMask 3 [])
    expectLeft "negative index" (NegativeActionIndex 1 (-1)) (mkActionMask 3 [0, -1])
    expectLeft "large index" (ActionMaskIndexOutOfBounds 1 3 3) (mkActionMask 3 [0, 3])
    expectLeft "duplicate index" (DuplicateActionIndex 2) (mkActionMask 3 [2, 0, 2])

structuralOperations :: IO ()
structuralOperations = do
    mask <- requireRight "ordered mask" (mkActionMask 3 [2, 0])
    assert "mask width" (actionMaskWidth mask == 3)
    assert "mask order" (actionMaskIndices mask == [2, 0])
    assert "mask flags" (actionMaskFlags mask == [True, False, True])
    assert "mask membership" (actionMaskContains 2 mask && not (actionMaskContains 1 mask))
    gathered <- requireRight "mask gather" (gatherActionMask mask [10 :: Int, 20, 30])
    assert "gather order" (gathered == [30, 10])
    scattered <- requireRight "mask scatter" (scatterActionMask mask [7, 9])
    assert "scatter order" (scattered == [9, 0, 7])
    case scattered of
        [_first, unavailable, _last] ->
            assert "unavailable scatter is positive zero" ((1 / unavailable) == (1 / 0.0))
        _ -> assert "scatter width changed" False
    expectLeft
        "gather one below"
        (ActionMaskGatherWidthMismatch 3 2)
        (gatherActionMask mask [10 :: Int, 20])
    expectLeft
        "gather one over"
        (ActionMaskGatherWidthMismatch 3 4)
        (gatherActionMask mask [10 :: Int, 20, 30, 40])
    expectLeft
        "gather infinite input"
        (ActionMaskGatherWidthMismatch 3 4)
        (gatherActionMask mask (repeat (10 :: Int)))
    expectLeft
        "scatter one below"
        (ActionMaskScatterCountMismatch 2 1)
        (scatterActionMask mask [7])
    expectLeft
        "scatter one over"
        (ActionMaskScatterCountMismatch 2 3)
        (scatterActionMask mask [7, 9, 11])
    expectLeft
        "scatter infinite input"
        (ActionMaskScatterCountMismatch 2 3)
        (scatterActionMask mask (repeat 7))
    expectLeft
        "scatter positive infinity"
        (ActionMaskScatterNonFiniteValue 1)
        (scatterActionMask mask [7, 1 / 0])
    expectLeft
        "scatter NaN"
        (ActionMaskScatterNonFiniteValue 0)
        (scatterActionMask mask [0 / 0, 9])

maskingCounterexamples :: IO ()
maskingCounterexamples = do
    reverseOrder <- requireRight "reverse order" (mkActionMask 3 [2, 0])
    forwardOrder <- requireRight "forward order" (mkActionMask 3 [0, 2])
    assert "equal membership has distinct tie order" (actionMaskFlags reverseOrder == actionMaskFlags forwardOrder && reverseOrder /= forwardOrder)
    let values = [-2.0, 100.0, -1.0] :: [Double]
        numericFlags = fmap (\flag -> if flag then 1.0 else 0.0) (actionMaskFlags forwardOrder)
        multiplied = zipWith (*) numericFlags values
    available <- requireRight "negative-value gather" (gatherActionMask forwardOrder values)
    assert "gather excludes unavailable large value" (maximum available == -1)
    assert "multiplicative zero masking changes argmax" (maximum multiplied == 0)

expectLeft :: (Eq error, Show error, Show value) => String -> error -> Either error value -> IO ()
expectLeft _ expected (Left actual) = assert ("wrong error: " ++ show actual) (actual == expected)
expectLeft label _ (Right value) = assert (label ++ " unexpectedly succeeded: " ++ show value) False
