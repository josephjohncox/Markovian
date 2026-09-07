{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module TensorLawLaboratory (runTensorLawLaboratory) where

import Control.Monad (unless)
import Data.Proxy (Proxy (..))
import Markovian.Tensor
import Markovian.Tensor.Primitive
import Markovian.Tensor.Reverse

checked :: (Show e) => Either e a -> IO a
checked = either (fail . show) pure

answer :: (Eq a, Show a) => String -> a -> a -> IO ()
answer label expected actual = do
    unless (expected == actual) (fail (label ++ ": " ++ show actual))
    putStrLn (label ++ " = " ++ show actual)

runTensorLawLaboratory :: IO ()
runTensorLawLaboratory = do
    result <- withTensorSession (tensorSessionLimits 4 64 128 1024 65536 64 65536) $ \session -> do
        -- Each add is a separate checked primitive: no compiler reassociation.
        (a, _) <- checked =<< finiteTensorFromList session SNil [1e16]
        (b, _) <- checked =<< finiteTensorFromList session SNil [-1e16]
        (c, _) <- checked =<< finiteTensorFromList session SNil [1]
        (ab, _) <- checked =<< add session a b
        (left, _) <- checked =<< add session ab c
        (bc, _) <- checked =<< add session b c
        (right, _) <- checked =<< add session a bc
        leftValues <- tensorToList (hostTensor left)
        rightValues <- tensorToList (hostTensor right)
        answer "E F64 ((a+b)+c, a+(b+c))" ([1], [0]) (leftValues, rightValues)
        answer "E rounded b+c" [-1e16] =<< tensorToList (hostTensor bc)
        let exactA = 10000000000000000 :: Rational
            exactB = -10000000000000000 :: Rational
            exactC = 1 :: Rational
        answer "E Rational associations" (1, 1) ((exactA + exactB) + exactC, exactA + (exactB + exactC))

        let matrixShape = SCons (Proxy @2) (SCons (Proxy @3) SNil)
            viewShape = SCons (Proxy @3) (SCons (Proxy @2) SNil)
        (base, _) <- checked =<< finiteTensorFromList session matrixShape [1, 2, 3, 4, 5, 6]
        let view = transposeFinite2D base
        (copy, _) <- checked =<< contiguousCopy session view
        answer "F logical view coordinates" [1, 4, 2, 5, 3, 6] =<< tensorToList (hostTensor view)
        answer "F view shares base storage" True (sameStorage (hostTensor view) (hostTensor base))
        answer "F copy shares view storage" False (sameStorage (hostTensor view) (hostTensor copy))
        answer "F materialized coordinates" [1, 4, 2, 5, 3, 6] =<< tensorToList (hostTensor copy)
        (seed, _) <- checked =<< finiteTensorFromList session viewShape [1, 2, 3, 4, 5, 6]
        (_, tape, _) <- checked =<< multiplyWithTape session view view
        ((dx, dy), _) <- checked =<< applyBinaryTape session tape seed
        -- The primitive has two logical inputs. For x*x their cotangents add.
        (viewGradient, _) <- checked =<< add session dx dy
        viewValues <- tensorToList (hostTensor viewGradient)
        answer "F primitive-on-view gradient" [2, 16, 12, 40, 30, 72] viewValues
        -- Explicit transpose pullback, NOT an automatic view-to-base tape.
        baseValues <- tensorToList (hostTensor (transposeFinite2D viewGradient))
        answer "F explicit view-to-base gradient" [2, 12, 30, 16, 40, 72] baseValues
        let coordinates = [1, 2, 3, 4, 5, 6]
            objective xs = sum [w * (xs !! i) * (xs !! i) | (w, i) <- zip [1 .. 6] [0, 3, 1, 4, 2, 5]]
            perturb i delta = [x + if j == i then delta else 0 | (j, x) <- zip [0 :: Int ..] coordinates]
            epsilon = 1 / 1024
            differences = [(objective (perturb i epsilon) - objective (perturb i (-epsilon))) / (2 * epsilon) | i <- [0 .. 5]]
        answer "F independent base-coordinate differences" differences baseValues
        pure (Right ())
    checked result
