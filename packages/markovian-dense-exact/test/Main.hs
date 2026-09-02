{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Data.Ratio ((%))
import Markovian.Algebra.NonNegativeRational (nonNegativeRational)
import Markovian.Backend.CPU.Exact (denseExactRows, lowerExactCircuit)
import Markovian.Category.Finite.Set (finiteSet)
import Markovian.Category.Matrix (matrixFromRows)
import Markovian.Category.Matrix.Stochastic (stochasticMatrix)
import Markovian.Circuit (Purity (Stochastic), stochasticPrimitive)
import Markovian.Circuit.Interpret.Exact (ExactPrimitiveInterpreter (..))
import StochasticCircuit (runStochasticCircuitTests)

main :: IO ()
main = do
    source <- either (ioError . userError . show) pure (finiteSet [()])
    target <- either (ioError . userError . show) pure (finiteSet [False, True])
    dense <- either (ioError . userError . show) pure (lowerExactCircuit interpreter (stochasticPrimitive source target Coin))
    if fmap toList (toList (denseExactRows dense)) == [[1 % 2, 1 % 2]]
        then putStrLn "PASS: circuit-to-dense exact lowering"
        else ioError (userError "dense exact row changed")
    runStochasticCircuitTests run
  where
    toList = foldr (:) []
    run name action = action >> putStrLn ("PASS: " ++ name)

data Primitive purity source target where
    Coin :: Primitive 'Stochastic () Bool

interpreter :: ExactPrimitiveInterpreter Primitive String
interpreter =
    ExactPrimitiveInterpreter
        { interpretDeterministicPrimitive = \_ _ primitive -> case primitive of {}
        , interpretStochasticPrimitive = \source target Coin -> do
            half <- either (Left . show) Right (nonNegativeRational (1 % 2))
            matrix <- either (Left . show) Right (matrixFromRows source target [[half, half]])
            either (Left . show) Right (stochasticMatrix matrix)
        }
