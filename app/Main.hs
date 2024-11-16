module Main where

import Markovian (evaluateMDPSample, evaluateMDPExpect, State(..))
import Control.Monad.Bayes.Sampler.Strict (sampleIO, SamplerT)
import qualified Data.Vector as V


-- Example States
state3 :: State
state3 = TerminalState "State3" 10

state2 :: State
state2 = State "State2" 5 (V.fromList [("toState3", 1.0, state3)])

state1 :: State
state1 = State "State1" 0 (V.fromList [("toState2", 0.5, state2), ("toState3", 0.5, state3)])

-- Main function
main :: IO ()
main = do
  -- Sampling from the MDP using MonadMeasure
  sampleValue <- sampleIO $ evaluateMDPSample state1
  putStrLn $ "Sampled Value: " ++ show sampleValue

  -- Computing the expected value using MonadMeasure
  expectedValue <- sampleIO $ evaluateMDPExpect state1
  putStrLn $ "Expected Value: " ++ show expectedValue
