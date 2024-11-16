module Main where

import Markovian (evaluateMDPSample, evaluateMDPExpect, Process(..), Action(..))
import Control.Monad.Bayes.Sampler.Strict (sampleIO, SamplerT)
import qualified Data.Vector as V




-- Example States
data State = State String deriving (Show, Eq)

sampleProcess :: Process State
sampleProcess = Process {
  initialState = State "State1",
  isTerminal = isTerminalState,
  processReward = getReward,
  processActions = getActions
}
  where
    isTerminalState (State s) = s == "State3"
    
    getReward (State s) = case s of
      "State1" -> 0
      "State2" -> 5
      "State3" -> 10
      _ -> 0
      
    getActions (State s) = case s of
      "State1" -> V.fromList [
        Action "toState2" 0.5 (State "State2"),
        Action "toState3" 0.5 (State "State3")
        ]
      "State2" -> V.singleton $ Action "toState3" 1.0 (State "State3")
      _ -> V.empty

-- Main function
main :: IO ()
main = do
  -- Sampling from the MDP using MonadMeasure
  sampleValue <- sampleIO $ evaluateMDPSample sampleProcess
  putStrLn $ "Sampled Value: " ++ show sampleValue

  -- Computing the expected value using MonadMeasure
  expectedValue <- sampleIO $ evaluateMDPExpect sampleProcess
  putStrLn $ "Expected Value: " ++ show expectedValue
