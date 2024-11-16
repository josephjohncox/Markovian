module Main where
import Control.Monad (replicateM_)
import Control.Monad.Trans.State.Strict (evalStateT, execStateT)
import Control.Monad.Random (evalRandIO)
import qualified Data.Map.Strict as Map
import QLearning (evaluateMDPQLearning, qLearning)
import Markovian ( Action(Action), Process(..), mdpCoalgebra, buildMDPF)
import qualified Data.Vector as V
import System.Environment (getArgs)


-- Example States
data State = State String deriving (Show, Eq, Ord)

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


-- Main function with subcommand selection
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["basic"] -> mainBasic
    ["stateful"] -> mainStateful 
    _ -> do
      putStrLn "Please specify a subcommand:"
      putStrLn "  basic     - Run basic Q-learning"
      putStrLn "  stateful  - Run stateful Q-learning"

-- Original basic Q-learning main renamed
mainBasic :: IO ()
mainBasic = do
  let episodes = 1000
      initialQTable = Map.empty
  finalQTable <- evalRandIO $ qLearning (buildMDPF sampleProcess (initialState sampleProcess)) episodes initialQTable
  putStrLn "Basic Q-learning completed."
  print finalQTable

-- Original stateful Q-learning main renamed  
mainStateful :: IO ()
mainStateful = do
  let episodes = 1000
      initialQTable = Map.empty
  finalQTable <- evalRandIO $ flip execStateT initialQTable $ do
    replicateM_ episodes $ do
      _ <- evaluateMDPQLearning sampleProcess
      return ()
  putStrLn "Stateful Q-learning completed."
  print finalQTable
