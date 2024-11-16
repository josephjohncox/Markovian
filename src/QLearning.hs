{-# LANGUAGE ScopedTypeVariables #-}

-- QLearning module
module QLearning (qLearning, evaluateMDPQLearning) where

import qualified Data.Map.Strict as Map
import Control.Monad.Random (MonadRandom, getRandomR)
import Control.Monad.State.Class (MonadState, get, modify)
import qualified Data.Vector as V
import Markovian (MendlerAlgebraM, MDPF(..), mdpCoalgebra, mdpAlgebraSample, mdpAlgebraExpect, mhyloM, Action(..), Process(..))

-- Type alias for Q-values
type QTable r = Map.Map (r, String) Double  -- (State name, Action name)

-- Parameters
alpha :: Double  -- Learning rate
alpha = 0.1

gamma :: Double  -- Discount factor
gamma = 0.9

epsilon :: Double  -- Exploration rate
epsilon = 0.1

-- Q-learning algorithm
qLearning :: (MonadRandom m, Ord r) => MDPF r -> Int -> QTable r -> m (QTable r)
qLearning _ 0 qTable = return qTable
qLearning initialState episodes qTable = do
  qTable' <- qLearningEpisode initialState qTable
  qLearning initialState (episodes - 1) qTable'

-- Run one episode
qLearningEpisode :: (MonadRandom m, Ord r) => MDPF r -> QTable r -> m (QTable r)
qLearningEpisode (TerminalMDP _ _) qTable = return qTable
qLearningEpisode (MDPF sName actions reward) qTable = do
  idx <- chooseAction sName actions qTable
  let action = actions V.! idx
      qKey = (sName, actionName action)
      qValue = Map.findWithDefault 0 qKey qTable
      maxNextQ = maxQValue (nextState action) qTable
      newQValue = qValue + alpha * (reward + gamma * maxNextQ - qValue)
      qTable' = Map.insert qKey newQValue qTable
  qLearningEpisode (nextState action) qTable'

-- Epsilon-greedy action selection
chooseAction :: (MonadRandom m, Ord r) => r -> V.Vector (Action (MDPF r)) -> QTable r -> m Int
chooseAction sName actions qTable = do
  r <- getRandomR (0.0, 1.0)
  if r < epsilon
    then getRandomR (0, V.length actions - 1)
    else return $ argmaxAction sName actions qTable

-- Find action with max Q-value
argmaxAction :: (Ord r) => r -> V.Vector (Action (MDPF r)) -> QTable r -> Int
argmaxAction sName actions qTable =
  V.maxIndex $ V.imap (\_ action ->
    Map.findWithDefault 0 (sName, actionName action) qTable) actions

-- Max Q-value for next state
maxQValue :: (Ord r) => MDPF r -> QTable r -> Double
maxQValue (TerminalMDP _ _) _ = 0
maxQValue (MDPF sName actions _) qTable =
  V.maximum $ V.map (\action ->
    Map.findWithDefault 0 (sName, actionName action) qTable) actions

-- Q-learning Algebra
mdpAlgebraQLearning ::
  forall r m.
  (MonadState (QTable r) m, MonadRandom m, Ord r) =>
  MendlerAlgebraM m MDPF Double r
mdpAlgebraQLearning _ (TerminalMDP _ reward) = return reward
mdpAlgebraQLearning rec (MDPF sName actions reward) = do
  qTable <- get

  -- Epsilon-greedy action selection
  eps <- getRandomR (0.0, 1.0)
  idx <- if eps < epsilon
    then getRandomR (0, V.length actions - 1)  -- Exploration
    else do  -- Exploitation
      let qValues = V.map (\action ->
            Map.findWithDefault 0 (sName, actionName action) qTable) actions
      return $ V.maxIndex qValues

  let action = actions V.! idx

  -- Recursive call to get the total value of the next state
  totalValue <- mdpAlgebraQLearning rec (nextState action)

  -- Q-learning update
  maxNextQ <- maximumQValue (nextState action)
  let qKey = (sName, actionName action)
      qValue = Map.findWithDefault 0 qKey qTable
      newQValue = qValue + alpha * (reward + gamma * maxNextQ - qValue)

  -- Update Q-table
  modify (Map.insert qKey newQValue)

  return $ reward + totalValue

-- Helper function to get max Q-value for a given state
maximumQValue :: (MonadState (QTable r) m, MonadRandom m, Ord r) => MDPF r -> m Double
maximumQValue nextState = do
  case nextState of
    TerminalMDP _ _ -> return 0
    MDPF sName actions _ -> do
      qTable <- get
      let qValues = V.map (\action ->
            Map.findWithDefault 0 (sName, actionName action) qTable) actions
      return $ if V.null qValues then 0 else V.maximum qValues

-- Evaluating the MDP with Q-learning
evaluateMDPQLearning ::
  (MonadState (QTable r) m, MonadRandom m, Ord r) =>
  Process r -> m Double
evaluateMDPQLearning process = mhyloM mdpAlgebraQLearning (mdpCoalgebra process) (initialState process)
