{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Markovian (evaluateMDPSample, evaluateMDPExpect, State(..)) where

import Control.Monad.Bayes.Class (MonadDistribution(..), MonadMeasure)
import qualified Data.Vector as V

-- State data type
data State = TerminalState {
  stateName   :: String,
  stateReward :: Double
} | State {
  stateName   :: String,
  stateReward :: Double,
  nextActions :: V.Vector (String, Double, State)
}

-- MDPF Functor
data MDPF a where
  TerminalMDP :: {
    reward :: Double
  } -> MDPF a

  MDPF :: {
    actions :: V.Vector (String, Double, a),  -- (Action name, probability, next state)
    reward  :: Double
  } -> MDPF a

-- Functor instance
instance Functor MDPF where
  fmap :: (a -> b) -> MDPF a -> MDPF b
  fmap _ (TerminalMDP reward) = TerminalMDP reward
  fmap f (MDPF actions reward) = MDPF (V.map (\(name, prob, next) -> (name, prob, f next)) actions) reward

-- Mendler-style Algebra Types
type MendlerAlgebraM m f a = forall r. (r -> m a) -> f r -> m a

-- Mendler-style Coalgebra Type
type MendlerCoalgebra f a = a -> f a

-- Mendler-style Hylomorphism
mhyloM :: Monad m => MendlerAlgebraM m f b -> MendlerCoalgebra f a -> a -> m b
mhyloM alg coalg a = alg (mhyloM alg coalg) (coalg a)

-- Coalgebra
mdpCoalgebra :: MendlerCoalgebra MDPF State
mdpCoalgebra (TerminalState _ reward) = TerminalMDP reward
mdpCoalgebra (State _ reward actions) = MDPF (V.map (\(actionName, prob, nextState) -> (actionName, prob, nextState)) actions) reward

-- Algebra for Sampling
mdpAlgebraSample :: MonadDistribution m => (r -> m Double) -> MDPF r -> m Double
mdpAlgebraSample _ (TerminalMDP reward) = return reward
mdpAlgebraSample rec (MDPF actions reward) = do
  let ps = V.map (\(_, prob, _) -> prob) actions
      vs = V.map (\(_, _, nextState) -> rec nextState) actions
  idx <- categorical ps
  totalValue <- vs V.! idx
  return $ reward + totalValue

-- Algebra for Expectation
mdpAlgebraExpect :: MonadDistribution m => (r -> m Double) -> MDPF r -> m Double
mdpAlgebraExpect _ (TerminalMDP reward) = return reward
mdpAlgebraExpect rec (MDPF actions reward) = do
  let ps = V.map (\(_, prob, _) -> prob) actions
      totalProb = sum ps
      normalizedPs = V.map (/ totalProb) ps
      vs = V.map (\(_, _, nextState) -> rec nextState) actions
  expectedValues <- sequence vs
  let expectedValue = V.sum $ V.zipWith (*) normalizedPs expectedValues
  return $ reward + expectedValue

-- Evaluating the MDP
evaluateMDPSample :: MonadDistribution m => State -> m Double
evaluateMDPSample = mhyloM mdpAlgebraSample mdpCoalgebra

evaluateMDPExpect :: MonadDistribution m => State -> m Double
evaluateMDPExpect = mhyloM mdpAlgebraExpect mdpCoalgebra
