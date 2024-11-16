{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Markovian (evaluateMDPSample, evaluateMDPExpect, MendlerAlgebraM, MDPF(..), mdpCoalgebra, mdpAlgebraSample, mdpAlgebraExpect, mhyloM, Process(..), Action(..), buildMDPF) where

import Control.Monad.Bayes.Class (MonadDistribution(..))
import qualified Data.Vector as V

-- newtype Fix f = Fix { unFix :: f (Fix f) }

-- State data type
-- type State = Fix MDPF

data Process s = Process {
  initialState :: s,
  isTerminal   :: s -> Bool,
  processReward       :: s -> Double,
  processActions      :: s -> V.Vector (Action s)
}

data Action s = Action {
  actionName  :: String,
  actionProbability :: Double,
  nextState   :: s
}

instance Functor Action where
  fmap :: (a -> b) -> Action a -> Action b
  fmap f (Action name prob next) = Action name prob (f next)

-- Convert a Process to an MDPF
buildMDPF :: Process s -> s -> MDPF s
buildMDPF process s = 
  if isTerminal process s
    then TerminalMDP s (processReward process s)
    else MDPF s mappedActions (processReward process s)
      where
        actions = processActions process s
        mappedActions = V.map (fmap (buildMDPF process)) actions

-- MDPF Functor
data MDPF a where
  TerminalMDP :: {
    state     :: a,
    reward    :: Double
  } -> MDPF a

  MDPF :: {
    state     :: a,
    actions   :: V.Vector (Action (MDPF a)),  -- (Action name, probability, next MDPF)
    reward    :: Double
  } -> MDPF a

-- Functor instance
instance Functor MDPF where
  fmap :: (a -> b) -> MDPF a -> MDPF b
  fmap f (TerminalMDP sName reward) = TerminalMDP (f sName) reward
  fmap f (MDPF sName actions reward) = MDPF (f sName) (V.map (fmap (fmap f)) actions) reward

-- Mendler-style Algebra Types
type MendlerAlgebraM m f a r = (r -> m a) -> f r -> m a

-- Mendler-style Coalgebra Type
type MendlerCoalgebra f a = a -> f a

-- Mendler-style Hylomorphism
mhyloM :: Monad m => MendlerAlgebraM m f b a -> MendlerCoalgebra f a -> a -> m b
mhyloM alg coalg a = alg (mhyloM alg coalg) (coalg a)

-- Coalgebra 
mdpCoalgebra :: Process s -> MendlerCoalgebra MDPF s
mdpCoalgebra process state = buildMDPF process state

-- Algebra for Sampling
mdpAlgebraSample :: MonadDistribution m => MendlerAlgebraM m MDPF Double r
mdpAlgebraSample _ (TerminalMDP _ reward) = return reward
mdpAlgebraSample rec (MDPF _ actions reward) = do
  let ps = V.map actionProbability actions
      states = V.map nextState actions
      vs = V.map (mdpAlgebraSample rec) states
  idx <- categorical ps
  totalValue <- vs V.! idx
  return $ reward + totalValue

-- Algebra for Expectation
mdpAlgebraExpect :: MonadDistribution m => MendlerAlgebraM m MDPF Double r 
mdpAlgebraExpect _ (TerminalMDP _ reward) = return reward
mdpAlgebraExpect rec (MDPF _ action reward) = do
  let ps = V.map actionProbability action
      totalProb = sum ps
      normalizedPs = V.map (/ totalProb) ps
      vs = V.map (mdpAlgebraExpect rec) (V.map nextState action)
  expectedValues <- sequence vs
  let expectedValue = V.sum $ V.zipWith (*) normalizedPs expectedValues
  return $ reward + expectedValue

-- Evaluating the MDP
evaluateMDPSample :: MonadDistribution m => Process s -> m Double
evaluateMDPSample process = mhyloM mdpAlgebraSample (mdpCoalgebra process) (initialState process)

evaluateMDPExpect :: MonadDistribution m => Process s -> m Double
evaluateMDPExpect process = mhyloM mdpAlgebraExpect (mdpCoalgebra process) (initialState process)




