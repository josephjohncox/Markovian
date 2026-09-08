module Main (main) where

import Control.Monad (forM_, unless)
import Markovian.Continuous.Kernel.JointAffine.Exact
import Markovian.Continuous.Measure.Exact
import Markovian.Continuous.Space (RealBorel)

data LeftOwner
data RightOwner
data InputLabel
data MiddleLabel
data LeftRewardLabel
data RightRewardLabel
data SuccessorLabel

main :: IO ()
main = do
    interval <- right (rationalInterval 0 1)
    let limits = exactJointAffineLimits 3 3 2 20 100 8
    left <- right (exactJointAffineKernel limits (affineInputCoordinate 0 2) (affineInputCoordinate 0 0) [(noiseOwner 1, interval, 0, 1)] :: Either ExactJointAffineError (ExactJointAffineKernel LeftOwner InputLabel LeftRewardLabel MiddleLabel))
    kernel <- right (exactJointAffineKernel limits (affineInputCoordinate 1 0) (affineInputCoordinate 0 0) [(noiseOwner 2, interval, 0, 1)] :: Either ExactJointAffineError (ExactJointAffineKernel RightOwner MiddleLabel RightRewardLabel SuccessorLabel))
    forM_ [sharedSuccessorOwners [(noiseOwner 2, noiseOwner 1)], freshSuccessorOwners [(noiseOwner 2, 3)]] $ \request -> do
        result <- right (substituteLeftSuccessor (exactSuccessorSubstitutionLimits 3 3 2 20 77 2) left kernel request)
        (law, _) <- right (materializeSuccessorSubstitution limits result 0 :: Either ExactJointAffineError (ExactJointLaw RealBorel RealBorel, ExactJointAffineReport))
        ((rewardBounds, successorBounds), _) <- right (successorSubstitutionSupportExtrema limits result interval)
        unless (intervalBounds rewardBounds == (0, 1) && intervalBounds successorBounds == (0, 1) && substitutionWork (successorSubstitutionReport result) == 77) (fail "positive client result")
        law `seq` pure ()
    putStrLn "PASS: installed public substitution client, both modes and projections"

right :: (Show e) => Either e a -> IO a
right = either (fail . show) pure
