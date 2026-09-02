module Main (main) where

import Markovian.Action (actionId)
import Markovian.Horizon (mkHorizon)
import Markovian.Interpreter.Exact (expectedExactReturn)
import Markovian.Kernel.Exact (exactKernel)
import Markovian.MDP.Exact (
    ExactStateStatus (..),
    exactMDP,
    exactTransitionOutcome,
 )
import Markovian.Objective.Exact (exactFiniteObjective, mkExactDiscount)
import Markovian.Policy.Exact (exactPolicy)
import Markovian.Probability.Exact (exactDirac)
import Markovian.Reward.Exact (exactReward, exactRewardValue)

main :: IO ()
main = do
    horizon <- either (fail . show) pure (mkHorizon 1)
    discount <- either (fail . show) pure (mkExactDiscount (1 / 2))
    let finish = actionId Finish
        status Start = ExactContinuing
        status Done = ExactTerminal (exactReward 7)
        available Start = [finish]
        available Done = []
        model =
            exactMDP
                Start
                status
                available
                (exactKernel (\_ -> exactDirac (exactTransitionOutcome (exactReward 2) Done)))
        selectedPolicy = exactPolicy (exactKernel (const (exactDirac finish)))
        objective = exactFiniteObjective horizon discount
    result <- either (fail . show) pure (expectedExactReturn objective model selectedPolicy)
    putStrLn ("Expected return: " ++ show (exactRewardValue result))

data State = Start | Done
    deriving (Eq, Show)

data Action = Finish
    deriving (Eq, Show)
