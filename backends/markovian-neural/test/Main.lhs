\begin{code}
module Main (main) where

import qualified ActionMask
import qualified ActorCritic
import qualified DenseFiniteDifference
import qualified DQN
import qualified Information
import qualified ParametricReverse
import qualified PolicyGradient
import qualified Reinforce
import qualified ReplayTarget
import qualified ReverseProgram

main :: IO ()
main = do
    ActionMask.tests
    PolicyGradient.tests
    Information.tests
    DenseFiniteDifference.tests
    ParametricReverse.tests
    ReverseProgram.tests
    Reinforce.tests
    ActorCritic.tests
    ReplayTarget.tests
    DQN.tests
    putStrLn "PASS: markovian-neural"
\end{code}
