\begin{code}
module Main (main) where

import qualified ActorCritic
import qualified DenseFiniteDifference
import qualified DQN
import qualified Information
import qualified ParametricReverse
import qualified PolicyGradient
import qualified Reinforce
import qualified ReplayTarget

main :: IO ()
main = do
    PolicyGradient.tests
    Information.tests
    DenseFiniteDifference.tests
    ParametricReverse.tests
    Reinforce.tests
    ActorCritic.tests
    ReplayTarget.tests
    DQN.tests
    putStrLn "PASS: markovian-neural"
\end{code}
