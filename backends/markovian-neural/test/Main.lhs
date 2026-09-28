\begin{code}
module Main (main) where

import qualified ActionMask
import qualified ActorCritic
import qualified DenseFiniteDifference
import qualified DenseInspection
import qualified DQN
import qualified FrozenInspection
import qualified Information
import qualified ParametricReverse
import qualified PolicyInspection
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
    DenseInspection.runTests
    ParametricReverse.tests
    ReverseProgram.tests
    Reinforce.tests
    ActorCritic.tests
    ReplayTarget.tests
    DQN.tests
    PolicyInspection.runTests
    FrozenInspection.runTests
    putStrLn "PASS: markovian-neural"
\end{code}
