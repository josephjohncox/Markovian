module MarkovianDifferential (tests) where

import Markovian.Action (actionId)
import Markovian.Backend.Neural (
    DQNTargetSelection (StandardDQN),
    denseForward,
    dqnBatchTargets,
    dqnOnlineNetwork,
    dqnUpdateEvaluation,
    dqnUpdatedState,
    mkActionMask,
    mkDQNConfig,
    mkDQNState,
    mkDenseNetwork,
    mkSGD,
    mkTerminalTransition,
    noAutomaticTargetUpdates,
    updateDQNBatch,
 )
import Markovian.Kernel (kernel)
import Markovian.Learning.QLearning (
    ObservedTransition (..),
    QUpdateResult (..),
    emptyQTable,
    mkLearningRate,
    qValue,
    updateQ,
 )
import Markovian.MDP (MDP, mdp)
import Markovian.MRP (StateStatus (..), transitionOutcome)
import Markovian.Objective (mkDiscount)
import Markovian.Probability (dirac)
import Markovian.Reward (mkReward)
import TestSupport (assertClose, assertVectorClose, requireRight)

data DifferentialState = DifferentialSource | DifferentialTerminal
    deriving (Eq, Show)

data DifferentialAction = DifferentialAction
    deriving (Eq, Show)

tests :: IO ()
tests = do
    rootAndNeuralUpdateAgree
    putStrLn "PASS: root/neural Q-learning differential"

rootAndNeuralUpdateAgree :: IO ()
rootAndNeuralUpdateAgree = do
    reward <- requireRight "differential reward" (mkReward 2)
    payoff <- requireRight "differential payoff" (mkReward 0)
    rate <- requireRight "differential rate" (mkLearningRate 0.1)
    discount <- requireRight "differential discount" (mkDiscount 0.5)
    let selected = actionId DifferentialAction
        model :: MDP DifferentialState DifferentialAction
        model =
            mdp
                DifferentialSource
                (\state -> if state == DifferentialTerminal then Terminal payoff else Continuing)
                (const [selected])
                (kernel (const (dirac (transitionOutcome reward DifferentialTerminal))))
        observation = ObservedTransition DifferentialSource selected reward DifferentialTerminal
    root <- requireRight "root Q update" (updateQ rate discount model observation emptyQTable)

    mask <- requireRight "differential mask" (mkActionMask 2 [0, 1])
    transition <- requireRight "differential transition" (mkTerminalTransition [0] mask 0 2 0)
    online <- requireRight "differential online" (mkDenseNetwork 1 [] 2 [0, 0, 0, 0])
    target <- requireRight "differential target" (mkDenseNetwork 1 [] 2 [0, 0, 0, 0])
    optimizer <- requireRight "differential SGD" (mkSGD 0.1)
    config <- requireRight "differential DQN config" (mkDQNConfig 0.5 optimizer StandardDQN noAutomaticTargetUpdates)
    state <- requireRight "differential DQN state" (mkDQNState online target)
    neural <- requireRight "neural Q update" (updateDQNBatch config state [transition])
    outputs <- requireRight "updated neural Q values" (denseForward (dqnOnlineNetwork (dqnUpdatedState neural)) [0])

    assertVectorClose "root/neural target" 1e-15 [qValue (qUpdateTarget root)] (dqnBatchTargets (dqnUpdateEvaluation neural))
    case outputs of
        selectedOutput : _ ->
            assertClose "root/neural selected Q update" 1e-15 (qValue (qUpdateNewValue root)) selectedOutput
        [] -> fail "differential network returned no outputs"
