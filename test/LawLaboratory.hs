module LawLaboratory (runLawLaboratory) where

import Control.Monad (unless)
import Data.List.NonEmpty qualified as NE
import Data.Ratio ((%))
import Markovian.Action
import Markovian.Algebra.NonNegativeRational
import Markovian.Bayesian.Exact
import Markovian.Category.Finite.Object
import Markovian.Category.Finite.Set (finiteSet)
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Deterministic
import Markovian.Category.Matrix.Stochastic
import Markovian.Horizon
import Markovian.Interpreter.Exact
import Markovian.Kernel.Exact
import Markovian.MDP.Exact
import Markovian.Objective.Exact
import Markovian.Policy.Exact
import Markovian.Probability.Exact
import Markovian.Reward.Exact
import Markovian.Trace

checked :: (Show e) => Either e a -> IO a
checked = either (fail . show) pure

answer :: (Eq a, Show a) => String -> a -> a -> IO ()
answer label expected actual = do
    unless (actual == expected) (fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual))
    putStrLn (label ++ " = " ++ show actual)

runLawLaboratory :: IO ()
runLawLaboratory = labA >> labB >> labC >> labD

labA :: IO ()
labA = do
    unit <- checked (finiteSet [()])
    bits <- checked (finiteSet [False, True])
    half <- checked (nonNegativeRational (1 % 2))
    coinRaw <- checked (matrixFromRows unit bits [[half, half]])
    coin <- checked (stochasticMatrix coinRaw)
    shared <- checked (composeStochastic coin (copyStochastic bits))
    independent <- checked (composeStochastic (copyStochastic unit) (tensorStochastic coin coin))
    let rows = map (map getNonNegativeRational) . matrixRows . forgetStochastic
    answer "A shared (00,01,10,11)" [[1 % 2, 0, 0, 1 % 2]] (rows shared)
    answer "A independent" [[1 % 4, 1 % 4, 1 % 4, 1 % 4]] (rows independent)
    negation <- checked (deterministicFromFunction bits bits not)
    after <- checked (composeStochastic (embedDeterministic negation) (copyStochastic bits))
    before <- checked (composeStochastic (copyStochastic bits) (tensorStochastic (embedDeterministic negation) (embedDeterministic negation)))
    answer "A deterministic copy law" True (stochasticEquivalent (after :: StochasticMatrix NonNegativeRational Bool (Bool, Bool)) before)

labB :: IO ()
labB = do
    source <- checked (finiteObject [False, True])
    target <- checked (finiteObject [0 :: Int, 1, 2])
    rows <- traverse (traverse (checked . nonNegativeRational)) [[1, 0, 0], [1 % 2, 1 % 2, 0]]
    raw <- checked (matrixFromRows (forgetNonempty source) (forgetNonempty target) rows)
    channel <- checked (stochasticMatrix raw)
    sourcePrior <- checked (prior source [(False, 1 % 4), (True, 3 % 4)])
    posterior <- checked (condition sourcePrior channel 0)
    let mass x = fmap getNonNegativeRational (posteriorMass posterior x)
    answer "B transpose first row" [[1, 1 % 2]] (map (map getNonNegativeRational) (take 1 (matrixRows (transposeMatrix raw))))
    answer "B posterior at observation 0" [Just (2 % 5), Just (3 % 5)] (map mass [False, True])
    inverse <- checked (bayesianInverse sourcePrior channel)
    answer "B inverse supported rows" [[2 % 5, 3 % 5], [0, 1]] (map (map getNonNegativeRational) (matrixRows (forgetStochastic (inverseMatrix inverse))))
    answer "B zero evidence" True (case condition sourcePrior channel 2 of Left (ZeroEvidence 2) -> True; _ -> False)

data State = Live | Done deriving (Eq, Show)
data Move = Finish deriving (Eq, Show)

-- A hand-enumerated observable, not a call to the evaluator being checked.
crossMoment :: ExactFiniteDist (ExactTransitionOutcome State) -> Rational
crossMoment law = sum [exactProbability p * exactRewardValue (exactTransitionReward o) | (o, p) <- NE.toList (exactOutcomes law), exactSuccessorState o == Done]

labC :: IO ()
labC = do
    correlated <- checked (exactFiniteDist [(exactTransitionOutcome (exactReward 2) Done, 1 % 2), (exactTransitionOutcome (exactReward 0) Live, 1 % 2)])
    independent <- checked (exactFiniteDist [(exactTransitionOutcome (exactReward r) s, 1 % 4) | r <- [0, 2], s <- [Live, Done]])
    answer "C E[reward * indicator Done] joint" 1 (crossMoment correlated)
    answer "C E[reward * indicator Done] independent" (1 % 2) (crossMoment independent)
    objective <- finiteObjective 1
    let policy = exactPolicy (exactKernel (const (exactDirac (actionId Finish))))
        model law = exactMDP Live status available (exactKernel (const law))
        status Live = ExactContinuing
        status Done = ExactTerminal (exactReward 4)
        available Live = [actionId Finish]
        available Done = []
    values <- traverse (checked . fmap exactRewardValue . (\law -> expectedExactReturn objective (model law) policy)) [correlated, independent]
    answer "C additive returns agree" [2, 2] values
    traces <- traverse (checked . (\law -> exactTraceDistribution objective (model law) policy)) [correlated, independent]
    answer "C trace support sizes differ" [2, 4] (map (length . exactOutcomes) traces)

finiteObjective :: Integer -> IO ExactFiniteObjective
finiteObjective steps = do
    horizon <- checked (mkHorizon steps)
    discount <- checked (mkExactDiscount (1 % 2))
    pure (exactFiniteObjective horizon discount)

labD :: IO ()
labD = do
    zero <- finiteObjective 0
    one <- finiteObjective 1
    let finish = actionId Finish
        status Live = ExactContinuing
        status Done = ExactTerminal (exactReward 7)
        available Live = [finish]
        available Done = []
        model initial = exactMDP initial status available (exactKernel (const (exactDirac (exactTransitionOutcome (exactReward 2) Done))))
        policy = exactPolicy (exactKernel (const (exactDirac finish)))
    values <- traverse (\(objective, initial) -> exactRewardValue <$> checked (expectedExactReturn objective (model initial) policy)) [(zero, Live), (zero, Done), (one, Live)]
    answer "D returns (truncated, terminal, one step)" [0, 7, 11 % 2] values
    traces <- traverse (\objective -> checked (exactTraceDistribution objective (model Live) policy)) [zero, one]
    answer "D stop reasons" [[HorizonStop], [TerminalStop (exactReward 7)]] (map (map (traceStopReason . exactTrace . fst) . NE.toList . exactOutcomes) traces)
    answer "D realized transition counts" [[0], [1]] (map (map (length . traceSteps . exactTrace . fst) . NE.toList . exactOutcomes) traces)
