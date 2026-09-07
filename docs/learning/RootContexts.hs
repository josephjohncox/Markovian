{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- Teaching contexts intentionally infer parameter constraints. These are not
-- new public APIs. The book checker compiles this entire module with -Wall.
module RootContexts where

import Data.List.NonEmpty (NonEmpty (..))
import Markovian.Action
import Markovian.Bayesian.Exact
import Markovian.Category.Convex.Exact
import Markovian.Category.Finite.Object
import Markovian.Category.Finite.Set (finiteSet)
import Markovian.Category.Matrix.Deterministic
import Markovian.Category.Matrix.Stochastic
import Markovian.Circuit
import Markovian.Circuit.Compile.Deterministic
import Markovian.Circuit.Interpret.Cost
import Markovian.Circuit.Interpret.Exact
import Markovian.Circuit.Rewrite.Deterministic
import Markovian.Circuit.Rewrite.Deterministic.Exact
import Markovian.Compile.Exact
import Markovian.Interpreter.Bellman.Exact
import Markovian.Interpreter.Control.Exact
import Markovian.Interpreter.DynamicProgramming.Exact
import Markovian.Interpreter.Exact
import Markovian.Kernel.Exact
import Markovian.MDP.Exact
import Markovian.Objective.Exact
import Markovian.Open.Acyclic
import Markovian.Open.Acyclic.Circuit.Exact
import Markovian.POMDP.Exact
import Markovian.POMDP.Planning.Exact
import Markovian.Policy.Exact
import Markovian.Probability.Exact
import Markovian.Reward.Exact
import Markovian.Trace

-- Preserve the named intermediate bindings and explicit parameters shown in the book.
{-# ANN module ("HLint: ignore Redundant pure" :: String) #-}
{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
{-# ANN module ("HLint: ignore Use lambda-case" :: String) #-}

type DeterministicExample = 'Deterministic
type StochasticExample = 'Stochastic

sourcePriorExample sourceObject = do
    sourcePrior <-
        prior
            sourceObject
            [ (Healthy, 99 / 100)
            , (Sick, 1 / 100)
            ]
    pure sourcePrior

data Health = Healthy | Sick deriving (Eq, Show)

pushExample sourcePrior channel observed = do
    outputPrior <- either (fail . show) pure (pushforward sourcePrior channel)
    evidence <- either (fail . show) pure (observationEvidence sourcePrior channel observed)
    pure (outputPrior, evidence)

conditionExample sourcePrior channel observed = do
    posterior <- condition sourcePrior channel observed
    pure posterior

inverseExample sourcePrior channel = do
    inverse <- bayesianInverse sourcePrior channel
    let reverseChannel = inverseMatrix inverse
    pure reverseChannel

sharingExample coin = do
    let shared = shareCircuit coin
    independent <- fanoutCircuit coin coin
    pure (shared, independent)

circuitExample primitives circuit input = do
    matrix <- interpretExactCircuit primitives circuit
    result <- runExactCircuit primitives circuit input
    pure (matrix, result)

costExample limits costInterpreter circuit = do
    report <- interpretCircuitCost limits costInterpreter circuit
    pure report

rewriteExample rewriteLimits primitives deterministicCircuit = do
    let candidate = deduplicateDeterministicRewrite deterministicCircuit
    checked <- checkDeterministicRewrite rewriteLimits primitives candidate
    pure checked

comparisonExample costLimits costInterpreter checked = do
    comparison <- compareCheckedRewriteCosts costLimits costInterpreter checked
    pure comparison

quotedExample bits = do
    notTerm <-
        quotedTableTerm
            bits
            bits
            [ (False, True)
            , (True, False)
            ]
    pure notTerm

compileQuotedExample notTerm = do
    circuit <- compileDeterministicTerm notTerm
    pure circuit

denoteQuotedExample notTerm = do
    matrix <- denoteDeterministicTerm notTerm
    pure matrix

compileExample stateSupport actionSupport model = do
    compiled <-
        compileExactMDP
            stateSupport
            actionSupport
            model
    pure compiled

closeExample compiled policy = do
    compiledPolicy <- closeCompiledExactPolicy compiled policy
    pure compiledPolicy

traceExample objective model policy = do
    traces <- exactTraceDistribution objective model policy
    value <- expectedExactReturn objective model policy
    pure (traces, value)

finiteExample objective compiledPolicy = do
    report <- evaluateCompiledExactFinite objective compiledPolicy
    pure report

bellmanExample contractionDiscount tolerance maximumIterations compiledPolicy = do
    let config =
            exactBellmanConfig
                contractionDiscount
                tolerance
                maximumIterations
    report <- solveCompiledExactPolicy config compiledPolicy
    pure report

controlExample contractionDiscount tolerance maximumIterations compiled = do
    let config =
            exactValueIterationConfig
                contractionDiscount
                tolerance
                maximumIterations
    report <- solveCompiledExactControl config compiled
    pure report

greedyExample contractionDiscount compiled values = do
    greedy <-
        extractExactGreedyActions
            contractionDiscount
            compiled
            values
    pure greedy

policyIterationExample contractionDiscount maximumPolicyImprovements compiled = do
    let config =
            exactPolicyIterationConfig
                contractionDiscount
                maximumPolicyImprovements
    report <- solveCompiledExactPolicyIteration config compiled
    pure report

data State = Start | Done deriving (Eq, Show)
data Move = Finish deriving (Eq, Show)
finish = actionId Finish
status Start = ExactContinuing
status Done = ExactTerminal (exactReward 7)

available Start = [finish]
available Done = []

outcomeExample = exactTransitionOutcome (exactReward 2) Done

selectedPolicy =
    exactPolicy (exactKernel (const (exactDirac finish)))

objectiveExample horizon discount =
    let objective = exactFiniteObjective horizon discount
     in objective

outcomesExample =
    exactFiniteDist
        [ (exactTransitionOutcome (exactReward 2) Done, 3 / 4)
        , (exactTransitionOutcome (exactReward (-1)) Start, 1 / 4)
        ]

data Color = Red | Green | Blue deriving (Eq, Show)
colorsExample = do
    colors <- finiteSet [Red, Green, Blue]
    pure colors

stochasticExample rawMatrix = do
    channel <- stochasticMatrix rawMatrix
    pure channel

negationExample bits = do
    negation <-
        deterministicFromFunction bits bits not
    let asChannel = embedDeterministic negation
    pure asChannel

convexExample safeWeight safePolicy fastWeight fastPolicy = do
    family <-
        convexFamily
            ((safeWeight, safePolicy) :| [(fastWeight, fastPolicy)])
    let mixedPolicy = convexMixture family
    pure mixedPolicy

outcomeConstructor reward successor = exactTransitionOutcome reward successor
statusConstructors payoff = (ExactContinuing, ExactTerminal payoff)
stopConstructors payoff = (TerminalStop payoff, HorizonStop)
objectiveConstructor horizon discount = exactFiniteObjective horizon discount

acyclicTopologyExample rawSystem = do
    topology <- acyclicOpenSystem rawSystem
    pure topology
acyclicDomainsExample domainEntries = do
    domains <- finiteValueDomains domainEntries
    pure domains
acyclicLabelsExample localCircuits = do
    labels <- labelCircuitTable localCircuits
    pure labels
acyclicNetworkExample topology domains labels = do
    network <- acyclicOpenCircuit topology domains labels
    pure network

pomdpExample model initialBelief observe =
    let pomdp = exactPOMDP model initialBelief observe
     in pomdp
predictExample pomdp action incomingPrior = do
    predicted <- predictExactBelief pomdp action incomingPrior
    pure predicted
conditionBeliefExample pomdp action observation predicted = do
    posterior <-
        conditionExactBelief pomdp action observation predicted
    pure posterior
filterExample pomdp action observation incomingPrior = do
    posterior <- filterExactBelief pomdp action observation incomingPrior
    pure posterior
beliefPolicyExample chooseAction objective pomdp = do
    let policy = exactBeliefPolicy chooseAction
    value <- expectedExactBeliefReturn objective pomdp policy
    pure value

data Weather = Dry | Wet deriving (Eq, Show)
data Sensor = Clear | Alarm deriving (Eq, Show)
weatherExample = do
    weather <-
        exactFiniteDist
            [ (Dry, 3 / 4)
            , (Wet, 1 / 4)
            ]
    pure weather
limitsExample = do
    limits <- exactBindLimits 4096 8320 13 13
    pure limits
bindExample limits outer continuation = do
    (result, report) <-
        bindExactFiniteDistChecked limits outer (Right . continuation)
    pure (result, report)
sensorExample = do
    drySensor <- exactFiniteDist [(Clear, 9 / 10), (Alarm, 1 / 10)]
    wetSensor <- exactFiniteDist [(Clear, 1 / 5), (Alarm, 4 / 5)]
    let sensor = exactKernel $ \surface ->
            case surface of
                Dry -> drySensor
                Wet -> wetSensor
    pure sensor
normalizeExample normalizeInput = exactKernel (exactDirac . normalizeInput)
supportExample left right = sameFiniteSupport left right
layoutExample left right = sameFiniteLayout left right
