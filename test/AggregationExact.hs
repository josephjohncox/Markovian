module AggregationExact (runAggregationTests, runAggregationLesson) where

import Control.Monad (forM_, unless)
import Data.Either (fromRight)
import Data.List (nub, sort)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Markovian.Action
import Markovian.Aggregation.Exact
import Markovian.Category.Finite.Set
import Markovian.Compile.Exact
import Markovian.Horizon
import Markovian.Interpreter.DynamicProgramming.Exact
import Markovian.Kernel.Exact
import Markovian.MDP.Exact
import Markovian.Objective.Exact
import Markovian.Policy.Exact
import Markovian.Probability.Exact
import Markovian.Reward.Exact

-- Independent source fixture: terminal payoff or raw reward/successor masses.
type Law = Either Rational [(Rational, Int, Rational)]
type Fixture = [(Int, Law)]
type Result = Either (AggregationError Int Int) (AggregationResult Int Int Int, AggregationAccounting)

right :: Either e a -> a
right = fromRight (error "invalid aggregation test fixture")

assert :: String -> Bool -> IO ()
assert label ok = unless ok (fail label)

equal :: (Eq a, Show a) => String -> a -> a -> IO ()
equal label expected actual = assert (label ++ ": expected " ++ show expected ++ ", got " ++ show actual) (expected == actual)

large :: AggregationLimits
large = AggregationLimits 100 100 1000 1000000 4096

set :: [Int] -> FiniteSet Int
set = right . finiteSet

model :: Fixture -> CompiledExactMRP Int
model fixture = right (closeCompiledExactPolicy compiled policy)
  where
    source = case fixture of (s, _) : _ -> s; [] -> error "empty fixture"
    get s = fromJust (lookup s fixture)
    status s = either (ExactTerminal . exactReward) (const ExactContinuing) (get s)
    available s = either (const []) (const [actionId ()]) (get s)
    law s = case get s of
        Left _ -> exactDirac (exactTransitionOutcome (exactReward 0) s)
        Right atoms -> right (exactFiniteDist [(exactTransitionOutcome (exactReward r) t, p) | (r, t, p) <- atoms])
    mdp = exactMDP source status available (exactKernel (\(s, _) -> law s))
    compiled = right (compileExactMDP (map fst fixture) [actionId ()] mdp)
    policy = exactPolicy (exactKernel (const (exactDirac (actionId ()))))

check :: AggregationLimits -> Fixture -> [Int] -> [(Int, Int)] -> [(Int, Int)] -> Result
check limits fixture blocks = checkFixedPolicyAggregation limits (model fixture) (set blocks)

success :: Fixture
success =
    [ (20, Right [(2, 10, 1 % 4), (0, 20, 1 % 2), (2, 10, 1 % 4)])
    , (30, Right [(0, 30, 1 % 2), (2, 11, 1 % 2)])
    , (10, Left 7)
    , (11, Left 7)
    ]

mapping :: [(Int, Int)]
mapping = [(20, 8), (30, 8), (10, 4), (11, 4)]

observations :: [(Int, Int)]
observations = [(20, 5), (30, 5), (10, 9), (11, 9)]

quotient :: Result -> CheckedAggregation Int Int Int
quotient (Right (AggregationEquivalent q, _)) = q
quotient _ = error "expected equivalent fixture"

witness :: Result -> AggregationWitness Int Int Int
witness (Right (AggregationDistinguished w, _)) = w
witness _ = error "expected distinguished fixture"

account :: Result -> AggregationAccounting
account = snd . right

-- Independent finite recursion over only the public quotient table.
quotientValue :: Int -> Rational -> CheckedAggregation Int Int Int -> Int -> Rational
quotientValue h gamma q b = case [law | (v, _, law) <- aggregationRows q, v == b] of
    [Left p] -> exactRewardValue p
    [Right atoms]
        | h == 0 -> 0
        | otherwise -> sum [p * (exactRewardValue r + gamma * quotientValue (h - 1) gamma q t) | (r, t, p) <- atoms]
    _ -> error "invalid quotient table"

-- Paths retain every state observation, transition reward and stop reason.
-- This source enumerator knows neither compiled tables nor canonical insertion.
data Stop = Truncated | Terminal Rational deriving (Eq, Ord, Show)
type Trace = ([Int], [Rational], Stop)
sourceTraces :: Fixture -> [(Int, Int)] -> Int -> Int -> [(Trace, Rational)]
sourceTraces fixture labels h s =
    let o = fromJust (lookup s labels)
     in case fromJust (lookup s fixture) of
            Left p -> [(([o], [], Terminal p), 1)]
            Right _ | h == 0 -> [(([o], [], Truncated), 1)]
            Right atoms ->
                [ ((o : os, r : rs, stop), p * q)
                | (r, t, p) <- atoms
                , ((os, rs, stop), q) <- sourceTraces fixture labels (h - 1) t
                ]

-- Deliberately a separate implementation over the quotient, not sourceTraces
-- applied to a converted production table.
quotientTraces :: CheckedAggregation Int Int Int -> Int -> Int -> [(Trace, Rational)]
quotientTraces q h b =
    concat
        [ case law of
            Left payoff -> [(([o], [], Terminal (exactRewardValue payoff)), 1)]
            Right _ | h == 0 -> [(([o], [], Truncated), 1)]
            Right atoms ->
                concatMap
                    ( \(r, t, p) ->
                        map
                            (\((os, rs, stop), mass) -> ((o : os, exactRewardValue r : rs, stop), p * mass))
                            (quotientTraces q (h - 1) t)
                    )
                    atoms
        | (v, o, law) <- aggregationRows q
        , v == b
        ]

-- Sum by exhaustive equality classes; no production sort/merge logic reused.
traceLaw :: [(Trace, Rational)] -> [(Trace, Rational)]
traceLaw paths = [(t, sum [p | (u, p) <- paths, t == u]) | t <- sort (nub (map fst paths))]

runAggregationTests :: (String -> IO () -> IO ()) -> IO ()
runAggregationTests run = do
    run "aggregation independent finite values and reward-observation-stop traces" testOracles
    run "aggregation mean, correlation, terminal, payoff and observation witnesses" testDifferences
    run "aggregation bounded malformed lists and failure precedence" testAdmission
    run "aggregation exact limits, cumulative work prefixes and witness exhaustion" testLimits
    run "aggregation rational duplicate growth and work-before-bits" testGrowth
    run "aggregation executable success and witness lesson" runAggregationLesson

testOracles :: IO ()
testOracles = do
    let result = check large success [4, 8] (reverse mapping) (reverse observations)
        q = quotient result
    equal "original layout" (compiledMRPStateIndex (model success)) (aggregationOriginalLayout q)
    equal "mapping canonical" mapping (aggregationMapping q)
    equal "block layout" [4, 8] (finiteSetValues (aggregationBlockLayout q))
    equal "initial block" 8 (aggregationInitialBlock q)
    equal "canonical rows" [(4, 9, Left (exactReward 7)), (8, 5, Right [(exactReward 0, 8, 1 % 2), (exactReward 2, 4, 1 % 2)])] (aggregationRows q)
    equal "input permutations accounting" (account (check large success [4, 8] mapping observations)) (account result)
    let signed = [(s, either (Left . negate) (Right . map (\(r, t, p) -> (r - 3, t, p))) law) | (s, law) <- success]
    forM_ [success, reverse success, signed] $ \fixture -> do
        let compiled = model fixture
            checked = quotient (check large fixture [8, 4] mapping observations)
        forM_ [0 .. 5] $ \h -> do
            forM_ [0, 1 % 2, 1] $ \gamma -> do
                let objective = exactFiniteObjective (right (mkHorizon (fromIntegral h))) (right (mkExactDiscount gamma))
                    report = right (evaluateCompiledExactFinite objective compiled)
                forM_ (NE.toList (compiledMRPStateEntries compiled)) $ \(i, row) -> do
                    let s = compiledMRPSourceState row
                        b = fromJust (lookup s mapping)
                    equal "lifted DP" (exactRewardValue (fromJust (lookup i (NE.toList (exactFiniteDPValues report))))) (quotientValue h gamma checked b)
            forM_ mapping $ \(s, b) -> equal "trace law" (traceLaw (sourceTraces fixture observations h s)) (traceLaw (quotientTraces checked h b))

meanOnly :: Fixture
meanOnly = [(0, Right [(0, 2, 1 % 2), (2, 2, 1 % 2)]), (1, Right [(1, 2, 1)]), (2, Left 0)]
meanMapping :: [(Int, Int)]
meanMapping = [(0, 0), (1, 0), (2, 1)]
meanObservations :: [(Int, Int)]
meanObservations = [(0, 0), (1, 0), (2, 1)]

testDifferences :: IO ()
testDifferences = do
    let cases =
            [ (meanOnly, [0, 1], meanMapping, meanObservations, AggregationJointMassDifference (exactReward 0) 1 (1 % 2) 0)
            , ([(0, Left 0), (1, Right [(0, 0, 1)])], [0], [(0, 0), (1, 0)], [(0, 0), (1, 0)], AggregationTerminalDifference True False)
            , ([(0, Left 2), (1, Left 3)], [0], [(0, 0), (1, 0)], [(0, 0), (1, 0)], AggregationPayoffDifference (exactReward 2) (exactReward 3))
            , ([(0, Left 2), (1, Right [(0, 0, 1)])], [0], [(0, 0), (1, 0)], [(0, 8), (1, 9)], AggregationObservationDifference 8 9)
            ,
                ( [(0, Right [(0, 2, 1 % 2), (2, 3, 1 % 2)]), (1, Right [(2, 2, 1 % 2), (0, 3, 1 % 2)]), (2, Left 0), (3, Left 0)]
                , [0, 1, 2]
                , [(0, 0), (1, 0), (2, 1), (3, 2)]
                , [(0, 0), (1, 0), (2, 1), (3, 2)]
                , AggregationJointMassDifference (exactReward 0) 1 (1 % 2) 0
                )
            ]
    forM_ cases $ \(fixture, bs, m, os, expected) -> do
        let w = witness (check large fixture bs m os)
        equal "witness states" (0, 1) (aggregationWitnessStates w)
        equal "witness block" 0 (aggregationWitnessBlock w)
        equal "witness difference" expected (aggregationWitnessDifference w)
        assert "independent trace distinction" (traceLaw (sourceTraces fixture os 1 0) /= traceLaw (sourceTraces fixture os 1 1))
        prefixes (\limits -> check limits fixture bs m os)
    equal "mean-only equal means" [1, 1] [sum [r * p | (r, _, p) <- atoms] | (_, Right atoms) <- meanOnly]
    -- Both separate marginals in the correlation fixture agree; only the joint differs.
    let correlation = [[(0 :: Rational, 2 :: Int, 1 % 2 :: Rational), (2, 3, 1 % 2)], [(2, 2, 1 % 2), (0, 3, 1 % 2)]]
    equal "equal reward marginals" (replicate 2 [1 % 2, 1 % 2]) [[sum [p | (r, _, p) <- atoms, r == reward] | reward <- [0, 2]] | atoms <- correlation]
    equal "equal successor marginals" (replicate 2 [1 % 2, 1 % 2]) [[sum [p | (_, s, p) <- atoms, s == target] | target <- [2, 3]] | atoms <- correlation]
    equal
        "joint missing head in representative"
        (AggregationJointMassDifference (exactReward 0) 1 0 (1 % 2))
        (aggregationWitnessDifference (witness (check large (reverse meanOnly) [0, 1] meanMapping meanObservations)))
    -- Equal keys but unequal masses.
    let massFixture = [(0, Right [(0, 2, 1 % 3), (1, 2, 2 % 3)]), (1, Right [(0, 2, 1 % 2), (1, 2, 1 % 2)]), (2, Left 0)]
    equal
        "unequal mass"
        (AggregationJointMassDifference (exactReward 0) 1 (1 % 3) (1 % 2))
        (aggregationWitnessDifference (witness (check large massFixture [0, 1] meanMapping meanObservations)))
    let conflicts = [(0, Left 0), (1, Left 1), (2, Left 2), (3, Left 3)]
        m = [(0, 8), (1, 8), (2, 4), (3, 4)]
        os = [(0, 0), (1, 0), (2, 0), (3, 0)]
    equal "block order picks first witness" (2, 3) (aggregationWitnessStates (witness (check large conflicts [4, 8] (reverse m) (reverse os))))
    equal "member order follows original layout" (1, 0) (aggregationWitnessStates (witness (check large (reverse conflicts) [8, 4] m os)))

leftIs :: String -> AggregationError Int Int -> Result -> IO ()
leftIs label expected actual = case actual of
    Left e -> equal label expected e
    Right _ -> fail (label ++ ": unexpectedly admitted")

testAdmission :: IO ()
testAdmission = do
    let checkM = check large success [4, 8]
    leftIs "duplicate before block" (AggregationDuplicatePartitionState 20) (checkM [(20, 8), (20, 99)] observations)
    leftIs "unknown before block" (AggregationUnknownPartitionState 99) (checkM [(99, 99)] observations)
    leftIs "unknown block" (AggregationUnknownBlock 99) (checkM [(20, 99)] observations)
    leftIs "missing state" (AggregationMissingPartitionState 30) (checkM [(20, 8)] [])
    leftIs "unused block before observations" (AggregationUnusedBlock 4) (checkM [(s, 8) | (s, _) <- mapping] [])
    leftIs "duplicate observation" (AggregationDuplicateObservationState 20) (checkM mapping [(20, 5), (20, 6)])
    leftIs "unknown observation" (AggregationUnknownObservationState 99) (checkM mapping [(99, 0)])
    leftIs "missing observation" (AggregationMissingObservationState 30) (checkM mapping [(20, 0)])
    leftIs "empty blocks" AggregationEmptyBlocks (check large success [] mapping observations)
    let tiny = large{maximumAggregationStates = 4}
    leftIs "infinite partition bounded before validation" (AggregationLimitExceeded AggregationStates 4 5) (check tiny success [4, 8] (repeat (99, 99)) observations)
    leftIs "infinite observation bounded before validation" (AggregationLimitExceeded AggregationStates 4 5) (check tiny success [4, 8] mapping (repeat (99, 99)))
    leftIs "spine before empty" (AggregationLimitExceeded AggregationStates 4 5) (check tiny success [] (repeat (99, 99)) [])
    leftIs "support before validation" (AggregationLimitExceeded AggregationSupport 0 1) (check large{maximumAggregationSupport = 0} success [4, 8] [] [])
    leftIs "reservation before validation" (AggregationLimitExceeded AggregationWork 0 1) (check large{maximumAggregationWork = 0} success [4, 8] [] [])
    let huge = fromIntegral (maxBound :: Int) + 1
    forM_
        [ (AggregationStates, \l -> l{maximumAggregationStates = huge})
        , (AggregationBlocks, \l -> l{maximumAggregationBlocks = huge})
        , (AggregationSupport, \l -> l{maximumAggregationSupport = huge})
        , (AggregationWork, \l -> l{maximumAggregationWork = huge})
        , (AggregationRationalBits, \l -> l{maximumAggregationRationalBits = huge})
        ]
        $ \(d, change) ->
            leftIs "machine cap" (AggregationMachineOverflow d) (check (change large) success [4, 8] mapping observations)
    leftIs "machine field order" (AggregationMachineOverflow AggregationStates) (check (AggregationLimits huge huge huge huge huge) success [] [] [])

prefixes :: (AggregationLimits -> Result) -> IO ()
prefixes operation = do
    let a = account (operation large)
        exact = AggregationLimits (aggregationStateCount a) (aggregationBlockCount a) (aggregationRawSupportCount a) (aggregationWorkCharged a) (aggregationMaximumObservedBits a)
    equal "simultaneous exact" a (account (operation exact))
    forM_ [0 .. aggregationWorkCharged a - 1] $ \w ->
        leftIs "every cumulative work prefix, including witness/retention" (AggregationLimitExceeded AggregationWork w (w + 1)) (operation exact{maximumAggregationWork = w})
    forM_
        [ (AggregationStates, aggregationStateCount a, \v -> exact{maximumAggregationStates = v})
        , (AggregationBlocks, aggregationBlockCount a, \v -> exact{maximumAggregationBlocks = v})
        , (AggregationSupport, aggregationRawSupportCount a, \v -> exact{maximumAggregationSupport = v})
        , (AggregationRationalBits, aggregationMaximumObservedBits a, \v -> exact{maximumAggregationRationalBits = v})
        ]
        $ \(d, n, change) ->
            unless (n == 0) (leftIs "one below dimension" (AggregationLimitExceeded d (n - 1) n) (operation (change (n - 1))))

testLimits :: IO ()
testLimits = do
    let op limits = check limits success [4, 8] mapping observations
    -- W0=121; 13 input observations, 9 insertion/addition units, 4 retained
    -- masses, 9 pair comparisons, and 6 retained quotient/mapping rows.
    equal "full success ledger" (AggregationAccounting 4 2 5 4 162 3) (account (op large))
    prefixes op
    let terminal limits = check limits [(0, Left 0)] [0] [(0, 0)] [(0, 0)]
    equal "terminal zero-support ledger" (AggregationAccounting 1 1 0 0 12 1) (account (terminal large))
    prefixes terminal
    leftIs "synthetic zero observed" (AggregationLimitExceeded AggregationRationalBits 0 1) (terminal large{maximumAggregationRationalBits = 0})

testGrowth :: IO ()
testGrowth = do
    -- 1/3+1/4=7/12 needs four bits; all raw inputs need at most three.
    -- The intermediate disappears into a retained unit mass.
    let fixture = [(0, Right [(0, 0, p) | p <- [1 % 3, 1 % 4, 1 % 4, 1 % 6]])]
        op limits = check limits fixture [0] [(0, 0)] [(0, 0)]
    equal "discarded addition maximum" 4 (aggregationMaximumObservedBits (account (op large)))
    leftIs "discarded addition bits" (AggregationLimitExceeded AggregationRationalBits 3 4) (op large{maximumAggregationRationalBits = 3})
    leftIs "addition work before bits" (AggregationLimitExceeded AggregationWork 31 32) (op large{maximumAggregationWork = 31, maximumAggregationRationalBits = 3})
    prefixes op
    let laterGrowth = [(0, Left 0), (1, Left 1), (2, Right [(0, 2, p) | p <- [1 % 3, 1 % 4, 1 % 4, 1 % 6]])]
    leftIs
        "all canonical rows before first witness"
        (AggregationLimitExceeded AggregationRationalBits 3 4)
        (check large{maximumAggregationRationalBits = 3} laterGrowth [0, 1] [(0, 0), (1, 0), (2, 1)] [(0, 0), (1, 0), (2, 0)])
    -- Discarded reward input in a later row precedes any first witness.
    let bad = [(0, Left 0), (1, Left 1), (2, Left 1024)]
        operation limits = check limits bad [0, 1] [(0, 0), (1, 0), (2, 1)] [(0, 0), (1, 0), (2, 0)]
    leftIs "all inputs before witness" (AggregationLimitExceeded AggregationRationalBits 4 5) (operation large{maximumAggregationRationalBits = 4})
    -- W0=53 for n=3,b=2,k=0; input 1024 is the third observation.
    leftIs "work before large input bits" (AggregationLimitExceeded AggregationWork 55 56) (operation large{maximumAggregationWork = 55, maximumAggregationRationalBits = 4})
    let growth = [(0, Right [(0, 0, 1 % 7), (0, 0, 1 % 7), (0, 0, 1 % 7), (0, 0, 1 % 7), (0, 0, 1 % 7), (0, 0, 1 % 7), (0, 0, 1 % 7)])]
    prefixes (\limits -> check limits growth [0] [(0, 0)] [(0, 0)])

runAggregationLesson :: IO ()
runAggregationLesson = do
    let q = quotient (check large success [4, 8] mapping observations)
        w = witness (check large meanOnly [0, 1] meanMapping meanObservations)
    equal "lesson quotient values" [0, 11 % 4, 55 % 16] [quotientValue h (1 % 2) q 8 | h <- [0, 1, 2]]
    equal "lesson terminal at horizon zero" 7 (quotientValue 0 (1 % 2) q 4)
    equal "lesson witness" (AggregationJointMassDifference (exactReward 0) 1 (1 % 2) 0) (aggregationWitnessDifference w)
    putStrLn ("aggregation mapping = " ++ show (aggregationMapping q))
    putStrLn ("live values h=0,1,2 = " ++ show [quotientValue h (1 % 2) q 8 | h <- [0, 1, 2]])
    putStrLn "terminal value h=0 = 7"
    putStrLn ("failed merge states = " ++ show (aggregationWitnessStates w))
    putStrLn ("failed merge witness = " ++ show (aggregationWitnessDifference w))
