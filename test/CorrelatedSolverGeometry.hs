{-# LANGUAGE ImportQualifiedPost #-}

{- | D083 §11.5 complete tiny geometry and §11.8 independent search ledger.

Standalone base-only probe compiled against @-isrc@. The oracle enumerates
subsets recursively and uses determinants, Cramer's rule, and ranks of minors;
it shares no builder, replacement, traversal, or elimination with production.
Only this tiny test collects vertices. Public solves still stop at first success.
-}
module Main (main) where

import Control.Monad (unless, void, when)
import Data.List (nub, sort, transpose)
import Data.Maybe (fromJust)
import Markovian.Category.Finite.Object
import Markovian.Game.Correlated.Exact qualified as Public
import Markovian.Game.Correlated.Exact.Internal
import Markovian.Game.NormalForm.Exact
import Markovian.Game.Profile.Finite
import Numeric.Natural (Natural)
import System.Environment (getArgs)

data Owner = Row | Column deriving (Eq, Show)
data Action = A | B deriving (Eq, Show)
data Fixture = Coordination | MatchingPennies deriving (Eq, Show)

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        [] -> mapM_ checkFixture [(fixture, mode) | fixture <- [Coordination, MatchingPennies], mode <- [CorrelatedMode, CoarseMode]]
        ["--final-boundary", mode, block, resource, edge] -> checkFinalBoundary mode block resource edge
        _ -> ioError (userError "unknown geometry control")

assert :: String -> Bool -> IO ()
assert message condition = unless condition (ioError (userError message))

orDie :: (Show problem) => Either problem value -> value
orDie = either (error . show) id

ceiling_ :: Natural
ceiling_ = fromIntegral (maxBound :: Int) - 1

gameCaps :: GameLimits
gameCaps = gameLimits 64 64 64 4096 ceiling_ 64 ceiling_

solveCaps :: CorrelationSolveLimits
solveCaps = correlationSolveLimits gameCaps ceiling_ ceiling_ ceiling_

private :: Solve Owner value -> value
private = fst . orDie . runSolve

-- These labelled tables feed the public constructor and the independent
-- evaluator. The evaluator changes a coordinate directly and reads the table;
-- it never asks the game for a payoff or calls replaceChoice.
profiles :: [(Action, Action)]
profiles = [(A, A), (A, B), (B, A), (B, B)]

payoffs :: Fixture -> [((Action, Action), [(Owner, Rational)])]
payoffs fixture = zip profiles (map (zip [Row, Column]) values)
  where
    values = case fixture of
        Coordination -> [[1, 1], [0, 0], [0, 0], [1, 1]]
        MatchingPennies -> [[1, -1], [-1, 1], [-1, 1], [1, -1]]

game :: Fixture -> ExactNormalGame Owner Action
game fixture = orDie (exactNormalGame gameCaps product_ (map entry (payoffs fixture)))
  where
    owners = orDie (finiteObject [Row, Column])
    choices = orDie (finiteObject [A, B])
    product_ = orDie (ownedProduct gameCaps owners [(Row, choices), (Column, choices)])
    entry ((row, column), values) =
        ( orDie (ownedProfile product_ [(Row, row), (Column, column)])
        , orDie (exactPlayerValues gameCaps owners values)
        )

labels :: SolveMode -> [RowLabel Owner Action]
labels mode =
    map NonnegativityRow [0 .. 3] ++ case mode of
        CorrelatedMode -> [ObedienceRow Row A B, ObedienceRow Row B A, ObedienceRow Column A B, ObedienceRow Column B A]
        CoarseMode -> [CoarseRow Row A, CoarseRow Row B, CoarseRow Column A, CoarseRow Column B]

labelledSlacks :: Fixture -> SolveMode -> [Rational] -> [Rational]
labelledSlacks fixture mode mass = map evaluate (labels mode)
  where
    utility owner profile = fromJust (lookup profile (payoffs fixture) >>= lookup owner)
    chosen Row = fst
    chosen Column = snd
    changed Row alternative (_, column) = (alternative, column)
    changed Column alternative (row, _) = (row, alternative)
    difference owner alternative profile = utility owner profile - utility owner (changed owner alternative profile)
    evaluate (NonnegativityRow index) = mass !! fromIntegral index
    evaluate (ObedienceRow owner recommendation alternative) =
        sum [weight * difference owner alternative profile | (profile, weight) <- zip profiles mass, chosen owner profile == recommendation]
    evaluate (CoarseRow owner alternative) =
        sum [weight * difference owner alternative profile | (profile, weight) <- zip profiles mass]

unit :: Int -> [Rational]
unit index = [if position == index then 1 else 0 | position <- [0 .. 3]]

vertices :: Fixture -> [[Rational]]
vertices Coordination = [unit 0, unit 3, [1 / 3, 1 / 3, 0, 1 / 3], [1 / 3, 0, 1 / 3, 1 / 3], replicate 4 (1 / 4)]
vertices MatchingPennies = [replicate 4 (1 / 4)]

-- Include/exclude enumeration is independent of the production successor.
choose :: Int -> [value] -> [[value]]
choose 0 _ = [[]]
choose _ [] = []
choose count (value : rest) = map (value :) (choose (count - 1) rest) ++ choose count rest

without :: Int -> [value] -> [value]
without index values = take index values ++ drop (index + 1) values

-- Laplace expansion, with det(empty)=1, rather than row reduction.
determinant :: [[Rational]] -> Rational
determinant [] = 1
determinant (row : rows) =
    sum [sign * value * determinant (map (without column) rows) | (column, value) <- zip [0 ..] row, let sign = if even column then 1 else -1]

matrixRank :: [[Rational]] -> Int
matrixRank [] = 0
matrixRank rows@(row : _) =
    maximum
        [ size
        | size <- [0 .. min (length rows) (length row)]
        , any ((/= 0) . determinant) [map (\selected -> map (selected !!) columns) selectedRows | selectedRows <- choose size rows, columns <- choose size [0 .. length row - 1]]
        ]

referenceMatrix :: [[Rational]] -> [Int] -> [[Rational]]
referenceMatrix rows selected = replicate 5 1 : [rows !! index ++ [0] | index <- selected]

referenceClassification :: [[Rational]] -> Classification
referenceClassification matrix
    | denominator_ /= 0 = FullRank [determinant (replaceColumn column) / denominator_ | column <- [0 .. 3]]
    | matrixRank matrix > matrixRank coefficients = Inconsistent
    | otherwise = RankDeficient
  where
    coefficients = map (take 4) matrix
    denominator_ = determinant coefficients
    replaceColumn column = [take column row ++ [rhs] ++ drop (column + 1) row | (row, rhs) <- zip coefficients (map last matrix)]

dot :: [Rational] -> [Rational] -> Rational
dot coefficients mass = sum (zipWith (*) coefficients mass)

-- An outcome records how far inequality checking gets, independently of the
-- production ledger. Rank-deficient/inconsistent matrices visit no inequalities.
data Outcome = Outcome
    { classification :: Classification
    , inequalitiesVisited :: Int
    , admitted :: Bool
    }
    deriving (Eq, Show)

referenceOutcome :: [[Rational]] -> [Int] -> Outcome
referenceOutcome rows selected = case referenceClassification (referenceMatrix rows selected) of
    full@(FullRank mass) ->
        let slacks = map (`dot` mass) rows
            passed = length (takeWhile (>= 0) slacks)
         in Outcome full (min (length rows) (passed + 1)) (passed == length rows)
    other -> Outcome other 0 False

productionTuples :: Int -> [[Int]]
productionTuples bound = map (map fromIntegral) (go (initialTuple 3))
  where
    go selected = selected : maybe [] go (successorTuple (fromIntegral bound) selected)

checkFamily :: String -> [[Rational]] -> [ConstraintRow Owner Action] -> [[Rational]] -> IO [([Int], Outcome)]
checkFamily context expectedRows actualRows expectedVertices = do
    let tuples = choose 3 [0 .. length expectedRows - 1]
        reference = [(tuple, referenceOutcome expectedRows tuple) | tuple <- tuples]
    assert (context ++ ": complete ordered tuple family") (productionTuples (length actualRows) == tuples)
    actual <- mapM checkCandidate reference
    let referenceVertices = [mass | (_, Outcome (FullRank mass) _ True) <- reference]
        actualVertices = [mass | (FullRank mass, True) <- actual]
    assert (context ++ ": independent complete vertex set") (sort (nub referenceVertices) == sort expectedVertices)
    assert (context ++ ": production complete vertex set") (sort (nub actualVertices) == sort expectedVertices)
    assert (context ++ ": several active bases represent one vertex") (length actualVertices > length (nub actualVertices))
    pure reference
  where
    dimension = fromIntegral (11 + length actualRows)
    checkCandidate (tuple, expected) = do
        let matrix = private (candidateMatrix solveCaps dimension actualRows (map fromIntegral tuple))
            (eliminated, pivots) = private (eliminate solveCaps dimension 4 matrix)
            result = classify 4 pivots eliminated
            accepted = case result of
                FullRank mass -> private (verifyCandidate solveCaps dimension actualRows mass)
                _ -> False
        assert (context ++ ": active matrix " ++ show tuple) (matrix == referenceMatrix expectedRows tuple)
        assert (context ++ ": classification " ++ show tuple) (result == classification expected)
        assert (context ++ ": inequalities " ++ show tuple) (accepted == admitted expected)
        pure (result, accepted)

checkFixture :: (Fixture, SolveMode) -> IO ()
checkFixture (fixture, mode) = do
    let context = show (fixture, mode)
        expectedRows = transpose [labelledSlacks fixture mode (unit index) | index <- [0 .. 3]]
        actualRows = private (buildConstraints solveCaps mode (game fixture) 19)
    assert (context ++ ": production row labels") (map rowLabel actualRows == labels mode)
    assert (context ++ ": production coefficients equal labelled payoff evaluations") (map rowCoefficients actualRows == expectedRows)
    reference <- checkFamily context expectedRows actualRows (vertices fixture)
    mapM_
        ( \mass -> do
            assert (context ++ ": literal mass completeness and normalization") (length mass == 4 && all (>= 0) mass && sum mass == 1)
            assert (context ++ ": every vertex satisfies independent deviations") (all (>= 0) (labelledSlacks fixture mode mass))
        )
        (vertices fixture)
    when (fixture == Coordination) $ do
        -- Each appended inequality is redundant. Activating sum(p)>=0
        -- contradicts sum(p)=1, so inconsistency must win over deficiency.
        let redundant = [replicate 4 0, expectedRows !! 4, map (* 2) (expectedRows !! 4), replicate 4 1]
            extraRows = map (ConstraintRow (NonnegativityRow 0)) redundant
        outcomes <- checkFamily (context ++ " with redundant rows") (expectedRows ++ redundant) (actualRows ++ extraRows) (vertices fixture)
        assert (context ++ ": redundant bases include rank deficiency") (any ((== RankDeficient) . classification . snd) outcomes)
        assert (context ++ ": redundant bases include inconsistency") (any ((== Inconsistent) . classification . snd) outcomes)
    checkPublic fixture mode reference

-- From §6: 3 owner, 6 action and 5 profile spine inspections; product
-- validation + four payoffs + geometry at d=11. All other blocks use d=19.
-- Each prefix candidate needs tuple, matrix, four columns and classification;
-- a full-rank candidate adds normalization and its visited inequality prefix.
-- The sole eligible candidate adds shadow, device, checker and publication.
referenceLedger :: [Outcome] -> (Natural, Natural)
referenceLedger = foldl charge (16 * 14, 0) . referenceSchedule

type Block = (Public.CorrelationSolvePhase, Natural)

referenceSchedule :: [Outcome] -> [Block]
referenceSchedule prefix =
    replicate 5 (Public.CorrelationAdmission, 11)
        ++ [(Public.CorrelationConstraints, 11)]
        ++ replicate 40 (Public.CorrelationConstraints, 19)
        ++ concatMap candidate prefix
        ++ replicate 3 (Public.CorrelationVerification, 19)
        ++ [(Public.CorrelationPublication, 19)]
  where
    candidate outcome =
        (Public.CorrelationCombination, 19)
            : replicate 6 (Public.CorrelationElimination, 19)
            ++ case classification outcome of
                FullRank _ -> replicate (1 + inequalitiesVisited outcome) (Public.CorrelationInequalities, 19)
                _ -> []

charge :: (Natural, Natural) -> Block -> (Natural, Natural)
charge (work, materialization) (_, dimension) =
    let fields = 1024 * dimension ^ (6 :: Int)
     in (work + 65 * fields, materialization + fields)

-- Check both ends of each phase's occurrence and each final block. This
-- includes the last rejected-prefix transitions without rerunning every block.
-- An exact cap admits the named block, then fails at the next scheduled block;
-- the final publication boundary admits the complete solution.
checkPhaseBoundaries :: Fixture -> SolveMode -> [Outcome] -> IO ()
checkPhaseBoundaries fixture mode prefix =
    mapM_
        (\(index, resource, below) -> checkBoundary fixture mode schedule index resource below)
        [(index, resource, below) | index <- checkpoints, resource <- [Public.CorrelationWork, Public.CorrelationMaterialization], below <- [False, True]]
  where
    schedule = referenceSchedule prefix
    phases = nub (map fst schedule)
    positions phase = [index | (index, (actual, _)) <- zip [0 ..] schedule, actual == phase]
    endpoints phase = take 1 (positions phase) ++ take 1 (reverse (positions phase))
    checkpoints = nub (concatMap endpoints phases ++ [length schedule - 4 .. length schedule - 1])

checkBoundary :: Fixture -> SolveMode -> [Block] -> Int -> Public.CorrelationSolveResource -> Bool -> IO ()
checkBoundary fixture mode schedule index resource below = do
    let (work, materialization) = foldl charge (16 * 14, 0) (take (index + 1) schedule)
        required = case resource of
            Public.CorrelationWork -> work
            Public.CorrelationMaterialization -> materialization
            _ -> error "unsupported boundary resource"
        cap = required - if below then 1 else 0
        limits = case resource of
            Public.CorrelationWork -> Public.correlationSolveLimits (gameCaps{maximumGameWork = cap}) 8 ceiling_ ceiling_
            _ -> Public.correlationSolveLimits gameCaps 8 ceiling_ cap
        expected = case drop (index + if below then 0 else 1) schedule of
            (phase, _) : _ -> Left (Public.CorrelationSolveLimitExceeded phase resource cap (cap + 1))
            [] -> Right ()
    assert
        (show (fixture, mode, index, resource, below) ++ ": independently scheduled block boundary")
        (void (publicResult fixture mode limits) == expected)

-- Used by scratch instrumentation to distinguish the shadow, device, checker,
-- and publication bodies, which share phases and cannot be separated by an
-- error tag alone. The exact case is each body's positive reservation control.
checkFinalBoundary :: String -> String -> String -> String -> IO ()
checkFinalBoundary modeName blockName resourceName edge = do
    let mode = fromJust (lookup modeName [("ce", CorrelatedMode), ("cce", CoarseMode)])
        offset = fromJust (lookup blockName [("shadow", 4), ("device", 3), ("checker", 2), ("publication", 1)])
        resource = fromJust (lookup resourceName [("work", Public.CorrelationWork), ("materialization", Public.CorrelationMaterialization)])
        below = fromJust (lookup edge [("below", True), ("exact", False)])
        rows = transpose [labelledSlacks Coordination mode (unit index) | index <- [0 .. 3]]
        outcomes = map (referenceOutcome rows) (choose 3 [0 .. 7])
        (rejected, rest) = break admitted outcomes
        schedule = referenceSchedule (rejected ++ take 1 rest)
    checkBoundary Coordination mode schedule (length schedule - offset) resource below

publicResult :: Fixture -> SolveMode -> Public.CorrelationSolveLimits -> Either (Public.CorrelationSolveError Owner Action) ([Rational], Public.CorrelationSolveAccounting)
publicResult fixture mode limits = case mode of
    CorrelatedMode -> do
        solution <- Public.solveCorrelatedEquilibrium limits (game fixture)
        pure (map snd (Public.correlationEntries (Public.correlatedSolutionDevice solution)), Public.correlatedSolutionAccounting solution)
    CoarseMode -> do
        solution <- Public.solveCoarseCorrelatedEquilibrium limits (game fixture)
        pure (map snd (Public.correlationEntries (Public.coarseCorrelatedSolutionDevice solution)), Public.coarseCorrelatedSolutionAccounting solution)

checkPublic :: Fixture -> SolveMode -> [([Int], Outcome)] -> IO ()
checkPublic fixture mode reference = do
    let (rejected, successes) = break (admitted . snd) reference
        (selected, success) = case successes of
            first : _ -> first
            [] -> error "oracle found no feasible vertex"
        prefix = map snd rejected ++ [success]
        (work, materialization) = referenceLedger prefix
        limits = Public.correlationSolveLimits (gameCaps{maximumGameWork = work}) 8 (fromIntegral (length prefix)) materialization
        context = show (fixture, mode)
        count predicate = fromIntegral (length (filter (predicate . snd) rejected))
        expectedMass = case classification success of FullRank mass -> mass; _ -> error "oracle selected a rejected candidate"
    case publicResult fixture mode limits of
        Left problem -> ioError (userError (context ++ ": independently derived exact limits: " ++ show problem))
        Right (mass, account) -> do
            assert (context ++ ": deterministic public first witness") (mass == expectedMass)
            assert (context ++ ": public witness independently obeys every inequality") (length mass == 4 && all (>= 0) mass && sum mass == 1 && all (>= 0) (labelledSlacks fixture mode mass))
            assert (context ++ ": selected tuple") (Public.correlationSolveSelectedInequalities account == map fromIntegral selected)
            assert (context ++ ": candidate count") (Public.correlationSolveCandidates account == fromIntegral (length prefix))
            assert (context ++ ": rank-deficient count") (Public.correlationSolveRankDeficientCandidates account == count ((== RankDeficient) . classification))
            assert (context ++ ": inconsistent count") (Public.correlationSolveInconsistentCandidates account == count ((== Inconsistent) . classification))
            assert (context ++ ": inequality rejection count") (Public.correlationSolveInequalityRejectedCandidates account == count ((> 0) . inequalitiesVisited))
            assert (context ++ ": independent cumulative work") (Public.correlationSolveReservedWork account == work)
            assert (context ++ ": independent cumulative materialization") (Public.correlationSolveReservedMaterialization account == materialization)
    checkPhaseBoundaries fixture mode prefix
