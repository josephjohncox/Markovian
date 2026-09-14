{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

-- Source-bound D083 constructor/checker/shadow probes. The script compiles
-- these against scratch instrumented copies, at O0 and O2.
module Main (main) where

import Control.Monad (unless, when)
import D083Trace (Event (..), capture, captureStrict)
import Data.Ratio (denominator, numerator, (%))
import Markovian.Category.Finite.Object
import Markovian.Game.Correlated.Exact qualified as Public
import Markovian.Game.Correlated.Exact.Internal
import Markovian.Game.NormalForm.Exact
import Markovian.Game.Profile.Finite
import Numeric.Natural (Natural)

type Coordinate = (String, String)

data Fixture = Fixture
    { fixtureName :: String
    , layout :: [(String, [String])]
    , coordinates :: [Coordinate]
    , payoffOffset :: Rational
    , zeroPayoffs :: Bool
    }

baseFixture :: Fixture
baseFixture = Fixture "mixed cancellation" [("Row", ["A", "B", "C"]), ("Column", ["L", "R"])] [("A", "L"), ("A", "R"), ("B", "L"), ("B", "R"), ("C", "L"), ("C", "R")] 0 False

reorderedFixture :: Fixture
reorderedFixture = Fixture "reordered owners/actions" [("Column", ["R", "L"]), ("Row", ["C", "B", "A"])] [("C", "R"), ("B", "R"), ("A", "R"), ("C", "L"), ("B", "L"), ("A", "L")] 0 False

offsetFixture :: Fixture
offsetFixture = baseFixture{fixtureName = "large equal payoff offsets", payoffOffset = 2 ^ (20 :: Int)}

frozenFixture :: Fixture
frozenFixture = Fixture "frozen section 11.10" [("Row", ["A", "B"]), ("Column", ["L", "R"])] [("A", "L"), ("A", "R"), ("B", "L"), ("B", "R")] 0 True

ceiling_ :: Natural
ceiling_ = fromIntegral (maxBound :: Int) - 1

gameBounds :: Natural -> GameLimits
gameBounds bits = gameLimits 64 64 64 4096 ceiling_ bits ceiling_

solveBounds :: Natural -> CorrelationSolveLimits
solveBounds bits = correlationSolveLimits (gameBounds bits) ceiling_ ceiling_ ceiling_

orDie :: (Show problem) => Either problem value -> value
orDie = either (error . show) id

assert :: String -> Bool -> IO ()
assert label condition = unless condition (ioError (userError label))

coordinateMass :: Coordinate -> Rational
coordinateMass coordinate = orDie $ maybe (Left "unknown mass coordinate") Right $ lookup coordinate [(("A", "L"), 1 % 3), (("A", "R"), 1 % 6), (("B", "L"), 1 % 6), (("B", "R"), 1 % 3), (("C", "L"), 0), (("C", "R"), 0)]

payoff :: Fixture -> String -> Coordinate -> Rational
payoff fixture owner coordinate
    | zeroPayoffs fixture = 0
    | otherwise =
        let (row, column) = orDie $ maybe (Left "unknown payoff coordinate") Right $ lookup coordinate [(("A", "L"), (2, 1)), (("A", "R"), (0, 0)), (("B", "L"), (0, 0)), (("B", "R"), (4, 1)), (("C", "L"), (-1, 128)), (("C", "R"), (-1, -128))]
         in payoffOffset fixture + if owner == "Row" then row else column

fixtureGame :: Fixture -> ExactNormalGame String String
fixtureGame fixture =
    let owners = orDie (finiteObject (map fst (layout fixture)))
        carriers = [(owner, orDie (finiteObject actions)) | (owner, actions) <- layout fixture]
        product_ = orDie (ownedProduct (gameBounds 64) owners carriers)
        entry coordinate = (profileAt product_ coordinate, orDie (exactPlayerValues (gameBounds 64) owners [(owner, payoff fixture owner coordinate) | (owner, _) <- layout fixture]))
     in orDie (exactNormalGame (gameBounds 64) product_ (reverse (map entry (coordinates fixture))))

profileAt :: OwnedProduct String String -> Coordinate -> OwnedProfile String String
profileAt product_ (row, column) = orDie (ownedProfile product_ [("Row", row), ("Column", column)])

-- The oracle operates only on the literal coordinate/payoff tables above. It
-- does not use profileChoice, replaceChoice, normalPayoff, deviationLabels,
-- either checker, or any shadow helper to derive arithmetic or row order.
rowSpecs :: SolveMode -> Fixture -> [(String, Maybe String, String)]
rowSpecs mode fixture = case mode of
    CorrelatedMode -> [(owner, Just recommended, alternative) | (owner, actions) <- layout fixture, recommended <- actions, alternative <- actions, alternative /= recommended]
    CoarseMode -> [(owner, Nothing, alternative) | (owner, actions) <- layout fixture, alternative <- actions]

choiceAt :: String -> Coordinate -> String
choiceAt owner = if owner == "Row" then fst else snd

replaceAt :: String -> String -> Coordinate -> Coordinate
replaceAt owner alternative (row, column) = if owner == "Row" then (alternative, column) else (row, alternative)

additions :: String -> String -> [Rational] -> ([Event], Rational)
additions producer stage = go 0
  where
    go total [] = ([], total)
    go total (value : remaining) =
        let next = total + value
            (rest, final) = go next remaining
         in (Binary producer stage total value next : rest, final)

rowOracle :: String -> Fixture -> [Rational] -> (String, Maybe String, String) -> ([Event], Rational, Rational)
rowOracle producer fixture masses (owner, recommendation, alternative) =
    let matching = [(coordinate, probability) | (coordinate, probability) <- zip (coordinates fixture) masses, maybe True (== choiceAt owner coordinate) recommendation]
        (recommendationEvents, recommendationTotal) = case recommendation of
            Nothing -> ([], 0)
            Just _ -> additions producer "recommendation" (map snd matching)
        contributions total [] = ([], total)
        contributions total ((coordinate, probability) : remaining) =
            let incumbent = payoff fixture owner coordinate
                deviating = payoff fixture owner (replaceAt owner alternative coordinate)
                difference = incumbent - deviating
                weighted = probability * difference
                next = total + weighted
                (rest, final) = contributions next remaining
             in (Binary producer "difference" incumbent deviating difference : Binary producer "weighted" probability difference weighted : Binary producer "slack" total weighted next : rest, final)
        (slackEvents, slack) = contributions 0 matching
     in (recommendationEvents ++ slackEvents, recommendationTotal, slack)

oracleEvents :: String -> SolveMode -> Fixture -> [Rational] -> [Event]
oracleEvents producer mode fixture masses =
    let constructor = map (Mass producer "constructor.mass") masses ++ fst (additions producer "constructor.total" masses)
        validation _ [] = []
        validation total (value : remaining) = Mass producer "validation.mass" value : Binary producer "validation.total" total value (total + value) : validation (total + value) remaining
        body = concat [events_ | specification <- rowSpecs mode fixture, let (events_, _, _) = rowOracle producer fixture masses specification]
     in constructor ++ validation 0 masses ++ body

shadowOracle :: SolveMode -> Fixture -> [Rational] -> [ShadowRow String String]
shadowOracle mode fixture masses =
    [ ShadowRow label recommendation slack
    | specification@(owner, recommended, alternative) <- rowSpecs mode fixture
    , let (_, recommendation, slack) = rowOracle "unused" fixture masses specification
          label = maybe (CoarseRow owner alternative) (\action -> ObedienceRow owner action alternative) recommended
    ]

ceOracle :: Fixture -> [Rational] -> Public.CorrelatedEquilibriumReport String String
ceOracle fixture masses =
    let checks = [Public.ObedienceCheck owner recommended alternative recommendation (if recommendation == 0 then Public.NullRecommendation else Public.PositiveRecommendation) slack | specification@(owner, Just recommended, alternative) <- rowSpecs CorrelatedMode fixture, let (_, recommendation, slack) = rowOracle "unused" fixture masses specification]
        profiles = fromIntegral (length masses)
        count = fromIntegral (length checks)
     in Public.CorrelatedEquilibriumReport (all ((>= 0) . Public.obedienceSlack) checks) profiles count (4 * count * profiles) checks

cceOracle :: Fixture -> [Rational] -> Public.CoarseCorrelatedEquilibriumReport String String
cceOracle fixture masses =
    let checks = [Public.CoarseDeviationCheck owner alternative slack | specification@(owner, _, alternative) <- rowSpecs CoarseMode fixture, let (_, _, slack) = rowOracle "unused" fixture masses specification]
        profiles = fromIntegral (length masses)
        count = fromIntegral (length checks)
     in Public.CoarseCorrelatedEquilibriumReport (all ((>= 0) . Public.coarseDeviationSlack) checks) profiles count (4 * count * profiles) checks

producerOf :: Event -> String
producerOf (Mass producer _ _) = producer
producerOf (Binary producer _ _ _ _) = producer

resultOf :: Event -> Rational
resultOf (Mass _ _ value) = value
resultOf (Binary _ _ _ _ value) = value

size :: Rational -> Natural
size value = bits (abs (numerator value)) + bits (denominator value)
  where
    bits integer = if integer < 2 then 1 else 1 + bits (integer `quot` 2)

peak :: [Event] -> Natural
peak = maximum . (2 :) . map (size . resultOf)

checkTraces :: SolveMode -> Fixture -> [Rational] -> [Event] -> IO Natural
checkTraces mode fixture masses observed = do
    let label = fixtureName fixture ++ " " ++ show mode ++ " ordered constructor/checker/shadow Rational trace"
        actual = filter ((== "actual") . producerOf) observed
        shadow = filter ((== "shadow") . producerOf) observed
    assert (label ++ " (actual operands/results)") (actual == oracleEvents "actual" mode fixture masses)
    assert (label ++ " (shadow operands/results)") (shadow == oracleEvents "shadow" mode fixture masses)
    assert (label ++ " (measured peaks)") (peak actual == peak shadow)
    pure (peak actual)

prepare :: CorrelationSolveLimits -> SolveMode -> ExactNormalGame String String -> [Rational] -> Solve String ()
prepare limits mode game masses = do
    _ <- admitGame limits game
    reserveBlock limits CorrelationConstraints 16
    _ <- admitGeometry limits mode 4 [2, 2]
    observeRational limits CorrelationConstraints 0
    observeRational limits CorrelationConstraints 1
    constraints <- buildConstraints limits mode game 32
    mapM_ (observeRational limits CorrelationVerification) masses
    accepted <- verifyCandidate limits 32 constraints masses
    unless accepted (abort (SolveInvariantFault CorrelationCandidateShapeInvariant))

shadowThread :: Natural -> SolveMode -> Fixture -> [Rational] -> Solve String [ShadowRow String String]
shadowThread bits mode fixture masses = do
    let game = fixtureGame fixture
        limits = solveBounds bits
        entries = zip (map (profileAt (normalGameProduct game)) (coordinates fixture)) masses
        labels = map shadowLabel (shadowOracle mode fixture masses)
    if zeroPayoffs fixture
        then prepare limits mode game masses
        else do
            _ <- admitGame limits game
            pure ()
    case mode of
        CorrelatedMode -> correlatedShadow limits game entries labels
        CoarseMode -> coarseShadow limits game entries labels

testPrivate :: Natural -> SolveMode -> Fixture -> [Rational] -> IO ()
testPrivate bits mode fixture masses = do
    let game = fixtureGame fixture
        product_ = normalGameProduct game
        canonical = zip (map (profileAt product_) (coordinates fixture)) masses
        result = do
            (rows, account) <- either (Left . show) Right (runSolve (shadowThread bits mode fixture masses))
            device <- either (Left . show) Right (Public.exactCorrelationDevice (gameBounds bits) product_ (reverse canonical))
            actualReport <- case mode of
                CorrelatedMode -> fmap Left (either (Left . show) Right (Public.checkCorrelatedEquilibrium (gameBounds bits) game device))
                CoarseMode -> fmap Right (either (Left . show) Right (Public.checkCoarseCorrelatedEquilibrium (gameBounds bits) game device))
            pure (rows, account, device, actualReport)
    (evaluated, observed) <- capture result
    let (rows, account, device, report) = orDie evaluated
    assert "every private shadow row field" (rows == shadowOracle mode fixture masses)
    assert "literal constructor canonicalizes reordered supplied entries" (Public.correlationEntries device == canonical)
    assert "every actual CE/CCE report field" (report == case mode of CorrelatedMode -> Left (ceOracle fixture masses); CoarseMode -> Right (cceOracle fixture masses))
    measured <- checkTraces mode fixture masses observed
    assert "measured private checker peak equals checker-covered account" (measured == correlationSolveCheckerCoveredRationalBits' account)
    assert "private checker-covered peak is included in historical observation" (measured <= correlationSolveObservedRationalBits' account)
    unless (zeroPayoffs fixture) $ assert "discarded zero-mass difference reaches ten bits" (measured == 10)
    when (payoffOffset fixture > 0) $
        assert "input payoff operands belong to H without inflating V" (correlationSolveObservedRationalBits' account > measured)
    when (zeroPayoffs fixture) $ do
        assert "frozen shadow historical Rational peak" (correlationSolveObservedRationalBits' account == bits)
        assert "frozen preparation and production-shadow observation trace" (filter ((== "observe") . producerOf) observed == preparationOracle masses ++ map observation (oracleEvents "shadow" mode fixture masses))

observation :: Event -> Event
observation = Mass "observe" (show CorrelationVerification) . resultOf

preparationOracle :: [Rational] -> [Event]
preparationOracle masses =
    let observed phase = map (Mass "observe" (show phase))
        identity = [[if row == column then 1 else 0 | column <- [0 .. 3 :: Int]] | row <- [0 .. 3 :: Int]]
        coefficients = identity ++ replicate 4 (replicate 4 0)
        inequality row = go 0 (zip row masses)
        go _ [] = []
        go total ((coefficient, value) : remaining) = let weighted = coefficient * value; next = total + weighted in weighted : next : go next remaining
     in observed CorrelationAdmission (replicate 8 0)
            ++ observed CorrelationConstraints (0 : 1 : concat coefficients)
            ++ observed CorrelationVerification masses
            ++ observed CorrelationInequalities (map resultOf (fst (additions "unused" "unused" masses)) ++ concatMap inequality coefficients)

testFrozenPreparation :: SolveMode -> IO ()
testFrozenPreparation mode = do
    let masses = [1 % 3, 1 % 6, 1 % 5, 3 % 10]
    (result, observed) <- capture (runSolve (prepare (solveBounds 7) mode (fixtureGame frozenFixture) masses))
    let (_, account) = orDie result
    assert "frozen preparation has exactly the independently ordered observations" (observed == preparationOracle masses)
    assert "frozen preparation independently peaks at seven bits" (peak observed == 7)
    assert "frozen preparation account ends at seven historical bits" (correlationSolveObservedRationalBits' account == 7)

testFrozenFailure :: IO ()
testFrozenFailure = do
    let masses = [1 % 3, 1 % 6, 1 % 5, 3 % 10]
        allShadow = oracleEvents "shadow" CorrelatedMode frozenFixture masses
        beforeFailure = takeWhile ((<= 7) . size . resultOf) allShadow
        failing = take 1 (drop (length beforeFailure) allShadow)
    (result, observed) <- capture (runSolve (shadowThread 7 CorrelatedMode frozenFixture masses))
    assert "frozen CE seven-bit failure is 7/8 verification limit" (result == Left (SolveLimitFault CorrelationVerification CorrelationRationalBits 7 8))
    assert "frozen CE failure occurs at recommendation 8/15" (map resultOf failing == [8 % 15])
    assert "frozen CE failure stops the ordered shadow trace immediately" (filter ((== "shadow") . producerOf) observed == beforeFailure ++ failing)
    assert "frozen CE failed observation keeps the preparation prefix" (filter ((== "observe") . producerOf) observed == preparationOracle masses ++ map observation (beforeFailure ++ failing))

testPublic :: SolveMode -> Fixture -> IO ()
testPublic mode fixture = do
    let game = fixtureGame fixture
        limits = Public.correlationSolveLimits (gameBounds 64) ceiling_ ceiling_ ceiling_
        result = case mode of
            CorrelatedMode -> do
                solution <- Public.solveCorrelatedEquilibrium limits game
                pure (Public.correlatedSolutionDevice solution, Left (Public.correlatedSolutionCheck solution), Public.correlatedSolutionAccounting solution)
            CoarseMode -> do
                solution <- Public.solveCoarseCorrelatedEquilibrium limits game
                pure (Public.coarseCorrelatedSolutionDevice solution, Right (Public.coarseCorrelatedSolutionCheck solution), Public.coarseCorrelatedSolutionAccounting solution)
    (evaluated, observed) <- captureStrict result
    let (device, report, account) = orDie evaluated
        masses = map snd (Public.correlationEntries device)
        copied = filter ((== "copy") . producerOf) observed
        observationsAfterCopies = [(left, right) | (left, right) <- zip observed (drop 1 observed), producerOf left == "copy"]
    assert "RHS candidate copies are observed before normalization" (length copied >= length masses && map resultOf (drop (length copied - length masses) copied) == masses)
    assert "each copied RHS is observed in the reserved classification phase" (all (\(left, right) -> right == Mass "observe" (show CorrelationElimination) (resultOf left)) observationsAfterCopies)
    measured <- checkTraces mode fixture masses observed
    assert "every public solution report field" (report == case mode of CorrelatedMode -> Left (ceOracle fixture masses); CoarseMode -> Right (cceOracle fixture masses))
    assert "same-run measured peak equals public checker-covered account" (measured == Public.correlationSolveCheckerCoveredRationalBits account)
    assert "public checker-covered peak remains below historical observation" (measured <= Public.correlationSolveObservedRationalBits account)
    assert "public witness still evaluates discarded ten-bit differences" (measured == 10)
    when (payoffOffset fixture > 0) $
        assert "public V excludes already admitted large payoff operands" (Public.correlationSolveObservedRationalBits account > measured)

-- Inject through the existing private pipeline callback. Public reports have
-- constructors; publication must check every field before retaining one.
testPublication :: IO ()
testPublication = do
    let game = fixtureGame frozenFixture
        bounds = gameBounds 64
        disagreement = Public.CorrelationSolveInvariantFailure Public.CorrelationCheckerDisagreement
        compareCE shadow device = Public.agreesWithCorrelatedShadow bounds game device shadow
        compareCCE shadow device = Public.agreesWithCoarseShadow bounds device shadow
        first change values = case values of
            [] -> error "publication fixture needs a report row"
            value : remaining -> change value : remaining
        ceRow change report = report{Public.correlatedObedienceChecks = first change (Public.correlatedObedienceChecks report)}
        cceRow change report = report{Public.coarseDeviationChecks = first change (Public.coarseDeviationChecks report)}
        ceChanges =
            [ ("CE satisfied", \r -> r{Public.correlatedEquilibriumSatisfied = False})
            , ("CE profiles", \r -> r{Public.correlatedProfileCount = 0})
            , ("CE count", \r -> r{Public.correlatedObedienceCount = 0})
            , ("CE work", \r -> r{Public.correlatedArithmeticWork = 0})
            , ("CE short rows", \r -> r{Public.correlatedObedienceChecks = drop 1 (Public.correlatedObedienceChecks r)})
            , ("CE extra rows", \r -> r{Public.correlatedObedienceChecks = Public.correlatedObedienceChecks r ++ Public.correlatedObedienceChecks r})
            , ("CE owner", ceRow (\r -> r{Public.recommendedFor = "absent"}))
            , ("CE recommended", ceRow (\r -> r{Public.recommendedAction = Public.alternativeAction r}))
            , ("CE alternative", ceRow (\r -> r{Public.alternativeAction = Public.recommendedAction r}))
            , ("CE mass", ceRow (\r -> r{Public.recommendationMass = 1}))
            , ("CE status", ceRow (\r -> r{Public.recommendationStatus = Public.PositiveRecommendation}))
            , ("CE slack", ceRow (\r -> r{Public.obedienceSlack = 1}))
            ]
        cceChanges =
            [ ("CCE satisfied", \r -> r{Public.coarseCorrelatedEquilibriumSatisfied = False})
            , ("CCE profiles", \r -> r{Public.coarseCorrelatedProfileCount = 0})
            , ("CCE count", \r -> r{Public.coarseDeviationCount = 0})
            , ("CCE work", \r -> r{Public.coarseArithmeticWork = 0})
            , ("CCE short rows", \r -> r{Public.coarseDeviationChecks = drop 1 (Public.coarseDeviationChecks r)})
            , ("CCE extra rows", \r -> r{Public.coarseDeviationChecks = Public.coarseDeviationChecks r ++ Public.coarseDeviationChecks r})
            , ("CCE owner", cceRow (\r -> r{Public.coarseDeviationOwner = "absent"}))
            , ("CCE alternative", cceRow (\r -> r{Public.coarseAlternativeAction = "absent"}))
            , ("CCE slack", cceRow (\r -> r{Public.coarseDeviationSlack = 1}))
            ]
        check label mode checker compareReport expected = do
            (result, _) <- capture (runSolve (Public.searchWitness (solveBounds 64) mode game checker compareReport))
            case result of
                Right (Left problem, account) -> do
                    assert ("publication rejects " ++ label) (problem == expected)
                    assert "publication failure cannot try another candidate or publish its tuple" (correlationSolveCandidates' account == 1 && null (correlationSolveSelectedInequalities' account))
                other -> ioError (userError ("publication rejects " ++ label ++ ": " ++ show other))
        checkerError = Public.CorrelationSolveCheckerError Public.CorrelatedInternalLayoutMismatch
    mapM_ (\(label, change) -> check label CorrelatedMode (either (Left . Public.CorrelationSolveCheckerError) (Right . change) . Public.checkCorrelatedEquilibrium bounds game) compareCE disagreement) ceChanges
    mapM_ (\(label, change) -> check label CoarseMode (either (Left . Public.CorrelationSolveCheckerError) (Right . change) . Public.checkCoarseCorrelatedEquilibrium bounds game) compareCCE disagreement) cceChanges
    check "CE checker error" CorrelatedMode (const (Left checkerError)) compareCE checkerError
    check "CCE checker error" CoarseMode (const (Left checkerError)) compareCCE checkerError

main :: IO ()
main = do
    let modes = [CorrelatedMode, CoarseMode]
        fixtures = [baseFixture, reorderedFixture, offsetFixture]
    mapM_ (\fixture -> mapM_ (\mode -> testPrivate 64 mode fixture (map coordinateMass (coordinates fixture))) modes) fixtures
    mapM_ (\fixture -> mapM_ (`testPublic` fixture) modes) fixtures
    mapM_ testFrozenPreparation modes
    testPrivate 8 CorrelatedMode frozenFixture [1 % 3, 1 % 6, 1 % 5, 3 % 10]
    testPrivate 7 CoarseMode frozenFixture [1 % 3, 1 % 6, 1 % 5, 3 % 10]
    testFrozenFailure
    testPublication
    putStrLn "PASS: D083 ordered constructor/checker/shadow Rational traces, complete reports, and measured checker-covered peaks"
