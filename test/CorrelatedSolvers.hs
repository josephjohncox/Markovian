{- | D083 public CE and CCE first-witness solver fixtures.

These cover the frozen public surface: the tiny vertex games, the CE/CCE
separating game's public witnesses, and the resource-boundary controls.  The
mandated private controls -- the production builder row comparison, the
linear-system controls, the arithmetic-only geometry gates, and the source-bound
shadow injection -- live in the standalone private probe
@test/CorrelatedSolverPrivate.hs@, because they must call the private core
directly without exposing it publicly.

The independently labelled evaluator here shares no production constraint
builder, elimination, or replacement function with the solver.  It reads
labelled payoff tables and labelled joint masses and constructs changed label
tuples directly.
-}
module CorrelatedSolvers (runCorrelatedSolverTests) where

import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Category.Finite.Object
import Markovian.Game.Correlated.Exact
import Markovian.Game.NormalForm.Exact
import Markovian.Game.Profile.Finite
import Numeric.Natural (Natural)

runCorrelatedSolverTests :: (String -> IO () -> IO ()) -> IO ()
runCorrelatedSolverTests run = do
    run "a singleton game solves to its only distribution in both modes" testSingleton
    run "a strict optimum rejects the inactive first basis before succeeding" testStrictOptimum
    run "the unique non-Dirac witness is all-quarter at candidate 53" testUniqueNonDirac
    run "a zero-payoff game returns the initial-basis Dirac witness" testZeroPayoff
    run "a degenerate coordination polytope returns its first-success vertex" testCoordination
    run "public CE and CCE witnesses coincide on the separating game" testSeparatingPublic
    run "solutions retain the game handle, device, actual report and accounting" testSolutionFields
    run "an independent labelled evaluator agrees with every returned witness" testIndependentEvaluator
    run "zero and one-below caps fail atomically with cap+1 and no solution" testResourceBoundaries
    run "materialization is selected before work in the same block" testMaterializationBeforeWork
    run "a zero CE row count still validates the device and calls the real checker" testZeroRowChecker
    run "rejected candidate reservations and bits remain in the final accounting" testRejectedHistoryRetained

-- Limits sized for the tiny fixtures.  Work and materialization are the
-- representational ceiling; B is generous but finite.

ceiling_ :: Natural
ceiling_ = fromIntegral (maxBound :: Int) - 1

tinyLimits :: GameLimits
tinyLimits = gameLimits 64 64 64 4096 ceiling_ 64 ceiling_

tinySolve :: CorrelationSolveLimits
tinySolve = correlationSolveLimits tinyLimits ceiling_ ceiling_ ceiling_

assert :: String -> Bool -> IO ()
assert message condition = if condition then pure () else ioError (userError message)

-- | Build a labelled game.  Construction failures are test bugs.
buildGame ::
    [(String, [String])] ->
    [([(String, String)], [(String, Rational)])] ->
    ExactNormalGame String String
buildGame rows table =
    let owners = orDie (finiteObject (map fst rows))
        carriers = [(owner, orDie (finiteObject actions)) | (owner, actions) <- rows]
        product_ = orDie (ownedProduct tinyLimits owners carriers)
        entry (assignment, payoffs) =
            ( orDie (ownedProfile product_ assignment)
            , orDie (exactPlayerValues tinyLimits owners payoffs)
            )
     in orDie (exactNormalGame tinyLimits product_ (map entry table))

orDie :: (Show problem) => Either problem value -> value
orDie = either (error . show) id

masses :: ExactCorrelationDevice String String -> [Rational]
masses = map snd . correlationEntries

profileLabels :: ExactNormalGame String String -> [[(String, String)]]
profileLabels game =
    [ ownedProfileEntries profile
    | profile <- NonEmpty.toList (finiteObjectValues (ownedProfiles (normalGameProduct game)))
    ]

-- Fixtures.

singletonGame :: ExactNormalGame String String
singletonGame = buildGame [("R", ["A"])] [([("R", "A")], [("R", 0)])]

-- | §11.2 one owner, actions @[A,B]@, payoffs @(1,0)@.
strictOptimumGame :: ExactNormalGame String String
strictOptimumGame =
    buildGame
        [("R", ["A", "B"])]
        [([("R", "A")], [("R", 1)]), ([("R", "B")], [("R", 0)])]

-- | §11.3 two owners with payoffs @(1,-1,-1,1)@ and their negatives.
uniqueNonDiracGame :: ExactNormalGame String String
uniqueNonDiracGame =
    buildGame
        [("R", ["A", "B"]), ("C", ["A", "B"])]
        [ ([("R", "A"), ("C", "A")], [("R", 1), ("C", -1)])
        , ([("R", "A"), ("C", "B")], [("R", -1), ("C", 1)])
        , ([("R", "B"), ("C", "A")], [("R", -1), ("C", 1)])
        , ([("R", "B"), ("C", "B")], [("R", 1), ("C", -1)])
        ]

-- | §11.4 the same layout with all eight payoffs zero.
zeroPayoffGame :: ExactNormalGame String String
zeroPayoffGame =
    buildGame
        [("R", ["A", "B"]), ("C", ["A", "B"])]
        [([("R", a), ("C", c)], [("R", 0), ("C", 0)]) | a <- ["A", "B"], c <- ["A", "B"]]

-- | §11.5 payoff one on @AA@ and @BB@, zero otherwise, for both owners.
coordinationGame :: ExactNormalGame String String
coordinationGame =
    buildGame
        [("R", ["A", "B"]), ("C", ["A", "B"])]
        [ ([("R", "A"), ("C", "A")], [("R", 1), ("C", 1)])
        , ([("R", "A"), ("C", "B")], [("R", 0), ("C", 0)])
        , ([("R", "B"), ("C", "A")], [("R", 0), ("C", 0)])
        , ([("R", "B"), ("C", "B")], [("R", 1), ("C", 1)])
        ]

{- | §11.6 the CE/CCE separating game.  Row payoffs in profile order
@(AL,AR,BL,BR,CL,CR)@ are @(0,0,1,-1,0,0)@; all Column payoffs are zero.
-}
separatingGame :: ExactNormalGame String String
separatingGame =
    buildGame
        [("Row", ["A", "B", "C"]), ("Col", ["L", "R"])]
        [ ([("Row", "A"), ("Col", "L")], [("Row", 0), ("Col", 0)])
        , ([("Row", "A"), ("Col", "R")], [("Row", 0), ("Col", 0)])
        , ([("Row", "B"), ("Col", "L")], [("Row", 1), ("Col", 0)])
        , ([("Row", "B"), ("Col", "R")], [("Row", -1), ("Col", 0)])
        , ([("Row", "C"), ("Col", "L")], [("Row", 0), ("Col", 0)])
        , ([("Row", "C"), ("Col", "R")], [("Row", 0), ("Col", 0)])
        ]

-- §11.1

testSingleton :: IO ()
testSingleton = do
    case solveCorrelatedEquilibrium tinySolve singletonGame of
        Left problem -> ioError (userError ("singleton CE: " ++ show problem))
        Right solution -> do
            assert "singleton CE mass" (masses (correlatedSolutionDevice solution) == [1])
            -- The empty selected tuple, not an empty owned profile.
            assert
                "singleton CE empty tuple"
                (null (correlationSolveSelectedInequalities (correlatedSolutionAccounting solution)))
            assert
                "singleton CE has no obedience rows"
                (correlatedObedienceCount (correlatedSolutionCheck solution) == 0)
            assert
                "singleton CE satisfied"
                (correlatedEquilibriumSatisfied (correlatedSolutionCheck solution))
    case solveCoarseCorrelatedEquilibrium tinySolve singletonGame of
        Left problem -> ioError (userError ("singleton CCE: " ++ show problem))
        Right solution -> do
            assert "singleton CCE mass" (masses (coarseCorrelatedSolutionDevice solution) == [1])
            -- One row, which is identically zero.
            assert
                "singleton CCE one zero row"
                ( coarseDeviationCount (coarseCorrelatedSolutionCheck solution) == 1
                    && map coarseDeviationSlack (coarseDeviationChecks (coarseCorrelatedSolutionCheck solution)) == [0]
                )

-- §11.2

testStrictOptimum :: IO ()
testStrictOptimum = do
    solution <- case solveCorrelatedEquilibrium tinySolve strictOptimumGame of
        Left problem -> ioError (userError ("strict optimum CE: " ++ show problem))
        Right value -> pure value
    let account = correlatedSolutionAccounting solution
    assert "strict optimum witness" (masses (correlatedSolutionDevice solution) == [1, 0])
    -- The first tuple [0] sets p_A = 0 and its candidate (0,1) fails a
    -- deviation inequality; the next tuple [1] yields (1,0).
    assert "strict optimum tuple" (correlationSolveSelectedInequalities account == [1])
    assert "strict optimum candidates" (correlationSolveCandidates account == 2)
    assert "strict optimum inactive rejection" (correlationSolveInequalityRejectedCandidates account == 1)
    -- CE's unused recommendation is explicitly null.
    assert
        "strict optimum null recommendation"
        ( any
            (\check -> recommendationStatus check == NullRecommendation)
            (correlatedObedienceChecks (correlatedSolutionCheck solution))
        )
    coarse <- case solveCoarseCorrelatedEquilibrium tinySolve strictOptimumGame of
        Left problem -> ioError (userError ("strict optimum CCE: " ++ show problem))
        Right value -> pure value
    assert "strict optimum CCE witness" (masses (coarseCorrelatedSolutionDevice coarse) == [1, 0])

-- §11.3

testUniqueNonDirac :: IO ()
testUniqueNonDirac = do
    solution <- case solveCorrelatedEquilibrium tinySolve uniqueNonDiracGame of
        Left problem -> ioError (userError ("unique non-Dirac CE: " ++ show problem))
        Right value -> pure value
    let account = correlatedSolutionAccounting solution
    assert
        "unique non-Dirac CE masses"
        (masses (correlatedSolutionDevice solution) == [1 / 4, 1 / 4, 1 / 4, 1 / 4])
    -- A feasible basis cannot include nonnegativity because every feasible
    -- mass is positive; the first three deviation equalities force equality.
    assert "unique non-Dirac CE tuple" (correlationSolveSelectedInequalities account == [4, 5, 6])
    assert "unique non-Dirac CE candidate 53" (correlationSolveCandidates account == 53)
    coarse <- case solveCoarseCorrelatedEquilibrium tinySolve uniqueNonDiracGame of
        Left problem -> ioError (userError ("unique non-Dirac CCE: " ++ show problem))
        Right value -> pure value
    let coarseAccount = coarseCorrelatedSolutionAccounting coarse
    assert
        "unique non-Dirac CCE masses"
        (masses (coarseCorrelatedSolutionDevice coarse) == [1 / 4, 1 / 4, 1 / 4, 1 / 4])
    assert "unique non-Dirac CCE tuple" (correlationSolveSelectedInequalities coarseAccount == [4, 5, 6])
    assert "unique non-Dirac CCE candidate 53" (correlationSolveCandidates coarseAccount == 53)

-- §11.4

testZeroPayoff :: IO ()
testZeroPayoff = do
    solution <- case solveCorrelatedEquilibrium tinySolve zeroPayoffGame of
        Left problem -> ioError (userError ("zero payoff CE: " ++ show problem))
        Right value -> pure value
    let account = correlatedSolutionAccounting solution
    -- The initial tuple [0,1,2] succeeds with e_3.  The returned point does
    -- not maximize p_0, whose maximum is at e_0, so first active-set success
    -- makes no global witness-optimization claim.
    assert "zero payoff CE witness" (masses (correlatedSolutionDevice solution) == [0, 0, 0, 1])
    assert "zero payoff CE initial tuple" (correlationSolveSelectedInequalities account == [0, 1, 2])
    assert "zero payoff CE first candidate" (correlationSolveCandidates account == 1)
    -- Zero and redundant deviation rows are preserved, with null recommendations.
    let report = correlatedSolutionCheck solution
    assert "zero payoff CE retains all rows" (correlatedObedienceCount report == 4)
    assert
        "zero payoff CE has null recommendations"
        (any (\check -> recommendationStatus check == NullRecommendation) (correlatedObedienceChecks report))
    coarse <- case solveCoarseCorrelatedEquilibrium tinySolve zeroPayoffGame of
        Left problem -> ioError (userError ("zero payoff CCE: " ++ show problem))
        Right value -> pure value
    assert "zero payoff CCE witness" (masses (coarseCorrelatedSolutionDevice coarse) == [0, 0, 0, 1])
    assert "zero payoff CCE retains rows" (coarseDeviationCount (coarseCorrelatedSolutionCheck coarse) == 4)

-- §11.5

testCoordination :: IO ()
testCoordination = do
    solution <- case solveCorrelatedEquilibrium tinySolve coordinationGame of
        Left problem -> ioError (userError ("coordination CE: " ++ show problem))
        Right value -> pure value
    assert "coordination CE first success" (masses (correlatedSolutionDevice solution) == [0, 0, 0, 1])
    assert
        "coordination CE initial tuple"
        (correlationSolveSelectedInequalities (correlatedSolutionAccounting solution) == [0, 1, 2])
    coarse <- case solveCoarseCorrelatedEquilibrium tinySolve coordinationGame of
        Left problem -> ioError (userError ("coordination CCE: " ++ show problem))
        Right value -> pure value
    assert "coordination CCE first success" (masses (coarseCorrelatedSolutionDevice coarse) == [0, 0, 0, 1])
    -- The complete tiny vertex set, verified by the independent evaluator
    -- without requiring the public operation to continue after success.
    let vertices =
            [ [1, 0, 0, 0]
            , [0, 0, 0, 1]
            , [1 / 3, 1 / 3, 0, 1 / 3]
            , [1 / 3, 0, 1 / 3, 1 / 3]
            , [1 / 4, 1 / 4, 1 / 4, 1 / 4]
            ]
    mapM_
        ( \vertex ->
            assert
                ("coordination vertex is CE: " ++ show vertex)
                (evaluateCorrelated coordinationGame vertex)
        )
        vertices

-- §11.6 public half.  The mandatory production-builder row comparison is in
-- the private probe.

testSeparatingPublic :: IO ()
testSeparatingPublic = do
    solution <- case solveCorrelatedEquilibrium tinySolve separatingGame of
        Left problem -> ioError (userError ("separating CE: " ++ show problem))
        Right value -> pure value
    coarse <- case solveCoarseCorrelatedEquilibrium tinySolve separatingGame of
        Left problem -> ioError (userError ("separating CCE: " ++ show problem))
        Right value -> pure value
    -- Both public first-success witnesses are e_6, because the initial
    -- nonnegativity basis succeeds.  Checking only the returned witnesses
    -- therefore cannot distinguish the two feasible sets.
    assert "separating CE witness" (masses (correlatedSolutionDevice solution) == [0, 0, 0, 0, 0, 1])
    assert "separating CCE witness" (masses (coarseCorrelatedSolutionDevice coarse) == [0, 0, 0, 0, 0, 1])
    -- The separating mass (e_1 + e_6)/2 is CCE but not CE, established with
    -- the independent labelled evaluator.
    let separatingMass = [1 / 2, 0, 0, 0, 0, 1 / 2]
    assert
        "separating mass is rejected by CE"
        (not (evaluateCorrelated separatingGame separatingMass))
    assert
        "separating mass is admitted by CCE"
        (evaluateCoarse separatingGame separatingMass)
    -- Column's zero rows and all null CE recommendations are preserved.
    let report = correlatedSolutionCheck solution
    assert "separating CE row count" (correlatedObedienceCount report == 8)
    assert "separating CCE row count" (coarseDeviationCount (coarseCorrelatedSolutionCheck coarse) == 5)

testSolutionFields :: IO ()
testSolutionFields = do
    solution <- case solveCorrelatedEquilibrium tinySolve uniqueNonDiracGame of
        Left problem -> ioError (userError ("solution fields: " ++ show problem))
        Right value -> pure value
    -- The retained game handle is the original, and the retained report is the
    -- actual checker's report on the retained device.
    assert "retained game handle" (correlatedSolutionGame solution == uniqueNonDiracGame)
    let device = correlatedSolutionDevice solution
        report = correlatedSolutionCheck solution
    assert
        "retained report is the real checker's report"
        (checkCorrelatedEquilibrium tinyLimits uniqueNonDiracGame device == Right report)
    assert "retained report is satisfied" (correlatedEquilibriumSatisfied report)
    -- Games with the same layout but different payoffs retain their own handle.
    other <- case solveCorrelatedEquilibrium tinySolve zeroPayoffGame of
        Left problem -> ioError (userError ("second solution: " ++ show problem))
        Right value -> pure value
    assert
        "distinct games retain distinct handles"
        (correlatedSolutionGame other /= correlatedSolutionGame solution)

{- | The independent labelled evaluator agrees with every returned witness.  It
constructs changed label tuples directly and shares no production function.
-}
testIndependentEvaluator :: IO ()
testIndependentEvaluator = do
    mapM_
        ( \(name, game) -> do
            case solveCorrelatedEquilibrium tinySolve game of
                Left problem -> ioError (userError (name ++ " CE: " ++ show problem))
                Right solution ->
                    assert
                        (name ++ " CE witness agrees with the independent evaluator")
                        (evaluateCorrelated game (masses (correlatedSolutionDevice solution)))
            case solveCoarseCorrelatedEquilibrium tinySolve game of
                Left problem -> ioError (userError (name ++ " CCE: " ++ show problem))
                Right solution ->
                    assert
                        (name ++ " CCE witness agrees with the independent evaluator")
                        (evaluateCoarse game (masses (coarseCorrelatedSolutionDevice solution)))
        )
        [ ("singleton", singletonGame)
        , ("strict optimum", strictOptimumGame)
        , ("unique non-Dirac", uniqueNonDiracGame)
        , ("zero payoff", zeroPayoffGame)
        , ("coordination", coordinationGame)
        , ("separating", separatingGame)
        ]

-- §11.8 resource boundaries.

testResourceBoundaries :: IO ()
testResourceBoundaries = do
    -- A zero candidate cap fails on the initial tuple, not at constructor time.
    let zeroCandidates = correlationSolveLimits tinyLimits ceiling_ 0 ceiling_
    case solveCorrelatedEquilibrium zeroCandidates zeroPayoffGame of
        Left (CorrelationSolveLimitExceeded phase resource cap required) ->
            assert
                "zero candidate cap fails in combination with cap+1"
                (phase == CorrelationCombination && resource == CorrelationCandidateCount && cap == 0 && required == 1)
        other -> ioError (userError ("zero candidate cap: " ++ describe other))
    -- A zero inequality cap fails in constraints.
    let zeroInequalities = correlationSolveLimits tinyLimits 0 ceiling_ ceiling_
    case solveCorrelatedEquilibrium zeroInequalities zeroPayoffGame of
        Left (CorrelationSolveLimitExceeded phase resource cap required) ->
            assert
                "zero inequality cap fails in constraints with cap+1"
                (phase == CorrelationConstraints && resource == CorrelationInequalityCount && cap == 0 && required == 1)
        other -> ioError (userError ("zero inequality cap: " ++ describe other))
    -- A zero materialization cap fails atomically with no solution.
    let zeroMaterialization = correlationSolveLimits tinyLimits ceiling_ ceiling_ 0
    case solveCorrelatedEquilibrium zeroMaterialization zeroPayoffGame of
        Left (CorrelationSolveLimitExceeded _ resource cap required) ->
            assert
                "zero materialization cap fails with cap+1"
                (resource == CorrelationMaterialization && cap == 0 && required == 1)
        other -> ioError (userError ("zero materialization cap: " ++ describe other))
    -- A zero work cap fails atomically in admission, whose spine block is (16,0).
    let zeroWork = gameLimits 64 64 64 4096 0 64 ceiling_
        zeroWorkSolve = correlationSolveLimits zeroWork ceiling_ ceiling_ ceiling_
    case solveCorrelatedEquilibrium zeroWorkSolve zeroPayoffGame of
        Left (CorrelationSolveLimitExceeded phase resource cap required) ->
            assert
                "zero work cap fails in admission with cap+1"
                (phase == CorrelationAdmission && resource == CorrelationWork && cap == 0 && required == 1)
        other -> ioError (userError ("zero work cap: " ++ describe other))
    -- A tight input-payoff Rational bound fails during admission.  Payoff -1
    -- has combined size two, so a bound of one cannot admit it.
    let tightBits = gameLimits 64 64 64 4096 ceiling_ 1 ceiling_
        tightSolve = correlationSolveLimits tightBits ceiling_ ceiling_ ceiling_
    case solveCorrelatedEquilibrium tightSolve uniqueNonDiracGame of
        Left (CorrelationSolveLimitExceeded phase resource cap required) ->
            assert
                "tight payoff bits fail in admission with cap+1"
                (phase == CorrelationAdmission && resource == CorrelationRationalBits && cap == 1 && required == 2)
        other -> ioError (userError ("tight payoff bits: " ++ describe other))
    -- The exact required candidate prefix admits, and one below fails.
    let atPrefix = correlationSolveLimits tinyLimits ceiling_ 53 ceiling_
        belowPrefix = correlationSolveLimits tinyLimits ceiling_ 52 ceiling_
    case solveCorrelatedEquilibrium atPrefix uniqueNonDiracGame of
        Right solution ->
            assert
                "the exact candidate prefix admits its boundary"
                (correlationSolveCandidates (correlatedSolutionAccounting solution) == 53)
        other -> ioError (userError ("candidate prefix boundary: " ++ describe other))
    case solveCorrelatedEquilibrium belowPrefix uniqueNonDiracGame of
        Left (CorrelationSolveLimitExceeded phase resource cap required) ->
            assert
                "one below the candidate prefix fails with cap+1"
                (phase == CorrelationCombination && resource == CorrelationCandidateCount && cap == 52 && required == 53)
        other -> ioError (userError ("one-below candidate prefix: " ++ describe other))

{- | Materialization and work crossing in the same block selects
materialization.  Admission's spine block is @(16,0)@ and charges no
materialization, so the work cap must first be large enough to pass every spine
inspection.  The zero-payoff game inspects 14 spine cells, costing 224 work.
With a work cap just above that, the first polynomial block -- product
validation -- crosses both caps at once, and materialization must be reported.
-}
testMaterializationBeforeWork :: IO ()
testMaterializationBeforeWork = do
    let pastSpine = gameLimits 64 64 64 4096 300 64 ceiling_
        solveBoth = correlationSolveLimits pastSpine ceiling_ ceiling_ 0
    case solveCorrelatedEquilibrium solveBoth zeroPayoffGame of
        Left (CorrelationSolveLimitExceeded phase resource cap required) ->
            assert
                "materialization is selected before work"
                ( phase == CorrelationAdmission
                    && resource == CorrelationMaterialization
                    && cap == 0
                    && required == 1
                )
        other -> ioError (userError ("materialization before work: " ++ describe other))
    -- With materialization admitted, the same block reports work instead.
    let workOnly = correlationSolveLimits pastSpine ceiling_ ceiling_ ceiling_
    case solveCorrelatedEquilibrium workOnly zeroPayoffGame of
        Left (CorrelationSolveLimitExceeded phase resource cap required) ->
            assert
                "with materialization admitted the same block reports work"
                ( phase == CorrelationAdmission
                    && resource == CorrelationWork
                    && cap == 300
                    && required == 301
                )
        other -> ioError (userError ("work after materialization: " ++ describe other))

{- | A zero CE row count still validates the literal device and calls the real
CE checker.  The singleton game has no obedience rows.
-}
testZeroRowChecker :: IO ()
testZeroRowChecker = do
    solution <- case solveCorrelatedEquilibrium tinySolve singletonGame of
        Left problem -> ioError (userError ("zero row checker: " ++ show problem))
        Right value -> pure value
    let report = correlatedSolutionCheck solution
        device = correlatedSolutionDevice solution
    assert "zero CE rows" (correlatedObedienceCount report == 0)
    assert "zero CE rows still satisfied" (correlatedEquilibriumSatisfied report)
    -- The retained report is exactly the unmodified checker's report on the
    -- validated device, so both really ran.
    assert
        "the real CE checker ran on the validated device"
        (checkCorrelatedEquilibrium tinyLimits singletonGame device == Right report)
    assert
        "the literal device was validated"
        ( exactCorrelationDevice
            tinyLimits
            (normalGameProduct singletonGame)
            (correlationEntries device)
            == Right device
        )

{- | Rejected-candidate reservations and historical bits remain in the final
successful accounting.
-}
testRejectedHistoryRetained :: IO ()
testRejectedHistoryRetained = do
    solution <- case solveCorrelatedEquilibrium tinySolve uniqueNonDiracGame of
        Left problem -> ioError (userError ("retained history: " ++ show problem))
        Right value -> pure value
    let account = correlatedSolutionAccounting solution
    -- 53 attempted candidates, of which 52 were rejected before the witness.
    assert "attempted candidates retained" (correlationSolveCandidates account == 53)
    assert
        "rejection counters retained"
        ( correlationSolveRankDeficientCandidates account
            + correlationSolveInconsistentCandidates account
            + correlationSolveInequalityRejectedCandidates account
            == 52
        )
    -- Historical observed bits include the rejected candidates' arithmetic and
    -- are at least the covered maximum.
    assert
        "covered bits do not exceed observed bits"
        (correlationSolveCheckerCoveredRationalBits account <= correlationSolveObservedRationalBits account)
    assert "observed bits are positive" (correlationSolveObservedRationalBits account > 0)
    -- Reservations are cumulative and are never refunded.
    assert "cumulative work retained" (correlationSolveReservedWork account > 0)
    assert "cumulative materialization retained" (correlationSolveReservedMaterialization account > 0)

describe :: (Show owner, Show action) => Either (CorrelationSolveError owner action) value -> String
describe result = case result of
    Left problem -> show problem
    Right _ -> "unexpected success"

-- The independent labelled oracle.  It reads labelled payoff tables and
-- labelled joint masses, and evaluates deviations by constructing changed label
-- tuples directly.  It shares no production builder, elimination, or
-- replacement function with the solver.

labelledPayoff :: ExactNormalGame String String -> String -> [(String, String)] -> Rational
labelledPayoff game owner assignment =
    case lookup (canonicalise assignment) (labelledTable game) of
        Nothing -> error ("labelledPayoff: missing profile " ++ show assignment)
        Just row -> case lookup owner row of
            Nothing -> error ("labelledPayoff: missing owner " ++ owner)
            Just value -> value

labelledTable :: ExactNormalGame String String -> [([(String, String)], [(String, Rational)])]
labelledTable game =
    [ (canonicalise (ownedProfileEntries profile), playerValueEntries values)
    | (profile, values) <- normalGamePayoffs game
    ]

canonicalise :: [(String, String)] -> [(String, String)]
canonicalise = foldr insertSorted []
  where
    insertSorted entry [] = [entry]
    insertSorted entry (existing : remaining)
        | fst entry <= fst existing = entry : existing : remaining
        | otherwise = existing : insertSorted entry remaining

ownersOf :: ExactNormalGame String String -> [String]
ownersOf game = NonEmpty.toList (finiteObjectValues (ownedOwners (normalGameProduct game)))

actionsOf :: ExactNormalGame String String -> String -> [String]
actionsOf game owner = case ownedChoices (normalGameProduct game) owner of
    Nothing -> error ("actionsOf: missing owner " ++ owner)
    Just carrier -> NonEmpty.toList (finiteObjectValues carrier)

-- | Replace one owner's label directly, without any production helper.
substitute :: String -> String -> [(String, String)] -> [(String, String)]
substitute owner action assignment =
    [(current, if current == owner then action else choice) | (current, choice) <- assignment]

{- | CE evaluation: group by recommendation labels, and for each owner,
recommended action and alternative action require the unconditional slack to be
nonnegative.  Mass completeness, nonnegativity and total one are checked
separately.
-}
evaluateCorrelated :: ExactNormalGame String String -> [Rational] -> Bool
evaluateCorrelated game supplied =
    literalMassesValid supplied
        && and
            [ slack owner recommended alternative >= 0
            | owner <- ownersOf game
            , recommended <- actionsOf game owner
            , alternative <- actionsOf game owner
            , alternative /= recommended
            ]
  where
    joint = zip (profileLabels game) supplied
    slack owner recommended alternative =
        sum
            [ mass * (labelledPayoff game owner assignment - labelledPayoff game owner (substitute owner alternative assignment))
            | (assignment, mass) <- joint
            , lookup owner assignment == Just recommended
            ]

{- | CCE evaluation: evaluate constant deviations before recommendation, so each
owner and alternative action gives one unconditional slack over all profiles.
-}
evaluateCoarse :: ExactNormalGame String String -> [Rational] -> Bool
evaluateCoarse game supplied =
    literalMassesValid supplied
        && and
            [ slack owner alternative >= 0
            | owner <- ownersOf game
            , alternative <- actionsOf game owner
            ]
  where
    joint = zip (profileLabels game) supplied
    slack owner alternative =
        sum
            [ mass * (labelledPayoff game owner assignment - labelledPayoff game owner (substitute owner alternative assignment))
            | (assignment, mass) <- joint
            ]

-- | Literal mass completeness, nonnegativity, and total one, checked separately.
literalMassesValid :: [Rational] -> Bool
literalMassesValid supplied = all (>= 0) supplied && sum supplied == 1
