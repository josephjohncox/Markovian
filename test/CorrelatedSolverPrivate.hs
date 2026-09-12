{-# LANGUAGE ImportQualifiedPost #-}

{- | D083 private solver-core probes.

Mirrors @test/FeedbackRewardJVPPrivate.hs@: a standalone base-only executable
compiled against @-isrc@, so it reaches the private
"Markovian.Game.Correlated.Exact.Internal" without exposing it publicly.  It is
invoked by @scripts/check-correlated-solver@, not by the @Markovian-test@ suite.

These are the frozen fixtures that must call the actual production private
machinery:

* §11.6 the production constraint builder's ordered rows for the CE/CCE
  separating game, compared against an independently labelled evaluator.
* §11.7 the private linear-system controls over the real elimination and
  traversal.
* §11.9 the arithmetic-only report-length admission gates, through the same
  production geometry route.
* §11.10 the source-bound private CE shadow 'Rational' control.
-}
module Main (main) where

import Control.Monad (unless)
import Data.Maybe (isNothing)
import Data.Ratio ((%))
import Markovian.Category.Finite.Object
import Markovian.Game.Correlated.Exact qualified as Public
import Markovian.Game.Correlated.Exact.Internal
import Markovian.Game.NormalForm.Exact
import Markovian.Game.Profile.Finite
import Numeric.Natural (Natural)
import System.Environment (getArgs)

-- Oracle equations use the same explicit coordinate notation for every column.
{-# ANN module ("HLint: ignore Avoid lambda using `infix`" :: String) #-}
{-# ANN module ("HLint: ignore Use head" :: String) #-}

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        [] -> runTests
        ["--admission-reservation"] -> testAdmissionReservation
        ["--spine-reservation"] -> testSpineReservation
        ["--publication-reservation"] -> testPublicationReservation
        ["--reservation-events"] -> testSingletonReservations
        _ -> ioError (userError "unknown private solver control")

runTests :: IO ()
runTests = do
    testProductionBuilderRows
    testLinearSystemControls
    testGeometryAdmission
    testAggregateCounterexamples
    testShadowRationalControl
    testTraversalOrder
    testCheckerRowSequenceAgreement
    testCompetingFailures
    testBoundedRationalObservation
    testSingletonReservations
    testAdmissionReservation
    testSpineReservation
    testPublicationReservation

-- Independently derive the singleton block schedule for one or three owners.
-- There are 3r+3 spine inspections, three blocks of dimension 2r+2, and
-- 3m+9 blocks of dimension 2r+2+m. Every mode selects the empty tuple once.
singletonReservations :: SolveMode -> Natural -> (Natural, Natural)
singletonReservations mode owners =
    let dimension = 2 * owners + 2
        rows = case mode of
            CorrelatedMode -> 1
            CoarseMode -> owners + 1
        fields d = 1024 * d ^ (6 :: Int)
        materialization = 3 * fields dimension + (3 * rows + 9) * fields (dimension + rows)
        work = 16 * (3 * owners + 3) + 65 * materialization
     in (materialization, work)

singletonOwners :: Natural -> ExactNormalGame String String
singletonOwners count =
    let owners = ["P" ++ show index | index <- [1 .. count]]
     in buildGame
            [(owner, ["A"]) | owner <- owners]
            [([(owner, "A") | owner <- owners], [(owner, 0) | owner <- owners])]

publicAccount ::
    SolveMode ->
    Public.CorrelationSolveLimits ->
    ExactNormalGame String String ->
    Either (Public.CorrelationSolveError String String) Public.CorrelationSolveAccounting
publicAccount mode limits game = case mode of
    CorrelatedMode -> Public.correlatedSolutionAccounting <$> Public.solveCorrelatedEquilibrium limits game
    CoarseMode -> Public.coarseCorrelatedSolutionAccounting <$> Public.solveCoarseCorrelatedEquilibrium limits game

testSingletonReservations :: IO ()
testSingletonReservations = mapM_ check [(mode, owners) | mode <- [CorrelatedMode, CoarseMode], owners <- [1, 3]]
  where
    check (mode, owners) = do
        let (materialization, work) = singletonReservations mode owners
            limits = Public.correlationSolveLimits (tinyLimits{maximumGameWork = work}) ceiling_ ceiling_ materialization
        case publicAccount mode limits (singletonOwners owners) of
            Left problem -> ioError (userError ("exact singleton reservations: " ++ show problem))
            Right account ->
                assert
                    "singleton reservations equal the independent block schedule"
                    ( Public.correlationSolveReservedMaterialization account == materialization
                        && Public.correlationSolveReservedWork account == work
                        && Public.correlationSolveCandidates account == 1
                        && null (Public.correlationSolveSelectedInequalities account)
                    )

-- The script also runs these controls against scratch instrumented source.
-- Its event checks reject a local result-list cons before zero-materialization
-- admission, a carrier inspection before zero-work admission, and a report
-- comparison before publication admission. Inputs use ordinary public builders.
testAdmissionReservation :: IO ()
testAdmissionReservation = mapM_ check [CorrelatedMode, CoarseMode]
  where
    check mode = do
        let game = singletonOwners 3
            limits = Public.correlationSolveLimits tinyLimits ceiling_ ceiling_ 0
        game `seq`
            assert
                "zero materialization rejects admission"
                (publicAccount mode limits game == Left (Public.CorrelationSolveLimitExceeded Public.CorrelationAdmission Public.CorrelationMaterialization 0 1))

testSpineReservation :: IO ()
testSpineReservation = mapM_ check [CorrelatedMode, CoarseMode]
  where
    check mode = do
        let game = singletonOwners 3
            limits = Public.correlationSolveLimits (tinyLimits{maximumGameWork = 0}) ceiling_ ceiling_ ceiling_
        game `seq`
            assert
                "zero work rejects before the first carrier inspection"
                (publicAccount mode limits game == Left (Public.CorrelationSolveLimitExceeded Public.CorrelationAdmission Public.CorrelationWork 0 1))

testPublicationReservation :: IO ()
testPublicationReservation = mapM_ check [(mode, owners) | mode <- [CorrelatedMode, CoarseMode], owners <- [1, 3]]
  where
    check (mode, owners) = do
        let game = singletonOwners owners
            (materialization, work) = singletonReservations mode owners
            fieldLimits = Public.correlationSolveLimits tinyLimits ceiling_ ceiling_ (materialization - 1)
            workLimits = Public.correlationSolveLimits (tinyLimits{maximumGameWork = work - 1}) ceiling_ ceiling_ ceiling_
        assert
            "one-below publication materialization rejects before comparison"
            (publicAccount mode fieldLimits game == Left (Public.CorrelationSolveLimitExceeded Public.CorrelationPublication Public.CorrelationMaterialization (materialization - 1) materialization))
        assert
            "one-below publication work rejects before comparison"
            (publicAccount mode workLimits game == Left (Public.CorrelationSolveLimitExceeded Public.CorrelationPublication Public.CorrelationWork (work - 1) work))

-- Numerator and denominator consume the same bit budget. Rejection stops at
-- the first bit over that budget, including when the denominator crosses it.
testBoundedRationalObservation :: IO ()
testBoundedRationalObservation = do
    mapM_
        ( \value ->
            mapM_
                ( \cap ->
                    assert
                        ("bounded size of " ++ show value ++ " at " ++ show cap)
                        (boundedRationalSize cap value == min (cap + 1) (rationalSizeBits value))
                )
                [0 .. 16]
        )
        [0, 1, -1, 3 % 256, -(3 % 256), 256 % 3, 255 % 257]
    let limits = correlationSolveLimits (gameLimits 64 64 64 4096 ceiling_ 7 ceiling_) ceiling_ ceiling_ ceiling_
        result = runSolve (observeRational limits CorrelationAdmission (3 % 256)) :: Either (SolveFault String) ((), CorrelationSolveAccounting)
    assert
        "an oversized denominator fails admission with cap+1"
        (result == Left (SolveLimitFault CorrelationAdmission CorrelationRationalBits 7 8))

ceiling_ :: Natural
ceiling_ = fromIntegral (maxBound :: Int) - 1

machine :: Natural
machine = fromIntegral (maxBound :: Int)

assert :: String -> Bool -> IO ()
assert message condition = unless condition (ioError (userError message))

orDie :: (Show problem) => Either problem value -> value
orDie = either (error . show) id

tinyLimits :: GameLimits
tinyLimits = gameLimits 64 64 64 4096 ceiling_ 64 ceiling_

tinySolve :: CorrelationSolveLimits
tinySolve = correlationSolveLimits tinyLimits ceiling_ ceiling_ ceiling_

-- | Run a private thread and require success.
runOrDie :: (Show owner) => String -> Solve owner value -> value
runOrDie label thread = case runSolve thread of
    Left fault -> error (label ++ ": " ++ show fault)
    Right (value, _) -> value

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

{- | §11.6 the separating game.  Row payoffs in profile order
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

{- | §11.6 Require the ACTUAL production constraint builder to generate this
game's ordered rows, then evaluate every row at every vertex in both complete
lists and at the separating mass, comparing against the independently labelled
evaluator.  Oracle self-agreement or public first witnesses cannot satisfy this.
-}
testProductionBuilderRows :: IO ()
testProductionBuilderRows = do
    -- The production builder, called directly.
    let ceRows = runOrDie "CE builder" (buildConstraints tinySolve CorrelatedMode separatingGame 64)
        cceRows = runOrDie "CCE builder" (buildConstraints tinySolve CoarseMode separatingGame 64)
    assert "CE row count is n+q = 6+8" (naturalCount ceRows == 14)
    assert "CCE row count is n+q = 6+5" (naturalCount cceRows == 11)
    -- Rows 0..5 are the nonnegativity rows in profile order.
    mapM_
        ( \index ->
            assert
                ("CE row " ++ show index ++ " is the nonnegativity row")
                (rowCoefficients (ceRows !! fromIntegral index) == unitRow index)
        )
        [0 .. 5 :: Natural]
    mapM_
        ( \index ->
            assert
                ("CCE row " ++ show index ++ " is the nonnegativity row")
                (rowCoefficients (cceRows !! fromIntegral index) == unitRow index)
        )
        [0 .. 5 :: Natural]
    -- Every ordered production row, evaluated at every test mass, must equal
    -- the independent labelled evaluation of the frozen expression table.
    mapM_ (checkModeRows "CE" ceRows ceExpected) testMasses
    mapM_ (checkModeRows "CCE" cceRows cceExpected) testMasses
    -- At the separating mass, CE row 6 is -1/2 and every CCE deviation row is
    -- zero.
    let separating = separatingMass
        ceRow6 = dotRow (rowCoefficients (ceRows !! 6)) separating
    assert "CE row 6 rejects the separating mass at -1/2" (ceRow6 == negate (1 / 2))
    assert
        "every CCE deviation row is zero at the separating mass"
        (all (\index -> dotRow (rowCoefficients (cceRows !! index)) separating == 0) [6 .. 10])
    assert "the separating mass is nonnegative" (all (>= 0) separating)
  where
    checkModeRows label rows expected mass =
        mapM_
            ( \(index, expression) -> do
                let produced = dotRow (rowCoefficients (rows !! index)) mass
                    oracle = expression mass
                assert
                    ( label
                        ++ " row "
                        ++ show index
                        ++ " must match the independent evaluator at "
                        ++ show mass
                        ++ " (production "
                        ++ show produced
                        ++ " vs oracle "
                        ++ show oracle
                        ++ ")"
                    )
                    (produced == oracle)
            )
            expected

unitRow :: Natural -> [Rational]
unitRow index = [if position == index then 1 else 0 | position <- [0 .. 5]]

dotRow :: [Rational] -> [Rational] -> Rational
dotRow coefficients mass = sum (zipWith (*) coefficients mass)

{- | The independently labelled expression table from §11.6, written directly
against the coordinate names @(p_AL,p_AR,p_BL,p_BR,p_CL,p_CR)@.  It shares no
production function.
-}
ceExpected :: [(Int, [Rational] -> Rational)]
ceExpected =
    [ (0, \p -> p !! 0)
    , (1, \p -> p !! 1)
    , (2, \p -> p !! 2)
    , (3, \p -> p !! 3)
    , (4, \p -> p !! 4)
    , (5, \p -> p !! 5)
    , (6, \p -> negate (p !! 0) + p !! 1) -- Row, A to B
    , (7, const 0) -- Row, A to C
    , (8, \p -> p !! 2 - p !! 3) -- Row, B to A
    , (9, \p -> p !! 2 - p !! 3) -- Row, B to C
    , (10, const 0) -- Row, C to A
    , (11, \p -> negate (p !! 4) + p !! 5) -- Row, C to B
    , (12, const 0) -- Column, L to R
    , (13, const 0) -- Column, R to L
    ]

cceExpected :: [(Int, [Rational] -> Rational)]
cceExpected =
    [ (0, \p -> p !! 0)
    , (1, \p -> p !! 1)
    , (2, \p -> p !! 2)
    , (3, \p -> p !! 3)
    , (4, \p -> p !! 4)
    , (5, \p -> p !! 5)
    , (6, \p -> p !! 2 - p !! 3) -- Row, alternative A
    , (7, \p -> negate (p !! 0) + p !! 1 - p !! 4 + p !! 5) -- Row, alternative B
    , (8, \p -> p !! 2 - p !! 3) -- Row, alternative C
    , (9, const 0) -- Column, alternative L
    , (10, const 0) -- Column, alternative R
    ]

-- | The complete CE and CCE vertex lists from §11.6, plus the separating mass.
testMasses :: [[Rational]]
testMasses = ceVertices ++ cceVertices ++ [separatingMass]

ceVertices :: [[Rational]]
ceVertices =
    [ basis 1
    , basis 2
    , basis 5
    , half 0 1
    , half 2 3
    , half 4 5
    ]

cceVertices :: [[Rational]]
cceVertices =
    [ basis 1
    , basis 2
    , basis 5
    , half 0 1
    , half 0 5
    , half 4 1
    , half 4 5
    , half 2 3
    ]

separatingMass :: [Rational]
separatingMass = half 0 5

basis :: Int -> [Rational]
basis index = [if position == index then 1 else 0 | position <- [0 .. 5]]

half :: Int -> Int -> [Rational]
half left right =
    [ if position == left || position == right then 1 / 2 else 0
    | position <- [0 .. 5]
    ]

{- | §11.7 the private linear-system controls, over the real 'eliminate',
'classify' and traversal.  Inputs are augmented matrices; the first row is the
fixed normalization row.
-}
testLinearSystemControls :: IO ()
testLinearSystemControls = do
    -- Normalization (1,1|1) with (0,0|0): consistent rank deficiency.
    expectClassification
        "normalization with a zero row is rank deficient"
        2
        [[1, 1, 1], [0, 0, 0]]
        RankDeficient
    -- Normalization (1,1|1) with (1,1|0): inconsistency, not rank deficiency.
    expectClassification
        "normalization with a parallel row is inconsistent"
        2
        [[1, 1, 1], [1, 1, 0]]
        Inconsistent
    -- A missing early-column pivot with a later nonzero column: the search
    -- continues left to right and classifies only after all columns.
    expectClassification
        "a missing early pivot still classifies after every column"
        3
        [[1, 1, 1, 1], [0, 0, 1, 0], [0, 0, 0, 0]]
        RankDeficient
    -- Cancellation control: observe 16*16 = 256 before the zero residual, and
    -- retain ten historical bits after the candidate is discarded.
    let cancelling = [[1, 1, 1, 1], [0, 1, 16, 0], [0, 16, 256, 0]]
    case runSolve (eliminationThread 3 cancelling) of
        Left fault -> ioError (userError ("cancellation control: " ++ show fault))
        Right ((_, pivots, rows), account) -> do
            assert
                "the cancellation control is consistently rank deficient"
                (classify 3 pivots rows == RankDeficient)
            -- size(256) = 10 and size(0) = 2; the peak must survive the discard.
            assert
                ( "historical observation retains ten bits, saw "
                    ++ show (correlationSolveObservedRationalBits' account)
                )
                (correlationSolveObservedRationalBits' account >= 10)
    -- Unique-system control: normalization by 1/16 makes the third pivot-row
    -- coefficient 16, and eliminating the other row multiplies it by 16,
    -- producing 256 (ten bits) before residual -255 (nine bits).  The unique
    -- solution is (1,0,0).
    let unique = [[1, 1, 1, 1], [0, 1 / 16, 1, 0], [0, 16, 1, 0]]
    case runSolve (eliminationThread 3 unique) of
        Left fault -> ioError (userError ("unique-system control: " ++ show fault))
        Right ((_, pivots, rows), account) -> do
            assert
                "the unique system solves to (1,0,0)"
                (classify 3 pivots rows == FullRank [1, 0, 0])
            assert
                ( "the unique system peaks at ten bits, saw "
                    ++ show (correlationSolveObservedRationalBits' account)
                )
                (correlationSolveObservedRationalBits' account >= 10)
    -- A Rational cap of nine fails during elimination despite six-bit inputs
    -- and a two-bit final solution; a cap of ten admits it.
    let nineBits = correlationSolveLimits (gameLimits 64 64 64 4096 ceiling_ 9 ceiling_) ceiling_ ceiling_ ceiling_
        tenBits = correlationSolveLimits (gameLimits 64 64 64 4096 ceiling_ 10 ceiling_) ceiling_ ceiling_ ceiling_
    case runSolve (eliminationWith nineBits 3 unique) of
        Left (SolveLimitFault phase resource cap required) ->
            assert
                "a nine-bit cap fails during elimination with cap+1"
                (phase == CorrelationElimination && resource == CorrelationRationalBits && cap == 9 && required == 10)
        other -> ioError (userError ("nine-bit elimination: " ++ describeThread other))
    case runSolve (eliminationWith tenBits 3 unique) of
        Right _ -> pure ()
        Left fault -> ioError (userError ("ten-bit elimination should admit: " ++ show fault))
    -- The private value 1/16 has combined size six, not Feedback's five.
    assert "1/16 has combined size six" (boundedRationalSize 64 (1 / 16) == 6)
    assert "zero has combined size two" (boundedRationalSize 64 0 == 2)
    assert "one has combined size two" (boundedRationalSize 64 1 == 2)
    assert "256 has combined size ten" (boundedRationalSize 64 256 == 10)
    assert "a negative numerator uses its magnitude" (boundedRationalSize 64 (negate 255) == 9)
    -- Two eligible nonzero rows below the pivot position: the first wins and
    -- the other row order is unchanged.
    let twoEligible = [[1, 1, 1, 1], [0, 2, 0, 0], [0, 4, 0, 0]]
    case runSolve (eliminationThread 3 twoEligible) of
        Left fault -> ioError (userError ("first eligible pivot: " ++ show fault))
        Right ((_, pivots, rows), _) -> do
            assert "two eligible rows still rank deficient" (classify 3 pivots rows == RankDeficient)
            -- Row 1 was chosen and normalized to a leading one in column 1;
            -- row 2 was reduced to zero rather than reordered.
            assert
                "the first eligible row was selected"
                (rowCoefficients' (rows !! 1) 1 == 1 && rowCoefficients' (rows !! 2) 1 == 0)
    -- The same §11.7 row, but shaped so that `swapRows` actually runs with
    -- DISTINCT arguments. Above, the first eligible row already sits at the
    -- pivot position, so the swap short-circuits and "other row order remains
    -- unchanged" goes untested. Here the pivot-position row holds a zero in the
    -- current column and two eligible rows sit strictly below it.
    --
    -- Columns 2 and 3 are inert identity markers: `variables = 2`, so they are
    -- never pivotable. Every pivot row carries zero in both marker columns, so
    -- elimination leaves the markers of non-pivot rows untouched and they
    -- faithfully report where each original row landed.
    --
    -- Layout per row: [c0, c1, markerA, markerB, rhs].
    let swapDriver =
            [ [1, 1, 0, 0, 1] -- normalization
            , [0, 0, 5, 0, 0] -- AT the pivot position, ZERO in column 1; markerA
            , [0, 2, 0, 0, 0] -- first eligible strictly below; becomes the pivot
            , [0, 4, 0, 7, 0] -- second eligible strictly below; markerB
            ]
    case runSolve (eliminationThread 2 swapDriver) of
        Left fault -> ioError (userError ("pivot swap control: " ++ show fault))
        Right ((_, pivots, rows), _) -> do
            assert "the pivot-swap control finds two pivots" (pivots == 2)
            -- The FIRST eligible row below the pivot position won: the row that
            -- held 2 became the normalized pivot at position 1, so its column-1
            -- coefficient is one and it carries neither marker.
            assert
                "the first eligible row below the pivot position was selected"
                ( rowCoefficients' (rows !! 1) 1 == 1
                    && rowCoefficients' (rows !! 1) 2 == 0
                    && rowCoefficients' (rows !! 1) 3 == 0
                )
            -- The two NON-selected rows keep their relative order: the displaced
            -- zero-coefficient row (markerA) moved from position 1 to position 2,
            -- and the unselected eligible row (markerB) stayed at position 3.
            -- Selecting the LAST eligible row instead would place markerA at 3
            -- and scatter markerB, so this assertion discriminates.
            assert
                "the non-selected rows retain their relative order"
                ( rowCoefficients' (rows !! 2) 2 == 5
                    && rowCoefficients' (rows !! 3) 3 == 7
                )
            -- Neither marker leaked into another row position.
            assert
                "the identity markers were not modified by elimination"
                ( map (\row -> rowCoefficients' row 2) rows == [0, 0, 5, 0]
                    && map (\row -> rowCoefficients' row 3) rows == [0, 0, 0, 7]
                )
    -- Infeasibility through a private linear system, not a fabricated
    -- finite-game no-equilibrium example: x>=0, y>=0, -x-y>=0 with x+y=1.
    -- Three one-row active tuples complete unsuccessfully; the first two
    -- violate the last inequality and the last tuple is inconsistent.
    let infeasibleRows =
            [ [1, 0]
            , [0, 1]
            , [negate 1, negate 1]
            ]
        outcomes = map (oneRowOutcome infeasibleRows) [0, 1, 2]
    assert
        "the private infeasible system completes with two violations and one inconsistency"
        (outcomes == [Just False, Just False, Nothing])

-- | Read one coefficient of an eliminated row.
rowCoefficients' :: [Rational] -> Int -> Rational
rowCoefficients' row index = row !! index

{- | Classify a one-row active tuple of the private infeasible system.
'Nothing' means the candidate was inconsistent; @Just False@ means it was
feasible-shaped but violated an inequality.
-}
oneRowOutcome :: [[Rational]] -> Int -> Maybe Bool
oneRowOutcome rows index =
    let matrix = [[1, 1, 1], (rows !! index) ++ [0]]
     in case runSolve (eliminationThread 2 matrix) of
            Left fault -> error ("infeasible control: " ++ show fault)
            Right ((_, pivots, eliminated), _) -> case classify 2 pivots eliminated of
                Inconsistent -> Nothing
                RankDeficient -> Nothing
                FullRank masses -> Just (all (\row -> dotTwo row masses >= 0) rows)

dotTwo :: [Rational] -> [Rational] -> Rational
dotTwo coefficients masses = sum (zipWith (*) coefficients masses)

-- | Drive the real elimination and report its pivot count and rows.
eliminationThread :: Natural -> [[Rational]] -> Solve String ([[Rational]], Natural, [[Rational]])
eliminationThread = eliminationWith tinySolve

eliminationWith ::
    CorrelationSolveLimits ->
    Natural ->
    [[Rational]] ->
    Solve String ([[Rational]], Natural, [[Rational]])
eliminationWith limits variables matrix = do
    (rows, pivots) <- eliminate limits 8 variables matrix
    pure (matrix, pivots, rows)

expectClassification :: String -> Natural -> [[Rational]] -> Classification -> IO ()
expectClassification label variables matrix expected =
    case runSolve (eliminationThread variables matrix) of
        Left fault -> ioError (userError (label ++ ": " ++ show fault))
        Right ((_, pivots, rows), _) ->
            assert
                (label ++ " (got " ++ show (classify variables pivots rows) ++ ")")
                (classify variables pivots rows == expected)

describeThread :: (Show owner, Show value) => Either (SolveFault owner) value -> String
describeThread result = case result of
    Left fault -> show fault
    Right value -> "unexpected success: " ++ show value

{- | §11.9 the arithmetic-only report-length admission gates, driven through the
same production geometry route.  These synthetic @(n,q)@ inputs test the exact
boundary; they do not claim every pair describes a constructor-admitted game.
-}
testGeometryAdmission :: IO ()
testGeometryAdmission = do
    -- n=1, q=I-1, cap=I: exact m=I fits and the report length fits at its
    -- ceiling.
    expectGeometry "n=1 q=I-1 cap=I admits at the ceiling" 1 (machine - 1) machine (Right (machine - 1, machine))
    -- n=1, q=I, cap=I+1: exact m=I+1 fits, but the report length is exceeded.
    expectGeometry
        "n=1 q=I cap=I+1 exceeds the report length"
        1
        machine
        (machine + 1)
        (Left (SolveRepresentationFault CorrelationReportLength))
    -- n=1, q=I, cap=I: q fits but m exceeds the inequality cap, so the
    -- inequality failure is reported rather than report length.
    expectGeometry
        "n=1 q=I cap=I reports the inequality failure"
        1
        machine
        machine
        (Left (SolveLimitFault CorrelationConstraints CorrelationInequalityCount machine (machine + 1)))
    -- n=1, q=I, cap=I-1: q already exceeds the cap, and its saturation
    -- sentinel is not used as an exact report count.
    expectGeometry
        "n=1 q=I cap=I-1 reports the inequality failure without the sentinel"
        1
        machine
        (machine - 1)
        (Left (SolveLimitFault CorrelationConstraints CorrelationInequalityCount (machine - 1) machine))
    -- Competing failures through the same reserved production route.
    -- Insufficient materialization wins before work or either geometry gate.
    let noMaterialization = correlationSolveLimits tinyLimits 0 ceiling_ 0
    case runSolve (geometryThread noMaterialization CoarseMode 1 [machine]) of
        Left (SolveLimitFault _ resource _ _) ->
            assert "insufficient materialization wins first" (resource == CorrelationMaterialization)
        other -> ioError (userError ("geometry materialization: " ++ describeThread other))
    -- With materialization admitted, insufficient work wins before either gate.
    let noWork = correlationSolveLimits (gameLimits 64 64 64 4096 0 64 ceiling_) 0 ceiling_ ceiling_
    case runSolve (geometryThread noWork CoarseMode 1 [machine]) of
        Left (SolveLimitFault _ resource _ _) ->
            assert "insufficient work wins before either gate" (resource == CorrelationWork)
        other -> ioError (userError ("geometry work: " ++ describeThread other))

{- | Reserve the geometry block, then run the production gate, exactly as §7
orders them.
-}
geometryThread ::
    CorrelationSolveLimits ->
    SolveMode ->
    Natural ->
    [Natural] ->
    Solve String (Natural, Natural)
geometryThread limits mode profiles locals = do
    reserveBlock limits CorrelationConstraints 8
    admitGeometry limits mode profiles locals

{- | Drive the production geometry gate with a synthetic @q@.  CCE's count fold
is @L@, so a single-entry local list supplies @q@ directly without constructing
a carrier.
-}
expectGeometry :: String -> Natural -> Natural -> Natural -> Either (SolveFault String) (Natural, Natural) -> IO ()
expectGeometry label profiles rows cap expected = do
    let limits = correlationSolveLimits tinyLimits cap ceiling_ ceiling_
    case runSolve (admitGeometry limits CoarseMode profiles [rows]) of
        Left fault -> assert (label ++ " (got " ++ show fault ++ ")") (Left fault == expected)
        Right (value, _) -> assert (label ++ " (got " ++ show value ++ ")") (Right value == expected)

{- | §11.9 the aggregate-count counterexamples, bound to the production count
formulas and the geometry gate.  These are scalar computations: no giant
carrier, game, report list, or row list is constructed.
-}
testAggregateCounterexamples :: IO ()
testAggregateCounterexamples = do
    -- CE, 64-bit Int: I = 2^63-1.  One owner with a = 2^32 actions has n = a,
    -- q = a(a-1) = 2^64 - 2^32, and m = 2^64.  Owner, local-action and profile
    -- counts each fit I-1, but the report-length gate must reject q.
    let actions = 2 ^ (32 :: Int) :: Natural
        expectedQ = actions * (actions - 1)
        expectedM = actions + expectedQ
    assert "I is 2^63-1 on this host" (machine == 2 ^ (63 :: Int) - 1)
    assert "the CE aggregate count is a(a-1)" (obedienceRowCount expectedM [actions] == expectedQ)
    assert "the CE aggregate m is 2^64" (expectedM == 2 ^ (64 :: Int))
    assert "the individual action count still fits I-1" (actions <= ceiling_)
    let ceLimits = correlationSolveLimits tinyLimits expectedM ceiling_ ceiling_
    case runSolve (admitGeometry ceLimits CorrelatedMode actions [actions] :: Solve String (Natural, Natural)) of
        Left fault ->
            assert
                ("the CE aggregate must be rejected by report length, got " ++ show fault)
                (fault == SolveRepresentationFault CorrelationReportLength)
        Right value -> ioError (userError ("CE aggregate admitted " ++ show value))
    -- CCE: let r = I-1, with two binary carriers and singleton carriers for the
    -- other owners.  Then n = 4, L = 2+2+(r-2) = r+2 = I+1, q = I+1, and
    -- m = I+5.  The singleton contribution is derived algebraically, not by
    -- traversing I-3 entries.
    let owners = machine - 1
        localSum = 2 + 2 + (owners - 2)
        coarseM = 4 + localSum
    assert "the CCE aggregate L is I+1" (localSum == machine + 1)
    assert "the CCE aggregate m is I+5" (coarseM == machine + 5)
    assert "the CCE count fold is L" (coarseRowCount coarseM [2, 2, owners - 2] == localSum)
    let cceLimits = correlationSolveLimits tinyLimits coarseM ceiling_ ceiling_
    case runSolve (admitGeometry cceLimits CoarseMode 4 [2, 2, owners - 2] :: Solve String (Natural, Natural)) of
        Left fault ->
            assert
                ("the CCE aggregate must be rejected by report length, got " ++ show fault)
                (fault == SolveRepresentationFault CorrelationReportLength)
        Right value -> ioError (userError ("CCE aggregate admitted " ++ show value))

{- | §11.10 the source-bound private CE shadow control.

A zero-payoff game with owners @[Row,Column]@, Row actions @[A,B]@ and Column
actions @[L,R]@, profile order @(AL,AR,BL,BR)@.  The complete candidate masses
@(1/3,1/6,1/5,3/10)@ are injected only into this private route; they are not the
public solver's selected candidate.
-}
testShadowRationalControl :: IO ()
testShadowRationalControl = do
    let game =
            buildGame
                [("Row", ["A", "B"]), ("Column", ["L", "R"])]
                [ ([("Row", a), ("Column", c)], [("Row", 0), ("Column", 0)])
                | a <- ["A", "B"]
                , c <- ["L", "R"]
                ]
        profiles = carrierValues (ownedProfiles (normalGameProduct game))
        injected = [1 / 3, 1 / 6, 1 / 5, 3 / 10]
        entries = zip profiles injected
    -- The independent labelled arithmetic trace.
    assert "the injected masses have sizes 3,4,4,6" (map (boundedRationalSize 64) injected == [3, 4, 4, 6])
    let prefixes = scanl1 (+) injected
    assert "the normalization prefixes are 1/3, 1/2, 7/10, 1" (prefixes == [1 / 3, 1 / 2, 7 / 10, 1])
    assert
        "the normalization prefix sizes are 3,3,7,2"
        (map (boundedRationalSize 64) prefixes == [3, 3, 7, 2])
    -- 1/3 + 1/5 = 8/15, whose reduced numerator and denominator each have four
    -- bits, so the combined size is eight: the first new peak after preparation.
    assert "1/3 + 1/5 is 8/15 with combined size eight" (boundedRationalSize 64 (1 / 3 + 1 / 5) == 8)
    assert "1/6 + 3/10 is 7/15 with combined size seven" (boundedRationalSize 64 (1 / 6 + 3 / 10) == 7)
    -- Preparation: admit the zero game and constants, build the production
    -- rows for each mode, observe the injected masses in profile order, and run
    -- the declared normalization and ordered inequality folds without
    -- elimination.  That cumulative account carries into the production shadow.
    let prepare limits mode = do
            _ <- admitGame limits game
            reserveBlock limits CorrelationConstraints 16
            _ <- admitGeometry limits mode 4 [2, 2]
            observeRational limits CorrelationConstraints 0
            observeRational limits CorrelationConstraints 1
            rows <- buildConstraints limits mode game 32
            mapM_ (observeRational limits CorrelationVerification) injected
            _ <- verifyCandidate limits 32 rows injected
            pure rows
    -- Preparation alone ends with historical H = 7 under a seven-bit bound.
    let sevenBits = correlationSolveLimits (gameLimits 64 64 64 4096 ceiling_ 7 ceiling_) ceiling_ ceiling_ ceiling_
    case runSolve (prepare sevenBits CorrelatedMode) of
        Left fault -> ioError (userError ("CE preparation should fit seven bits: " ++ show fault))
        Right (_, account) ->
            assert
                ( "CE preparation ends with H=7, saw "
                    ++ show (correlationSolveObservedRationalBits' account)
                )
                (correlationSolveObservedRationalBits' account == 7)
    -- Through the ACTUAL production CE shadow, B=7 must fail at the Column-L
    -- recommendation addition.
    let ceLabels = deviationLabels CorrelatedMode (normalGameProduct game)
        ceShadowThread limits = do
            _ <- prepare limits CorrelatedMode
            correlatedShadow limits game entries ceLabels
    case runSolve (ceShadowThread sevenBits) of
        Left fault ->
            assert
                ("the CE shadow must fail at seven bits with 7/8, got " ++ show fault)
                (fault == SolveLimitFault CorrelationVerification CorrelationRationalBits 7 8)
        Right _ -> ioError (userError "the CE shadow should not fit seven bits")
    -- With B=8 the same CE shadow history fits and reaches H=8.
    let eightBits = correlationSolveLimits (gameLimits 64 64 64 4096 ceiling_ 8 ceiling_) ceiling_ ceiling_ ceiling_
    case runSolve (ceShadowThread eightBits) of
        Left fault -> ioError (userError ("the CE shadow should fit eight bits: " ++ show fault))
        Right (rows, account) -> do
            assert
                ( "the CE shadow reaches H=8, saw "
                    ++ show (correlationSolveObservedRationalBits' account)
                )
                (correlationSolveObservedRationalBits' account == 8)
            assert "every CE shadow slack is zero" (all ((== 0) . shadowSlack) rows)
            -- All recommendation totals are positive.
            assert "every CE recommendation total is positive" (all ((> 0) . shadowRecommendation) rows)
    -- The corresponding production CCE shadow has no recommendation-subset
    -- folds, so its history fits seven bits and retains H=7.
    let cceLabels = deviationLabels CoarseMode (normalGameProduct game)
        cceShadowThread limits = do
            _ <- prepare limits CoarseMode
            coarseShadow limits game entries cceLabels
    case runSolve (cceShadowThread sevenBits) of
        Left fault -> ioError (userError ("the CCE shadow should fit seven bits: " ++ show fault))
        Right (rows, account) -> do
            assert
                ( "the CCE shadow retains H=7, saw "
                    ++ show (correlationSolveObservedRationalBits' account)
                )
                (correlationSolveObservedRationalBits' account == 7)
            assert "every CCE shadow slack is zero" (all ((== 0) . shadowSlack) rows)

{- | The deterministic traversal: the initial tuple, the lexicographic
successor, and the empty-tuple case, over the real production functions.
-}
testTraversalOrder :: IO ()
testTraversalOrder = do
    assert "the initial tuple is [0..k-1]" (initialTuple 3 == [0, 1, 2])
    -- For k = 0 the empty tuple is emitted exactly once.
    assert "the empty tuple has no successor" (isNothing (successorTuple 5 []))
    assert "the initial empty tuple is empty" (null (initialTuple 0))
    -- Lexicographic order over 3-subsets of 0..7, matching §11.3's family.
    let family = enumerate 8 (initialTuple 3)
    assert "there are 56 three-subsets of eight rows" (length family == 56)
    assert "the family starts at [0,1,2]" (take 1 family == [[0, 1, 2]])
    assert "the family ends at [5,6,7]" (drop 55 family == [[5, 6, 7]])
    -- Candidate 53 is [4,5,6], and only three tuples follow it.
    assert "candidate 53 is [4,5,6]" (take 1 (drop 52 family) == [[4, 5, 6]])
    assert "three tuples follow [4,5,6]" (length (drop 53 family) == 3)
    -- The successor is strictly increasing lexicographically.
    assert
        "the family is strictly lexicographically increasing"
        (and (zipWith (<) family (drop 1 family)))
    -- Two-subsets of 0..3, checked exhaustively.
    assert
        "the two-subsets of four rows are complete and ordered"
        (enumerate 4 (initialTuple 2) == [[0, 1], [0, 2], [0, 3], [1, 2], [1, 3], [2, 3]])

{- | §11.8 unmodified-call row-sequence agreement.

Compares the ACTUAL production shadow's ordered per-row outputs against the
ACTUAL unmodified legacy checker's ordered per-row outputs, on a device chosen
to contain zero-mass entries and a null recommendation. This measures agreement
at the level of observable per-row results rather than re-deriving them.

It does NOT capture the discarded source-level intermediate operands inside the
legacy bodies; doing so would require instrumenting those unmodified bodies,
which the contract forbids refactoring. That residue is disclosed in the report:
'Public.correlationSolveCheckerCoveredRationalBits' remains argued from sequence
equality, not measured.
-}
testCheckerRowSequenceAgreement :: IO ()
testCheckerRowSequenceAgreement = do
    let game = zeroPayoffPair
        product_ = normalGameProduct game
        profiles = carrierValues (ownedProfiles product_)
        -- Zero-mass entries at AR and BR. For CE this also forces a NULL
        -- recommendation on Column/R, whose matching masses are both zero.
        supplied = [1 / 2, 0, 1 / 2, 0]
        entries = zip profiles supplied
    device <- case Public.exactCorrelationDevice tinyLimits product_ entries of
        Left problem -> ioError (userError ("row-sequence device: " ++ show problem))
        Right value -> pure value
    -- The real production CE shadow.
    ceShadow <- case runSolve (correlatedShadow tinySolve game entries (deviationLabels CorrelatedMode product_)) of
        Left fault -> ioError (userError ("row-sequence CE shadow: " ++ show fault))
        Right (rows, _) -> pure rows
    -- The real unmodified CE checker.
    ceReport <- case Public.checkCorrelatedEquilibrium tinyLimits game device of
        Left problem -> ioError (userError ("row-sequence CE checker: " ++ show problem))
        Right value -> pure value
    let ceChecks = Public.correlatedObedienceChecks ceReport
    assert
        "the CE shadow and the real CE checker produce the same row count"
        (naturalCount ceShadow == naturalCount ceChecks)
    -- The control genuinely contains a null recommendation and zero masses.
    assert
        "the CE control exercises a null recommendation"
        (any (\c -> Public.recommendationStatus c == Public.NullRecommendation) ceChecks)
    assert
        "the CE control exercises positive recommendations too"
        (any (\c -> Public.recommendationStatus c == Public.PositiveRecommendation) ceChecks)
    assert "the CE control exercises zero-mass entries" (0 `elem` supplied)
    -- Ordered, element-by-element agreement including the zero-mass and null rows.
    mapM_
        ( \(index, shadowRow, check) -> do
            assert
                ( "CE row "
                    ++ show index
                    ++ " recommendation must agree (shadow "
                    ++ show (shadowRecommendation shadowRow)
                    ++ " vs checker "
                    ++ show (Public.recommendationMass check)
                    ++ ")"
                )
                (shadowRecommendation shadowRow == Public.recommendationMass check)
            assert
                ( "CE row "
                    ++ show index
                    ++ " slack must agree (shadow "
                    ++ show (shadowSlack shadowRow)
                    ++ " vs checker "
                    ++ show (Public.obedienceSlack check)
                    ++ ")"
                )
                (shadowSlack shadowRow == Public.obedienceSlack check)
            assert
                ("CE row " ++ show index ++ " status must follow the shadow recommendation")
                ( Public.recommendationStatus check
                    == (if shadowRecommendation shadowRow == 0 then Public.NullRecommendation else Public.PositiveRecommendation)
                )
        )
        (zip3 [0 :: Int ..] ceShadow ceChecks)
    -- The same comparison for CCE, which has no recommendation folds.
    cceShadow <- case runSolve (coarseShadow tinySolve game entries (deviationLabels CoarseMode product_)) of
        Left fault -> ioError (userError ("row-sequence CCE shadow: " ++ show fault))
        Right (rows, _) -> pure rows
    cceReport <- case Public.checkCoarseCorrelatedEquilibrium tinyLimits game device of
        Left problem -> ioError (userError ("row-sequence CCE checker: " ++ show problem))
        Right value -> pure value
    let cceChecks = Public.coarseDeviationChecks cceReport
    assert
        "the CCE shadow and the real CCE checker produce the same row count"
        (naturalCount cceShadow == naturalCount cceChecks)
    mapM_
        ( \(index, shadowRow, check) ->
            assert
                ( "CCE row "
                    ++ show index
                    ++ " slack must agree (shadow "
                    ++ show (shadowSlack shadowRow)
                    ++ " vs checker "
                    ++ show (Public.coarseDeviationSlack check)
                    ++ ")"
                )
                (shadowSlack shadowRow == Public.coarseDeviationSlack check)
        )
        (zip3 [0 :: Int ..] cceShadow cceChecks)

{- | §11.8 and §11.9 competing-failure order, through the real reserved routes.

Covers two orderings the public fixtures do not isolate: within one admitted
block, work is reported before an observed 'Rational' would exceed its cap; and
the report-length gate fires before any constant 'Rational' observation, because
'admitGeometry' performs no observation at all.
-}
testCompetingFailures :: IO ()
testCompetingFailures = do
    -- Work fails before arithmetic. Both would fail: the block's work exceeds a
    -- tiny work cap, and B=1 could not admit a two-bit constant. Work wins,
    -- because the reservation precedes the observation.
    let tinyWork = correlationSolveLimits (gameLimits 64 64 64 4096 100 1 ceiling_) ceiling_ ceiling_ ceiling_
        blockThenObserve :: CorrelationSolveLimits -> Solve String ()
        blockThenObserve limits = do
            reserveBlock limits CorrelationConstraints 8
            observeRational limits CorrelationConstraints 1
    case runSolve (blockThenObserve tinyWork) of
        Left (SolveLimitFault phase resource cap required) ->
            assert
                "work is reported before the admitted block's Rational output"
                ( phase == CorrelationConstraints
                    && resource == CorrelationWork
                    && cap == 100
                    && required == 101
                )
        other -> ioError (userError ("work before arithmetic: " ++ describeThread other))
    -- With work and materialization admitted, the same route now reports the
    -- Rational failure on the constant one, whose combined size is two.
    let roomyWork = correlationSolveLimits (gameLimits 64 64 64 4096 ceiling_ 1 ceiling_) ceiling_ ceiling_ ceiling_
    case runSolve (blockThenObserve roomyWork) of
        Left (SolveLimitFault phase resource cap required) ->
            assert
                "with work admitted the same block reports the Rational failure"
                ( phase == CorrelationConstraints
                    && resource == CorrelationRationalBits
                    && cap == 1
                    && required == 2
                )
        other -> ioError (userError ("arithmetic after work: " ++ describeThread other))
    -- The report-length gate precedes constant observation: even with B=1, which
    -- cannot admit the constants zero or one, a q above the representational
    -- ceiling reports ReportLength, because admitGeometry observes nothing.
    let bitStarved = correlationSolveLimits (gameLimits 64 64 64 4096 ceiling_ 1 ceiling_) (machine + 5) ceiling_ ceiling_
    case runSolve (admitGeometry bitStarved CoarseMode 4 [machine + 1] :: Solve String (Natural, Natural)) of
        Left fault ->
            assert
                ("report length must precede constant observation, got " ++ show fault)
                (fault == SolveRepresentationFault CorrelationReportLength)
        Right value -> ioError (userError ("geometry admitted " ++ show value))

-- | A zero-payoff two-owner game, used by the row-sequence control.
zeroPayoffPair :: ExactNormalGame String String
zeroPayoffPair =
    buildGame
        [("Row", ["A", "B"]), ("Column", ["L", "R"])]
        [ ([("Row", a), ("Column", c)], [("Row", 0), ("Column", 0)])
        | a <- ["A", "B"]
        , c <- ["L", "R"]
        ]

enumerate :: Natural -> [Natural] -> [[Natural]]
enumerate bound = go
  where
    go current = current : maybe [] go (successorTuple bound current)
