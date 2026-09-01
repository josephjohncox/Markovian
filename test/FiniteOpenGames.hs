module FiniteOpenGames (runFiniteOpenGameTests) where

import Data.IORef
import Data.List (sort)
import Markovian.Category.Finite.Set
import Markovian.Category.Payoff.Exact
import Markovian.Game.Open.Exact
import Markovian.Game.Open.Finite
import Markovian.Game.Optic.Finite
import System.IO.Unsafe (unsafePerformIO)

runFiniteOpenGameTests :: (String -> IO () -> IO ()) -> IO ()
runFiniteOpenGameTests run = do
    run "finite functions check totality boundaries and pre-allocation limits" testFiniteFunctions
    run "finite function spaces include the empty function and 0^0 = 1" testFunctionCardinalities
    run "concrete finite optics satisfy checked fixture laws" testOpticLaws
    run "optic labelled equality is separate from represented layout equality" testOpticLayouts
    run "open-game construction checks complete tables and owner overlap" testConstruction
    run "open-game identity and associativity use explicit strategy bijections" testOpenIdentity
    run "exact payoff contexts reject carrier mismatch and preserve ties" testExactPayoffContext
    run "tensor play coplay and incumbent continuations are exact" testTensorFormula
    run "pure equilibrium fixtures cover dilemma coordination pennies and ties" testEquilibriumFixtures
    run "equilibrium enumeration is bounded before relation traversal" testEquilibriumBudgets
    run "sequential composition includes a non-credible-threat equilibrium" testSequentialThreat
    run "observational equality checks best-response membership" testBestResponseObservation
    run "strategy bijections reject owner-swapping whole-profile permutations" testOwnerSwapRejection
    run "open-game contexts canonicalize continuation layout" testContinuationCanonicalization
    run "open-game performed counts force all callbacks and result spines" testStrictPerformedCounts
    run "reordered carriers preserve labelled observations but not layouts" testReorderedObservation
    run "all represented binary two-player payoff tables match unilateral enumeration" testBinaryDifferential
    run "finite open-game reports are deterministic golden evidence" testGoldenReports

data Agent = Row | Column
    deriving (Eq, Ord, Show)

data Action = Cooperate | Defect
    deriving (Eq, Ord, Show)

data Entry = Out | In
    deriving (Eq, Ord, Show)

data Response = Fight | Accommodate
    deriving (Eq, Ord, Show)

largeFinite :: FiniteBudget
largeFinite = finiteBudget 1000000

largeEquilibrium :: EquilibriumBudget
largeEquilibrium = equilibriumBudget 1000000 1000000

set :: (Eq value, Show value) => [value] -> IO (FiniteSet value)
set values = right "finite set" (finiteSet values)

right :: (Show error) => String -> Either error value -> IO value
right _ (Right value) = pure value
right label (Left problem) = ioError (userError (label ++ ": " ++ show problem))

assert :: String -> Bool -> IO ()
assert message condition = if condition then pure () else ioError (userError message)

expectLeft :: String -> (error -> Bool) -> Either error value -> IO ()
expectLeft label predicate result = case result of
    Left problem -> assert (label ++ ": wrong error ") (predicate problem)
    Right _ -> ioError (userError (label ++ ": unexpectedly succeeded"))

unitSet :: IO (FiniteSet ())
unitSet = set [()]

testFiniteFunctions :: IO ()
testFiniteFunctions = do
    source <- set [False, True]
    target <- set [0 :: Int, 1]
    function <- right "function" (finiteFunction largeFinite source target [(True, 1), (False, 0)])
    assert "table was not canonicalized" (finiteFunctionEntries function == [(False, 0), (True, 1)])
    expectLeft "duplicate" duplicate (finiteFunction largeFinite source target [(False, 0), (False, 1)])
    expectLeft "missing" missing (finiteFunction largeFinite source target [(False, 0)])
    expectLeft "outside input" outsideInput (finiteFunction largeFinite source target [(False, 0), (True, 1), (False, 1)])
    expectLeft "outside output" outsideOutput (finiteFunction largeFinite source target [(False, 2), (True, 1)])
    expectLeft "work" work (finiteFunction (finiteBudget 1) source target [(False, 0), (True, 1)])
  where
    duplicate (DuplicateFunctionInput False) = True
    duplicate _ = False
    missing (MissingFunctionInput True) = True
    missing _ = False
    -- An excess list is rejected before unbounded duplicate inspection.
    outsideInput ExcessFunctionEntries = True
    outsideInput _ = False
    outsideOutput (FunctionOutputOutsideTarget False 2) = True
    outsideOutput _ = False
    work (FiniteFunctionWorkLimitExceeded 2 1) = True
    work _ = False

testFunctionCardinalities :: IO ()
testFunctionCardinalities = do
    empty <- set ([] :: [Bool])
    binary <- set [False, True]
    assert "0^0 convention" (functionSpaceCardinality empty empty == 1)
    assert "2^0 convention" (functionSpaceCardinality empty binary == 1)
    assert "0^2 convention" (functionSpaceCardinality binary empty == 0)
    assert "2^2 cardinality" (functionSpaceCardinality binary binary == 4)
    emptyFunctions <- right "empty functions" (enumerateFiniteFunctions (finiteBudget 1) empty empty)
    assert "empty function enumeration" (length emptyFunctions == 1 && maybe False ((== []) . finiteFunctionEntries) (firstMaybe emptyFunctions))
    expectLeft "function allocation" allocation (enumerateFiniteFunctions (finiteBudget 11) binary binary)
  where
    allocation (FiniteFunctionWorkLimitExceeded 12 11) = True
    allocation _ = False

testOpticLaws :: IO ()
testOpticLaws = do
    bits <- set [False, True]
    numbers <- set [0 :: Int, 1]
    chars <- set ['a', 'b']
    utilities <- set [10 :: Int, 20]
    first <- right "first optic" (finiteOptic largeFinite bits utilities numbers utilities [(False, 0), (True, 1)] [((x, r), if x then r else 10) | x <- [False, True], r <- [10, 20]])
    second <- right "second optic" (finiteOptic largeFinite numbers utilities chars bits [(0, 'a'), (1, 'b')] [((n, q), if q then 20 else 10) | n <- [0, 1], q <- [False, True]])
    identityLeft <- right "left optic identity" (identityFiniteOptic largeFinite bits utilities)
    identityRight <- right "right optic identity" (identityFiniteOptic largeFinite chars bits)
    leftComposed <- right "left composition" (composeFiniteOptic largeFinite identityLeft first)
    rightComposed <- right "right composition" (composeFiniteOptic largeFinite second identityRight)
    assert "optic left identity" (finiteOpticEquivalent leftComposed first)
    assert "optic right identity" (finiteOpticEquivalent rightComposed second)
    composed <- right "optic composition" (composeFiniteOptic largeFinite first second)
    assert "optic play formula" (opticPlay composed True == Just 'b')
    assert "optic coplay formula" (opticCoplay composed False True == Just 10)
    third <- right "third optic" (identityFiniteOptic largeFinite chars bits)
    firstSecond <- right "first then second" (composeFiniteOptic largeFinite first second)
    leftAssociated <- right "optic left associated" (composeFiniteOptic largeFinite firstSecond third)
    secondThird <- right "second then third" (composeFiniteOptic largeFinite second third)
    rightAssociated <- right "optic right associated" (composeFiniteOptic largeFinite first secondThird)
    assert "optic associativity fixture" (finiteOpticEquivalent leftAssociated rightAssociated)
    tensor <- right "optic tensor" (tensorFiniteOptic largeFinite first first)
    assert "tensor play" (opticPlay tensor (True, False) == Just (1, 0))
    assert "tensor coplay" (opticCoplay tensor (True, False) (10, 20) == Just (10, 10))

testOpticLayouts :: IO ()
testOpticLayouts = do
    normal <- set [False, True]
    reordered <- set [True, False]
    unit <- unitSet
    first <- right "normal optic" (finiteOptic largeFinite normal unit normal unit [(False, False), (True, True)] [((x, ()), ()) | x <- [False, True]])
    second <- right "reordered optic" (finiteOptic largeFinite reordered unit reordered unit [(False, False), (True, True)] [((x, ()), ()) | x <- [False, True]])
    assert "optic semantics changed" (finiteOpticEquivalent first second)
    assert "optic layout erased" (not (sameFiniteOpticLayout first second))

testConstruction :: IO ()
testConstruction = do
    unit <- unitSet
    actions <- set [Cooperate, Defect]
    strategies <- set [Cooperate, Defect]
    let schema = ownedStrategySchema Row strategies
    expectLeft
        "missing play"
        missingPlay
        (finiteOpenGame largeFinite schema unit unit actions unit [((Cooperate, ()), Cooperate)] [((strategy, (), ()), ()) | strategy <- [Cooperate, Defect]] (\_ _ _ _ -> True))
    utilities <- set [0, 1]
    row <- right "row decision" (exactMaximizingDecision largeFinite Row unit actions utilities)
    anotherRow <- right "another row decision" (exactMaximizingDecision largeFinite Row unit actions utilities)
    column <- right "column decision" (exactMaximizingDecision largeFinite Column unit actions utilities)
    expectLeft "duplicate owner" overlap (tensorOpenGame largeFinite row anotherRow)
    expectLeft "composition continuation budget" continuationBudget (tensorOpenGame (finiteBudget 1) row column)
  where
    missingPlay (OpenGamePlayTableError (MissingFunctionInput (Defect, ()))) = True
    missingPlay _ = False
    overlap (OverlappingStrategyOwner Row) = True
    overlap _ = False
    continuationBudget (OpenGameCompositionWorkLimitExceeded requiredWork 1) = requiredWork > 1
    continuationBudget _ = False

testOpenIdentity :: IO ()
testOpenIdentity = do
    unit <- unitSet
    actions <- set [Cooperate, Defect]
    utilities <- set [0, 1]
    game <- right "decision" (exactMaximizingDecision largeFinite Row unit actions utilities)
    identity <- right "identity" (identityOpenGame largeFinite unit unit)
    composed <- right "identity composition" (composeOpenGame largeFinite identity game)
    bijection <- right "unit bijection" (leftUnitStrategyBijection largeFinite (openGameStrategySchema game))
    report <- right "identity observation" (observationallyEqualUnder (observationBudget 100000) bijection composed game)
    assert "open identity" (observationEquivalent report)
    firstIdentity <- right "first associativity identity" (identityOpenGame largeFinite unit unit :: Either (FiniteOpenGameError () () () () ()) (FiniteOpenGame Agent () () () () ()))
    secondIdentity <- right "second associativity identity" (identityOpenGame largeFinite unit unit)
    thirdIdentity <- right "third associativity identity" (identityOpenGame largeFinite unit unit)
    firstSecond <- right "first identity pair" (composeOpenGame largeFinite firstIdentity secondIdentity)
    leftAssociated <- right "left associated identities" (composeOpenGame largeFinite firstSecond thirdIdentity)
    secondThird <- right "second identity pair" (composeOpenGame largeFinite secondIdentity thirdIdentity)
    rightAssociated <- right "right associated identities" (composeOpenGame largeFinite firstIdentity secondThird)
    let unitSchema = unitStrategySchema :: StrategySchema Agent ()
    associator <- right "strategy associator" (associatorStrategyBijection largeFinite unitSchema unitSchema unitSchema)
    associativityReport <- right "associativity observation" (observationallyEqualUnder (observationBudget 100) associator leftAssociated rightAssociated)
    assert "open associativity" (observationEquivalent associativityReport)

testExactPayoffContext :: IO ()
testExactPayoffContext = do
    unit <- unitSet
    actions <- set [Cooperate, Defect]
    utilities <- set [0, 1]
    game <- right "exact context decision" (exactMaximizingDecision largeFinite Row unit actions utilities)
    payoff <- right "exact payoff" (exactPayoff actions [(Cooperate, 1), (Defect, 0)])
    context <- right "exact payoff context" (contextFromExactPayoff largeFinite game () payoff)
    report <- right "exact payoff equilibrium" (enumeratePureEquilibria largeEquilibrium game context)
    assert "exact payoff maximizer" ([functionAt strategy () | strategy <- equilibriumProfiles report] == [Cooperate])
    outside <- right "outside exact payoff" (exactPayoff actions [(Cooperate, 2), (Defect, 0)])
    expectLeft "outside utility" outsideUtility (contextFromExactPayoff largeFinite game () outside)
    singletonAction <- set [Cooperate]
    wrongTarget <- right "wrong target payoff" (exactPayoff singletonAction [(Cooperate, 1)])
    expectLeft "payoff target" targetMismatch (contextFromExactPayoff largeFinite game () wrongTarget)
    singletonGame <- right "singleton decision" (exactMaximizingDecision largeFinite Row unit singletonAction utilities)
    wrongContext <- right "singleton context" (contextFromExactPayoff largeFinite singletonGame () wrongTarget)
    expectLeft "equilibrium context" equilibriumContext (enumeratePureEquilibria largeEquilibrium game wrongContext)
  where
    outsideUtility (ExactContextUtilityOutsideCarrier Cooperate 2) = True
    outsideUtility _ = False
    targetMismatch ExactContextPayoffTargetMismatch = True
    targetMismatch _ = False
    equilibriumContext EquilibriumContextMismatch = True
    equilibriumContext _ = False

testTensorFormula :: IO ()
testTensorFormula = do
    unit <- unitSet
    actions <- set [Cooperate, Defect]
    utilities <- set [0, 1]
    row <- right "row decision" (exactMaximizingDecision largeFinite Row unit actions utilities)
    column <- right "column decision" (exactMaximizingDecision largeFinite Column unit actions utilities)
    game <- right "tensor decisions" (tensorOpenGame largeFinite row column)
    let profile = required "first tensor profile" (firstMaybe (finiteSetValues (strategySchemaProfiles (openGameStrategySchema game))))
    assert "tensor play order" (playOpenGame game profile ((), ()) == Just (Cooperate, Cooperate))
    assert "tensor coplay order" (coplayOpenGame game profile ((), ()) (0, 1) == Just ((), ()))
    optic <- right "strategy optic" (strategyOptic largeFinite game profile)
    assert "strategy optic play" (opticPlay optic ((), ()) == Just (Cooperate, Cooperate))
    assert "strategy optic coplay" (opticCoplay optic ((), ()) (0, 1) == Just ((), ()))
    payoff <- continuation game [((Cooperate, Cooperate), (0, 1)), ((Cooperate, Defect), (0, 0)), ((Defect, Cooperate), (1, 1)), ((Defect, Defect), (1, 0))]
    context <- right "tensor context" (openGameContext game ((), ()) payoff)
    responses <- right "tensor responses" (enumerateBestResponses largeEquilibrium game context)
    assert "incumbent continuation did not produce responses" (not (null (equilibriumProfiles responses)))

testEquilibriumFixtures :: IO ()
testEquilibriumFixtures = do
    prisoner <- normalGame prisonerPayoff [-1, 0, 1, 2, 3]
    prisonerEquilibria <- equilibria prisoner
    assert "prisoner's dilemma equilibrium" (projectActions prisoner prisonerEquilibria == [(Defect, Defect)])
    coordination <- normalGame coordinationPayoff [0, 1]
    coordinationEquilibria <- equilibria coordination
    assert "coordination equilibria" (sort (projectActions coordination coordinationEquilibria) == [(Cooperate, Cooperate), (Defect, Defect)])
    pennies <- normalGame penniesPayoff [-1, 1]
    penniesEquilibria <- equilibria pennies
    assert "matching pennies has a pure equilibrium" (null (projectActions pennies penniesEquilibria))
    tied <- normalGame (const (0, 0)) [0]
    tiedEquilibria <- equilibria tied
    assert "exact ties were discarded" (projectActions tied tiedEquilibria == [(Cooperate, Cooperate), (Cooperate, Defect), (Defect, Cooperate), (Defect, Defect)])

testEquilibriumBudgets :: IO ()
testEquilibriumBudgets = do
    gameAndContext <- normalGame coordinationPayoff [0, 1]
    let (game, context) = gameAndContext
    expectLeft "profile budget" profileLimit (enumeratePureEquilibria (equilibriumBudget 3 100) game context)
    expectLeft "relation budget" relationLimit (enumerateBestResponses (equilibriumBudget 4 15) game context)
  where
    profileLimit (EquilibriumProfileLimitExceeded 4 3) = True
    profileLimit _ = False
    relationLimit (EquilibriumRelationLimitExceeded 16 15) = True
    relationLimit _ = False

testSequentialThreat :: IO ()
testSequentialThreat = do
    (game, context) <- entryGame
    report <- right "entry equilibria" (enumeratePureEquilibria largeEquilibrium game context)
    let outcomes = [(firstAction profile, secondAt In profile) | profile <- equilibriumProfiles report]
    assert "non-credible threat equilibrium absent" ((Out, Fight) `elem` outcomes)
    assert "credible entry equilibrium absent" ((In, Accommodate) `elem` outcomes)
    let independent = [profile | profile <- finiteSetValues (strategySchemaProfiles (openGameStrategySchema game)), sequentialNash profile]
    assert "sequential differential" (equilibriumProfiles report == independent)
  where
    firstAction (firstStrategy, _) = functionAt firstStrategy ()
    secondAt input (_, secondStrategy) = functionAt secondStrategy input
    payoff profile = entryPayoff (firstAction profile, secondAt (firstAction profile) profile)
    sequentialNash profile =
        let (_, secondStrategy) = profile
            firstBest = fst (payoff profile) >= fst (entryPayoff (otherEntry (firstAction profile), functionAt secondStrategy (otherEntry (firstAction profile))))
            current = firstAction profile
            secondBest = snd (payoff profile) >= snd (entryPayoff (current, otherResponse (functionAt secondStrategy current)))
         in firstBest && secondBest

testBestResponseObservation :: IO ()
testBestResponseObservation = do
    unit <- unitSet
    actions <- set [Cooperate, Defect]
    strategies <- set [Cooperate, Defect]
    let schema = ownedStrategySchema Row strategies
        playEntries = [((strategy, ()), strategy) | strategy <- [Cooperate, Defect]]
        coplayEntries = [((strategy, (), ()), ()) | strategy <- [Cooperate, Defect]]
    accepting <- right "accepting game" (finiteOpenGame largeFinite schema unit unit actions unit playEntries coplayEntries (\_ _ _ _ -> True))
    rejecting <- right "rejecting game" (finiteOpenGame largeFinite schema unit unit actions unit playEntries coplayEntries (\_ _ _ _ -> False))
    finiteIdentity <- right "identity strategies" (identityFiniteBijection largeFinite strategies)
    let bijection = ownedStrategyBijection Row finiteIdentity
    expectLeft "observation budget" observationLimit (observationallyEqualUnder (observationBudget 0) bijection accepting rejecting)
    observation <- right "response observation" (observationallyEqualUnder (observationBudget 1000) bijection accepting rejecting)
    assert "best responses were ignored" (not (observationEquivalent observation))
    continuationUnit <- right "unit continuation" (finiteFunction largeFinite actions unit [(Cooperate, ()), (Defect, ())])
    acceptingContext <- right "accepting context" (openGameContext accepting () continuationUnit)
    rejectingContext <- right "rejecting context" (openGameContext rejecting () continuationUnit)
    acceptingEquilibria <- right "accepting equilibria" (enumeratePureEquilibria largeEquilibrium accepting acceptingContext)
    rejectingEquilibria <- right "rejecting equilibria" (enumeratePureEquilibria largeEquilibrium rejecting rejectingContext)
    assert "equilibrium relation counterexample" (length (equilibriumProfiles acceptingEquilibria) == 2 && null (equilibriumProfiles rejectingEquilibria))
  where
    observationLimit (ObservationWorkLimitExceeded 11 0) = True
    observationLimit _ = False

testOwnerSwapRejection :: IO ()
testOwnerSwapRejection = do
    unit <- unitSet
    actions <- set [Cooperate, Defect]
    utilities <- set [0, 1]
    row <- right "owner-swap row" (exactMaximizingDecision largeFinite Row unit actions utilities)
    column <- right "owner-swap column" (exactMaximizingDecision largeFinite Column unit actions utilities)
    game <- right "owner-swap tensor" (tensorOpenGame largeFinite row column)
    witness <- right "owner-swap structural symmetry" (symmetryStrategyBijection largeFinite (openGameStrategySchema row) (openGameStrategySchema column))
    expectLeft
        "whole-profile owner swap"
        (== ObservationOwnerMismatch)
        (observationallyEqualUnder (observationBudget 100000) witness game game)

testContinuationCanonicalization :: IO ()
testContinuationCanonicalization = do
    unit <- unitSet
    actions <- set [Cooperate, Defect]
    reversedActions <- set [Defect, Cooperate]
    strategies <- set [Cooperate]
    let schema = ownedStrategySchema Row strategies
        evaluator _ suppliedContinuation _ _ = map fst (finiteFunctionEntries suppliedContinuation) == [Cooperate, Defect]
    game <- right "layout-adversarial game" (finiteOpenGame largeFinite schema unit unit actions unit [((Cooperate, ()), Cooperate)] [((Cooperate, (), ()), ())] evaluator)
    reorderedContinuation <- right "reordered continuation" (finiteFunction largeFinite reversedActions unit [(Defect, ()), (Cooperate, ())])
    context <- right "canonicalized context" (openGameContext game () reorderedContinuation)
    response <- right "layout-adversarial response" (bestResponse game context Cooperate Cooperate)
    assert "callback observed caller continuation order" response
    assert "stored continuation was not canonical" (map fst (finiteFunctionEntries (contextContinuation context)) == [Cooperate, Defect])

testStrictPerformedCounts :: IO ()
testStrictPerformedCounts = do
    unit <- unitSet
    actions <- set [Cooperate, Defect]
    singletonOutput <- set [Cooperate]
    leftCounter <- newIORef 0
    rightCounter <- newIORef 0
    let schema = ownedStrategySchema Row actions
        playEntries = [((strategy, ()), Cooperate) | strategy <- [Cooperate, Defect]]
        coplayEntries = [((strategy, (), ()), ()) | strategy <- [Cooperate, Defect]]
    game <- right "counting game" (finiteOpenGame largeFinite schema unit unit singletonOutput unit playEntries coplayEntries (countingResponse False leftCounter))
    disagreeing <- right "disagreeing counting game" (finiteOpenGame largeFinite schema unit unit singletonOutput unit playEntries coplayEntries (countingResponse True rightCounter))
    finiteIdentity <- right "counting identity" (identityFiniteBijection largeFinite actions)
    let witness = ownedStrategyBijection Row finiteIdentity
    report <- right "strict observation" (observationallyEqualUnder (observationBudget 1000) witness game disagreeing)
    leftCalls <- readIORef leftCounter
    rightCalls <- readIORef rightCounter
    assert ("observation short-circuited callbacks: " ++ show (observationPerformedBestResponseChecks report, leftCalls, rightCalls, observationEquivalent report)) (observationPerformedBestResponseChecks report == 4 && leftCalls == 4 && rightCalls == 4 && not (observationEquivalent report))

    writeIORef leftCounter 0
    continuationUnit <- right "counting continuation" (finiteFunction largeFinite singletonOutput unit [(Cooperate, ())])
    context <- right "counting context" (openGameContext game () continuationUnit)
    equilibriaReport <- right "strict equilibrium" (enumeratePureEquilibria largeEquilibrium game context)
    equilibriumCalls <- readIORef leftCounter
    assert "equilibrium result spine or callbacks remained lazy" (performedRelationChecks equilibriaReport == 2 && equilibriumCalls == 2 && length (equilibriumProfiles equilibriaReport) == 2)

countingResponse :: Bool -> IORef Int -> () -> FiniteFunction Action () -> Action -> Action -> Bool
countingResponse negateResult counter _ _ incumbent deviation = unsafePerformIO $ do
    modifyIORef' counter (+ 1)
    let same = incumbent == deviation
    pure (if negateResult then not same else same)
{-# NOINLINE countingResponse #-}

testReorderedObservation :: IO ()
testReorderedObservation = do
    unit <- unitSet
    normalActions <- set [Cooperate, Defect]
    reorderedActions <- set [Defect, Cooperate]
    utilities <- set [0]
    normal <- right "normal decision" (exactMaximizingDecision largeFinite Row unit normalActions utilities)
    reordered <- right "reordered decision" (exactMaximizingDecision largeFinite Row unit reorderedActions utilities)
    let normalStrategies = finiteSetValues (strategySchemaProfiles (openGameStrategySchema normal))
        reorderedStrategies = strategySchemaProfiles (openGameStrategySchema reordered)
        match strategy = required "reordered strategy match" (firstMaybe [candidate | candidate <- finiteSetValues reorderedStrategies, functionAt candidate () == functionAt strategy ()])
    finiteRelayout <- right "reordered bijection" (finiteBijection largeFinite (strategySchemaProfiles (openGameStrategySchema normal)) reorderedStrategies [(strategy, match strategy) | strategy <- normalStrategies])
    let bijection = ownedStrategyBijection Row finiteRelayout
    observation <- right "reordered observation" (observationallyEqualUnder (observationBudget 10000) bijection normal reordered)
    assert "reordered labelled semantics" (observationEquivalent observation)
    assert "reordered layout diagnostic" (not (sameOpenGameLayout normal reordered))

testBinaryDifferential :: IO ()
testBinaryDifferential = do
    let payoffVectors = sequences 8 [0, 1]
    mapM_ checkTable [splitAt 4 values | values <- payoffVectors]
  where
    outcomes = [(Cooperate, Cooperate), (Cooperate, Defect), (Defect, Cooperate), (Defect, Defect)]
    checkTable (rowValues, columnValues) = do
        let payoff outcome = (lookupRequired outcome (zip outcomes rowValues), lookupRequired outcome (zip outcomes columnValues))
        gameAndContext <- normalGame payoff [0, 1]
        report <- equilibria gameAndContext
        let actual = projectActions gameAndContext report
            expected = [outcome | outcome <- outcomes, unilateral payoff outcome]
            unilateral payoffTable outcome@(rowAction, columnAction) =
                fst (payoffTable outcome) >= fst (payoffTable (otherAction rowAction, columnAction))
                    && snd (payoffTable outcome) >= snd (payoffTable (rowAction, otherAction columnAction))
        assert ("binary payoff differential: " ++ show (rowValues, columnValues)) (actual == expected)

testGoldenReports :: IO ()
testGoldenReports = do
    prisoner@(prisonerGame, prisonerContext) <- normalGame prisonerPayoff [-1, 0, 1, 2, 3]
    prisonerReport <- equilibria prisoner
    pennies@(penniesGame, penniesContext) <- normalGame penniesPayoff [-1, 1]
    penniesReport <- equilibria pennies
    checkGolden "test/golden/open-game-prisoners-dilemma.txt" (renderEquilibriumReport prisonerGame prisonerContext prisonerReport)
    checkGolden "test/golden/open-game-matching-pennies.txt" (renderEquilibriumReport penniesGame penniesContext penniesReport)

normalGame ::
    ((Action, Action) -> (Rational, Rational)) ->
    [Rational] ->
    IO
        ( FiniteOpenGame Agent (FiniteFunction () Action, FiniteFunction () Action) ((), ()) ((), ()) (Action, Action) (Rational, Rational)
        , OpenGameContext ((), ()) (Action, Action) (Rational, Rational)
        )
normalGame payoff utilityValues = do
    unit <- unitSet
    actions <- set [Cooperate, Defect]
    utilities <- set utilityValues
    row <- right "row exact decision" (exactMaximizingDecision largeFinite Row unit actions utilities)
    column <- right "column exact decision" (exactMaximizingDecision largeFinite Column unit actions utilities)
    game <- right "normal tensor" (tensorOpenGame largeFinite row column)
    payoffFunction <- continuation game [(outcome, payoff outcome) | outcome <- [(Cooperate, Cooperate), (Cooperate, Defect), (Defect, Cooperate), (Defect, Defect)]]
    context <- right "normal context" (openGameContext game ((), ()) payoffFunction)
    pure (game, context)

continuation ::
    (Eq strategy, Eq x, Eq s, Eq y, Eq r, Show y, Show r) =>
    FiniteOpenGame owner strategy x s y r ->
    [(y, r)] ->
    IO (FiniteFunction y r)
continuation game = right "continuation" . finiteFunction largeFinite (openGameTarget game) (openGameUtility game)

equilibria ::
    (Eq strategy, Eq x, Eq y, Eq r, Show strategy) =>
    (FiniteOpenGame owner strategy x s y r, OpenGameContext x y r) ->
    IO (EquilibriumReport strategy)
equilibria (game, context) = right "equilibria" (enumeratePureEquilibria largeEquilibrium game context)

projectActions ::
    ( FiniteOpenGame Agent (FiniteFunction () Action, FiniteFunction () Action) ((), ()) ((), ()) (Action, Action) (Rational, Rational)
    , OpenGameContext ((), ()) (Action, Action) (Rational, Rational)
    ) ->
    EquilibriumReport (FiniteFunction () Action, FiniteFunction () Action) ->
    [(Action, Action)]
projectActions (game, _) report = [required "project play" (playOpenGame game profile ((), ())) | profile <- equilibriumProfiles report]

entryGame ::
    IO
        ( FiniteOpenGame Agent (FiniteFunction () Entry, FiniteFunction Entry Response) () () (Entry, Response) (Rational, Rational)
        , OpenGameContext () (Entry, Response) (Rational, Rational)
        )
entryGame = do
    unit <- unitSet
    entries <- set [Out, In]
    responses <- set [Fight, Accommodate]
    firstUtilities <- set [0, 1, 2]
    first <- right "entry first decision" (exactMaximizingDecision largeFinite Row unit entries firstUtilities)
    secondStrategiesList <- right "second strategies" (enumerateFiniteFunctions largeFinite entries responses)
    secondStrategies <- set secondStrategiesList
    firstPayoffs <- set [0, 1, 2]
    pairPayoffs <- set [(0, 0), (1, 0), (2, 1)]
    pathOutputs <- set [(entry, response) | entry <- [Out, In], response <- [Fight, Accommodate]]
    let schema = ownedStrategySchema Column secondStrategies
        playEntries = [((strategy, entry), (entry, functionAt strategy entry)) | strategy <- secondStrategiesList, entry <- [Out, In]]
        coplayEntries = [((strategy, entry, payoff), fst payoff) | strategy <- secondStrategiesList, entry <- [Out, In], payoff <- finiteSetValues pairPayoffs]
        response entry payoffContinuation _incumbent deviation =
            let value strategy = snd (functionAt payoffContinuation (entry, functionAt strategy entry))
                deviationValue = value deviation
             in all ((deviationValue >=) . value) secondStrategiesList
    second <- right "entry second game" (finiteOpenGame largeFinite schema entries firstPayoffs pathOutputs pairPayoffs playEntries coplayEntries response)
    game <- right "entry composition" (composeOpenGame largeFinite first second)
    payoffFunction <- continuation game [(path, entryPayoff path) | path <- finiteSetValues pathOutputs]
    context <- right "entry context" (openGameContext game () payoffFunction)
    pure (game, context)

prisonerPayoff :: (Action, Action) -> (Rational, Rational)
prisonerPayoff (Cooperate, Cooperate) = (2, 2)
prisonerPayoff (Cooperate, Defect) = (-1, 3)
prisonerPayoff (Defect, Cooperate) = (3, -1)
prisonerPayoff (Defect, Defect) = (0, 0)

coordinationPayoff :: (Action, Action) -> (Rational, Rational)
coordinationPayoff (Cooperate, Cooperate) = (1, 1)
coordinationPayoff (Defect, Defect) = (1, 1)
coordinationPayoff _ = (0, 0)

penniesPayoff :: (Action, Action) -> (Rational, Rational)
penniesPayoff (Cooperate, Cooperate) = (1, -1)
penniesPayoff (Defect, Defect) = (1, -1)
penniesPayoff _ = (-1, 1)

entryPayoff :: (Entry, Response) -> (Rational, Rational)
entryPayoff (Out, _) = (1, 0)
entryPayoff (In, Fight) = (0, 0)
entryPayoff (In, Accommodate) = (2, 1)

otherAction :: Action -> Action
otherAction Cooperate = Defect
otherAction Defect = Cooperate

otherEntry :: Entry -> Entry
otherEntry Out = In
otherEntry In = Out

otherResponse :: Response -> Response
otherResponse Fight = Accommodate
otherResponse Accommodate = Fight

functionAt :: (Eq input) => FiniteFunction input output -> input -> output
functionAt function input = required "function application" (applyFiniteFunction function input)

required :: String -> Maybe value -> value
required _ (Just value) = value
required label Nothing = error ("test invariant: " ++ label)

lookupRequired :: (Eq key) => key -> [(key, value)] -> value
lookupRequired key entries = required "lookup" (lookup key entries)

firstMaybe :: [value] -> Maybe value
firstMaybe [] = Nothing
firstMaybe (value : _) = Just value

sequences :: Int -> [value] -> [[value]]
sequences 0 _ = [[]]
sequences amount values = [value : remaining | value <- values, remaining <- sequences (amount - 1) values]

checkGolden :: FilePath -> String -> IO ()
checkGolden path actual = do
    expected <- readFile path
    assert (path ++ " changed") (actual == expected)
