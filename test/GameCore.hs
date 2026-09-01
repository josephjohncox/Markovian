module GameCore (runGameCoreTests) where

import Data.Maybe (isNothing)
import Markovian.Category.Finite.Object
import Markovian.Category.Finite.Set
import Markovian.Game.Arena hiding (arena, arenaEquivalent, legalHistory, sameArenaLayout)
import Markovian.Game.Arena qualified as Arena
import Markovian.Game.Strategy hiding (legalPlay, observationallyEqual, sameStrategyLayout)
import Markovian.Game.Strategy qualified as Strategy

runGameCoreTests :: (String -> IO () -> IO ()) -> IO ()
runGameCoreTests run = do
    run "game arena rejects an unknown initial position" testUnknownInitial
    run "game arena rejects missing duplicate and outside move edges" testMoveEdgeErrors
    run "game arena rejects outside edge endpoints and unreachable decoration" testReachabilityErrors
    run "game arena rejects mixed ownership and Player-owned initial turns" testOwnershipErrors
    run "game arena rejects nonalternation and returns a stable cycle witness" testAlternationAndCycle
    run "arena validation and equality have exact atomic work bounds" testArenaBounds
    run "legal histories replay and reject wrong-source and post-terminal moves" testHistoryReplay
    run "arena semantic equality is separate from represented layout equality" testArenaLayouts
    run "domain ownership reverses while codomain ownership is retained" testRoleReversal
    run "strategy construction checks empty play duplicates and prefix closure" testStrategySetErrors
    run "strategy construction enforces receptivity and one total Player response" testStrategyResponseErrors
    run "strategy construction is explicitly bounded" testStrategyBudget
    run "strategy validation uses one exact operation-wide work bound" testStrategyExactWork
    run "copycat is receptive deterministic and mirrors endpoint projections" testCopycat
    run "copycat is a left and right observational identity on a finite fixture" testCopycatIdentities
    run "composition hides synchronized middle moves" testHiddenMiddle
    run "valid strategies can be incompatible after hiding" testHiddenDeadlock
    run "composition accepts labelled-equivalent reordered middle layouts" testReorderedMiddleComposition
    run "representative composition is associative observationally" testAssociativity
    run "observational equality is an equivalence and a composition congruence" testObservationalEquality
    run "strategy equality has an exact atomic work bound" testStrategyEqualityBound
    run "strategy layout order is distinct from observational equality" testStrategyLayouts
    run "composition reports are deterministic" testDeterministicReport
    run "composition uses one exact operation-wide work bound" testCompositionExactWork
    run "composition rejects incompatible middles and budget exhaustion atomically" testCompositionErrors

-- A terminal arena is the protocol unit used by the fixtures.
data UPos = U
    deriving (Eq, Show)

data UMove = NoMove
    deriving (Eq, Show)

data QPos = Ready | Waiting | DoneYes | DoneNo
    deriving (Eq, Show)

data QMove = Ask | Yes | No | Bogus
    deriving (Eq, Show)

data CyclePos = CycleO | CycleP
    deriving (Eq, Show)

data CycleMove = ToP | ToO
    deriving (Eq, Show)

data EndPos = EndReady | EndDone
    deriving (Eq, Show)

data LeftMove = LeftRequest
    deriving (Eq, Show)

data RightMove = RightRequest
    deriving (Eq, Show)

data DagPos = DagRoot | DagLeft | DagRight | DagJoin | DagDone
    deriving (Eq, Show)

data DagMove = BranchLeft | BranchRight | JoinLeft | JoinRight | FinishDag
    deriving (Eq, Show)

largeBudget :: EnumerationBudget
largeBudget = enumerationBudget 100000 100000

largeArenaBudget :: ArenaBudget
largeArenaBudget = arenaBudget 100000

arena :: (Eq position, Eq move) => FiniteObject position -> FiniteSet move -> position -> [ArenaEdge position move] -> Either (ArenaError position move) (Arena position move)
arena = Arena.arena largeArenaBudget

legalHistory :: (Eq position, Eq move) => Arena position move -> [move] -> Either (HistoryError position move) (LegalHistory position move)
legalHistory = Arena.legalHistory (replayBudget 100000)

legalPlay :: (Eq domainPosition, Eq domainMove, Eq codomainPosition, Eq codomainMove) => Arena domainPosition domainMove -> Arena codomainPosition codomainMove -> [BoundaryMove domainMove codomainMove] -> Either (PlayError domainPosition domainMove codomainPosition codomainMove) (LegalPlay domainPosition domainMove codomainPosition codomainMove)
legalPlay = Strategy.legalPlay largeBudget

arenaEquivalent :: (Eq position, Eq move) => Arena position move -> Arena position move -> Bool
arenaEquivalent left other = Arena.arenaEquivalent largeArenaBudget left other == Right True

sameArenaLayout :: (Eq position, Eq move) => Arena position move -> Arena position move -> Bool
sameArenaLayout left other = Arena.sameArenaLayout largeArenaBudget left other == Right True

observationallyEqual :: (Eq aPosition, Eq aMove, Eq bPosition, Eq bMove) => Strategy aPosition aMove bPosition bMove -> Strategy aPosition aMove bPosition bMove -> Bool
observationallyEqual left other = Strategy.observationallyEqual largeBudget left other == Right True

sameStrategyLayout :: (Eq aPosition, Eq aMove, Eq bPosition, Eq bMove) => Strategy aPosition aMove bPosition bMove -> Strategy aPosition aMove bPosition bMove -> Bool
sameStrategyLayout left other = Strategy.sameStrategyLayout largeBudget left other == Right True

unitArena :: IO (Arena UPos UMove)
unitArena = do
    positions <- object [U]
    moves <- set []
    right "unit arena" (arena positions moves U [])

questionArena :: IO (Arena QPos QMove)
questionArena = questionArenaWith [Ready, Waiting, DoneYes, DoneNo] [Ask, Yes, No]

questionArenaWith :: [QPos] -> [QMove] -> IO (Arena QPos QMove)
questionArenaWith positionLayout moveLayout = do
    positions <- object positionLayout
    moves <- set moveLayout
    right
        "question arena"
        ( arena
            positions
            moves
            Ready
            [ ArenaEdge Ask Opponent Ready Waiting
            , ArenaEdge Yes Player Waiting DoneYes
            , ArenaEdge No Player Waiting DoneNo
            ]
        )

selectedYes :: IO (Strategy UPos UMove QPos QMove)
selectedYes = do
    unit <- unitArena
    question <- questionArena
    right
        "yes strategy"
        ( strategy
            largeBudget
            unit
            question
            [ []
            , [CodomainMove Ask]
            , [CodomainMove Ask, CodomainMove Yes]
            ]
        )

object :: (Eq value, Show value) => [value] -> IO (FiniteObject value)
object = right "finite object" . finiteObject

set :: (Eq value, Show value) => [value] -> IO (FiniteSet value)
set = right "finite set" . finiteSet

right :: (Show error) => String -> Either error value -> IO value
right _ (Right value) = pure value
right label (Left problem) = ioError (userError (label ++ ": " ++ show problem))

assert :: String -> Bool -> IO ()
assert message condition = if condition then pure () else ioError (userError message)

expectLeft :: String -> (error -> Bool) -> Either error value -> IO ()
expectLeft label predicate result = case result of
    Left problem -> assert (label ++ ": wrong error") (predicate problem)
    Right _ -> ioError (userError (label ++ ": unexpectedly succeeded"))

-- Arena has no Show instance by design, so construction rejection uses a separate helper.
expectArenaLeft :: String -> (ArenaError position move -> Bool) -> Either (ArenaError position move) (Arena position move) -> IO ()
expectArenaLeft label predicate result = case result of
    Left problem -> assert (label ++ ": wrong error") (predicate problem)
    Right _ -> ioError (userError (label ++ ": unexpectedly succeeded"))

testUnknownInitial :: IO ()
testUnknownInitial = do
    positions <- object [Ready]
    moves <- set ([] :: [QMove])
    expectArenaLeft "initial" (== InitialPositionOutsideArena Waiting) (arena positions moves Waiting [])

testMoveEdgeErrors :: IO ()
testMoveEdgeErrors = do
    positions <- object [Ready, Waiting, DoneYes]
    moves <- set [Ask, Yes]
    expectArenaLeft "missing" (== MissingMoveEdge Yes) (arena positions moves Ready [ArenaEdge Ask Opponent Ready Waiting])
    expectArenaLeft
        "duplicate"
        (== DuplicateMoveEdge Ask)
        (arena positions moves Ready [ArenaEdge Ask Opponent Ready Waiting, ArenaEdge Ask Opponent Ready Waiting, ArenaEdge Yes Player Waiting DoneYes])
    onlyAsk <- set [Ask]
    expectArenaLeft
        "outside move"
        (== EdgeMoveOutsideArena Yes)
        (arena positions onlyAsk Ready [ArenaEdge Ask Opponent Ready Waiting, ArenaEdge Yes Player Waiting DoneYes])
    twoPositions <- object [Ready, Waiting]
    expectArenaLeft
        "infinite edge input"
        (== DuplicateMoveEdge Ask)
        (arena twoPositions onlyAsk Ready (repeat (ArenaEdge Ask Opponent Ready Waiting)))

testReachabilityErrors :: IO ()
testReachabilityErrors = do
    positions <- object [Ready, Waiting]
    moves <- set [Ask]
    expectArenaLeft "outside source" (== EdgeSourceOutsideArena Ask DoneNo) (arena positions moves Ready [ArenaEdge Ask Opponent DoneNo Waiting])
    expectArenaLeft "outside target" (== EdgeTargetOutsideArena Ask DoneNo) (arena positions moves Ready [ArenaEdge Ask Opponent Ready DoneNo])
    decorated <- object [Ready, Waiting, DoneNo]
    expectArenaLeft "unreachable" (== UnreachablePosition DoneNo) (arena decorated moves Ready [ArenaEdge Ask Opponent Ready Waiting])

testOwnershipErrors :: IO ()
testOwnershipErrors = do
    positions <- object [Ready, Waiting, DoneYes]
    moves <- set [Ask, Yes]
    expectArenaLeft
        "mixed"
        mixed
        (arena positions moves Ready [ArenaEdge Ask Opponent Ready Waiting, ArenaEdge Yes Player Ready DoneYes])
    oneMove <- set [Ask]
    twoPositions <- object [Ready, Waiting]
    expectArenaLeft
        "initial Player"
        (== PlayerOwnedInitialPosition Ready)
        (arena twoPositions oneMove Ready [ArenaEdge Ask Player Ready Waiting])
  where
    mixed (MixedOwnershipAtPosition Ready Opponent Player) = True
    mixed _ = False

testAlternationAndCycle :: IO ()
testAlternationAndCycle = do
    positions <- object [Ready, Waiting, DoneYes]
    moves <- set [Ask, Yes]
    expectArenaLeft
        "nonalternating"
        (== NonAlternatingEdge Ask Opponent Opponent)
        (arena positions moves Ready [ArenaEdge Ask Opponent Ready Waiting, ArenaEdge Yes Opponent Waiting DoneYes])
    cyclePositions <- object [CycleO, CycleP]
    cycleMoves <- set [ToP, ToO]
    let cyclic = arena cyclePositions cycleMoves CycleO [ArenaEdge ToP Opponent CycleO CycleP, ArenaEdge ToO Player CycleP CycleO]
    expectArenaLeft "cycle" (== DirectedCycle [CycleO, CycleP, CycleO]) cyclic

testArenaBounds :: IO ()
testArenaBounds = do
    positions <- object [Ready, Waiting, DoneYes, DoneNo]
    moves <- set [Ask, Yes, No]
    let edges = [ArenaEdge Ask Opponent Ready Waiting, ArenaEdge Yes Player Waiting DoneYes, ArenaEdge No Player Waiting DoneNo]
    _ <- right "exact arena work boundary" (Arena.arena (arenaBudget 38) positions moves Ready edges)
    expectArenaLeft
        "arena one below"
        (== ArenaWorkLimitExceeded 38 37)
        (Arena.arena (arenaBudget 37) positions moves Ready edges)
    normal <- questionArena
    reordered <- questionArenaWith [DoneNo, Waiting, Ready, DoneYes] [No, Ask, Yes]
    assert "exact arena equality boundary" (Arena.arenaEquivalent (arenaBudget 35) normal reordered == Right True)
    assert "arena equality one below was not atomic" (Arena.arenaEquivalent (arenaBudget 34) normal reordered == Left (ArenaComparisonWorkLimitExceeded 35 34))

    dagPositions <- object [DagRoot, DagLeft, DagRight, DagJoin, DagDone]
    dagMoves <- set [BranchLeft, BranchRight, JoinLeft, JoinRight, FinishDag]
    _ <-
        right
            "shared-suffix DAG fanout"
            ( Arena.arena
                (arenaBudget 76)
                dagPositions
                dagMoves
                DagRoot
                [ ArenaEdge BranchLeft Opponent DagRoot DagLeft
                , ArenaEdge BranchRight Opponent DagRoot DagRight
                , ArenaEdge JoinLeft Player DagLeft DagJoin
                , ArenaEdge JoinRight Player DagRight DagJoin
                , ArenaEdge FinishDag Opponent DagJoin DagDone
                ]
            )
    pure ()

testHistoryReplay :: IO ()
testHistoryReplay = do
    checked <- questionArena
    history <- right "history" (legalHistory checked [Ask, Yes])
    assert "history move replay" (historyMoves history == [Ask, Yes])
    assert "history endpoint" (historyPosition history == DoneYes)
    assert "terminal history" (historyTerminal history && isNothing (historyNextOwner history))
    expectLeft "unknown move" unknownMove (legalHistory checked [Bogus])
    expectLeft "wrong source" wrongSource (legalHistory checked [Yes])
    expectLeft "post terminal" postTerminal (legalHistory checked [Ask, Yes, No])
  where
    unknownMove (UnknownHistoryMove Bogus) = True
    unknownMove _ = False
    wrongSource (MoveFromWrongPosition Yes Ready Waiting) = True
    wrongSource _ = False
    postTerminal (MoveAfterTerminal No DoneYes) = True
    postTerminal _ = False

testArenaLayouts :: IO ()
testArenaLayouts = do
    normal <- questionArena
    reordered <- questionArenaWith [DoneNo, Waiting, Ready, DoneYes] [No, Ask, Yes]
    assert "labelled arenas differ" (arenaEquivalent normal reordered)
    assert "layouts were collapsed" (not (sameArenaLayout normal reordered))

testRoleReversal :: IO ()
testRoleReversal = do
    checked <- questionArena
    unit <- unitArena
    expectLeft "reversed domain" wrongRole (legalPlay checked unit [DomainMove Ask])
    play <- right "codomain role" (legalPlay unit checked [CodomainMove Ask])
    assert "codomain first move" (playNextOwner play == Player)
  where
    wrongRole (MoveOwnedByWrongRole (DomainMove Ask) Opponent Player) = True
    wrongRole _ = False

testStrategySetErrors :: IO ()
testStrategySetErrors = do
    unit <- unitArena
    checked <- questionArena
    expectLeft "empty" (== MissingEmptyPlay) (strategy largeBudget unit checked [[CodomainMove Ask]])
    expectLeft "duplicate" duplicate (strategy largeBudget unit checked [[], []])
    expectLeft
        "prefix"
        prefix
        (strategy largeBudget unit checked [[], [CodomainMove Ask, CodomainMove Yes]])
    expectLeft
        "illegal play"
        illegal
        (strategy largeBudget unit checked [[], [CodomainMove Bogus]])
  where
    duplicate (DuplicateStrategyPlay []) = True
    duplicate _ = False
    prefix (MissingPlayPrefix [CodomainMove Ask]) = True
    prefix _ = False
    illegal (IllegalStrategyPlay [CodomainMove Bogus] (CodomainHistoryError (UnknownHistoryMove Bogus))) = True
    illegal _ = False

testStrategyResponseErrors :: IO ()
testStrategyResponseErrors = do
    unit <- unitArena
    checked <- questionArena
    expectLeft "receptivity" omitted (strategy largeBudget unit checked [[]])
    expectLeft
        "missing response"
        missing
        (strategy largeBudget unit checked [[], [CodomainMove Ask]])
    expectLeft
        "multiple response"
        multiple
        ( strategy
            largeBudget
            unit
            checked
            [ []
            , [CodomainMove Ask]
            , [CodomainMove Ask, CodomainMove Yes]
            , [CodomainMove Ask, CodomainMove No]
            ]
        )
  where
    omitted (OmittedOpponentExtension [] (CodomainMove Ask)) = True
    omitted _ = False
    missing (MissingPlayerResponse [CodomainMove Ask]) = True
    missing _ = False
    multiple (MultiplePlayerResponses [CodomainMove Ask] 2) = True
    multiple _ = False

testStrategyBudget :: IO ()
testStrategyBudget = do
    unit <- unitArena
    checked <- questionArena
    expectLeft
        "result budget"
        resultBudget
        (strategy (enumerationBudget 100 2) unit checked [[], [CodomainMove Ask], [CodomainMove Ask, CodomainMove Yes]])
    expectLeft
        "work budget"
        workBudget
        (strategy (enumerationBudget 2 100) unit checked [[], [CodomainMove Ask], [CodomainMove Ask, CodomainMove Yes]])
    expectLeft
        "infinite represented list"
        resultBudget
        (strategy (enumerationBudget 100 2) unit checked (repeat []))
  where
    resultBudget (StrategyBudgetError (EmittedResultBudgetExhausted 2)) = True
    resultBudget _ = False
    workBudget (StrategyBudgetError (ExploredItemBudgetExhausted 2)) = True
    workBudget _ = False

testStrategyExactWork :: IO ()
testStrategyExactWork = do
    unit <- unitArena
    checked <- questionArena
    selected <- selectedYes
    let work = strategyCheckedWork selected
        moves = [[], [CodomainMove Ask], [CodomainMove Ask, CodomainMove Yes]]
    exact <- right "exact strategy work" (strategy (enumerationBudget work 3) unit checked moves)
    assert "strategy exact work changed" (strategyCheckedWork exact == work)
    expectLeft
        "strategy one below"
        (== StrategyBudgetError (ExploredItemBudgetExhausted (work - 1)))
        (strategy (enumerationBudget (work - 1) 3) unit checked moves)

testCopycat :: IO ()
testCopycat = do
    checked <- questionArena
    identity <- right "copycat" (copycatStrategy largeBudget checked)
    let plays = strategyPlays identity
    assert "copycat empty" ([] `elem` map playMoves plays)
    mapM_ checkPlayerMirror plays
  where
    checkPlayerMirror play =
        if playNextOwner play == Opponent || playTerminal play
            then pure ()
            else do
                assert "copycat Player has a legal response" (not (null (playExtensions play)))
                assert "copycat reaches equality after a Player response" (any mirrors [next | extension <- playExtensions play, Right next <- [extendPlay play extension]])
    mirrors play = domainProjection play == codomainProjection play

testCopycatIdentities :: IO ()
testCopycatIdentities = do
    unit <- unitArena
    checked <- questionArena
    selected <- selectedYes
    unitIdentity <- right "unit copycat" (copycatStrategy largeBudget unit)
    questionIdentity <- right "question copycat" (copycatStrategy largeBudget checked)
    (leftIdentity, _) <- right "left identity" (composeStrategy largeBudget unitIdentity selected)
    (rightIdentity, _) <- right "right identity" (composeStrategy largeBudget selected questionIdentity)
    assert "left identity" (observationallyEqual leftIdentity selected)
    assert "right identity" (observationallyEqual rightIdentity selected)

testHiddenMiddle :: IO ()
testHiddenMiddle = do
    checked <- questionArena
    selected <- selectedYes
    identity <- right "copycat" (copycatStrategy largeBudget checked)
    (composed, report) <- right "hidden composition" (composeStrategy largeBudget selected identity)
    assert "middle moves visible" (all (onlyExternal . playMoves) (strategyPlays composed))
    assert "no hidden work was reported" (hiddenMiddleStepsConsidered report > 0)
  where
    onlyExternal = all isCodomain
    isCodomain (CodomainMove _) = True
    isCodomain (DomainMove _) = False

testHiddenDeadlock :: IO ()
testHiddenDeadlock = do
    leftPositions <- object [EndReady, EndDone]
    leftMoves <- set [LeftRequest]
    leftArena <- right "deadlock left arena" (arena leftPositions leftMoves EndReady [ArenaEdge LeftRequest Opponent EndReady EndDone])
    middle <- questionArena
    rightPositions <- object [EndReady, EndDone]
    rightMoves <- set [RightRequest]
    rightArena <- right "deadlock right arena" (arena rightPositions rightMoves EndReady [ArenaEdge RightRequest Opponent EndReady EndDone])
    leftStrategy <-
        right
            "deadlock left strategy"
            ( strategy
                largeBudget
                leftArena
                middle
                [ []
                , [CodomainMove Ask]
                , [CodomainMove Ask, CodomainMove Yes]
                ]
            )
    rightStrategy <-
        right
            "deadlock right strategy"
            ( strategy
                largeBudget
                middle
                rightArena
                [ []
                , [CodomainMove RightRequest]
                , [CodomainMove RightRequest, DomainMove Ask]
                , [CodomainMove RightRequest, DomainMove Ask, DomainMove Yes]
                , [CodomainMove RightRequest, DomainMove Ask, DomainMove No]
                ]
            )
    expectLeft
        "hidden Player deadlock"
        (== HiddenInternalDeadlock [CodomainMove RightRequest])
        (composeStrategy largeBudget leftStrategy rightStrategy)

testReorderedMiddleComposition :: IO ()
testReorderedMiddleComposition = do
    selected <- selectedYes
    reordered <- questionArenaWith [DoneNo, Waiting, Ready, DoneYes] [No, Ask, Yes]
    reorderedIdentity <- right "reordered copycat" (copycatStrategy largeBudget reordered)
    (composed, _) <- right "reordered middle composition" (composeStrategy largeBudget selected reorderedIdentity)
    assert "labelled-equivalent middle was rejected" (observationallyEqual composed selected)
    assert "represented endpoint layout was erased" (not (sameStrategyLayout composed selected))

testAssociativity :: IO ()
testAssociativity = do
    checked <- questionArena
    selected <- selectedYes
    firstIdentity <- right "copycat 1" (copycatStrategy largeBudget checked)
    secondIdentity <- right "copycat 2" (copycatStrategy largeBudget checked)
    (selectedThenFirst, _) <- right "selected;first" (composeStrategy largeBudget selected firstIdentity)
    (leftAssociated, _) <- right "left associated" (composeStrategy largeBudget selectedThenFirst secondIdentity)
    (identities, _) <- right "identities" (composeStrategy largeBudget firstIdentity secondIdentity)
    (rightAssociated, _) <- right "right associated" (composeStrategy largeBudget selected identities)
    assert "representative associativity" (observationallyEqual leftAssociated rightAssociated)

testObservationalEquality :: IO ()
testObservationalEquality = do
    selected <- selectedYes
    checked <- questionArena
    identity <- right "copycat" (copycatStrategy largeBudget checked)
    (once, _) <- right "once" (composeStrategy largeBudget selected identity)
    (twice, _) <- right "twice" (composeStrategy largeBudget once identity)
    assert "reflexivity" (observationallyEqual selected selected)
    assert "symmetry" (observationallyEqual selected once && observationallyEqual once selected)
    assert "transitivity" (observationallyEqual selected once && observationallyEqual once twice && observationallyEqual selected twice)
    (leftCongruent, _) <- right "left congruent" (composeStrategy largeBudget once identity)
    (rightCongruent, _) <- right "right congruent" (composeStrategy largeBudget selected identity)
    assert "congruence" (observationallyEqual leftCongruent rightCongruent)

testStrategyEqualityBound :: IO ()
testStrategyEqualityBound = do
    selected <- selectedYes
    assert "exact strategy equality boundary" (Strategy.observationallyEqual (enumerationBudget 91 0) selected selected == Right True)
    assert "strategy equality one below was not atomic" (Strategy.observationallyEqual (enumerationBudget 90 0) selected selected == Left (ExploredItemBudgetExhausted 90))

testStrategyLayouts :: IO ()
testStrategyLayouts = do
    unit <- unitArena
    checked <- questionArena
    first <- selectedYes
    second <-
        right
            "reordered strategy"
            ( strategy
                largeBudget
                unit
                checked
                [ [CodomainMove Ask, CodomainMove Yes]
                , []
                , [CodomainMove Ask]
                ]
            )
    assert "play set changed" (observationallyEqual first second)
    assert "play layout collapsed" (not (sameStrategyLayout first second))

testDeterministicReport :: IO ()
testDeterministicReport = do
    checked <- questionArena
    selected <- selectedYes
    identity <- right "copycat" (copycatStrategy largeBudget checked)
    first <- right "first composition" (composeStrategy largeBudget selected identity)
    second <- right "second composition" (composeStrategy largeBudget selected identity)
    assert "reports differ" (snd first == snd second)
    assert "strategies differ" (observationallyEqual (fst first) (fst second))

testCompositionExactWork :: IO ()
testCompositionExactWork = do
    checked <- questionArena
    selected <- selectedYes
    identity <- right "exact-work copycat" (copycatStrategy largeBudget checked)
    (_, report) <- right "measure composition work" (composeStrategy largeBudget selected identity)
    let work = exploredInteractionHistories report
    _ <- right "exact composition work" (composeStrategy (enumerationBudget work 100000) selected identity)
    expectLeft
        "composition one below"
        (== CompositionBudgetError (ExploredItemBudgetExhausted (work - 1)))
        (composeStrategy (enumerationBudget (work - 1) 100000) selected identity)

testCompositionErrors :: IO ()
testCompositionErrors = do
    selected <- selectedYes
    unit <- unitArena
    -- Keep endpoint types aligned for the static API, but change the labelled middle arena.
    alteredPositions <- object [Ready, Waiting, DoneYes, DoneNo]
    alteredMoves <- set [Ask, Yes, No]
    altered <- right "altered arena" (arena alteredPositions alteredMoves Ready [ArenaEdge Ask Opponent Ready Waiting, ArenaEdge Yes Player Waiting DoneNo, ArenaEdge No Player Waiting DoneYes])
    alteredIdentity <- right "altered identity" (copycatStrategy largeBudget altered)
    expectLeft "middle mismatch" (== IncompatibleMiddleArenas) (composeStrategy largeBudget selected alteredIdentity)
    identity <- right "unit identity" (copycatStrategy largeBudget unit)
    expectLeft "composition work budget" exhausted (composeStrategy (enumerationBudget 0 100) identity selected)
    expectLeft "composition result budget" resultExhausted (composeStrategy (enumerationBudget 100 0) identity selected)
  where
    exhausted (CompositionBudgetError (ExploredItemBudgetExhausted 0)) = True
    exhausted _ = False
    resultExhausted (CompositionBudgetError (EmittedResultBudgetExhausted 0)) = True
    resultExhausted _ = False
