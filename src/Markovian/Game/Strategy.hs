{-# LANGUAGE RoleAnnotations #-}

{- | Finite deterministic strategies and bounded synchronized composition.

Strategies are exact finite prefix sets.  Opponent branching is exhaustive and
Player branching is total and single-valued.  Composition hides synchronized
middle moves and validates the resulting external strategy again.  This is not
contextual equivalence or an AJM quotient.
-}
module Markovian.Game.Strategy (
    BoundaryMove (..),
    effectiveOwner,
    LegalPlay,
    PlayError (..),
    legalPlay,
    emptyPlay,
    extendPlay,
    playMoves,
    domainProjection,
    codomainProjection,
    playNextOwner,
    playExtensions,
    playTerminal,
    EnumerationBudget,
    BudgetError (..),
    enumerationBudget,
    maximumExploredItems,
    maximumEmittedResults,
    Strategy,
    StrategyError (..),
    strategy,
    strategyDomain,
    strategyCodomain,
    strategyPlays,
    strategyCheckedWork,
    copycatStrategy,
    CompositionError (..),
    CompositionReport (..),
    composeStrategy,
    observationallyEqual,
    sameStrategyLayout,
) where

import Data.List (find)
import Data.Maybe (mapMaybe)
import Markovian.Category.Finite.Object (finiteObjectCardinality)
import Markovian.Category.Finite.Set (finiteSetCardinality)
import Markovian.Game.Arena
import Numeric.Natural (Natural)

-- | A visible move on the contravariant domain or covariant codomain.
data BoundaryMove domainMove codomainMove
    = DomainMove !domainMove
    | CodomainMove !codomainMove
    deriving (Eq, Ord, Show)

-- | Domain ownership is reversed; codomain ownership is unchanged.
effectiveOwner :: Owner -> BoundaryMove domainMove codomainMove -> Owner
effectiveOwner owner (DomainMove _) = otherOwner owner
effectiveOwner owner (CodomainMove _) = owner

-- | Opaque evidence that both projections replay and effective roles alternate.
data LegalPlay domainPosition domainMove codomainPosition codomainMove
    = LegalPlay
        !(Arena domainPosition domainMove)
        !(Arena codomainPosition codomainMove)
        ![BoundaryMove domainMove codomainMove]
        !(LegalHistory domainPosition domainMove)
        !(LegalHistory codomainPosition codomainMove)

type role LegalPlay nominal nominal nominal nominal

-- | Play replay failures.
data PlayError domainPosition domainMove codomainPosition codomainMove
    = PlayBudgetError !BudgetError
    | DomainHistoryError !(HistoryError domainPosition domainMove)
    | CodomainHistoryError !(HistoryError codomainPosition codomainMove)
    | MoveOwnedByWrongRole
        !(BoundaryMove domainMove codomainMove)
        !Owner
        !Owner
    deriving (Eq, Show)

-- | Replay a tagged move list against both endpoint arenas.
legalPlay ::
    (Eq domainPosition, Eq domainMove, Eq codomainPosition, Eq codomainMove) =>
    EnumerationBudget ->
    Arena domainPosition domainMove ->
    Arena codomainPosition codomainMove ->
    [BoundaryMove domainMove codomainMove] ->
    Either (PlayError domainPosition domainMove codomainPosition codomainMove) (LegalPlay domainPosition domainMove codomainPosition codomainMove)
legalPlay budget domain codomain = go 0 (emptyPlay domain codomain)
  where
    go _ play [] = Right play
    go used _ (_ : _) | used >= maximumExploredItems budget = Left (PlayBudgetError (ExploredItemBudgetExhausted (maximumExploredItems budget)))
    go used play (move : remaining) = extendPlay play move >>= \next -> go (used + 1) next remaining

legalPlayUnchecked ::
    (Eq domainPosition, Eq domainMove, Eq codomainPosition, Eq codomainMove) =>
    Arena domainPosition domainMove ->
    Arena codomainPosition codomainMove ->
    [BoundaryMove domainMove codomainMove] ->
    Either (PlayError domainPosition domainMove codomainPosition codomainMove) (LegalPlay domainPosition domainMove codomainPosition codomainMove)
legalPlayUnchecked domain codomain = go (emptyPlay domain codomain)
  where
    go play [] = Right play
    go play (move : remaining) = extendPlay play move >>= \next -> go next remaining

-- | Construct the empty legal play for two checked endpoints.
emptyPlay :: Arena domainPosition domainMove -> Arena codomainPosition codomainMove -> LegalPlay domainPosition domainMove codomainPosition codomainMove
emptyPlay domain codomain = LegalPlay domain codomain [] (emptyHistory domain) (emptyHistory codomain)

-- | Extend a play by one move with endpoint replay and role checks.
extendPlay ::
    (Eq domainPosition, Eq domainMove, Eq codomainPosition, Eq codomainMove) =>
    LegalPlay domainPosition domainMove codomainPosition codomainMove ->
    BoundaryMove domainMove codomainMove ->
    Either (PlayError domainPosition domainMove codomainPosition codomainMove) (LegalPlay domainPosition domainMove codomainPosition codomainMove)
extendPlay (LegalPlay domain codomain moves domainHistory codomainHistory) move = do
    actualOwner <- ownerOfMove domain codomain move
    let expectedOwner = turnOwner (length moves)
    if actualOwner /= expectedOwner
        then Left (MoveOwnedByWrongRole move expectedOwner actualOwner)
        else case move of
            DomainMove domainMove -> do
                next <- mapLeft DomainHistoryError (extendHistory domainHistory domainMove)
                Right (LegalPlay domain codomain (moves ++ [move]) next codomainHistory)
            CodomainMove codomainMove -> do
                next <- mapLeft CodomainHistoryError (extendHistory codomainHistory codomainMove)
                Right (LegalPlay domain codomain (moves ++ [move]) domainHistory next)

-- | Read the represented tagged move sequence.
playMoves :: LegalPlay domainPosition domainMove codomainPosition codomainMove -> [BoundaryMove domainMove codomainMove]
playMoves (LegalPlay _ _ moves _ _) = moves

-- | Project a play onto its domain history.
domainProjection :: LegalPlay domainPosition domainMove codomainPosition codomainMove -> [domainMove]
domainProjection = foldr project [] . playMoves
  where
    project (DomainMove move) remaining = move : remaining
    project (CodomainMove _) remaining = remaining

-- | Project a play onto its codomain history.
codomainProjection :: LegalPlay domainPosition domainMove codomainPosition codomainMove -> [codomainMove]
codomainProjection = foldr project [] . playMoves
  where
    project (DomainMove _) remaining = remaining
    project (CodomainMove move) remaining = move : remaining

-- | Read the owner required by the next alternating turn.
playNextOwner :: LegalPlay domainPosition domainMove codomainPosition codomainMove -> Owner
playNextOwner = turnOwner . length . playMoves

-- | Enumerate legal one-move extensions in domain-then-codomain layout order.
playExtensions ::
    (Eq domainPosition, Eq domainMove, Eq codomainPosition, Eq codomainMove) =>
    LegalPlay domainPosition domainMove codomainPosition codomainMove ->
    [BoundaryMove domainMove codomainMove]
playExtensions play@(LegalPlay _ _ _ domainHistory codomainHistory) =
    filter canExtend candidates
  where
    candidates = map DomainMove (historyExtensions domainHistory) ++ map CodomainMove (historyExtensions codomainHistory)
    canExtend move = case extendPlay play move of
        Right _ -> True
        Left _ -> False

-- | Test whether neither endpoint supplies a legal move for the next role.
playTerminal ::
    (Eq domainPosition, Eq domainMove, Eq codomainPosition, Eq codomainMove) =>
    LegalPlay domainPosition domainMove codomainPosition codomainMove ->
    Bool
playTerminal = null . playExtensions

-- | Two independent finite-enumeration limits.  Zero is allowed and rejects any charged work.
data EnumerationBudget = EnumerationBudget !Natural !Natural
    deriving (Eq, Show)

-- | Exhaustion of one explicit enumeration dimension.
data BudgetError
    = ExploredItemBudgetExhausted !Natural
    | EmittedResultBudgetExhausted !Natural
    deriving (Eq, Show)

-- | Set maximum explored work items and maximum emitted results.
enumerationBudget :: Natural -> Natural -> EnumerationBudget
enumerationBudget = EnumerationBudget

-- | Read the explored-work limit.
maximumExploredItems :: EnumerationBudget -> Natural
maximumExploredItems (EnumerationBudget maximumItems _) = maximumItems

-- | Read the emitted-result limit.
maximumEmittedResults :: EnumerationBudget -> Natural
maximumEmittedResults (EnumerationBudget _ maximumResults) = maximumResults

-- | A checked finite strategy.  The play list retains represented order.
data Strategy domainPosition domainMove codomainPosition codomainMove
    = Strategy
        !(Arena domainPosition domainMove)
        !(Arena codomainPosition codomainMove)
        ![LegalPlay domainPosition domainMove codomainPosition codomainMove]
        !Natural

type role Strategy nominal nominal nominal nominal

-- | Strategy construction failures.
data StrategyError domainPosition domainMove codomainPosition codomainMove
    = StrategyBudgetError !BudgetError
    | MissingEmptyPlay
    | DuplicateStrategyPlay ![BoundaryMove domainMove codomainMove]
    | IllegalStrategyPlay
        ![BoundaryMove domainMove codomainMove]
        !(PlayError domainPosition domainMove codomainPosition codomainMove)
    | MissingPlayPrefix ![BoundaryMove domainMove codomainMove]
    | OmittedOpponentExtension
        ![BoundaryMove domainMove codomainMove]
        !(BoundaryMove domainMove codomainMove)
    | MultiplePlayerResponses ![BoundaryMove domainMove codomainMove] !Natural
    | MissingPlayerResponse ![BoundaryMove domainMove codomainMove]
    deriving (Eq, Show)

-- | Check prefix closure, receptivity, and total deterministic Player response.
strategy ::
    (Eq domainPosition, Eq domainMove, Eq codomainPosition, Eq codomainMove) =>
    EnumerationBudget ->
    Arena domainPosition domainMove ->
    Arena codomainPosition codomainMove ->
    [[BoundaryMove domainMove codomainMove]] ->
    Either (StrategyError domainPosition domainMove codomainPosition codomainMove) (Strategy domainPosition domainMove codomainPosition codomainMove)
strategy budget domain codomain suppliedMoves = fst <$> strategyFromWork budget 0 domain codomain suppliedMoves

-- | Read the strategy's checked domain arena.
strategyDomain :: Strategy domainPosition domainMove codomainPosition codomainMove -> Arena domainPosition domainMove
strategyDomain (Strategy domain _ _ _) = domain

-- | Read the strategy's checked codomain arena.
strategyCodomain :: Strategy domainPosition domainMove codomainPosition codomainMove -> Arena codomainPosition codomainMove
strategyCodomain (Strategy _ codomain _ _) = codomain

-- | Read checked plays in represented strategy order.
strategyPlays :: Strategy domainPosition domainMove codomainPosition codomainMove -> [LegalPlay domainPosition domainMove codomainPosition codomainMove]
strategyPlays (Strategy _ _ plays _) = plays

-- | Operation-wide charged work used to validate this strategy.
strategyCheckedWork :: Strategy domainPosition domainMove codomainPosition codomainMove -> Natural
strategyCheckedWork (Strategy _ _ _ work) = work

-- | Construct the finite copycat strategy by mirroring each received move.
copycatStrategy ::
    (Eq position, Eq move) =>
    EnumerationBudget ->
    Arena position move ->
    Either (StrategyError position move position move) (Strategy position move position move)
copycatStrategy budget checkedArena = do
    (generated, generationWork) <- mapLeft StrategyBudgetError (generateCopycat budget checkedArena)
    fst <$> strategyFromWork budget generationWork checkedArena checkedArena generated

-- | Composition failures.  No partial strategy is returned.
data CompositionError aPosition aMove bPosition bMove cPosition cMove
    = IncompatibleMiddleArenas
    | CompositionBudgetError !BudgetError
    | HiddenInternalDeadlock ![BoundaryMove aMove cMove]
    | InvalidComposedStrategy !(StrategyError aPosition aMove cPosition cMove)
    deriving (Eq, Show)

-- | Deterministic diagnostics.  These counts are not timing or semantic equality data.
data CompositionReport = CompositionReport
    { exploredInteractionHistories :: !Natural
    , acceptedVisiblePlays :: !Natural
    , hiddenMiddleStepsConsidered :: !Natural
    , duplicateVisibleProjectionsRemoved :: !Natural
    , maximumInteractionDepth :: !Natural
    , configuredMaximumExploredItems :: !Natural
    , configuredMaximumEmittedResults :: !Natural
    }
    deriving (Eq, Show)

{- | Synchronize literal middle move identities, hide them, canonicalize visible
plays, and revalidate the result. Composition is partial: individually valid
strategies need not be compatible after hiding. In particular, a visible
Player position can lose every internal response; this is reported as
'HiddenInternalDeadlock'. No category instance or unrestricted closure or
associativity claim follows from this checked operation.
-}
composeStrategy ::
    (Eq aPosition, Eq aMove, Eq bPosition, Eq bMove, Eq cPosition, Eq cMove) =>
    EnumerationBudget ->
    Strategy aPosition aMove bPosition bMove ->
    Strategy bPosition bMove cPosition cMove ->
    Either
        (CompositionError aPosition aMove bPosition bMove cPosition cMove)
        (Strategy aPosition aMove cPosition cMove, CompositionReport)
composeStrategy budget left right
    | middleWork > maximumExploredItems budget = Left (CompositionBudgetError (ExploredItemBudgetExhausted (maximumExploredItems budget)))
    | arenaEquivalent (arenaBudget middleWork) (strategyCodomain left) (strategyDomain right) /= Right True = Left IncompatibleMiddleArenas
    | otherwise = do
        (rawVisible, counts) <- mapLeft CompositionBudgetError (composeTraces budget middleWork left right)
        (visible, duplicates, afterDedup) <- mapLeft CompositionBudgetError (nubBounded budget (countExplored counts) rawVisible)
        (composed, finalWork) <-
            case strategyFromWork budget afterDedup (strategyDomain left) (strategyCodomain right) visible of
                Left (StrategyBudgetError problem) -> Left (CompositionBudgetError problem)
                Left (MissingPlayerResponse moves) -> Left (HiddenInternalDeadlock moves)
                Left problem -> Left (InvalidComposedStrategy problem)
                Right result -> Right result
        let report =
                CompositionReport
                    { exploredInteractionHistories = finalWork
                    , acceptedVisiblePlays = naturalLength visible
                    , hiddenMiddleStepsConsidered = countHidden counts
                    , duplicateVisibleProjectionsRemoved = duplicates
                    , maximumInteractionDepth = countDepth counts
                    , configuredMaximumExploredItems = maximumExploredItems budget
                    , configuredMaximumEmittedResults = maximumEmittedResults budget
                    }
        Right (composed, report)
  where
    middleWork = arenaComparisonCharge (strategyCodomain left) (strategyDomain right)

{- | Checked exact equality of external finite prefix sets, ignoring represented
order. Exhaustion is atomic; no partial Boolean is returned.
-}
observationallyEqual ::
    (Eq aPosition, Eq aMove, Eq bPosition, Eq bMove) =>
    EnumerationBudget ->
    Strategy aPosition aMove bPosition bMove ->
    Strategy aPosition aMove bPosition bMove ->
    Either BudgetError Bool
observationallyEqual budget left right = do
    ensureWorkTotal budget requiredWork
    domainEqual <- mapArenaComparison (arenaEquivalent (arenaBudget domainWork) (strategyDomain left) (strategyDomain right))
    codomainEqual <- mapArenaComparison (arenaEquivalent (arenaBudget codomainWork) (strategyCodomain left) (strategyCodomain right))
    let playsEqual = sameSet leftPlays rightPlays
    playsEqual `seq` Right (domainEqual && codomainEqual && playsEqual)
  where
    leftPlays = map playMoves (strategyPlays left)
    rightPlays = map playMoves (strategyPlays right)
    domainWork = arenaComparisonCharge (strategyDomain left) (strategyDomain right)
    codomainWork = arenaComparisonCharge (strategyCodomain left) (strategyCodomain right)
    requiredWork = strategyObservationWork left right

-- | Compare external semantics and represented arena and play layouts.
sameStrategyLayout ::
    (Eq aPosition, Eq aMove, Eq bPosition, Eq bMove) =>
    EnumerationBudget ->
    Strategy aPosition aMove bPosition bMove ->
    Strategy aPosition aMove bPosition bMove ->
    Either BudgetError Bool
sameStrategyLayout budget left right = do
    ensureWorkTotal budget requiredWork
    domainEqual <- mapArenaComparison (arenaEquivalent (arenaBudget domainWork) (strategyDomain left) (strategyDomain right))
    codomainEqual <- mapArenaComparison (arenaEquivalent (arenaBudget codomainWork) (strategyCodomain left) (strategyCodomain right))
    domainLayout <- mapArenaComparison (sameArenaLayout (arenaBudget domainWork) (strategyDomain left) (strategyDomain right))
    codomainLayout <- mapArenaComparison (sameArenaLayout (arenaBudget codomainWork) (strategyCodomain left) (strategyCodomain right))
    let equivalent = domainEqual && codomainEqual && sameSet leftPlays rightPlays
        result = equivalent && domainLayout && codomainLayout && leftPlays == rightPlays
    result `seq` Right result
  where
    domainWork = arenaComparisonCharge (strategyDomain left) (strategyDomain right)
    codomainWork = arenaComparisonCharge (strategyCodomain left) (strategyCodomain right)
    leftPlays = map playMoves (strategyPlays left)
    rightPlays = map playMoves (strategyPlays right)
    maximumLength = foldl max 0 (map naturalLength (leftPlays ++ rightPlays))
    orderWork = naturalLength leftPlays * naturalLength rightPlays * (maximumLength + 1)
    requiredWork = strategyObservationWork left right + domainWork + codomainWork + orderWork

strategyObservationWork :: Strategy aPosition aMove bPosition bMove -> Strategy aPosition aMove bPosition bMove -> Natural
strategyObservationWork left right = domainWork + codomainWork + playWork
  where
    leftPlays = map playMoves (strategyPlays left)
    rightPlays = map playMoves (strategyPlays right)
    maximumLength = foldl max 0 (map naturalLength (leftPlays ++ rightPlays))
    playWork = naturalLength leftPlays * naturalLength rightPlays * (maximumLength + 1) * 2
    domainWork = arenaComparisonCharge (strategyDomain left) (strategyDomain right)
    codomainWork = arenaComparisonCharge (strategyCodomain left) (strategyCodomain right)

ownerOfMove ::
    (Eq domainMove, Eq codomainMove) =>
    Arena domainPosition domainMove ->
    Arena codomainPosition codomainMove ->
    BoundaryMove domainMove codomainMove ->
    Either (PlayError domainPosition domainMove codomainPosition codomainMove) Owner
ownerOfMove domain _ move@(DomainMove domainMove) =
    case moveEdge domain domainMove of
        Nothing -> Left (DomainHistoryError (UnknownHistoryMove domainMove))
        Just edge -> Right (effectiveOwner (edgeOwner edge) move)
ownerOfMove _ codomain move@(CodomainMove codomainMove) =
    case moveEdge codomain codomainMove of
        Nothing -> Left (CodomainHistoryError (UnknownHistoryMove codomainMove))
        Just edge -> Right (effectiveOwner (edgeOwner edge) move)

turnOwner :: Int -> Owner
turnOwner depth
    | even depth = Opponent
    | otherwise = Player

checkPrefix :: (Eq a, Eq b) => [[BoundaryMove a b]] -> [BoundaryMove a b] -> Either (StrategyError p a q b) ()
checkPrefix _ [] = Right ()
checkPrefix plays moves
    | init moves `elem` plays = Right ()
    | otherwise = Left (MissingPlayPrefix (init moves))

checkResponses ::
    (Eq p, Eq a, Eq q, Eq b) =>
    [[BoundaryMove a b]] ->
    LegalPlay p a q b ->
    Either (StrategyError p a q b) ()
checkResponses represented play =
    case playNextOwner play of
        Opponent -> case find (\extension -> playMoves play ++ [extension] `notElem` represented) extensions of
            Just omitted -> Left (OmittedOpponentExtension (playMoves play) omitted)
            Nothing -> Right ()
        Player -> case includedCount of
            0 | null extensions -> Right ()
            0 -> Left (MissingPlayerResponse (playMoves play))
            1 -> Right ()
            count -> Left (MultiplePlayerResponses (playMoves play) (fromIntegral count))
  where
    extensions = playExtensions play
    includedCount = length [extension | extension <- extensions, playMoves play ++ [extension] `elem` represented]

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate [] = Nothing
firstDuplicate (value : remaining)
    | value `elem` remaining = Just value
    | otherwise = firstDuplicate remaining

{- | Validate with one monotonic operation-wide explored-work cursor. Move
replay, duplicate/prefix comparisons, extension generation, strategy
membership, and final response checks are charged before the corresponding
unrestricted finite pass. The formulas are conservative upper bounds, so the
recorded work never understates framework traversal.
-}
strategyFromWork ::
    (Eq domainPosition, Eq domainMove, Eq codomainPosition, Eq codomainMove) =>
    EnumerationBudget ->
    Natural ->
    Arena domainPosition domainMove ->
    Arena codomainPosition codomainMove ->
    [[BoundaryMove domainMove codomainMove]] ->
    Either (StrategyError domainPosition domainMove codomainPosition codomainMove) (Strategy domainPosition domainMove codomainPosition codomainMove, Natural)
strategyFromWork budget initialWork domain codomain suppliedMoves = do
    representedOuter <- mapLeft StrategyBudgetError (collectEmitted budget suppliedMoves)
    (representedMoves, afterReplay) <- mapLeft StrategyBudgetError (collectMoveLists budget edgeCount initialWork representedOuter)
    let playCount = naturalLength representedMoves
        maximumLength = foldl max 0 (map naturalLength representedMoves)
        duplicateWork = (playCount * (playCount - min 1 playCount) `div` 2) * (maximumLength + 1)
        prefixWork = playCount * playCount * (maximumLength + 1)
        responseWork =
            sum
                [ edgeCount * (edgeCount + playLength + 1 + playCount * (playLength + 2))
                | moves <- representedMoves
                , let playLength = naturalLength moves
                ]
        requiredWork = afterReplay + playCount + duplicateWork + prefixWork + responseWork
    mapLeft StrategyBudgetError (ensureWorkTotal budget requiredWork)
    checked <- traverse checkOne representedMoves
    if [] `notElem` representedMoves
        then Left MissingEmptyPlay
        else case firstDuplicate representedMoves of
            Just duplicate -> Left (DuplicateStrategyPlay duplicate)
            Nothing -> do
                traverse_ (checkPrefix representedMoves) representedMoves
                traverse_ (checkResponses representedMoves) checked
                Right (Strategy domain codomain checked requiredWork, requiredWork)
  where
    edgeCount = naturalLength (arenaEdges domain) + naturalLength (arenaEdges codomain)
    checkOne moves = mapLeft (IllegalStrategyPlay moves) (legalPlayUnchecked domain codomain moves)

collectMoveLists :: EnumerationBudget -> Natural -> Natural -> [[move]] -> Either BudgetError ([[move]], Natural)
collectMoveLists budget edgeCount = goLists
  where
    goLists used [] = Right ([], used)
    goLists used (moves : remaining) = do
        (checkedMoves, afterMoves) <- goMoves 0 used moves
        (checkedRemaining, afterRemaining) <- goLists afterMoves remaining
        Right (checkedMoves : checkedRemaining, afterRemaining)
    goMoves _ used [] = Right ([], used)
    goMoves depth used (move : moves) = do
        let charged = used + edgeCount + depth + 1
        ensureWorkTotal budget charged
        (checked, final) <- goMoves (depth + 1) charged moves
        Right (move : checked, final)

ensureWorkTotal :: EnumerationBudget -> Natural -> Either BudgetError ()
ensureWorkTotal budget wanted
    | wanted > maximumExploredItems budget = Left (ExploredItemBudgetExhausted (maximumExploredItems budget))
    | otherwise = Right ()

traverse_ :: (value -> Either error ()) -> [value] -> Either error ()
traverse_ _ [] = Right ()
traverse_ operation (value : values) = operation value >> traverse_ operation values

-- Read at most the configured number of represented results.  Unlike
-- @length@, this rejects an overlong or infinite caller-supplied list after a
-- bounded prefix and never returns a truncated strategy.
collectEmitted :: EnumerationBudget -> [value] -> Either BudgetError [value]
collectEmitted budget = go 0
  where
    go _ [] = Right []
    go emitted (value : remaining)
        | emitted >= maximumEmittedResults budget = Left (EmittedResultBudgetExhausted (maximumEmittedResults budget))
        | otherwise = (value :) <$> go (emitted + 1) remaining

-- Copycat generation uses one charged node for every emitted prefix.
generateCopycat ::
    (Eq position, Eq move) =>
    EnumerationBudget ->
    Arena position move ->
    Either BudgetError ([[BoundaryMove move move]], Natural)
generateCopycat budget checkedArena = go 0 [emptyPlay checkedArena checkedArena] []
  where
    go explored [] emitted = Right (emitted, explored)
    go explored (play : pending) emitted = do
        ensureExplored budget explored
        ensureEmitted budget (naturalLength emitted + 1)
        let extensions = playExtensions play
            selected = case playNextOwner play of
                Opponent -> extensions
                Player -> filter (mirrors play) extensions
            next = mapMaybe (either (const Nothing) Just . extendPlay play) selected
        go (explored + 1) (pending ++ next) (emitted ++ [playMoves play])
    mirrors play extension =
        case extendPlay play extension of
            Left _ -> False
            Right next -> domainProjection next == codomainProjection next

-- Composition accounting internal to the bounded merge.
data Counts = Counts
    { countExplored :: !Natural
    , countHidden :: !Natural
    , countDepth :: !Natural
    }

composeTraces ::
    (Eq aMove, Eq bMove, Eq cMove) =>
    EnumerationBudget ->
    Natural ->
    Strategy aPosition aMove bPosition bMove ->
    Strategy bPosition bMove cPosition cMove ->
    Either BudgetError ([[BoundaryMove aMove cMove]], Counts)
composeTraces budget initialWork left right = goPairs initialWork 0 0 [] pairs
  where
    pairs = [(playMoves first, playMoves second) | first <- strategyPlays left, second <- strategyPlays right]
    goPairs explored hidden depth emitted [] = Right (emitted, Counts explored hidden depth)
    goPairs explored hidden depth emitted ((first, second) : remaining) = do
        (merges, nextExplored) <- synchronizeBounded budget explored first second
        let nextHidden = hidden + sum (map mergeHidden merges)
            nextDepth = maximum (depth : map mergeDepth merges)
            outputs = map mergeVisible merges
        ensureEmitted budget (naturalLength emitted + naturalLength outputs)
        goPairs nextExplored nextHidden nextDepth (emitted ++ outputs) remaining

data Merge a c = Merge
    { mergeVisible :: [BoundaryMove a c]
    , mergeHidden :: Natural
    , mergeDepth :: Natural
    }

synchronizeBounded ::
    (Eq b) =>
    EnumerationBudget ->
    Natural ->
    [BoundaryMove a b] ->
    [BoundaryMove b c] ->
    Either BudgetError ([Merge a c], Natural)
synchronizeBounded budget initialExplored = go initialExplored [] 0 0
  where
    go explored visible hidden depth leftMoves rightMoves = do
        ensureExplored budget explored
        let nextExplored = explored + 1
        case (leftMoves, rightMoves) of
            ([], []) -> Right ([Merge visible hidden depth], nextExplored)
            (DomainMove move : left, right) ->
                go nextExplored (visible ++ [DomainMove move]) hidden (depth + 1) left right
            (left, CodomainMove move : right) ->
                go nextExplored (visible ++ [CodomainMove move]) hidden (depth + 1) left right
            (CodomainMove leftMove : left, DomainMove rightMove : right)
                | leftMove == rightMove -> go nextExplored visible (hidden + 1) (depth + 1) left right
                | otherwise -> Right ([], nextExplored)
            _ -> Right ([], nextExplored)

ensureExplored :: EnumerationBudget -> Natural -> Either BudgetError ()
ensureExplored budget alreadyUsed
    | alreadyUsed >= maximumExploredItems budget = Left (ExploredItemBudgetExhausted (maximumExploredItems budget))
    | otherwise = Right ()

ensureEmitted :: EnumerationBudget -> Natural -> Either BudgetError ()
ensureEmitted budget wanted
    | wanted > maximumEmittedResults budget = Left (EmittedResultBudgetExhausted (maximumEmittedResults budget))
    | otherwise = Right ()

naturalLength :: [value] -> Natural
naturalLength = fromIntegral . length

nubBounded :: (Eq left, Eq right) => EnumerationBudget -> Natural -> [[BoundaryMove left right]] -> Either BudgetError ([[BoundaryMove left right]], Natural, Natural)
nubBounded budget = go [] 0
  where
    go unique duplicates used [] = Right (reverse unique, duplicates, used)
    go unique duplicates used (moves : remaining) = do
        let comparisonWidth = naturalLength moves + foldl max 0 (map naturalLength unique) + 1
            charged = used + naturalLength unique * comparisonWidth
        ensureWorkTotal budget charged
        if moves `elem` unique
            then go unique (duplicates + 1) charged remaining
            else go (moves : unique) duplicates charged remaining

sameSet :: (Eq value) => [value] -> [value] -> Bool
sameSet left right = all (`elem` right) left && all (`elem` left) right

arenaComparisonCharge :: Arena p m -> Arena p m -> Natural
arenaComparisonCharge left right =
    leftPositions * rightPositions + 2 * leftMoves * rightMoves + 1
  where
    leftPositions = fromIntegral (finiteObjectCardinality (arenaPositions left))
    rightPositions = fromIntegral (finiteObjectCardinality (arenaPositions right))
    leftMoves = fromIntegral (finiteSetCardinality (arenaMoves left))
    rightMoves = fromIntegral (finiteSetCardinality (arenaMoves right))

mapArenaComparison :: Either ArenaComparisonError value -> Either BudgetError value
mapArenaComparison (Right value) = Right value
mapArenaComparison (Left (ArenaComparisonWorkLimitExceeded _ limit)) = Left (ExploredItemBudgetExhausted limit)

mapLeft :: (left -> other) -> Either left value -> Either other value
mapLeft operation (Left problem) = Left (operation problem)
mapLeft _ (Right value) = Right value
