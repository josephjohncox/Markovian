{-# LANGUAGE RoleAnnotations #-}

{- | Checked finite acyclic alternating interaction arenas.

This module implements a finite protocol substrate.  It has no justification
pointers, views, innocence, payoff, probability, recursion, or equilibrium
semantics.  Constructors are opaque so that legal histories are replay
evidence rather than unchecked move lists.
-}
module Markovian.Game.Arena (
    Owner (..),
    otherOwner,
    ArenaEdge (..),
    Arena,
    ArenaBudget,
    arenaBudget,
    ArenaError (..),
    arena,
    arenaPositions,
    arenaMoves,
    arenaInitialPosition,
    arenaEdges,
    ArenaComparisonError (..),
    arenaEquivalent,
    sameArenaLayout,
    outgoingMoves,
    moveEdge,
    positionOwner,
    LegalHistory,
    ReplayBudget,
    replayBudget,
    HistoryError (..),
    legalHistory,
    emptyHistory,
    extendHistory,
    historyMoves,
    historyPosition,
    historyNextOwner,
    historyExtensions,
    historyTerminal,
    historiesEquivalent,
    sameHistoryLayout,
) where

import Data.List (find)
import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Category.Finite.Object
import Markovian.Category.Finite.Set
import Numeric.Natural (Natural)

-- | The two protocol roles.  'Opponent' is the externally receptive role.
data Owner = Opponent | Player
    deriving (Eq, Ord, Show)

-- | Reverse one protocol role.
otherOwner :: Owner -> Owner
otherOwner Opponent = Player
otherOwner Player = Opponent

-- | A labelled move edge.  Every represented move must have exactly one edge.
data ArenaEdge position move = ArenaEdge
    { edgeMove :: !move
    , edgeOwner :: !Owner
    , edgeSource :: !position
    , edgeTarget :: !position
    }
    deriving (Eq, Show)

-- | An arena whose reachability, ownership, alternation, and acyclicity were checked.
data Arena position move
    = Arena
        !(FiniteObject position)
        !(FiniteSet move)
        !position
        ![ArenaEdge position move]

type role Arena nominal nominal

-- | Arena construction failures.  Cycle witnesses start and end at the same position.
newtype ArenaBudget = ArenaBudget Natural
    deriving (Eq, Show)

-- | Set the complete arena validation or equality work limit.
arenaBudget :: Natural -> ArenaBudget
arenaBudget = ArenaBudget

-- | Arena construction failures. Work exhaustion returns no partial arena.
data ArenaError position move
    = ArenaWorkLimitExceeded !Natural !Natural
    | InitialPositionOutsideArena !position
    | MissingMoveEdge !move
    | DuplicateMoveEdge !move
    | EdgeMoveOutsideArena !move
    | EdgeSourceOutsideArena !move !position
    | EdgeTargetOutsideArena !move !position
    | UnreachablePosition !position
    | MixedOwnershipAtPosition !position !Owner !Owner
    | PlayerOwnedInitialPosition !position
    | NonAlternatingEdge !move !Owner !Owner
    | DirectedCycle ![position]
    deriving (Eq, Show)

-- | Validate an arena in deterministic position-layout and move-layout order.
arena ::
    (Eq position, Eq move) =>
    ArenaBudget ->
    FiniteObject position ->
    FiniteSet move ->
    position ->
    [ArenaEdge position move] ->
    Either (ArenaError position move) (Arena position move)
arena (ArenaBudget workLimit) positions moves initial suppliedEdges
    | requiredWork > workLimit = Left (ArenaWorkLimitExceeded requiredWork workLimit)
    | initial `notElem` positionValues = Left (InitialPositionOutsideArena initial)
    | hasExcessEdge
    , Just move <- find (\move -> countMove move boundedEdges > 1) moveValues =
        Left (DuplicateMoveEdge move)
    | hasExcessEdge
    , Just edge <- find (\edge -> edgeMove edge `notElem` moveValues) boundedEdges =
        Left (EdgeMoveOutsideArena (edgeMove edge))
    | Just move <- find (\move -> countMove move boundedEdges == 0) moveValues = Left (MissingMoveEdge move)
    | Just move <- find (\move -> countMove move boundedEdges > 1) moveValues = Left (DuplicateMoveEdge move)
    | Just edge <- find (\edge -> edgeMove edge `notElem` moveValues) boundedEdges = Left (EdgeMoveOutsideArena (edgeMove edge))
    | Just edge <- find (\edge -> edgeSource edge `notElem` positionValues) orderedEdges = Left (EdgeSourceOutsideArena (edgeMove edge) (edgeSource edge))
    | Just edge <- find (\edge -> edgeTarget edge `notElem` positionValues) orderedEdges = Left (EdgeTargetOutsideArena (edgeMove edge) (edgeTarget edge))
    | Just position <- find (`notElem` reachable) positionValues = Left (UnreachablePosition position)
    | Just problem <- firstMixedOwner positionValues orderedEdges = Left problem
    | Just owner <- ownerAt initial orderedEdges
    , owner == Player =
        Left (PlayerOwnedInitialPosition initial)
    | Just problem <- firstAlternationProblem orderedEdges = Left problem
    | Just cycleWitness <- firstCycle positionValues orderedEdges = Left (DirectedCycle cycleWitness)
    | otherwise = Right (Arena positions moves initial orderedEdges)
  where
    positionValues = NonEmpty.toList (finiteObjectValues positions)
    moveValues = finiteSetValues moves
    -- An accepted arena has exactly one edge per represented move.  Inspecting
    -- one item beyond that cardinality rejects even an infinite supplied list
    -- without traversing it in full.
    boundedEdges = takeAtMost (fromIntegral (finiteSetCardinality moves) + 1) suppliedEdges
    hasExcessEdge = length boundedEdges > finiteSetCardinality moves
    orderedEdges = [edge | move <- moveValues, edge <- boundedEdges, edgeMove edge == move]
    reachable = reachableFrom orderedEdges [initial] []
    positionCount = fromIntegral (length positionValues)
    moveCount = fromIntegral (finiteSetCardinality moves)
    requiredWork = 1 + positionCount * positionCount + positionCount * moveCount + moveCount * moveCount

-- | Read positions in represented layout order.
arenaPositions :: Arena position move -> FiniteObject position
arenaPositions (Arena positions _ _ _) = positions

-- | Read moves in represented layout order.
arenaMoves :: Arena position move -> FiniteSet move
arenaMoves (Arena _ moves _ _) = moves

-- | Read the represented initial position.
arenaInitialPosition :: Arena position move -> position
arenaInitialPosition (Arena _ _ initial _) = initial

-- | Read edges in move-layout order.
arenaEdges :: Arena position move -> [ArenaEdge position move]
arenaEdges (Arena _ _ _ edges) = edges

-- | Atomic exhaustion for bounded arena comparison.
data ArenaComparisonError = ArenaComparisonWorkLimitExceeded !Natural !Natural
    deriving (Eq, Show)

-- | Compare labelled arena data while ignoring represented layout order.
arenaEquivalent :: (Eq position, Eq move) => ArenaBudget -> Arena position move -> Arena position move -> Either ArenaComparisonError Bool
arenaEquivalent (ArenaBudget limit) left right
    | required > limit = Left (ArenaComparisonWorkLimitExceeded required limit)
    | otherwise = Right (arenaEquivalentUnchecked left right)
  where
    leftPositions = fromIntegral (NonEmpty.length (finiteObjectValues (arenaPositions left)))
    rightPositions = fromIntegral (NonEmpty.length (finiteObjectValues (arenaPositions right)))
    leftMoves = fromIntegral (finiteSetCardinality (arenaMoves left))
    rightMoves = fromIntegral (finiteSetCardinality (arenaMoves right))
    required = leftPositions * rightPositions + leftMoves * rightMoves + leftMoves * rightMoves + 1

arenaEquivalentUnchecked :: (Eq position, Eq move) => Arena position move -> Arena position move -> Bool
arenaEquivalentUnchecked left right =
    sameFiniteSupport (arenaPositions left) (arenaPositions right)
        && sameFiniteSet (arenaMoves left) (arenaMoves right)
        && arenaInitialPosition left == arenaInitialPosition right
        && all edgeAgrees (arenaEdges left)
  where
    edgeAgrees edge = lookupEdge (edgeMove edge) (arenaEdges right) == Just edge

-- | Compare labelled semantics and both represented layouts.
sameArenaLayout :: (Eq position, Eq move) => ArenaBudget -> Arena position move -> Arena position move -> Either ArenaComparisonError Bool
sameArenaLayout budget left right = do
    equivalent <- arenaEquivalent budget left right
    Right
        ( equivalent
            && Markovian.Category.Finite.Object.sameFiniteLayout (arenaPositions left) (arenaPositions right)
            && Markovian.Category.Finite.Set.sameFiniteLayout (arenaMoves left) (arenaMoves right)
        )

-- | Read legal outgoing moves in move-layout order. Outside positions have none.
outgoingMoves :: (Eq position) => Arena position move -> position -> [move]
outgoingMoves (Arena _ _ _ edges) position = [edgeMove edge | edge <- edges, edgeSource edge == position]

-- | Look up the unique checked edge for a represented move.
moveEdge :: (Eq move) => Arena position move -> move -> Maybe (ArenaEdge position move)
moveEdge (Arena _ _ _ edges) move = lookupEdge move edges

-- | Read the common outgoing owner, or 'Nothing' at a terminal or outside position.
positionOwner :: (Eq position) => Arena position move -> position -> Maybe Owner
positionOwner (Arena _ _ _ edges) position = ownerAt position edges

-- | Opaque evidence that a move sequence replays from an arena's initial position.
data LegalHistory position move = LegalHistory !(Arena position move) ![move] !position

type role LegalHistory nominal nominal

-- | Legal-history replay failures.
newtype ReplayBudget = ReplayBudget Natural
    deriving (Eq, Show)

-- | Set the maximum number of moves replayed for one caller-supplied history.
replayBudget :: Natural -> ReplayBudget
replayBudget = ReplayBudget

-- | Legal-history replay failures.
data HistoryError position move
    = HistoryReplayLimitExceeded !Natural
    | UnknownHistoryMove !move
    | MoveFromWrongPosition !move !position !position
    | MoveAfterTerminal !move !position
    deriving (Eq, Show)

-- | Replay a move list from the initial position.
legalHistory :: (Eq position, Eq move) => ReplayBudget -> Arena position move -> [move] -> Either (HistoryError position move) (LegalHistory position move)
legalHistory (ReplayBudget limit) checkedArena = go 0 (emptyHistory checkedArena)
  where
    go _ history [] = Right history
    go used _ (_ : _) | used >= limit = Left (HistoryReplayLimitExceeded limit)
    go used history (move : remaining) = extendHistory history move >>= \next -> go (used + 1) next remaining

-- | Construct the empty replay witness at the initial position.
emptyHistory :: Arena position move -> LegalHistory position move
emptyHistory checkedArena = LegalHistory checkedArena [] (arenaInitialPosition checkedArena)

-- | Replay one move, rejecting unknown, wrong-source, and post-terminal moves.
extendHistory :: (Eq position, Eq move) => LegalHistory position move -> move -> Either (HistoryError position move) (LegalHistory position move)
extendHistory (LegalHistory checkedArena moves position) move =
    case moveEdge checkedArena move of
        Nothing -> Left (UnknownHistoryMove move)
        Just edge
            | null (outgoingMoves checkedArena position) -> Left (MoveAfterTerminal move position)
            | edgeSource edge /= position -> Left (MoveFromWrongPosition move position (edgeSource edge))
            | otherwise -> Right (LegalHistory checkedArena (moves ++ [move]) (edgeTarget edge))

-- | Read the replayed moves.
historyMoves :: LegalHistory position move -> [move]
historyMoves (LegalHistory _ moves _) = moves

-- | Read the reached position.
historyPosition :: LegalHistory position move -> position
historyPosition (LegalHistory _ _ position) = position

-- | Read the owner of the next turn, if the reached position is nonterminal.
historyNextOwner :: (Eq position) => LegalHistory position move -> Maybe Owner
historyNextOwner (LegalHistory checkedArena _ position) = positionOwner checkedArena position

-- | Read legal one-move extensions in move-layout order.
historyExtensions :: (Eq position) => LegalHistory position move -> [move]
historyExtensions (LegalHistory checkedArena _ position) = outgoingMoves checkedArena position

-- | Test whether the reached position has no outgoing move.
historyTerminal :: (Eq position) => LegalHistory position move -> Bool
historyTerminal = null . historyExtensions

-- | Compare labelled arena data, moves, and endpoint while ignoring layout order.
historiesEquivalent :: (Eq position, Eq move) => LegalHistory position move -> LegalHistory position move -> Bool
historiesEquivalent (LegalHistory leftArena leftMoves leftPosition) (LegalHistory rightArena rightMoves rightPosition) =
    arenaEquivalentUnchecked leftArena rightArena && leftMoves == rightMoves && leftPosition == rightPosition

-- | Compare history semantics and the represented arena layouts.
sameHistoryLayout :: (Eq position, Eq move) => LegalHistory position move -> LegalHistory position move -> Bool
sameHistoryLayout (LegalHistory leftArena leftMoves leftPosition) (LegalHistory rightArena rightMoves rightPosition) =
    arenaEquivalentUnchecked leftArena rightArena
        && Markovian.Category.Finite.Object.sameFiniteLayout (arenaPositions leftArena) (arenaPositions rightArena)
        && Markovian.Category.Finite.Set.sameFiniteLayout (arenaMoves leftArena) (arenaMoves rightArena)
        && leftMoves == rightMoves
        && leftPosition == rightPosition

lookupEdge :: (Eq move) => move -> [ArenaEdge position move] -> Maybe (ArenaEdge position move)
lookupEdge move = find ((== move) . edgeMove)

countMove :: (Eq move) => move -> [ArenaEdge position move] -> Int
countMove move = length . filter ((== move) . edgeMove)

takeAtMost :: Natural -> [value] -> [value]
takeAtMost 0 _ = []
takeAtMost _ [] = []
takeAtMost remaining (value : values) = value : takeAtMost (remaining - 1) values

ownerAt :: (Eq position) => position -> [ArenaEdge position move] -> Maybe Owner
ownerAt position edges = edgeOwner <$> find ((== position) . edgeSource) edges

firstMixedOwner :: (Eq position) => [position] -> [ArenaEdge position move] -> Maybe (ArenaError position move)
firstMixedOwner [] _ = Nothing
firstMixedOwner (position : remaining) edges =
    case [edgeOwner edge | edge <- edges, edgeSource edge == position] of
        [] -> firstMixedOwner remaining edges
        owner : owners -> case find (/= owner) owners of
            Just different -> Just (MixedOwnershipAtPosition position owner different)
            Nothing -> firstMixedOwner remaining edges

firstAlternationProblem :: (Eq position) => [ArenaEdge position move] -> Maybe (ArenaError position move)
firstAlternationProblem edges = findProblem edges
  where
    findProblem [] = Nothing
    findProblem (edge : remaining) = case ownerAt (edgeTarget edge) edges of
        Just targetOwner | targetOwner == edgeOwner edge -> Just (NonAlternatingEdge (edgeMove edge) (edgeOwner edge) targetOwner)
        _ -> findProblem remaining

reachableFrom :: (Eq position) => [ArenaEdge position move] -> [position] -> [position] -> [position]
reachableFrom _ [] seen = seen
reachableFrom edges (position : pending) seen
    | position `elem` seen = reachableFrom edges pending seen
    | otherwise =
        let targets = [edgeTarget edge | edge <- edges, edgeSource edge == position]
         in reachableFrom edges (pending ++ targets) (seen ++ [position])

-- Color-based DFS: the black set prevents repeated traversal of shared DAG
-- suffixes, while the gray path supplies a deterministic cycle witness.
firstCycle :: (Eq position) => [position] -> [ArenaEdge position move] -> Maybe [position]
firstCycle positions edges = roots [] positions
  where
    roots _ [] = Nothing
    roots black (position : remaining)
        | position `elem` black = roots black remaining
        | otherwise = case visit [] black position of
            Left witness -> Just witness
            Right nextBlack -> roots nextBlack remaining

    visit gray black position
        | position `elem` gray = Left (dropWhile (/= position) gray ++ [position])
        | position `elem` black = Right black
        | otherwise = children (gray ++ [position]) black [edgeTarget edge | edge <- edges, edgeSource edge == position]
      where
        children _ currentBlack [] = Right (position : currentBlack)
        children currentGray currentBlack (target : targets) = case visit currentGray currentBlack target of
            Left witness -> Left witness
            Right nextBlack -> children currentGray nextBlack targets
