{- | Bounded deterministic FIFO replay storage.

Appending assigns a monotonic stable ID and evicts the oldest entry when the
positive capacity is exceeded. Batch selection is caller-controlled and
preserves requested ID order; this core module performs no random sampling.
-}
module Markovian.Backend.Neural.Replay (
    ReplayError (..),
    ReplacementMode (..),
    ReplayEntryId,
    replayEntryIdOrdinal,
    ReplayEntry,
    replayEntryId,
    replayEntryTransition,
    ReplayBuffer,
    mkReplayBuffer,
    replayCapacity,
    replaySize,
    replayEntries,
    appendReplay,
    selectReplay,
) where

import Markovian.Backend.Neural.Transition (NeuralTransition)

-- | Replay construction and selection failures.
data ReplayError
    = InvalidReplayCapacity !Int
    | UnknownReplayEntryId !Integer
    | DuplicateReplaySelection !Integer
    deriving (Eq, Show)

-- | Whether repeated IDs are permitted in one explicit request.
data ReplacementMode
    = WithReplacement
    | WithoutReplacement
    deriving (Eq, Show)

{- | A stable replay entry identifier.

The constructor is private so callers select IDs obtained from the buffer.
-}
newtype ReplayEntryId = ReplayEntryId Integer
    deriving (Eq, Ord, Show)

-- | Monotonic zero-based ordinal, useful for diagnostics and persistence.
replayEntryIdOrdinal :: ReplayEntryId -> Integer
replayEntryIdOrdinal (ReplayEntryId value) = value

{- | A stable ID paired with one immutable validated transition snapshot.

The constructor is private so this pairing cannot be forged.
-}
data ReplayEntry = ReplayEntry !ReplayEntryId !NeuralTransition
    deriving (Eq, Show)

-- | Stable entry ID.
replayEntryId :: ReplayEntry -> ReplayEntryId
replayEntryId (ReplayEntry entryId _) = entryId

-- | Stored immutable transition snapshot.
replayEntryTransition :: ReplayEntry -> NeuralTransition
replayEntryTransition (ReplayEntry _ transition) = transition

-- | A positive-capacity FIFO replay buffer. The oldest entry occurs first.
data ReplayBuffer = ReplayBuffer !Int !Integer ![ReplayEntry]
    deriving (Eq, Show)

-- | Construct an empty positive-capacity buffer.
mkReplayBuffer :: Int -> Either ReplayError ReplayBuffer
mkReplayBuffer capacity
    | capacity <= 0 = Left (InvalidReplayCapacity capacity)
    | otherwise = Right (ReplayBuffer capacity 0 [])

-- | Maximum retained entry count.
replayCapacity :: ReplayBuffer -> Int
replayCapacity (ReplayBuffer capacity _ _) = capacity

-- | Current retained entry count.
replaySize :: ReplayBuffer -> Int
replaySize (ReplayBuffer _ _ entries) = length entries

-- | Retained entries in FIFO order.
replayEntries :: ReplayBuffer -> [ReplayEntry]
replayEntries (ReplayBuffer _ _ entries) = entries

-- | Append a snapshot and return its ID plus the post-eviction buffer.
appendReplay :: NeuralTransition -> ReplayBuffer -> (ReplayEntryId, ReplayBuffer)
appendReplay transition (ReplayBuffer capacity nextId entries) =
    let entryId = ReplayEntryId nextId
        appended = entries ++ [ReplayEntry entryId transition]
        retained = drop (max 0 (length appended - capacity)) appended
     in (entryId, ReplayBuffer capacity (nextId + 1) retained)

-- | Select retained entries in exactly the requested order.
selectReplay :: ReplacementMode -> [ReplayEntryId] -> ReplayBuffer -> Either ReplayError [ReplayEntry]
selectReplay mode requested buffer = do
    case mode of
        WithReplacement -> Right ()
        WithoutReplacement -> validateUnique [] requested
    traverse findEntry requested
  where
    entries = replayEntries buffer
    findEntry requestedId =
        case findById requestedId entries of
            Nothing -> Left (UnknownReplayEntryId (replayEntryIdOrdinal requestedId))
            Just entry -> Right entry
    validateUnique _ [] = Right ()
    validateUnique seen (entryId : remaining)
        | entryId `elem` seen = Left (DuplicateReplaySelection (replayEntryIdOrdinal entryId))
        | otherwise = validateUnique (entryId : seen) remaining

findById :: ReplayEntryId -> [ReplayEntry] -> Maybe ReplayEntry
findById _ [] = Nothing
findById requested (entry : remaining)
    | replayEntryId entry == requested = Just entry
    | otherwise = findById requested remaining
