{- | Explicit tabular Q-learning values, configuration, and one pure update.

A Q-table key is one state and one available action ID. Missing keys have value
zero. Terminal targets include the reached terminal payoff exactly once.
-}
module Markovian.Learning.QLearning (
    QValue,
    QValueError (..),
    mkQValue,
    qValue,
    QKey (..),
    QTable,
    QTableError (..),
    emptyQTable,
    qTable,
    qEntries,
    qValueAt,
    LearningRate,
    LearningRateError (..),
    mkLearningRate,
    learningRateValue,
    ExplorationRate,
    ExplorationRateError (..),
    mkExplorationRate,
    explorationRateValue,
    LearningRateSchedule (..),
    learningRateAt,
    ExplorationSchedule (..),
    explorationRateAt,
    QLearningConfig,
    qLearningConfig,
    qLearningDiscount,
    qLearningRateSchedule,
    qExplorationSchedule,
    qEpisodeLimit,
    qEpisodeStepLimit,
    ObservedTransition (..),
    QUpdateError (..),
    QUpdateResult (..),
    updateQ,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Markovian.Horizon (Horizon)
import Markovian.MDP (
    ActionId,
    Decision (..),
    MDP,
    ModelError,
    inspectMDP,
 )
import Markovian.Objective (Discount, discountValue)
import Markovian.Reward (Reward, rewardValue)
import Numeric.Natural (Natural)

-- | One finite floating action value.
newtype QValue = QValue Double
    deriving (Eq, Ord, Show)

-- | Errors from Q-value construction.
newtype QValueError = NonFiniteQValue Double
    deriving (Eq, Show)

-- | Validate one finite Q-value.
mkQValue :: Double -> Either QValueError QValue
mkQValue value
    | isNaN value || isInfinite value = Left (NonFiniteQValue value)
    | value == 0 = Right (QValue 0)
    | otherwise = Right (QValue value)

-- | Read one Q-value.
qValue :: QValue -> Double
qValue (QValue value) = value

-- | A tabular key containing a state and stable action ID.
data QKey state action = QKey
    { qKeyState :: !state
    , qKeyAction :: !(ActionId action)
    }
    deriving (Eq, Show)

-- | A duplicate-free finite Q-table. Missing keys denote zero.
newtype QTable state action = QTable [(QKey state action, QValue)]
    deriving (Eq, Show)

-- | Errors from explicit Q-table construction.
data QTableError state action
    = DuplicateQKey !(QKey state action)
    | InvalidInitialQValue !Integer !QValueError
    deriving (Eq, Show)

-- | The empty, everywhere-zero Q-table.
emptyQTable :: QTable state action
emptyQTable = QTable []

-- | Validate explicit finite Q-table entries.
qTable ::
    (Eq state, Eq action) =>
    [(QKey state action, Double)] ->
    Either (QTableError state action) (QTable state action)
qTable entries = do
    case firstDuplicate (fmap fst entries) of
        Just duplicate -> Left (DuplicateQKey duplicate)
        Nothing -> pure ()
    validated <- traverse validateEntry (zip [0 ..] entries)
    Right (QTable validated)
  where
    validateEntry (index, (key, raw)) =
        case mkQValue raw of
            Left err -> Left (InvalidInitialQValue index err)
            Right value -> Right (key, value)

-- | Read all explicitly stored entries in stable table order.
qEntries :: QTable state action -> [(QKey state action, QValue)]
qEntries (QTable entries) = entries

-- | Read a key, returning zero when it is not explicitly stored.
qValueAt :: (Eq state, Eq action) => QTable state action -> state -> ActionId action -> QValue
qValueAt (QTable entries) state selected =
    case lookup (QKey state selected) entries of
        Nothing -> QValue 0
        Just value -> value

-- | A finite learning rate in the interval @(0, 1]@.
newtype LearningRate = LearningRate Double
    deriving (Eq, Ord, Show)

-- | Errors from learning-rate construction.
data LearningRateError
    = NonFiniteLearningRate !Double
    | LearningRateOutOfRange !Double
    deriving (Eq, Show)

-- | Validate a learning rate.
mkLearningRate :: Double -> Either LearningRateError LearningRate
mkLearningRate value
    | isNaN value || isInfinite value = Left (NonFiniteLearningRate value)
    | value <= 0 || value > 1 = Left (LearningRateOutOfRange value)
    | otherwise = Right (LearningRate value)

-- | Read a learning rate.
learningRateValue :: LearningRate -> Double
learningRateValue (LearningRate value) = value

-- | A finite epsilon value in the closed interval @[0, 1]@.
newtype ExplorationRate = ExplorationRate Double
    deriving (Eq, Ord, Show)

-- | Errors from exploration-rate construction.
data ExplorationRateError
    = NonFiniteExplorationRate !Double
    | ExplorationRateOutOfRange !Double
    deriving (Eq, Show)

-- | Validate an epsilon exploration rate.
mkExplorationRate :: Double -> Either ExplorationRateError ExplorationRate
mkExplorationRate value
    | isNaN value || isInfinite value = Left (NonFiniteExplorationRate value)
    | value < 0 || value > 1 = Left (ExplorationRateOutOfRange value)
    | value == 0 = Right (ExplorationRate 0)
    | otherwise = Right (ExplorationRate value)

-- | Read an exploration rate.
explorationRateValue :: ExplorationRate -> Double
explorationRateValue (ExplorationRate value) = value

-- | Explicit supported learning-rate schedules.
newtype LearningRateSchedule = ConstantLearningRate LearningRate
    deriving (Eq, Show)

-- | Evaluate a learning-rate schedule at a zero-based update number.
learningRateAt :: LearningRateSchedule -> Natural -> LearningRate
learningRateAt (ConstantLearningRate value) _ = value

-- | Explicit supported epsilon schedules.
newtype ExplorationSchedule = ConstantExploration ExplorationRate
    deriving (Eq, Show)

-- | Evaluate an exploration schedule at a zero-based episode number.
explorationRateAt :: ExplorationSchedule -> Natural -> ExplorationRate
explorationRateAt (ConstantExploration value) _ = value

-- | Complete bounded episodic Q-learning configuration.
data QLearningConfig = QLearningConfig
    { qLearningDiscount :: !Discount
    , qLearningRateSchedule :: !LearningRateSchedule
    , qExplorationSchedule :: !ExplorationSchedule
    , qEpisodeLimit :: !Horizon
    , qEpisodeStepLimit :: !Horizon
    }
    deriving (Eq, Show)

-- | Construct Q-learning configuration from validated values.
qLearningConfig ::
    Discount ->
    LearningRateSchedule ->
    ExplorationSchedule ->
    Horizon ->
    Horizon ->
    QLearningConfig
qLearningConfig = QLearningConfig

-- | One sampled transition supplied to the pure update.
data ObservedTransition state action = ObservedTransition
    { observedState :: !state
    , observedAction :: !(ActionId action)
    , observedReward :: !Reward
    , observedSuccessor :: !state
    }
    deriving (Eq, Show)

-- | Failures from one pure Q-update.
data QUpdateError action
    = QUpdateModelError !(ModelError action)
    | QUpdateSourceTerminal !Reward
    | QUpdateUnavailableAction !(ActionId action)
    | QUpdateArithmeticError !QValueError
    deriving (Eq, Show)

-- | The complete algebraic result of one pure Q-update.
data QUpdateResult state action = QUpdateResult
    { qUpdateKey :: !(QKey state action)
    , qUpdateOldValue :: !QValue
    , qUpdateTarget :: !QValue
    , qUpdateNewValue :: !QValue
    , qUpdateTable :: !(QTable state action)
    }
    deriving (Eq, Show)

{- | Apply one tabular Q-learning update.

For a terminal successor with payoff @g@, the target is @r + gamma*g@. For a
continuing successor, the target is @r + gamma*max_a Q(s',a)@.
-}
updateQ ::
    (Eq state, Eq action) =>
    LearningRate ->
    Discount ->
    MDP state action ->
    ObservedTransition state action ->
    QTable state action ->
    Either (QUpdateError action) (QUpdateResult state action)
updateQ rate discount model observed table = do
    sourceDecision <- mapModelError (inspectMDP model (observedState observed))
    case sourceDecision of
        TerminalDecision payoff -> Left (QUpdateSourceTerminal payoff)
        ActionDecision available
            | observedAction observed `notElem` available ->
                Left (QUpdateUnavailableAction (observedAction observed))
            | otherwise -> pure ()
    successorDecision <- mapModelError (inspectMDP model (observedSuccessor observed))
    let future =
            case successorDecision of
                TerminalDecision payoff -> rewardValue payoff
                ActionDecision available -> maximumAvailable table (observedSuccessor observed) available
        targetRaw =
            rewardValue (observedReward observed)
                + discountValue discount * future
        old = qValueAt table (observedState observed) (observedAction observed)
        updatedRaw =
            qValue old
                + learningRateValue rate * (targetRaw - qValue old)
    target <- validateQValue targetRaw
    updated <- validateQValue updatedRaw
    let key = QKey (observedState observed) (observedAction observed)
        nextTable = setQValue key updated table
    Right
        QUpdateResult
            { qUpdateKey = key
            , qUpdateOldValue = old
            , qUpdateTarget = target
            , qUpdateNewValue = updated
            , qUpdateTable = nextTable
            }
  where
    validateQValue value =
        case mkQValue value of
            Left err -> Left (QUpdateArithmeticError err)
            Right valid -> Right valid

maximumAvailable ::
    (Eq state, Eq action) =>
    QTable state action ->
    state ->
    NonEmpty (ActionId action) ->
    Double
maximumAvailable table state (first :| remaining) =
    foldr
        (max . qValue . qValueAt table state)
        (qValue (qValueAt table state first))
        remaining

setQValue :: (Eq state, Eq action) => QKey state action -> QValue -> QTable state action -> QTable state action
setQValue requested updated (QTable entries) = QTable (go entries)
  where
    go [] = [(requested, updated)]
    go ((key, value) : remaining)
        | key == requested = (key, updated) : remaining
        | otherwise = (key, value) : go remaining

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate [] = Nothing
firstDuplicate (value : remaining)
    | value `elem` remaining = Just value
    | otherwise = firstDuplicate remaining

mapModelError :: Either (ModelError action) value -> Either (QUpdateError action) value
mapModelError = either (Left . QUpdateModelError) Right
