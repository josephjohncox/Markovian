{- | Shared validated values, tables, schedules, and observations for tabular learning.

State-value and action-value tables remain distinct types. Missing keys denote
zero, and every explicit value is finite.
-}
module Markovian.Learning.Tabular (
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
    setQValue,
    VValue,
    VValueError (..),
    mkVValue,
    vValue,
    VTable,
    VTableError (..),
    emptyVTable,
    vTable,
    vEntries,
    vValueAt,
    setVValue,
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
    ObservedTransition (..),
) where

import Data.Maybe (fromMaybe)
import Markovian.MDP (ActionId)
import Markovian.Reward (Reward)
import Numeric.Natural (Natural)

-- | One finite floating action value.
newtype QValue = QValue Double
    deriving (Eq, Ord, Show)

-- | Errors from action-value construction.
newtype QValueError = NonFiniteQValue Double
    deriving (Eq, Show)

-- | Validate one finite action value.
mkQValue :: Double -> Either QValueError QValue
mkQValue value
    | not (finite value) = Left (NonFiniteQValue value)
    | otherwise = Right (QValue (canonicalZero value))

-- | Read one action value.
qValue :: QValue -> Double
qValue (QValue value) = value

-- | A tabular key containing a state and stable action ID.
data QKey state action = QKey
    { qKeyState :: !state
    , qKeyAction :: !(ActionId action)
    }
    deriving (Eq, Show)

-- | A duplicate-free finite action-value table. Missing keys denote zero.
newtype QTable state action = QTable [(QKey state action, QValue)]
    deriving (Eq, Show)

-- | Errors from explicit action-value table construction.
data QTableError state action
    = DuplicateQKey !(QKey state action)
    | InvalidInitialQValue !Integer !QValueError
    deriving (Eq, Show)

-- | The empty, everywhere-zero action-value table.
emptyQTable :: QTable state action
emptyQTable = QTable []

-- | Validate explicit finite action-value entries.
qTable ::
    (Eq state, Eq action) =>
    [(QKey state action, Double)] ->
    Either (QTableError state action) (QTable state action)
qTable entries = do
    case firstDuplicate (fmap fst entries) of
        Just duplicate -> Left (DuplicateQKey duplicate)
        Nothing -> pure ()
    QTable <$> traverse validateEntry (zip [0 ..] entries)
  where
    validateEntry (index, (key, raw)) =
        case mkQValue raw of
            Left err -> Left (InvalidInitialQValue index err)
            Right value -> Right (key, value)

-- | Read explicitly stored action values in stable table order.
qEntries :: QTable state action -> [(QKey state action, QValue)]
qEntries (QTable entries) = entries

-- | Read an action value, returning zero when the key is absent.
qValueAt :: (Eq state, Eq action) => QTable state action -> state -> ActionId action -> QValue
qValueAt (QTable entries) state selected =
    fromMaybe (QValue 0) (lookup (QKey state selected) entries)

-- | Insert or replace one already validated action value.
setQValue :: (Eq state, Eq action) => QKey state action -> QValue -> QTable state action -> QTable state action
setQValue requested updated (QTable entries) = QTable (go entries)
  where
    go [] = [(requested, updated)]
    go ((key, value) : remaining)
        | key == requested = (key, updated) : remaining
        | otherwise = (key, value) : go remaining

-- | One finite floating state value.
newtype VValue = VValue Double
    deriving (Eq, Ord, Show)

-- | Errors from state-value construction.
newtype VValueError = NonFiniteVValue Double
    deriving (Eq, Show)

-- | Validate one finite state value.
mkVValue :: Double -> Either VValueError VValue
mkVValue value
    | not (finite value) = Left (NonFiniteVValue value)
    | otherwise = Right (VValue (canonicalZero value))

-- | Read one state value.
vValue :: VValue -> Double
vValue (VValue value) = value

-- | A duplicate-free finite state-value table. Missing states denote zero.
newtype VTable state = VTable [(state, VValue)]
    deriving (Eq, Show)

-- | Errors from explicit state-value table construction.
data VTableError state
    = DuplicateVState !state
    | InvalidInitialVValue !Integer !VValueError
    deriving (Eq, Show)

-- | The empty, everywhere-zero state-value table.
emptyVTable :: VTable state
emptyVTable = VTable []

-- | Validate explicit finite state-value entries.
vTable :: (Eq state) => [(state, Double)] -> Either (VTableError state) (VTable state)
vTable entries = do
    case firstDuplicate (fmap fst entries) of
        Just duplicate -> Left (DuplicateVState duplicate)
        Nothing -> pure ()
    VTable <$> traverse validateEntry (zip [0 ..] entries)
  where
    validateEntry (index, (state, raw)) =
        case mkVValue raw of
            Left err -> Left (InvalidInitialVValue index err)
            Right value -> Right (state, value)

-- | Read explicitly stored state values in stable table order.
vEntries :: VTable state -> [(state, VValue)]
vEntries (VTable entries) = entries

-- | Read a state value, returning zero when the state is absent.
vValueAt :: (Eq state) => VTable state -> state -> VValue
vValueAt (VTable entries) state = fromMaybe (VValue 0) (lookup state entries)

-- | Insert or replace one already validated state value.
setVValue :: (Eq state) => state -> VValue -> VTable state -> VTable state
setVValue requested updated (VTable entries) = VTable (go entries)
  where
    go [] = [(requested, updated)]
    go ((state, value) : remaining)
        | state == requested = (state, updated) : remaining
        | otherwise = (state, value) : go remaining

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
    | not (finite value) = Left (NonFiniteLearningRate value)
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
    | not (finite value) = Left (NonFiniteExplorationRate value)
    | value < 0 || value > 1 = Left (ExplorationRateOutOfRange value)
    | otherwise = Right (ExplorationRate (canonicalZero value))

-- | Read an epsilon exploration rate.
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

-- | Evaluate an epsilon schedule at a zero-based episode number.
explorationRateAt :: ExplorationSchedule -> Natural -> ExplorationRate
explorationRateAt (ConstantExploration value) _ = value

-- | One immutable sampled transition supplied to a pure update.
data ObservedTransition state action = ObservedTransition
    { observedState :: !state
    , observedAction :: !(ActionId action)
    , observedReward :: !Reward
    , observedSuccessor :: !state
    }
    deriving (Eq, Show)

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate [] = Nothing
firstDuplicate (value : remaining)
    | value `elem` remaining = Just value
    | otherwise = firstDuplicate remaining

finite :: Double -> Bool
finite value = not (isNaN value || isInfinite value)

canonicalZero :: Double -> Double
canonicalZero value
    | value == 0 = 0
    | otherwise = value
