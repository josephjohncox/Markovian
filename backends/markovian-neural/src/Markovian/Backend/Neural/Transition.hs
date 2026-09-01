{- | Validated immutable transition snapshots for neural Q-learning.

Action masks preserve caller order. They are nonempty and duplicate-free, so
that order can define deterministic tie-breaking without a global action order.
-}
module Markovian.Backend.Neural.Transition (
    module Markovian.Backend.Neural.Mask,
    TransitionError (..),
    SuccessorSnapshot,
    foldSuccessorSnapshot,
    NeuralTransition,
    mkTerminalTransition,
    mkContinuingTransition,
    transitionFeatures,
    transitionActionMask,
    transitionAction,
    transitionReward,
    transitionSuccessor,
) where

import Markovian.Backend.Neural.Mask
import Markovian.Backend.Neural.Numeric (
    NeuralNumericError,
    validateFinite,
    validateFiniteVector,
 )

-- | Snapshot validation failures.
data TransitionError
    = EmptyFeatureVector !String
    | SelectedActionNotInMask !Int
    | TransitionNumericFailure !NeuralNumericError
    deriving (Eq, Show)

-- | Terminal payoff or a continuing successor observation and action mask.
data SuccessorSnapshot
    = TerminalSnapshot !Double
    | ContinuingSnapshot ![Double] !ActionMask
    deriving (Eq, Show)

-- | Eliminate a validated successor snapshot without exposing forgeable constructors.
foldSuccessorSnapshot :: (Double -> value) -> ([Double] -> ActionMask -> value) -> SuccessorSnapshot -> value
foldSuccessorSnapshot terminal _ (TerminalSnapshot payoff) = terminal payoff
foldSuccessorSnapshot _ continuing (ContinuingSnapshot features mask) = continuing features mask

{- | A validated immutable transition snapshot.

The constructor is private because all scalar and vector fields must be finite.
-}
data NeuralTransition = NeuralTransition ![Double] !ActionMask !Int !Double !SuccessorSnapshot
    deriving (Eq, Show)

-- | Construct a transition to a terminal payoff.
mkTerminalTransition :: [Double] -> ActionMask -> Int -> Double -> Double -> Either TransitionError NeuralTransition
mkTerminalTransition features mask action reward payoff = do
    validateSource features mask action reward
    mapNumeric (validateFinite "terminal payoff" payoff)
    Right (NeuralTransition features mask action reward (TerminalSnapshot payoff))

-- | Construct a transition to a continuing state.
mkContinuingTransition :: [Double] -> ActionMask -> Int -> Double -> [Double] -> ActionMask -> Either TransitionError NeuralTransition
mkContinuingTransition features mask action reward successorFeatures successorMask = do
    validateSource features mask action reward
    validateFeatures "successor features" successorFeatures
    Right (NeuralTransition features mask action reward (ContinuingSnapshot successorFeatures successorMask))

-- | Source feature snapshot.
transitionFeatures :: NeuralTransition -> [Double]
transitionFeatures (NeuralTransition features _ _ _ _) = features

-- | Source action-mask snapshot.
transitionActionMask :: NeuralTransition -> ActionMask
transitionActionMask (NeuralTransition _ mask _ _ _) = mask

-- | Selected source action output index.
transitionAction :: NeuralTransition -> Int
transitionAction (NeuralTransition _ _ action _ _) = action

-- | Immediate reward.
transitionReward :: NeuralTransition -> Double
transitionReward (NeuralTransition _ _ _ reward _) = reward

-- | Successor snapshot.
transitionSuccessor :: NeuralTransition -> SuccessorSnapshot
transitionSuccessor (NeuralTransition _ _ _ _ successor) = successor

validateSource :: [Double] -> ActionMask -> Int -> Double -> Either TransitionError ()
validateSource features mask action reward = do
    validateFeatures "source features" features
    if actionMaskContains action mask
        then Right ()
        else Left (SelectedActionNotInMask action)
    mapNumeric (validateFinite "transition reward" reward)

validateFeatures :: String -> [Double] -> Either TransitionError ()
validateFeatures label [] = Left (EmptyFeatureVector label)
validateFeatures label values = mapNumeric (validateFiniteVector label values)

mapNumeric :: Either NeuralNumericError value -> Either TransitionError value
mapNumeric = either (Left . TransitionNumericFailure) Right
