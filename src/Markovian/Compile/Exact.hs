{- | Policy-free finite compilation for exact Markov decision processes.

Compilation validates explicit state and action indexes once, then stores every
available action and its joint @(reward, successor)@ distribution. Policy
closure is a separate operation over the compiled model.
-}
module Markovian.Compile.Exact (
    StateIndex,
    stateIndexValue,
    ActionIndex,
    actionIndexValue,
    FiniteIndexError (..),
    FiniteStateIndex,
    finiteStateIndex,
    lookupStateIndex,
    stateAtIndex,
    FiniteActionIndex,
    finiteActionIndex,
    lookupActionIndex,
    actionAtIndex,
    ExactMDPCompileError (..),
    ExactPolicyCompileError (..),
    CompiledExactOutcome (..),
    CompiledExactState,
    compiledSourceState,
    foldCompiledExactState,
    CompiledExactMDP,
    compileExactMDP,
    compiledInitialState,
    compiledStateIndex,
    compiledActionIndex,
    compiledStates,
    compiledStateEntries,
    CompiledExactMRPState (..),
    CompiledExactMRP,
    closeCompiledExactPolicy,
    compiledMRPInitialState,
    compiledMRPStateIndex,
    compiledMRPStates,
    compiledMRPStateEntries,
    CompiledExactStep (..),
    CompiledRuntimeError (..),
    stepCompiledExactMDP,
    stepCompiledExactMRP,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Markovian.MDP (ActionId)
import Markovian.MDP.Exact (
    ExactDecision (..),
    ExactMDP,
    ExactModelError,
    ExactTransitionOutcome,
    exactMDPInitialState,
    exactSuccessorState,
    exactTransitionReward,
    inspectExactMDP,
    stepExactMDP,
 )
import Markovian.Policy.Exact (
    ExactPolicy,
    ExactPolicyError,
    exactPolicyActions,
    validateExactPolicySupport,
 )
import Markovian.Probability.Exact (
    ExactDistributionError,
    ExactFiniteDist,
    exactFiniteDist,
    exactOutcomes,
    exactProbability,
 )
import Markovian.Reward.Exact (ExactReward)

-- | A zero-based state position in one compiled model.
newtype StateIndex = StateIndex Integer
    deriving (Eq, Ord, Show)

-- | Read a state index.
stateIndexValue :: StateIndex -> Integer
stateIndexValue (StateIndex value) = value

-- | A zero-based action position in one compiled model.
newtype ActionIndex = ActionIndex Integer
    deriving (Eq, Ord, Show)

-- | Read an action index.
actionIndexValue :: ActionIndex -> Integer
actionIndexValue (ActionIndex value) = value

-- | Errors from constructing a finite value index.
data FiniteIndexError value
    = EmptyFiniteIndex
    | DuplicateFiniteIndexValue !value
    deriving (Eq, Show)

-- | A nonempty, duplicate-free finite state index.
newtype FiniteStateIndex state = FiniteStateIndex (NonEmpty state)
    deriving (Eq, Show)

-- | Validate an ordered finite state support.
finiteStateIndex :: (Eq state) => [state] -> Either (FiniteIndexError state) (FiniteStateIndex state)
finiteStateIndex values = FiniteStateIndex <$> validateFiniteIndex values

-- | Find one state's zero-based index.
lookupStateIndex :: (Eq state) => FiniteStateIndex state -> state -> Maybe StateIndex
lookupStateIndex (FiniteStateIndex values) requested =
    StateIndex <$> lookupPosition requested (NonEmpty.toList values)

-- | Decode one state index without partial indexing.
stateAtIndex :: FiniteStateIndex state -> StateIndex -> Maybe state
stateAtIndex (FiniteStateIndex values) (StateIndex requested) =
    valueAtPosition requested (NonEmpty.toList values)

-- | A duplicate-free finite action-ID index. Terminal-only models may use an empty index.
newtype FiniteActionIndex action = FiniteActionIndex [ActionId action]
    deriving (Eq, Show)

-- | Validate an ordered finite action-ID support.
finiteActionIndex ::
    (Eq action) =>
    [ActionId action] ->
    Either (FiniteIndexError (ActionId action)) (FiniteActionIndex action)
finiteActionIndex values =
    case firstDuplicate values of
        Just duplicate -> Left (DuplicateFiniteIndexValue duplicate)
        Nothing -> Right (FiniteActionIndex values)

-- | Find one action ID's zero-based index.
lookupActionIndex :: (Eq action) => FiniteActionIndex action -> ActionId action -> Maybe ActionIndex
lookupActionIndex (FiniteActionIndex values) requested =
    ActionIndex <$> lookupPosition requested values

-- | Decode one action index without partial indexing.
actionAtIndex :: FiniteActionIndex action -> ActionIndex -> Maybe (ActionId action)
actionAtIndex (FiniteActionIndex values) (ActionIndex requested) =
    valueAtPosition requested values

-- | Exhaustive policy-free model-compilation failures.
data ExactMDPCompileError state action
    = ExactMDPStateIndexError !(FiniteIndexError state)
    | ExactMDPActionIndexError !(FiniteIndexError (ActionId action))
    | ExactMDPUnindexedInitialState !state
    | ExactMDPModelError !state !(ExactModelError action)
    | ExactMDPUnindexedAvailableAction !state !(ActionId action)
    | ExactMDPUnindexedSuccessor !state !(ActionId action) !state
    | ExactMDPDistributionError !ExactDistributionError
    deriving (Eq, Show)

-- | Failures while closing a compiled model under an exact policy.
data ExactPolicyCompileError state action
    = ExactPolicyAtStateError !state !(ExactPolicyError action)
    | ExactPolicyUnindexedAction !state !(ActionId action)
    | ExactPolicyCompileInvariant !state !CompiledRuntimeError
    | ExactPolicyCompileDistributionError !ExactDistributionError
    deriving (Eq, Show)

-- | A compiled transition reward paired with an indexed successor.
data CompiledExactOutcome = CompiledExactOutcome
    { compiledTransitionReward :: !ExactReward
    , compiledSuccessorState :: !StateIndex
    }
    deriving (Eq, Show)

-- | One exhaustively validated policy-free state.
data CompiledExactState state
    = CompiledTerminalState
        { compiledSourceState :: !state
        -- ^ Original source state.
        , compiledTerminalPayoff :: !ExactReward
        }
    | CompiledContinuingState
        { compiledSourceState :: !state
        , compiledAvailableActions :: !(NonEmpty ActionIndex)
        , compiledTransitions :: ![(ActionIndex, ExactFiniteDist CompiledExactOutcome)]
        }
    deriving (Eq, Show)

-- | Eliminate a validated compiled state without exposing forgeable constructors.
foldCompiledExactState ::
    (state -> ExactReward -> value) ->
    (state -> NonEmpty ActionIndex -> [(ActionIndex, ExactFiniteDist CompiledExactOutcome)] -> value) ->
    CompiledExactState state ->
    value
foldCompiledExactState terminal _ (CompiledTerminalState source payoff) = terminal source payoff
foldCompiledExactState _ continuing (CompiledContinuingState source available transitions) =
    continuing source available transitions

-- | An exact MDP compiled over explicit finite supports, without a policy.
data CompiledExactMDP state action
    = CompiledExactMDP
        !StateIndex
        !(FiniteStateIndex state)
        !(FiniteActionIndex action)
        !(NonEmpty (CompiledExactState state))

-- | Compile every available action of an exact finite MDP.
compileExactMDP ::
    (Eq state, Eq action) =>
    [state] ->
    [ActionId action] ->
    ExactMDP state action ->
    Either (ExactMDPCompileError state action) (CompiledExactMDP state action)
compileExactMDP stateSupport actionSupport model = do
    states <- mapLeft ExactMDPStateIndexError (finiteStateIndex stateSupport)
    actions <- mapLeft ExactMDPActionIndexError (finiteActionIndex actionSupport)
    initial <-
        case lookupStateIndex states (exactMDPInitialState model) of
            Nothing -> Left (ExactMDPUnindexedInitialState (exactMDPInitialState model))
            Just index -> Right index
    compiled <- traverse (compileState states actions) (stateValues states)
    Right (CompiledExactMDP initial states actions compiled)
  where
    compileState states actions state = do
        decision <- mapModelError state (inspectExactMDP model state)
        case decision of
            ExactTerminalDecision payoff -> Right (CompiledTerminalState state payoff)
            ExactActionDecision available -> do
                availableIndexes <- traverse (requireAvailableAction state actions) available
                transitions <-
                    traverse
                        (compileTransition states actions state)
                        (NonEmpty.toList available)
                Right
                    CompiledContinuingState
                        { compiledSourceState = state
                        , compiledAvailableActions = availableIndexes
                        , compiledTransitions = transitions
                        }

    compileTransition states actions state selectedAction = do
        selectedIndex <- requireAvailableAction state actions selectedAction
        transition <- mapModelError state (stepExactMDP model state selectedAction)
        compiled <- compileTransitionDistribution states state selectedAction transition
        Right (selectedIndex, compiled)

-- | Read the compiled initial-state index.
compiledInitialState :: CompiledExactMDP state action -> StateIndex
compiledInitialState (CompiledExactMDP initial _ _ _) = initial

-- | Read the validated state index.
compiledStateIndex :: CompiledExactMDP state action -> FiniteStateIndex state
compiledStateIndex (CompiledExactMDP _ states _ _) = states

-- | Read the validated action index.
compiledActionIndex :: CompiledExactMDP state action -> FiniteActionIndex action
compiledActionIndex (CompiledExactMDP _ _ actions _) = actions

-- | Read all policy-free compiled states in index order.
compiledStates :: CompiledExactMDP state action -> NonEmpty (CompiledExactState state)
compiledStates (CompiledExactMDP _ _ _ states) = states

-- | Read policy-free compiled states paired with their zero-based indexes.
compiledStateEntries :: CompiledExactMDP state action -> NonEmpty (StateIndex, CompiledExactState state)
compiledStateEntries compiled =
    NonEmpty.zip (StateIndex 0 :| fmap StateIndex [1 ..]) (compiledStates compiled)

-- | A state after a compiled policy has been closed over the model.
data CompiledExactMRPState state
    = CompiledMRPTerminalState
        { compiledMRPSourceState :: !state
        , compiledMRPTerminalPayoff :: !ExactReward
        }
    | CompiledMRPContinuingState
        { compiledMRPSourceState :: !state
        , compiledMRPTransition :: !(ExactFiniteDist CompiledExactOutcome)
        }
    deriving (Eq, Show)

-- | A compiled exact Markov reward process obtained by one policy closure.
data CompiledExactMRP state
    = CompiledExactMRP
        !StateIndex
        !(FiniteStateIndex state)
        !(NonEmpty (CompiledExactMRPState state))

-- | Close a policy over every continuing state of a compiled exact MDP.
closeCompiledExactPolicy ::
    (Eq action) =>
    CompiledExactMDP state action ->
    ExactPolicy state action ->
    Either (ExactPolicyCompileError state action) (CompiledExactMRP state)
closeCompiledExactPolicy compiled selectedPolicy = do
    indexedStates <- traverse closeState (compiledStateEntries compiled)
    let states = fmap snd indexedStates
    Right (CompiledExactMRP (compiledInitialState compiled) (compiledStateIndex compiled) states)
  where
    closeState (stateIndex, state) =
        case state of
            CompiledTerminalState source payoff -> Right (stateIndex, CompiledMRPTerminalState source payoff)
            CompiledContinuingState source available transitions -> do
                availableIds <- traverse (decodeAction source) available
                let selected = exactPolicyActions selectedPolicy source
                mapLeft (ExactPolicyAtStateError source) (validateExactPolicySupport availableIds selected)
                selectedIndexes <- traverse (compileSelected source) (NonEmpty.toList (exactOutcomes selected))
                branches <- fmap concat (traverse (selectedBranches source stateIndex transitions) selectedIndexes)
                distribution <- mapLeft ExactPolicyCompileDistributionError (exactFiniteDist branches)
                Right (stateIndex, CompiledMRPContinuingState source distribution)

    decodeAction source index =
        case actionAtIndex (compiledActionIndex compiled) index of
            Nothing -> Left (ExactPolicyCompileInvariant source (InvalidCompiledActionIndex index))
            Just action -> Right action

    compileSelected source (selectedAction, mass) =
        case lookupActionIndex (compiledActionIndex compiled) selectedAction of
            Nothing -> Left (ExactPolicyUnindexedAction source selectedAction)
            Just index -> Right (index, exactProbability mass)

    selectedBranches source stateIndex transitions (selectedAction, actionMass) =
        case lookup selectedAction transitions of
            Nothing -> Left (ExactPolicyCompileInvariant source (MissingCompiledTransition stateIndex selectedAction))
            Just transition ->
                Right
                    [ (outcome, actionMass * exactProbability transitionMass)
                    | (outcome, transitionMass) <- NonEmpty.toList (exactOutcomes transition)
                    ]

-- | Read a closed process's initial-state index.
compiledMRPInitialState :: CompiledExactMRP state -> StateIndex
compiledMRPInitialState (CompiledExactMRP initial _ _) = initial

-- | Read a closed process's validated state index.
compiledMRPStateIndex :: CompiledExactMRP state -> FiniteStateIndex state
compiledMRPStateIndex (CompiledExactMRP _ states _) = states

-- | Read closed-process states in state-index order.
compiledMRPStates :: CompiledExactMRP state -> NonEmpty (CompiledExactMRPState state)
compiledMRPStates (CompiledExactMRP _ _ states) = states

-- | Read closed-process states paired with their zero-based indexes.
compiledMRPStateEntries :: CompiledExactMRP state -> NonEmpty (StateIndex, CompiledExactMRPState state)
compiledMRPStateEntries compiled =
    NonEmpty.zip (StateIndex 0 :| fmap StateIndex [1 ..]) (compiledMRPStates compiled)

-- | One compiled exact state step.
data CompiledExactStep
    = CompiledExactTerminalStep !ExactReward
    | CompiledExactTransitionStep !(ExactFiniteDist CompiledExactOutcome)
    deriving (Eq, Show)

-- | Runtime invariant failures from an otherwise compiled model.
data CompiledRuntimeError
    = InvalidCompiledStateIndex !StateIndex
    | InvalidCompiledActionIndex !ActionIndex
    | CompiledActionRequestedAtTerminal !StateIndex !ExactReward
    | MissingCompiledTransition !StateIndex !ActionIndex
    deriving (Eq, Show)

-- | Step one state-action pair in a policy-free compiled MDP.
stepCompiledExactMDP ::
    CompiledExactMDP state action ->
    StateIndex ->
    ActionIndex ->
    Either CompiledRuntimeError (ExactFiniteDist CompiledExactOutcome)
stepCompiledExactMDP compiled requested selected = do
    _ <-
        case actionAtIndex (compiledActionIndex compiled) selected of
            Nothing -> Left (InvalidCompiledActionIndex selected)
            Just action -> Right action
    state <- requireCompiledState compiled requested
    case state of
        CompiledTerminalState _ payoff -> Left (CompiledActionRequestedAtTerminal requested payoff)
        CompiledContinuingState _ _ transitions ->
            case lookup selected transitions of
                Nothing -> Left (MissingCompiledTransition requested selected)
                Just transition -> Right transition

-- | Step one state in a closed compiled exact MRP.
stepCompiledExactMRP ::
    CompiledExactMRP state ->
    StateIndex ->
    Either CompiledRuntimeError CompiledExactStep
stepCompiledExactMRP compiled requested =
    case compiledMRPStateAt compiled requested of
        Nothing -> Left (InvalidCompiledStateIndex requested)
        Just (CompiledMRPTerminalState _ payoff) -> Right (CompiledExactTerminalStep payoff)
        Just (CompiledMRPContinuingState _ transition) -> Right (CompiledExactTransitionStep transition)

requireCompiledState ::
    CompiledExactMDP state action ->
    StateIndex ->
    Either CompiledRuntimeError (CompiledExactState state)
requireCompiledState compiled requested =
    case compiledStateAt compiled requested of
        Nothing -> Left (InvalidCompiledStateIndex requested)
        Just state -> Right state

compiledStateAt :: CompiledExactMDP state action -> StateIndex -> Maybe (CompiledExactState state)
compiledStateAt (CompiledExactMDP _ _ _ states) (StateIndex requested) =
    valueAtPosition requested (NonEmpty.toList states)

compiledMRPStateAt :: CompiledExactMRP state -> StateIndex -> Maybe (CompiledExactMRPState state)
compiledMRPStateAt (CompiledExactMRP _ _ states) (StateIndex requested) =
    valueAtPosition requested (NonEmpty.toList states)

compileTransitionDistribution ::
    (Eq state) =>
    FiniteStateIndex state ->
    state ->
    ActionId action ->
    ExactFiniteDist (ExactTransitionOutcome state) ->
    Either (ExactMDPCompileError state action) (ExactFiniteDist CompiledExactOutcome)
compileTransitionDistribution states source selected transition = do
    entries <-
        traverse
            ( \(outcome, mass) -> do
                successor <-
                    case lookupStateIndex states (exactSuccessorState outcome) of
                        Nothing -> Left (ExactMDPUnindexedSuccessor source selected (exactSuccessorState outcome))
                        Just value -> Right value
                Right
                    ( CompiledExactOutcome (exactTransitionReward outcome) successor
                    , exactProbability mass
                    )
            )
            (NonEmpty.toList (exactOutcomes transition))
    mapLeft ExactMDPDistributionError (exactFiniteDist entries)

requireAvailableAction ::
    (Eq action) =>
    state ->
    FiniteActionIndex action ->
    ActionId action ->
    Either (ExactMDPCompileError state action) ActionIndex
requireAvailableAction state actions selected =
    case lookupActionIndex actions selected of
        Nothing -> Left (ExactMDPUnindexedAvailableAction state selected)
        Just index -> Right index

stateValues :: FiniteStateIndex state -> NonEmpty state
stateValues (FiniteStateIndex values) = values

validateFiniteIndex :: (Eq value) => [value] -> Either (FiniteIndexError value) (NonEmpty value)
validateFiniteIndex [] = Left EmptyFiniteIndex
validateFiniteIndex values@(first : remaining) =
    case firstDuplicate values of
        Just duplicate -> Left (DuplicateFiniteIndexValue duplicate)
        Nothing -> Right (first :| remaining)

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate [] = Nothing
firstDuplicate (value : remaining)
    | value `elem` remaining = Just value
    | otherwise = firstDuplicate remaining

lookupPosition :: (Eq value) => value -> [value] -> Maybe Integer
lookupPosition requested = go 0
  where
    go _ [] = Nothing
    go index (value : remaining)
        | value == requested = Just index
        | otherwise = go (index + 1) remaining

valueAtPosition :: Integer -> [value] -> Maybe value
valueAtPosition requested
    | requested < 0 = const Nothing
    | otherwise = go requested
  where
    go _ [] = Nothing
    go 0 (value : _) = Just value
    go remaining (_ : values) = go (remaining - 1) values

mapModelError ::
    state ->
    Either (ExactModelError action) value ->
    Either (ExactMDPCompileError state action) value
mapModelError state = mapLeft (ExactMDPModelError state)

mapLeft :: (error -> otherError) -> Either error value -> Either otherError value
mapLeft wrap = either (Left . wrap) Right
