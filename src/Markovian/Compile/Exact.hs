{- | Validated finite indexing and compilation for exact MDP policies.

Compilation exhaustively validates the supplied finite state and action
supports. The result contains only integer indexes and exact distributions.
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
    ExactCompileError (..),
    CompiledExactOutcome (..),
    CompiledExactState (..),
    CompiledExactMDP,
    compileExactPolicyMDP,
    compiledInitialState,
    compiledStateIndex,
    compiledActionIndex,
    compiledStates,
    compiledStateEntries,
    CompiledExactStep (..),
    CompiledRuntimeError (..),
    stepCompiledExactPolicy,
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

-- | Exhaustive exact-compilation failures.
data ExactCompileError state action
    = StateIndexError !(FiniteIndexError state)
    | ActionIndexError !(FiniteIndexError (ActionId action))
    | UnindexedInitialState !state
    | CompileModelError !state !(ExactModelError action)
    | CompilePolicyError !state !(ExactPolicyError action)
    | UnindexedAvailableAction !state !(ActionId action)
    | UnindexedPolicyAction !state !(ActionId action)
    | UnindexedSuccessor !state !(ActionId action) !state
    | CompileDistributionError !ExactDistributionError
    deriving (Eq, Show)

-- | A compiled transition reward paired with an indexed successor.
data CompiledExactOutcome = CompiledExactOutcome
    { compiledTransitionReward :: !ExactReward
    , compiledSuccessorState :: !StateIndex
    }
    deriving (Eq, Show)

-- | One exhaustively validated compiled state.
data CompiledExactState state
    = CompiledTerminalState
        { compiledSourceState :: !state
        , compiledTerminalPayoff :: !ExactReward
        }
    | CompiledContinuingState
        { compiledSourceState :: !state
        , compiledAvailableActions :: !(NonEmpty ActionIndex)
        , compiledPolicyActions :: !(ExactFiniteDist ActionIndex)
        , compiledTransitions :: ![(ActionIndex, ExactFiniteDist CompiledExactOutcome)]
        }
    deriving (Eq, Show)

-- | An exact MDP and policy compiled over explicit finite supports.
data CompiledExactMDP state action
    = CompiledExactMDP
        !StateIndex
        !(FiniteStateIndex state)
        !(FiniteActionIndex action)
        !(NonEmpty (CompiledExactState state))

-- | Compile and exhaustively validate an exact policy and MDP.
compileExactPolicyMDP ::
    (Eq state, Eq action) =>
    [state] ->
    [ActionId action] ->
    ExactMDP state action ->
    ExactPolicy state action ->
    Either (ExactCompileError state action) (CompiledExactMDP state action)
compileExactPolicyMDP stateSupport actionSupport model selectedPolicy = do
    states <- mapIndexError StateIndexError (finiteStateIndex stateSupport)
    actions <- mapIndexError ActionIndexError (finiteActionIndex actionSupport)
    initial <-
        case lookupStateIndex states (exactMDPInitialState model) of
            Nothing -> Left (UnindexedInitialState (exactMDPInitialState model))
            Just index -> Right index
    compiled <- traverse (compileState states actions) (stateValues states)
    Right (CompiledExactMDP initial states actions compiled)
  where
    compileState states actions state = do
        decision <- mapModelError state (inspectExactMDP model state)
        case decision of
            ExactTerminalDecision payoff -> Right (CompiledTerminalState state payoff)
            ExactActionDecision available -> do
                let selected = exactPolicyActions selectedPolicy state
                mapPolicyError state (validateExactPolicySupport available selected)
                availableIndexes <- traverse (requireAvailableAction state actions) available
                selectedIndexes <- compilePolicyDistribution state actions selected
                transitions <-
                    traverse
                        (compileTransition states actions state)
                        (NonEmpty.toList available)
                Right
                    CompiledContinuingState
                        { compiledSourceState = state
                        , compiledAvailableActions = availableIndexes
                        , compiledPolicyActions = selectedIndexes
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

-- | Read all compiled states in index order.
compiledStates :: CompiledExactMDP state action -> NonEmpty (CompiledExactState state)
compiledStates (CompiledExactMDP _ _ _ states) = states

-- | Read compiled states paired with their zero-based indexes.
compiledStateEntries :: CompiledExactMDP state action -> NonEmpty (StateIndex, CompiledExactState state)
compiledStateEntries compiled =
    NonEmpty.zip (StateIndex 0 :| fmap StateIndex [1 ..]) (compiledStates compiled)

-- | One compiled closed-policy layer.
data CompiledExactStep
    = CompiledExactTerminalStep !ExactReward
    | CompiledExactTransitionStep !(ExactFiniteDist CompiledExactOutcome)
    deriving (Eq, Show)

-- | Runtime invariant failures from an otherwise compiled model.
data CompiledRuntimeError
    = InvalidCompiledStateIndex !StateIndex
    | MissingCompiledTransition !StateIndex !ActionIndex
    | CompiledRuntimeDistributionError !ExactDistributionError
    deriving (Eq, Show)

-- | Step one compiled state under its compiled exact policy.
stepCompiledExactPolicy ::
    CompiledExactMDP state action ->
    StateIndex ->
    Either CompiledRuntimeError CompiledExactStep
stepCompiledExactPolicy compiled requested = do
    state <-
        case compiledStateAt compiled requested of
            Nothing -> Left (InvalidCompiledStateIndex requested)
            Just value -> Right value
    case state of
        CompiledTerminalState _ payoff -> Right (CompiledExactTerminalStep payoff)
        CompiledContinuingState _ _ selected transitions -> do
            branches <-
                fmap
                    concat
                    ( traverse
                        (selectedBranches requested transitions)
                        (NonEmpty.toList (exactOutcomes selected))
                    )
            case exactFiniteDist branches of
                Left err -> Left (CompiledRuntimeDistributionError err)
                Right distribution -> Right (CompiledExactTransitionStep distribution)
  where
    selectedBranches stateIndex transitions (selectedAction, actionMass) =
        case lookup selectedAction transitions of
            Nothing -> Left (MissingCompiledTransition stateIndex selectedAction)
            Just transition ->
                Right
                    [ (outcome, exactProbability actionMass * exactProbability transitionMass)
                    | (outcome, transitionMass) <- NonEmpty.toList (exactOutcomes transition)
                    ]

compiledStateAt :: CompiledExactMDP state action -> StateIndex -> Maybe (CompiledExactState state)
compiledStateAt (CompiledExactMDP _ _ _ states) (StateIndex requested) =
    valueAtPosition requested (NonEmpty.toList states)

compilePolicyDistribution ::
    (Eq action) =>
    state ->
    FiniteActionIndex action ->
    ExactFiniteDist (ActionId action) ->
    Either (ExactCompileError state action) (ExactFiniteDist ActionIndex)
compilePolicyDistribution state actions selected = do
    entries <-
        traverse
            ( \(selectedAction, mass) -> do
                index <-
                    case lookupActionIndex actions selectedAction of
                        Nothing -> Left (UnindexedPolicyAction state selectedAction)
                        Just value -> Right value
                Right (index, exactProbability mass)
            )
            (NonEmpty.toList (exactOutcomes selected))
    mapDistributionError (exactFiniteDist entries)

compileTransitionDistribution ::
    (Eq state) =>
    FiniteStateIndex state ->
    state ->
    ActionId action ->
    ExactFiniteDist (ExactTransitionOutcome state) ->
    Either (ExactCompileError state action) (ExactFiniteDist CompiledExactOutcome)
compileTransitionDistribution states source selected transition = do
    entries <-
        traverse
            ( \(outcome, mass) -> do
                successor <-
                    case lookupStateIndex states (exactSuccessorState outcome) of
                        Nothing -> Left (UnindexedSuccessor source selected (exactSuccessorState outcome))
                        Just value -> Right value
                Right
                    ( CompiledExactOutcome (exactTransitionReward outcome) successor
                    , exactProbability mass
                    )
            )
            (NonEmpty.toList (exactOutcomes transition))
    mapDistributionError (exactFiniteDist entries)

requireAvailableAction ::
    (Eq action) =>
    state ->
    FiniteActionIndex action ->
    ActionId action ->
    Either (ExactCompileError state action) ActionIndex
requireAvailableAction state actions selected =
    case lookupActionIndex actions selected of
        Nothing -> Left (UnindexedAvailableAction state selected)
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

mapIndexError ::
    (error -> compiledError) ->
    Either error value ->
    Either compiledError value
mapIndexError wrap = either (Left . wrap) Right

mapModelError ::
    state ->
    Either (ExactModelError action) value ->
    Either (ExactCompileError state action) value
mapModelError state = either (Left . CompileModelError state) Right

mapPolicyError ::
    state ->
    Either (ExactPolicyError action) value ->
    Either (ExactCompileError state action) value
mapPolicyError state = either (Left . CompilePolicyError state) Right

mapDistributionError ::
    Either ExactDistributionError value ->
    Either (ExactCompileError state action) value
mapDistributionError = either (Left . CompileDistributionError) Right
