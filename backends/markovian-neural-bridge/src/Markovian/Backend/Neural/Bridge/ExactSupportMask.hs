{-# LANGUAGE RoleAnnotations #-}

{- | Compile exact finite action availability into sized structural neural masks.

The adapter checks a neural head against one explicit global action layout. It
then preserves each continuing state's exact availability order as the mask's
argmax tie order. Terminal states remain a separate result and never acquire an
empty or fabricated neural mask.
-}
module Markovian.Backend.Neural.Bridge.ExactSupportMask (
    SupportMaskLimitDimension (..),
    SupportMaskLimits (..),
    SupportMaskError (..),
    ActionOutputLayout,
    policyActionOutputLayout,
    denseActionOutputLayout,
    actionOutputLayoutWidth,
    sameActionOutputSupport,
    sameActionOutputLayout,
    ExactSupportMask,
    exactSupportMaskActions,
    exactSupportMaskFlags,
    exactSupportNeuralMask,
    sameExactSupportMaskLayout,
    ExactStateMask,
    foldExactStateMask,
    compileExactSupportMaskAt,
    compileAllExactSupportMasks,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Action (ActionId)
import Markovian.Backend.Neural.Dense (DenseNetwork, denseOutputSize)
import Markovian.Backend.Neural.Mask (
    ActionMask,
    ActionMaskError,
    actionMaskFlags,
    mkActionMask,
 )
import Markovian.Backend.Neural.Policy (
    LinearCategoricalPolicy,
    linearPolicyActionCount,
 )
import Markovian.Compile.Exact (
    ActionIndex,
    CompiledExactMDP,
    CompiledExactState,
    FiniteActionIndex,
    StateIndex,
    actionAtIndex,
    actionIndexValue,
    compiledActionIndex,
    compiledStateEntries,
    finiteActionIndexValues,
    foldCompiledExactState,
    sameFiniteActionIndex,
    sameFiniteActionIndexLayout,
 )
import Numeric.Natural (Natural)

-- | A bounded complete-compilation dimension.
data SupportMaskLimitDimension
    = SupportMaskStates
    | SupportMaskActions
    | SupportMaskWork
    deriving (Eq, Show)

{- | Limits for one complete bridge traversal.

The action limit counts represented global actions plus all local availability
entries. The work limit bounds a conservative preflight charge for layout
comparison, state traversal, global-index lookup, mask validation, and result
construction. Preflight completes before any mask collection is returned.
-}
data SupportMaskLimits = SupportMaskLimits
    { maximumSupportMaskStates :: !Natural
    , maximumSupportMaskActions :: !Natural
    , maximumSupportMaskWork :: !Natural
    }
    deriving (Eq, Show)

-- | Head, layout, state, limit, and internal compiled-index failures.
data SupportMaskError
    = SupportMaskHeadWidthMismatch !Natural !Int
    | SupportMaskGlobalLayoutMismatch
    | SupportMaskLimitExceeded
        { supportMaskLimitDimension :: !SupportMaskLimitDimension
        , supportMaskLimit :: !Natural
        , supportMaskFirstExceededValue :: !Natural
        }
    | SupportMaskInvalidStateIndex !StateIndex
    | SupportMaskInvalidActionIndex !StateIndex !ActionIndex
    | SupportMaskActionIndexOverflow !StateIndex !ActionIndex
    | SupportMaskNeuralMaskFailure !ActionMaskError
    deriving (Eq, Show)

-- | One checked exact action orientation for a neural output head.
data ActionOutputLayout action = ActionOutputLayout !(FiniteActionIndex action) !Int
    deriving (Eq, Show)

type role ActionOutputLayout nominal

-- | Check a linear policy head against an exact global action layout.
policyActionOutputLayout :: FiniteActionIndex action -> LinearCategoricalPolicy -> Either SupportMaskError (ActionOutputLayout action)
policyActionOutputLayout actions policy =
    actionOutputLayout actions (linearPolicyActionCount policy)

-- | Check a dense Q head against an exact global action layout.
denseActionOutputLayout :: FiniteActionIndex action -> DenseNetwork -> Either SupportMaskError (ActionOutputLayout action)
denseActionOutputLayout actions network =
    actionOutputLayout actions (denseOutputSize network)

-- | Complete checked neural output width.
actionOutputLayoutWidth :: ActionOutputLayout action -> Int
actionOutputLayoutWidth (ActionOutputLayout _ width) = width

-- | Compare labelled global action support while ignoring represented order.
sameActionOutputSupport :: (Eq action) => ActionOutputLayout action -> ActionOutputLayout action -> Bool
sameActionOutputSupport (ActionOutputLayout left _) (ActionOutputLayout right _) =
    sameFiniteActionIndex left right

-- | Compare represented global neural output orientation exactly.
sameActionOutputLayout :: (Eq action) => ActionOutputLayout action -> ActionOutputLayout action -> Bool
sameActionOutputLayout (ActionOutputLayout left leftWidth) (ActionOutputLayout right rightWidth) =
    leftWidth == rightWidth && sameFiniteActionIndexLayout left right

-- | Exact labelled availability paired with its structural neural mask.
data ExactSupportMask action = ExactSupportMask ![ActionId action] !ActionMask
    deriving (Eq, Show)

type role ExactSupportMask nominal

-- | Available exact action IDs in model-defined tie order.
exactSupportMaskActions :: ExactSupportMask action -> [ActionId action]
exactSupportMaskActions (ExactSupportMask actions _) = actions

-- | Boolean membership in checked global neural output order.
exactSupportMaskFlags :: ExactSupportMask action -> [Bool]
exactSupportMaskFlags (ExactSupportMask _ mask) = actionMaskFlags mask

-- | The sized mask accepted by neural policy and DQN consumers.
exactSupportNeuralMask :: ExactSupportMask action -> ActionMask
exactSupportNeuralMask (ExactSupportMask _ mask) = mask

-- | Compare labelled availability order and represented neural layout exactly.
sameExactSupportMaskLayout :: (Eq action) => ExactSupportMask action -> ExactSupportMask action -> Bool
sameExactSupportMaskLayout (ExactSupportMask leftActions leftMask) (ExactSupportMask rightActions rightMask) =
    leftActions == rightActions && leftMask == rightMask

-- | A compiled state is explicitly terminal or has nonempty exact support.
data ExactStateMask action
    = ExactTerminalStateMask
    | ExactContinuingStateMask !(ExactSupportMask action)
    deriving (Eq, Show)

type role ExactStateMask nominal

-- | Eliminate a terminal-or-continuing mask without exposing constructors.
foldExactStateMask :: value -> (ExactSupportMask action -> value) -> ExactStateMask action -> value
foldExactStateMask terminal _ ExactTerminalStateMask = terminal
foldExactStateMask _ continuing (ExactContinuingStateMask support) = continuing support

-- | Compile one indexed exact state after checking the complete global layout.
compileExactSupportMaskAt ::
    (Eq action) =>
    SupportMaskLimits ->
    CompiledExactMDP state action ->
    ActionOutputLayout action ->
    StateIndex ->
    Either SupportMaskError (ExactStateMask action)
compileExactSupportMaskAt limits compiled layout requested = do
    preflightCompleteCompilation limits compiled layout
    case lookup requested (NonEmpty.toList (compiledStateEntries compiled)) of
        Nothing -> Left (SupportMaskInvalidStateIndex requested)
        Just state -> compileState compiled layout requested state

-- | Compile every exact state in deterministic state-index order.
compileAllExactSupportMasks ::
    (Eq action) =>
    SupportMaskLimits ->
    CompiledExactMDP state action ->
    ActionOutputLayout action ->
    Either SupportMaskError (NonEmpty (StateIndex, ExactStateMask action))
compileAllExactSupportMasks limits compiled layout = do
    preflightCompleteCompilation limits compiled layout
    traverse compileEntry (compiledStateEntries compiled)
  where
    compileEntry (stateIndex, state) = do
        stateMask <- compileState compiled layout stateIndex state
        Right (stateIndex, stateMask)

compileState ::
    CompiledExactMDP state action ->
    ActionOutputLayout action ->
    StateIndex ->
    CompiledExactState state ->
    Either SupportMaskError (ExactStateMask action)
compileState compiled layout stateIndex =
    foldCompiledExactState
        (\_ _ -> Right ExactTerminalStateMask)
        ( \_ available _ -> do
            entries <- traverse (compileAction compiled stateIndex) available
            let actions = fmap fst entries
                indices = fmap snd entries
            mask <- mapMask (mkActionMask (actionOutputLayoutWidth layout) (NonEmpty.toList indices))
            Right (ExactContinuingStateMask (ExactSupportMask (NonEmpty.toList actions) mask))
        )

compileAction ::
    CompiledExactMDP state action ->
    StateIndex ->
    ActionIndex ->
    Either SupportMaskError (ActionId action, Int)
compileAction compiled stateIndex actionIndex = do
    action <-
        case actionAtIndex (compiledActionIndex compiled) actionIndex of
            Nothing -> Left (SupportMaskInvalidActionIndex stateIndex actionIndex)
            Just value -> Right value
    let rawIndex = actionIndexValue actionIndex
    if rawIndex < 0 || rawIndex > toInteger (maxBound :: Int)
        then Left (SupportMaskActionIndexOverflow stateIndex actionIndex)
        else Right (action, fromInteger rawIndex)

preflightCompleteCompilation ::
    (Eq action) =>
    SupportMaskLimits ->
    CompiledExactMDP state action ->
    ActionOutputLayout action ->
    Either SupportMaskError ()
preflightCompleteCompilation limits compiled (ActionOutputLayout expected _) = do
    globalActions <-
        compareGlobalLayouts
            limits
            (finiteActionIndexValues (compiledActionIndex compiled))
            (finiteActionIndexValues expected)
    (_, _, requiredWork) <-
        preflightStates
            limits
            globalActions
            0
            globalActions
            globalActions
            (NonEmpty.toList (compiledStateEntries compiled))
    checkLimit SupportMaskWork (maximumSupportMaskWork limits) requiredWork

compareGlobalLayouts ::
    (Eq action) =>
    SupportMaskLimits ->
    [ActionId action] ->
    [ActionId action] ->
    Either SupportMaskError Natural
compareGlobalLayouts limits = go 0
  where
    go count [] [] = Right count
    go count (left : lefts) (right : rights) = do
        let next = count + 1
        checkLimit SupportMaskActions (maximumSupportMaskActions limits) next
        if left == right
            then go next lefts rights
            else Left SupportMaskGlobalLayoutMismatch
    go count (_ : _) [] = rejectExtra count
    go count [] (_ : _) = rejectExtra count
    rejectExtra count = do
        checkLimit SupportMaskActions (maximumSupportMaskActions limits) (count + 1)
        Left SupportMaskGlobalLayoutMismatch

preflightStates ::
    SupportMaskLimits ->
    Natural ->
    Natural ->
    Natural ->
    Natural ->
    [(StateIndex, CompiledExactState state)] ->
    Either SupportMaskError (Natural, Natural, Natural)
preflightStates _ _ states actions work [] = Right (states, actions, work)
preflightStates limits globalActions states actions work ((_, compiledState) : remaining) = do
    let nextStates = states + 1
    checkLimit SupportMaskStates (maximumSupportMaskStates limits) nextStates
    let available =
            foldCompiledExactState
                (\_ _ -> [])
                (\_ choices _ -> NonEmpty.toList choices)
                compiledState
    (nextActions, localActions) <- countLocalActions limits actions available
    -- Three state visits conservatively cover preflight, collection
    -- construction, and the requested-state lookup. The per-action term covers
    -- global-index lookup, list materialization, and duplicate validation.
    let nextWork = work + 3 + localActions * (globalActions + localActions + 8)
    preflightStates limits globalActions nextStates nextActions nextWork remaining

countLocalActions ::
    SupportMaskLimits ->
    Natural ->
    [ActionIndex] ->
    Either SupportMaskError (Natural, Natural)
countLocalActions limits initial = go initial 0
  where
    go total local [] = Right (total, local)
    go total local (_ : remaining) = do
        let nextTotal = total + 1
        checkLimit SupportMaskActions (maximumSupportMaskActions limits) nextTotal
        go nextTotal (local + 1) remaining

checkLimit ::
    SupportMaskLimitDimension ->
    Natural ->
    Natural ->
    Either SupportMaskError ()
checkLimit dimension limit actual
    | actual > limit = Left (SupportMaskLimitExceeded dimension limit actual)
    | otherwise = Right ()

actionOutputLayout :: FiniteActionIndex action -> Int -> Either SupportMaskError (ActionOutputLayout action)
actionOutputLayout actions width
    | width < 0 = Left (SupportMaskHeadWidthMismatch 0 width)
    | actual /= fromIntegral width = Left (SupportMaskHeadWidthMismatch actual width)
    | otherwise = Right (ActionOutputLayout actions width)
  where
    actual = boundedActionCardinality width (finiteActionIndexValues actions)

-- Inspect at most the neural width and one witness that the exact layout is longer.
boundedActionCardinality :: Int -> [value] -> Natural
boundedActionCardinality width = go 0
  where
    go count [] = count
    go count (_ : remaining)
        | count >= fromIntegral width = count + 1
        | otherwise = go (count + 1) remaining

mapMask :: Either ActionMaskError value -> Either SupportMaskError value
mapMask = either (Left . SupportMaskNeuralMaskFailure) Right
