{- | Exact discounted control for finite compiled MDPs.

All backups, residuals, bounds, and policy-evaluation solves use 'Rational'.
Actions are visited in each state's model availability order. Greedy selection
replaces the current candidate only on strict @>@, so exact ties select the
first available action.
-}
module Markovian.Interpreter.Control.Exact (
    ExactValueIterationConfig,
    exactValueIterationConfig,
    exactValueIterationDiscount,
    exactValueIterationTolerance,
    exactValueIterationMaximumIterations,
    ExactValueIterationStopReason (..),
    ExactValueIterationReport (..),
    ExactActionValue (..),
    ExactGreedyReport (..),
    ExactControlError (..),
    exactActionValues,
    exactOptimalityResidual,
    extractExactGreedyActions,
    solveCompiledExactControl,
    ExactPolicyIterationConfig,
    exactPolicyIterationConfig,
    exactPolicyIterationDiscount,
    exactPolicyIterationMaximumIterations,
    ExactPolicyIterationStopReason (..),
    ExactLinearSolveError (..),
    ExactPolicyIterationReport (..),
    solveCompiledExactPolicyIteration,
) where

import Data.List (findIndex)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Compile.Exact (
    ActionIndex,
    CompiledExactMDP,
    CompiledExactOutcome (..),
    CompiledRuntimeError,
    StateIndex,
    actionAtIndex,
    compiledActionIndex,
    compiledInitialState,
    compiledSourceState,
    compiledStateEntries,
    foldCompiledExactState,
    stepCompiledExactMDP,
 )
import Markovian.Horizon (Horizon, horizonValue)
import Markovian.Interpreter.Bellman.Exact (
    ExactBellmanTolerance,
    exactBellmanToleranceValue,
 )
import Markovian.MDP (ActionId)
import Markovian.Objective.Exact (
    ExactContractionDiscount,
    exactContractionDiscountValue,
 )
import Markovian.Probability.Exact (ExactFiniteDist, exactOutcomes, exactProbability)
import Markovian.Reward.Exact (ExactReward, exactReward, exactRewardValue)
import Numeric.Natural (Natural)

-- | Validated parameters for bounded exact value iteration.
data ExactValueIterationConfig = ExactValueIterationConfig
    { exactValueIterationDiscount :: !ExactContractionDiscount
    , exactValueIterationTolerance :: !ExactBellmanTolerance
    , exactValueIterationMaximumIterations :: !Horizon
    }
    deriving (Eq, Show)

-- | Construct exact value-iteration configuration from validated values.
exactValueIterationConfig ::
    ExactContractionDiscount ->
    ExactBellmanTolerance ->
    Horizon ->
    ExactValueIterationConfig
exactValueIterationConfig = ExactValueIterationConfig

-- | Why exact value iteration stopped.
data ExactValueIterationStopReason
    = ExactValueIterationConverged
    | ExactValueIterationLimit
    deriving (Eq, Show)

-- | One indexed action value, in model availability order.
data ExactActionValue = ExactActionValue
    { exactActionValueAction :: !ActionIndex
    , exactActionValueReward :: !ExactReward
    }
    deriving (Eq, Show)

-- | Exact finite-iterate values and contraction evidence.
data ExactValueIterationReport = ExactValueIterationReport
    { exactValueIterationConfigUsed :: !ExactValueIterationConfig
    , exactValueIterationBackups :: !Natural
    , exactValueIterationValues :: !(NonEmpty (StateIndex, ExactReward))
    , exactValueIterationInitialValue :: !ExactReward
    , exactValueIterationResidual :: !Rational
    , exactValueIterationValueErrorBound :: !Rational
    , exactValueIterationGreedyPerformanceBound :: !Rational
    , exactValueIterationStopReason :: !ExactValueIterationStopReason
    }
    deriving (Eq, Show)

-- | Greedy choices decoded to model states and user action IDs.
data ExactGreedyReport state action = ExactGreedyReport
    { exactGreedyChoices :: ![(state, ActionId action)]
    , exactGreedyIndexedChoices :: ![(StateIndex, ActionIndex)]
    , exactGreedyActionValues :: ![(StateIndex, NonEmpty ExactActionValue)]
    , exactGreedyResidual :: !Rational
    , exactGreedyPerformanceBound :: !Rational
    }
    deriving (Eq, Show)

-- | Failures from exact optimal-control operations.
data ExactControlError
    = ExactControlRuntimeError !CompiledRuntimeError
    | ExactControlMissingValue !StateIndex
    | ExactControlDuplicateValue !StateIndex
    | ExactControlUnexpectedValue !StateIndex
    | ExactControlTerminalValueMismatch !StateIndex !ExactReward !ExactReward
    | ExactControlMissingActionValue !StateIndex !ActionIndex
    | ExactControlActionDecodeError !StateIndex !ActionIndex
    | ExactControlLinearSolveError !ExactLinearSolveError
    deriving (Eq, Show)

{- | Compute every continuing-state action value from one supplied value vector.

The result preserves state-index order and each state's model availability
order. Terminal states do not appear in the result.
-}
exactActionValues ::
    ExactContractionDiscount ->
    CompiledExactMDP state action ->
    NonEmpty (StateIndex, ExactReward) ->
    Either ExactControlError [(StateIndex, NonEmpty ExactActionValue)]
exactActionValues discount compiled values = do
    validateValueVector compiled values
    fmap concat (traverse valuesForState (NonEmpty.toList (compiledStateEntries compiled)))
  where
    gamma = exactContractionDiscountValue discount

    valuesForState (stateIndex, state) =
        foldCompiledExactState
            (\_ _ -> Right [])
            ( \_ available _ -> do
                actionValues <- traverse (valueForAction stateIndex) available
                Right [(stateIndex, actionValues)]
            )
            state

    valueForAction stateIndex actionIndex = do
        transition <- mapRuntime (stepCompiledExactMDP compiled stateIndex actionIndex)
        value <- transitionValue gamma compiled values transition
        Right (ExactActionValue actionIndex (exactReward value))

{- | Compute the exact Bellman-optimality residual over continuing states.
Terminal-only models have residual zero.
-}
exactOptimalityResidual ::
    ExactContractionDiscount ->
    CompiledExactMDP state action ->
    NonEmpty (StateIndex, ExactReward) ->
    Either ExactControlError Rational
exactOptimalityResidual discount compiled values = do
    actionValues <- exactActionValues discount compiled values
    differences <-
        traverse
            ( \(stateIndex, available) -> do
                current <- requireValue stateIndex values
                let best = exactActionValueReward (selectFirstMaximum available)
                Right (abs (exactRewardValue best - exactRewardValue current))
            )
            actionValues
    Right (maximumOrZero differences)

{- | Extract deterministic greedy actions from a supplied value vector.

Exact ties select the first action in model availability order, independently
of the global action-index order. The performance bound is
@2 * gamma * residual / (1 - gamma)^2@.
-}
extractExactGreedyActions ::
    ExactContractionDiscount ->
    CompiledExactMDP state action ->
    NonEmpty (StateIndex, ExactReward) ->
    Either ExactControlError (ExactGreedyReport state action)
extractExactGreedyActions discount compiled values = do
    allActionValues <- exactActionValues discount compiled values
    indexed <- traverse select allActionValues
    decoded <- traverse decode indexed
    residual <- exactOptimalityResidual discount compiled values
    let gamma = exactContractionDiscountValue discount
    Right
        ExactGreedyReport
            { exactGreedyChoices = decoded
            , exactGreedyIndexedChoices = indexed
            , exactGreedyActionValues = allActionValues
            , exactGreedyResidual = residual
            , exactGreedyPerformanceBound = greedyPerformanceBound gamma residual
            }
  where
    select (stateIndex, available) =
        Right (stateIndex, exactActionValueAction (selectFirstMaximum available))

    decode (stateIndex, actionIndex) = do
        state <- requireSourceState compiled stateIndex
        action <-
            case actionAtIndex (compiledActionIndex compiled) actionIndex of
                Nothing -> Left (ExactControlActionDecodeError stateIndex actionIndex)
                Just value -> Right value
        Right (state, action)

{- | Run bounded discounted value iteration from terminal payoffs and zero
continuing-state values.

The residual and both bounds are evaluated at the returned vector. A zero
iteration limit performs no backups. This function reports a bounded finite
iterate; it does not label that iterate an exact optimum.
-}
solveCompiledExactControl ::
    ExactValueIterationConfig ->
    CompiledExactMDP state action ->
    Either ExactControlError ExactValueIterationReport
solveCompiledExactControl config compiled = iterateValues 0 initialValues
  where
    discount = exactValueIterationDiscount config
    gamma = exactContractionDiscountValue discount
    tolerance = exactBellmanToleranceValue (exactValueIterationTolerance config)
    maximumIterations = horizonValue (exactValueIterationMaximumIterations config)
    entries = compiledStateEntries compiled
    initialValues = fmap baseValue entries

    baseValue (stateIndex, state) =
        foldCompiledExactState
            (\_ payoff -> (stateIndex, payoff))
            (\_ _ _ -> (stateIndex, exactReward 0))
            state

    iterateValues completed values = do
        residual <- exactOptimalityResidual discount compiled values
        let valueBound = residual / (1 - gamma)
            performanceBound = greedyPerformanceBound gamma residual
        if valueBound <= tolerance
            then makeReport completed values residual valueBound performanceBound ExactValueIterationConverged
            else
                if completed >= maximumIterations
                    then makeReport completed values residual valueBound performanceBound ExactValueIterationLimit
                    else do
                        updated <- optimalityBackup discount compiled values
                        iterateValues (completed + 1) updated

    makeReport completed values residual valueBound performanceBound reason = do
        initial <- requireValue (compiledInitialState compiled) values
        Right
            ExactValueIterationReport
                { exactValueIterationConfigUsed = config
                , exactValueIterationBackups = completed
                , exactValueIterationValues = values
                , exactValueIterationInitialValue = initial
                , exactValueIterationResidual = residual
                , exactValueIterationValueErrorBound = valueBound
                , exactValueIterationGreedyPerformanceBound = performanceBound
                , exactValueIterationStopReason = reason
                }

-- | Validated parameters for exact deterministic policy iteration.
data ExactPolicyIterationConfig = ExactPolicyIterationConfig
    { exactPolicyIterationDiscount :: !ExactContractionDiscount
    , exactPolicyIterationMaximumIterations :: !Horizon
    }
    deriving (Eq, Show)

-- | Construct exact policy-iteration configuration from validated values.
exactPolicyIterationConfig :: ExactContractionDiscount -> Horizon -> ExactPolicyIterationConfig
exactPolicyIterationConfig = ExactPolicyIterationConfig

-- | Why exact policy iteration stopped.
data ExactPolicyIterationStopReason
    = ExactPolicyIterationStable
    | ExactPolicyIterationLimit
    deriving (Eq, Show)

-- | Structured failures from the signed rational Gaussian solver.
data ExactLinearSolveError
    = ExactLinearNonSquareSystem !Integer !Integer
    | ExactLinearRightHandSideMismatch !Integer !Integer
    | ExactLinearSingularSystem !Integer
    | ExactLinearResultInvariant !Integer
    deriving (Eq, Show)

-- | Exact values and deterministic choices returned by policy iteration.
data ExactPolicyIterationReport state action = ExactPolicyIterationReport
    { exactPolicyIterationConfigUsed :: !ExactPolicyIterationConfig
    , exactPolicyIterationImprovements :: !Natural
    , exactPolicyIterationValues :: !(NonEmpty (StateIndex, ExactReward))
    , exactPolicyIterationInitialValue :: !ExactReward
    , exactPolicyIterationChoices :: ![(state, ActionId action)]
    , exactPolicyIterationIndexedChoices :: ![(StateIndex, ActionIndex)]
    , exactPolicyIterationResidual :: !Rational
    , exactPolicyIterationStopReason :: !ExactPolicyIterationStopReason
    }
    deriving (Eq, Show)

{- | Run exact deterministic policy iteration.

The initial policy selects the first model-available action in each continuing
state. Each policy is evaluated by signed 'Rational' Gaussian elimination.
Improvement selects the first exact maximizer and continues only when some
state has a strict value improvement.
-}
solveCompiledExactPolicyIteration ::
    ExactPolicyIterationConfig ->
    CompiledExactMDP state action ->
    Either ExactControlError (ExactPolicyIterationReport state action)
solveCompiledExactPolicyIteration config compiled = do
    initialPolicy <- firstAvailablePolicy compiled
    iteratePolicies 0 initialPolicy
  where
    discount = exactPolicyIterationDiscount config
    maximumIterations = horizonValue (exactPolicyIterationMaximumIterations config)

    iteratePolicies completed current = do
        values <- evaluateDeterministicPolicy discount compiled current
        actionValues <- exactActionValues discount compiled values
        improved <- traverse improveState actionValues
        let next = fmap fst improved
            hasStrictImprovement = any snd improved
        if not hasStrictImprovement
            then makeReport completed next values ExactPolicyIterationStable
            else
                if completed >= maximumIterations
                    then makeReport completed current values ExactPolicyIterationLimit
                    else iteratePolicies (completed + 1) next
      where
        improveState (stateIndex, available) = do
            currentAction <- requirePolicyAction stateIndex current
            currentValue <- requireActionValue stateIndex currentAction available
            let best = selectFirstMaximum available
            Right
                ( (stateIndex, exactActionValueAction best)
                , exactRewardValue (exactActionValueReward best)
                    > exactRewardValue currentValue
                )

    makeReport completed choices values reason = do
        initial <- requireValue (compiledInitialState compiled) values
        decoded <- traverse decode choices
        residual <- exactOptimalityResidual discount compiled values
        Right
            ExactPolicyIterationReport
                { exactPolicyIterationConfigUsed = config
                , exactPolicyIterationImprovements = completed
                , exactPolicyIterationValues = values
                , exactPolicyIterationInitialValue = initial
                , exactPolicyIterationChoices = decoded
                , exactPolicyIterationIndexedChoices = choices
                , exactPolicyIterationResidual = residual
                , exactPolicyIterationStopReason = reason
                }

    decode (stateIndex, actionIndex) = do
        state <- requireSourceState compiled stateIndex
        action <-
            case actionAtIndex (compiledActionIndex compiled) actionIndex of
                Nothing -> Left (ExactControlActionDecodeError stateIndex actionIndex)
                Just value -> Right value
        Right (state, action)

optimalityBackup ::
    ExactContractionDiscount ->
    CompiledExactMDP state action ->
    NonEmpty (StateIndex, ExactReward) ->
    Either ExactControlError (NonEmpty (StateIndex, ExactReward))
optimalityBackup discount compiled values = do
    actionValues <- exactActionValues discount compiled values
    traverse (backupState actionValues) (compiledStateEntries compiled)
  where
    backupState allActionValues (stateIndex, state) =
        foldCompiledExactState
            (\_ payoff -> Right (stateIndex, payoff))
            ( \_ _ _ ->
                case lookup stateIndex allActionValues of
                    Nothing -> Left (ExactControlMissingValue stateIndex)
                    Just available ->
                        Right (stateIndex, exactActionValueReward (selectFirstMaximum available))
            )
            state

evaluateDeterministicPolicy ::
    ExactContractionDiscount ->
    CompiledExactMDP state action ->
    [(StateIndex, ActionIndex)] ->
    Either ExactControlError (NonEmpty (StateIndex, ExactReward))
evaluateDeterministicPolicy discount compiled policy = do
    system <- traverse equation (NonEmpty.toList entries)
    let coefficients = fmap fst system
        rightHandSide = fmap snd system
    solution <- mapLinear (solveRationalSystem coefficients rightHandSide)
    case NonEmpty.nonEmpty (zipWith (\(stateIndex, _) value -> (stateIndex, exactReward value)) (NonEmpty.toList entries) solution) of
        Nothing -> Left (ExactControlLinearSolveError (ExactLinearResultInvariant 0))
        Just values -> Right values
  where
    entries = compiledStateEntries compiled
    gamma = exactContractionDiscountValue discount
    indexes = fmap fst (NonEmpty.toList entries)

    equation (stateIndex, state) =
        foldCompiledExactState
            ( \_ payoff ->
                Right
                    ( fmap (\column -> if column == stateIndex then 1 else 0) indexes
                    , exactRewardValue payoff
                    )
            )
            ( \_ _ _ -> do
                selected <- requirePolicyAction stateIndex policy
                transition <- mapRuntime (stepCompiledExactMDP compiled stateIndex selected)
                let coefficient column =
                        (if column == stateIndex then 1 else 0)
                            - gamma * successorMass column transition
                    reward =
                        sum
                            [ exactProbability mass * exactRewardValue (compiledTransitionReward outcome)
                            | (outcome, mass) <- NonEmpty.toList (exactOutcomes transition)
                            ]
                Right (fmap coefficient indexes, reward)
            )
            state

successorMass :: StateIndex -> ExactFiniteDist CompiledExactOutcome -> Rational
successorMass requested distribution =
    sum
        [ exactProbability mass
        | (outcome, mass) <- NonEmpty.toList (exactOutcomes distribution)
        , compiledSuccessorState outcome == requested
        ]

transitionValue ::
    Rational ->
    CompiledExactMDP state action ->
    NonEmpty (StateIndex, ExactReward) ->
    ExactFiniteDist CompiledExactOutcome ->
    Either ExactControlError Rational
transitionValue gamma compiled values distribution = do
    contributions <-
        traverse
            ( \(outcome, mass) -> do
                successor <- successorValue (compiledSuccessorState outcome)
                Right
                    ( exactProbability mass
                        * ( exactRewardValue (compiledTransitionReward outcome)
                                + gamma * exactRewardValue successor
                          )
                    )
            )
            (NonEmpty.toList (exactOutcomes distribution))
    Right (sum contributions)
  where
    successorValue requested =
        case lookup requested (NonEmpty.toList (compiledStateEntries compiled)) of
            Just state ->
                foldCompiledExactState
                    (\_ payoff -> Right payoff)
                    (\_ _ _ -> requireValue requested values)
                    state
            Nothing -> Left (ExactControlMissingValue requested)

validateValueVector ::
    CompiledExactMDP state action ->
    NonEmpty (StateIndex, ExactReward) ->
    Either ExactControlError ()
validateValueVector compiled supplied = do
    case firstDuplicate (fmap fst suppliedList) of
        Just duplicate -> Left (ExactControlDuplicateValue duplicate)
        Nothing -> Right ()
    case [index | (index, _) <- suppliedList, index `notElem` expectedIndexes] of
        unexpected : _ -> Left (ExactControlUnexpectedValue unexpected)
        [] -> Right ()
    case [index | index <- expectedIndexes, index `notElem` fmap fst suppliedList] of
        missing : _ -> Left (ExactControlMissingValue missing)
        [] -> Right ()
    traverse_ validateTerminal expectedEntries
  where
    suppliedList = NonEmpty.toList supplied
    expectedEntries = NonEmpty.toList (compiledStateEntries compiled)
    expectedIndexes = fmap fst expectedEntries
    validateTerminal (index, state) =
        foldCompiledExactState
            ( \_ expected ->
                case lookup index suppliedList of
                    Just actual
                        | actual == expected -> Right ()
                        | otherwise -> Left (ExactControlTerminalValueMismatch index expected actual)
                    Nothing -> Left (ExactControlMissingValue index)
            )
            (\_ _ _ -> Right ())
            state

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate [] = Nothing
firstDuplicate (value : remaining)
    | value `elem` remaining = Just value
    | otherwise = firstDuplicate remaining

traverse_ :: (value -> Either error ()) -> [value] -> Either error ()
traverse_ operation = foldr (\value rest -> operation value >> rest) (Right ())

firstAvailablePolicy ::
    CompiledExactMDP state action ->
    Either ExactControlError [(StateIndex, ActionIndex)]
firstAvailablePolicy compiled =
    fmap concat (traverse firstChoice (NonEmpty.toList (compiledStateEntries compiled)))
  where
    firstChoice (stateIndex, state) =
        foldCompiledExactState
            (\_ _ -> Right [])
            (\_ (firstAction :| _) _ -> Right [(stateIndex, firstAction)])
            state

selectFirstMaximum :: NonEmpty ExactActionValue -> ExactActionValue
selectFirstMaximum (first :| remaining) = foldl choose first remaining
  where
    choose selected candidate
        | exactRewardValue (exactActionValueReward candidate)
            > exactRewardValue (exactActionValueReward selected) =
            candidate
        | otherwise = selected

requireValue ::
    StateIndex ->
    NonEmpty (StateIndex, ExactReward) ->
    Either ExactControlError ExactReward
requireValue requested values =
    case lookup requested (NonEmpty.toList values) of
        Nothing -> Left (ExactControlMissingValue requested)
        Just value -> Right value

requirePolicyAction ::
    StateIndex ->
    [(StateIndex, ActionIndex)] ->
    Either ExactControlError ActionIndex
requirePolicyAction requested choices =
    case lookup requested choices of
        Nothing -> Left (ExactControlMissingValue requested)
        Just action -> Right action

requireActionValue ::
    StateIndex ->
    ActionIndex ->
    NonEmpty ExactActionValue ->
    Either ExactControlError ExactReward
requireActionValue stateIndex requested available =
    case [ exactActionValueReward value
         | value <- NonEmpty.toList available
         , exactActionValueAction value == requested
         ] of
        [] -> Left (ExactControlMissingActionValue stateIndex requested)
        value : _ -> Right value

requireSourceState ::
    CompiledExactMDP state action ->
    StateIndex ->
    Either ExactControlError state
requireSourceState compiled requested =
    case [ compiledSourceState state
         | (stateIndex, state) <- NonEmpty.toList (compiledStateEntries compiled)
         , stateIndex == requested
         ] of
        [] -> Left (ExactControlMissingValue requested)
        state : _ -> Right state

greedyPerformanceBound :: Rational -> Rational -> Rational
greedyPerformanceBound gamma residual =
    2 * gamma * residual / ((1 - gamma) * (1 - gamma))

maximumOrZero :: [Rational] -> Rational
maximumOrZero [] = 0
maximumOrZero (first : remaining) = foldl max first remaining

mapRuntime :: Either CompiledRuntimeError value -> Either ExactControlError value
mapRuntime = either (Left . ExactControlRuntimeError) Right

mapLinear :: Either ExactLinearSolveError value -> Either ExactControlError value
mapLinear = either (Left . ExactControlLinearSolveError) Right

solveRationalSystem :: [[Rational]] -> [Rational] -> Either ExactLinearSolveError [Rational]
solveRationalSystem coefficients rightHandSide = do
    let rowCount = length coefficients
        columnCounts = fmap length coefficients
    case [count | count <- columnCounts, count /= rowCount] of
        count : _ -> Left (ExactLinearNonSquareSystem (fromIntegral rowCount) (fromIntegral count))
        [] -> Right ()
    if length rightHandSide /= rowCount
        then Left (ExactLinearRightHandSideMismatch (fromIntegral rowCount) (fromIntegral (length rightHandSide)))
        else do
            reduced <- eliminate 0 (zipWith (++) coefficients (fmap (: []) rightHandSide))
            traverse (resultAt rowCount) (zip [0 :: Int ..] reduced)
  where
    eliminate column rows
        | column >= length rows = Right rows
        | otherwise = do
            pivotOffset <-
                case findIndex (\row -> valueAt column row /= Just 0) (drop column rows) of
                    Nothing -> Left (ExactLinearSingularSystem (fromIntegral column))
                    Just offset -> Right offset
            swapped <- swapRows column (column + pivotOffset) rows
            pivotRow <- requireListValue column swapped
            pivot <- requireRational column pivotRow
            let normalized = fmap (/ pivot) pivotRow
                reduced =
                    [ if rowIndex == column
                        then normalized
                        else eliminateRow column normalized row
                    | (rowIndex, row) <- zip [0 ..] swapped
                    ]
            eliminate (column + 1) reduced

    eliminateRow column pivotRow row =
        case valueAt column row of
            Nothing -> row
            Just factor -> zipWith (\value pivotValue -> value - factor * pivotValue) row pivotRow

    resultAt size (rowIndex, row) =
        case valueAt size row of
            Nothing -> Left (ExactLinearResultInvariant (fromIntegral rowIndex))
            Just value -> Right value

swapRows :: Int -> Int -> [value] -> Either ExactLinearSolveError [value]
swapRows left right values = do
    leftValue <- requireListValue left values
    rightValue <- requireListValue right values
    traverse
        ( \(index, value) ->
            if index == left
                then Right rightValue
                else
                    if index == right
                        then Right leftValue
                        else Right value
        )
        (zip [0 ..] values)

requireListValue :: Int -> [value] -> Either ExactLinearSolveError value
requireListValue requested values =
    case valueAt requested values of
        Nothing -> Left (ExactLinearResultInvariant (fromIntegral requested))
        Just value -> Right value

requireRational :: Int -> [Rational] -> Either ExactLinearSolveError Rational
requireRational requested values =
    case valueAt requested values of
        Nothing -> Left (ExactLinearResultInvariant (fromIntegral requested))
        Just value -> Right value

valueAt :: Int -> [value] -> Maybe value
valueAt requested
    | requested < 0 = const Nothing
    | otherwise = go requested
  where
    go _ [] = Nothing
    go 0 (value : _) = Just value
    go remaining (_ : values) = go (remaining - 1) values
