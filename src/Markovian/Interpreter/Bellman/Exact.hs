{- | Exact discounted Bellman fixed-point evaluation for compiled policies.

The solver uses the sup norm. For contraction factor @gamma@ and Bellman
residual @r = ||T v - v||_infinity@, the reported stopping bound is
@r / (1 - gamma)@.
-}
module Markovian.Interpreter.Bellman.Exact (
    ExactBellmanTolerance,
    ExactBellmanToleranceError (..),
    mkExactBellmanTolerance,
    exactBellmanToleranceValue,
    ExactBellmanConfig,
    exactBellmanConfig,
    exactBellmanDiscount,
    exactBellmanTolerance,
    exactBellmanMaximumIterations,
    BellmanStopReason (..),
    ExactBellmanError (..),
    ExactBellmanReport (..),
    solveCompiledExactPolicy,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Semigroup (Max (..), sconcat)
import Markovian.Compile.Exact (
    CompiledExactMDP,
    CompiledExactOutcome (..),
    CompiledExactState (..),
    CompiledExactStep (..),
    CompiledRuntimeError,
    StateIndex,
    compiledInitialState,
    compiledStateEntries,
    stepCompiledExactPolicy,
 )
import Markovian.Horizon (Horizon, horizonValue)
import Markovian.Objective.Exact (
    ExactContractionDiscount,
    exactContractionDiscountValue,
 )
import Markovian.Probability.Exact (exactOutcomes, exactProbability)
import Markovian.Reward.Exact (ExactReward, exactReward, exactRewardValue)
import Numeric.Natural (Natural)

-- | A strictly positive exact sup-norm error target.
newtype ExactBellmanTolerance = ExactBellmanTolerance Rational
    deriving (Eq, Ord, Show)

-- | Errors from exact tolerance construction.
newtype ExactBellmanToleranceError = NonPositiveExactBellmanTolerance Rational
    deriving (Eq, Show)

-- | Validate a strictly positive exact tolerance.
mkExactBellmanTolerance :: Rational -> Either ExactBellmanToleranceError ExactBellmanTolerance
mkExactBellmanTolerance value
    | value <= 0 = Left (NonPositiveExactBellmanTolerance value)
    | otherwise = Right (ExactBellmanTolerance value)

-- | Read an exact Bellman tolerance.
exactBellmanToleranceValue :: ExactBellmanTolerance -> Rational
exactBellmanToleranceValue (ExactBellmanTolerance value) = value

-- | Complete behavior-changing configuration for exact policy evaluation.
data ExactBellmanConfig = ExactBellmanConfig
    { exactBellmanDiscount :: !ExactContractionDiscount
    , exactBellmanTolerance :: !ExactBellmanTolerance
    , exactBellmanMaximumIterations :: !Horizon
    }
    deriving (Eq, Show)

-- | Construct exact Bellman configuration from validated values.
exactBellmanConfig :: ExactContractionDiscount -> ExactBellmanTolerance -> Horizon -> ExactBellmanConfig
exactBellmanConfig = ExactBellmanConfig

-- | Why fixed-point iteration stopped.
data BellmanStopReason
    = BellmanConverged
    | BellmanIterationLimit
    deriving (Eq, Show)

-- | Runtime failures while applying the compiled Bellman operator.
data ExactBellmanError
    = ExactBellmanRuntimeError !CompiledRuntimeError
    | MissingExactBellmanValue !StateIndex
    deriving (Eq, Show)

-- | Values and proof-relevant convergence information.
data ExactBellmanReport = ExactBellmanReport
    { exactBellmanConfigUsed :: !ExactBellmanConfig
    , exactBellmanIterations :: !Natural
    , exactBellmanResidual :: !Rational
    , exactBellmanStoppingBound :: !Rational
    , exactBellmanStopReason :: !BellmanStopReason
    , exactBellmanValues :: !(NonEmpty (StateIndex, ExactReward))
    , exactBellmanInitialValue :: !ExactReward
    }
    deriving (Eq, Show)

{- | Solve one compiled exact policy's discounted Bellman fixed point.

Terminal values are clamped to terminal payoffs on initialization and every
backup. The returned residual is evaluated at the returned value vector.
-}
solveCompiledExactPolicy ::
    ExactBellmanConfig ->
    CompiledExactMDP state action ->
    Either ExactBellmanError ExactBellmanReport
solveCompiledExactPolicy config compiled = iterateValues 0 initialValues
  where
    entries = compiledStateEntries compiled
    gamma = exactContractionDiscountValue (exactBellmanDiscount config)
    tolerance = exactBellmanToleranceValue (exactBellmanTolerance config)
    maximumIterations = horizonValue (exactBellmanMaximumIterations config)
    initialValues = fmap baseValue entries

    baseValue (index, state) =
        case state of
            CompiledTerminalState _ payoff -> (index, payoff)
            CompiledContinuingState{} -> (index, exactReward 0)

    iterateValues iteration values = do
        next <- traverse (backup values) entries
        residual <- supDistance values next
        let stoppingBound = residual / (1 - gamma)
            converged = stoppingBound <= tolerance
            exhausted = iteration >= maximumIterations
        if converged || exhausted
            then makeReport iteration residual stoppingBound values converged
            else iterateValues (iteration + 1) next

    makeReport iteration residual stoppingBound values converged = do
        initial <- requireValue (compiledInitialState compiled) values
        Right
            ExactBellmanReport
                { exactBellmanConfigUsed = config
                , exactBellmanIterations = iteration
                , exactBellmanResidual = residual
                , exactBellmanStoppingBound = stoppingBound
                , exactBellmanStopReason =
                    if converged then BellmanConverged else BellmanIterationLimit
                , exactBellmanValues = values
                , exactBellmanInitialValue = initial
                }

    backup previous (index, state) =
        case state of
            CompiledTerminalState _ payoff -> Right (index, payoff)
            CompiledContinuingState{} -> do
                step <- mapRuntimeError (stepCompiledExactPolicy compiled index)
                case step of
                    CompiledExactTerminalStep payoff -> Right (index, payoff)
                    CompiledExactTransitionStep distribution -> do
                        contributions <-
                            traverse
                                (outcomeContribution previous)
                                (NonEmpty.toList (exactOutcomes distribution))
                        Right (index, exactReward (sum contributions))

    outcomeContribution previous (outcome, mass) = do
        future <- requireValue (compiledSuccessorState outcome) previous
        Right
            ( exactProbability mass
                * ( exactRewardValue (compiledTransitionReward outcome)
                        + gamma * exactRewardValue future
                  )
            )

supDistance ::
    NonEmpty (StateIndex, ExactReward) ->
    NonEmpty (StateIndex, ExactReward) ->
    Either ExactBellmanError Rational
supDistance left right = do
    differences <-
        traverse
            ( \(index, value) -> do
                other <- requireValue index right
                Right (abs (exactRewardValue value - exactRewardValue other))
            )
            left
    Right (getMax (sconcat (fmap Max differences)))

requireValue ::
    StateIndex ->
    NonEmpty (StateIndex, ExactReward) ->
    Either ExactBellmanError ExactReward
requireValue requested values =
    case lookup requested (NonEmpty.toList values) of
        Nothing -> Left (MissingExactBellmanValue requested)
        Just value -> Right value

mapRuntimeError :: Either CompiledRuntimeError value -> Either ExactBellmanError value
mapRuntimeError = either (Left . ExactBellmanRuntimeError) Right
