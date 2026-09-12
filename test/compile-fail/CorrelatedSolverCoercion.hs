module CorrelatedSolverCoercion where

import Data.Coerce (coerce)
import Markovian.Game.Correlated.Exact

newtype OtherOwner = OtherOwner String
newtype OtherAction = OtherAction Bool

-- Owner/action coercion. Both solution types and the solve error carry
-- nominal owner/action roles, so no representational coercion of either
-- parameter is available in either direction.

badCorrelatedSolutionOwnerCoerce ::
    CorrelatedEquilibriumSolution String Bool ->
    CorrelatedEquilibriumSolution OtherOwner Bool
badCorrelatedSolutionOwnerCoerce = coerce

badCorrelatedSolutionActionCoerce ::
    CorrelatedEquilibriumSolution String Bool ->
    CorrelatedEquilibriumSolution String OtherAction
badCorrelatedSolutionActionCoerce = coerce

badCoarseSolutionOwnerCoerce ::
    CoarseCorrelatedEquilibriumSolution String Bool ->
    CoarseCorrelatedEquilibriumSolution OtherOwner Bool
badCoarseSolutionOwnerCoerce = coerce

badCoarseSolutionActionCoerce ::
    CoarseCorrelatedEquilibriumSolution String Bool ->
    CoarseCorrelatedEquilibriumSolution String OtherAction
badCoarseSolutionActionCoerce = coerce

badSolveErrorOwnerCoerce ::
    CorrelationSolveError String Bool -> CorrelationSolveError OtherOwner Bool
badSolveErrorOwnerCoerce = coerce

badSolveErrorActionCoerce ::
    CorrelationSolveError String Bool -> CorrelationSolveError String OtherAction
badSolveErrorActionCoerce = coerce
