module CorrelatedSolverInterchange where

import Data.Coerce (coerce)
import Markovian.Game.Correlated.Exact

-- CE/CCE interchange. Contract §3 requires that CE and CCE solution types are
-- not interchangeable. Neither substitution nor coercion may convert one into
-- the other, and neither mode's accessor may be applied to the other's
-- solution.

badSolutionInterchange ::
    CorrelatedEquilibriumSolution owner action ->
    CoarseCorrelatedEquilibriumSolution owner action
badSolutionInterchange = id

badCoarseSolutionInterchange ::
    CoarseCorrelatedEquilibriumSolution owner action ->
    CorrelatedEquilibriumSolution owner action
badCoarseSolutionInterchange = id

badSolutionInterchangeCoerce ::
    CorrelatedEquilibriumSolution String Bool ->
    CoarseCorrelatedEquilibriumSolution String Bool
badSolutionInterchangeCoerce = coerce

badCoarseAccessorOnCorrelated ::
    CorrelatedEquilibriumSolution owner action ->
    CoarseCorrelatedEquilibriumReport owner action
badCoarseAccessorOnCorrelated = coarseCorrelatedSolutionCheck

badCorrelatedAccessorOnCoarse ::
    CoarseCorrelatedEquilibriumSolution owner action ->
    CorrelatedEquilibriumReport owner action
badCorrelatedAccessorOnCoarse = correlatedSolutionCheck

badReportInterchange ::
    CorrelatedEquilibriumReport owner action ->
    CoarseCorrelatedEquilibriumReport owner action
badReportInterchange = id
