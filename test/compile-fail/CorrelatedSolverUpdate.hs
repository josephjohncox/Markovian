module CorrelatedSolverUpdate where

import Markovian.Game.Correlated.Exact

-- Record update. Contract §3 requires the accessors to be functions, not record
-- selectors, so no record-update syntax may exist for a solution, its
-- accounting, or the solve limits. Clients cannot update a solution.

badCorrelatedSolutionUpdate ::
    CorrelatedEquilibriumSolution owner action ->
    CorrelatedEquilibriumSolution owner action
badCorrelatedSolutionUpdate solution = solution{correlatedSolutionDevice = undefined}

badCoarseSolutionUpdate ::
    CoarseCorrelatedEquilibriumSolution owner action ->
    CoarseCorrelatedEquilibriumSolution owner action
badCoarseSolutionUpdate solution =
    solution{coarseCorrelatedSolutionDevice = undefined}

badAccountingUpdate :: CorrelationSolveAccounting -> CorrelationSolveAccounting
badAccountingUpdate account = account{correlationSolveCandidates = undefined}

badLimitsUpdate :: CorrelationSolveLimits -> CorrelationSolveLimits
badLimitsUpdate limits = limits{maximumCorrelationSolveCandidates = undefined}
