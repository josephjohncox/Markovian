module CorrelatedSolverConstruction where

import Markovian.Game.Correlated.Exact

-- Solution construction. The solution, limits and accounting types have hidden
-- positional constructors, so naming any of them at the term level must fail.

badCorrelatedSolutionConstructor :: CorrelatedEquilibriumSolution owner action
badCorrelatedSolutionConstructor =
    CorrelatedEquilibriumSolution undefined undefined undefined undefined

badCoarseSolutionConstructor :: CoarseCorrelatedEquilibriumSolution owner action
badCoarseSolutionConstructor =
    CoarseCorrelatedEquilibriumSolution undefined undefined undefined undefined

badSolveLimitsConstructor :: CorrelationSolveLimits
badSolveLimitsConstructor =
    CorrelationSolveLimits undefined undefined undefined undefined

badSolveAccountingConstructor :: CorrelationSolveAccounting
badSolveAccountingConstructor =
    CorrelationSolveAccounting undefined undefined undefined undefined undefined undefined undefined undefined undefined
