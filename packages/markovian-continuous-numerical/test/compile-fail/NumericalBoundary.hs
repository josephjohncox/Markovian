module NumericalBoundary where

import Markovian.Continuous.Measure.Exact
import Markovian.Continuous.Numerical.Generator
import Markovian.Continuous.Numerical.MonteCarlo
import Markovian.Continuous.Numerical.Quadrature
import Markovian.Continuous.Numerical.Value

badGeneratorConstructor :: Generator
badGeneratorConstructor = Generator 0 1

badGeneratorStateConstructor :: GeneratorState
badGeneratorStateConstructor = GeneratorState 0 1

badReportConstructor :: QuadratureReport
badReportConstructor = QuadratureReport 0 0 0 0 0 EstimatedToleranceMet

badEstimateAsExact :: QuadratureReport -> ExactIntegralReport
badEstimateAsExact = id

badMonteCarloStateConstructor :: MonteCarloState
badMonteCarloStateConstructor = MonteCarloState 0 0 0 badGeneratorStateConstructor badGeneratorConstructor
