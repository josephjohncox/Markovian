{-# LANGUAGE DataKinds #-}

module ContinuousBoundary where

import Data.Coerce (coerce)
import Markovian.Continuous.Condition.Exact
import Markovian.Continuous.Kernel.Exact
import Markovian.Continuous.Measure.Exact
import Markovian.Continuous.Space

data OwnerA
data OwnerB

badLawConstructor :: ExactLaw RealBorel
badLawConstructor = ExactLaw 0 []

badKernelConstructor :: ExactContinuousKernel RealBorel RealBorel
badKernelConstructor = ExactContinuousKernel 1 0 []

badPosteriorConstructor :: ExactPosterior Bool
badPosteriorConstructor = ExactPosterior True 1 badLawConstructor 0 1

badOwnerCoerce :: NoiseOwner OwnerA -> NoiseOwner OwnerB
badOwnerCoerce = coerce

badLawCoerce :: ExactLaw (Discrete Bool) -> ExactLaw RealBorel
badLawCoerce = coerce

badArbitraryDensity = density (const 1)

badConditionPoint = conditionPoint

badGeneralDisintegrate = disintegrate
