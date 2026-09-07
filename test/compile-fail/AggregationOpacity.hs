module AggregationOpacity where

import Markovian.Aggregation.Exact

badCheckedConstructor :: CheckedAggregation Int Int Int
badCheckedConstructor = CheckedAggregation undefined undefined [] 0 []

badWitnessConstructor :: AggregationWitness Int Int Int
badWitnessConstructor = AggregationWitness (0, 1) 0 (AggregationObservationDifference 0 1)
