module AggregationWitnessUpdate where

import Markovian.Aggregation.Exact

badWitnessUpdate :: AggregationWitness Int Int Int -> AggregationWitness Int Int Int
badWitnessUpdate w = w{aggregationWitnessStates = (0, 1)}
