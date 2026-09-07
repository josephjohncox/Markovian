module AggregationRoles where

import Data.Coerce (coerce)
import Markovian.Aggregation.Exact

newtype State = State Int
newtype Block = Block Int
newtype Observation = Observation Int

badCheckedState :: CheckedAggregation Int Int Int -> CheckedAggregation State Int Int
badCheckedState = coerce
badCheckedBlock :: CheckedAggregation Int Int Int -> CheckedAggregation Int Block Int
badCheckedBlock = coerce
badCheckedObservation :: CheckedAggregation Int Int Int -> CheckedAggregation Int Int Observation
badCheckedObservation = coerce
badWitnessState :: AggregationWitness Int Int Int -> AggregationWitness State Int Int
badWitnessState = coerce
badWitnessBlock :: AggregationWitness Int Int Int -> AggregationWitness Int Block Int
badWitnessBlock = coerce
badWitnessObservation :: AggregationWitness Int Int Int -> AggregationWitness Int Int Observation
badWitnessObservation = coerce
badResultState :: AggregationResult Int Int Int -> AggregationResult State Int Int
badResultState = coerce
badResultBlock :: AggregationResult Int Int Int -> AggregationResult Int Block Int
badResultBlock = coerce
badResultObservation :: AggregationResult Int Int Int -> AggregationResult Int Int Observation
badResultObservation = coerce
