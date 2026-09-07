module AggregationUpdate where

import Markovian.Aggregation.Exact

badCheckedUpdate :: CheckedAggregation Int Int Int -> CheckedAggregation Int Int Int
badCheckedUpdate q = q{aggregationMapping = []}
