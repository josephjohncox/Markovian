{-# LANGUAGE DataKinds #-}

module CircuitCachePrimitiveRole where

import Data.Coerce
import Markovian.Circuit
import Markovian.Open.Acyclic.Circuit.Exact

newtype OtherValue = OtherValue Bool
badPrimitiveCoerce :: ExactTablePrimitive 'Stochastic Bool Bool -> ExactTablePrimitive 'Stochastic OtherValue OtherValue
badPrimitiveCoerce = coerce
