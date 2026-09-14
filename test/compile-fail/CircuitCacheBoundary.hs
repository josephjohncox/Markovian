{-# LANGUAGE DataKinds #-}

module CircuitCacheBoundary where

import Data.Coerce (coerce)
import Markovian.Circuit
import Markovian.Open.Acyclic.Circuit.Exact

newtype OtherValue = OtherValue Bool

badOwnerCoerce ::
    RetainedAcyclicOpenCircuit purity () Int Int Int Int Int Bool ->
    RetainedAcyclicOpenCircuit purity () Int Int Int Int Int OtherValue
badOwnerCoerce = coerce

badOwnerConstructor :: RetainedAcyclicOpenCircuit 'Stochastic () Int Int Int Int Int Bool
badOwnerConstructor = RetainedAcyclicOpenCircuit undefined undefined undefined

badPrimitiveConstructor :: ExactTablePrimitive 'Stochastic Bool Bool
badPrimitiveConstructor = StochasticTablePrimitive undefined

badCallbackOwner ::
    AcyclicOpenCircuit primitive purity () Int Int Int Int Int Bool ->
    RetainedAcyclicOpenCircuit purity () Int Int Int Int Int Bool
badCallbackOwner = retainAcyclicOpenCircuit ExactTableInterpreterV1
